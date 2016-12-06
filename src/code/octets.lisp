;;;; code for string to octet conversion

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;; FIXME: The latin9 stuff is currently #+sb-unicode, because I
;;; don't like the idea of trying to do CODE-CHAR #x<big>.  Is that a
;;; justified fear?  Can we arrange that it's caught and converted to
;;; a decoding error error?  Or should we just give up on non-Unicode
;;; builds?

(in-package "SB-IMPL")

;;; Utilities used in both to-string and to-octet conversions

(defmacro instantiate-octets-definition (definer)
  `(progn
    (,definer aref (simple-array (unsigned-byte 8) (*)))
    (,definer sap-ref-8 system-area-pointer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-od-name (sym1 sym2)
    (package-symbolicate (cl:symbol-package sym1) sym1 "-" sym2)))

;;;; to-octets conversions

;;; to latin (including ascii)

;;; Converting bytes to character codes is easy: just use a 256-element
;;; lookup table that maps each possible byte to its corresponding
;;; character code.
;;;
;;; Converting character codes to bytes is a little harder, since the
;;; codes may be spare (e.g. we use codes 0-127, 3490, and 4598).  The
;;; previous version of this macro utilized a gigantic CASE expression
;;; to do the hard work, with the result that the code was huge (since
;;; SBCL's then-current compilation strategy for CASE expressions was
;;; (and still is) converting CASE into COND into if-the-elses--which is
;;; also inefficient unless your code happens to occur very early in the
;;; chain.
;;;
;;; The current strategy is to build a table:
;;;
;;; [ ... code_1 byte_1 code_2 byte_2 ... code_n byte_n ... ]
;;;
;;; such that the codes are sorted in order from lowest to highest.  We
;;; can then binary search the table to discover the appropriate byte
;;; for a character code.  We also implement an optimization: all unibyte
;;; mappings do not remap ASCII (0-127) and some do not remap part of
;;; the range beyond character code 127.  So we check to see if the
;;; character code falls into that range first (a quick check, since
;;; character codes are guaranteed to be positive) and then do the binary
;;; search if not.  This optimization also enables us to cut down on the
;;; size of our lookup table.
(defmacro define-unibyte-mapper (byte-char-name code-byte-name &rest exceptions)
  (let* (;; Build a list of (CODE BYTE) pairs
         (pairs (loop for byte below 256
                   for code = (let ((exception (cdr (assoc byte exceptions))))
                                (cond
                                  ((car exception) (car exception))
                                  ((null exception) byte)
                                  (t nil)))
                   when code collect (list code byte) into elements
                   finally (return elements)))
         ;; Find the smallest character code such that the corresponding
         ;; byte is != to the code.
         (lowest-non-equivalent-code
          (caar (sort (copy-seq exceptions) #'< :key #'car)))
         ;; Sort them for our lookup table.
         (sorted-pairs (sort (subseq pairs lowest-non-equivalent-code)
                             #'< :key #'car))
         ;; Create the lookup table.
         (sorted-lookup-table
          (reduce #'append sorted-pairs :from-end t :initial-value nil)))
    `(progn
         ;; We *could* inline this, but it's not obviously the right thing,
         ;; because each use of the inlined function in a different file
         ;; would be forced to dump the large-ish array. To do things like
         ;; this, you generally want a load-time ref to a global constant.
       ;(declaim (inline ,byte-char-name))
         (defun ,byte-char-name (byte)
           (declare (optimize speed (safety 0))
                    (type (unsigned-byte 8) byte))
           ,(let ((byte-to-code
                   (loop for byte below 256
                      collect (let ((exception (cdr (assoc byte exceptions))))
                                (if exception
                                    (car exception)
                                    byte)))))
              (if (position nil byte-to-code)
                  ;; There are bytes with no translation. Represent "missing"
                  ;; as -1 when stored, convert to NIL when accessed.
                  ;; We could use a single otherwise-unused point to mean NIL,
                  ;; but it would be confusing if in one table #xFFFF represents
                  ;; NIL and another #xF00D represents NIL.
                  `(let ((code (aref ,(sb-c::coerce-to-smallest-eltype
                                       (substitute -1 nil byte-to-code))
                                     byte)))
                     (if (>= code 0) code))
                  ;; Every byte has a translation
                  `(aref ,(sb-c::coerce-to-smallest-eltype byte-to-code)
                         byte))))
         (defun ,code-byte-name (code)
           (declare (optimize speed (safety 0))
                    (type char-code code))
           (if (< code ,lowest-non-equivalent-code)
               code
               (loop with code-to-byte-table =
                    ,(sb-c::coerce-to-smallest-eltype sorted-lookup-table)
                  with low = 0
                  with high = (- (length code-to-byte-table) 2)
                  while (< low high)
                  do (let ((mid (logandc2 (truncate (+ low high 2) 2) 1)))
                       (if (< code (aref code-to-byte-table mid))
                           (setf high (- mid 2))
                           (setf low mid)))
                  finally (return (if (eql code (aref code-to-byte-table low))
                                      (aref code-to-byte-table (1+ low))
                                      nil))))))))

(declaim (inline get-latin-bytes))
(defun get-latin-bytes (mapper external-format string pos)
  (let ((code (funcall mapper (char-code (char string pos)))))
    (declare (type (or null char-code) code))
    (values (cond
              ((and code (< code 256)) code)
              (t
               (encoding-error external-format string pos)))
            1)))

(declaim (inline string->latin%))
(defun string->latin% (string sstart send get-bytes null-padding)
  (declare (optimize speed)
           (type simple-string string)
           (type index sstart send)
           (type (integer 0 1) null-padding)
           (type function get-bytes))
  ;; The latin encodings are all unibyte encodings, so just directly
  ;; compute the number of octets we're going to generate.
  (let ((octets (make-array (+ (- send sstart) null-padding)
                            ;; This takes care of any null padding the
                            ;; caller requests.
                            :initial-element 0
                            :element-type '(unsigned-byte 8)))
        (index 0)
        (error-position 0)
        (error-replacement))
    (tagbody
     :no-error
       (loop for pos of-type index from sstart below send
          do (let ((byte (funcall get-bytes string pos)))
               (typecase byte
                 ((unsigned-byte 8)
                  (locally (declare (optimize (sb-c::insert-array-bounds-checks 0)))
                    (setf (aref octets index) byte)))
                 ((simple-array (unsigned-byte 8) (*))
                  ;; KLUDGE: We ran into encoding errors.  Bail and do
                  ;; things the slow way (does anybody actually use this
                  ;; functionality besides our own test suite?).
                  (setf error-position pos error-replacement byte)
                  (go :error)))
               (incf index))
          finally (return-from string->latin% octets))
     :error
       ;; We have encoded INDEX octets so far and we ran into an
       ;; encoding error at ERROR-POSITION; the user has asked us to
       ;; replace the expected output with ERROR-REPLACEMENT.
       (let ((new-octets (make-array (* index 2)
                                     :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer index)))
         (replace new-octets octets)
         (flet ((extend (thing)
                 (typecase thing
                   ((unsigned-byte 8) (vector-push-extend thing new-octets))
                   ((simple-array (unsigned-byte 8) (*))
                    (dotimes (i (length thing))
                      (vector-push-extend (aref thing i) new-octets))))))
           (extend error-replacement)
           (loop for pos of-type index from (1+ error-position) below send
                 do (extend (funcall get-bytes string pos))
                 finally (return-from string->latin%
                           (progn
                             (unless (zerop null-padding)
                               (vector-push-extend 0 new-octets))
                             (copy-seq new-octets)))))))))

;;;; to-string conversions

;;; from latin (including ascii)

(defmacro define-latin->string* (accessor type)
  (let ((name (make-od-name 'latin->string* accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (string sstart send array astart aend mapper)
        (declare (optimize speed (safety 0))
                 (type simple-string string)
                 (type ,type array)
                 (type array-range sstart send astart aend)
                 (function mapper))
        (loop for spos from sstart below send
           for apos from astart below aend
           do (setf (char string spos)
                    (code-char (funcall mapper (,accessor array apos))))
           finally (return (values string spos apos)))))))
(instantiate-octets-definition define-latin->string*)

(defmacro define-latin->string (accessor type)
  (let ((name (make-od-name 'latin->string accessor)))
    `(progn
      (declaim (inline ,name))
      (defun ,name (array astart aend mapper)
        (declare (optimize speed (safety 0))
                 (type ,type array)
                 (type array-range astart aend)
                 (type function mapper))
        (let ((length (the array-range (- aend astart))))
          (values (,(make-od-name 'latin->string* accessor) (make-string length) 0 length
                                                            array astart aend
                                                            mapper)))))))
(instantiate-octets-definition define-latin->string)


;;;; public interface

(defun maybe-defaulted-external-format (external-format)
  (get-external-format-or-lose (if (eq external-format :default)
                                   (default-external-format)
                                   external-format)))

(declaim (ftype (sfunction ((vector (unsigned-byte 8)) &key (:external-format t)
                                   (:start index)
                                   (:end sequence-end))
                           (or (simple-array character (*))
                               (simple-array base-char (*))))
                octets-to-string))
(defun octets-to-string (vector &key (external-format :default) (start 0) end)
  "Return a string obtained by decoding VECTOR according to EXTERNAL-FORMAT.

If EXTERNAL-FORMAT is given, it must designate an external format.

If given, START and END must be bounding index designators and
designate a subsequence of VECTOR that should be decoded.

If some of the octets of VECTOR (or the subsequence bounded by START
and END) cannot be decoded by EXTERNAL-FORMAT an error of a subtype of
SB-INT:CHARACTER-DECODING-ERROR is signaled.

Note that for some values of EXTERNAL-FORMAT the length of the
returned string may be different from the length of VECTOR (or the
subsequence bounded by START and END)."
  (declare (explicit-check start end :result))
  (with-array-data ((vector vector)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type (simple-array (unsigned-byte 8) (*)) vector))
    (let ((ef (maybe-defaulted-external-format external-format)))
      (funcall (ef-octets-to-string-fun ef) vector start end))))

(declaim (ftype (sfunction (string &key (:external-format t)
                                   (:start index)
                                   (:end sequence-end)
                                   (:null-terminate t))
                           (simple-array (unsigned-byte 8) (*)))
                string-to-octets))
(defun string-to-octets (string &key (external-format :default)
                         (start 0) end null-terminate)
  "Return an octet vector that is STRING encoded according to EXTERNAL-FORMAT.

If EXTERNAL-FORMAT is given, it must designate an external format.

If given, START and END must be bounding index designators and
designate a subsequence of STRING that should be encoded.

If NULL-TERMINATE is true, the returned octet vector ends with an
additional 0 element that does not correspond to any part of STRING.

If some of the characters of STRING (or the subsequence bounded by
START and END) cannot be encoded by EXTERNAL-FORMAT an error of a
subtype of SB-INT:CHARACTER-ENCODING-ERROR is signaled.

Note that for some values of EXTERNAL-FORMAT and NULL-TERMINATE the
length of the returned vector may be different from the length of
STRING (or the subsequence bounded by START and END)."
  (declare (explicit-check start end :result))
  (with-array-data ((string string)
                    (start start)
                    (end end)
                    :check-fill-pointer t)
    (declare (type simple-string string))
    (let ((ef (maybe-defaulted-external-format external-format)))
      (funcall (ef-string-to-octets-fun ef) string start end
               (if null-terminate 1 0)))))

#+sb-unicode
(defvar +unicode-replacement-character+ (string (code-char #xfffd)))
#+sb-unicode
(defun use-unicode-replacement-char (condition)
  (use-value +unicode-replacement-character+ condition))

;;; Utilities that maybe should be exported

#+sb-unicode
(defmacro with-standard-replacement-character (&body body)
  `(handler-bind ((octet-encoding-error #'use-unicode-replacement-char))
    ,@body))

(defmacro with-default-decoding-replacement ((c) &body body)
  (let ((cname (gensym)))
  `(let ((,cname ,c))
    (handler-bind
        ((octet-decoding-error (lambda (c)
                                 (use-value ,cname c))))
      ,@body))))

(defun add-replacements-to-character-coding (character-coding replacement)
  (wrap-character-coding-functions
   character-coding
   (lambda (function)
     (declare (type (or null function) function))
     (when function
       (lambda (&rest rest)
         (declare (dynamic-extent rest))
         (handler-bind
             ((stream-decoding-error
               (lambda (c)
                 (declare (ignore c))
                 (invoke-restart 'input-replacement replacement)))
              (stream-encoding-error
               (lambda (c)
                 (declare (ignore c))
                 (invoke-restart 'output-replacement replacement)))
              (octets-encoding-error
               (lambda (c) (use-value replacement c)))
              (octet-decoding-error
               (lambda (c) (use-value replacement c))))
           (apply function rest)))))))

;;; This function was moved from 'fd-stream' because it depends on
;;; the various error classes, two of which are defined just above.
(defun get-external-format (external-format)
  (flet ((keyword-external-format (keyword)
           (declare (type keyword keyword))
           (gethash keyword *external-formats*))
         (replacement-handlerify (entry replacement)
           (when entry
             (add-replacements-to-character-coding entry))))
    (typecase external-format
      (keyword (keyword-external-format external-format))
      ((cons keyword)
       (let ((entry (keyword-external-format (car external-format)))
             (replacement (getf (cdr external-format) :replacement)))
         (if replacement
             (replacement-handlerify entry replacement)
             entry))))))

(push
  `("SB-IMPL"
    char-class char-class2 char-class3
    ,@(let (macros)
        (flet ((ends-with-p (s1 s2)
                 (let ((diff (- (length s1) (length s2))))
                   (and (>= diff 0) (string= s1 s2 :start1 diff)))))
          (do-symbols (s "SB-IMPL" macros)
            (let ((name (symbol-name s)))
              (when (and (macro-function s)
                         (eql (mismatch name "DEFINE-") 7)
                         (or (ends-with-p name "->STRING")
                             (ends-with-p name "->STRING*")))
                (push s macros)))))))
  *!removable-symbols*)
