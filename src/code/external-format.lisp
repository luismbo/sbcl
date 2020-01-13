;;;; External formats: character and newline coding

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")


;;; Utilities

(deftype error-policy ()
  '(or null function-name function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ensure-list-of-names (context name-or-names)
    (let* ((names (ensure-list name-or-names))
           (offender (find-if-not #'keywordp names)))
      (when offender
        (error "~@<~A name ~S is not a keyword.~@:>"
               context offender))
      names)))


;;; CHARACTER-CODING

(defstruct (character-coding
            (:constructor %make-character-coding)
            (:constructor %copy-character-coding
                          (other
                           &key
                           (read-n-chars-fun (cc-read-n-chars-fun other))
                           (read-char-fun (cc-read-char-fun other))
                           (write-char-none-buffered-fun (cc-write-char-none-buffered-fun other))
                           (write-char-line-buffered-fun (cc-write-char-line-buffered-fun other))
                           (write-char-full-buffered-fun (cc-write-char-full-buffered-fun other))
                           (names (cc-names other))
                           (default-replacement-character (cc-default-replacement-character other))
                           (bytes-for-char-fun (cc-bytes-for-char-fun other))
                           (write-n-bytes-fun (cc-write-n-bytes-fun other))
                           (resync-fun (cc-resync-fun other))
                           (read-c-string-fun (cc-read-c-string-fun other))
                           (write-c-string-fun (cc-write-c-string-fun other))
                           (octets-to-string-fun (cc-octets-to-string-fun other))
                           (string-to-octets-fun (cc-string-to-octets-fun other))))
            (:conc-name cc-)
            (:copier nil))
  ;; All the names that can refer to this character coding.  The first
  ;; one is the canonical name.
  (names                         (missing-arg) :type list               :read-only t)

  (default-replacement-character (missing-arg) :type character          :read-only t)

  ;; Character sizes.
  (bytes-for-char-fun            (missing-arg) :type function           :read-only t)
  ;; Reading characters from streams.
  (read-n-chars-fun              (missing-arg) :type function           :read-only t)
  (read-char-fun                 (missing-arg) :type function           :read-only t)
  ;; Writing characters to stream.
  (write-n-bytes-fun             (missing-arg) :type function           :read-only t)
  (write-char-none-buffered-fun  (missing-arg) :type function           :read-only t)
  (write-char-line-buffered-fun  (missing-arg) :type function           :read-only t)
  (write-char-full-buffered-fun  (missing-arg) :type function           :read-only t)
  ;; Can be nil for fixed-width formats.
  (resync-fun                    nil           :type (or function null) :read-only t)
  ;; Reading/writing C strings from/to stream.
  (read-c-string-fun             (missing-arg) :type function           :read-only t)
  (write-c-string-fun            (missing-arg) :type function           :read-only t)
  ;; Conversion between strings and octet-vectors.
  (octets-to-string-fun          (missing-arg) :type function           :read-only t)
  (string-to-octets-fun          (missing-arg) :type function           :read-only t))
(declaim (freeze-type character-coding))

(declaim (ftype (sfunction (character-coding) symbol) cc-name))
(defun cc-name (character-coding)
  (first (cc-names character-coding)))

(defmethod print-object ((object character-coding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cc-name object) stream)))

(defun variable-width-character-coding-p (character-coding)
  (and (character-coding-p character-coding)
       (cc-resync-fun character-coding)))

(defun cc-char-size (character-coding)
  (if (variable-width-character-coding-p character-coding)
      (cc-bytes-for-char-fun character-coding)
      (funcall (cc-bytes-for-char-fun character-coding) #\x)))

(defun wrap-character-coding-functions (character-coding function)
  (%copy-character-coding
   character-coding
   :read-n-chars-fun (funcall function (cc-read-n-chars-fun character-coding))
   :read-char-fun (funcall function (cc-read-char-fun character-coding))
   :write-n-bytes-fun (funcall function (cc-write-n-bytes-fun character-coding))
   :write-char-none-buffered-fun (funcall function (cc-write-char-none-buffered-fun character-coding))
   :write-char-line-buffered-fun (funcall function (cc-write-char-line-buffered-fun character-coding))
   :write-char-full-buffered-fun (funcall function (cc-write-char-full-buffered-fun character-coding))
   :resync-fun (funcall function (cc-resync-fun character-coding))
   :bytes-for-char-fun (funcall function (cc-bytes-for-char-fun character-coding))
   :read-c-string-fun (funcall function (cc-read-c-string-fun character-coding))
   :write-c-string-fun (funcall function (cc-write-c-string-fun character-coding))
   :octets-to-string-fun (funcall function (cc-octets-to-string-fun character-coding))
   :string-to-octets-fun (funcall function (cc-string-to-octets-fun character-coding))))

;;; All available character codings. The table maps from
;;; character-coding names to CHARACTER-CODING structures.
(declaim (type hash-table **character-codings**))
(define-load-time-global **character-codings** (make-hash-table :test #'eq))

(declaim (ftype (sfunction (keyword &key (:if-does-not-exist error-policy))
                           (or null character-coding))
                find-character-coding))
(defun find-character-coding (designator &key if-does-not-exist)
  "Return the character coding designated by DESIGNATOR.

IF-DOES-NOT-EXIST controls the behavior in case the requested newline
coding cannot be found:

NIL

  Return NIL if DESIGNATOR does not designate a newline coding.

ERROR

  TODO Signal an error.

Experimental."
  (or (gethash designator **character-codings**)
      (etypecase if-does-not-exist
        (null
         if-does-not-exist)
        ((or function-name function)
         (funcall if-does-not-exist
                  "~@<Undefined character-coding: ~S~@:>"
                  designator)))))

(declaim (ftype (sfunction (character-coding keyword &key (:if-does-not-exist t))
                           character-coding)
                (setf find-character-coding)))
(defun (setf find-character-coding) (new-value designator &key if-does-not-exist)
  "Install NEW-VALUE as the character coding for the name DESIGNATOR.

IF-DOES-NOT-EXIST is accepted for parity with FIND-CHARACTER-CODING
and ignored.

Experimental."
  (declare (ignore if-does-not-exist))
  (setf (gethash designator **character-codings**) new-value))

(defun register-character-coding (names character-coding)
  (dolist (name names)
    (setf (find-character-coding name) character-coding))
  character-coding)

(defmacro define-character-coding (name-or-names
                                   &rest args
                                   &key
                                   (default-replacement-character (missing-arg))
                                   (read-n-chars-fun (missing-arg))
                                   (read-char-fun (missing-arg))
                                   (write-n-bytes-fun (missing-arg))
                                   (write-char-none-buffered-fun (missing-arg))
                                   (write-char-line-buffered-fun (missing-arg))
                                   (write-char-full-buffered-fun (missing-arg))
                                   (resync-fun (missing-arg))
                                   (bytes-for-char-fun (missing-arg))
                                   (read-c-string-fun (missing-arg))
                                   (write-c-string-fun (missing-arg))
                                   (octets-to-string-fun (missing-arg))
                                   (string-to-octets-fun (missing-arg)))
  (declare (ignore default-replacement-character read-n-chars-fun
                   read-char-fun write-n-bytes-fun
                   write-char-none-buffered-fun
                   write-char-line-buffered-fun
                   write-char-full-buffered-fun resync-fun
                   bytes-for-char-fun read-c-string-fun
                   write-c-string-fun octets-to-string-fun
                   string-to-octets-fun))
  (let* ((names (with-current-source-form (name-or-names)
                  (ensure-list-of-names "character coding" name-or-names))))
    `(let ((names ',names))
       (register-character-coding
        names (%make-character-coding :names names ,@args))
       (first names))))

(defmacro define-character-coding/unibyte
    ((canonical-name &rest other-names)
     &key
     (out-form                (missing-arg))
     (in-form                 (missing-arg))
     (octets-to-string-symbol (missing-arg))
     (string-to-octets-symbol (missing-arg)))
  `(define-character-coding/variable-width (,canonical-name ,@other-names)
     :output-restart t
     :replacement-character #\?
     :out-size-expr 1 :out-expr ,out-form
     :in-size-expr  1 :in-expr  ,in-form
     :octets-to-string-symbol ,octets-to-string-symbol
     :string-to-octets-symbol ,string-to-octets-symbol))

(defmacro define-character-coding/unibyte-mapping
    ((canonical-name &rest other-names) &body exceptions)
  (let ((->code-name (symbolicate canonical-name '->code-mapper))
        (code->-name (symbolicate 'code-> canonical-name '-mapper))
        (get-bytes-name (symbolicate 'get- canonical-name '-bytes))
        (string->-name (symbolicate 'string-> canonical-name))
        (define-string*-name (symbolicate 'define- canonical-name '->string*))
        (string*-name (symbolicate canonical-name '->string*))
        (define-string-name (symbolicate 'define- canonical-name '->string))
        (string-name (symbolicate canonical-name '->string))
        (->string-aref-name (symbolicate canonical-name '->string-aref)))
    `(progn
       (define-unibyte-mapper ,->code-name ,code->-name
         ,@exceptions)

       (declaim (inline ,get-bytes-name))
       (defun ,get-bytes-name (string pos)
         (declare (optimize speed (safety 0))
                  (type simple-string string)
                  (type array-range pos))
         (get-latin-bytes #',code->-name ,canonical-name string pos))

       (defun ,string->-name (string sstart send null-padding)
         (declare (optimize speed (safety 0))
                  (type simple-string string)
                  (type array-range sstart send))
         (values (string->latin% string sstart send #',get-bytes-name null-padding)))

       (defmacro ,define-string*-name (accessor type)
         (declare (ignore type))
         (let ((name (make-od-name ',string*-name accessor)))
           `(progn
              (defun ,name (string sstart send array astart aend)
                (,(make-od-name 'latin->string* accessor)
                  string sstart send array astart aend #',',->code-name)))))
       (instantiate-octets-definition ,define-string*-name)

       (defmacro ,define-string-name (accessor type)
         (declare (ignore type))
         (let ((name (make-od-name ',string-name accessor)))
           `(progn
              (defun ,name (array astart aend)
                (,(make-od-name 'latin->string accessor)
                  array astart aend #',',->code-name)))))
       (instantiate-octets-definition ,define-string-name)

       (define-character-coding/unibyte (,canonical-name ,@other-names)
         :out-form
         (let ((octet (,code->-name bits)))
           (if octet
               (setf (sap-ref-8 sap tail) octet)
               (external-format-encoding-error stream bits)))
         :in-form
         (let ((code (,->code-name byte)))
           (if code
               (code-char code)
               (return-from decode-break-reason 1)))
         :octets-to-string-symbol ,->string-aref-name
         :string-to-octets-symbol ,string->-name))))

(defmacro define-character-coding/variable-width
    ((&whole names canonical-name &rest other-names)
     &key
     (output-restart          (missing-arg))
     (replacement-character   (missing-arg))

     (out-size-expr           (missing-arg))
     (out-expr                (missing-arg))

     (in-size-expr            (missing-arg))
     (in-expr                 (missing-arg))

     (octets-to-string-symbol (missing-arg))
     (string-to-octets-symbol (missing-arg))

     base-string-direct-mapping)
  (declare (ignore other-names))
  ;; TODO names
  (let* ((size-function (symbolicate "BYTES-FOR-CHAR/" canonical-name))
         (out-function (symbolicate "OUTPUT-BYTES/" canonical-name))
         (format (format nil "OUTPUT-CHAR-~A-~~A-BUFFERED" (string canonical-name)))

         (in-char-function (symbolicate "INPUT-CHAR/" canonical-name))
         (in-function (symbolicate "FD-STREAM-READ-N-CHARACTERS/" canonical-name))

         (resync-function (symbolicate "RESYNC/" canonical-name))

         (read-c-string-function (symbolicate "READ-FROM-C-STRING/" canonical-name))
         (output-c-string-function (symbolicate "OUTPUT-TO-C-STRING/" canonical-name))

         (n-buffer (gensym "BUFFER"))

         (in-size-min  (if (consp in-size-expr) (first in-size-expr) in-size-expr))
         (in-size-expr (when (consp in-size-expr) (second in-size-expr))))
    `(progn
       (defun ,size-function (byte)
         (declare (ignorable byte))
         ,out-size-expr)

       (defun ,out-function (stream string flush-p start end)
         (let ((start (or start 0))
               (end (or end (length string)))
               (obuf (fd-stream-obuf stream)))
           (declare (type index start end))
           (synchronize-stream-output stream)
           (unless (<= 0 start end (length string))
             (sequence-bounding-indices-bad-error string start end))

           (do ()
               ((= end start))
             (string-dispatch (simple-base-string
                               #+sb-unicode (simple-array character (*))
                               string)
                 string
               (let ((len (buffer-length obuf))
                     (sap (buffer-sap obuf))
                     ;; FIXME: Rename
                     (tail (buffer-tail obuf)))
                 (declare (type index tail)
                          (optimize (safety 0))) ; STRING bounds have already been checked
                 (,@(if output-restart
                        `(block output-nothing)
                        `(progn))
                  (do* ()
                       ((or (= start end) (< (- len tail) 4)))
                    (let* ((byte (aref string start))
                           (bits (char-code byte))
                           (size ,out-size-expr))
                      ,out-expr
                      (incf tail size)
                      (setf (buffer-tail obuf) tail)
                      (incf start)))
                  (go flush))
                 ;; Exited via RETURN-FROM OUTPUT-NOTHING: skip the
                 ;; current character.
                 (incf start)))
            flush
             (when (< start end)
               (flush-output-buffer stream)))
           (when flush-p
             (flush-output-buffer stream))))

       (def-output-routines/variable-width (,format
                                            ,out-size-expr
                                            ,output-restart
                                            ,names ; TODO just canonical-name?
                                            (:none character)
                                            (:line character)
                                            (:full character))
         ;; FIXME We expect the newline coding to set the column to
         ;; 0. But if the newline coding is the identity, it is
         ;; bypassed, so the column does not get set.
         (setf (fd-stream-output-column stream)
               (+ (truly-the unsigned-byte (fd-stream-output-column stream)) 1))
         (let ((bits (char-code byte))
               (sap (buffer-sap obuf))
               (tail (buffer-tail obuf)))
           ,out-expr))

       (def-input-routine/variable-width ,in-char-function
           (character ,names ,(if in-size-expr (list in-size-min in-size-expr) in-size-min) sap head)
         (let ((byte (sap-ref-8 sap head)))
           (declare (ignorable byte))
           ,in-expr))

       (defun ,in-function (stream buffer start requested eof-error-p
                            &aux (total-copied 0))
         (declare (type fd-stream stream)
                  (type index start requested total-copied)
                  (type
                   (simple-array character (#.+ansi-stream-in-buffer-length+))
                   buffer))
         (when (fd-stream-eof-forced-p stream)
           (setf (fd-stream-eof-forced-p stream) nil)
           (return-from ,in-function 0))
         ;; TODO explain
         (do ((instead (fd-stream-instead stream)))
             ((= (fill-pointer instead) 0)
              (setf (fd-stream-listen stream) nil))
           (setf (aref buffer (+ start total-copied)) (vector-pop instead))
           (incf total-copied)
           (when (= requested total-copied)
             (when (= (fill-pointer instead) 0)
               (setf (fd-stream-listen stream) nil))
             (return-from ,in-function total-copied)))
         ;;
         (do ()
             (nil)
           (let* ((ibuf (fd-stream-ibuf stream))
                  (head (buffer-head ibuf))
                  (tail (buffer-tail ibuf))
                  (sap (buffer-sap ibuf))
                  (decode-break-reason nil))
             (declare (type index head tail))
             ;; Copy data from stream buffer into user's buffer.
             (do ((size nil nil))
                 ((or (= tail head) (= requested total-copied)))
               (setf decode-break-reason
                     (block decode-break-reason
                       ,@(when in-size-expr
                           `((when (> ,in-size-min (- tail head))
                               (return))))
                       (let ((byte (sap-ref-8 sap head)))
                         (declare (ignorable byte))
                         (setq size ,(or in-size-expr in-size-min))
                         (when (> size (- tail head))
                           (return))
                         (setf (aref buffer (+ start total-copied)) ,in-expr)
                         (incf total-copied)
                         (incf head size))
                       nil))
               (setf (buffer-head ibuf) head)
               (when decode-break-reason
                 ;; If we've already read some characters on when the invalid
                 ;; code sequence is detected, we return immediately. The
                 ;; handling of the error is deferred until the next call
                 ;; (where this check will be false). This allows establishing
                 ;; high-level handlers for decode errors (for example
                 ;; automatically resyncing in Lisp comments).
                 (if (and (zerop total-copied)
                          (stream-decoding-error-and-handle
                           stream decode-break-reason)
                          eof-error-p)
                     (error 'end-of-file :stream stream)
                     ;; we might have been given stuff to use instead,
                     ;; so we have to return (and trust our caller to
                     ;; know what to do about TOTAL-COPIED being 0).
                     (return-from ,in-function total-copied))))

             (setf (buffer-head ibuf) head)
             ;; Maybe we need to refill the stream buffer.
             (cond ( ;; If was data in the stream buffer, we're done.
                    (plusp total-copied)
                    (return total-copied))
                   ( ;; If EOF, we're done in another way.
                    (or (eq decode-break-reason 'eof)
                        (null (catch 'eof-input-catcher
                                (refill-input-buffer stream))))
                    (if eof-error-p
                        (error 'end-of-file :stream stream)
                        (return total-copied)))
                   ;; Otherwise we refilled the stream buffer, so fall
                   ;; through into another pass of the loop.
                   ))))

       (defun ,resync-function (stream)
         (let ((ibuf (fd-stream-ibuf stream))
               (size ,in-size-min))
           (catch 'eof-input-catcher
             (loop :do
                      (incf (buffer-head ibuf))
                      (input-at-least stream size)
                   :while
                      (block decode-break-reason
                        (let* ((sap (buffer-sap ibuf))
                               (head (buffer-head ibuf))
                               (byte (sap-ref-8 sap head)))
                          (declare (ignorable byte))
                          ,@(when in-size-expr
                              `((setq size ,in-size-expr)
                                (input-at-least stream size)))
                          (setf head (buffer-head ibuf))
                          ,in-expr)
                        nil)))))

       (defun ,read-c-string-function (sap element-type)
         (declare (type system-area-pointer sap))
         (locally
             (declare (optimize (speed 3) (safety 0)))
           (let* ((stream ,canonical-name)
                  (size 0) (head 0) (byte 0) (char nil)
                  (length (dotimes (count (1- sb-xc:array-dimension-limit) count)
                            ;; TODO repeated below
                            (let ((decode-break-reason
                                    (block decode-break-reason
                                      (setf byte (sap-ref-8 sap head)
                                            size ,(or in-size-expr in-size-min)
                                            char ,in-expr)
                                      (incf head size)
                                      nil)))
                              (when decode-break-reason
                                (c-string-decoding-error
                                 ,canonical-name sap head decode-break-reason)))
                            (when (zerop (char-code char))
                              (return count))))
                  (string (case element-type
                            (base-char
                             (make-string length :element-type 'base-char))
                            (character
                             (make-string length :element-type 'character))
                            (t
                             (make-string length :element-type element-type)))))
             (declare (ignorable stream)
                      (type index head length) ;; size
                      (type (unsigned-byte 8) byte)
                      (type (or null character) char)
                      (type string string))
             (setf head 0)
             ;; TODO repeated above
             (dotimes (index length string)
               (let ((decode-break-reason
                       (block decode-break-reason
                         (setf byte (sap-ref-8 sap head)
                               size ,(or in-size-expr in-size-min)
                               char ,in-expr)
                         (incf head size)
                         nil)))
                 (when decode-break-reason
                   (c-string-decoding-error
                    ,canonical-name sap head decode-break-reason)))
               (setf (aref string index) char)))))

       (defun ,output-c-string-function (string)
         (declare (type simple-string string))
         (cond ,@(and base-string-direct-mapping
                      `(((simple-base-string-p string)
                         string)))
               (t
                (locally
                    (declare (optimize (speed 3) (safety 0)))
                  (block output-nothing
                    (let* ((length (length string))
                           (null-size (let* ((byte (code-char 0))
                                             (bits (char-code byte)))
                                        (declare (ignorable byte bits))
                                        (the index ,out-size-expr)))
                           (buffer-length
                            (+ (loop for i of-type index below length
                                  for byte of-type character = (aref string i)
                                  for bits = (char-code byte)
                                  sum (the index ,out-size-expr) of-type index)
                               null-size))
                           (tail 0)
                           (,n-buffer (make-array buffer-length
                                                  :element-type '(unsigned-byte 8)))
                           stream)
                      (declare (type index length buffer-length tail)
                               (type null stream)
                               (ignorable stream))
                      (with-pinned-objects (,n-buffer)
                        (let ((sap (vector-sap ,n-buffer)))
                          (declare (system-area-pointer sap))
                          (loop for i of-type index below length
                             for byte of-type character = (aref string i)
                             for bits = (char-code byte)
                             for size of-type index = ,out-size-expr
                             do (prog1
                                    ,out-expr
                                  (incf tail size)))
                          (let* ((bits 0)
                                 (byte (code-char bits))
                                 (size null-size))
                            (declare (ignorable bits byte size))
                            ,out-expr)))
                      ,n-buffer))))))

       (define-character-coding ,names
         :default-replacement-character ,replacement-character

         :read-char-fun        #',in-char-function
         :read-n-chars-fun     #',in-function

         :bytes-for-char-fun   #',size-function
         :write-n-bytes-fun    #',out-function
         ,@(mapcan (lambda (buffering)
                     (list (intern (format nil "WRITE-CHAR-~A-BUFFERED-FUN" buffering) :keyword)
                           `#',(intern (format nil format (string buffering)))))
                   '(:none :line :full))

         :resync-fun           #',resync-function

         :read-c-string-fun    #',read-c-string-function
         :write-c-string-fun   #',output-c-string-function

         :octets-to-string-fun (lambda (&rest rest)
                                 (declare (dynamic-extent rest))
                                 (apply ',octets-to-string-symbol rest))
         :string-to-octets-fun (lambda (&rest rest)
                                 (declare (dynamic-extent rest))
                                 (apply ',string-to-octets-symbol rest))))))

;;; NEWLINE-CODING

(defstruct (newline-coding
            (:constructor %make-newline-coding)
            (:conc-name nc-)
            (:copier nil))
  ;; All the names that can refer to this newline coding.  The first
  ;; one is the canonical name.
  (names             (missing-arg) :type list               :read-only t)
  ;;
  (newline-sequence  (missing-arg) :type string             :read-only t)
  ;;
  (read-newline-fun  nil           :type (or null function) :read-only t)
  (write-newline-fun nil           :type (or null function) :read-only t))

(declaim (ftype (sfunction (newline-coding) symbol) nc-name))
(defun nc-name (newline-coding)
  (first (nc-names newline-coding)))

(defmethod print-object ((object newline-coding) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (nc-name object) stream)))

(defun nc-single-character-p (newline-coding)
  (= (length (nc-newline-sequence newline-coding)) 1))

(defun nc-identity-p (newline-coding)
  (string= (string #\Newline) (nc-newline-sequence newline-coding)))

;;; All available newline codings. The table maps from newline coding
;;; names to NEWLINE-CODING instances.
(declaim (type hash-table **newline-codings**))
(define-load-time-global **newline-codings** (make-hash-table :test #'eq))

;; TODO should this be thread-safe?
(declaim (ftype (sfunction (keyword &key (:if-does-not-exist error-policy))
                           (or null newline-coding))
                find-newline-coding))
(defun find-newline-coding (designator &key if-does-not-exist)
  "Return the newline coding designated by DESIGNATOR.

IF-DOES-NOT-EXIST controls the behavior in case the requested newline
coding cannot be found:

NIL

  Return NIL if DESIGNATOR does not designate a newline coding.

ERROR

  TODO Signal an error.

Experimental."
  (or (gethash designator **newline-codings**)
      (etypecase if-does-not-exist
        (null
         if-does-not-exist)
        ((or function-name function)
         (funcall if-does-not-exist
                  "~@<Undefined newline-coding: ~S~@:>"
                  designator)))))

(declaim (ftype (sfunction (newline-coding keyword &key (:if-does-not-exist t))
                           newline-coding)
                (setf find-newline-coding)))
(defun (setf find-newline-coding) (new-value designator &key if-does-not-exist)
  "Install NEW-VALUE as the newline coding for the name DESIGNATOR.

IF-DOES-NOT-EXIST is accepted for parity with FIND-NEWLINE-CODING and
ignored.

Experimental."
  (declare (ignore if-does-not-exist))
  (setf (gethash designator **newline-codings**) new-value))

(defun register-newline-coding (names newline-coding)
  (dolist (name names)
    (setf (find-newline-coding name) newline-coding))
  newline-coding)

;; TODO document
(defmacro define-newline-coding (name-or-names
                                 &key
                                 (newline-sequence (missing-arg))
                                 (read-newline-fun (missing-arg))
                                 (write-newline-fun (missing-arg)))
  (let ((names (with-current-source-form (name-or-names)
                 (ensure-list-of-names "newline coding" name-or-names))))
    `(let ((names ',names))
       (register-newline-coding
        names (%make-newline-coding
               :names             names
               :newline-sequence  ,newline-sequence
               :read-newline-fun  ,read-newline-fun
               :write-newline-fun ,write-newline-fun))
       (first names))))


;;; EXTERNAL-FORMAT

(defstruct (external-format
            (:constructor %make-external-format)
            (:conc-name ef-)
            (:copier %copy-external-format))
  (character-coding             (missing-arg) :type character-coding   :read-only t)
  (newline-coding               (missing-arg) :type newline-coding     :read-only t)
  ;; Character sizes.
  (bytes-for-char-fun           (missing-arg) :type (or null function) :read-only t)
  ;; Reading characters from streams.
  (read-char-fun                (missing-arg) :type function           :read-only t)
  (read-n-chars-fun             (missing-arg) :type function           :read-only t)
  ;; Writing characters to streams.
  (write-n-bytes-fun            (missing-arg) :type function           :read-only t)
  (write-char-none-buffered-fun (missing-arg) :type function           :read-only t)
  (write-char-line-buffered-fun (missing-arg) :type function           :read-only t)
  (write-char-full-buffered-fun (missing-arg) :type function           :read-only t)
  ;; Reading/writing C strings from/to stream.
  (read-c-string-fun            (missing-arg) :type function           :read-only t)
  (write-c-string-fun           (missing-arg) :type function           :read-only t)
  ;; Conversion between strings and octet-vectors.
  (octets-to-string-fun         (missing-arg) :type function           :read-only t)
  (string-to-octets-fun         (missing-arg) :type function           :read-only t))
(declaim (freeze-type external-format))

(declaim (ftype (sfunction (external-format) cons) ef-names))
(defun ef-names (external-format)
  (cc-names (ef-character-coding external-format))) ; TODO newline-coding?

(defmethod print-object ((object external-format) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A ~A"
            (cc-name (ef-character-coding object))
            (nc-name (ef-newline-coding object)))))

(defun make-external-format (character-coding newline-coding)
  (flet ((maybe-wrap-bytes-for-char-fun (fun)
           (declare (type function fun))
           (let ((length (length (nc-newline-sequence newline-coding))))
             (cond ((= length 1)
                    fun)
                   (fun
                    (lambda (char)
                      (if (eql char #\Newline)
                          length
                          (funcall fun char))))
                   (t
                    (lambda (char)
                      (if (eql char #\Newline)
                          length
                          1))))))

         (maybe-wrap-read-char-fun (fun)
           (declare (type function fun))
           (let ((read-newline-fun (nc-read-newline-fun newline-coding)))
             (if read-newline-fun
                 (lambda (stream eof-error eof-value) ; TODO types
                   (ecase (funcall read-newline-fun stream eof-error)
                     ((t) #\Newline)
                     (:eof eof-value)
                     (:mismatch (funcall fun stream eof-error eof-value))))
                 fun)))

         (maybe-wrap-read-n-chars-fun (fun)
           (declare (type function fun))
           (let ((newline-sequence (nc-newline-sequence newline-coding)))
             (cond
               ((nc-identity-p newline-coding)
                fun)
               ((nc-single-character-p newline-coding)
                (let ((newline-character (aref newline-sequence 0)))
                  (lambda (stream buffer start requested eof-error-p)
                    (declare (type string buffer))
                    (let ((count (funcall fun stream buffer start requested eof-error-p)))
                      (nsubstitute #\Newline newline-character buffer
                                   :start start :end (+ start count))
                      count))))
               (t
                (let ((newline-sequence (coerce newline-sequence 'simple-base-string))) ; TODO necessary?
                  (lambda (stream buffer start requested eof-error-p)
                    (declare (type string buffer))
                    (let* ((count (funcall fun stream buffer start requested eof-error-p))
                           (end (+ start count)))
                      (declare (type index count))
                      (loop for previous = start then index
                            for index = (search newline-sequence buffer
                                                :start2 previous :end2 end)
                            while index
                            do (setf (aref buffer index) #\Newline
                                     (subseq buffer (+ index 1) (- end 1))
                                     (subseq buffer (+ index 2) end))
                               (incf index)
                               (decf count))
                      count)))))))

         (maybe-wrap-write-n-bytes-fun (fun)
           (declare (type function fun))
           (cond
             ((nc-identity-p newline-coding)
              fun)
             ((nc-single-character-p newline-coding)
              (let* ((newline-sequence (nc-newline-sequence newline-coding))
                     (newline-character (aref newline-sequence 0)))
                (lambda (stream buffer flush-p start end)
                  (declare (type string buffer))
                  (let ((buffer (substitute newline-character #\Newline buffer
                                            :start start :end end)))
                    (funcall fun stream buffer flush-p 0 (- end start))))))
             (t
              (let* ((newline-sequence (nc-newline-sequence newline-coding))
                     (newline-sequence-length (length newline-sequence)))
                (lambda (stream buffer flush-p start end)
                  (let ((first-newline (position #\Newline buffer
                                                 :start start :end end)))
                    (if first-newline
                        (let* ((newline-count (1+ (count #\Newline buffer
                                                         :start (1+ first-newline)
                                                         :end end)))
                               (new-length (+ (- end start newline-count)
                                              (* newline-sequence-length newline-count)))
                               (new-buffer (make-vector-like buffer new-length)))
                          (loop for previous = start then (1+ index)
                                for index = (position #\Newline buffer
                                                      :start previous :end end)
                                for previous* = 0 then index*
                                for index* = (when index
                                               (+ previous* (- index previous)))
                                while index
                                do (setf (subseq new-buffer previous* index*)
                                         (subseq buffer previous index)
                                         (subseq new-buffer index* (incf index* newline-sequence-length))
                                         newline-sequence)
                                finally (unless (eql previous end)
                                          (setf (subseq new-buffer previous*)
                                                (subseq buffer previous))))
                          (funcall fun stream new-buffer flush-p 0 new-length))
                        (funcall fun stream buffer flush-p start end))))))))

         (maybe-wrap-write-char-fun (fun)
           (declare (type function fun))
           (let ((write-newline-fun (nc-write-newline-fun newline-coding)))
             (if t ;; FIXME should only happen for non-NIL write-newline-fun, but
                   ;; WRITE-NEWLINE-FUN resets the stream column so we can't bypass it
                 (lambda (stream char)
                   (declare (type stream stream)
                            (type character char))
                   (if (char= char #\Newline)
                       (funcall write-newline-fun stream)
                       (funcall fun stream char)))
                 fun))))
    (%make-external-format
     :character-coding character-coding
     :newline-coding   newline-coding
     ;; Size
     :bytes-for-char-fun
     (maybe-wrap-bytes-for-char-fun
      (cc-bytes-for-char-fun character-coding))
     ;; Reading characters from streams
     :read-char-fun
     (maybe-wrap-read-char-fun
      (cc-read-char-fun character-coding))
     :read-n-chars-fun
     (maybe-wrap-read-n-chars-fun
      (cc-read-n-chars-fun character-coding))
     ;; Writing characters to streams
     :write-n-bytes-fun
     (maybe-wrap-write-n-bytes-fun
      (cc-write-n-bytes-fun character-coding))
     :write-char-none-buffered-fun
     (maybe-wrap-write-char-fun
      (cc-write-char-none-buffered-fun character-coding))
     :write-char-line-buffered-fun
     (maybe-wrap-write-char-fun
      (cc-write-char-line-buffered-fun character-coding))
     :write-char-full-buffered-fun
     (maybe-wrap-write-char-fun
      (cc-write-char-full-buffered-fun character-coding))
     ;; Conversion between strings and octet-vectors
     :octets-to-string-fun (cc-octets-to-string-fun character-coding)
     :string-to-octets-fun (cc-string-to-octets-fun character-coding)
     ;;
     :read-c-string-fun (cc-read-c-string-fun character-coding)
     :write-c-string-fun (cc-write-c-string-fun character-coding))))

;; TODO add replacements in external format wrappers
(defun make-external-format/maybe-add-replacements
    (character-coding newline-coding replacement)
  (make-external-format
   (if replacement
       (add-replacements-to-character-coding character-coding replacement)
       character-coding)
   newline-coding))

;; TODO get rid of this?
(macrolet ((frob (accessor)
             (let ((ef-name (symbolicate 'ef- accessor))
                   (cc-name (symbolicate 'cc- accessor)))
               `(defun ,ef-name (external-format)
                  (,cc-name (ef-character-coding external-format))))))
  (frob default-replacement-character)

  (frob resync-fun))

(defun variable-width-external-format-p (external-format)
  (and external-format
       (variable-width-character-coding-p
        (ef-character-coding external-format))))

(defun ef-char-size (ef-entry)
  (let ((character-coding (ef-character-coding ef-entry)))
    (cc-char-size character-coding)
    #+no (if (variable-width-external-format-p ef-entry)
        (lambda (char)
          (if (char= char #\Newline)
              ()
              (cc-bytes-for-char-fun ef-entry)))
        (funcall (bytes-for-char-fun character-coding) #\x))))

(defun bytes-for-char-fun (external-format) ; TODO is this used? shouldn't this use ef-bytes-for-char-fun?
  (if external-format
      (cc-bytes-for-char-fun (ef-character-coding external-format))
      (constantly 1)))

(defun sb-alien::string-to-c-string (string external-format)
  (declare (type simple-string string)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (find-external-format external-format))) ; TODO was get-external-format-OR-LOSE
      (funcall (ef-write-c-string-fun external-format) string))))

(defun sb-alien::c-string-to-string (sap external-format element-type)
  (declare (type system-area-pointer sap)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (find-external-format external-format))) ; TODO was get-external-format-OR-LOSE
      (funcall (ef-read-c-string-fun external-format) sap element-type))))

(defun external-format-keyword (spec)
  (typecase spec
    (keyword spec)
    ((cons keyword) (car spec))))

(defun canonize-external-format (spec external-format)
  (let ((name (cc-name (ef-character-coding external-format))))
    (typecase spec
      (keyword
       name)
      ((cons keyword) ; TODO normalize newline-coding name
       (cons name (rest spec))))))

(defun external-format-designator-to-key (designator
                                          &key
                                            (default-newline-coding :lf) ; TODO platform-specific
                                            default-replacement)
  (destructuring-bind (character-coding-name
                       &key
                       ((:newline-coding newline-coding-name) default-newline-coding)
                       (replacement default-replacement))
      (ensure-list designator)
    (list character-coding-name newline-coding-name replacement)))

;;; Keys are lists of the form
;;;
;;;   (CHARACTER-CODING-NAME NEWLINE-CODING-NAME REPLACEMENT)
;;;
;;; .
(declaim (type hash-table **external-format-cache**))
(define-load-time-global **external-format-cache**
    (make-hash-table :test #'equal))

(defun external-format-for-spec (spec)
  (destructuring-bind (character-coding-name newline-coding-name replacement)
      spec
    (flet ((missing (&rest args)
             (return-from external-format-for-spec args)))
      (let ((character-coding (find-character-coding
                               character-coding-name
                               :if-does-not-exist #'missing))
            (newline-coding (find-newline-coding
                             newline-coding-name
                             :if-does-not-exist #'missing)))
        (values (lambda ()
                  (make-external-format/maybe-add-replacements
                   character-coding newline-coding replacement))
                (list (cc-name character-coding)
                      (nc-name newline-coding)
                      replacement))))))

;;; Try to find the external format designated by SPEC (which is the
;;; result of calling EXTERNAL-FORMAT-DESIGNATOR-TO-KEY on a
;;; designator) in **EXTERNAL-FORMAT-CACHE**.
;;;
;;; If there is no matching entry
;;; 1. Make a suitable EXTERNAL-FORMAT instance or an error condition
;;;    by calling EXTERNAL-FORMAT-FOR-SPEC
;;; 2. Make a hash-table that is like the one stored in
;;;    **EXTERNAL-FORMAT-CACHE** but additionally contains the result
;;;    of 1.
;;; 3. Atomically install that table as the value of
;;;    **EXTERNAL-FORMAT-CACHE**
;;;
;;; Always return the EXTERNAL-FORMAT instance of error condition.
(defun %find-external-format (spec)
  (or (gethash spec **external-format-cache**)
      (binding* (((external-format canonical-spec)
                  (external-format-for-spec spec))
                 (external-format
                  (cond
                    ((gethash canonical-spec **external-format-cache**))
                    ((functionp external-format)
                     (funcall external-format))
                    (t
                     external-format))))
        (flet ((new-table (old-table)
                 (let* ((test (hash-table-test old-table))
                        (new-table (make-hash-table :test test)))
                   (maphash (lambda (key value)
                              (setf (gethash key new-table) value))
                            old-table)
                   (setf (gethash spec new-table) external-format)
                   new-table)))
          (loop for old-table = **external-format-cache**
             for new-table = (new-table old-table)
             until (eq old-table (cas **external-format-cache**
                                      old-table new-table))))
        external-format)))

;;; Defined in octets.lisp
(declaim (ftype (function (character-coding t) (values character-coding &optional))
                add-replacements-to-character-coding))

(declaim (ftype (sfunction (external-format-designator
                            &key (:if-does-not-exist error-policy))
                           (or null external-format))
                find-external-format))
(defun find-external-format (designator &key if-does-not-exist)
  "Return the external format designated by DESIGNATOR which can be of
one of the following forms

  KEYWORD ; TODO how about accepting strings and keywords here? that would make default external format detection less awkward

    Name of a character coding. TODO Newline coding

  (KEYWORD &key NEWLINE-CODING REPLACEMENT)

    Character coding named by KEYWORD

IF-DOES-NOT-EXIST controls the behavior in case the requested
character-coding or newline-coding cannot be found:

NIL

  Return NIL if SPEC does not designate an external format.

ERROR

  TODO Signal an error.

Experimental."
  (let* ((key (external-format-designator-to-key designator))
         (result (%find-external-format key)))
    (cond ((external-format-p result)
           result)
          ((typep if-does-not-exist '(and (not null) (or function-name function)))
           (apply if-does-not-exist result))
          (t
           if-does-not-exist))))


;;; Default external format

(defvar *default-external-format* nil)

#-win32
(defun unix-default-codeset ()
  (let ((code-set (or #-android
                      (alien-funcall
                       (extern-alien
                        "nl_langinfo"
                        (function (c-string :external-format :latin-1) int))
                       sb-unix:codeset)
                      "LATIN-1")))
    (intern code-set *keyword-package*)))

(defun default-external-format ()
  (or *default-external-format*
      ;; On non-unicode, use iso-8859-1 instead of detecting it from
      ;; the locale settings. Defaulting to an external-format which
      ;; can represent characters that the CHARACTER type can't
      ;; doesn't seem very sensible.
      #-sb-unicode
      (setf *default-external-format* :latin-1)
      (let ((external-format #-win32 (intern (or #-android
                                                 (alien-funcall
                                                  (extern-alien
                                                   "nl_langinfo"
                                                   (function (c-string :external-format :latin-1)
                                                             int))
                                                  sb-unix:codeset)
                                                 "LATIN-1")
                                             *keyword-package*)
                             #+win32 (sb-win32::ansi-codepage)))
        (/show0 "cold-printing defaulted external-format:")
        #+sb-show
        (cold-print external-format)
        (/show0 "matching to known aliases")
        (let ((entry (find-external-format external-format)))
          (cond
            (entry
             (/show0 "matched"))
            (t
             ;; FIXME! This WARN would try to do printing
             ;; before the streams have been initialized,
             ;; causing an infinite erroring loop. We should
             ;; either print it by calling to C, or delay the
             ;; warning until later. Since we're in freeze
             ;; right now, and the warning isn't really
             ;; essential, I'm doing what's least likely to
             ;; cause damage, and commenting it out. This
             ;; should be revisited after 0.9.17. -- JES,
             ;; 2006-09-21
             #+nil
             (warn "Invalid external-format ~A; using LATIN-1"
                   external-format)
             (setf external-format :latin-1))))
        (/show0 "/default external format ok")
        (setf *default-external-format* external-format))))


;;;; Backward compatibility

(defun get-external-format (spec)
  (find-external-format spec))

(defun get-external-format-or-lose (spec)
  "Like GET-EXTERNAL-FORMAT, but signal an error instead of returning
NIL."
  (or (get-external-format spec)
      (error "Undefined external-format: ~S" spec)))

(declaim (deprecated
          :early ("SBCL" "1.4.12")
          (function get-external-format :replacement find-external-format)
          (function get-external-format-or-lose :replacement find-external-format)))
