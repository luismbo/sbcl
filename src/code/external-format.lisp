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

(defun wrap-character-coding-functions (character-coding fun)
  (declare (ignore fun))
  (let ((result (%copy-character-coding character-coding)))
    (macrolet ((frob (accessor)
                 `(setf (,accessor result) (funcall fun (,accessor result)))))
      #+later (frob ef-read-n-chars-fun)
      #+later (frob ef-read-char-fun)
      #+no (frob ef-write-n-bytes-fun)
      #+later (frob ef-write-char-none-buffered-fun)
      #+later (frob ef-write-char-line-buffered-fun)
      #+later (frob ef-write-char-full-buffered-fun)
      #+no (frob ef-resync-fun)
      #+no (frob ef-bytes-for-char-fun)
      #+no (frob ef-read-c-string-fun)
      #+no (frob ef-write-c-string-fun)
      #+no (frob ef-octets-to-string-fun)
      #+no (frob ef-string-to-octets-fun))
    result))

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
                  (ensure-list-of-names "character coding" name-or-names)))
         (canonical-name (first names)))
    `(let ((character-coding (%make-character-coding :names ',names ,@args)))
       ,@(loop for name in names
            collect `(setf (find-character-coding ,name) character-coding))
       ,canonical-name)))

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
                                            ,names
                                            (:none character)
                                            (:line character)
                                            (:full character))
         (if (eql byte #\Newline)
             (setf (fd-stream-output-column stream) 0)
             (setf (fd-stream-output-column stream)
                   (+ (truly-the unsigned-byte (fd-stream-output-column stream)) 1)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-newline-reader (octet-sequence) ; TODO just put into macro
    (flet ((make-unread (amount)
             (unless (zerop amount)
               `((decf (buffer-head (fd-stream-ibuf stream)) ,amount)) ; TODO are these fd-streams?
               )))
      `(block nil
         ,@(loop :for position :from 0
              :for octet :in octet-sequence :collect
              `(let ((byte (input-unsigned-8bit-byte stream eof-error :eof)))
                 (cond
                   ((eql byte :eof)
                    ,@(make-unread position)
                    (return :eof))
                   ((not (eql byte ,octet))
                    ,@(make-unread position)
                    (return :mismatch)))))
         t))))

;; TODO document
(defmacro define-newline-coding (name-or-names &key newline-sequence)
  (let ((names (with-current-source-form (name-or-names)
                 (ensure-list-of-names "newline coding" name-or-names)))
        (trivialp (equal newline-sequence '(#x0a))))
    (flet ((make-read-newline-fun ()
             `(lambda (stream eof-error)
                (declare (type stream stream)) ; TODO fd-stream?
                ,(make-newline-reader newline-sequence)))
           (make-write-newline-fun ()
             `(lambda (write-byte-fun stream)
                (declare (type function write-byte-fun)
                         (type stream stream)) ; TODO fd-stream?
                (setf (fd-stream-output-column stream) 0)
                ,@(mapcar (lambda (byte) ; TODO write whole sequence at once
                            `(funcall write-byte-fun stream ,byte))
                          newline-sequence))))
      `(let ((newline-coding
              (%make-newline-coding
               :names             ',names
               :newline-sequence  (map 'string #'code-char ',newline-sequence) ; TODO unused?
               :read-newline-fun  ,(unless trivialp
                                     (make-read-newline-fun))
               :write-newline-fun ,(unless trivialp
                                     (make-write-newline-fun)))))
         ,@(loop for name in names
              collect `(setf (find-newline-coding ,name) newline-coding))
         newline-coding))))

(define-newline-coding :unix
  :newline-sequence (#x0a))

(define-newline-coding :mac
  :newline-sequence (#x0d))

(define-newline-coding (:dos :windows)
  :newline-sequence (#x0d #x0a))


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
  (write-char-none-buffered-fun (missing-arg) :type function           :read-only t)
  (write-char-line-buffered-fun (missing-arg) :type function           :read-only t)
  (write-char-full-buffered-fun (missing-arg) :type function           :read-only t))

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
             (cond
               ((= length 1)
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
           (let ((read-newline-fun (nc-read-newline-fun newline-coding)))
             (if read-newline-fun
                 (lambda (stream eof-error eof-value) ; TODO types
                   (funcall read-newline-fun fun stream eof-error eof-value))
                 fun)))

         (maybe-wrap-write-char-fun (fun)
           (declare (type function fun))
           (let ((write-newline-fun (nc-write-newline-fun newline-coding)))
             (if write-newline-fun
                 (lambda (stream char)
                   (declare (type stream stream)
                            (type character char))
                   (cond
                     ((char= char #\Newline)
                      (funcall write-newline-fun stream))
                     (t
                      (funcall fun stream char))))
                 fun))))
    (%make-external-format
     :character-coding character-coding
     :newline-coding   newline-coding
     ;;
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
     :write-char-none-buffered-fun
     (maybe-wrap-write-char-fun
      (cc-write-char-none-buffered-fun character-coding))
     :write-char-line-buffered-fun
     (maybe-wrap-write-char-fun
      (cc-write-char-line-buffered-fun character-coding))
     :write-char-full-buffered-fun
     (maybe-wrap-write-char-fun
      (cc-write-char-full-buffered-fun character-coding)))))

(defun make-external-format/maybe-handlerify
    (character-coding newline-coding replacement)
  (flet ((replacement-handlerify (character-coding replacement)
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
                        #+TODO-was (lambda (c) (use-value replacement c))
                        (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'use-value replacement)))
                       (octet-decoding-error
                        #+TODO-was (lambda (c) (use-value replacement c))
                        (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'use-value replacement))))
                    (apply function rest))))))))
    (make-external-format
     (if replacement
         (replacement-handlerify character-coding replacement)
         character-coding)
     newline-coding)))

(macrolet ((frob (accessor)
             (let ((ef-name (symbolicate 'ef- accessor))
                   (cc-name (symbolicate 'cc- accessor)))
               `(defun ,ef-name (external-format)
                  (,cc-name (ef-character-coding external-format))))))
  (frob default-replacement-character)

  (frob write-n-bytes-fun)

  (frob resync-fun)

  (frob read-c-string-fun)
  (frob write-c-string-fun)

  (frob octets-to-string-fun)
  (frob string-to-octets-fun))

(defun variable-width-external-format-p (external-format)
  (and external-format
       (variable-width-char-coding-p (ef-character-coding external-format))))

(defun ef-char-size (ef-entry)
  (let ((character-coding (ef-character-coding ef-entry)))
    (cc-char-size character-coding)
    #+no (if (variable-width-external-format-p ef-entry)
        (lambda (char)
          (if (char= char #\Newline)
              ()
              (cc-bytes-for-char-fun ef-entry)))
        (funcall (bytes-for-char-fun character-coding) #\x))))

(defun bytes-for-char-fun (external-format)
  (if external-format
      (cc-bytes-for-char-fun (ef-character-coding external-format))
      (constantly 1)))

(defun sb-alien::string-to-c-string (string external-format)
  (declare (type simple-string string)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (get-external-format-or-lose external-format)))
      (funcall (ef-write-c-string-fun external-format) string))))

(defun sb-alien::c-string-to-string (sap external-format element-type)
  (declare (type system-area-pointer sap)
           (explicit-check :result))
  (locally
      (declare (optimize (speed 3) (safety 0)))
    (let ((external-format (get-external-format-or-lose external-format)))
      (funcall (ef-read-c-string-fun external-format) sap element-type))))

(defun wrap-external-format-functions (external-format fun)
  (declare (ignore fun))
  (let ((result (%copy-external-format external-format)))
    (macrolet ((frob (accessor)
                 `(setf (,accessor result) (funcall fun (,accessor result)))))
      #+later (frob ef-read-n-chars-fun)
      #+later (frob ef-read-char-fun)
      #+no (frob ef-write-n-bytes-fun)
      #+later (frob ef-write-char-none-buffered-fun)
      #+later (frob ef-write-char-line-buffered-fun)
      #+later (frob ef-write-char-full-buffered-fun)
      #+no (frob ef-resync-fun)
      #+no (frob ef-bytes-for-char-fun)
      #+no (frob ef-read-c-string-fun)
      #+no (frob ef-write-c-string-fun)
      #+no (frob ef-octets-to-string-fun)
      #+no (frob ef-string-to-octets-fun))
    result))

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

(defun find-external-format (spec &key if-does-not-exist)
  "Return the EXTERNAL-FORMAT structure designated by SPEC which can
be of one of the following forms

  KEYWORD

    Name of a character coding. TODO Newline coding

  (KEYWORD &key NEWLINE-CODING REPLACEMENT)

    Character coding named by KEYWORD

IF-DOES-NOT-EXIST controls the behavior in case the requested
character-coding or newline-coding cannot be found:

NIL

  Return NIL if SPEC does not designate an external format.

ERROR

  TODO Signal an error."
  (declare (type (or null function-name function) if-does-not-exist))
  (destructuring-bind (character-coding-name
                       &key
                       ((:newline-coding newline-coding-name) :unix)
                       replacement)
      (ensure-list spec)
    (let ((character-coding (find-character-coding
                             character-coding-name
                             :if-does-not-exist if-does-not-exist))
          (newline-coding (find-newline-coding
                           newline-coding-name
                           :if-does-not-exist if-does-not-exist)))
      (when (and character-coding newline-coding)
        (make-external-format/maybe-handlerify
         character-coding newline-coding replacement)))))


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
