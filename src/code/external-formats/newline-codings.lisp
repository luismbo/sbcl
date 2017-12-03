;;;; Coding of #\Newline characters as octet sequences

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

;;; TODO terminology:
;;; newline vs. eol vs. line-break


;;;; Decoding newlines in string buffers

(defun make-single-character-newline-decoder (newline-character)
  (lambda (buffer start end count)
    (declare (ignore count))
    (nsubstitute #\Newline newline-character buffer :start start :end end)))

;;; Replace (in-place) occurrences of the newline sequence with
;;; #\Newline, shrinking the buffer in the process by decreasing the
;;; upper boundary.
(defun make-multi-character-newline-decoder (newline-sequence)
  (lambda (buffer start end count) ; TODO not sure what the interface should be here
    (declare (ignore end))
    (let* ((end (+ start count)))
      (declare (type index count))
      (loop for previous = start then index
            for index = (search newline-sequence buffer
                                :start2 previous :end2 end)
            while index do
              (setf (aref buffer index)
                    #\Newline
                    (subseq buffer (+ index 1) (- end 1))
                    (subseq buffer (+ index 2) end))
              (incf index)
              (decf count))
      count)))

(defun auto-newline-decoder (buffer start end count)
  (declare (ignore end))
  (let* ((end (+ start count)))
    (declare (type index count))
    (loop for index from start below end
          for previous = nil then current
          for current = (aref buffer index)
          do (cond
               ((not #+TODO (eql previous #\Return) (and previous (= (char-code previous) #x10))))
               ((char= current #\Newline)
                (setf (aref buffer (1- index))
                      #\Newline
                      (subseq buffer (+ index 0) (- end 1))
                      (subseq buffer (+ index 1) end))
                (decf count))
               (t
                (setf (aref buffer (1- index)) #\Newline))))
    count))


;;;; Encoding newlines in string buffers

(defun make-single-character-newline-encoder (newline-character)
  (lambda (buffer start end)
    (let ((first-newline (position #\Newline buffer :start start :end end)))
      (if first-newline
          (values (substitute newline-character #\Newline buffer
                              :start start :end end)
                  0 (- start end))
          (values buffer start end)))))

;;; In case the buffer contains any #\Newline characters, compute the
;;; size required to expand all of them and prepare a buffer of that
;;; size.
(defun make-multi-character-newline-encoder (newline-sequence)
  (let ((newline-sequence-length (length newline-sequence)))
    (lambda (buffer start end)
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
                    for index = first-newline
                              then (position #\Newline buffer
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
              (values new-buffer 0 new-length))
            (values buffer start end))))))


;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-newline-reader (octet-sequence) ; TODO just put into macro
    (flet ((make-unread (amount)
             (unless (zerop amount)
               `((decf (buffer-head (fd-stream-ibuf stream)) ,amount)) ; TODO are these fd-streams?
               )))
      `(block nil
         ,@(loop for position from 0
                 for octet in octet-sequence
                 collect `(let ((byte (input-unsigned-8bit-byte
                                       stream eof-error :eof)))
                            (cond
                              ((eql byte :eof)
                               ,@(make-unread position)
                               (return :eof))
                              ((not (eql byte ,octet))
                               ,@(make-unread position)
                               (return :mismatch)))))
         t))))

(defmacro define-newline-coding/newline-sequence (name-or-names
                                                  &key
                                                  (newline-sequence (missing-arg))
                                                    decode-in-string-fun)
  (declare (ignore decode-in-string-fun))
  (let ((trivialp (equal newline-sequence '(#x0a))))
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
      `(define-newline-coding
         ,name-or-names
         :newline-sequence  (map 'string #'code-char ',newline-sequence) ; TODO unused?
         :read-newline-fun  ,(unless trivialp
                               (make-read-newline-fun))
         :write-newline-fun ,(unless trivialp
                               (make-write-newline-fun))))))


;;;; Linefeed/UNIX newline codings

;;; octets -> string:
;;;   #x0d #x0a -> #\Return #\Newline
;;;   #x0a      -> #\Newline
;;;   #x0d      -> #\Return
;;; string -> octets:
;;;   #\Newline -> #x0a
;;;   #\Return  -> #X0d
(define-newline-coding/newline-sequence (:lf :linefeed :unix)
    :newline-sequence (#x0a))

;;; octets -> string:
;;;   #x0d #x0a -> #\Newline
;;;   #x0a      -> #\Newline
;;;   #x0d      -> #\Newline
;;; string -> octets:
;;;   #\Newline -> #x0a
;;;   #\Return  -> #x0d
(define-newline-coding/newline-sequence (:auto/lf :auto/linefeed :auto/unix)
  :newline-sequence     (#x0a)
  :decode-in-string-fun #'auto-newline-decoder)


;;;; CRLF/DOS/Windows newline codings

;;; octets -> string:
;;;   #x0d #x0a -> #\Newline
;;;   #x0a      -> #\Newline
;;;   #x0d      -> #\Return
;;; string -> octets:
;;;   #\Newline -> #x0d #x0a
;;;   #\Return  -> #x0d
(define-newline-coding/newline-sequence (:crlf :dos :windows)
  :newline-sequence (#x0d #x0a))

;;; octets -> string:
;;;   #x0d #x0a -> #\Newline
;;;   #x0a      -> #\Newline
;;;   #x0d      -> #\Newline
;;; string -> octets:
;;;   #\Newline -> #x0d #x0a
;;;   #\Return  -> #x0d
(define-newline-coding/newline-sequence (:auto/crlf :auto/dos :auto/windows)
  :newline-sequence     (#x0d #x0a)
  :decode-in-string-fun #'auto-newline-decoder)


;;;; Carriage Return/Classic Mac newline codings

;;; octets -> string:
;;;   #x0d #x0a -> #\Newline #\Newline
;;;   #x0a      -> #\Newline
;;;   #x0d      -> #\Newline
;;; string -> octets:
;;;   #\Newline -> #x0d
;;;   #\Return  -> #x0d
(define-newline-coding/newline-sequence (:cr :carrige-return)
  :newline-sequence (#x0d))

;;; octets -> string:
;;;   #x0d #x0a -> #\Newline
;;;   #x0a      -> #\Newline
;;;   #x0d      -> #\Newline
;;; string -> octets:
;;;   #\Newline -> #x0d
;;;   #\Return  -> #x0d
(define-newline-coding/newline-sequence (:auto/cr :auto/carrige-return)
  :newline-sequence     (#x0d)
  :decode-in-string-fun #'auto-newline-decoder)
