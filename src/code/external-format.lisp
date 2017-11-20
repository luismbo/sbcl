;;;; External formats

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defstruct (external-format
             (:constructor %make-external-format)
             (:conc-name ef-)
             (:predicate external-format-p)
             (:copier %copy-external-format))
  ;; All the names that can refer to this external format.  The first
  ;; one is the canonical name.
  (names (missing-arg) :type list :read-only t)
  (default-replacement-character (missing-arg) :type character)
  (read-n-chars-fun (missing-arg) :type function)
  (read-char-fun (missing-arg) :type function)
  (write-n-bytes-fun (missing-arg) :type function)
  (write-char-none-buffered-fun (missing-arg) :type function)
  (write-char-line-buffered-fun (missing-arg) :type function)
  (write-char-full-buffered-fun (missing-arg) :type function)
  ;; Can be nil for fixed-width formats.
  (resync-fun nil :type (or function null))
  (bytes-for-char-fun (missing-arg) :type function)
  (read-c-string-fun (missing-arg) :type function)
  (write-c-string-fun (missing-arg) :type function)
  ;; We indirect through symbols in these functions so that a
  ;; developer working on the octets code can easily redefine things
  ;; and use the new function definition without redefining the
  ;; external format as well.  The slots above don't do any
  ;; indirection because a developer working with those slots would be
  ;; redefining the external format anyway.
  (octets-to-string-fun (missing-arg) :type function)
  (string-to-octets-fun (missing-arg) :type function))

(defun ef-char-size (ef-entry)
  (if (variable-width-external-format-p ef-entry)
      (bytes-for-char-fun ef-entry)
      (funcall (bytes-for-char-fun ef-entry) #\x)))

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
  (let ((result (%copy-external-format external-format)))
    (macrolet ((frob (accessor)
                 `(setf (,accessor result) (funcall fun (,accessor result)))))
      (frob ef-read-n-chars-fun)
      (frob ef-read-char-fun)
      (frob ef-write-n-bytes-fun)
      (frob ef-write-char-none-buffered-fun)
      (frob ef-write-char-line-buffered-fun)
      (frob ef-write-char-full-buffered-fun)
      (frob ef-resync-fun)
      (frob ef-bytes-for-char-fun)
      (frob ef-read-c-string-fun)
      (frob ef-write-c-string-fun)
      (frob ef-octets-to-string-fun)
      (frob ef-string-to-octets-fun))
    result))

(define-load-time-global *external-formats* (make-hash-table)
  "Hashtable of all available external formats. The table maps from
  external-format names to EXTERNAL-FORMAT structures.")

(defun get-external-format-or-lose (external-format)
  (or (get-external-format external-format)
      (error "Undefined external-format: ~S" external-format)))

(defun external-format-keyword (external-format)
  (typecase external-format
    (keyword external-format)
    ((cons keyword) (car external-format))))

(defun canonize-external-format (external-format entry)
  (typecase external-format
    (keyword (first (ef-names entry)))
    ((cons keyword) (cons (first (ef-names entry)) (rest external-format)))))
