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


;;;; Linefeed/UNIX newline codings

;;; Input:  #x0d #x0a -> #\Return #\Newline
;;;         #x0a      -> #\Newline
;;;         #x0d      -> #\Return
;;; Output: #\Newline -> #x0a
;;;         #\Return  -> #X0d
(define-newline-coding (:lf :linefeed :unix)
  :newline-sequence (#x0a))

;;; Input:  #x0d #x0a -> #\Newline
;;;         #x0a      -> #\Newline
;;;         #x0d      -> #\Newline
;;; Output: #\Newline -> #x0a
;;;         #\Return  -> #x0d
(define-newline-coding (:auto/lf :auto/linefeed :auto/unix)
  :newline-sequence (#x0a))


;;;; CRLF/DOS/Windows newline codings

;;; Input:  #x0d #x0a -> #\Newline
;;;         #x0a      -> error?
;;;         #x0d      -> error?
;;; Output: #\Newline -> #x0d #x0a
;;;         #\Return  -> ?
(define-newline-coding (:crlf :dos :windows)
  :newline-sequence (#x0d #x0a))

;;; Input:  #x0d #x0a -> #\Newline
;;;         #x0a      -> #\Newline
;;;         #x0d      -> #\Newline
;;; Output: #\Newline -> #x0d #x0a
;;;         #\Return  -> ?
(define-newline-coding (:auto/crlf :auto/dos :auto/windows)
  :newline-sequence (#x0d #x0a))


;;;; Carriage Return/Classic Mac newline codings

;;; Input:  #x0d #x0a -> #\Newline #\Newline
;;;         #x0a      -> #\Newline
;;;         #x0d      -> #\Newline
;;; Output: #\Newline -> #x0d
;;;         #\Return  -> ?
(define-newline-coding (:cr :carrige-return :mac)
  :newline-sequence (#x0d))

;;; Input:  #x0d #x0a -> #\Newline
;;;         #x0a      -> #\Newline
;;;         #x0d      -> #\Newline
;;; Output: #\Newline -> #x0d
;;;         #\REturn  -> ?
(define-newline-coding (:auto/cr :auto/carrige-return :auto/mac)
  :newline-sequence (#x0d))
