#+sbcl
;;;; shell.lisp

(in-package #:qc_website)

(defun sh (x)
  (sb-ext:run-program "/bin/sh" (list "-c" x) :output t))
