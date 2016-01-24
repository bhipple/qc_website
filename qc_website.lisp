;;;; qc_website.lisp

(in-package #:qc_website)

(defun start ()
  (start-on-port 4242)
  (start-on-port 80))
