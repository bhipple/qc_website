#+sbcl
(provide "shell")

(defun sh (x)
  (run-program "/bin/sh" (list "-c" x) :output t))
