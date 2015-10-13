(ql:quickload "hunchentoot")

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
                                 (setf (hunchentoot:content-type*) "text/plain")
                                 (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (arbitrary :uri "/t") ()
                                 (setf (hunchentoot:content-type*) "text/plain")
                                 (princ "Hey Ben"))

; Reads a file and returns the lines as a list
(defun get-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start acceptor)
