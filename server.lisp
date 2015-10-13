; Reads a file and returns the lines as a list
(defun get-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun print-line (line)
    (format nil "Line = ~a~&" line))

(defun print-lines (lines)
  (mapcar #'print-line lines))


;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(ql:quickload "hunchentoot")

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
                                 (setf (hunchentoot:content-type*) "text/plain")
                                 (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (tickets :uri "/t") ()
                                 (setf (hunchentoot:content-type*) "text/plain")
                                 (format nil "~A" (print-lines (get-file "tickets.csv"))))

;(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
;(hunchentoot:start acceptor)
