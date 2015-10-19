; Reads a file and returns the lines as a list

(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Hey~@[ ~A~]!" name))

(hunchentoot:define-easy-handler (tickets :uri "/t") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (handle-tickets))

(defun get-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun get-fname-content-pair (fname)
  (cons (file-namestring fname) (get-file fname)))

(defun get-csv-files (path)
  (directory (concatenate 'string path "*.csv")))

(defun print-lines (lines)
  (format nil "~{~a~&~}" lines))

(defun handle-tickets ()
  (let* ((filenames (get-csv-files "./"))
         (name-content-pairs (mapcar
                               #'get-fname-content-pair
                               filenames)))
    (loop for f in name-content-pairs
          collect (format nil "~a:~%~a~&~%" (car f) (print-lines (cdr f))))))

;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(ql:quickload "hunchentoot")

;(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
;(hunchentoot:start acceptor)
