;; ============================================================================
;;                               File Handling
;; ============================================================================
(defun get-file-lines (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun get-fname-content-pair (fname)
  (let ((str (file-namestring fname)))
    (cons (subseq str 0 (- (length str) 4)) (get-file-lines fname))))

(defun get-txt-files (path)
  (directory (concatenate 'string path "*.txt")))

;; ============================================================================
;;                                Formatting
;; ============================================================================
(defun format-lines (lines)
  (format nil "~{~a<br>~}" lines))

(defun description (fname)
  (let ((name-content-pair (get-fname-content-pair fname)))
    (format nil "<h3>~a:</h3><p>~a</p>"
            (car name-content-pair)
            (format-lines (cdr name-content-pair)))))

(defun get-header ()
  (format nil "<h1>Scraping Commits in QC</h1><h2>(On SCIQ but not SCIP)</h2>"))

(defun handle-tickets ()
  (let* ((filenames (get-txt-files "./"))
         (descriptions (mapcar #'description filenames)))
    (format nil "~a~{~a~}" (get-header) descriptions)))

;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(ql:quickload "hunchentoot")

(hunchentoot:define-easy-handler (tickets :uri "/t") ()
  (setf (hunchentoot:content-type*) "html")
  (handle-tickets))

;(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
;(hunchentoot:start acceptor)
