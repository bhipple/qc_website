(load "shell.lisp")

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

(defun extract-archive (path archiveName)
  (let ((fullName (concatenate 'string path archiveName)))
    (princ "Extracting: ")
    (princ archiveName)
    (sh (concatenate 'string "tar -xf " fullName))
    (print "Moving to archives.")
    (sh (concatenate 'string "mv " fullName " /home/ubuntu/scrp_qc_website/archives/" archiveName))))

(defun check-for-new-archive (path)
  (let ((archive (directory (concatenate 'string path "archive*.tar.gz"))))
    (if archive (extract-archive path (file-namestring (car archive))))))

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
  (format nil "<p>~a<h1><u>Scraping Commits in QC</u></h1><h2>(On SCIQ but not SCIP)</h2></p><hr>" (display-images)))

(defun wrap-img (img)
  (let ((path "img/"))
    (concatenate 'string "<img src=\"" path img "\"></img>")))

(defun display-images ()
  (concatenate 'string (wrap-img "lisplogo_fancy_256.png") (wrap-img "lisplogo_warning_256.png")))

(defun handle-tickets ()
  (let* ((filenames (get-txt-files "./"))
         (descriptions (mapcar #'description filenames)))
    (format nil "<html><body>~a~{~a~}<hr>~a</body></html>" (get-header) descriptions (display-images))))

;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(ql:quickload "hunchentoot")

(hunchentoot:define-easy-handler (tickets :uri "/t") ()
  (setf (hunchentoot:content-type*) "html")
  (handle-tickets))

;(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
;(hunchentoot:start acceptor)
