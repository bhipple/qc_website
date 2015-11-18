; For some reason, none of these things load automatically in the REPL
(load "shell.lisp")
(load "html.lisp")
(ql:quickload "split-sequence")
(ql:quickload "hunchentoot")

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
    (sh "rm -f *.txt")
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
(defparameter bbgithub "https://bbgithub.dev.bloomberg.com/scrp/")

; Each line is a commit in QC for the task
(defun format-line (line task)
  (let* ((parts (split-sequence:split-sequence #\| line))
         (author (car parts))
         (sha (cadr parts))
         (date (caddr parts))
         (msg (cadddr parts))
         (commit-link (concatenate 'string bbgithub task "/commit/" sha)))
    (concatenate 'string
                 (tag td () date)
                 (tag td () author)
                 (tag td () (tag a (href commit-link) msg)))))

; Formats all lines for a task into a table
(defun format-lines (lines task)
  (apply #'concatenate 'string (mapcar (lambda (line) (tag tr ()  (format-line line task))) lines)))

; Generates the table data for one task
(defun task-status (fname)
  (let* ((name-content-pair (get-fname-content-pair fname))
         (task (car name-content-pair))
         (content (cdr name-content-pair))
         (bbgh-task-link (concatenate 'string bbgithub task))
         (formatted-lines (format-lines content task)))
    (concatenate 'string
                 (tag h3 () (tag a (href bbgh-task-link) task ":<br>"))
                 (tag table (border "1" cellpadding "3" cellspacing "1")
                   (tag tr ()
                     (tag th () "Date")
                     (tag th () "Author")
                     (tag th () "Commit"))
                     formatted-lines))))

(defun get-header ()
  (tag p ()
    (display-images)
    (tag h1 ()
      (tag u () "Scraping Commits in QC"))
    (tag h2 () "(On SCIQ but not SCIP)")
    (tag hr ())))

(defun display-images ()
  (concatenate 'string
               (tag img (src "img/lisplogo_fancy_256.png"))
               (tag img (src "img/lisplogo_warning_256.png"))))

(defun handle-tickets ()
  (check-for-new-archive "/home/ubuntu/")
  (let* ((filenames (get-txt-files "./"))
         (descriptions (mapcar #'task-status filenames)))
    (html (tag body ()
            (get-header)

            (format nil "~{~a~}" descriptions)
            (tag img (src "img/lisplogo_flag2_256.png"))))))

;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(hunchentoot:define-easy-handler (tickets :uri "/t") ()
  (setf (hunchentoot:content-type*) "html")
  (handle-tickets))

;(defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
;(hunchentoot:start acceptor)
