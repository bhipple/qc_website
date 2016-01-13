
(require "configs" "configs/config.lisp")
(require "shell" "shell.lisp")
(require "html" "html.lisp")

(eval-when (:compile-toplevel)
    (ql:quickload :split-sequence)
    (ql:quickload :hunchentoot))

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
    (sh (concatenate 'string "mv " fullName " " *archive-location* archiveName))))

(defun check-for-new-archive (path)
  (let ((archive (directory (concatenate 'string path "archive*.tar.gz"))))
    (if archive (extract-archive path (file-namestring (car archive))))))

;; ============================================================================
;;                                Formatting
;; ============================================================================
; Each line is a commit in QC for the task
(defun format-line (line task)
  (let* ((parts (split-sequence:split-sequence #\| line))
         (github-link (concatenate 'string *github* task))
         (author (car parts))
         (sha (cadr parts))
         (date (caddr parts))
         (msg (cadddr parts))
         (commit-link (concatenate 'string *github* task "/commit/" sha)))
    (concatenate 'string
                 (tag td () (tag a (href github-link) task))
                 (tag td () author)
                 (tag td () date)
                 (tag td () (tag a (href commit-link) msg)))))

; Formats all lines for a task into a table row
(defun format-lines (lines task)
  (apply #'concatenate 'string
         (mapcar (lambda (line)
                   (tag tr ()  (format-line line task))) lines)))

; Generates the table data for one task
(defun task-status (fname)
  (let* ((name-content-pair (get-fname-content-pair fname))
         (task (car name-content-pair))
         (content (cdr name-content-pair)))
    (format-lines content task)))

(defun get-header ()
  (tag p ()
    (display-images)
    (tag h1 ()
      (tag u () "Commits in QC"))
    (tag h2 () "(On Beta but not Production)")
    (tag hr ())))

(defun display-images ()
  (concatenate 'string
               (tag img (src "img/lisplogo_fancy_256.png"))
               (tag img (src "img/lisplogo_warning_256.png"))))

(defun define-table (descriptions)
  (tag table (border "1" cellpadding "3" cellspacing "1")
    (tag tr ()
      (tag th () "Task")
      (tag th () "Author")
      (tag th () "Date")
      (tag th () "Commit"))
    (format nil "~{~a~}" descriptions)))

(defun handle-tickets ()
  (check-for-new-archive *commit-location*)
  (let* ((filenames (get-txt-files *commit-location*))
         (descriptions (mapcar #'task-status filenames)))
    (html (tag body ()
            (get-header)
            (define-table descriptions)
            (tag img (src "img/lisplogo_flag2_256.png"))))))

;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(hunchentoot:define-easy-handler (tickets :uri "/t") ()
  (setf (hunchentoot:content-type*) "html")
  (handle-tickets))

(defun main ()
  (defparameter acceptor (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start acceptor))
