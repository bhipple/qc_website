;;;; server.lisp
(in-package #:qc_website)

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
    (sh (concatenate 'string "rm -f " *commit-location* "*.txt"))
    (sh (concatenate 'string "tar -C " *commit-location* " -xvf " fullName))
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
         (age (cadddr parts))
         (msg (car (cddddr parts)))
         (commit-link (concatenate 'string *github* task "/commit/" sha)))
    (concatenate 'string
                 (tag td () (tag a (href github-link) task))
                 (tag td () author)
                 (tag td () date)
                 (tag td () age)
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
    (tag h1 ()
      (tag u () "Commits in QC"))
    (tag h2 () "(On Beta but not Production)")))

(defun display-images ()
  (concatenate 'string
               "<br>"
               (tag img (src "img/lisplogo_fancy_256.png"))
               (tag img (src "img/lisplogo_flag2_256.png"))
               (tag img (src "img/lisplogo_warning_256.png"))))

(defun create-table (descriptions)
  (tag div (class "datagrid")
    (tag table (class "order-table table")
    (tag tr ()
      (tag th () "Task")
      (tag th () "Author")
      (tag th () "Date")
      (tag th () "Age")
      (tag th () "Commit"))
    (format nil "~{~a~}" descriptions))))

(defun handle-tickets ()
  (check-for-new-archive "./")
  (let* ((filenames (get-txt-files *commit-location*))
         (descriptions (mapcar #'task-status filenames)))
    (html
      (head-section)
      (tag body ()
        (get-header)
        (tag section (class "container")
          "<input type=\"search\" class=\"light-table-filter\" data-table=\"order-table\" placeholder=\"Filter\">"
          (create-table descriptions))
        (display-images)))))

;; ============================================================================
;;                           Hunchentoot Handlers
;; ============================================================================
(hunchentoot:define-easy-handler (tickets :uri "/t") ()
  (setf (hunchentoot:content-type*) "html")
  (handle-tickets))

(defun start-on-port (port)
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port port
                                    :address "localhost")))

(defun main (&rest args)
  (declare (ignore args))
  (start)
  (setf swank::*loopback-interface* "0.0.0.0")
  (swank-loader:init)
  (swank:create-server :port 4005
                       :style swank:*communication-style*
                       :dont-close t))
