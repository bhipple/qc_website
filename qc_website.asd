;;;; qc_website.asd

(asdf:defsystem #:qc_website
  :description "Website for displaying git commits"
  :author "Benjamin Hipple <benjamin.hipple@gmail.com"
  :license "MIT"
  :serial t
  :depends-on (#:hunchentoot
               #:split-sequence)
  :components ((:file "package")
               (:file "src/shell")
               (:file "src/html")
               (:file "configs/config")
               (:file "src/server")
               (:file "qc_website")))

; vim:set ft=lisp:
