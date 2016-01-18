; This provides a DSL for writing XML and HTML tags built on Lisp macros 
; Copied from Land of Lisp
(provide "html")

;; ============================================================================
;;                                  Macros
;; ============================================================================
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
         (let ((head (car ,g))
               (tail (cdr ,g)))
           ,yes)
         ,no))))

; Generates an html tag and returns it as a string
(defmacro tag (name atts &body body)
  `(concatenate 'string
                (print-tag ',name
                           (list ,@(mapcar (lambda (x)
                                             `(cons (string ',(car x)) ,(cdr x)))
                                           (pairs atts)))
                           nil)
                (concatenate 'string "" ,@body)
                (fresh-line)
                (print-tag ',name nil t)))

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

;; ============================================================================
;;                             Helper Functions
;; ============================================================================
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                      (f (cdr tail) (cons (cons head (car tail)) acc))
                      (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(defun print-tag (name alst closingp)
  (concatenate 'string
               "<"
               (if closingp "/")
               (string-downcase name)
               (apply #'concatenate 'string (mapcar (lambda (att)
                       (format nil " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
                     alst))
               ">"))

;; ============================================================================
;;                            Content Header
;; ============================================================================
; This is used for the sorttable
(defun head-section ()
  (tag head ()
    "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">"
    (tag script (type "text/javascript" src "lightfilter.js"))
    ; Generated with tablestyler.com
    (tag style (type "text/css")
      ".datagrid table { border-collapse: collapse; text-align: left; width: 100%; } .datagrid {font: normal 12px/150% Geneva, Arial, Helvetica, sans-serif; background: #fff; overflow: hidden; border: 1px solid #006699; -webkit-border-radius: 3px; -moz-border-radius: 3px; border-radius: 3px; }.datagrid table td, .datagrid table th { padding: 3px 10px; }.datagrid table thead th {background:-webkit-gradient( linear, left top, left bottom, color-stop(0.05, #006699), color-stop(1, #00557F) );background:-moz-linear-gradient( center top, #006699 5%, #00557F 100% );filter:progid:DXImageTransform.Microsoft.gradient(startColorstr='#006699', endColorstr='#00557F');background-color:#006699; color:#FFFFFF; font-size: 21px; font-weight: bold; border-left: 1px solid #0070A8; } .datagrid table thead th:first-child { border: none; }.datagrid table tbody td { color: #00557F; border-left: 1px solid #E1EEF4;font-size: 17px;font-weight: normal; }.datagrid table tbody .alt td { background: #E1EEf4; color: #00557F; }.datagrid table tbody td:first-child { border-left: none; }.datagrid table tbody tr:last-child td { border-bottom: none; }"
      )))
