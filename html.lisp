; This provides a DSL for writing XML and HTML tags built on Lisp macros 
; Copied from Land of Lisp
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

(defmacro tag (name atts &body body)
  `(concatenate 'string
               (print-tag ',name
                          (list ,@(mapcar (lambda (x)
                                            `(cons ',(car x) ,(cdr x)))
                                          (pairs atts)))
                          nil)
               ,@body
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
               (when closingp
                 "/")
               (string-downcase name)
               (mapc (lambda (att)
                       (format nil " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
                     alst)
               ">"))
