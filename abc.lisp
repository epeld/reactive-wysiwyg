
(defpackage :psx
  (:use :common-lisp))

(in-package :psx)

(defun attrs (sexp)
  "Extract the attributes part from an HTML-sexp"
  (let (attr-list body)
    (loop for rest on sexp by #'cddr
       if (keywordp (first rest))
       collect (cons (first rest) (second rest)) into attr
       else
       do (progn (setq attr-list attr)
		 (setq body rest)
		 (return)))
    
    (values attr-list body)))


(defun flatten-alist (alist)
  "Returns keys and values interspersed in a flat list"
  (let (list)
    (loop for item in alist
	 do (setf list (cons (car item) (cons (cdr item) list))))
    list))



(defmacro psx (body)
  "Compile html-like DSL using captured psx-element, psx-attrs, psx-children"
  (if (and (consp body) (typep (first body) 'keyword))
      (multiple-value-bind (attrs children) (attrs (rest body))
	 
	 `(psx-element ,(first body)
 
		       (psx-attrs ,@(flatten-alist attrs)) 
	     
		       ;; Compile all the children
		       (psx-children ,@(loop for child in children collect `(psx ,child)))))
      
      ;; Pass everything else through
      body))


(defmacro psx-to-who (psx)
  "Compile psx to cl-who syntax"
  `(macrolet ((psx-attrs (&body kvs)
		`(list ,@kvs))
	      
	      (psx-children (&body children)
		`(list ,@children))
	      
	      (psx-element (name attrs children)
		`(append (list ,name) ,attrs ,children)))
    
     (psx ,psx)))



(defmacro psx-to-html (psx)
  "Shortcut for converting psx directly to an HTML string"
  `(cl-who:with-html-output-to-string (s) 
     ,(eval `(psx-to-who ,psx))))


(psx-to-who (:div :class "abc" "Hello, World!" "."))


(psx-to-html (:div :class "abc" (:p "Hello, World!")
		   (:p "blub")
		   (:div (:span "erik"))))


