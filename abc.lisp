
(defpackage :psx
  (:use :common-lisp))

(in-package :psx)

(defun attrs (sexp)
  "Extract the attributes part from ab HTML-sexp"
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
  (let (list)
    (loop for item in alist
	 do (setf list (cons (car item) (cons (cdr item) list))))
    list))




(defmacro psx (body)
  "Anaphoric macro that captures psx-element and psx-attrs from its environment"
  (if (atom body)
      body
      (let ((first (first body)))
	(etypecase first
      
	  ;; keyword means builtin DOM element
	  (keyword
	   (multiple-value-bind (attrs children) (attrs (rest body))
	 
	     ;; create-element will look for an installed component,
	     ;; or use a builtin one (if keyword!)
	     `(psx-element ,first (psx-attrs ,@(flatten-alist attrs)) 
	     
			   ;; Compile all the children
			   ,@(loop for child in children collect `(psx ,child)))))
	
	  ;; Symbol means 'call this function'
	  (symbol
	   ;; Try just passing the form through
	   body)))))




(defmacro psx-to-who (psx)
  `(macrolet ((psx-attrs (&body body)
		`(quote ,body))
	      
	      (psx-element (name attrs &rest children)
		`(append (list ,name) ,attrs (list ,@children))))
    
     (psx ,psx)))


(defmacro who-to-html (who)
  `(cl-who:with-html-output-to-string (s)
     ,who))


(defmacro psx-to-html (psx)
  `(who-to-html ,(eval `(psx-to-who ,psx))))

(psx-to-who (:div :class "abc" "Hello, World!"))
(psx-to-html (:div :class "abc" (:p "Hello, World!")
		   (:p "blub")
		   (:div (:span "erik"))))
(psx-to-html (mytest2 2 "AA"))


