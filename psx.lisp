
(defpackage :peldan.psx
  (:use :common-lisp)
  (:import-from :peldan.string :transpose)
  (:export :psx))

;(ql:quickload "cl-who")
(in-package :peldan.psx)


(defun extract-element-parts (sexp)
  "Extract the parts from an HTML-like sexp"
  (the list sexp)
  (let (attr-list body)
    (loop for rest on (rest sexp) by #'cddr
       if (keywordp (first rest))
       collect (cons (first rest) (second rest)) into attr
       else
       do (progn (setq attr-list attr)
		 (setq body rest)
		 (return)))
    
    (values (the atom (first sexp))
	    (the list attr-list) 
	    (the list body))))


(defun psx-element (sexp)
  (multiple-value-bind (first attrs children) (extract-element-parts sexp)

    `(create-reactive-element ,first (create ,@attrs) (list ,@children))))


(defun psx-atom (atom)
  "Compile an atom"
  (cond
    ((keywordp atom)
     (psx-element `(,atom)))
    
    (otherwise
     (string atom))))


(defun psx-list (sexp)
  (let ((first (first sexp)))
    (etypecase first
      (keyword
       (psx-element sexp))
      
      (string
       (psx-element sexp))
      
      ;; Leave logic be
      (symbol
       sexp))))


(defun psx-compile (sexp)
  "Compile html-like DSL but leaves logic as-is"
  (if (atom sexp)
    (psx-atom sexp)
    (psx-list sexp)))


(defmacro psx (sexp)
  ;; Smooth out the 'cl-who edges':
  `(macrolet ((component (component-expr &rest args)
		(if (and (list component-expr) 
			 (eq (first component-expr) 'function))
		    `(quote (create-reactive-element ,(second component-expr) ,@args))
		    `(quote (apply create-reactive-element component-expr ,@args))))
	     
	      (htm (html)
		`(psx ,html))
	     
	      (str (code)
		`(new (-string ,code))))
    
     ;; Compile the code, but this will leave htm, str and component-forms since
     ;; these look like logic. Hence the (recursive) macros above
     ,(psx-compile sexp)))


(defun make-renderer (lambda-list html)
  (let ((this-list (mapcar (lambda (var) 
			     `(ps:@ this ,var))
			   lambda-list)))
    `(lambda ()
       (ps:symbol-macrolet (,@(transpose (list lambda-list this-list)))
	 (psx ,html)))))


(defmacro defcomponent (name lambda-list html)
  `(quote 
    (defparameter ,name 
      (create-reactive-component 
       (create :render ,(make-renderer lambda-list html))))))


(defcomponent testA (a) (:div "test" a (component #'test1)))




