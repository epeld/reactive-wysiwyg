
(defpackage :peldan.psx
  (:use :common-lisp)
  (:import-from :peldan.string :transpose)
  (:import-from :peldan.alist :interleaved-alist :flatten-alist)
  (:export :psx))

(in-package :peldan.psx)


;; 
;; This file contains lisp-code for compiling cl-who code to ReactJS
;; 

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

(defun psx-element (head attrs children)
  `(create-reactive-element ,head
			    (ps:create ,@(flatten-alist attrs))
			    (ps:list ,@children)))


(defun psx-element-sexp (sexp)
  (multiple-value-bind (first attrs children) (extract-element-parts sexp)
    
    ;; Expand all the children before continuing!
    (let ((children (mapcar #'psx-compile children)))
      (psx-element first attrs children))))


(defun psx-atom (atom)
  "Compile an atom"
  (cond
    ((keywordp atom)
     (psx-element-sexp `(,atom)))
    
    (t
     (string atom))))


(defun psx-list (sexp)
  (let ((first (first sexp)))
    (etypecase first
      
      (list

       ;; Sexp looks like: ((:p :attr 2) "child")
       (psx-element (first first) 
		    (interleaved-alist (rest first))
		    (rest sexp)))
      
      (keyword
       ;; Sexp looks like (:p :attr 3 :attr2 "val" "child")
       (psx-element-sexp sexp))
      
      (string
       ;; Sexp looks like ("p" :attr 3 :attr2 "val" "child")
       (psx-element-sexp sexp))
      
      ;; Sexp is a function call (logic). Leave it be.
      (symbol
       sexp))))


(defun psx-compile (sexp)
  "Compile html-like DSL but leaves logic as-is"
  (if (atom sexp)
    (psx-atom sexp)
    (psx-list sexp)))


(defun psx* (sexp)
  ;; Smooth out the 'cl-who edges':
  `(macrolet ((component (component-expr &rest args)
		(if (and (list component-expr) 
			 (eq (first component-expr) 'function))
		    `(create-reactive-element ,(second component-expr) ,@args)
		    `(apply create-reactive-element component-expr ,@args)))
	     
	      (htm (html)
		`(psx ,html))
	     
	      (str (code)
		`(new (-string ,code))))
    
     ;; Compile the code, but this will leave htm, str and component-forms since
     ;; these look like logic. Hence the (recursive) macros above
     ,(psx-compile sexp)))


;; 
;; Exports to ParenScript.
;; 

;; The end-result of compiling psx-code will be ReactJS-like code with
;; calls to these JS-functions:
;; - createReactiveElement
;; - createReactiveComponent
(ps:defpsmacro psx (sexp)
  (psx* sexp))


(defun make-renderer (lambda-list html)
  `(lambda ()
       (ps:with-slots (,@lambda-list) this
	 ,(psx* html))))


(ps:defpsmacro defcomponent (name lambda-list html)
  `(ps:defvar ,name 
     (create-reactive-component 
      (ps:create :render ,(make-renderer lambda-list html)))))


(ps:ps (defcomponent testar () ((:p :class "ff") "child")))
