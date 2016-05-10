
(defpackage :web-component
  (:use :common-lisp))

(in-package :web-component)


(quicklisp:quickload "alexandria")

;; 
;; Utilities
;; 

;; "Constructive" traversal
(defun map-tree (fun tree)
  "Traverses a tree-like structure, calling fun for every sexp it finds. Returns the new tree"
  
  ;; If the tree is a list..
  (if (consp tree)
      ;; Call fun on that list
      (cons (funcall fun tree) 
	
	    ;; And traverse the child trees
	    (mapcar (lambda (child)
		      (map-tree fun child))
		    
		    (cdr tree)))
      
      ;; Else just return tree as-is
      tree))

;; Destructive traversal
(defun mapc-tree (fun tree)
  "Traverses a tree-like structure, calling fun for every sexp it finds. Returns tree"
  
  ;; If the tree is a list..
  (if (consp tree)

      ;; Call fun on that list
      (progn (funcall fun tree) 
	       
	     ;; And traverse the child trees
	     (mapc (lambda (child)
		     (map-tree fun child))
		     
		   (cdr tree)))
      
      ;; Else just return tree as-is
      tree))



(defun find-in-code (code car)
  "Find all sexps matching with a given car in the tree given by code"
  (let (matches)
    (mapc-tree (lambda (node)
		 (when (and (consp node) (eql car (first node)))
		   (pushnew node matches)))
		   
		   code)
    matches))


;; 
;; The Web Component class
;; - Represents a graphical component with some html-like code
;; 
;; Example:
(defvar example-code
  '(:div :class "my-example-div"
    "Hello, " (param name)))
;; 
;; this is now code for a component with one state-variable that can be changed live (eventually..)


(defclass component ()
  ((name :initarg :name :initform (error "Please give your component a name!") :reader component-name)
   (code :initarg :code :initform (error "The component must contain some code") :reader code)))


;; 
;; Compilation utils
;; 

(defvar *signals* nil
  "List of all defined state-signals")

(defvar *components* nil
  "List of all defined components")


(defun find-component (name list)
  "Find a component from a list, given its name"
  (or (find name list :test #'component-name)
      (error "Unknown component ~a" name)))


(defun resolve-param (param-form env)
  "Resolves the parameter indicated by param-form within the given environment"
  
  (destructuring-bind (param name &rest _) param-form
    (declare (ignore _))
    (assert (eql param 'param))

    (or (cdr (assoc name env :test #'eql))
	(error "The param ~a is undefined" name))))


(defun resolve-state (state-form &optional (signals *signals*))
  "Takes a snapshot of the state variable indicated by state-form and returns it"
  (destructuring-bind (state name &rest _) state-form
    (declare (ignore _))
    (assert (eql state 'state))
    
    (let ((sgnl (frp-signal:find-signal name signals)))
      (frp-signal:signal-value sgnl))))


(defun compile-component (component-form &optional (components *components*))
  (destructuring-bind (component name &rest params) component-form
    (assert (eql component 'component))
    
    (let ((c (find-component name components))
	  (env (loop for (key value) on params by #'cddr collect (cons key value))))
      (generate-html c env))))


;; 
;; Code generators
;; 

(defun generate-html (component &optional env)
  (flet ((process-sexp (sexp)
	   (case (first sexp)
	       (param 
		(resolve-param sexp env))
		
	       (state 
		(resolve-state sexp))
		
	       (component 
		(compile-component sexp))
		
	       (otherwise sexp))))
    
    (map-tree #'process-sexp (code component))))

(defparameter test-instance (make-instance 'component :code example-code :name "example"))

;; TODO work on tree traversal...
;(generate-html test-instance (acons 'name "erik" nil))
