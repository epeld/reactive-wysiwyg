
(defpackage :web-component
  (:use :common-lisp))

(in-package :web-component)


(quicklisp:quickload "alexandria")

;; 
;; Utilities
;; 

(defun traverse-tree (fun tree)
  "Traverses a tree-like structure, calling fun for every sexp it finds"

  ;; When the tree is a list..
  (when (consp tree)
    
    ;; Call fun on that list
    (funcall fun tree)

    ;; And traverse the child trees
    (mapc (lambda (child)
	    (traverse-tree fun child))
	  (cdr tree))
    t))



(defun find-in-code (code car)
  "Find all sexps matching with a given car in the tree given by code"
  (let (matches)
    (traverse-tree (lambda (node)
		     (when (and (consp node) (eql car (first node)))
		       (pushnew node matches)))
		   
		   code)
    matches))


;; 
;; Functions for finding various info about a given code segment
;; 

(defun find-parameters (code)
  "Find and return a list of all referenced parameters of this code"
  (find-in-code code 'param))


(defun find-components (code)
  "Find and return a list of all referenced subcomponents in this code"
  (find-in-code code 'component))


(defun find-state (code)
  "Find and return a list of all referenced state variables in this code"
  (find-in-code code 'state))


;; 
;; The Web Component class
;; - Represents a graphical component with some html-like code
;; 
;; Example:
(defvar example-code
  '(:div :class "my-example-div"
    "Hello, " (state name)))
;; 
;; this is now code for a component with one state-variable that can be changed live (eventually..)


(defclass component ()
  ((name :initarg name :initform (error "Please give your component a name!"))
   (code :initarg code :initform (error "The component must contain some code") :reader code)))


(defun parameters (component)
  (find-parameters (code component)))


(defun state-vars (component)
  (find-state (code component)))


(defun sub-components (component)
  (find-components (code component)))

;; 
;; Code generators
;; 
(defun generate-html (component &optional env)
  ;; TODO use cl-who to 
  ;; replace (state bla) by e.g (lisp (get-store-value bla)))
  ;; replace (param bla) by e.g (lisp (get-env bla)))
  ;; replace (component bla) by (generate-html (get-component bla)))
  ;; etc
  ;; Note: the aforementioned operations can all be performed BEFORE invoking cl-who.
  ;; just create gensyms for them and assign to them in an enclosing let, THEN call cl-who
  ;; Could then be done by e.g traversing the tree and (signal ..)ing everytime a form is found
  ;; 
  ;; The only difference between (state ) and (param) is how the values are looked up.
  ;; That is, they produce the same code in the end once the lookup has been performed.
  )

;; TODO for this we require a code walker that can produce a modified code-tree

;; Note: 'environment' above refers to the set of param-bindings, store-bindings and
;; available subcomponents that can be referenced during compilation
