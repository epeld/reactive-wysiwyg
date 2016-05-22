
(defpackage :peldan.component
  (:use :common-lisp :hunchentoot)
  
  (:import-from :peldan.string 
		:string-bind-case)
  
  (:import-from :peldan.resource 
		
		:replace-resource
		:resource
		:find-resource
		:resource-name)
  
  (:export :component :source-code :args :*components*))


(in-package :peldan.component)

;(quicklisp:quickload "hunchentoot")
;(quicklisp:quickload "alexandria")

(defclass component (resource)
  ((code :reader source-code
	 :initarg :code
	 :initform (error "Source code required"))
   (args :reader args
	 :initarg :args)))


(defvar *components* nil
  "List of all publicly available components")


(defmacro defcomponent (name args psx &key (publish t))
  (let ((sname (string-downcase name)))
    `(progn (defparameter ,name (make-instance 'component
					       :code (quote ,psx)
					       :args (quote ,args)
					       :name ,sname))
	    ,(when publish
		   `(replace-resource ,name *components*)))))


(defun keywordize (string)
  (intern (string-upcase string) "KEYWORD"))


(defun generate-html (component)
  'todo)

(defun component-page (component output)
  (case (keywordize output)
    
    (:js (format nil "Component ~s has source code ~&~s" (resource-name component) (source-code component)))
    
    ; (:html (format nil "HTML! ~s" (resource-name component)))
    
    (otherwise (format nil "You have requested the componet ~s as ~s ~&~s" (resource-name component) output (source-code component)))))


(defun process-component-request (name output)
  (let ((component (find-resource name *components*)))
    (if component
	(component-page component output)
	(format nil "Component ~a doesn't exist" name))))


;; The sole handler for all requests
(defun dispatch-on-script-name ()
  (string-bind-case (script-name*)
		    
		    (("component" name output)
		     (process-component-request name output))
		    
		    (("data" name)
		     (format nil "The data-page is still work in progress (~s)" name))
		    
		    (otherwise
		     (format nil "Page not found! ~s" (script-name*)))))


