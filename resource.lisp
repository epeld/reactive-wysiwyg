
(defpackage :peldan.resource
  (:use :common-lisp)
  (:export :replace-resource
	   :find-resource
	   :resource
	   :resource-name))

(in-package :peldan.resource)


(defclass resource ()
  ((name :reader resource-name 
	 :initarg :name
	 :initform (error "A name is required"))))


(defun find-resource (name resources)
  (common-lisp:find name resources :key #'resource-name :test #'string=))



(defun replace-resource (resource resources)
  (cons resource (remove (resource-name resource) resources :test #'string= :key #'resource-name :count 1)))
