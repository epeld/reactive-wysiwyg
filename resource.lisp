
(defpackage :peldan.resource
  (:use :common-lisp))

(in-package :peldan.resource)


(defclass resource ()
  ((name :reader resource-name 
	 :initarg :name
	 :initform (error "A name is required"))))


(defun find-resource (name resources)
  (common-lisp:find name resources :key #'resource-name :test #'string=))



