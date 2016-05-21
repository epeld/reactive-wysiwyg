
(defpackage :component
  (:use :common-lisp :hunchentoot)
  (:import-from :peldan.string :string-bind-case)
  (:import-from :peldan.resource :resource :find-resource :resource-name))


(in-package :component)

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
		   `(pushnew ,name *components*)))))


(defun generate-component-output (name output)
  (let ((component (find-resource name *components*)))
    (if component
	(format nil "You have requested the componet ~s as ~s" name output)
	(format nil "Component ~a doesn't exist" name))))


;; The sole handler for all requests
(defun handle-component-request ()
  (string-bind-case (script-name*)
		    (("component" name output)
		     (generate-component-output name output))
		    
		    (otherwise
		     (format nil "Page not found! ~s" (script-name*)))))




;; Set up the server
(define-easy-handler (components-handler :uri (constantly t)) ()
  (handle-component-request))

(defparameter server (make-instance 'easy-acceptor :port 4243))

(start server)




(setf *show-lisp-errors-p* t)

(defcomponent testdiv (a b)
  (:div "This is a test div" a (:p "This is a paragraph!") (mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3)) b))

(setf *components* nil)
(pushnew testdiv *components*)

(resource-name testdiv)
