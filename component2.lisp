
(defpackage :component
  (:use :common-lisp :hunchentoot)
  (:import-from :peldan.string :string-bind-case))


(in-package :component)

;(quicklisp:quickload "hunchentoot")
;(quicklisp:quickload "alexandria")

(defclass component ()
  ((code :reader source-code
	 :initarg :code
	 :initform (error "Source code required"))
   (args :reader args
	 :initarg :args)))


(defvar *components* nil
  "List of all publicly available components")


(defmacro defcomponent (name args psx)
  `(defvar ,name (make-instance 'component
				:code (quote ,psx)
				:args (quote ,args))))




;; The sole handler for all requests
(defun handle-component-request ()
  (string-bind-case (script-name*)
		    (("component" name output)
		     (format nil "You have requested the componet ~s as ~s" name output))))




;; Set up the server
(define-easy-handler (components-handler :uri (constantly t)) ()
  (handle-component-request))

(defparameter server (make-instance 'easy-acceptor :port 4243))

(start server)




(setf *show-lisp-errors-p* t)

(defcomponent testdiv (a b)
  (:div "This is a test div" a (:p "This is a paragraph!") (mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3)) b))


(psx (:div "This is a test div" a (:p "This is a paragraph!") (mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3)) b))

(mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3))






