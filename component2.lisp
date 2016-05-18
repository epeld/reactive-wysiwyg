
(defpackage :component
  (:use :common-lisp :hunchentoot :psx))


(in-package :component)


(defvar server (make-instance 'easy-acceptor :port 4242))



(defmacro define-component-handler (component name args contents)
  `(define-easy-handler (,(make-handler-name component name)
			  :uri ,(make-handler-uri component name)) ,args
     ,contents))


(defun make-handler-name (component name)
  (intern (string-upcase (concatenate 'string (string component) "-" (string name)))))


(defun make-handler-uri (component name)
  (string-downcase (concatenate 'string "/components/" (string component) "/" (string name))))
 

(defmacro defcomponent (name args psx)
  `(progn (define-component-handler ,name :html ,args
				    (let ((who (psx-to-who ,psx)))
				      (eval `(cl-who:with-html-output-to-string (s)
					       ,who))))
  
	  (define-component-handler ,name :who ,args
				    (format nil "~s" (psx-to-who ,psx)))))




(setf *show-lisp-errors-p* t)

(defcomponent testdiv (a b)
  (:div "This is a test div" a b))





