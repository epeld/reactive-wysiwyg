
(defpackage :redux
  (:use :common-lisp))

(in-package :redux)

(quicklisp:quickload "parenscript")

(defpsmacro declare (&rest args)
  )

(ps (defun create-stateless (args renderer)
  (declare (ignore args))
  (let ((spec (create :render renderer)))
    
    ;; TODO eventually make use of args here to do some type checking etc..
    ((@ -react create-class) spec))))



(defmacro defcomponent (name args body)
  `(let (component)
       
     ;; Define a new word in our DSL:
     (defun ,name ,args
       (create-react-element component ,@body))
     
     ;; Instantiate the internal component class
     (setf component (create-stateless ,args ,name))))

(ps
  (defcomponent test1 (a b)
    (:div "Hello, World!" (map (lambda (x) x) (list 1 2 3)))))



(defmacro defcomponent (name args body)
  (let ((attrs (loop for (k v) on (cdr body) by #'cddr))
	(el (car body))))
  `(defun ,name ,args
     (cl-who:with-html-output (stream)	; stream is special!
       ,@body)))


;; example:
(defcomponent mytest (a b c)
  (:div "This component has three parameters: "
	a b c
	
	"It also has one subcomponent:"
	(mytest2 b c)))


(defcomponent mytest2 (b c)
  (:div "I am just a div" b c))




