
(defpackage :sites-webserver
  (:use :common-lisp :frp-signal :hunchentoot))

(in-package :sites-webserver)

;; Load dependencies
(quicklisp:quickload "hunchentoot")
(quicklisp:quickload "yason")

(defvar server (make-instance 'easy-acceptor :port 4242))


(defvar components nil)
; (defvar signals nil)


(define-easy-handler (list-stores :uri "/stores") ()
  (setf (content-type*) "text/plain")
  (format nil "Testing 1 2 3"))


(defcomponent mycomponent (a b c)
  (:h1 a)
  (map #'subcomp b)
  (subcomp2 b c)
  (when c
    (:div "postamble" (str c))))


(defun display-components-list ()
  (cl-who:with-html-output-to-string (s)
    (:h1 "All components")
    (:div :class "components-list"
	  (if (null components)
	      (cl-who:htm (:div (cl-who:str "No components yet!")))
	      (loop for c in components
		 do (cl-who:htm (:div (cl-who:str "Component"))))))))


(define-easy-handler (list-components :uri "/components") (name code)
  
  (case (request-method*)
    (:get
     (if (null name)
	 (display-components-list)
	 (display-component name)))
    
    (:post
     (when (and name code)
       (format nil "Received a new component named ~a with code ~a" name code)))))


; (setf *show-lisp-errors-p* t)



