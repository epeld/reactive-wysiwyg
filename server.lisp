
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



(define-easy-handler (list-components :uri "/components") ()
  (setf (content-type*) "text/plain")
  
  (yason:with-output-to-string* ()
    
    ;; TODO write encode-array JSON fn..
    (yason:with-array ()
      (loop for c in components
	   do (yason:encode-array-element c)))))



