
(defpackage :sites-webserver
  (:use :common-lisp :frp-signal :hunchentoot))

(in-package :sites-webserver)

;; Load dependencies
(quicklisp:quickload "hunchentoot")

;; TODO create a url handler macro that can extract variable names from the url!
(define-easy-handler (list-stores :uri "/stores") ()
  (setf (content-type*) "text/plain")
  (format nil "Testing 1 2 3"))


(defvar server (make-instance 'easy-acceptor :port 4242))
