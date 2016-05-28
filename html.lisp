
(defpackage :peldan.html
  (:use :common-lisp)
  (:import-from :cl-html-parse :parse-html)
  (:import-from :drakma :http-request))

(in-package :peldan.html)


(defun parse-psx (html)
  (parse-html html))


(defun parse-psx-from-url (url)
  )


(defvar test (http-request "http://news.ycombinator.com"))

(eval `(cl-who:with-html-output-to-string (s)
	 ,(car (parse-html test)))) 
