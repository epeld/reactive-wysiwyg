
(defpackage :peldan.alist
  (:use :common-lisp)
  (:export :flatten-alist))

(in-package :peldan.alist)


(defun flatten-alist (alist)
  "Returns keys and values interspersed in a flat list"
  (let (list)
    (loop for item in alist
	 do (setf list (cons (car item) (cons (cdr item) list))))
    list))
