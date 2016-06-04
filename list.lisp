
(in-package :peldan.list)

(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))


(defun alist-to-plist (alist)
  "Returns keys and values interspersed (i.e, a plist)"
  (let (list)
    (loop for item in alist
	 do (setf list (cons (car item) (cons (cdr item) list))))
    list))


(defun plist-to-alist (list)
  (loop for rest on list by #'cddr
     collect (cons (first rest) (second rest))))


(defun assocdr (&rest args)
  "Take the CDR of the ASSOC"
  (cdr (apply #'assoc args)))


(defun assocar (&rest args)
  "Take the CAR of the ASSOC"
  (car (apply #'assoc args)))
