
(in-package :peldan.h)

(defun sexp (var)
  `(:span (:class "sexp") 
	  "(" ,var ")"))
