
(in-package peldan.ps)


(defparameter *user-ps-library* nil
  "Contains all user defined functions (from defun+ps)")

(defpsmacro log-message (&rest args)
  `((@ console log) ,@args))


(defpsmacro log-warning (&rest args)
  `((@ console warn) ,@args))


(defpsmacro json-stringify (object)
  `((@ -j-s-o-n stringify) ,object))


(defpsmacro json-parse (string)
  `((@ -j-s-o-n parse) ,string))

;; To allow including other files a la npm et al
(defpsmacro load (filename)
  (ps-compile-file filename))


(defmacro defun+ps (name args &body body)
  "Define a function that works both in lisp and in PS"
  `(progn #1=(defun ,name ,args ,@body)
	  (push '#1# *user-ps-library*)
	  (setf *user-ps-library*
		(remove-duplicates *user-ps-library* :test #'equal))
	  #',name))




