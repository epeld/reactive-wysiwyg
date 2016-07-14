
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


(defpsmacro load (filename)
  (ps-compile-file filename))


(defmacro defun+ps (name args &body body)
  "Define a function that works both in lisp and in PS"
  `(progn (defun ,name ,args ,@body)
	  (defps ,name ,args ,@body)
	  #',name))


(defmacro defps (name args &body body)
  (let ((form `(defun ,name ,args ,@body)))
    `(progn (setf *user-ps-library*
		  (remove ',name *user-ps-library* :key #'car))
	    (push (cons ',name ',form) *user-ps-library*))))



(defun generate-user-js (&optional (stream *standard-output*))
  "Generate JS from the user defined functions"
  (let ((*parenscript-stream* stream))
    (apply #'ps* (mapcar #'cdr *user-ps-library*))))
