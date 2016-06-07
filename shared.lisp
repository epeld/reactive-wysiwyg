
(in-package :peldan.shared)

(defgroup code)


(defmacro defun+ps (name lambda-list &body body)
  (replace-code (make-code name
			   :args lambda-list
			   :body body))
  `(defun ,name ,lambda-list
	 ,@body))


(defun generate-ps ()
  "Generate PS code from the stored function definitions"
  `(progn 
     ,@(loop for item in (members code-group) collect
	    `(defun ,(field-value :symbol item) ,(field-value :args item)
		    ,@(field-value :body item)))))
