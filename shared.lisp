
(in-package :peldan.shared)

(defgroup code)

;; TODO figure out how to 'tag' PS code to be able to filter them etc
(defmacro defun+ps (name lambda-list &body body)
  (add-code (make-code name
		       :args lambda-list
		       :body body))
  `(defun ,name ,lambda-list
	 ,@body))


;; TODO (name item) will be a string but we want the symbol. fix!
(defun generate-ps (&optional (type 'defun))
  "Generate PS code from the stored function definitions"
  `(progn 
     ,@(loop for item in (members code-group) collect
	    `(,type ,(name item) ,(field-value :args item)
		    ,@(field-value :body item)))))
