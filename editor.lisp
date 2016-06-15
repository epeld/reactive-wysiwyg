
(in-package :peldan.editor)


(defparameter test
  (make-hash-table :test #'equal))


(setf (gethash :mykey test)
      "abc")


(setf (gethash :mykey2 test)
      "def")


(setf (gethash :mykey3 test)
      1533)


(setf (gethash :mykey4 test)
      (list 1 2 3 4))

;; For compatibility with PS, downcase symbols in JSON
(defmethod yason:encode ((symbol symbol) &optional stream)
  (yason:encode (string-downcase symbol)
		stream))

(loop for k being the hash-keys of test collect k)


(defun generate (object &optional data)
  "Generate psx for editing object. Use the second arg to make it a live view"
  (etypecase object
    (hash-table
     `(:div :class "editor"
	    ,@(loop for key being the hash-keys of object
		 for value being the hash-values of object
		 collect
		   `(:div :class "entry"
				  
			  (:div :class "key"
				(cl-who:str ,key))
				  
			  (:div :class "value"
				;; Recurse
				,(generate value (or data 
						     `(chain data ,key))))))))
		   
    (string
     `(:input :value ,(or data object)))
		   
    (number
     `(:input :value ,(or data object)))
		   
    (cons
     ;; TODO
     `(:div "<List>"))))

(generate test)
