
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


;; TODO maintain a path into the object so we can dynamically refer to state within it!
(defun generate (object)
  "Generate psx for editing object"
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
				,(generate value))))))
		   
    (string
     `(:input :value ,object))
		   
    (number
     `(:input :value ,object))
		   
    (cons
     `(:div "<List>"))))

(generate test)
