
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


(defun hash-table-editor (hash-table &key data)
  `(:div :class "editor"
	 ,@(loop for key being the hash-keys of hash-table
	      for value being the hash-values of hash-table
	      collect
		`(:div :class "entry"
				  
		       (:div :class "key"
			     (cl-who:str ,key))
				  
		       (:div :class "value"
			     ;; Recurse
			     ,(generate value 
					:data (and data 
						   `(chain ,data ,key))))))))


(defun list-editor (list &key data (template (and list 
						  (first list))))
  "Generate a list editor, basing each individuall element off of the indicated template"
  (with-ps-gensyms (index item)
    `(:div :class "list-editor"
	   (imapcar (lambda (,index ,item)
		      (psx ,(generate template 
				      :data item)))
		    ,(if data 
			 data
			 `(list ,@list))))))


;; class lambda, action lambda
(defun generate (object &key data)
  "Generate psx for editing object. Use the second arg to make it a live view"
  (etypecase object
    ;; 
    ;; Composites
    (hash-table
     (hash-table-editor object :data data))
		   
    (list
     (list-editor object :data data))
		 
    ;; Leaves
    (string
     `(:input :value ,(or data object)))
		   
    (number
     `(:input :value ,(or data object)))))

(generate test)

;; TODO generate an endpoint for POSTING data and producing a rendered component
;; that can edit said data and serialie it back out

;; TODO make the GET for the same URL be a html form for POSTing
