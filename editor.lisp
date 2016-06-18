
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


;; TODO use the set-field action to give life to the editor
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


(defun request-handler (request)
  (let ((script-name (hunchentoot:script-name request)))
    
    (when (peldan.string:starts-with-p "/edit/" script-name)
      
      (let ((raw (hunchentoot:post-parameter "json")))
	(if (and raw 
		 (< 0 (length raw)))
	    ;; TODO generate editor HTML here for the data
	    (format nil "You posted: ~a"
		    (with-output-to-string (s)
		      (yason:encode (yason:parse raw) 
				    s)))

	    (cl-who:with-html-output-to-string (s)
	      (:div (:h1 "Editor Generator")
		    (:form :method "POST"
			   (:div
			    (:p "Please enter some JSON below to be used as a template")
			    (:textarea :name "json"
				       :rows 20 :cols 50))
			   (:button :type "submit"
				    "OK")))))))))


(defun install-handler ()
  "Install the request handler that will maek components accessible through hunchentoot"
  (pushnew (lambda (req) (request-handler req)) peldan.dispatch:*handlers*))



;; TODO generate an endpoint for POSTING data and producing a rendered component
;; that can edit said data and serialie it back out

;; TODO make the GET for the same URL be a html form for POSTing
