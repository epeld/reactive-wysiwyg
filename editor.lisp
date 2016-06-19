
(in-package :peldan.editor)

(defparameter *cached-editor-styling*
  ".editor {
    border: solid 1px black;
    padding: 0.25em;
    display: flex;
    flex-direction: column;
    flex-shrink: 1;
}

.entry {
    display: flex;
    align-items: flex-start;
    justify-content: flex-start;
    margin-bottom: 0.5em;
}

.key {
    margin-right: 0.5em;
}")

#|
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
|#

;; For compatibility with PS, downcase symbols in JSON
(defmethod yason:encode ((symbol symbol) &optional stream)
  (yason:encode (string-downcase symbol)
		stream))


(defun hash-table-editor (hash-table &key data)
  `(:div :class-name "editor"
	 ,@(loop for key being the hash-keys of hash-table
	      for value being the hash-values of hash-table
	      collect
		`(:div :class-name "entry"
				  
		       (:div :class-name "key"
			     ,key)
				  
		       (:div :class-name "value"
			     ;; Recurse
			     ,(generate value 
					:data (and data 
						   `(chain ,data ,key))))))))


(defun list-editor (list &key data (template (and list 
						  (first list))))
  "Generate a list editor, basing each individuall element off of the indicated template"
  (with-ps-gensyms (index item)
    `(:div :class-name "list-editor"
	   (imapcar (lambda (,index ,item)
		      (psx ,(generate template 
				      :data item)))
		    ,(if data 
			 data
			 `(list ,@list))))))


;; TODO use the set-field action to give life to the editor
;; TODO we need some way of tracking the path on recursive calls to generate
;; then we need functions for producing e.g actions and classes etc
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


(defun anonymous-editor (initial-state)
  "Create an anonymous editor for the state passed in"
  (peldan.component:make-component 
   'anonymous-component
   :code `(psx ,(generate initial-state))
   :initial-state `(peldan.ps:json-parse 
		    ,(with-output-to-string (s) 
					    (yason:encode initial-state s)))))


(defun generate-editor-html (data)
  (let ((code (generate data)))
    (cl-who:with-html-output-to-string (s)
      
      (:style :type "text/css" 
	      (cl-who:str *cached-editor-styling*))
      
      (:div (:h1 "Result")
	    (:div "You posted:"
		  (:pre :class "result"
			(yason:encode data s)))
	    (:div "Result:"
		  (:pre :class "result"
			(pprint code s)))
	    (:div "JS:"
		  (:pre :class "result"
			(let ((*parenscript-stream* s))
			  (ps* `(psx ,code)))))
		      
	    (cl-who:fmt (peldan.component:generate-component-html (anonymous-editor data)))))))


(defun generate-editor-form-html ()
  (cl-who:with-html-output-to-string (s)
    (:div (:h1 "Editor Generator")
	  (:form :method "POST"
		 (:div
		  (:p "Please enter some JSON below to be used as a template")
		  (:textarea :name "json"
			     :rows 20 :cols 50))
		 (:button :type "submit"
			  "OK")))))


(defun request-handler (request)
  (let ((script-name (hunchentoot:script-name request)))
    
    (when (peldan.string:starts-with-p "/edit/" script-name)
      
      (let ((raw (hunchentoot:post-parameter "json")))
	(if (and raw (< 0 (length raw)))
	    (generate-editor-html (yason:parse raw))
	    (generate-editor-form-html))))))


(defun install-handler ()
  "Install the request handler that will maek components accessible through hunchentoot"
  (pushnew (lambda (req) (request-handler req)) peldan.dispatch:*handlers*))



;; TODO generate an endpoint for POSTING data and producing a rendered component
;; that can edit said data and serialie it back out

;; TODO make the GET for the same URL be a html form for POSTing

;(ql:quickload "cl-html-parse")
;(cl-html-parse:parse-html (drakma:http-request "http://lisp.org"))
