
(in-package :peldan.import)


(defun generate-import-form-html ()
       (cl-who:with-html-output-to-string (s)
	 (:style :type "text/css"
		 ".entry { margin-bottom: 1em }
.label { margin-right: 1em }")
	 (:div (:h1 "Import a webpage")
	       (:p "Import html code and turn it into a virtual DOM component by either supplying the URL or pasting the html code to be parsed below")
	       (:form :method "POST"
		      (:div :class "entry entry--url"
			    (:span :class "label" "URL:")
			    (:input :name "url" :placeholder "http://.."))
		      (:div :class "entry entry--html"
			    (:span :class "label" "HTML:")
			    (:textarea :name "html" :placeholder "<div><p..."))
		      (:button :type "submit"
			       "OK")))))


(defun generate-response-from-html (html)
  (let ((parsed (parse-html html)))
    (cl-who:with-html-output-to-string (s)
      (:style :type "text/css"
	      ".result { border-bottom: solid 2px black; padding-bottom: 1em }")
      (:div (:h2 "You Posted")
	    (:pre :class "result input"
		  (cl-who:esc html)))
      
      (:div (:h2 "Parsed")
	    (:pre :class "result parsed"
		  (pprint parsed s)))
      
      (:div (:h2 "Generated")
	    (:pre :class "result javascript" :style "white-space: pre-wrap"
		       (let ((*parenscript-stream* s))
			 (ps* `(peldan.psx:psx ,(car parsed)))))))))


(defun request-handler (request)
  (let ((script-name (hunchentoot:script-name request)))
    
    (when (peldan.string:starts-with-p "/import" script-name)
      
      (case (hunchentoot:request-method request)
	(:get
	 (generate-import-form-html))
    
	(:post 
	 (let ((html (hunchentoot:post-parameter "html"))
	       (url (hunchentoot:post-parameter "url")))
	   
	 (cond ((and html (not (string= html "")))
		(generate-response-from-html html))
	       
	       ((and url (not (string= url "")))
		(generate-response-from-html (http-request url))))))))))


(defun install-handler ()
  (pushnew 'request-handler
	   peldan.dispatch:*handlers*))



