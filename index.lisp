
(in-package :peldan.dispatch)


(defgeneric session-page (session)
  (:documentation "Generate a html view of the session, for e.g editing/overviewing"))

(defmethod session-page ((meta session:meta-session))
  (peldan.component:generate-component-html
   `(:div (:h2 "This is the Meta Session. ") 
	  (:div "Below you will see a list of available sessions"
		(:ul
		 (mapcar (lambda (session)
			   (peldan.ml:h 
			    (:li (peldan.ps:json-stringify (ps:@ session data))
				 " - "
				 (:a (:href ,(session-url "meta")) ;TODO URL is wrong here
				     
				     "Session #" (ps:@ session uuid)))))
			 (peldan.component:state data)))))
   :session-uuid (session:uuid meta)))

(defmethod session-page ((session session:app-session))
  (peldan.component:generate-component-html
   `(:div "App Session " ,(session:uuid session)
	  (:div (:class "state")
		(peldan.ps:json-stringify (peldan.component:state)))
	  
	  (:div (:class "actions")
		,@(mapcar #'car (slot-value session 'peldan.session:actions))))
   :session-uuid (session:uuid session)))


(defun session-url (uuid)
  "This function determines the url for which the session with uuid should be available"
  (when (string-equal uuid (peldan.session:uuid peldan.websocket:*meta*))
    (return-from session-url ""))
  (concatenate 'string "/sessions/" uuid))


(defun index (request)
  (declare (ignore request))
  (session-page websocket:*meta*))





(defun sffsession-page (request)
  (let* ((uuid (subseq (script-name request) (length "/session/")))
	 (session (peldan.websocket:find-session uuid)))
    (assert session)
    
    "TODO"))



(defun main-dispatcher (req)
  
  (cond ((string= "/" (script-name req))
	 (index req))
	  
	((string= "/session/" (script-name req))
	 (session-page req))))

(install-handler 'main-dispatcher)
