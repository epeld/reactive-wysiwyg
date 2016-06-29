
(in-package :peldan.dispatch)


(defun session-url (uuid)
  "This function determines the url for which the session with uuid should be available"
  (when (string-equal uuid (peldan.websocket:uuid peldan.websocket:*meta*))
    (return-from session-url ""))
  (concatenate 'string "/sessions/" uuid))


(defun index (request)
  (peldan.component:generate-component-html
   `(:div (:h2 "This is the Meta Session. ") 
	  (:div "Below you will see a list of available sessions"
		(:ul
		 (mapcar (lambda (session)
			   (peldan.ml:h 
			    (:li (peldan.ps:json-stringify (ps:@ session data))
				 " - "
				 (:a (:href ,(session-url "meta"))
				     
				     "Session #" (ps:@ session uuid)))))
			 (peldan.component:state data)))))
   :session-uuid (peldan.websocket:uuid peldan.websocket:*meta*)))


(defun session-page (request)
  (let* ((uuid (subseq (script-name request) (length "/session/")))
	 (session (peldan.websocket:find-session uuid)))
    (assert session)
    
    (peldan.component:generate-component-html
     `(:div "Session " (peldan.component:state uuid)
	    (:div (:class "state")
		  (peldan.ps:json-stringify (peldan.component:state)))
	  
	    (:div (:class "actions")
		  ,@(mapcar #'car (slot-value session 'peldan.websocket:actions))))
     :session-uuid uuid)))


(defun deploy (h uuid &optional (url uuid))
  "Deploy a new endpoint with the given hyperscript and session uuid"
  (install-handler 
   (lambda (req)
     (when (string= url (script-name req))
       (peldan.component:generate-component-html h :session-uuid uuid)))))



(defun main-dispatcher (req)
  
  (cond ((string= "/" (script-name req))
	 (index req))
	  
	((string= "/session/" (script-name req))
	 (session-page req))))

(install-handler 'main-dispatcher)
