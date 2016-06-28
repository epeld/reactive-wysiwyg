
(in-package :peldan.dispatch)


(defun index (request)
  (peldan.component:generate-component-html
   `(:div "This is the Meta Session " (peldan.component:state name)
	  (mapcar (lambda (session)
		    (peldan.ml:h 
		     (:div (ps:@ session uuid)
			   (peldan.ps:json-stringify (ps:@ session data)))))
		  (peldan.component:state data)))
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
