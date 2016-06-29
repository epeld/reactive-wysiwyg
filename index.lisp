
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
			    (:li (:a (:href ,(session-url "meta")) ;TODO URL is wrong here
				     
				     "Session #" (ps:@ session uuid))
				 (:pre (peldan.ps:json-stringify (ps:@ session data))))))
			 (peldan.component:state data)))))
   :session-uuid (session:uuid meta)))


(defmethod session-page ((session session:app-session))
  (peldan.component:generate-component-html
   `(:div (:h2 "App Session #" ,(session:uuid session))
	  (:div (:class "state")
		(:pre (peldan.ps:json-stringify (peldan.component:state data))))
	  
	  (:div (:class "actions")
		"Defined actions: "
		(:select 
		,@(loop for (name . action) in (slot-value session 'peldan.session:actions)
		       collect `(:option ,name))))
	  
	  (:a (:href ,(session-url (session:uuid websocket:*meta*)))
	      "Navigate to Overview"))
   :session-uuid (session:uuid session)))


(defun session-url (uuid)
  "This function determines the url for which the session with uuid should be available"
  (when (string-equal uuid (peldan.session:uuid peldan.websocket:*meta*))
    (return-from session-url "/"))
  (concatenate 'string "/sessions/" uuid))


(defun index (request)
  (declare (ignore request))
  (session-page websocket:*meta*))




(defun uuid-from-script-name (request)
  (subseq (script-name request) (length "/session/")))


(defun main-dispatcher (req)
  
  (cond ((string= "/" (script-name req))
	 (index req))
	  
	((string= #1="/session/" (script-name req) :end2 (length #1#))
	 (let ((uuid (uuid-from-script-name req)))
	   (session-page (or (session:find-session uuid)
			     websocket:*meta*))))))

(install-handler 'main-dispatcher)
