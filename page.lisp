

(in-package :peldan.page)

(unless (boundp 'session-url)
  (defconstant session-url "/session/"
    "The base prefix for accessing sessions through HTTP request"))


(defun uuid-from-script-name (request)
  "Extract the requested uuid from request script-name"
  (subseq (script-name request) (length session-url)))


(defun session-url (uuid)
  "This function determines the url for which the session with uuid should be available"
  (when (string-equal uuid (peldan.session:uuid peldan.websocket:*meta*))
    (return-from session-url "/"))
  (concatenate 'string session-url uuid))


(peldan.ps:defps session-url (uuid)
  (+ (ps:lisp session-url) uuid))


(defgeneric session-page (session)
  (:documentation "Generate a html view of the session, for e.g editing/overviewing"))


(defmethod session-page ((meta session:meta-session))
  (peldan.component:generate-component-html
   `(:div (:h2 "This is the Meta Session. ") 
	  (:div "Below you will see a list of available sessions"
		(:ul
		 (mapcar (lambda (session)
			   (peldan.ml:h 
			    (:li (:a (:href (session-url (ps:@ session uuid)))
				     
				     "Session #" (ps:@ session uuid))
				 (:pre (peldan.ps:json-stringify (ps:@ session data))))))
			 (peldan.component:state data)))))
   :session-uuid (session:uuid meta)))


(defun some-action (session)
  (declare (ignore session))
  (format t "SOME ACTION CALLED"))

(defmethod session-page ((session session:app-session))
  (peldan.component:generate-component-html
   `(:div (:h2 "App Session #" ,(session:uuid session))
	  (:div (:class "state")
		(:pre (peldan.ps:json-stringify (peldan.component:state data))))
	  
	  (:div (:button (:onclick (lambda () (action some-action))) "Random Action"))
	  
	  (:a (:href ,(session-url (session:uuid websocket:*meta*)))
	      "Navigate to Overview"))
   :session-uuid (session:uuid session)
   :add-missing-actions t))


(defun index (request)
  (declare (ignore request))
  (session-page websocket:*meta*))



(defun page-dispatcher (req)
  
  (cond ((string= "/" (script-name req))
	 (index req))
	  
	((string= session-url (script-name req) :end2 (length session-url))
	 (let ((uuid (uuid-from-script-name req)))
	   (session-page (or (session:find-session uuid)
			     websocket:*meta*))))))

(peldan.dispatch:install-handler 'page-dispatcher)


(session:find-session "2")
