

(in-package :peldan.page)


(defparameter *deployed-views*
  nil
  "A list of (url . view)")


(defun index (request)
  (declare (ignore request))
  "You found the index page")



(defun page-dispatcher (req)
  (let ((assoc (assoc (script-name req) *deployed-views* :test #'string=)))
    (cond 
	
      (assoc
       (funcall (cdr assoc) req))
      
      ((string= "/" (script-name req))
       (index req))
      
      (t
       "Unkown page"))))


(defun render-view (view &optional session)
  "Renders a full view, with all necessary JS and HTML"
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head (:title (cl-who:str (view:view-name view))))
     (:body (:script :type "text/javascript" (virtual-dom:library-js s))
	    (:script :type "text/javascript" (peldan.ps:generate-user-js s))
	    (:script :type "text/javascript" (let ((ps:*parenscript-stream* s))
					       (ps:ps* (view:view-ps view 
								     :session session
								     :name 'component))))))))


(defun deploy-page (url fn)
  "Deploy a request handler to a given url"
  (unless (eq (aref url 0) #\/)
    (error "URL must start with a forward slash!"))

  ;; Remove obsolete page(s)
  (setf *deployed-views* (delete url *deployed-views* :test #'string= :key #'car))
  
  (push (cons url fn)
	*deployed-views*))


(defun deploy-view (url view &optional (session view:*default-session*))
  "Deploy a view of the given session, with the specified URL"
  (unless (typep view 'view:view)
    ;; Assume we were passed hyperscript and create the view on the fly!
    (return-from deploy-view 
      (deploy-view url (view:make-view (the list view)) session)))
  
  (when session
    (session:register-actions session (view:view-actions view)))
  
  (deploy-page url (lambda (request)
		     (declare (ignore request))
		     (render-view view session))))



(defun print-it (&rest args)
  (format t "PRINTINNG ~a" args))

(deploy-view "/def" 
	     '(:div "Hello" (:b "WORLD")
	       (:button (:onclick (lambda ()
				    (view:action print-it)))
		"CLICK ME")))



;; This installs the dispatch function for this entire package
(dispatch:install-handler 'page-dispatcher)


(defclass simple-session (session:app-session)
  ()
  (:documentation "Dummy session just to have something to fall back on"))


(defmethod session:state-message (session)
  (message:make-message :type :state :value '(:debug nil)))



(defclass test-session (view:view)
  ()
  (:documentation "A session for testing out the action translation!"))

(defun print-it (session client &rest args)
  (format t "~&Print it action called with args ~a" args))

(defun setup-default-session ()
  (setf view:*default-session*
	(make-instance 'simple-session))
  
  (websocket:install-resource view:*default-session*))
