

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

;; This installs the dispatch function for this entire package
(dispatch:install-handler 'page-dispatcher)


(defun render-view (view session)
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head (:title (cl-who:str (view:view-name view))))
     (:body (:script :type "text/javascript" (virtual-dom:library-js s))
	    (:script :type "text/javascript" (peldan.ps:generate-user-js s))
	    (:script :type "text/javascript" (let ((ps:*parenscript-stream* s))
					       (view:view-ps view 
							     :session session
							     :name 'component)))))))


(defun deploy-page (url fn)
  "Deploy a request handler to a given url"
  (unless (eq (aref url 0) #\/)
    (error "URL currently must start with a forward slash!"))

  (pushnew (cons url fn)
	   *deployed-views*))


(defun deploy-view (url view session)
  "Deploy a view of the given session, with the specified URL"
  (session:register-actions session (view:view-actions view))
  (push (cons url (lambda (request)
		    (declare (ignore request))
		    (render-view view session)))
	*deployed-views*))

