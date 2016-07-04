

(in-package :peldan.page)


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

