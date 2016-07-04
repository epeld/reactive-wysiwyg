

(in-package :peldan.page)


(defun index (request)
  (declare (ignore request))
  "HELOOO")



(defun page-dispatcher (req)
  
  (cond ((string= "/" (script-name req))
	 (index req))))

(peldan.dispatch:install-handler 'page-dispatcher)

