
(defpackage peldan.dispatch
  (:use common-lisp hunchentoot)
  (:export "*HANDLERS*"))

(in-package peldan.dispatch)

(ql:quickload "hunchentoot")

;; We define our own list of handlers
;; that will get to work on the requests in turn
(defparameter *handlers* nil)


(defun central-dispatch (request)
  "The central request dispatcher for the whole app"

  ;; Go through the list of handlers until one handles the request
  (loop for fn in *handlers*
       
     do (let ((reply (funcall fn request)))
	  (when reply
	    (return-from central-dispatch reply)))))



(define-easy-handler (central-handler :uri (constantly t)) ()
  
  (central-dispatch *request*))


(defvar server (make-instance 'easy-acceptor :port 4243))

(quote (start server))
(setf *show-lisp-errors-p* t)
