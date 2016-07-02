
(in-package peldan.dispatch)

;; We define our own list of handlers
;; that will get to work on the requests in turn
(defparameter *handlers* nil)


(defun central-dispatch (request)
  "The central request dispatcher for the whole app"

  ;; Go through the list of handlers until one handles the request
  (loop for fn in *handlers*
     for reply = (funcall fn request)
     until reply
     finally (return (or reply "404"))))



(define-easy-handler (central-handler :uri (constantly t)) ()
  (central-dispatch *request*))


(defvar server (make-instance 'easy-acceptor :port 4243))


(defun start-server (&optional (websocket t))
  (setf *show-lisp-errors-p* t)
  (start server)
  (when websocket
    (websocket:start-server)))

(defun stop-server ()
  (stop server))


(defun install-handler (handler)
  (pushnew handler *handlers*))
