;; Define the websocket interface of this server

(in-package :peldan.websocket)


(defparameter *port* 3344)

(defparameter *ping-interval* 10000)

(defvar *resources* nil
  "List of resources and their script name (name . resource)")

(defun request-handler (request)
  "Looks for a dispatcher willing to "
  (let ((script-name (subseq (hunchentoot:script-name request) 1)))
    (format t "~&Got WS request for ~a~%" script-name)
    (find script-name *resources* :test #'string= :key #'session:uuid)))


(setf hunchensocket:*websocket-dispatch-table* '(request-handler))


(defvar server (make-instance 'hunchensocket:websocket-acceptor :port *port*))


(defun start-server ()
  (hunchentoot:start server))


(defun install-resource (resource &optional remove-others)
  "Install a new resource"
  (the hunchensocket:websocket-resource resource)
  (if remove-others
      (setf *resources* nil)
      (pushnew resource *resources*)))


(defun generate-uri (session)
  (concatenate 'string
	       (format nil "ws://localhost:~s/" *port*) 
	       (session:uuid session)))


(defun session-websocket-ps (session)
  "PS: Return a websocket connecting to the server-side session"
  `(let ((ws (ps:new (-web-socket ,(generate-uri session)))) 
	 interval)
     (with-slots (onclose onopen onmessage send-message) ws
       
       ;; Setup a keep-alive timer
       (setf interval
	     (set-interval (lambda ()
			     ((ps:@ ws send) ((ps:@ -j-s-o-n stringify) (ps:create :type :ping))))
			   (ps:lisp *ping-interval*)))
       
       (setf onclose
	     (lambda ()
	       (peldan.ps:log-message "Connection closed")
	       (clear-interval interval)))
	     
       (setf onopen
	     (lambda ()
	       (peldan.ps:log-message "Connection established")))
	     
       (setf onmessage
	     (lambda (msg)
	       (let ((content (peldan.ps:json-parse (ps:@ msg data))))
		 (with-slots (type value) content
		   (case type
		     (:error
		      (peldan.ps:log-warning "Server Error:" (ps:@ content error)))

		     (:state
		      (if onstate
			  (funcall onstate value)
			  (peldan.ps:log-warning "Got state message from server but no handler found" value)))
		      
		     (:pong
		      (return))
		      
		     (:message
		      (peldan.ps:log-message "Server:" (ps:@ content message)))
		      
		     (t
		      (peldan.ps:log-message "Got strange message" msg)))))))
       
       ;; Use this for sending JSON messages to the server
       (setf send-message
	     (lambda (obj)
	       (ps-util:log-message "Sending" obj)
	       ((@ ws send) (ps-util:json-stringify obj))))
       
       ws)))

(defun dummy-websocket-ps ()
  "PS: Return a dummy websocket faking a server-side session"
  `(let ((ws (create)))
     (with-slots (send-message) ws
                    
       ;; Use this for sending JSON messages to the server
       (setf send-message
	     (lambda (obj)
	       (ps-util:log-message "Would have sent message:" obj)))
       
       ws)))


(defun websockets-enabled ()
  (hunchentoot:started-p server))

