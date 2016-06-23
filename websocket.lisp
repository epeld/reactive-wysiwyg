;; Define the websocket interface of this server

(in-package :peldan.websocket)


(defparameter *port* 3344)

(defparameter *ping-interval* 5000)

(defparameter *sessions* nil)

(defclass hunchensocket-client (websocket-client)
  ())


(defclass hunchensocket-session (hunchensocket:websocket-resource)
  ((uuid :initarg :uuid :initform (error "UUID required") :reader session-uuid)
   (state :initarg :state :initform '(:debug t) :reader session-state)
   (action-log :initform nil :reader action-log))
  (:default-initargs :client-class 'hunchensocket-client))


(defun update-state (update session)
  "Update the session's state by applying the function update to it,
 broadcasting out the update"
  (with-slots (state) session
    (setf state
	  (funcall update state))
    (broadcast session (state-message state))))


;; TODO remove this and start using peldan.action:compute-state
(defun run-action (session action)
  "Execute an action in a given session,
 broadcasting out the change of state"
  (update-state (lambda (state)
		  (peldan.action:run-action action state))
		session))


(defun generate-uuid ()
  "123")


(defun session-with-uuid (uuid &optional initial-state)
  "Returns a session with the given uuid and state
 by either looking it up or creating"
  (let ((instance (find uuid *sessions* 
			:key #'session-uuid 
			:test #'string=)))
    (unless instance
      (format t "~&Creating new session '~a'" uuid)
      (setf instance (make-instance 'hunchensocket-session :uuid uuid))
      (push instance *sessions*))
    
    (when initial-state
      (format t "~&Setting state to ~&~s" initial-state)
      (setf (slot-value instance 'state) initial-state))
    
    instance))


(defun request-handler (request)
  "Hunchensocket request dispatch function"
  (let ((uuid (subseq (script-name request) 
		      1)))
    (format t "~&Got WS request for ~a" uuid)
    (session-with-uuid uuid)))


(defun broadcast (instance message)
  "Broadcast a message to all a session's clients"
  (loop for client in (clients instance)
       do (send-text-message client message)))


(defun make-message (&rest rest)
  (with-output-to-string (s)
    (yason:with-output (s)
      (peldan.data:encode-nested-plist rest))))


(defun send-message (client &rest rest)
  (let ((msg (apply #'make-message rest)))
    (format t "Sending message ~a" msg)
    (send-text-message client msg)))


;; 
;; Event handling
;; 
(defmethod client-connected ((instance hunchensocket-session) client)
  (format t "Client connected to ~a!~%" (session-uuid instance))
  (send-message client 
		:type :message
		:message "Hello!")

  (send-message client
		:type :state
		:value (session-state instance)))


(defmethod client-disconnected ((instance hunchensocket-session) client)
  (declare (ignore instance))
  (format t "Client disconnected!~%"))


(defun unknown-type-message (type)
  (make-message :type :message
		:message (format nil "Unknown command '~a'~%" type)))


(defun state-message (new-state)
  "Construct a message for changing the state of a client to a new value"
  (make-message :type :state
		:value new-state))

(defun ping-message ()
  "Construct a ping message"
  (make-message :type :pong))


(defun get-action (message)
  (make-instance 'peldan.action:action 
		 :name (getf message :name)
		 :args (getf message :args)))


(defmethod text-message-received ((instance hunchensocket-session) client message)
  (format t "Got message ~s!~%" message)
  
  ;; JSON parsing
  (let* ((message (parse message 
			 :object-as :plist
			 :object-key-fn #'peldan.data:find-keyword)))
    
    (format t "Parsed ~s~%" message)
      
    ;; Command execution
    (let ((type (getf message :type)))
      (handler-bind ((error
		      (lambda (c)
			(format t "Error: ~a" c))))
	(cond
	  ((string= "ping" type)
	   (send-message client (ping-message)))
	      
	  ((string= "action" type)
	   (run-action instance (get-action message)))
	      
	  (t
	   (send-text-message client 
			      (unknown-type-message type)))))))) 


(setf *websocket-dispatch-table* '(request-handler))


(defvar server (make-instance 'websocket-acceptor :port *port*))


(defun start-server ()
  (start server))


(defun generate-uri (uuid)
  (concatenate 'string (format nil "ws://localhost:~s/" *port*) uuid)) ;TODO give different uuids


(defun connect-ps (initial-state set-state &optional (uuid (generate-uuid)))
  "Produce PS code for connecting to this websocket server"
  ;; Ensure session exists with state
  (session-with-uuid uuid initial-state)
  `(let (interval (ws (ps:new (-web-socket ,(generate-uri uuid)))))
     (with-slots (onclose onopen onmessage) ws
       
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
	       (peldan.ps:log-message "Server message" msg)
	       (let ((content (peldan.ps:json-parse (ps:@ msg data))))
		 (with-slots (type value) content
		   (case type
		     (:state
		      (,set-state value))
		      
		     (:pong
		      (return))
		      
		     (:message
		      (peldan.ps:log-message "Server:" (ps:@ content message)))
		      
		     (t
		      (peldan.ps:log-message "Got strange message" msg)))))))
       ws)))


(defun websockets-enabled ()
  (hunchentoot:started-p server))

