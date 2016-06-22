;; Define the websocket interface of this server

(in-package peldan.websocket)


(defparameter *port* 3344)

(defparameter *ping-interval* 5000)

(defparameter *sessions* nil)


(defclass hunchensocket-client (websocket-client)
  ())


(defclass hunchensocket-session (hunchensocket:websocket-resource)
  ((uuid :initarg :uuid :initform (error "UUID required") :reader session-uuid)
   (state :initarg :state :initform '(:debug t) :reader session-state))
  (:default-initargs :client-class 'hunchensocket-client))


(defun session-with-uuid (uuid)
  (the hunchensocket-session 
       (or (find uuid *sessions* 
		 :key #'session-uuid 
		 :test #'string=)
	
	   (let ((instance (make-instance 'hunchensocket-session 
					  :uuid uuid)))
	     (format t "Creating new session '~a'~%" uuid)
	     (push instance *sessions*)
	     instance))))

(defun request-handler (request)
  "Hunchensocket request dispatch function"
  (let ((uuid (subseq (script-name request) 
		      1)))
    (format t "Got WS request for ~a" uuid)
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


(defun broadcast-message (instance type &rest rest)
  (broadcast instance (apply #'make-message type rest)))

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


(defun unknown-command-message (command)
  (make-message :type :message
		:message (format nil "Unknown command '~a'~%" command)))


(defmethod text-message-received ((instance hunchensocket-session) client message)
  (format t "Got message ~s!~%" message)
  
  ;; JSON parsing
  (let* ((*parse-object-as* :alist)
	 (message (parse message)))
    
    (format t "Parsed ~s~%" message)
      
    ;; Command execution
    (let ((command (assocdr "type" message :test #'string=)))
      (cond
	((string= "ping" command)
	 (send-message client :type :pong))
	      
	((string= "state" command)
	 (with-slots (state) instance
	   (setf state
		 (cdr (assoc "value" message :test #'string=)))
		 
	   (broadcast-message instance :type :state :value state)))
	
	((and (string= "action" command)
	      (string= "debug" (cdr (assoc "name" message :test #'string=))))
	 
	 (with-slots (state) instance
	   (format t "State was ~s" state)
	   (setf state (peldan.data:map-inside #'not state :debug))
	   (broadcast-message instance :type :state :value state)))
	      
	(t
	 (send-text-message client 
			    (unknown-command-message command))))))) 


(setf *websocket-dispatch-table* '(request-handler))


(defvar server (make-instance 'websocket-acceptor :port *port*))


(defun start-server ()
  (start server))


(defun generate-uuid ()
  "123") 				;TODO

(defun generate-uri (uuid)
  (concatenate 'string (format nil "ws://localhost:~s/" *port*) uuid)) ;TODO give different uuids


(defun connect-ps (initial-state set-state &optional (uuid (generate-uuid)))
  "Produce PS code for connecting to this websocket server"
  (let ((session (session-with-uuid uuid)))
    
    (setf (slot-value session 'state)
	  initial-state)
    
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
	 ws))))


(defun websockets-enabled ()
  (hunchentoot:started-p server))

