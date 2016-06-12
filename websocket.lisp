;; Define the websocket interface of this server

(in-package peldan.websocket)


(defgroup session)


(defparameter *port* 3344)

(defparameter *ping-interval* 5000)


(defclass hunchensocket-client (websocket-client)
  ())


(defclass hunchensocket-session (hunchensocket:websocket-resource)
  ((uuid :initarg :uuid :initform (error "UUID required") :reader session-uuid))
  (:default-initargs :client-class 'hunchensocket-client))



(defun make-session-from-request (request)
  (let ((uuid (script-name request)))
    (make-session uuid
		  :instance (make-instance 'hunchensocket-session
					   :uuid uuid)
		  :state nil)))


(defun session-for-request (request)
  "Hunchensocket request dispatch function"
  (let ((uuid (script-name request)))
    (let ((result (cdr (assoc :instance (or (find-session uuid)
					    (add-session (make-session-from-request request)))))))
      (format t "UUID ~a~%" uuid)
      (the hunchensocket-session result)
      result)))


(defun broadcast (instance message)
  "Broadcast a message to all a session's clients"
  (loop for client in (clients instance)
       do (send-text-message client message)))


(defun make-message (type &rest rest)
  (with-output-to-string (s)
    (yason:with-output (s)
      (yason:with-object ()
	(apply #'yason:encode-object-elements 
	       "type" 
	       (string type) 
	       (mapcar #'string rest))))))


(defun send-message (client type &rest rest)
  (send-text-message client (apply #'make-message type rest)))

(defun broadcast-message (instance type &rest rest)
  (broadcast instance (apply #'make-message type rest)))

;; 
;; Event handling
;; 
(defmethod client-connected ((instance hunchensocket-session) client)
  (declare (ignore instance))
  (format t "Client connected!~%"))


(defmethod client-disconnected ((instance hunchensocket-session) client)
  ;; TODO consider removing the session here!
  (declare (ignore instance))
  (format t "Client disconnected!~%"))


(defmethod text-message-received ((instance hunchensocket-session) client message)
  (format t "Got message ~s!~%" message)
  
  ;; JSON parsing
  (let* ((session (find-session (session-uuid instance) :force t))
	 (*parse-object-as* :alist)
	 (message (parse message)))
      
      (format t "Parsed ~s~%" message)
      
      ;; Command execution
      (let ((command (assocdr "type" message :test #'string=)))
	    (cond
	      ((string= "ping" command)
	       (send-message client :pong))
	      
	      ((string= "state" command)
	       (setf (cdr (assoc :state session))
		     (cdr (assoc "value" message :test #'string=)))
	       (broadcast-message instance :state :value (assocdr :state session)))
	
	      (t
	       (send-text-message client 
				  (format nil "Unknown command '~a'~%" command))))))) 


(setf *websocket-dispatch-table* '(session-for-request))


(defvar server (make-instance 'websocket-acceptor :port *port*))


(defun start-server ()
  (start server))


(defun generate-uri ()
  (concatenate 'string (format nil "ws://localhost:~s/" *port*) "123")) ;TODO give different uuids


(defun connect-ps (set-state)
  "Produce PS code for connecting to this websocket server"
  `(let (interval (ws (ps:new (-web-socket (ps:lisp (generate-uri))))))
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
	       (peldan.ps:log-message "Connection estabilished")))
	     
       (setf onmessage
	     (lambda (msg)
	       (let ((content ((ps:@ -j-s-o-n parse) msg.data)))
		  (with-slots (type value) content
		    (case ((ps:@ type to-lower-case))
		      (:state
		       (,set-state value))
		      
		      (:pong
		       (return))
		      (t
		       (peldan.ps:log-message "Got strange message" msg)))))))
       ws)))


(defun broadcast-state (state)
  (broadcast (assocdr :instance (find-session "/123"))
	     (make-message :state :value state)))



