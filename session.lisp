
(in-package :peldan.session)


(defclass base-client (hunchensocket:websocket-client)
  ())

(defclass base-session (hunchensocket:websocket-resource)
  ((uuid :initarg :uuid 
	 :initform (error "UUID required") 
	 :reader uuid)
   (actions :initarg :actions
	    :reader actions
	    :type list
	    :documentation "an alist of (string . symbol) mapping strings to actions"))
  (:default-initargs :client-class 'base-client))


(defclass app-session (base-session)
  ()
  (:documentation "A standard app session"))

(defgeneric execute-action (session name &rest args)
  (:documentation "Execute an action named 'name' on the session"))

(defgeneric state-message (session)
  (:documentation "Compose a string describing the state of the session"))


(defgeneric message-received (session type message)
  (:documentation "Process a message of type 'type', returning the reply as a string"))


;; 
;; Helpers
;;  

(defun register-actions (session new-actions &key (use-uuids t))
  "Register a bunch of new actions, assigning them random uuids if they do not already have mappings"
  (with-slots (actions) session
    (let ((new-mappings (loop for action in new-actions
			   unless (find action actions 
					:key #'car)
			   collect (cons (if use-uuids (peldan.string:generate-uuid) (string-downcase action))
					 action))))
      (setf actions 
	    (union actions new-mappings)))))


(defun print-stacktrace (c &optional (stream *standard-output*))
  (format stream "Error: ~a~%~%~%" c)
  (sb-debug:print-backtrace :stream stream :count 15))


;; 
;; Event handling
;; 
(defmethod hunchensocket:client-connected ((instance base-session) client)
  (format t "Client connected to ~a!~%" (uuid instance))
  (send-message client (hello-message))
  (format t "Said hello.")
  (send-message client (state-message instance)))


(defmethod hunchensocket:client-disconnected ((instance base-session) client)
  (declare (ignore instance))
  (format t "Client disconnected!~%"))


(defmethod hunchensocket:text-message-received ((instance base-session) client message)
  (format t "Message from client: ~s~%" message)
  (setq message (yason:parse message :object-as :plist :object-key-fn #'peldan.data:find-keyword))
  (let ((type (getf message :type)))
    (message:send-message client (message-received instance type message))))


(defmethod hunchensocket:text-message-received :around ((instance base-session) client message)
  (handler-bind ((error 
		  (lambda (err)
		    (format t "Got error ~&~a~% so will not process futher" err)
		    (message:send-message client 
					  (message:error-message (with-output-to-string (s)
							   (print-stacktrace err s))))
		    (return-from hunchensocket:text-message-received))))
    
    (call-next-method)))

;; 
;; Message handling
;; 

(defmethod message-received ((session base-session) type message)
  (format t "Unkown message ~a! sent to ~a" type session)
  (message:unknown-type-message type))


(defmethod message-received ((session base-session) (type (eql :ping)) message)
  (format t "Ping message!")
  (message:pong-message))


(defmethod message-received ((session base-session) (type (eql :action)) message)
  (format t "Action message!")
  (let ((name (getf message :name))
	(args (getf message :args)))
  (apply #'execute-action session name args)
  (state-message session)))

(defmethod execute-action ((session base-session) name &rest args)
  (warn "Unknown action ~a called with args ~s" name args))
