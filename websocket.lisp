;; Define the websocket interface of this server

(in-package peldan.websocket)


(defgroup session)


(defparameter *port* 3344)


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
      (let ((command (cdr (assoc "command" message :test #'string=))))
	    (cond
	      ((string= "set" command)
	       (setf (cdr (assoc :state session))
		     (cdr (assoc "value" message :test #'string=))))
	
	      (t
	       (send-text-message client 
				  (format nil "Unknown command '~a'~%" command)))))
      
      ;; Response generation
      (let ((response (with-output-to-string (s)
			(encode (cdr (assoc :state session)) 
				s))))
	(format t "Response ~s~%" response)
	(broadcast instance response)))) 


(setf *websocket-dispatch-table* '(session-for-request))


(defvar server (make-instance 'websocket-acceptor :port *port*))


(defun start-server ()
  (start server))
