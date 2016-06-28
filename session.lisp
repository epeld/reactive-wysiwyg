
(in-package :peldan.websocket)

(defparameter *message-handlers* 
  '(("ping" . pong)
    ("action" . run-action))
  "Handlers for websocket message types")


(defclass hunchensocket-client (websocket-client)
  ())


(defclass identifiable ()
  ((uuid :initarg :uuid 
	 :initform (error "UUID required") 
	 :reader uuid))
  (:documentation "Supports being identified by a uuid"))


(defclass base-session (hunchensocket:websocket-resource identifiable)
  ((actions :initarg :actions
	    :reader actions
	    :type list
	    :documentation "an alist of (string . symbol) mapping strings to actions"))
  (:default-initargs :client-class 'hunchensocket-client))


(defclass app-session (peldan.state:app-state base-session)
  ()
  (:documentation "A standard app session")
  (:default-initargs :actions '(("debug" . peldan.state:toggle-debug))))


(defmethod yason:encode ((s base-session) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "uuid" (uuid s)))))


(defmethod yason:encode ((s app-session) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "uuid" (uuid s))
      (yason:encode-object-element "log" (slot-value s 'peldan.state:action-log))
      (yason:with-object-element ("data")
	(peldan.data:encode-nested-plist (slot-value s 'peldan.state:state))))))


;(yason:encode-object-element "data" (slot-value s 'peldan.state:app-state))






;; 
;; Helpers
;;  

(defun print-stacktrace (c &optional (stream *standard-output*))
  (format stream "Error: ~a~%~%~%" c)
  (sb-debug:print-backtrace :stream stream :count 15))


(defun find-handler (type &optional (handlers *message-handlers*))
  "Find an appropriate handler based on a message type"
  (the (or symbol function)
       (or (cdr (assoc type handlers :test #'string-equal))
	   #'unknown-message)))


(defun handle-client-message (instance client message)
  (let ((handler (find-handler (getf message :type))))
    (funcall handler instance client message)))


;; 
;; Event handling
;; 
(defmethod client-connected ((instance session) client)
  (format t "Client connected to ~a!~%" (uuid instance))
  (send-message client (hello-message))
  (send-message client (state-message instance)))


(defmethod client-disconnected ((instance session) client)
  (declare (ignore instance))
  (format t "Client disconnected!~%"))


(defmethod text-message-received ((instance session) client message)
  
  (handler-bind ((warning #'print-stacktrace)
		 (error 
		  (lambda (err)
		    (let ((msg (make-message :type :error
					     :error (with-output-to-string (s)
						      (print-stacktrace err s)))))
		      (send-message client msg))
		    (return-from text-message-received))))
    
    (let ((message (parse message :object-as :plist :object-key-fn #'peldan.data:find-keyword)))
      (format t "Message from client: ~s~%" message)
      (handle-client-message instance client message))))
