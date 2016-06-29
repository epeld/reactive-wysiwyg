
(in-package :peldan.session)

(defparameter *message-handlers* 
  '(("ping" . message:pong)
    ("action" . message:run-action))
  "Handlers for websocket message types")


(defclass hunchensocket-client (hunchensocket:websocket-client)
  ())


(defclass identifiable ()
  ((uuid :initarg :uuid 
	 :initform (error "UUID required") 
	 :reader uuid))
  (:documentation "Supports being identified by a uuid"))
(unuse-package :hunchentoot)

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
    (the (or symbol function) handler)
    (funcall handler instance client message)))


(defmethod yason:encode ((symbol symbol) &optional stream)
  (yason:encode (string-downcase symbol)
		stream))

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
  (format t "Message from client~%")
  (handler-bind ((warning #'print-stacktrace)
		 (error 
		  (lambda (err)
		    (let ((msg (make-message :type :error
					     :error (with-output-to-string (s)
						      (print-stacktrace err s)))))
		      (message:send-message client msg))
		    (return-from hunchensocket:text-message-received))))
    
    (let ((message (yason:parse message :object-as :plist :object-key-fn #'peldan.data:find-keyword)))
      (format t "Message from client: ~s~%" message)
      (handle-client-message instance client message))))
