
(in-package :peldan.websocket)

(defparameter *message-handlers* 
  `(("ping" . pong)
    ("action" . run-action))
  "Handlers for websocket message types")


(defclass hunchensocket-client (websocket-client)
  ())


(defclass identifiable ()
  ((uuid :initarg :uuid 
	 :initform (error "UUID required") 
	 :reader uuid))
  (:documentation "Supports being identified by a uuid"))


(defclass session (hunchensocket:websocket-resource identifiable)
  ((actions :initarg :actions
	    :initform '(("debug" . peldan.state:toggle-debug))
	    :reader actions
	    :type list
	    :documentation "an alist of (string . symbol) mapping strings to actions"))
  (:default-initargs :client-class 'hunchensocket-client))

; (remove-method #'initialize-instance (find-method #'initialize-instance '(:after) '(stateful)))

;; After every action, broadcast state to clients
(defmethod peldan.state:execute :after (action (s session))
  (broadcast s (state-message (peldan.state:current-state s))))


(defclass app-session (peldan.state:app-state session)
  ()
  (:documentation "A standard app session"))


;; TODO make this contain a list of all normal sessions!
(defclass meta-session (session)
  () 
  (:documentation "A session about sessions"))





(defun print-stacktrace (c)
  (format *standard-output* "Error: ~a~%~%~%" c)
  (sb-debug:print-backtrace :stream *standard-output*
			    :count 15))


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
  (send-message client (state-message (peldan.state:current-state instance))))


(defmethod client-disconnected ((instance session) client)
  (declare (ignore instance))
  (format t "Client disconnected!~%"))


(defmethod text-message-received ((instance session) client message)
  (handler-bind ((warning #'print-stacktrace)
		 (error #'print-stacktrace))
    (let ((message (parse message :object-as :plist :object-key-fn #'peldan.data:find-keyword)))
      (format t "Message from client: ~s~%" message)
      (handle-client-message instance client message))))
