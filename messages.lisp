
(in-package :peldan.message)


(defun broadcast (session message)
  "Broadcast a message to all a session's clients"
  (loop for client in (Hunchensocket:clients session)
       do (send-message client message)))


(defun send-message (client message)
  (format t "Sending message ~a~%" message)
  (hunchensocket:send-text-message client message))

;; 
;; Messages
;; 

(defun make-message (&rest rest)
  (with-output-to-string (s)
    (yason:with-output (s)
      (peldan.data:encode-nested-plist rest))))


(defun unknown-type-message (type)
  (make-message :type :message
		:message (format nil "Unknown command '~a'~%" type)))


(defun state-message (new-state)
  "Construct a message for changing the state of a client to a new value"
  (make-message :type :state
		:value new-state))

(defun pong-message ()
  "Construct a ping message"
  (make-message :type :pong))


(defun hello-message ()
  (make-message :type :message
		:message "Hello!"))


;; 
;; Helpers
;; 

(defun resolve-symbol (alist name)
  "Given a list of (string . symbol) pairs, lookup name"
  (let ((fn (cdr (assoc name alist :test #'string-equal))))
  
    (unless fn
      (error "Unknown action ~a" name))
  
    (assert (typep fn 'symbol))
  
    (the symbol fn)))

(defun get-action (instance message)
  (let ((actions (slot-value instance 'session:actions)))
    `(,(resolve-symbol actions (getf message :name))
       ,@(getf message :args))))


(defun broadcast-state (instance)
  (broadcast instance (state-message instance)))
;; 
;; Message handlers
;; 
(defun unknown-message (instance client message)
  "Send 'unknown message' back to the client"
  (declare (ignore instance))
  (send-message client (unknown-type-message (getf message :type))))


(defun pong (instance client message)
  "Send a pong reply to client"
  (declare (ignore instance))
  (declare (ignore message))
  (the Hunchensocket:websocket-client client)
  (send-message client (pong-message)))


(defun run-action (instance client message)
  "Execute an action on the given session"
  (declare (ignore client))
  (peldan.state:execute (get-action instance message) 
			instance)
  (broadcast-state instance))
