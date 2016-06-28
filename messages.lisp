
(in-package :peldan.websocket)



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
  (let ((actions (slot-value instance 'actions)))
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
  (send-text-message client (unknown-type-message (getf message :type))))


(defun pong (instance client message)
  "Send a pong reply to client"
  (declare (ignore instance))
  (declare (ignore message))
  (the websocket-client client)
  (send-message client (pong-message)))


(defun run-action (instance client message)
  "Execute an action on the given session"
  (declare (ignore client))
  (the session instance)
  (peldan.state:execute (get-action instance message) 
			instance)
  (broadcast-state instance))
