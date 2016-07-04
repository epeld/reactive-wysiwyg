
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


(defun pong-message ()
  "Construct a ping message"
  (make-message :type :pong))


(defun hello-message ()
  (make-message :type :message
		:message "Hello!"))

(defun error-message (text)
  (make-message :type :error
		:error text))



