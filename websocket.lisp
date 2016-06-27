;; Define the websocket interface of this server

(in-package :peldan.websocket)


(defparameter *port* 3344)

(defparameter *ping-interval* 10000)

(defparameter *sessions* nil)


(defun request-handler (request)
  "Hunchensocket request dispatch function"
  (let ((uuid (subseq (script-name request) 
		      1)))
    (format t "~&Got WS request for ~a" uuid)
    (find uuid *sessions* 
	  :key #'uuid
	  :test #'string-equal)))


(defun broadcast (session message)
  "Broadcast a message to all a session's clients"
  (loop for client in (clients session)
       do (send-message client message)))


(defun send-message (client message)
  (format t "Sending message ~a" message)
  (send-text-message client message))



(setf *websocket-dispatch-table* '(request-handler))


(defvar server (make-instance 'websocket-acceptor :port *port*))


(defun start-server ()
  (start server))


(defun generate-uri (uuid)
  (concatenate 'string (format nil "ws://localhost:~s/" *port*) uuid)) ;TODO give different uuids


;; TODO split up session creation from PS generation
;; TODO consider NOT creating a session here ever. 
;; instead, demand that session was created before-hand
;; have a mechanism for automatic clean-up of expired sessions
;; TODO make use client IP to generate a session UUID? same client gets same session
(defun connect-ps (initial-state set-state uuid)
  "Produce PS code for connecting to this websocket server"
  ;; Ensure session exists with state
  (session-with-uuid uuid initial-state)
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
       ws)))


(defun websockets-enabled ()
  (hunchentoot:started-p server))

