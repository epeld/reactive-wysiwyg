;; Define the websocket interface of this server

(in-package :peldan.websocket)


(defparameter *port* 3344)

(defparameter *ping-interval* 10000)


(defparameter *meta*
  (make-instance 'session:meta-session
		 :uuid "meta")
  "The session of sessions")

(defun request-handler (request)
  "Hunchensocket request dispatch function"
  (let ((uuid (subseq (hunchentoot:script-name request) 
		      1)))
    (format t "~&Got WS request for ~a~%" uuid)
    (session:find-session uuid *meta*)))


(setf hunchensocket:*websocket-dispatch-table* '(request-handler))


(defvar server (make-instance 'hunchensocket:websocket-acceptor :port *port*))


(defun start-server ()
  (hunchentoot:start server))


(defun generate-uri (uuid)
  (concatenate 'string (format nil "ws://localhost:~s/" *port*) uuid))


(defun connect-ps (set-state uuid)
  "Produce PS code for connecting to this websocket server"
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
	       (let ((content (peldan.ps:json-parse (ps:@ msg data))))
		 (with-slots (type value) content
		   (case type
		     (:error
		      (peldan.ps:log-warning "Server Error:" (ps:@ content error)))

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

