
(in-package peldan.component)


;; TODO this should be moved into the actual page (html) generation
(defun component-module-ps (h)
  "Generates the PS that defines a module (renderer + setState)"

  ;; Define the component module
  `(defvar component
     (virtual-dom:make-module ,(generate-component-renderer h))))


(defun component-server-session-ps (session-uuid &optional (component 'component))
  "Generate the PS to connect to a server web socket-session.
Optionally takes the symbol used to refer to the component module"
  `(progn (peldan.ps:log-message "Using Server session" ,session-uuid)

	  (setf (@ ,component ws)
		,(peldan.websocket:connect-ps `(@ ,component set-state)
					      session-uuid))
	  (defun send-message (obj)
	    (peldan.ps:log-message "Sending" obj)
	    ((@ ,component ws send) (peldan.ps:json-stringify obj)))))


(defun component-dummy-session (&optional (component 'component))
  "Generate a dummy server WS-session. This is useful when rendering a component
that presupposes a server session"

  ;; Define a dummy send-message
  `(progn (peldan.ps:log-message "Serverless.")
	       
	  (defun send-message (obj)
	    (peldan.ps:log-warning "Cannot send message" obj))
	       
	  ((@ ,component set-state) (peldan.ps:json-parse "{}"))))


(defun component-session-ps (session)
  ;; Web socket support
  (if session
      (component-server-session-ps session 'component)
      (component-dummy-session))
  
  ;; TODO when server-less, call set-state with snapshot of state!
  )


;; just generate the javascript
(defun generate-component-js (hyperscript &key 
				  session
				  (stream *standard-output*))

  ;; Parenscript
  (let ((*parenscript-stream* stream))
    (ps* (component-module-ps hyperscript)
	 (component-session-ps session))))


