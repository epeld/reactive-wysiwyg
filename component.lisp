
(in-package peldan.component)


(defun generate-component-renderer (h)
  "Extracts (and wraps) the PS contained in this component for appearing on a page"
  `(lambda (state)
     (peldan.ps:log-message "Rendering")
       
     ;; Allow components to references state easily
     (macrolet ((state (&rest args)
		  `(@ state ,@args)))
       (peldan.ml:h (:div (when (state)
			    (if (state debug)
				,(peldan.debugger:debugger)
				(peldan.ml:h ,h)))
			  (:small "Generated using component package"))))))


(defun component-module-ps (h)
  "Generates the PS that defines a module (renderer + setState)"

  ;; Define the component module
  `(defvar component
     (make-module ,(generate-component-renderer h))))


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


(defun component-session-ps (session-uuid)
  ;; Web socket support
  (if session-uuid
      (component-session-ps session-uuid 'component)
      (component-dummy-session 'component))
  
  ;; TODO when server-less, call set-state with snapshot of state!
  )



(defun generate-component-html (h &key (session-uuid (generate-uuid)) (include-libraries t))
  (when (and session-uuid (not (peldan.websocket:websockets-enabled)))
    (error "Websocket server not started"))
  
  (cl-who:with-html-output-to-string (s)
    (:div (when include-libraries
	    (htm (:script :type "text/javascript" (peldan.virtual-dom:library-js s)))
	    (htm (:script :type "text/javascript" (peldan.ps:generate-user-js s))))
		    
	  (:script :type "text/javascript" 
		   (let ((*parenscript-stream* s))
		     (ps* (component-module-ps h)
			  (component-session-ps session-uuid)))))))





