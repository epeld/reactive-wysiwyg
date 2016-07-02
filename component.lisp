
(in-package peldan.component)



(defun generate-html (&rest ps)
  "Generates a standard HTML 'frame' containing the passed in PS code"
  (cl-who:with-html-output-to-string (s)
    (:div (:script :type "text/javascript" (peldan.virtual-dom:library-js s))
	  (:script :type "text/javascript" (peldan.ps:generate-user-js s))
		    
	  (:script :type "text/javascript" 
		   (let ((*parenscript-stream* s))
		     (apply #'ps* ps)))
	  (:small "Generated using component package"))))


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
				(peldan.ml:h ,h))))))))


(defun generate-component-html (h &key (session-uuid (generate-uuid)))
  (when (and session-uuid 
	     (not (peldan.websocket:websockets-enabled)))
    (error "Websocket server not started"))
  
  (generate-html
   
   ;; Define the component module
   `(defvar component
      (make-module ,(generate-component-renderer h)))
   
   ;; Web socket support
   (if session-uuid
       `(progn (peldan.ps:log-message "Server session provided")
	       (setf (@ component ws)
		     ,(peldan.websocket:connect-ps `(@ component set-state)
						   session-uuid))
	       (defun send-message (obj)
		 (peldan.ps:log-message "Sending" obj)
		 ((@ component ws send) (peldan.ps:json-stringify obj))))
       
       ;; Define a dummy send-message
       `(progn (peldan.ps:log-message "Serverless.")
	       
	       (defun send-message (obj)
		 (peldan.ps:log-warning "Cannot send message" obj))
	       
	       ;; TODO pass state to client as JSON
	       ((@ component set-state) (peldan.ps:json-parse "{}"))))))





