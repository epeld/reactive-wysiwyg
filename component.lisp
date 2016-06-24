
(in-package peldan.component)



(defun generate-html (&rest ps)
  "Generates a standard HTML 'frame' containing the passed in PS code"
  (cl-who:with-html-output-to-string (s)
    (:div (:small "Generated using component package")
	  (:script :type "text/javascript" (peldan.virtual-dom:library-js s))
		    
	  (:script :type "text/javascript" 
		   (let ((*parenscript-stream* s))
		     (apply #'ps* ps))))))


(defun generate-component-renderer (psx)
  "Extracts (and wraps) the PS contained in this component for appearing on a page"
  `(lambda (state)
     (peldan.ps:log-message "Rendering")
       
     ;; Allow components to references state easily
     (macrolet ((state (&rest args)
		  `(@ state ,@args)))
       (psx (:div (when (state)
		    (if (state debug)
			,(peldan.debugger:debugger)
			(psx ,psx))))))))


(defun generate-component-html (psx &key state (uuid (generate-uuid)))
  (generate-html
   
   ;; Define the component module
   `(defvar component
      (make-module ,(generate-component-renderer psx)))
   
   ;; Web socket support
   (if (and uuid (peldan.websocket:websockets-enabled))
       `(progn (setf (@ component ws)
		     ,(peldan.websocket:connect-ps state 
						   `(@ component set-state)
						   uuid))
	       (defun send-message (obj)
		 (peldan.ps:log-message "Sending" obj)
		 ((@ component ws send) (peldan.ps:json-stringify obj))))
       
       ;; Define a dummy send-message
       `(defun send-message (obj)
	  (peldan.ps:log-warning "Cannot send message" obj)))))


;; Test code
(defun test-it (req)
  (generate-component-html `(:b "Hello, " (state name)) :state '(:name "Erik" :data (:items nil))))

(push 'test-it peldan.dispatch:*handlers*)

