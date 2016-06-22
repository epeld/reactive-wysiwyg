
(in-package peldan.component)


(defvar *cached-virtual-dom-js*
  (peldan.virtual-dom:read-virtual-dom-js))

(defvar *cached-ps-library*
  (ps* *ps-lisp-library*))


(defgroup component)


(defun component-ps (component)
  "Extracts (and wraps) the PS contained in this component for appearing on a page"
  (let ((psx (field-value :code component)))
    `(psx (:div (when state
		  (if (@ state debug)
		      ;; Wrap in (lisp ..) form to ensure it gets reevaluated on every PS compilation
		      (lisp (peldan.debugger:debugger))

		      ;; TODO replace state by actual state later
		      (let ((state state))
			,psx)))))))


(defun component-module-ps (renderer)
  "Generate all the PS needed to render a component"
  `(let ((module (create)))
             
     ;; This is typically set when WS connection is established
     (setf (@ module state)
	   nil)
       
     (setf (@ module actions)
	   ,(peldan.action:action-ps `(@ module update-state)))
       
     (setf (@ module set-state)
	   ,(peldan.virtual-dom:render-ps renderer
					  `(@ module state)))
       
     ;; Helper for *updating* (as opposed to just *setting*) state
     (setf (@ module update-state)
	   (lambda (fn) ((@ module set-state) (funcall fn (@ module state)))))
     
     module))


(defmacro javascript (&rest strings)
  `(cl-who:htm (:script :type "text/javascript" 
			,@(loop for string in strings
			     collect (list 'cl-who:str string)))))

(defun library-js (stream)
  "Generate a string containing all the javascript needed to render components"
  (write-string *cached-virtual-dom-js* stream)
  (write-string *cached-ps-library* stream)
  (write-string (ps* `(defun make-module (renderer)
			,(component-module-ps 'renderer))
		     
		     ;; Helper function for periodically executing an action (to be moved)
		     `(defun continuously (action-name interval &rest args)
			(let ((interval (or interval 300)))
			  (set-interval (lambda ()
					  (apply #'action action-name args))
					(or interval 300))
			  interval))
		     
		     
		     ;; Execute an action (helper)
		     `(defun action (action &rest args)
			(lisp 
			 (if (peldan.websocket:websockets-enabled)
			     `(send-message (create :type :action
						    :name action
						    :args args))
			     `(apply (chain component actions run)
				     ((chain action to-lower-case))
				     args))))
		     
		     ;; Helper
		     `(defun imapcar (fn &rest args)
			"Like mapcar but adds an index as the first argument"
			(let ((is (list)))
			  (dotimes (i (length (@ args 0)))
			    ((@ is push) i))
			  (apply #'mapcar fn is args))))
		stream))


(defun title-ify (string)
  ; Currently only upper cases first letter..
  (the string string)
  (when (string= "" string)
    (error "Empty string"))
  (string-upcase (peldan.string:replace-all (string-downcase string)
					    "-" " ")
		 :start 0
		 :end 1))


(defun get-initial-state (request)
  ;; TODO later on look in request 
  nil)


(defun request-handler (request)
  "Handle requests for components"
  (let ((script-name (hunchentoot:script-name request)))
    
    (when (peldan.string:starts-with-p #1="/component/" script-name)
      
      (loop for component in (members component-group)
	 for name = (name component)
	 with requested = (subseq script-name (length #1#))
     
	 when (string-equal requested name)
	 do (return
	      (generate-component-html component (get-initial-state request)))
	   
	 finally (return (format nil "No such component '~a'" (string-downcase name)))))))


(defun generate-component-html (component &optional state)
  "Generate a string of html representing the component"
  (let ((name (name component))
	(state (or state 
		   (field-value :initial-state component))))
    
    (cl-who:with-html-output-to-string (s)
      (:div (:h1 (cl-who:str (title-ify (string name))))
	    (:script :type "text/javascript" (library-js s))
		    
	    (:script :type "text/javascript" 
		     (let ((*parenscript-stream* s))
		       (ps*
			;; A publicly available virtual DOM renderer for the component
			`(defun render (state)
			   ,(component-ps component))
				     
			;; Define the component module
			`(defvar component
			   (make-module render))
			
			(lisp 
			 
			 ;; Estabilish WebSocket Connection
			 (if (peldan.websocket:websockets-enabled)
			   `(progn (setf (@ component ws)
					 ,(peldan.websocket:connect-ps state 
								       `(@ component set-state)))
				   (defun send-message (obj)
				     (peldan.ps:log-message "Sending" obj)
				     ((@ component ws send) (peldan.ps:json-stringify obj))))
			   
			   `(defun send-message (obj)
			      (peldan.ps:log-warning "Cannot send message" obj)))))))))))



(defun register-component (name &rest args)
  "Register a new component, making it accessible by HTTP request"
  (replace-component (apply #'make-component name args)))


;; This is a test action to see how fast the updates can be
(peldan.action:register-action 
 'randomize-rows 
 :body '((let ((rows (list)))
	   (unless (defined myvar)
	     (setq myvar 0))
	   (dotimes (i 100)
	     (let ((cols (list)))
	       (dotimes (j 12)
		 (setf (aref cols j) 
		       ((@ -math round) (* 100 ((@ -math random))))))
	       (setf (aref rows i) 
		     cols))
	     (setf myvar (+ 1 (or myvar 0))))
	   (set-field rows "data" "items"))))


(defun generate-test-state ()
  (list :data (list :items (loop for i upto 100 collect 
				      (loop for j upto 12 collect
					   (random 100))))))


(register-component 'testcomponent 
 :initial-state 
 (generate-test-state)
 
 :code 
 `(psx (:div "This is a Virtual DOM element" 
		       
	     (:table
	      (:thead (:tr (mapcar (lambda (item)
				     (psx (:th "Column")))
				   (@ state data items 0))))
	      
	      (:tbody (imapcar (lambda (ix item)
				(psx (:tr (mapcar (lambda (x)
						    (psx (:td 
							  (if (eq ix 10)
							      (psx (:b x))
							      x))))
						  item))))
		      
			      (@ state data items))))
     
	     (:div :onclick (lambda () 
			      (action "hello-world"))
		   "And this is the end of it. (Rendered " 
		   (length (@ state data items))
		   " elements)"))))


(defun install-handler ()
  "Install the request handler that will maek components accessible through hunchentoot"
  (pushnew 'request-handler peldan.dispatch:*handlers*))
