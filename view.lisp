
(in-package :peldan.view)


(defclass view ()
  ((hyperscript :initarg :hyperscript
		:reader view-hyperscript)
   (name :initarg :name
	 :type string
	 :reader view-name)
   (actions :initarg :actions
	    :type list
	    :reader view-actions
	    :documentation "A list of all (symbols of) serverside actions referenced from the view"))
  (:documentation "A graphical view of some data"))


(defun make-view (hyperscript &optional (name "Anonymous view"))
  "Construct a new view by supplying its hyperscript"
  (make-instance 'view
		 :hyperscript hyperscript
		 :name name
		 :actions (ml:find-actions hyperscript)))


(defun encode-symbol (name mappings)
  "Use the mappings to translate a symbol into a string uuid"
  (let ((assoc (rassoc name mappings)))
    (unless assoc
      (error "Unknown action ~a" name))
    
    (car assoc)))


(defun view-renderer-ps (view &optional mappings)
  "Generates a renderer for the view in PS"
  `(lambda (current-state)
     (peldan.ps:log-message "Rendering")
     
     (macrolet ((state (&rest args)
		  (if args
		      `(ps:getprop current-state ,@args)
		      'current-state))

		(action (name &rest args)
		  `(,(encode-symbol name (quote ,mappings)) ,@args)))
       
       (ml:h (:div (when (state)
		     (if (state 'debug)
			 ,(peldan.debugger:debugger)
			 (peldan.ml:h ,(view-hyperscript view))))
		   (:small "Generated using component package"))))))




(defun view-ps (view &key session (name 'component))
  ;; TODO check that all actions are known to session!
  (when (and (view-actions view)
	     (or (null session)
		 (endp (session:session-actions session))))
    (error "Cannot encode actions ~a because unknown to session" (view-actions view)))
  `(progn 
     
     (defvar ,name
       (virtual-dom:make-module 
	,(view-renderer-ps view (and session
				     (session:session-actions session)))))
     
     
     ,(if session
	  ;; With Session
	  (let ((uuid (session:uuid session))) 
	    `(progn
	       (peldan.ps:log-message "Using Server session" ,uuid)
	    
	       (setf (@ ,name ws)
		     ,(peldan.websocket:connect-ps `(ps:@ ,name set-state) uuid))
	      
	       (defun send-message (obj)
		 (peldan.ps:log-message "Sending" obj)
		 ((ps:@ ,name ws send) (peldan.ps:json-stringify obj)))))
	    

	  ;; Without session
	  `(progn
	     (peldan.ps:log-message "Serverless.")
	       
	     (defun send-message (obj)
	       (peldan.ps:log-warning "Cannot send message" obj))
	       
	     ((ps:@ ,name set-state) (peldan.ps:json-parse "{}"))))))
