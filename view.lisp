
(in-package :peldan.view)

(defvar *default-session* nil
  "The fallback session when you are lazy and don't care to create a new session")

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
  
  ;; TODO introduce temporary state here as well
  `(lambda (current-state)
     (peldan.ps:log-message "Rendering")
     
     (macrolet ((state (&rest args)
		  (if args
		      `(ps:getprop current-state ,@args)
		      'current-state))

		(action (name &rest args)
		  `(send-message (ps:create :type :action
					    :name ,(encode-symbol name (quote ,mappings))
					    :args (list ,@args)))))
       
       (ml:h (:div (when (state)
		     (if (state 'debug)
			 ,(peldan.debugger:debugger)
			 (peldan.ml:h ,(view-hyperscript view))))
		   (:small "Generated using component package"))))))




(defun view-ps (view &key (session *default-session*) (name 'component))
  
  (let ((actions (view-actions view))
	mappings)
    
    ;; TODO move this part into separate function, e.g
    ;; (check-session-actions)
    (when actions
      
      ;; No backend?
      (unless session
	(error "View contains serverside actions ~a, but no session specified" actions))
      
      (setq mappings (session:session-actions session))
    
      ;; Make sure the session knows about all our actions
      (let ((diff (set-difference actions (mapcar #'cdr mappings))))
	(when diff
	  (error "Cannot encode actions ~a because unknown to session" diff))))
    
    `(progn 
     
       ;; TODO don't do defvar here.
       ;; instead just return a module with a connection established (if requested)
       (defvar ,name
	 (virtual-dom:make-module 
	  ,(view-renderer-ps view mappings)))
     
     
       ;; TODO remove the branch here
       ,(if session
	    ;; With Session
	    (let ((uuid (session:uuid session))) 
	      `(progn
		 ;; TODO move this log message into (session-connect-ps) below
		 (peldan.ps:log-message "Using Server session" ,(format nil "~a" (type-of session)) ,uuid)
	    
		 ;; TODO always set (@ ,name ws)
		 ;; but set it to a dummy when session is missing
		 ;; e.g (session-connection-ps)
		 (setf (ps:@ ,name ws)
		       ,(peldan.websocket:connect-ps uuid))
	      
		 ;; TODO define this the same, regardless of using session or not,
		 ;; just change the ws object instead
		 (defun send-message (obj)
		   (peldan.ps:log-message "Sending" obj)
		   ((ps:@ ,name ws send) (peldan.ps:json-stringify obj)))))
	    

	    ;; Without session
	    `(progn
	       (peldan.ps:log-message "Serverless.")
	       
	       (defun send-message (obj)
		 (peldan.ps:log-warning "Cannot send message" obj))
	       
	       ((ps:@ ,name set-state) (peldan.ps:json-parse "{}")))))))
