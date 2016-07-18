
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



(defun view-renderer-ps (view)
  "Generates a renderer for the view in PS"
  
  `(lambda (server-state local-state temp-state)
     
     ;; Ensure server state and local state are defined
     (unless local-state
       (setf local-state (ps:create)))
     
     (unless server-state
       (setf server-state (ps:create)))
     
     (unless temp-state
       (setf temp-state (ps:create)))
     
     (macrolet ((state (&rest args)
		  (if args
		      `(ps:getprop server-state ,@args)
		      'server-state))
		
		(local-state (&rest args)
		  `(ps:getprop local-state ,@args))
		
		(temp (&rest args)
		  `(or (ps:getprop temp-state ,@args) 
			      nil)))
       
       (ml:h (:div (when (state)
		     (if (state 'debug)
			 ,(peldan.debugger:debugger)
			 (peldan.ml:h ,(view-hyperscript view))))
		   (:small "Generated using component package"))))))


(defun verified-session-actions (session actions)
  "Behaves like (session:session-actions) but also verifies that the returned map
contains all the actions of the second argument"

  (when actions
      
    ;; No backend?
    (unless session
      (error "View contains serverside actions ~a, but no session specified" actions))
      
    (let* ((mappings (session:session-actions session))
	   (diff (set-difference actions (mapcar #'cdr mappings)))) 
    
      ;; Make sure the session knows about all our actions
      (when diff
	  (error "Cannot encode actions ~a because unknown to session" diff))
      
      mappings)))


(defun view-module-ps (view &optional (session *default-session*))
  "PS: Return an object representing the view and with an (optional) connection to the server-side session"
  (let ((actions (verified-session-actions session (view-actions view)))
	(initial-state (if session 
			   (session:state-message session)
			   "{}")))
    
    `(let* (module
	    (ws ,(if session 
		     (websocket:session-websocket-ps session)
		     (websocket:dummy-websocket-ps))))
	 
       (setf module 
	     (virtual-dom:make-module 
	      (macrolet (
			 ;; Client side storage:
			 (store-to-temp (name) 
			   `(lambda (ev)
			      ((@ module set-temp) (ps:create ,name (@ ev target value)))))
			 
			 (clear-temp (name) 
			   `(lambda ()
			      ((@ module set-temp) (ps:create))))
			 
			 ;; Server side actions:
			 (action (name &rest args)
			   `((@ ws send-message) 
			     (ps:create :type :action
					:name ,(encode-symbol name (quote ,actions))
					:args (list ,@args)))))
		,(view-renderer-ps view))))
       
       (setf (@ module ws)
	     ws)
       
       (setf (@ ws onstate)
	     (@ module set-state))
       
       ;; Set initial state
       ((@ module set-state) 
	(ps-util:json-parse 
	 ,initial-state))
       
       module)))
