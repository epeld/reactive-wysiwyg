
(in-package :peldan.view)


(defclass view ()
  ((hyperscript :initarg :hyperscript
		:reader view-hyperscript)
   (name :initarg :name
	 :type string
	 :reader view-name))
  (:documentation "A graphical view of some data"))


(defun make-view (hyperscript &optional (name "Anonymous view"))
  "Construct a new view by supplying its hyperscript"
  (make-instance 'view
		 :hyperscript hyperscript
		 :name name))


(defun encode-symbol (name mappings)
  "Use the mappings to translate a symbol into a string uuid"
  (let ((assoc (rassoc name mappings)))
    (unless assoc
      (error "Unknown action ~a" name))
    
    (car assoc)))


(defun view-actions (view)
  "Find all (the symbols of) serverside actions of a view"
  (ml:find-actions (view-hyperscript view)))


(defun view-renderer-ps (view)
  "Generates a renderer for the view in PS"
  `(lambda (state)
     (peldan.ps:log-message "Rendering")
     
     (macrolet ((state (&rest args)
		  `(ps:getprop state ,@args))

		(action (name &rest args)
		  `(,(encode-symbol name (view-mappings view)) ,@args)))
       
       (ml:h (:div (when (state)
		     (if (state 'debug)
			 ,(peldan.debugger:debugger)
			 (peldan.ml:h ,(view-hyperscript view))))
		   (:small "Generated using component package"))))))




(defun view-ps (view &key session (name 'component))
  `(progn 
     
     (defvar ,name
	    (virtual-dom:make-module ,(view-renderer-ps view)))
     
     
     ,(if session
	  ;; With Session
	  (let ((uuid (session:uuid session))) 
	    `(progn
	       (peldan.ps:log-message "Using Server session" ,uuid)
	    
	       (setf (@ ,name ws)
		     ,(peldan.websocket:connect-ps `(@ ,name set-state) uuid))
	      
	       (defun send-message (obj)
		 (peldan.ps:log-message "Sending" obj)
		 ((@ ,name ws send) (peldan.ps:json-stringify obj)))))
	    

	  ;; Without session
	  `(progn
	     (peldan.ps:log-message "Serverless.")
	       
	     (defun send-message (obj)
	       (peldan.ps:log-warning "Cannot send message" obj))
	       
	     ((@ ,name set-state) (peldan.ps:json-parse "{}"))))))
