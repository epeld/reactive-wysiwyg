
(in-package :peldan.view)


(defclass view ()
  ((hyperscript :initarg :hyperscript
		:reader view-hyperscript)
   (mappings :reader view-mappings
	     :documentation "An alist of mappings from string to symbol
that helps translate hyperscript to parenscript"))
  (:documentation "A graphical view of some data"))


(defun make-view (hyperscript &optional mappings)
  "Construct a new view by supplying its hyperscript"
  (unless mappings
    (setq mappings (data:generate-mappings (ml:find-actions hyperscript))))
  
  (make-instance 'view
		 :hyperscript hyperscript
		 :mappings mappings))


(defun encode-symbol (view name)
  "Use the view's mappings to translate a symbol into a string uuid"
  (let ((assoc (rassoc name (view-mappings view))))
    (unless assoc
      (error "Unknown action ~a" name))
    
    (car assoc)))


(defun ps-renderer (view)
  "Generates a renderer for the view in PS"
  `(lambda (state)
     (peldan.ps:log-message "Rendering")
     
     (macrolet ((state (&rest args)
		  `(ps:getprop state ,@args))

		(action (name &rest args)
		  `(,(encode-symbol view name) ,@args)))
       
       (ml:h (:div (when (state)
		     (if (state 'debug)
			 ,(peldan.debugger:debugger)
			 (peldan.ml:h ,(view-hyperscript view))))
		   (:small "Generated using component package"))))))




(defun ps-view (view &key session (name 'component))
  `(progn 
     
     (defvar ,name
	    (virtual-dom:make-module ,(ps-renderer view)))
     
     
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
