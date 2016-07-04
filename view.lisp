
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


(defun ps-module (view)

  `(defvar component
     (virtual-dom:make-module ,(ps-renderer view))))
