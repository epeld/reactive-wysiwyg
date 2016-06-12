
(in-package :peldan.action)

;; 
;; Idea:
;; defaction defines an action that can be executed both in PS and in lisp
;; actions are functions that produces function from state to state

(defgroup action)


(defun generate-action-lambda (item)
  `(lambda ,(field-value :args item)
     ,@(field-value :body item)))


(defun generate-action-defun (item update-state)
  `(defun ,(field-value :name item) ,(field-value :args item)
     (update-state (action-))))


(defun register-action (name lambda-list expr)
  (replace-action (make-action name
			       :args lambda-list
			       :body (list expr)
			       :action t)))


(defmacro defaction (name lambda-list expr)
  "Define the function as through defun+ps but also mark it as an action"
  `(register-action ',name ',lambda-list ',expr))


;; TODO remove macro, move into application-js somehow
(defpsmacro action (name &rest args)
  `(lambda () 
     ((@ module actions do) (@ module actions ,name) ,@args)))


(defun action-ps (update-state &optional (actions (members action-group)))
  `(let ((actions (ps:create :map (ps:create)))
	 ,@(mapcar #'name actions))
     
     ;; Perform an action by calling its lambda
     ;; and then using that to modify state
     (setf (@ actions run)
	   (lambda (name &rest args)
	     ;; TODO later store all actions
	     (peldan.ps:log-message "Action: " 
				    (peldan.ps:json-stringify (ps:create :name name :args args)))
	     
	     (let ((fn (getprop (chain module actions :map) name)))
	       (if (defined fn) 
		   (apply (@ actions do) fn args)
		   (peldan.ps:log-warning "Unkown action" name)))))
     

     (setf (@ actions do)
	   (lambda (fn &rest args)
	     (,update-state (apply fn args))))

     
     ,@(loop for action in actions collect 
	    `(progn 

	       ;; This defines the action's 'lambda' -
	       ;; a function that produces a state endofunction
	       (setf (chain actions ,(name action))
		     (lambda (,@(field-value :args action))
		       ,@(field-value :body action)))
	       

	       (setf (@ actions :map ,(string-downcase (name action)))
		     (chain actions ,(name action)))
	       
	       ;; So actions can refer to eachother
	       (setf ,(name action)
		     (@ actions ,(name action)))))
     actions))


(defun list-of-actions ()
  (members action-group))


;; TODO generate defuns that modify state here!
(defun shortcuts-ps ()
  'todo)


(defaction set-field (val &rest keys)
  (apply #'update-field
	 (lambda (ignored) val)
	 keys))


(defaction update-field (fn &rest keys)
  (flet ((set-field-in-state (state)
		  (let ((obj state))
	     
		    ;; Walk keys until only one remains..
		    (dotimes (ix (- (length keys) 1))
		      (let ((key (aref keys ix)))
		 
			;; Creating empty objects when necessary
			(unless (defined (ps:getprop obj key))
			  (setf (ps:getprop obj key)
				(ps:create)))
		 
			(setf obj (ps:getprop obj key))))
	     
		    (let ((current (ps:getprop obj (aref keys (- (length keys) 1)))))
		      (setf (ps:getprop obj (aref keys (- (length keys) 1)))
			    (funcall fn current))))
	   
		  state))
	   #'set-field-in-state))


(defaction debug ()
  (update-field (lambda (val)
		  (if val 0 1))
		"debug"))

