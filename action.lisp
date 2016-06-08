
(in-package :peldan.action)

;; 
;; Idea:
;; defaction defines an action that can be executed both in PS and in lisp
;; actions are functions that produces function from state to state

(defgroup action)


(defun generate-code-defun (item)
  "Generate a defun from the code stored in code-item"
  `(defun ,(field-value :name item) ,(field-value :args item)
     ,@(field-value :body item)))


(defun generate-code-flet (item)
  "Generate a flet-like form from the code stored in code-item"
  `(,(field-value :name item) ,(field-value :args item)
     ,@(field-value :body item)))


(defun register-action (name lambda-list expr)
  (replace-action (make-action name
			       :args lambda-list
			       :body (list expr)
			       :action t)))


(defmacro defaction (name lambda-list expr)
  "Define the function as through defun+ps but also mark it as an action"
  `(register-action ',name ',lambda-list ',expr))



(defpsmacro with-action-context (set-state &body body)
  `(defun ((action (name &rest args)
	    (,set-state ((apply name args) state))))
     ,@body))


(defun action-ps (&optional (actions (members action-group)))
  `(let ((actions (ps:create)))
     (flet (,@(mapcar #'generate-code-defun actions))
       
       ,@(mapcar (lambda (action)
		   `(setf (ps:@ actions ,(name action))
			  ,(name action)))
		 actions))
     actions))


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


(generate-ps (members action-group))
