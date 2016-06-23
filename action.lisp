
(in-package :peldan.action)


(defvar *actions* (make-hash-table :test #'equal))



(defun register-action-function (action-function &optional (name (string action-function)))
  "Register a new action, passing in the action function's symbol"
  (the symbol action-function)
  (setf (gethash name *actions*)
	action-function)
  name)


(defun find-action-function (action)
  "Return the appropriate function to invoke based on the action name"
  (the string action)
  (symbol-function
   (the symbol 
	(or (gethash (string-upcase action) *actions*)
	    (error "Unkown action ~a" action)))))



(defclass stateful ()
  ((initial-state :initarg :initial-state)
   (action-log :initform nil :reader action-log))
  (:documentation "Represents something that has state"))



(defclass action ()
  ((name :initarg :name 
	 :type string
	 :reader action-name)
   (args :initarg :args 
	 :type list
	 :reader action-args
	 :initform nil))
  (:documentation
  "Represents an action invocation"))


(defun run-action (action state)
  "Run an action on the given state"
  (let ((fn (apply (find-action-function (action-name action)) 
		   (action-args action))))
    (funcall fn state)))


(defun push-action (stateful action)
  "Push an action to the stateful's action log"
  (with-slots (action-log) stateful
    (push (the action action) 
	  action-log)))


(defun compute-state (stateful &optional count)
  "Compute the state after 'count' actions have been applied to the stateful"
  (with-slots (initial-state action-log) stateful
    (loop for action in (reverse action-log)
	 for c upto (or count 1)
	 with state = initial-state
	 do (setf state (run-action action state))
	 finally (return state))))


;; Test actions

(defun toggle-debug ()
  (lambda (state)
    (peldan.data:map-inside #'not state :debug)))

(defun randomize ()
  (lambda (state)
    (peldan.data:map-inside (lambda (item)
			      (loop for i in item collect (random 100))) 
			    state
			    :data :items 10)))

(defun generate-rows (count)
  (lambda (state)
    (peldan.data:set-inside (loop for i upto (min 500 (- count 1)) collect
				 (loop for j upto 12 collect (random 100))) 
			    state
			    :data :items)))

(register-action-function 'toggle-debug "DEBUG")

(register-action-function 'generate-rows)

(register-action-function 'randomize)
