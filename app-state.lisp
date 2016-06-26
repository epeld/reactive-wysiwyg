(in-package :peldan.state)


(defclass app-state (stateful)
  ((action-log :initform nil :reader action-log)
   (initial-state :reader initial-state)
   (state :initarg :state :reader current-state))
  (:documentation "More concrete example of having state"))


(defmethod current-state ((instance app-state))
  (slot-value instance 'state))

(defun update-state (fn stateful)
  "Apply fn to state"
  (setf (slot-value stateful 'state)
	(funcall fn (slot-value stateful 'state))))


(defun run-action (action stateful)
  "Execute an action on the given stateful"
  (the stateful stateful)
  (the action action)
  (let ((fn (the function (eval action))))
    
    (update-state fn stateful)
    (push (slot-value stateful 'action-log)
	  action)))

(defmethod execute ((a action) (s app-state))
  (run-action a s))


(defun toggle-debug (state)
  "Toggle debug flag in state"
  (peldan.data:map-inside #'not state :debug))


