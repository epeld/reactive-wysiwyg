(in-package :peldan.state)


(defclass app-state (stateful)
  ((action-log :initform nil :reader action-log)
   (initial-state :reader initial-state)
   (state :initarg :state :reader current-state))
  (:documentation "More concrete example of having state"))


(defmethod initialize-instance :after ((s app-state) &rest rest)
  (declare (ignore rest))
  (setf (slot-value s 'initial-state)
	(slot-value s 'state)))


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
  (let ((fn (if (= 1 (length action))
		(symbol-function (first action))
		(eval action))))
    
    (update-state (the function fn) stateful)
    (push action (slot-value stateful 'action-log))))


(defmethod execute (action (s app-state))
  (run-action action s))


(defun toggle-debug (state)
  "Toggle debug flag in state"
  (peldan.data:map-inside #'not state :debug))

