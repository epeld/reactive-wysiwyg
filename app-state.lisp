(in-package :peldan.state)


(defclass app-state (stateful)
  ((initial-state :reader initial-state)
   (state :initarg :state :reader current-state))
  (:documentation "More concrete example of having state"))


(defmethod initialize-instance :after ((s app-state) &rest rest)
  (declare (ignore rest))
  (setf (slot-value s 'initial-state)
	(slot-value s 'state)))


(defmethod current-state ((instance app-state))
  (slot-value instance 'state))


(defmethod update-state (fn (stateful app-state))
  "Apply fn to state"
  (setf (slot-value stateful 'state)
	(funcall fn (slot-value stateful 'state))))


(defun toggle-debug (state)
  "Toggle debug flag in state"
  (peldan.data:map-inside #'not state :debug))

