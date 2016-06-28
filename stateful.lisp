
(in-package :peldan.state)


(defclass stateful ()
  ((action-log :initform nil :reader action-log))
  (:documentation "An abstract conept of having state"))


(defgeneric update-state (fn stateful)
  (:documentation "apply a function over a stateful's state. Returning the new state"))


(defun callp (list)
  (symbolp (first list)))

(deftype action ()
  `(and list
       (satisfies callp)))


(defun action-to-state-lambda (action)
  "Converts an action into a state 'endolambda'"
  (the action action)
  (assert (symbol-function (first action)))
  (lambda (state)
    (apply (first action) state (rest action))))


(defun execute (action stateful)
  "Execute an action on a stateful"
  (update-state (action-to-state-lambda action) stateful)
  (push action (slot-value stateful 'action-log)))



