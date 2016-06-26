
(in-package :peldan.state)


(defclass stateful ()
  ()
  (:documentation "An abstract conept of having state"))


(defgeneric current-state (stateful)
  (:documentation "Return the current state"))


(defun callp (list)
  (symbolp (first list)))

(deftype action ()
  `(and list
       (satisfies callp)))


(defgeneric execute (action stateful)
  (:documentation "Execute an action on the stateful"))






