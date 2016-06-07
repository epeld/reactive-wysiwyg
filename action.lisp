
(in-package :peldan.action)

;; 
;; Idea:
;; defaction defines an action that can be executed both in PS and in lisp
;; actions are functions that produces function from state to state

(defgroup action)


;; TODO this is not specific to actions. There may be lots of code that can be shared in this fashion!
(defmacro defaction (name lambda-list expr)
  `(progn
     ;; This defines the action in lisp
     (defun ,name ,lambda-list
       ,(format nil "Return a state endofunction representing the action ~a on the current state" name)
       ,expr)
     
     ;; This stores the action source so that it can be made available to PS code later
     (add-action (make-action ,name
			    :args (quote ,lambda-list)
			    :code (quote ,expr)))))


(defun find-action-function (name)
  (let ((action (find-action name)))
    (unless action
      (error "Unknown action ~a" name))
    `(defun ,name (state)
       )))


(defun perform-action (name &rest args)
  "Invoke an action, given its name and arguments"
  (let ((action (find-action-function name)))
    (apply action args)))




(defpsmacro action (name &rest args)
  "This executes an action when I get around to it!"
  (if *server-side-logic*
      "serverside"
      "clientside"))


(defun create-actions-ps ()
  "Creates postscript code for executing each action"
  (let ((actions (unique-members action-group :name)))
    `(let ()
       ,@(loop for action in actions collect
	    `(defun ,(name action) ,(field-value :args action)
	       ,@(field-value :code action))))))
