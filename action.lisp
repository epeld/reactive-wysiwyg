
(in-package :peldan.action)


(defgroup action)


(defmacro defaction (name lambda-list &body code)
  `(add-action (make-action ,name
			    :args (quote ,lambda-list)
			    :code (quote ,code))))



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
