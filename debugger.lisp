
(in-package :peldan.debugger)


(defun action-list (actions)
  (let ((action-names (mapcar (lambda (action)
				(string-downcase (peldan.resource:name action)))
			      actions)))
    `(psx (:div "Available Actions:"
		(:select (mapcar (lambda (action)
				   (psx (:option action)))
				 (list ,@action-names)))))))


(defun debugger ()
  "Creates ps code for a debugger"
  `(psx (:div (:h2 "Debugger")
	      (:pre ((@ -j-s-o-n stringify) peldan.component:state nil "    "))
	      (lisp (action-list (peldan.action:list-of-actions)))
	      (:button :onclick (action debug)
		       "Back"))))

