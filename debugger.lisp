
(in-package :peldan.debugger)


(defun action-list (actions)
  (let ((action-names (mapcar (lambda (action)
				(string-downcase (peldan.resource:name action)))
			      actions)))
    `(psx (:div "Available Actions:"
		(:select (mapcar (lambda (action)
				   (psx (:option action)))
				 (list ,@action-names)))))))


(peldan.component:register-component 
 'debugger ()
 `(:div (:h2 "Debbuging!")
	(:pre ((@ -j-s-o-n stringify) (@ peldan.component:state component state) nil "    "))
	(lisp (action-list (peldan.action:list-of-actions)))
	(:button :onclick (action debug)
		 "Back")))




;; TODO create an action timeline component
(sxhash 'peldan.ps:stringify)
(sxhash 'stringify)
