
(in-package :peldan.debugger)

(peldan.component:register-component
 'action-list ()
 ;; TODO this is actually evaluated during "component compile-time".
 ;; figure out a way to make it happen when component is 'called'!
 (let ((action-names (mapcar (lambda (action)
			       (string-downcase (peldan.resource:name action)))
			     (peldan.action:list-of-actions))))
   `(:div "Available Actions:"
	  (:select (mapcar (lambda (action)
			     (psx (:option action)))
			   (list ,@action-names))))))


(peldan.component:register-component 
 'debugger ()
 `(:div (:h2 "Debbuging!")
	(:pre ((@ -j-s-o-n stringify) (@ peldan.component:state component state) nil "    "))
	(peldan.component:subcomponent action-list nil)
	(:button :onclick (action debug)
		 "Back")))


;; TODO create a dropdown component for selecting actions


;; TODO create an action timeline component
