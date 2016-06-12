
(in-package :peldan.debugger)

(peldan.component:register-component 
 'debugger ()
 `(:div (:h2 "Debbuging!")
	(:pre ((@ -j-s-o-n stringify) (@ peldan.component:state component state) nil "    "))
	(:button :onclick (action debug)
		 "Back")))


;; TODO create a dropdown component for selecting actions


;; TODO create an action timeline component
