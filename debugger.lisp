
(in-package :peldan.debugger)


(defun debugger ()
  "Creates ps code for a debugger"
  `(psx (:div (:h2 "Debugger")
	      (:pre ((@ -j-s-o-n stringify) (peldan.component:state) nil "    "))
	      (:button :onclick (lambda () 
				  (peldan.virtual-dom:action "debug"))
		       "Back"))))

