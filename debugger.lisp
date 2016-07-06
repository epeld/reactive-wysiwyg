
(in-package :peldan.debugger)


(defun debugger ()
  "Creates ps code for a debugger"
  `(ml:h (:div (:h2 "Debugger")
	       (:pre ((@ -j-s-o-n stringify) (component:state) nil "    "))
	       (:button (:onclick (lambda () 
				    (+ 1 2)))
			"Back"))))



(defun request-handler (req)
  )



