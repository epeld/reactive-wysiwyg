
(in-package :peldan.debugger)


(defun debugger ()
  "Creates ps code for a debugger"
  `(psx (:div (:h2 "Debugger")
	      (:pre ((@ -j-s-o-n stringify) (peldan.component:state) nil "    "))
	      (:button :onclick (lambda () 
				  (peldan.virtual-dom:action "debug"))
		       "Back"))))



(defun request-handler (req)
  (when (string= "/sessions" (hunchentoot:script-name req))
    (peldan.component:generate-component-html
     `(:div (:class "sessions")
	    (:h1 "Open Sessions")
	    ,@(loop for session in peldan.websocket:*sessions*
		   collect 
		   (let ((uuid (peldan.websocket:session-uuid session)))
		     `(peldan.ml:h 
		       (:div 
			(:a (:href ,(format nil "/sessions/~a" uuid))
			    ,(concatenate 'string
					  "#"
					  uuid))
			" - "
			(:small
			,(with-output-to-string (s)
						(prin1 (peldan.websocket:session-state session)
						       s))))))))
     :uuid nil :state nil)))


(ps:ps* `(peldan.ml:h (:div (:a (:href ,(concatenate 'string
						     "#"
						     "33"))
					     "How does this work?"))))

(pushnew 'request-handler peldan.dispatch:*handlers*)
