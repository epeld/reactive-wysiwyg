
(in-package :peldan.dispatch)


(defun index (request)
  (peldan.component:generate-component-html
   `(:div "This is the Meta Session " (peldan.component:state name)
	  ;"Current state: "
	  ;(peldan.ps:json-stringify (peldan.component:state))
	  (mapcar (lambda (session)
		    (peldan.ml:h 
		     (:div (ps:@ session uuid)
			   (peldan.ps:json-stringify (ps:@ session data)))))
		  (peldan.component:state data)))
   :session-uuid (peldan.websocket:uuid peldan.websocket:*meta*)))


(quote
 (install-handler 
  (lambda (req)
    (when (string= "/" (script-name req))
      (index req)))))
