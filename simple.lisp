
(in-package peldan.simple)

(defsnippet bootstrap ()
  (let ((ws (new (-web-socket (lisp (concatenate
				     'string 
				     "ws://localhost:"
				     (write-to-string peldan.websocket:*port*)))))))
    (with-slots (onclose onmessage onopen) ws
      
      (setf onclose (lambda ()
		      (log-message "Connection closed")))
      
      (setf onmessage (lambda (ev)
		      (log-message "Got message" ev)))
      
      (setf onopen (lambda ()
		      (log-message "Connection estabilished"))))
    ws))




(defcomponent-macro load-scripts (&rest names)
  `(:script :type "text/javascript" 
	    ,@(mapcar (lambda (name) `(str ,(funcall name))) names)))


(defcomponent-macro javascript (&body ps)
  `(:script :type "text/javascript" (str (ps ,@ps))))


(defcomponent notice ()
  (:p "Please wait a while as the connection is being set up.."))


(defcomponent hello-world ()
  (:div (:h1 "Hello, World")
	(component #'notice)
	(:iframe)
	(load-scripts bootstrap)))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))


(render hello-world)
;(psx ())


;(pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*)
