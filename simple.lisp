
(in-package peldan.simple)


;; TODO start generating JS here..



(defmacro javascript (&rest strings)
  `(htm (:script :type "text/javascript" 
		 ,@(loop for string in strings
		      collect (list 'str string)))))



(defcomponent hello-world ()
  (:div (:h1 "This is an example of using Virtual DOM")
	(javascript
	  (read-virtual-dom-js)
	  (bootstrap-code
	   '(:div "This is a virtual dom element" (@ state 0))
	   (list 1 2 "hej")))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))



(defun install-handler ()
  (pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*))
