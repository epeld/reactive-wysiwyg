
(in-package peldan.simple)


;; TODO start generating JS here..



(defmacro javascript (&rest strings)
  `(htm (:script :type "text/javascript" 
		 ,@(loop for string in strings
		      collect (list 'str string)))))


(defun component-js-code ()
  (render-loop
	   
   '(:div "This is a virtual dom element" 
     
     (mapcar (lambda (x)
	       (psx (:p "Value:" x)))
      state)
     
     (:div 
      "And this is the end of it. (Rendered " 
      (length state)
      " elements)"))
	   
   (list 1 2 "hej")))


(defcomponent hello-world ()
  (:div (:h1 "This is an example of using Virtual DOM")
	(javascript
	  (read-virtual-dom-js)
	  (ps* *ps-lisp-library*)
	  (component-js-code))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))



(defun install-handler ()
  (pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*))
