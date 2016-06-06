
(in-package peldan.simple)


;; TODO start generating JS here..



(defmacro javascript (&rest strings)
  `(htm (:script :type "text/javascript" 
		 ,@(loop for string in strings
		      collect (list 'str string)))))


(defun test-component ()
  '(:div "This is a virtual dom element" 
     
     (mapcar (lambda (x)
	       (psx (:p "Value:" x)))
      state)
     
     (:div 
      "And this is the end of it. (Rendered " 
      (length state)
      " elements)")
     
     (:textarea
      :value ((@ -j-s-o-n stringify) state))))



(defcomponent hello-world ()
  (:div (:h1 "This is an example of using Virtual DOM")
	(javascript
	  (read-virtual-dom-js)
	  (ps* 
	   *ps-lisp-library*
	   (render-loop (test-component)
			(list 1 2 "haj"))))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))



(defun install-handler ()
  (pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*))
