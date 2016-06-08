
(in-package peldan.simple)


;; TODO start generating JS here..



(defmacro javascript (&rest strings)
  `(htm (:script :type "text/javascript" 
		 ,@(loop for string in strings
		      collect (list 'str string)))))


(defun test-component ()
  '(if (@ state debug)
    (psx (:div ((@ -j-s-o-n stringify) state nil "    ")))
    (psx (:div "This is a virtual dom element" 
     
	  (mapcar (lambda (x)
		    (psx (:p "Value:" x)))
		  state)
     
	  (:div 
	   "And this is the end of it. (Rendered " 
	   (length state)
	   " elements)")
     
	  (:textarea
	   :value ((@ -j-s-o-n stringify) state))))))


(defun application-js (&optional initial-state)
  `(defvar App
     (let ((module (create)))
       
       (setf (@ module actions)
	     ,(peldan.action:generate-ps (@ module update)))
       
       (setf (@ module state)
	     ((@ -j-s-o-n parse) ,(json-string initial-state)))
	      
       (setf (@ module update)
	     ,(render-ps (test-component)
			 `(@ module state)))
     
       (setf (@ module ws)
	     ,(connect-ps `(@ module update)))
     
       module)))


(defcomponent hello-world ()
  (:div (:h1 "This is an example of using Virtual DOM")
	(javascript
	  (read-virtual-dom-js)
	  (ps* *ps-lisp-library*)
	  (ps* (application-js (list 2 3 "bluu"))))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))



(defun install-handler ()
  (pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*))
