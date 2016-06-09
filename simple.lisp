
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
		    (psx (:div "Value:" x)))
		  (@ state items))
     
	  (:div :onclick (action peldan.action:set-field 333 "items")
	   "And this is the end of it. (Rendered " 
	   (length state)
	   " elements)")
     
	  (:textarea
	   :value ((@ -j-s-o-n stringify) state))))))


(defun application-js (psx &optional initial-state)
  `(defvar App
     (let ((module (create)))
             
       (setf (@ module actions)
	     ,(peldan.action:action-ps `(@ module update-state)))
       
       (setf (@ module state)
	     ((@ -j-s-o-n parse) ,(json-string initial-state)))
	      
       (setf (@ module set-state)
	     ,(render-ps psx 
			 `(@ module state)))
       
       (setf (@ module update-state)
	     (lambda (fn) ((@ module set-state) (funcall fn (@ module state)))))
     
       (setf (@ module ws)
	     ,(connect-ps `(@ module update)))
     
       module)))


;; TODO this is not a component!! Just the standard html for our pages
;; The component is called test-component (see above) FIX!!!
(defcomponent hello-world ()
  (:div (:h1 "This is an example of using Virtual DOM")
	(javascript
	  (read-virtual-dom-js)
	  (ps* *ps-lisp-library*)
	  (ps* (application-js
		(test-component)
		(pairlis '(:a :b :c "items") '(2 3 "bluu" (15 33 77))))))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))



(defun install-handler ()
  (pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*))
