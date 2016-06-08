
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
		  (@ state items))
     
	  (:div :onclick (action peldan.action:set-field 333 4)
	   "And this is the end of it. (Rendered " 
	   (length state)
	   " elements)")
     
	  (:textarea
	   :value ((@ -j-s-o-n stringify) state))))))


;; TODO remove macro, move into application-js somehow
(defpsmacro action (name &rest args)
  `(lambda () 
     (let ((new-state ((apply (chain module actions ,name) ,@args) (@ module state))))
       ((@ module update) new-state))))


(defun application-js (&optional initial-state)
  `(defvar App
     (let ((module (create)))
       
       (setf (@ module run-action)
	     (lambda (name &rest args)
	       (let ((new-state ((apply (getprop (chain module actions) name) args) (@ module state))))
		 ((@ module update) new-state))))
       
       (setf (@ module actions)
	     ,(peldan.action:action-ps))
       
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
	  (ps* (application-js (pairlis '(:a :b :c "items") '(2 3 "bluu" (15 33 77))))))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))



(defun install-handler ()
  (pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*))
