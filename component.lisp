
(in-package peldan.component)


(defvar *cached-virtual-dom-js*
  (peldan.virtual-dom:read-virtual-dom-js))

(defvar *cached-ps-library*
  (ps* *ps-lisp-library*))


(defgroup component)


(defun component-ps (component)
  "Extracts (and wraps) the PS contained in this component for appearing on a page"
  (let ((psx (field-value :code component)))
    `(psx (:div (if (@ state debug)
		    (lisp (peldan.debugger:debugger))

		    ;; TODO replace state by actual state later
		    (let ((state state))
		      (psx ,psx)))))))


(defun component-module-ps (component &optional initial-state)
  "Generate all the PS needed to render a component"
  `(let ((module (create)))
             
     (setf (@ module actions)
	   ,(peldan.action:action-ps `(@ module update-state)))
       
     (setf (@ module state)
	   (or ,initial-state (create)))
       
     (setf (@ module set-state)
	   ,(peldan.virtual-dom:render-ps (component-ps component)
					  `(@ module state)))
       
     (setf (@ module update-state)
	   (lambda (fn) ((@ module set-state) (funcall fn (@ module state)))))
     
     (setf (@ module ws)
	   ,(peldan.websocket:connect-ps `(@ module update)))
     
     module))


(defmacro javascript (&rest strings)
  `(cl-who:htm (:script :type "text/javascript" 
			,@(loop for string in strings
			     collect (list 'cl-who:str string)))))



(defun title-ify (string)
  ; Currently only upper cases first letter..
  (the string string)
  (when (string= "" string)
    (error "Empty string"))
  (string-upcase (peldan.string:replace-all (string-downcase string)
					    "-" " ")
		 :start 0
		 :end 1))


(defun get-initial-state (request component)
  ;; TODO later on look in request as well
  (field-value :initial-state component))


(defun request-handler (request)
  (loop for component in (members component-group)
     
     if (string-equal (hunchentoot:script-name request)
		      (concatenate 'string "/component/" (string (name component))))
     do (let ((state (get-initial-state request component)))
      
	  (return 
	    (cl-who:with-html-output-to-string (s)
	      (:div (:h1 (cl-who:str (title-ify (string (name component)))))
		    (javascript *cached-virtual-dom-js*
				*cached-ps-library*
				
				;; Define the component loop and make it accessible through the js inspector
				(ps* `(defvar component
					,(component-module-ps component 
						       `((@ -j-s-o-n parse) ,(peldan.virtual-dom:json-string state))))
				     
				     ;; Helper function for periodically executing an action (to be moved)
				     `(defun continuously (action interval &rest args)
					(set-interval (lambda ()
							(apply (chain component actions run) ((chain action to-lower-case)) args))
						      (or interval 300)))))))))))




(defun register-component (name &rest args)
  "Register a new component, making it accessible by HTTP request"
  (replace-component (apply #'make-component name args)))




;; TODO not sure if this will ever be needed. Consider removing
(defpsmacro subcomponent (name state)
  (let ((component (find-component name)))
    (unless component
      (error "Component doesn't exist: ~a" name))
   
    ;; TODO how do we expose state to subcomponents? this doesn't work!
    `(let ((state ,state))
       (peldan.psx:psx ,(field-value :code component)))))



;; This is a test action to see how fast the updates can be
(peldan.action:defaction randomize-rows ()
  (let ((rows (list)))
    (unless (defined myvar)
      (setq myvar 0))
    (dotimes (i 100)
      (setf (aref rows i) (create :count (or myvar 0)
				   :value (random 100)))
      (setf myvar (+ 1 (or myvar 0))))
    (set-field rows "items")))


(register-component'testcomponent 
 :initial-state (acons "debug" 1 (acons "items" (list 1 2 3 "foo" "bar" "baz") nil))
 :code `(:div "This is a Virtual DOM element" 
		       
	      (:table
	       (:thead (:tr (:th 1) (:th 2) (:th 3) (:th 1) (:th 2) (:th 3)))
	       (mapcar (lambda (x)
			 (psx (:tr (:td (@ x count)) (:td (@ x value)) (:td "-") (:td (@ x count)) (:td (@ x value)) (:td "-"))))
		       (@ state items)))
     
	      (:div :onclick (peldan.action:action peldan.action:set-field 333 "items")
		    "And this is the end of it. (Rendered " 
		    (length state)
		    " elements)")
     
	      (:textarea
	       :value ((@ -j-s-o-n stringify) state))))


(defun install-handler ()
  (pushnew (lambda (req) (request-handler req)) peldan.dispatch:*handlers*))
