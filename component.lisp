
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
		    ;; Wrap in (lisp ..) form to ensure it gets reevaluated on every PS compilation
		    (lisp (peldan.debugger:debugger))

		    ;; TODO replace state by actual state later
		    (let ((state state))
		      ,psx))))))


(defun component-module-ps (renderer initial-state)
  "Generate all the PS needed to render a component"
  `(let ((module (create)))
             
     (setf (@ module state)
	   ,initial-state)
       
     (setf (@ module actions)
	   ,(peldan.action:action-ps `(@ module update-state)))
       
     (setf (@ module set-state)
	   ,(peldan.virtual-dom:render-ps renderer
					  `(@ module state)))
       
     ;; Helper for *updating* (as opposed to just *setting*) state
     (setf (@ module update-state)
	   (lambda (fn) ((@ module set-state) (funcall fn (@ module state)))))
     
     ;; Establish server connection
     (lisp (when (peldan.websocket:websockets-enabled)
	     `(setf (@ module ws)
		    ,(peldan.websocket:connect-ps `(@ module update)))))
     
     
     module))


(defmacro javascript (&rest strings)
  `(cl-who:htm (:script :type "text/javascript" 
			,@(loop for string in strings
			     collect (list 'cl-who:str string)))))

(defun library-js (stream)
  "Generate a string containing all the javascript needed to render components"
  (write-string *cached-virtual-dom-js* stream)
  (write-string *cached-ps-library* stream)
  (write-string (ps* `(defun make-module (renderer state)
			,(component-module-ps 'renderer 'state))
		     
		     ;; Helper function for periodically executing an action (to be moved)
		     `(defun continuously (action interval &rest args)
			(let ((interval (or interval 300)))
			  (set-interval (lambda ()
					  (apply (chain component actions run) ((chain action to-lower-case)) args))
					(or interval 300))
			  interval))
		     
		     ;; Helper
		     `(defun imapcar (fn &rest args)
			"Like mapcar but adds an index as the first argument"
			(let ((is (list)))
			  (dotimes (i (length (@ args 0)))
			    ((@ is push) i))
			  (apply #'mapcar fn is args))))
		stream))


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
		    (:script :type "text/javascript" (library-js s))
		    
		    (:script :type "text/javascript" 
			     (let ((*parenscript-stream* s))
			       (ps*
				;; A publicly available virtual DOM renderer for the component
				`(defun render (state)
				   ,(component-ps component))
				     
				;; Define the component module
				`(defvar component
				   (make-module render ,state)))))))))))



(defun register-component (name &rest args)
  "Register a new component, making it accessible by HTTP request"
  (replace-component (apply #'make-component name args)))



;; This is a test action to see how fast the updates can be
(peldan.action:register-action 
 'randomize-rows 
 :body '((let ((rows (list)))
	   (unless (defined myvar)
	     (setq myvar 0))
	   (dotimes (i 100)
	     (let ((cols (list)))
	       (dotimes (j 12)
		 (setf (aref cols j) 
		       ((@ -math round) (* 100 ((@ -math random))))))
	       (setf (aref rows i) 
		     cols))
	     (setf myvar (+ 1 (or myvar 0))))
	   (set-field rows "data" "items"))))


(defun generate-test-state ()
  (let ((test-data-string
	(yason:with-output-to-string* ()
	  (yason:with-object ()
	    (yason:with-object-element ("data")
	      (yason:with-object ()
		(yason:with-object-element ("items")
		  (yason:with-array ()
		    (loop for i upto 99 do
			 (yason:with-array ()
			   (loop for j upto 12 do
				(yason:encode-array-element (random 100)))))))))))))
   `(peldan.ps:json-parse 
     ,test-data-string)))


(register-component 'testcomponent 
 :initial-state 
 (generate-test-state)
 
 :code 
 `(psx (:div "This is a Virtual DOM element" 
		       
	     (:table
	      (:thead (:tr (mapcar (lambda (item)
				     (psx (:th "Column")))
				   (@ state data items 0))))
	      
	      (:tbody (mapcar (lambda (item)
				(psx (:tr (mapcar (lambda (x)
						    (psx (:td x)))
						  item))))
		      
			      (@ state data items))))
     
	     (:div :onclick (peldan.action:action peldan.action:set-field 333 "data" "items")
		   "And this is the end of it. (Rendered " 
		   (length (@ state data items))
		   " elements)"))))


(defun install-handler ()
  "Install the request handler that will maek components accessible through hunchentoot"
  (pushnew (lambda (req) (request-handler req)) peldan.dispatch:*handlers*))
