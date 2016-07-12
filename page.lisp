

(in-package :peldan.page)


(defparameter *deployed-views*
  nil
  "A list of (url . view)")


(defun index (request)
  (declare (ignore request))
  "You found the index page")



(defun page-dispatcher (req)
  (let ((assoc (assoc (script-name req) *deployed-views* :test #'string=)))
    (cond 
	
      (assoc
       (funcall (cdr assoc) req))
      
      ((string= "/" (script-name req))
       (index req))
      
      (t
       "Unkown page"))))


(defun render-view (view &optional session)
  "Renders a full view, with all necessary JS and HTML"
  (cl-who:with-html-output-to-string (s)
    (:html
     (:head (:title (cl-who:str (view:view-name view))))
     (:body (:script :type "text/javascript" (virtual-dom:library-js s))
	    (:script :type "text/javascript" (peldan.ps:generate-user-js s))
	    (:script :type "text/javascript" (let ((ps:*parenscript-stream* s))
					       ;; TODO do (defvar) here instead of deferring it to into the view-ps call
					       (ps:ps* (view:view-ps view 
								     :session session
								     :name 'component))))))))


(defun deploy-page (url fn)
  "Deploy a request handler to a given url"
  (unless (eq (aref url 0) #\/)
    (error "URL must start with a forward slash!"))

  ;; Remove obsolete page(s)
  (setf *deployed-views* (delete url *deployed-views* :test #'string= :key #'car))
  
  (push (cons url fn)
	*deployed-views*))


(defun deploy-view (url view &optional (session view:*default-session*))
  "Deploy a view of the given session, with the specified URL"
  (unless (typep view 'view:view)
    ;; Assume we were passed hyperscript and create the view on the fly!
    (return-from deploy-view 
      (deploy-view url (view:make-view (the list view)) session)))
  
  (when session
    (session:register-actions session (view:view-actions view)))
  
  (deploy-page url (lambda (request)
		     (declare (ignore request))
		     (render-view view session))))



(defun print-it (session client &rest args)
  (declare (ignore client))
  (declare (ignore args))
  (format t "Assigning random id as name")
  (setf (slot-value session 'name) 
	(peldan.string:generate-uuid)))

(defun generate-rows (session client &rest args)
  (declare (ignore client))
  (declare (ignore args))
  (format t "Assigning random id as name")
  (setf (slot-value session 'items) 
	(loop for i from 1 upto 30 collect (let ((h (make-hash-table)))
					     (setf (gethash :name h)
						   (concatenate 'string "row-" (write-to-string i)))
					     (setf (gethash :value h)
						   i)
					     h))))

(defun toggle-background (session client)
  (declare (ignore client))
  (with-slots (style) session
    (setf style (cadr (or (member style #1='("red" "pink" "orange" "magenta" "red") :test #'string=)
			  #1#)))))

(defun change-row (session client ix)
   (declare (ignore client))
   (with-slots (items) session
     (let ((item (nth ix items)))
       (setf (gethash :value item)
	     (+ 10 (gethash :value item))))))

(deploy-view "/def" 
	     '(:div (:style (+ "background: " (view:state 'style))) ;this is a style attribute
	       "Hello"
	       (:style (+ "

div {margin-bottom: 3em }

b { background: black; color: " (view:state 'style) "}")) ; this is a style element
	       (:b (:class-name "test") "WORLD")
	       (:div (:i "Name: " (view:state 'name)))
	       (:div (:style "border: solid 1px black")
		"Commence the big row data generation: "
		(:table 
		 (:thead (:tr (:th "Name") (:th "Value") (:th "Calculated") (:th "Noise")))
		 (:tbody (imapcar (lambda (ix item)
				   (ml:h (:tr (:onclick (lambda ()
							  (view:action change-row ix)))
					      (:class "hello")
					      
					      (:td  (ps:getprop item 'name))
					      (:td (ps:getprop item 'value))
					      (:td (* 2 (ps:getprop item 'value)))
					      (:td (* 33 (ps:getprop item 'value))))))
				 (or (view:state 'items) (list))))))
	       (:button (:onclick (lambda ()
				    (view:action print-it)))
		"CLICK ME")
	       (:button (:onclick (lambda ()
				    (view:action toggle-background)))
		"Change background?")
	       (:button (:onclick (lambda ()
				    (view:action generate-rows)))
		"Generate rows?")))


(ps:ps* `(ml:h (:tr (:onclick (lambda ()
			       (view:action change-row ix)))
     
		   (:onhover (lambda ()
			       (alert "ffw")))
		   (:td (ps:getprop item 'name))
		   (:td (ps:getprop item 'value))
		   (:td (* 2 (ps:getprop item 'value)))
		   (:td (* 33 (ps:getprop item 'value))))))

;(session:state-message view:*default-session*)

;; This installs the dispatch function for this entire package
(dispatch:install-handler 'page-dispatcher)


(defclass simple-session (session:app-session)
  ((name :initform "Erik" :initarg :name :accessor test-name)
   (style :initform "background: red" :initarg :style :accessor test-style)
   (items :initarg :items :initform nil :accessor test-items))
  (:documentation "Dummy session just to have something to fall back on"))


(defmethod session:state-message (session)
  (with-output-to-string (s)
    (yason:with-output (s)
      (yason:with-object ()
	(yason:encode-object-element "type" "state")
	(yason:with-object-element ("value") 
	  (yason:with-object ()
	    (yason:encode-object-element "style" (test-style session))
	    (yason:encode-object-element "name" (test-name session))
	    (yason:encode-object-element "debug" nil)
	    (yason:with-object-element ("items")
	      (yason:with-array ()
		(loop for item in (test-items session)
		   do (yason:encode-array-element item))))))))))

;(session:state-message view:*default-session*)

(defun setup-default-session ()
  (setf view:*default-session*
	(make-instance 'simple-session))
  
  (websocket:install-resource view:*default-session*))

