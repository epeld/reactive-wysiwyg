
(in-package :peldan.virtual-dom)


(defun read-virtual-dom-js ()
  (read-file-to-string "js/virtual-dom.js"))

(defvar *cached-virtual-dom-js*
  (read-virtual-dom-js))


(defvar *cached-ps-library*
  (ps* *ps-lisp-library*))


;; Macros to try and hide which virtual dom library we are using
(ps:defpsmacro create-element (name attrs &rest children)
  `((ps:@ virtual-dom h) ,name ,attrs (list ,@children)))

(ps:defpsmacro reify (arg)
  `((ps:@ virtual-dom create) ,arg))

(ps:defpsmacro diff-tree (old new)
  `((ps:@ virtual-dom diff) ,old ,new))

(ps:defpsmacro apply-patch (element patches)
  `((ps:@ virtual-dom patch) ,element ,patches))


(defun render-ps (render-fn state)
  "Defines a PS block that will render and attach a new element to BODY"
  (with-ps-gensyms (render)
    `(let ((,render ,render-fn))
    
       (let* ((tree (,render ,state))
	      (element (reify tree)))
	    
	 ((ps:@ document body append-child) element)
	
	 (lambda (new-state)
	   (setf ,state new-state)
	   (let* ((new-tree (,render new-state))
		  (patches (diff-tree tree new-tree)))
	     (setf tree new-tree)
	     (apply-patch element patches)))))))


(defun library-js (stream)
  "Generate a string containing all the javascript needed to render components"
  (write-string *cached-virtual-dom-js* stream)
  (write-string *cached-ps-library* stream)
  (write-string (ps* `(defun make-module (renderer)
			(let ((module (create)))
             
			  ;; This is typically set when WS connection is established
			  (setf (@ module state) nil)
       
			  (setf (@ module set-state)
				,(render-ps 'renderer `(@ module state)))
       
			  module))
		     
		     ;; Helper function for periodically executing an action (to be moved)
		     `(defun continuously (action-name interval &rest args)
			(let ((interval (or interval 300)))
			  (set-interval (lambda ()
					  (apply #'action action-name args))
					(or interval 300))
			  interval))
		     
		     
		     ;; Execute an action (helper)
		     `(defun action (action &rest args)
			(lisp 
			 (if (peldan.websocket:websockets-enabled)
			     `(send-message (create :type :action
						    :name action
						    :args args))
			     `(apply (chain component actions run)
				     ((chain action to-lower-case))
				     args))))
		     
		     ;; Helper
		     `(defun imapcar (fn &rest args)
			"Like mapcar but adds an index as the first argument"
			(let ((is (list)))
			  (dotimes (i (length (@ args 0)))
			    ((@ is push) i))
			  (apply #'mapcar fn is args))))
		stream))



