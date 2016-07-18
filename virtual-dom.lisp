
(in-package :peldan.virtual-dom)

(setf *default-pathname-defaults* #P"/Users/peldan/Documents/Code/blub/")

(defun read-virtual-dom-js ()
  (read-file-to-string (make-pathname :name "virtual-dom"
				      :type "js"
				      :directory '(:relative "js"))))

(defvar *cached-virtual-dom-js*
  (read-virtual-dom-js))


(defvar *cached-ps-library*
  (ps* *ps-lisp-library*))

(defun convert-attr (attr)
  (cond ((eq :class attr)
	 "className")
	
	((eq :class-name attr)
	 "className")
	
	(t attr)))

;; Macros to try and hide which virtual dom library we are using
(ps:defpsmacro create-element (name attrs &rest children)
  `((ps:@ virtual-dom h) ,name ,(mapcar #'convert-attr attrs) (list ,@children)))

(ps:defpsmacro reify (arg)
  `((ps:@ virtual-dom create) ,arg))

(ps:defpsmacro diff-tree (old new)
  `((ps:@ virtual-dom diff) ,old ,new))

(ps:defpsmacro apply-patch (element patches)
  `((ps:@ virtual-dom patch) ,element ,patches))



(ps-util:defps make-module (render)
  "Returns a virtual DOM module with the given renderer.
The renderer must be able to render the state NIL successfully for the bootstrapping to work"
  (let ((module (create))
	(vtree (funcall render nil nil))
	element)

    ;; The three types of states
    (setf (@ module state) nil) 	;server state
    (setf (@ module local-state) nil)
    (setf (@ module temp) nil)
    
    ;; Construct the node
    (setf element (reify vtree))
    (setf (@ module element) element)
    (setf (@ module render) render)
    
    ;; This function will describe how the component reacts to changes in data
    (setf (@ module refresh) 
	  (lambda ()
	    (ps-util:log-message "Refresh")
	    (let* ((new-vtree (funcall render
				       (@ module state) 
				       (@ module local-state)
				       (@ module temp)))
		   (patch (diff-tree vtree new-vtree)))
	      (setf element (apply-patch element patch))
	      (setf (@ module element) element)
	      (setf vtree new-vtree))))

    ;; set the "global" session state for this component
    (setf (@ module set-state) 
	  (lambda (state)
	    (setf (@ module state) state)
	    ((@ module refresh))))
    
    ;; set the local state for this component
    (setf (@ module set-local) 
	  (lambda (local)
	    (setf (@ module local-state) local-state)
	    ((@ module refresh))))
    
    ;; set the temporary local state for this component
    (setf (@ module set-temp) 
	  (lambda (temp)
	    (setf (@ module temp) temp)
	    ((@ module refresh))))
    
    
    ;; Call this when ready to 'activate' the component in the DOM
    (setf (@ module add-to-dom)
	  (lambda ()
	    ((@ document body append-child) element)
	    ((@ module refresh))))
    
       
    module))



(ps-util:defps continuously (action-name interval &rest args)
  "Continously run an action"
  (let ((interval (or interval 300)))
    (set-interval (lambda ()
		    (apply #'action action-name args))
		  (or interval 300))
    interval))
		     


(ps-util:defps action (action &rest args)
  "Execute a serverside action"
  (lisp 
   (if (peldan.websocket:websockets-enabled)
       `(send-message (create :type :action
			      :name action
			      :args args))
       
       ;; TODO work on this..
       `(apply (chain component actions run)
	       ((chain action to-lower-case))
	       args))))
		     

(ps-util:defps imapcar (fn &rest args)
  "Like mapcar but adds an index as the first argument"
  (let ((is (list)))
    (dotimes (i (length (@ args 0)))
      ((@ is push) i))
    (apply #'mapcar fn is args)))


(defun library-js (stream)
  "Generate a string containing all the javascript needed to render components"
  (write-string *cached-virtual-dom-js* stream)
  (write-string *cached-ps-library* stream))



