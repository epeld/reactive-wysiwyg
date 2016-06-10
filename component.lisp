
(in-package peldan.component)


(defvar *cached-virtual-dom-js*
  (peldan.virtual-dom:read-virtual-dom-js))

(defvar *cached-ps-library*
  (ps* *ps-lisp-library*))


(defgroup component)


(defun component-ps (psx &optional initial-state)
  "Generate all the PS needed to render a component"
  `(defvar App
     (let ((module (create)))
             
       (setf (@ module actions)
	     ,(peldan.action:action-ps `(@ module update-state)))
       
       (setf (@ module state)
	     ((@ -j-s-o-n parse) ,(peldan.virtual-dom:json-string initial-state)))
       
       (setf (@ module set-state)
	     ,(peldan.virtual-dom:render-ps psx `(@ module state)))
       
       (setf (@ module update-state)
	     (lambda (fn) ((@ module set-state) (funcall fn (@ module state)))))
     
       (setf (@ module ws)
	     ,(peldan.websocket:connect-ps `(@ module update)))
     
       module)))


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
     do (let ((psx (field-value :code component))
	      (state (get-initial-state request component)))
      
	  (return 
	    (cl-who:with-html-output-to-string (s)
	      (:div (:h1 (cl-who:str (title-ify (string (name component)))))
		    (javascript *cached-virtual-dom-js*
				*cached-ps-library* 
				(ps* (component-ps psx state))
				(ps* (field-value :ps component)))))))))




(defun register-component (name options psx)
  (destructuring-bind (&key initial-state ps) options
    (replace-component (make-component name
				       :code psx
				       :initial-state initial-state
				       :ps ps))))



(defmacro defcomponent (name options psx)
  `(register-component ',name (list ,@options) ',psx))


(peldan.action:defaction randomize-rows ()
  (let ((rows (list)))
    (unless (defined myvar)
      (setq myvar 0))
    (dotimes (i 100)
      (setf (aref rows i) (create :count (or myvar 0)
				   :value (random 100)))
      (setf myvar (+ 1 (or myvar 0))))
    (set-field rows "items")))


(defcomponent testcomponent (:initial-state
			     (acons "debug" 0 (acons "items" (list 1 2 3) nil)))
  (:div (if (@ state debug)
	    (psx (:pre ((@ -j-s-o-n stringify) state nil "    ")))
	    (psx (:div "This is a virtual dom element" 
		 
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
			:value ((@ -j-s-o-n stringify) state)))))))


(defun install-handler ()
  (pushnew (lambda (req) (request-handler req)) peldan.dispatch:*handlers*))
