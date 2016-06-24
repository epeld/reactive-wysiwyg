


'(:div (:class "emphasis")
      "This will be bold"
      (:p "child element")
      (:p (:width "100px")
          "This is a child element"))


(defmacro any-of (x &rest alternatives)
  `(case ,x
     ,@(loop for a in alternatives collect
	    `(,a t))))


(defun attrp (item)
  "Determine if a node should be serialized as an attr"
  (and (consp item)
       (any-of (first item)
	       :class :width :height :size)))


(defun convert-to-who (ml)
  (if (atom ml)
      ml
      (let (children)
	`(,(first ml)
       
	   ;; items
	   ,@(loop for (item . rest) on (rest ml)
		until (not (attrp item))
		nconc item
		finally (setf children (cons item rest)))
       
	   ;; children
	   ,@(mapcar #'convert-to-who children)))))


(defun write-html (ml &optional (stream *standard-output*))
  (if (atom ml)
      (write-string (cl-who:escape-string ml) stream)
      (let ((name (string-downcase (first ml)))
	    (contents (rest ml))
	    children)
	(progn 
	  (format stream "<~a" name)
       
	  
	  (unless (endp contents)
	    ;; items
	    (loop for (item . rest) on contents
	       until (not (attrp item))
	       do (format stream " ~(~a~)=\"~a\"" (first item) (second item))
	       finally (setf children (cons item rest))))
	  
	   (if children
	       (progn (write-string ">" stream)
		      (loop for child in children do
			   (write-html child stream))
		      (format stream "</~a>" name))
	       (write-string "/>" stream)))))
  nil)


(defun generate-hyperscript (ml)
  (if (atom ml)
      ml
      (let ((name (string-downcase (first ml)))
	    (contents (rest ml))
	    children)
	
	`(create-element ,name
			 (
			  ;; items
			  ,@(loop for (item . rest) on contents
			       until (not (attrp item))
			       nconc item
			       finally (setf children (cons item rest))))
			 
			 
       
			 ,@(when children
				(loop for child in children collect
				     (generate-hyperscript child)))))))


(generate-hyperscript '(:div (:class "emphasis")
			"This will be bold"
			(:p "child element")
			(:p (:width "100px")
			 "This is a child element")))

(convert-to-who '(:div (:class "emphasis")
		  "This will be bold"
		  (:p "child element")
		  (:p (:width "100px")
		   "This is a child element")))

(with-output-to-string (s)
  (write-html '(:div (:class "emphasis")
		"This will be bold"
		(:br)
		(:p "child element ååh")
		(:p (:width "100px")
		 "This is a child element"))
	      s))
