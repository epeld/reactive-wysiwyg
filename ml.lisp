
(in-package :peldan.ml)


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
	  (when (not (keywordp (first ml)))
	    (return-from write-html))
	  
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
  (when (or (atom ml) (not (keywordp (first ml))))
    (return-from generate-hyperscript ml))
  

  (let ((name (string-downcase (first ml)))
	(contents (rest ml))
	children)
	
    `(peldan.virtual-dom:create-element ,name
					(ps:create
					 ;; items
					 ,@(loop for (item . rest) on contents
					      until (not (attrp item))
					      nconc item
					      finally (setf children (cons item rest))))
			 
			 
       
					,@(when children
						(loop for child in children collect
						     (generate-hyperscript child))))))

;; h as in hyperscript
(ps:defpsmacro h (ml)
  (generate-hyperscript ml))


(defun find-children-by-tag-name (name el)
  "find all children with a given tag name"
  (remove-if-not (lambda (el)
		  (and (consp el)
		       (eq (first el) 
			   name)))
	     (rest el)))


(defun join (element &rest others)
  "Create a new element with same tag name as element and all children"
  `(element
    ,@(rest element)
    ,@(loop for other in others
	   nconc (rest other))))


(defun write-selector (selector stream)
  (write-string (ecase (first selector)
			(class (concatenate 'string "." (second selector)))
			(id (concatenate 'string "#" (second selector))))
		      stream))

(defun write-style (style stream)
  (format stream "~(~a~): ~(~a~)~a" 
	  (first style)
	  (second style)
	  (or (second (third style)) "")))


(defun write-css (ml &optional (stream *standard-output*))
  (assert (eq (first ml) 'rule))
  (let ((selectors (first (find-children-by-tag-name 'select ml)))
	(styles (first (find-children-by-tag-name 'style ml))))
    
    (if selectors
	(loop for (selector . next) on (rest selectors)
	   do (write-selector selector stream)
	   unless (endp next)
	   do (write-string ", " stream))
	
	(write-string "*" stream))
    
    (write-string " {
    " stream)
    
    (loop for (style . next) on (rest styles)
       do (write-style style stream)
       unless (endp next)
       do (write-string ";
    " stream))
    
    (write-string "
}" stream)))


(setq example
  '(rule 
    (select 
     (id "hello")
     (class "emphasis")
     (class "other"))
    (style
     (:font-weight :bold)
     (:font-size 1.5 (:unit "em")))))


(with-output-to-string (s)
  (write-css example s))


