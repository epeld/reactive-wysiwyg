
(in-package :peldan.data)


(defun map-inside (fn plist &rest keys)
  "Behaves like mapcar without a third argument, otherwise recurses into plist using keys first"
  (if (endp keys)
      (funcall fn plist)
      (let ((key (first keys)))
	(unless (consp plist)
	  (error "Cannot look up keys ~s inside value ~a" keys plist))
	(if (numberp key)
	    (append (subseq plist 0 key)
		    (list (apply #'map-inside fn (nth key plist) (rest keys)))
		    (subseq plist (+ 1 key)))
	    (cons key
		  (cons (apply #'map-inside fn (getf plist key) (rest keys))
			plist))))))


(defun set-inside (val plist &rest keys)
  (apply #'map-inside
	 (constantly val)
	 plist
	 keys))



(defun encode-nested-plist (plist &optional (stream *standard-output*))
  "Like encode-plist but tries to be recursive"
  (yason:with-object ()
    (loop for (key value) on plist by #'cddr
       with keys = nil
       unless (member key keys :test #'eq)
       do (progn
	    (push key keys)
	    (if (and (consp value) 
			(keywordp (car value)))
		
		  (yason:with-object-element (key)
		     (encode-nested-plist value stream))
		  
		  (yason:encode-object-element key value))))))

