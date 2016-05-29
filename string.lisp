
(in-package :peldan.string)


(defun split-by (string &optional (char #\Space))
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (setq string (string-trim "/" string))
    (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))

(defvar *error-on-mismatch* nil)


(defun string-var-match (strings-and-vars strings)
  (labels ((no-match (&rest message)
	   (if *error-on-mismatch* 
	       (apply #'error message)
	       (return-from string-var-match nil))))
	 (loop 
	    for var in strings-and-vars
	    for rest on strings
	    for string = (car rest) then (car rest)
	  
	    with filled = (acons '&whole strings nil)
       
	    do (progn (check-type var (or string symbol))
		      (cond ((and (stringp var) 
				  (string= var string))
			     t)
		
			    ((symbolp var)
			     (setq filled (acons var string filled)))
		
			    (t
			     (no-match "Mismatch: ~s (~a) ~s (~a)" var (type-of var) string (type-of string)))))
	  
	    finally (let ((rest (cdr rest)))
		      (if rest
			  (no-match "Incomplete match. Remaining ~s out of ~s" rest strings)
			  (return-from string-var-match (acons '&rest rest filled)))))))



(defmacro strings-bind (vars-and-strings strings &body body)
  ;; Special case: empty list or otherwise treated as simple match
  (when (or (null vars-and-strings) (eql vars-and-strings 'otherwise))
    (return-from strings-bind
      `(progn ,@body)))
  
  (alexandria:with-gensyms (match)
    (let ((vars (remove-if #'stringp vars-and-strings)))
      `(let ((,match (string-var-match ',vars-and-strings ,strings)))
	   (when ,match
	     (let (,@(transpose (list vars 
					(mapcar (lambda (var)
						  `(cdr (assoc ',var ,match)))
						vars))))
		 ,@body))))))


(defmacro string-bind-case (string &rest forms)
  (with-gensyms (split block-name)
    `(block ,block-name
       (let ((,split (split-by ,string #\/)))
	 (progn ,@(loop for form in forms
		     collect (destructuring-bind (vars-and-strings &rest body) form
			       `(strings-bind ,vars-and-strings ,split (return-from ,block-name (progn ,@body))))))))))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 
