
(defpackage :component
  (:use :common-lisp :hunchentoot))


(in-package :component)

(quicklisp:quickload "hunchentoot")
(quicklisp:quickload "alexandria")

(defparameter server (make-instance 'easy-acceptor :port 4243))


(defmacro defcomponent (name args psx)
  `(progn (defvar ,name (make-instance 'component
				       :code (quote ,psx)
				       :args (quote ,args)))))

(defun uri-var-match (strings-and-vars strings)
  (loop for var in strings-and-vars
        for string in strings
	  
	  with filled = (acons '&whole strings nil)
          do (check-type var (or string symbol))
	  do (cond ((and (stringp var) 
			 (string= var string))
		    t)
		
		   ((symbolp var)
		    (setq filled (acons var string filled)))
		
		   (t
		    (error "Mismatch: ~s (~a) ~s (~a)" var (type-of var) string (type-of string))))
	  
	  finally (return filled)))


(defun transpose (list-of-lists)
  (apply #'mapcar #'list list-of-lists))


(let ((vars '(a b c))
      (match '(1 2 3)))
  (transpose (list (reverse vars) (mapcar (constantly `(pop match)) vars))))



(defmacro strings-bind (vars-and-strings strings &body body)
  (alexandria:with-gensyms (match)
    (let ((vars (remove-if #'stringp vars-and-strings)))
      `(let ((,match (uri-var-match ',vars-and-strings ',strings)))
	 (when ,match
	   (let (,@(transpose (list vars 
				    (mapcar (lambda (var)
					      `(cdr (assoc ',var ,match)))
					    vars))))
	     ,@body))))))


(defmacro string-bind-case (string &rest forms)
  (the string string)
  (let ((split (split-by string #\/)))
    `(progn ,@(loop for form in forms
		   collect (destructuring-bind (vars-and-strings &rest body) form
			     `(strings-bind ,vars-and-strings ,split ,@body))))))



(defun handle-component-request ()
  (format nil "~s" (split-by (script-name*) #\/)))

;; Uri must be a function returning t when its argument's script-name
;; matches components?

;; the method body could then dispatch again based on the script-name
;; components
(define-easy-handler (components-handler :uri (constantly t)) ()
  (handle-component-request))

(define-easy-handler (hello-world :uri "/hello") ()
  "Hi there")

(start server)


	  
(defun split-by (string &optional (char #\Space))
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))



(setf *show-lisp-errors-p* t)

(defcomponent testdiv (a b)
  (:div "This is a test div" a (:p "This is a paragraph!") (mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3)) b))


(psx (:div "This is a test div" a (:p "This is a paragraph!") (mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3)) b))

(mapcar (lambda (x) `(:p "Count" ,(write-to-string x))) (list 0 1 2 3))


(defclass component ()
  ((code :reader source-code
	 :initarg :code
	 :initform (error "Source code required"))
   (args :reader args
	 :initarg :args)))



