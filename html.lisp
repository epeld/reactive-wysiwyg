
(defpackage :peldan.html
  (:use :common-lisp)
  (:import-from :cl-html-parse :parse-html)
  (:import-from :drakma :http-request))

(in-package :peldan.html)


(defun htm-to-hyperscript (node)
  "Converts from the output of parse-html to our hyperscript format"
  (when (or (stringp node) (keywordp node))
    (return-from htm-to-hyperscript node))

  (if (listp (first node))
      `(,(first (first node))
	 ,@(loop for (name value) on (rest (first node)) by #'cddr
	      collect `(,name ,value))
	 ,@(mapcar #'htm-to-hyperscript (rest node)))
      
      `(,(first node) ,@(mapcar #'htm-to-hyperscript (rest node)))))


(defun parse-hyperscript (html)
  (htm-to-hyperscript (parse-html html)))



(defun make-hrefs-absolute (hyperscript base-url)
  "Convert all hrefs to absolute"
  (when (listp hyperscript)
    
    (if (and (eql :href (first hyperscript))
	     (not (eql #\/ (aref (second hyperscript) 0))))
	
	(setf (second hyperscript)
	      (concatenate 'string base-url (second hyperscript)))
	
	(loop for child in (rest hyperscript) do (make-hrefs-absolute child base-url))))
  
  hyperscript)


(defun remove-scripts (hyperscript)
  "Remove all script tags from the hyperscript"
  (when (listp hyperscript)
    
    (if (eql :script (first hyperscript))
	
	(setf (cdr hyperscript) nil)
	
	(loop for child in (rest hyperscript) do (remove-scripts child))))
  
  hyperscript)


(defun parse-hyperscript-from-url (url)
  ;; Doesnt end in /
  (assert (not (eql #\/ (aref url (- (length url) 1)))))

  (let ((h (htm-to-hyperscript (first (parse-html (http-request url))))))
    (remove-scripts h)
    (make-hrefs-absolute h (concatenate 'string url "/"))
    h))

(defparameter ycomb (parse-hyperscript-from-url "http://news.ycombinator.com"))
(page:deploy-view "/erik-testar" ycomb)

