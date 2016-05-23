(defpackage :peldan.page
  (:use :cl)
  (:import-from :cl-who :with-html-output-to-string :str :htm)
  (:import-from :alexandria :with-gensyms)
  (:import-from :peldan.resource :resource-name :find-resource)
  (:import-from :peldan.string :replace-all)
  (:import-from :peldan.component
		:source-code :code :*components*)
  (:export :page-dispatch))

(in-package :peldan.page)


(defvar *pages* nil)


(defclass page (peldan.component:component)
  ((url-parts :initarg :url-parts)))


(defmacro defpage (name lambda html)
  (destructuring-bind (url params) lambda
    `(progn (defparameter ,name (make-instance 'page 
					       :name ,(string-downcase name)
					       :url-parts (quote ,url)
					       :args (quote ,params)
					       :code (quote ,html)))
       
	    (setf *pages* (peldan.resource:replace-resource ,name *pages*)))))


(defun page-url-symbols (page)
  "Returns a list of the symbols that the page expects to extract from its url"
  (remove-if (lambda (item)
	       (or (stringp item)
		   (eql '&whole item)))
	     (page-url-parts page)))


(defun page-arg-symbols (page)
  "returns a list of the symbols used to bind a page's args"
  (mapcar (lambda (arg)
	    (if (consp arg)
		(first arg)
		arg))
	  (peldan.component:args page)))

;; 
;; Url resolution
;; 
(defun url-parts (url-parts &rest args)
  "Fill in the 'holes' in the url-parts with the values passed"
  (mapcar (lambda (part)
	    (if (and (symbolp part) args)
		(pop args)
		part))
	  url-parts))


(defun page-url-parts (page &rest args)
  "Fill in a page's url-parts with the values passed in"
  (apply #'url-parts (slot-value page 'url-parts) args))


(defun url (url-parts &rest args)
  "Produce a string url by filling in the holes in the url-parts with values"
  (format nil "/~{~a~^/~}" (apply #'url-parts url-parts args)))


(defun page-url (page &rest args)
  "Produce a page url by filling in the holes in the url-parts with values"
  (url (apply #'page-url-parts page args)))


(defun alist-values (url-match &optional keys)
  (loop for (key . value) in url-match
       when (member key keys)
       collect value))


(defun split-script-name (request)
  (peldan.string:split-by (hunchentoot:script-name request) #\/))


(defmacro generate-html (psx)
  `(psx-to-html ,psx))


(defun render-page (page request url-match)
  "Render a page, passing in the request and the url-match alist"
  (with-slots (args code url) page
    
    (let* ((url-symbols (page-url-symbols page))
	   (url-vals (alist-values url-match url-symbols))
	   
	   (arg-symbols (page-arg-symbols page))
	   (arg-vals (mapcar (lambda (s)
			       (hunchentoot:parameter (string s) request))
			     arg-symbols)))
      
      `(let (,@(peldan.string:transpose (list url-symbols url-vals)))
	 (let (,@(peldan.string:transpose (list arg-symbols arg-vals)))
	   (with-html-output-to-string (,(gensym))
	     ,code))))))


(defun handle-request (page request)
  (let ((match (peldan.string:string-var-match (page-url-parts page)
					       (split-script-name request))))
    (when match
      (when nil (let ((*print-pretty* t) (*print-readably* t))
		  (common-lisp:with-output-to-string (s)
		    (pprint (render-page page request match) s))))
      (eval (render-page page request match)))))



(defun page-dispatch (request)

  ;; Go through pages until we find one that handles the request!
  (loop for page in *pages*
     do (let ((reply (handle-request page request)))
	  (when reply
	    (return-from page-dispatch reply)))))


(defun readable-name (symbol)
  (replace-all (string-downcase (string symbol)) "-" " "))


;; Examples:
(defpage components-page (("components") ())
  (:div :class "components"
	
	(:h1 "Components Overview")
	
	(mapcar (lambda (component)
		  
		  `(:div (:a :href ,(page-url component) ,(resource-name component))))
		*pages*)))


(defpage pages-overview (("pages") ())
  (:div :class "components"
	
	(:h1 "Pages Overview")
	
	(mapcar (lambda (page)
		  
		  (htm (:div (:a :href (str (page-url page-overview (resource-name page))) (str (resource-name page))))))
		*pages*)))


(defpage page-overview (("pages" page) ())
  (:div :class "components"
	
	(:h1 "Overview of page - " (:span (str (readable-name page))))
	
	(:div (:p "This page has code:")
	      (:p (str (princ (source-code (find-resource page *pages*)) nil))))
	
	(:a :href (page-url pages-overview) "Back")))

;(page-url page-overview (resource-name page-overview))

