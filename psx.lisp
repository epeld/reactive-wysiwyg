
(defpackage :peldan.psx
  (:use :common-lisp :peldan.alist :cl-who)
  (:export :psx :psx-to-who :psx-to-html))

;(ql:quickload "cl-who")
(in-package :peldan.psx)


(defun extract-element-parts (sexp)
  "Extract the parts from an HTML-like sexp"
  (the list sexp)
  (let (attr-list body)
    (loop for rest on (rest sexp) by #'cddr
       if (keywordp (first rest))
       collect (cons (first rest) (second rest)) into attr
       else
       do (progn (setq attr-list attr)
		 (setq body rest)
		 (return)))
    
    (values (first sexp) attr-list body)))


(defun psx-element (sexp)
  (multiple-value-bind (first attrs children) sexp
    '(create-element )))


(defun psx-atom (atom)
  "Compile an atom"
  (cond
    ((keywordp atom)
     (psx-element `(,atom)))
    
    (otherwise
     (string atom))))


(defun psx-list (sexp)
  (let ((first (first sexp)))
    (etypecase first
      (keyword
       (psx-element sexp))
      
      (string
       (psx-element sexp))
      
      (symbol
       'funcall))))


(defun psx (sexp)
  "Compile html-like DSL using captured psx-element, psx-attrs, psx-children"
  (if (atom sexp)
    (psx-atom sexp)
    (psx-list sexp)))



#|
;(ql:quickload "cl-who")

(defun test2 (x &optional s)
  (cl-who:with-html-output (s)
    (:div "Hello Test2" (cl-who:str x))))

(defun test1 (&optional s)
  (labels ((component (name &rest args)
	     (apply name (append args (list s)))))
    (cl-who:with-html-output (s)
      (:div "Hello Test1" (component #'test2 3)))))

(cl-who:with-html-output (s)
      (:div "Hello Test1" (component #'test2 3)))


(cl-who:with-html-output-to-string (s)
  (test1 s))
|#




(quote (psx-to-who (:div :class "abc" "Hello, World!" ".")))

(quote (psx-to-html (:div :class "abc" (:p "Hello, World!")
		   (:p "blub")
		   (mapcar (lambda (x) (write-to-string (+ x 3))) (list 1 2 3))
		   (:div (:span "erik")))))

