;;;; package.lisp

(defpackage peldan.dispatch
  (:use :common-lisp :hunchentoot)
  (:export :*handlers* :server :start-server))


(defpackage peldan.component
  (:use :common-lisp)
  (:import-from :cl-who 
		:htm
		:with-html-output
		:with-html-output-to-string)
  (:import-from :alexandria :with-gensyms)
  (:export :defcomponent :defcomponent-macro :render :component))


(defpackage :peldan.list
  (:use :common-lisp)
  (:export :transpose :alist-to-plist :plist-to-alist))



(defpackage :peldan.string
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms)
  (:import-from :peldan.list :transpose)
  (:export :strings-bind
	   :string-bind-case
	   :string-var-match
	   :split-by
	   :replace-all))


(defpackage :peldan.psx
  (:use :common-lisp)
  (:import-from :peldan.list :transpose :plist-to-alist :alist-to-plist)
  (:export :psx))
