;;;; package.lisp

(defpackage :peldan.dispatch
  (:use :common-lisp :hunchentoot)
  (:export :*handlers* :server :start-server))


(defpackage :peldan.component
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


(defpackage :peldan.ps
  (:use :common-lisp :parenscript)
  (:import-from :peldan.list :transpose)
  (:export :log-message :defsnippet))


(defpackage :peldan.simple
  (:use :common-lisp :parenscript :peldan.ps)
  (:import-from :cl-who :str)
  (:import-from :peldan.ps :defsnippet :log-message)
  (:import-from :peldan.component 
		:defcomponent
		:defcomponent-macro
		:render
		:component))


(defpackage :peldan.websocket
  (:use :common-lisp :hunchensocket)
  (:export :*port*))


(defpackage :peldan.resource
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms)
  (:import-from :peldan.symbol :new-symbol)
  (:export :replace-resource
	   :find-resource
	   :resource
	   :resource-name))


(defpackage :peldan.symbol
  (:use :common-lisp)
  (:export :new-symbol))


(defpackage :peldan.state
  (:use :common-lisp)
  (:import-from :peldan.resource :defgroup))
