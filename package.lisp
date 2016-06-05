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
	   :read-file-to-string
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
  (:import-from :cl-who :str :htm)
  (:import-from :peldan.ps :log-message)
  (:import-from :peldan.psx :psx)
  (:import-from :peldan.virtual-dom :read-virtual-dom-js :render-loop)
  (:import-from :peldan.component :defcomponent :render))


(defpackage :peldan.websocket
  (:use :common-lisp :hunchensocket :hunchentoot)
  (:import-from :yason :*parse-object-as* :parse :encode)
  (:import-from :peldan.resource :defgroup :named)
  (:import-from :peldan.list :assocdr)
  (:export :*port*))


(defpackage :peldan.resource
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms)
  (:import-from :peldan.symbol :new-symbol)
  (:import-from :peldan.list :plist-to-alist)
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


(defpackage :peldan.action
  (:use :common-lisp :parenscript)
  (:export :defaction))


(defpackage :peldan.virtual-dom
  (:use :common-lisp :parenscript)
  (:import-from :peldan.ps :load)
  (:import-from :yason :encode)
  (:import-from :peldan.string :read-file-to-string)
  (:import-from :peldan.psx :psx :psx* :diff-tree :apply-patch)
  (:export :render-loop))
