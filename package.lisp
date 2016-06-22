;;;; package.lisp

(defpackage :peldan.dispatch
  (:use :common-lisp :hunchentoot)
  (:export :*handlers* :server :start-server))


(defpackage :peldan.component
  (:use :common-lisp :parenscript :peldan.psx)
  (:import-from :cl-who 
		:htm
		:with-html-output
		:with-html-output-to-string)
  (:import-from :peldan.dispatch :*handlers*)
  (:import-from :peldan.resource :name :field-value :defgroup :members)
  (:export :request-handler
	   :register-component
	   :state
	   :action
	   :generate-component-html
	   :make-component))


(defpackage :peldan.list
  (:use :common-lisp)
  (:export :transpose :alist-to-plist :plist-to-alist))


(defpackage :peldan.string
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms)
  (:import-from :peldan.list :transpose)
  (:export :strings-bind
	   :starts-with-p
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
  (:export :log-message :log-warning :json-stringify :json-parse))


(defpackage :peldan.websocket
  (:use :common-lisp :hunchensocket :hunchentoot)
  (:import-from :yason :*parse-object-as* :parse :encode)
  (:import-from :peldan.resource :defgroup :named)
  (:import-from :peldan.list :assocdr)
  (:import-from :peldan.virtual-dom :json-string)
  (:export :*port* :connect-ps :websockets-enabled))


(defpackage :peldan.resource
  (:use :common-lisp)
  (:import-from :alexandria :with-gensyms)
  (:import-from :peldan.symbol :new-symbol)
  (:import-from :peldan.list :plist-to-alist :assocdr)
  (:export :replace-resource
	   :find-resource
	   :resource
	   :resource-name
	   :defgroup
	   :members
	   :name
	   :field-value
	   :unique-members))


(defpackage :peldan.symbol
  (:use :common-lisp)
  (:export :new-symbol))



(defpackage :peldan.action
  (:use :common-lisp :parenscript :peldan.resource)
  (:export :find-action))


(defpackage :peldan.virtual-dom
  (:use :common-lisp :parenscript)
  (:import-from :peldan.ps :load)
  (:import-from :yason :encode :encode-alist)
  (:import-from :peldan.string :read-file-to-string :read-file-to-stream)
  (:import-from :peldan.psx :psx :psx* :diff-tree :apply-patch)
  (:export :render-ps :json-string :read-virtual-dom-js))


(defpackage :peldan.debugger
  (:use :common-lisp :parenscript :peldan.action :peldan.ps)
  (:import-from :peldan.resource :members)
  (:import-from :peldan.psx :psx)
  (:export :debugger))


(defpackage :peldan.editor
  (:use :common-lisp :parenscript :peldan.action)
  (:import-from :peldan.psx :psx)
  (:export :generate))


(defpackage :peldan.import
  (:use :common-lisp :parenscript :drakma)
  (:import-from :cl-html-parse :parse-html))


(defpackage :peldan.data
  (:use :common-lisp)
  (:export :encode-nested-plist 
	   :map-inside
	   :set-inside
	   :find-keyword))
