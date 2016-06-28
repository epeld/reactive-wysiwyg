;;;; package.lisp

(defpackage :peldan.dispatch
  (:use :common-lisp :hunchentoot)
  (:export :*handlers* :server :start-server))


(defpackage :peldan.component
  (:use :common-lisp :parenscript)
  (:import-from :cl-who 
		:htm
		:with-html-output
		:with-html-output-to-string)
  (:import-from :peldan.string :generate-uuid)
  (:export :generate-component-html 
	   :state))


(defpackage :peldan.string
  (:use :common-lisp)
  (:export :strings-bind
	   :starts-with-p
	   :read-file-to-string
	   :split-by
	   :replace-all
	   :generate-uuid))


(defpackage :peldan.ps
  (:use :common-lisp :parenscript)
  (:export :log-message :log-warning :json-stringify :json-parse))


(defpackage :peldan.state
  (:use :common-lisp)
  (:export :stateful 
	   :action
	   :execute
	   :current-state
	   :app-state
	   :execute
	   :toggle-debug
	   :update-state))


(defpackage :peldan.websocket
  (:use :common-lisp :hunchensocket :hunchentoot)
  (:import-from :yason :*parse-object-as* :parse :encode)
  (:import-from :peldan.virtual-dom)
  (:export :*port* 
	   :connect-ps
	   :websockets-enabled
	   :*sessions*
	   :app-session
	   :meta-session
	   :add-session
	   :clear-sessions
	   :*meta*))



(defpackage :peldan.virtual-dom
  (:use :common-lisp :parenscript)
  (:import-from :peldan.ps :load)
  (:import-from :peldan.string :read-file-to-string :read-file-to-stream)
  (:export :library-js :action :create-element))


(defpackage :peldan.debugger
  (:use :common-lisp :parenscript :peldan.ps)
  (:export :debugger))


(defpackage :peldan.data
  (:use :common-lisp)
  (:export :encode-nested-plist 
	   :map-inside
	   :set-inside
	   :find-keyword))


(defpackage :peldan.ml
  (:use :common-lisp)
  (:export :generate-hyperscript :h))

