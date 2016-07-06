;;;; package.lisp

#+sbcl
(progn
  (sb-ext:lock-package :hunchensocket)
  (sb-ext:lock-package :hunchentoot))


(defpackage :peldan.dispatch
  (:nicknames :dispatch)
  (:use :common-lisp :hunchentoot)
  (:export :*handlers*
	   :server
	   :start-server
	   :install-handler))


(defpackage :peldan.string
  (:use :common-lisp)
  (:export :strings-bind
	   :starts-with-p
	   :read-file-to-string
	   :read-file-to-stream
	   :split-by
	   :replace-all
	   :generate-uuid))

(defpackage :peldan.ps
  (:use :common-lisp :parenscript)
  (:export :log-message
	   :log-warning
	   :json-stringify
	   :json-parse
	   :defps
	   :generate-user-js))


(defpackage :peldan.virtual-dom
  (:nicknames :virtual-dom)
  (:use :common-lisp :parenscript)
  (:import-from :peldan.ps :load)
  (:import-from :peldan.string :read-file-to-string :read-file-to-stream)
  (:export :library-js 
	   :action
	   :create-element
	   :make-module))


(defpackage   :peldan.message
  (:nicknames :message)
  (:use       :cl)
  (:export :unknown-type-message
	   :hello-message
	   :send-message
	   :error-message
	   :make-message
	   :broadcast
	   :pong-message
	   :run-action)
  (:documentation "Utils for creating websocket messages"))


(defpackage   :peldan.session
  (:nicknames :session)
  (:use       :cl :message)
  (:export :app-session
	   :uuid
	   :session-actions
	   :base-session
	   :register-actions
	   :execute-action
	   :message-received
	   :state-message)
  (:documentation "Different kinds of data sessions"))


(defpackage :peldan.websocket
  (:nicknames :websocket)
  (:use :common-lisp)
  (:import-from :yason :*parse-object-as* :parse :encode)
  (:import-from :peldan.virtual-dom)
  (:export :*port* 
	   :connect-ps
	   :start-server
	   :stop-server
	   :websockets-enabled
	   :*sessions*
	   :find-session))



(defpackage :peldan.state
  (:nicknames :state)
  (:use :common-lisp)
  (:export :stateful 
	   :action
	   :execute
	   :current-state
	   :app-state
	   :state
	   :action-log
	   :execute
	   :toggle-debug
	   :update-state))


(defpackage :peldan.debugger
  (:use :common-lisp :parenscript :peldan.ps)
  (:export :debugger))


(defpackage :peldan.data
  (:nicknames :data)
  (:use :common-lisp)
  (:export :encode-nested-plist 
	   :map-inside
	   :set-inside
	   :find-keyword
	   :traverse
	   :generate-mappings))


(defpackage :peldan.ml
  (:nicknames :ml)
  (:use :common-lisp)
  (:export :generate-hyperscript
	   :find-actions
	   :h))


(defpackage :peldan.view
  (:nicknames :view)
  (:use :cl)
  (:export :encode-symbol
	   :view
	   :action
	   :make-view
	   :view-name
	   :view-actions
	   :view-ps
	   :*default-session*))

(defpackage :peldan.url
  (:nicknames :url)
  (:use :cl)
  (:import-from :hunchentoot :script-name)
  (:export :uuid-from-script-name
	   :session-url))


(defpackage :peldan.page
  (:use :cl :hunchentoot))
