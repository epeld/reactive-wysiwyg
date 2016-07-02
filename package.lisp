;;;; package.lisp

#+sbcl
(progn
  (sb-ext:lock-package :hunchensocket)
  (sb-ext:lock-package :hunchentoot))


(defpackage :peldan.dispatch
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
  (:use :common-lisp :parenscript)
  (:import-from :peldan.ps :load)
  (:import-from :peldan.string :read-file-to-string :read-file-to-stream)
  (:export :library-js :action :create-element))

(defpackage   :peldan.message
  (:nicknames :message)
  (:use       :cl)
  (:export :unknown-message
	   :hello-message
	   :state-message
	   :send-message
	   :make-message
	   :broadcast
	   :pong
	   :run-action)
  (:documentation "Utils for creating websocket messages"))

(defpackage   :peldan.session
  (:nicknames :session)
  (:use       :cl :message)
  (:export :uuid
	   :actions
	   :app-session
	   :meta-session
	   :add-session
	   :find-session
	   :clear-sessions
	   :ensure-action-exists)
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
	   :find-session
	   :*meta*))


(defpackage :peldan.component
  (:nicknames :component)
  (:use :common-lisp :parenscript)
  (:import-from :cl-who 
		:htm
		:with-html-output
		:with-html-output-to-string)
  (:import-from :peldan.string :generate-uuid)
  (:export :generate-component-html 
	   :component-module-ps
	   :component-session-ps
	   :state
	   :action
	   :find-server-actions))


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
	   :traverse))


(defpackage :peldan.ml
  (:nicknames :ml)
  (:use :common-lisp)
  (:export :generate-hyperscript
	   :h))


(defpackage :peldan.page
  (:nicknames :page)
  (:use :cl :hunchentoot)
  (:import-from :peldan.component
		:action)
  (:export :session-url 
	   :session-page))
