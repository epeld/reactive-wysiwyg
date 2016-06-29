;;;; package.lisp

(defpackage :peldan.dispatch
  (:use :common-lisp :hunchentoot)
  (:export :*handlers* :server :start-server))


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
  (:export :log-message :log-warning :json-stringify :json-parse))


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
	   :clear-sessions)
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
  (:use :common-lisp :parenscript)
  (:import-from :cl-who 
		:htm
		:with-html-output
		:with-html-output-to-string)
  (:import-from :peldan.string :generate-uuid)
  (:export :generate-component-html 
	   :state))


(defpackage :peldan.state
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
  (:use :common-lisp)
  (:export :encode-nested-plist 
	   :map-inside
	   :set-inside
	   :find-keyword))


(defpackage :peldan.ml
  (:use :common-lisp)
  (:export :generate-hyperscript :h))

