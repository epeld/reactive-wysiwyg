
;; Define the websocket interface of this server
(defpackage peldan.websocket
  (:use common-lisp hunchensocket)
  (:export "*PORT*"))

(in-package peldan.websocket)


(defparameter *port* 3344)

;(ql:quickload "hunchensocket")



;; TODO make it so pages can connect and somehow get a uuid
;; that we will then be able to POST to that url to supply the page
;; with new state!


(defclass page-client (hunchensocket:websocket-resource)
  ((uuid :initarg :uuid :initform (error "Each page must have a uuid") :reader uuid))
  (:default-initargs :client-class 'uuid))

(defclass page-client (hunchensocket:websocket-client)
  ((uuid :initarg :uuid :reader uuid :initform (error "Each page must have a uuid"))))
