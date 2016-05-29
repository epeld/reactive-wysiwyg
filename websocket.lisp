;; Define the websocket interface of this server

(in-package peldan.websocket)


(defgroup session)

(defparameter *port* 3344)


(defclass session (hunchensocket:websocket-resource named)
  ()
  (:default-initargs :client-class 'client))


(defclass client (hunchensocket:websocket-client named)
  ())



(defun session-for-request (request)
  "Hunchensocket request dispatch function"
  (let ((uuid (hunchentoot:script-name request)))
    (or (find-session uuid)
	(add-session uuid))))
