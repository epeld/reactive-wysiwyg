
(in-package :peldan.session)



;; 
;; Meta
;; 
(defun find-session (uuid &optional (meta websocket:*meta*))
  "Find a session within the meta session"
  (if (string-equal uuid (uuid meta))
    meta
    (find uuid (slot-value meta 'sessions) 
	  :key #'uuid
	  :test #'string-equal)))


(defun add-session (meta state uuid)
  "Add a new app session to the meta session"
  (the meta-session meta)
  (let ((s (make-instance (slot-value meta 'app-session-class)
			  :state state
			  :uuid uuid)))
    (pushnew s (slot-value meta 'sessions))))


(defun clear-sessions (meta)
  (setf (slot-value meta 'sessions) nil))


(defun reset (meta)
  (setf (slot-value meta 'peldan.state:action-log) nil)
  (clear-sessions meta))


(defclass meta-session (base-session peldan.state:stateful)
  ((sessions :type list
	     :reader sessions
	     :initform nil)
   (app-session-class :type class
		      :initform 'app-session)) 
  (:documentation "A session about sessions")
  (:default-initargs 
      :actions '(("add" . add-session)
		 ("clear" . clear-sessions)
		 ("reset" . reset))
    :client-class 'hunchensocket-client))


(defmethod yason:encode ((s meta-session) &optional (stream *standard-output*))
  (yason:with-output (stream)
    (yason:with-object ()
      (yason:encode-object-element "uuid" (uuid s))
      (yason:with-object-element ("log")
	(yason:with-array ()
	  (loop for element in (slot-value s 'peldan.state:action-log) do
	       (yason:encode-array-element element))))
      (yason:with-object-element ("data")
	(yason:with-array ()
	  (loop for element in (slot-value s 'sessions) do
	       (yason:encode-array-elements element)))))))


(defmethod peldan.state:update-state (fn (s meta-session))
  (funcall fn s))


