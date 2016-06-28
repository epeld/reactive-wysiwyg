
(in-package :peldan.websocket)

;; 
;; Meta
;; 
(defun find-session (uuid meta)
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


(defclass meta-session (peldan.state:stateful session)
  ((sessions :type list
	     :reader sessions
	     :initform nil)
   (app-session-class :type class
		      :initform 'app-session)) 
  (:documentation "A session about sessions")
  (:default-initargs :actions '(("add" . add-session)
				("clear" . clear-sessions))))


(defmethod peldan.state:current-state ((s meta-session))
  `(:data
    ,(loop for session in (slot-value s 'sessions) collect
	 `(:data ,(peldan.state:current-state session) :uuid ,(uuid session)))
    
    :uuid ,(uuid s)))


(defmethod peldan.state:update-state (fn (s meta-session))
  (funcall fn s))


;(peldan.state:execute '(add-session (:name "Abraham") "12345") *meta*)
;(peldan.state:execute '(clear-sessions) *meta*)
