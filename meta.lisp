
(in-package :peldan.websocket)

;; 
;; Meta
;; 
(defun find-session (uuid meta)
  "Find a session withing the meta session"
  (find uuid (slot-value meta 'sessions) 
	:key #'uuid
	:test #'string-equal))


(defun add-session (session meta)
  "Add a new app session to the meta session"
  (the meta-session meta)
  (the app-session session)
  (pushnew session (slot-value meta 'sessions)))


(defun clear-sessions (meta)
  (setf (slot-value meta 'sessions)
	nil))


(defun add-action (state &optional (uuid (peldan.string:generate-uuid)))
  (lambda (meta)
    (add-session (make-instance (slot-value meta 'app-session-class)
				:uuid uuid
				:state state)
		 meta)))


(defclass meta-session (peldan.state:stateful session)
  ((sessions :type list
	     :reader sessions
	     :initform nil)
   (app-session-class :type class
		      :initform 'app-session)) 
  (:documentation "A session about sessions")
  (:default-initargs :actions '(("add" . add-action)
				("clear" . clear-sessions))))


(defmethod peldan.state:execute (action (s meta-session))
  (the peldan.state:action action)
  (let ((fn (if (= 1 (length action))
		(symbol-function (first action))
		(eval action))))
    
    (funcall fn s)
    (call-next-method)))


(defmethod peldan.state:current-state ((s meta-session))
  ;; TODO also attach uuid
  (mapcar #'peldan.state:current-state (slot-value s 'sessions)))


(peldan.state:execute '(add-action '(:name "Abraham") "12345") *meta*)
(peldan.state:execute '(clear-sessions) *meta*)
