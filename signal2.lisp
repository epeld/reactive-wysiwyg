
(defpackage :frp-signal
  (:use :common-lisp)
  (:export 
   frp-signal signal-value signal-name find-signal
   fmap foldp make-frp-signal frp-signal-value set-value frp-signal-name))


(in-package :frp-signal)

(defun info (&rest args)
  (apply #'format t args))


;; For giving anonymous signals a placeholder name
(defvar *frp-signal-counter* 0)


(defclass frp-signal
  ()
  ((value :initform (error "Please supply an initial signal value") :initarg :value :reader signal-value)
   (listeners :initform nil)
   (update-fn :initform #'identity :initarg :fn)
   (name :initform (format nil "frp-signal-~a" (incf *frp-signal-counter*)) :initarg :name :reader signal-name)))


(defun push-listener (sgnl-a sgnl-b)
  "Add 'Signal b' as a downstream listener of 'Signal a'"
  
  (with-slots (listeners) sgnl-a
    (pushnew sgnl-b listeners)))


(defun notify-listeners (sgnl)
  "Notify all the signal's listeners that it has a new value"
  
  (with-slots (listeners value) sgnl
    (loop for listener in listeners
       do (notify-updated listener value))
    
    value))


(defun notify-updated (sgnl val)
  "Notify the signal that an upstream update has happened"


  (with-slots (value update-fn listeners name) sgnl
    (info "~%~a got an upstream update: ~a" name val)

    ;; Ask the update-fn how the upstream value looks to us
    (setf value (funcall update-fn val))
    (info "~%~a now has value: ~a" name value)

    (notify-listeners sgnl)))


(defun set-value (sgnl val)
  "Synonym for notify-updated."
  (notify-updated sgnl val))


(defun fmap (fn sgnl-a)
  "fmap : (a -> b) -> Signal a -> Signal b"
  (with-slots (value) sgnl-a
    
    ;; Create a new signal..
    (let ((sgnl-b (make-instance 'frp-signal :fn fn :value (funcall fn value))))
      
      ;; Downstream of Signal a
      (push-listener sgnl-a sgnl-b)
      
      ;; Return Signal b
      sgnl-b)))


(defun foldp (a->state->state state sgnl-a)
  "foldp : (a -> State -> State) -> State -> Signal a -> Signal State)"

  ;; We will close over a state variable 'current-state' reflected in the signal's value
  ;; The difference between fmap and foldp could be said to be this 'memory' of previous states
  (let* ((current-state state)
	 (sgnl-state (make-instance 'frp-signal
			
				    ;; Apply the fold-function to the initial a
				    :value 
				    (funcall a->state->state (signal-value sgnl-a) current-state)
				    
				    ;; .. and to all coming ones
				    :fn (lambda (a)
					  (setf current-state (funcall a->state->state a current-state))))))
    
    (push-listener sgnl-a sgnl-state)
    
    sgnl-state))


(defun find-signal (name list)
  "Find a signal from a list, given its name"
  (or (find name list :test #'signal-name)
      (error "Unknown signal ~a" name)))
