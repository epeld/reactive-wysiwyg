
(defpackage :frp-signal
  (:use :common-lisp)
  (:export fmap foldp make-frp-signal frp-signal-value set-value frp-signal-name))


(in-package :frp-signal)


(defvar *frp-signal-counter* 0)


(defstruct frp-signal
  (value)
  (listeners)
  (name (format nil "frp-signal-~a" (incf *frp-signal-counter*)) :type string))


(defun info (&rest args)
  (apply #'format t args))


;; Internal function
(defun set-value (sgnl value)
  (info "~&Setting value of ~a to ~a" (frp-signal-name sgnl) value)
    
  ;; Set value..
  (setf (frp-signal-value sgnl)  value)
  
  ;; Notify listeners
  (loop for f in (frp-signal-listeners sgnl)
       do (funcall f value))
  
  value)


(defun fmap-update-fn (sgnl fn)
  (lambda (v) (set-value sgnl (funcall fn v))))


;; fmap a la Haskell/Elm/Purescript
(defun fmap (fn sgnl)
  "Produces a new signal downstream of sgnl, with a value type matching fn's codomain"

  (let ((new-sgnl (make-frp-signal :value (funcall fn (frp-signal-value sgnl)))))
    
    (push (fmap-update-fn new-sgnl fn)
	  (frp-signal-listeners sgnl))
    
    new-sgnl))


(defun foldp (a->state->state state sgnl-a)

  (let ((new-signal (make-frp-signal :value (funcall a->state->state (frp-signal-value sgnl-a) state))))
    
    (flet ((state-update (a) (set-value new-signal
			  (funcall a->state->state a (frp-signal-value new-signal)))))
      
      (push #'state-update (frp-signal-listeners sgnl-a)))
    
    new-signal))

;; Give foldp a docstring without messing up formatting in my emacs editor..
(setf (documentation 'foldp 'function)
      (format nil "Fold past information into a signal: ~&(a -> State -> State) -> state -> Signal a -> Signal State"))


(defun side-effect (sgnl fn)
  "Add a side effect to this signal. I.e an impure function that gets called on value change"
  (push fn (frp-signal-listeners sgnl)))
