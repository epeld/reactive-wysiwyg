
(in-package :peldan.action)


(defvar *actions* (make-hash-table :test #'equal))



(defun register-action (action-function &optional (name (string action-function)))
  "Register a new action, passing in the action function's symbol"
  (the symbol action-function)
  (setf (gethash name *actions*)
	action-function)
  name)


(defun find-action (action)
  "Return the appropriate function to invoke based on the action name"
  (the string action)
  (symbol-function
   (the symbol 
	(or (gethash (string-upcase action) *actions*)
	    (error "Unkown action ~a" action)))))


;; Test actions

(defun toggle-debug ()
  (lambda (state)
    (peldan.data:map-inside #'not state :debug)))

(defun randomize ()
  (lambda (state)
    (peldan.data:map-inside (lambda (item)
			      (loop for i in item collect (random 100))) 
			    state
			    :data :items 10)))

(defun generate-rows (count)
  (lambda (state)
    (peldan.data:set-inside (loop for i upto (min 500 (- count 1)) collect
				 (loop for j upto 12 collect (random 100))) 
			    state
			    :data :items)))

(register-action 'toggle-debug "DEBUG")

(register-action 'generate-rows)

(register-action 'randomize)
