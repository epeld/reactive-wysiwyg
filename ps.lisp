
(in-package peldan.ps)


(defpsmacro log-message (&rest args)
  `((@ console log) ,@args))


(defpsmacro log-warning (&rest args)
  `((@ console warn) ,@args))



(defmacro defsnippet (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (ps ,@body)))
