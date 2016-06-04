
(in-package peldan.ps)


(defpsmacro log-message (&rest args)
  `((@ console log) ,@args))


(defpsmacro log-warning (&rest args)
  `((@ console warn) ,@args))



(defmacro defsnippet (name lambda-list &body body)
  `(defun ,name ,lambda-list
     (ps ,@body)))


;; To allow including other files a la npm et al
(defpsmacro load (filename)
  (ps-compile-file filename))


(defpsmacro include (filename)
  (with-open-file (s (concatenate 'string "js/" filename))
    (loop for line = (read-line s nil 'foo)
       until (eq line 'foo)
       do (write-line line ps:*parenscript-stream*))))
