
(in-package peldan.ps)


(defpsmacro log-message (&rest args)
  `((@ console log) ,@args))


(defpsmacro log-warning (&rest args)
  `((@ console warn) ,@args))



;; To allow including other files a la npm et al
(defpsmacro load (filename)
  (ps-compile-file filename))




