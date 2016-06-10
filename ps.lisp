
(in-package peldan.ps)


(defpsmacro log-message (&rest args)
  `((@ console log) ,@args))


(defpsmacro log-warning (&rest args)
  `((@ console warn) ,@args))


(defpsmacro json-stringify (object)
  `((@ -j-s-o-n stringify) ,object))


(defpsmacro json-parse (string)
  `((@ -j-s-o-n parse) ,string))

;; To allow including other files a la npm et al
(defpsmacro load (filename)
  (ps-compile-file filename))




