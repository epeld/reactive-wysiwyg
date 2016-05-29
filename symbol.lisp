
(in-package :peldan.symbol)



(defmacro new-symbol (&rest args)
  `(intern (string-upcase (apply #'concatenate 'string (mapcar #'string (list ,@args))))))




