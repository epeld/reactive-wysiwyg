
(in-package peldan.component)


(defun add-parameter (lambda-list name)
  (append lambda-list (list name)))


(defmacro defcomponent (name lambda html)
  (with-gensyms (stream)
    `(defun ,name ,(add-parameter lambda stream)
       (the stream ,stream)
       (with-html-output (,stream)
	   ,html))))


(defmacro render (component &rest args)
  (with-gensyms (var)
    `(with-html-output-to-string (,var)
       (,component ,@args ,var))))

