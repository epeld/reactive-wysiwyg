
(defpackage :peldan.simple
  (:use :common-lisp :parenscript))


(defun simple-handler (request)
  (declare (ignore request))
  (render #'hello-world)
  "Hello, World!")


(psx ())


(pushnew #'simple-handler peldan.dispatch:*handlers*)
