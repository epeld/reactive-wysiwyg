
(defpackage peldan.simple
  (:use common-lisp parenscript)
  (:import-from parenscript ps ps*)
  (:import-from cl-who str)
  (:import-from peldan.component 
		defcomponent
		defcomponent-macro
		render
		component))

(in-package peldan.simple)


(defcomponent-macro javascript (&body ps)
  `(:script :type "text/javascript" (str (ps ,@ps))))


(defcomponent notice ()
  (:p "Please wait a while as the connection is being set up.."))


(defcomponent hello-world ()
  (:div (:h1 "Hello, World")
	(component #'notice)
	(:iframe)
	(javascript ((ps:@ console log) "Hi"))))


(defun simple-handler (request)
  (declare (ignore request))
  (render hello-world))


(render hello-world)
;(psx ())


;(pushnew (lambda (req) (simple-handler req)) peldan.dispatch:*handlers*)
