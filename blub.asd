;;;; blub.asd

;; Working name: blub
(asdf:defsystem #:blub
  :description "Web page maker"
  :author "Erik Peldan <erik.peldan+lisp@gmail.com>"
  :license "GNU"
  :depends-on (#:drakma
               #:cl-who
               #:hunchentoot
               #:hunchensocket
               #:parenscript
	       #:alexandria)
  :serial t
  :components ((:file "package")
	       (:file "dispatch")
	       (:file "component")
	       (:file "list")
	       (:file "websocket")
	       (:file "string" :depends-on ("list"))
	       (:file "ps" :depends-on ("list"))
	       (:file "psx" :depends-on ("list"))
	       (:file "simple" :depends-on ("ps" "component"))))

