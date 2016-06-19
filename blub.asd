;;;; blub.asd

;; Working name: blub
(asdf:defsystem #:blub
  :description "Web page maker"
  :author "Erik Peldan <erik.peldan+lisp@gmail.com>"
  :license "GNU GPL"
  :depends-on (#:drakma
               #:cl-who
               #:hunchentoot
               #:hunchensocket
               #:parenscript
	       #:alexandria
	       #:cl-html-parse
	       #:yason)
  :serial t
  :components ((:file "package")
	       (:file "dispatch")
	       (:file "component")
	       (:file "list")
	       (:file "symbol")
	       (:file "websocket" :depends-on ("resource"))
	       (:file "resource" :depends-on ("symbol"))
	       (:file "string" :depends-on ("list"))
	       (:file "ps" :depends-on ("list"))
	       (:file "psx" :depends-on ("list"))
	       (:file "simple" :depends-on ("ps" "component"))))

