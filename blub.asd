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
  :components ((:file "package") 	;TODO this is a mess. fix
	       (:file "dispatch")
	       (:file "component")
	       (:file "stateful")
	       (:file "app-state" :depends-on ("stateful"))
	       (:file "websocket")
	       (:file "messages")
	       (:file "session" :depends-on ("messages"))
	       (:file "string")
	       (:file "ps")
	       (:file "virtual-dom" :depends-on ("ps" "string"))
	       (:file "ml" :depends-on ("virtual-dom"))))

