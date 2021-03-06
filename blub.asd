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
	       (:file "ps")
	       (:file "url")
	       (:file "data")
	       (:file "string")
	       (:file "stateful")
	       (:file "messages")
	       (:file "session")
	       (:file "app-state")
	       (:file "websocket")
	       (:file "virtual-dom" :depends-on ("ps" "string"))
	       (:file "ml" :depends-on ("virtual-dom"))
	       (:file "debugger")
	       (:file "view")
	       (:file "page" :depends-on ("view"))))

