
(in-package :peldan.action)


(defmacro defaction (name &rest args)
  )


(defpsmacro defaction (name &rest args)
  )


(defpsmacro action (name &rest args)
  "This executes an action when I get around to it!"
  (if *server-side-logic*
      "serverside"
      "clientside"))
