



(defun finalp (field)
  (not (zerop (logand (jfield "java.lang.reflect.Modifier" "FINAL") (jcall "getModifiers" field)))))


(defun make-subeditor (java-object field)
  (if (finalp field)
      (jnew (jclass "javax.swing.JLabel") (jcall "getName" field))
      (jnew (jclass "javax.swing.JLabel") "TODO")))



(defvar test-class (jclass "java.lang.String"))


(defun jfields (java-object)
  (coerce (jcall "getFields" (jclass java-object)) 'list))

(defparameter *windows* nil)

(defun window-title (java-object)
  (format nil "Editor for ~a" (jcall "getSimpleName" (jcall "getClass" java-object))))


(defun initialize-window (window)
  (jcall "setVisible" window t))


(defun add (container child)
  (jcall "add" container child))

(defmacro jnew-progn (init-args &body body)
  `(let ((it (jnew ,@init-args)))
     ,@body
     it))

(defun make-label (&rest args)
  (jnew-progn ("javax.swing.JLabel" (apply #'format nil args))
    (jcall "setHorizontalAlignment" it (jfield "javax.swing.SwingConstants" "CENTER"))))


(defun class-name (java-object)
  (jcall "getSimpleName" (jcall "getClass" java-object)))


(defun java-class (java-object)
  (jcall "getClass" java-object))


(defun pack (obj)
  (jcall "pack" obj))

(defun box-layout (window &optional (axis "Y_AXIS"))
  (let ((layout (jnew "javax.swing.BoxLayout"
		      (if (jcall "isInstance" (jclass "javax.swing.RootPaneContainer") window) 
			  (jcall "getContentPane" window)
			  window)
		      (jfield "javax.swing.BoxLayout" axis))))
    (jcall "setLayout" window layout)))


(defun make-editor-window (java-object)
  (jnew-progn ("javax.swing.JFrame" (window-title java-object))
    (initialize-window it)
    (push it *windows*)
    (add it (make-label "Fields in ~a" (class-name java-object)))
    (add it (make-editor java-object))
    (pack it))



(defun make-editor (java-object)
  (jnew-progn ("javax.swing.JPanel")
    (box-layout it)
    (loop for f in (jfields (java-class java-object))
       do (add it (make-subeditor java-object f)))))

(make-editor-window 3)



