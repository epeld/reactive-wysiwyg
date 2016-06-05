
(in-package :peldan.virtual-dom)


(defun read-virtual-dom-js ()
  (read-file-to-string "js/virtual-dom.js"))



(ps:defpsmacro json (data)
  `(lisp (encode ,data)))


(defun json-string (data)
  (with-output-to-string (s)
    (encode data s)))


(defun bootstrap-code (html state)
  (ps* 
   `(flet ((render (state)
	     (psx ,html)))
    
      (let* ((tree (render ,(json-string state)))
	     (element (reify tree)))
	((ps:@ document body append-child) element)
	element))))
