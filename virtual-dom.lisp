
(in-package :peldan.virtual-dom)


(defun read-virtual-dom-js ()
  (read-file-to-string "js/virtual-dom.js"))



(ps:defpsmacro json (data)
  `(lisp (encode ,data)))


(defun json-string (data)
  (with-output-to-string (s)
    (encode data s)))


(defun make-renderer (html state)
  `(flet ((render (state)
		 (psx ,html)))
    
	  (let* ((state ((@ -j-s-o-n parse) ,(json-string state)))
		 (tree (render state))
		 (element (reify tree)))
	    
	    ((ps:@ document body append-child) element)
	
	    (lambda (new-state)
	      (setf state new-state)
	      (let* ((new-tree (render new-state))
		     (patches (diff-tree tree new-tree)))
		(setf tree new-tree)
		(apply-patch element patches))))))


(defun render-loop (html state)
  (ps* 
   `(defvar
	set-state
      ,(make-renderer html state))))
