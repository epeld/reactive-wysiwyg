
(in-package :peldan.virtual-dom)


(defun read-virtual-dom-js ()
  (read-file-to-string "js/virtual-dom.js"))


(defun json-string (data)
  (with-output-to-string (s)
    (encode-alist data s)))


(defun render-ps (psx state)
  "Render and attach the resulting psx element to the DOM, returning a lambda for changing state"
  `(flet ((render (state)
	    ,psx))
    
     (let* ((tree (render ,state))
	    (element (reify tree)))
	    
       ((ps:@ document body append-child) element)
	
       (lambda (new-state)
	 (setf ,state new-state)
	 (let* ((new-tree (render new-state))
		(patches (diff-tree tree new-tree)))
	   (setf tree new-tree)
	   (apply-patch element patches))))))

