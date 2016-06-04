

;; Define a context for working with the virtual DOM
(ps:defpsmacro with-virtual-dom (&body body)
  `(macrolet ((create-reactive-element (name attrs children)
		`((ps:@ virtual-dom h) ,name ,attrs ,children))
	   
	      (reify (arg)
		`((ps:@ virtual-dom create) ,arg)))
  
     ,@body))



(ps:defpsmacro component-loop (psx state)
  `(with-virtual-dom
     (flet ((render (state)
	      ;; TODO compile with an action context
	      (psx ,psx)))
    
       (let* ((tree (render (state ,state)))
	      (element (reify tree)))
	 
	 ;; TODO hide this using some macro:
	 ((ps:@ document body appendChild) element)
	 
	 element))))
