

(defun get-attrs (psx-code)
  (let ((key (first psx-code)))
    (when (keywordp key)
      (cons key (cons (cadr psx-code) (get-attrs (cddr psx-code)))))))


(defun get-content (psx-code)
  (let ((key (first psx-code)))
    (if (keywordp key)
	(get-content (cddr psx-code))
	psx-code)))


(defun psx-to-ps (code)
  (if (typep code 'string)
      code
      (let ((el (first code))
	    (attrs (get-attrs (cdr code)))
	    (content (mapcar #'psx-to-ps (get-content (cdr code)))))
	(if (typep el 'string)
	    `((parenscript:getprop -react '-d-o-m ,el) (parenscript:create ,@attrs) (parenscript:list ,@content))
	    `((parenscript:getprop -react '-d-o-m 'create-element) ,el (parenscript:create ,@attrs) (parenscript:list ,@content))))))

(defmacro psx (forms)
  `(parenscript:ps ,(psx-to-ps (macroexpand forms))))


#|
(psx
 (-my-element :width "33px" :height "50%"
   (:span :id "erik" "Hello, World!")))
|#

