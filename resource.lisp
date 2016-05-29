
(in-package :peldan.resource)


(defclass named ()
  ((name :reader name
	 :initarg :name
	 :initform (error "A name is required")))
  (:documentation "Something that has a name of some sort"))


(defclass group (named)
  ((members :reader members
	      :type list))
  
  (:documentation "A group of resources"))


(defun find-member (name group &key (test #'equal))
  (find name (members group) :key #'name :test test))



(defun replace-member (new-member group &key (test #'equal))
  (with-slots (members) group
    (setf members (cons new-member 
			(remove (name new-member)
				members
				:test test
				:key #'name 
				:count 1))))
  new-member)


(defun add-member (new-member group)
  (with-slots (members) group
    (setf members (cons new-member members)))
  new-member)


;; TODO verify works!
(defmacro defgroup (name &key (test '#'equal))
  (let ((group (new-symbol name "-group")))
    (with-gensyms (name-arg member-arg)
      `(progn
	 (defvar ,group (make-instance 'group 
				       :name ,(string name)))
     
	 (defun ,(new-symbol "find-" name) (,name-arg)
	   (find-member ,name-arg ,group :test ,test))
	 
	 (defun ,(new-symbol "replace-" name) (,member-arg)
	   (replace-member ,member-arg ,group :test ,test))
	 
	 (defun ,(new-symbol "add-" name) (,member-arg)
	   (add-member ,member-arg ,group))))))

