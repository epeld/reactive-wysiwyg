
(in-package :peldan.resource)

(defun name (object)
  (cdr (assoc :name object)))


(defun members (group)
  (cdr (assoc :members group)))


(defun resource-type (object)
  (cdr (assoc :type object)))

(defun find-member (name group &key (test #'string=))
  (find name (members group)
	:key #'name
	:test test))


(defun replace-member (new-member group &key (test #'string=))
  (let ((assoc (assoc :members group)))
    (setf (cdr assoc)
	  (cons new-member 
		(remove (name new-member)
			(cdr assoc)
			:test test
			:key #'name))))
  new-member)


(defun add-member (new-member group)
  "Add a member to a group, shadowing any old member with the same name"
  (push new-member (cdr (assoc :members group)))
  new-member)


(defun empty-group (name)
  "Create a named group without any members (yet)"
  (pairlis '(:members :name :type)
	   `(nil ,name 'group)))


(defmacro defgroup (name &key (test '#'string=))
  (let ((group (new-symbol name "-group")))
    (with-gensyms (name-arg member-arg)
      `(progn
	 (defvar ,group (empty-group ,(string name)))
     
	 (defun ,(new-symbol "find-" name) (,name-arg)
	   (find-member ,name-arg ,group :test ,test))
	 
	 (defun ,(new-symbol "replace-" name) (,member-arg)
	   (replace-member ,member-arg ,group :test ,test))
	 
	 (defun ,(new-symbol "make-" name) (,name-arg)
	   (pairlis '(:name :type) (list ,name-arg ',name)))
	 
	 (defun ,(new-symbol "add-" name) (,member-arg)
	   (add-member ,member-arg ,group))))))

