
(in-package :peldan.resource)

(defun field-value (key object)
  (assocdr key object))

(defun name (object)
  (field-value :name object))


(defun members (group)
  (field-value :members group))


(defun unique-members (group key)
  (remove-duplicates (members group)
		     :key (if (keywordp key)
			      (lambda (item) (field-value key item))
			      key)))


(defun resource-type (object)
  (field-value :type object))


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
  (let ((group (new-symbol name "-group"))
	(sname (string name)))
    (with-gensyms (name-arg member-arg rest)
      `(progn
	 (defvar ,group (empty-group ,sname))
     
	 (defun ,(new-symbol "find-" name) (,name-arg &key (force nil))
	   (or (find-member ,name-arg ,group :test ,test)
	       (and force
		    (error ,(concatenate 'string "Unknown " sname ": ~a")
			   ,name-arg))))
	 
	 (defun ,(new-symbol "replace-" name) (,member-arg)
	   (replace-member ,member-arg ,group :test ,test))
	 
	 (defun ,(new-symbol "make-" name) (,name-arg &rest ,rest)
	   ,(concatenate 'string "Construct a new " sname " with the given name and fields")
	   (pairlis '(:name :type)
		    (list ,name-arg ',name)
		    (plist-to-alist ,rest)))
	 
	 (defun ,(new-symbol "add-" name) (,member-arg)
	   (add-member ,member-arg ,group))))))

