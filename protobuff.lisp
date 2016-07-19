
(in-package :peldan.protobuff)

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline) :test #'char=))


(defun read-string (word stream)
  "Read the expected word or raise an error"
  (loop 
     for char across word
     unless (eql char (peek-char nil stream))
     do (error "Error reading '~a' at position ~a" word (file-position stream))
     do (read-char stream))
  word)


(defun parse-word (stream)
  "Read a space-delimited word from stream"
  (let (word)
    (loop 
       for char = (peek-char nil stream)
       until (whitespace-p char)
       do (progn (push char word)
		 (read-char stream)))
    
    (unless word
      (error "Was expecting a word at position ~a" (file-position stream)))
    
    (coerce (reverse word) 'string)))


(defun parse-delimited (delimiter stream)
  "Read everything between two delimiters"
  (unless (char= (peek-char nil stream) delimiter)
    (error "Expected starting delimiter: ~a" delimiter))

  (read-char stream)
  (loop
       for char = (read-char stream nil 'eof)
       when (eql char 'eof)
       do (error "Reached EOF while looking for closing delimiter: ~a" delimiter)
       until (char= char delimiter)
       with chars
       do (push char chars)
       finally (return (coerce (reverse chars) 'string))))

(defun read-whitespace (stream)
  "Read until no more whitespace remains"
  (peek-char t stream nil))


(defun parse-many (fn stream)
  (loop
       with parsed
       for pos = (file-position stream)
       do (handler-case (push (funcall fn stream) 
			      parsed)
	    (error ()
	      (file-position stream pos)
	      (return (reverse parsed))))))

(defun parse-number (stream)
  (loop 
     with chars = nil
     while (digit-char-p (peek-char nil stream nil #\Space))
     do (push (read-char stream) chars)
     finally (if chars
		 (return (parse-integer (coerce (reverse chars) 'string)))
		 (error "Expected number at ~a" (file-position stream)))))


(defmacro parse-either (stream &body choices)
  (let ((pos (gensym "stream-position"))
	(result (gensym "result")))
    
    `(catch ',result 
       (let ((,pos (file-position ,stream)))
	 ,@(loop for choice in choices collect
		`(handler-case (throw ',result ,choice)
		   ;; Reset the position on error
		   (error ()
		     (file-position ,stream ,pos))))
	 
	 (error "Expected one of ~a at position ~a" ',choices ,pos)))))


(defun parse-field (stream)
  "Tries parsing something like:
required string query = 1;"
  (let* (option type name ordinal)
    (read-whitespace stream)
    (setf option (parse-word stream))
    (unless (member option '("optional" "required") :test #'string=)
      (error "Expected either 'optional' or 'required' at ~a" (file-position stream)))
    (read-whitespace stream)
    (setf type (parse-word stream))
    (read-whitespace stream)
    (setf name (parse-word stream))
    (read-whitespace stream)
    (read-string "=" stream)
    (read-whitespace stream)
    (setf ordinal (parse-number stream))
    (read-string ";" stream)
    `(:option ,option :type ,type :name ,name :ordinal ,ordinal)))


(defun parse-enum-field (stream)
  (let (name ordinal)
    (read-whitespace stream)
    (setf name (parse-word stream))
    (read-whitespace stream)
    (read-string "=" stream)
    (read-whitespace stream)
    (setf ordinal (parse-number stream))
    (read-string ";" stream)
    `(:type :enum-field :name ,name :ordinal ,ordinal)))


(defun parse-enum (stream)
  (read-whitespace stream)
  (read-string "enum " stream)
  (let (name fields)
    (setf name (parse-word stream))
    (read-whitespace stream)
    (read-string "{" stream)
    (read-whitespace stream)
    (parse-many #'parse-enum stream)
    (read-whitespace stream)
    (read-string "}" stream)
    `(:type :enum :name ,name :fields ,fields)))


(defun parse-definition (stream)
  (parse-either stream
    (parse-message stream)
    (parse-enum stream)))

(defun parse-message-content (stream)
  (parse-either stream
    (parse-definition stream)
    (parse-field stream)))

(defun parse-message (stream)
  (read-whitespace stream)
  (read-string "message " stream)
  (let ((name (parse-word stream))
	content)
    (read-whitespace stream)
    (read-string "{" stream)
    (read-whitespace stream)
    (setf content (parse-many #'parse-message-content stream))
    (read-whitespace stream)
    (read-string "}" stream)
    `(:type :message :name ,name  :fields ,content)))


(defun parse-import (stream)
  (let (name)
    (read-whitespace stream)
    (read-string "import " stream)
    (setf name (parse-delimited #\" stream))
    (read-string ";" stream)
    `(:type :import :name ,name)))

(defun import-name (import)
  (getf import :name))


(defun parse-file (path)
  (let (imports definitions)
    (with-open-file (stream path :direction :input)
      (setf imports (parse-many #'parse-import stream))
      (setf definitions (parse-many #'parse-definition stream))
      `(:type :file :imports ,imports :definitions ,definitions :name ,path))))


(defun file-imports (file)
  (getf file :imports))

(defun file-name (file)
  (getf file :name))


(defun parse-file-group (path)
  "Parse a whole group of files by parsing the one indicated and following imports"
  (let ((parsed (list (parse-file path))))
    (loop
       ;; Upper limit to number of files
       repeat 100
	 
       for imported = (remove-duplicates (loop for p in parsed nconc
					      (loop for import in (file-imports p)
						 collect (import-name import)))
					 :test #'string=)
	 
       for difference = (set-difference imported (mapcar #'file-name parsed)
					:test #'string=)
       while difference
       do (push (parse-file (first difference))
		parsed)

       ;; TODO: we can sort the files by imposing the relation f1 < f2 if f2 imports f1
       finally (return parsed))))


(defun load-file (path)
  ;; Idea: Dijkstra 
  ;; 1. Start with empty set of loaded files 
  ;; 2. Find a file that has all its imports in the loaded files
  ;; 3. Load that file, add it to the loaded set
  ;; 4. Go to 2
  ;; 5. When all files are loaded, we are done
  (let ((parsed (parse-file path))
	loaded)
    ))


(with-input-from-string
 (s "message SearchRequest {
  required string query = 1;
  optional int32 page_number = 2;
  message Nested {
    required string id = 1;
  }

  optional int32 result_per_page = 3;
}")
  
 (parse-message s))

(with-input-from-string
 (s "enum SearchRequest {
   A = 1;
   TTR = 2;
}")
  
 (parse-enum s))

(with-input-from-string
 (s "enum SearchRequest {
   A = 1;
   TTR = 2;
}")
  
 (read-string "enum " s)
 (read-string "SearchRequest {" s))








