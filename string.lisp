
(in-package :peldan.string)


(defun split-by (string &optional (char #\Space))
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (setq string (string-trim "/" string))
    (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))


(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 


(defun read-file-to-string (filename)
  (with-output-to-string (s)
    (with-open-file (in filename)
      (loop for line = (read-line in nil 'foo)
	 until (eq line 'foo)
	 do (write-line line s)))))


(defun read-file-to-stream (filename stream)
  (with-open-file (in filename)
      (loop for line = (read-line in nil 'foo)
	 until (eq line 'foo)
	 do (write-line line stream))))


(defun starts-with-p (prefix s)
  "Return true if s starts with prefix"
  (when (< (length prefix) (length s))
    (string-equal prefix s
		  :end2 (length prefix))))


(defun title-ify (string)
  ; Currently only upper cases first letter..
  (the string string)
  (when (string= "" string)
    (error "Empty string"))
  (string-upcase (peldan.string:replace-all (string-downcase string)
					    "-" " ")
		 :start 0
		 :end 1))


(defun generate-uuid ()
  "Generates a uuid of the form 6ABE-CCF6-9A26-1DBC"
  (with-output-to-string (s)
    (let ((parts 4))
      (loop for i from 1 upto parts do
	   (loop for j from 1 upto 4 collect
		(write (random 16)
		       :base 16
		       :stream s))
	 when (/= i parts)
	 do (write-string "-" s)))))
