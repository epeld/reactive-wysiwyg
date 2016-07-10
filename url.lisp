
(in-package :peldan.url)

(unless (boundp 'session-url)
  (defconstant session-url "/session/"
    "The base prefix for accessing sessions through HTTP request"))


(defun uuid-from-script-name (request)
  "Extract the requested uuid from request script-name"
  (subseq (script-name request) (length session-url)))


(defun session-url (uuid)
  "This function determines the url for which the session with uuid should be available"
  
  (concatenate 'string session-url uuid))


(peldan.ps:defps session-url (uuid)
  (+ (ps:lisp session-url) uuid))
