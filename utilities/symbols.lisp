(in-package :monkeylib-utilities)

(defun keywordize (s)
  "Return a keyword symbol with the name of the given string upcased."
  (intern (string-upcase s) :keyword))


(defun camel-to-kebab (s &optional (package :keyword))
  (intern (string-upcase (cl-ppcre:regex-replace-all "([a-z])([A-Z])" s "\\1-\\2")) package))
