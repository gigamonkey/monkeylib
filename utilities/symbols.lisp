(in-package :monkeylib-utilities)

(defun keywordize (s)
  "Return a keyword symbol with the name of the given string upcased."
  (intern (string-upcase s) :keyword))

(defun keywordize-plist (plist)
  "Make sure the keys of a plist are keyword symbols."
  (loop for (a b . rest) on plist by #'cddr collect (keywordize a) collect b))

(defun camel-to-kebab (s &optional (package :keyword))
  "Convert a string in CamelCase to kebab-case."
  (intern (string-upcase (cl-ppcre:regex-replace-all "([a-z])([A-Z])" s "\\1-\\2")) package))
