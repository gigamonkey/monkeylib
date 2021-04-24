;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem monkeylib-prose-diff
  :name "monkeylib-prose-diff"
  :components
  ((:file "packages")
   (:file "lcs" :depends-on ("packages"))
   (:file "tokenize" :depends-on ("packages"))
   (:file "text" :depends-on ("packages"))
   (:file "utilities" :depends-on ("packages"))
   (:file "diff" :depends-on ("packages"))
   (:file "html" :depends-on ("packages")))
  :depends-on (:cl-ppcre
               :monkeylib-macro-utilities
               :monkeylib-markup
               :monkeylib-markup-html
               :monkeylib-pathnames
               :monkeylib-utilities))
