;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem monkeylib-markup-html
  :name "monkeylib-markup-html"
  :components
  ((:file "packages")
   (:file "html" :depends-on ("packages"))
   (:file "footnotes" :depends-on ("packages"))
   (:file "handy-tags" :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.macro-utilities
   :alexandria
   :com.gigamonkeys.markup
   :com.gigamonkeys.utilities
   :com.gigamonkeys.foo))
