;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem monkeylib-markup-html
  :name "monkeylib-markup-html"
  :components
  ((:file "packages")
   (:file "html" :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.macro-utilities
   :com.gigamonkeys.utilities
   :com.gigamonkeys.foo))
