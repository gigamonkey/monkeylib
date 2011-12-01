;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem monkeylib-markup-xml
  :name "monkeylib-markup-xml"
  :description "Library for generating XML from Markup-formatted text."
  :components
  ((:file "packages")
   (:file "xml" :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.macro-utilities
   :com.gigamonkeys.utilities
   :com.gigamonkeys.foo))
