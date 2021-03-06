;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(defsystem monkeylib-yamp
  :name "monkeylib-yamp"
  :description "Yet Another Markup Parser."
  :components
  ((:file "packages")
   (:file "parser"      :depends-on ("packages"))
   (:file "combinators" :depends-on ("packages" "parser"))
   (:file "iso-8601"    :depends-on ("packages" "parser" "combinators"))
   (:file "markup"      :depends-on ("packages" "parser" "combinators"))
   (:file "markup-lite" :depends-on ("packages" "parser" "combinators"))
   (:file "math"        :depends-on ("packages" "parser"))
   (:file "math-tests"  :depends-on ("packages" "math"))
   (:file "trees"       :depends-on ("packages"))
   (:file "html"        :depends-on ("packages" "trees"))
   (:file "tests"       :depends-on ("packages" "markup")))
  :depends-on
  (:cl-ppcre
   :alexandria
   :monkeylib-json
   :monkeylib-macro-utilities
   :monkeylib-pathnames
   :monkeylib-utilities
   :monkeylib-html))
