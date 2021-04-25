;;
;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;

(defsystem monkeylib-parser
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :description "Parser generator, loosely based on Henry Baker's META paper."
  :components
  ((:file "packages")
   (:file "parser" :depends-on ("packages")))
  :depends-on (:monkeylib-macro-utilities
               :monkeylib-utilities))
