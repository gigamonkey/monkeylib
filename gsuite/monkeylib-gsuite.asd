;;; Copyright (c) 2021, Peter Seibel.
;;; All rights reserved. See LICENSE for details.

(defsystem monkeylib-gsuite
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :description "Library for interacting with Google docs, sheets, etc. APIs"
  :components
  ((:file "packages")
   (:file "macros"  :depends-on ("packages"))
   (:file "schemas" :depends-on ("packages" "macros"))
   (:file "docs"    :depends-on ("packages" "schemas")))
  :depends-on
  (:alexandria
   :drakma
   :monkeylib-utilities
   :monkeylib-json))
