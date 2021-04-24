;;; Copyright (c) 2009-2012, Peter Seibel.
;;; All rights reserved. See LICENSE for details.

(defsystem monkeylib-json
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :description "Library for reading and writing JSON-formatted data."
  :components
  ((:file "packages")
   (:file "json"               :depends-on ("packages"))
   (:file "json-builder"       :depends-on ("packages")))
  :depends-on
  (:monkeylib-parser
   :monkeylib-utilities))
