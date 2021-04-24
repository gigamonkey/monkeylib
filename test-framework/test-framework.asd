;;
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;

(defsystem monkeylib-test-framework
  :name "test-framework"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Simple unit test framework for Common Lisp"
  :long-description ""
  :components
  ((:file "packages")
   (:file "test" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages" "test"))
   (:file "test-tests" :depends-on ("packages" "test")))
  :depends-on (:monkeylib-macro-utilities))
