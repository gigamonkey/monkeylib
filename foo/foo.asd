;;
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;

(defsystem monkeylib-foo
  :name "monkeylib-foo"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "FOO Outputs Output"
  :components
  ((:file "packages")
   (:file "text-output"      :depends-on ("packages"))
   (:file "language"         :depends-on ("packages" "text-output"))
   (:file "file-compiler"    :depends-on ("packages" "language" "text-output"))
   (:file "string-escaping"  :depends-on ("packages"))
   (:file "html"             :depends-on ("packages" "language" "text-output" "string-escaping" "xml"))
   (:file "html-legacy"      :depends-on ("packages" "language" "text-output" "html"))
   (:file "html-tests"       :depends-on ("packages" "html"))
   (:file "xml"              :depends-on ("packages" "language" "text-output"))
   (:file "css"              :depends-on ("packages" "language" "html"))
   (:file "html-macros"      :depends-on ("packages" "html" "css"))
   (:file "xml-macros"       :depends-on ("packages" "xml"))
   (:file "javascript"       :depends-on ("packages" "language"))
   (:file "lispscript"       :depends-on ("packages" "language" "javascript" "html"))
   (:file "lispscript-tests" :depends-on ("packages" "lispscript" "html" "html-macros" "html-legacy")))
  :depends-on (:monkeylib-test-framework
               :monkeylib-pathnames
               :monkeylib-utilities))
