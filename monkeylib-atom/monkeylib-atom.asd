;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(defsystem monkeylib-atom
  :name "monkeylib-atom"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "ATOM feed generation"
  :depends-on (:com.gigamonkeys.pathnames
               :com.gigamonkeys.utilities
               :com.gigamonkeys.markup
               :monkeylib-html
               :monkeylib-text-output
               :monkeylib-text-languages)
  :components ((:file "packages")
               (:file "atom" :depends-on ("packages"))
               (:file "feed" :depends-on ("packages" "atom"))))
