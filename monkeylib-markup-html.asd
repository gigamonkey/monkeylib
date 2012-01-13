;;; Copyright (c) 2010-2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(defsystem monkeylib-markup-html
  :name "monkeylib-markup-html"
  :description "Library for generating HTML from Markup formatted text."
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
   :monkeylib-html))
