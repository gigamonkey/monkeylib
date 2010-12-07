;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.markup.html
  (:use :common-lisp
        :com.gigamonkeys.markup
        :com.gigamonkeys.utilities
        :com.gigamonkeys.foo)
  (:import-from :alexandria :compose)
  (:export
   :render
   :render-to-stream
   :render-sexps-to-stream
   :footnotes
   :htmlize-links
   :make-retagger
   :footnotes
   :just-text))

(defpackage :com.gigamonkeys.markup.html.handy-tags
  (:use :common-lisp :com.gigamonkeys.markup.html)
  (:export
   :add-amazon-image-bugs
   :mailto-link
   :url-link))
