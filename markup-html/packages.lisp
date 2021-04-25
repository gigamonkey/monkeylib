;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :monkeylib-markup-html
  (:use :common-lisp
        :monkeylib-markup
        :monkeylib-utilities)
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

(defpackage :monkeylib-markup-html-handy-tags
  (:use :common-lisp :monkeylib-markup-html)
  (:export
   :add-amazon-image-bugs
   :mailto-link
   :url-link))
