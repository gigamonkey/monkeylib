;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :monkeylib-prose-diff
  (:use :common-lisp
        :cl-ppcre
        :monkeylib-pathnames
        :monkeylib-utilities
        :monkeylib-markup
        :monkeylib-markup.html)
  (:import-from :alexandria :compose)
  (:export
   :show-cuts))
