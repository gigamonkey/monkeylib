;;; Copyright (c) 2011, Peter Seibel.;
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :monkeylib-atom
  (:use :common-lisp
        :com.gigamonkeys.utilities
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.markup
        :monkeylib-html
        :monkeylib-text-output
        :monkeylib-text-languages)
  (:shadow :atom)
  (:export :feed
           :parse-feed
           :permalink

           ;; Feed slots
           :title
           :subtitle
           :updated
           :tag
           :feed-url
           :index-url
           :rights
           :default-author-name
           :default-author-email
           :default-author-uri
           :canonical-host
           :full-prefix
           :entries

           ;; Entry slots
           :file
           :title
           :body
           :published
           :updated
           :categories))
