;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :monkeylib-yamp
  (:use :common-lisp
        :monkeylib-json
        :monkeylib-pathnames
        :monkeylib-utilities)
  (:export

   ;; Markup
   :>>>
   :markup
   :config
   :rewriter
   :rewriter-if
   :replacing-with
   :deleter
   :extract
   :without
   :before
   :has
   :after
   :just-text
   :*input-file*

   ;; HTML
   :generate-html
   :html-filename
   :html-config
   :generic-html

   ;; API
   :defparser
   :defterm
   :defparserfun ;; Not clear this needs to be public.

   ;; Parser functions
   :!
   :?
   :any-char
   :counted
   :eof
   :many
   :many1
   :optional
   :peek
   :text

   ;; Tracing
   :tracing
   :tracer
   :tracemsg

   ;; Special syntactic symbols
   :&state
   :_
   :->
   :=>

   ;; Pre-processors and section handlers
   :links
   :endnotes
   :images

   :sourcecode
   :format-dateline
   :tweet-by-id
   :formatted-code)

  (:import-from :alexandria :with-gensyms)
  (:import-from :monkeylib-text-output :with-text-output)
  (:import-from :monkeylib-html :element-p :html)
  (:import-from :monkeylib-text-languages :special-form-p :top-level-environment)
  (:shadow :!))
