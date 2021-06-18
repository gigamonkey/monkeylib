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
   :markup
   :config
   :rewriter
   :extract
   :before
   :has
   :after
   :just-text
   :*input-file*

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
  (:shadow :!))
