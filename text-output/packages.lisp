;;; Copyright (c) 2005-2011, Peter Seibel.
;;; All rights reserved. See LICENSE for details.

(in-package :cl-user)

(defpackage :monkeylib-text-output
  (:use :cl)
  (:export
   :*pretty*
   :*text-output*
   :*text-pretty-printer*
   :codegen-text
   :embed-code
   :embed-value
   :freshline
   :get-pretty-printer
   :indent
   :newline
   :ops
   :raw-string
   :text-compiler
   :toggle-indenting
   :unindent
   :with-text-output))
