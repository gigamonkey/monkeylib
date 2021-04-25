;;; Copyright (c) 2005-2011, Peter Seibel.
;;; All rights reserved. See LICENSE for details.

(in-package :cl-user)

(defpackage :monkeylib-text-languages
  (:use :cl
        :monkeylib-text-output
        :monkeylib-macro-utilities)
  (:export
   :case-preserving-readtable
   :comment
   :compile-special-op-body
   :define-language
   :define-language-macro
   :define-macro
   :define-special-operator
   :embeddable-value-form
   :emit
   :emit-for-language
   :environment
   :expand-macro-form
   :fully-expand-macro-form
   :generate
   :identifier
   :input-package
   :input-readtable
   :language
   :macro-form-p
   :macro-symbols
   :output-file-type
   :parse-&environment
   :process
   :process-sexp
   :process-special-form
   :self-evaluating-p
   :sexp->ops
   :sexp-form-p
   :special-form-p
   :special-operator-symbols
   :syntax-error
   :top-level-environment))
