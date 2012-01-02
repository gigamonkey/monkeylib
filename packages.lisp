;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :cl-user)

(defpackage :monkeylib-atom
  (:use :common-lisp
	:com.gigamonkeys.macro-utilities
	:com.gigamonkeys.utilities
        :com.gigamonkeys.pathnames
        :com.gigamonkeys.markup
        :monkeylib-html
	:monkeylib-text-output
	:monkeylib-text-languages)
  (:shadow :atom)
  (:export
   :atom))



