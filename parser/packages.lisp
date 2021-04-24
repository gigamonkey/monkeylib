(in-package :cl-user)

(defpackage :monkeylib-parser
  (:use :cl)
  (:shadow :type)
  (:export
   :defprod
   :defchartype
   :deflexer
   :defparser
   :last-match
   :parselet
   :value
   :kind
   :?
   :*
   :+
   :~
   :!
   :/
   :&
   :@
   :^
   :%))

(defpackage :monkeylib-new-parser
  (:use :cl)
  (:shadow :type)
  (:export
   :defprod
   :defchartype
   :deflexer
   :defparser
   :last-match
   :parselet
   :?
   :*
   :+
   :~
   :!
   :/
   :&
   :@
   :^
   :%))

(defpackage :monkeylib-math-parser
  (:use :common-lisp :monkeylib-parser)
  (:export :arithmetic :calculator))

(defpackage :monkeylib-java-lexer
  (:use :cl :monkeylib-parser))


(defpackage :monkeylib-css
  (:use :cl :monkeylib-parser))

(defpackage :monkeylib-parser.dot-parser
  (:use :cl :monkeylib-parser
	:monkeylib-utilities)
  (:shadowing-import-from :monkeylib-parser :!)
  (:shadow :string))

(defpackage :monkeylib-parser.time-period-parser
  (:use :cl :monkeylib-parser
	:monkeylib-utilities)
  (:shadowing-import-from :monkeylib-parser :!)
  (:shadow :step :time))
