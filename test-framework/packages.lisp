;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :monkeylib-test
  (:use :common-lisp :monkeylib-macro-utilities)
  (:export :deftest
	   :check
	   :expect
	   :test
	   :with-test-cases
	   :test-package
	   :clear-package-tests
	   :remove-test-function
	   :*debug*
	   :*debug-on-fail*))

(defpackage :monkeylib-test-tests
  (:use :common-lisp)
  (:import-from :monkeylib-test :deftest :check :test :expect))