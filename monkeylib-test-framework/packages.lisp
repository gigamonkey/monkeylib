;;
;; Copyright (c) 2005, Gigamonkeys Consulting All rights reserved.
;;

(in-package :cl-user)

(defpackage :com.gigamonkeys.test
  (:use :common-lisp :com.gigamonkeys.macro-utilities)
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

(defpackage :com.gigamonkeys.test-tests
  (:use :common-lisp)
  (:import-from :com.gigamonkeys.test :deftest :check :test :expect))