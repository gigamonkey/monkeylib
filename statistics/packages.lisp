(in-package :cl-user)

(defpackage :monkeylib-statistics
  (:use :cl)
  (:export :mean
	   :median
	   :mode
	   :variance
           :standard-deviation
	   :numbers-stats
           :normal-random-number))

