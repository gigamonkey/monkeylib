(in-package :cl-user)

(defpackage :monkeylib-spam
  (:nicknames :spam)
  (:use :common-lisp
        :monkeylib-pathnames
        :monkeylib-utilities)
  (:export
   :make-feature-database
   :clear-database
   :intern-feature
   :train
   :untrain
   :classify
   :all-features
   :feature-spamminess

   :id
   :hams
   :spams))
