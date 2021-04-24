;;
;; Copyright (c) 2005-2010, Peter Seibel. All rights reserved.
;;

(in-package :cl-user)

(defpackage :monkeylib-macro-utilities
  (:use :common-lisp)
  (:export 
   :gensyms
   :mapticks
   :with-gensyms
   :once-only
   :spliceable))
           
