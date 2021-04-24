;;
;; Copyright (c) 2005-2011, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.pathnames
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :description "Library to smooth over some implementation differences in treatment of pathnames."
  :components
  ((:file "packages")
   (:file "pathnames" :depends-on ("packages"))))
