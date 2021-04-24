;;
;; Copyright (c) 2009, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.statistics
  :description "Some basic statistics-computing functions."
  :components
  ((:file "packages")
   (:file "statistics" :depends-on ("packages"))))
