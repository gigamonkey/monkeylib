;;
;; Copyright (c) 2009, Gigamonkeys Consulting All rights reserved.
;;

(defsystem monkeylib-statistics
  :description "Some basic statistics-computing functions."
  :components
  ((:file "packages")
   (:file "statistics" :depends-on ("packages"))))
