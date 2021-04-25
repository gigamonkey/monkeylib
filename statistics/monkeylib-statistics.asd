;;
;; Copyright (c) 2009, Peter Seibel All rights reserved.
;;

(defsystem monkeylib-statistics
  :description "Some basic statistics-computing functions."
  :components
  ((:file "packages")
   (:file "statistics" :depends-on ("packages"))))
