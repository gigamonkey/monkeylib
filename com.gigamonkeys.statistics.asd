;;
;; Copyright (c) 2009, Gigamonkeys Consulting All rights reserved.
;;

(defsystem com.gigamonkeys.statistics
  :components
  ((:file "packages")
   (:file "statistics" :depends-on ("packages"))))
