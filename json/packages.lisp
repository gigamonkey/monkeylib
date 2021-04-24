(in-package :cl-user)

(defpackage :monkeylib-json.parser
  (:use :cl :monkeylib-parser)
  (:shadow :string :exp)
  (:export :parse-json :*object-type*))

(defpackage :monkeylib-json
  (:use :cl
        :monkeylib-utilities
        :monkeylib-json.parser)
  (:export
   :write-json
   :json
   :parse-json
   :*object-type*
   :to-json
   :json-stringify))
