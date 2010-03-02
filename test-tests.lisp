(in-package :com.gigamonkeys.test-tests)

(com.gigamonkeys.test:clear-package-tests)

(define-condition foo-condition () ())
(define-condition foo-error (error) ())

(deftest always-pass () (check t))
(deftest always-fail () (check nil))
(deftest should-pass-n (n) (loop repeat n do (check t)))
(deftest should-fail-n (n) (loop repeat n do (check nil)))
(deftest should-pass-n-fail-m (n m)
  (test should-pass-n n)
  (test should-fail-n m))

(deftest expect/pass () (expect foo-condition (signal 'foo-condition)))
(deftest expect/fail () (expect foo-condition (signal 'condition)))
(deftest expect/abort () (expect foo-condition (error 'foo-error)))