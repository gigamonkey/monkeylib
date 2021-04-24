(in-package :com.gigamonkeys.test)

(clear-package-tests)

(deftest pass ()
  (multiple-value-bind (ok passes failures)
      ;; ignore-results keeps results of (test pass) being recorded as
      ;; part of this test function. But TEST still returns the
      ;; results accumulated by running the test function.
      (ignore-results (test com.gigamonkeys.test-tests::always-pass))
    (check ok (= passes 1) (= failures 0))))

(deftest fail ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::always-fail))
    (check (not ok) (= passes 0) (= failures 1))))

(deftest multiple-passes ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::should-pass-n 10))
    (check ok (= passes 10) (= failures 0))))

(deftest multiple-failures ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::should-fail-n 10))
    (check (not ok) (= passes 0) (= failures 10))))

(deftest mixed-results ()
  (multiple-value-bind (ok passes failures)
      (ignore-results
	(test com.gigamonkeys.test-tests::should-pass-n-fail-m 10 13))
    (check (not ok) (= passes 10) (= failures 13))))

(deftest test-test-package () 
  (multiple-value-bind (ok passes failures aborts)
      (let ((*debug* nil))
	(ignore-results (test-package :print t :summary nil :package :com.gigamonkeys.test-tests)))
    (check (not ok) (= passes 2) (= failures 2) (= aborts 1))))

(deftest test-expect ()
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::expect/pass))
    (check ok (= passes 1) (= failures 0)))
  (multiple-value-bind (ok passes failures)
      (ignore-results (test com.gigamonkeys.test-tests::expect/fail))
    (check (not ok) (= passes 0) (= failures 1)))
  (multiple-value-bind (ok passes failures aborts)
      (let ((*debug* nil))
	(ignore-results (test com.gigamonkeys.test-tests::expect/abort)))
    (check (not ok) (= passes 0) (= failures 0) (= aborts 1))))
