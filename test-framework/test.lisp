;;
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;

(in-package :monkeylib-test)

(defvar *test-name* nil)
(defvar *report-passes* nil)

(defparameter *debug* t
  "If true, then the Lisp debugger is invoked when an unexpected
condition is signalled during testing. See also:
*debug-on-fail*.")

(defparameter *debug-on-fail* t
  "If true, then the Lisp debugger is invoked when a test fails.
See also: *debug*.")

(defun listify (thing)
  (if (listp thing) thing (list thing)))

(defmacro with-test-results ((&key print summary) &body body)
  "Collect test results signaled in `body' (except from forms in
the dynamic extent of an IGNORE-RESULTS."
  (with-gensyms (pass fail abort)
    (once-only (print summary)
      `(let ((,pass 0)
	     (,fail 0)
	     (,abort 0)
	     (,print (listify ,print)))
	 (handler-bind ((test-result
			 #'(lambda (c) 
			     (ecase (result c)
			       (:pass (incf ,pass))
			       (:fail (incf ,fail))
			       (:abort (incf ,abort)))
			     (when (or (eql (car ,print) t) (member (result c) ,print))
			       (print-result c)))))
	     (handler-case
		 (handler-bind ((error #'report-unexpected-error)) ,@body)
	       (error ())))
	 (let ((ok (zerop (+ ,fail ,abort))))
	   (when ,summary (print-result-summary ok ,pass ,fail ,abort))
	   (values ok ,pass ,fail ,abort))))))

(defgeneric report-unexpected-error (error))

(defmethod report-unexpected-error :around (error)
  (if *debug* 
      (restart-case
	  (invoke-debugger error)
	(proceed () (call-next-method)))
      (call-next-method)))

(defun proceed (&optional condition)
  (break "here")
  (let ((restart (find-restart 'proceed condition)))
    (break "got restart ~a" restart)
    (invoke-restart restart)))

(defmethod report-unexpected-error ((error cell-error))
  (report-abort (format nil "~a ~a" (class-name (class-of error)) (cell-error-name error))))

(defmethod report-unexpected-error ((error error))
  (report-abort (format nil "~a" error)))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other
test functions or use `check' to run individual test cases."
  `(setf (get ',name 'test-function)
	 (lambda ,parameters
	   (with-test-results ()
	     (let ((*test-name* (append *test-name* (list ',name))))
	       ,@body)))
	 (get ',name 'test-function-parameters)
	 ',parameters))

(defmacro check (&body forms)
  "Run each expression in `forms' as a test case."
  `(progn
     ,@(loop for f in forms collect `(multiple-value-bind (result sub-forms) (eval-with-values ,f)
				       (report-result result ',f sub-forms)))))

(defmacro expect (condition-type form)
  "Pass iff a condition of the given type is signaled  while executing form."
  `(progn
     (handler-case
	 (multiple-value-bind (result sub-forms) (eval-with-values ,form)
	   (declare (ignore result))
	   (report-result nil ',form sub-forms))
       (,condition-type () (report-result t ',form nil)))))

(defmacro test (name &rest arguments)
  "Run a named test."
  `(with-test-results (:print '(:fail :abort) :summary t)
     (run-test ',name ,@arguments)))

(defmacro with-test-cases ((&rest parameters) test-form &rest test-cases)
  "Check a single test form with multple test cases."
  (with-gensyms (test p)
    `(flet ((,test (,p) (destructuring-bind (,@parameters) ,p ,test-form)))
       ,@(loop for test-case in test-cases collect 
	      `(let ((*test-name* (append *test-name* ',test-case)))
		 (,test (list ,@test-case)))))))

(defmacro ignore-results (&body body)
  "Ignore results signaled in `body'. This is, at the very least,
handy when writing tests for the test framework itself."
  `(let ((*debug-on-fail* nil))
     (handler-bind ((test-result #'ignore-result)) ,@body)))

(define-condition test-result ()
  ((result :initarg :result :reader result)
   (name :initarg :name :reader name)
   (form :initarg :form :reader form)
   (sub-forms :initarg :sub-forms :reader sub-forms)))

(define-condition test-pass (test-result) ())
(define-condition test-fail (test-result) ())
(define-condition test-abort (test-result) ())

(defgeneric run-test (thing &rest arguments))

(defmethod run-test ((name symbol) &rest arguments)
  (let ((test-function (test-function name)))
    (if test-function
	(apply test-function arguments)
	(error "No test named ~a" name))))

(defun test-package (&key (print '(:abort :fail)) (summary t) (package *package*))
  (with-test-results (:print print :summary summary)
    (do-symbols (sym package)
      (when (and (test-function sym) (not (get sym 'test-function-parameters)))
	(run-test sym)))))

(defun clear-package-tests (&key (package *package*))
  (do-symbols (sym package)
    (when (test-function sym)
      (remove-test-function sym))))

(defun test-function (symbol) (get symbol 'test-function))

(defun remove-test-function (symbol) (remprop symbol 'test-function))

(defun report-result (result form sub-forms)
  "Report the results of a single test case. Called by `check'."
  (let ((condition 
	 (make-condition (if result 'test-pass 'test-fail) 
			 :result (if result :pass :fail)
			 :name *test-name* 
			 :form form
			 :sub-forms sub-forms)))  
    (when (and *debug-on-fail* (not result))
      (restart-case (invoke-debugger condition)
	(continue ())))
    (restart-case 
	(signal condition)
      (ignore-result () nil))))

(defun report-abort (message)
  (restart-case 
      (signal 'test-abort 
	      :result :abort
	      :name *test-name*
	      :form message)
    (ignore-result () nil)))

(defun ignore-result (&optional condition)
  (let ((restart (find-restart 'ignore-result condition)))
    (when restart (invoke-restart restart))))

(defgeneric print-result (result))

(defmethod print-result ((result test-result))
  (format t "~a ... ~a: ~a~%" (result result) (name result) (form result)))

(defmethod print-result ((result test-fail))
  (if *debug-on-fail*
      (call-next-method)
      (format t "~a ... ~a: ~a~%~@[~:{~2t~a~20t=> ~a~%~}~]" (result result) (name result) (form result) (sub-forms result))))

(defmethod print-object ((result test-fail) stream)
  (format stream "Failure ~a: ~a~%~@[~:{~2t~a~20t=> ~a~%~}~]"  (name result) (form result) (sub-forms result)))



(defun print-result-summary (ok passes failures aborts)
  (format t "~:[NOT o~;O~]kay: ~d passes; ~d failures; ~d aborts.~%" ok passes failures aborts))

(defmacro eval-with-values (form)
  "Transform `form' into an equivalent form to be evaluated and
an alist of sub-forms and their values."
  (let ((bindings-vector (make-array 10 :adjustable t :fill-pointer 0)))
    (let ((var (process form bindings-vector)))
      (let ((bindings (coerce bindings-vector 'list)))
	`(let* ,(mapcar #'butlast bindings)
	 (values ,var (list ,@(loop for (var nil key) in bindings collect `(list ',key ,var)))))))))

     
(defun process (sexp vars)
  (typecase sexp
    ((and symbol (not keyword))
     (let ((var (gensym)))
       (vector-push-extend (list var sexp sexp) vars)
       var))
    (cons
       (destructuring-bind (op &rest args) sexp
	 (if (function-name-p op)
	     (let ((var (gensym)))
	       (vector-push-extend (list var `(,op ,@(mapcar #'(lambda (f) (process f vars)) args)) sexp) vars)
	       var)
	     (process-for-op op sexp vars))))
    (t sexp)))

(defun function-name-p (name)
  (and (symbolp name)
       (fboundp name)
       (not (macro-function name))
       (not (special-operator-p name))))

(defgeneric process-for-op (op sexp vars)
  (:documentation "Define special processing for macros and
  special operators which would otherwise be opaque. It is
  important to preserve the proper evaluation of the subforms."))

(defmethod process-for-op ((op t) sexp vars)
  (let ((var (gensym)))
    (vector-push-extend (list var sexp sexp) vars)
    var))

(defmethod process-for-op ((op (eql 'if)) sexp vars)
  (let ((var (gensym)))
    (destructuring-bind (test then &optional else) (rest sexp)
      (let ((test-var (process test vars)))
	(vector-push-extend (list var `(if ,test-var ,then ,else) sexp) vars))
      var)))
