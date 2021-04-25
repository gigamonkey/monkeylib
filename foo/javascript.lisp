;;
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;

(in-package :monkeylib-foo-javascript)

(defclass javascript (language)
  ()
  (:default-initargs
   :special-operator-symbol 'javascript-special-operator
    :macro-symbol 'javascript-macro
    :input-readtable (case-preserving-readtable)
    :input-package (find-package :monkeylib-foo-javascript)
    :output-file-type "js"))

(defun new-env (key value env)
  (acons key value env))

(defun statement-or-expression (env)
  (cdr (assoc 'statement-or-expression env)))

(defun parenthesized-p (env)
  (cdr (assoc 'parenthesized env)))

(defun parenthesized (env)
  (new-env 'parenthesized t env))

(defun not-parenthesized (env)
  (new-env 'parenthesized nil env))

(defvar *javascript-gensym-counter* 0)

(defun javascript-gensym (&optional (prefix "g$"))
  (make-symbol (format nil "~a~d" prefix (incf *javascript-gensym-counter*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File compiler implementation

(defmethod comment ((language javascript) text)
  (format nil "// ~a" text))

(defmethod top-level-environment ((language javascript))
  '((statement-or-expression . :statement)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language implementation

(defmethod special-operator-symbol ((language javascript)) 'javascript-special-operator)

(defmethod macro-symbol ((language javascript)) 'javascript-macro)

(defmethod process-sexp ((language javascript) processor form environment)
  (if (consp form)
      (destructuring-bind (name &rest arguments) form
        (if (method-name name)
            (process-method-call language processor environment (method-name name) (first arguments) (rest arguments))
            (process-function-call language processor environment name arguments)))
      (process-javascript-scalar processor form))
  (maybe-semicolon processor environment))

(defun method-name (name)
  (cond
    ((and (consp name) (eql (first name) 'method))
     (second name))
    ((and (symbolp name) (char= (char (string name) 0) #\.))
     (intern (subseq (string name) 1) (symbol-package name)))
    (t nil)))

(defun process-method-call (language processor environment method-name object arguments)
  (process language processor object (new-env 'statement-or-expression :expression environment))
  (raw-string processor ".")
  (process language processor method-name (new-env 'statement-or-expression :expression environment))
  (raw-string processor "(")
  (loop for (arg . rest) on arguments do
       (process language processor arg (parenthesized (new-env 'statement-or-expression :expression environment)))
     when rest do (raw-string processor ", "))
  (raw-string processor ")"))

(defun process-function-call (language processor environment name arguments)
  (let ((function-expression-statement-p
         (and (consp name) (eql (car name) 'function) (eql (statement-or-expression environment) :statement))))
    (when function-expression-statement-p (raw-string processor "("))
    (process language processor name (new-env 'statement-or-expression :expression environment))
    (when function-expression-statement-p (raw-string processor ")"))
    (raw-string processor "(")
    (loop for (arg . rest) on arguments do
         (process language processor arg (parenthesized (new-env 'statement-or-expression :expression environment)))
       when rest do (raw-string processor ", "))
    (raw-string processor ")")))


(defun process-javascript-scalar (processor value)
  ;; This is where better smarts about translating Lisp values to
  ;; Javascript syntax goes. (E.g. (foo x 123.4d0) doesn't work
  ;; because this function will generate 123.4d0 in the Javascript
  ;; output.
  (etypecase value
    (string (raw-string processor (format nil "~s" value)))
    (symbol (raw-string processor (format nil "~a" (dash-to-intercap value))))
    (number (raw-string processor (javascript-numeric-literal value)))
    (character (raw-string processor (javascript-character-text value)))))

(defun javascript-character-text (char)
  (case char
    (#\Tab "'\\t'")
    (#\Newline "'\\n'")
    (#\Return "'\\r'")
    (#\Backspace "'\\b'")
    #-openmcl(#\vt "'\\v'")
    (#\Page "'\\f'")
    (#\Null "'\\0'")
    (t (format nil "'~a'" char))))

(defun dash-to-intercap (symbol)
  (with-output-to-string (s)
    (loop with up = nil
       for char across (symbol-name symbol)
       when (char= char #\-) do (setf up t)
       else do
         (write-char (if up (char-upcase char) char) s)
         (setf up nil))))

(defun javascript-numeric-literal (number)
  (cond
    ((= (round number) number) (format nil "~d" (round number)))
    (t (let ((*read-default-float-format* 'double-float)) (format nil "~f" (float number 0d0))))))

(defmethod process-special-form :after ((language javascript) processor form environment)
  (when (eql (special-op-type (car form)) :expression)
    ;; The special form is naturally an expression but if it is being
    ;; proceessed as a statement then we need to tack on a
    ;; semicolon. If it's already a statement then it will have taken
    ;; care of emitting any necessary semicolon.
    (maybe-semicolon processor environment)))

(defun maybe-semicolon (processor environment)
  (ecase (statement-or-expression environment)
    (:statement (raw-string processor ";"))
    (:expression)))

(defmacro define-javascript-macro (name (&rest parameters) &body body)
  `(define-macro ,name javascript-macro (,@parameters) ,@body))

(defmacro define-javascript-special-operator (name statement-or-expression (language processor &rest parameters) &body body)
  "Special ops that are always statements are responsible for
outputting their own semicolon if necessary. This allows
statements such as blocks to *not* emit a semicolon."
  (multiple-value-bind (parameters env) (parse-&environment parameters)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (remprop ',name 'javascript-special-operator)
       (remprop ',name 'javascript-special-operator-type)
       (define-special-operator ,name javascript-special-operator (,language ,processor ,@parameters &environment ,env)
         (macrolet ((out (&rest stuff)
                      `(progn ,@(compile-special-op-body ',processor stuff)))
                    (emit (thing)
                      `(raw-string ,',processor ,thing)))
           (flet ((statement (thing &optional (environment ,env))
                    (process ,language ,processor thing (new-env 'statement-or-expression :statement environment)))
                  (expression (thing &optional (environment ,env))
                    (process ,language ,processor thing (new-env 'statement-or-expression :expression environment)))
                  (name (thing)
                    (raw-string ,processor (dash-to-intercap thing))))
             (out ,@body))))
       (setf (get ',name 'javascript-special-operator-type) ,statement-or-expression))))

(defun special-op-type (name)
  (get name 'javascript-special-operator-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operators -- special operators that produce expressions.

(macrolet ((define-unary-ops (&rest ops)
             `(progn
                ,@(loop for op in ops collect
                       `(define-javascript-special-operator ,op :expression (language processor expr)
                          ,(format nil "~(~a~)(" op) (expression expr) ")")))))
  (define-unary-ops delete void typeof ~ !))

(macrolet ((define-binary-ops (&rest ops)
             `(progn
                ,@(loop for op in ops collect
                       `(define-javascript-special-operator ,op :expression (language processor &rest expressions &environment env)
                          (unless (parenthesized-p env) (out "("))
                          (loop for (e . rest) on expressions
                             do (expression e (not-parenthesized env))
                             when rest do (out ,(format nil " ~(~a~) " op)))
                          (unless (parenthesized-p env) (out ")")))))))
  ;; In theory, we could keep track of precedence levels and avoid
  ;; over parenthesizing in the generated code. Though it's not clear
  ;; that's actually worth it or even a good idea.
  (define-binary-ops * / %)
  (define-binary-ops + -)
  ;;(define-binary-ops << >> >>>)
  ;;(define-binary-ops < > <= >= instanceof in)
  (define-binary-ops instanceof in)
  ;;(define-binary-ops == != === !===)
  (define-binary-ops &)
  (define-binary-ops ^)
  (define-binary-ops \|) ;; hmmm. This may not be the best name. Unless we put the reader into a special mode.
  (define-binary-ops &&)
  (define-binary-ops \|\|))

(macrolet ((define-true-binary-ops (&rest ops)
             `(progn
                ,@(loop for op in ops collect
                       `(define-javascript-special-operator ,op :expression (language processor e1 e2 &environment env)
                          (unless (parenthesized-p env) (out "("))
                          (expression e1 (not-parenthesized env))
                          (out ,(format nil " ~(~a~) " op))
                          (expression e2 (not-parenthesized env))
                          (unless (parenthesized-p env) (out ")")))))))
  (define-true-binary-ops << >> >>>)
  (define-true-binary-ops < > <= >= instanceof in)
  (define-true-binary-ops == != === !===))

(macrolet ((define-assignment-ops (&rest ops)
             `(progn
                ,@(loop for op in ops collect
                       `(define-javascript-special-operator ,op :expression (language processor lvalue rvalue &environment env)
                          (process language processor lvalue (new-env 'statement-or-expression :expression env))
                          (raw-string processor ,(format nil " ~a " (symbol-name op)))
                          (process language processor  rvalue (new-env 'statement-or-expression :expression env)))))))
  (define-assignment-ops = *= /= %= += -= <<= >>= >>>= &= ^= \|=))

(define-javascript-special-operator comment :statement (language processor &rest lines)
  :freshline
  (dolist (line lines)
    (out "// " (emit line) :newline)))


(define-javascript-special-operator array :expression (language processor &rest elements)
  "["
  (loop for (e . rest) on elements
     do (expression e)
     when rest do (out ", "))
  "]")

(define-javascript-special-operator object :expression (language processor &rest elements)
  "{ "
  (loop for (key value . rest) on elements by #'cddr
     do (out "\"" (name key) "\" : " (expression value))
     when rest do (out ", "))
  " }")

(define-javascript-special-operator @ :expression (language processor expr &rest slots)
  (expression expr)
  (loop for slot in slots do
       (if (symbolp slot)
         (out "." (name slot))
         (out "[" (expression slot) "]"))))

(define-javascript-special-operator ref :expression (language processor expr &rest slots)
  (expression expr)
  (loop for slot in slots do
       (out "[" (expression slot) "]")))

(define-javascript-special-operator new :expression (language processor expr &rest args)
  "new " (expression expr)
  (out "("
       (loop for (e . rest) on args
          do (expression e)
          when rest do (out ", "))
       ")"))

(define-javascript-special-operator ++ :expression (language processor lvalue &optional post)
  (if (eql post :post)
      (out (expression lvalue) "++")
      (out "++" (expression lvalue))))

(define-javascript-special-operator -- :expression (language processor lvalue &optional post)
  (if (eql post :post)
      (out (expression lvalue) "--")
      (out "--" (expression lvalue))))





(define-javascript-special-operator ? :expression (language processor condition then &optional (else 'null) &environment env)
  (process language processor condition (new-env 'statement-or-expression :expression env))
  (raw-string processor " ? ")
  (process language processor then (new-env 'statement-or-expression :expression env))
  (raw-string processor " : ")
  (process language processor else (new-env 'statement-or-expression :expression env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statements -- special operators that produce statements

(define-javascript-special-operator progn :expression (language processor &rest body)
  "("
  (loop for (e . rest) on body do (out (expression e)) (when rest (out ", ")))
  ")")

(define-javascript-special-operator prog :statement (language processor &rest body)
  (loop for s in body do (out (statement s) :freshline)))

;; Block -- we handle this a bit specially to avoid creating redundant
;; blocks in the generated code. In the case where a block contains
;; only another block (or a macro that expands into a block) we strip
;; the inner block.
(define-javascript-special-operator block :statement (language processor &rest body &environment env)
  (when (and body (not (cdr body)))
    (loop while (macro-form-p language (car body)) do
         (setf (car body) (expand-macro-form language (car body) env)))
    (loop while (and body (not (cdr body)) (consp (car body)) (eql (caar body) 'block)) do
         (setf body (rest (car body)))))
  (out
   "{" :newline :indent
   (loop for stmt in body do (out (statement stmt) :freshline))
   :unindent
   "}"))

;; Var -- can only define one variable at a time.
(define-javascript-special-operator var :statement (language processor variable &optional value)
  :freshline
  "var " (name variable)
  (when value (out " = " (expression value))) ";")

;; If
(define-javascript-special-operator if :statement (language processor condition then &optional else &environment env)
  "if (" (expression condition (parenthesized env)) ") " (statement then)
  (when else
    (let ((expanded (fully-expand-macro-form language else env)))
      (if (and (eql (car expanded) 'block)
               (eql (caadr expanded) 'if)
               (not (cddr expanded)))
          (out " else " (statement (cadr expanded)))
          (out " else " (statement else))))))

;; Do-While
(define-javascript-special-operator do-while :statement (language processor body condition &environment env)
  "do " (statement body) " while (" (expression condition (parenthesized env)) ");")

;; While
(define-javascript-special-operator while :statement (language processor condition body &environment env)
  "while (" (expression condition (parenthesized env)) ") " (statement body))

;; For
(define-javascript-special-operator for :statement (language processor var-test-step statement)
  (destructuring-bind (var test &optional step) var-test-step
    (let* ((var-p (and (consp var) (eql (first var) 'var)))
           (initialised (and (consp var) (if var-p (third var) (second var)))))
      (if var-p (setf var (second var)))
      (if (eql test 'in)
          (out "for (" (if var-p (out "var ")) (expression var) " in " (expression step) ") " (statement statement))
          (out "for (" (if var-p (out "var "))
               (if initialised (statement `(= ,var ,initialised)) (statement var))
               " " (statement test) " " (expression step) ") " (statement statement))))))

;; Continue
(define-javascript-special-operator continue :statement (language processor &optional id)
  "continue" (when id (out " " (name id))) ";")

;; Break
(define-javascript-special-operator break :statement (language processor &optional id)
  "break" (when id (out " " (name id))) ";")

;; Return
#+(or)(define-javascript-special-operator return :statement (language processor &optional expr)
  "return" (when expr (out " " (expression expr))) ";")

(define-javascript-special-operator return :statement (language processor &optional expr &environment env)
  (loop while (macro-form-p language  expr) do
       (setf expr (expand-macro-form language expr env)))
  (cond
    ((and (consp expr) (consp (car expr)))
     (loop while (macro-form-p language (car expr)) do
          (setf expr (cons (expand-macro-form language (car expr) env) (cdr expr))))
     (if (redundant-function-p expr)
         (loop for form in (cddar expr) do
              (out (statement form) :freshline))
         (out "return " (expression expr) ";")))

    ((redundant-apply-p expr)
     (destructuring-bind (name (function empty &rest body) this &optional args) expr
       (declare (ignore name this args function empty))
       (loop for form in body  do (out (statement form) :freshline))))

    ((redundant-call-p expr)
     (destructuring-bind (name (function empty &rest body) this &rest args) expr
       (declare (ignore name this args function empty))
       (loop for form in body  do (out (statement form) :freshline))))
    (t
     (out "return" (when expr (out " " (expression expr))) ";"))))


;;; Is this needed anymore? Probabyl won't work for Lispscript since
;;; we generate calls to apply. On the other hand, Lispscript has its
;;; own way of avoiding generating redundant scopes. Maybe.
(defun redundant-function-p (expr)
  (and
   (consp expr)
   (consp (car expr))
   (eql (caar expr) 'function)
   (eql (cadar expr) '())))

;; Bit of a hack to help out Lispscript generation.
(defun redundant-apply-p (expr)
  (and (consp expr) (eql (car expr) 'monkeylib-foo-lispscript::|.apply|)
       (destructuring-bind (name function this &optional args) expr
         (declare (ignore name function))
         (and
          (eql this 'monkeylib-foo-lispscript::|this|)
          (equal args '(array))))))

(defun redundant-call-p (expr)
  (and (consp expr) (eql (car expr) 'monkeylib-foo-lispscript::|.call|)
       (destructuring-bind (name function this &rest args) expr
         (declare (ignore name function))
         (and
          (eql this 'monkeylib-foo-lispscript::|this|)
          (null args)))))

;; With
(define-javascript-special-operator with :statement (language processor expr stmt &environment env)
  "with (" (expression expr (parenthesized env)) ") " (statement stmt))

;; Switch
(define-javascript-special-operator switch :statement (language processor expr &rest clauses &environment env)
  "switch (" (expression expr (parenthesized env)) ") {" :newline :indent
  (loop for (e . statements) in clauses do
       (if (eql e :default)
           (out "default:" :newline :indent)
           (out "case " (expression e) ":" :newline :indent))
       (loop for s in statements do (statement s) (out :freshline))
     (out :freshline :unindent))
  :freshline :unindent
  "}")

;; Labeled statement
(define-javascript-special-operator label :statement (language processor label statement)
  (name label) ": " (statement statement))

;; Throw
(define-javascript-special-operator throw :statement (language processor expr)
  "throw " (expression expr) ";")

;; Try
(define-javascript-special-operator try :statement (language processor &rest body)
  (flet ((key (e) (if (consp e) (first e))))
    (let ((catch-clause (find 'catch body :key #'key))
          (finally-clause (find 'finally body :key #'key)))
      (when catch-clause
        (assert
         (let ((next (cdr (member catch-clause body))))
           (or (null next) (eql (car next) finally-clause)))))
      (when finally-clause
        (assert (null (cdr (member finally-clause body)))))

      (setf body (ldiff body (or (member catch-clause body)
                                 (member finally-clause body))))
      (out
       "try {" :newline :indent
       (loop for stmt in body do (out (statement stmt) :freshline))
       :unindent :freshline "}"
       (when catch-clause
         (destructuring-bind (var &rest body) (rest catch-clause)
           (out
            " catch (" (name var) ") {" :newline :indent
            (loop for stmt in body do (out (statement stmt) :freshline))
            :unindent :freshline "}")))
       (when finally-clause
         (out
          " finally {" :newline :indent
            (loop for stmt in (rest finally-clause) do (out (statement stmt) :freshline))
            :unindent :freshline "}"))))))

;; Function -- two kinds, named and anonymous. The former is a
;; statement; the latter an expression.
(define-javascript-special-operator function :statement (language processor &rest body &environment env)
  (flet ((params (params)
           (out "("
                (loop for (p . rest) on params do
                     (out (name p)) (when rest (out ", ")))
                ")"))
         (body (body)
           (process language processor `(block ,@body) (new-env 'statement-or-expression :statement env))))
    (if (and (symbolp (first body)) (not (null (first body))))
        (destructuring-bind (name (&rest params) &rest body) body
          (out "function " (name name) " " (params params) " " (body body)))
        (destructuring-bind ((&rest params) &rest body) body
          #+(or)(when (eql (statement-or-expression env) :expression) (raw-string processor "("))
          (out "function " (params params) " " (body body))
          #+(or)(when (eql (statement-or-expression env) :expression) (raw-string processor ")"))))))


(define-javascript-special-operator augment-environment :statement (language processor (&rest pairs) &body body &environment env)
  (let ((env (append pairs env)))
    (loop for form in body do
         (process language processor form env))))
