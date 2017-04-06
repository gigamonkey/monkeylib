;;
;; Copyright (c) 2017, Peter Seibel. All rights reserved.
;;

(in-package :com.gigamonkeys.yamp)

;;; TODO:
;;;
;;; - state manipulation
;;;
;;; - IF forms
;;;

#|

* Evaluation rules

SYMBOL naming a production or external parser

  => `(lambda (text state position) (,SYMBOL text state position))

(SYMBOL &rest args) where SYMBOL names a production or an external parser

  => `(lambda (text state position) (,SYMBOL ,@args text state position))

SYMBOL not naming a production or external parser. This is an error at
the top-level of a production except as the last form.

  => SYMBOL

(SYMBOL &rest args) where SYMBOL does not name a production or an
external parser. This is an error at the top-level of a production
except as the last form.

  => (,SYMBOL ,@args)

(MATCH "STRING")

  => (string-matcher "STRING")

(MATCH #\Character)

  => (character-matcher #\Character)

(OR ...)

  =>


(PROGN p), p a parser function

  => (lambda (text state position)
       (funcall p text state position))

(PROGN x), x not a parser function

  => (lambda (text state position)
       (declare (ignore text))
       (values t x state position))


(PROGN p1 p2 ... x) where X evaluates to a production or parser

  => (lambda (text state position)
       (multiple-value-bind (ok r s pos) (funcall ,p1 text state position)
         (if ok
             (multiple-value-bind (ok r s pos) (funcall ,p2 text s p)
               ...
               (if ok
                   (multiple-value-bind (ok r s pos) (funcall ,x text s p)
                     (if ok
                         (values t r s p)
                         (values nil nil state position))))))))


(PROGN p1 p2 ... x) where X does not evaluate to a production or parser

  => (lambda (text state position)
       (multiple-value-bind (ok r s pos) (funcall ,p1 text state position)
         (if ok
             (multiple-value-bind (ok r s pos) (funcall ,p2 text s p)
               (if ok
                   (values t ,x s p)
                   (values nil nil state position))))))


(-> p SYMBOL)

  => (multiple-value-bind (ok SYMBOL s pos) (funcall p text state position)
       (if ok
           <continuation>
            (t (values nil nil state position))))))


(STATE SYMBOL)

  => (cdr (assoc symbol state))

(SETF (STATE SYMBOL) VALUE)

  => (setf state (acons symbol value state))

Examples:

(many (try eol))

  => (lambda (text state position)
       (many (lambda (text state position)
               (try
                (lambda (text state position) (eol text state position))))
             text
             state
             position))

(many (p foo (-> bar res) baz res))

  => (lambda (text state position)
        (many (lambda (text state position)
                (multiple-value-bind (ok r s pos) (funcall foo text state position)
                  (if ok
                      (multiple-value-bind (ok res s pos) (funcall bar text state position)
                        (if ok
                            (multiple-value-bind (ok r s pos) (funcall baz text state position)
                              (if ok
                                  (values t res s pos)
                                  (values nil nil state pos)))
                            (values nil nil state pos)))
                      (values nil nil state pos))))
              text state position))

(count 3 " ")

  => (lambda (text state position)
       (count 3 (lambda (text state position) (match-string " " text state position))))

(progn foo bar)

  => (lambda (text state position)
       (multiple-value-bind (ok x s pos) (funcall foo text state position)
         (if ok
             (multiple-value-bind (ok r s pos) (funcall bar text state position)
               (if ok
                   (values t r s pos)
                   (values nil nil state position)))
             (values nil nil state position))))


(progn (-> foo x) `(:whatever x))

  => (lambda (text state position)
       (multiple-value-bind (ok x s pos) (funcall foo text state position)
         (if ok
             (values t `(:whatever ,x) s pos)
             (values nil nil state position))))

(or foo bar)

  => (multiple-value-bind (ok r s pos)
         (foo text state position)
       (if ok
           (values t r s pos) ;; success
           (multiple-value-bind (ok r s pos)
               (bar text state position)
             (if ok
                 (values t r s pos) ;; success
                 (values nil nil state position)))))



(STATE :name) (except in SETF, et al. forms) access the named
value in the current (i.e. passed in) state.

(SETF (STATE :name) ...) modify the named value in the new state.
(INCF (STATE :name) ...) modify the named value in the new state.
(DECF (STATE :name) ...) modify the named value in the new state.

|#

(defun compile-parser (p)
  (destructuring-bind (defparser name (&rest args) &rest body) p
    (declare (ignore defparser))
    (%compile-parser name args body)))

(defun %compile-parser (name args body)
  (let* ((productions (mapcar #'normalize-production body))
         (names (mapcar #'caar productions)))
    `(defun ,name (text position ,@args)
       (flet (,@(loop for p in productions collect
                    (destructuring-bind ((name &rest args) &rest body) p
                      `(,name (,@args text state position)
                              ,(compile-progn body 'text 'state 'position names (gensym "R"))))))
         (,(first names)
           text
           (list ,@(mapcar #'(lambda (x) `(cons ',x ,x)) args))
           position)))))

(defun normalize-production (p)
  (destructuring-bind (x &rest body) p
    `(,(typecase  x
         (cons x)
         (symbol (list x)))
       ,@body)))

(defun production (p)
  "A parser is made up of productions."
  (destructuring-bind ((name &rest args) &rest body) p
    `(:name ,name :args ,args :body ,body)))

(defun grammar-p (name names)
  (or (member name names) (get name 'parser-function)))

(defun compile-progn (body txt st pos names result)
  "Compile the forms in an PROGN so that the PROGN matches if each of
the elements matches in sequence."
  (unless (null body)
    (with-gensyms (s p)
      (compile-wrapped-form
       #'progn-wrapper
       (first body)
       txt st pos s p names result
       (compile-progn (rest body) txt s p names (gensym "R"))))))

(defun progn-wrapper (expr ok result s p continuation failure)
  (if (null continuation)
      expr
      `(multiple-value-bind (,ok ,result ,s ,p) ,expr
         (if ,ok
             ,(or continuation `(values t ,result ,s ,p))
             ,failure))))

(defun compile-or (body txt st pos names result)
  "Compile the forms in an OR so that the OR matches if any of the
elements matches, returning the result from the first match."
  (unless (null body)
    (with-gensyms (s p)
      (compile-wrapped-form
       #'or-wrapper
       (first body)
       txt st pos s p names result
       (compile-or (rest body) txt s p names (gensym "R"))))))

(defun or-wrapper (expr ok result s p continuation failure)
  `(multiple-value-bind (,ok ,result ,s ,p) ,expr
     (if ,ok
         (values t ,result ,s ,p)
         ,(or continuation failure))))

(defun compile-wrapped-form (wrapper form text s-in p-in s p names r continuation)
  (with-gensyms (ok)
    (let ((failure `(values nil nil ,s-in ,p-in)))
      (labels ((wrap (expr) (funcall wrapper expr ok r s p continuation failure)))
          (cond

            ((parser-function-invocation-p form names)
             (wrap (compile-parser-call form names text s-in p-in)))

            ((binding-form-p form)
             (destructuring-bind (expr var) (cdr form)
               (let ((cexp (compile-wrapped-form wrapper expr text s-in p-in s p names (gensym "R") nil)))
                 (funcall wrapper cexp ok var s p continuation failure))))

            ((progn-p form)
             (compile-progn (rest form) text s-in p-in names r))

            ((or-p form)
             (compile-or (rest form) text s-in p-in names r))

            ((stringp form)
             (wrap (compile-string form text s-in p-in)))

            ((match-form-p form)
             (wrap (compile-match (cadr form) text s-in p-in)))

            (t form))))))

(defun compile-expression (exp names)
  (with-gensyms (text state position)
    (cond
      ((parser-function-invocation-p exp names)
       (maybe-thunk (rewrite-form exp names) names))

      ((progn-p exp)
       (thunk (compile-progn (rest exp) text state position names (gensym "R")) text state position))

      ((or-p exp)
       (thunk (compile-or (rest exp) text state position names (gensym "R")) text state position))

      ((match-form-p exp)
       ())

      ((stringp exp)
       (thunk (compile-string exp text state position) text state position))
      (t exp))))

(defun thunk (exp text state position)
  `(lambda (,text ,state ,position) ,exp))

(defun maybe-thunk (exp names)
  (destructuring-bind (name &rest args) exp
    (if args
        (with-gensyms (txt s p)
          (thunk (compile-parser-call exp names txt s p) txt s p))
        `(function ,name))))

(defun compile-string (str text state position)
  `(matching ,str ,(length str) ,text ,state ,position))

(defun matching (s len text state position)
  (let* ((end (+ len position))
         (ok (string= s text :start2 position :end2 end)))
    (if ok
        (values t s state end)
        (values nil nil state position))))

(defun compile-match (parser txt s p)
  `(funcall ,parser ,txt ,s ,p))

(defun compile-parser-call (exp names txt s p)
  (destructuring-bind (name &rest args) (rewrite-form exp names)
    (flet ((comp (x) (compile-expression x names)))
      `(,name ,@(mapcar #'comp args) ,txt ,s ,p))))

(defun rewrite-form (form names)
  "Rewrite bare symbols naming parser productions or parser functions
into cannonical list from."
  (cond
    ((and (symbolp form) (grammar-p form names)) (list form))
    (t form)))

(defun parser-function-invocation-p (form names)
  (or
   (and (symbolp form) (grammar-p form names))
   (and (consp form) (symbolp (car form)) (grammar-p (car form) names))))

(defun binding-form-p (form)
  (and (consp form) (eql (car form) '->)))

(defun progn-p (form)
  (and (consp form) (eql (car form) 'progn)))

(defun or-p (form)
  (and (consp form) (eql (car form) 'or)))

(defun match-form-p (form)
  (and (consp form) (eql (car form) 'match)))

(defmacro defparserfun (name (&rest args) &body body)
  `(progn
     (setf (get ',name 'parser-function) t)
     (defun ,name (,@args) ,@body)))


(defparserfun any-char (text state position)
  "Match a single character. Succeeds everywhere except at the EOF."
  (if (< position (length text))
      (values t (char text position) state (1+ position))
      (values nil nil state position)))

(defparserfun try (p text state position)
  "Attempt to parse using p, moving forward if it succeeds. However if
it fails, it does not consume any input."
  (multiple-value-bind (ok r ns np) (funcall p text state position)
    (if ok
        (values t r ns np)
        (values nil nil state position))))

(defparserfun many (p text state position)
  "Match P as many times as possible. Always succeeds as zero is an
acceptable number of times to match."
  (loop with r = nil
     while t do
       (multiple-value-bind (ok result new-state new-position)
           (funcall p text state position)
         (cond
           (ok
            (push r result)
            (setf state new-state)
            (setf position new-position))
           (t
            (return (values t (nreverse r) state position)))))))

(defparserfun many1 (p text state position)
  "Match P as many times as possible but at least once."
  (multiple-value-bind (ok r s p) (many p text state position)
    (if (and ok r)
        (values t r s p)
        (values nil nil state position))))

(defparserfun not-followed-by (p text state position)
  "Match only if not followed by P."
  (multiple-value-bind (ok r s pos)
      (funcall p text state position)
    (declare (ignore r s pos))
    (values (not ok) text state position)))

(defparserfun optional (p text state position)
  "Match P if we can. Doesn't return anything. Only fails if P
consumes input before failing."
  (multiple-value-bind (ok r s pos) (funcall p text state position)
    (declare (ignore ok r))
    (values (= pos position) t nil s pos)))

(defparserfun counted (n p text state position)
  "Match P N times."
  (if (zerop n)
      (values text state position)
      (multiple-value-bind (ok r s pos) (funcall p text state position)
        (declare (ignore r))
        (if ok
            (counted (1- n) p text s pos)
            (values nil nil state position)))))

(defparserfun look-ahead (p text state position)
  (multiple-value-bind (ok r s pos) (funcall p text state position)
    (declare (ignore r s pos))
    (if ok
        (values t nil state position)
        (values nil nil state position))))
