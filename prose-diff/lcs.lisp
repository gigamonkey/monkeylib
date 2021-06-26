(in-package :monkeylib-prose-diff)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public API for getting the LCS, the length of the LCS, the
;;; positions of the LCS in each input, and the raw LCS table.

(defun lcs (a b &key (test #'eql))
  "Compute the longest common subsequence of vectors `a' and `b'"
  (multiple-value-call #'%lcs a (lcs-table a b :test test)))

(defun lcs-length (a b &key (test #'eql))
  "Compute the length of the longest common subsequence of vectors `a' and `b'"
  (multiple-value-call #'aref (lcs-table a b :test test)))

(defun lcs-positions (a b &key (test #'eql))
  "Find the indices in a and b of the elements of the LCS."
  (multiple-value-call #'%lcs-positions (lcs-table a b :test test)))

(defun lcs-table (a b &key (test #'eql))
  "Compute the MxN table from which we can extract the LCS, and a
bunch of other good stuff."
  (let* ((m (length a))
         (n (length b))
         (table (make-array (list (1+ n) (1+ m)) :initial-element 0)))

    (flet ((lcs-length (j i)
             (cond
               ((funcall test (aref a (1- i)) (aref b (1- j)))
                (+ 1 (aref table (1- j) (1- i))))
               (t
                (max (aref table (1- j) i) (aref table j (1- i)))))))

      (loop for j from 1 to n do
           (loop for i from 1 to m do
              (setf (aref table j i) (lcs-length j i)))))

    (values table n m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive functions that take a pre-computed LCS table is input.

(defun %lcs (a table n m)
  "Compute the longest common subsequence given one of the strings and
an already computed table."
  ;;; FIXME: I wonder if we can use (class-of a) rather than 'vector.
  ;;; Seems to work but not sure if it is guaranteed to.
  (map 'vector (lambda (i) (aref a i)) (%lcs-positions table n m)))

(defun %lcs-positions (table n m)
  "Find the indices in a and b of the elements of the LCS from an already computed table."
  (let* ((len (aref table n m))
         (a-indices (make-array len))
         (b-indices (make-array len))
         (idx (1- len))
         (i m)
         (j n))

    (loop while (> (aref table j i) 0) do
      (let* ((current (aref table j i))
             (previous (1- current)))

        (cond
          ((and (= previous (aref table (1- j) (1- i)))
                (= previous (aref table j (1- i)))
                (= previous (aref table (1- j) i)))
           (decf j)
           (decf i)
           (setf (aref a-indices idx) i)
           (setf (aref b-indices idx) j)
           (decf idx))
          ((= current (aref table (1- j) i)) (decf j))
          ((= current (aref table j (1- i))) (decf i))
          (t (error "Assertion gone haywire: ~s ~s" j i)))))
    (values a-indices b-indices)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some other handy functions.

(defun show-lcs-table (a b &key (test #'eql))
  "Show the LCS table with labels."
  (multiple-value-bind (table rows columns) (lcs-table a b :test test)
    (format t "~&    ")
    (loop for c across a do (format t " ~a" c))
    (loop for r from 0 to rows do
      (loop for c from -1 to columns do
        (if (= c -1)
          (format t "~& ~a" (if (zerop r) " " (aref b (1- r))))
          (format t " ~d" (aref table r c)))))))

(defun similarity (a b)
  "Compute the similarity of vectors `a' and `b' in terms of the
average of the ratios of the length of the LCS to their length."
  (let ((lcs-length (lcs-length a b)))
    (/ (+ (/ lcs-length (length a)) (/ lcs-length (length b))) 2.0d0)))

(defun one-way-similarity (a b)
  "Like `similarity' but in only one direction."
  (float (/ (lcs-length a b) (length a)) 0d0))
