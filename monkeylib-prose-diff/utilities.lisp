(in-package :com.gigamonkeys.prose-diff)

;;; Bits of utility code that perhaps should be moved into
;;; com.gigamonkeys.utilities or replaced with calls to equivalent
;;; bits o fsome standard utility library.

(defun maximum (list &key (key #'identity))
  (when list
    (destructuring-bind (first . rest) list
      (loop with best-score = (funcall key first)
         with best = first
         for x in rest
         for score = (funcall key x) do
           (when (> score best-score)
             (setf best-score score)
             (setf best x))
         finally (return (values best best-score))))))

(defun take (list n)
  "Return a list of of the first n values of list and the left-over
tail as a secondary value."
  (let ((tail (nthcdr n list)))
    (values (ldiff list tail) tail)))

(defun vector-push-extend* (list v)
  "Push all the elements of `list' onto v as if by vector-push-extend"
  (loop for item in list do (vector-push-extend item v)))

(defun split-list (list tail)
  (let ((rest (nthcdr (or (search tail list) (error "~a not found in ~a" tail list)) list)))
    (values (ldiff list rest) rest)))

(defun longer (list-a list-b)
  (cond
    ((endp list-b)
     (not (endp list-a)))
    ((endp list-a)
     nil)
    (t (longer (cdr list-a) (cdr list-b)))))

(defun concatenate-vectors (vectors)
  (reduce (lambda (a b) (concatenate (class-of a) a b)) vectors))

(defun map-tree (fn tree)
  "Map fn down tree, replacing each element of the tree with the
  return value of fn. When the return value is identical to the
  original sub-tree it is recursively mapped."
  (typecase tree
    (cons (let ((new (funcall fn tree)))
            (if (eql new tree)
                (mapcar (lambda (x) (map-tree fn x)) tree)
                new)))
    (t tree)))