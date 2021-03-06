(in-package :monkeylib-prose-diff)

;;;
;;; Generic functions for diffing vectors of objects.
;;;

(defun diff-vectors (old new &key (lcs-frobber #'identity) (test #'eql))
  "Diff two vectors returning a vector with the elements of old and
new wrapped in conses whose CAR is either :LCS, :DELETE, or :ADD.
Optionally frob the computed LCS before computing the diff."
  (loop with output = (make-array (length new) :adjustable t :fill-pointer 0)
     with old-i = 0
     with old-length = (length old)
     with new-i = 0
     with new-length = (length new)
     for next-lcs across (funcall lcs-frobber (lcs old new :test test))
     do
       (setf old-i (emit-diffs next-lcs old old-i old-length :delete output :test test))
       (setf new-i (emit-diffs next-lcs new new-i new-length :add output :test test))
       (vector-push-extend (cons :lcs next-lcs) output)

     finally
       (emit-diffs (cons nil nil) old old-i old-length :delete output)
       (emit-diffs (cons nil nil) new new-i new-length :add output)
       (return output)))

(defun emit-diffs (next-lcs v i max-i marker output &key (test #'eql))
  (cond
    ((< i max-i)
     (let ((idx (or (position next-lcs v :start i :test test) max-i)))
       (cond
         ((> idx i)
          (loop for j from i below idx do (vector-push-extend (cons marker (aref v j)) output))
          (1+ idx))
         (t
          (1+ i)))))
    (t i)))
