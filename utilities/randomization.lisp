(in-package :monkeylib-utilities)

(defun nshuffle-vector (vector)
  "Shuffle a vector in place using Fisher-Yates algorithm." 
  (loop for idx downfrom (1- (length vector)) to 1
        for other = (random (1+ idx))
        do (unless (= idx other)
             (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  "Return a shuffled copy of vector."
  (nshuffle-vector (copy-seq vector)))


(defun shuffle-list (list)
  "Return a shuffled copy of list. Based on a post by Ben
Rudiak-Gould in comp.lang.functional, Message-ID:
<j7o400l04cdn4oaihgcu9ats5r0eel3hg0@4ax.com>"
  (if (or (null list) (null (cdr list)))
      list
      (let (heads tails)
	(dolist (item list (nconc (shuffle-list heads) (shuffle-list tails)))
	  (if (zerop (random 2))
	      (push item heads)
	      (push item tails))))))

(defun random-selection (list n)
  (let* ((rest (nthcdr n list))
         (sample (coerce (ldiff list rest) 'vector)))
    (loop for item in rest
       for p from (1+ n)
       for r = (random p)
       when (< r n) do (setf (aref sample r) item))
    (coerce sample 'list)))

(defun test-random (n m iters)
  (let ((h (make-hash-table :test #'equal))
        (list (loop for i below m collect i)))
    (loop repeat iters
       for sample = (sort (random-selection list n) #'<)
       do (incf (gethash sample h 0)))
    (loop for k being the hash-keys of h using (hash-value v) do
         (format t "~&~a => ~f" k (* 100d0 (/ v (float iters 0d0)))))))
               
          



