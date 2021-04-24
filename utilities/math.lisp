(in-package :monkeylib-utilities)

(defun ! (n)
  (loop for i to n for x = 1 then (* x i) finally (return x)))