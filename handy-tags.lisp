(in-package :com.gigamonkeys.markup.html.handy-tags)

;;;
;;; FIXME: Some of this stuff should be moved out of monkeylib as it's
;;; very specific to me. Though everyone should feel free to make
;;; links to amazon with this code. ;-)
;;;


(defparameter *amazon-link* "http://www.amazon.com/gp/product/~a?ie=UTF8&tag=gigamonkeys-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=~:*~a")

(defparameter *amazon-image-bug* "http://www.assoc-amazon.com/e/ir?t=gigamonkeys-20&l=as2&o=1&a=~a")

(defparameter *asins*
  (progn
    (let ((ht (make-hash-table :test #'equal)))
      (loop for (k v) in 
           '(("Peopleware" "0932633439")
             ("Practical Common Lisp" "1590592395")
             ("Rapid Development" "1556159005")
             ("Software Estimation" "0735605351")
             ("Software Estimation: Demystifying the Black Art" "0735605351")
             ("The Mythical Man Month" "0201835959")
             ("The Wisdom of Crowds" "0385503865")
             ("Founders at Work" "1590597141")
             ("Programmers at Work" "0914845713")
             ("Coders at Work" "1430219483" )
             ("Land of Lisp" "1593272006")
             ("Beautiful Code" "0596510047")
             ("Test Driven Development" "0321146530")
             ("The C++ Programming Language" "0201700735")
             ("The Design and Evolution of C++" "0201543303")
             ("Pink Brain, Blue Brain" "0618393110")
             ("Structure and Interpretation of Computer Programs" "0262011530")
             )
           do (setf (gethash k ht) v))
      ht)))

(defun amazon-link (sexp)
  (let* ((asin (gethash (just-text sexp) *asins*))
         (href (format nil *amazon-link* asin)))
    `((:a :href ,href) (:i ,@(rest sexp)))))

(defun amazon-image-bug (sexp)
  (let* ((asin (gethash (just-text sexp) *asins*))
         (src (format nil *amazon-image-bug* asin)))
    `(:img :src ,src :width "1" :height "1" :alt "" :style "border:none !important; margin:0px !important;")))

(defun add-amazon-image-bugs (sexp)
  (let ((books ()))
    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :book)
                  (push (just-text x) books)
                  x)
                 (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))

      (let ((walked (walker sexp)))
        `(,@walked ,@(mapcar (lambda (x) `(:amazon-image-bug ,x)) (nreverse books)))))))

(defun mailto-link (sexp)
  `(:a :href ,(format nil "mailto:~a" (just-text sexp)) ,@(rest sexp)))

(defun url-link (sexp)
  `(:a :href ,(just-text sexp) ,@(rest sexp)))
