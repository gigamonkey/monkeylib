(in-package :com.gigamonkeys.markup.html)

(defun footnotes (tag sexp &key (number-format "~d"))
  (let ((note-num 0)
        (notes ())
        (a-href-class (format nil "~(~a~)-ref" tag))
        (note-number-class (format nil "~(~a~)-number" tag))
        (note-name-fmt (format nil "~(~a~)_~~d" tag))
        (note-href-fmt (format nil "#~(~a~)_~~d" tag))
        (noteref-name-fmt (format nil "~(~a~)ref_~~d" tag))
        (noteref-href-fmt (format nil "#~(~a~)ref_~~d" tag)))
    
    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((numberp x) x)
                 ((symbolp x) x)
                 ((eql (car x) tag)
                  (push x notes)
                  (let ((num (incf note-num)))
                    `((:a :href (:format ,note-href-fmt ,num) :class ,a-href-class) (:format ,number-format ,num))))
                 ((eql (car x) :p)
                  (let ((starting-note-num note-num)
                        (body (mapcar #'walker (cdr x))))
                    `(:p ,@(wrap-with-noterefs body starting-note-num note-num noteref-name-fmt))))
                 (t 
                  `(,(car x) ,@(mapcar #'walker (cdr x)))))))

      (let ((walked (walker sexp)))
        `(,@walked 
          ,@(when notes
                  `(((:div :class (:format "~(~a~)s" ,tag))
                   ,@(loop for num from 1 
                        for note in (nreverse notes)
                        collect 
                          (destructuring-bind (notetag (ptag . prest) . nrest) note
                            (declare (ignore notetag))
                            `((:div :class ,(string-downcase tag))
                              (,ptag
                               ((:a :name (:format ,note-name-fmt ,num)) "")
                               ((:a :href (:format ,noteref-href-fmt ,num) :class ,note-number-class)
                                ,(princ-to-string num)) " "
                                ,@prest)
                              ,@nrest)))))))))))

#+(or)(defun wrap-with-noterefs (body start current noteref-name-fmt)
  (assert (>= current start))
  (if (= start current)
     body
     `(((:a :name (:format ,noteref-name-fmt ,(1+ start)))
        ,@(wrap-with-noterefs body (1+ start) current noteref-name-fmt)))))

(defun wrap-with-noterefs (body start current noteref-name-fmt)
  ;; For HTML5 we should use :id instead of :name
  (assert (>= current start))
  (if (= start current)
     body
     `(((:a :name (:format ,noteref-name-fmt ,(1+ start))) "")
       ,@(wrap-with-noterefs body (1+ start) current noteref-name-fmt))))
