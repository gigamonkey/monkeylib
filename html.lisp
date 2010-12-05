(in-package :com.gigamonkeys.markup.html)

(defparameter *default-subdocument-tags* '(:note :comment))

(defun render (file &key title stylesheets scripts (links t) (subdocument-tags *default-subdocument-tags*))
  "Render `file' to an html file with a header made from `title',
`stylesheets', and `scripts'. If `title' is not supplied, we try to
guess from the first H1 in sexps. Tags specified with
`subdocument-tags' are parsed as subdocuments."
  (with-output-to-file (out (make-pathname :type "html" :defaults file))
    (render-to-stream
     file out
     :title title
     :stylesheets stylesheets
     :scripts scripts
     :links links
     :subdocument-tags subdocument-tags)))

(defun render-to-stream (file out &key
                         title 
                         stylesheets
                         scripts
                         (links t) 
                         (subdocument-tags *default-subdocument-tags*))
  "Render `file' to the stream `out' with a header made from `title',
`stylesheets', and `scripts'. If `title' is not supplied, we try to
guess from the first H1 in sexps. Tags specified with
`subdocument-tags' are parsed as subdocuments."
  (render-sexps-to-stream
   (parse-file file :parse-links-p links :subdocument-tags subdocument-tags) out
   :title title
   :stylesheets stylesheets
   :scripts scripts))

(defun render-sexps-to-stream (sexps out &key title stylesheets scripts)
  "Render `sexps' to `out' with a header made from `title',
`stylesheets', and `scripts'. If `title' is not supplied, we try to
guess from the first H1 in sexps."
  (with-foo-output (out)
    (emit-html
     `(:html
        ,(make-head (or title (guess-title sexps)) stylesheets scripts)
        ,(rewrite-body sexps)))))

(defun make-head (title stylesheets scripts)
  `(:head
    (:title ,title)
    (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
    ,@(loop for s in stylesheets collect `(:link :rel "stylesheet" :href ,s :type "text/css"))
    ,@(loop for s in scripts collect `(:script :src ,s))))

(defun rewrite-body (sexps) 
  (multiple-value-bind (sexps links) (extract-link-defs sexps)
    (fix-comments (fix-notes (rewrite-links (remap-tags (add-amazon-image-bugs sexps)) links)))))

(defun guess-title (sexps)
  (let ((first-h1 (find :h1 sexps :key (lambda (x) (and (consp x) (first x))))))
    (if first-h1
        (just-text first-h1)
        (format nil "HTML generated at ~/iso:8601/" (get-universal-time)))))

(defun just-text (sexp)
  (with-output-to-string (s)
    (labels ((walker (x)
               (typecase x
                 (string (write-string x s))
                 (cons (mapcar #'walker x)))))
      (walker sexp))))

(defun remap-tags (sexp)
  (labels ((walker (x)
             (cond
               ((stringp x) x)
               ((consp sexp) (remap-one-tag x #'walker)))))
    (walker sexp)))

(defun remap-one-tag (sexp walker-fn)
  (destructuring-bind (tag . content) sexp
    (let ((mapper (cdr (assoc tag *tag-mappings*))))
      (typecase mapper
        (null `(,tag ,@(mapcar walker-fn content)))
        (keyword `(,mapper ,@(mapcar walker-fn content)))
        (cons `(,mapper ,@(mapcar walker-fn content)))
        (symbol (funcall mapper sexp))))))

(defun fix-notes (sexp)
  (extract-things sexp :note #'make-reference/note #'make-replacement/note))

(defun make-replacement/note (what num)
  `(:a :name ,(format nil "~(~a~)ref_~d" what num)
       (:a :href ,(format nil "#~(~a~)_~d" what num) (:sup ,(princ-to-string num)))))

(defun make-reference/note (what num)
  (declare (ignore what))
  `((:sup ,(princ-to-string num))))

(defun fix-comments (sexp)
  (extract-things sexp :comment #'make-reference/comment #'make-replacement/comment))

(defun make-replacement/comment (what num)
  `(:a :name ,(format nil "~(~a~)ref_~d" what num) (:a :href ,(format nil "#~(~a~)_~d" what num) :class ,(format nil "~(~a~)_ref" what) ,(format nil "~:(~a~) " what) ,(princ-to-string num))))

(defun make-reference/comment (what num)
  `(:class ,(format nil "~(~a~)_number" what) ,(princ-to-string num)))

(defun extract-things (sexp what make-replacement make-reference)
  (let ((thing-num 0)
        (things ()))

    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) what)
                  (push x things)
                  (funcall make-replacement what (incf thing-num)))
                 (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))

      (let ((walked (walker sexp)))
        `(,@walked 
          ((:div :class ,(format nil "~(~a~)s" what))
           ,@(loop for num from 1 
                for thing in (nreverse things)
                collect 
                  (destructuring-bind (thingtag (ptag . prest) . nrest) thing
                    (declare (ignore thingtag))
                    `((:div :class ,(format nil "~(~a~)" what))
                      (,ptag
                       (:a :name ,(format nil "~(~a~)_~d" what num) (:a :href ,(format nil "#~(~a~)ref_~d" what num) ,@(funcall make-reference what num))) " "
                       ,@prest)
                      ,@nrest)))))))))


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

(defun extract-link-defs (sexp)
  (let ((links (make-hash-table :test #'equalp))
        (strip (gensym)))

    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :link_def)
                  (destructuring-bind (link url) (rest x)
                    (setf (gethash (just-text link) links) (just-text url)))
                  strip)
                 (t `(,(car x) ,@(remove strip (mapcar #'walker (cdr x))))))))
      (values (walker sexp) links))))

(defun rewrite-links (sexp links)
  (labels ((walker (x)
             (cond
               ((stringp x) x)
               ((symbolp x) x)
               ((eql (car x) :link)
                `((:a :href ,(link-url (link-key x) links)) ,@(rest (remove-key x))))
               (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))
    (walker sexp)))

(defun link-key (link)
  (just-text (or (find-if (lambda (x) (and (consp x) (eql (car x) :key))) link) link)))

(defun remove-key (link)
  (remove-if (lambda (x) (and (consp x) (eql (car x) :key))) link))

(defun link-url (key links)
  (or
   (gethash key links)
   (progn
     (warn "No link definition for ~a" key)
     "nowhere.html")))

