(in-package :monkeylib-markup-html)

(defparameter *default-subdocument-tags* '(:note :comment))

(defun render (file &key
               title
               stylesheets
               scripts
               (links t)
               (subdocument-tags *default-subdocument-tags*)
               (rewriter #'identity))
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
     :subdocument-tags subdocument-tags
     :rewriter rewriter)))

(defun render-to-stream (file out &key
                         title
                         stylesheets
                         scripts
                         (links t)
                         (subdocument-tags *default-subdocument-tags*)
                         (rewriter #'identity))
  "Render `file' to the stream `out' with a header made from `title',
`stylesheets', and `scripts'. If `title' is not supplied, we try to
guess from the first H1 in sexps. Tags specified with
`subdocument-tags' are parsed as subdocuments."
  (render-sexps-to-stream
   (parse-file file :parse-links-p links :subdocument-tags subdocument-tags) out
   :title title
   :stylesheets stylesheets
   :scripts scripts
   :rewriter rewriter))

(defun render-sexps-to-stream (sexps out &key title stylesheets scripts (rewriter #'identity))
  "Render `sexps' to `out' with a header made from `title',
`stylesheets', and `scripts'. If `title' is not supplied, we try to
guess from the first H1 in sexps."
  (with-foo-output (out)
    (emit-html
     `(:html
        (:head
         (:title ,(or title (guess-title sexps)))
         (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
         ,@(loop for s in stylesheets collect `(:link :rel "stylesheet" :href ,s :type "text/css"))
         ,@(loop for s in scripts collect `(:script :src ,s)))
        ,(funcall rewriter sexps)))))

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

(defun make-retagger (mappings)
  (lambda (sexp) (remap-tags sexp mappings)))

(defun remap-tags (sexp mappings)
  (labels ((walker (x)
             (cond
               ((numberp x) (list x))
               ((stringp x) (list x))
               ;; FIXME: should this be x or sexp. If the latter,
               ;; comment to explain why.
               ((consp sexp) (remap-one-tag x mappings #'walker)))))
    (first (walker sexp))))

(defun remap-one-tag (sexp mappings walker-fn)
  (destructuring-bind (tag . content) sexp
    (let ((entry (assoc tag mappings)))
      (cond
        ((not entry)
        `((,tag ,@(mapcan walker-fn content))))
        (t
         (let ((mapper (cdr entry)))
           (typecase mapper
             (null ())
             (keyword `((,mapper ,@(mapcan walker-fn content))))
             (cons `((,mapper ,@(mapcan walker-fn content))))
             ((or symbol function)
              (multiple-value-bind (mapped recurse) (funcall mapper sexp)
                (list (if recurse (funcall walker-fn mapped) mapped)))))))))))

(defun htmlize-links (sexps)
  (multiple-value-bind (sexps links) (extract-link-defs sexps)
    (rewrite-links sexps links)))

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
