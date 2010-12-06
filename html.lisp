(in-package :com.gigamonkeys.markup.html)

(defparameter *default-subdocument-tags* '(:note :comment))

(defun render (file &key 
               title
               stylesheets
               scripts
               (links t)
               (subdocument-tags *default-subdocument-tags*)
               rewriter)
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
                         rewriter)
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

(defun render-sexps-to-stream (sexps out &key title stylesheets scripts rewriter)
  "Render `sexps' to `out' with a header made from `title',
`stylesheets', and `scripts'. If `title' is not supplied, we try to
guess from the first H1 in sexps."
  (with-foo-output (out)
    (emit-html
     `(:html
        ,(make-head (or title (guess-title sexps)) stylesheets scripts)
        ,(funcall rewriter sexps)))))

(defun make-head (title stylesheets scripts)
  `(:head
    (:title ,title)
    (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
    ,@(loop for s in stylesheets collect `(:link :rel "stylesheet" :href ,s :type "text/css"))
    ,@(loop for s in scripts collect `(:script :src ,s))))

(defun rewrite-body (sexps pre-mapping post-mapping tag-mappings)
  "Given `sexps' extract the link-defs and then process the remaining
sexp first with the pre-mapping processors, then remap tags and
rewrite links, and finally with the post-mapping processors."
  (multiple-value-bind (sexps links) (extract-link-defs sexps)
    (let ((pre (apply #'compose pre-mapping))
          (post (apply #'compose post-mapping)))
      (funcall post (rewrite-links (remap-tags (funcall pre sexps) tag-mappings) links)))))

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
               ((stringp x) x)
               ((consp sexp) (remap-one-tag x mappings #'walker)))))
    (walker sexp)))

(defun remap-one-tag (sexp mappings walker-fn)
  (destructuring-bind (tag . content) sexp
    (let ((mapper (cdr (assoc tag mappings))))
      (typecase mapper
        (null `(,tag ,@(mapcar walker-fn content)))
        (keyword `(,mapper ,@(mapcar walker-fn content)))
        (cons `(,mapper ,@(mapcar walker-fn content)))
        (symbol (funcall mapper sexp))))))

(defun fix-comments (sexp) (footnotes :comment sexp :number-format "Comment ~d"))

(defun fix-notes (sexp) (footnotes :note sexp))

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

