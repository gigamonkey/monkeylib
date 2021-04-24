;;; Copyright (c) 2011, 2012 Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :monkeylib-atom)

(defun parse-feed (pathname)
  "Parse a feed.sexp file into a feed object."
  (let* ((init-args (loop for (key value) in (file->list pathname) collect key collect value))
         (feed (apply #'make-instance 'feed init-args)))
    (setf (updated feed) (file-write-date pathname))
    (setf (feed-url feed) (format nil "http://~a~afeed.atom" (canonical-host feed) (full-prefix feed)))
    (setf (entries feed) (parse-entries (merge-pathnames (entries feed) pathname)))
    feed))

(defun parse-entries (pathname)
  "Parse the entries specified in an entries file."
  (let ((content-dir (merge-pathnames "content/" (merge-pathnames (parent-directory pathname)))))
    (loop for x in (file->list pathname) collect
         (destructuring-bind (file published updated &key categories) x
           (setf file (merge-pathnames content-dir file))
           (destructuring-bind (title body) (parse-entry file)
             (make-instance 'entry
               :file file
               :title title
               :body body
               :published published
               :updated updated
               :categories categories))))))

(defun parse-entry (file)
  (destructuring-bind (body-tag h1 &rest body) (parse-file file :parse-links-p t)
    (declare (ignore body-tag))
    (list `(:progn ,@(rest h1)) body)))