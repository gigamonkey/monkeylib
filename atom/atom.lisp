;;; Copyright (c) 2011, Peter Seibel.
;;; All rights reserved. See LICENSE for details.

(in-package :monkeylib-atom)

(define-xml-language atom
  (:block-elements
   :feed :entry :author :content)
  (:paragraph-elements
   :contributor :email :generator :id :link :name :published :rights :subtitle :title :updated :uri
   :category))

(defclass feed ()
  ((title                :initarg :title                :accessor title)
   (subtitle             :initarg :subtitle             :accessor subtitle)
   (updated              :initarg :updated              :accessor updated)
   (tag                  :initarg :tag                  :accessor tag)
   (feed-url             :initarg :feed-url             :accessor feed-url)
   (index-url            :initarg :index-url            :accessor index-url)
   (rights               :initarg :rights               :accessor rights)
   (default-author-name  :initarg :default-author-name  :accessor default-author-name)
   (default-author-email :initarg :default-author-email :accessor default-author-email)
   (default-author-uri   :initarg :default-author-uri   :accessor default-author-uri)
   (canonical-host       :initarg :canonical-host       :accessor canonical-host)
   (full-prefix          :initarg :full-prefix          :accessor full-prefix)
   (entries              :initarg :entries              :accessor entries)))

(defclass entry ()
  ((file                 :initarg :file                 :accessor file)
   (title                :initarg :title                :accessor title)
   (body                 :initarg :body                 :accessor body)
   (published            :initarg :published            :accessor published)
   (updated              :initarg :updated              :accessor updated)
   (categories           :initarg :categories           :accessor categories)))

(defun feed (feed)
  (with-slots (title subtitle updated tag feed-url index-url rights entries) feed
    (atom
      (:? :xml :version "1.0" :encoding "utf-8")
      ((:feed :xmlns "http://www.w3.org/2005/Atom")
       ((:title :type "text") title)
       (when subtitle (atom ((:subtitle :type "html") subtitle)))
       (:updated (:print (timestamp updated)))
       (:id tag)
       (:link :rel "self" :type "application/atom+xml" :href feed-url)
       (:link :rel "alternate" :type "text/html" :href index-url)
       (:rights rights)
       ((:generator :uri "http://www.gigamonkeys.com/monkeylib/" :version "1.0") "Monkeylib")
       (dolist (entry entries) (entry entry feed))))))

(defun entry (entry feed)
  (with-slots (default-author-name default-author-uri default-author-email canonical-host full-prefix)
      feed
    (with-slots (file title body published updated categories) entry
      (with-time (year month date) published
        (let ((name (pathname-name file)))
          (atom
            (:entry
             (:title title)
             (:link :rel "alternate" :type "text/html" :href (:print (absolute-permalink canonical-host full-prefix name year month date)))
             (:id (:print (tag-uri "gigamonkeys.com" 2007 name :month month :date date)))
             (:updated (:print (timestamp updated)))
             (:published (:print (timestamp published)))
             (:author
              (:name default-author-name)
              (:uri default-author-uri)
              (:email default-author-email))
             (dolist (category categories)
               (atom (:category :term category)))
             ((:content :type "xhtml" :xml\:lang "en" :xml\:base "*blog-base-url*")
              (emit-xhtml `((:div :xmlns "http://www.w3.org/1999/xhtml") ,@body))))))))))

(defun absolute-permalink (canonical-host prefix name year month date)
  (format nil "http://~a~a~a" canonical-host prefix (permalink name year month date)))

(defun permalink (name year month date)
  (format nil "~4,'0d/~2,'0d/~2,'0d/~a" year month date name))

(defun timestamp (utc)
  (format-iso-8601-time utc :time-zone 0))

(defun tag-uri (authority-name year specific &key month date fragment)
  "Generate a tag: URL as described in
http://www.taguri.org/07/draft-kindberg-tag-uri-07.html"
  (format nil "tag:~a,~4,'0d~@[-~2,'0d~]~@[-~2,'0d~]:~a~@[#~a~]"
          authority-name year month date specific fragment))
