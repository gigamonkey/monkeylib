;;
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;

(in-package :monkeylib-foo.xml)

(defvar *css* (make-instance 'monkeylib-foo.css::css))

(define-html-macro :comment (&body body)
  `(:progn
    (:noescape "<!-- ")
    (:newline)
    ,@body
    (:newline)
    (:noescape " -->")))

(define-html-macro :character (name)
  `(:noescape (:format ,(if (numberp name) "&#~d;" "&~(~a~);") ,name)))

(define-html-macro :pi (name &rest attrs)
  `(:progn 
    (:noescape (:format "<?~a ~@{~(~a~)=\"~a\"~^ ~}?>" ,name ,@attrs))
    (:newline)))

(define-html-macro :doctype (name type id url)
  `(:progn
    (:noescape (:format "<!DOCTYPE ~a ~a \"~a\" \"~a\">" ,name ,type ,id ,url))
    (:newline)))

(define-html-macro :xhtml (&body body)
  `(:progn
     (:pi "xml" :version 1.0 :encoding "UTF-8")
    (:doctype "html" "PUBLIC" "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
    ((:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en")
     ,@body)))

(define-html-macro :css-stylesheet (href)
  `(:link :rel "stylesheet" :type "text/css" :href ,href))

;(define-html-macro :css (&rest values)
;  `(:attribute (:format "~@{~(~a~): ~a;~^ ~}" ,@values)))

(define-html-macro :css (&body body)
  `((:style :type "text/css")
    (:comment 
     (:with-language (*css*)
       ,@body))))


;; This macro doesn't really work so well since Lispscript is
;; case-sensitive and, at the moment, FOO/HTML is not. Not to mention
;; packaging issues. Bah.
(define-html-macro :lispscript (&body body)
  `((:script :type "text/javascript")
    (:comment
     (:with-language (monkeylib-foo.lispscript::*lispscript*)
       ,@body))))

(defparameter *interpolated-foo* '((:title . "My TITLE")))

(define-html-macro :interpolate (name)
  (cdr (assoc name *interpolated-foo*)))

