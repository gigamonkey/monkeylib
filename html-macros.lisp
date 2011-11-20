;;; Copyright (c) 2005-2011, Peter Seibel.
;;; All rights reserved. See COPYING for details.

(in-package :monkeylib-html)

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

