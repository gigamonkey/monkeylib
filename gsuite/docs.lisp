(in-package :monkeylib-gsuite)

(defparameter *auth-token-file* #.(merge-pathnames "token.txt" (or *load-truename* *compile-file-truename*)))

(defvar *auth-token*)

(defparameter *doc-id* "15SIOd6AJYbTTSuzB6ogD06H9SLrnqesF4nJ-0weemi0")

(defparameter *docs-base-url* "https://docs.googleapis.com")

(defparameter *drive-base-url* "https://www.googleapis.com/drive/v3")

(defparameter *docs-discovery* "https://docs.googleapis.com/$discovery/rest?version=v1")

(defparameter *drive-discovery* "https://www.googleapis.com/discovery/v1/apis/drive/v3/rest")

(defparameter *discovery-discovery* "https://discovery.googleapis.com/discovery/v1/apis")

;; Drakma out of the box only considers text/* to be text.
(pushnew (cons "application" "json") *text-content-types* :test #'equal)

;;; If we want to generate a full API use this.

(defun save-discovery (file &key (url *docs-discovery*))
  (with-output-to-file (out file)
    (write-string (http-request url) out)))

;;; Hack for now. I guess this could just be a macro. But it's nice to be able to see the definitions.
(defun gen-schemas (file out)
  (let* ((*print-case* :downcase)
         (*print-right-margin* 120)
         (schemas (gethash "schemas" (parse-json (file-text file))))
         (names (sort (loop for k being the hash-keys of schemas collect k) #'string<)))
    (print `(in-package :monkeylib-gsuite) out)
    (loop for name in names
          do (format out "~2&~s" `(defschema ,(camel-to-kebab name *package*) ,@(loop for f being the hash-keys of (gethash "properties" (gethash name schemas)) collect f))))))


;;; HTTP foo

(defun reload-auth-token ()
  (setq *auth-token* (string-trim '(#\Newline) (file-text *auth-token-file*))))

(defun http-get (url params)
  (http-request
   url
   :method :get
   :parameters params
   :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *auth-token*)))))

(defun http-post (url content)
  (http-request
   url
   :method :post
   :content-type "application/json; charset=UTF-8"
   :content (flexi-streams:string-to-octets  (json content) :external-format :utf-8)
   :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *auth-token*)))))


;;; REST methods


(defun document-create (title)
  (let ((url (format nil "~a/v1/documents" *docs-base-url*)))
    (gethash
     "documentId"
     (monkeylib-json-parser:parse-json (http-post url (document :title title))))))

(defun document-batch-update (document-id requests)
  (let ((url (format nil "~a/v1/documents/~a:batchUpdate" *docs-base-url* document-id)))
    (http-post url (batch-update-document-request :requests requests))))

(defun document-get (document-id &key suggestions-view-mode)
  (let ((url (format nil "~a/v1/documents/~a" *docs-base-url* document-id))
        (params ()))
    (when suggestions-view-mode (push (cons "suggestionsViewMode" suggestions-view-mode) params))
    (http-get url params)))

(defun drive-comments-list (file-id &key include-deleted page-size page-token start-modified-time)
  (let ((url (format nil "~a/files/~a/comments" *drive-base-url* file-id))
        (params (list (cons "fields" "*"))))
    (when include-deleted (push (cons "includeDeleted" "true") params))
    (when page-size (push (cons "pageSize" page-size) params))
    (when page-token (push (cons "pageToken" page-token) params))
    (when start-modified-time (push (cons "startModifiedTime" start-modified-time) params))
    (http-get url params)))


;;; API

(defun append-to-doc (document-id text)
  (document-batch-update
   document-id
   (vector
    (request :insert-text
             (insert-text-request :text text :end-of-segment-location (end-of-segment-location))))))

(defun make-update-text-style-request (start end style)
  (request :update-text-style
           (update-text-style-request
            :fields (format nil "~{~a~^,~}" (loop for x in style by #'cddr collecting x))
            :range (range :start-index start :end-index end)
            :text-style style)))

(defun make-update-paragraph-style-request (start end style)
  (request :update-paragraph-style
           (update-paragraph-style-request
            :fields (format nil "~{~a~^,~}" (loop for x in style by #'cddr collecting x))
            :range (range :start-index start :end-index end)
            :paragraph-style style)))

(defun grey (amt)
  (color :rgb-color (rgb-color :blue amt :green amt :red amt)))
