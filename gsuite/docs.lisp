(in-package :monkeylib-gsuite)

(defparameter *auth-token-file* "token.sexp")

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

(defun http-get (url params)
  (http-request
   url
   :method :get
   :parameters params
   :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" (auth-token))))))


(defun http-post (url &key (content nil content-supplied-p) (parameters nil parameters-supplied-p))
  (multiple-value-bind (resp status)
      (cond
        (content-supplied-p
         (http-request
          url
          :method :post
          :content-type "application/json; charset=UTF-8"
          :content (flexi-streams:string-to-octets  (json content) :external-format :utf-8)
          :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" (auth-token))))))
        (parameters-supplied-p
         (http-request
          url
          :method :post
          :parameters parameters
          :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *auth-token*)))))
        (t (error "Must supply either :content or :parameters")))
    (cond
      ((= status 200) resp)
      (t (error "~d: resp: ~a" status resp)))))


;;; Authentication

(defun auth-token (&optional (token-file *auth-token-file*))
  "Load the auth token, refreshing if needed."
  (getf (maybe-refresh-token token-file) :token))

(defun save-token (token-file &key token expires refresh-token token-uri client-id client-secret)
  (with-data-to-file (out token-file)
    (prin1
     (list :token token
           :expires expires
           :refresh-token refresh-token
           :token-uri token-uri
           :client-id client-id
           :client-secret client-secret)
     out)))

(defun maybe-refresh-token (token-file)
  (let ((data (file->sexp token-file)))
    (when (< (getf data :expires) (1+ (get-universal-time)))
      (let ((refreshed (apply #'refresh-token data)))
        (setf
         (getf data :token) (gethash "access_token" refreshed)
         (getf data :expires) (+ (get-universal-time) (gethash "expires_in" refreshed)))
        (apply #'save-token token-file data)))
    data))

(defun refresh-token (&key refresh-token client-id client-secret token-uri &allow-other-keys)
  "Refresh an expired token with the refresh token."
   (parse-json
    (http-post
     token-uri
     :parameters
     `(("client_id" . ,client-id)
       ("client_secret" . ,client-secret)
       ("refresh_token" . ,refresh-token)
       ("grant_type" . "refresh_token")))))


;;; REST methods


(defun document-create (title)
  (let ((url (format nil "~a/v1/documents" *docs-base-url*)))
    (gethash
     "documentId"
     (parse-json (http-post url :content (document :title title))))))

(defun document-batch-update (document-id requests)
  (let ((url (format nil "~a/v1/documents/~a:batchUpdate" *docs-base-url* document-id)))
    (parse-json (http-post url :content (batch-update-document-request :requests requests)))))

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
  (document-batch-update document-id (vector (make-insert-text-request text))))

(defun append-to-segment (document-id text segment-id)
  (document-batch-update document-id (vector (make-insert-text-request text :segment-id segment-id))))

(defun make-create-footnote-request (index)
  (request :create-footnote
           (create-footnote-request
            :location (location :index index))))

(defun make-insert-text-request (text &key (segment-id :null))
  (request :insert-text
           (insert-text-request
            :text text
            :end-of-segment-location (end-of-segment-location :segment-id segment-id))))

(defun make-update-text-style-request (start end style &key (segment-id :null))
  (request :update-text-style
           (update-text-style-request
            :fields (format nil "~{~a~^,~}" (loop for x in style by #'cddr collecting x))
            :range (range :start-index start :end-index end :segment-id segment-id)
            :text-style style)))

(defun make-update-paragraph-style-request (start end style &key (segment-id :null))
  (request :update-paragraph-style
           (update-paragraph-style-request
            :fields (format nil "~{~a~^,~}" (loop for x in style by #'cddr collecting x))
            :range (range :start-index start :end-index end :segment-id segment-id)
            :paragraph-style style)))

(defun grey (amt)
  (color :rgb-color (rgb-color :blue amt :green amt :red amt)))
