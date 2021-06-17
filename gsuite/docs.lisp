(in-package :monkeylib-gsuite)

(defvar *auth-token*)

(defparameter *docs-base-url* "https://docs.googleapis.com/")

(defparameter *docs-discovery* "https://docs.googleapis.com/$discovery/rest?version=v1")

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
  (setq *auth-token* (auth-token)))

(defun auth-token ()
  "Fetch the auth token we've stashed in a local file."
  (string-trim '(#\Newline) (file-text "/Users/peter/hacks/monkeylib/gsuite/token.txt")))

(defun post (url content)
  (http-request
   url
   :method :post
   :content-type "application/json; charset=UTF-8"
   :content (flexi-streams:string-to-octets  (json content) :external-format :utf-8)
   :additional-headers (list (cons "Authorization" (format nil "Bearer ~a" *auth-token*)))))

(defun create-document (title)
  (let ((url (format nil "~a/v1/documents" *docs-base-url*)))
    (gethash
     "documentId"
     (monkeylib-json-parser:parse-json (post url (document :title title))))))

(defun batch-update-request (document-id requests)
  (let ((url (format nil "~a/v1/documents/~a:batchUpdate" *docs-base-url* document-id)))
    (post url (batch-update-document-request :requests requests))))

(defun append-to-doc (text document-id)
  (batch-update-request
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


(defun markup-styles (ranges)
  (let ((updates ()))
    (labels ((walk (x)
               (when (consp x)
                 (destructuring-bind ((tag start end) . rest) x
                   (flet ((add-text-style (&rest keys &key &allow-other-keys)
                            (push (make-update-text-style-request (1+ start) (1+ end) (apply #'text-style keys)) updates))
                          (add-paragraph-style (&rest keys &key &allow-other-keys)
                            (push (make-update-paragraph-style-request (1+ start) (1+ end) (apply #'paragraph-style keys)) updates)))
                     (case tag
                       (:h1
                        (add-text-style :bold t :font-size (dimension :magnitude 24 :unit "PT"))
                        (add-paragraph-style  :named-style-type "TITLE"))
                       (:preface
                        (add-text-style :italic t :font-size (dimension :magnitude 10 :unit "PT")))
                       (:§
                        (add-paragraph-style :alignment "CENTER"))
                       (:i
                        (add-text-style :italic t)))
                     (dolist (x rest)
                       (walk x)))))))
      (walk ranges)
      (coerce (nreverse updates) 'vector))))

(defun to-google-doc (markup document-id)
  (let ((text (wlal:text-of markup))
        (ranges (wlal:text-ranges markup)))
    (append-to-doc text document-id)
    (batch-update-request document-id (markup-styles ranges))))
