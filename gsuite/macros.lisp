(in-package :monkeylib-gsuite)

(defun property-args (properties)
  (mapcar #'(lambda (p)
              (list (make-symbol (string (camel-to-kebab p)))
                    nil
                    (make-symbol (string (camel-to-kebab (format nil "~a-P" p))))))
          properties))


(defmacro defschema (name &rest properties)
  (with-gensyms (data)
    (let ((prop-args (property-args properties)))
      `(defun ,name (&key ,@prop-args)
         (let ((,data ()))
           ,@(loop for prop in properties
                   for (arg nil arg-p) in prop-args
                   collect `(when (and ,arg-p (not (eql ,arg :null)))
                              (push ,arg ,data)
                              (push ,prop ,data)))
           ,data)))))
