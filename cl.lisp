(cl:in-package "https://github.com/g000001/srfi-46#internals")
(in-readtable :quasiquote)

(cl:defparameter *cl-define-syntax-mstore* (null-mstore))

(cl:defmacro define-syntax (cl:&whole whole name cl:&body body)
  (cl:declare (cl:ignore body))
  #+lispworks (cl:declare (hcl:lambda-list name cl:&body body))
  `(cl:eval-when (:compile-toplevel :load-toplevel :execute)
     (expand-top-level-forms!
      (list ',whole)
      *cl-define-syntax-mstore*)
     (cl:defmacro ,name (cl:&whole whole cl:&rest args)
       (cl:declare (cl:ignore args))
       (car (expand-top-level-forms!
             (list whole)
             *cl-define-syntax-mstore*)))))

(cl:defmacro let-syntax (cl:&whole whole binds cl:&body body)
  (cl:declare (cl:ignore binds body))
  (car (expand-top-level-forms! (list whole)
                                *cl-define-syntax-mstore*)))

(cl:defmacro letrec-syntax (cl:&whole whole binds cl:&body body)
  (cl:declare (cl:ignore binds body))
  (car (expand-top-level-forms! (list whole)
                                *cl-define-syntax-mstore*)))

#|(cl:defmacro with-scheme-macro (cl:&body body)
  (expand-top-level-forms!
   `(cl:progn ,@body)
   *cl-define-syntax-mstore*))|#

;;; eof
