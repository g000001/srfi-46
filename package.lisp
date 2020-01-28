;;;; package.lisp

(cl:in-package :cl-user)

(defpackage "https://github.com/g000001/srfi-46"
  (:use)
  (:import-from #:cl #:***)
  (:export #:define-syntax
           #:syntax-rules
           #:let-syntax
           #:letrec-syntax
           #:***))

(defpackage "https://github.com/g000001/srfi-46#internals"
  (:use "https://github.com/g000001/srfi-46"
        "https://github.com/g000001/srfi-23"
        #:rnrs
        #:named-readtables
        #:fiveam)
  (:import-from #:rnrs-compat-internal
                #:define-function
                #:with-local-define-function)
  (:import-from #:cl #:funcall #:nil)
  (:shadowing-import-from #:cl #:quote #:***)
  (:shadowing-import-from
   "https://github.com/g000001/srfi-46"
   #:define-syntax #:syntax-rules #:let-syntax #:letrec-syntax)
  ;; 5am vs srfi-46
  (:shadow #:skip))
