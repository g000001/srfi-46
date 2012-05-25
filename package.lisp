;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :srfi-46
  (:use)
  (:export))
;(g1:delete-package* :srfi-46.internal)
(defpackage :srfi-46.internal
  (:use :srfi-46 :srfi-23 :rnrs :named-readtables :fiveam)
  (:import-from :rnrs-compat-internal
                :define-function
                :with-local-define-function)
  (:import-from :cl :funcall :nil)
  (:shadowing-import-from :cl :quote))
