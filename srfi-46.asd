;;;; srfi-46.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)

(defsystem :srfi-46
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :rnrs-compat)
  :components ((:file "package")
               (:file "readtable")
               (:file "srfi-46")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-46))))
  (load-system :srfi-46)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-46.internal :srfi-46))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))

