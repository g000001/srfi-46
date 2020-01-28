;;;; srfi-46.asd -*- Mode: Lisp;-*-

(cl:in-package :asdf)

(defsystem :srfi-46
  :version "20200129"
  :description
  "SRFI 46 for Common Lisp: Basic Syntax-rules Extensions"
  :long-description
  "SRFI 46 for Common Lisp: Basic Syntax-rules Extensions
https://srfi.schemers.org/srfi-46"
  :author "Taylor Campbell"
  :maintainer "CHIBA Masaomi"
  :license "GPL 2.0"
  :serial t
  :depends-on (:named-readtables
               :rnrs-compat
               :srfi-23
               :quasiquote1)
  :components ((:file "package")
               ;; (:file "readtable")
               (:file "srfi-46")
               (:file "cl")))

(defmethod perform :after ((o load-op)
                           (c (eql (find-system :srfi-46))))
  (let ((name "https://github.com/g000001/srfi-46")
        (nickname :srfi-46))
    (if (and (find-package nickname)
             (not (eq (find-package nickname) (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))

#|(defsystem :srfi-46
  :serial t
  :depends-on (:fiveam
               :named-readtables
               :rnrs-compat
               :srfi-23
               :quasiquote1)
  :components ((:file "package")
               (:file "readtable")
               (:file "srfi-46")
               (:file "cl")
               (:file "test")))|#


(defsystem :srfi-46.test
  :description
  "SRFI 46 for Common Lisp: Basic Syntax-rules Extensions"
  :author "CHIBA Masaomi"
  :maintainer "CHIBA Masaomi"
  :version "20200129"
  :serial t
  :depends-on (:srfi-46
               :fiveam
               :named-readtables
               :rnrs-compat
               :srfi-23
               :quasiquote1)
  :components ((:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-46.test))))
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let* ((pkg "https://github.com/g000001/srfi-46#internals")
               (result (funcall
                        (_ :fiveam :run)
                        (_ pkg :srfi-46))))
          (funcall (_ :fiveam :explain!) result)
          (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
