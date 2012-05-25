(cl:in-package :srfi-46.internal)
(in-readtable :quasiquote)

(expand-top-level-forms '(1.23
                          cl:t
                          "foo"
                          #\a)
                        builtins-store 0 #'list)

(car (expand-top-level-forms '(list) builtins-store 0 #'list))
;=>  (LIST)

(car (expand-top-level-forms null-prog builtins-store 0 #'list))

;;; ???
(car (expand-top-level-forms '(8 (define foo 8))
                             builtins-store 0 #'list))
;=>  (8
;     (DEFINE FOO
;       8))

(car (expand-top-level-forms '(8 (set! foo 8))
                             builtins-store 0 #'list))
;=>  (8 (SET! FOO 8))

(car (expand-top-level-forms '(8 (lambda (x) x))
                             builtins-store 0 #'list))
;=>  (8 (LAMBDA (|_X_0|) |_X_0|))

(car (expand-top-level-forms '(8 (if a b c))
                             builtins-store 0 #'list))
;=>  (8 (IF A B C))

(car (expand-top-level-forms '((quote 8))
                             builtins-store 0 #'list))
;=>  ((QUOTE 8))

;;; beginが消える => どうもokらしい
(car (expand-top-level-forms '((begin z8 8))
                             builtins-store 0 #'list))
;=>  (Z8 8)
(car (expand-top-level-forms '((define foo (lambda (n)
                                             (begin 8))))
                             builtins-store 0 #'list))
;=>  ((DEFINE FOO
;       (LAMBDA (|_N_0|) 8)))

(expand-top-level-forms '((foo 8) (define-syntax foo
                            (syntax-rules ()
                              ((foo a ***)
                               (progn a ***))))
                          (define-syntax bar
                            (syntax-rules ()
                              ((bar a ***)
                               (progn a ***)))))
                        builtins-store 0 #'list)


(expand-program '((define-syntax foo
                    (syntax-rules ()
                      ((foo a)
                       (progn a))))
                  (foo 8)))

(cl:defparameter *defsyn-mstore* (null-mstore))

(expand-top-level-forms!
 '((define-syntax fake-begin
     (syntax-rules ()
       ((fake-begin ?body *** ?tail)
        (cl:let* ((ignored ?body) ***) ?tail))))
   (FAKE-BEGIN
    (DISPLAY "Hello,")
    (WRITE-CHAR #\SPACE)
    (DISPLAY "world!")
    (NEWLINE))
   (let-syntax
    ((foo (syntax-rules ()
            ((foo ?x ?y *** ?z)
             (list ?x (list ?y ***) ?z)))))
     (foo 1 2 3 4 5)))
 *defsyn-mstore*)


(cl:defmacro with-scheme-macro (cl:&body body)
  (expand-top-level-forms!
   `(cl:progn ,@body)
   *defsyn-mstore*))

(with-scheme-macro
  #|(FAKE-BEGIN
   (DISPLAY "Hello,")
   (WRITE-CHAR #\SPACE)
   (DISPLAY "world!")
   (NEWLINE))|#
  (define-syntax foo
    (syntax-rules %%% ()
                  ((_ a %%%)
                   (cl:progn a %%%)
                   )))
  (foo 8 9 10))


(with-scheme-macro
  (define-syntax help-compose
    (syntax-rules ()
      ((help-compose 1 (import-name record) import *** (export-name (label exp) ***))
       (meta (quasiquote
              (help-compose 2
                            (meta (unquote (intersection
                                            (meta (unquote (export-name ("labels"))))
                                            (meta (unquote (remove-from (meta (unquote (import-name ("labels"))))
                                                                        (label ***)
                                                                        if-free= )))
                                            if-free= )))
                            (import-name record)
                            import ***
                            (export-name (label exp) ***) ))))
      ((help-compose 2
                     (copy-label ***)
                     (import-name record)
                     import ***
                     (export-name . bindings) )
       (meta (quasiquote
              (let ((r record))
                (record-compose import ***
                                (export-name
                                 (copy-label
                                  ((meta
                                         (unquote (import-name ("getter") copy-label)) )
                                   r )) ***
                                   . bindings))))))))
  (help-compose 2 (copy-label cl1 cl2 cl3)
                (import-name record)
                import  (export-name b1 b2 b3 b4))
  )

(with-scheme-macro
  (let-syntax ((foo (syntax-rules ()
                      ((_ n)
                       (cl:progn z n)))))
    (foo 8)))

(expand-program
 '((define-syntax be
     (syntax-rules ()
       ((_ ((var init) ***) . body)
        (cl:let ((hello 3) (var init) *** (bye 8)) .body))))
   ;;
   (be ((zfoo 3)) foo)))

(expand-program
 '((define-syntax case
     (syntax-rules (else)
       ((case expr0
          ((key ***) res1 res2 ***)
          ***
          (else else-res1 else-res2 ***) )
        (let ((tmp expr0))
          (cond
            ((memv tmp '(key ***)) res1 res2 ***)
            ***
            (else else-res1 else-res2 ***) )))
       ((case expr0
          ((keya ***) res1a res2a ***)
          ((keyb ***) res1b res2b ***)
          *** )
        (let ((tmp expr0))
          (cond
            ((memv tmp '(keya ***)) res1a res2a ***)
            ((memv tmp '(keyb ***)) res1b res2b ***)
            *** )))))
   ;;
   (case 8
     ((8 8 8 8) 8))))


;;; --------------------
;;; Examples of tail patterns

;;; This example of the tail pattern extension is a crippled version of
;;; R5RS's BEGIN special form.  (It is crippled because it does not
;;; support internal definitions or commands within its body returning
;;; fewer or more than one value.)

(expand-program
 '((define-syntax fake-begin
     (syntax-rules ()
       ((fake-begin ?body *** ?tail)
        (cl:let* ((ignored ?body) ***) ?tail))))
   (FAKE-BEGIN
    (DISPLAY "Hello,")
    (WRITE-CHAR #\SPACE)
    (DISPLAY "world!")
    (NEWLINE))
   (let-syntax
    ((foo (syntax-rules ()
            ((foo ?x ?y *** ?z)
             (list ?x (list ?y ***) ?z)))))
  (foo 1 2 3 4 5))))
((DEFINE _EQV?_15
   EQV?)
 (DEFINE _CONS_30
   CONS)
 (DEFINE _APPEND_31
   APPEND)
 (DEFINE _LIST_32
   LIST)
 (DEFINE _VECTOR_33
   VECTOR)
 (DEFINE _LIST->VECTOR_34
   LIST->VECTOR)
 (DEFINE _MAP_35
   MAP)
 (COMMON-LISP:LET* ((IGNORED (DISPLAY "Hello,"))
                    (IGNORED (WRITE-CHAR #\ ))
                    (IGNORED (DISPLAY "world!")))
   (NEWLINE))
 (LIST 1 (LIST 2 3 4) 5))




(expand-program
 '(
  ;;; Examples of the user-specified ellipsis token extension

;;; Utility macro for CPS macros
   (define-syntax apply-syntactic-continuation
     (syntax-rules ()
       ((apply-syntactic-continuation (?k ?env ***) . ?args)
        (?k ?env *** . ?args))))

;;; Generates a list of temporaries, for example to implement LETREC
;;; (see below), and 'returns' it by CPS.
   (define-syntax generate-temporaries
     (syntax-rules ()
       ((generate-temporaries ?origs ?k)
        (letrec-syntax
         ((aux (syntax-rules %%% ()
                             ;; We use a trick here: pass the continuation again
                             ;; to AUX in case it contains ellipsis.  If we stuck
                             ;; it right into AUX's template, AUX would process the
                 ;; ellipsis in ?K as ellipsis for something in the AUX
                             ;; macro.
                             ((aux ?temps () ?k*)
                              (apply-syntactic-continuation ?k* ?temps))
                             ;; Be careful about the ellipsis!
                             ((aux (?temp %%%) (?x ?more %%%) ?k*)
                              (aux (?temp %%% new-temp)
                                   (?more %%%)
                                   ?k*)))))
          (aux () ?origs ?k)))))

;;; Instead of having lots of auxiliary clauses in LETREC, like in the
;;; R5RS sample implementation, we use GENERATE-TEMPORARIES.  Instead
;;; of 'returning,' like an ordinary function, we create a continuation
;;;  for GENERATE-TEMPORARIES with LET-SYNTAX.  Since this continuation
;;; uses ellipsis, we must use the ellipsis token extension.
   (define-syntax letrec
     (syntax-rules ()
       ((letrec ((?var ?init) ***) ?body1 ?body2 ***)
        (let-syntax
                    ((k (syntax-rules %%% ()
                                      ;; Use the same trick as with the continuations in
                                      ;; GENERATE-TEMPORARIES.  Be careful about the ellipsis!
                                      ((k ((?var* ?init*) %%%)
                                          (?body1* ?body2* %%%)
                                          ;; Here are the actual arguments to the continuation
                                          ;; -- the previous bits of the pattern were just the
                                          ;; 'environment' of the continuation --:
                                          (?temp %%%))
                                       (let ((?var* (if 'NIL 'NIL)) ; Get an 'unspecific' value.
                                             %%%)
                                         (let ((?temp ?init*) %%%)
                                           (set! ?var* ?temp) %%%
                                           (let () ?body1* ?body2* %%%)))))))
          (generate-temporaries (?var ***)
                                ;; Pass K the environment.  GENERATE-TEMPORARIES will add the
                                ;; temporary variable list argument.
                                (k ((?var ?init) ***) (?body1 ?body2 ***)))))))
   ;;
   (letrec ((foo (lambda (x) x))
            (bar (lambda (x) x)))
     (foo (bar 8)))
   ))
((DEFINE _EQV?_15
   EQV?)
 (DEFINE _CONS_30
   CONS)
 (DEFINE _APPEND_31
   APPEND)
 (DEFINE _LIST_32
   LIST)
 (DEFINE _VECTOR_33
   VECTOR)
 (DEFINE _LIST->VECTOR_34
   LIST->VECTOR)
 (DEFINE _MAP_35
   MAP)
 ((LAMBDA (_FOO_50 _BAR_51)
          ((LAMBDA (_NEW-TEMP_52 _NEW-TEMP_53)
                   (BEGIN
                     (SET! _FOO_50 _NEW-TEMP_52)
                     (SET! _BAR_51 _NEW-TEMP_53)
                     (_FOO_50 (_BAR_51 8))))
           (LAMBDA (|_X_52|) |_X_52|) (LAMBDA (|_X_52|) |_X_52|)))
  (IF 'NIL 'NIL) (IF 'NIL 'NIL)))

(expand-program
 '(
;;; ALL-SYMBOLS digs out all the symbols in a syntax.
   (define-syntax all-symbols
     (syntax-rules ()
       ((all-symbols (?x . ?y) ?k)
        (let-syntax ((k (syntax-rules %%%0 ()
                          ((k ?y* ?k*  (?symbol %%%0))
                           (let-syntax ((k* (syntax-rules %%%1 ()
                                                          ;; Doubly nested ellipsis: we use another
                                                          ;; distinct ellipsis token.
                                              ((k* ?k** (?symbol* %%%1))
                                               (union (?symbol  %%%0)
                                                      (?symbol* %%%1)
                                                      ?k**)))))
                             (all-symbols ?y* (k* ?k*)))))))
          (all-symbols ?x (k ?y ?k))))

       ((all-symbols #(?x ***) ?k)
        (all-symbols (?x ***) ?k))

       ((all-symbols ?x ?k)
        (syntax-symbol? ?x
                        (apply-syntactic-continuation ?k (?x))
                        (apply-syntactic-continuation ?k ())))))

   #|(all-symbols (foo 4 bar #(T (baz (NIL quux)) zot) (mumble #(frotz)))
    (quote))|#                  ; => (frotz mumble zot quux baz bar foo)

   #|(all-symbols 1 2)|#
   (all-symbols #(1) 2)
   ))


(expand-program
 '(
   (let-syntax ((foo (syntax-rules %%%0 ()
                                   ((_ a %%%0)
                                    (let-syntax ((bar (syntax-rules %%%1 ()
                                                                    ((_ b %%%1)
                                                                     (list :bar-expanded b %%%1 a %%%0)))))
                                      (bar :b1 :b2))))))
     (foo 1 2 3))
   ))
;=>  ((DEFINE _EQV?_15
;       EQV?)
;     (DEFINE _CONS_30
;       CONS)
;     (DEFINE _APPEND_31
;       APPEND)
;     (DEFINE _LIST_32
;       LIST)
;     (DEFINE _VECTOR_33
;       VECTOR)
;     (DEFINE _LIST->VECTOR_34
;       LIST->VECTOR)
;     (DEFINE _MAP_35
;       MAP)
;     (LIST :BAR-EXPANDED :B1 :B2 1 2 3))


(expand-program
 '(

;;; This example demonstrates the hygienic renaming of the ellipsis
;;; identifiers.

  (let-syntax ((f (syntax-rules ()
                  ((f ?e)
                   (let-syntax ((g (syntax-rules %%% ()
                                     ((g (??x ?e) (??y %%%))
                                      '((??x) ?e (??y) %%%)))))
                     (g (1 2) (3 4)))))))
  (f %%%))
    ; => ((1) 2 (3) (4)), if hygienic rules of ellipsis identifiers are
    ;      correctly implemented, not ((1) (2) (3) (4))


   ))
;=>  ((DEFINE _EQV?_15
;       EQV?)
;     (DEFINE _CONS_30
;       CONS)
;     (DEFINE _APPEND_31
;       APPEND)
;     (DEFINE _LIST_32
;       LIST)
;     (DEFINE _VECTOR_33
;       VECTOR)
;     (DEFINE _LIST->VECTOR_34
;       LIST->VECTOR)
;     (DEFINE _MAP_35
;       MAP)
;     '((1) 2 (3) (4)))
