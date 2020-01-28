(cl:in-package "https://github.com/g000001/srfi-46#internals")
(in-readtable :quasiquote)

(5am:def-suite srfi-46)

(5am:in-suite srfi-46)

(define-syntax isqu
  (syntax-rules ()
    ((_ x y)
     (5am:is (equal? x y)))))

(define-syntax isquq
  (syntax-rules ()
    ((_ x y)
     (isqu x 'y))))

(define-syntax isqp
  (syntax-rules ()
    ((_ x y)
     (5am:is (cl:tree-equal x y
                            :test (lambda (a b)
                                    (cl:string-equal
                                     (cl:write-to-string a)
                                     (cl:write-to-string b))))))))

(test |Expand-Top-Level-Forms|
  (isquq (car (expand-top-level-forms '(1.23
                                        cl:t
                                        "foo"
                                        #\a )
                                      builtins-store 0 #'list))
         (1.23 COMMON-LISP:T "foo" #\a) )
  (isquq (car (expand-top-level-forms '(8 (define foo 8))
                                      builtins-store 0 #'list))
         (8 (DEFINE FOO 8)))
  (isquq (car (expand-top-level-forms '(8 (set! foo 8))
                                      builtins-store 0 #'list))
         (8 (SET! FOO 8)))
  (isqp (car (expand-top-level-forms '(8 (lambda (x) x))
                                      builtins-store 0 #'list))
        '(8 (LAMBDA (#:|_X_0|) #:|_X_0|)))
  (isquq (car (expand-top-level-forms '(8 (if a b c))
                             builtins-store 0 #'list))
         (8 (IF A B C)))
  (isquq (car (expand-top-level-forms '((quote 8))
                             builtins-store 0 #'list))
         ('8))
  (isquq (car (expand-top-level-forms '((begin z8 8))
                             builtins-store 0 #'list))
         (Z8 8))
  (isqp (car (expand-top-level-forms '((define foo (lambda (n)
                                             (begin 8))))
                             builtins-store 0 #'list))
        '((DEFINE FOO
            (LAMBDA (#:|_N_0|) 8)))))

;;; --------------------
;;; Examples of tail patterns

;;; This example of the tail pattern extension is a crippled version of
;;; R5RS's BEGIN special form.  (It is crippled because it does not
;;; support internal definitions or commands within its body returning
;;; fewer or more than one value.)

(define-syntax fake-begin
  (syntax-rules ()
    ((fake-begin ?body *** ?tail)
     (let* ((ignored ?body) ***) ?tail))))

(test |Examples of tail patterns|
  (isqu (cl:with-output-to-string (out)
          (fake-begin
           (DISPLAY "Hello," out)
           (WRITE-CHAR #\SPACE out)
           (DISPLAY "world!" out)
           (cl:terpri out)))
        "Hello, world!
"))


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

(define-syntax test-letrec
  (syntax-rules ()
    ((letrec ((?var ?init) ***) ?body1 ?body2 ***)
     (let-syntax ((k (syntax-rules %%% ()
                       ;; Use the same trick as with the continuations in
                       ;; GENERATE-TEMPORARIES.  Be careful about the ellipsis!
                       ((k ((?var* ?init*) %%%)
                           (?body1* ?body2* %%%)
                           ;; Here are the actual arguments to the continuation
                           ;; -- the previous bits of the pattern were just the
                           ;; 'environment' of the continuation --:
                           (?temp %%%))
                        (let ((?var* '#:|unspecific|) ; Get an 'unspecific' value.
                              %%%)
                          (let ((?temp ?init*) %%%)
                            (set! ?var* ?temp) %%%
                            (let () ?body1* ?body2* %%%)))))))
       (generate-temporaries (?var ***)
         ;; Pass K the environment.  GENERATE-TEMPORARIES will add the
         ;; temporary variable list argument.
                             (k ((?var ?init) ***) (?body1 ?body2 ***)))))))


(test |Examples of the user-specified ellipsis token extension|
  (isqu (test-letrec ((fib (lambda (n)
                             (if (< n 2)
                                 (funcall fib1 n)
                                 (+ (funcall fib (- n 1))
                                    (funcall fib (- n 2))))))
                      (fib1 (lambda (x) x)))
                     (funcall fib 10))
        55))

(test |Let-Syntax|
  (isquq (let-syntax
          ((foo (syntax-rules %%%0
                  ()
                  ((_ a %%%0)
                   (let-syntax
                               ((bar (syntax-rules %%%1
                                       ()
                                       ((_ b %%%1)
                                        (list :bar-expanded b %%%1 a %%%0)))))
                     (bar :b1 :b2))))))
          (foo 1 2 3))
        (:BAR-EXPANDED :B1 :B2 1 2 3)))

(test |LetRec-Syntax|
  (isquq (letrec-syntax
          ((foo (syntax-rules %%%0
                  ()
                  ((_ a %%%0)
                   (letrec-syntax
                               ((bar (syntax-rules %%%1
                                       ()
                                       ((_ b %%%1)
                                        (list :bar-expanded b %%%1 a %%%0)))))
                     (bar :b1 :b2))))))
          (foo 1 2 3))
        (:BAR-EXPANDED :B1 :B2 1 2 3)))


;;; This example demonstrates the hygienic renaming of the ellipsis
;;; identifiers.

(test |Hygienic renaming of the ellipsis identifiers|
  (isquq (let-syntax ((f (syntax-rules ()
                  ((f ?e)
                   (let-syntax ((g (syntax-rules %%% ()
                                     ((g (??x ?e) (??y %%%))
                                      '((??x) ?e (??y) %%%) ))))
                     (g (1 2) (3 4)) )))))
           (f %%%) )
         ((1) 2 (3) (4))) )

;;; eof
