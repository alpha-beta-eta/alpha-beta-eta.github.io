#lang racket
(provide (all-defined-out))
(require SMathML)
(define pad
  (case-lambda
    ((x) (pad x "2em"))
    ((x length)
     (: (Mspace #:attr* `((width ,length)))
        x
        (Mspace #:attr* `((width ,length)))))))
(define $op (Mi "op"))
(define (&op C)
  (^ C $op))
(define (Arrow f A B)
  (&: f (&-> A B)))
(define (ARROW . arg*)
  (apply : (map-toggle
            #f (curry ^^ $->)
            arg*)))
(define (Functor F C D)
  (&: F (&-> C D)))
(define $Hom (Mi "Hom"))
(define Hom
  (case-lambda
    ((X Y) (appl $Hom X Y))
    ((C X Y) (appl (_ $Hom C) X Y))))
(define $Aut (Mi "Aut"))
(define Aut
  (case-lambda
    ((X) (app $Aut X))
    ((C X) (app (_ $Aut C) X))))
(define-syntax-rule (define-simple* (&id $id str) ...)
  (begin
    (define $id (Mi str))
    ...
    (define (&id x) (app $id x))
    ...))
(define-simple*
  (&dom $dom "dom")
  (&cod $cod "cod")
  (&range $range "range"))
(define-@lized-op*
  (@op &op))
(define (Cat name)
  (Mi name #:attr* '((mathvariant "sans-serif"))))
(define-syntax-rule (define-Cat* (id str) ...)
  (begin
    (define id (Cat str))
    ...))
(define (make-cat str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define-syntax-rule (define-cat* (id str) ...)
  (begin
    (define id (make-cat str))
    ...))
(define (RcdType . x*)
  (apply setE (map2 &: x*)))
(define (AbsRcdType l T i n)
  (setE (^ (&: (_ l i) (_ T i))
           (∈ i (&cm $1 $..h n)))))
(define (Record . x*)
  (apply setE (map2 &= x*)))
(define (RLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define (Lam x t)
  (: $lambda x $. t))
(define &split2 (&split 2))
(define &split16 (&split 16))
(define App &split2)
(define card &abs)
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (let ((b (car b*)))
      (: (apply &cm a*) $vdash b))))
(define (G!- . x*)
  (apply !- Γ x*))
