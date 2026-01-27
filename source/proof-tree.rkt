#lang racket
(provide (except-out (all-defined-out) $vdash &vdash add))
(require SMathML)
(define &split8 (&split 8))
(define (&infer premise* label conclusion)
  (define infer
    (~ #:attr* '((displaystyle "true"))
       (apply &split8 premise*)
       conclusion))
  (if label
      (: infer label)
      infer))
(struct RuleInstance
  (premise* label conclusion))
(struct ProofInstance
  (presentation conclusion))
(define (tell obj msg . arg*)
  (apply (get-method obj msg) obj arg*))
(define (get-method obj msg)
  (let ((method (obj msg)))
    (if (procedure? method)
        method
        (error 'get-method "unknown message ~s" msg))))
(define $vdash (Mo "&vdash;"))
(define-infix*
  (&vdash $vdash))
(define (add x multiset equal?)
  (if (null? multiset)
      (list (cons x 1))
      (if (equal? x (caar multiset))
          (cons (cons x (+ (cdar multiset) 1))
                (cdr multiset))
          (cons (car multiset)
                (add x (cdr multiset) equal?)))))
(define (build-multiset lst equal?)
  (let iter ((rest lst) (result '()))
    (if (null? rest)
        result
        (iter (cdr rest)
              (add (car rest) result equal?)))))
(define (multiset-subset? m1 m2 equal?)
  (let iter ((rest m1))
    (cond ((null? rest) #t)
          ((assoc (caar rest) m2 equal?)
           => (lambda (a)
                (and (<= (cdar rest) (cdr a))
                     (iter (cdr rest)))))
          (else (iter (cdr rest))))))
(define (multiset-equal? m1 m2 equal?)
  (and (multiset-subset? m1 m2 equal?)
       (multiset-subset? m2 m1 equal?)))
(define (list-equal? lst1 lst2 equal?)
  (multiset-equal?
   (build-multiset lst1 equal?)
   (build-multiset lst2 equal?)
   equal?))
(define (Sequent left* right*)
  (lambda (msg)
    (case msg
      ((left*) (lambda (self) left*))
      ((right*) (lambda (self) right*))
      ((equal?) (lambda (self obj)
                  (and (eq? (tell obj 'type) 'Sequent)
                       (list-equal?
                        left* (tell obj 'left*)
                        equal?)
                       (list-equal?
                        right* (tell obj 'right*)
                        equal?))))
      ((render) (lambda (self)
                  (&vdash (apply &cm left*)
                          (apply &cm right*))))
      ((type) (lambda (self) 'Sequent))
      (else #f))))
;; (define Sequent1
;;   (Sequent (list $A $B $C)
;;            (list $D $E $F)))
;; (define Sequent2
;;   (Sequent (list $B $A $C)
;;            (list $F $E $D)))
;; (tell Sequent1 'equal? Sequent2)
;; (tell Sequent2 'equal? Sequent1)
;; (tell Sequent1 'render)
;; (tell Sequent2 'render)
(define (RuleInstance->ProofInstance ruleInstance)
  (define premise*
    (RuleInstance-premise* ruleInstance))
  (define label
    (RuleInstance-label ruleInstance))
  (define conclusion
    (RuleInstance-conclusion ruleInstance))
  (define premise*-presentation
    (map (lambda (premise)
           (tell premise 'render))
         premise*))
  (define conclusion-presentation
    (tell conclusion 'render))
  (ProofInstance
   (&infer
    premise*-presentation
    label
    conclusion-presentation)
   conclusion))
(define (check-validity proofInstance* ruleInstance)
  (let ((conclusion*
         (map ProofInstance-conclusion proofInstance*))
        (premise*
         (RuleInstance-premise* ruleInstance)))
    (unless (list-equal?
             conclusion*
             premise*
             (lambda (x y)
               (tell x 'equal? y)))
      (error 'check-validity "invalid composition!"))))
(define (ProofInstanceCompose ruleInstance . proofInstance*)
  (check-validity proofInstance* ruleInstance)
  (let ((presentation*
         (map ProofInstance-presentation proofInstance*))
        (label (RuleInstance-label ruleInstance))
        (conclusion (RuleInstance-conclusion ruleInstance)))
    (ProofInstance
     (&infer
      presentation*
      label
      (tell conclusion 'render))
     conclusion)))
(define (ProofTreeRender tree env)
  (define (lookup label env)
    (cond ((assq label env) => cdr)
          (else (error 'ProofTreeRender
                       "unknown rule ~s" label))))
  (define (interp-rule rule)
    (match rule
      ((,label . ,arg*)
       (apply (lookup label env) arg*))))
  (define (interp tree)
    (match tree
      ((<== ,rule . ,proof*)
       (apply ProofInstanceCompose
              (interp-rule rule)
              (map interp proof*)))
      ((axiom ,conclusion)
       (ProofInstance
        (tell conclusion 'render)
        conclusion))
      (,rule
       (RuleInstance->ProofInstance
        (interp-rule rule)))))
  (ProofInstance-presentation
   (interp tree)))
(define Δ $Delta:normal)
(define Δ^ (&prime Δ))
(define Δ^^ (&Prime Δ))
(define $⊗ (Mo "&otimes;"))
(define $⊗:id (Mi "&otimes;"))
(define $⊗L (: $⊗:id $L))
(define $⊗R (: $⊗:id $R))
(define-infix*
  (&⊗ $⊗))
(define (⊗L Δ A B C)
  (RuleInstance
   (list
    (Sequent (append Δ (list A B))
             (list C)))
   $⊗L
   (Sequent (append Δ (list (&⊗ A B)))
            (list C))))
(define (⊗R Δ A Δ^ B)
  (RuleInstance
   (list
    (Sequent Δ (list A))
    (Sequent Δ^ (list B)))
   $⊗R
   (Sequent (append Δ Δ^)
            (list (&⊗ A B)))))
(define $cut (Mi "cut"))
(define (cut Δ A Δ^ B)
  (RuleInstance
   (list
    (Sequent Δ (list A))
    (Sequent (append Δ^ (list A))
             (list B)))
   (_ $cut A)
   (Sequent (append Δ Δ^)
            (list B))))
(define env:linear
  `((⊗L . ,⊗L)
    (⊗R . ,⊗R)
    (cut . ,cut)
    ))
(define proof_tree_test.html
  (TmPrelude
   #:title "证明树排版测试"
   #:css "styles.css"
   (H1 "证明树排版测试")
   (CodeB "(MB (ProofTreeRender
     `(&lt;== (cut (,Δ ,Δ^)
                ,(&amp;⊗ $A $B)
                (,Δ^^)
                ,$C)
           (⊗R (,Δ) ,$A (,Δ^) ,$B)
           (⊗L (,Δ^^) ,$A ,$B ,$C))
     env:linear))")
   (MB (ProofTreeRender
        `(<== (cut (,Δ ,Δ^)
                   ,(&⊗ $A $B)
                   (,Δ^^)
                   ,$C)
              (⊗R (,Δ) ,$A (,Δ^) ,$B)
              (⊗L (,Δ^^) ,$A ,$B ,$C))
        env:linear))
   (CodeB "(MB (ProofTreeRender
     `(&lt;== (cut (,Δ^) ,$B
                (,Δ ,Δ^^) ,$C)
           (axiom
            ,(Sequent
              (list Δ^) (list $B)))
           (cut (,Δ) ,$A
                (,Δ^^ ,$B) ,$C))
     env:linear))")
   (MB (ProofTreeRender
        `(<== (cut (,Δ^) ,$B
                   (,Δ ,Δ^^) ,$C)
              (axiom
               ,(Sequent
                 (list Δ^) (list $B)))
              (cut (,Δ) ,$A
                   (,Δ^^ ,$B) ,$C))
        env:linear))
   ))
