#lang racket
(provide manifolds_and_geometry.html)
(require SMathML)
(define (family A i I)
  (_ (setE (_ A i)) (∈ i I)))
(define (format-num section index)
  (cond ((and section index)
         (format "~a.~a"
                 (apply string-append
                        (add-between
                         (map number->string
                              (cdr (reverse section))) "."))
                 index))
        (section
         (format "~a.~a"
                 (apply string-append
                        (add-between
                         (map number->string
                              (cdr (reverse section))) "."))
                 "*"))
        (index (format "~a" index))
        (else #f)))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
(define (Entry name class)
  (define (present %entry attr* . html*)
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define head (format-head name section index))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define id (%entry-id %entry))
    (define href (string-append "#" id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define num (format-num section index))
    (if num
        (Cite `(a ((href ,href)) ,name ,num))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite
                          #:class class)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Theorem "定理" "theorem")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise")
  (Remark "评注" "remark")
  (Corollary "推论" "corollary"))
(define manifolds_and_geometry.html
  (TnTmPrelude
   #:title "流形与几何笔记"
   #:css "styles.css"
   (H1. "流形与几何笔记")
   (H2. "微分流形")
   (H3. "流形的定义和例子")
   ((Example)
    
    )
   ((Example)
    
    )
   (P "具有可数拓扑基的拓扑空间称为" $A_2
      "的, 具有Hausdorff性质的拓扑空间称为"
      $T_2 "的.")
   ((Definition)
    "设" $M "是具有" $A_2 "和" $T_2
    "性质的拓扑空间. 如果存在" $M "的开覆盖"
    (family $U $alpha $Gamma) "以及相应的连续映射族"
    (func $phi_alpha $U_alpha $RR^n) "满足"
    (Ol #:attr* '((type "i"))
        (Li ""
            )
        )
    )
   (H2. "流形上的微积分")
   
   ))