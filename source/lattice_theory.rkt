#lang racket
(provide lattice_theory.html)
(require SMathML)
(define $. (Mo "."))
(define (∀ x P)
  (: $forall x $. P))
(define Σ $Sigma:normal)
(define Σ* (&* Σ))
(define $Sub (Mi "Sub"))
(define (&Sub G)
  (app $Sub G))
(define (powerset X)
  (app $P:script X))
(define $lfloor (Mo "&lfloor;"))
(define $rfloor (Mo "&rfloor;"))
(define (&floor x)
  (: $lfloor x $rfloor))
(define $pr (Mo "&pr;"))
(define $pre (Mo "&pre;"))
(define $<==> (Mo "&DoubleLongLeftRightArrow;"))
(define $\| (Mo "|"))
(define $NN* (&* $NN))
(define-infix*
  (&pr $pr)
  (&pre $pre)
  (&<==> $<==>)
  (&\| $\|))
(define ((answer #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "answer")))
   (B (format "解答~a." n)) " " x*))
(define (format-num section index)
  (cond ((eq? (car section) '*)
         (if index
             (format "~a" index)
             #f))
        (else
         (if index
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     index)
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     "*")))))
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
(define lattice_theory.html
  (TnTmPrelude
   #:title "序与格论基础"
   #:css "styles.css"
   (H1. "序与格论基础")
   (P "小平邦彦将抄书学习法发扬光大, 我则是用电脑抄书, 以期学会数学.")
   (P "本书的优点是细致, 缺点或许也是细致. "
      "这导致我在阅读的时候不得不仔细纠结于字面, "
      "不能想当然, 必须参考定义, 以防止其和别的文献或者主流惯例有所不同. "
      "许多时候的确是失之毫厘, 谬以千里!")
   (H2. "偏序集与格")
   (H3. "偏序集")
   ((Definition)
    "这个定义非常主流, 就不抄写了, "
    "其定义了预序, 预序集, 偏序, 偏序集. "
    "和主流不同的是, 其要求预序集和偏序集非空, "
    "之后的论述的确也默认非空性, 必须小心.")
   ((Example)
    (Ol (Li "实数集" $RR ", 有理数集" $QQ ", 整数集" $ZZ
            ", 自然数集" $NN ", 非负整数集" $NN*
            "在通常的小于等于关系下构成偏序集, "
            "其实就是通常的序关系下的意思. "
            "这本书的一个背离当前主流记号的地方是" $NN
            "是不包含零的, 而" $NN* "是加了零的.")
        (Li "如果使用" $\| "表示整除, 即" (&\| $m $n)
            "表示" $m "整除" $n ", 那么" (tu0 $NN $\|)
            "是一个偏序集.")
        (Li "定义" $RR "上的一个二元关系" $pre "如下: "
            (MB (&<==> (&pre $x $y)
                       (&<= (&floor $x)
                            (&floor $y))))
            "则" (tu0 $RR $pre) "是一个预序集, "
            "但不是偏序集.")
        (Li "对于集合" $X ", 其幂集" (powerset $X)
            "在关系" $sube "下是一个偏序集. "
            "当然了, 这个幂集的非空子集"
            "继承了自然的序关系也成为偏序集. "
            "原书要求" $X "非空是不必要的, "
            "因为" (powerset $X) "即便在" $X
            "是空集的情况下也非空.")
        (Li "设" $G "是一个群, " (&Sub $G)
            "是由其所有子群构成的集合. 那么, "
            (tu0 (&Sub $G) $sube) "是一个偏序集.")
        (Li "设" Σ* "是由所有" (&cm $0 $1)
            "字符的有限序列构成的集合, 定义"
            (&<= $a $b) "当且仅当" $a
            "是" $b "的前缀, 则"
            (tu0 Σ* $<=) "是一个偏序集.")
        (Li "设" $P "是一个偏序集 (当然已经默认非空了), "
            $X "是一个集合, 定义" $P^X "上的关系" $<= "为"
            (MB (&<==> (&<= $f $g)
                       (∀ (∈ $x $X)
                          (&<= (app $f $x)
                               (app $g $x)))))
            "那么" $<= "是一个偏序关系, 其被称为"
            $P^X "上的逐点序 (pointwise order). "
            "原文需要" $X "非空, 实则不需要.")
        (Li ""
            )
        )
    )
   ((Definition)
    "设" $P "是一个偏序集, " (&sube $A $P) "."
    (Ol (Li "如果对于任意的" (∈ $x $A) "和"
            (∈ $y $P) ", " (&<= $x $y)
            "可以推出" (∈ $y $A) ", 那么称"
            $A "为上集 (upper set).")
        (Li ""
            )
        )
    )
   ((Definition)
    "设" $P "是一个偏序集, 对于" (∈ $x $y $P)
    ", 如果" (&< $x $y) ", 且对于每个"
    (∈ $z $P) ", " (&<= $x $z $y)
    "可以推出" (&= $z $x) "或" (&= $z $y)
    ", 那么称" $y "覆盖 (cover) " $x
    ", 记作" (&pr $x $y) ".")
   
   ))