#lang racket
(provide fp_domain.html)
(require SMathML)
(define $dArr (Mo "&dArr;"))
(define &dArr (make-infix $dArr '&dArr))
(define comb:Y (_ $Y:sans-serif $sigma))
(define (fix M) (app comb:Y M))
(define const:zero (Mi "zero" #:attr* '((mathvariant "sans-serif"))))
(define op:succ (Mi "succ" #:attr* '((mathvariant "sans-serif"))))
(define (&succ M) (app op:succ M))
(define op:pred (Mi "pred" #:attr* '((mathvariant "sans-serif"))))
(define (&pred M) (app op:pred M))
(define op:ifz (Mi "ifz" #:attr* '((mathvariant "sans-serif"))))
(define (&ifz A B C) (appl op:ifz A B C))
(define t:nat
  (Mi "nat" #:attr* '((mathvariant "bold"))))
(define $: (Mo ":" #:attr* '((lspace "0") (rspace "0"))))
(define &: (make-infix $: '&:))
(define $-> (Mo "&rarr;" #:attr* '((lspace "0") (rspace "0"))))
(define &-> (make-infix $-> '&->))
(define @-> (@lize &->))
(define (&label x . t*)
  (Table #:attr* '((align "center"))
         (Tr (Td #:attr* '((align "center")) x))
         (Tr (keyword-apply
              Td '(#:attr*) '(((align "center")))
              t*))))
(define (G!- #:env [env $Gamma:normal] . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&entailL (apply &cm env b*) (car x*))))
(define (!- . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&entailL (apply &cm b*) (car x*))))
(define $. (Mo "." #:attr* '((lspace "0") (rspace "0"))))
(define (tlam x t M)
  (: $lambda (&: x t) $. M))
(define @tlam (@lize tlam))
(define fp_domain.html
  (TnTmPrelude
   #:title "函数式编程的论域论基础"
   #:css "styles.css"
   (H1. "函数式编程的论域论基础")
   (H2. "引论")
   (H2. "PCF及其操作语义")
   (P "本章我们引入原型函数式编程语言PCF及其操作语义.")
   (P "语言PCF是一个类型化的语言, 其类型集合" (M "Type") "被归纳定义如下"
      (Ul (Li "基本类型" t:nat "是一个类型, 并且")
          (Li "每当" $sigma "和" $tau "是类型, 那么"
              (@-> $sigma $tau) "也是一个类型."))
      "我们经常将基本类型" t:nat "记作" $iota ", 将" (@-> $sigma $tau)
      "记作" (&-> $sigma $tau) ", 其中" $-> "被理解为" (M "Type")
      "上的一个右结合的二元运算, 例如" (&-> $sigma_1 $sigma_2 $sigma_3)
      "代表" (&-> $sigma_1 (@-> $sigma_2 $sigma_3)) ". "
      )
   (&label
    (set-attr*
     (&Table
      ((&rule $ (G!- (&: $x $sigma) $Delta:normal (&: $x $sigma)))
       (&rule (G!- (&: $x $sigma) (&: $M $tau))
              (G!- (&: (@tlam $x $sigma $M) (&-> $sigma $tau)))))
      ((&rule (G!- (&: $M (&-> $sigma $tau)))
              (G!- (&: $N $sigma))
              (G!- (&: (app $M $N) $tau)))
       (&rule (G!- (&: $M (&-> $sigma $sigma)))
              (G!- (&: (fix $M) $sigma))))
      ((&rule $ (G!- (&: const:zero t:nat)))
       (&rule (G!- (&: $M t:nat))
              (G!- (&: (&succ $M) t:nat))))
      ((&rule (G!- (&: $M t:nat))
              (G!- (&: (&pred $M) t:nat)))
       (&rule (G!- (&cm (&: $M_i t:nat) (&= $i (&cm $1 $2 $3))))
              (G!- (&: (&ifz $M_1 $M_2 $M_3) t:nat)))))
     'columnalign "left" 'columnspacing "20.0ex" 'rowspacing "10.0ex")
    "图2.1 PCF的定型规则")
   
   (H2. "PCF的Scott模型")
   (P "本章我们引入一种结构, 其中Dana Scott解释了PCF语言 (及其逻辑LCF). "
      "(See Scott [1969] for a reprint of a widely circulated "
      (Q "underground") " paper from 1969 where this interpretation "
      "was presented for the first time.) "
      "但是在此之前, 我们将会讨论PCF的指称语义的一般形式并"
      "试图找出我们所施加的结构要求的动机.")
   (P "PCF的一个指称语义联系每个类型" $sigma "以一个所谓的"
      (Em "domain") " " $D_sigma ", 而每个项"
      
      )
   (H3. "基本的domain论")
   (H3. "PCF的domain模型")
   (H3. "LCF&mdash;&mdash;可计算函数逻辑")
   (H2. "Computational Adequacy")
   (H2. "Milner的上下文引理")
   (H2. "完全抽象问题")
   (H2. "逻辑关系 (Logical Relations)")
   (H2. $D_sigma "的一些结构性质")
   (H2. "递归domain方程的解")
   (H2. "完全抽象模型的刻画")
   (H2. "作为PCF模型的顺序domain")
   
   ))