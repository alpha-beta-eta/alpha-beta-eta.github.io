#lang racket
(provide cps.html)
(require SMathML)
(define $phiv (Mi "&phiv;"))
(define $force
  (Mi "force" #:attr* '((mathvariant "sans-serif"))))
(define cps.html
  (TnTmPrelude
   #:title "延续传递风格的范畴结构"
   #:css "styles.css"
   (H1. "延续传递风格的范畴结构")
   (H2. "引论")
   (H3. "编程语言中的延续简介")
   (H3. "引入延续函子")
   (P "我们首先从编程的角度对核心概念作初步阐述.")
   (P "所有重要示例都在图中给出, 并且是双语的, "
      "同时用ML和Scheme写成. 正文通常略偏向ML, "
      "主要是因为ML在给出结果的同时还会给出类型信息.")
   (P "实验所用的ML实现是Standard ML of New Jersey 0.93版. "
      "写作本文时, 还没有哪个ML实现同时支持新的"
      "值多态 (value polymorphism) 和"
      "一等续延 (first-class continuations). "
      "这就是为什么使用续延的程序中会出现弱类型变量 ("
      (Code "'1a") ", " (Code "'2b")
      ", ...) (参见 [HDM93] 和 [NJ93]). "
      "Scheme方面使用了Bigloo (v1.6) 和Gambit.")
   (P "范畴语义的一个副产品是一组所谓的"
      "范畴组合子 (categorical combinators). 以"
      $lambda "演算为例, 它在笛卡尔闭范畴 "
      "(Cartesian closed categories) "
      "中的范畴语义给出了求值映射 (evaluation map) "
      "和态射配对 (morphism pairing) 等组合子. "
      "这些组合子可以看作一种组合子逻辑, "
      "而且比Schönfinkel组合子"
      $S "和" $K "更有语义上的依据 "
      "(相关讨论见 [FH88]). 如果事先知道目标是一个伴随 "
      "(adjunction), 定义否定函子 (negation functor) "
      "其实会更容易. 伴随同构" $phi
      "和单位" $force "都很容易定义, 借助它们可得"
      (&= (&neg $f) (app $phiv (&compose $f $force)))
      ", 如图1.12和图1.13所示. 如果一步直接定义, "
      "否定函子就比较难读, 主要是因为其中嵌套了"
      (Code "callcc") ":"
      
      )
   ))