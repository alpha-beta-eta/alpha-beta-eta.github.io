#lang racket
(provide smathml_comments.html)
(require SMathML)
(define smathml_comments.html
  (TmPrelude
   #:title "SMathML注记"
   #:css "styles.css"
   (H1 "SMathML注记")
   (P "这里主要是为了记录MathML和SMathML微妙的地方, "
      "以及当前的SMathML有哪些地方需要修改或补充. "
      "这里只是草稿而已, 或许会有些混乱, 而且也不一定正确.")
   (H2 "关于基于数据类型的可扩展渲染")
   (P "在某种意义上来说, 提供给用户自行扩展的功能总是好的, 因此"
      "SMathML设计了一种相当一般的变换机制, 即过程" (Code "T")
      ". 并且, 基于过程" (Code "T") ", 还提供了一个用于数学内容渲染的过程"
      (Code "Tm") ". " (Code "Tm") "会对于整数和分数以及符号进行额外的"
      "变换, 但这是硬编码的, 或许" (Code "Tm") "乃至其他什么部分应该给用户"
      "自行设计渲染方式的空间.")
   (P "问题主要在于自行设计的这些部分该如何与SMathML的其他部分进行交互. "
      "最妥当的方式当然是强制要求不能产生任何交互, 必须从头开始编写, "
      "但这的确不方便, 且没有必要. 那么, 或许这种机制需要像" (Code "T")
      "一样让用户有选择是否交互的余地. 但是, 似乎把事情做得漂亮相当困难, "
      "所以我卡在这里了. 当我想明白的时候, 我会将其加入SMathML之中.")
   (H2 "实验性特性")
   (P "这里记录一些编写博客的时候创造的抽象.")
   (CodeB "(define $Hom (Mi &quot;Hom&quot;))
(define Hom
  (case-lambda
    ((A B) (appl $Hom A B))
    ((C A B) (appl (_ $Hom C) A B))))
(define $Aut (Mi &quot;Aut&quot;))
(define Aut
  (case-lambda
    ((A) (app $Aut A))
    ((C A) (app (_ $Aut C) A))))")
   
   ))