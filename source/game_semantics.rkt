#lang racket
(provide game_semantics.html)
(require SMathML)
(define game_semantics.html
  (TnTmPrelude
   #:title "游戏语义"
   #:css "styles.css"
   (H1. "游戏语义")
   (P "游戏语义是编程语言的一种指称语义, "
      "其将项解释为自身与上下文之间的交互. "
      
      )
   (H2. "引论")
   (P "游戏语义 (GS) 是数学地描述编程语言 (PL) 的行为的一种方式; "
      "其是一种所谓的" (Em "指称语义") " (DS). "
      "一种理解指称语义的方法是将其与" (Em "操作语义")
      " (OS) 进行对比, 操作语义是另一种对于编程语言的数学描述.")
   (P "操作语义是句法性的, 也是自足的. "
      "其通过一集具有形式"
      (MB (&-> (&cm $t $c) (&cm $t^ $c^)))
      "的转换规则来定义编程语言, 其中" (&cm $t $t^)
      "是项而" (&cm $c $c^) "是额外的配置信息.")
   (P ""
      )
   (H2. "游戏语义, 一种交互语义")
   
   (H2. "理想化的并发Algol")

   (H2. "trace semantics, 另一种交互语义")
   
   ))