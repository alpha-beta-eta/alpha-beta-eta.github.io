#lang racket
(provide plan.html)
(require SMathML)
(define plan.html
  (TnTmPrelude
   #:title "学习计划"
   #:css "styles.css"
   (H1. "学习计划")
   (H2. "当前进行的计划")
   (Ol (Li "Category Theory (Steve Awodey)")
       (Li "编译器构造"
           (Ol (Li "IUB P423")
               (Li "Three Implementation Models for Scheme")))
       (Li "Commutative Algebra")
       (Li "Plane Algebraic Curves")
       (Li "一般性的计算理论"
           (Ol (Li "Computablity and Complexity (Neil Jones)")
               (Li "G&ouml;del Without Tears")
               (Li "Partial Evaluation and Automatic Program Generation")))
       (Li "指称语义, 格论, domain论, 无点拓扑")
       (Li "游戏编程, 特别是状态机和行为树")
       (Li "运动")
       (Li "按时睡觉")
       (Li "好好吃饭")
       )
   (H2. "玩玩而已的计划")
   (Ol (Li "TSPL4和CSUG")
       (Li "nlab翻译计划")
       
       )
   (H2. "暂告一段落的计划")
   (Ol (Li "Hoffman &amp; Kunze")
       (Li "The Little Typer")
       
       )
   ))