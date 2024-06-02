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
       (Li "IUB P423")
       (Li "Commutative Algebra")
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