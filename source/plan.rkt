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
       (Li "Computablity and Complexity (Neil Jones)")
       (Li "序与格论基础")
       (Li "Three Implementation Models for Scheme")
       (Li "Game Engine Architecture")
       (Li "CMU 15-210"))
   ))