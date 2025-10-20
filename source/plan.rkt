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
       (Li "Schemer的monad之见")
       (Li "同伦类型论")
       )
   ))