#lang racket
(provide plan.html)
(require SMathML)
(define plan.html
  (Prelude
   #:title "计划"
   #:css "styles.css"
   (H1 "学习计划")
   (Ol (Li "范畴论")
       (Li "logical relations")
       (Li "构造性逻辑 (直觉主义逻辑)")
       (Li "亚结构逻辑, 包括线性逻辑")
       (Li "高阶并发分离逻辑和Iris"))
   
   ))