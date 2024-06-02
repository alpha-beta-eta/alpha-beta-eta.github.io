#lang racket
(provide lazy_eager_greedy.html)
(require SMathML)
(define lazy_eager_greedy.html
  (TnTmPrelude
   #:title "寄存器分配"
   #:css "styles.css"
   (H1. "寄存器分配")
   (P "这是对于论文Register Allocation Using Lazy Saves, "
      "Eager Restores, and Greedy Shuffling的试译, "
      "其描述了一种相当有趣的寄存器分配方法.")
   (H2. "摘要" #:auto? #f)
   (P "这篇论文呈现了一个快速且高效的线性过程内寄存器分配策略, "
      "其优化了过程调用间的寄存器使用. "
      "其基于我们的观察, 即尽管不包含调用 (句法叶例程) "
      "的过程只占所有过程活动 (procedure activation) "
      "的三分之一不到, 但是实际并不进行调用 (有效叶例程) "
      "的过程却占了所有过程活动超过三分之二的部分. "
      
      )
   (H2. "引论")
   
   ))