#lang racket
(provide modal_types.html)
(require SMathML)
(define $□ (Mo "&EmptySmallSquare;"))
(define λ□ (^ $lambda $□))
(define modal_types.html
  (TnTmPrelude
   #:title "模态类型作为运行时代码生成的阶段化描述"
   #:css "styles.css"
   (H1. "模态类型作为运行时代码生成的阶段化描述")
   (H2. "运行时代码生成")
   (P "提高计算机程序的性能的一个众所周知的技术在于将计算划分为不同的阶段, "
      "以使得早期计算的结果可以被之后的计算充分运用. "
      "这种技术的一个特定实现是安排编译后的代码在运行时生成特化的代码. "
      "这种方法被称为运行时代码生成, "
      "甚至允许低层次的代码优化利用在运行时之前尚不可知的值. "
      "之前的工作表明了这种方法在许多场合下的效能.")
   
   (H2. "一个模态lambda演算")
   (P "我们将简要介绍" λ□ "语言"
      )
   ))