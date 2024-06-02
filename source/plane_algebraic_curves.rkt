#lang racket
(provide plane_algebraic_curves.html)
(require SMathML)
(define plane_algebraic_curves.html
  (TnTmPrelude
   #:title "平面代数曲线"
   #:css "styles.css"
   (H1. "平面代数曲线")
   (H2. "引论")
   (P "这些笔记意图作为对于" (Em "代数几何")
      "的温和介绍, 结合了" (Em "线性代数")
      "和" (Em "代数") "的内容:"
      (Ol #:attr* '((type "a"))
          (Li "在线性代数中, 我们研究了一个固定的基域"
              $K "上的多变元线性方程组.")
          (Li "在代数中, 一个中心主题是" $K
              "上的单变元多项式."))
      
      )
   (H2. "仿射曲线")
   
   ))