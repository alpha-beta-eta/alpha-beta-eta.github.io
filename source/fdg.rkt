#lang racket
(provide fdg.html)
(require SMathML)
(define fdg.html
  (TnTmPrelude
   #:title "函数式微分几何"
   #:css "styles.css"
   (H1 "函数式微分几何")
   (H2 "第1章 引入")
   (H2 "第2章 流形")
   (P "流形是对于嵌入Euclid空间的光滑曲面的概念的一般化. "
      )
   ))