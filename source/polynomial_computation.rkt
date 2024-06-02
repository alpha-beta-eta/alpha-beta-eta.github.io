#lang racket
(provide polynomial_computation.html)
(require SMathML)
(define polynomial_computation.html
  (TnTmPrelude
   #:title "高效多项式计算"
   #:css "styles.css"
   (H1. "高效多项式计算")
   (P "本书是Richard Zippel所著的一本关于计算机代数和多项式计算的书籍. "
      "作者发明了一种多项式GCD的概率算法, 其被广泛应用于各种计算机代数系统之中. "
      "我完全是因为Gerald Sussman和其所著的SICP以及SICM才得知Zippel和此书.")
   (H2. "Euclid算法")
   (H3. "Euclid算法")
   (H3. "Diophantine近似")
   (H3. "连分式")
   (H3. "Diophantine方程")
   (H2. "连分式")
   (H2. "Diophantine方程")
   (H2. "格技巧")
   
   ))