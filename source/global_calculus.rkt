#lang racket
(provide global_calculus.html)
(require SMathML)
(define global_calculus.html
  (TnTmPrelude
   #:title "全局微积分"
   #:css "styles.css"
   (H1. "全局微积分")
   (H2. "层和微分流形: 定义和例子")
   (P "在几何学和物理学中, "
      "人们经常需要在拓扑空间上使用微分和积分演算的工具, "
      "这些空间局部上类似于Euclid空间" $RR^n
      "的开子集, 但不容许存在处处有效的坐标. 例如, 球面"
      (setI (∈ (tu0 $x $y $z) $RR^3)
            (&= (&+ $x^2 $y^2 $z^2) $1))
      ", 或者更一般地, 空间"
      (setI (∈ (tu0 $x_1 $..h $x_n) $RR^n)
            (&= (sum (_^ $x $i $2)) $1))
      "显然具有几何趣味. "
      "另一方面, 受约束的运动与" $RR^3
      "中的曲面上的动力学有关. "
      "在广义相对论中, 人们研究"
      
      )
   (H2. "微分算子")
   (H2. "微分流形上的积分")
   (H2. "层上同调及其应用")
   
   ))