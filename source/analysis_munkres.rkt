#lang racket
(provide analysis_munkres.html)
(require SMathML)
(define (dd f a u)
  (app (&prime f) (&\; a u)))
(define analysis_munkres.html
  (TmPrelude
   #:title "流形上的分析"
   #:css "styles.css"
   (H1 "流形上的分析")
   (H2 "第2章 微分")
   (H3 "第5节 导数")
   (P "先来回顾单实变元的实值函数的导数的定义. 令" $A "是" $RR
      "的一个子集, 考虑函数" (func $phi $A $RR) ". 对于"
      (∈ $a $A) ", 若" $A "包含" $a "的一个邻域, 那么" $phi
      "在点" $a "的导数被定义为"
      (MB (&= (app $phi^ $a)
              (lim $t $0
                   (~ (&- (app $phi (&+ $a $t))
                          (app $phi $a))
                      $t))))
      "在这个极限存在的情况下, 我们称" $phi "在点" $a "是可微/可导的.")
   (P "那么, 若" $f "是一个从" $RR^m "的某个子集到" $RR^n
      "的函数时, 该如何合理地推广导数的定义呢?")
   (P "这是第一个尝试. 令" $A "是" $RR^m "的一个子集, 而函数"
      (func $f $A $RR^n) ". 对于" (∈ $a $A) ", 假设" $A "包含" $a
      "的一个邻域, 考虑" (B "非零") "向量" (∈ $u $RR^m)
      ", 那么" $f "在点" $a "沿着方向" $u "的导数被定义为"
      (MB (&= (dd $f $a $u)
              (lim $t $0
                   (~ (&- (app $f (&+ $a (&i* $t $u)))
                          (app $f $a))
                      $t))))
      "这个定义的确是有用的, 然而并没有勾勒出&quot;导数就是线性近似&quot;的完整神韵.")
   
   ))