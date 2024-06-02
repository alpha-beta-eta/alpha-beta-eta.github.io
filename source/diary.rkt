#lang racket
(provide diary.html)
(require SMathML)
(define diary.html
  (TnTmPrelude
   #:title "无聊的日记"
   #:css "styles.css"
   (H1. "无聊的日记")
   (H2. "2026年7月28日")
   (P "今日回看之前写的生命游戏实现:"
      (CodeB "(define conway-kernel
  (&lt;&lt; (Array #(1 1 1 1 0 1 1 1 1))
      (rho2 3 3)))
(define (neighbor grid)
  (&lt;&lt; conway-kernel
      (Convolve2 grid '(1 1))))
(define (step grid)
  (Materialize2
   ((Zip-with
     (λ (x n)
       (if (= x 1)
           (if (or (= n 2) (= n 3)) 1 0)
           (if (= n 3) 1 0))))
    grid (neighbor grid))))")
      "参考APL的通常写法改了一个版本:"
      (CodeB "(define kernel
  (array '(3 3) (const 1)))
(define (sum9 grid)
  (&lt;&lt; kernel (Convolve2 grid '(1 1))))
(define (step^ grid)
  (Materialize2
   ((Zip-with
     (λ (x n)
       (if (or (= n 3)
               (and (= x 1) (= n 4)))
           1 0)))
    grid (sum9 grid))))")
      "根据测试, 新的版本比旧的版本用时平均长" (&/ $1 $8)
      ", 这个差距大概是由于原本的卷积核里有一个零, "
      "现在的卷积核里没有零而导致的. "
      "其他的细节几乎没有产生速度上的差异. "
      "不过, 就字数而言, 新版的确比旧版更少. "
      "然而, 新版也比旧版更难理解. "
      "大概新的版本只有热爱节约字数的APL人才会喜欢就是了.")
   
   ))