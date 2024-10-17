#lang racket
(provide exercises_in_analysis.html)
(require SMathML)
(define tuple tu0)
(define $Hat (Mo "&Hat;"))
(define (Hat x) (^^ x $Hat))
(define exercises_in_analysis.html
  (TmPrelude
   #:title "分析学练习"
   #:css "styles.css"
   (H1 "分析学练习")
   (P "这本书看上去内容符合我的期望, 我想我或许可以通过这本书学会分析.")
   (P "这里不是对于本书的翻译, 只是一些笔记, 但是不排除我会翻译其中一些内容.")
   (H2 "第1章 度量空间")
   (H3 "第1.1节 引论")
   (H4 "第1.1.1小节 基本定义和记号")
   (P "唉, 又是标号狂魔.")
   ((definition #:n "1.1")
    "度量空间的定义而已, 没必要写了.")
   ((remark #:n "1.2")
    "若是修改度量空间的定义, 允许互异的元素之间的距离为" $0
    ", 那么我们就得到了伪度量空间, 或者说半度量空间, 此时的距离函数被称为"
    "伪度量, 半度量, 或者ecart (这应该是法语).")
   ((example #:n "1.3")
    "举了不少不那么平凡的例子, 之后再抄吧.")
   ((proposition #:n "1.4")
    "如果" (tuple $X $d_X) "是一个度量空间, 并且"
    (MB (&= (appl (_ (Hat $d) $X) $x $y)
            (~ (appl $d_X $x $y)
               (&+ $1 (appl $d_X $x $y)))))
    "那么" (tuple $X (_ (Hat $d) $X)) "也是一个度量空间.")
   ((remark #:n "1.5")
    "注意到"
    (MB (&< (appl (_ (Hat $d) $X) $x $y) $1))
    
    )
   (H2 "第2章 拓扑空间")
   
   (H2 "第3章 测度, 积分和鞅")
   
   (H2 "第4章 测度和拓扑")
   
   (H2 "第5章 泛函分析")
   
   ))