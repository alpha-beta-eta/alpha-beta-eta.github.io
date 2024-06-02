#lang racket
(provide sicm.html)
(require SMathML)
(define (Center . html*)
  `(div ((style "text-align: center;")) . ,html*))
(define sicm.html
  (TnTmPrelude
   #:title "古典力学的结构和解释"
   #:css "styles.css"
   (H1. "古典力学的结构和解释")
   (H2 "题献")
   (Center
    "怀着崇高的敬意" (Br)
    "本书献给" (Br)
    "最小作用量原理")
   (H2 "前言")
   (P "近年来对于古典力学的兴趣有着显著的复兴. "
      "现在我们知道古典力学的内容比我们之前所设想的还要多得多. "
      "古典系统的行为令人惊异地丰富; 力学传统呈现的焦点, "
      "即运动方程的推导, 只是开始而已. 古典系统呈现了一系列复杂的现象, "
      "例如非线性共振, 混沌行为, 和混沌演变 (transition to chaos).")
   (H2 "致谢")
   (H2. "Lagrange力学")
   (P "本书的主题是运动以及描述运动的数学工具.")
   (P "千百年来对于行星运动的仔细观察揭示了这种运动的规律, "
      "其允许我们精确预测诸如食 (eclipse) 与合 (conjunction) 这样的现象. "
      "刻画并最终理解这些规律的努力导致了数学的发展, "
      "并使得人们发现数学可以用来有效地描述物质世界的诸多方面. "
      "数学可以用来描述自然现象的确是一个并非平凡的事实.")
   (H3. "配置空间")
   (H3. "广义坐标")
   (H3. "稳定作用量原理")
   (H3. "计算作用量")
   (H3. "Euler-Lagrange方程")
   (H3. "如何寻找Lagrange量")
   ))