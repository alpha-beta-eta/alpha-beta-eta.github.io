#lang racket
(provide pointless.html)
(require SMathML)
(define pointless.html
  (TmPrelude
   #:title "无点拓扑学"
   #:css "styles.css"
   (H1 "无点拓扑学")
   (H2 "前言")
   (P "大致上说, 无点拓扑学基于以下事实: 抽象的开集格包含大量关于拓扑空间的信息, "
      "并且代数式的处理可以提供对于空间本质的新洞察. 这种方法背后的直觉是相当自然的: "
      "我们将空间想成是由非平凡的" (Q "实际地方") "构成, 并且装备有它们如何相交以及"
      "更大的地方是如何由更小的地方组合而成的信息.")
   
   ))