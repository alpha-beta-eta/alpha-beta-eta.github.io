#lang racket
(provide topology.html)
(require SMathML)
(define topology.html
  (TmPrelude
   #:title "拓扑学 (Munkres) 笔记"
   #:css "styles.css"
   (H1 "拓扑学 (Munkres) 笔记")
   (H2 "第1章 集合论和逻辑学")
   (H2 "第2章 拓扑空间和连续函数")
   (H3 "第12节 拓扑空间")
   ((definition)
    "一个集合" $X "上的一个拓扑是一个具有以下性质的"
    $X "的子集族" $T:script ":"
    (Ol (Li (∈ $empty $X $T:script) ".")
        (Li $T:script "的任意子族之并仍然在" $T:script "之中.")
        (Li $T:script "的有限子族之交仍然在" $T:script "之中."))
    "装备了拓扑的集合被称为拓扑空间.")
   
   (H2 "第3章 连通性和紧性")
   (H2 "第4章 可数性和可分性公理")
   
   ))