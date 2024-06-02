#lang racket
(provide useless.html)
(require SMathML)
(define useless.html
  (Prelude
   #:title "一些没用的笔记"
   #:css "styles.css"
   (H1 "一些没用的笔记")
   (P "之所以没用, 是因为还没有什么内容.")
   (columnize
    (H2 (A "格论 (Birkhoff)" #:attr* '((href "lattices.html"))))
    (H2 (A "Stone空间翻译" #:attr* '((href "stone_spaces.html"))))
    (H2 (A "OPLSS" #:attr* '((href "oplss.html"))))
    (H2 (A "归纳定义" #:attr* '((href "induc.html"))))
    (H2 (A "元对象协议艺术" #:attr* '((href "mop.html"))))
    (H2 (A "Lem编辑器使用笔记" #:attr* '((href "lem_editor.html"))))
    (H2 (A "部分求值" #:attr* '((href "pe_jones.html"))))
    (H2 (A "拓扑学 (Munkres) 笔记" #:attr* '((href "topology.html"))))
    (H2 (A "SICM笔记" #:attr* '((href "sicm.html"))))
    (H2 (A "拓扑via逻辑" #:attr* '((href "topology_via_logic.html"))))
    (H2 (A "数据结构和算法" #:attr* '((href "dsaa.html"))))
    (H2 (A "线性逻辑: 其句法和语义" #:attr* '((href "synsem.html"))))
    (H2 (A $lambda "演算: 其句法和语义" #:attr* '((href "lambda.html"))))
    (H2 (A "灵活软件设计" #:attr* '((href "sdf.html"))))
    (H2 (A "函数式微分几何" #:attr* '((href "fdg.html"))))
    (H2 (A "盲点" #:attr* '((href "blind.html"))))
    (H2 (A "高阶范畴逻辑导论" #:attr* '((href "hocl.html"))))
    
    )
   ))