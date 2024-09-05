#lang racket
(provide index.html)
(require SMathML racket/date)
(define index.html
  (Prelude
   #:title "愚者"
   #:css "styles.css"
   (H1 "愚者 " (A "RSS" #:attr* '((href "rss.xml"))) " "
       (A "网站介绍" #:attr* '((href "intro.html"))) " "
       (A "有趣的链接" #:attr* '((href "ilinks.html"))) " "
       (A "书籍推荐" #:attr* '((href "books.html"))) " "
       (A "胡言乱语" #:attr* '((href "huyan.html"))))
   (P "吾生也有涯, 而知也无涯. 以有涯随无涯, 殆已.")
   (columnize
    (H2 (A "编程语言的邀请" #:attr* '((href "notes/pl.pdf"))) ": "
        (A "程序集" #:attr* '((href "notes/pl.zip"))))
    (H2 (A "SMathML" #:attr* '((href "smathml_everything.html"))))
    (H2 (A "Brzozowski导数" #:attr* '((href "brzozowski.html"))))
    (H2 (A "Linear Algebra (Hoffman &amp; Kunze)" #:attr* '((href "hoffman.html"))))
    (H2 (A "EoPL3笔记" #:attr* '((href "eopl.html"))))
    (H2 (A "Proofs and Types第一章翻译"
           #:attr* '((href "notes/prot-ch1.pdf"))))
    (H2 (A "一些程序" #:attr* '((href "some_programs.html"))))
    (H2 (A "程序设计的邀请" #:attr* '((href "programming.html"))))
    (H2 (A "指称语义学讲义翻译" #:attr*
           '((href "denotational_semantics_notes.html"))))
    (H2 (A "Curry-Howard同构讲义翻译" #:attr* '((href "curry-howard.html"))))
    (H2 (A "语义工程和PLT Redex" #:attr* '((href "sewpr.html"))))
    (H2 (A "计算机科学考古" #:attr* '((href "archaeology.html"))))
    (H2 (A "可计算性和计算复杂度" #:attr* '((href "jones.html"))))
    (H2 (A "Miscellaneous Notes" #:attr* '((href "misc.html"))))
    (H2 (A "灵活软件设计" #:attr* '((href "sdf.html"))))
    (H2 (A "证明与类型" #:attr* '((href "prot.html"))))
    (H2 (A "绘制数学图形" #:attr* '((href "illustrations.html"))))
    (H2 (A "计算机辅助设计的几何编程" #:attr* '((href "cad.html"))))
    (H2 (A "Topos: 逻辑的范畴分析" #:attr* '((href "topos.html"))))
    (H2 (A "Abel定理" #:attr* '((href "abel.html"))))
    (H2 (A "数学家学法语" #:attr* '((href "french.html"))))
    (H2 (A "分析笔记" #:attr* '((href "analysis_notes.html"))))
    (H2 (A "线性逻辑: 其句法和语义" #:attr* '((href "synsem.html"))))
    (H2 (A "格与序导论" #:attr* '((href "order.html"))))
    (H2 (A $lambda "演算: 其句法和语义" #:attr* '((href "lambda.html"))))
    (H2 (A "函数式编程的论域论基础" #:attr* '((href "fp_domain.html"))))
    (H2 (A "结构证明论" #:attr* '((href "spt.html"))))
    (H2 (A "友情链接" #:attr* '((href "links.html"))))
    (H2 (A "SICM笔记" #:attr* '((href "sicm.html"))))
    (H2 (A "Geometric Algebra (E. Artin) 翻译"
           #:attr* '((href "geometric_algebra.html"))))
    (H2 (A "函数式微分几何" #:attr* '((href "fdg.html"))))
    (H2 (A "归纳定义" #:attr* '((href "induc.html"))))
    (H2 (A "元对象协议艺术" #:attr* '((href "mop.html"))))
    (H2 (A "数据结构和算法" #:attr* '((href "dsaa.html"))))
    (H2 (A "Lem编辑器使用笔记"
           #:attr* '((href "lem_editor.html"))))
    (H2 (A "部分求值" #:attr* '((href "pe_jones.html"))))
    (H2 (A "线性代数笔记"
           #:attr* '((href "linear_algebra_notes.html"))))
    (H2 (A "范畴论笔记" #:attr* '((href "cat_awodey.html"))))
    (H2 (A "拓扑学 (Munkres) 笔记" #:attr* '((href "topology.html"))))
    (H2 (A "组合学笔记" #:attr* '((href "combinatorics_notes.html"))))
    (H2 (A "抽象Stone对偶笔记" #:attr* '((href "asd.html"))))
    (H2 (A "Stone空间翻译" #:attr* '((href "stone_spaces.html"))))
    (H2 (A "范畴论作业" #:attr* '((href "cat_homework.html"))))
    (H2 (A "格论 (Birkhoff)" #:attr* '((href "lattices.html"))))
    (H2 (A "CMU 15-122" #:attr* '((href "cmu15122.html"))))
    (H2 (A "OPLSS" #:attr* '((href "oplss.html"))))
    (H2 (A "拓扑via逻辑" #:attr* '((href "topology_via_logic.html"))))
    (H2 (A "同伦类型论" #:attr* '((href "hott.html"))))
    
    )
   ))