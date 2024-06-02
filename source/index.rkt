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
   (P "这个页面上的诸多不完满之处, 都可以用句法和语义之间的gap来解释.")
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
    (H2 (A "证明与类型" #:attr* '((href "prot.html"))))
    (H2 (A "绘制数学图形" #:attr* '((href "illustrations.html"))))
    (H2 (A "计算机辅助设计的几何编程" #:attr* '((href "cad.html"))))
    (H2 (A "Topos: 逻辑的范畴分析" #:attr* '((href "topos.html"))))
    (H2 (A "Abel定理" #:attr* '((href "abel.html"))))
    (H2 (A "数学家学法语" #:attr* '((href "french.html"))))
    (H2 (A "分析笔记" #:attr* '((href "analysis_notes.html"))))
    
    (H2 (A "函数式编程的论域论基础" #:attr* '((href "fp_domain.html"))))
    (H2 (A "结构证明论" #:attr* '((href "spt.html"))))
    (H2 (A "友情链接" #:attr* '((href "links.html"))))
    (H2 (A "Geometric Algebra (E. Artin) 翻译"
           #:attr* '((href "geometric_algebra.html"))))
    (H2 (A "线性代数笔记"
           #:attr* '((href "linear_algebra_notes.html"))))
    (H2 (A "范畴论笔记" #:attr* '((href "cat_awodey.html"))))
    (H2 (A "组合学笔记" #:attr* '((href "combinatorics_notes.html"))))
    (H2 (A "抽象Stone对偶笔记" #:attr* '((href "asd.html"))))
    (H2 (A "范畴论作业" #:attr* '((href "cat_homework.html"))))
    (H2 (A "CMU 15-122" #:attr* '((href "cmu15122.html"))))
    (H2 (A "同伦类型论" #:attr* '((href "hott.html"))))
    (H2 (A "一些没用的笔记" #:attr* '((href "useless.html"))))
    (H2 (A "格论笔记" #:attr* '((href "lattice_theory_notes.html"))))
    
    )
   ))