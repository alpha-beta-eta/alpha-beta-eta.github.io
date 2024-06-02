#lang racket
(provide archaeology.html)
(require SMathML)
(define archaeology.html
  (Prelude
   #:title "计算机科学考古"
   #:css "styles.css"
   (H1 "计算机科学考古")
   (columnize
    (H2 (A "句法闭包" #:attr* '((href "synclo.html"))))
    (H2 (A "卫生宏技术" #:attr* '((href "macro.html"))))
    (H2 (A "Core War介绍" #:attr* '((href "corewar.html"))))
    (H2 (A "理解Maxima" #:attr* '((href "maxima.html"))))
    (H2 (A "Scheme的三种实现模型" #:attr* '((href "timp.html"))))
    (H2 (A "把东西粘在一起" #:attr* '((href "scheme_cg.html"))))
    (H2 (A "Standard ML的历史" #:attr* '((href "sml-history.html"))))
    (H2 (A "构建问题解决器" #:attr* '((href "bps.html"))))
    (H2 (A "乌龟几何" #:attr* '((href "tg.html"))))
    (H2 (A "操作语义的结构方法" #:attr* '((href "sos.html"))))
    (H2 (A "指称语义学" #:attr* '((href "ds.html"))))
    (H2 (A "LearningZIL笔记" #:attr* '((href "zil.html"))))
    (H2 (A "Computation Structures笔记"
           #:attr* '((href "computation_structures.html"))))
    
    )
   ))