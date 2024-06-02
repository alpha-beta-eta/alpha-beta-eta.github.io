#lang racket
(provide analysis_notes.html)
(require SMathML)
(define analysis_notes.html
  (TmPrelude
   #:title "分析笔记"
   #:css "styles.css"
   (H1 "分析笔记")
   (P "希望我有朝一日能学会分析, 结束自大学一年级以来的梦魇.")
   (columnize
    (H2 (A "分析一 (Amann &amp; Escher)" #:attr* '((href "analysis1_amann.html"))))
    (H2 (A "分析三 (Amann &amp; Escher)" #:attr* '((href "analysis3_amann.html"))))
    (H2 (A "复平面中的古典分析笔记" #:attr* '((href "classical_complex_analysis.html"))))
    (H2 (A "分析专著" #:attr* '((href "analysis_dieudonne.html"))))
    (H2 (A "微积分 (Apostol)" #:attr* '((href "calculus.html"))))
    (H2 (A "无穷小演算" #:attr* '((href "calculus_dieudonne.html"))))
    (H2 (A "无穷级数的理论和应用" #:attr* '((href "series.html"))))
    (H2 (A "分析学练习" #:attr* '((href "exercises_in_analysis.html"))))
    (H2 (A "函数论基础" #:attr* '((href "elements_knopp.html"))))
    
    )
   ))