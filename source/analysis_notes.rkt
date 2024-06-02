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
    (TITLE "分析一 (Amann &amp; Escher)" "analysis1_amann.html")
    (TITLE "分析三 (Amann &amp; Escher)" "analysis3_amann.html")
    (TITLE "复平面中的古典分析笔记" "classical_complex_analysis.html")
    (TITLE "分析专著" "analysis_dieudonne.html")
    (TITLE "微积分 (Apostol)" "calculus.html")
    (TITLE "无穷小演算" "calculus_dieudonne.html")
    (TITLE "无穷级数的理论和应用" "series.html")
    (TITLE "分析学练习" "exercises_in_analysis.html")
    (TITLE "函数论基础" "elements_knopp.html")
    (TITLE "流形引论" "manifolds.html")
    
    )))