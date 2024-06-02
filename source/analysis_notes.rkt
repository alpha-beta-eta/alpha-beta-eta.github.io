#lang racket
(provide analysis_notes.html)
(require SMathML)
(define analysis_notes.html
  (TmPrelude
   #:title "分析笔记"
   #:css "styles.css"
   (H1 "分析笔记")
   (P "作为数学系的学生, 我的分析和代数学的是一团糟. "
      "距本科入学算起已是过去六年了, 此时悲伤又有何用呢? "
      "或许, 真正促使我鼓起勇气重学数学的契机在于" $J_P
      "事件 (2024年6月). 尽管我从最开始就知道这是显然的造假, 但我不能做出"
      "有力的批判. 若是在非数学方面, 我还可以拿出拙于言辞的"
      "借口. 然而在数学方面, 这次事件中我的确是只能做出笼统无力的论证. "
      "一个不懂数学的数学人和死了无异, 而死了就相当于没有活过.")
   (columnize
    (H2 (A "分析一 (Amann &amp; Escher)" #:attr* '((href "analysis1_amann.html"))))
    (H2 (A "分析三 (Amann &amp; Escher)" #:attr* '((href "analysis3_amann.html"))))
    (H2 (A "复平面中的古典分析笔记" #:attr* '((href "classical_complex_analysis.html"))))
    (H2 (A "分析专著" #:attr* '((href "analysis_dieudonne.html"))))
    (H2 (A "微积分 (Apostol)" #:attr* '((href "calculus.html"))))
    (H2 (A "无穷小演算" #:attr* '((href "calculus_dieudonne.html"))))
    (H2 (A "无穷级数的理论和应用" #:attr* '((href "series.html"))))
    (H2 (A "流形上的分析" #:attr* '((href "analysis_munkres.html"))))
    
    )
   ))