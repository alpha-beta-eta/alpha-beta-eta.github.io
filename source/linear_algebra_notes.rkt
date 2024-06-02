#lang racket
(provide linear_algebra_notes.html)
(require SMathML)
(define linear_algebra_notes.html
  (Prelude
   #:title "线性代数笔记"
   #:css "styles.css"
   (H1 "线性代数笔记")
   (columnize
    (H2 (A "Linear Algebra (Hoffman &amp; Kunze)" #:attr* '((href "hoffman.html"))))
    (H2 (A "线性代数问题书" #:attr* '((href "lapb.html"))))
    (H2 (A "有限维向量空间" #:attr* '((href "fdvs.html"))))
    
    )))