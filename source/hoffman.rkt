#lang racket
(provide hoffman.html)
(require SMathML)
(define hoffman.html
  (TmPrelude
   #:title "Linear Algebra (Hoffman &amp; Kunze)"
   #:css "styles.css"
   (H1 "Linear Algebra (Hoffman &amp; Kunze)")
   (P "Linear Algebra (Hoffman &amp; Kunze) 是一本经典的线性代数入门教材.")
   (H2 (A "Linear Algebra (Hoffman &amp; Kunze) 翻译 (单页版本)"
          #:attr* '((href "linear_algebra.html"))))
   (H2 (A "Linear Algebra (Hoffman &amp; Kunze) 习题作答"
          #:attr* '((href "linear_algebra_exercises.html"))))
   (H2 (A "Linear Algebra (Hoffman &amp; Kunze) 翻译 (按章划分版本)"
          #:attr* '((href "linear_algebra_contents.html"))))
   
   ))