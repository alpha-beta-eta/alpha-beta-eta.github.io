#lang racket
(provide monad.html)
(require SMathML)
(define monad.html
  (TnTmPrelude
   #:title "Schemer看monad"
   #:css "styles.css"
   (H1. "Schemer看monad")
   (P "这个教程性的讲座基于Eugenio Moggi写的"
      (Q "Notions of Computation and Monads")
      "的前四章, 他从范畴论中拿来了monad的想法, "
      "并指出了其与编程语言的联系.")
   
   (H2. "状态monad")
   
   (H2. "其他monad")
   
   ))