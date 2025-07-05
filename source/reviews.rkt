#lang racket
(provide reviews.html)
(require SMathML)
(define reviews.html
  (TmPrelude
   #:title "书评"
   #:css "styles.css"
   (H1 "书评")
   (H2 "The Little Schemer")
   
   (H2 "The Seasoned Schemer")

   (H2 "The Reasoned Schemer")

   (H2 "The Little Prover")

   (H2 "A Little Java, A Few Patterns")

   (H2 "Semantics Engineering with PLT Redex")

   (H2 "A Structural Approach to Operational Semantics")

   (H2 "Definitional Interpreters for Higher-Order Programming Languages")

   
   ))