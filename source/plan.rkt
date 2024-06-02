#lang racket
(provide plan.html)
(require SMathML)
(define plan.html
  (TnTmPrelude
   #:title "计划"
   #:css "styles.css"
   (H1. "学习计划")
   (Ol (Li "Category Theory (Steve Awodey)")
       (Li "Introduction to Boolean Algebras")
       (Li "序与格论基础")
       (Li "Stone Spaces")
       (Li "Amann &amp; Escher")
       (Li "Measure Theory (Fremlin)")
       (Li "A Gentle Introduction to Multi-Stage Programming")
       (Li "Lecture Notes on Denotational Semantics")
       (Li "The Art of Metaobject Protocol")
       
       )
   (H1. "论文翻译计划 (包括技术报告)")
   (Ol (Li (A "The Zipper (已完成)" #:attr* '((href "zipper.html"))))
       (Li "A unified approach to global program optimization")
       (Li "Abstract interpretation: "
           "a unified lattice model for static analysis of programs "
           "by construction or approximation of fixpoints")
       (Li "Call-By-Name, Call-By-Value, and the λ-Calculus")
       (Li "Definitional interpreters for higher-order programming languages")
       (Li "The discoveries of continuations")
       
       )
   ))