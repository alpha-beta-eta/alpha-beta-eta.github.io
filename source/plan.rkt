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
   (H1. "阅读材料备忘表")
   (Ol (Li "Computability and Complexity (Neil Jones)")
       (Li "Algorithmic Information Theory (Chaitin)")
       (Li "Lectures Notes on Constructive Logic (Frank Pfenning)")
       (Li "Lectures Notes on Substructural Logics (Frank Pfenning)")
       (Li "Basic Simple Type Theory")
       (Li "Types and Programming Languages")
       (Li "Advanced Topics in Types and Programming Languages")
       (Li "Practical Foundations for Programming Languages")
       (Li "Software Foundations")
       (Li "Programming Language Foundations in Agda")
       (Li "Theories of Programming Languages (John Reynolds)")
       (Li "Control structures in programming languages: from goto to algebraic effects")
       (Li "Stone Spaces (Johnstone)")
       (Li "序与格论基础")
       )
   ))