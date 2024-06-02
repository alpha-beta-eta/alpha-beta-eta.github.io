#lang racket
(provide plan.html)
(require SMathML)
(define plan.html
  (TnTmPrelude
   #:title "学习计划"
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
   ))