#lang racket
(provide papers_plan.html)
(require SMathML)
(define papers_plan.html
  (Prelude
   #:title "论文翻译计划"
   #:css "styles.css"
   (H1 "论文翻译计划")
   (P "许多编程语言论文 (包括技术报告) 值得阅读和欣赏, "
      "遂决定将其翻译为中文."
      (Ol (Li "The Zipper: 提出(具体)zipper的论文")
          (Li "The Discoveries of Continuations: 关于延续的历史")
          (Li "Call-by-Name, Call-by-Value and the Lambda Calculus: "
              "最早的关于CPS变换的论文")
          (Li "A Structural Approach to Operational Semantics: "
              "被认为是现代操作语义的先声")
          (Li "SCHEME: An Interpreter for Extended Lambda Calculus: "
              "最早的关于Scheme语言的设计的论文")
          (Li "Definitional Interpreters for Higher-Order Programming Languages: "
              "或许可以称为古典操作语义的总结")
          (Li "Abstracting Abstract Machines: "
              "通过抽象机器推导抽象解释")
          (Li "A Formulae-as-Types Notion of Control: "
              "探究经典逻辑的Curry-Howard对应")
          (Li "Extensible Denotational Language Specifications: "
              )
          (Li "Notions of Computation and Monads: "
              "关于monad在编程语言中的应用的原始论文")
          (Li ""
              )
          )
      )
   ))