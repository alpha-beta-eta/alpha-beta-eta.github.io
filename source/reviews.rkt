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
   (P "抽去一切的逻辑编程语言还剩下什么? 那就是unification了.")
   (H2 "The Little Prover")
   (P "这本书介绍了ACL2的原理, 抽去了一切不必要的自动化, "
      "只剩下了项重写和归纳法这两个核心.")
   (H2 "A Little Java, A Few Patterns")
   (P "这本书的目的只是介绍访问者模式, 别无其他. "
      "介绍访问者模式的目的只是为了在Java里写解释器之类的程序.")
   (H2 "Semantics Engineering with PLT Redex")

   (H2 "A Structural Approach to Operational Semantics")

   (H2 "Definitional Interpreters for Higher-Order Programming Languages")

   (H2 "Calculus (Apostol)")
   (P "作者在写书之前就已经想好了本书应该具有的三个特点:"
      (Ol (Li "数学基础一塌糊涂的人也应该能够读懂;")
          (Li "要使用公理化方法;")
          (Li "要从历史发展顺序出发."))
      "最后得到的就是这样一本相当奇怪的教科书.")
   
   ))