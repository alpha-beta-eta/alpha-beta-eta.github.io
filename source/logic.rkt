#lang racket
(provide logic.html)
(require SMathML)
(define logic.html
  (TnTmPrelude
   #:title "逻辑学习指南"
   #:css "styles.css"
   (H1. "逻辑学习指南")
   (P "这是逻辑学家Peter Smith所写的逻辑学习指南, 我曾经读过相当一部分, 但不是全部. "
      "我翻译此书的目的或许是希望有更多的人能够享受逻辑学的乐趣.")
   (H2. "指南本身, 以及如何使用它")
   (H3. "本书是为谁而写的?")
   (H3. "指南的结构")
   (H3. "从逻辑书籍中自学的策略")
   (H3. "选择, 还是选择")
   (H2. "一点非形式化的集合论")
   (H3. "集合: 基本知识清单")
   (H3. "关于朴素 (naivety) 的注记")
   (H3. "非形式化的基本集合论的推荐材料")
   (P "如果你是一个数学系学生, 那么你必然已经对于我们清单上列出的想法相当熟悉了. "
      "数学书籍中经常出现的导引章节或者附录中往往会包含这些内容. 一个特别好的例子是"
      (Ol (Li "James R. Munkres, " (I "Topology")
              " (Prentice Hall, 2nd edition, 2000). Chapter 1, "
              "'Set Theory and Logic'. 这部分内容将非常清晰地告诉你基本的集合论概念, "
              "直至可数集和不可数集的对比以及选择公理 (外加其他一些值得晓得的东西)."))
      "但是非数学系的学生或者锈蚀了的数学系学生或许会发现下面的诸多书籍之一可能更符合他们的口味:"
      (Ol #:attr* '((start "2"))
          (Li "Tim Button, " (I "Set Theory: An Open Introduction")
              " (Open Logic Project), Chapters 1-5. 可在"
              (A #:attr* '((href "https://tinyurl.com/opensettheory")) "tinyurl.com/opensettheory")
              "上找到."
              )
          )
      )
   (H3. "虚拟的类, 真实的集合")
   (H2. "一阶逻辑")
   (H3. "命题逻辑")
   (H3. "FOL基础")
   (H3. "一点关于证明系统的类型的注记")
   (H3. "FOL读物的基本推荐")
   (H3. "一些并行但稍微高级一些的读物")
   (H2. "二阶逻辑, 相当扼要地")
   (H2. "模型论")
   
   ))