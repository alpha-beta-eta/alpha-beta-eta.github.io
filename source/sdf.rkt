#lang racket
(provide sdf.html)
(require SMathML)
(define sdf.html
  (TnTmPrelude
   #:title "灵活软件设计"
   #:css "styles.css"
   (H1 "灵活软件设计")
   (H2 "前言")
   (P "有时当你写着程序, 就卡住了. 或许这是因为你意识到你没能理解问题的某个方面, "
      "但更多情况下往往是因为你先前在程序设计的阶段做出了一个关于数据结构或者"
      "代码组织方式的决策, 太过局限而又很难重来.")
   (P "本书是关于维护灵活性的特定程序组织策略的大师课程. 现在我们都知道尽管声明一个"
      "固定长度的数组用于存放要被处理的数据是很容易的事情, 然而这样一个设计决定"
      "往往成为令人不快的限制, "
      )
   (H2 "自序")
   (H2 "第1章 自然和设计中的灵活性")
   
   (H2 "第2章 领域特定语言")
   (H3 "第2.1节 组合子")
   (H4 "第2.1.1小节 函数组合子")
   (CodeB "(define (compose f g)
  (lambda args
    (f (apply g args))))")
   (CodeB "(define (compose f g)
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)")
   (CodeB "(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))
(define (identity x) x)")
   (CodeB "(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)")
   (P "以下是译者擅自补充的例子, 算术平均值."
      (CodeB "(define mean
  (parallel-combine / + (lambda args (length args))))"))
   (H3 "第2.2节 正则表达式")
   (P "注记: 从本质上说, 本节只是为惯常的正则表达式设计了另一种句法.")
   
   (H3 "第2.3节 包装器")
   (P "有时我们可以通过包装现有的程序而不是重写来改变目的. "
      )
   (H4 "第2.3.1小节 特化包装器")
   
   (H4 "第2.3.2小节 实现特化器")
   
   (H2 "第3章 ")
   (H2 "第4章 模式匹配")
   (H2 "第5章 求值")
   (H2 "第6章 Layering")
   (H2 "第7章 传播")
   ))