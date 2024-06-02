#lang racket
(provide synclo.html)
(require SMathML)
(define synclo.html
  (TnTmPrelude
   #:title "句法闭包"
   #:css "styles.css"
   (H1 "句法闭包")
   (P "注记: 本文是1988年发表的, 和KFFD流派相比, 句法闭包未能成为主流.")
   (H2 "摘要")
   (P "这篇论文我们描述了句法闭包. 句法闭包解决了编写宏时的作用域问题. "
      "我们讨论了将句法闭包引入宏展开接口引起的一些问题, 并将句法闭包与其他"
      "方法进行了比较. 完整实现也随附其中.")
   (H2 "1&nbsp;&nbsp;&nbsp;&nbsp;宏的麻烦")
   (H2 "2&nbsp;&nbsp;&nbsp;&nbsp;术语")
   (H2 "3&nbsp;&nbsp;&nbsp;&nbsp;我们的解决方案")
   (P "就像由" (Code "lambda") "表达式返回的闭包, 一个句法闭包由某种形式的环境, "
      "一列名字和一个表达式构成. 对于这两种闭包, 表达式中出现的名字都被视为相对于"
      "那个环境而言的, 除了出现于给定列表中的名字. 列表中的名字的含义是之后确定的. "
      "不论是哪一种情况, 闭包都是一种参数化表达式的方式.")
   (P "不同之处在于" (Code "lambda") "表达式闭包是根据实际参数调用的, 而句法闭包的"
      "调用遵循&quot;call-by-context&quot;的方式. "
      )
   ))