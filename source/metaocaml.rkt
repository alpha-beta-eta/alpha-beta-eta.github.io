#lang racket
(provide metaocaml.html)
(require SMathML)
(define metaocaml.html
  (TnTmPrelude
   #:title "协调抽象和高性能: MetaOCaml方法"
   #:css "styles.css"
   (H1. "协调抽象和高性能: MetaOCaml方法")
   (H2 "摘要")
   (P "生成式编程的一个常见应用是生成高度特化于手头问题的高性能计算内核. "
      "一个典型的线性代数内核会针对数值域 (有理数, 浮点数, 双精度浮点数, 等等), "
      "循环展开因子, 数组布局和先验知识 (例如, 矩阵是正定的) 进行特化. "
      "手动特化 (编写相同算法的诸多变种) 是乏味无聊且容易出错的.")
   (P "广泛使用的生成器, 例如ATLAS和SPIRAL, 能够可靠地产生高度特化的代码, "
      "但是很难扩展. 对于ATLAS而言, 其使用printf生成代码, "
      "甚至连括号匹配都成为了挑战. 根据ATLAS的作者所言, debug如同梦魇.")
   
   (H2. "引入")
   (H3. "为什么元编程?")
   (H3. "为什么这个教程?")
   (H3. "为什么MetaOCaml")
   (H3. "概览")
   (H3. "获得MetaOCaml")
   (H2. "第一步")
   (H3. "Now or later")
   (H3. "幂")
   (P "尽管已经成为陈词滥调, " (Code "power") "这个例子仍然是介绍元编程的最快方式之一. "
      "以下的" (Code "power") "函数可以计算" $x^n ":"
      (CodeB "let square x = x * x
let rec power : int -> int -> int = fun n x ->
  if n = 0 then 1
  else if n mod 2 = 0 then square(power (n/2) x)
  else x * (power (n-1) x)")
      "我们已经显式地附加了类型签名, 尽管并无必要: 类型可以推断出来. "
      "不过, 给top-level的函数添加签名一般而言是个好的想法: "
      "它可以使得代码更容易编写, 之后更容易理解, 并且能够使得错误信息更加清晰. "
      
      )
   (P "设我们的程序需要计算" $x^7 "很多次, 那么我们可以定义"
      (CodeB "let power7 x = power 7 x")
      "来命名和分享这部分求值.")
   (P "在MetaOCaml里, 我们也可以" (Em "特化") "幂函数于一个特定的值"
      $n ", 得到之后可以接受" $x "并计算" $x^n "的代码. "
      )
   ))