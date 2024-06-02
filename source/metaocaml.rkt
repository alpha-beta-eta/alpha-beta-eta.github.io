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
   
   ))