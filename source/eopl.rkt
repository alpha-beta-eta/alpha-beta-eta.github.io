#lang racket
(provide eopl.html)
(require SMathML)
(define eopl.html
  (Prelude
   #:title "EoPL3笔记"
   #:css "styles.css"
   (H1 "EoPL3笔记")
   (P "Essentials of Programming Languages (3ed) 已经是许久以前读的了, "
      "现在记下一些东西, 或许对于后来者有用.")
   (H2 "第1章 归纳性的数据集合")
   (P "引入了数据的归纳 (或者说递归) 定义, 尤其是BNF文法, 并介绍了与之匹配的"
      "编程技术和证明技术 (结构归纳法).")
   (H2 "第2章 数据抽象")
   (P "引入了数据抽象的想法, 也就是抽象数据类型和其实现的分离; "
      "介绍了一个定义数据类型的工具, 以及解构的工具; "
      "引入了抽象句法树的想法, 为之后的章节做铺垫.")
   (H2 "第3章 表达式")
   
   ))