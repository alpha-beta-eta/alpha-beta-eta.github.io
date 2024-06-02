#lang racket
(provide exercises_in_programming.html)
(require SMathML)
(define exercises_in_programming.html
  (TnTmPrelude
   #:title "编程习题集"
   #:css "styles.css"
   (H1. "编程习题集")
   (P "这个名字可能招致误解. 看到" (Q "编程习题集")
      "这几个字的人可能会想, 编程也需要做习题集吗? "
      "这个作者一定是热爱做题的" (Q "小镇做题家")
      ". 不过, 这里的习题集并不典型, "
      "可能和Paul Halmos的"
      (Q "Linear Algebra Problem Book")
      "比较类似. 事实上, 编程的重要性被全然低估了, "
      "而基于LLM的AI的出现更是对于编程这个学科的毁灭性打击. "
      "绝大多数程序员宁可认为数据结构和算法, 软件工程是重要的, "
      "也不愿承认编程 (或者说程序设计) 是重要的. "
      "这个习题集承载着作者本人的偏执, "
      "即编程在计算机科学中的地位应该比其他绝大多数分支都要更高.")
   (H2. "简单递归")
   ((exercise)
    "如果我们使用没有重复元素的列表来表示有限集合, "
    "那么请你编写函数"
    (apply enum (map* Code "U" "I" "D"))
    ", 其分别计算两个集合之并, 交, 差. "
    "这里相等使用" (Code "equal?") "谓词来判断.")
   
   ))