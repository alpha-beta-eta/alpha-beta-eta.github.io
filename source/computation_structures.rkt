#lang racket
(provide computation_structures.html)
(require SMathML)
(define computation_structures.html
  (TmPrelude
   #:title "Computation Structures笔记"
   #:css "styles.css"
   (H1 "Computation Structures笔记")
   (H2 "第1章 数字抽象")
   
   (H2 "第2章 二进制表示与记号")
   (P $n "(二进制)位可表示" (^ $2 $n) "个状态. "
      "对于通常的二进制表示" (: (_ $b (&- $n $1)) $..h $b_0)
      ", 其代表了值" (sum (&= $i $0) (&- $n $1) (&d* $b_i (^ $2 $i)))
      ". 如果我们在想象中为某个固定的间隙插入了小数点, 那么就如同通常的十进制那样, "
      "可以表示一些有理数, 这就是所谓的定点表示.")
   (P "表示有符号的整数的话, 主要有三种约定, 即2's complement, 1's complement和"
      "sign/magnitude. 这三种表示的最高位都用来指示正负, " (Code "0")
      "表示正, " (Code "1") "表示负. 正数的情况下, 都是通常的无符号表示.")
   (P "或许最直接的方式就是sign/magnitude. 除了最高位指示正负之外, 其余位就和通常表示一样"
      "指示大小. 例如, " (Code "1101") "可以表示" (&- $5) ". ")
   (P "对于1's complement表示而言, 负数的话, 在sign/magnitude的基础之上, 除了最高位全部取反, "
      "即" (Code "0") "变成" (Code "1") ", " (Code "1") "变成" (Code "0") ". 例如, "
      (Code "1010") "可以表示" (&- $5) "."
      )
   (H2 "第3章 组合设备与电路")

   (H2 "第4章 序列和状态")

   (H2 "第5章 数字系统的合成")
   
   (H2 "第6章 有限状态机器")

   (H2 "第7章 控制结构与自律")

   (H2 "第8章 性能度量与取舍")

   
   ))