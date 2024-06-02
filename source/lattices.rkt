#lang racket
(provide lattices.html)
(require SMathML)
(define lattices.html
  (TmPrelude
   #:title "格论 (Birkhoff)"
   #:css "styles.css"
   (H1 "格论 (Birkhoff)")
   (P "本书可谓是格论的第一本书, 极大地推动了格论的传播和发展. 这里是我的读书笔记, "
      "原书许多陈旧的记号被替换为了现代的表述.")
   (H2 "第1章 格的类型")
   (H3 "第1节 偏序集; 链")
   ((definition)
    "一个偏序集 (poset) 是一个其上定义了某个偏序关系的集合. 所谓偏序关系, "
    "指的是满足自反性, 反对称性, 传递性的二元关系.")
   ((example)
    "任意集合的幂集在子集关系下成为一个偏序集.")
   ((example)
    "正整数集在整除关系下成为一个偏序集.")
   ((example)
    "区间" (li0 -1 1) "上的所有实值函数构成的集合在这样的二元关系下成为一个偏序集, 函数"
    (&<= $f $g) "如果对于每个" (∈ $x (li0 -1 1)) ", " (&<= (app $f $x) (app $g $x)) ".")
   
   ))