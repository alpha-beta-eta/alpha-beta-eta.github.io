#lang racket
(provide extensible_spec.html)
(require SMathML)
(define $o* (Mo "&otimes;"))
(define $o+ (Mo "&oplus;"))
(define $d*_bottom (_ $d* $bottom))
(define-infix*
  (&o* $o*)
  (&o+ $o+))
(define $environments (Mi "environments"))
(define $stores (Mi "stores"))
(define $continuations (Mi "continuations"))
(define $values (Mi "values"))
(define extensible_spec.html
  (TnTmPrelude
   #:title "可扩展指称性语言描述"
   #:css "styles.css"
   (H1. "可扩展指称性语言描述")
   (P "当前状态: 试译, 以机翻为主.")
   (H2 "摘要")
   (P "传统的指称语义为同一个phrase赋予截然不同的意义, "
      "具体取决于编程语言的其余部分. 如果语言是纯函数式的, "
      "一个numeral的指称是从环境到整数的函数. "
      "但在具有命令式控制算子的函数式语言中, "
      "一个numeral的指称是从环境和延续到整数的函数. "
      "本文引入了一种新的格式用于指称性语言描述, 即"
      (Em "扩展直接语义") ", 它能够容纳语言的正交扩展, "
      "而无需改变既有phrase的指称. "
      "扩展直接语义始终将一个numeral映射到相同的指称: "
      "即将相应的数字inject到值域中. 一般而言, "
      "一个phrase在函数式语言中的指称始终是"
      "同一phrase在一个扩展语言的语义中的指称的投影"
      -- "不论扩展是什么. 基于扩展直接语义, "
      "还可以通过组合语言片段的解释器来构造完整语言的解释器.")
   (H2. "复杂语言的指称性描述")
   (P "像Scheme [25], Common LISP [32] 或ML [19] 这样的编程语言, "
      "由一个丰富的函数式核心组成, "
      "并辅以对数据对象的destructive operation, "
      "控制构造, 以及可能的其他命令式算子. "
      "传统的指称性语言描述 [1, 15, 27, 35] "
      "通过将程序phrase解释为从"
      (&c* $environments $stores $continuations)
      "到" (&c* $values $stores)
      "的函数来应对这些构造. 然而, "
      "程序员在推理程序phrase时依赖的是更简单的语义描述. "
      "大多数程序phrase并不利用语言的全部一般性, "
      "因此可以用更简单的语义模型来分析. 例如, "
      "如果一个程序phrase是纯函数式的, "
      "且其自由变量始终绑定到无副作用的过程, "
      "那么它可以被解释为从" $environments "到" $values
      "的函数. 类似地, 如果一个命令式程序phrase不使用像"
      (apply enum (map* Code "callcc" "goto" "catch"))
      "这样的一般性控制算子, "
      "且其自由变量始终绑定到满足相同约束的值和过程, "
      "那么它可以被解释为从" (&c* $environments $stores)
      "到" (&c* $values $stores) "的函数. 遗憾的是, "
      "实际编程语言的指称定义所采用的形式使得"
      "很难为一个有纪律的子集提取出简化的定义" --
      "更不用说证明这些定义在受限语言上是等价的了.")
   (P "描述同一问题的另一种方式是观察当语言被扩展时, 像numeral "
      (Code "5") "这样的简单程序phrase的意义会发生什么变化. "
      "在函数式语言中, numeral " (Code "5")
      "的指称正是程序员所期望的: 一个从环境到整数" $5
      "的函数. 但如果我们向语言中添加引用单元 (reference cell), "
      (Code "5") "就变成指称一个从"
      (&c* $environments $stores)
      "到" (&c* $values $stores)
      "的常函数. 此外, 如果我们再添加控制算子 (如"
      (Code "goto") "或" (Code "callcc")
      "), " (Code "5") "的意义就变成了一个从"
      (&c* $environments $stores $continuations)
      "到" (&c* $values $stores)
      "的常函数. 指称语义的这一恼人性质在语言研究者中是众所周知的. "
      "事实上, 在最近的一篇综述论文中, Peter Mosses [23] 指出, "
      "这一现象一直是指称语义作为定义编程语言的实用工具被广泛接受的主要障碍.")
   
   (H2. "可扩展的操作语义")
   
   (H2. "可扩展的指称性描述")
   (H3. "语义框架: 扩展了的直接语义")
   (H3. "纯Scheme")
   (H3. "状态Scheme")
   (H3. "控制Scheme")
   (H3. "稳定指称")
   (H2. "将解释器复合")
   (H2. "相关工作")
   (H2. "结论")
   (H2 "附录A. domain描述的记号")
   (P "本篇论文所呈现的语义定义依赖于domain构造子"
      $o* " (smash product), " $o+
      " (coalesced sum), " $d*_bottom
      " (lifting), " $--> " (连续函数), "
      
      )
   ))