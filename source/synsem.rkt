#lang racket
(provide synsem.html)
(require SMathML)
(define $Implies (Mo "&Implies;"))
(define $! (Mo "!"))
(define (&! A) (: $! A))
(define $-o (Mo "&multimap;"))
(define $t* (Mo "&CircleTimes;"))
(define $& (Mo "&"))
(define-infix*
  (&Implies $Implies)
  (&-o $-o)
  (&t* $t*)
  
  )
(define synsem.html
  (TmPrelude
   #:title "线性逻辑: 其句法和语义"
   #:css "styles.css"
   (H1 "线性逻辑: 其句法和语义")
   (P "作者: Jean-Yves Girard")
   (H2 "第1章 线性逻辑的句法")
   (H3 "第1.1节 线性逻辑的联结词")
   (P "线性逻辑不是另一种逻辑, 其应该被视为通常逻辑的一种扩展. "
      "既然无望于修改现存的古典或直觉主义联结词, 线性逻辑引入了新的联结词.")
   (H4 "第1.1.1小节 Exponentials: actions vs situations")
   (P "古典和直觉主义逻辑处理稳定的事实:"
      (MB "如果" $A "且" (&Implies $A $B) ", 那么" $B ", 但" $A "仍然成立.")
      "这在数学中是完美的, 但是在实际生活中却是错误的, 既然real implication是"
      (Em "causal") "的. 一个causal implication是不可迭代的, 因为条件将在其使用后"
      "被修改. 这种修改前提 (或者条件) 的过程在物理中被称为" (Em "reaction")
      ". 例如, 若" $A "是花" $1 "美元买一包香烟, 而" $B "是获得这包香烟, 那么"
      "你在这个过程之中就失去了" $1 "美元, 而你不能再进行第二次. 这里的reaction是"
      $1 "美元从你的钱包里溜走了. 对于这种观点的第一个反驳在于不论在数学还是现实生活中, "
      "都存在reaction不存在或者可以被忽略的情形: 试想一个永远正确的引理, 抑或是"
      "Mr. Soros, 他有近乎无限的钱. 这样的情形在稳定事实的意义下是" (Em "situation")
      ". 我们的logical refinement不应该阻止我们对付situation, 并且应该存在特别的一种"
      "联结词 (" (Em "exponentials") ", &quot;" '! "&quot;和&quot;" '? "&quot;) "
      "来表达一个action的可迭代性, 即reaction的缺失. 典型地, " (&! $A)
      "的意思是想花多少钱就花多少钱. 如果我们使用符号" $-o " (" (Em "linear implication")
      ") 代表causal implication, 那么一个通常的直觉主义implication " (&Implies $A $B)
      "因而就以"
      (MB (&= (&Implies $A $B) (&-o (@ (&! $A)) $B)))
      "的样子显现, 即" $A "推出" $B "恰当" $B "由" $A "的一些迭代导致. 这个公式是将"
      "直觉主义逻辑忠实地翻译为线性逻辑的基本成分. 当然, 古典逻辑也是可以被忠实地翻译为"
      "线性逻辑的, 所以什么也不会失去... 至于获得了什么, 还需要观察.")
   (H4 "第1.1.2小节 两个合取")
   (P "在线性逻辑中, 两种合取" $t* " (" (Em "times") ") 和" $& " (" (Em "with")
      ") 并存. "
      )
   ))