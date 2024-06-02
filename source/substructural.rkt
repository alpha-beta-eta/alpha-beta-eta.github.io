#lang racket
(provide substructural.html)
(require SMathML "proof-tree.rkt")
(define $square (Mi "&EmptySmallSquare;"))
(define (&square A) (ap $square A))
(define (&! A) (ap $! A))
(define -- "&mdash;&mdash;")
(define (Const str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $edge (Const "edge"))
(define (&edge x y)
  (appl $edge x y))
(define $path (Const "path"))
(define (&path x y)
  (appl $path x y))
(define $Edge (Const "Edge"))
(define $Trans (Const "Trans"))
(define (&rull #:label [label #f] . j*)
  (if label
      (: (apply &rule j*) label)
      (apply &rule j*)))
(define &split:16 (&split 16))
(define substructural.html
  (TnTmPrelude
   #:title "亚结构逻辑笔记"
   #:css "styles.css"
   (H1. "亚结构逻辑笔记")
   (H2 "注记")
   (P "这是阅读Pfenning的亚结构逻辑讲义所做的笔记. "
      "这个课程之前的版本叫做线性逻辑. "
      "线性逻辑是亚结构逻辑的一种. "
      "虽然我之前学过一些线性逻辑, "
      "但仍然对于线性逻辑和亚结构逻辑不甚了解.")
   (P "implication一般翻译为推出? 不过我保留了原文, "
      "因为翻译了之后容易和其他一些概念引起混淆. "
      "valid我没有翻译, 通行的翻译" (Q "有效")
      "也往往招致误解, 实际上在逻辑学中它具有特定的意义.")
   (H2. "真性是瞬态的 (Truth is Ephemeral)")
   (H3. "引论")
   (P "在入门课程里学习逻辑, 我们习惯于将真性当作一种数学概念, "
      "某种客观且就像物理定律那样不可改变的东西. "
      "真性是某种我们可以揭示和理解的东西, "
      "但并不是我们可以创造的东西. "
      "在这门课程里我将试图说服你其实存在着别的不同观念.")
   (P "首先, 真性是瞬态的. 在讲座的这个时间点我拿着一支粉笔. "
      "直接的证据可以确认: " (Q (Em "Frank拿着一支粉笔."))
      " 在我将这粉笔置于我面前的桌子上之后, "
      "这个命题就不再为真了. 因此, 显然, " (Em "真性是瞬态的")
      ". 亚结构逻辑捕获并分析这种现象, "
      "其在计算机科学中具有根本的重要性. "
      "例如, 当在某个命令式语言中执行着一个程序时某个变量"
      $x "或者存放着值" $5 ". 这就是一个瞬态的真性, "
      "因为在给" $x "赋值" $7 "之后其就不再为真了" --
      "转而, " $x "的值会是" $7 ".")
   (P "其次, 特定的真性是永恒的. "
      "例如, 对于过剩的证明而言, "
      "很难否定Pythagoras定理. "
      "抑或是, 根据implication的本性, 对于任意的命题"
      $A ", " $A "总是可以推出" $A
      ". 亚结构逻辑既考虑到了瞬态也考虑到了永恒, "
      "因此其是为了" (Em "泛化") "而非取代研究永恒真性的传统逻辑. "
      "在这门课程的上下文里, 我们将这样的逻辑称为" (Em "亚结构的")
      ". 术语" (Em "结构") "和" (Em "亚结构")
      "的起源将在这次讲座的之后得到澄清.")
   (P "逻辑是对于valid推理的法则的研究, "
      "所以说我们将以同样的方式开始我们的课程. "
      "今天的讲座我们将会全然避免逻辑联结词, 只使用推理规则. "
      "实际上, 我们可以使用推理规则描述一些算法, "
      "它们可以视为执行逻辑推理, "
      "这是逻辑和计算机科学的诸多联系之一.")
   (H3. "结构推理")
   (P "考虑有向图中顶点" $x "和" $y "之间的关系" (&edge $x $y)
      ". 我们想要定义何时从" $x "到" $y "存在着一条路径. "
      "从数学上来说, 我们可以称路径 (path) 关系是边 (edge) 关系的传递闭包. "
      "我们以两条推理规则定义这个概念:"
      (MB (&split:16
           (&rull #:label $Edge
                  (&edge $x $y)
                  (&path $x $y))
           (&rull #:label $Trans
                  (&path $x $y)
                  (&path $y $z)
                  (&path $x $z))))
      "一些术语: "
      )
   (H2. "从推理到逻辑联结词")
   (H2. "cut和identity消去")
   (P "{译注: 最早Pfenning使用的术语是cut归约和identity展开, 但意思完全相同.}")
   (H2. "证明项")
   (H2. "线性消息传递I")
   (H2. "线性消息传递II")
   (H2. "保持和进展")
   (H2. "子定型")
   (H2. "validity")
   (H3. "引论")
   (P "到目前为止我们已经为有序逻辑, 线性逻辑, 以及亚结构逻辑划定了严格的界限. "
      "为了使得线性逻辑更富表现力, 我们使用了递归, "
      "因为从编程角度而言递归是相当自然的. "
      "在讲座2里我们简要提及了指数模态" (&! $A)
      "以及它的规则, 其服从于弱化和收缩.")
   (P "本次讲座里我们将会进行解释, " (&! $A) "实际上是某种更为一般的构造的结果, "
      "而这种构造也可以对于其他逻辑施行. 例如, 在结构逻辑中, 其通常记作"
      (&square $A) ", 表达了" $A "必然为真或者" $A "在所有的可能世界中为真. "
      "从这个角度来看" (&! $A) "的证明论在某种意义上是更加令人愉快的, "
      "但最终也并不全然令人满意. 下次讲座我们将会回到这里进行改进, "
      "甚至进一步将其泛化.")
   (H3. "Girard的指数")
   (P "Girard [1987]的指数模态, 在直觉主义情形下 [Girard and Lafont, 1987], "
      "可以在相继式演算中以如下规则定义."
      (MB (&split:16
           (ProofTreeRender
            `(!R (,Δ) ,$A)
            env:linear)
           (ProofTreeRender
            `(!L (,Δ) ,$A ,$C)
            env:linear)))
      (MB (&split:16
           (ProofTreeRender
            `(contract (,Δ) ,$A ,$C)
            env:linear)
           (ProofTreeRender
            `(weaken (,Δ) ,$A ,$C)
            env:linear)))
      "这里" (&! Δ) "的意思是" Δ "中的每个前件都具有形式" (&! $B)
      ". 以这些规则, 我们可以从" (&! $A) "中得到任意多份想要的"
      $A "的复制.")
   (H3. "Andreoli的指数")
   
   (H2. "一个混合的线性/非线性逻辑")
   (H2. "伴随逻辑")
   (H2. "聚焦")
   (H2. "量化子")
   (H2. "半公理化相继式演算")
   
   ))