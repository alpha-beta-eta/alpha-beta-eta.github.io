#lang racket
(provide hot_01_sax.html)
(require SMathML "hot_utils.rkt")
(define (Cite . str*)
  (apply string-append "[" (append str* '("]"))))
(define $c:A⊗B (@: $c (&⊗ $A $B)))
(define $c:Σ (@: $c (Σ* $ell $A_ell $L)))
(define (Read⊕ c x P)
  (&read c (_ (setE (&=> (app $ell x) (app P x)))
              (∈ $ell $L))))
(define (P_k a) (app $P_k a))
(define $⊗R (: $⊗:id $R))
(define $P_k (_ $P $k))
(define hot_01_sax.html
  (TnTmPrelude
   #:title "Sax: 一种命令式中间语言"
   #:css "styles.css"
   (H1. "Sax: 一种命令式中间语言")
   (P "Frank Pfenning. 原文标题为"
      (Q "Lecture Notes on Sax: An Imperative Intermediate Language")
      ", 是CMU 15-417/817: HOT Compilation的第1讲讲义, 2025年1月14日.")
   (P "返回" (LINK "HOT编译" "hot.html") "的索引页面; 本文的译名见"
      (LINK "术语翻译对照" "hot_glossary.html") ".")
   (H2. "引论" #:id "introduction")
   (P "编写编译器的关键方面之一在于设计表示源语言和目标语言之间的步骤的合适"
      (Em "中间语言") ". 源语言, 中间语言, 目标语言在整个学期将会逐渐演化, "
      "因为你将会为愈来愈复杂的语言编写一系列编译器. "
      "第一次lab的目标在于让你熟悉中间语言; "
      "第二次lab的目标在于如何转换进和出这种语言.")
   (P "所有 (或者应该说几乎所有?) 的编译器都依赖于中间语言. "
      "在15-411编译器设计这门课里, 常用的中间语言是静态单赋值 (SSA) 或者LLVM. "
      "CMU开发的Standard ML编译器TIL " (Cite "Tarditi et al., 2004")
      " 使用了多个中间语言, 并且基于这样一个前提: 中间语言应该是有类型的, "
      "一路向下直到有类型汇编语言 (TAL) " (Cite "Morrisett et al., 2003")
      " 为止.")
   (P "Sax " (Cite "DeYoung et al., 2020")
      " 是这个领域中相对较新的成员. 它是相当高层次的, 这是就以下意义而言的: "
      "作为Curry-Howard对应的一个实例, 它对应于一个基于某种形式的相继式演算的逻辑系统. "
      "然而, 它又有着直观的命令式解释. 因为其相对高层次的本性, "
      "定义和使用其亚结构变体是容易的, 探索如何高效地实现这些变体, "
      "以及如何利用它们所保证的性质, 也是容易的.")
   (P "我们不会一次性展示整个Sax, 而是从一个正的且线性的核心开始. "
      "在这个语境下, 正的意味着语言是一阶的, 也就是说, 它没有任何高阶函数. "
      "而这里的线性意味着不需要传统的垃圾回收, "
      "因为所有的内存单元都可以在被读取时释放.")
   (H2. "正类型与大值" #:id "positive-types")
   (P "从使用者角度而言, 正类型的值是那些我们可以直接观察结构的值, "
      "其包括布尔值, 自然数, 或者是元素具有可观察类型的列表或树, 等等. "
      "反过来, 负类型的值是那些我们" (Em "不能")
      "直接观察结构但是可以与之进行交互的值, 例如函数和对象.")
   (P "编程语言理论已经建立了对于类型的构建块的基本理解. "
      "与之对应的是, 证明论也确定了命题的构建块. "
      "我们将几乎完全坚持编程语言的视角, "
      "但是有时会将其和直觉主义(线性)命题联系起来.")
   (P "我们对大值进行定型的基本判断是"
      (MB (&: $V $A))
      "其表达了大值" $V "具有类型" $A
      ". 我们所观察的值并不包含任何变量, "
      "因而我们无需使用任何上下文来描述其类型. "
      "之所以我们将这些值称为" (Em "大的")
      ", 是因为一个单独的值就包含了关于计算结果的所有信息.")
   (P (B "序对. ")
      "对于序对的正类型我们记" (&⊗ $A $B) "."
      (MB (&rull
           $⊗I
           (&: $V $A) (&: $W $B)
           (&: (tu0 $V $W) (&⊗ $A $B))))
      "这个记号是借用自线性逻辑的, 将规则命名为" $⊗I
      "以指示这是一条" (Em "引入规则")
      "也是如此. 在直觉主义逻辑中, 这是两种合取形式之一. "
      "在具有按值调用语义的函数式语言 (ML语族) 中, 其被记为"
      (Code "A * B") ", 这也是我们实际所采用的具体句法.")
   (P (B "单位. ")
      "对于单位类型我们将其记为" $unit
      ", 其中只寓居着单位元素."
      (MB (&rull
           $unitI
           (&: (tu0) $unit)))
      "在常见的语言中, 单位类型通常记作" $unit0
      "; 在我们的具体句法中就记作" (Code "1") ".")
   (P (B "和. ")
      "对于类型" $A_ell "的无交和我们记作"
      (Σ* $ell $A_ell $L)
      ", 其中" (Em "标签") $ell "是从一个有限索引集合"
      (&!= $L (setE)) "中取的. 在线性逻辑中, 二元的版本记作"
      (&⊕ $A $B) ", 其与直觉主义逻辑中的" (&disj $A $B)
      "是相同的. 为了简单起见, 我们避免了空和. "
      "另外, 因为空和并不包含值, 故其编程应用有限. "
      "我们一般使用" $ell "和" $k "代表标签."
      (MB (&split16
           (&rull
            $⊕I
            (@ (∈ $k $L)) (&: $V $A_k)
            (&: (app $k $V) (Σ* $ell $A_ell $L)))
           (&rull
            $⊕I_1
            (&: $V $A)
            (&: (app $inl $V)
                (&⊕ $A $B)))
           (&rull
            $⊕I_2
            (&: $V $B)
            (&: (app $inr $V)
                (&⊕ $A $B)))))
      "右边我们展示了二元版本这一特殊情形, 其中"
      (&≜ (&⊕ $A $B)
          (&+ (@: $inl $A)
              (@: $inr $B)))
      ". 它们并非真的是语言的组成部分.")
   (P "以这些值构造子, 我们已经可以表示诸如布尔"
      (&+ (@: $false:lab $unit) (@: $true:lab $unit))
      "这样的有限类型. "
      "为了表示诸如列表和自然数这样的无限类型, "
      "我们也需要递归.")
   (P (B "等价递归类型. ")
      "我们没有添加用于构造递归类型的绑定构造子"
      (μ $alpha (app $A $alpha))
      ", 而是以顶层定义来表示递归类型, 例如"
      (MB (&type $nat
                 (&⊕ (@: $zero $unit)
                     (@: $succ $nat))))
      "{译注: 不要将其与二元和" (&⊕ $A $B)
      "混淆, 这个其实是和类型的某种具体句法.} "
      "从现在开始, 我们对于和采用一种更贴近于我们的具体句法的记号."
      (MB (&type $nat
                 (⊕ $zero $unit
                    $succ $nat)))
      "说这些类型是" (Em "等价递归的") "是什么意思? "
      "这意味着我们将这种定义视为元层次的" (Em "等式")
      ", 而非需要强制的" (Em "同构")
      ". 下一讲我们将会看到这种简单性是有代价的, "
      "但是我相信这个代价非常值得付出.")
   (P "因为类型定义是等价递归的, 所以我们有着诸如"
      (MB (&: (&succ (&succ (&zero))) $nat))
      "这样的判断. 对于" $nat "这样的类型名称, 我们也没有单独的规则, "
      "因为我们可以" (Q "悄无声息")
      "地应用和的规则来导出以上的判断."
      (MB (&rull
           $⊕I
           (&rull
            $⊕I
            (&rull
             $⊕I
             (&rull
              $unitI
              (&: (tu0) $unit))
             (&: (&zero) $nat))
            (&: (&succ (&zero)) $nat))
           (&: (&succ (&succ (&zero))) $nat)))
      "以数学语言, 所有以这种方式定义的正类型都是" (Em "归纳的")
      ". 这需要一个限制: 类型定义的右侧不应该是类型名称本身, "
      "而是应该以某个构造子 (" $⊗ ", " $unit ", " $⊕
      ") 开始. 这避免了形如" (&type $t $t) "的定义.")
   (P "此外, 我们允许所有的类型定义都是互递归的, "
      "假如我们使用显式类型构造子" $mu
      "的话, 这可能会有点令人不快.")
   (P "作为另一个例子, 自然数的列表可以定义为"
      (MB (&type $list
                 (⊕ $nil $unit
                    $cons (&⊗ $nat $list)))))
   (H2. "内存和小值" #:id "memory")
   (P "到目前为止我们知道了我们想要表示大值, "
      "但是我们也想要以某种方式在内存中表示大值, "
      "以使得我们能够命令式地对其进行操作. "
      "然而, 一个内存单元不能存放具有任意大小的值, "
      "因而我们需要将大值分解为更小的部分. "
      "我们的方法是使用一种抽象语义对象" (&cell $a $v)
      ", 其表示地址" $a "处的内存单元存放着小值" $v
      ". 一个小值要么是一对地址, 要么是单位值, "
      "要么是一个标签连带着一个地址. "
      "例如, 之前的例子里的数字二可以表示为"
      (MB (set-attr*
           (&Table
            ($cell $0 (&succ $1))
            ($cell $1 (&succ $3))
            ($cell $3 (&zero $4))
            ($cell $4 (tu0)))
           'columnalign "left"))
      "这里我们用了整数来表示地址, 这对于真实内存而言实际上是典型的. "
      "我们也假定了每个内存单元都有足够的空间用于存放任意的小值. "
      "通常来说, 两个机器字就足以存放一对地址, 带标签的值, 以及单位值.")
   (P "我们或许已经可以预见到这里的一些优化, 例如不分配一个显式的单元来存放单位值. "
      "然而, 重要的是在这个阶段不要沉迷于这些考虑, 而是先实现直接的版本. "
      "一方面, 它是有理论支撑的, 并且其实现应该是简单的, "
      "因而比一个更复杂但可能经过优化的实现更有可能是正确的. "
      "另一方面, 它为我们提供了一个基线, "
      "使得我们之后可以测量优化实际上给我们带来了什么.")
   (P "还有一点: 程序可被观察到的结果是它的大值, 而不是它精确的布局. "
      "在上面的例子中, 我们跳过了地址" $2 ", 就是为了说明这一点. "
      "因此, 在求值结束时, 需要一个小程序来遍历内存并打印出大值. "
      "这个程序将依赖于对于内存布局的特定选择, 例如单位值是否被显式表示.")
   (H2. "对于内存进行定型" #:id "typing-memory")
   (P "我们如何给一个内存单元指派类型呢? 例如, 单元"
      (MB (&cell $c (tu0 $a $b)))
      "定义了地址" $c "处的小值, 并引用了地址" $a "和" $b
      "处的小值. 因此, 我们将其定型为"
      (MB (&seq (&cm (&: $a $A) (&: $b $B))
                (&cell $c (tu0 $a $b))
                $c:A⊗B))
      "这与大值的定型有两处不同. 一是现在有了代表其他内存单元 (这里是"
      $a "和" $b ") 的类型的前件. 二是我们也为后件提供了一个地址 (这里是"
      $c "), 以追踪小值存放在何处.")
   (P "一般而言, 我们希望在程序执行时维持类型" --
      "这是一个几乎普适的原则, 用以确保计算的结果满足关于其类型的原初期望. "
      "因此, 一个写入内存单元的命令应该恰以结果单元的方式被定型. "
      "这给出了以下规则."
      (MB (&split16
           (&rull
            $⊗X
            (&seq (&cm (&: $a $A) (&: $b $B))
                  (&write $c (tu0 $a $b))
                  $c:A⊗B))
           (&rull
            $unitX
            (&seq $· (&write $c (tu0)) (@: $c $unit)))))
      (MB (&rull
           $⊕X
           (@ (∈ $k $L))
           (&seq (&: $a $A_k)
                 (&write $c (app $k $a))
                 $c:Σ)))
      "我们将它们命名为" $X "是因为如果忽略命令的话, 它们代表了逻辑公理."
      (MB (&split16
           (&rull $⊗X (&vdash (&cm $A $B) (&⊗ $A $B)))
           (&rull $unitX (&vdash $· $unit))
           (&rull $⊕X_1 (&vdash $A (&⊕ $A $B)))
           (&rull $⊕X_2 (&vdash $B (&⊕ $A $B)))))
      "于是这些规则保持了与逻辑规则的对应. 定型判断现在具有形式"
      (MB (&seq (UnderBrace
                 (&cm (&: $a_1 $A_1) $..h (&: $a_n $A_n))
                 (% "从这里读取"))
                $P
                (UnderBrace (@: $c $C) (% "写入这里"))))
      "其中" (&cm $a_1 $..h $a_n) "和" $c "代表地址, 而" $P
      "是一个要执行的命令. 我们常常将" $P "视为一个" (Em "进程")
      ", 因为 (正如我们将会看到的) 这个语言有着自然的并行解释. "
      "如果我们想要强调顺序意义, 我们就说" (Em "命令") ".")
   (P "在我们的动态语义中, 语义对象有两种形式. "
      (&cell $c $v) "表示地址为" $c "的单元存放着小值" $v
      ". 我们也记" (&proc $P) ", 其意味着进程" $P
      "正在执行. 于是, 写操作的动态语义的规定几乎是平凡的:"
      (MB (&--> (&proc (@ (&write $c $v))) (&cell $c $v))))
   (H2. "从内存中读取" #:id "reading")
   (P "将小值写入内存很容易借由内存单元本身必须如何定型来解释. "
      "那么读取呢? 例如, 当读取一个序对时, 我们将两个变量绑定到地址, "
      "一个绑定到第一个分量, 另一个绑定到第二个分量. 相应的构造是"
      (MB (&read $c (&=> (tu0 $x $y) (appl $P $x $y))))
      "其中我们显式写出了" $P "对于" $x "和" $y "的依赖.")
   (P "我们可以看到, 如果" (&: $c (&⊗ $A $B)) "存放着一个小值"
      (tu0 $a $b) ", 那么在我们执行" $P "时, " $x "应该被绑定到"
      $a ", " $y "应该被绑定到" $b ". 我们将其记为"
      (MB (&--> (&cm (&cell $c (tu0 $a $b))
                     (&proc (@ (&read $c (&=> (tu0 $x $y) (appl $P $x $y))))))
                (&proc (appl $P $a $b))))
      "这对于定型意味着什么呢? 当我们从" $c "读取时, 我们使用了它, "
      "然后在给" $P "定型时我们就可以使用" $a "和" $b
      " (在程序中由" $x "和" $y "表示)."
      (MB (&rull
           $⊗L
           (&seq (&cm Γ (&: $x $A) (&: $y $B))
                 (appl $P $x $y)
                 $delta)
           (&seq (&cm Γ (&: $c (&⊗ $A $B)))
                 (&read $c (&=> (tu0 $x $y) (appl $P $x $y)))
                 $delta)))
      "这里, 我们用" $delta "表示诸如" (&: $d $D)
      "这样的东西, 它命名了" $P "最终必须写入的地址. 我们常常将"
      $d "称为" (Em "目的地") ".")
   (P "单位类型的情况是类似的, 但是并不绑定任何变量."
      (MB (&--> (&cm (&cell $c (tu0))
                     (&proc (@ (&read $c (&=> (tu0) $P)))))
                (&proc $P)))
      "而从定型的角度来看:"
      (MB (&rull
           $unitL
           (&seq Γ $P $delta)
           (&seq (&cm Γ (&: $c $unit))
                 (&read $c (&=> (tu0) $P))
                 $delta))))
   (P "为了读取和, 进程必须考虑所有可能的和项."
      (MB (&rull
           $⊕L
           (: (@ (&seq (&cm Γ (&: $x_ell $A_ell))
                       (app $P_ell $x_ell)
                       $delta))
              (@ (: $forall (∈ $ell $L))))
           (&seq (&cm Γ (&: $c (Σ* $ell $A_ell $L)))
                 (Read⊕ $c $x_ell $P_ell)
                 $delta)))
      $⊕L "规则对于" $L "中的每个标签都有一个前提, "
      "因此在命令执行期间正确的分支总是可用的."
      (MB (&split16
           (&--> (&cm (&cell $c (app $k $a))
                      (&proc (@ (Read⊕ $c $x_ell $P_ell))))
                 (&proc (P_k $a)))
           (@ (∈ $k $L)))))
   (P "这些规则被称为" (Em "左规则")
      ", 因为它们考虑的是前件的类型, 也就是说, 位于十字转门左侧的东西. "
      "它们就是Gentzen的相继式演算 " (Cite "Gentzen, 1935")
      " 中的左规则, 读者可能对此有所了解."
      (MB (&split16
           (&rull
            $⊗L
            (&vdash (&cm Γ $A $B) $C)
            (&vdash (&cm Γ (&⊗ $A $B)) $C))
           (&rull
            $unitL
            (&vdash Γ $C)
            (&vdash (&cm Γ $unit) $C))
           (&rull
            $⊕L
            (&vdash (&cm Γ $A) $C)
            (&vdash (&cm Γ $B) $C)
            (&vdash (&cm Γ (&⊕ $A $B)) $C))))
      "我们观察到, 从逻辑的角度来看, 我们保留了相继式演算的所有左规则, "
      "而将右规则替换为了公理. 这就是为什么我们称其为"
      (Em "半公理相继式演算") " (semi-axiomatic sequent calculus), 简称Sax.")
   (H2. "切和恒等" #:id "cut-identity")
   (P "我们还缺少相继式演算中的两条重要规则, 即切和恒等."
      (MB (&split16
           (&rull
            $cut:rule
            (&vdash Γ $A)
            (&vdash (&cm Δ $A) $C)
            (&vdash (&cm Γ Δ) $C))
           (&rull
            $id:rule
            (&vdash $A $A))))
      "它们在这里是什么意思呢? 原来, 切分配了一个新鲜的内存单元."
      (MB (&rull
           $cut:rule
           (&seq Γ (app $P $x) (@: $x $A))
           (&seq (&cm Δ (&: $x $A)) (app $Q $x) $delta)
           (&seq (&cm Γ Δ)
                 (&cut $x $A (app $P $x) (app $Q $x))
                 $delta)))
      "我们既可以将其读作一个并行构造, 也可以将其读作一个顺序构造, "
      "后者是并行程序的一种特定调度. 进程"
      (MB (&cut $x $A (app $P $x) (app $Q $x)))
      "分配一个新的单元" $a " (在程序中以" $x
      "指称) {译注: 原文此处写作" (Q "referred to as a in the program")
      ", 应为笔误.} 并派生一个进程" (app $P $a) ", 其职责是写入" $a
      ". 进程" (app $Q $a) "将会从" $a
      "读取. 我们的定型判断的解释在这里并没有留下多少余地. 注意"
      $a "被称为一个" (Em "期值") " (future) "
      (Cite "Halstead, 1985, Pruiksma and Pfenning, 2022")
      ", 也是一个同步点: 如果" (app $Q $a) "试图在" (app $P $a)
      "写入" $a "之前从" $a "读取, 那么" (app $Q $a) "将会阻塞, 直到"
      $a "被写入.")
   (P "顺序解释完整地执行" (app $P $a) " (也就是说, 直到它写入其目的地"
      $a "为止), 然后才执行" (app $Q $a) ". 于是, " (app $Q $a)
      "永远无需检查" $a "是否已经被写入, 因为它保证已经被写入了.")
   (P "从动态语义的角度来看, 它不过就是"
      (MB (&split16
           (&--> (&proc (@ (&cut $x $A (app $P $x) (app $Q $x))))
                 (&cm (&proc (app $P $a)) (&proc (app $Q $a))))
           (&fresh $a))))
   (P "我们看到, 在动态语义的这种表述中, 状态里可以有不止一个"
      (Q "proc") "对象. 抽象规则允许其中任何有可应用规则的对象进行转移. "
      "当然, 存在一些方法可以形式化地强制" (&proc (app $P $a))
      "在" (&proc (app $Q $a))
      "开始之前完成, 但是如果我们的关键性质 (例如标准的进展和保持) "
      "并不依赖于此, 我们也可以让它保持非形式化. "
      "某种程度上说, 任何与规则相容的策略对于实现而言都是允许的.")
   (P "这种规定动态语义的方式被称为" (Em "亚结构操作语义")
      " (substructural operational semantics, SSOS) "
      (Cite "Pfenning, 2004, Pfenning and Simmons, 2009, Simmons, 2012")
      ", 它基于多重集重写 " (Cite "Cervesato and Scedrov, 2009")
      ", 因为构成计算状态的语义对象的汇集被解释为一个多重集. "
      "关于这一点, 下一讲会讲更多.")
   (P "恒等规则只是通过在一步之内从一个单元读取并写入另一个单元, "
      "将一个小值从一个单元移动到另一个单元."
      (MB (&rull
           $id:rule
           (&seq (&: $b $A) (&id $a $b) (@: $a $A))))
      "其动态语义为"
      (MB (&--> (&cm (&cell $b $v) (&proc (@ (&id $a $b))))
                (&cell $a $v))))
   (H2. "过程定义" #:id "procedures")
   (P "到目前为止, 我们的语言只有正类型, 那么如何可能定义过程呢? "
      "(我们说过程, 是因为执行它们是为了其对于存储的效应, "
      "而不像函数那样接受一个参数并返回一个结果.) "
      "我们在课上没有讨论这一点, 但是我们在现场编写的例子中用到了它.")
   (P "这一点有些重要, 因为我们想要在允许第一级函数, 对象等之前定义一个一阶语言. "
      "为了定义一个名为" $p "的过程, 我们写"
      (MB (&= (&split2 $proc:kw $p (@: $x $A)
                       (@: $y_1 $B_1) $..h (@: $y_n $B_n))
              (appl $P $x $y_1 $..h $y_n))))
   (P "每个过程" $p "首先接受一个目的地 (这里是" (&: $x $A)
      "), 然后是额外的参数" $y_i ". 在执行时, " $x "和所有的" $y_i
      "都将被绑定到地址. 为了实现这一点, 我们有一个新的构造"
      (MB (&call $p $a $b_1 $..h $b_n))
      "其中参数的数目, 位置和类型必须与过程定义中的那些相匹配.")
   (P "就像类型定义那样, 过程定义可以是任意互递归的. "
      "这与诸如Standard ML和OCaml这样的语言中以句法方式表达的(互)递归形式不同.")
   (P "我们已经说过过程不是函数, 那么它们是什么呢? 原来, 它们是上下文模态类型论 "
      (Cite "Nanevski et al., 2008") " 意义下的" (Em "元变量")
      ". 考虑对于某个元变量" $p "的相继式"
      (MB (&seq (&cm (&: $y_1 $B_1) $..h (&: $y_n $B_n))
                $p (@: $x $A)))
      $p "的类型是什么? 它既依赖于所有的前件, 也依赖于后件. "
      "在上下文模态类型论中, 我们可能会写"
      (MB (&:: $p (: $A (li0 (&: $y_1 $B_1) $..h (&: $y_n $B_n)))))
      "但是这里目的地" $x "也必须被命名, 因此它也许可以写成"
      (MB (&:: $p (@ (&vdash (&cm (&: $y_1 $B_1) $..h (&: $y_n $B_n))
                             (&: $x $A)))))
      "因此, " $p "代表的是整个相继式的推导, 而不是诸如" (&-> $B $A)
      "这样的单个函数. 此外, " $p "将是闭的, 意思是它的所有变量都将是"
      $x "和" $y_i ", 而没有其他东西. 正是这种闭的性质, "
      "使得即便类型系统在其他方面是线性的, 我们也可以任意地重用这些过程定义.")
   (P "我们将在下一讲中处理过程定义和调用的定型.")
   (H2. "一些示例代码" #:id "sample-code")
   (P "在具体句法中有一些选择要做. 我们将" $⊕ "简单地写成" (Code "+")
      ", 将" $⊗ "写成" (Code "*") ". 此外, 所有的标签都具有形式"
      (Code "'&lt;ident&gt;") ". 注释具有形式" (Code "// ...\\n")
      "或" (Code "/* ... */") ", 后者可以正确地嵌套. 另外, "
      (Code "read") "的分支被括在花括号" (Code "{ ... }")
      "中, 并且每个选项前面都有一条竖线" (Code "|")
      ". 唯一的例外是只有单个分支的情况. "
      "更多细节可以在Lab 1的规范中找到, 我们在课上现场编写了这些代码.")
   (P "尽管句法本身使其无歧义, 我们还是使用缩进将写入新分配单元的命令与读取者区分开来. "
      "下面的文件是" (Code "lec01.sax") "."
      (CodeB "/* Lecture 1 */
/* 15-{4,6,8}17 Spring 2025 */

type nat = +{'zero : 1, 'succ : nat}

type bin = +{'b0 : bin, 'b1 : bin, 'e : 1}

// two = 'b0 'b1 'e ()

proc zero (d : bin) =
  cut u : 1
    write u ()
  write d 'e(u)

proc one (d : bin) =
  cut x : bin
    call zero x
  write d 'b1(x)

proc succ (d : bin) (x : bin) =
  read x {
  | 'b0(x0) => write d 'b1(x0)
  | 'b1(x1) => cut y : bin
                  call succ y x1
               write d 'b0(y)
  | 'e(u) => read u ()
             call one d
  }

proc two (d : bin) =
  cut x : bin
    call one x
  call succ d x"))
   (P "无参数过程 (那些只有一个目的地的过程) 可以被执行, 这里就是"
      (Code "zero") ", " (Code "one") "和" (Code "two")
      ". 它们产生一个终态, 从中可以读出相应大值的表示. 相应的文件"
      (Code "lec01.sax.val") "由编译器写出, 其内容如下."
      (CodeB "value zero = 'e ()
value one = 'b1 'e ()
value two = 'b0 'b1 'e ()"))
   (H2. "总结" #:id "summary")
   (P "我们还没有讨论这个系统的许多性质, 包括命令的" (Q "线性")
      "意味着什么. 我们将在下一讲回到这一点. 以下是今天的规则的总结.")
   (H3. "定型" #:id "summary-typing")
   (P "假设有一个程序" Σ "带有声明"
      (&seq (&: $y:bold $B:bold) $p (@: $x $A))
      ". " $p "的定义与过程的类型检查无关, "
      "但是我们必须对照每个过程定义的类型检查该定义 "
      "(假设所有其他定义都满足其类型)."
      (MB (&split16
           (&rull
            $id:rule
            (&seq (&: $y $A) (&id $x $y) (@: $x $A)))
           (&rull
            $cut:rule
            (&seq Γ $P (@: $x $A))
            (&seq (&cm Δ (&: $x $A)) $Q $delta)
            (&seq (&cm Γ Δ) (&cut $x $A $P $Q) $delta))))
      (MB (&split16
           (&rull
            $unitX
            (&seq $· (&write $x (tu0)) (@: $x $unit)))
           (&rull
            $unitL
            (&seq Γ $Q $delta)
            (&seq (&cm Γ (&: $x $unit))
                  (&read $x (&=> (tu0) $Q))
                  $delta))))
      (MB (&rull
           $⊗X
           (&seq (&cm (&: $x $A) (&: $y $B))
                 (&write $z (tu0 $x $y))
                 (@: $z (&⊗ $A $B)))))
      (MB (&rull
           $⊗L
           (&seq (&cm Γ (&: $x $A) (&: $y $B)) $Q $delta)
           (&seq (&cm Γ (&: $z (&⊗ $A $B)))
                 (&read $z (&=> (tu0 $x $y) $Q))
                 $delta)))
      "{译注: 原文中这条规则的名字写作" $⊗R
      ", 应为笔误, 这里改正为" $⊗L ".}"
      (MB (&rull
           $⊕X
           (@ (∈ $k $L))
           (&seq (&: $y $A_k)
                 (&write $x (app $k $y))
                 (@: $x (⊕* $ell $A_ell $L)))))
      (MB (&rull
           $⊕L
           (: (@ (&seq (&cm Γ (&: $y $A_ell)) $Q_ell $delta))
              (@ (: $forall (∈ $ell $L))))
           (&seq (&cm Γ (&: $x (⊕* $ell $A_ell $L)))
                 (&read $x (_ (setE (&=> (app $ell $y_ell) $Q_ell))
                              (∈ $ell $L)))
                 $delta)))
      (MB (&rull
           $call:rule
           (&in (@ (&seq (&: $y:bold $B:bold) $p (@: $x $A))) Σ)
           (&seq Γ (&subst $b:bold $y:bold) (@: $y:bold $B:bold))
           (&seq Γ (&call $p $a $b:bold) (@: $a $A))))
      (MB (&split16
           (&rull
            #f
            (&seq Γ_1 $eta_1 Δ_1)
            (&seq Γ_2 $eta_2 Δ_2)
            (&seq (&cm Γ_1 Γ_2)
                  (tu0 $eta_1 $eta_2)
                  (@ (&cm Δ_1 Δ_2))))
           (&rull
            #f
            (&seq $· (tu0 $·) (@ $·)))
           (&rull
            #f
            (&seq (&: $y $A) (&subst $y $x) (@: $x $A))))))
   (H3. "动态语义" #:id "summary-dynamics")
   (P "我们假定有一个环境" Σ "带有定义"
      (&= (&split2 $proc:kw $p $x $y:bold) (appl $Q $x $y:bold))
      ". 这些规则是多重集重写规则. 它们可以被顺序地应用, "
      "方法是在切中总是先完全求值" (app $P $a) " (也就是说, 直到它写入"
      $a "为止), 再求值" (app $Q $a) ".")
   (P "下面的规则是对于课上规则的一个小小的重构. "
      "我们没有分别考虑每种小值" $v
      ", 而是给出了一般的规则, 以及一种将小值传递给模式匹配构造的方式."
      (MB sax-dynamics)
      (MB sax-▷))
   (H2 "参考文献")
   (Ul
    (Li "Iliano Cervesato and Andre Scedrov. "
        "Relating state-based and process-based concurrency through linear logic. "
        "Information and Computation, 207(10):1044-1077, 2009年10月.")
    (Li "Henry DeYoung, Frank Pfenning, and Klaas Pruiksma. "
        "Semi-axiomatic sequent calculus. In Z. Ariola, editor, "
        "5th International Conference on Formal Structures for Computation and Deduction "
        "(FSCD 2020), pages 29:1-29:22, Paris, France, 2020年6月. LIPIcs 167.")
    (Li "Gerhard Gentzen. Untersuchungen &uuml;ber das logische Schlie&szlig;en. "
        "Mathematische Zeitschrift, 39:176-210, 405-431, 1935. "
        "English translation in M. E. Szabo, editor, "
        "The Collected Papers of Gerhard Gentzen, pages 68-131, North-Holland, 1969.")
    (Li "Robert H. Halstead. "
        "Multilisp: A language for parallel symbolic computation. "
        "ACM Transactions on Programming Languages and Systems, 7(4):501-539, 1985年10月.")
    (Li "J. Gregory Morrisett, Karl Crary, Neal Glew, and David Walker. "
        "Stack-based typed assembly language. "
        "Journal of Functional Programming, 13(5):957-959, 2003.")
    (Li "Aleksandar Nanevski, Frank Pfenning, and Brigitte Pientka. "
        "Contextual modal type theory. Transactions on Computational Logic, 9(3), 2008.")
    (Li "Frank Pfenning. "
        "Substructural operational semantics and linear destination-passing style. "
        "In W.-N. Chin, editor, Proceedings of the 2nd Asian Symposium on "
        "Programming Languages and Systems (APLAS'04), page 196, Taipei, Taiwan, "
        "2004年11月. Springer-Verlag LNCS 3302. Abstract of invited talk.")
    (Li "Frank Pfenning and Robert J. Simmons. "
        "Substructural operational semantics as ordered logic programming. "
        "In Proceedings of the 24th Annual Symposium on Logic in Computer Science "
        "(LICS 2009), pages 101-110, Los Angeles, California, 2009年8月. "
        "IEEE Computer Society Press.")
    (Li "Klaas Pruiksma and Frank Pfenning. Back to futures. "
        "Journal of Functional Programming, 32:e6, 2022.")
    (Li "Robert J. Simmons. Substructural Logical Specifications. "
        "PhD thesis, Carnegie Mellon University, 2012年11月. "
        "Available as Technical Report CMU-CS-12-142.")
    (Li "David Tarditi, Greg Morrisett, Perry Cheng, Chris Stone, Robert Harper, "
        "and Peter Lee. TIL: A type-directed optimizing compiler for ML. "
        "ACM SIGPLAN Notices, 39(4):554-567, 2004."))
   ))
