#lang racket
(provide hot.html)
(require SMathML)
(define &split16 (&split 16))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . j*)
  (if label
      (: (apply &rule j*) label)
      (apply &rule j*)))
(define $UnderBar (Mo "&UnderBar;"))
(define (UnderBar x)
  (__ x $UnderBar))
(define (Miv str variant)
  (Mi str #:attr* `((mathvariant ,variant))))
(define (Label str)
  (UnderBar (Miv str "sans-serif")))
(define-syntax-rule (define-label* (id str) ...)
  (begin
    (define id (Label str))
    ...))
(define-label*
  ($inl "inl")
  ($inr "inr")
  ($zero "zero")
  ($succ "succ")
  ($false "false")
  ($true "true")
  ($nil "nil")
  ($cons "cons"))
(define (&zero) (ap $zero (tu0)))
(define (&succ nat) (app $succ nat))
(define $⊗ (Mo "&otimes;"))
(define $⊗:id (Mi "&otimes;"))
(define $⊗I (: $⊗:id $I))
(define $unit (Miv "1" "bold"))
(define $unit0 (Miv "unit" "bold"))
(define $unitI (: $unit $I))
(define $⊕ (Mo "&oplus;"))
(define $⊕:id (Mi "&oplus;"))
(define $⊕I (: $⊕:id $I))
(define $⊕I_1 (: $⊕:id $I_1))
(define $⊕I_2 (: $⊕:id $I_2))
(define (⊕ . x*)
  (: $⊕:id (apply setE (map2 &: x*))))
(define $def= (^^ $= $Delta:normal))
(define $. (Mo "."))
(define $type (Miv "type" "bold"))
(define $nat (Miv "nat" "sans-serif"))
(define $list (Miv "list" "sans-serif"))
(define $space2 (&space 2))
(define (&type id de)
  (: $type $space2
     (&= id de)))
(define-infix*
  (&def= $def=)
  (&disj $disj)
  (&⊗ $⊗)
  (&⊕ $⊕))
(define-@lized-op*
  (@: &:))
(define (μ a A)
  (: $mu a $. A))
(define hot.html
  (TnTmPrelude
   #:title "HOT编译"
   #:css "styles.css"
   (H1. "HOT编译")
   (H2. "Sax: 一种命令式中间语言")
   (H3. "引论")
   (P "编写编译器的关键方面之一在于设计表示源语言和目标语言之间的步骤的合适"
      (Em "中间语言") ". 源语言, 中间语言, 目标语言在整个学期将会逐渐演化, "
      "因为你将会为愈来愈复杂的语言编写一系列编译器. "
      "第一次lab的目标在于让你熟悉中间语言; "
      "第二次lab的目标在于如何转换进和出这种语言.")
   (P "所有 (或者应该说几乎所有?) 的编译器都依赖于中间语言. "
      "在CMU 15-411这门课里, 常用的中间语言是SSA或者LLVM. "
      
      )
   (H3. "正类型与大值")
   (P "从使用者角度而言, 正类型的值是那些我们可以直接观察结构的值, "
      "其包括布尔值, 自然数, 或者是元素具有可观察类型的列表或树. "
      "反过来, 负类型的值是那些我们" (B "不能")
      "直接观察结构但是可以与之进行交互的值, 例如函数和对象.")
   (P "编程语言理论已经建立了对于类型的构建块的基本理解. "
      "与之对应的是, 证明论有着同样的命题的构建块. "
      "我们将几乎完全坚持编程语言的视角, "
      "但是有时会将其和直觉主义(线性)命题联系起来.")
   (P "我们关于对于大值进行定型的基本判断是"
      (MB (&: $V $A))
      "其表达了大值" $V "具有类型" $A
      ". 我们所观察的大值并不包含任何变量, "
      "因而我们无需使用任何上下文来描述其类型. "
      "之所以我们将这些值称为" (Em "大的")
      ", 是因为一个单独的值就包含了关于计算输出结果的所有信息.")
   (P (B "序对. ")
      "对于正类型序对我们记" (&⊗ $A $B) "."
      (MB (&rull
           $⊗I
           (&: $V $A) (&: $W $B)
           (&: (tu0 $V $W) (&⊗ $A $B))))
      "这个记号是借用自线性逻辑的, 规则命名中的"
      $I "指示" (Em "引入规则")
      "也是如此. 在直觉主义逻辑中, 这是两种合取形式之一. "
      "在具有按值调用的函数式语言 (ML语族) 中, 其被记为"
      (Code "A * B") ", 这也是我们实际所采用的具体句法.")
   (P (B "单位. ")
      "对于单位类型我们将其记为" $unit
      ", 其中只寓居着一个单位元素."
      (MB (&rull
           $unitI
           (&: (tu0) $unit)))
      "以常见的语言, 单位类型通常记作" $unit0
      "; 对于我们的具体句法我们就记作" (Code "1") ".")
   (P (B "和. ")
      "对于类型" $A_l "的无交和我们记作"
      (sum (∈ $l $L) (@: $l $A_l))
      ", 其中" (Em "标签") $l "是从一个有限索引集合"
      (&!= $L (setE)) "中取的. 在线性逻辑中, 二元的版本记作"
      (&⊕ $A $B) ", 其与直觉主义逻辑中的" (&disj $A $B)
      "是相同的. 为了简单起见, 我们避免了空和. "
      "另外, 因为空和并不包含值, 故其编程应用有限. "
      "我们一般使用" $l "和" $k "代表标签."
      (MB (&split16
           (&rull
            $⊕I
            (@ (∈ $k $L)) (&: $V $A_k)
            (&: (app $k $V)
                (sum (∈ $l $L) (@: $l $A_l))))
           (brac
            (&split16
             (&rull
              $⊕I_1
              (&: $V $A)
              (&: (app $inl $V)
                  (&⊕ $A $B)))
             (&rull
              $⊕I_2
              (&: $V $B)
              (&: (app $inr $V)
                  (&⊕ $A $B)))))))
      "右边我们展示了二元版本这一特殊情形, 其中"
      (&def= (&⊕ $A $B)
             (&+ (@: $inl $A)
                 (@: $inr $B)))
      ". 它们并非真的是语言的组成部分.")
   (P "以这些类型构造子, 我们已经可以表示诸如布尔"
      (&+ (@: $false $unit) (@: $true $unit))
      "这样的有限类型. "
      "为了表示诸如列表和自然数这样的无限类型, "
      "我们也需要递归.")
   (P (B "等价递归类型 (Equirecursive Types). ")
      "我们没有添加用于构造递归类型的绑定构造子"
      (μ $alpha (app $A $alpha))
      ", 而是在顶层定义之中表示递归类型, 例如"
      (MB (&type $nat
                 (&⊕ (@: $zero $unit)
                     (@: $succ $nat))))
      "{译注: 不要将其与二元和" (&⊕ $A $B)
      "混淆, 这个其实是和类型的某种具体句法.} "
      "从现在开始, 我们对于和采用一种更贴近于我们的具体句法的记号."
      (MB (&type $nat
                 (⊕ $zero $unit
                    $succ $nat)))
      "我们称这些类型为" (Em "等价递归的") "是什么意思? "
      "这意味着我们将这种定义视为元层次的" (Em "等式")
      ", 而非需要强制 (coercion) 的" (Em "同构")
      ". 下一次讲座将会看到这种简洁性伴随着代价, "
      "但是我相信这是值得的.")
   (P "因为类型定义是等价递归的, 所以我们有着判断诸如"
      (MB (&: (&succ (&succ (&zero))) $nat))
      "对于" $nat "这样的类型我们也没有单独的类型规则, "
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
      ") 开始. 这避免了形式为" (&type $t $t) "的定义.")
   (P "而且, 我们应该允许类型定义为互递归的, "
      "假如我们使用显式类型构造子" $mu
      "的话可能不太令人愉快了.")
   (P "作为另一个例子, 自然数的列表可以定义为"
      (MB (&type $list
                 (⊕ $nil $unit
                    $cons (&⊗ $nat $list)))))
   (H3. "内存和小值")
   (P "到目前为止我们知道了我们如何表示大值, "
      "但是我们也想要以某种方式在内存中表示大值, "
      "以使得我们能够命令式地对于它们进行操作. "
      "然而, 一个内存单元 (memory cell) "
      "不能存放具有任意大小的值, "
      "因而我们需要将大值分解为小的部分. "
      
      )
   (H3. "对于内存进行定型")
   (H3. "从内存中读取")
   (H2. "线性类型检查")
   (H2. "线性自然演绎")
   (H2. "编译")
   (H2. "求值")
   (H2. "优化")
   (H2. "负类型")
   (H2. "闭包")
   (H2. "闭包变换")
   (H2. "伴随类型")
   
   ))