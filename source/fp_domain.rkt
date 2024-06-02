#lang racket
(provide fp_domain.html)
(require SMathML)
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define :nat (Ms "nat"))
(define :Y (Ms "Y"))
(define :zero (Ms "zero"))
(define :succ (Ms "succ"))
(define (Succ M)
  (app :succ M))
(define (fix σ M)
  (app (_ :Y σ) M))
(define :pred (Ms "pred"))
(define (Pred M)
  (app :pred M))
(define :ifz (Ms "ifz"))
(define (Ifz M1 M2 M3)
  (appl :ifz M1 M2 M3))
(define (Contextualize C)
  (lambda (E)
    (ap C (bra0 E))))
(define ContextC (Contextualize $C))
(define (deno P) (&db0 P))
(define $UnderBar (Mo "&UnderBar;"))
(define (UnderBar x)
  (Munder x $UnderBar))
(define todo.svg
  (Svg
   #:attr* '((width "320")
             (height "160")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Path #:attr* '((x "0")
                   (y "0")
                   (d "M 0 0 h 320 v 160 h -320 z")
                   (fill "none")))
   (Text #:attr* '((x "130") (y "80")) "欠一张图")))
(define (if-then-else a b c)
  (: (Ms "if") a (Ms "then") b
     (Ms "else") c (Ms "fi")))
(define $\| (Mo "|"))
(define $dArr (Mo "&dArr;"))
(define $cong (Mo "&cong;"))
(define Γ $Gamma:normal)
(define Γ^ (&prime $Gamma:normal))
(define Δ $Delta:normal)
(define $vdash (Mo "&vdash;"))
(define (Γ⊢ #:env [env Γ] . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&vdash (apply &cm env b*) (car x*))))
(define $.
  (Mo "." #:attr* '((lspace "0") (rspace "0"))))
(define (Lam var type term)
  (: $lambda (&: var type) $. term))
(define (subst M x N)
  (: M (bra0 (&/ N x))))
(define $::= (Mo "&Colone;"))
(define-infix*
  (&\| $\|)
  (&dArr $dArr)
  (&cong $cong)
  (&vdash $vdash)
  (&::= $::=))
(define-@lized-op*
  (@-> &->)
  (@Lam Lam)
  (@= &=))
(define fp_domain.html
  (TnTmPrelude
   #:title "函数式编程的domain论基础"
   #:css "styles.css"
   (H1. "函数式编程的domain论基础")
   (H2. "前言" #:auto? #f)
   (P "这本小书是我过去十年间为Technical University Darmstadt"
      "的数学系和计算机系学生开设的一门课程的产物. "
      "这门课程的目的在于为想要撰写指称语义领域的硕士论文或者"
      "想要在这个领域开始PhD生涯的学生提供坚实的基础. "
      "对于后一个目的而言, 本书也在University of Birmingham (UK) "
      "由Martin Escard&oacute;的学生成功地运用.")
   (P "因此, 我认为这本小册子很好地满足了预想的目标, "
      "即填补介绍性教科书 (例如[Winskel 1993]) "
      "和指称语义领域的诸多研究性论文之间的沟壑. "
      "我有意专注于基于" (Em "domain论") "的指称语义, "
      "而忽略了近来的" (Em "博弈语义(Game Semantics)")
      " (见[Hyland and Ong 2000; Abramsky et al. 2000]), "
      "在某种意义上其坐落于操作语义和指称语义之间. "
      "这种选择的理由一方面在于博弈语义在[McCusker 1998]"
      "里已经很好地涵盖了, 另一方面在于我发现基于domain的语义"
      "在数学上相比其他方法更加简单, "
      "因为其本质更加抽象而更少组合 (combinatorial). "
      "当然, 这种偏好是有些主观性的, "
      "但是我的借口在于我们写书应该写人们更不熟悉的领域, "
      "而不是写那些已经广为人知的领域.")
   (P "我们建立我们的主题的方式是研究为人熟知的函数式的内核语言PCF, "
      "其由Dana Scott于1960年代末引入. "
      "第2章和第3章中我们设置了本书的场景 (scene), "
      "这两章分别引入了PCF的操作语义和domain语义. "
      "之后我们专注于以越来越精细的" (Em "逻辑关系")
      "技术来研究操作语义和domain语义之间的关系, "
      "而这在第11章和第12章中对于PCF的完全抽象模型的构造里达到顶峰. "
      "我认为我们对于完全抽象模型的构造比既有文献中的记述"
      "更加优雅和简明, 然而当然其也是基于那些文献的. "
      "稍微偏离了主线地, 我们也呈现了如何以Scott domain解释递归类型 (第9章) "
      "以及给出了基于Scott domain对于可计算性的描述 (第13章), "
      "其中我们证明了[Plotkin 1977]的经典定理, "
      
      )
   (H2. "引论")
   (P "函数式编程语言基本上和更为人熟知的命令式语言 "
      "(例如FORTRAN, PASCAL, C, 等等) 一样古老. "
      "最古老的函数式语言是LISP, 其由John McCarthy于1950年代建立, "
      "即基本上和FORTRAN同时. 尽管像FORTRAN这样的"
      (Em "命令式") "或者说" (Em "面向状态") "语言是为了"
      (Em "数值计算") "的目的而建立的, 诸如LISP这样的函数式编程语言"
      "的意图应用领域是 (并且至今仍然是) "
      "对于诸如列表, 树等" (Em "符号性数据") "的算法性操纵.")
   (P "命令式语言的基本构造是修改状态的命令 (例如赋值"
      (&:= $x $E) ") 和命令的条件性迭代 (例如" (Ms "while")
      "循环). 而且, 命令式语言对于诸如数组这样的" (Em "随机访问")
      "数据结构有着强力的支持, 这在数值计算中是重要的.")
   (P "然而, 在" (Em "纯函数式语言")
      "之中, 并不存在状态或者状态改变指称的概念. "
      "其基本概念是"
      (Ul (Li "一个函数之于一个参数的应用")
          (Li "函数的定义, 要么" (Em "显式地")
              " (例如" (&= (app $f $x) (&+ (&* $x $x) $1))
              "), 要么" (Em "递归地") " (例如"
              (&= (app $f $x)
                  (if-then-else
                   (&= $x $0)
                   $1
                   (&* $x (app $f (&- $x $1)))))
              ")."))
      "这些例子表明除了除了函数的应用和定义, "
      "我们还需要基本数据类型 (例如自然数或者布尔) "
      "上的基本操作以及用于分类讨论定义的条件式. "
      "而且, 所有常见的函数式编程语言 "
      "(例如LISP, Scheme, (S)ML, Haskell, 等等) "
      "都提供了通过显式列举构造子来定义"
      (Em "递归数据类型") "的机制, "
      "例如在以下对于二叉树的数据类型的定义里"
      (MB (&= (Ms "tree")
              (&\| (appl (Ms "empty"))
                   (appl (Ms "mk_tree")
                         (Ms "tree") (Ms "tree")))))
      (Ms "empty") "是对于没有儿子的空树的一个" $0
      "元构造子, 而" (Ms "mk_tree")
      "是一个二元构造子, 其取两个树" $t_1 "和" $t_2
      ", 然后构造一个新的树, 其根的左右儿子分别是"
      $t_1 "和" $t_2 ". 因此, 函数式语言不仅支持函数的递归定义, "
      "也支持数据类型的递归定义. "
      "后一个特点应该被认为是相较于PASCAL这样的命令式语言的一大优势, "
      "其中的递归数据类型需要通过指针来实现, "
      "这是精细的工作, 也是引入难以消除的微妙错误的一个源头.")
   (P "命令式程序开发的典型方法是设计" (Em "流程图(flow chart)")
      ", 其描述并可视化了程序的" (Em "动态行为")
      ". 因此, 命令式语言编程的主要任务是组合"
      (Em "复杂的动态行为") ", 也就是所谓的"
      (Em "控制流") ".")
   (P "然而, 在函数式编程语言里, 程序的动态行为无需显式描述. "
      "转而, 人们只需要" (Em "定义") "要被实现的函数. "
      "当然, 在实践中, 这些函数定义是相当层次性的, "
      "即基于一整个预先定义的辅助函数的级联. "
      "然后, 一个" (Em "程序") " (与一个函数定义相对) "
      "通常具有应用" (appl $f $e_1 $..h $e_n)
      "的形式, 其会由解释器" (Em "求值")
      ". 鉴于在函数式语言中编程基本上由定义函数 "
      "(显式地或者递归地) 构成, "
      "我们无需担心执行的动态方面, "
      "因为这项任务被解释器完全接管了. "
      "因此, 在函数式语言中编程时我们可以专注于"
      (Em "what") "而忘记" (Em "how")
      ". 然而, 当在函数式编程语言中定义函数时, "
      "我们不得不拘泥于 (stick to) 由语言所提供的"
      (Em "定义形式")
      ", 而不能使用日常数学中通常的集合论语言.")
   (P "在这个课程讲义之中, 我们将分别就以下三个方面"
      "探究函数式的(内核)语言."
      todo.svg
      "或者"
      todo.svg
      "特别是, 这些方面时如何交互的.")
   (P "首先, 我们将会引入一个最简单的函数式编程语言, "
      "即带有自然数作为基类型但没有一般性的递归类型的"
      "PCF (Programming Computable Functionals).")
   (P "PCF的" (Em "操作语义") "将由一个" (Em "归纳")
      "定义的" (Em "求值关系")
      (MB (&dArr $E $V))
      "给出, 其刻画了哪个表达式" $E "会" (Em "求值")
      "至哪个" (Em "值") ", 而值是不能被进一步求值的特定表达式. "
      "例如, 如果" (&dArr $E $V) "而" $E
      "是一个具有自然数类型" :nat
      "的封闭项, 那么" $V "会是一个具有形式"
      (UnderBar $n) "的表达式, 即自然数" $n
      "的一个canonical表达式 (通常其被称为"
      (Em "数码(numeral)") "). 实际上求值关系" $dArr
      "应该具有一个性质, 即每当" (&dArr $E $V_1) "且"
      (&dArr $E $V_2) ", 那么" (&= $V_1 $V_2)
      ". 这意味着关系" $dArr "是" (Em "确定性") "的, 意即"
      $dArr "分配给一个给定的表达式" $E
      "以至多一个值. 由(归纳定义的)求值关系" $dArr
      "所给出的操作语义通常被称为" (Q "大步语义")
      ", 因为其抽象掉了由" $E "得到" $V "的计算的中间步骤. "
      "我们注意到一般情况下对于任意的表达式" $E
      ", 可能并不存在一个值" $V "使得" (&dArr $E $V)
      ", 即不是每个程序都会终止. "
      "{译注: 其实还有另外的可能, 例如产生了运行时异常, "
      "其并非类型错误.} "
      "这 {译注: 指不能保证终止} 是由于" (Em "一般递归")
      "在我们语言PCF中的存在, 其保证了" (Em "所有")
      "的可计算函数都可以由PCF程序所表达.")
   (P "基于由" $dArr "所给出的PCF的大步语义, "
      "我们将会引入具有相同类型的封闭PCF表达式的"
      (Em "观察性相等(observational equality)")
      "的概念, 其中" $E_1 "和" $E_2
      "被认为是观察性相等的当且仅当对于所有具有基类型"
      :nat "的上下文" (ContextC $) ", 都有"
      (MB (&<=> (&dArr (ContextC $E_1)
                       (UnderBar $n))
                (&dArr (ContextC $E_2)
                       (UnderBar $n))))
      "对于每个自然数" (∈ $n $NN)
      "成立. 从直觉上来说, 表达式" $E_1 "和" $E_2
      "是观察性相等的当且仅当对于" $E_1 "和" $E_2
      "可以作出相同的观察, 其中一个对于" $E "的"
      (Em "观察") "由观察到对于某个具有基类型"
      :nat "的上下文" (ContextC $)
      "和某个自然数" $n "有"
      (&dArr (ContextC $E) (UnderBar $n))
      ". 这种观察的概念是对于以下通常实践的数学形式化, "
      "即对于程序进行测试并秉持着程序是(观察性)相等的"
      "当且仅当其通过了相同的测试这一观念.")
   (P "然而, 这个观察性相等的概念并不容易使用, "
      "因为其牵涉所有上下文上的量化, "
      "而这所有上下文构成的集合并不容易抓住 (grasp). "
      "因此, 我们想要找到更为方便的判则, "
      "其可以用来推出观察性相等, "
      "但又避免了诉诸于(有些复杂的)句法概念, "
      "例如求值关系和上下文.")
   (P "为此目的, 我们引入了所谓的PCF的"
      (Em "指称语义") ", 其为每个具有类型" $sigma
      "的封闭表达式" $E "赋予一个元素"
      (∈ (deno $E) $D_sigma)
      ", 其被称为" $E "的" (Em "指称")
      "或者" (Em "意义") "或者" (Em "语义")
      ", 其中" $D_sigma "是一个预先定义的结构化集合 "
      "(称为" (Q "语义domain")
      "), 在这里面具有类型" $sigma
      "的封闭表达式可以找到它们的解释.")
   (P "指称语义的想法于1960年代末由Christopher Strachey和"
      "Dana Scott引入. 当然了, 一个自然的问题是"
      "我们应该给语义domain施加什么样的数学结构性质. "
      "尽管实际上语义domain作为特定的拓扑空间来考虑是合适的, "
      "然而其又和分析学和几何学中出现的拓扑空间在风味上相当不同. "
      "{原注: 特别地, 语义domain不是Hausdorff空间.} "
      "合适的语义domain概念由Dana Scott引入, "
      "其也发展了语义domain的基本数学理论到相当成熟的程度. "
      "自1970年代初起, 全世界的诸多研究小组都在发展"
      "语义domain的理论上投入了相当的能量, "
      "自那时起这个学科被简称为" (Em "domain论")
      ", 其既可以从纯粹数学的观点出发, "
      "也可以从计算机科学的观点出发, "
      "作为编程语言的语义理论的一种.")
   (P "尽管之后还将远为细致地进行讨论, "
      "现在我们将给出预备性的陈述, "
      "关于如何构造domain " $D_sigma
      ", 其中具有类型" $sigma
      "的封闭项可以找到它们的指称. "
      "对于自然数类型" :nat ", 我们置"
      (&= (_ $D :nat) (&union $NN (setE $bottom)))
      ", 其中的" $bottom " (称为" (Q "底 (bottom)")
      ") 代表具有类型" :nat
      "的项中那些求值" (Q "发散 (diverge)")
      "的项的指称, 发散意即并不终止. "
      "{译注: 正如我之前已经提到的, 一般情况下抛出异常什么的也是发散.} "
      "我们将" (_ $D :nat) "视为装备了"
      (Q "信息序") ", 相对于这个序" $bottom "是最小元素, "
      "而其他的元素 (即自然数) 之间均不可比较. "
      "{译注: 这构成了所谓的扁平格 (flat lattice).} "
      "PCF的类型从基类型" :nat
      "开始由二元构成运算符" $-> "构筑而成, 其中"
      (_ $D (&-> $sigma $tau))
      "被认为是从" $D_sigma "到" $D_tau
      "的(可计算或者说连续)函数的类型 "
      "{译注: 我怀疑这里是笔误, 其实是domain而非type}, 即"
      (&sube (_ $D (&-> $sigma $tau))
             (&= (^ $D_tau $D_sigma)
                 (setI $f
                       (&: $f (&-> $D_sigma $D_tau)))))
      ". 特别地, domain " (_ $D (&-> :nat :nat))
      "由从" (_ $D :nat) "到自身的特定函数构成. "
      
      )
   (H2. "PCF及其操作语义")
   (P "本章我们引入原型性质的函数式编程语言PCF及其操作语义.")
   (P "PCF是一个有类型的语言, 其类型的集合" (Ms "Type")
      "归纳定义如下"
      (Ul (Li "基类型" :nat "是一个类型, 而")
          (Li "每当" $sigma "和" $tau "是类型, 那么"
              (@-> $sigma $tau) "也是一个类型."))
      "我们经常将基类型" :nat "记为" $iota
      ", 而记" (@-> $sigma $tau) "为"
      (&-> $sigma $tau) ", 其中" $->
      "被理解为一个" (Ms "Type") "上的右结合二元运算, 例如"
      (&-> $sigma_1 $sigma_2 $sigma_3) "应该读作"
      (@-> $sigma_1 (@-> $sigma_2 $sigma_3))
      ". 根据" (Ms "Type") "的归纳定义, 每个类型" $sigma
      "都以唯一的方式具有形式"
      (&-> $sigma_1 $..c $sigma_n $iota) ".")
   (P "鉴于PCF的项可能包含自由变量, 我们将相对于"
      (Em "类型上下文(type context)")
      "来定义项, 在类型上下文之中有限多个变量和其类型一起声明, "
      "即类型上下文是具有形式"
      (MB (&equiv Γ
                  (&cm (&: $x_1 $sigma_1) $..h
                       (&: $x_n $sigma_n))))
      "的表达式, 其中" $sigma_i "是类型而" $x_i
      "是两两互异的变量. 因为变量无法出现于类型表达式之中, 那么"
      Γ "之中单个变量声明" (&: $x_i $sigma_i)
      "的顺序就是不重要的. 据此, 如果" Γ^ "是由" Γ
      "通过置换得到的, 那么我们就将它们视为等同的.")
   (P "具有形式"
      (MB (&vdash Γ (&: $M $sigma)))
      "的合法判断在图2.1中由归纳定义, 其意即"
      $M "在上下文" Γ "之中是一个具有类型"
      $sigma "的项.")
   (Table
    #:attr* '((align "center")
              (style "border-spacing: 3em 1em;"))
    (Tr (Td (&rule (Γ⊢ (&: $x $sigma) Δ (&: $x $sigma))))
        (Td (&rule (Γ⊢ (&: $x $sigma) (&: $M $tau))
                   (Γ⊢ (&: (@Lam $x $sigma $M) (&-> $sigma $tau))))))
    (Tr (Td (&rule (Γ⊢ (&: $M (&-> $sigma $tau)))
                   (Γ⊢ (&: $N $sigma))
                   (Γ⊢ (&: (app $M $N) $tau))))
        (Td (&rule (Γ⊢ (&: $M (&-> $sigma $sigma)))
                   (Γ⊢ (&: (fix $sigma $M) $sigma)))))
    (Tr (Td (&rule (Γ⊢ (&: :zero :nat))))
        (Td (&rule (Γ⊢ (&: $M :nat))
                   (Γ⊢ (&: (Succ $M) :nat)))))
    (Tr (Td (&rule (Γ⊢ (&: $M :nat))
                   (Γ⊢ (&: (Pred $M) :nat))))
        (Td (&rule (Γ⊢ (: (&: $M_i :nat) (&space 4)
                          (@= $i (&cm $1 $2 $3))))
                   (Γ⊢ (&: (Ifz $M_1 $M_2 $M_3) :nat)))))
    (Tr (Td #:attr* '((colspan "2") (align "center"))
            "图2.1 PCF的定型规则")))
   (P "根据推导结构上的归纳, 我们很容易证明每当"
      (&vdash Γ (&: $M $sigma)) "可以被推导出来, 那么"
      (&vdash (app $pi Γ) (&: $M $sigma))
      "也可以被推导出来, 其中" $pi "是任意对于"
      Γ "的置换.")
   (P "鉴于每个PCF的语言构造只有一条定型规则与之对应, "
      "那么我们很容易证明" (&vdash Γ (&: $M $sigma))
      "中的" $sigma "由" Γ "和" $sigma
      "唯一确定 (练习!). "
      "{译注: 唯一确定意即如果" (&vdash Γ (&: $M $sigma_1))
      "和" (&vdash Γ (&: $M $sigma_2))
      "都是可推导的, 那么" (&= $sigma_1 $sigma_2)
      ".} 因此, 反向应用定型规则产生了一个递归的"
      (Em "类型检查(type checking)")
      "算法, 其给出" $M "和" Γ "即可计算满足"
      (&vdash Γ (&: $M $sigma)) "的类型"
      $sigma ", 只要这个类型的确存在, 否则的话也能够报错. "
      "{译注: 满足" (&vdash Γ (&: $M $sigma))
      "的意思是" (&vdash Γ (&: $M $sigma)) "是可推导的. "
      "也就是说, 如果存在类型" $sigma "使得"
      (&vdash Γ (&: $M $sigma))
      "是可推导的, 那么算法就能计算出这个类型, 并且我们还知道这个类型是唯一的, "
      "不存在这种类型的话算法也能够主动报错.} "
      "(我们邀请读者对于一些简单的例子测试这个算法!)")
   (P "接下来我们不拘泥于PCF项的" (Q "官方 (offcial)")
      "句法. 经常我们记" (ap $M $N) "或" (@ap $M $N)
      "而非" (app $M $N) ". 为了与" $->
      "的右结合性保持一致, 我们将由并置给出的应用视为左结合的, "
      "意即" (: $M_1 $..h $M_n) "应该读作"
      (@ $..h (@ap $M_1 $M_2) $..h $M_n) "或者"
      (app (ap (app $M_1 $M_2) $..h) $M_n) ".")
   (P "对于由" $lambda "所绑定的变量, 我们采取通常的"
      (Em $alpha "转换") "的约定, 即项被认为是相等的, "
      "如果它们互相可以通过对于绑定变量进行适当换名得到. "
      "{译注: 换言之, 更技术性地说, 在某种意义上我们要考虑的是"
      $lambda "项的" $alpha "等价类.} "
      "而且, 当替换项" $M "中的变量" $x "为项" $N
      "时, 我们首先会对于" $M "的绑定变量进行换名以使得"
      $N "的自由变量不会被" $M "中的" (Em "lambda抽象")
      "绑定, 也就是说我们所用的是"
      (Em "无捕获替换(capture-free substitution)") ".")
   (P "在我们定义PCF的操作语义之前, 我们先引入PCF的"
      (Q "原项 (raw term)") "的概念, 根据以下的BNF语法:"
      (MB (&::= $M (&\| $x
                        (@Lam $x $sigma $M)
                        (app $M $M)
                        :zero
                        (Succ $M)
                        (Pred $M)
                        (Ifz $M $M $M))))
      "当然了, 不是每个原项都可定型, 例如"
      (Lam $x :nat (app $x $x))
      ", 其中第一个" $x "的出现显然应该具有函数类型以使得"
      (app $x $x) "是良类型的.")
   (P "现在我们呈现PCF的一个" (Q "大步")
      "语义, 其由一个原项上的二元关系" $dArr
      "给出, 而这个关系是根据图2.2中的规则归纳定义的. "
      "这里的" (UnderBar $n) "是自然数" $n
      "的canonical" (Em "数码")
      ", 其被递归地定义为" (&equiv (UnderBar $0) :zero) "而"
      (&equiv (UnderBar (&+ $k $1)) (Succ (UnderBar $k))) ".")
   (Table
    #:attr* '((align "center")
              (style "border-spacing: 3em 1em;"))
    (Tr (Td (&rule (&dArr $x $x)))
        (Td (&rule (&dArr (Lam $x $sigma $M) (Lam $x $sigma $M)))))
    (Tr (Td (&rule (&dArr $M (Lam $x $sigma $E))
                   (&dArr (subst $E $x $N) $V)
                   (&dArr (app $M $N) $V)))
        (Td (&rule (&dArr (app $M (fix $sigma $M)) $V)
                   (&dArr (fix $sigma $M) $V))))
    (Tr (Td (&rule (&dArr (UnderBar $0) (UnderBar $0))))
        (Td (&rule (&dArr $M (UnderBar $n))
                   (&dArr (Succ $M) (UnderBar (&+ $n $1))))))
    (Tr (Td (&rule (&dArr $M (UnderBar $0))
                   (&dArr (Pred $M) (UnderBar $0))))
        (Td (&rule (&dArr $M (UnderBar (&+ $n $1)))
                   (&dArr (Pred $M) (UnderBar $n)))))
    (Tr (Td (&rule (&dArr $M (UnderBar $0))
                   (&dArr $M_1 $V)
                   (&dArr (Ifz $M $M_1 $M_2) $V)))
        (Td (&rule (&dArr $M (UnderBar (&+ $n $1)))
                   (&dArr $M_2 $V)
                   (&dArr (Ifz $M $M_1 $M_2) $V))))
    (Tr (Td #:attr* '((colspan "2") (align "center"))
            "图2.2 PCF的大步语义")))
   (P "每当" (&dArr $E $V) ", 那么" $V
      "是一个变量, 数码, 或者" $lambda "抽象. "
      "根据推导" (&dArr $E $V) "的结构上的归纳, "
      $V "的自由变量包含于" $E "的自由变量之中. "
      "因此, 如果" $E "是一个封闭表达式, 那么"
      $V "要么是一个数码, 要么是一个没有自由变量的"
      $lambda "抽象. 这样的项被称为(句法)"
      (Em "值") ", 并且很容易看出对于每个这样的值"
      $V "我们都有" (&dArr $V $V) ". "
      )
   (H2. "PCF的Scott模型")
   (H3. "基本的domain论")
   (H3. "PCF的domain模型")
   (H3. "LCF&mdash;&mdash;可计算函数逻辑")
   ))