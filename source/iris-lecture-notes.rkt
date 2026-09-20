#lang racket
(provide iris-lecture-notes.html)
(require SMathML)
(define $ell (Mi "&ell;"))
(define $ref (Mi "ref"))
(define $conc (Mi "conc"))
(define |$lambda_ref,conc|
  (_cm $lambda $ref $conc))
(define (RLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define (subst t . p*)
  (: t (bra0 (apply &cm (map2 &/ p*)))))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define $Persistent (Mo "&square;"))
(define (Persistent P)
  (ap $Persistent P))
(define $Later (Mo "▷"))
(define (Later P)
  (ap $Later P))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define (Lam x t)
  (: $lambda x $. t))
(define split2 (&split 2))
(define split16 (&split 16))
(define App (&split 2))
(define $Var (Mi "Var"))
(define $Val (Mi "Val"))
(define $Expr (Mi "Expr"))
(define $Prop (Mi "Prop"))
(define $unitType (Mi "1"))
(define $unit (tu0))
(define $inl (Mi "inl"))
(define $inr (Mi "inr"))
(define (Inl t) (ap $inl t))
(define (Inr t) (ap $inr t))
(define $case (Mi "case"))
(define (Case t c1 c2)
  (appl $case t c1 c2))
(define $False (Mi "False"))
(define $True (Mi "True"))
(define $impl $=>)
(define $wand
  (Mo "&minus;&#8270;"))
(define $CAS (Pid "CAS"))
(define (CAS e1 e2 e3)
  (appl $CAS e1 e2 e3))
(define $fork (Pid "fork"))
(define $let (Pid "let"))
(define $in (Pid "in"))
(define (Let x e body)
  (split2 $let (&:= x e) $in body))
(define $rec (Pid "rec"))
(define (Rec f x e)
  (split2 $rec (&:= (App f x) e)))
(define (Fork e)
  (ap $fork (cur0 e)))
(define $loop (Pid "loop"))
(define $def Δ)
(define $def= (^^ $= $def))
(define eq
  (case-lambda
    ((t1 t2) (eq t1 $tau t2))
    ((t1 τ t2) (: t1 (_ $= τ) t2))))
(define (Hoare0 P t Q)
  (split2 (cur0 P) t (cur0 Q)))
(define PointsTo &\|->)
(define $!- $vdash)
(define $!-_S (_ $!- $S:script))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define-infix*
  (&def= $def=)
  (&wand $wand)
  (&impl $impl)
  (Bind $.))
(define-@lized-op*
  (@Lam Lam))
(define iris-lecture-notes.html
  (TnTmPrelude
   #:title "iris讲义"
   #:css "styles.css"
   (H1. "iris讲义")
   (H2 "前言")
   (P "这些讲义旨在介绍Iris. "
      "Iris是一个高阶并发分离逻辑框架, "
      "它在Coq证明助手中实现并经过验证.")
   (P "Iris已有多年的发展历史, 起初是两个团队的联合研究: "
      "一个是Aarhus大学由Lars Birkedal领导的逻辑与语义研究组, "
      "另一个是Max Planck软件系统研究所由Derek Dreyer领导的编程基础研究组. "
      "近来, 其他几个国际研究组也参与了开发, "
      "尤其是TU Delft的Robbert Krebbers研究组.")
   (P "描述Iris程序逻辑框架的主要研究论文包括三篇会议论文 [9, 7, 10], "
      "以及一篇篇幅更长的期刊论文 [8]. "
      "期刊论文更详细地介绍了该逻辑的语义和最新进展. "
      "这些论文和其他几篇与Iris相关的研究论文都可以在Iris项目网站上找到:"
      (Blockquote "iris-project.org")
      "在该网站上也可以获取Iris的Coq实现.")
   (P (B "设计选择 ")
      "应该如何介绍Iris这样复杂的逻辑框架, 并不是一个显而易见的问题. "
      "尤其是因为Iris在不止一种意义上是一个框架: "
      "首先, Iris可以被实例化, "
      "用来对不同编程语言编写的程序进行推理; "
      "其次, Iris有一个基础逻辑, "
      "可以用来定义各种程序逻辑和关系模型. "
      "下面我们说明这些讲义中的一些设计选择.")
   (P "这些讲义面向没有任何程序逻辑基础的学生. "
      "因此我们从零开始, 并专注于Iris的一个特定实例化, "
      "用来对一个核心的并发高阶命令式编程语言"
      |$lambda_ref,conc| "进行推理. "
      "(正如Martin Hyland曾经说过的[5]: "
      (Q "一个好的例子胜过一大堆泛泛之谈") ".)")
   (P "我们从Hoare三元组及其证明规则等高层概念讲起. "
      "随着引入的概念逐渐增多, 我们会展示一开始"
      "作为公设给出的证明规则如何从更简单的概念推导出来. "
      "此外, 每个新的逻辑概念都会配有具体的验证示例, "
      "不过这些示例往往是人为构造的. "
      "讲义还包含一些较大的案例研究, "
      "用来说明该逻辑可以用于验证实际的程序. "
      "在此提醒读者: 讲义的开头部分, "
      "大约到第4章为止, 相当形式化和抽象. "
      "请不要因此气馁. 这一部分是必要的, 它用来确定记号, "
      "并解释后面具体程序验证示例中所用推理的基本结构.")
   (P "由于Iris逻辑涉及若干新的逻辑模态和联结词, "
      "我们在给出程序的示例证明时采用了相当详细的风格, "
      "而不是常用的证明概要 (proof outline). "
      "我们希望这能帮助读者理解该逻辑中新颖的部分是如何运作的.")
   (P "我们收录了大量难度不一的练习. "
      "有些练习会引入后文用到的推理原则. "
      "因此, 练习是讲义不可分割的一部分, 不应跳过.")
   (P "在介绍逻辑时, 我们只用直观的语义来解释证明规则为何是可靠的 (sound). "
      "关于Iris模型的详尽描述, 目前请读者参阅研究论文 [8]. "
      "这样选择有几个原因: 第一, 形式语义并不简单 "
      "(例如, 它涉及递归域方程的求解); "
      "第二, 语义实际上是为基础逻辑定义的, "
      "而基础逻辑要到讲义后面才会引入; "
      "第三, 根据我们用这些讲义讲授课程的经验, "
      "学生不需要接触该逻辑的形式语义也能学会使用它.")
   (P "既然Iris有Coq实现, 也许有人会想从一开始就借助Coq实现来讲授Iris. "
      "但我们决定不这样做. 原因是我们的学生缺乏足够的Coq经验, 这种方式并不可行. "
      "而且我们认为, 对大多数读者来说, 这样做需要同时学习的东西太多了. "
      "讲义中确实有一节介绍Coq实现, 并且会讲解使用Coq实现所需的Iris的所有部分. "
      "讲义中的示例都已在Iris的Coq实现中形式化, 可以在Iris项目网站上获取.")
   (P "我们没有试图引用原始研究论文, 也没有加入历史评述. "
      "有关早期工作的参考文献, 请参阅Iris的研究论文.")
   (P (B "致谢 ")
      "我们感谢Aarhus大学程序分析与验证 "
      "(Program Analysis and Verification) "
      "课程的学生对这些讲义早期版本提出的反馈. "
      "我们感谢Ambal Guillaume, Jonas Kastberg Hinrichsen, "
      "Marianna Rapoport和Lily Tsai提出的宝贵意见.")
   (H2. "引论")
   (P "这些讲义的目标是介绍一个强大的逻辑, 名为Iris, "
      "用于证明并发高阶命令式程序的部分函数正确性 "
      "(partial functional correctness). "
      "部分正确性 (partial correctness) 的含义是: "
      "当一个程序被证明满足某个规约时, "
      "这只能保证如果程序终止, 那么其结果满足所述性质. "
      "如果程序不终止, 规约对其行为不作任何断言, "
      "只保证它不会卡住 (get stuck). "
      "(知道一个无限计算不会卡住也是有用的: "
      "这意味着程序是安全的, 特别是不会出现内存错误, "
      "例如试图读取内存中不存在的位置.)")
   (P "Iris是一个高阶逻辑. 这意味着程序规约可以由任意命题参数化. "
      "这样做的一个主要好处是, 高阶逻辑规约的一般性支持模块化: "
      "库和模块可以一次性地给出规约并证明正确, "
      "而该库的不同客户端(或使用者)可以各自独立地验证, "
      "只需用到它们所使用的库的规约, 而不需要库的实现.")
   (P "Iris借助所谓的幽灵状态 (ghost state) "
      "和不变式 (invariant) 来支持并发程序的验证. "
      "不变式这一机制允许不同的程序线程访问共享资源, "
      "例如读写同一个位置, 前提是它们不会破坏其他线程所依赖的性质, "
      "也就是说, 前提是它们维持不变式. "
      "幽灵状态这一机制允许不变式随时间演化. "
      "它让逻辑能够记录一些额外的信息. "
      "这些信息不出现在被验证的程序代码中, "
      "但对于证明程序的正确性却是必不可少的, "
      "例如不同程序变量的值之间的关系.")
   (P "在这些讲义中, 我们逐步介绍Iris: "
      "先给出推理简单顺序程序所需的基本要素, "
      "再不断细化和扩展, 直到该逻辑能够推理"
      "高阶, 并发, 命令式的程序. "
      "之后我们会展示如何把该逻辑简化为一个最小的基础逻辑, "
      "在其中之前用到的所有规则都可以作为定理推导出来.")
   (P "用来引入和解释逻辑规则的示例通常是最小化的, 也有些刻意构造. "
      "不过讲义也在单独的章节中包含了一些较大的案例研究, "
      "说明该逻辑同样可以用于验证和推理更大, 更贴近实际的程序. "
      "这些案例研究也更清楚地展示了该逻辑的模块化. "
      "更多案例研究可以在Iris项目主页上找到.")
   (H2. "编程语言")
   (P "程序逻辑用于对程序进行推理, 也就是刻画程序的行为. "
      "逻辑的具体规则取决于编程语言中有哪些构造. "
      "Iris实际上是一个框架, 可以被实例化到多种不同的编程语言, "
      "但为了学习Iris, 最好先在某一种特定的编程语言上积累一些经验. "
      "因此在本节中, 我们确定一门具体的编程语言, 并在整个讲义中使用它. "
      "我们选择的语言记作" |$lambda_ref,conc|
      ", 它是一门无类型的高阶类ML语言, "
      "支持一般引用 (general references) 和并发. "
      "并发通过两个原语来支持: 一个是" (Fork $e)
      ", 用于派生一个新线程来计算" $e
      "; 另一个是比较并设置 (compare and set) 操作"
      $CAS ". " $CAS "是该语言中唯一的同步原语. "
      "其他同步构造, 例如锁, 信号量等, 都可以在该语言中定义. "
      "事实上, 我们会实现两种不同的锁并给出它们的规约. "
      "语言的句法和操作语义如图1所示.")
   (P (B "句法糖 ")
      "除了给定的构造之外, 我们在编写示例时还会使用一些额外的句法糖. 我们用"
      (Lam $x $e) "表示项" (Rec $f $x $e)
      ", 其中" $f "是某个不在" $e "中出现的新鲜变量. 因此"
      (Lam $x $e) "是一个参数为" $x ", 函数体为" $e
      "的非递归函数. 我们还用" (Let $x $e_1 $e_2)
      "表示项" (App (@Lam $x $e_2) $e_1)
      ". 这就是标准的let表达式, "
      "它的操作含义 (见下文的操作语义) 是先把项"
      $e_1 "求值为一个值" $v ", 然后求值"
      (subst $e_2 $v $x)
      ", 即把" $e_2 "中的变量" $x
      "替换为值" $v "后得到的项. "
      "引用函数定义可能相当冗长, 例如"
      (&\; (&def= $loop (Rec $loop $unit $unit))
           (ap $loop $unit))
      ". 因此, 我们经常把最外层的绑定和函数绑定合并, "
      "直接写成"
      (&\; (&= (ap $loop $unit) $unit)
           (ap $loop $unit)) ".")
   (P (B "操作语义 ")
      "操作语义由三部分定义: 纯归约, "
      "涉及堆的单步归约, 以及配置之间的一般归约. "
      "一个配置由一个堆和一个线程池组成, "
      "而线程池是从线程标识符 (自然数) 到表达式的映射, "
      "即一个有限的具名线程集合. "
      "注意配置的归约是非确定性的: "
      "我们可以选择在线程池中的任意一个线程上进行归约. "
      "这反映了我们建模的是一种抢占式并发系统. "
      "任何线程都可能在任意时刻被挂起, 转而让其他线程运行. "
      "还要注意, 在线程" $i "中归约" (Fork $e)
      "表达式时, 会创建一个线程标识符为" $j
      ", 初始表达式为" $e "的新线程, 并且" (Fork $e)
      "的求值结果是单位值" $unit
      ". 求值上下文用于规定求值策略, "
      "也就是线程中下一步归约可以发生在哪里. "
      "我们采用传值调用 (call-by-value), 从左到右的求值策略. "
      "从左到右指的是函数应用, 序对, 二元运算, "
      "赋值以及比较并设置" $CAS "的求值顺序. 例如, 序对"
      (tu0 $e_1 $e_2) "的求值方式是先求值" $e_1
      " (最左边的项), 再求值" $e_2
      ". 最后我们对比较并设置原语" $CAS
      "作一点说明: 在堆" $h "中, "
      (CAS $ell $v $v^)
      "以原子方式, 即在一个归约步内, 查找"
      $ell "在" $h "中的值, 将其与"
      $v "比较, 如果等于" $v ", 就把"
      (app $h $ell) "更新为" $v^
      ". (在真实机器上, " $CAS
      "只能用来比较能放进一个机器字的值, 例如指针和整数. "
      "为简单起见, 我们在这里不形式化这一限制; "
      "关于如何形式化, 可参见[18]中的一个例子.)")
   (P "我们来演示一个简单并发程序的执行过程. "
      "该程序分配一个初始值为" $0
      "的新位置, 并创建一个新线程, "
      "该线程把这个位置更新为" $3
      "后终止. 主线程 (即原始线程) 一直等待, "
      "直到该位置的值不再是" $0
      ", 此时它读取该位置并将其加" $1
      ". 程序如下:"
      (CodeB "let x := ref(0) in
let y := fork {CAS(x,0,3)} in
(rec f := if !x = 0 then f() else x ← !x + 1)()")
      "让我们称其为" $e ".")
   ((Exercise)
    "给出该程序的至少两种不同的执行过程.")
   
   (H2. "资源的逻辑")
   (P "iris是一种高阶逻辑. 逻辑是用来陈述和证明" (Q "东西")
      "的性质的. 例如, 一个自然数就是这样一个" (Q "东西")
      ", 一个自然数的列表, 一个编程语言的一个值, 等等亦是如此. "
      "这些东西需要用某种语言写下. "
      "在iris的情形下, " (Q "东西")
      "的潜在语言是带有诸多基本常量的" (Em "简单类型论")
      ". 这些基本常量是由签名" $S:script "给出的.")
   (P "注意到这种" (Q "东西") "的语言和第2章引入的编程语言是不同的. "
      "实际上, 编程语言的项是我们所要进行推理的" (Q "东西")
      "中的一种. 令人遗憾的是, 记号往往非常类似, "
      "例如iris项的语言和编程语言都有lambda抽象, 序对, 和. "
      "我们希望读者能够习惯于这种区分.")
   (P "我们将逐步引入iris的不同概念, "
      "现在从对于顺序语言有用的一种最小分离逻辑开始.")
   (P (B "句法. ")
      "iris的句法由一个签名" $S:script "和一个可数无限的变量集合"
      $Var "构成 (变量可由元变量" (&cm $x $y $z)
      "遍历). 具体而言, 签名" $F:script
      "是函数符号及其arity的列表, "
      "arity即类型. 对于一个函数符号" $F "而言, 我们记"
      (∈ (&: $F (&-> (&cm $tau_1 $..h $tau_n)
                     (_ $tau (&+ $n $1))))
         $F:script)
      "以表达其可以应用于类型为" (&cm $tau_1 $..h $tau_n)
      "的项元组, 然后结果具有类型" (_ $tau (&+ $n $1))
      ". 函数符号的一个例子是整数加法. 其arity为"
      (&-> (&cm $ZZ $ZZ) $ZZ)
      ", 其中" $ZZ "是整数的类型.")
   (P "iris的类型由以下语法构建, 其中" $T
      "代表我们之后要添加的额外基类型, "
      $Val "和" $Expr
      "分别是语言的值和句法的类型, 而"
      $Prop "是iris命题的类型."
      (MB (&::= $tau
                (&\| $T $ZZ $Val $Expr $Prop
                     $unitType (&+ $tau $tau)
                     (&c* $tau $tau)
                     (&-> $tau $tau))))
      "相应的iris项定义如下. "
      "当我们之后引入新的iris概念时, 其会得到扩展, "
      "而一些我们现在视为原语的项实际上是被定义的概念."
      (eqn*
       ((&cm $t $P)
        $::=
        (&\| $x $n $v $e (appl $F $t_1 $..h $t_n)))
       ($ $\| (&\| $unit (tu0 $t $t) (App $pi_i $t)
                   (Lam (&: $x $tau) $t) (app $t $t)))
       ($ $\| (&\| (Inl $t) (Inr $t)
                   (Case $t (Bind $x $t) (Bind $y $t))))
       ($ $\| (&\| $False $True (eq $t $t)
                   (&impl $P $P) (&conj $P $P)
                   (&disj $P $P) (&* $P $P)
                   (&wand $P $P)))
       ($ $\| (&\| (∃ (&: $x $tau) $P)
                   (∀ (&: $x $tau) $P)))
       ($ $\| (&\| (Persistent $P) (Later $P)))
       ($ $\| (Hoare0 $P $t $P))
       ($ $\| (PointsTo $t $t)))
      "其中" $x "是变量, " $n "是整数, " $v "和" $e
      "遍历语言的值 (即它们分别是类型"
      $Val "和" $Expr "的原始项), 而"
      $F "遍历签名" $S:script "中的函数符号.")
   (P "项" $unit "是唯一具有单元类型" $unitType
      "的项, " (tu0 $t $t) "是序对, " (App $pi_i $t)
      "是投影, " (Lam (&: $x $tau) $t)
      "是lambda抽象, 而" (app $t $t)
      "代表函数应用. 接着我们有和的引入形式 ("
      $inl "和" $inr ") 以及对应的消去形式"
      $case ".")
   (P "剩下来的项是逻辑构造. "
      "它们绝大多数都是标准的命题联结词. "
      "额外的构造是分离合取" (@ $*) "和魔杖" (@ $wand)
      ", 其将会在第3章进行解释. 然后我们有later模态"
      (Later $P) ", 其在第6章进行解释, 以及持续模态"
      (Persistent $P) ", 其在第7章进行解释. "
      "最后我们有Hoare三元组" (Hoare0 $P $t $P)
      "和指向谓词" (PointsTo $t $t)
      ", 其于第4章进行解释.")
   (P "项语言的定型规则见于图2. 判断具有形式"
      (: Γ $!-_S (&: $t $tau))
      ", 其表达的是给定签名" $S:script
      ", 合适一个项" $t "在上下文" Γ
      "之中具有类型" $tau
      ". 变量上下文" Γ
      "为逻辑的变量指派类型. "
      "其是变量" $x "和类型" $tau
      "的序对列表, 并且所有的变量都是不同的. "
      "我们以通常的方式写下上下文, 例如"
      (&cm (&: $x_1 $tau_1)
           (&: $x_2 $tau_2))
      "是一个上下文.")
   (let ((comb (λ (C)
                 (MB (&rule (G!- (&: $P $Prop))
                            (G!- (&: $Q $Prop))
                            (G!- (&: (C $P $Q) $Prop)))))))
     (P "基本逻辑的良类型项"
        (MB (&rule (!- (&: $x $tau) (&: $x $tau))))
        (MB (&rule (G!- (&: $t $tau))
                   (G!- (&: $x $tau^) (&: $t $tau))))
        (MB (&rule (G!- (&: $x $tau^)
                        (&: $y $tau^)
                        (&: $t $tau))
                   (G!- (&: $x $tau^)
                        (&: (subst $t $x $y) $tau))))
        
        (MB (&rule (∈ $v $Val)
                   (G!- (&: $v $Val))))
        (MB (&rule (∈ $e $Expr)
                   (G!- (&: $e $Expr))))
        (MB (&rule (G!- (&: $unit $unitType))))
        (MB (&rule (G!- (&: $t $tau_1))
                   (G!- (&: $u $tau_2))
                   (G!- (&: (tu0 $t $u)
                            (&c* $tau_1 $tau_2)))))
        (MB (&rule (G!- (&: $t (&c* $tau_1 $tau_2)))
                   (∈ $i (setE $1 $2))
                   (G!- (&: (App $pi_i $t) $tau_i))))
        (MB (&rule (G!- (&: $x $tau) (&: $t $tau^))
                   (G!- (&: (Lam $x $t)
                            (&-> $tau $tau^)))))
        (MB (&rule (G!- (&: $t (&-> $tau $tau^)))
                   (G!- (&: $u $tau))
                   (G!- (&: (app $t $u) $tau^))))
        (MB (&rule (G!- $False $Prop)))
        (MB (&rule (G!- $True $Prop)))
        (MB (&rule (G!- (&: $t $tau))
                   (G!- (&: $u $tau))
                   (G!- (&: (eq $t $u) $Prop))))
        (comb &impl)
        (comb &conj)
        (comb &disj)
        (comb &*)
        (comb &wand)
        (MB (&rule (G!- (&: $x $tau) (&: $P $Prop))
                   (G!- (&: (∃ (&: $x $tau) $P) $Prop))))
        (MB (&rule (G!- (&: $x $tau) (&: $P $Prop))
                   (G!- (&: (∀ (&: $x $tau) $P) $Prop))))
        "图2. 逻辑项的定型规则"))
   (H3. "命题和蕴涵")
   (P "对于投影, " $lambda "和" $mu
      "我们有通常的" $eta "和" $beta "规则."
      (MB (&rull (RLabel "UNIT-&eta;")
                 (G!- (&: $t $unitType))
                 (G!- (&equiv $t $unit))))
      (MB (&rull (RLabel "&lambda;-&beta;")
                 (G!- (&equiv (app (@Lam (&: $x $tau) $e_1) $e_2)
                              (subst $e_1 $e_2 $x)))))
      
      "图3. 逻辑蕴涵"
      )
   (P "逻辑的蕴涵规则具有形式"
      (MB (&\| Γ (!- $P $Q)))
      "和通常一样, 这从直觉上表达了"
      $Q "由假设" $P "是可证明的. 这里的"
      $P "和" $Q "应该是良类型的命题, 即"
      (G!- (&: $P $Prop)) "和"
      (G!- (&: $Q $Prop)) ".")
   (P "陈述逻辑规则时, 如果上下文" Γ
      "从规则的前提到结论没有发生改变, "
      "我们就会省略. 而且, "
      "如果存在多个前提, "
      "我们假定省略的上下文" Γ
      "对于它们都是一样的. "
      "规则可以在图3里找到. "
      "第一集规则是直觉主义高阶逻辑的标准蕴涵规则. "
      "除此之外, 我们还有新逻辑联结词" $* "和" $wand
      "的规则. 以下我们解释了新的规则.")
   
   (H3. "分离合取与魔杖的规则")
   (H3. "iris中的基本数学构造")
   (H2. "顺序程序的分离逻辑")
   (H2. "")
   (H2. "later模态")
   
   ))