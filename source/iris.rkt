#lang racket
(provide iris.html)
(require SMathML)
(define $later (Mo "▷"))
(define (Later P)
  (ap $later P))
(define $wand
  (Mo "&minus;&#8270;"))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define $prcue (Mo "&prcue;"))
(define $⊎ (Mo "⊎"))
(define $? (Mi "?"))
(define (&? x) (^ x $?))
(define $dummy (Mi "&minus;"))
(define (Core a) (&abs a))
(define (RLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define (Box #:attr* [attr* '()] . xml*)
  `(menclose ,(attr*-set attr* 'notation "box")
             . ,xml*))
(define (DBox . xml*)
  `(mrow ((style "border: 0.8px dashed black; padding: 0px 4px;"))
         . ,xml*))
(define (Inv N P)
  (^ (Box P) N))
(define (Own γ a)
  (^ (DBox a) γ))
(define $ViewShift (Mo "⇛"))
(define ViewShift
  (case-lambda
    ((P Q) (ViewShift P $E:script Q))
    ((P E Q) (: P (_ $ViewShift E) Q))))
(define $Valid (OverBar $V:script))
(define (Valid a) (app $Valid a))
(define $Valid_1 (_ $Valid $1))
(define (Valid_1 a) (app $Valid_1 a))
(define $Valid_2 (_ $Valid $2))
(define (Valid_2 a) (app $Valid_2 a))
(define $Persistently (Mo "&square;"))
(define (Persistently P) (ap $Persistently P))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define (appc f . x*)
  (ap f (cur0 (apply : x*))))
(define $Prop (Mi "Prop"))
(define $iProp (Mi "iProp"))
(define $Val (Mi "Val"))
(define $Expr (Mi "Expr"))
(define $Ctx (Mi "Ctx"))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define $Inl (Pid "inl"))
(define $Inr (Pid "inr"))
(define (Inl t) (app $Inl t))
(define (Inr t) (app $Inr t))
(define $true (Pid "true"))
(define $false (Pid "false"))
(define $fork (Pid "fork"))
(define $assert (Pid "assert"))
(define $ref (Pid "ref"))
(define $CAS (Pid "CAS"))
(define $unit (tu0))
(define $ell (Mi "&ell;"))
(define $hole $bull)
(define (Lam x e)
  (: $lambda x $. e))
(define (Fork e) (appc $fork e))
(define (Assert e) (app $assert e))
(define (Ref e) (app $ref e))
(define (Deref e) (ap $! e))
(define (CAS e e1 e2)
  (appl $CAS e e1 e2))
(define (Meta str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $inl (Meta "inl"))
(define $inr (Meta "inr"))
(define (&inl x)
  (app $inl x))
(define (&inr x)
  (app $inr x))
(define $atomic (Meta "atomic"))
(define (Atomic e)
  (app $atomic e))
(define $True (Meta "True"))
(define $False (Meta "False"))
(define $Emp (Meta "Emp"))
(define $persistent (Meta "persistent"))
(define (Persistent P)
  (app $persistent P))
(define $pending (Meta "pending"))
(define $shot (Meta "shot"))
(define (&shot n)
  (app $shot n))
(define $lightning (Mi "↯"))
(define $impl $=>)
(define PointsTo &\|->)
(define Assign &<-)
(define split2 (&split 2))
(define App (&split 2))
(define split16 (&split 16))
(define Hoare
  (case-lambda
    ((P e v Q) (Hoare P e v Q $E:script))
    ((P e v Q E) (_ (split2 (cur0 P) e (cur0 (Bind v Q))) E))))
(define Hoare^
  (case-lambda
    ((P e Q) (split2 (cur0 P) e (cur0 Q)))
    ((P e Q E) (_ (Hoare^ P e Q) E))))
(define (subst e v x)
  (: e (bra0 (&/ v x))))
(define $⇝ (Mo "⇝"))
(define $\\ (Mo "\\"))
(define $+_l (_ $+ $lightning))
(define $Ex (Mi "Ex"))
(define (Ex X)
  (app $Ex X))
(define $ex (Meta "ex"))
(define (&ex x)
  (app $ex x))
(define $AG (Mi "AG"))
(define $AG_0 (_ $AG $0))
(define (AG_0 X) (app $AG_0 X))
(define $ag (Meta "ag"))
(define $ag_0 (_ $ag $0))
(define (&ag_0 x)
  (app $ag_0 x))
(define $OneShot (Mi "OneShot"))
(define (OneShot X)
  (app $OneShot X))
(define $Own (Meta "Own"))
(define (&Own a)
  (app $Own a))
(define-infix*
  (&+_l $+_l)
  (&wand $wand)
  (&\\ $\\)
  (&⇝ $⇝)
  (&prcue $prcue)
  (&⊎ $⊎)
  (Bind $.)
  (&impl $impl))
(define-@lized-op*
  (@∃ ∃)
  (@Lam Lam)
  (@∈ ∈))
(define iris.html
  (TnTmPrelude
   #:title "Iris from the ground up"
   #:css "styles.css"
   (H1. "Iris from the ground up")
   (H2. "引论")
   
   (H2. "Iris之旅")
   (P "Iris是一种通用的高阶并发分离逻辑. 这里的" (Q "通用")
      "指的是该逻辑以人们希望推理的程序表达式所属的语言为参数, "
      "因此同一种逻辑可以用于多种多样的语言. "
      "为了使本节的讨论更加具体, "
      "我们用一种类ML的语言来实例化Iris, "
      "该语言支持高阶存储, fork以及"
      "比较并交换 (compare-and-set, CAS), 如下所示:"
      (eqn*
       ((∈ $v $Val)
        $::=
        (&\| $unit $z $true $false $ell (Lam $x $e) $..h)
        (@∈ $z $ZZ))
       ((∈ $e $Expr)
        $::=
        (&\| $v $x (app $e_1 $e_2) (Fork $e) (Assert $e)))
       ($ $\| (&\| (Ref $e) (Deref $e) (Assign $e_1 $e_2)
                   (CAS $e $e_1 $e_2) $..h))
       ((∈ $K $Ctx)
        $::=
        (&\| $hole (app $K $e) (app $v $K) (Assert $K)
             (Ref $K) (Deref $K) (Assign $K $e) (Assign $v $K)))
       ($ $\| (&\| (CAS $K $e_1 $e_2) (CAS $v $K $e_2)
                   (CAS $v $v_1 $K) $..h)))
      "(我们省略了序对与和上的常规操作.)")
   (P "该逻辑包含高阶分离逻辑中常见的联结词和规则, "
      "其中一部分已在下面的语法中列出. "
      "(实际上, 该语法中给出的许多联结词在Iris"
      "中都是作为导出形式 (derived forms) 定义的, "
      "这种灵活性是该逻辑的一个重要方面, "
      "不过我们将这一点的进一步讨论留到第5-7章.)"
      (eqn*
       ((&cm $P $Q $R)
        $::=
        (&\| $True $False (&conj $P $Q)
             (&disj $P $Q) (&impl $P $Q)))
       ($ $\| (&\| (∀ $x $P) (∃ $x $P) (&* $P $Q)
                   (PointsTo $ell $v)
                   (Persistently $P) (&= $t $u)))
       ($ $\| (&\| (Inv $N:script $P) (Own $gamma $a)
                   (Valid $a) (Hoare $P $e $v $Q)
                   (ViewShift $P $Q) $..h))))
   (P "Iris的证明规则包含并发分离逻辑中关于Hoare三元组的常见规则, "
      "如图1所示. 注意, " (RLabel "HOARE-BIND")
      "是将常见的顺序规则推广到任意求值上下文的结果.")
   (P "图1. Hoare三元组的基本规则."
      (MB (&rull (RLabel "HOARE-FRAME")
                 (Hoare $P $e $w $Q)
                 (Hoare (&* $P $R) $e $w (&* $Q $R))))
      (MB (&rull (RLabel "HOARE-VAL")
                 (Hoare $True $v $w (&= $w $v))))
      (MB (&rull (RLabel "HOARE-BIND")
                 (Hoare $P $e $v $Q)
                 (∀ $v (Hoare $Q (app $K $v) $w $R))
                 (Hoare $P (app $K $e) $w $R)))
      (MB (&rull (RLabel "HOARE-&lambda;")
                 (Hoare $P (subst $e $v $x) $w $Q)
                 (Hoare $P (App (@Lam $x $e) $v) $w $Q)))
      (MB (&rull (RLabel "HOARE-FORK")
                 (Hoare^ $P $e $True)
                 (Hoare^ $P (Fork $e) $True $E:script)))
      (MB (&rull (RLabel "HOARE-ASSERT")
                 (Hoare^ $True (Assert $true) $True $E:script)))
      (MB (&rull (RLabel "HOARE-ALLOC")
                 (Hoare $True (Ref $v) $ell
                        (PointsTo $ell $v))))
      (MB (&rull (RLabel "HOARE-LOAD")
                 (Hoare (PointsTo $ell $v)
                        (Deref $ell)
                        $w (&* (&= $w $v)
                               (PointsTo $ell $v)))))
      (MB (&rull (RLabel "HOARE-STORE")
                 (Hoare^ (PointsTo $ell $v)
                         (Assign $ell $w)
                         (PointsTo $ell $w)
                         $E:script)))
      (MB (&rull (RLabel "HOARE-CAS-SUC")
                 (Hoare (PointsTo $ell $v)
                        (CAS $ell $v $w)
                        $b (&* (&= $b $true)
                               (PointsTo $ell $w)))))
      (MB (&rull (RLabel "HOARE-CAS-FAIL")
                 (&!= $v $v^)
                 (Hoare (PointsTo $ell $v)
                        (CAS $ell $v^ $w)
                        $b (&* (&= $b $false)
                               (PointsTo $ell $v))))))
   (P "使Iris成为高阶分离逻辑的原因在于, "
      "全称量词和存在量词可以遍历任意类型, "
      "包括命题和(高阶)谓词. "
      "而且, 注意到Hoare三元组" (Hoare $P $e $v $Q)
      "是命题逻辑 (经常也被称为断言逻辑) "
      "的一部分而非独立的实体. "
      "因此, 三元组可以像任何逻辑命题一样使用. "
      "特别地, 它们可以嵌套使用, 以给出高阶函数的规约. "
      "此外, Hoare三元组" (Hoare $P $e $v $Q)
      "还标注了一个掩码" $E:script
      ", 用于记录当前哪些不变式正在生效. "
      "我们将在第2.2节再回到不变式和掩码的话题, "
      "目前暂且将其省略.")
   (P "图2. 示例代码和规约." (Br)
      "代码:"
      (CodeB "mk_oneshot := λ_.
  let x = ref (inl 0) in
  {
    tryset = λn. CAS(x, inl 0, inr n),

    check  = λ_.
      let y = !x in
      λ_.
        match (y, !x) with
        | (inl _, _)     => ()
        | (inr n, inl _) => assert(false)
        | (inr n, inr m) => assert(n = m)
        end
  }")
      "规约:"
      (CodeB "{ True }
  mk_oneshot ()
{ c. ∀v.
       { True } c.tryset(v) { w. w ∈ {true, false} }
     *
       { True } c.check()   { f. { True } f() { True } }
}"))
   (P (B "一个启发性的例子. ")
      "我们将通过验证图2中给出的一个简单高阶程序的安全性, "
      "来展示Iris的高阶特性以及它的其他一些核心特性. "
      "这个程序当然颇为刻意, 但它足以展示Iris的核心特性.")
   (P "函数" (Code "mk_oneshot()") "在" (Code "x")
      "处分配一个oneshot位置, "
      "并返回一个包含两个闭包的记录" (Code "c")
      ". (形式上, 记录是二元组的语法糖.) 函数"
      (Code "c.tryset(n)") "尝试将位置"
      (Code "x") "设置为" (Code "n")
      ", 如果该位置已经被设置过, 则会失败. "
      "我们使用" (Code "CAS")
      "来保证即使两个线程并发地尝试设置该位置, "
      "检查也依然正确. 函数" (Code "c.check()")
      "记录位置" (Code "x")
      "的当前状态, 然后返回一个闭包; 如果位置"
      (Code "x") "已经被初始化, "
      "该闭包会检查它的值没有发生改变.")
   (P "这个规范看起来有点奇怪, "
      "因为大多数前置条件和后置条件都是" (Code "True")
      ". 原因在于, 我们在这里想要证明的仅仅是代码是安全的, "
      "也就是说, 断言会" (Q "成功")
      ": 带有" (Code "assert(false)")
      "的分支永远不会被执行, 并且在最后一个分支中, "
      (Code "n") "总是等于" (Code "m")
      ". 在Iris中, Hoare三元组可以推出安全性, "
      "因此我们不需要再施加任何额外的条件.")
   (P "与函数式程序的Hoare三元组的常见做法一样, "
      "每个Hoare三元组的后条件都带有一个绑定子, 用于引用返回值. "
      "如果结果总是unit, 我们将省略该绑定子.")
   (P "我们使用嵌套的Hoare三元组来表达"
      (Code "mk_oneshot()")
      "返回闭包这一事实: "
      "由于Hoare三元组本身就是命题, "
      "我们可以将它们放入"
      (Code "mk_oneshot()")
      "的后条件中, 以描述客户端可以对" (Code "c")
      "做出哪些假设. 此外, 由于Iris是一种并发程序逻辑, "
      (Code "mk_oneshot()")
      "的规约实际上允许客户端从多个线程以任意组合并发地调用"
      (Code "c.tryset(n)") "和" (Code "c.check()")
      ", 以及由" (Code "c.check()")
      "返回的闭包" (Code "f") ".")
   (P "值得指出的是, Iris是一种仿射分离逻辑, "
      "这意味着它满足弱化规则"
      (&impl (&* $P $Q) $P)
      ". 直观地说, 这条规则允许人们丢弃资源; "
      "例如, 在Hoare三元组的后置条件中, "
      "人们可以丢弃对未使用内存位置的所有权. "
      "由于Iris是仿射的, 它没有" $Emp
      "联结词 (该联结词断言不拥有任何资源 "
      "(O'Hearn et al., 2001)); "
      "相反, Iris的" $True
      "联结词 (它描述对任意资源的所有权) "
      "是分离合取的单位元 (即"
      (&<=> (&* $P $True) $P)
      "). 我们将在第9.5节中进一步讨论这一设计选择.")
   (P (B "高层证明结构. ")
      "为了完成这个证明, "
      "我们需要以某种方式编码这样一个事实: 我们对"
      (Code "x") "只进行一次性 (oneshot) 更新. "
      "为此, 我们将分配一个名为"
      $gamma ", 值为" $a
      "的幽灵位置 (ghost location) "
      (Own $gamma $a) ", 它镜像" (Code "x")
      "的当前状态. 乍一听这似乎毫无意义: "
      "为什么要在幽灵状态中记录一个"
      "与某个物理位置中的值完全相同的值呢?")
   (P "关键在于, 使用幽灵状态让我们能够选择该位置上可以进行何种共享. "
      "对于物理位置" $ell ", 命题"
      (PointsTo $ell $v) "表示对" $ell
      "的完全所有权 (因此也意味着它不存在任何共享). "
      "与之相对, Iris允许我们为幽灵位置" $gamma
      "选择任意我们想要的结构和所有权形式. "
      "特别地, 我们可以这样定义它: 虽然"
      $gamma "的内容镜像" (Code "x")
      "的内容, 但一旦" $gamma
      "被初始化 (通过调用" (Code "tryset")
      "), 我们就可以自由地共享" $gamma
      "的所有权. 这又使得" (Code "check")
      "返回的闭包能够拥有" $gamma
      "的一部分, 用以见证 (witness) 其初始化之后的值. "
      "然后我们会有一个不变式 (见第2.2节), 将"
      $gamma "的值与" (Code "x")
      "的值联系起来. 这样我们就知道该闭包从"
      (Code "x") "读取时将会看到哪个值, "
      "并且知道该值将与" (Code "y") "相匹配.")
   (P "描述这一过程的另一种方式是: "
      "我们正在应用虚构分离 (fictional separation) "
      "的思想 (Dodds et al., 2009): "
      $gamma "上的分离是" (Q "虚构的")
      ", 其含义是多个线程可以拥有" $gamma
      "的各个部分, 从而操作同一个共享变量" (Code "x")
      ", 二者通过一个不变式联系在一起.")
   (P "在理解了这一高层证明结构之后, "
      "我们现在来解释幽灵状态的所有权与共享究竟是如何被控制的.")
   (H3. "Iris中的幽灵状态: 资源代数")
   (P "图3. 资源代数." (Br)
      "一个资源代数 (RA) 是一个元组"
      (tu0 $M (&: $Valid (&-> $M $Prop))
           (&: (Core $dummy) (&-> $M (&? $M)))
           (&: (@ $d*) (&-> (&c* $M $M) $M)))
      ", 其满足:"
      (MBL (RLabel "(RA-ASSOC)")
           (∀ $a $b $c
              (associate &d* $a $b $c)))
      (MBL (RLabel "(RA-COMM)")
           (∀ $a $b (commute &d* $a $b)))
      (MBL (RLabel "(RA-CORE-ID)")
           (∀ $a (&impl (∈ (Core $a) $M)
                        (&= (&d* (Core $a) $a) $a))))
      (MBL (RLabel "(RA-CORE-IDEM)")
           (∀ $a (&impl (∈ (Core $a) $M)
                        (&= (Core (Core $a))
                            (Core $a)))))
      (MBL (RLabel "(RA-CORE-MONO)")
           (∀ $a $b
              (&impl (&conj (∈ (Core $a) $M)
                            (&prcue $a $b))
                     (&conj (∈ (Core $b) $M)
                            (&prcue (Core $a) (Core $b))))))
      (MBL (RLabel "(RA-VALID-OP)")
           (∀ $a $b
              (&impl (Valid (&d* $a $b))
                     (Valid $a))))
      "其中" (&def= (&? $M) (&⊎ $M (setE $bottom))) "而"
      (&def= (&d* (&? $a) $bottom)
             (&d* $bottom (&? $a))
             (&? $a))
      (MBL (RLabel "(RA-INCL)")
           (&def= (&prcue $a $b)
                  (∃ (∈ $c $M)
                     (&= $b (&d* $a $c)))))
      (MB (&def= (&⇝ $a $B)
                 (∀ (∈ (&? $c) (&? $M))
                    (&impl (Valid (&d* $a (&? $c)))
                           (∃ (∈ $b $B)
                              (Valid (&d* $b (&? $c))))))))
      (MB (&def= (&⇝ $a $b)
                 (&⇝ $a (setE $b))))
      )
   (P "一个单位资源代数 (uRA) 是一个资源代数" $M
      "并带有一个元素" $epsilon "满足:"
      (MB (split16 (Valid $epsilon)
                   (∀ (∈ $a $M)
                      (&= (&d* $epsilon $a) $a))
                   (&= (Core $epsilon)
                       $epsilon)))
      )
   (P "Iris允许人们通过命题" (Own $gamma $a)
      "使用幽灵状态, 其断言了幽灵位置" $gamma
      "的一个部分" $a "的所有权. "
      "Iris的灵活性是从以下事实中生发的: "
      "对于每个幽灵位置" $gamma
      ", 逻辑的使用者可以挑选其值" $a "的类型" $M
      ", 而非这个类型提前由逻辑固定. "
      "然而, 为了能够以有趣的方式使用"
      (Own $gamma $a) ", " $M
      "不能只是任意的类型, "
      "而应该具有某种额外的结构, 即"
      (Ul (Li "其应该能够对于不同线程的所有权进行复合. "
              "为了使得这成为可能, 类型" $M
              "应该具有一个运算" (@ $d*)
              "用于复合. 逻辑中这种运算的重要规则是"
              (&<=> (Own $gamma (&d* $a $b))
                    (&* (Own $gamma $a)
                        (Own $gamma $b)))
              " (见图4的" (RLabel "GHOST-OP") ").")
          (Li "无意义的复合"
              (&* (Own $gamma $a)
                  (Own $gamma $b))
              "应当被逻辑排除 (即它们应该蕴涵" $False
              "). 例如, 这发生于多个线程"
              "声称具有一个互斥资源的所有权. "
              "为了使得这成为可能, 运算" (@ $d*)
              "应当是部分的 (partial).")))
   (P "此外, 所有权的组合应当满足结合律和交换律, "
      "以反映分离合取的结合性与交换性. "
      "正因如此, 部分交换幺半群 "
      "(partial commutative monoids, PCMs) "
      "已成为分离逻辑中表示幽灵状态的标准结构. "
      "在Iris中, 我们对此略有偏离, "
      "使用了我们自己的资源代数 (resource algebra, RA) 概念, "
      "其定义见图3. 每个PCM都是一个RA, 但反之则不然" --
      "正如我们将在例子中看到的, "
      "RA所提供的额外灵活性带来了额外的逻辑表达能力.")
   (P "图4. 一些Iris证明规则."
      (MB (&rull (RLabel "GHOST-ALLOC")
                 (Valid $a)
                 (ViewShift $True (∃ $gamma (Own $gamma $a)))))
      (MB (&rull (RLabel "GHOST-OP")
                 (&<=> (Own $gamma (&d* $a $b))
                       (&* (Own $gamma $a)
                           (Own $gamma $b)))))
      (MB (&rull (RLabel "GHOST-VALID")
                 (&impl (Own $gamma $a)
                        (Valid $a))))
      
      (MB (&rull (RLabel "HOARE-VS")
                 (ViewShift $P $P^)
                 (Hoare $P^ $e $v $Q^)
                 (∀ $v (ViewShift $Q^ $Q))
                 (Hoare $P $e $v $Q)))
      (MB (&rull (RLabel "VS-TRANS")
                 (ViewShift $P $Q)
                 (ViewShift $Q $R)
                 (ViewShift $P $R)))
      (MB (&rull (RLabel "VS-FRAME")
                 (ViewShift $P $Q)
                 (ViewShift (&* $P $R) (&* $Q $R))))
      (MB (&rull (RLabel "INV-ALLOC")
                 (ViewShift $P (Inv $N:script $P))))
      (MB (&rull (RLabel "HOARE-INV")
                 (Hoare (&* (Later $P) $Q_1)
                        $e $v
                        (&* (Later $P) $Q_2)
                        (&\\ $E:script $N:script))
                 (Atomic $e)
                 (&sube $N:script $E:script)
                 (Hoare (&* (Inv $N:script $P)
                            $Q_1)
                        $e $v
                        (&* (Inv $N:script $P)
                            $Q_2)
                        $E:script)))
      (MB (&rull (RLabel "HOARE-CTX")
                 (Hoare (&* $P $Q) $e $v $R)
                 (Persistent $Q)
                 (&wand $Q (Hoare $P $e $v $R))))
      (MB (&rull (RLabel "PERSISTENT-DUP")
                 (Persistent $P)
                 (&<=> $P (&* $P $P))))
      (MB (&rull (RLabel "PERSISTENT-SEP")
                 (Persistent $P)
                 (Persistent $Q)
                 (Persistent (&* $P $Q))))
      (MB (&rull (RLabel "PERSISTENT-GHOST")
                 (&= (Core $a) $a)
                 (Persistent (Own $gamma $a))))
      )
   (P "RA与PCM之间有两个关键区别:"
      (Ol (Li "没有使用部分性, "
              "RA使用合法性来排除非法的所有权组合. "
              "具体而言, 存在一个谓词"
              (func $Valid $M $Prop)
              "可以识别出合法的元素. "
              "合法性与复合运算是兼容的 ("
              (RLabel "RA-VALID-OP")
              "). 我们在第4.3节将会看到合法性取代部分性对于定义"
              (Em "高阶幽灵状态") "的结构而言是必要的, "
              "即结构依赖于" $iProp "的幽灵状态, "
              "这是Iris逻辑的命题的类型.")
          (Li "RA并不具有一个对每个元素都是恒元的单一单位元"
              $epsilon " (即对于任意的" $a "都有"
              (&= (&d* $epsilon $a) $a)
              "), 而是具有一个部分函数" (Core $dummy)
              "为元素" $a "指派其可复制的核"
              (Core $a) ", 如" (RLabel "RA-CORE-ID")
              "所要求的那样. 我们进一步要求"
              (Core $dummy) "是幂等的 ("
              (RLabel "RA-CORE-IDEM")
              ") 和单调的 ("
              (RLabel "RA-CORE-MONO")
              "), 相对于" (Em "扩展序")
              "而言, 其定义类似于PCM的 ("
              (RLabel "RA-INCL") ")." (Br)
              "一个元素可以没有核, 这是由"
              (&= (Core $a) $bottom)
              "指示的. 为了方便地处理部分核, "
              "我们使用元变量" (&? $a) "遍历"
              (&def= (&? $M) (&⊎ $M (setE $bottom)))
              "的元素, 并将复合" (@ $d*) "提升至" (&? $M)
              ". 我们将会在第3.1节看到, "
              "部分核有助于我们从更小的原语构建有趣的复合RA." (Br)
              "在RA的确有一个单位元" $epsilon
              "的特殊情形下, 我们将其称为单位RA (uRA). 根据"
              (RLabel "RA-CORE-MONO")
              ", 可以推出uRA的核是一个完全函数, 即"
              (&!= (Core $a) $bottom) "." (Br)
              "可复制核的想法并不新颖, "
              "我们将在第9.3节讨论相关工作.")))
   ((tcomment)
    "不仅需要使用" (RLabel "RA-CORE-MONO")
    ", 还需要使用uRA的公理"
    (&= (Core $epsilon) $epsilon) ".")
   (P (B "我们例子的一个资源代数. ")
      "现在我们来定义可用于验证我们示例的RA, "
      "我们称之为oneshot RA. "
      "这个RA的目标是恰当地反映物理位置" $x
      ". carrier的定义使用了如下的类数据类型记号."
      (MB (&def= $M (&\| $pending (&shot (&: $n $ZZ))
                         $lightning))))
   (P "幽灵位置的两种重要状态为: " $pending
      ", 代表单次更新尚未发生, 以及" (&shot $n)
      ", 其是说位置已经被置为" $n
      ". 我们需要额外的元素" $lightning
      "来表示部分性; 其是唯一的非法元素:"
      (MB (&def= (Valid $a)
                 (&!= $a $lightning))))
   (P "当然了, 一个RA最有趣的地方在于其复合: "
      "两个线程的所有权合并时会发生什么? "
      "(以下等式没有定义的复合自动映射至"
      $lightning ".)"
      (MB (&def= (&d* (&shot $n)
                      (&shot $m))
                 (Choice0
                  ((&shot $n) ", 如果" (&= $n $m))
                  ($lightning ", 否则的话")))))
   (P "这个定义具有三种重要性质:"
      (MBL (RLabel "(PENDING-EXCL)")
           (&impl (Valid (&d* $pending $a))
                  $False))
      (MBL (RLabel "(SHOT-AGREE)")
           (&impl (Valid (&d* (&shot $n) (&shot $m)))
                  (&= $n $m)))
      (MBL (RLabel "(SHOT-IDEM)")
           (&= (&d* (&shot $n) (&shot $n))
               (&shot $n))))
   (P "性质" (RLabel "(PENDING-EXCL)")
      "表明, " $pending "与任何其他元素的复合都是无效的. "
      "因此, 如果我们拥有" $pending
      ", 就可以知道没有其他线程能够拥有该位置的其他部分. 此外, "
      (RLabel "(SHOT-AGREE)") "表明, 两个" (&shot $dummy)
      "元素的复合仅在参数 (即为oneshot选定的值) 相同时才有效. "
      "这体现了这样一种思想: 一旦某个值被选定, "
      "它就成为该位置唯一可能的值; "
      "所有线程对这个值是什么都达成一致.")
   (P "最后, " (RLabel "(SHOT-IDEM)")
      "表明, 一旦该位置被设置为某个" $n
      ", 我们就可以任意复制对它的所有权. "
      "这使得我们可以在任意数量的线程之间共享"
      "这一幽灵状态的所有权.")
   (P "请注意, 我们在这里使用的" (Q "所有权")
      "一词含义相当宽泛: RA 中的任何元素都可以被"
      (Q "拥有") ". 对于像" (&shot $n)
      "这样满足性质" (&= $a (&d* $a $a))
      "的元素, 拥有" $a "等价于拥有" $a
      "的多个副本. 在这种特殊情况下, "
      "所有权不再是排他的, "
      "称之为知识或许更为恰当. 因此, "
      "我们可以把" (&shot $n) "理解为表示"
      (Q "该位置的值已被设置为" $n)
      "这一知识. 由此可见, "
      "资源代数用同一种机制实现了双重目的: "
      "既能建模 (1) 资源的所有权, "
      "也能建模 (2) 知识的共享.")
   (P "回到我们的oneshot RA, "
      "我们还需要定义核" (Core $dummy) ":"
      (MB (split16
           (&def= (Core $pending) $bottom)
           (&def= (Core (&shot $n))
                  (&shot $n))
           (&def= (Core $lightning)
                  $lightning)))
      "注意到既然" $pending
      "的所有权是排他的, "
      "其没有合适的单位元素, "
      "故没有给它分配一个核.")
   (P "现在我们完成了oneshot RA的定义, "
      "验证该RA满足RA的公理是直接的.")
   (P (B "保持框架的更新. ")
      "到目前为止, 我们已经定义了幽灵位置可以处于哪些状态, "
      "以及该位置的状态如何分布在多个线程之间. "
      "然而, 还缺少一种改变幽灵位置状态的方法. "
      "当幽灵状态发生变化时, 重要的是它必须保持有效: "
      "Iris始终维护这样一个不变式, "
      "即把所有线程各自的贡献组合起来"
      "得到的状态是一个合法的RA元素. "
      "我们把维持这一不变式的状态变化"
      "称为保持框架的更新.")
   (P "最简单的保持框架的更新是确定性的. "
      "当满足以下条件时, 我们可以进行从"
      $a "到" $b "的保持框架的更新 (记作"
      (&⇝ $a $b) "):"
      (MB (∀ (∈ (&? $c) (&? $M))
             (&impl (Valid (&d* $a (&? $c)))
                    (Valid (&d* $b (&? $c))))))
      "换言之, 对于任意的资源 (称为框架) "
      (∈ (&? $c) (&? $M)) "满足" $a
      "与" (&? $c) "兼容 (即"
      (Valid (&d* $a (&? $c)))
      "), 必然也有" $b "与"
      (&? $c) "兼容.")
   (P "例如, 对于我们的oneshot RA而言, "
      "如果它仍在pending, 取一个值是可能的:"
      (MBL (RLabel "(ONESHOT-SHOOT)")
           (&⇝ $pending (&shot $n)))
      "原因在于" (RLabel "(PENDING-EXCL)")
      ": " $pending "实际上不与任何元素兼容; "
      "复合总是产生" $lightning
      ". 因此由" (Valid (&d* $pending (&? $c)))
      ", 我们可以知道" (&= (&? $c) $bottom)
      ". 这使得证明的剩余内容变得平凡.")
   (P "如果把框架" (&? $c)
      "看作所有其他线程所拥有资源的组合, "
      "那么保持框架的更新就能保证不会使并发运行的线程的资源失效. "
      "如果没有其他线程对该幽灵位置拥有任何所有权, "
      "框架可以是" $bottom
      ". 只要只进行保持框架的更新, "
      "我们就知道自己永远不会"
      (Q "踩到别人的脚趾")
      " (即不会干扰其他线程).")
   (P "一般而言, 我们也允许非确定性的保持框架的更新 (记作"
      (&⇝ $a $B) "). 此时目标元素" $b
      "并非事先固定, 而是固定一个集合" $B
      ", 再根据当前框架从中选取某个元素" (∈ $b $B)
      ". 其形式化定义见图3. "
      "当我们在第3.2节中遇到第一个此类更新的例子时, "
      "会进一步讨论非确定性的保持框架的更新.")
   (P (B "幽灵状态的证明规则. ")
      "资源代数通过命题" (Own $gamma $a)
      "嵌入到逻辑中, 该命题断言对幽灵位置"
      $gamma "中的一个片段" $a "拥有所有权. "
      "操作这些幽灵断言的主要联结词称为视图转换 (或幽灵移动): "
      (ViewShift $P $Q) "表示, 给定满足" $P
      "的资源, 我们可以改变幽灵状态, 最终得到满足"
      $Q "的资源. (我们将在第2.2节中再讨论掩码标注"
      $E:script ".) 直观上, 视图转换就像Hoare三元组, "
      "只是没有任何代码: 只有前置条件和后置条件. "
      "它们不需要代码, 因为它们只涉及幽灵状态, "
      "而幽灵状态不对应实际程序中的任何操作.")
   (P "图4中的证明规则" (RLabel "GHOST-ALLOC")
      "可用于分配一个新的幽灵位置, 其初始状态"
      $a "可以任意选取, 只要" $a
      "在所选的RA中是有效的. 规则"
      (RLabel "GHOST-UPDATE")
      "表示, 我们可以按照上文所述, "
      "对幽灵位置进行保持框架的更新.")
   (P "Hoare三元组的所有常用结构规则对视图转换同样成立, "
      "例如框架规则 (" (RLabel "VS-FRAME")
      "). 规则" (RLabel "HOARE-VS")
      "说明了视图转换在程序验证中的用法: "
      "我们可以在Hoare三元组的前置条件和后置条件中应用视图转换. "
      "这相当于把" (Q "幽灵移动的三元组")
      " (即视图转换) 与关于" $e
      "的Hoare三元组组合起来. "
      "这样做不会改变三元组中的表达式, "
      "因为视图转换执行的幽灵状态操作不涉及任何实际代码.")
   (P "规则" (RLabel "GHOST-OP")
      "表示, 幽灵状态可以按照该RA所定义的组合运算"
      (@ $d*) "进行分离 (在分离逻辑的意义上); 而"
      (RLabel "GHOST-VALID")
      "刻画了这样一个事实: "
      "只有合法的RA元素才可能被拥有.")
   (P "请注意, 视图转换" (@ $ViewShift)
      "与推出 " (@ $impl) "和魔杖" (@ $wand)
      "非常不同: 推出" (&impl $P $Q)
      "是说每当" $P "成立则" $Q "必然也成立. "
      "与之相对比的是, 视图转换"
      (ViewShift $P $Q)
      "是说每当" $P "成立, " $Q
      "在我们改变幽灵状态的proviso下成立. "
      "因此, 不像推出和魔杖, "
      "唯一能够消去视图转换的方式是藉由"
      (RLabel "HOARE-VS") ".")
   (H3. "不变量")
   (P "既然我们已经搭建好了幽灵位置" $gamma
      "的结构, 接下来就需要把"
      $gamma "的状态与" $x
      "的实际物理值联系起来. "
      "这一步是通过不变式来完成的.")
   (P "不变式 (Ashcroft, 1975) 是一种在任何时刻都成立的性质: "
      "每个访问该状态的线程在执行每一步计算之前都可以假定不变式成立, "
      "但它也必须保证在每一步执行之后不变式仍然成立. "
      "由于我们使用的是分离逻辑, 不变式不仅仅是"
      (Q "成立") "而已; 它表达的是对某些资源的所有权, "
      "访问该不变式的线程就能获得这些资源的访问权. "
      "规则HOARE-INV按如下方式实现了这一思想:"
      (MB (&rule (Hoare (&* (Later $P) $Q_1)
                        $e $v
                        (&* (Later $P) $Q_2)
                        (&\\ $E:script $N:script))
                 (Atomic $e)
                 (&sube $N:script $E:script)
                 (Hoare (&* (Inv $N:script $P)
                            $Q_1)
                        $e $v
                        (&* (Inv $N:script $P)
                            $Q_2)
                        $E:script))))
   (P "这条规则内容相当多, 所以我们会仔细地逐一讲解. "
      "首先是命题" (Inv $N:script $P)
      ", 它表示" $P " (一个任意命题) 被维持为一个不变式. "
      "该规则表明, 在上下文中拥有这个命题就允许我们访问该不变式, "
      "具体包括: 在验证" $e "之前获得" $P
      "的所有权, 并在验证完成之后交还" $P
      "的所有权. 关键在于, 我们要求" $e
      "是原子的, 即计算保证在单步之内完成. "
      "这对可靠性至关重要: "
      "该规则允许我们暂时使用甚至破坏不变式, "
      "但在一个原子步骤之后 "
      "(也就是在任何其他线程有机会执行之前), "
      "我们必须重新建立它.")
   (P "注意, " (Inv $N:script $P)
      "只是另一种命题, "
      "它可以用在任何普通命题可以使用的地方, "
      "包括Hoare三元组的前置条件和后置条件, "
      "以及不变式本身, 从而产生嵌套不变式. "
      "后一种性质有时被称为非直谓性. "
      "正是由于非直谓性, 不变式" $P
      "在前置条件和后置条件中是以带有"
      (Q "later") "模态的形式" (Later $P)
      "出现的; 如果没有later模态, "
      "打开不变式的规则就是不可靠的 (见第8.2节). "
      "不过, 如果不在不变式中存放Hoare三元组或不变式, "
      "通常就可以忽略later模态, "
      "本节中我们也将一直这样做. "
      "我们会在第5.5节进一步讨论later模态.")
   (P "最后, 我们来看掩码" $E:script
      "和命名空间" $N:script
      ": 它们用来避免重入问题. "
      "我们必须确保同一个不变式不会同时被访问两次, "
      "否则会错误地复制其底层资源. 为此, "
      "每个不变式都有一个用于标识它的命名空间" $N:script
      ". 此外, 每个Hoare三元组都标注了一个掩码, "
      "用来记录哪些不变式仍处于启用状态. "
      "访问一个不变式会将它的命名空间从掩码中移除, "
      "从而确保它不能以嵌套的方式被再次访问.")
   (P "不变式是通过INV-ALLOC规则 (图 4) 创建的: "
      "只要一个命题" $P "已被建立, "
      "就可以把它转化为一个不变式. "
      "这可以看作是一种资源转移: 支撑"
      $P "的资源从由某个线程局部拥有, "
      "转变为通过不变式由所有线程共享. "
      "创建不变式是一个视图转换; "
      "其原因我们将在第7章看到, "
      "届时我们会了解Iris中的不变式在" (Q "底层")
      "究竟是如何运作的. 在那之前, 我们将忽略不变式"
      (Inv $N:script $P) "的命名空间" $N:script
      ", 以及Hoare三元组 ("
      (Hoare $P $e $v $Q)
      ") 和视图转换 ("
      (ViewShift $P $Q)
      ") 的掩码" $E:script ".")
   (H3. "持久命题")
   (P "我们已经看到, Iris既能表达对独占资源的所有权 (例如"
      (PointsTo $ell $v) "或者" (Own $gamma $pending)
      "), 也能表达关于某些性质的知识, 如"
      (Inv $N:script $P) "或者" (Own $gamma (&shot $n))
      ", 其一旦成立则永远成立. "
      "我们把后一类命题称为持久的. "
      "持久命题的其他例子还有合法性"
      (Valid $a) ", 相等性" (&= $t $u)
      ", Hoare三元组" (Hoare^ $P $e (Bind $v $Q))
      ", 以及视图转换" (ViewShift $P $ $Q)
      ". 持久命题可以被自由复制 (PERSISTENT-DUP); "
      "资源只能使用一次这一通常的限制对它们并不适用.")
   (P "幽灵所有权的多用途性也体现在它与持久性的关系上: "
      "对某些元素 (如" $pending ") 的幽灵所有权是短暂的, "
      "而对核的所有权则是持久的 (PERSISTENT-GHOST). "
      "这体现了第2.1节中提到的思想, "
      "即RA能够在一个统一的框架中同时表达所有权和知识, "
      "而知识仅仅是指对持久资源的所有权. "
      "从这个角度看, 核" (Core $a)
      "是一个从RA元素" $a
      "中提取出知识的函数. 在" (Code "check")
      "的证明中, 持久幽灵所有权将起到至关重要的作用.")
   (P "持久命题的一个重要作用与嵌套Hoare三元组有关: "
      "正如规则HOARE-CTX所表达的, "
      "嵌套的Hoare三元组只能使用"
      (Q "外部") "上下文中持久的命题" $Q
      ". 持久性保证了当该Hoare三元组被"
      (Q "调用") "时 (即它所描述的代码被执行时), "
      $Q "仍然成立, 即使调用发生多次也是如此.")
   (P "一个与之密切相关的概念是可复制命题, 即满足"
      (&<=> $P (&* $P $P)) "的命题" $P
      ". 然而, 这是一个严格更弱的概念: "
      "并非所有可复制命题都是持久的. "
      "例如, 考虑带有分数权限" $q
      "的指向联结词" (: $ell (^^ $\|-> $q) $v)
      " (Boyland, 2003; Bornat et al., 2005), 命题"
      (∃ $q (: $ell (^^ $\|-> $q) $v))
      "是可复制的 (这可以通过将分数权限"
      $q "减半得到), 但它不是持久的.")
   (H3. "例子的证明")
   (P "现在我们已经了解了Iris足够多的特性, "
      "可以着手处理图2中概述的实际验证问题了. "
      "我们在图5中给出了该证明的Hoare大纲. "
      "注意, 我们把" (Code "match")
      "中对" (Code "x") "的读取用"
      (Code "let") "展开了. "
      "这样做仅仅是为了便于书写证明大纲; "
      "Iris完全有能力验证原始形式代码的正确性.")
   (P (B "mk_oneshot的证明. ")
      "首先, 通过" (Code "ref")
      "构造所执行的分配, 我们得到"
      (PointsTo $x (Inl $0))
      ". 接着, 我们分配一个新的幽灵位置"
      $gamma ", 其结构为上文定义的oneshot RA, "
      "并选取初始状态" $pending
      ". (这里隐式地使用了HOARE-VS, "
      "以证明在验证Hoare三元组的过程中"
      "应用视图转换是合理的.) "
      "最后, 我们建立并创建如下不变式:"
      (MB (&def= $I (&disj (@* (PointsTo $x (Inl $0))
                               (Own $gamma $pending))
                           (@∃ $n (&* (PointsTo $x (Inr $n))
                                      (Own $gamma (&shot $n)))))))
      "由于" $x "被初始化为" (Inl $0)
      ", 不变式" $I "在初始时成立. "
      "剩下要做的就是建立我们的后置条件, "
      "它由两个Hoare三元组组成. "
      "借助HOARE-CTX, 我们可以在证明"
      "这两个三元组时使用刚刚分配的不变式. "
      "(在证明大纲中, HOARE-CTX允许我们在"
      (Q "跨过一个" $lambda)
      "时保留资源, 但前提是这些资源是持久的. "
      "这对应于该函数可能被调用多次这一事实, "
      "因此资源不应被某一次调用耗尽.)")
   
   (H2. "高级幽灵状态构造")
   (P "在上一节中我们已经看到, "
      "用户自定义的幽灵状态在Iris中起着重要作用. "
      "在本节中, 我们将更深入地考察幽灵状态. "
      "首先, 在第3.1节中, 我们表明许多常用的资源代数"
      "都可以通过组合更小的, 可复用的组件来构造. "
      "接着在第3.2节中, 我们表明所有权联结词"
      (Own $gamma $a) "实际上可以用一个更为基础的"
      (Q "全局") "幽灵所有权概念来定义. "
      "在第3.3节中, 我们引入更高级的高阶幽灵状态概念, "
      "并表明它的朴素形式是不一致的.")
   (H3. "RA的构造")
   (P "Iris的关键特性之一是"
      "它把幽灵状态的结构完全交给逻辑的使用者来决定. "
      "如果需要某种专用的资源代数, 使用者可以直接使用它. "
      "然而事实证明, 许多常用的资源代数都可以通过"
      "组合更小的, 可复用的组件来构造. 因此, "
      "虽然在需要时我们可以使用整个资源代数空间, "
      "但我们不必为每一个新证明都构造定制的资源代数.")
   (P "例如, 回顾§2.1中的一次性 (oneshot) 资源代数, "
      "它实际上做了三件事:"
      (Ol (Li "它把资源代数中元素的分配与"
              "决定在那里存储什么值分离开来 (ONESHOT-SHOOT).")
          (Li "当一次性位置尚未初始化时, 所有权是排他的, "
              "即至多只有一个线程可以拥有该位置.")
          (Li "一旦值被决定, 它确保所有人都认同这个值."))
      "因此, 我们可以把一次性资源代数分解为和资源代数, "
      "排他资源代数与一致资源代数, 如下所述. "
      "(在所有资源代数的定义中, 省略的复合与核的情形都为"
      $lightning ".)")
   (P (B "和. ")
      "对任意资源代数" $M_1 "和" $M_2
      ", 和资源代数" (&+_l $M_1 $M_2)
      "定义为:"
      (eqn*
       ((&+_l $M_1 $M_2)
        $def=
        (&\| (&inl (&: $a_1 $M_1))
             (&inr (&: $a_2 $M_2))
             $lightning))
       ((Valid $a)
        $def=
        (&disj (@∃ (∈ $a_1 $M_1)
                   (&conj (&= $a (&inl $a_1))
                          (Valid_1 $a_1)))
               (@∃ (∈ $a_2 $M_2)
                   (&conj (&= $a (&inr $a_2))
                          (Valid_2 $a_2)))))
       ((&d* (&inl $a_1) (&inl $a_2))
        $def=
        (&inl (&d* $a_1 $a_2)))
       ((&d* (&inr $a_1) (&inr $a_2))
        $def=
        (&inr (&d* $a_1 $a_2)))
       ((Core (&inl $a_1))
        $def=
        (Choice0
         ($bottom ", 如果" (&= (Core $a_1) $bottom))
         ((&inl (Core $a_1)) ", 否则的话")))
       ((Core (&inr $a_2))
        $def=
        (Choice0
         ($bottom ", 如果" (&= (Core $a_2) $bottom))
         ((&inr (Core $a_2)) ", 否则的话")))))
   (P (B "排他. ")
      "给定集合" $X ", 排他资源代数" (Ex $X)
      "的作用是确保某一方排他地拥有一个值"
      (∈ $x $X) ". 我们按照以下方式定义"
      $Ex ":"
      (eqn*
       ((Ex $X) $def= (&\| (&ex (&: $x $X))
                           $lightning))
       ((Valid $a) $def= (&!= $a $lightning))
       ((Core (&ex $x)) $def= $bottom))
      "复合的结果总是" $lightning
      ", 以确保所有权是排他的. 这就像"
      $pending "与任何其他元素的复合都是"
      $lightning "一样 (PENDING-EXCL).")
   (P (B "一致. ")
      "给定集合" $X ", 一致资源代数"
      (AG_0 $X) "是为了保证多方可以对于已取得的值"
      (∈ $x $X) "达成一致意见. "
      "(之所以我们将其称为" $AG_0
      ", 是因为我们将在第4.3节对于该定义进行改进"
      "以得到最终版本" $AG
      ".) 我们定义" $AG_0 "如下:"
      (eqn*
       ((AG_0 $X)
        $def=
        (&\| (&ag_0 (&: $x $X))
             $lightning))
       ((Valid $a)
        $def=
        (∃ (∈ $x $X) (&= $a (&ag_0 $x))))
       ((&d* (&ag_0 $x) (&ag_0 $y))
        $def=
        (Choice0
         ((&ag_0 $x) ", 如果" (&= $x $y))
         ($lightning ", 否则的话")))
       ((Core (&ag_0 $x))
        $def=
        (&ag_0 $x)))
      "特别地, 一致资源代数满足以下性质, "
      "其对应于SHOT-AGREE:"
      (MBL "(AG0-AGREE)"
           (&impl (Valid (&d* (&ag_0 $x)
                              (&ag_0 $y)))
                  (&= $x $y))))
   (P (B "oneshot. ")
      "现在我们可以将oneshot RA的一般想法定义为"
      (&def= (OneShot $X)
             (&+_l (Ex $1)
                   (AG_0 $X)))
      ", 并将我们例子的RA恢复为"
      (OneShot $ZZ) ".")
   (P (B "保持框架的更新. ")
      "将RA分解为单独部分的另一个有点在于"
      "对于这些部分我们可以证明一般的保持框架更新. "
      "对于和与排他, 我们有以下一般的保持框架更新:"
      (MB (&rull
           "INL-UPDATE"
           (&⇝ $a $B)
           (&⇝ (&inl $a)
               (setI (&inl $b)
                     (∈ $b $B)))))
      (MB (&rull
           "INR-UPDATE"
           (&⇝ $a $B)
           (&⇝ (&inr $a)
               (setI (&inr $b)
                     (∈ $b $B)))))
      (MB (&rull
           "EX-UPDATE"
           (&⇝ (&ex $x) (&ex $y))))
      "一致RA不允许非平凡的保持框架更新.")
   (P "考虑oneshot RA, 以上规则尚不足够. "
      
      )
   (P "图6. 幽灵状态的原始规则."
      (MB (&rull
           "OWN-OP"
           (&<=> (&Own (&d* $a $b))
                 (&* (&Own $a)
                     (&Own $b)))))
      (MB (&rull
           "OWN-UNIT"
           (&impl $True (&Own $epsilon))))
      (MB (&rull
           "OWN-CORE"
           (&impl (&Own (Core $a))
                  (Persistently
                   (&Own (Core $a))))))
      (MB (&rull
           "OWN-VALID"
           (&impl (&Own $a)
                  (Valid $a))))
      (MB (&rull
           "OWN-UPDATE"
           (&⇝ $a $B)
           (ViewShift
            (&Own $a) $
            (∃ (∈ $b $B) (&Own $b)))))
      )
   (H3. "导出形式和全局幽灵状态")
   (P "Iris非常强调只提供一个最小的核心逻辑, "
      "并尽可能在逻辑内部推导出其余的构造, "
      "而不是把它们作为原语内置. "
      "例如, Hoare三元组和形如"
      (PointsTo $ell $v)
      "的命题实际上都是导出形式. "
      "我们将在§4和§5中看到, "
      "这样做的好处是模型可以保持得更简单, "
      "因为它只需要证明一个最小核心逻辑的可靠性.")
   (P "在本节中, 我们讨论幽灵所有权命题"
      (Own $gamma $a)
      "的编码, 它同样不是内置概念. "
      "如前所述, 这个命题允许我们拥有多个幽灵位置"
      $gamma ", 并且每个位置都可以取值于不同的资源代数. "
      "作为原语, Iris只提供单个全局幽灵位置, "
      "其结构由使用者选定的单个全局资源代数描述. "
      "然而, 通过选择合适的资源代数, "
      "我们可以在Iris中定义命题" (Own $gamma $a)
      ", 并在逻辑内部推导出图4中给出的它的规则.")
   (P "Iris对于幽灵所有权的原始构造是" (&Own $a)
      ", 其规则给定于图6. 值得注意的是, "
      "全局RA必须要是单位的, "
      "这意味着其应该有一个单位元素" $epsilon
      " (图3). 原因有两方面. "
      "首先, 其允许我们有规则OWN-UNIT, "
      "这可以用来证明GHOST-ALLOC. "
      "其次, 单位RA享有这样的性质: "
      "扩展序是自反的, " (Core $dummy)
      "函数是完全的. 这简化了第4章和第5章的模型构造. "
      "注意到我们总是可以将一个RA转换为一个uRA, "
      "通过以一个单位元素扩展它, "
      "保持所有的保持框架更新.")
   (P "为了定义联结词" (Own $gamma $a)
      ", 我们需要初始化单个全局幽灵状态RA以"
      "一个幽灵cell的堆. "
      )
   (H2. "一个Iris的模型")
   (H2. "Iris基逻辑")
   (H2. "最弱前条件")
   ))