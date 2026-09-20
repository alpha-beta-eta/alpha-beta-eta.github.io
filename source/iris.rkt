#lang racket
(provide iris.html)
(require SMathML)
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
(define $Persistently (Mo "&square;"))
(define (&Persistently P) (ap $Persistently P))
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
(define $True (Meta "True"))
(define $False (Meta "False"))
(define $Emp (Meta "Emp"))
(define $Persistent (Meta "persistent"))
(define (Persistent P)
  (app $Persistent P))
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
(define-infix*
  (&prcue $prcue)
  (&⊎ $⊎)
  (Bind $.)
  (&impl $impl))
(define-@lized-op*
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
                   (&Persistently $P) (&= $t $u)))
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
      
      )
   (H2. "高级幽灵状态构造")
   (H2. "一个Iris的模型")
   (H2. "Iris基逻辑")
   (H2. "最弱前条件")
   ))