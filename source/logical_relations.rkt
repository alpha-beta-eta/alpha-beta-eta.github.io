#lang racket
(provide logical_relations.html)
(require SMathML)
(define $? (Mi "?"))
(define $emptyCtx $d*)
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define $!- (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define $bool (Mi "bool"))
(define App (&split 2))
(define (If e1 e2 e3)
  (App $if e1 $then e2 $else e3))
(define Lam
  (case-lambda
    ((x e) (: $lambda x $. e))
    ((x t e) (Lam (&: x t) e))))
(define (LAM α e)
  (: $Lambda:normal α $. e))
(define $hole (bra0 $))
(define $\|->* (&* $\|->))
(define $⇓ (Mo "&dArr;"))
(define &⇓
  (make-op $⇓
           (err0 '&⇓)
           (lambda (x)
             (: x $⇓))))
(define-infix*
  (&\|->* $\|->*)
  (&\| $\|))
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign "left center right")
                (width "100%"))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))
(define (RLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define (subst e v x)
  (: e (bra0 (&/ v x))))
(define-@lized-op*
  (@Lam Lam))
(define logical_relations.html
  (TnTmPrelude
   #:title "逻辑关系"
   #:css "styles.css"
   (H1. "逻辑关系")
   (H2. "引论")
   (P "逻辑关系是一种多产且极为有用的证明方法, "
      "可用于证明程序和语言的各种性质. "
      "例如, 逻辑关系可以帮助证明:"
      (Ul (Li "诸如简单类型lambda演算 (STLC) "
              "这样的语言中的良类型程序的终止性;")
          (Li "诸如STLC这样的语言的类型可靠性/安全性;")
          (Li "程序等价性, 其形式多样:"
              (Ul (Li "验证经过优化的算法或实现等价于更简单的朴素算法或实现;")
                  (Li "论证编译器优化或变换的正确性;")
                  (Li "表明表示独立性 [Mit86], 即某一接口背后的实现不会影响客户端的行为;"
                      (Ul (Li "例如, 栈接口可以用数组或链表来实现, "
                              "但这对于接口的使用者而言应当是不可区分的. "
                              "为了证明栈接口的表示独立性, "
                              "我们需要证明使用数组实现的程序与使用链表实现的程序是等价的.")))
                  (Li "将参数化多态性及其对应的自由定理 [Wad89] 概念化为一种关系, "
                      "大致描述为" (Q "相关的输入映射到相关的输出") ";")
                  (Li "Proving noninterference for security-typed languages: "
                      "showing that two runs of a given program are equivalent "
                      "on low-security outputs for any variation on high-security "
                      "(e.g. confidentiality) program data;")))
          (Li "以及更多!")))
   (P "近期研究中, 逻辑关系已被用于证明不同逻辑的可靠性. "
      "跨语言逻辑关系则用于推理编译过程, 外部函数接口或多语言语义.")
   (P "逻辑谓词与一元逻辑关系这两个术语是同义的, "
      "逻辑关系与二元逻辑关系亦然. "
      "这两种术语在文献和日常用语中均有使用. "
      "逻辑谓词" (app $P_tau $e)
      "是集合, 用于对单个程序进行推理. "
      "程序终止性, 类型安全性与可靠性均属于逻辑谓词. "
      "而逻辑关系" (appl $R_tau $e_1 $e_2)
      "是二元关系, 可用于更广泛地刻画程序等价性.")
   (H2. "STLC的规范化")
   (P "本节我们展示了逻辑关系的一个例子, 用于证明STLC的规范化.")
   (H3. "STLC的形式化")
   (P "图1回忆了STLC的句法和操作语义. "
      "我们所考虑的是STLC的一个微小变体, 其有布尔基类型, "
      "以及if-then-else表达式."
      (eqn*
       ($tau $::= (&\| $bool (&-> $tau $tau)))
       ($e   $::= (&\| $x $true $false (If $e $e $e) (Lam $x $tau $e) (App $e $e)))
       ($v   $::= (&\| $true $false (Lam $x $tau $e)))
       ($E   $::= (&\| $hole (If $E $e $e) (App $E $e) (App $v $E))))
      (MB (&rull (RLabel "E-IfTrue")
                 (&\|-> (If $true $e_1 $e_2) $e_1)))
      (MB (&rull (RLabel "E-IfFalse")
                 (&\|-> (If $false $e_1 $e_2) $e_2)))
      (MB (&rull (RLabel "E-App")
                 (&\|-> (App (@Lam $x $tau $e) $v)
                        (subst $e $v $x))))
      (MB (&rull (RLabel "E-Step")
                 (&\|-> $e $e^)
                 (&\|-> (ap $E (bra0 $e))
                        (ap $E (bra0 $e^)))))
      "图1: 带求值上下文的STLC按值调用小步语义, "
      $\|-> "是头归约关系, 也称原始归约.")
   (P "这个操作语义实现了" (Em "按值调用")
      "求值顺序, 即函数的参数需要在应用函数前被求值为一个值. "
      "我们注意到" (subst $e $v $x) "是将项" $e
      "里的变量" $x "替换为" $v "的替换. 我们记"
      (&\|-> $e $e^) "以表达" $e "以单个计算步骤归约至了"
      $e^ ". 对于归约关系的自反传递闭包, 我们记"
      (&\|->* $e $e^) ".")
   (P "图2展示了定型判断规则. 关系" (G!- (&: $e $tau))
      "表达了" $e "在定型上下文" Γ "下被定型以"
      $tau ". 上下文" Γ "是一个从变量到其类型的映射. "
      "我们不会详细说明定型规则的细节, 因为它们太过经典. "
      "这里我们假定了隐式" $alpha "重命名, 也就是"
      Γ "中的自由变量不会与绑定变量发生冲突. "
      "{译注: 大概就是" Γ "中的变量不会与" $lambda
      "绑定的变量重名吧. 若有重名, 可以进行"
      $alpha "变换以换成其他名字.}"
      (MB (&rull (RLabel "T-True")
                 (G!- (&: $true $bool))))
      (MB (&rull (RLabel "T-False")
                 (G!- (&: $false $bool))))
      (MB (&rull (RLabel "T-Var")
                 (&= (app Γ $x) $tau)
                 (G!- (&: $x $tau))))
      (MB (&rull (RLabel "T-If")
                 (G!- (&: $e $bool))
                 (G!- (&: $e_1 $tau))
                 (G!- (&: $e_2 $tau))
                 (G!- (&: (If $e $e_1 $e_2) $tau))))
      (MB (&rull (RLabel "T-Abs")
                 (G!- (&: $x $tau_1) (&: $e $tau_2))
                 (G!- (&: (Lam $x $tau_1 $e)
                          (&-> $tau_1 $tau_2)))))
      (MB (&rull (RLabel "T-App")
                 (G!- (&: $e_1 (&-> $tau_2 $tau_1)))
                 (G!- (&: $e_2 $tau_2))
                 (G!- (&: (App $e_1 $e_2) $tau_1))))
      "图2: STLC的定型规则.")
   (P "然后, 我们形式化地定义STLC的规范化, "
      "其表达了任意良类型的封闭项实际都会终止. "
      "我们使用记号" (&⇓ $e $v) "以表达项" $e
      "求值至值" $v ", 即" (&\|->* $e $v)
      ", 另以记号" (&⇓ $e)
      "表达" $e "可以求值至某个值, 即"
      (∃ $v (&\|->* $e $v)) ".")
   ((theorem)
    "对于每个项" $e ", 若"
    (!- $emptyCtx (&: $e $tau))
    ", 那么" (&⇓ $e) ".")
   (H3. "第一次尝试: 以定型推导上的归纳证明STLC是规范化的")
   (P "为了看明白为什么我们想要使用逻辑关系, "
      "让我们先尝试朴素地证明第2.1节里的这个规范化定理, "
      "并理解我们卡在了哪里.")
   ((proof)
    "设" (!- $emptyCtx (&: $e $tau))
    ", 欲证明" (&⇓ $e)
    ". 我们以定型推导上的归纳进行处理:"
    (Ul (Li (RLabel "T-True") "情形:"
            (MB (&rule (!- $emptyCtx (&: $true $bool))))
            $true "已经是一个值了, 所以终止.")
        (Li (RLabel "T-False") "情形:"
            (MB (&rule (!- $emptyCtx (&: $false $bool))))
            $false "已经是一个值了, 所以终止.")
        (Li (RLabel "T-Var") "情形:"
            (MB (&rule (&= (app Γ $x) $tau)
                       (G!- (&: $x $tau))))
            "根据归纳假设, 我们得到了"
            (&= (app $emptyCtx $x) $tau)
            ", 这意味着该情形空虚为真, "
            "鉴于我们的上下文为空. "
            "{译注: 不如说是并不适用于此情形.}")
        (Li (RLabel "T-Abs") "情形:"
            (MB (&rule (G!- (&: $x $tau_1) (&: $e $tau_2))
                       (G!- (&: (Lam $x $tau_1 $e)
                                (&-> $tau_1 $tau_2)))))
            (Lam $x $tau_1 $e)
            "已经是一个值了, 所以终止.")
        (Li (RLabel "T-If") "情形:"
            (MB (&rule (G!- (&: $e $bool))
                       (G!- (&: $e_1 $tau))
                       (G!- (&: $e_2 $tau))
                       (G!- (&: (If $e $e_1 $e_2) $tau))))
            "根据归纳假设, 我们知道"
            (&cm (&⇓ $e) (&⇓ $e_1) (&⇓ $e_2))
            ". 既然对于某个" $v "有" (&⇓ $e $v)
            ", 通过检视我们的归约语义, 我们知道"
            (let ((E (λ (e) (If e $e_1 $e_2))))
              (&\|->* (E $e) (E $v)))
            ". 感谢canonical form引理, "
            "我们知道具有类型" $bool
            "的值只能是" $true "和" $false
            ". 根据" $v "上的情形分析, 要么"
            (&\|->* (If $e $e_1 $e_2)
                    (If $true $e_1 $e_2)
                    $e_1 $v_1)
            ", 要么"
            (&\|->* (If $e $e_1 $e_2)
                    (If $false $e_1 $e_2)
                    $e_2 $v_2) ".")
        (Li (RLabel "T-App") "情形:"
            (MB (&rule (G!- (&: $e_1 (&-> $tau_2 $tau_1)))
                       (G!- (&: $e_2 $tau_2))
                       (G!- (&: (App $e_1 $e_2) $tau_1))))
            "根据归纳假设, 我们知道"
            (&⇓ $e_1) "和" (&⇓ $e_2 $v_2)
            ". 类型为" (&-> $tau_2 $tau_1)
            "的值是lambda表达式, 故"
            (&⇓ $e_1 (Lam $x $tau_2 $e^))
            ", 于是"
            (&\|->* (App $e_1 $e_2)
                    (App (@Lam $x $tau_2 $e^) $e_2)
                    (subst $e^ $v_2 $x))
            ". "
            (Span #:attr* '((style "color: red"))
                  "问题来了, 我们对于"
                  (subst $e^ $v_2 $x)
                  "的情况一无所知, 于是证明就卡住了."))))
   (P "我们这里主要的insight在于, 归纳假设太弱了: "
      "定型判断没有告诉我们关于规范化所产生的值的任何信息. "
      "为了解决这个问题, 我们将会定义一个逻辑关系, "
      "其精确地加强了归纳假设, "
      "这允许我们知道关于求值所产生的值的更多信息.")
   (H3. "定义逻辑关系的原则")
   (P "逻辑关系是表达式上的谓词" (app $P_tau $e)
      ", 由类型索引. 在定义一个逻辑关系时, "
      "通常我们需要遵循三条原则:"
      (Ul (Li "逻辑关系应包含良类型的项, 这里即"
              (!- $emptyCtx (&: $e $tau)) ";")
          (Li "我们所关心的性质" $P
              "应当内嵌于逻辑关系之中, "
              "这里即规范化;")
          (Li "我们所关心的性质应当在相应类型"
              $tau "的消去形式下得到保持. "
              "从直觉上来说, 这意味着我们有方法在消费了类型为"
              $tau "的值之后继续."))
      "接下来我们将会看到如何具体构造逻辑关系以证明STLC是规范化的.")
   (H3. "STLC规范化的逻辑关系")
   (P "我们根据STLC类型上的归纳定义规范化的逻辑谓词, "
      "遵循之前提及的三条原则:"
      (eqn*
       ((app (_ $N $bool) $e)
        $def= (!- $emptyCtx (&: $e $bool))
        $conj (&⇓ $e))
       ((app (_ $N (&-> $tau_1 $tau_2)) $e)
        $def= (!- $emptyCtx (&: $e (&-> $tau_1 $tau_2)))
        $conj (&⇓ $e) $conj
        (∀ $e^ (&==> (app (_ $N $tau_1) $e^)
                     (app (_ $N $tau_2) (App $e $e^))))))
      "与前述原则相比较, 我们可以看出, "
      "两种情况下最左边的合取分量都确保了逻辑关系中只包含良类型的项. "
      "类似地, 两种情况下的下一个合取分量确保了逻辑关系中"
      "只包含满足所需性质 (即规范化) 的项. 最后, "
      "我们需要确保所关注的性质在消去形式下得以保持, 这一点由"
      (&-> $tau_1 $tau_2) "情况中的第三个合取分量来处理. "
      "有趣的是, 我们无需对" $bool "的消去形式作任何说明, "
      "这是由其正极性所决定的. 作为一条经验法则, "
      "具有正极性的类型不需要任何额外的合取分量, "
      "而具有负极性的类型则需要.")
   (P "由此出发, 我们可以将规范化的证明拆分为两个步骤:"
      (Ol #:attr* '((type "A"))
          (Li "对于所有的项" $e ", 如果"
              (!- $emptyCtx (&: $e $tau))
              ", 那么" (app $N_tau $e) ";")
          (Li "对于所有的项" $e ", 如果"
              (app $N_tau $e) ", 那么"
              (&⇓ $e) ".")))
   (P "步骤B可以直接由逻辑谓词" $N_tau
      "的定义推出. 根据" $tau
      "上的归纳, 我们有两种需要考虑的情形, "
      "而每种情形下" (&⇓ $e)
      "都可以直接由" (app $N_tau $e)
      "的定义得到. 我们或许可以评注一下, "
      "这实际上来源于第二条原则: "
      "我们关心的性质 (规范化) "
      "就在逻辑关系的定义之中.")
   (P "然而, 由于lambda抽象的问题, "
      "步骤A并不如此直接. 设"
      (!- $emptyCtx (&: (Lam $x $tau_1 $e)
                        (&-> $tau_1 $tau_2)))
      ", 于是我们知道"
      (!- (&: $x $tau_1) (&: $e $tau_2))
      ". 为了证明A, 我们需要表明"
      (!- $emptyCtx (&: (Lam $x $tau_1 $e)
                        (&-> $tau_1 $tau_2)))
      ", 这由假设成立. 我们也必须表明"
      (&⇓ (Lam $x $tau_1 $e))
      ", 这之所以成立, 是因为lambda项已然为值. "
      "最后的一步是要证明对于任意的项" $e^
      ", 如果" (app (_ $N $tau_1) $e^) ", 那么"
      (app (_ $N $tau_2)
           (App (@Lam $x $tau_1 $e) $e^))
      ". 此时我们卡住了, 因为" $e "不是一个封闭项, "
      "于是我们不能通过定型推导上的归纳进行处理. "
      
      )
   (H2. "STLC的类型安全性")
   (H3. "STLC类型安全性的逻辑关系")
   (H2. "具有递归类型的STLC的类型安全性的逻辑关系")
   (H3. "递归类型的目的")
   (P "递归类型可以捕获潜在无限的数据结构, "
      "例如列表, 流, 树, 等等. "
      "加入递归类型到语言里允许非终止程序通过类型检查.")
   (P "为了刻画递归类型的用处, "
      "让我们检视无类型lambda演算里的"
      Ω "组合子, 其无限循环:"
      (MB (&= Ω (App (@Lam $x (App $x $x))
                     (@Lam $x (App $x $x))))))
   (P "如果我们试着将第一个项应用于第二个项, "
      "我们就又步入了" Ω "组合子. "
      "现在让我们试着在简单类型lambda演算中对于该项进行定型:"
      (MB (&= Ω (App (@Lam $x $? (App $x $x))
                     (@Lam $x $? (App $x $x))))))
   (P $x "的一个面貌是其在项" (@Lam $x (App $x $x))
      "中被应用于自身, 所以我们期望其类型差不多是"
      (&-> $tau_1 $tau_2) ". 然而, 作为函数"
      $x "的参数的" $x "的类型就不大对了. "
      "我们将会看到递归类型是如何帮助对于"
      Ω "组合子进行定型的.")
   (P "考虑描述了一个树的递归类型 "
      "(这里我们使用OCaml的句法):"
      (CodeB "type tree = Leaf | Node of int * tree * tree"))
   
   (H3. "具有递归类型的STLC的形式化")
   (H3. "具有递归类型的STLC的步骤索引逻辑关系")
   (H2. "多态性")
   (H3. "System F的形式化")
   (eqn*
    ($tau $::= (&\| $bool (&-> $tau $tau) $alpha (∀ $alpha $tau)))
    ($e   $::= (&\| $x $true $false (If $e $e $e)
                    (Lam $x $e) (LAM $alpha $e)
                    (ap $e (bra0 $tau)) (App $e $e)))
    ($v   $::= (&\| $true $false (Lam $x $tau $e)
                    (LAM $alpha $v)))
    ($E   $::= (&\| $hole (If $E $e_1 $e_2)
                    (App $E $e) (App $v $E)
                    (ap $E (bra0 $tau)))))
   
   (H3. "动机和自由定理")
   (H3. "System F的参数性的逻辑关系")
   
   ))