#lang racket
(provide logical_relations.html)
(require SMathML)
(define $dummy $d*)
(define $.
  (Mo "." #:attr* '((lspace "0"))))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define Γ $Gamma:normal)
(define $!- (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define $bool (Mi "bool"))
(define $true (Mi "true"))
(define $false (Mi "false"))
(define $if (Mi "if"))
(define $then (Mi "then"))
(define $else (Mi "else"))
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
(define $\| (Mo "|"))
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
      "程序终止性, 类型安全性与健全性均属于逻辑谓词. "
      "而逻辑关系" (app $R_tau $e_1 $e_2)
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
    (!- $dummy (&: $e $tau))
    ", 那么" (&⇓ $e) ".")
   (H3. "第一次尝试: 以定型推导上的归纳证明STLC是规范化的")
   (H3. "定义逻辑关系的原则")
   (P "逻辑关系是表达式上的谓词" (app $P_tau $e)
      ", 由类型索引. 在定义一个逻辑关系时, "
      "通常我们需要遵循三条原则:"
      (Ul (Li "逻辑关系应包含良类型的项, 这里即"
              (!- $dummy (&: $e $tau)) ";")
          (Li "我们所关心的性质" $P
              "应当内嵌于逻辑关系之中, "
              "这里即规范化;")
          (Li "我们所关心的性质应当在相应类型"
              $tau "的消去形式下得到保持. "
              "从直觉上来说, 这意味着我们有方法在消费了类型为"
              $tau "的值之后继续."))
      "接下来我们将会看到如何具体构造逻辑关系以证明STLC是规范化的.")
   (H3. "STLC规范化的逻辑关系")
   
   (H2. "STLC的类型安全性")
   (H3. "STLC类型安全性的逻辑关系")
   (H2. "具有递归类型的STLC的类型安全性的逻辑关系")
   (H3. "递归类型的目的")
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