#lang racket
(provide iris-lecture-notes.html)
(require SMathML)
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
(define $Inl (Mi "inl"))
(define $Inr (Mi "inr"))
(define (Inl t) (ap $Inl t))
(define (Inr t) (ap $Inr t))
(define $Case (Mi "case"))
(define (Case t c1 c2)
  (appl $Case t c1 c2))
(define $False (Pid "False"))
(define $True (Pid "True"))
(define $impl $=>)
(define $wand
  (Mo "&minus;&#8270;"))
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
   (H2. "引论")
   (H2. "编程语言")
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
      $Inl "和" $Inr ") 以及对应的消去形式"
      $Case ".")
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
      "我们由通常的" $eta "和" $beta "规则."
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