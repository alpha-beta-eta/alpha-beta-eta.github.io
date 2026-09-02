#lang racket
(provide type_theory.html)
(require SMathML)
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (: (apply &rule x*) label))
(define split2 (&split 2))
(define split16 (&split 16))
(define (Mi:sans-serif str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define (Mi:monospace str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define (Mi:bold str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define $ZERO (Mi "ZERO"))
(define $zero (Mi:monospace "zero"))
(define $SUCC (Mi "SUCC"))
(define $succ (Mi:monospace "succ"))
(define (&succ n)
  (app $succ n))
(define $nat (Mi:sans-serif "nat"))
(define (&nat n)
  (split2 n $nat))
(define $even (Mi:sans-serif "even"))
(define (&even n)
  (split2 n $even))
(define $odd (Mi:sans-serif "odd"))
(define (&odd n)
  (split2 n $odd))
(define $ZERO-EVEN (Mi "ZERO-EVEN"))
(define $SUCC-ODD (Mi "SUCC-ODD"))
(define $SUCC-EVEN (Mi "SUCC-EVEN"))
(define $type:one (Mi:bold "1"))
(define $type:two (Mi:bold "2"))
(define $yes (Mi:sans-serif "yes"))
(define $no (Mi:sans-serif "no"))
(define $star (Mi:sans-serif "&Star;"))
(define $proj:first (Mi:sans-serif "1"))
(define (&proj:first M)
  (&d* M $proj:first))
(define $proj:second (Mi:sans-serif "2"))
(define (&proj:second M)
  (&d* M $proj:second))
(define (Lam A x M)
  (app (_ $lambda A) (&. x M)))
(define $ap (Mi:sans-serif "ap"))
(define (&ap M1 M2)
  (appl $ap M1 M2))
(define $VAR (Mi "VAR"))
(define $YES (Mi "YES"))
(define $NO (Mi "NO"))
(define $UNIT (Mi "UNIT"))
(define $PAIR (Mi "PAIR"))
(define $LFT (Mi "LFT"))
(define $RHT (Mi "RHT"))
(define $LAM (Mi "LAM"))
(define $APP (Mi "APP"))
(define $LFT-PAIR (Mi "LFT-PAIR"))
(define $RHT-PAIR (Mi "RHT-PAIR"))
(define $APP-LAM (Mi "APP-LAM"))
(define $final (Mi:sans-serif "final"))
(define (&final M)
  (split2 M $final))
(define (PreFixed F X)
  (&sube (app F X) X))
(define (PreFixed0 F X)
  (&<= (app F X) X))
(define (PostFixed F X)
  (&sube X (app F X)))
(define (PostFixed0 F X)
  (&<= X (app F X)))
(define $wp (Mi "&wp;"))
(define (powerset X)
  (ap $wp X))
(define (∀ x P)
  (: $forall x $. P))
(define $->_beta (_ $-> $beta))
(define $impl $sup)
(define μ
  (case-lambda
    ((F) (app $mu F))
    ((F G) (appl $mu F G))))
(define ν
  (case-lambda
    ((F) (app $nu F))
    ((F G) (appl $nu F G))))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $vdash (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define $step (Mo "&xmap;"))
(define $step* (&* $step))
(define (subst M x N)
  (ap (bra0 (&/ M x)) N))
(define-infix*
  (&step $step)
  (&step* $step*)
  (&->_beta $->_beta)
  (&impl $impl)
  (&. $.))
(define Join Disj)
(define Meet Conj)
(define-@lized-op*
  (@∀ ∀)
  (@impl &impl)
  (@. &.))
(define type_theory.html
  (TnTmPrelude
   #:title "计算类型论"
   #:css "styles.css"
   (H1. "计算类型论")
   (P "这是Robert Harper写下的一系列关于类型论的讲义, "
      "主要是从更语义的视角看待类型论.")
   (H2. "如何(重新)发明Tait的方法")
   (H3. "引论")
   (P "类型论中两个最为重要的发展分别是由W. W. Tait发明的函数类型的"
      (Em "Tait方法") ", 以及之后由J. Y. Girard扩展得到的类型量化的"
      (Em "Girard方法") ", 这两种方法融为一体形成了"
      (Em "逻辑关系") "这个一般性理论, 可用于诸多类型理论. "
      "Tait方法继续以其原始名称而为人所知, 即" (Em "可计算性方法")
      ", 其以下面要建立的方式将类型解释为谓词.")
   (P "Tait所考虑的问题是证明对于简单类型" $lambda
      "演算而言" $beta "归约是" (Em "强规范化的")
      ", 这通常定义为不存在自某个良类型项开始的无限" $beta
      "归约序列: " (&= $M (&->_beta $M_0 $M_1 $..c))
      ". 一种更好的定义 (具有直接的可用性) 为"
      (Em "归约上的超限归纳") "的有效性, 陈述如下: "
      "对于任意的类型化" $lambda "项的性质" $P:script
      ", 为了表明" $P:script "对于所有这样的项都成立, "
      "仅需表明对于每个良类型的类型化项" $M
      ", 若其所有直接" $beta "归约项 (reduct) 满足"
      $P:script "则能够推出" $M "也满足" $P:script
      ". 更简单地说, 即"
      (MB (&impl
           (@∀ (&: $M $tau)
               (@impl (@∀ (&: $N $tau)
                          (&impl (&->_beta $M $N)
                                 (app $P:script $N)))
                      (app $P:script $M)))
           (∀ (&: $M $tau) (app $P:script $M))) ".")
      "强规范化的重要性恰恰就在于由这条原理兑现的证明其他性质的实用性. "
      "例如, 使用归约上的超限归纳, 可以证明弱合流性 "
      )
   (H3. "简单类型")
   (P "这里所考虑的语言的句法由以下语法给出:"
      (eqn*
       ($A $::= (&\| $type:one
                     $type:two
                     (&c* $A_1 $A_2)
                     (&-> $A_1 $A_2)))
       ($M $::= (&\| $x $yes $no $star
                     (tupa0 $M_1 $M_2)
                     (&proj:first $M)
                     (&proj:second $M)
                     (Lam $A $x $M)
                     (&ap $M_1 $M_2)))))
   (P "其静态(语义)全然是标准的, "
      "其以结构性质admissible的方式定义了类型判断"
      (G!- (&: $M $A))
      ". 收缩和交换是通过将类型上下文" Γ
      "处理为变量定型的有限集合"
      (&cm (&: $x_1 $A_1) $..h (&: $x_n $A_n))
      "得到的, 其中每当" (&!= $i $j) "则"
      (&!= $x_i $x_j) ". 弱化是内建的, "
      "通过陈述规则时都顺带搭上ambient类型环境" Γ
      ". 定型的定义见图1. 替换 (传递性), 即如果"
      (G!- (&: $x $A) (&: $N $B)) "且"
      (G!- (&: $M $A)) "则能推出"
      (G!- (&: (subst $M $x $N) $B))
      ", 可由在第一个前提上进行归纳证明.")
   (MB (split16
        (&rull $VAR
               (G!- (&: $x $A) (&: $x $A)))
        (&rull $YES
               (G!- (&: $yes $type:two)))
        (&rull $NO
               (G!- (&: $no $type:two)))
        (&rull $UNIT
               (G!- (&: $star $type:one)))))
   (MB (split16
        (&rull $PAIR
               (G!- (&: $M_1 $A_1))
               (G!- (&: $M_2 $A_2))
               (G!- (&: (tupa0 $M_1 $M_2)
                        (&c* $A_1 $A_2))))
        (&rull $LFT
               (G!- (&: $M (&c* $A_1 $A_2)))
               (G!- (&: (&proj:first $M) $A_1)))
        (&rull $RHT
               (G!- (&: $M (&c* $A_1 $A_2)))
               (G!- (&: (&proj:second $M) $A_2)))))
   (MB (split16
        (&rull $LAM
               (G!- (&: $x $A_1) (&: $M_2 $A_2))
               (G!- (&: (Lam $A_1 $x $M_2) (&-> $A_1 $A_2))))
        (&rull $APP
               (G!- (&: $M_1 (&-> $A_2 $A)))
               (G!- (&: $M_2 $A_2))
               (G!- (&: (&ap $M_1 $M_2) $A)))))
   (P "动态(语义)由具有某个类型的封闭" $lambda
      "项之间的转换系统" (&step $M $M^)
      "给出. 任何封闭的类型化项都是合法的初始状态. "
      "终状态和转换关系一并在图2中定义.")
   (MB (split16
        (&rull $YES
               (&final $yes))
        (&rull $NO
               (&final $no))
        (&rull $UNIT
               (&final $star))
        (&rull $PAIR
               (&final (tupa0 $M_1 $M_2)))))
   (MB (split16
        (&rull $LFT
               (&step $M $M^)
               (&step (&proj:first $M)
                      (&proj:first $M^)))
        (&rull $RHT
               (&step $M $M^)
               (&step (&proj:second $M)
                      (&proj:second $M^)))))
   (MB (split16
        (&rull $LFT-PAIR
               (&step (&proj:first (tupa0 $M_1 $M_2))
                      $M_1))
        (&rull $RHT-PAIR
               (&step (&proj:second (tupa0 $M_1 $M_2))
                      $M_2))))
   (MB (split16
        (&rull $LAM
               (&final (Lam $A_1 $x $M_2)))
        (&rull $APP
               (&step $M_1 (_prime $M $1))
               (&step (&ap $M_1 $M_2)
                      (&ap (_prime $M $1) $M_2)))))
   (MB (&rull $APP-LAM
              (&step (&ap (Lam $A_2 $x $M) $M_2)
                     (subst $M_2 $x $M))))
   ((theorem #:n "1. 保持性")
    "如果" (&: $M $A) "而" (&step $M $M^)
    ", 那么" (&: $M^ $A) ".")
   ((proof)
    "根据转换关系上的归纳.")
   (H3. "终止性证明")
   (P "目标在于对于可观察类型的项证明终止性.")
   ((theorem #:n "2. 终止性")
    "如果" (&: $M $type:two) ", 那么要么"
    (&step* $M $yes) ", 要么" (&step* $M $no) ".")
   (P "也就是说, 任意完备的程序要么接受要么拒绝.")
   (P "考虑到该定理的陈述方式, "
      "几乎唯一可能的动作就是藉由定型上的归纳进行处理. "
      "让我们考虑一些情形.")
   (P $VAR "不适用于封闭项.")
   (P "个人感觉不如说是定型上下文非空, 但我们要求空的上下文.")
   (P $YES "立即, 鉴于" (&final $yes) ".")
   (P $NO "立即, 鉴于" (&final $no) ".")
   (P $UNIT "并不适用, 因为不具有类型" $type:two ".")
   (P $PAIR "并不适用, 因为不具有类型" $type:two ".")
   (P $LFT "根据归纳, ...")
   (P $RHT "根据归纳, ...")
   (P $LAM "并不适用, 因为不具有类型" $type:two ".")
   (P $APP "根据第一个前提上的归纳, ...")
   (P "所有情形要么是平凡的, 要么完全不清楚.")
   
   ((lemma #:n "3")
    "如果" (&: $M $A) ", 那么存在" $N "使得"
    (&final $N) "且" (&step* $M $N) ".")
   
   (H2. "幂集的Tarski不动点定理")
   (H3. "引论")
   (P "Tarski定理说的是完备格上的单调函数具有由不动点构成的完备格, "
      "特别是有最小不动点和最大不动点. "
      "{译注: 即其不动点构成了完备格.} "
      "一类实用的特殊情形是由包含关系排序的幂集格.")
   (H3. "Tarski定理")
   (P "令" $X "是一个集合, 不必非空, 并令" (powerset $X)
      "是由" $X "的所有子集构成的集合. 集合" (powerset $X)
      "在集合包含关系下构成了一个完备格, 其meet由交给出, "
      "join由并给出. 也就是说, 如果"
      (&sube $X:script (powerset $X))
      ", 那么" (Cap $X:script) "是其meet (最大下界), 而"
      (Cup $X:script) "是其join (最小上界). "
      "最小元素是空集的join, 即" $empty
      ", 而最大元素是空集的meet, 即" $X ".")
   (P "一个函数" (func $F (powerset $X) (powerset $X))
      "是单调的, 如果其保持包含关系: 如果"
      (&sube $A $B $X) ", 那么"
      (&sube (app $F $A) (app $F $B) $X)
      ". 对于" (powerset $X) "上的单调函数" $F
      ", " $F "的一个前不动点是一个集合" (&sube $A $X)
      "满足" (PreFixed $F $A) ", 而" $F
      "的一个后不动点是一个集合" (&sube $A $X)
      "满足" (PostFixed $F $A)
      ". " $F "的前不动点也被称为是" $F
      "封闭的, " $F "的后不动点也被称为是" $F
      "一致的. 单调函数" $F "的最小前不动点指的是"
      "(在包含关系下)最小的" $F "封闭集合, " $F
      "的最大后不动点指的是最大的" $F
      "一致集合. 将这种格视为一个(瘦)范畴的话, "
      "其上的一个单调函数" $F "是一个函子, "
      $F "的一个前不动点是一个" $F "代数, 而"
      $F "的一个后不动点是一个" $F
      "余代数. 因此, " $F "的一个最小前不动点是一个始"
      $F "代数, " $F "的一个最大后不动点是一个终"
      $F "余代数.")
   (P "每个单调函数" (func $F (powerset $X) (powerset $X))
      "都有一个(唯一的)最小前不动点和一个(唯一的)最大后不动点, "
      "由以下等式给出:"
      (MB (&Table
           ((μ $F) $= (Cap (setI (&sube $A $X)
                                 (PreFixed $F $A))))
           ((ν $F) $= (Cup (setI (&sube $A $X)
                                 (PostFixed $F $A))))))
      "显然" (μ $F) "被包含于所有" $F
      "的前不动点之中, 鉴于其是它们之交. "
      "{译注: 即" (μ $F) "是每个" $F "的前不动点的子集.} "
      "实际上, " (μ $F) "本身就是" $F "的一个前不动点, 即"
      (PreFixed $F (μ $F)) ", 因而是最小前不动点. "
      "为了看出这一点, 表明如果" (PreFixed $F $A)
      "则有" (&sube (app $F (μ $F)) $A)
      ". {译注: 这说明" (app $F (μ $F)) "是"
      (setI (&sube $A $X) (PreFixed $F $A))
      "的一个下界, 而" (μ $F) "根据定义是其最大下界.} "
      "但是如果" (PreFixed $F $A) ", 那么根据定义有"
      (&sube (μ $F) $A) ", 然后根据单调性可得"
      (&sube (app $F (μ $F)) (app $F $A) $A)
      ", 而这正是我们所要的. 接着我们又运用单调性可知"
      (PreFixed $F (app $F (μ $F)))
      ", 这是在说" (app $F (μ $F)) "是" $F
      "的一个前不动点, 因而"
      (PostFixed $F (μ $F))
      ". 换言之, " (μ $F) "是" $F
      "的一个不动点. 并且, 鉴于任何的不动点都是前不动点, "
      (μ $F) "是最小的不动点. 对偶地, " (ν $F)
      "包含了所有" $F "的后不动点, 鉴于其是它们之并. "
      "并且, 通过和之前对偶的论证, " (ν $F) "是" $F
      "的一个后不动点. 因此, 其是最大的后不动点, "
      "又是最大的不动点. (用范畴论的语言来说, "
      "这是Lambek引理, 其是说始" $F
      "代数和终" $F "余代数都是同构.)")
   (P (powerset $X) "上的单调函数" $F
      "的最小前不动点兑现了" (Em "归纳原理")
      ": 为了证明" (&sube (μ $F) $A)
      ", 证明" (PreFixed $F $A)
      "就足够了, 这是在说" $A
      "是" $F "封闭的. 类似地, " $F
      "的最大后不动点" (ν $F)
      "兑现了" (Em "余归纳原理")
      ": 为了证明" (&sube $A (ν $F))
      ", 证明" (PostFixed $F $A)
      "就足够了, 这是在说" $A
      "是" $F "一致的. "
      "以谓词和后承 (implication) 的术语重述, "
      "单调函数" $F "的最小不动点" (μ $F)
      "是关于" $X "的元素的性质" $A
      "中满足若" (∈ $x (app $F $A)) "则"
      (∈ $x $A) "的" (Em "最强")
      "性质. 对偶地, 单调函数" $F
      "的最大不动点是关于" $X "的元素的性质"
      $A "中满足若" (∈ $x $A) "则"
      (∈ $x (app $F $A)) "的" (Em "最弱")
      "性质. {译注: 这里性质的弱强由满足性质的元素多寡度量, "
      "直觉是如果一个性质被很少的元素满足, "
      "那么我们知道一个元素满足该性质的话, "
      "这就是很多的信息.}")
   (P "举个例子以说明我所言非虚, 存在两个对于"
      (&sube (μ $F) (ν $F))
      "的证明, 一个用到了" (μ $F)
      "的最小性, 另一个用到了" (ν $F)
      "的最大性. 因为" (μ $F) "是" $F
      "的最小前不动点, 其本身是" $F
      "封闭的, 而因为" (ν $F) "是" $F
      "的最大后不动点, 其本身是" $F
      "一致的. 因此, 为了表明包含关系成立, "
      "要么证明" (ν $F) "是" $F
      "封闭的, 即" (PreFixed $F (ν $F))
      ", 要么证明" (μ $F) "是" $F
      "一致的, 即" (PostFixed $F (μ $F))
      ", 就足够了. 但是, 这些恰恰是之前为了论证"
      (μ $F) "和" (ν $F) "是" $F
      "的不动点而得到的逆向包含关系.")
   (P "关于单调函数" $F "的最小前不动点和最大后不动点的"
      "另一视角由以下对于归纳和余归纳证明的可视化给出. "
      "为了表明" (μ $F) "的每个元素也在表示了我们所关心的性质的集合"
      $A "之中, 仅需表明" (&cap (μ $F) $A) "在" $F
      "下是封闭的. 因为若的确如此, 则" (μ $F)
      "被包含于这个交, 因而也就包含于" $A
      "之中了. 这个交" (&cap (μ $F) $A)
      (Em "先验地") "就小于(等于)" (μ $F)
      ", 仅仅包含了" (μ $F) "中那些" (Q "足够好")
      "以具有性质" $A "的元素. "
      "但是如果这个交不那么具有限制性到破坏"
      $F "封闭性的地步, "
      "那么实际上这个交压根就不构成限制, 即"
      (&= (&cap (μ $F) $A) (μ $F))
      ". 对偶地, 为了表明某个元素集合" (&sube $A $X)
      "包含于" (ν $F) "之中, 仅需大胆断言本就如此, "
      "通过构造" (&cup (ν $F) $A) ", 这" (Em "先验地")
      "就大于(等于)" (ν $F) ". 但是如果这个并是"
      $F "一致的, 那么这关于" $A "的断言就不言自明了, 因而"
      (&sube (&cup (ν $F) $A) (ν $F))
      ". 这个并实际上并没有比" (ν $F)
      "更大" -- $A "的元素一开始就在其中.")
   (P "我们也能够表明单调函数的任意不动点集合的meet和join仍然是不动点, "
      "但是似乎这不像最小不动点和最大不动点的构造那样有用. "
      "因为我们这里的展开仅仅依赖于交和并的泛性质, "
      "即其相对于集合包含关系是meet和join, "
      "得到对于完备格而言的一般形式Tarski定理是直截了当的, "
      "those pre-orders for which all subsets "
      "have meets and joins. "
      "{译注: 我不太懂数学, 我不知道这里用"
      "预序而不是偏序是不是有什么深意, "
      "抑或是仅是单纯的笔误.}")
   ((proposition #:n "(仅为译注补充)")
    "对于完备格" $X "和其上的单调函数" $F
    ", " $F "的最小不动点和最大不动点均存在.")
   ((proof)
    "令"
    (eqn*
     ((μ $F) $= (Meet (setI (∈ $x $X) (PreFixed0 $F $x))))
     ((ν $F) $= (Join (setI (∈ $x $X) (PostFixed0 $F $x)))))
    "对于每个" (∈ $x $X) "满足" (PreFixed0 $F $x)
    ", 我们希望证明" (&<= (app $F (μ $F)) $x)
    ". 鉴于" (μ $F) "本身就是"
    (setI (∈ $x $X) (PreFixed0 $F $x))
    "的一个下界, 我们有"
    (&<= (app $F (μ $F))
         (app $F $x) $x)
    ". 换言之, " (app $F (μ $F)) "也是"
    (setI (∈ $x $X) (PreFixed0 $F $x))
    "的一个下界, 而" (μ $F)
    "是最大下界, 故有"
    (&<= (app $F (μ $F)) (μ $F))
    ", 即" (μ $F) "是" $F "的一个前不动点. "
    "当然, 最小性已由定义给出, 所以"
    (μ $F) "是" $F "的最小前不动点. "
    "接着, 继续根据单调性, 可知"
    (MB (PreFixed0 $F (app $F (μ $F))))
    "即" (app $F (μ $F)) "也是"
    $F "的一个前不动点, 所以有"
    (MB (&<= (μ $F) (app $F (μ $F))))
    "根据反对称性, 我们就得到了"
    (MB (&= (app $F (μ $F)) (μ $F)))
    "即" (μ $F) "是" $F
    "的一个不动点. 并且, 对于每个" $F
    "的不动点" $x ", " $x "也是" $F
    "的前不动点 (自反性), 那么"
    (&<= (μ $F) $x) ", 即"
    (μ $F) "是" $F "的最小不动点. "
    "如法炮制, 可以证明" (ν $F) "是" $F
    "的最大后不动点, 也是最大不动点.")
   (H3. "Bekić引理")
   (P "有时会出现两个集合 (性质) 是" (Em "同时")
      "归纳定义的, 因为一个定义会依赖于另一个. "
      "这种情况可以通过考虑两个单调算子"
      (func (&cm $F $G)
            (&c* (powerset $X) (powerset $X))
            (powerset $X))
      "来表达, 单调意即如果"
      (&sube $A $A^) "且" (&sube $B $B^)
      ", 那么"
      (&sube (appl $F $A $B)
             (appl $F $A^ $B))
      "而"
      (&sube (appl $G $A $B)
             (appl $G $A $B^))
      ". 算子"
      (&def= (appl (tu0 $F $G) $A $B)
             (tu0 (appl $F $A $B)
                  (appl $G $A $B)))
      "因而相对于逐分量序是单调的, "
      "这个序指的是"
      (&sube (tu0 $A $B) (tu0 $A^ $B^))
      "当且仅当"
      (&sube $A $A^) "且" (&sube $B $B^)
      ". 根据和之前给出的论证类似的方法, "
      "其有一个最小不动点" (μ $F $G)
      ", 这是" $X "的子集的一个序对"
      (tu0 $A_0 $B_0) ", 满足"
      (&= (appl $F $A_0 $B_0) $A_0) "而"
      (&= (appl $G $A_0 $B_0) $B_0)
      ", 每个都要" (Q "交叉引用")
      "另一个, 这是我们所期望的.")
   
   (H3. "断言和规则")
   (P "不动点构造的一个典型应用是通过一集" (Em "规则")
      "来澄清一个或多个" (Em "断言") ", 或者" (Em "形式判断")
      ". 想法在于规则构成了提及的断言的归纳定义. "
      "例如, 以下规则定义了判断"
      (MB (split16
           (&rull $ZERO
                  (&nat $zero))
           (&rull $SUCC
                  (&nat $n)
                  (&nat (&succ $n)))))
      "类似地, 偶数和奇数可以由如下规则同时定义:"
      (MB (split16
           (&rull $ZERO-EVEN
                  (&even $zero))
           (&rull $SUCC-ODD
                  (&even $n)
                  (&odd (&succ $n)))
           (&rull $SUCC-EVEN
                  (&odd $n)
                  (&even (&succ $n)))))
      "在这两类情况下, 断言的subject都是Harper (2016) "
      "意义下的抽象绑定树.")
   
   (H2. "规范化的Kripke风格逻辑关系")
   
   ))