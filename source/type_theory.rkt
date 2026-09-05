#lang racket
(provide type_theory.html)
(require SMathML)
(define (PreFixed F X)
  (&sube (app F X) X))
(define (PostFixed F X)
  (&sube X (app F X)))
(define $wp (Mi "&wp;"))
(define (powerset X)
  (ap $wp X))
(define (∀ x P)
  (: $forall x $. P))
(define $->_beta (_ $-> $beta))
(define $impl $sup)
(define (μ F) (app $mu F))
(define (ν F) (app $nu F))
(define-infix*
  (&->_beta $->_beta)
  (&impl $impl))
(define-@lized-op*
  (@∀ ∀)
  (@impl &impl))
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
      "的最小不动点兑现了" (Em "归纳原理")
      ": 为了证明" (&sube (μ $F) $A)
      ", 证明" (PreFixed $F $A)
      "就足够了, 这是在说" $A
      "是" $F "封闭的. 类似地, " $F
      "的最大不动点" (ν $F)
      "兑现了" (Em "余归纳原理")
      ": 为了证明" (&sube $A (ν $F))
      ", 证明" (PostFixed $F $A)
      "就足够了, 这是在说" $A
      "是" $F "一致的. "
      "以谓词和后承 (implication) 的术语重述, "
      "单调函数" $F "的最小不动点" (μ $F)
      
      )
   ))