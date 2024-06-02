#lang racket
(provide lattice_notes.html)
(require SMathML)
(define idp
  (case-lambda
    ((a) (&= (&d* a a) a))
    ((& a) (&= (& a a) a))))
(define com
  (case-lambda
    ((&) (&= (& $a $b) (& $b $a)))
    ((a b) (&= (&d* a b) (&d* b a)))
    ((a & b) (&= (& a b) (& b a)))))
(define @d* (@lize &d*))
(define ass
  (case-lambda
    ((&) (&= (& (@ (& $a $b)) $c)
             (& $a (@ (& $b $c)))))
    ((a b c) (&= (&d* (@d* a b) c)
                 (&d* a (@d* b c))))
    ((& a b c) (&= (& (@ (& a b)) c)
                   (& a (@ (& b c)))))))
(define sor
  (case-lambda
    ((& *) (&= (& $a (@ (* $a $b))) $a))
    ((a & * b) (&= (& a (@ (* a b))) a))))
(define tuple tu0)
(define $join $disj)
(define $meet $conj)
(define $max (Mi "max"))
(define $min (Mi "min"))
(define (set-compact op)
  (set-attr* op 'lspace "0" 'rspace "0"))
(define $up (set-compact (Mo "&uarr;")))
(define $down (set-compact (Mo "&darr;")))
(define (&up P) (ap $up P))
(define (&down P) (ap $down P))
(define $exist (Mo "&exist;"))
(define s.t. (% "&nbsp;s.t.&nbsp;"))
(define (exist I P)
  (: $exist I s.t. P))
(define $forall (Mo "&forall;"))
(define $<=:compact
  (set-compact $<=))
(define poset:default
  (tuple $P $<=:compact))
(define (lower-topology P)
  (app $D:script P))
(define (upper-topology P)
  (app $U:script P))
(define-infix*
  (&join $join)
  (&meet $meet))
(define lattice_notes.html
  (TmPrelude
   #:title "瞎写的格论笔记"
   #:css "styles.css"
   (H1 "瞎写的格论笔记")
   (P "这里的笔记的目的主要是提供清晰的定义和随意的理解, 而不是任何其他东西.")
   ((definition)
    "对于集合" $P "和其上的一个二元关系" $<=
    ", 如果该二元关系是自反的, 传递的, 反对称的, 则称" $<= "为" $P
    "上的一个偏序, 而" $P "是一个偏序集, 有时也称" $P "是一个偏序. "
    "当然, 这里我们遵循通常的数学实践, 将" poset:default "记成了" $P
    ". 有的书籍会要求" $P "非空, 有的则不会.")
   ((definition)
    "如果不要求反对称性而只有自反和对称, 则我们就得到了预序的定义.")
   ((definition)
    "如果一个偏序集的任意两个元素之间均可比较大小, 则将其称为链, 或者线序, 全序.")
   ((definition)
    "对于偏序集" $P "和" (&sube $A $P) ", 若" (∈ $x $A) "和" (&<= $x $y)
    "可以推出" (∈ $y $A) ", 那么称" $A "是一个上集 (upper set). 当然, 这里的"
    (∈ $y $P) ". 对偶地, 可以定义下集 (lower set).")
   ((definition)
    "记" (upper-topology $P) "为偏序集" $P "的所有上集构成的族, "
    (lower-topology $P) "为偏序集" $P "的所有下集构成的族. "
    "它们都是" $P "上的Alexandrov拓扑.")
   ((definition)
    "对于偏序集" $P "和" (&sube $A $P) ", 我们定义" $A
    "的上闭包 (upper closure) 为"
    (MB (&= (&up $A)
            (setI (∈ $x $P)
                  (exist (∈ $a $A) (&<= $a $x)))))
    "对偶地, 可以定义" $A "的下闭包 (lower closure) "
    (&down $A) ". 我们注意到, " (&up $A)
    "是一个上集, " (&down $A) "是一个下集. 而且, " (&up $A)
    "是以" $A "为子集的最小上集, " (&down $A) "是以" $A
    "为子集的最小下集. 当" $A "是单元素集" (setE $x)
    "时, 我们将" (&up (setE $x)) "记为" (&up $x)
    ", " (&down (setE $x)) "记为" (&down $x) ".")
   ((definition)
    "对于偏序集" $P "和" (∈ $x $y $P) ", 如果"
    (&< $x $y) "且" (&<= $x $z $y) "可以推出"
    (&= $z $x) "或" (&= $z $y) ", 那么我们称"
    $y "覆盖 (cover) " $x ". 覆盖关系构成了Hasse图. "
    "但是, 在我们说Hasse图的时候, 往往指的不是图论的"
    "graph, 而指的是用以形象描绘偏序集的具体图示.")
   ((definition)
    "对于偏序集" $P ", 如果" (∈ $a $P) "满足对于每个"
    (∈ $x $P) ", " (&<= $a $x) ", 那么就称" $a
    "是" $P "的最小元. 对偶地, 可以定义最大元.")
   ((definition)
    "对于偏序集" $P ", 如果" (∈ $a $P) "满足对于每个"
    (∈ $x $P) ", 要么" $a "和" $x "不可比较, 要么"
    (&<= $a $x) ", 那么就称" $a "是" $P
    "的一个极小元. 换句话说, " (&<= $x $a)
    "可以推出" (&= $x $a) ". 对偶地, 可以定义极大元. "
    "最小元也是极小元, 最大元也是极大元.")
   ((definition)
    "对于偏序集" $P "和" (&sube $S $P)
    ", 称" (∈ $a $P) "是" $S "的一个上界, 如果对于每个"
    (∈ $s $S) ", " (&<= $s $a) ". " $S
    "的所有上界构成的集合记为" $S^u ". " $S^u
    "的最小元被称为最小上界. 对偶地, 我们可以定义下界, "
    $S^l ", 以及最大下界.")
   ((definition)
    "对于偏序集" $P "和" (∈ $x $y $P)
    ", 如果" (setE $x $y) "具有最小上界, 那么就将其记为"
    (&join $x $y) ", 读作" $x "和" $y
    "的并 (join). 对偶地, 我们可以定义" $x "和" $y
    "的交 (meet) " (&meet $x $y) ".")
   ((definition)
    "对于偏序集" $P ", 如果对于每个" (∈ $x $y $P)
    "都有" (&join $x $y) "存在, 那么就称" $P
    "是一个并半格 (join-semilattice). 对偶地, "
    "我们可以定义交半格 (meet-semilattice). 如果偏序集"
    $P "既是并半格又是交半格, 那么就称" $P
    "是一个格 (lattice).")
   ((definition)
    "对于格" $L ", 我们有以下性质."
    (Ol (Li "幂等律: " (idp &join $a) ", "
            (idp &meet $a) ";")
        (Li "交换律: " (com &join) ", "
            (com &meet) ";")
        (Li "结合律: " (ass &join) ", "
            (ass &meet) ";")
        (Li "吸收律: " (sor &join &meet) ", "
            (sor &meet &join) ".")))
   ((proposition)
    "偏序集" $L "是一个格当且仅当其任意的非空有限子集都有上确界和下确界, 也就是并和交.")
   
   ))