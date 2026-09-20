#lang racket
(provide notes0.html)
(require SMathML)
(define $Join $Disj)
(define $Meet $Conj)
(define-bigop*
  (Join $Join)
  (Meet $Meet))
(define $dummy (Mi "-"))
(define $Max (Mi "Max"))
(define $Min (Mi "Min"))
(define (&Max S)
  (app $Max S))
(define (&Min S)
  (app $Min S))
(define $Up (Mi "&uarr;"))
(define (&Up a) (ap $Up a))
(define $Down (Mi "&darr;"))
(define (&Down a) (ap $Down a))
(define (∀ x P)
  (: $forall x $. P))
(define (∃ x P)
  (: $exists x $. P))
(define $id (Mi "id"))
(define (&id A) (_ $id A))
(define $dashv (Mo "&dashv;"))
(define $rharu (Mo "&rharu;"))
(define $meet $conj)
(define $join $disj)
(define-infix*
  (&meet $meet)
  (&join $join)
  (&dashv $dashv)
  (&rharu $rharu))
(define (GaloisAdjunction f g P Q)
  (&: (&dashv f g) (&rharu P Q)))
(define-@lized-op*
  (@cap &cap)
  (@meet &meet)
  (@join &join))
(define notes0.html
  (TnTmPrelude
   #:title "笔记0"
   #:css "styles.css"
   (H1. "笔记0")
   (H2. "Galois伴随和连接")
   ((Definition)
    "对于偏序集" $P "和" $Q
    ", 其间单调映射"
    (func $f $P $Q) "和"
    (func $g $Q $P)
    "被称为是构成了一个Galois伴随, "
    "如果对于每个" (∈ $a $P) "和"
    (∈ $b $Q) ", 我们都有"
    (MB (&<= (app $f $a) $b) "等价于"
        (&<= $a (app $g $b)) ".")
    "我们将其记作"
    (MB (GaloisAdjunction $f $g $P $Q) ".")
    "我们称" $g "是" $f "的右伴随, " $f
    "是" $g "的左伴随.")
   (P "以下定理可以视为Galois伴随的另一种刻画, "
      "或者说等价的定义方式.")
   ((Theorem)
    "设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的单调映射, 则下列条件等价:"
    (Ol (Li (GaloisAdjunction $f $g $P $Q) ";")
        (Li (&cm (&<= (&id $P) (&i* $g $f))
                 (&<= (&i* $f $g) (&id $Q))) ".")))
   ((Definition)
    "对于偏序集" $P "上的映射" (func $h $P $P)
    ", 称" $h "是闭包算子, 如果" $h
    "是单调的, 幂等的, 增值的. "
    "增值的意思是对于每个" (∈ $x $P)
    ", 都有" (&<= $x (app $h $x))
    ". 称" $h "是内部算子, 如果" $h
    "是单调的, 幂等的, 减值的. "
    "减值的定义类比于增值. "
    "之所以取这些名字, "
    "一种理解方式为拓扑空间上的"
    "闭包运算和内部运算的确满足这些性质.")
   ((Theorem)
    "如果" (GaloisAdjunction $f $g $P $Q)
    ", 那么"
    (Ol (Li (&= (&i* $f $g $f) $f) ", "
            (&= (&i* $g $f $g) $g) ";")
        (Li (func (&i* $g $f) $P $P)
            "是闭包算子, "
            (func (&i* $f $g) $Q $Q)
            "是内部算子.")))
   (P "以下定理说明当我们知道" $f "和" $g
      "构成了Galois伴随时, " $f "和" $g
      "可以互相确定.")
   ((Theorem)
    "设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的单调映射, 那么下列条件等价:"
    (Ol (Li (GaloisAdjunction $f $g $P $Q) ";")
        (Li (∀ (∈ $a $P)
               (&= (app $f $a) (&Min (app (inv $g) (&Up $a))))) ";")
        (Li (∀ (∈ $b $Q)
               (&= (app $g $b) (&Max (app (inv $f) (&Down $b))))) "."))
    "这里的" $Min "表示最小元, " $Max "表示最大元.")
   ((Theorem)
    "设" (GaloisAdjunction $f $g $P $Q)
    ", 则下列条件等价:"
    (Ol (Li $f "是单射;")
        (Li (&= (&i* $g $f) (&id $P)) ";")
        (Li $g "是满射.")))
   ((Definition)
    "设" (func $h $P $Q) "是偏序集之间的一个映射."
    (Ol (Li "如果主理想的原像是主理想, 则称"
            $h "为剩余映射 (residuated mapping).")
        (Li "如果主滤子的原像是主滤子, 则称"
            $h "为残余映射 (residual mapping).")))
   ((Theorem)
    "设" (func $h $P $Q)
    "是偏序集之间的一个单调映射, 那么"
    (Ol (Li $h "有右伴随当且仅当" $h "是一个剩余映射;")
        (Li $h "有左伴随当且仅当" $h "是一个残余映射.")))
   
   ((Theorem)
    "在Galois伴随中, 左伴随保持任意存在的并, 右伴随保持任意存在的交. "
    "更细致地说, 对于偏序集" $P "和" $Q
    "之间的Galois伴随" $f "和" $g
    ", 对于" (&sube $S $P)
    ", 若" (Join $S) "存在, 则"
    (Join (app $f $S)) "存在, 且"
    (&= (app $f (Join $S))
        (Join (app $f $S)))
    "; 对于" (&sube $T $Q)
    ", 若" (Meet $T) "存在, 则"
    (Meet (app $g $T)) "存在, 且"
    (&= (app $g (Meet $T))
        (Meet (app $g $T))) ".")
   ((Theorem)
    "设" (&cm $P $Q) "是两个完备格."
    (Ol (Li "保序映射" (func $f $P $Q)
            "保持任意的并当且仅当" $f
            "有右伴随" (func $g $Q $P) ", 且"
            (&= (app $g $y)
                (Join (setI (∈ $x $P)
                            (&<= (app $f $x) $y))))
            ";")
        (Li "保序映射" (func $g $Q $P)
            "保持任意的交当且仅当" $g
            "有左伴随" (func $f $P $Q) ", 且"
            (&= (app $f $x)
                (Meet (setI (∈ $y $Q)
                            (&<= $x (app $g $y))))) ".")))
   
   (H2. "Heyting代数")
   ((Definition)
    "设" $H "是一个格, 若存在二元运算"
    (func $-> (&c* $H $H) $H) "使得"
    (MB (∀ (∈ $a $b $c $H)
           (&<==> (&<= (&meet $c $a) $b)
                  (&<= $c (&-> $a $b)))))
    "则称" $H "为Heyting代数.")
   ((Theorem)
    "设" $H "是一个格, 则下列条件等价:"
    (Ol (Li $H "为Heyting代数;")
        (Li "对于任意的" (∈ $a $H) ", "
            (tu0 (&meet $a (@ $dummy))
                 (&-> $a (@ $dummy)))
            "构成" $H "上的Galois伴随;")
        (Li "对于任意的" (∈ $a $b $H) ", "
            (setI (∈ $x $H)
                  (&<= (&meet $a $x) $b))
            "存在最大元.")))
   (P "鉴于伴随可以相互确定, "
      "以上定理说明了实际上" $->
      "运算若存在则是唯一的.")
   ((Theorem)
    "每个Heyting代数都是分配格.")
   ((Theorem)
    "每个有限分配格都是Heyting代数.")
   ((Theorem)
    "设" $H "是一个Heyting代数, 则对于任意的"
    (∈ $a $b $c $H) ", 我们有"
    (Ol (Li (&<= $b (&-> $a $b)) ";")
        (Li (&= (&-> $1 $a) $a) ";")
        (Li (&<==> (&= (&-> $a $b) $1)
                   (&<= $a $b)) ";")
        (Li (&= (&meet $a (@-> $a $b))
                (&meet $a $b)) ";")
        (Li (&-> $a (@ $dummy)) "保序, "
            (&-> (@ $dummy) $a) "逆序;")
        (Li (distributeR $a &-> $b &meet $c) ";")
        (Li (&= (&-> (@join $a $b) $c)
                (&meet (@-> $a $c)
                       (@-> $b $c))) ";")
        (Li (&= (&-> $a (@-> $b $c))
                (&-> (@meet $a $b) $c)) ";")
        (Li (&<= $a (&-> (@-> $a $b) $b)) ";")
        (Li (&= (&-> (@-> (@-> $a $b) $b) $b)
                (&-> $a $b)) ";")
        (Li (&<= (&meet (@-> $a $b) (@-> $b $c))
                 (&-> $a $c)) ".")))
   (H2. "frame")
   ((Definition)
    "设" $L "是一个完备格, 如果对于任意的"
    (∈ $a $L) "和" (&sube $S $L)
    ", 都有"
    (MB (&= (&meet $a (Join $S))
            (Join (∈ $s $S) (@meet $a $s))))
    "则称" $L "为一个frame. "
    "我们将这种性质叫做无限分配律.")
   ((Theorem)
    "对于一个完备格" $L "而言, "
    $L "是一个Heyting代数当且仅当"
    $L "满足无限分配律, 即" $L
    "是一个frame.")
   
   ))