#lang racket
(provide notes0.html)
(require SMathML)
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
   
   ))