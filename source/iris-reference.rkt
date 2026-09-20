#lang racket
(provide iris-reference.html)
(require SMathML)
(define (&eq n) (^^ $= n))
(define $eq0 (&eq $0))
(define $eqn (&eq $n))
(define $eqm (&eq $m))
(define $impl $=>)
(define-infix*
  (&eq0 $eq0)
  (&eqn $eqn)
  (&eqm $eqm)
  (&impl $impl))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define-@lized-op*
  (@∀ ∀))
(define iris-reference.html
  (TnTmPrelude
   #:title "iris参考"
   #:css "styles.css"
   (H1. "iris参考")
   (H2. "iris from the ground up")
   (H2. "代数结构")
   (H3. "OFE")
   (P "iris的模型活在有序等价族 (OFE) 的范畴之中. "
      "这里的定义和原本[2]中的定义有些许不同.")
   ((Definition)
    "一个有序等价族是一个元组"
    (tu0 $T (_ (@ (&sube $eqn (&c* $T $T)))
               (∈ $n $NN)))
    "满足"
    (MBL "(OFE-EQUIV)"
         (∀ $n (: (@ $eqn) "是一个等价关系")))
    (MBL "(OFE-MONO)"
         (∀ $n $m
            (&impl (&>= $n $m)
                   (&sube (@ $eqn) (@ $eqm)))))
    (MBL "(OFE-LIMIT)"
         (∀ $x $y
            (&<=> (&= $x $y)
                  (@∀ $n (&eqn $x $y)))))
    "OFE背后的关键直觉在于元素" $x "和" $y
    "是" $n "-等价的, 记号" (&eqn $x $y)
    ", 如果它们"
    (Em "对于" $n "步计算是等价的")
    ", 即它们不能由运行不超过" $n
    "步的程序区分开来. 换言之, 随着"
    $n "的增长, " $eqn "变得愈发精良 (OFE-MONO)"
    -- "并且在极限情况下, "
    "其就与平直的相等性重合 (OFE-LIMIT).")
   
   ((Definition)
    
    )
   ((Definition)
    "一个OFE的一个元素" (∈ $x $T)
    "称为离散的, 如果"
    (MB (∀ (∈ $y $T)
           (&impl (&eq0 $x $y)
                  (&= $x $y))))
    "一个OFE被称为离散的, "
    "如果其所有元素都是离散的. "
    "对于一个集合" $X
    
    )
   (H3. "COFE")
   (H3. "RA")
   (H3. "相机")
   (H2. "OFE和COFE构造")
   (H2. "RA和相机构造")
   (H2. "基逻辑")
   (H2. "模型和语义")
   (H2. "基逻辑的扩展")
   (H2. "语言")
   (H2. "程序逻辑")
   (H2. "导出构造")
   (H2. "逻辑悖论")
   (H2. "HeapLang")
   
   ))