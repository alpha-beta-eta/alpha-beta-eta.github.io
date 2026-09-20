#lang racket
(provide lattice_theory.html)
(require SMathML)
(define $∘ (Mo "∘"))
(define (^∘ x) (^ x $∘))
(define (_∘ x) (_ x $∘))
(define $h^∘ (^∘ $h))
(define $h_∘ (_∘ $h))
(define $R^-> (^ $R $->))
(define $R^<- (^ $R $<-))
(define $f^-> (^ $f $->))
(define $f^<- (^ $f $<-))
(define (Mpresup a b)
  (Mmultiscripts a (Mprescripts) $ b))
(define ->^R$ (Mpresup $R $->))
(define <-^R$ (Mpresup $R $<-))
(define $op (Mi "op"))
(define (&op X) (^ X $op))
(define (|[]| a b)
  (: $lb0 a $cm b $rb0))
(define (|[)| a b)
  (: $lb0 a $cm b $rp0))
(define $dummy (Mi "-"))
(define $Up (Mi "&uarr;"))
(define (&Up a) (ap $Up a))
(define $Down (Mi "&darr;"))
(define (&Down a) (ap $Down a))
(define $Im (Mi "Im"))
(define (&Im f) (ap $Im f))
(define $id (Mi "id"))
(define (&id A) (_ $id A))
(define $dashv (Mo "&dashv;"))
(define $rharu (Mo "&rharu;"))
(define (Upper P)
  (app $U:script P))
(define (Lower P)
  (app $D:script P))
(define $Max (Mi "Max"))
(define $Min (Mi "Min"))
(define $max (Mi "max"))
(define $min (Mi "min"))
(define (&Max S)
  (app $Max S))
(define (&Min S)
  (app $Min S))
(define (&max S)
  (app $max S))
(define (&min S)
  (app $min S))
(define (Open X)
  (app $O:script X))
(define (Closure X)
  (^ X $-))
(define (∀ x P)
  (: $forall x $. P))
(define (∃ x P)
  (: $exists x $. P))
(define Σ* (&* Σ))
(define $Sub (Mi "Sub"))
(define (&Sub G)
  (app $Sub G))
(define $lfloor (Mo "&lfloor;"))
(define $rfloor (Mo "&rfloor;"))
(define (&floor x)
  (: $lfloor x $rfloor))
(define $pr (Mo "&pr;"))
(define $pre (Mo "&pre;"))
(define $NN* (&* $NN))
(define $meet $conj)
(define $join $disj)
(define $o* (Mo "&otimes;" #:attr* '((displaystyle "false"))))
(define-infix*
  (&o* $o*)
  (&meet $meet)
  (&join $join)
  (&dashv $dashv)
  (&rharu $rharu)
  (&sqsube $sqsube)
  (&pr $pr)
  (&pre $pre))
(define (format-num section index)
  (cond ((eq? (car section) '*)
         (if index
             (format "~a" index)
             #f))
        (else
         (if index
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     index)
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     "*")))))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
(define (Entry name class)
  (define (present %entry attr* . html*)
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define head (format-head name section index))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define id (%entry-id %entry))
    (define href (string-append "#" id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define num (format-num section index))
    (if num
        (Cite `(a ((href ,href)) ,name ,num))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite
                          #:class class)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Theorem "定理" "theorem")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise")
  (Remark "评注" "remark")
  (Corollary "推论" "corollary"))
(define $Join $Disj)
(define $Meet $Conj)
(define-bigop*
  (Join $Join)
  (Meet $Meet))
(define-@lized-op*
  (@meet &meet)
  (@join &join))
(define lattice_theory.html
  (TnTmPrelude
   #:title "序与格论基础"
   #:css "styles.css"
   (H1. "序与格论基础")
   (P "小平邦彦将抄书学习法发扬光大, 我则是用电脑抄书, 以期学会数学.")
   (P "本书的优点是细致, 缺点或许也是细致. "
      "这导致我在阅读的时候不得不仔细纠结于字面, "
      "不能想当然, 必须参考定义, 以防止其和别的文献或者主流惯例有所不同. "
      "许多时候的确是失之毫厘, 谬以千里!")
   (H2. "偏序集与格")
   (H3. "偏序集")
   ((Definition)
    "这个定义非常主流, 就不抄写了, "
    "其定义了预序, 预序集, 偏序, 偏序集. "
    "和主流不同的是, 其要求预序集和偏序集非空, "
    "之后的论述的确也默认非空性, 必须小心.")
   ((Example)
    (Ol (Li "实数集" $RR ", 有理数集" $QQ ", 整数集" $ZZ
            ", 自然数集" $NN ", 非负整数集" $NN*
            "在通常的小于等于关系下构成偏序集, "
            "其实就是通常的序关系下的意思. "
            "这本书的一个背离当前主流记号的地方是" $NN
            "是不包含零的, 而" $NN* "是加了零的.")
        (Li "如果使用" $\| "表示整除, 即" (&\| $m $n)
            "表示" $m "整除" $n ", 那么" (tu0 $NN $\|)
            "是一个偏序集.")
        (Li "定义" $RR "上的一个二元关系" $pre "如下: "
            (MB (&<==> (&pre $x $y)
                       (&<= (&floor $x)
                            (&floor $y))))
            "则" (tu0 $RR $pre) "是一个预序集, "
            "但不是偏序集.")
        (Li "对于集合" $X ", 其幂集" (powset $X)
            "在关系" $sube "下是一个偏序集. "
            "当然了, 这个幂集的非空子集"
            "继承了自然的序关系也成为偏序集. "
            "原书要求" $X "非空是不必要的, "
            "因为" (powset $X) "即便在" $X
            "是空集的情况下也非空.")
        (Li "设" $G "是一个群, " (&Sub $G)
            "是由其所有子群构成的集合. 那么, "
            (tu0 (&Sub $G) $sube) "是一个偏序集.")
        (Li "设" Σ* "是由所有" (&cm $0 $1)
            "字符的有限序列构成的集合, 定义"
            (&<= $a $b) "当且仅当" $a
            "是" $b "的前缀, 则"
            (tu0 Σ* $<=) "是一个偏序集.")
        (Li "设" $P "是一个偏序集 (当然已经默认非空了), "
            $X "是一个集合, 定义" $P^X "上的关系" $<= "为"
            (MB (&<==> (&<= $f $g)
                       (∀ (∈ $x $X)
                          (&<= (app $f $x)
                               (app $g $x)))))
            "那么" $<= "是一个偏序关系, 其被称为"
            $P^X "上的逐点序 (pointwise order). "
            "原文需要" $X "非空, 实则不需要.")
        (Li "设"
            (&= (app $I $RR)
                (setI (|[]| $a $b)
                      (&cm (&<= $a $b)
                           (∈ $a $b $RR))))
            ", 定义"
            (MB (&<==> (&sqsube (|[]| $a_1 $b_1)
                                (|[]| $a_2 $b_2))
                       (&sube (|[]| $a_2 $b_2)
                              (|[]| $a_1 $b_1))))
            "则" (tu0 (app $I $RR) $sqsube)
            "是一个偏序集, 而" $sqsube "是" $RR
            "上的区间序或者信息序. "
            "信息序这个名字应该来源于指称语义学, "
            "区间越小越精细, 信息越多.")
        (Li "设" (tu0 $X (Open $X))
            "是一个拓扑空间, 定义"
            (MB (&= (_ $<= (Open $X))
                    (setI (∈ (tu0 $x $y) (&c* $X $X))
                          (∈ $x (Closure (setE $y))))))
            "那么" (_ $<= (Open $X)) "是一个预序, 称为"
            $X "的特殊化序 (specialization order). 记"
            (&= (appl $Theta $X (Open $X))
                (tu0 $X (_ $<= (Open $X))))
            ". 容易证明, " (_ $<= (Open $X))
            "是一个偏序当且仅当" $X "是" $T_0
            "空间. 之所以称为特殊化, "
            "或许是特殊化序还要另外一种刻画方式, 即"
            (&<= $x $y) "当且仅当包含" $x
            "的开集族是包含" $y "的开集族的子集. "
            "一个点所处于的开集越多, 也就越特殊.")))
   (P "设" (tu0 $P $<=) "是一个偏序集, " $Q
      "是" $P "的一个非空子集. 记"
      (MB (&= (_ $<= $Q)
              (setI (∈ (tu0 $x $y) (&c* $Q $Q))
                    (&<= $x $y))))
      "则" (_ $<= $Q) "是" $Q "上的一个偏序, 称为"
      $Q "在" $P "中的继承序或者导出序或者诱导序或者限制序, 而"
      (tu0 $Q (_ $<= $Q)) "是" (tu0 $P $<=)
      "的一个子偏序集. 对于" (∈ $n $NN) ", 记"
      (&= $n:bold (setE $0 $1 $2 $..h (&- $n $1)))
      "为" $NN* "的子偏序集.")
   ((Definition)
    "设" $P "是一个偏序集, " (&sube $A $P) "."
    (Ol (Li "如果对于任意的" (∈ $x $A) "和"
            (∈ $y $P) ", " (&<= $x $y)
            "可以推出" (∈ $y $A) ", 那么称"
            $A "为上集 (upper set).")
        (Li "如果对于任意的" (∈ $x $A) "和"
            (∈ $y $P) ", " (&<= $y $x)
            "可以推出" (∈ $y $A) ", 那么称"
            $A "为下集 (lower set)."))
    "分别记" $P "的所有下集和上集构成的集合为"
    (Lower $P) "和" (Upper $P)
    ", 它们都是" $P "上的Alexandrov拓扑.")
   (P "设" $P "是一个偏序集, " (&sube $A $P) ", 令"
      
      )
   ((Definition)
    "设" $P "是一个偏序集, 对于" (∈ $x $y $P)
    ", 如果" (&< $x $y) ", 且对于每个"
    (∈ $z $P) ", " (&<= $x $z $y)
    "可以推出" (&= $z $x) "或" (&= $z $y)
    ", 那么称" $y "覆盖 (cover) " $x
    ", 记作" (&pr $x $y) ".")
   (P "一般来说, 偏序集可以用Hasse图来描述. "
      "Hasse图存在几个方面, 或者说几种不同的含义. "
      "一种可能是Hasse图是纯粹技术意义上的graph, "
      "还有一种可能是Hasse图是一种图示. "
      "不过, 我感觉还是图示意义下的Hasse图更频繁出现. "
      "我们将具有覆盖关系的元素用线段连接, "
      "一般较小元素安排在下方, 较大元素安排在上方, "
      "无法比较的平行元素则一般尽可能安排在同一高度.")
   (P "设" $P "是一个偏序集, " (∈ $a $P)
      ". 如果对于每个" (∈ $x $P) "有"
      (&<= $a $x) ", 那么称" $a "是" $P
      "的最小元或者底 (bottom), 记作" $0_P
      "或者" $0 ". 类似地可以定义" $P
      "的最大元并记作" $1_P "或者" $1
      ". 具有最小元和最大元的偏序集称为有界的 (bounded). "
      "如果对于每个" (∈ $x $P) "都有"
      (&<= $x $a) "可以推出" (&= $x $a)
      ", 那么就称" $a "为" $P
      "的极小元. 换言之, 没有其他元素比" $a
      "更小了. 类似地可以定义极大元. "
      "显然, 最小元是一个极小元, 最大元是一个极大元. "
      "设" $S "是" $P "的一个子偏序集, "
      $S "的极小元集合记作" (&min $S)
      ", 而极大元集合记作" (&max $S)
      ". 它们当然都有可能是空集. 另外, 最小元记作"
      (&Min $S) ", 最大元记作" (&Max $S)
      ". 最小元和最大元并不一定存在, "
      "但是若存在则显然唯一 (对于偏序集而言).")
   ((Example)
    )
   ((Definition)
    "设" (_ (setE (tu0 $P_i (_ $<= $i))) (∈ $i $I))
    "是一族偏序集, 在笛卡尔积" (prod $i $P_i)
    "上定义关系" $<= "如下:"
    (MB (&<==> (&<= (_ (@ $x_i) (∈ $i $I))
                    (_ (@ $y_i) (∈ $i $I)))
               (∀ (∈ $i $I)
                  (: $x_i (_ $<= $i) $y_i))))
    "易证" $<= "是" (prod $i $P_i) "上的一个偏序, "
    "我们称其为逐点序, 而" (tu0 (prod $i $P_i) $<=)
    "称为" (_ (setE (tu0 $P_i (_ $<= $i))) (∈ $i $I))
    "的直积.")
   ((Example)
    )
   ((Theorem)
    )
   (H3. "格与完备格")
   ((Definition)
    "定义了上确界 (join) 和下确界 (meet), 不再赘述.")
   ((Remark)
    )
   ((Definition)
    "设" $L "是一个偏序集."
    (Ol (Li "如果对于任意的" (∈ $x $y $L)
            "都有" (Join (setE $x $y))
            "存在, 则称" $L "为并半格.")
        (Li "如果对于任意的" (∈ $x $y $L)
            "都有" (Meet (setE $x $y))
            "存在, 则称" $L "为交半格.")
        (Li "如果" $L "既是并半格又是交半格, 则称"
            $L "为格.")))
   ((Theorem)
    "设" $L "是一个格, 则对于任意的"
    (∈ $a $b $c $L) ", 运算"
    $join "和" $meet "满足"
    (Ol (Li "幂等")
        (Li "交换")
        (Li "结合")
        (Li "吸收: "
            (&= (&join $a (@meet $a $b)) $a) ", "
            (&= (&meet $a (@join $a $b)) $a) ".")))
   ((Example)
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   ((Definition)
    "设" $L "是一个格, " $S "是" $L "的一个非空子集."
    (Ol (Li "如果对于任意的" (∈ $x $y $S) "都有"
            (∈ (&meet $x $y) (&join $x $y) $S)
            ", 称" $S "为" $L "的子格.")
        (Li "如果" $L "是有界格, " $S "是" $L
            "的子格且" (∈ $0 $1 $S) ", 则称"
            $S "为" $L "的保界子格.")))
   ((Example)
    
    )
   ((Definition)
    "设" $L "是一个偏序集, 如果" $L
    "的每个子集 (包括空集) 都有上确界和下确界, 则称"
    $L "是一个完备格.")
   
   ((Theorem)
    "设" $L "是一个偏序集, 则下列条件等价:"
    (Ol (Li $L "是一个完备格;")
        (Li (&op $L) "是一个完备格;")
        (Li $L "的每个子集都有上确界;")
        (Li $L "有最小元" $0
            "且每个非空子集都有上确界;")
        (Li $L "的每个子集都有下确界;")
        (Li $L "有最大元" $1
            "且每个非空子集都有下确界.")))
   ((proof)
    
    )
   ((Corollary)
    
    )
   ((Definition)
    "设" (&cm $P $Q) "是偏序集, "
    (func $f $P $Q) "是一个映射."
    (Ol (Li "如果对于任意的" (∈ $x $y $P)
            "都有" (&<= $x $y) "可以推出"
            (&<= (app $f $x) (app $f $y))
            ", 就称" $f "为保序的, 或"
            $f "是从" $P "到" $Q "的序同态.")
        (Li "如果对于任意的" (∈ $x $y $P)
            "都有" (&<= $x $y) "可以推出"
            (&<= (app $f $y) (app $f $x))
            ", 则称" $f "为逆序的.")))
   ((Theorem)
    (B "Knaster-Tarski不动点定理. ")
    "设" $L "是一个完备格, 如果"
    (func $f $L $L) "是一个保序映射, 则"
    $f "有最大和最小不动点.")
   ((proof)
    "太过经典, 不写了.")
   ((Definition)
    "设" $P "是一个偏序集, " (func $f $P $P)
    "是一个自映射. 如果"
    (Ol (Li "保序性: " (&<= $x $y)
            "可以推出" (&<= (app $f $x) (app $f $y)) ";")
        (Li "增值性: " (&<= $x (app $f $x)) ";")
        (Li "幂等性: " (&= (app $f (app $f $x)) (app $f $x))))
    "则称" $f "是" $P "上的一个闭包算子 (closure operator).")
   ((Definition)
    "设" $P "是一个偏序集, " (func $g $P $P)
    "是一个自映射. 如果"
    (Ol (Li "保序性: " (&<= $x $y)
            "可以推出" (&<= (app $g $x) (app $g $y)) ";")
        (Li "减值性: " (&<= (app $g $x) $x) ";")
        (Li "幂等性: " (&= (app $g (app $g $x)) (app $g $x))))
    "则称" $g "是" $P "上的一个内部算子 (interior operator).")
   (P "设" (func $h $P $P) "是偏序集" $P
      "上的一个自映射. 记"
      (&= (&Im $h) (setI (app $h $x) (∈ $x $P)))
      ". 当" $h "是幂等的, 那么" (&Im $h)
      "恰是" $h "的所有不动点之集合. "
      "{译注: 实际上, 幂等性与" (&Im $h)
      "是所有不动点之集合等价.} "
      "幂等保序自映射也称为投影.")
   ((Theorem)
    "设" $L "是一个完备格, " (func $f $L $L)
    "是一个闭包算子, 则" (&Im $f)
    "也是一个完备格, "
    )
   ((Corollary)
    
    )
   ((Definition)
    
    )
   
   (H3. "序同构与格同构")
   ((Definition)
    
    )
   (H3. "分配格与Boole代数")
   ((Theorem)
    "设" $L "是一个格, 则下面两个有限分配律是等价的:"
    (Ol (Li (distributeR $x &meet $y &join $z) ";")
        (Li (distributeR $x &join $y &meet $z) ".")))
   ((proof)
    "1推出2: "
    )
   ((Definition)
    "设" $L "是一个格, 如果" $L
    "满足上述定理中描述的两个条件之一, 那么称"
    $L "为分配格.")
   ((Example)
    (Ol (Li "每个链都是分配格.")
        (Li (tu0 $NN $\|) "是分配格.")
        (Li "设" (tu0 $X (Open $X)) "是拓扑空间, 则"
            (tu0 (Open $X) $sube) "是分配格.")))
   ((proof)
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    "设" $L "是一个格, 则" $L "是分配格当且仅当"
    (MB (&= (&meet $z $x) (&meet $z $y))
        "且"
        (&= (&join $z $x) (&join $z $y))
        "可以推出"
        (&= $x $y)))
   ((proof)
    
    )
   (H3. "习题1" #:auto? #f)
   ((Exercise)
    "找出所有" $4 "元偏序集和" $5 "元格.")
   ((Exercise)
    
    )
   ((Exercise)
    "设" $P "是一个偏序集, " (func (&cm $f $g) $P $P)
    "是闭包算子, 请证明下列条件等价:"
    (Ol (Li (&<= $f $g) ";")
        (Li (&= (&compose $f $g) $g) ";")
        (Li (&= (&compose $g $f) $g) ";")
        (Li (&sube (&Im $g) (&Im $f)) ".")))
   ((proof)
    "由1推出2: 对于每个" (∈ $x $P) ", 我们知道"
    (&>= (app (@compose $f $g) $x) (app $g $x))
    ", 这是增值性. 另外, 由于" (&<= $f $g)
    ", 所以"
    (&<= (app $f (app $g $x))
         (app $g (app $g $x)))
    ". 根据幂等性, 可以推出"
    (&<= (app (@compose $f $g) $x) (app $g $x))
    ". 因此, "
    (&= (app (@compose $f $g) $x) (app $g $x))
    ", 即" (&= (&compose $f $g) $g) "." (Br)
    "由2推出3: 对于每个" (∈ $x $P)
    ", 根据增值性和单调性, 我们有"
    (&>= (app $g (app $f $x)) (app $g $x))
    ". 然后, 我们又知道"
    (&= $g (&compose $g $g)
        (&compose $g $f $g))
    ". " (&compose $g $f)
    "当然是单调的, 所以说"
    (&<= (app $g (app $f $x))
         (app $g (app $f (app $g $x))))
    ", 于是"
    (&<= (app $g (app $f $x)) (app $g $x))
    ". 因此, " (&= (&compose $g $f) $g) "." (Br)
    "由3推出4: 对于" (∈ $x (&Im $g))
    ", 我们知道存在" (∈ $y $P) "使得"
    (&= $x (app $g $y))
    ". 现在我们希望说明存在" (∈ $z $P)
    "使得" (&= (app $f $z) (app $g $y))
    ". 然而, 我们将要证明"
    (&= (app $f (app $g $y)) (app $g $y))
    ". 首先根据增值性, "
    (&>= (app $f (app $g $y)) (app $g $y))
    ". 于是, 我们还需要证明"
    (&<= (app $f (app $g $y)) (app $g $y))
    ". 不过, 我们有"
    (&= (app $g $y)
        (app $g (app $g $y))
        (app $g (app $f (app $g $y))))
    ". 根据增值性这也是显然的." (Br)
    "由4推出1: 对于每个" (∈ $x $P)
    ", 我们要证明"
    (&<= (app $f $x) (app $g $x))
    ". 因为" (&sube (&Im $g) (&Im $f))
    ", 所以既然" (∈ (app $g $x) (&Im $g))
    ", 那么存在" (∈ $y $P) "使得"
    (&= (app $g $x) (app $f $y))
    ". 接着, 我们可以推出"
    (MB (deriv
         (app $g $x)
         (app $f $y)
         (app $f (app $f $y))
         (app $f (app $g $x))))
    "因为" (&<= $x (app $g $x))
    ", 所以" (&<= (app $f $x) (app $f (app $g $x)))
    ", 即" (&<= (app $f $x) (app $g $x)) ".")
   ((Exercise)
    
    )
   ((Exercise)
    
    )
   ((Exercise)
    "设" $L "是一个格, "
    (func $d $L (|[)| $0 (&+ $inf)))
    "是一个映射满足"
    (MB (&= (&+ (app $d $x) (app $d $y))
            (&+ (app $d (&join $x $y))
                (app $d (&meet $x $y)))))
    "且" (&< $x $y) "可以推出"
    (&< (app $d $x) (app $d $y))
    ", 我们称" (tu0 $L $d)
    "是一个度量格. 定义"
    (func $rho (&c* $L $L) (|[)| $0 (&+ $inf)))
    "为"
    (MB (&= (appl $rho $x $y)
            (&- (app $d (&join $x $y))
                (app $d (&meet $x $y)))))
    "证明" $rho "是" $L "上的一个度量, "
    "并描述其所诱导的拓扑的开集和闭集.")
   ((Exercise)
    
    )
   ((Exercise)
    
    )
   ((Exercise)
    
    )
   ((Exercise)
    
    )
   ((Exercise)
    
    )
   (H2. "Galois伴随和Galois连接")
   (P "本章标题中的两个名字在许多文献中存在着大量混用的现象, "
      "现在我们从历史发展的角度阐述它们的异同点. "
      "特别指出, Galois伴随的英文是Galois correspondence或"
      "Galois adjunction, 而Galois连接 (也称Galois联络) "
      "英文则是Galois connection, "
      "两者都是两个偏序集之间满足一定条件的映射序对.")
   
   (H3. "Galois伴随")
   (P "虽然Galois连接出现得比Galois伴随更早, "
      "然而从格论的角度来看, Galois伴随给人以更加自然和实用之感. "
      "因此, 我们先在本节介绍Galois伴随及其相关结果.")
   ((Definition)
    "如图2.1所示, 设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的保序映射, 如果对于任意的"
    (∈ $a $P) "和" (∈ $b $Q) "都有"
    (MB (&<==> (&<= (app $f $a) $b)
               (&<= $a (app $g $b))))
    "则称" (tu0 $f $g) "是从" $P "到" $Q
    "的一个Galois伴随, 记作"
    (MB (&: (&dashv $f $g) (&rharu $P $Q)))
    "如果" (&= $P $Q) ", 则称" (tu0 $f $g)
    "是" $P "上的一个Galois伴随.")
   (P "这里的" $f "和" $g "有位置之分: " $f
      "位于不等号的左侧, 而" $g "位于右侧. "
      "一般情况下, 称" $f "是" $g "的左伴随, "
      $g "是" $f "的右伴随. 有些文献也将"
      (&cm $f $g) "分别称为上伴随和下伴随.")
   ((Example)
    (Ol (Li "设" (&sube $R (&c* $X $Y))
            "是一个二元关系, 分别定义"
            (func $R^-> (powset $X) (powset $Y)) "和"
            (func $R^<- (powset $Y) (powset $X)) "为"
            (MB (&= (app $R^-> $A)
                    (setI (∈ $y $Y)
                          (∃ (∈ $x $A)
                             (∈ (tu0 $x $y) $R)))))
            (MB (&= (app $R^<- $B)
                    (setI (∈ $x $X)
                          (&=> (∈ (tu0 $x $y) $R)
                               (∈ $y $B)))))
            "则"
            (&: (&dashv $R^-> $R^<-)
                (&rharu (powset $X) (powset $Y))) ".")
        (Li "设" (&sube $R (&c* $X $Y))
            "是一个二元关系, 分别定义"
            (func ->^R$ (powset $X) (powset $Y)) "和"
            (func <-^R$ (powset $Y) (powset $X)) "为"
            (MB (&= (app ->^R$ $A)
                    (setI (∈ $y $Y)
                          (&=> (∈ (tu0 $x $y) $R)
                               (∈ $x $A)))))
            (MB (&= (app <-^R$ $B)
                    (setI (∈ $x $X)
                          (∃ (∈ $y $B)
                             (∈ (tu0 $x $y) $R)))))
            "则"
            (&: (&dashv <-^R$ ->^R$)
                (&rharu (powset $Y) (powset $X))) ".")
        (Li "设" (func $f $X $Y) "是一个映射. 将"
            $f "视为二元关系"
            (setI (tu0 $x (app $f $x)) (∈ $x $X))
            ", 由1知"
            (&: (&dashv $f^-> $f^<-)
                (&rharu (powset $X) (powset $Y)))
            "其中"
            (MB (&= (app $f^-> $A)
                    (setI (app $f $x) (∈ $x $A))))
            (MB (&= (app $f^<- $B)
                    (setI (∈ $x $X)
                          (∈ (app $f $x) $B)))))))
   (P "Galois伴随除了用当且仅当的方式定义之外, "
      "还可以利用映射的复合与偏序关系进行刻画.")
   ((Theorem)
    "设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的保序映射, 则下列条件等价:"
    (Ol (Li (&: (&dashv $f $g) (&rharu $P $Q)) ";")
        (Li (&cm (&<= (&id $P) (&i* $g $f))
                 (&<= (&i* $f $g) (&id $Q))) ".")))
   ((proof)
    "1推出2: 对于任意的" (∈ $x $P)
    ", 由" (&<= (app $f $x) (app $f $x))
    "可以推出" (&<= $x (app (@i* $g $f) $x))
    ", 即" (&<= (&id $P) (&i* $g $f))
    ". 对于任意的" (∈ $y $Q)
    ", 由" (&<= (app $g $y) (app $g $y))
    "可以推出" (&<= (app (@i* $f $g) $y) $y)
    ", 即" (&<= (&i* $f $g) (&id $Q)) "." (Br)
    "2推出1: 对于任意的" (∈ $x $P) "和"
    (∈ $y $Q) ", 如果" (&<= (app $f $x) $y)
    ", 根据单调性可知"
    (&<= (app (@i* $g $f) $x) (app $g $y))
    ", 又因为"
    (&<= (app (&id $P) $x)
         (app (@i* $g $f) $x))
    ", 于是"
    (&<= (app (&id $P) $x) (app $g $y))
    ", 即" (&<= $x (app $g $y))
    ". 如果" (&<= $x (app $g $y))
    ", 那么根据单调性可知"
    (&<= (app $f $x) (app (@i* $f $g) $y))
    ", 又因为"
    (&<= (app (@i* $f $g) $y) (app (&id $Q) $y))
    ", 于是"
    (&<= (app $f $x) (app (&id $Q) $y))
    ", 即" (&<= (app $f $x) $y) ".")
   (P "由以上定理易得如下结论:")
   ((Theorem)
    "设" (&: (&dashv $f $g) (&rharu $P $Q)) ", 则"
    (Ol (Li (&= (&i* $f $g $f) $f) ", "
            (&= (&i* $g $f $g) $g) ";")
        (Li (func (&i* $g $f) $P $P)
            "是闭包算子, "
            (func (&i* $f $g) $Q $Q)
            "是内部算子.")))
   ((proof)
    (Ol (Li "我们知道" (&<= (&id $P) (&i* $g $f))
            ", 于是对于每个" (∈ $x $P) "都有"
            (&<= $x (app (@i* $g $f) $x))
            ". 根据单调性, 可知"
            (&<= (app $f $x)
                 (app (@i* $f $g $f) $x))
            ". 另外, 根据"
            (&<= (&i* $f $g) (&id $Q))
            ", 我们知道对于每个" (∈ $x $P) "都有"
            (&<= (app (@i* $f $g) (app $f $x))
                 (app (&id $Q) (app $f $x)))
            ", 即"
            (&<= (app (@i* $f $g $f) $x)
                 (app $f $x))
            ". 因此, " (&= (&i* $f $g $f) $f)
            ". 我们知道" (&<= (&i* $f $g) (&id $Q))
            ", 于是对于每个" (∈ $y $Q) "都有"
            (&<= (app (@i* $f $g) $y) $y)
            ". 根据单调性, 可知"
            (&<= (app (@i* $g $f $g) $y)
                 (app $g $y))
            ". 另外, 根据"
            (&<= (&id $P) (&i* $g $f))
            ", 我们知道对于每个" (∈ $y $Q) "都有"
            (&<= (app (&id $P) (app $g $y))
                 (app (@i* $g $f) (app $g $y)))
            ", 即"
            (&<= (app $g $y)
                 (app (@i* $g $f $g) $y))
            ". 因此, " (&= (&i* $g $f $g) $g) ".")
        (Li (&i* $g $f) "的保序性和增值性都是显然的. "
            "为了说明幂等性, 我们需要证明对于每个"
            (∈ $x $P) "都有"
            (MB (&= (app (@i* $g $f $g $f) $x)
                    (app (@i* $g $f) $x)))
            "由1的结论这也是显然的. " (&i* $f $g)
            "的保序性和减值性也是显然的, "
            "而幂等性则需要证明"
            (MB (&= (app (@i* $f $g $f $g) $x)
                    (app (@i* $f $g) $x)))
            "由1的结论这也是显然的.")))
   (P "虽然Galois伴随的两个偏序集不一定序同构, "
      "但是我们可以通过缩小定义域和陪域的方式"
      "得到一组序同构的偏序集.")
   ((Theorem)
    "如果" (&: (&dashv $f $g) (&rharu $P $Q))
    ", 则" (&cong (app $f $P) (app $g $Q)) ".")
   ((proof)
    "将" $f "限制为"
    (func $phi (app $g $Q) (app $f $P))
    ", 将" $g "限制为"
    (func $psi (app $f $P) (app $g $Q))
    ". 对于每个" (∈ $x (app $g $Q))
    ", 存在" (∈ $y $Q) "满足"
    (&= $x (app $g $y)) ", 于是"
    (MB (deriv (app (@i* $psi $phi) $x)
               (app (@i* $g $f) $x)
               (app (@i* $g $f) (app $g $y))
               (app (@i* $g $f $g) $y)
               (app $g $y)
               $x))
    "同理, 我们可以推出"
    (MB (&= (app (@i* $phi $psi) $y) $y))
    "对于每个" (∈ $y (app $f $P))
    "成立. 也就是说, "
    $phi "和" $psi
    "是可逆的保序映射, "
    "也就是序同构.")
   (P "由定义2.1.1和定理2.1.1可以看出, "
      "Galois伴随的两个映射之间具有良好的协调关系, "
      "以至于它们可以相互唯一确定.")
   ((Theorem)
    "设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的保序映射, 那么下列条件等价:"
    (Ol (Li (&: (&dashv $f $g) (&rharu $P $Q)) ";")
        (Li (∀ (∈ $a $P)
               (&= (app $f $a) (&Min (app (inv $g) (&Up $a))))) ";")
        (Li (∀ (∈ $b $Q)
               (&= (app $g $b) (&Max (app (inv $f) (&Down $b))))) ".")))
   ((proof)
    "1推出2: 对于任意的" (∈ $a $P) ", 由于"
    (&<= $a (app $g (app $f $a))) ", 我们有"
    (∈ (app $f $a) (app (inv $g) (&Up $a)))
    "; 任取" (∈ $y (app (inv $g) (&Up $a)))
    ", 我们有" (&<= $a (app $g $y))
    ", 从而" (&<= (app $f $a) $y)
    ". 这说明" (&= (app $f $a) (&Min (app (inv $g) (&Up $a))))
    "." (Br)
    "2推出1: 对于任意的" (∈ $a $P) "和" (∈ $b $Q)
    ", 如果" (&<= (app $f $a) $b) ", 则由"
    (&= (app $f $a) (&Min (app (inv $g) (&Up $a))))
    "可知" (∈ (app $f $a) (app (inv $g) (&Up $a)))
    ", 于是" (&<= $a (app $g (app $f $a)))
    ", 再根据" $g "的单调性就有"
    (&<= $a (app $g $b)) "; 反过来, 如果"
    (&<= $a (app $g $b)) ", 那么"
    (∈ (app $g $b) (&Up $a))
    ", " (∈ $b (app (inv $g) (&Up $a)))
    ", 于是" (&<= (app $f $a) $b)
    ". 因此, " (&: (&dashv $f $g) (&rharu $P $Q))
    "." (Br)
    "通过类似的方法我们也可以证明1等价于3.")
   ((Definition)
    "设" (func $h $P $Q) "是偏序集之间的一个映射."
    (Ol (Li "如果主理想的原像是主理想, 则称"
            $h "为剩余映射 (residuated mapping).")
        (Li "如果主滤子的原像是主滤子, 则称"
            $h "为残余映射 (residual mapping).")))
   ((Theorem)
    "设" (func $h $P $Q) "是偏序集之间的一个保序映射, 那么"
    (Ol (Li $h "有右伴随当且仅当" $h "是一个剩余映射;")
        (Li $h "有左伴随当且仅当" $h "是一个残余映射.")))
   ((proof)
    "2是1的对偶结论, 故我们只证明1." (Br)
    "左推右: 设" $h "有右伴随, 根据前述定理, 对于任意的"
    (∈ $b $Q) ", " (app (inv $h) (&Down $b))
    "都有最大元; 又因为" $h "是保序的, 易知"
    (app (inv $h) (&Down $b))
    "是一个下集, 从而是一个主理想." (Br)
    "右推左: 对于任意的" (∈ $b $Q) ", 由于"
    (app (inv $h) (&Down $b))
    "是一个主理想, "
    (&= (app $g $b) (&Max (app (inv $h) (&Down $b))))
    "是良定的, 由此得到映射" (func $g $Q $P)
    ". " $g "按照定义自动是单调的. "
    "{译注: 原文说根据" $h
    "的保序性, 这是不对的.} "
    "由前述定理可知, "
    $g "是" $h "的右伴随.")
   (P "集合论中我们熟知对于映射" (func $f $X $Y)
      ", 对应的" (func $f (powset $X) (powset $Y))
      "保持并, " (func (inv $f) (powset $Y) (powset $X))
      "既保持并又保持交. 实际上, "
      "映射序对的保并性和保交性是对于Galois伴随的一种刻画.")
   ((Theorem)
    "在Galois伴随中, 左伴随保持任意存在的并, 右伴随保持任意存在的交.")
   ((proof)
    "设" (tu0 $f $g) "是从偏序集" $P "到偏序集" $Q
    "的一个Galois伴随. 设" (&sube $A $P) "且"
    (Join $A) "存在. 如果" (&= $A $empty)
    ", 那么" $P "有最小元素" $0_P
    )
   ((Theorem)
    "设" (&cm $P $Q) "是两个完备格."
    (Ol (Li "保序映射" (func $f $P $Q)
            "保持任意的并当且仅当" $f
            "有右伴随" (func $g $Q $P) ", 且"
            
            )
        )
    )
   ((proof)
    
    )
   ((Theorem)
    "设" (&: (&dashv $f $g) (&rharu $P $Q))
    ", 则下列条件等价:"
    (Ol (Li $f "是单射;")
        (Li (&= (&i* $g $f) (&id $P)) ";")
        (Li $g "是满射.")))
   ((proof)
    "1推出2: 由定理2.1.2, 我们知道"
    (&= (&i* $f $g $f) $f)
    ". 换言之, 对于每个" (∈ $x $P)
    ", 我们都有"
    (&= (app $f (app (@i* $g $f) $x))
        (app $f $x))
    ". 因为" $f "是单射, 所以"
    (&= (app (@i* $g $f) $x) $x)
    ", 也就是" (&= (&i* $g $f) (&id $P))
    "." (Br)
    "2推出3: 我们知道"
    (&sube (&Im (@i* $g $f)) (&Im $g))
    ", 又知道"
    (&= (&Im (@i* $g $f))
        (&Im (&id $P)) $P)
    ", 所以" $g "是满射." (Br)
    "3推出2: 对于每个" (∈ $x $P)
    ", 鉴于" $g "是满射, 所以存在"
    (∈ $y $Q) "使得"
    (&= (app $g $y) $x)
    ". 于是, 我们有"
    (MB (&= (app (@i* $g $f) $x)
            (app (@i* $g $f) (app $g $y))
            (app (@i* $g $f $g) $y)
            (app $g $y)
            $x))
    "换言之, " (&= (&i* $g $f) (&id $P))
    "." (Br)
    "2推出1: 对于" (∈ $x $y $P)
    ", 如果" (&= (app $f $x) (app $f $y))
    ", 那么"
    (MB (deriv
         (app $g (app $f $x))
         (app (@i* $g $f) $x)
         (app (&id $P) $x)
         $x
         (app $g (app $f $y))
         (app (@i* $g $f) $y)
         (app (&id $P) $y)
         $y))
    "即" (&= $x $y) ", 换言之则是"
    $f "为单射.")
   ((Theorem)
    "设" (&: (&dashv $f $g) (&rharu $P $Q))
    ", 则下列条件等价:"
    (Ol (Li $f "是满射;")
        (Li (&= (&i* $f $g) (&id $Q)) ";")
        (Li $g "是单射.")))
   ((proof)
    
    )
   ((Corollary)
    
    )
   (H3. "内部算子和闭包算子与Galois伴随的关系")
   (P "定理2.1.2指出从Galois伴随出发可以得到一个闭包算子和一个内部算子. "
      "实际上, 每个闭包算子和内部算子也都可以分解为Galois伴随.")
   
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   (H3. "Galois连接")
   (P "Galois连接是一种逆序的映射对, "
      "故常称为逆序Galois伴随或者对偶Galois伴随.")
   ((Definition)
    
    )
   (H3. "形式概念分析的格论基础")
   (H3. "偏序集的Dedekin-MacNeille完备化")
   (H3. "习题2" #:auto? #f)
   ((Exercise)
    
    )
   (H2. "Heyting代数")
   (P "Heyting代数由荷兰数学家A. Heyting于1930年引入. "
      "由于其逻辑排中律一般不再成立, "
      "所以Heyting代数可以看作直觉主义逻辑演算的"
      "Tarski-Lindenbaum代数. "
      "在数学方面, Heyting代数是Boole代数的一般化, "
      "曾被称为伪Boole代数或Brouwer格.")
   
   (H3. "Heyting代数的基本概念")
   ((Definition)
    "设" $H "是一个格, 若存在二元运算"
    (func $-> (&c* $H $H) $H) "使得"
    (MB (∀ (∈ $a $b $c $H)
           (&<==> (&<= (&meet $c $a) $b)
                  (&<= $c (&-> $a $b)))))
    "则称" $H "为Heyting代数.")
   (P "容易验证, " $H "有最大元但不一定有最小元. "
      "有些文献会假定Heyting代数是一个有界格.")
   (P "由Heyting代数的定义和第2章中Galois伴随之性质, 我们有以下定理.")
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
   ((proof)
    "1推出2: 根据Galois伴随的定义, "
    "我们需要证明对于任意的"
    (∈ $x $y $H) ", 都有"
    (MB (&<==> (&<= (&meet $a $x) $y)
               (&<= $x (&-> $a $y))))
    "而这由Heyting代数的定义所保证. "
    "我们还需要说明这两个函数的单调性. 如果"
    (&<= $x $y) ", 那么"
    (&<= (&meet $a $x) (&meet $a $y))
    ", 这可由定义得到. 如果"
    (&<= $x $y) ", 我们要证明"
    (&<= (&-> $a $x) (&-> $a $y))
    ". 根据" (&<= (&-> $a $x) (&-> $a $x))
    ", 根据剩余律我们有"
    (&<= (&meet (@-> $a $x) $a) $x)
    ", 鉴于" (&<= $x $y)
    ", 所以有"
    (&<= (&meet (@-> $a $x) $a) $y)
    ", 再用一次剩余律可知"
    (&<= (&-> $a $x) (&-> $a $y))
    "." (Br)
    "2推出1: 根据1推出2的证明, 这就是立即可以得到的." (Br)
    "1推出3: 我们欲说明" (&-> $a $b) "是"
    (setI (∈ $x $H) (&<= (&meet $a $x) $b))
    "的最大元. 首先, 根据1推出2的证明过程, 我们知道"
    (&<= (&meet $a (@-> $a $b)) $b)
    "这一事实, 于是"
    (∈ (@-> $a $b)
       (setI (∈ $x $H) (&<= (&meet $a $x) $b)))
    ". 其次, 如果" (∈ $c $H) "满足"
    (&<= (&meet $a $c) $b)
    ", 那么根据剩余律" (&<= $c (&-> $a $b))
    ". 由此, 我们知道" (&-> $b $c)
    "就是最大元." (Br)
    "3推出1: 我们定义"
    (&:= (@-> $a $b)
         (&Max (setI (∈ $x $H)
                     (&<= (&meet $a $x) $b))))
    ", 当然这是良定的. "
    "接着, 我们需要证明剩余律. "
    "对于任意的" (∈ $a $b $c $H)
    ", 如果" (&<= (&meet $c $a) $b) ", 即"
    (∈ $c (setI (∈ $x $H)
                (&<= (&meet $a $x) $b)))
    ", " (&<= $c (&-> $a $b))
    "根据定义即得. 如果"
    (&<= $c (&-> $a $b))
    ", 那么"
    (&<= (&meet $c $a)
         (&meet (@-> $a $b) $a)
         $b)
    ", 这就是根据" (&-> $a $b)
    "的定义得到的.")
   ((Example)
    
    )
   ((Theorem)
    "每个Heyting代数都是分配格.")
   ((proof)
    
    )
   ((Theorem)
    "每个有限分配格都是Heyting代数.")
   ((Theorem)
    "每个Boole代数都是Heyting代数.")
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
        (Li ""
            )
        )
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   (H3. "滤子和同余关系之间的一一对应")
   (H2. "Frame和拓扑表示定理")
   
   (H3. "Frame的定义和基本性质")
   
   (H3. "空间式frame和sober空间")
   (H2. "Domain与连续格")
   (H2. "完全分配格")
   (H2. "剩余格")
   (H3. "剩余格的基本概念")
   ((Definition)
    "设" $L "是一个有界格, " $0 "和" $1
    "分别是其最小元和最大元, "
    (func (&cm $o* $->) (&c* $L $L) $L)
    "是两个其上的二元运算. 如果"
    (Ol (Li (tu0 $L $o* $1) "是交换幺半群;")
        (Li "对于任意的" (∈ $a $b $c $L) "都有"
            (MB (&<==> (&<= (&o* $a $b) $c)
                       (&<= $a (&-> $b $c))))))
    "则称" (tu0 $L $o* $->) "为剩余格. 当"
    $L "还是完备格时, 则称" $L "为完备剩余格.")
   ((Theorem)
    "设" $L "是一个剩余格, 则对于任意的"
    (∈ $a $b $c $L) ", 我们有"
    (Ol (Li (&<= (&o* $a $b) (&meet $a $b)) ";")
        (Li (&cm (&= (&o* $a $0) $0)
                 (&= (&-> $1 $a) $a)) ";")
        (Li (&<= $b (&-> $a $b)) ";")
        (Li (&<= $a $b) "当且仅当"
            (&= (&-> $a $b) $1) ";")
        
        )
    )
   ))