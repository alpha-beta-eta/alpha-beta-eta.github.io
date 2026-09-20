#lang racket
(provide lattice_theory.html)
(require SMathML)
(define $Frm
  (Mi "Frm" #:attr* '((mathvariant "bold"))))
(define $LHS (Mi "LHS"))
(define $RHS (Mi "RHS"))
(define $Idl (Mi "Idl"))
(define (&Idl P) (app $Idl P))
(define $Fil (Mi "Fil"))
(define (&Fil P) (app $Fil P))
(define (UpperBound A)
  (^ A $u))
(define (LowerBound A)
  (^ A $l))
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
(define $up (Mi "&uarr;"))
(define (&up a) (ap $up a))
(define $down (Mi "&darr;"))
(define (&down a) (ap $down a))
(define $Up (Mi "&uArr;"))
(define (&Up a) (ap $Up a))
(define $Down (Mi "&dArr;"))
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
(define $o*
  (Mo "&otimes;" #:attr* '((displaystyle "false"))))
(define $o+
  (Mo "&oplus;" #:attr* '((displaystyle "false"))))
(define $o-
  (Mo "&ominus;" #:attr* '((displaystyle "false"))))
(define $<< (Mo "&Lt;"))
(define-infix*
  (&o+ $o+)
  (&o- $o-)
  (&<< $<<)
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
  (@o+ &o+)
  (@o* &o*)
  (@cap &cap)
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
   ((Definition)
    "设" $P "是一个偏序集, " (&sube $A $P)
    ", 我们定义"
    (MB (&= (UpperBound $A)
            (setI (∈ $x $P) (&sube $A (&down $x)))))
    (MB (&= (LowerBound $A)
            (setI (∈ $x $P) (&sube $A (&up $x)))))
    "称" (UpperBound $A) "为" $A
    "的上界集, " (LowerBound $A) "为" $A
    "的下界集. 注意到"
    (&= (UpperBound $empty)
        (LowerBound $empty)
        $P) ".")
   ((Theorem)
    "设" $P "是一个偏序集, 而" $A "和" $B
    "是" $P "的子集, 那么"
    (Ol (Li (&sube $A $B) "可以推出"
            (&sube (UpperBound $B) (UpperBound $A)) "和"
            (&sube (LowerBound $B) (LowerBound $A)) ".")
        (Li (&sube $A (&cap (LowerBound (UpperBound $A))
                            (UpperBound (LowerBound $A)))) ".")
        (Li (&= (UpperBound $A)
                (UpperBound (LowerBound (UpperBound $A))))
            ", "
            (&= (LowerBound $A)
                (LowerBound (UpperBound (LowerBound $A)))) ".")))
   ((proof)
    (Ol (Li "如果" (∈ $x (UpperBound $B)) ", 那么"
            (&sube $B (&down $x)) ", 又因为" (&sube $A $B)
            ", 所以" (&sube $A (&down $x))
            ", 于是" (∈ $x (UpperBound $A))
            ". 另外一个的证明也是类似的.")
        (Li "设" (∈ $x $A) ", 我们要证明"
            (∈ $x (LowerBound (UpperBound $A))) "和"
            (∈ $x (UpperBound (LowerBound $A)))
            ". " (∈ $x (LowerBound (UpperBound $A)))
            "等价于"
            (&sube (UpperBound $A) (&up $x))
            ". 对于任意的" (∈ $y (UpperBound $A))
            ", 我们知道" (&sube $A (&down $y))
            ". 又因为" (∈ $x $A) ", 所以"
            (∈ $x (&down $y)) ", 这等价于"
            (∈ $y (&up $x)) ", 由此得证. "
            "另外一种情形是类似的.")
        (Li "根据1和2, 我们知道"
            (&sube (UpperBound (LowerBound (UpperBound $A)))
                   (UpperBound $A))
            "和"
            (&sube (LowerBound (UpperBound (LowerBound $A)))
                   (LowerBound $A))
            ". 另外, 如果再次应用2, 将"
            (UpperBound $A) "代入" $A
            ", 就会发现"
            (&sube (UpperBound $A)
                   (UpperBound (LowerBound (UpperBound $A))))
            "; 将" (LowerBound $A) "代入" $A
            ", 就会发现"
            (&sube (LowerBound $A)
                   (LowerBound (UpperBound (LowerBound $A))))
            ".")))
   
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
   ((proof)
    "根据上确界和下确界的定义很容易推出.")
   ((Example)
    (Ol (Li "每个链都是格, 例如实数集, 有理数集, 自然数集.")
        (Li "设" (tu0 $X (Open $X)) "是一个拓扑空间, 则"
            (tu0 (Open $X) $sube) "是一个格; 幂集"
            (powset $X) "作为" $X "上的离散拓扑自然也是一个格.")
        (Li (tu0 $NN $\|) "是一个格, "
            (&join $m $n) "是" $m "和" $n
            "的最小公倍数, " (&meet $m $n)
            "是" $m "和" $n "的最大公约数.")))
   ((Theorem)
    "设" $L "是一个偏序集, 那么"
    (Ol (Li $L "是一个格当且仅当" $L
            "的任意有限非空子集都有上确界和下确界.")
        (Li $L "是格当且仅当" (&op $L) "是格.")))
   ((proof)
    
    )
   ((Theorem)
    "非空集合" $L "可以作为一个格当且仅当"
    $L "上存在两个二元运算" $o* "和" $o+ "满足"
    (Ol (Li (tu0 $L $o*) "和" (tu0 $L $o+)
            "都是交换幂等半群;")
        (Li $o* "和" $o+ "满足吸收律, 即"
            (&= (&o* $x (@o+ $x $y)) $x
                (&o+ $x (@o* $x $y))) ".")))
   ((proof)
    
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
    "首先, 1和2, 3和4, 5和6显然都是等价的. "
    
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
    "(在继承的序下)也是一个完备格, 其中对于"
    (&Im $f) "的任意子集" $A "都有"
    (&= (Meet $f $A) (Meet $A)) ", "
    (&= (Join $f $A) (app $f (Join $A))) ".")
   ((proof)
    "由幂等性可知" (&Im $f) "是" $f
    "的不动点集合. 设" $A "是"
    (&Im $f) "的一个子集. 令"
    (&= $a (Meet $A)) ", 如果"
    (∈ $a (&Im $f)) ", 那么"
    (&= (Meet $f $A) (Meet $A))
    ". 我们将要证明" (∈ $a (&Im $f))
    ", 即" (&= (app $f $a) $a) "."
    (MB (&<= (&= (app $f $a)
                 (app $f (Meet $A)))
             (&= (Meet (app $f $A))
                 (Meet $A)
                 $a)))
    "而又根据" $f "是闭包算子, 所以"
    (&<= $a (app $f $a)) ", 于是"
    (&= (app $f $a) $a)
    ". 我们已经证明了一半, "
    "接着我们还需要证明"
    (app $f (Join $A))
    "是" $A "在" (&Im $f)
    "中的上确界, 如果"
    $A "是" (&Im $f)
    "的任意一个子集. 显然, "
    (app $f (Join $A)) "是"
    (app $f $A)
    "的一个上界, 也是" $A
    "的一个上界. 因此, "
    (app $f (Join $A)) "在"
    (&Im $f) "中也是" $A
    "的一个上界. 如果"
    (∈ $b (&Im $f))
    "也是" $A "在" (&Im $f)
    "中的一个上界, 我们想要证明"
    (&<= (app $f (Join $A)) $b)
    ". 然而, 既然" $b "在" (&Im $f)
    "中是" $A "的上界, 那么在" $L
    "中自然也是" $A "的上界. 换言之, "
    (&<= (Join $A) $b) ". 根据"
    $f "的保序性质, 立即得到"
    (&<= (app $f (Join $A))
         (&= (app $f $b) $b))
    ". "
    )
   ((tcomment)
    "上述证明用到了这样一个事实: "
    "对于偏序集" $P ", 对于其某个非空子集"
    $S ", " $S "继承了一个自然的偏序. 对于"
    (&sube $A $S) ", 如果"
    (Meet $A) "存在且" (∈ (Meet $A) $S)
    ", 那么" (Meet $A) "在子偏序集"
    $S "中也是" $A "的最大下界. "
    "类似的事实当然对于" $Join "也成立.")
   ((Corollary)
    
    )
   ((Definition)
    "设" $P "是一个偏序集, " (&sube $S $P) "."
    (Ol (Li "如果对于任意的" (∈ $x $P) "都有"
            (&= $x (Join (@cap $S (&down $x))))
            ", 那么就称" $S "为" $P
            "的并生成子集, 或称" $P "可由" $S
            "生成. "
            )
        )
    )
   ((Example)
    
    )
   (H3. "序同构与格同构")
   ((Definition)
    "设" (&cm $P $Q) "是两个偏序集, "
    (func $f $P $Q) "是一个映射."
    (Ol (Li "如果对于任意的" (∈ $x $y $P)
            "都有" (&<= $x $y) "当且仅当"
            (&<= (app $f $x) (app $f $y))
            ", 则称" $f "为从" $P "到" $Q
            "的序嵌入, 记作"
            
            )
        )
    )
   (H3. "分配格与Boole代数")
   ((Theorem)
    "设" $L "是一个格, 则下面两个有限分配律是等价的:"
    (Ol (Li (distributeR $x &meet $y &join $z) ";")
        (Li (distributeR $x &join $y &meet $z) ".")))
   ((proof)
    "1推出2: 设" (∈ $x $y $z $L) ", 那么"
    (MB (deriv
         (&meet (@join $x $y) (@join $x $z))
         (&join (@meet (@join $x $y) $x)
                (@meet (@join $x $y) $z))
         (&join $x (@meet (@join $x $y) $z))
         (&join $x (@meet $x $z) (@meet $y $z))
         (&join $x (@meet $y $z))))
    "2推出1也是类似的.")
   ((Definition)
    "设" $L "是一个格, 如果" $L
    "满足上述定理中描述的两个条件之一, 那么称"
    $L "为分配格.")
   ((Example)
    (Ol (Li "每个链都是分配格.")
        (Li (tu0 $NN $\|) "是分配格.")
        (Li "设" (tu0 $X (Open $X)) "是拓扑空间, 则"
            (tu0 (Open $X) $sube) "是分配格.")))
   ((Theorem)
    "设" $L "是一个格, 则" $L "是分配格当且仅当"
    $L "不含有形如" $M_3 "或" $N_5 "的子格.")
   ((proof)
    
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
   (H3. "理想和滤子")
   ((Definition)
    "设" $P "是一个偏序集, " $I "和" $F "是" $P "的子集."
    (Ol (Li "如果" $I "是一个定向的下集, 则称"
            $I "是" $P "的理想 (ideal).")
        (Li "如果" $F "是一个滤过的上集, 则称"
            $F "是" $P "的滤子 (filter)."))
    "如果" $S "是" $P "的一个理想且" (&!= $S $P)
    ", 则称" $S "为" $P "的真理想. "
    "类似地, 我们可以定义真滤子. "
    "对于任意的" (∈ $x $P) ", 子集"
    (&down $x) "都是一个理想, 其被称为关于"
    $x "的主理想 (principal ideal). "
    "类似地, " (&up $x) "都是一个滤子, "
    "其被称为关于" $x
    "的主滤子 (principal filter). 记"
    (&Idl $P) "为" $P "的所有理想之集合, "
    (&Fil $P) "为" $P "的所有滤子之集合. "
    "读者应该注意的是, 理想和滤子首先都是非空的.")
   ((tcomment)
    "主理想也可以刻画为具有最大元的下集, "
    "主滤子也可以刻画为具有最小元的上集.")
   ((Example)
    
    )
   ((Theorem)
    "设" $L "是一个格, " $I "是" $L
    "的非空子集, 则下列条件等价:"
    (Ol (Li $I "是理想;")
        (Li $I "是对于" $join "封闭的下集;")
        (Li $I "对于" $join "封闭, 对于"
            $meet "吸收."))
    $I "对于" $meet "吸收的意思是对于每个" (∈ $a $I) "和"
    (∈ $b $L) ", 都有" (∈ (&meet $a $b) $I) ".")
   ((proof)
    "1推出2: "
    )
   (P "对偶地, 我们有以下定理.")
   ((Theorem)
    
    )
   ((Theorem)
    "设" $L "是一个格, " $S "是" $L
    "的一个非空子集, 令"
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   ((Corollary)
    
    )
   ((Definition)
    
    )
   ((Theorem)
    
    )
   ((Theorem)
    
    )
   (H3. "格中的特殊元素")
   ((Definition)
    "设" $L "是一个格, " (∈ $a $L) "但不是最大元素, "
    (∈ $b $L) "但不是最小元素."
    (Ol (Li "如果对于任意的" (∈ $x $y $L) ", "
            (&<= (&meet $x $y) $a) "可以推出"
            (&<= $x $a) "或" (&<= $y $a)
            ", 则称" $a "为交素元 (meet-prime element).")
        (Li "如果对于任意的" (∈ $x $y $L) ", "
            (&= (&meet $x $y) $a) "可以推出"
            (&= $x $a) "或" (&= $y $a)
            ", 则称" $a "为交既约元 "
            "(meet-irreducible element).")
        (Li "如果对于任意的" (∈ $x $y $L) ", "
            (&<= $b (&join $x $y)) "可以推出"
            (&<= $b $x) "或" (&<= $b $y)
            ", 则称" $b "为并素元 (join-prime element).")
        (Li "如果对于任意的" (∈ $x $y $L) ", "
            (&= $b (&join $x $y)) "可以推出"
            (&= $b $x) "或" (&= $b $y)
            ", 则称" $b "为并既约元 "
            "(join-irreducible element)."))
    "用" (app $M $L) "表示" $L "的交素元之集, "
    (app $J $L) "表示" $L "的并素元之集. "
    
    )
   ((Theorem)
    "设" $L "是一个格, 那么"
    (Ol (Li "交素元必为交既约元, 并素元必为并既约元.")
        (Li "如果" $L "是分配格, 那么"
            "交素元等价于交既约元, "
            "并素元等价于并既约元.")))
   ((proof)
    
    )
   ((Theorem)
    "设" $L "是一个格, " $I "和" $F "是" $L
    "的子集, 那么"
    (Ol (Li ""
            )
        )
    )
   ((proof)
    
    )
   ((Theorem)
    "设" $L "是一个格, " (∈ $x $L) ", 那么"
    (Ol (Li $x "是交素元当且仅当主理想"
            (&down $x) "是素理想.")
        (Li $x "是并素元当且仅当主滤子"
            (&up $x) "是素滤子.")))
   ((proof)
    
    )
   ((Theorem)
    
    )
   ((proof)
    
    )
   ((Theorem)
    
    )
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
               (&= (app $f $a) (&Min (app (inv $g) (&up $a))))) ";")
        (Li (∀ (∈ $b $Q)
               (&= (app $g $b) (&Max (app (inv $f) (&down $b))))) ".")))
   ((proof)
    "1推出2: 对于任意的" (∈ $a $P) ", 由于"
    (&<= $a (app $g (app $f $a))) ", 我们有"
    (∈ (app $f $a) (app (inv $g) (&up $a)))
    "; 任取" (∈ $y (app (inv $g) (&up $a)))
    ", 我们有" (&<= $a (app $g $y))
    ", 从而" (&<= (app $f $a) $y)
    ". 这说明" (&= (app $f $a) (&Min (app (inv $g) (&up $a))))
    "." (Br)
    "2推出1: 对于任意的" (∈ $a $P) "和" (∈ $b $Q)
    ", 如果" (&<= (app $f $a) $b) ", 则由"
    (&= (app $f $a) (&Min (app (inv $g) (&up $a))))
    "可知" (∈ (app $f $a) (app (inv $g) (&up $a)))
    ", 于是" (&<= $a (app $g (app $f $a)))
    ", 再根据" $g "的单调性就有"
    (&<= $a (app $g $b)) "; 反过来, 如果"
    (&<= $a (app $g $b)) ", 那么"
    (∈ (app $g $b) (&up $a))
    ", " (∈ $b (app (inv $g) (&up $a)))
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
    (∈ $b $Q) ", " (app (inv $h) (&down $b))
    "都有最大元; 又因为" $h "是保序的, 易知"
    (app (inv $h) (&down $b))
    "是一个下集, 从而是一个主理想." (Br)
    "右推左: 对于任意的" (∈ $b $Q) ", 由于"
    (app (inv $h) (&down $b))
    "是一个主理想, "
    (&= (app $g $b) (&Max (app (inv $h) (&down $b))))
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
    ". 于是, 对于任意的" (∈ $y $Q)
    ", 我们有" (&<= $0_P (app $g $y))
    ", 而这等价于" (&<= (app $f $0_P) $y)
    ". 由于" $y "的任意性, " (app $f $0_P)
    "是" $Q "的最小元" $0_Q ", 那么"
    (&= (app $f (Join $empty))
        (app $f $0_P)
        $0_Q
        (Join (app $f $empty)))
    ". 如果" (&!= $A $empty) ", 令"
    (&= $b (Join $A)) ", 我们需要证明"
    (&= (app $f $b)
        (Join (setI (app $f $a) (∈ $a $A))))
    ". 一方面, 由" $f "的保序性可知, "
    (app $f $b) "是" (setI (app $f $a) (∈ $a $A))
    "的一个上界. 另一方面, 设" $c "也是"
    (setI (app $f $a) (∈ $a $A))
    "的一个上界, 那么对于每个" (∈ $a $A)
    "都有" (&<= (app $f $a) $c)
    ", 于是" (&<= $a (app $g $c))
    ". 也就是说, " (app $g $c) "是"
    $A "的一个上界. 那么, "
    (&<= (&= $b (Join $A)) (app $g $c))
    ". 这等价于" (&<= (app $f $b) $c)
    ". 所以说, " (app $f $b) "是"
    (setI (app $f $a) (∈ $a $A))
    "的最小上界, 即"
    (&= (app $f (Join $A))
        (Join (app $f $A)))
    ". 类似地, 可以证明" $g
    "保持任意存在的交.")
   ((tcomment)
    "如果说得更清楚一些的话, 则是: "
    "对于偏序集" $P "和" $Q
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
   ((proof)
    "2和1是对偶的, 这里仅证明1." (Br)
    "如果" $f "有右伴随, 根据前述定理, " $f
    "当然保持任意的并. 反过来, 如果" $f
    "保持任意的并, 我们想要证明" $f "有伴随. 令"
    (&= (app $g $y)
        (Join (setI (∈ $x $P)
                    (&<= (app $f $x) $y))))
    ". 对于任意的" (∈ $x $P) "和" (∈ $y $Q)
    ", 如果" (&<= (app $f $x) $y)
    ", 那么由于" (app $g $y) "是"
    (setI (∈ $x $P)
          (&<= (app $f $x) $y))
    "的一个上界, 所以有"
    (&<= $x (app $g $y))
    ". 反过来, 如果" (&<= $x (app $g $y))
    ", 那么"
    (&<= (app $f $x)
         (app $f (app $g $y)))
    ", 而"
    (MB (deriv0
         (app $f (app $g $y))
         $=
         (app $f (Join (setI (∈ $x $P)
                             (&<= (app $f $x) $y))))
         $=
         (Join (app $f (setI (∈ $x $P)
                             (&<= (app $f $x) $y))))
         $<= $y))
    "即" (&<= (app $f $x) $y)
    ". 至于" $g "的单调性, 由定义是容易看出来的. 因此, "
    $g "是" $f "的右伴随.")
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
    "仅需证明对于任意的" (∈ $a $b $c $H) "都有"
    (&<= (&meet $a (@join $b $c))
         (&join (@meet $a $b) (@meet $a $c)))
    ", 因为另一个方向对于普通的格都成立. 令"
    (&= $RHS (&join (@meet $a $b) (@meet $a $c)))
    ". 因为" (&<= (&meet $a $b) $RHS)
    ", 所以" (&<= $b (&-> $a $RHS))
    ". 因为" (&<= (&meet $a $c) $RHS)
    ", 所以" (&<= $c (&-> $a $RHS))
    ". 因此, " (&<= (&join $b $c) (&-> $a $RHS))
    ". 于是, " (&<= (&meet $a (@join $b $c)) $RHS) ".")
   ((Theorem)
    "每个有限分配格都是Heyting代数.")
   ((proof)
    "设" $H "是一个有限分配格. 对于任意的"
    (∈ $a $b $H) ", 令"
    (&= $S (setI (∈ $x $H)
                 (&<= (&meet $a $x) $b)))
    ". 我们想要说明" (∈ (Join $S) $S)
    ". 换言之, 我们要证明"
    (&<= (&meet $a (Join $S)) $b) ", 即"
    (&<= (Join (@meet $a $S)) $b)
    ". 根据" $S "的定义, " (&meet $a $S)
    "的元素都小于等于" $b
    ". 换言之, " $b "是" (&meet $a $S)
    "的一个上界, 当然有"
    (&<= (&meet $a (Join $S)) $b)
    ". 这说明" $H "是一个Heyting代数.")
   ((Theorem)
    "每个Boole代数都是Heyting代数.")
   ((proof)
    
    )
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
                 (&-> $a $c)) ";")
        (Li "如果" $a "是交素元, 那么"
            (&= (&-> $b $a) $1) "或"
            (&-> $b $a) "也是交素元.")))
   ((proof)
    (Ol (Li "由" (&<= (&meet $b $a) $b)
            "和剩余律立即得到.")
        (Li "由1可知" (&<= $a (&-> $1 $a))
            ", 另外"
            (&<= (&-> $1 $a)
                 (&meet (@-> $1 $a) $1)
                 $a)
            ", 由此可知"
            (&= (&-> $1 $a) $a) ".")
        (Li "如果" (&<= $a $b) ", 那么"
            (&<= (&-> $a $a) (&-> $a $b))
            ", 即" (&<= $1 (&-> $a $b))
            ", 又因为" (&<= (&-> $a $b) $1)
            ", 故" (&= (&-> $a $b) $1)
            ". 反过来, 如果"
            (&= (&-> $a $b) $1)
            ", 那么" (&<= $1 (&-> $a $b))
            ", 于是" (&<= (&meet $1 $a) $b)
            ", 即" (&<= $a $b) ".")
        (Li (&<= (&meet $a (@-> $a $b)) $a)
            "是" $meet "的定义的一部分, "
            (&<= (&meet $a (@-> $a $b)) $b)
            "可以根据剩余律轻松得到, 于是"
            (&<= (&meet $a (@-> $a $b))
                 (&meet $a $b))
            ". 由1知"
            (&<= $b (&-> $a $b))
            ", 故根据" $meet "的保序性有"
            (&<= (&meet $a $b)
                 (&meet $a (@-> $a $b)))
            ". 因此, 我们有"
            (&= (&meet $a (@-> $a $b))
                (&meet $a $b)) ".")
        (Li "如果" (&<= $x $y)
            ", 我们需要证明"
            (&<= (&-> $a $x) (&-> $a $y))
            ". 首先, 我们有"
            (&<= (&-> $a $x) (&-> $a $x))
            ", 于是"
            (&<= (&meet (@-> $a $x) $a) $x)
            ", 接着由" (&<= $x $y) "可知"
            (&<= (&meet (@-> $a $x) $a) $y)
            ", 最后得到"
            (&<= (&-> $a $x) (&-> $a $y))
            ". 如果" (&<= $x $y)
            ", 我们需要证明"
            (&<= (&-> $y $a) (&-> $x $a))
            ". 因为" (&<= $x $y)
            ", 所以"
            (&<= (&meet (@-> $y $a) $x)
                 (&meet (@-> $y $a) $y)
                 $a)
            ", 于是"
            (&<= (&-> $y $a) (&-> $x $a)) ".")
        (Li "因为" (&<= (&meet $b $c) $b) "和"
            (&<= (&meet $b $c) $c) ", 所以说"
            (&<= (&-> $a (@meet $b $c))
                 (&-> $a $b))
            "且"
            (&<= (&-> $a (@meet $b $c))
                 (&-> $a $c))
            ", 于是"
            (&<= (&-> $a (@meet $b $c))
                 (&meet (@-> $a $b) (@-> $a $c)))
            ". 我们还需要证明"
            (&<= (&meet (@-> $a $b) (@-> $a $c))
                 (&-> $a (@meet $b $c)))
            ". 这等价于证明"
            (&<= (&meet $a (@-> $a $b) (@-> $a $c))
                 (&meet $b $c))
            ". 我们知道"
            (MB (deriv
                 (&meet $a (@-> $a $b) (@-> $a $c))
                 (&meet (@meet $a $a) (@-> $a $b) (@-> $a $c))
                 (&meet (@meet $a (@-> $a $b))
                        (@meet $a (@-> $a $c)))))
            "鉴于" (&<= (&meet $a (@-> $a $b)) $b) "和"
            (&<= (&meet $a (@-> $a $c)) $c) ", 所以"
            (&<= (&meet (@meet $a (@-> $a $b))
                        (@meet $a (@-> $a $c)))
                 (&meet $b $c)) ".")
        (Li "因为" (&>= (&join $a $b) $a) "和"
            (&>= (&join $a $b) $b) ", 所以"
            (&<= (&-> (@join $a $b) $c)
                 (&-> $a $c))
            "且"
            (&<= (&-> (@join $a $b) $c)
                 (&-> $b $c))
            ", 于是"
            (&<= (&-> (@join $a $b) $c)
                 (&meet (@-> $a $c) (@-> $b $c)))
            ". 我们还需要证明"
            (&<= (&meet (@-> $a $c) (@-> $b $c))
                 (&-> (@join $a $b) $c))
            ". 这等价于证明"
            (&<= (&meet (@join $a $b)
                        (@-> $a $c) (@-> $b $c))
                 $c)
            ". 于是, 我们有"
            (MB (deriv^0
                 (&meet (@join $a $b)
                        (@-> $a $c) (@-> $b $c))
                 $=
                 (&join (@meet $a (@-> $a $c) (@-> $b $c))
                        (@meet $b (@-> $a $c) (@-> $b $c)))
                 $<=
                 (&join (@meet $c (@-> $b $c))
                        (@meet $c (@-> $a $c)))
                 $<= $c)))
        (Li "对于每个" (∈ $x $H) ", 我们有"
            (MB (deriv^0
                 (&<= $x (&-> $a (@-> $b $c)))
                 $<==>
                 (&<= (&meet $x $a) (&-> $b $c))
                 $<==>
                 (&<= (&meet $x $a $b) $c)
                 $<==>
                 (&<= $x (&-> (@meet $a $b) $c))))
            "分别取" (&= $x (&-> $a (@-> $b $c))) "和"
            (&= $x (&-> (@meet $a $b) $c)) "就能得到等式.")
        (Li "由" (&<= (&-> $a $b) (&-> $a $b)) "可知"
            (&<= (&meet (@-> $a $b) $a) $b) ", 于是"
            (&<= $a (&-> (@-> $a $b) $b)) ".")
        (Li "首先根据9, 我们可以知道"
            (&<= (&-> $a $b)
                 (&-> (@-> (@-> $a $b) $b) $b))
            ". 因此, 我们还需要证明"
            (&<= (&-> (@-> (@-> $a $b) $b) $b)
                 (&-> $a $b))
            ". 根据" (&-> (@ $dummy) $b)
            "的逆序性, 再次应用9, 可以得到这个结果.")
        (Li (&<= (&meet $a (@-> $a $b) (@-> $b $c))
                 (&meet $b (@-> $b $c))
                 $c)
            ", 于是"
            (&<= (&meet (@-> $a $b) (@-> $b $c))
                 (&-> $a $c)) ".")
        (Li ""
            )
        )
    )
   ((Theorem)
    "设" $H "是一个有最大元" $1 "的格, "
    (func $-> (&c* $H $H) $H)
    "是一个二元运算, 则" $H "在" $->
    "下是Heyting代数当且仅当对于任意的"
    (∈ $a $b $c $H) ", 有"
    (Ol (Li (&= (&-> $a $a) $1) ";")
        (Li (&= (&meet $a (@-> $a $b))
                (&meet $a $b)) ";")
        (Li (&= (&meet (@-> $a $b) $b) $b) ";")
        (Li (distributeR $a &-> $b &meet $c) ".")))
   ((proof)
    
    )
   ((Theorem)
    "设" $H "是具有最小元" $0
    "的Heyting代数, 定义"
    (&= $x^ (&-> $x $0))
    ", 那么对于任意的"
    (∈ $a $b $H) ", 我们有"
    (Ol (Li (&= (&prime $0) $1) ", "
            (&= (&prime $1) $0) ";")
        (Li (&= (&meet $a^ $a) $0) ";")
        (Li (func (&prime (@ $dummy)) $H $H)
            "是逆序的;")
        (Li (&= (&prime (@join $a $b))
                (&meet $a^ $b^)) ";")
        (Li (&<= $a (&prime (&prime $a))) ";")
        (Li (&= (&prime $a)
                (&prime (&prime (&prime $a)))) ".")))
   ((proof)
    (Ol (Li (&= (&prime $0)
                (&-> $0 $0)
                $1)
            "是显然的, 因为" (&= (&-> $a $a) $1)
            ", 不论" $a "是什么元素. "
            (&= (&prime $1) (&-> $1 $0)
                (&Max (setI (∈ $x $H) (&<= $x $0)))
                (&Max (setE $0)) $0) ".")
        (Li "我们知道"
            (&<= (&conj (@-> $a $0) $a) $0)
            ", 于是"
            (&= (&conj $a^ $a) $0) ".")
        (Li "此即" (&-> (@ $dummy) $a)
            "总是逆序的推论.")
        (Li "定理3.1.5的第7条的推论.")
        (Li "由" (&= (&conj $a^ $a) $0)
            "和剩余律立即得到, "
            "或者说是定理3.1.5的第9条的推论.")
        (Li "定理3.1.5的第10条的推论.")))
   ((Theorem)
    "设" $H "是一个有最小元" $0
    "的Heyting代数, 则下列条件等价:"
    (Ol (Li $H "相对于" (&prime (@ $dummy))
            "运算是一个Boole代数;")
        (Li (∀ (∈ $a $H) (&= (&join $a^ $a) $1)) ";")
        (Li (∀ (∈ $a $b $H)
               (&= (&-> $a $b)
                   (&join (&prime $a) $b))) ";")
        (Li "对于任意的" (∈ $a $H) ", "
            (&= (&-> $a $H)
                (setI (&-> $a $x) (∈ $x $H)))
            "是上集;")
        (Li (∀ (∈ $a $b $H)
               (&= (&-> (@-> $a $b) $b)
                   (&join $a $b))) ";")
        (Li (∀ (∈ $a $H)
               (&= (&prime (&prime $a)) $a)) ";")
        (Li (func (&prime (@ $dummy)) $H $H)
            "是满射;")
        (Li "对于任意的" (∈ $a $H) ", 如果"
            (&= (&prime $a) $0) ", 那么"
            (&= $a $1) ".")))
   ((proof)
    "1推出2: 定义." (Br)
    "2推出3: 先证明"
    (&<= (&join (&prime $a) $b) (&-> $a $b))
    ". 这等价于证明"
    (&<= (&prime $a) (&-> $a $b)) "和"
    (&<= $b (&-> $a $b))
    ". 鉴于" (&<= $0 $b) ", 所以"
    (&<= (&-> $a $0) (&-> $a $b))
    ". 第二个不等式等价于"
    (&<= (&meet $b $a) $b)
    ", 这个根据定义即得. "
    "现在我们要证明"
    (&<= (&-> $a $b)
         (&join (&prime $a) $b))
    ". 鉴于" (&= (&join $a^ $a) $1)
    ", 我们有"
    (MB (&= (&-> $a $b)
            (&meet (@-> $a $b) (@join $a^ $a))
            (&join (@meet (@-> $a $b) $a^)
                   (@meet (@-> $a $b) $a))))
    "我们知道这个析取左支小于等于" $a^
    ", 这个析取右支小于等于" $b
    ", 而这正是我们想要的." (Br)
    "3推出4: 对于" (∈ $x (@-> $a $H))
    ", 存在" (∈ $x_0 $H) "使得"
    (&= $x (&-> $a $x_0))
    ". 根据3的条件, 即"
    (&= $x (&join (&prime $a) $x_0))
    ". 对于" (∈ $y $H) "满足" (&<= $x $y)
    ", 我们知道"
    (&<= (&join (&prime $a) $x_0) $y)
    ", 于是"
    (MB (deriv^
         (&join (@join (&prime $a) $x_0) $y)
         (&join (&prime $a) (@join $x_0 $y))
         (&-> $a (@join $x_0 $y))
         $y))
    "即" (∈ $y $H) "." (Br)
    "4推出5: 首先, "
    (&<= (&join $a $b)
         (&-> (@-> $a $b) $b))
    "在任意的Heyting代数中均成立, "
    "所以我们仅需要证明"
    (&<= (&-> (@-> $a $b) $b)
         (&join $a $b))
    ". "
    )
   (P "设" $H "是一个有最小元" $0 "的Heyting代数, "
      "如果元素" $a "满足"
      (&= (&prime (&prime $a)) $a)
      ", 则称" $a "为" $H "的一个正则元素, "
      "或者说Boole元素. 记" (app $B $H)
      "为" $H "的正则元素全体构成的集合, 显然"
      (∈ $0 $1 (app $B $H)) ", 而且"
      (∈ $a (app $B $H)) "当且仅当存在"
      (∈ $b $H) "满足" (&= $a (&prime $b)) ".")
   ((Theorem)
    "设" $H "是一个Heyting代数, 如果"
    (∈ $x $y (app $B $H)) ", 那么"
    (∈ (&meet $x $y) (&-> $x $y)
       (app $B $H)) ".")
   ((proof)
    
    )
   (H3. "滤子和同余关系之间的一一对应")
   (H2. "Frame和拓扑表示定理")
   
   (H3. "Frame的定义和基本性质")
   ((Definition)
    "设" $L "是一个完备格, 如果对于任意的"
    (∈ $a $L) "和" (&sube $S $L)
    ", 都有"
    (MBL "(IFD)"
         (&= (&meet $a (Join $S))
             (Join (∈ $s $S) (@meet $a $s))))
    "则称" $L "为一个frame. "
    "我们将IFD这种性质叫做无限分配律.")
   ((Definition)
    "设" (func $f $A $B)
    "是frame之间的一个映射, 如果"
    $f "保持有限的交和任意的并 "
    "(包括空集的交和并), 则称" $f
    "是从" $A "到" $B "的frame同态. "
    "全体frame和frame同态构成的范畴记为"
    $Frm ".")
   ((Remark)
    (Ol (Li "若" (func $f $A $B)
            "是一个frame同态, 那么"
            (&= (app $f $0) $0) ", "
            (&= (app $f $1) $1) ".")
        (Li "当" $A "和" $B "仅为完备格时, 如果"
            (func $f $A $B) "保持有限交和任意的并, "
            "我们仍然将这种映射称为frame同态.")))
   ((Example)
    
    )
   ((Definition)
    "设" $L "是一个完备格, " (∈ $F (&Fil $L))
    ". 如果对于任意的" (&sube $S $L) "都有"
    (∈ (Join $S) $F) "可以推出"
    (&!= (&cap $S $F) $empty)
    ", 则称" $F "为" $L "的完全素滤子.")
   
   (H3. "空间式frame和sober空间")
   (H2. "Domain与连续格")
   (H3. "基本domain结构")
   ((Definition)
    "设" $L "是一个偏序集, 如果" $L
    "的每个定向子集都有上确界, 那么称"
    $L "为定向完备偏序集 (dcpo).")
   ((Definition)
    "设" $L "是一个dcpo, " (∈ $x $y $L)
    ". 如果对于" $L "的任意定向子集"
    $D ", " (&<= $x (Join $D))
    "可以推出" (∈ $y (&down $D))
    ", 则称" $y "双小于 (way below) "
    $x ", 记作" (&<< $x $y)
    ". 并且, 我们记"
    (MB (&= (&Down $x)
            (setI (∈ $y $L)
                  (&<< $y $x))))
    (MB (&= (&Up $x)
            (setI (∈ $z $L)
                  (&<< $x $z)))))
   ((Remark)
    
    )
   ((Theorem)
    "设" $L "是一个dcpo, 那么对于任意的"
    (∈ $u $x $y $z $L) ", 我们有"
    (Ol (Li (&<< $x $y) "可以推出" (&<= $x $y) ";")
        (Li ""
            )
        )
    )
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
        (Li "如果" (&<= $a $b) ", 那么"
            (&<= (&o* $a $c) (&o* $b $c)) ", "
            (&<= (&-> $c $a) (&-> $c $b)) ", "
            (&<= (&-> $b $c) (&-> $a $c)) ";")
        (Li (&<= (&o* $a (@-> $a $b))
                 (&meet $a $b)) ";")
        (Li (&<= $b (&-> $a (@o* $a $b))) ";")
        (Li (&= (&-> $a (@-> $b $c))
                (&-> (@o* $a $b) $c)
                (&-> $b (@-> $a $c))) ";")
        (Li (&<= (&o* (@-> $a $b)
                      (@-> $b $c))
                 (&-> $a $c)) ";")
        (Li (&= (&-> $a (@o* $a (@-> $a $b)))
                (&-> $a $b)) ";")
        (Li (&<= (&join $a $b)
                 (&-> (@-> $a $b) $b)) ";")
        (Li (&= (&-> (@-> (@-> $a $b) $b) $b)
                (&-> $a $b)) ";")
        (Li (&= (&-> (@join $a $b) $c)
                (&meet (@-> $a $c) (@-> $b $c))) ";")
        (Li (&= (&-> $a (@meet $b $c))
                (&meet (@-> $a $b) (@-> $a $c))) ";")
        (Li (&<= (&-> $b $c)
                 (&-> (@meet $a $b)
                      (@meet $a $c)))
            ", "
            (&<= (&-> $b $c)
                 (&-> (@join $a $b)
                      (@join $a $c))) ";")
        (Li (&<= (&-> $b $c)
                 (&-> (@-> $a $b)
                      (@-> $a $c)))
            ", "
            (&<= (&-> $b $c)
                 (&-> (@-> $c $a)
                      (@-> $b $a))) ";")
        (Li (&= (&-> (@join $a $b) $b)
                (&-> $a $b)
                (&-> $a (@meet $a $b))) ";"))
    "如果" $L "是完备剩余格, 那么对于任意的"
    )
   ((proof)
    (Ol (Li "由" (&<= $a $a) "和" $1 "为" $o*
            "的单位元可知"
            (&<= (&o* $1 $a) $a) ", 而这等价于"
            (&<= $1 (&-> $a $a))
            ". 因此, " (&= (&-> $a $a) $1)
            ". (这是一般性的结论.) 根据"
            (&<= $a (&-> $b $b))
            "可知" (&<= (&o* $a $b) $b)
            ", 根据" (&<= $b (&-> $a $a))
            "可知" (&<= (&o* $b $a) $a) ", 即"
            (&<= (&o* $a $b) $a) ". 于是, "
            (&<= (&o* $a $b) (&meet $a $b)) ".")
        (Li "由" (&<= $a (&-> $0 $0)) "可知"
            (&<= (&o* $a $0) $0) ", 于是"
            (&= (&o* $a $0) $0)
            ". 由" (&<= $x (&-> $1 $a))
            "等价于" (&<= (&o* $x $1) $a)
            ", 即" (&<= $x $a) ", 可知"
            (&= (&-> $1 $a) $a) ".")
        (Li "由" (&<= $a (&-> $b $b)) "可知"
            (&<= (&o* $a $b) $b) ", 于是"
            (&<= (&o* $b $a) $b) ", 因此"
            (&<= $b (&-> $a $b)) ".")
        (Li (&= (&-> $a $b) $1) "和"
            (&<= $1 (&-> $a $b))
            "是等价的. " (&<= $1 (&-> $a $b))
            "和" (&<= (&o* $1 $a) $b)
            "是等价的. " (&<= (&o* $1 $a) $b)
            "和" (&<= $a $b) "是等价的. 于是, "
            (&<= $a $b) "和" (&= (&-> $a $b) $1)
            "是等价的.")
        (Li "因为" (&<= (&o* $b $c) (&o* $b $c)) ", 那么"
            (&<= $b (&-> $c (@o* $b $c)))
            ", 又因为" (&<= $a $b) ", 所以"
            (&<= $a (&-> $c (@o* $b $c))) ", 于是"
            (&<= (&o* $a $c) (&o* $b $c))
            ". 因为" (&<= (&-> $c $a) (&-> $c $a))
            ", 所以" (&<= (&o* (@-> $c $a) $c) $a)
            ", 又因为" (&<= $a $b) ", 所以"
            (&<= (&o* (@-> $c $a) $c) $b)
            ", 于是" (&<= (&-> $c $a) (&-> $c $b))
            ". 因为" (&<= (&-> $b $c) (&-> $b $c))
            ", 所以" (&<= (&o* (@-> $b $c) $b) $c)
            ", 于是" (&<= $b (&-> (@-> $b $c) $c))
            ", 又" (&<= $a $b) ", 可知"
            (&<= $a (&-> (@-> $b $c) $c))
            ", 于是" (&<= (&o* $a (@-> $b $c)) $c)
            ", 最终得到" (&<= (&-> $b $c) (&-> $a $c)) ".")
        (Li "由" (&<= (&-> $a $b) (&-> $a $b)) "可知"
            (&<= (&o* $a (@-> $a $b)) $b)
            ", 而由1我们知道"
            (&<= (&o* $a (@-> $a $b))
                 (&meet $a (@-> $a $b))
                 $a)
            ", 于是"
            (&<= (&o* $a (@-> $a $b))
                 (&meet $a $b)) ".")
        (Li "由" (&<= (&o* $a $b) (&o* $a $b)) "可知"
            (&<= $b (&-> $a (@o* $a $b))) ".")
        (Li "设" (∈ $x $L) ", 我们有"
            (MB (deriv^0
                 (&<= $x (&-> $a (@-> $b $c)))
                 $<==>
                 (&<= (&o* $x $a) (&-> $b $c))
                 $<==>
                 (&<= (&o* $x $a $b) $c)
                 $<==>
                 (&<= $x (&-> (@o* $a $b) $c))
                 $<==>
                 (&<= (&o* $x $b $a) $c)
                 $<==>
                 (&<= (&o* $x $b) (&-> $a $c))
                 $<==>
                 (&<= $x (&-> $b (@-> $a $c))))))
        (Li (MB (deriv^0
                 (&o* $a (@-> $a $b) (@-> $b $c))
                 $<=
                 (&o* $b (@-> $b $c))
                 $<= $c))
            "于是, 我们有"
            (&<= (&o* (@-> $a $b) (@-> $b $c))
                 (&-> $a $c)) ".")
        (Li "设" (∈ $x $L) ", 我们有"
            (MB (deriv^0
                 (&<= $x (&-> $a (@o* $a (@-> $a $b))))
                 $==>
                 (&<= (&o* $x $a) (&o* $a (@-> $a $b)))
                 $==>
                 (&<= (&o* $x $a) $b)
                 $==>
                 (&<= $x (&-> $a $b))))
            "于是"
            (&<= (&-> $a (@o* $a (@-> $a $b)))
                 (&-> $a $b))
            ". 另外, 根据"
            (&<= (&o* $a (@-> $a $b))
                 (&o* $a (@-> $a $b)))
            ", 可知"
            (&<= (&-> $a $b)
                 (&-> $a (@o* $a (@-> $a $b)))) ".")
        (Li "首先, "
            (&<= $a (&-> (@-> $a $b) $b))
            "等价于"
            (&<= (&o* $a (@-> $a $b)) $b)
            ", 这显然成立. 其次, "
            (&<= $b (&-> (@-> $a $b) $b))
            "等价于"
            (&<= (&-> $a $b) (&-> $b $b))
            ", 这也显然成立.")
        (Li "根据"
            (&<= (&o* (@-> $a $b)
                      (@-> (@-> $a $b) $b))
                 $b)
            "这一事实, 可以得到"
            (&<= (&-> $a $b)
                 (&-> (@-> (@-> $a $b) $b) $b))
            ". 另外, 根据11我们有"
            (&<= $a (&-> (@-> $a $b) $b))
            ", 再根据5可得"
            (&<= (&-> (@-> (@-> $a $b) $b) $b)
                 (&-> $a $b)) ".")
        (Li "因为" (&>= (&join $a $b) $a) ", 所以"
            (&<= (&-> (@join $a $b) $c) (&-> $a $c))
            ". 因为" (&>= (&join $a $b) $b) ", 所以"
            (&<= (&-> (@join $a $b) $c) (&-> $b $c))
            ". 因此, "
            (&<= (&-> (@join $a $b) $c)
                 (&meet (@-> $a $c) (@-> $b $c)))
            ". 我们还需要证明"
            (&<= (&meet (@-> $a $c) (@-> $b $c))
                 (&-> (@join $a $b) $c))
            ". 令" (&= $x (&meet (@-> $a $c) (@-> $b $c)))
            ". 因为" (&<= $x (&-> $a $c))
            ", 所以" (&<= $a (&-> $x $c))
            ". 同理, " (&<= $b (&-> $x $c))
            ". 于是, " (&<= (&join $a $b) (&-> $x $c))
            ". 那么, " (&<= $x (&-> (@join $a $b) $c)) ", 即"
            (&<= (&meet (@-> $a $c) (@-> $b $c))
                 (&-> (@join $a $b) $c)) ".")
        (Li "因为" (&<= (&meet $b $c) $b) "和"
            (&<= (&meet $b $c) $c) ", 所以"
            (&<= (&-> $a (@meet $b $c)) (&-> $a $b)) "且"
            (&<= (&-> $a (@meet $b $c)) (&-> $a $c))
            ", 于是"
            (&<= (&-> $a (@meet $b $c))
                 (&meet (@-> $a $b) (@-> $a $c)))
            ". 我们还需要证明"
            (&<= (&meet (@-> $a $b) (@-> $a $c))
                 (&-> $a (@meet $b $c)))
            ". 这等价于"
            (&<= (&o* $a (@meet (@-> $a $b) (@-> $a $c)))
                 (&meet $b $c))
            ". 我们将其处理为分别证明"
            (&<= (&o* $a (@meet (@-> $a $b) (@-> $a $c))) $b)
            "和"
            (&<= (&o* $a (@meet (@-> $a $b) (@-> $a $c))) $c)
            ". 先来看第一个, 第二个的证明是类似的. "
            "我们知道"
            (&<= (&meet (@-> $a $b) (@-> $a $c))
                 (&-> $a $b))
            ", 再根据单调性, 我们有"
            (&<= (&o* $a (@meet (@-> $a $b) (@-> $a $c)))
                 (&o* $a (@-> $a $b))
                 $b)
            ". 这就是我们所要的, 另一个是差不多的.")
        (Li "对于第一条, 这等价于证明"
            (MB (&<= (&o* (@meet $a $b) (@-> $b $c))
                     (&meet $a $c)))
            "而这等价于"
            (MB (&<= (&o* (@meet $a $b) (@-> $b $c)) $a) "且"
                (&<= (&o* (@meet $a $b) (@-> $b $c)) $c))
            "对于右边的不等式, 我们可以这样证明:"
            (MB (deriv0
                 (&o* (@meet $a $b) (@-> $b $c))
                 $<=
                 (&o* $b (@-> $b $c))
                 $<=
                 $c))
            "对于左边的不等式, 我们需要用到1来证明:"
            (MB (deriv0
                 (&o* (@meet $a $b) (@-> $b $c))
                 $<=
                 (&meet (@meet $a $b) (@-> $b $c))
                 $<=
                 (&meet $a $b)
                 $<=
                 $a))
            "对于第二条, 这等价于证明"
            (MB (&<= (&-> $b $c)
                     (&meet (@-> $a (@join $a $c))
                            (@-> $b (@join $a $c)))))
            "而这又等价于证明"
            (MB (&<= (&o* $a (@-> $b $c))
                     (&join $a $c))
                "且"
                (&<= (&o* $b (@-> $b $c))
                     (&join $a $c)))
            "左边的证明:"
            (MB (deriv0
                 (&o* $a (@-> $b $c))
                 $<=
                 (&meet $a (@-> $b $c))
                 $<= $a
                 $<= (&join $a $c)))
            "右边的证明:"
            (MB (deriv0
                 (&o* $b (@-> $b $c))
                 $<= $c
                 $<= (&join $a $c))))
        (Li "对于第一条, 这等价于"
            (MB (&<= (&o* (@-> $a $b) (@-> $b $c))
                     (&-> $a $c)))
            "这就是9. 对于第二条, 这等价于"
            (MB (&<= (&o* (@-> $b $c) (@-> $c $a))
                     (&-> $b $a)))
            "这还是9.")
        (Li "根据13, 我们有"
            (MB (&= (&-> (@join $a $b) $b)
                    (&meet (@-> $a $b) (@-> $b $b))
                    (&meet (@-> $a $b) $1)
                    (&-> $a $b)))
            "根据14, 我们有"
            (MB (&= (&-> $a (@meet $a $b))
                    (&meet (@-> $a $a) (@-> $a $b))
                    (&meet $1 (@-> $a $b))
                    (&-> $a $b))))
        )
    )
   ((Theorem)
    "设" $L "是一个有界格, "
    (func (&cm $o* $->) (&c* $L $L) $L)
    "是两个运算, 其中" $o* "是以" $1
    "为单位元的交换幺半群运算, 那么"
    (tu0 $L $o* $->)
    "构成剩余格当且仅当对于任意的"
    (∈ $a $b $c $L) "有"
    (Ol (Li (&= (&-> (@o* $a $b) $c)
                (&-> $a (@-> $b $c))) ";")
        (Li (&= (&join (@o* $a (@-> $a $b)) $b)
                $b) ";")
        (Li (&= (&-> $a (@join $a $b)) $1) ".")))
   ((proof)
    "由剩余格推出这三条性质, 根据前述定理, 基本上是显然的. "
    "显然我们证明反方向, 对于满足这些性质的运算, "
    "我们说明" (tu0 $L $o* $->)
    "构成了一个剩余格. "
    )
   ((Theorem)
    "在剩余格中, 如果" $o* "是幂等运算, 那么" (&= $o* $meet)
    ". 换言之, 有界Heyting代数是唯一一类乘法运算具有幂等性的剩余格.")
   ((proof)
    
    )
   (P "设" $L "是一个剩余格, 定义"
      (func (&prime (@ $dummy)) $L $L)
      "为" (&= $a^ (&-> $a $0))
      ". 称" $a^ "为" $a "的否定.")
   ((Theorem)
    "设" $L "是一个剩余格, 那么"
    (Ol (Li (&= (&prime $0) $1) ", "
            (&= (&prime $1) $0) ";")
        (Li (&<= $a (&prime (&prime $a))) ", "
            (&= (&prime (&prime (&prime $a)))
                (&prime $a)) ";")
        (Li (&= (&o* $a (&prime $a)) $0) ";")
        (Li ""
            )
        )
    )
   ((proof)
    
    )
   (P "设" $L "是一个剩余格, 定义"
      (func (&cm $o+ $o-) (&c* $L $L) $L) "为"
      (MB (&cm (&= (&o+ $a $b)
                   (&prime (@o* $a^ $b^)))
               (&= (&o- $a $b)
                   (&o* $a $b^))))
      "显然, " $o+ "是一个交换的半群运算.")
   ((Theorem)
    "设" $L "是一个剩余格, 那么对于任意的"
    (∈ $a $b $c $L) ", 我们有"
    (Ol (Li (&<= (&o- $a $b) $a (&o+ $a $b)) ";")
        (Li (&<= $a $b) "可以推出"
            (&= (&o- $a $b) $0) ";")
        (Li ""
            )
        )
    )
   ))