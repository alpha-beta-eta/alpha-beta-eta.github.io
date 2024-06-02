#lang racket
(provide lattice_theory.html)
(require SMathML)
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
(define $sqsube (Mo "&sqsube;"))
(define (|[]| a b)
  (: $lb a $cm b $rb))
(define $. (Mo "."))
(define (∀ x P)
  (: $forall x $. P))
(define Σ $Sigma:normal)
(define Σ* (&* Σ))
(define $Sub (Mi "Sub"))
(define (&Sub G)
  (app $Sub G))
(define (powerset X)
  (app $P:script X))
(define $lfloor (Mo "&lfloor;"))
(define $rfloor (Mo "&rfloor;"))
(define (&floor x)
  (: $lfloor x $rfloor))
(define $pr (Mo "&pr;"))
(define $pre (Mo "&pre;"))
(define $<==> (Mo "&DoubleLongLeftRightArrow;"))
(define $\| (Mo "|"))
(define $NN* (&* $NN))
(define-infix*
  (&sqsube $sqsube)
  (&pr $pr)
  (&pre $pre)
  (&<==> $<==>)
  (&\| $\|))
(define ((answer #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "answer")))
   (B (format "解答~a." n)) " " x*))
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
        (Li "对于集合" $X ", 其幂集" (powerset $X)
            "在关系" $sube "下是一个偏序集. "
            "当然了, 这个幂集的非空子集"
            "继承了自然的序关系也成为偏序集. "
            "原书要求" $X "非空是不必要的, "
            "因为" (powerset $X) "即便在" $X
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
    
    )
   ((Definition)
    )
   ((Theorem)
    )
   ))