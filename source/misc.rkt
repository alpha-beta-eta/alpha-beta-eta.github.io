#lang racket
(provide misc.html)
(require SMathML "misc-utils.rkt")
(define $pr (Mo "&pr;"))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define (∀ x P)
  (: $forall x $. P))
(define $<=:id (Mi "&le;"))
(define $det (Mi "det"))
(define (&det A) (app $det A))
(define (&det0 A) (@ap $det A))
(define $adj (Mi "adj"))
(define (&adj A) (@ap $adj A))
(define eqnderiv (compose MB deriv))
(define @sum (compose pare sum))
(define (&delta i j)
  (appl $delta i j))
(define (∃ x P)
  (: $exists x $cm P))
(define-infix*
  (&=> $=>)
  (&pr $pr))
(define misc.html
  (TmPrelude
   #:title "misc"
   #:css "styles.css"
   (H1 "边角情况 (Corner Cases) 和等价定义")
   (P "学习数学必然要面对边角情况和等价定义 (甚至是并非等价而存在着微妙不同的定义), "
      "这总是令我异常烦躁.")
   ((example)
    "空集" $empty "可以被视为一个拓扑空间, 其上装备的拓扑必然是" (setE $empty) ".")
   ((proposition)
    "对于群" $G "的子群" $H ", 如果" $1_G "是" $G "的幺元, " $1_H
    "是" $H "的幺元, 那么" (&= $1_G $1_H) ".")
   ((proof)
    (MB (&= (&i* $1_H $1_H) $1_H
            (&i* $1_H $1_G)))
    "根据消去律, " (&= $1_H $1_G) ".")
   (H1 "算术")
   ((theorem)
    "对于" (&in (&cm $a $b) $NN) "且" (&!= $b $0) ", 如果" $r "是" $a "除以" $b "的余数, 那么"
    (&= (&gcd $a $b) (&gcd $b $r)) ".")
   ((proof)
    "作带余除法, " (&= $a (&+ (&i* $q $b) $r)) ". 因为" (div (&gcd $b $r) $b) "且"
    (div (&gcd $b $r) $r) ", 所以" (div (&gcd $b $r) (&+ (&i* $q $b) $r)) ", 即"
    (div (&gcd $b $r) $a) ". 于是, " (&gcd $b $r) "是" $a "和" $b "的公因数, 可知"
    (&<= (&gcd $b $r) (&gcd $a $b)) ". 类似地, " (&<= (&gcd $a $b) (&gcd $b $r)) ". 因此, "
    (&= (&gcd $a $b) (&gcd $b $r)) ".")
   (H1 "代数")
   ((definition)
    "给定集合" $X "和一个集合上的运算" $*0 ", 那么" (tu0 $X $*0) "被称为一个熔岩 (magma).")
   ((definition)
    "给定熔岩" (tu0 $X $*0) ", 对于" (&in $e $X) ", 称" $e "是熔岩的单位元, 如果对于每个"
    (&in $x $X) "有" (&= (&* $e $x) (&* $x $e) $x) ".")
   ((theorem)
    "如果熔岩" (tu0 $X $*0) "具有一个单位元, 那么熔岩的单位元是唯一的.")
   ((proof)
    "如果" $e_1 "和" $e_2 "是" $X "的单位元, 那么"
    (MB (&= $e_1 (&* $e_1 $e_2) $e_2) ".")
    )
   ((theorem)
    "给定具有单位元的结合熔岩" (tu0 $X $*0) ", 如果" (&in $x $X)
    "具有一个逆元, 那么这个逆元就是唯一的.")
   ((proof)
    "设" $e "是" $X "的单位元, 设" $y "和" $z "都是" $x "的逆, 那么"
    (MB (&= $y (&* $y $e) (&* $y (@ (&* $x $z)))
            (&* (@ (&* $y $x)) $z) (&* $e $z) $z) "."))
   ((definition)
    "我们称具有单位元的结合熔岩为幺半群 (monoid).")
   ((definition)
    "对于具有单位元的结合熔岩, 如果其每个元素均可逆, 那么就将其称为群 (group).")
   ((theorem #:n " (Cayley-Hamilton)")
    "令" $T "是有限维向量空间" $V "上的一个线性算子. 如果" $f "是" $T "的特征多项式, 那么" (&= (app $f $T) $0)
    ". 换言之, 极小多项式整除特征多项式.")
   ((proof)
    "设" (setE $alpha_1 $..h $alpha_n) "是" $V "的一个有序基, 而" $A
    "是" $T "在此有序基下的矩阵表示. 令" (&= $B (&- (&i* $x $I) $A))
    ", 这是多项式环上的矩阵. 另外, 设" $f "是" $T "的特征多项式, 那么我们知道"
    (&= (&det $B) $f) ", 以及" (&= (&i* $B (&adj $B)) (&i* $f $I))
    ". 根据" $A "的定义, 我们知道"
    (MB (&cm (&= (sum (&= $i $1) $n
                      (ap (app (mref $B $i $j) $T) $alpha_i))
                 $0)
             (&<= $1 $j $n)) ".")
    "这里的" (mref $B $i $j) "是一个多项式. 应用一个多项式于线性算子的"
    "结果是一个线性算子. 接着, 我们可以推出对于"
    (&= $k (&cm $1 $..h $n)) "有"
    (eqnderiv
     $0 (ap (app (mref (&adj $B) $j $k) $T)
            (@sum (&= $i $1) $n
                  (ap (app (mref $B $i $j) $T) $alpha_i)))
     (sum (&= $i $1) $n
          (ap (app (mref (&adj $B) $j $k) $T)
              (ap (app (mref $B $i $j) $T) $alpha_i)))
     (sum (&= $i $1) $n
          (ap (app (bra0 (&i* (mref (&adj $B) $j $k)
                              (mref $B $i $j))) $T)
              $alpha_i))
     (sum (&= $i $1) $n
          (ap (app (bra0 (&i* (mref $B $i $j)
                              (mref (&adj $B) $j $k))) $T)
              $alpha_i)))
    "在" $j "上求和, 我们得到"
    (eqnderiv
     $0 (sum (&= $j $1) $n
             (sum (&= $i $1) $n
                  (ap (app (bra0 (&i* (mref $B $i $j)
                                      (mref (&adj $B) $j $k))) $T)
                      $alpha_i)))
     (sum (&= $i $1) $n
          (sum (&= $j $1) $n
               (ap (app (bra0 (&i* (mref $B $i $j)
                                   (mref (&adj $B) $j $k))) $T)
                   $alpha_i)))
     (sum (&= $i $1) $n
          (ap (@sum (&= $j $1) $n
                    (app (bra0 (&i* (mref $B $i $j)
                                    (mref (&adj $B) $j $k))) $T))
              $alpha_i))
     (sum (&= $i $1) $n
          (ap (app (@sum (&= $j $1) $n
                         (&i* (mref $B $i $j)
                              (mref (&adj $B) $j $k))) $T)
              $alpha_i))
     (sum (&= $i $1) $n
          (ap (app (&i* (&delta $i $k) (&det0 $B)) $T)
              $alpha_i))
     (sum (&= $i $1) $n
          (&i* (&delta $i $k)
               (ap (app $f $T) $alpha_i))))
    "分别令" (&= $k (&cm $1 $..h $n)) ", 可得"
    (MB (&cm (&= (ap (app $f $T) $alpha_k) $0)
             (&<= $1 $k $n)))
    "既然" (setE $alpha_1 $..h $alpha_n) "是" $V "的一个基, 而" (app $f $T)
    "在基的每个向量上都为零, 那么" (app $f $T) "本身肯定是一个零变换.")
   ((theorem)
    "对于域" $F "的一个首项系数为一的多项式" $f ", 设其素因子分解为"
    (&= $f (&..i* $p_1 $p_k)) ", 那么" (&..cm $p_1 $p_k) "互异当且仅当"
    $f "和" $f^ "互素.")
   ((proof)
    "设" (&..cm $p_1 $p_k) "互异. 若" $f "和" $f^ "不互素, 存在" $i
    "使得" $p_i "整除" $f "和" $f^ ". 令" (&= $f_j (&/ $f $p_j)) ", 那么"
    (MB (&= $f^ (LC0 (_^ $p $1 $prime) $f_1 (_^ $p $k $prime) $f_k)) ".")
    "对于" (&!= $j $i) ", 我们知道" $p_i "整除" $f_j ". 又因为" $p_i
    "整除" $f^ ", 所以" $p_i "整除" (&i* (_^ $p $i $prime) $f_i)
    ", 这等价于" $p_i "整除" (_^ $p $i $prime) "或" $f_i
    ". 但是, " $p_i "不可能整除" (_^ $p $i $prime) ", 鉴于"
    (_^ $p $i $prime) "的次数小于" $p_i "的次数. 而且, " $p_i
    "也不可能整除" $f_i ", 鉴于" (&..cm $p_1 $p_k)
    "是互异的. 这就推导出了一个矛盾, 于是" $f "和" $f^ "必然是互素的." (Br)
    "反过来, 设" $f "和" $f^ "互素. 若" $f "的素因子分解中出现重复的因子"
    $p ", 那么存在多项式" $h "使得" (&= $f (&i* $p^2 $h)) ", 于是"
    (MB (&= $f^ (&+ (&i* $p^2 $h^) (&i* $2 $p $p^ $h))
            (&i* $p (@+ (&i* $p $h^) (&i* $2 $p^ $h)))) ".")
    "因此, " $p "也整除" $f^ ", 但这与" $f "和" $f^ "矛盾. 换言之, "
    (&..cm $p_1 $p_k) "互异." (Br)
    "证明的最后, 我们想要澄清一下" (&= $f $1) "的极端情况. 此时, " $f
    "的素因子分解应该理解为&quot;空积&quot;, 因而互异的条件得到满足. 鉴于"
    (&= (&prime $1) $0) ", " (&= (&gcd $1 $0) $1) ", " $f "和" $f^
    "也是互素的. 我们看到, 即便是" (&= $f $1) ", 定理也是成立的.")
   (H1 "分析")
   ((definition) $RR "是具有最小上界性的有序域.")
   ((definition)
    "对于域" $F "和其上的一个全序关系" i.<= ", 如果"
    (Ul (Li "对于所有的" (&in (&cm $x $y $z) $F) ", "
            (&<= $x $y) "可以推出" (&<= (&+ $x $z) (&+ $y $z)) ";")
        (Li "对于所有的" (&in (&cm $x $y) $F) ", "
            (&<= $0 $x) "和" (&<= $0 $y) "可以推出"
            (&<= $0 (&i* $x $y)) "."))
    "那么称带有这个序关系的域是一个有序域.")
   ((theorem) "对于有序域" $F ", " (&< $0 $1) ".")
   ((theorem) "不能为" $CC "赋一个序关系使其成为有序域.")
   ((theorem)
    "对于" (&in (&cm $x $y) $RR) ", 如果" (&> $x $0)
    ", 那么总存在" (&in $n $NN) "满足" (&> (&d* $n $x) $y)
    ". 此即所谓的Archimedes公理.")
   ((proof)
    "对于集合" (&= $X (setI (&d* $n $x) (&in $n $NN))) ", 若对于每个" (&in $n $NN)
    "有" (&<= (&d* $n $x) $y) ", 那么" $y "就是集合" $X "的上界. 显然" $X
    "是非空的, 所以" $X "有最小上界" $y_0 ". 对于每个" (&in $n $NN)
    ", 我们知道" (&in (@ (&+ $n $1)) $NN) ", 于是"
    (&<= (&d* (@ (&+ $n $1)) $x) $y_0) ", 即" (&<= (&d* $n $x) (&- $y_0 $x))
    ". 那么, " (&- $y_0 $x) "也是一个上界, 这与" $y_0 "是最小上界矛盾.")
   ((theorem) "嵌套区间公理.")
   ((definition)
    "给定集合" $X ", 称" (func $d (&c* $X $X) $RR) "是" $X "上的度量, 如果"
    (Ul (Li "对于任意的" (&in (&cm $x $y) $X) ", " (&>= (&d $x $y) $0) ";")
        (Li "对于任意的" (&in (&cm $x $y) $X) ", "
            (&<=> (&= (&d $x $y) $0) (&= $x $y)) ";")
        (Li "对于任意的" (&in (&cm $x $y) $X) ", "
            (&= (&d $x $y) (&d $y $x)) ";")
        (Li "对于任意的" (&in (&cm $x $y $z) $X) ", "
            (&>= (&+ (&d $x $y) (&d $y $z)) (&d $x $z)) "."))
    "如果" $d "是" $X "上的度量, 那么就称序对" (tu0 $X $d)
    "是一个度量空间. " $X "的子集继承了自然的度量结构而成为子空间. "
    "在度量可以从上下文中推断出来的时候, 我们也说度量空间" $X "而不是"
    (tu0 $X $d) ".")
   ((theorem)
    "给定度量空间" (tu0 $X $d) ", 对于任意的" (&in (&cm $x_1 $..h $x_n) $X)
    ", " (&>= (&+ (&d $x_1 $x_2) $..c (&d (_ $x (&- $n $1)) $x_n)) (&d $x_1 $x_n)) ".")
   ((proof) "根据归纳即得.")
   ((theorem)
    "给定度量空间" (tu0 $X $d) ", 对于任意的" (&in (&cm $x $y $z) $X)
    ", " (&>= (&d $x $z) (&abs (&- (&d $x $y) (&d $y $z)))) ".")
   
   ((definition)
    "给定度量空间" (tu0 $X $d) ", 对于" (&in $x $X) "和" (&> $r $0)
    ", 定义开球" (&:= (appl $BB $x $r) (setI (&in $y $X) (&< (&d $x $y) $r)))
    ", 闭球" (&:= (appl (OverBar $BB) $x $r) (setI (&in $y $X) (&<= (&d $x $y) $r)))
    ".")
   ((definition)
    "给定度量空间" (tu0 $X $d) ", 对于" (&in $x $X)
    ", 称" (&sube $U $X) "是" $x "的邻域, 如果存在" (&> $r $0)
    "满足" (&sube (appl $BB $x $r) $U) ".")
   ((definition)
    "给定集合" $X ", " $X "中的一个序列是一个类型为" (&-> $NN $X) "的映射. 对于序列"
    (func $x $NN $X) ", 对于" (&in $n $NN) ", 我们也将" (app $x $n)
    "记作" $x_n ".")
   ((definition)
    "给定度量空间" (tu0 $X $d) ", 对于" $X "中序列" (func $x $NN $X)
    ", 如果存在" (&in $y $X) "满足对于每个" (&> $epsilon $0)
    ", 存在" (&in $N $NN) ", 对于每个自然数" (&>= $n $N)
    "有" (&< (&d $x_n $y) $epsilon) ", 那么就称" $x
    "在" $X "中收敛, 并以" $y "为极限, 记作" (&= (lim $n $inf $x_n) $y)
    ". 换言之, 序列" (func $x $NN $X) "收敛于" $y "当且仅当以" $y
    "为中心的每个开球都包含序列的几乎所有项.")
   ((definition)
    "给定度量空间" (tu0 $X $d) ", 对于" $X "中序列" (func $x $NN $X)
    ", 称其为Cauchy序列, 如果对于每个" (&> $epsilon $0)
    ", 存在" (&in $N $NN) ", 对于任意的" (&>= (&cm $m $n) $N)
    ", " (&< (&d $x_m $x_n) $epsilon) ".")
   ((theorem)
    "给定度量空间" (tu0 $X $d) ", 对于" $X "中序列" (func $x $NN $X)
    ", 如果它收敛, 那么它是Cauchy序列.")
   ((definition)
    "对于序列" (func $x $NN $X) "和严格单调递增映射" (func $phi $NN $NN)
    ", 序列" (func (&compose $x $phi) $NN $X)
    "被称为序列" (func $x $NN $X) "的子序列.")
   ((theorem)
    "给定度量空间" (tu0 $X $d) ", 对于序列" (func $x $NN $X)
    ", 如果" (&= (lim $n $inf $x_n) $y) ", 那么对于每个严格单调递增映射"
    (func $phi $NN $NN) "有"
    (&= (lim $k $inf (app (@ (&compose $x $phi)) $k)) $y) ".")
   ((proof)
    "注意到对于每个" (&in $k $NN) ", " (&>= (app $phi $k) $k) ".")
   ((definition)
    "对于序列" (func $x $NN $RR) ", 我们将其称为实序列. 对于序列"
    (func $x $NN $CC) ", 我们将其称为复序列.")
   ((theorem)
    "对于单调递增的实序列" (func $x $NN $RR)
    ", 如果其像" (&img $x) "有界, 那么该序列收敛, 并以"
    (&sup0 (&img $x)) "为极限.")
   ((theorem)
    "对于实序列" (func (&cm $x $y) $NN $RR)
    ", 如果" (&= (lim $n $inf $x_n) $a) "且"
    (&= (lim $n $inf $y_n) $b) ", 那么序列"
    (func (&+ $x $y) $NN $RR) "收敛, 并且"
    (&= (lim $n $inf (app (@+ $x $y) $n)) (&+ $a $b)) ".")
   ((theorem)
    "对于实序列" (func $x $NN $RR) ", 如果"
    (&= (lim $n $inf $x_n) $a) ", 那么对于每个" (&in $k $RR)
    ", 序列" (func (&i* $k $x) $NN $RR) "收敛, 并且"
    (&= (lim $n $inf (app (@i* $k $x) $n)) (&i* $k $a)) ".")
   ((theorem)
    "对于实序列" (func (&cm $x $y) $NN $RR)
    ", 如果" (&= (lim $n $inf $x_n) $a) "且"
    (&= (lim $n $inf $y_n) $b) ", 那么序列"
    (func (&i* $x $y) $NN $RR) "收敛, 并且"
    (&= (lim $n $inf (app (@i* $x $y) $n)) (&i* $a $b)) ".")
   ((definition)
    "对于实序列" (func $x $NN $RR)
    ", 定义序列如下"
    (MB (&: $S (&cm (&-> $NN $RR)
                    (&\|-> $n (Choice0
                               ($x_0 $cm (&= $n $0))
                               ((&+ (app $S (&- $n $1)) $x_n)
                                $cm (&>= $n $1)))))) ".")
    "序列" $S "被称为序列" $x "的部分和. 如果" $S
    "收敛, 那么我们就称级数" (sum (&= $n $0) $inf $x_n)
    "收敛. 如果" (&= (lim $n $inf $S_n) $s)
    ", 那么记" (&= (sum (&= $n $0) $inf $x_n) $s) ".")
   ((theorem)
    "对于实序列" (func (&cm $x $y) $NN $RR)
    ", 如果对于每个" (&in $n $NN) "有"
    (&<= $0 $x_n $y_n) "并且级数" (sum (&= $n $0) $inf $y_n)
    "收敛, 那么" (sum (&= $n $0) $inf $x_n) "亦收敛, 并有"
    (MB (&<= (sum (&= $n $0) $inf $x_n)
             (sum (&= $n $0) $inf $y_n)) "."))
   ((proof)
    "级数" (sum (&= $n $0) $inf $y_n) "收敛的话, 说明其部分和有上界, "
    "并且当然收敛至最小上界. 可以看出, 对于级数" (sum (&= $n $0) $inf $x_n)
    "而言, 有"
    (MB (&cm (&<= (sum (&= $n $0) $k $x_n)
                  (sum (&= $n $0) $k $y_n)
                  (sum (&= $n $0) $inf $y_n))
             (&in $k $NN)) ".")
    "于是, 其部分和亦有上界, 且以" (sum (&= $n $0) $inf $y_n)
    "为一个上界. 而" (sum (&= $n $0) $inf $x_n)
    "又是最小上界, 故"
    (&<= (sum (&= $n $0) $inf $x_n)
         (sum (&= $n $0) $inf $y_n)) ".")
   ((definition)
    "指数函数定义如下:"
    (MB (&: $exp (&cm (&-> $RR $RR)
                      (&\|-> $x (sum (&= $n $0) $inf
                                     (~ $x^n (&fact $n)))))) "."))
   ((definition)
    "给定度量空间" (tu0 $X $d_X) "和" (tu0 $Y $d_Y)
    ", 对于函数" (func $f $X $Y) ", 称" $f "于点" (&in $a $X)
    "连续, 如果对于每个以" (app $f $a) "为中心的开球" $B_Y
    "存在以" $a "为中心的开球" $B_X "满足"
    (&sube (app $f $B_X) $B_Y) ". 等价地, 把开球替换成开集或者邻域也可以.")
   
   (H1 "良基关系")
   ((definition)
    "一个偏序集被称为良基 (well-founded) 的, 如果其每个非空子集都有极小元. "
    "对于良基的偏序集, 我们可以施行良基归纳. 对于良基的偏序集" (tu0 $X $<=:id)
    "和性质" $P ", 如果对于每个" (∈ $x $X) ", 我们可以根据"
    (∀ (∈ $y $X) (&=> (&< $y $x) (app $P $y))) "推出"
    (app $P $x) ", 那么性质" $P "对于每个" (∈ $x $X)
    "均成立. 一种证明良基归纳正确性的方式是反证法. "
    "对于满足良基归纳条件的性质" $P ", 假设" $X
    "中存在不满足" $P "的元素, 那么这些元素构成了" $X
    "的一个非空子集, 即" (setI (∈ $x $X) (&neg (app $P $x)))
    ". 根据良基的定义, 这个集合具有一个极小元. "
    "对于所有小于这个极小元的元素, 我们知道其必然满足性质"
    $P ", 因为这是极小的定义. 根据良基归纳的条件, "
    "我们可以推出这个极小元也必然满足性质" $P
    ", 那么这就导出了一个矛盾.")
   (P "接下来抄一抄nlab, 这和传统意义上的良基关系还不太一样.")
   (H2 "1. 想法")
   (P "一个集合" $S "上的一个二元关系" $pr "是良基的, "
      "如果在" $S "上我们可以对于" $pr "做归纳.")
   (H2 "2. 定义")
   
   (H1 "未归类")
   ((theorem)
    "给定集合" $A "和" $B "以及函数" (func $f $A $B) ", 那么"
    (Ol (Li $f "左可逆当且仅当" $f "是单射且" $A "为空可以推出" $B "为空. "
            "(许多书籍的表述并不正确)")
        (Li $f "右可逆当且仅当" $f "是满射. (这条需要选择公理)")))
   ((theorem)
    "在选择公理下, 升链条件等价于极大条件, 降链条件等价于极小条件 (良基关系).")
   ((theorem)
    "对于函数" (func $f $X $Y) "和" $Y "中的集族"
    (_@ $A_i (∈ $i $I)) ", 我们有"
    (MB (&= (ap (inv $f) (pare (Union (∈ $i $I) $A_i)))
            (Union (∈ $i $I) (app (inv $f) $A_i))) "."))
   ((proof)
    "对于" (∈ $x $X) ", 我们知道"
    (MB (∈ $x (ap (inv $f) (pare (Union (∈ $i $I) $A_i)))))
    "等价于"
    (MB (∈ (app $f $x) (Union (∈ $i $I) $A_i)))
    "等价于"
    (MB (∃ (∈ $i $I) (∈ (app $f $x) $A_i)))
    "等价于"
    (MB (∃ (∈ $i $I) (∈ $x (app (inv $f) $A_i))))
    "等价于"
    (MB (∈ $x (Union (∈ $i $I) (app (inv $f) $A_i)))))
   ((proposition)
    "对于实数" (&> $a $0) ", 定义序列"
    (MB (&\; (&= $x_0 $1)
             (&= (_ $x (&+ $n $1))
                 (~ (&+ $x_n (&/ $a $x_n)) $2))))
    "那么"
    (MB (&= (lim $n $inf $x_n) (Msqrt $a)) "."))
   ((proof)
    "首先, 易知对于" (∈ $n $NN) ", " (&> $x_n $0)
    ". 那么, 根据基本不等式, 我们有对于" (∈ $n $NN)
    ", " (&>= (_ $x (&+ $n $1)) (Msqrt $a))
    ". 对于" (&>= $n $1) ", 现在"
    (MB (deriv0
         (&- (_ $x (&+ $n $1)) $x_n)
         $=
         (&- (~ (&+ $x_n (&/ $a $x_n)) $2) $x_n)
         $=
         (~ (&- $a (_^ $x $n $2))
            (&i* $2 $x_n))
         $<= $0))
    "换言之, 从第" $1 "项起, 序列单调递减. "
    "既然序列单调递减有下界, 那么其就收敛于它的最大下界. "
    "设这个序列收敛于" $y ", 那么"
    (MB (&= (lim $n $inf $x_n)
            $y
            (lim $n $inf (_ $x (&+ $n $1)))
            (~ (&+ $y (&/ $a $y)) $2)))
    "由此可知" (&= $y^2 $a)
    ". 另外, 我们知道" (&>= $y (Msqrt $a))
    ", 故" (&= $y (Msqrt $a)) ".")
   (H1 "吉米多维奇习题集")
   (P "完全出于玩票的心态写一写, 鉴于对本科时期的怀念. "
      "当时发现书上有很多错误, 这使我对于分析这一学科缺乏信心.")
   ((exercise #:n "1")
    (MB (&= (&+ $1 $2 $..c $n) (~ (&i* $n (@+ $n $1)) $2)) "."))
   ((proof)
    (Ul (Li "对于正整数" $1 ", 命题平凡地成立, 即"
            (MB (&= $1 (~ (&i* $1 (@+ $1 $1)) $2)) "."))
        (Li "如果命题对于正整数" $n "成立, 那么对于"
            (&+ $n $1) ", 我们有"
            (MB (deriv
                 (&+ $1 $2 $..c $n (@+ $n $1))
                 (&+ (~ (&i* $n (@+ $n $1)) $2)
                     (@+ $n $1))
                 (&+ (~ (&i* $n (@+ $n $1)) $2)
                     (~ (&i* $2 (@+ $n $1)) $2))
                 (~ (&i* (@+ $n $2) (@+ $n $1)) $2)
                 (~ (&i* (@+ $n $1)
                         (@+ (@+ $n $1) $1))
                    $2)))
            "换言之, 命题对于" (&+ $n $1) "也成立."))
    "命题的成立可由归纳得到.")
   ((exercise #:n "2")
    (MB (&= (&+ $1^2 $2^2 $..c $n^2)
            (~ (&i* $n (@+ $n $1) (@+ (&i* $2 $n) $1)) $6)) "."))
   ((proof)
    (Ul (Li "对于正整数" $1 ", 命题平凡地成立, 即"
            (MB (&= $1^2
                    (~ (&i* $1 (@+ $1 $1) (@+ (&d* $2 $1) $1)) $6)) "."))
        (Li "如果命题对于正整数" $n "成立, 那么对于"
            (&+ $n $1) ", 我们有"
            (MB (deriv
                 (&+ $1^2 $2^2 $..c $n^2 (^ (@+ $n $1) $2))
                 (&+ (~ (&i* $n (@+ $n $1) (@+ (&i* $2 $n) $1)) $6)
                     (^ (@+ $n $1) $2))
                 (&+ (~ (&i* (@+ $n $1) (@+ (&i* $2 $n^2) $n)) $6)
                     (~ (&i* (@+ $n $1) (@+ (&i* $6 $n) $6)) $6))
                 (~ (&i* (@+ $n $1)
                         (@+ (&i* $2 $n^2)
                             (&i* $7 $n) $6))
                    $6)
                 (~ (&i* (@+ $n $1)
                         (@+ $n $2)
                         (@+ (&i* $2 $n) $3))
                    $6)
                 (~ (&i* (@+ $n $1)
                         (@+ (@+ $n $1) $1)
                         (@+ (&i* $2 (@+ $n $1)) $1))
                    $6)))
            "换言之, 命题对于" (&+ $n $1) "也成立."))
    "命题的成立可由归纳得到.")
   ((exercise #:n "3")
    (MB (&= (&+ $1^3 $2^3 $..c $n^3)
            (^ (@+ $1 $2 $..c $n) $2)) "."))
   ((proof)
    (Ul (Li "对于正整数" $1 ", 命题平凡地成立, 即"
            (MB (&= $1^3 $1^2) "."))
        (Li "如果命题对于正整数" $n "成立, 那么对于"
            (&+ $n $1) ", 我们有"
            (MB (deriv
                 (&+ $1^3 $2^3 $..c $n^3
                     (^ (@+ $n $1) $3))
                 (&+ (^ (@+ $1 $2 $..c $n) $2)
                     (^ (@+ $n $1) $3))
                 (&+ (~ (&i* (^ (@+ $n $1) $2) $n^2) $4)
                     (^ (@+ $n $1) $3))
                 (&+ (~ (&i* (^ (@+ $n $1) $2) $n^2) $4)
                     (~ (&i* (^ (@+ $n $1) $2)
                             (@+ (&i* $4 $n) $4))
                        $4))
                 (~ (&i* (^ (@+ $n $1) $2)
                         (^ (@+ $n $2) $2))
                    $4)
                 (^ (brac (~ (&i* (@+ $n $1) (@+ (@+ $n $1) $1)) $2)) $2)
                 (^ (@+ $1 $2 $..c $n (@+ $n $1)) $2)))
            "换言之, 命题对于" (&+ $n $1) "也成立."))
    "命题的成立可由归纳得到.")
   
   ((exercise #:n "5")
    )
   ((proof)
    "这题还挺有趣的, 因为我每次看到都要想一想, "
    "但是只要动笔就会发现很简单."
    )
   (H1 "读书的地图")
   (P "大致上记录了我读书的痕迹, 然而不甚完整. 仅是为了备忘.")
   (Ol (Li "分析学"
           (Ol (Li "Principles of Mathematical Analysis (Baby Rudin)")
               (Li "微积分学教程 (菲赫金哥尔茨)")
               (Li "数学分析习题集 (吉米多维奇)")
               (Li "Amann &amp; Escher")
               (Li "Godement")
               (Li "An Introduction to Manifolds")
               (Li "Stein")
               (Li "Exercises in Analysis (Part I)")
               (Li "Treatise on Analysis (Dieudonné)")
               (Li "无穷小计算 (Dieudonné)")
               )
           )
       (Li "代数学"
           (Ol (Li "Hoffman &amp; Kunze")
               (Li "代数学方法 (李文威)")
               (Li "Algebra: Chapter 0")
               (Li "Algebra (Bourbaki)")
               (Li "Godement")
               (Li "Introduction to Commutative Algebra")
               )
           )
       (Li "逻辑学"
           (Ol (Li "集合论导引 (冯琦)")
               (Li "Set Theory (Kunen)")
               (Li "Beginning Mathematical Logic (Peter Smith)")
               (Li "Gödel Without (Too Many) Tears")
               (Li "Structural Proof Theory")
               (Li "An Introduction to Formal Logic (Peter Smith)")
               (Li "An Introduction to Inductive Definitions (Peter Aczel)")
               )
           )
       (Li "范畴论"
           (Ol (Li "Category Theory (Steve Awodey)")
               (Li "Category Theory in Context")
               (Li "Abstract and Concrete Categories")
               (Li "Topoi: The Categorial Analysis of Logic")
               )
           )
       (Li "编程语言"
           (Ol (Li "程序设计"
                   (Ol (Li "SICP")
                       (Li "HtDP")
                       (Li "Concepts, Techniques, and Models of Computer Programming "
                           "(我挺讨厌这本书的)")
                       (Li "ML for the Working Programmer")
                       (Li "Programming in Standard ML")
                       (Li "CMU 15-122 (Principles of Imperative Computation)")
                       )
                   )
               (Li "一般性入门导引"
                   (Ol (Li "Essentials of Programming Languages")
                       (Li "Programming Languages: Application and Interpretation")
                       (Li "Lisp in Small Pieces")
                       )
                   )
               (Li "一般性严肃导引"
                   (Ol (Li "TaPL")
                       (Li "PFPL")
                       (Li "Software Foundations")
                       (Li "PLFA")
                       (Li "The Formal Semantics of Programming Languages")
                       (Li "Semantics Engineering with PLT Redex "
                           "(讲义形式为Programming Languages and Lambda Calculi)")
                       (Li "Domain-Theoretic Foundations of Functional Programming")
                       )
                   )
               (Li "编译器构造"
                   (Ol (Li "Three Implementation Models for Scheme")
                       (Li "Compiling with Continuations")
                       (Li "P423 Assignments")
                       (Li "Essentials of Compilation")
                       )
                   )
               (Li "证明助手"
                   (Ol (Li "The Little Typer")
                       (Li "Proofs and Types")
                       (Li "Lectures on the Curry-Howard Isomorphism")
                       (Li "HoTT")
                       (Li "The Little Prover")
                       )
                   )
               (Li "部分求值"
                   (Ol (Li "Partial Evaluation and Automatic Program Generation")
                       )
                   )
               (Li "抽象解释"
                   )
               (Li "计算理论"
                   (Ol (Li "Computability and Complexity (Neil Jones)"
                           )
                       )
                   )
               (Li "(自然)语言学"
                   (Ol (Li "Continuations and Natural Language"
                           )
                       )
                   )
               (Li "形式语言学(和句法分析)"
                   (Ol (Li "Parsing with Derivatives")
                       (Li "Parsing with Zippers")
                       )
                   )
               )
           )
       (Li "计算机硬件"
           (Ol (Li "Computation Structures")
               (Li "Foundations of Analog and Digital Electronic Circuits")
               )
           )
       (Li "物理学"
           (Ol (Li "力学概论 (方励之)")
               (Li "Structure and Interpretation of Classical Mechanics")
               )
           )
       )
   ))