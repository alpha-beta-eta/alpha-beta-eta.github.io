#lang racket
(provide calculus_dieudonne.html)
(require SMathML)
(define (dis a b)
  (&abs (&- a b)))
(define calculus_dieudonne.html
  (TmPrelude
   #:title "无穷小演算"
   #:css "styles.css"
   (H1 "无穷小演算")
   (P "无穷小演算是微分和积分演算的另一个名字, 不过说到底, 我是怎么也学不会微积分.")
   (H2 "第一章 求上界, 求下界")
   
   (H2 "第二章 方程的根的逼近")
   (H3 "1. 问题的地位")
   (H3 "2. 试位法")
   (P "本节假定函数" $f "在某个区间" (li0 $a $b) "上定义, 具有连续的二阶导数, "
      $f^ "在该区间上恒不为零 (由于连续性, 其符号不变), 并且"
      (&< (&i* (app $f $a) (app $f $b)) $0) ". 设" $xi_0 "是" $f
      "在区间上唯一的那个根, 而" $xi "是将函数的两端连起来的直线 (也就是一次插值) "
      "与横轴的交点. 若将" $xi "当作" $xi_0 "的近似值, 那么我们的问题是考虑误差"
      (&abs (&- $xi $xi_0)) "的上界.")
   ((lemma)
    "设" $J "是" $RR "的一个区间, " $f "是" $J "上两次连续可导 "
    "[注记: 其实两次连续可导的意思就是连续可导且导函数连续可导] 的函数, "
    (&cm $x_0 $x_1) "是" $J "上不同的两点, " $L "是在点" $x_0 "和" $x_1
    "处取与" $f "相同的值的一次多项式:"
    (MB (&= (app $L $z)
            (~ (&- (&i* (@- $z $x_0) (app $f $x_1))
                   (&i* (@- $z $x_1) (app $f $x_0)))
               (&- $x_1 $x_0))) ";")
    "那么在含有" (&cm $x $x_0 $x_1) "的最小区间上, 存在(与" $x
    "有关的)一个点" $zeta "满足"
    (MB (&= (&- (app $f $x) (app $L $x))
            (&i* 1/2 (app (&Prime $f) $zeta)
                 (@- $x $x_0) (@- $x $x_1))) "."))
   ((proof)
    "显然仅考虑" (&!= $x $x_0) "且" (&!= $x $x_1) "的情况即可, 设" $J "上函数"
    (MB (&= (app $u $z)
            (&- (app $f $z) (app $L $z)
                (&i* $c (@- $z $x_0) (@- $z $x_1)))))
    "其中常数" $c "由" (&= (app $u $x) $0) "确定, 根据假设这是可能的. [注记: 这句话"
    "看上去有些奇怪, 但实际上就是令"
    (MB (&= $c (~ (&- (app $f $x) (app $L $x))
                  (&i* (@- $x $x_0) (@- $x $x_1)))))
    "于是" (&= (app $u $x) $0) ", 并不存在循环.] 因此, 我们有"
    (&= (app $u $x) (app $u $x_0) (app $u $x_1) $0) ". 由Rolle定理, 在包含"
    (&cm $x $x_0 $x_1) "的最小区间上, 存在不同的两点" (&cm $y_1 $y_2)
    "使得" (&= (app $u^ $y_1) (app $u^ $y_2) $0) ". 对于" $u^
    "再次应用Rolle定理, 在以" (&cm $y_1 $y_2) "为端点的区间上存在" $zeta "满足"
    (&= (app (&Prime $u) $zeta) $0) ". 可是"
    (&= (app (&Prime $u) $z) (&- (app (&Prime $f) $z) (&i* $2 $c)))
    ", 从而" (&= $c (&i* 1/2 (app (&Prime $f) $zeta))) ", 即"
    (MB (&= (&- (app $f $x) (app $L $x))
            (&i* 1/2 (app (&Prime $f) $zeta)
                 (@- $x $x_0) (@- $x $x_1))) "."))
   ((corollary)
    "如果" $f^ "在" $J "上恒不为零, 且" (∈ $xi $xi_0 $J) "满足"
    (&cm (&= (app $f $xi_0) $0) (&= (app $L $xi) $0)) ", 那么存在"
    (∈ $zeta $zeta^ $J) "满足"
    (MB (&= (&- $xi $xi_0)
            (&i* 1/2
                 (~ (app (&Prime $f) $zeta)
                    (app $f^ $zeta^))
                 (@- $xi $x_0) (@- $xi $x_1))) ".")
    "如果对于" (∈ $x $J) "有" (&> (&>= (&abs (app $f^ $x)) $m) $0)
    "且" (&<= (app (&Prime $f) $x) $M) ", 那么"
    (MB (&<= (&abs (&- $xi $xi_0))
             (&i* (~ $M (&i* $2 $m))
                  (&abs (&- $xi $x_0)) (&abs (&- $xi $x_1)))) "."))
   ((proof)
    "根据引理, 我们有"
    (MB (&= (app $f $xi)
            (&i* 1/2 (app (&Prime $f) $zeta)
                 (@- $xi $x_0) (@- $xi $x_1))))
    "另一方面, 根据有限增量公式, 存在以" $xi_0 "和" $xi
    "为端点的区间上的一个点" $zeta^ "满足"
    (MB (&= (app $f $xi) (&i* (@- $xi $xi_0) (app $f^ $zeta^))) ".")
    "[注记: " (&= $xi $xi_0) "的退化情况下任取即可.] 由此即得推论.")
   ((remark)
    "若此式给出的误差还不足够小, 那么可以重复进行此步骤. 计算" (app $f $xi)
    "的符号, 由此判断根" $xi_0 "是在区间" (li0 $a $xi) "还是" (li0 $xi $b)
    "中, 然后可以得到第二近似值" $xi^ ". 理论上, 这种方法可以应用无限次, "
    "也可证明得到的数列收敛于" $xi_0 ".")
   (P "注记: 看起来好像根据推论, 不选择根所在的区间也可以缩小误差, 但是问题在于"
      "此时我们没法控制一次插值多项式的根仍然还在区间上, 所以不行. 另外, 本节"
      "要求二阶导数连续是为了保证" (&abs (app (&Prime $f) $x))
      "在闭区间上有最大值, 可以控制误差.")
   ((exercise #:n "9")
    "设在" $RR "中的区间" (&= $J (li0 $a $b)) "上, 二次连续可导函数" $f
    "满足" (&> (&>= (&abs (app $f^ $x)) $m) $0)
    "且" (&<= (app (&Prime $f) $x) $M) ", 且" (app $f $a) "和" (app $f $b)
    "异号, 证明若" (&< (&= (&i* (~ $M (&i* $4 $m)) (@- $b $a)) $q) $1)
    ", 便可逐步应用试位法" $n "次, 在区间" (li0 $a $b) "上找到端点是"
    (&cm $a_n $b_n) "的一个区间, 其包含" (&= (app $f $x) $0) "的唯一一根, 并且"
    (MB (&<= (&abs (&- $b_n $a_n))
             (&i* (~ (&i* $4 $m) $M) (^ $q $2^n))) "."))
   ((proof)
    "实在是不知道怎么做. " (&= (dis $b_0 $a_0) (dis $b $a)) ". 设"
    (&= $t_n (dis $b_n $a_n)) ". 根据答案, 如果能够控制"
    (MB (&<= (_ $t (&+ $n $1))
             (&i* $2 (dis (_ $xi (&+ $n $1)) $xi_0))
             (&i* (~ $M $m) (dis (_ $xi (&+ $n $1)) $a_n)
                  (dis (_ $xi (&+ $n $1)) $b_n))))
    "其中" $xi_0 "表示根的精确值, 而" $xi_n "在" (&>= $n $1)
    "时表示试位法的第" $n "个猜测值, 那么"
    (MB (&<= (_ $t (&+ $n $1))
             (&i* (~ $M $m) (dis (_ $xi (&+ $n $1)) $a_n)
                  (dis (_ $xi (&+ $n $1)) $b_n))
             (&= (&i* (~ $M $m) (^ (pare (~ (dis $b_n $a_n) $2)) $2))
                 (&i* (~ $M (&i* $4 $m)) (_^ $t $n $2)))))
    "这实际上就可以和要证明的结论合上了. 设" (&= $s_0 $t_0) ", 而"
    (&= (_ $s (&+ $n $1)) (&i* (~ $M (&i* $4 $m)) (_^ $s $n $2)))
    ", 那么" (&<= $t_n $s_n) ". 现在考虑求" $s_n "的通项, 这是很简单的. 设"
    (MB (&= $r_n (Mroot $s_n $2^n)))
    "[注记: 这破浏览器MathML渲染根式怎么都有问题.] 那么"
    (MB (&= (_^ $r (&+ $n $1) (^ $2 (&+ $n $1)))
            (&i* (~ $M (&i* $4 $m))
                 (^ (pare (_^ $r $n $2^n)) $2))
            (&i* (~ $M (&i* $4 $m))
                 (_^ $r $n (^ $2 (&+ $n $1))))))
    "于是"
    (MB (&= (~ (_ $r (&+ $n $1)) $r_n)
            (Mroot (~ $M (&i* $4 $m)) (^ $2 (&+ $n $1)))))
    "对于" (&>= $n $1) ", 我们有"
    (MB
     (deriv
      $r_n (&i* (~ $r_n (_ $r (&- $n $1))) $..c (~ $r_1 $r_0) $r_0)
      (&i* (@- $b $a)
           (prod (&= $k $1) $n
                 (^ (pare (~ $M (&i* $4 $m)))
                    (~ $1 $2^k))))
      (&i* (@- $b $a)
           (^ (pare (~ $M (&i* $4 $m)))
              (&- $1 (~ $1 $2^n))))))
    "我们发现" $r_0 "也满足此公式. 那么, 我们有"
    (MB (deriv $s_n (_^ $r $n $2^n)
               (&i* (^ (@- $b $a) $2^n)
                    (^ (pare (~ $M (&i* $4 $m)))
                       (&- $2^n $1)))
               (&i* (~ (&i* $4 $m) $M)
                    (^ (brac (&i* (~ $M (&i* $4 $m)) (@- $b $a)))
                       $2^n))
               (&i* (~ (&i* $4 $m) $M) (^ $q $2^n))))
    
    )
   (H3 "3. 用迭代法解" (&= $x (app $g $x)))
   (P "前一节我们有关步骤的出发点是有限增量公式, 即如果" (app $f $xi)
      "是小的, 而" $f^ "却不太小, 那么误差" (&abs (&- $xi $xi_0))
      "就是小的. 在下面, 我们要明确这种模糊的想法. 首先是迭代法.")
   (P "令" (&= (app $g $x) (&- $x (app $f $x))) ", 那么方程" (&= (app $f $x) $0)
      "就等价于方程"
      (MB (&= $x (app $g $x)) ".")
      "换言之, 就是寻找" $g "的不动点. 我们有以下结果:")
   
   ))