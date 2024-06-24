#lang racket
(provide div:hoffman_ch8)
(require SMathML "linear_algebra_utils.rkt")
(define div:hoffman_ch8
  (TmDiv
   (H2 "第8章 内积空间")
   (H3 "第8.1节 内积")
   (P "整章我们只考虑实或复向量空间, 即实数域或复数域上的向量空间. "
      "我们的主要目的在于研究可以讨论向量长度和夹角的向量空间. "
      "我们将研究一类特定的标量值函数, 其定义于向量的序对之上, "
      "被称为内积. 内积的一个例子是" $RR^3 "中的标量积或者说点积. "
      $RR^3 "中的向量"
      (MB (&= $alpha (tu0 $x_1 $x_2 $x_3)) "和"
          (&= $beta (tu0 $y_1 $y_2 $y_3)))
      "的标量积是实数"
      (MB (&= (inner* $alpha $beta)
              (&+ (&i* $x_1 $y_1)
                  (&i* $x_2 $y_2)
                  (&i* $x_3 $y_3))) ".")
      "从几何上说, 这个点积是" $alpha "的长度, " $beta "的长度, 以及"
      $alpha "和" $beta "的夹角的余弦之积. 因此, 藉由代数地定义的"
      "标量积来定义" $RR^3 "中的长度和夹角这样的几何概念完全是可能的.")
   (P "向量空间上的内积是性质与" $RR^3 "中的点积类似的函数, 而"
      "基于这样的内积我们又可以定义长度和角度. 我们关于角度的"
      "一般概念的注记将仅限于向量的垂直性 (或者说正交性). 第一节我们"
      "将定义何谓内积, 考虑一些实际的例子, 并建立内积的一些基本性质. "
      "之后, 我们将回到讨论长度和正交性的任务上来.")
   ((definition)
    "令" $F "是实数域或复数域, " $V "是域" $F "上的一个向量空间. "
    $V "上的一个内积是一个函数"
    (&cm (&-> (&c* $V $V) $F)
         (&\|-> (tu0 $alpha $beta)
                (inner* $alpha $beta)))
    "满足对于任意的" (∈ $alpha $beta $gamma $V)
    "和任意的标量" (∈ $c $F) "有"
    (Ol #:attr* '((type "a"))
        (Li (linear+ (lambda (v) (inner* v $gamma))
                     $alpha $beta) ";")
        (Li (linear* (lambda (v) (inner* v $beta))
                     $c $alpha) ";")
        (Li (csym inner* $beta $alpha)
            ", 一横代表复共轭;")
        (Li "如果" (&!= $alpha $0) ", 那么"
            (&> (inner* $alpha $alpha) $0) ".")))
   (P "读者应该注意到条件a, b, c可以推出条件e:"
      (MB (clinear (lambda (v) (inner* $alpha v))
                   $c $beta $gamma) ".")
      "另一点值得说明的是, 当" $F "是实数域" $RR
      "时, 条件c和e中的复共轭是多余的. 然而, 在复数域的"
      "情况下, 为了条件的一致性, 复共轭则是必要的. "
      "若是没有这些复共轭, 我们就会得到以下矛盾:"
      (MB (&> (inner* $alpha $alpha) $0) "且"
          (&> (&= (inner* (&i* $i $alpha)
                          (&i* $i $alpha))
                  (&i* $-1 (inner* $alpha $alpha)))
              $0) "."))
   (P "在本章的剩余部分里, " $F "要么代表实数域, 要么代表复数域.")
   ((example #:n "1")
    $F^n "上存在一个内积, 我们称之为" (B "标准内积")
    ". 对于向量" (&= $alpha (tu0 $x_1 $..h $x_n)) "和"
    (&= $beta (tu0 $y_1 $..h $y_n)) ", 其标准内积被定义为"
    (MB (&= (inner* $alpha $beta)
            (sum (&= $j $1) $n
                 (&i* $x_j (_ (OverBar $y) $j)))) ".")
    "当" (&= $F $RR) "时, 这也可以记成"
    (MB (&= (inner* $alpha $beta)
            (sum (&= $j $1) $n (&i* $x_j $y_j))) ".")
    "在实数域的情形下, 标准内积常被称为点积或者标量积, 并记为"
    (&d* $alpha $beta) ".")
   ((example #:n "2")
    "对于" $RR^2 "中的向量" (&= $alpha (tu0 $x_1 $x_2))
    "和" (&= $beta (tu0 $y_1 $y_2)) ", 令"
    (MB (&= (inner* $alpha $beta)
            (: (&i* $x_1 $y_1) $-
               (&i* $x_2 $y_1) $-
               (&i* $x_1 $y_2) $+
               (&i* $4 $x_2 $y_2))) ".")
    "既然"
    (&= (inner* $alpha $alpha)
        (&+ (^ (@- $x_1 $x_2) $2)
            (&i* $3 (_^ $x $2 $2))))
    ", 可直接推得" (&!= $alpha $0) "时有"
    (&> (inner* $alpha $alpha) $0)
    ". 内积定义中的条件a, b, c则是容易验证的.")
   ((example #:n "3")
    "令" $V "是" (^ $F n*n) ", 那么" $V "以自然的方式同构于"
    (^ $F $n^2) ", 因而由例子1可知"
    (MB (&= (inner* $A $B)
            (sum (&= $j $1) $n
                 (sum (&= $k $1) $n
                      (&i* (mref $A $j $k)
                           (mref (OverBar $B) $j $k))))))
    "定义了" $V "上的一个内积. 而且, 如果我们引入"
    (B "共轭转置") "矩阵" (&* $B) ", 其由"
    (&= (_^ $B (&cm $k $j) $*)
        (mref (OverBar $B) $j $k))
    "定义, 那么我们可以基于迹函数来表达内积:"
    (MB (&= (inner* $A $B)
            (&tr (&i* $A (&* $B)))
            (&tr (&i* (&* $B) $A))) ".")
    "这是因为"
    (eqnderiv
     (&tr (&i* $A (&* $B)))
     (sum (&= $j $1) $n
          (mref (@i* $A (&* $B)) $j $j))
     (sum (&= $j $1) $n
          (sum (&= $k $1) $n
               (&i* (mref $A $j $k)
                    (_^ $B (&cm $k $j) $*))))
     (sum (&= $j $1) $n
          (sum (&= $k $1) $n
               (&i* (mref $A $j $k)
                    (mref (OverBar $B) $j $k))))))
   ((example #:n "4")
    "令" (∈ $Q (^ $F n*n)) "是一个可逆矩阵, 对于"
    (∈ $X $Y (^ $F n*1)) ", 置"
    (MB (&= (inner* $X $Y)
            (&i* (&* $Y) (&* $Q) $Q $X)) ".")
    "注意到我们这里将右边的" (&c* $1 $1)
    "矩阵与其唯一的元素等同起来了. 当" $Q
    "为恒等矩阵时, 这个内积本质上和例子1是相同的, "
    "我们将其称为" (^ $F n*1) "上的"
    (B "标准内积") ". 读者应该注意到术语"
    "&quot;标准内积&quot;在两种特定的"
    "上下文中使用. 对于一般的域" $F
    "上的有限维向量空间, 并不存在显然可称之为标准的内积.")
   ((example #:n "5")
    "令" $V "是所有类型为" (&-> (li0 $0 $1) $CC)
    "的连续函数构成的向量空间, 那么"
    (MB (&= (inner* $f $g)
            (uintegral
             (&i* (app $f $t)
                  (OverBar (app $g $t))))))
    "是" $V "上的一个内积. 可能读者更熟悉单位区间"
    "上的实值连续函数构成的向量空间, 此时" (app $g $t)
    "上的复共轭是可以省略的.")
   ((example #:n "6")
    "这实际上是一类例子. 读者可以通过以下方法根据已有的内积"
    "构造出新的内积来. 令" $V "和" $W "是域" $F
    "上的向量空间, 设" (inner* $ $) "是" $W
    "上的一个内积. 如果" $T "是一个从" $V "到" $W
    "的非奇异线性变换, 那么"
    (MB (&= (appl $p_T $alpha $beta)
            (inner* (ap $T $alpha) (ap $T $beta))))
    "定义了" $V "上的一个内积" $p_T
    ". 例子4中的内积可以被视为这个的一种特殊情形, "
    "以下同样也是特殊情形."
    (Ol #:attr* '((type "a"))
        (Li "令" $V "是一个有限维向量空间, 令"
            (MB basis:def)
            "是" $V "的一个有序基. 令"
            (&..cm $epsilon_1 $epsilon_n)
            "是" $F^n "的标准有序基, 令" $T "是由"
            (&cm (&= (ap $T $alpha_j) $epsilon_j)
                 (&= $j (&..cm $1 $n)))
            "定义的从" $V "到" $F^n
            "的线性变换. 换言之, 令" $T
            "是由" $BBB "确定的从" $V "到" $F^n
            "的&quot;自然&quot;同构. 如果我们取"
            $F^n "上的标准内积, 那么"
            (MB (&= (ap $p_T
                        (tup (sum (&= $j $1) $n
                                  (&i* $x_j $alpha_j))
                             (sum (&= $k $1) $n
                                  (&i* $y_k $alpha_k))))
                    (sum (&= $j $1) $n
                         (&i* $x_j
                              (_ (OverBar $y) $j)))) ".")
            "因此, 对于" $V "的任意的有序基, 都存在一个具有性质"
            (&= (inner* $alpha_j $alpha_k) (&delta $j $k))
            "的内积. 实际上, 很容易表明恰存在一个这样的内积. "
            "之后我们将证明" $V "上的每个内积都可根据某个有序基"
            $BBB "按照以上方式确定.")
        (Li "让我们再次检视例子5, 令" $V
            "是单位区间上的所有连续函数构成的空间, 取" (&= $W $V)
            ". 令" $T "是&quot;乘上" $t "&quot;的线性算子, 即"
            (&cm (&= (app (@ap $T $f) $t)
                     (&i* $t (app $f $t)))
                 (&<= $0 $t $1))
            ". 容易验证" $T "是线性的. 而且, " $T
            "也是非奇异的. 这是因为, 设" (&= (ap $T $f) $0) ", 那么对于"
            (&<= $0 $t $1) "有" (&= (&i* $t (app $f $t)) $0)
            ", 因而" (&> $t $0) "时" (&= (app $f $t) $0)
            ". 鉴于" $f "是连续的, 我们也有" (&= (app $f $0) $0)
            ", 于是" (&= $f $0) ". 现在使用例子5的内积, 我们可以构造"
            $V "上的一个新的内积"
            (eqnderiv
             (appl $p_T $f $g)
             (uintegral
              (&i* (app (@ap $T $f) $t)
                   (OverBar (app (@ap $T $g) $t))))
             (uintegral
              (&i* (app $f $t) (OverBar (app $g $t)) $t^2))))))
   (P "我们现在开始检视内积的一些一般性质. 设" $V
      "是一个带有内积的复向量空间, 那么对于" (∈ $alpha $beta $V)
      ", 我们有"
      (MB (&= (inner* $alpha $beta)
              (Complex (Re (inner* $alpha $beta))
                       (Im (inner* $alpha $beta)))))
      "其中" (Re (inner* $alpha $beta)) "和"
      (Im (inner* $alpha $beta)) "分别是复数" (inner* $alpha $beta)
      "的实部和虚部. 如果" $z "是一个复数, 那么"
      (&= (Im $z) (Re (@- (&i* $i $z)))) ", 这可以推出"
      (MB (&= (Im (inner* $alpha $beta))
              (Re (bra0 (&i* (&- $i) (inner* $alpha $beta))))
              (Re (inner* $alpha (&i* $i $beta)))) ".")
      "因此, 按照"
      (MB (&= (inner* $alpha $beta)
              (Complex (Re (inner* $alpha $beta))
                       (Re (inner* $alpha (&i* $i $beta))))))
      "内积完全可由其&quot;实部&quot;确定.")
   (P "偶尔知道实或复向量空间上的内积可由另一种函数确定是很有用的, "
      "这种函数即所谓的二次形式. 为了定义二次形式, 我们首先以"
      (&norm $alpha) "代表" (inner* $alpha $alpha)
      "的正平方根; " (&norm $alpha) "被称为" $alpha
      "相对于内积的" (B "范数") ". 通过考察"
      (&cm $RR^1 $CC $RR^2 $RR^3) "上由标准内积导出的范数, "
      "读者应该说服自己将" $alpha "的范数想成是" $alpha
      "的长度是很贴切的. 由内积决定的" (B "二次形式")
      "是函数" (&\|-> $alpha (^ (&norm $alpha) $2))
      ". 根据内积的性质, 我们可以推出, 对于任意的向量"
      $alpha "和" $beta "有"
      (MB (&= (&sqr (&norm (&+- $alpha $beta)))
              (: (&sqr (&norm $alpha)) $+-
                 (&i* $2 (Re (inner* $alpha $beta))) $+
                 (&sqr (&norm $beta)))) ".")
      "因此, 在实数域的情形下, 我们有"
      (MB (&= (inner* $alpha $beta)
              (&- (&i* 1/4 (&sqr (&norm (&+ $alpha $beta))))
                  (&i* 1/4 (&sqr (&norm (&- $alpha $beta)))))) ".")
      "在复数域的情形下, 我们得到的是更复杂的表达式"
      (MB (&= (inner* $alpha $beta)
              (&+ (&- (&i* 1/4 (&sqr (&norm (&+ $alpha $beta))))
                      (&i* 1/4 (&sqr (&norm (&- $alpha $beta)))))
                  (&- (&i* (~ $i $4)
                           (&sqr
                            (&norm
                             (&+ $alpha (&i* $i $beta)))))
                      (&i* (~ $i $4)
                           (&sqr
                            (&norm
                             (&- $alpha (&i* $i $beta)))))))) ".")
      "这两个公式都被称为" (B "极化恒等式")
      ", 我们也应该注意到在复数域的情形下其也可以写成以下形式:"
      (MB (&= (inner* $alpha $beta)
              (&i* 1/4
                   (sum (&= $n $1) $4
                        (&i* $i^n
                             (&sqr
                              (&norm
                               (&+ $alpha (&i* $i^n $beta)))))))) "."))
   (P "刚才我们所得到的性质对于任意的实或复向量空间上的内积"
      "均成立, 不论其维数如何. 现在我们转向" $V
      "是有限维向量空间的情形. 正如读者可能会猜到的, "
      "有限维向量空间上的内积总是可以基于一个有序基由矩阵刻画.")
   (P "设" $V "是有限维的, 令"
      (MB basis:def)
      "是" $V "的一个有序基, 并且给定" $V "上的一个特定的内积. "
      "我们将表明, 这个内积完全由以下的这些值"
      (MB (&= (mref $G $j $k) (inner* $alpha_k $alpha_j)))
      "决定. 如果" (&= $alpha (sum (&= $k $1) $n (&i* $x_k $alpha_k)))
      "且" (&= $beta (sum (&= $j $1) $n (&i* $y_j $alpha_j))) ", 那么"
      (eqnderiv
       (inner* $alpha $beta)
       (Inner* (sum (&= $k $1) $n
                    (&i* $x_k $alpha_k)) $beta)
       (sum (&= $k $1) $n
            (&i* $x_k (inner* $alpha_k $beta)))
       (sum (&= $k $1) $n
            (&i* $x_k
                 (sum (&= $j $1) $n
                      (&i* (_ (OverBar $y) $j)
                           (inner* $alpha_k $alpha_j)))))
       (sum (&= $j $1) $n
            (sum (&= $k $1) $n
                 (&i* (_ (OverBar $y) $j)
                      (mref $G $j $k)
                      $x_k)))
       (&i* (&* $Y) $G $X))
      "其中" $X "和" $Y "分别是" $alpha "和" $beta
      "在有序基" $BBB "下的坐标矩阵, 而" $G "是以"
      (&= (mref $G $j $k) (inner* $alpha_k $alpha_j))
      "为元素的矩阵. 我们称" $G "为"
      (B "内积在有序基" $BBB "下的矩阵") ". 根据定义, "
      $G "是一个Hermite矩阵, 即" (&= $G (&* $G))
      ". 然而, " $G "是一种相当特殊的Hermite矩阵, "
      "因为其必须满足附加的条件"
      (MB (&cm (&> (&i* (&* $X) $G $X) $0)
               (&!= $X $0)) ".")
      "特别地, " $G "必须是可逆的. 否则的话, 存在一个"
      (&!= $X $0) "使得" (&= (&i* $G $X) $0)
      ", 那么对于这样的" $X "就不能满足以上要求了. "
      "更显式地说, 以上的条件即对于任意不全为零的标量"
      (&..cm $x_1 $x_n) "有"
      (MB (&> (sum (&= $j $1) $n
                   (sum (&= $k $1) $n
                        (&i* (_ (OverBar $x) $j)
                             (mref $G $j $k)
                             $x_k))) $0) ".")
      "从中我们立即可以看出" $G "的每个对角线元素都必然是正数. "
      "[译注: 提及正数, 一定是实数.] "
      "然而, 这个施加于对角线元素上的条件并不足以保证"
      (&cm (&> (&i* (&* $X) $G $X) $0) (&!= $X $0))
      ", 之后我们将给出使其成立的充分条件. "
      "[译注: 这个施加于Hermite矩阵上的条件一般"
      "被称为&quot;正定&quot;条件.]")
   (P "以上这样的过程是可逆的, 即若任意的Hermite矩阵" (∈ $G (^ $F n*n))
      "满足" (&cm (&> (&i* (&* $X) $G $X) $0) (&!= $X $0))
      ", 那么" $G "是" $V "上的一个内积在有序基" $BBB
      "下的矩阵. 这个内积是由公式"
      (MB (&= (inner* $alpha $beta) (&i* (&* $Y) $G $X)))
      "给定的, 其中" $X "和" $Y "分别是" $alpha "和"
      $beta "在有序基" $BBB "下的坐标矩阵.")
   ((exercise #:n "1")
    "令" $V "是一个向量空间而" (inner* $ $) "是" $V "上的一个内积."
    (Ol #:attr* '((type "a"))
        (Li "证明对于任意的" (∈ $beta $V) "有"
            (&= (inner* $0 $beta) $0) ".")
        (Li "证明若对于任意的" (∈ $beta $V) "有"
            (&= (inner* $alpha $beta) $0) ", 那么"
            (&= $alpha $0) ".")))
   ((exercise #:n "2")
    "令" $V "是域" $F "上的一个向量空间. 证明" $V
    "上的两个内积之和仍然是" $V "上的一个内积. "
    "两个内积之差是内积吗? 证明一个内积的正倍数"
    "仍然是一个内积.")
   ((exercise #:n "3")
    "显式描述" $RR^1 "和" $CC^1 "上的所有内积.")
   ((exercise #:n "4")
    "验证" $F^n "上的标准内积的确是一个内积.")
   ((exercise #:n "5")
    "令" (inner* $ $) "是" $RR^2 "上的标准内积."
    (Ol #:attr* '((type "a"))
        (Li "令" (&cm (&= $alpha (tu0 $1 $2)) (&= $beta (tu0 $-1 $1)))
            ", 如果向量" $gamma "满足" (&= (inner* $alpha $gamma) $-1)
            "且" (&= (inner* $beta $gamma) $3) ", 求出" $gamma ".")
        (Li "证明对于任意的" (∈ $alpha $RR^2) ", 我们有"
            (&= $alpha (LC (inner* $alpha $epsilon_1) $epsilon_1
                           (inner* $alpha $epsilon_2) $epsilon_2)) ".")))
   ((exercise #:n "6")
    "令" (inner* $ $) "是" $RR^2 "上的标准内积, 而"
    (&= (appl $T $x_1 $x_2) (tu0 (&- $x_2) $x_1))
    "是" $RR^2 "上的线性算子. 现在" $T "是&quot;逆时针旋转90度&quot;"
    "的变换, 并且对于所有的" (∈ $alpha $RR^2) ", 都有"
    (&= (inner* $alpha (ap $T $alpha)) $0) ". 找出所有这样的"
    $RR^2 "上的内积" (in* $ $) ", 其对于每个向量" $alpha "有"
    (&= (in* $alpha (ap $T $alpha)) $0) ".")
   ((exercise #:n "7")
    "令" (inner* $ $) "是" $CC^2 "上的标准内积, 证明不存在非零的"
    $CC^2 "上的线性算子" $T "使得对于每个" (∈ $alpha $CC^2)
    "有" (&= (inner* $alpha (ap $T $alpha)) $0) ". 推广这个结果.")
   ((exercise #:n "8")
    "令" (∈ $A (^ $RR 2*2)) ", 定义映射"
    (func $f_A (&c* (^ $RR (&c* $2 $1)) (^ $RR (&c* $2 $1))) $RR)
    "为"
    (MB (&= (appl $f_A $X $Y) (&i* $Y^t $A $X)) ".")
    "证明" $f_A "是" (^ $RR (&c* $2 $1)) "上的一个内积当且仅当"
    (&cm (&= $A $A^t) (&> (mref $A $1 $1) $0)
         (&> (mref $A $2 $2) $0) (&> (&det $A) $0)) ".")
   ((exercise #:n "9")
    "令" $V "是一个带有的内积的实或复向量空间, 证明由内积确定"
    "的范数满足" (B "平行四边形定律")
    (MB (&= (&+ (&sqr (&norm (&+ $alpha $beta)))
                (&sqr (&norm (&- $alpha $beta))))
            (LC $2 (&sqr (&norm $alpha))
                $2 (&sqr (&norm $beta)))) "."))
   ((exercise #:n "10")
    "找出例子2中的内积在" $RR^2 "的标准有序基下的矩阵.")
   ((exercise #:n "11")
    "证明公式"
    (MB (&= (Inner*
             (sum (&= $j $0) $l (&i* $a_j $x^j))
             (sum (&= $k $0) $m (&i* $b_k $x^k)))
            (sum (&= $j $0) $l
                 (sum (&= $k $0) $m
                      (~ (&i* $a_j $b_k)
                         (&+ $j $k $1))))))
    "定义了" (&poly $RR) "上的一个内积. 令" $W
    "是次数小于等于" $n "的多项式构成的子空间. "
    "限制以上内积于" $W ", 找出其相对于有序基"
    (setE $1 $x $x^2 $..h $x^n)
    "的矩阵. (提示: 为了表明这个公式的确定义了一个内积, 观察到"
    (MB (&= (inner* $f $g)
            (uintegral
             (&i* (app $f $t) (app $g $t)))))
    "然后处理这个积分表达式.)")
   ((exercise #:n "12")
    "令" $V "是一个有限维向量空间, " basis:def
    "是" $V "的一个有序基, " (inner* $ $)
    "是" $V "上的一个内积. 如果" (&..cm $c_1 $c_n)
    "是任意的" $n "个标量, 那么恰存在一个向量"
    (∈ $alpha $V) "使得"
    (&cm (&= (inner* $alpha $alpha_j) $c_j)
         (&= $j (&..cm $1 $n))) ".")
   ((exercise #:n "13")
    "令" $V "是一个复向量空间. 一个函数" (func $J $V $V)
    "被称为一个" (B "共轭 (conjugation)") ", 如果"
    (&cm (linear+ (lambda (v) (app $J v))
                  $alpha $beta)
         (&= (app $J (&i* $c $alpha))
             (&i* (OverBar $c) (app $J $alpha)))
         (&= (app $J (app $J $alpha)) $alpha))
    ", 其中" $c "是任意的标量而" (∈ $alpha $beta $V)
    ". 如果" $J "是一个共轭, 证明:"
    (Ol #:attr* '((type "a"))
        (Li (&= $W (setI (∈ $alpha $V) (&= (ap $J $alpha) $alpha)))
            "相对于" $V "中所定义的运算可以被视为域" $RR
            "上的一个向量空间.")
        (Li "对于每个" (∈ $alpha $V) ", 存在唯一的向量"
            (∈ $beta $gamma $W) "使得"
            (&= $alpha (&+ $beta (&i* $i $gamma))) ".")))
   ((exercise #:n "14")
    "令" $V "是一个复向量空间, " $W "是一个满足以下性质的"
    $V "的子集:"
    (Ol #:attr* '((type "a"))
        (Li "相对于" $V "中所定义的运算, " $W
            "可以被视为一个实向量空间.")
        (Li "对于每个" (∈ $alpha $V) ", 存在唯一的向量"
            (∈ $beta $gamma $W) "满足"
            (&= $alpha (&+ $beta (&i* $i $gamma))) "."))
    "证明" (&= (ap $J $alpha) (&- $beta (&i* $i $gamma)))
    "定义了" $V "上的一个共轭, 其满足"
    (&= (ap $J $alpha) $alpha) "当且仅当" (∈ $alpha $W)
    ". 另外, 证明" $J "是" $V "上唯一带有此性质的共轭.")
   ((exercise #:n "15")
    "找出" $CC^1 "和" $CC^2 "上的所有共轭.")
   ((exercise #:n "16")
    "令" $W "是复向量空间" $V "的一个有限维实子空间. 证明"
    $W "满足练习14的条件b当且仅当" $W
    "的每个基也是" $V "的一个基.")
   ((exercise #:n "17")
    "令" $V "是一个复向量空间, " $J "是" $V "上的一个共轭, "
    (&= $W (setI (∈ $alpha $V) (&= (ap $J $alpha) $alpha)))
    "是" $V "的一个实子空间, " $f "是" $W "上的一个内积, 证明:"
    (Ol #:attr* '((type "a"))
        (Li "存在唯一的" $V "上的内积" $g "使得对于任意的"
            (∈ $alpha $beta $W) "有"
            (&= (appl $g $alpha $beta)
                (appl $f $alpha $beta)) ".")
        (Li "对于所有的" (∈ $alpha $beta $V) ", "
            (&= (appl $g (ap $J $alpha) (ap $J $beta))
                (appl $g $beta $alpha)) "."))
    "以上的部分a是在说" $RR^1 "和" $CC^1 " (或者"
    $RR^n "和" $CC^n ") 上的标准内积之间的什么关系?")
   (H3 "第8.2节 内积空间")
   (P "既然现在我们已经对于内积有所了解, 那么我们将注意力"
      "转移到向量空间与其上的某个特定内积结合产生的代数结构上来. "
      "具体来说, 我们将建立由内积赋予向量空间的"
      "&quot;长度&quot;和&quot;正交性&quot;的概念的基本性质.")
   ((definition)
    "一个" (B "内积空间") "是一个其上带有特定内积的实或复向量空间.")
   (P "一个有限维的实内积空间常被称为一个" (B "Euclid空间")
      ". 一个复内积空间经常被称为一个" (B "酉空间") ".")
   ((theorem #:n "1")
    "如果" $V "是一个内积空间, 那么对于任意的向量"
    (∈ $alpha $beta $V) "和标量" $c ", 我们有"
    (Ol #:attr* '((type "i"))
        (Li (&= (&norm (&i* $c $alpha))
                (&i* (&abs $c) (&norm $alpha))) ";")
        (Li "对于" (&!= $alpha $0) ", "
            (&> (&norm $alpha) $0) ";")
        (Li (&<= (&abs (inner* $alpha $beta))
                 (&i* (&norm $alpha)
                      (&norm $beta))) ";")
        (Li (&<= (&norm (&+ $alpha $beta))
                 (&+ (&norm $alpha)
                     (&norm $beta))) ".")))
   ((proof)
    "陈述i和ii几乎可由定义直接推出. iii中的不等式在"
    (&= $alpha $0) "时是显然成立的. 若"
    (&!= $alpha $0) ", 置"
    (MB (&= $gamma
            (&- $beta (orthoproj $beta $alpha))))
    "那么" (&= (inner* $gamma $alpha) $0) ", 然后"
    (eqnderiv
     (&<= $0 (&sqr (&norm $gamma)))
     (Inner* (&- $beta (orthoproj $beta $alpha))
             (&- $beta (orthoproj $beta $alpha)))
     (&- (inner* $beta $beta)
         (~ (&i* (inner* $beta $alpha)
                 (inner* $alpha $beta))
            (&sqr (&norm $alpha))))
     (&- (inner* $beta $beta)
         (~ (&sqr (&abs (inner* $alpha $beta)))
            (&sqr (&norm $alpha)))))
    "因此, "
    (&<= (&sqr (&abs (inner* $alpha $beta)))
         (&i* (sqrnorm $alpha) (sqrnorm $beta)))
    ", 再开根即可. 现在使用iii, 我们可以推出"
    (MB (deriv0
         (sqrnorm (&+ $alpha $beta))
         $=
         (&+ (sqrnorm $alpha)
             (&i* $2 (Re (inner* $alpha $beta)))
             (sqrnorm $beta))
         $<=
         (&+ (sqrnorm $alpha)
             (&i* $2 (&abs (inner* $alpha $beta)))
             (sqrnorm $beta))
         $<=
         (&+ (sqrnorm $alpha)
             (&i* $2 (&norm $alpha) (&norm $beta))
             (sqrnorm $beta))
         $=
         (&sqr (@+ (&norm $alpha) (&norm $beta)))))
    "于是, "
    (&<= (&norm (&+ $alpha $beta))
         (&+ (&norm $alpha)
             (&norm $beta))) ".")
   (P "iii被称为" (B "Cauchy-Schwarz不等式")
      ", 其有着各种各样的应用. 根据刚才我们的证明, 如果"
      (&!= $alpha $0) ", 那么除非"
      (MB (&= $beta (orthoproj $beta $alpha)))
      "该不等式严格成立. 也就是说, Cauchy-Schwarz不等式"
      "取等号当且仅当" $alpha "和" $beta "线性相关.")
   ((tcomment)
    "以上对于Cauchy-Schwarz不等式的证明看似复杂, "
    "实则在某种意义上有着简单的几何解释. 例如, 在" $RR^2
    "及其上的标准内积下, 很容易看出来"
    (M #:attr* '((displaystyle "true"))
       (orthoproj $beta $alpha))
    "是" $beta "在" $alpha "上的垂直投影, "
    (&= (inner* $gamma $alpha) $0)
    "就是对于垂直的表述, 而"
    (MB (&= (sqrnorm $gamma)
            (&- (inner* $beta $beta)
                (~ (&sqr (&abs (inner* $alpha $beta)))
                   (&sqr (&norm $alpha))))))
    "差不多就是勾股定理/Pythagoras定理的一个应用.")
   ((example #:n "7")
    "如果我们将Cauchy-Schwarz不等式应用于例子1, 2, 3, 5"
    "中给出的内积, 那么我们就会得到以下结果:"
    (Ol #:attr* '((type "a"))
        (Li (MB (&<= (&Abs (sum (&= $k $1) $n
                                (&i* $x_k
                                     (_ (OverBar $y) $k))))
                     (&i* (^ (@sum (&= $k $1) $n
                                   (&sqr (&abs $x_k)))
                             1/2)
                          (^ (@sum (&= $k $1) $n
                                   (&sqr (&abs $y_k)))
                             1/2)))))
        (Li (MB (&<= (&abs (: (&i* $x_1 $y_1) $-
                              (&i* $x_2 $y_1) $-
                              (&i* $x_1 $y_2) $+
                              (&i* $4 $x_2 $y_2)))
                     (&i* (^ (@+ (^ (@- $x_1 $x_2) $2)
                                 (&i* $3 (_^ $x $2 $2)))
                             (&/ $1 $2))
                          (^ (@+ (^ (@- $y_1 $y_2) $2)
                                 (&i* $3 (_^ $y $2 $2)))
                             (&/ $1 $2))))))
        (Li (MB (&<= (&abs (&tr (&i* $A (&* $B))))
                     (&i* (^ (@ (&tr (&i* $A (&* $A))))
                             (&/ $1 $2))
                          (^ (@ (&tr (&i* $B (&* $B))))
                             (&/ $1 $2))))))
        (Li (MB (&<= (&Abs (uintegral
                            (&i* (app $f $t)
                                 (OverBar (app $g $t)))))
                     (&i* (^ (pare
                              (uintegral
                               (&sqr (&abs (app $f $t)))))
                             1/2)
                          (^ (pare
                              (uintegral
                               (&sqr (&abs (app $g $t)))))
                             1/2)))))))
   ((definition)
    "令" $alpha "和" $beta "是内积空间" $V "中的向量, 那么"
    $alpha (B "正交") "于" $beta ", 如果"
    (&= (inner* $alpha $beta) $0) ". 既然这能推出" $beta
    "正交于" $alpha ", 我们常就简单说" $alpha "和" $beta
    "是正交的. 对于" $V "的一个子集" $S ", 我们称" $S
    "是一个" (B "正交集合") ", 若其中不同向量之间均是"
    "正交的. 如果对于正交集合" $S "的每个向量" $alpha
    "有" (&= (&norm $alpha) $1) ", 那么我们就称" $S
    "是一个" (B "规范正交集合") ".")
   (P "零向量正交于" $V "中的每个向量, 而且是唯一具有此性质"
      "的向量. 另外, 读者应该将规范正交集合想成是由长度为"
      $1 "且相互垂直的向量构成的集合.")
   ((example #:n "8")
    $RR^n "的标准基相对于其上的标准内积是一个规范正交集合, "
    $CC^n "也是如此.")
   ((example #:n "9")
    $RR^2 "中的向量" (tu0 $x $y) "相对于标准内积与"
    (tu0 (&- $y) $x) "正交, 因为"
    (MB (&= (inner* (tu0 $x $y) (tu0 (&- $y) $x))
            (&+ (&- (&i* $x $y)) (&i* $y $x)) $0) ".")
    "然而, 如果" $RR^2 "装备的是例子2中的内积, 那么"
    (tu0 $x $y) "和" (tu0 (&- $y) $x) "正交当且仅当"
    (MB (&= $y (&i* (~ (&+- (&- $3) (Msqrt 13)) $2)
                    $x)) "."))
   ((example #:n "10")
    "令" $V "是" (^ $CC n*n) ", " (&E $p $q) "是仅第"
    $p "行" $q "列为" $1 "其余均为" $0 "的矩阵, 那么"
    "所有这样的矩阵" (&E $p $q) "构成的集合相对于例子3"
    "中给出的内积是规范正交的, 因为"
    (MB (&= (inner* (&E $p $q) (&E $r $s))
            (&tr (&i* (&E $p $q) (&E $s $r)))
            (&i* (&delta $q $s)
                 (&tr (&E $p $r)))
            (&i* (&delta $q $s)
                 (&delta $p $r))) "."))
   ((example #:n "11")
    "令" $V "是区间" (li0 $0 $1) "上的连续复值 (或者实值) "
    "函数构成的向量空间, 并定义其上的内积为"
    (MB (&= (inner* $f $g)
            (uintegral
             (&i* (app $f $t)
                  (OverBar (app $g $t))))) ".")
    "设"
    (&= (app $f_n $x)
        (&i* (Msqrt $2)
             (&cos (&i* $2 $pi $n $x))))
    "且"
    (&= (app $g_n $x)
        (&i* (Msqrt $2)
             (&sin (&i* $2 $pi $n $x))))
    ", 那么"
    (setE $1 $f_1 $g_1 $f_2 $g_2 $..h)
    "构成了一个无穷的规范正交集合. "
    "在复情形下, 我们也可以构造以下线性组合"
    (MB (&cm (&i* (~ $1 (Msqrt $2))
                  (@ (&+- $f_n (&i* $i $g_n))))
             (&= $n (&cm $1 $2 $..h))))
    "以这种方式, 我们构造了一个新的规范正交集合"
    $S ", 其由所有具有形式"
    (MB (&cm (&= (app $h_n $x)
                 (^ $e (&i* $2 $pi $i $n $x)))
             (&= $n (&cm (&+- $1) (&+- $2) $..h))))
    "的函数构成. 将常函数" $1 "加入" $S
    "得到的集合" $S^ "也是规范正交的. 我们假定"
    "读者熟悉以上内容所牵涉的积分计算.")
   (P "以上例子给出的规范正交集合均是线性无关的, "
      "现在我们将表明诚然如此.")
   ((theorem #:n "2")
    "由非零向量构成的正交集合是线性无关的.")
   ((proof)
    "令" $S "是某给定内积空间中由非零向量"
    "构成的有限或无限的正交集合, 设"
    (&..cm $alpha_1 $alpha_2 $alpha_m)
    "是" $S "中的不同向量, 并且"
    (MB (&= $beta (LC0 $c_1 $alpha_1
                       $c_2 $alpha_2
                       $c_m $alpha_m)))
    "那么"
    (eqnderiv
     (inner* $beta $alpha_k)
     (Inner* (sum (&= $j $1) $m
                  (&i* $c_j $alpha_j))
             $alpha_k)
     (sum (&= $j $1) $m
          (&i* $c_j
               (inner* $alpha_j
                       $alpha_k)))
     (&i* $c_k (inner* $alpha_k $alpha_k)))
    "既然" (&!= (inner* $alpha_k $alpha_k) $0)
    ", 这可以推出"
    (MB (&cm (&= $c_k (~ (inner* $beta $alpha_k)
                         (sqrnorm $alpha_k)))
             (&<= $1 $k $m)) ".")
    "因此, 当" (&= $beta $0) "时, 每个"
    (&= $c_k $0) ", 即" $S "是线性无关的集合.")
   ((corollary)
    "如果一个向量" $beta "是由非零向量"
    (&..cm $alpha_1 $alpha_m)
    "构成的一个正交序列的线性组合, 那么"
    $beta "必然是以下特定的线性组合"
    (MB (&= $beta
            (sum (&= $k $1) $m
                 (orthoproj $beta $alpha_k))) "."))
   (P "以上的推论是定理的证明的直接结果. 另外, 还有"
      "一个应该提及的显然推论. 如果"
      (setE $alpha_1 $..h $alpha_m)
      "是某个有限维内积空间" $V
      "中由非零向量构成的正交集合, 那么"
      (&<= $m (&dim $V)) ". 这是在说" $V
      "中相互正交的方向的数目不可能超过" $V
      "的由代数定义的维数. " $V
      "中相互正交的方向的最大数目可以被理解为"
      $V "的几何维数, 并且我们刚才看到其不会大于"
      "代数维数. 这两种维数相等的事实是"
      "以下结果的一个特定推论.")
   ((theorem #:n "3")
    "令" $V "是一个内积空间, 而" (&..cm $beta_1 $beta_n)
    "是" $V "中线性无关的向量, 那么我们可以构造" $V
    "中相互正交的向量" (&..cm $alpha_1 $alpha_n)
    "使得对于每个" (&= $k (&..cm $1 $2 $n)) ", 集合"
    (MB (setE $alpha_1 $..h $alpha_k))
    "是由" (&..cm $beta_1 $beta_k) "张成的子空间的一个基.")
   ((proof)
    "向量" (&..cm $alpha_1 $alpha_n)
    "可由一种被称为" (B "Gram-Schmidt正交化过程")
    "的构造方式得到. 首先, 令" (&= $alpha_1 $beta_1)
    ", 而其他向量则按以下方法由归纳给定: 设已经挑选了"
    (&..cm $alpha_1 $alpha_m) "使得对于每个" $k "有"
    (MB (&cm (setE $alpha_1 $..h $alpha_k)
             (&<= $1 $k $m)))
    "是由" (&..cm $beta_1 $beta_k) "张成的" $V
    "的子空间的一个正交基, 其中" (: $1 $<= $m $< $n)
    ". 为了构造下一个向量" (_ $alpha (&+ $m $1)) ", 令"
    (MB (&= (_ $alpha (&+ $m $1))
            (&- (_ $beta (&+ $m $1))
                (sum (&= $k $1) $m
                     (orthoproj (_ $beta (&+ $m $1))
                                $alpha_k)))))
    "那么" (&!= (_ $alpha (&+ $m $1)) $0)
    ", 因为否则的话" (_ $beta (&+ $m $1)) "就是"
    (&..cm $alpha_1 $alpha_m) "的线性组合了, 也就是"
    (&..cm $beta_1 $beta_m) "的线性组合. 而且, 如果"
    (&<= $1 $j $m) ", 那么"
    (eqnderiv
     (inner* (_ $alpha (&+ $m $1)) $alpha_j)
     (&- (inner* (_ $beta (&+ $m $1)) $alpha_j)
         (sum (&= $k $1) $m
              (&i* (~ (inner* (_ $beta (&+ $m $1))
                              $alpha_k)
                      (sqrnorm $alpha_k))
                   (inner* $alpha_k $alpha_j))))
     (&- (inner* (_ $beta (&+ $m $1)) $alpha_j)
         (inner* (_ $beta (&+ $m $1)) $alpha_j))
     $0)
    "因此, " (setE $alpha_1 $..h (_ $alpha (&+ $m $1)))
    "是由" (&+ $m $1) "个非零向量构成的正交集合, 并且它们都在由"
    (&..cm $beta_1 (_ $beta (&+ $m $1)))
    "张成的子空间之中. 根据定理2, 其的确是该子空间的一个基. "
    "换言之, 向量" (&..cm $alpha_1 $alpha_n)
    "可按以上公式一个接着一个地构造出来. 特别地, 当"
    (&= $n $4) "时, 我们有"
    (MB (set-attr*
         (&Table
          ($alpha_1 $= $beta_1)
          ($alpha_2 $= (&- $beta_2
                           (orthoproj $beta_2 $alpha_1)))
          ($alpha_3 $= (&- $beta_3
                           (orthoproj $beta_3 $alpha_1)
                           (orthoproj $beta_3 $alpha_2)))
          ($alpha_4 $= (&- $beta_4
                           (orthoproj $beta_4 $alpha_1)
                           (orthoproj $beta_4 $alpha_2)
                           (orthoproj $beta_4 $alpha_3))))
         'columnalign "left"
         'displaystyle "true")))
   ((corollary)
    "每个有限维内积空间都拥有一个规范正交基.")
   ((proof)
    "令" $V "是一个有限维内积空间, 而"
    (setE $beta_1 $..h $beta_n) "是" $V
    "的一个基. 应用Gram-Schmidt过程, 我们可以构造一个正交基"
    (setE $alpha_1 $..h $alpha_n)
    ". 那么, 为了获得一个规范正交基, 我们仅需将每个向量"
    $alpha_k "替换以" (&/ $alpha_k (&norm $alpha_k))
    "就够了.")
   (P "规范正交基相较于其他任意的基的一个主要优势在于牵涉"
      "坐标的计算会更加简单. 为了澄清这个断言, 设" $V
      "是一个有限维内积空间. 那么, 根据上一节的讨论, "
      "我们可以构造这个内积相对于" $V "的某个有序基"
      basis:def "的矩阵" $G ", 其由"
      (MB (&= (mref $G $j $k)
              (inner* $alpha_k $alpha_j)))
      "定义, 然后便可基于坐标来计算内积. 若" $BBB
      "是一个规范正交基, 那么" $G "就是恒等矩阵, 而对于任意的标量"
      $x_j "和" $y_k ", 我们有"
      (MB (&= (Inner* (sum (&= $j $1) $n
                           (&i* $x_j $alpha_j))
                      (sum (&= $k $1) $n
                           (&i* $y_k $alpha_k)))
              (sum (&= $j $1) $n
                   (&i* $x_j (_ (OverBar $y) $j)))))
      "因此, 基于规范正交基, " $V "中的内积看起来就像是"
      $F^n "中的标准内积.")
   (P "尽管实际计算上的用途有限, 但有趣的是, Gram-Schmidt"
      "过程也可以用来判定是否线性相关. 设" (&..cm $beta_1 $beta_n)
      "是" $V "中线性相关的向量, 排除" (&= $beta_1 $0)
      "的平凡情况. [译注: 其实不排除也可以, 只是对于极端情况"
      "需要一些说明.] 令" $m "是使得" (&..cm $beta_1 $beta_m)
      "能够线性无关的最大整数, 那么" (: $1 $<= $m $< $n)
      ". 若" (&..cm $alpha_1 $alpha_m) "是施行正交化过程于"
      (&..cm $beta_1 $beta_m) "得到的向量, 那么"
      (MB (&= (_ $alpha (&+ $m $1))
              (&- (_ $beta (&+ $m $1))
                  (sum (&= $k $1) $m
                       (orthoproj (_ $beta (&+ $m $1))
                                  $alpha_k)))))
      "必然为" $0 ". 这是因为, " (_ $alpha (&+ $m $1))
      "在由" (&..cm $alpha_1 $alpha_m)
      "张成的子空间之中并且正交于这些向量, 因而根据定理2的推论可知"
      (&= (_ $alpha (&+ $m $1)) $0) ". 也就是说, "
      (_ $beta (&+ $m $1)) "是" (&..cm $alpha_1 $alpha_m)
      "的线性组合, 即" (&..cm $beta_1 $beta_m)
      "的线性组合, 那么" (&..cm $beta_1 (_ $beta (&+ $m $1)))
      "是线性相关的.")
   ((tcomment)
    "上一段的内容告诉我们, 即便为了施行Gram-Schmidt正交化过程, "
    "也无需提前判断出" (&..cm $beta_1 $beta_n)
    "是线性无关的. 这是因为, 在正交化的过程中, 一旦遇到某个"
    (&= $alpha_k $0) ", 那么便可知" (&..cm $beta_1 $beta_k)
    "线性相关. 而若正交化过程结束也没有出现哪个" (&= $alpha_k $0)
    ", 就可以断言" (&..cm $beta_1 $beta_n) "线性无关.")
   ((example #:n "12")
    "对于装备有标准内积的" $RR^3 ", 考虑向量"
    (MB (&cm (&= $beta_1 (tu0 3 0 4))
             (&= $beta_2 (tu0 -1 0 7))
             (&= $beta_3 (tu0 2 9 11))))
    "施行Gram-Schmidt过程于" (&cm $beta_1 $beta_2 $beta_3)
    ", 我们就得到了以下向量."
    (MB (set-attr*
         (&Table
          ($alpha_1 $= (tu0 3 0 4))
          ($alpha_2 $= (&- (tu0 -1 0 7)
                           (&i* (~ (inner* (tu0 -1 0 7)
                                           (tu0 3 0 4))
                                   25)
                                (tu0 3 0 4))))
          ($ $= (&- (tu0 -1 0 7) (tu0 3 0 4)))
          ($ $= (tu0 -4 0 3))
          ($alpha_3 $= (&- (tu0 2 9 11)
                           (&i* (~ (inner* (tu0 2 9 11)
                                           (tu0 3 0 4))
                                   25)
                                (tu0 3 0 4))
                           (&i* (~ (inner* (tu0 2 9 11)
                                           (tu0 -4 0 3))
                                   25)
                                (tu0 -4 0 3))))
          ($ $= (&- (tu0 2 9 11)
                    (&i* $2 (tu0 3 0 4))
                    (tu0 -4 0 3)))
          ($ $= (tu0 0 9 0)))
         'columnalign "left"
         'displaystyle "true"))
    "这些向量显然是非零的且相互正交, 因而"
    (setE $alpha_1 $alpha_2 $alpha_3)
    "是" $RR^3 "的一个正交基. 为了将" $RR^3
    "中任意的向量" (tu0 $x_1 $x_2 $x_3)
    "表达为" (&cm $alpha_1 $alpha_2 $alpha_3)
    "的线性组合, 我们无需求解任何线性方程组, "
    "运用定理2的推论即可. 因此, 我们就有"
    (MB (&= (tu0 $x_1 $x_2 $x_3)
            (LC (~ (LC $3 $x_1 $4 $x_3) 25)
                $alpha_1
                (~ (LC -4 $x_1 $3 $x_3) 25)
                $alpha_2
                (~ $x_2 $9)
                $alpha_3)) ".")
    "例如, " (tu0 $1 $2 $3) "可以被表示为线性组合"
    (MB (&= (tu0 $1 $2 $3)
            (LC 3/5 (tu0 $3 $0 $4)
                1/5 (tu0 -4 $0 $3)
                2/9 (tu0 $0 $9 $0))) ".")
    "实际上, 我们可以换个角度陈述以上的结果: 对偶于基"
    (setE $alpha_1 $alpha_2 $alpha_3) "的"
    (&* (@ $RR^3)) "的基" (setE $f_1 $f_2 $f_3)
    "可由以下公式所显式定义"
    (MB (set-attr*
         (&Table
          ((appl $f_1 $x_1 $x_2 $x_3)
           $= (~ (LC $3 $x_1 $4 $x_3) 25))
          ((appl $f_2 $x_1 $x_2 $x_3)
           $= (~ (LC -4 $x_1 $3 $x_3) 25))
          ((appl $f_3 $x_1 $x_2 $x_3)
           $= (~ $x_2 $9)))
         'columnalign "left"
         'displaystyle "true"))
    "当然, 这些公式可以写成以下更为一般的形式"
    (MB (&= (appl $f_j $x_1 $x_2 $x_3)
            (~ (inner* (tu0 $x_1 $x_2 $x_3)
                       $alpha_j)
               (sqrnorm $alpha_j))) ".")
    "最后一点, 注意到从" (&cm $alpha_1 $alpha_2 $alpha_3)
    "中我们可以得到规范正交基"
    (MB (&cm (&i* 1/5 (tu0 3 0 4))
             (&i* 1/5 (tu0 -4 0 3))
             (tu0 0 1 0)) "."))
   ((example #:n "13")
    "令"
    (MB (&= $A (Mat ($a $b) ($c $d))))
    "是一个复矩阵, 置" (&= $beta_1 (tu0 $a $b))
    "和" (&= $beta_2 (tu0 $c $d))
    ", 并设" (&!= $beta_1 $0)
    ". 如果我们使用" $CC^2 "上的标准内积对于"
    (&cm $beta_1 $beta_2)
    "施行正交化过程, 就会得到以下向量:"
    (eqn* ($alpha_1 $= (tu0 $a $b))
          ($alpha_2
           $= (&- (tu0 $c $d)
                  (&i* (~ (inner* (tu0 $c $d)
                                  (tu0 $a $b))
                          (&+ (&sqr (&abs $a))
                              (&sqr (&abs $b))))
                       (tu0 $a $b))))
          ($ $= (&- (tu0 $c $d)
                    (&i* (~ (LC $c (OverBar $a)
                                $d (OverBar $b))
                            (&+ (&sqr (&abs $a))
                                (&sqr (&abs $b))))
                         (tu0 $a $b))))
          ($ $= (tup (~ (&- (&i* $c (OverBar $b) $b)
                            (&i* $d (OverBar $b) $a))
                        (&+ (&sqr (&abs $a))
                            (&sqr (&abs $b))))
                     (~ (&- (&i* $d (OverBar $a) $a)
                            (&i* $c (OverBar $a) $b))
                        (&+ (&sqr (&abs $a))
                            (&sqr (&abs $b))))))
          ($ $= (&i* (~ (ap $det $A)
                        (&+ (&sqr (&abs $a))
                            (&sqr (&abs $b))))
                     (tu0 (&- (OverBar $b))
                          (OverBar $a)))))
    "之前的一般理论告诉我们" (&!= $alpha_2 $0) "当且仅当"
    (&cm $beta_1 $beta_2) "线性无关. 另一方面, " $alpha_2
    "的公式告诉我们" (&!= $alpha_2 $0) "当且仅当"
    (&!= (ap $det $A) $0) ".")
   (P "从本质上说, Gram-Schmidt过程就是不断重复应用一种被称为"
      "正交投影的基本几何操作. 并且, 从这一角度理解正交化过程"
      "最为恰当. 在解决近似问题时, 正交投影也会自然出现.")
   (P "设" $W "是内积空间" $V "的一个子空间, 令" $beta "是"
      $V "中的任意一个向量. 我们的问题在于找出" $W
      "中对于" $beta "的最佳的可能近似. 这意味着在向量"
      $alpha "属于" $W "的限制下寻找使得"
      (&norm (&- $beta $alpha))
      "尽可能小的向量" $alpha
      ". 让我们用更加精确的语言来陈述这件事情.")
   (P "以" $W "中的向量对于" $beta "进行的"
      (B "最佳近似") "是这样一个向量"
      (∈ $alpha $W) ", 其满足对于每个向量" (∈ $gamma $W)
      ", 我们都有"
      (MB (&<= (&norm (&- $beta $alpha))
               (&norm (&- $beta $gamma))) "."))
   (P "通过检视这个问题在" $RR^2 "或者" $RR^3
      "中的情况, 读者从直觉上可以感受到以" $W
      "的向量对于" $beta "的最佳近似应该是使得"
      (&- $beta $alpha) "垂直 (或者说正交) 于"
      $W "的向量" $alpha ". 而且, 这样的" $alpha
      "应该恰只有一个. 这些直觉性的想法对于有限维子空间是"
      "正确的, 而仅对于部分而不是全部的无限维子空间成立. "
      "鉴于精确的情况太过复杂而难以在这里处理, "
      "我们将只证明以下的结果.")
   ((theorem #:n "4")
    "令" $W "是内积空间" $V "的一个子空间, 并设"
    $beta "是" $V "中的一个向量."
    (Ol #:attr* '((type "i"))
        (Li "向量" (∈ $alpha $W) "是以" $W
            "中的向量对于" $beta
            "进行的最佳近似当且仅当"
            (&- $beta $alpha) "正交于"
            $W "中的每个向量.")
        (Li "如果以" $W "的向量对于" $beta
            "进行的最佳近似存在, 那么其是唯一的.")
        (Li "如果" $W "是有限维的并且"
            (setE $alpha_1 $..h $alpha_n)
            "是" $W "的任意的正交基, 那么向量"
            (MB (&= $alpha
                    (sum (&= $k $1) $n
                         (orthoproj
                          $beta $alpha_k))))
            "是以" $W "的向量对于" $beta
            "的(唯一的)最佳近似.")))
   ((proof)
    "首先, 注意到如果" $gamma "是" $V "中的任意向量, 那么"
    (&= (&- $beta $gamma)
        (&+ (@- $beta $alpha) (@- $alpha $gamma)))
    ", 而且"
    (MB (&= (sqrnorm (&- $beta $gamma))
            (&+ (sqrnorm (&- $beta $alpha))
                (&i* $2 (Re (inner* (&- $beta $alpha)
                                    (&- $alpha $gamma))))
                (sqrnorm (&- $alpha $gamma)))) ".")
    "现在设" (&- $beta $alpha) "正交于" $W
    "中的每个向量, 如果" (∈ $gamma $W) "且"
    (&!= $gamma $alpha) ", 那么既然"
    (∈ (&- $alpha $gamma) $W) ", 我们可以推出"
    (MB (deriv0 (sqrnorm (&- $beta $gamma))
                $=
                (&+ (sqrnorm (&- $beta $alpha))
                    (sqrnorm (&- $alpha $gamma)))
                $>
                (sqrnorm (&- $beta $alpha))))
    "反过来, 设对于每个" (∈ $gamma $W) "有"
    (&>= (&norm (&- $beta $gamma))
         (&norm (&- $beta $alpha)))
    ", 那么根据上面的第一个等式, 这可以推出"
    (MB (&>= (&+ (&i* $2 (Re (inner* (&- $beta $alpha)
                                     (&- $alpha $gamma))))
                 (sqrnorm (&- $alpha $gamma)))
             $0))
    "对于每个" (∈ $gamma $W) "成立. 鉴于"
    (&= (setI (&- $alpha $gamma) (∈ $gamma $W)) $W)
    ", 实际上其等价于"
    (MB (&>= (&+ (&i* $2 (Re (inner* (&- $beta $alpha) $tau)))
                 (sqrnorm $tau)) $0))
    "对于每个" (∈ $tau $W) "成立. 对于非零的" (∈ $tau $W)
    ", 我们可以构造向量"
    (MB (&= $phi
            (∈ (&- (orthoproj (&- $beta $alpha) $tau))
               $W)))
    "代入即得"
    (let ((ϕ (&- (orthoproj (&- $beta $alpha) $tau)))
          (foo (~ (&sqr (&abs (inner* (&- $beta $alpha) $tau)))
                  (sqrnorm $tau))))
      (MB (deriv0
           (&+ (&i* $2 (Re (inner* (&- $beta $alpha) $phi)))
               (sqrnorm $phi))
           $=
           (&+ (&i* $2 (Re (Inner* (&- $beta $alpha) ϕ)))
               (&sqr (dver ϕ)))
           $= (LC -2 foo foo)
           $= (&- foo)
           $>= $0)))
    "于是, " (&= (inner* (&- $beta $alpha) $tau) $0)
    ". 换言之, " (&- $beta $alpha) "正交于" $W
    "中的每个向量. 到目前为止, 我们完成了对于i的证明. 不过, "
    "根据上面的讨论, 若存在" $W "中的向量满足最佳近似的条件, "
    "那么显然至多只有一个这样的向量. 也就是说, ii的确成立." (Br)
    "现在设" $W "是" $V "的一个有限维子空间, 那么我们知道, "
    "根据定理3, " $W "的确拥有正交基. 令"
    (setE $alpha_1 $..h $alpha_n) "是" $W
    "的任意的正交基, 按照iii的方式定义" $alpha
    ". 然后, 根据定理3的证明中的计算, 我们知道" (&- $beta $alpha)
    "正交于每个" $alpha_k ". 换言之, " (&- $beta $alpha)
    "正交于" $W "中的每个向量. 根据已经证明了的i, 我们可以断言"
    $alpha "是以" $W "中的向量对于" $beta "的最佳近似.")
   ((definition)
    "令" $V "是一个内积空间, " $S "是" $V "的一个子集, 那么"
    $S "的" (B "正交补") "被定义为"
    (MB (&= (^ $S $perp)
            (setI (∈ $beta $V)
                  (: "对于每个" (∈ $alpha $S) ",&nbsp;"
                     (&= (inner* $beta $alpha) $0)))) "."))
   (P $V "的正交补是零子空间. 反过来, " (&= (^ (setE $0) $perp) $V)
      ". 如果" $S "是" $V "的任意子集, 那么其正交补" (^ $S $perp)
      "总是" $V "的子空间. 这是因为, 首先" (^ $S $perp)
      "是非空的, 鉴于其总是包含" $0 "; 其次, 每当"
      (∈ $alpha $beta (^ $S $perp)) "而" $c
      "是任意的标量, 对于每个" (∈ $gamma $S) ", 我们有"
      (eqnderiv (inner* (LC $c $alpha $beta) $gamma)
                (LC $c (inner* $alpha $gamma)
                    (inner* $beta $gamma))
                (LC $c $0 $0)
                $0)
      "因而" (∈ (LC $c $alpha $beta) (^ $S $perp))
      ". 在定理4中, 最佳近似" $alpha
      "的特征性质在于其是" $W "中唯一使得"
      (∈ (&- $beta $alpha) (^ $W $perp))
      "的向量.")
   ((definition)
    "每当定理4中的向量" $alpha "存在, 其被称为"
    (B $beta "在" $W "上的正交投影")
    ". 如果" $V "中的每个向量都在" $W
    "上具有正交投影, 那么赋" $V "的向量以其在"
    $W "上的正交投影的确是一个映射, 这被称为"
    (B $V "在" $W "上的正交投影") ".")
   (P "根据定理4, 内积空间在有限维子空间上的正交投影"
      "总是存在的. 但是, 定理4也能推出以下结果.")
   ((corollary)
    "令" $V "是一个内积空间, " $W "是其一个有限维子空间, "
    $E "是" $V "在" $W "上的正交投影, 那么映射"
    (MB (&\|-> $beta (&- $beta (ap $E $beta))))
    "是" $V "在" (^ $W $perp) "上的正交投影.")
   ((proof)
    "对于任意的向量" (∈ $beta $V) ", 根据" $E
    "的定义和定理4, 我们知道"
    (∈ (&- $beta (ap $E $beta)) (^ $W $perp))
    ". 然后, 既然"
    (MB (&= (&- $beta (@- $beta (ap $E $beta)))
            (∈ (ap $E $beta) $W)))
    "而又根据" (^ $W $perp) "的定义, " $W
    "中的向量总是正交于" (^ $W $perp)
    "的每个向量, 于是" (&- $beta (@- $beta (ap $E $beta)))
    "也正交于" (^ $W $perp) "的每个向量. 换言之, "
    (&\|-> $beta (&- $beta (ap $E $beta)))
    "是" $V "在" (^ $W $perp) "上的正交投影.")
   ((example #:n "14")
    "给定装备有标准内积的" $RR^3 ", 那么" (tu0 -10 2 8)
    "在由" (tu0 3 12 -1) "张成的子空间" $W "上的正交投影为"
    (eqnderiv
     $alpha
     (orthoproj (tu0 -10 2 8) (tu0 3 12 -1))
     (&i* (~ -14 154) (tu0 3 12 -1)))
    $RR^3 "在" $W "上的正交投影" $E "为"
    (MB (&\|-> (tu0 $x_1 $x_2 $x_3)
               (&i* (~ (&- (LC $3 $x_1 12 $x_2) $x_3)
                       154) (tu0 3 12 -1))) ".")
    $E "的秩显然为" $1 ", 因而" $E "的零化度为" $2
    ". 另一方面,"
    (MB (&= (ap $E (tu0 $x_1 $x_2 $x_3)) (tu0 0 0 0)))
    "当且仅当" (&= (&- (LC $3 $x_1 12 $x_2) $x_3) 0)
    ", 而这等价于" (∈ (tu0 $x_1 $x_2 $x_3) (^ $W $perp))
    ". 因此, " (^ $W $perp) "是" $E "的零空间, 而"
    (&= (&dim (^ $W $perp)) $2) ". 通过计算"
    (MB (&- (tu0 $x_1 $x_2 $x_3)
            (&i* (~ (&- (LC $3 $x_1 12 $x_2) $x_3)
                    154) (tu0 3 12 -1))))
    "我们知道" $RR^3 "在" (^ $W $perp) "上的正交投影"
    (&- $I $E) "为"
    (MB (&\|-> (tu0 $x_1 $x_2 $x_3)
               (&i* 1/154
                    (tu0 (: (&i* 145 $x_1) $-
                            (&i* 36 $x_2) $+
                            (&i* $3 $x_3))
                         (: (&i* -36 $x_1) $+
                            (&i* 10 $x_2) $+
                            (&i* 12 $x_3))
                         (: (&i* $3 $x_1) $+
                            (&i* 12 $x_2) $+
                            (&i* 153 $x_3)))))))
   (P "例子14中的观察将以如下形式得到泛化.")
   ((theorem #:n "5")
    "令" $W "是内积空间" $V "的一个有限维子空间, 设"
    $E "是" $V "在" $W "上的正交投影, 那么" $E
    "是" $V "上的一个幂等线性算子. 而且, " $W
    "是" $E "的像, " (^ $W $perp) "是" $E
    "的零空间, 于是"
    (MB (&= $V (&d+ $W (^ $W $perp))) "."))
   ((proof)
    "对于每个" (∈ $beta $V) ", 既然" (∈ (ap $E $beta) $W)
    ", 那么" (&= (app $E (ap $E $beta)) (ap $E $beta))
    "是显然的. 换言之, " (&= $E^2 $E) ", 即" $E
    "是幂等的. 现在我们需要证明" $E "是线性的. 对于"
    (∈ $alpha $beta $V) ", 我们知道"
    (∈ (&- $alpha (ap $E $alpha))
       (&- $beta (ap $E $beta))
       (^ $W $perp))
    ". 设" $c "是任意的标量, 那么"
    (MB (&= (LC $c (@- $alpha (ap $E $alpha))
                (@- $beta (ap $E $beta)))
            (∈ (&- (@LC $c $alpha $beta)
                   (@LC $c (ap $E $alpha) (ap $E $beta)))
               (^ $W $perp))))
    "其中" (∈ (LC $c (ap $E $alpha) (ap $E $beta)) $W)
    ". 换言之, 即"
    (&= (app $E (LC $c $alpha $beta))
        (LC $c (ap $E $alpha) (ap $E $beta)))
    ", 由此" $E "是线性算子." (Br)
    "只需稍微检视一下正交投影的定义, 便可知" $E
    "的像是" $W ". 另外, 根据定理4的推论, "
    (&- $I $E) "是" $V "在" (^ $W $perp)
    "上的正交投影. 而且, " (&- $I $E) "的像是" (^ $W $perp)
    ". 现在让我们回忆一下第6章的定理9及其之前的讨论, 就知道"
    $E "的零空间是" (^ $W $perp) ", 于是"
    (&= $V (&d+ $W (^ $W $perp))) ".")
   ((corollary)
    "在定理5的条件下, " (&- $I $E) "是" $V
    "在" (^ $W $perp) "上的正交投影. 而且, "
    (&- $I $E) "是" $V "上的幂等线性算子, 其以"
    (^ $W $perp) "为像而" $W "为零空间.")
   ((tcomment)
    "对于定理4的推论还有定理5及其推论而言, " $W
    "是有限维子空间的条件并不是必要的, 只是为了"
    "确保正交投影的存在性. 实际上, 若" $V "在" $W
    "上的正交投影的确存在, 那么这些命题依旧成立.")
   (P "现在我们可以按照如下方式几何地陈述Gram-Schmidt过程了. "
      "给定内积空间" $V "和线性无关的向量" (&..cm $beta_1 $beta_n)
      ", 令" (&cm $P_k (&> $k $1)) "是" $V
      "在由" (&..cm $beta_1 (_ $beta (&- $k $1)))
      "张成的子空间的正交补上的正交投影, 并设"
      (&= $P_1 $I) ", 那么应用正交化过程于"
      (&..cm $beta_1 $beta_n) "得到的向量"
      (&..cm $alpha_1 $alpha_n) "可由"
      (MB (&cm (&= $alpha_k (ap $P_k $beta_k))
               (&<= $1 $k $n)))
      "定义.")
   (P "定理5也可以推出所谓的" (B "Bessel不等式") ".")
   ((corollary)
    "令" (setE $alpha_1 $..h $alpha_n) "是内积空间" $V
    "中由非零向量构成的正交集合, 如果" (∈ $beta $V) ", 那么"
    (MB (&<= (sum (&= $k $1) $n
                  (~ (&sqr (&abs (inner* $beta $alpha_k)))
                     (sqrnorm $alpha_k)))
             (sqrnorm $beta)) ".")
    "并且, 此不等式取得等号当且仅当"
    (MB (&= $beta
            (sum (&= $k $1) $n
                 (orthoproj $beta $alpha_k))) "."))
   ((proof)
    "设" $W "是由" (&..cm $alpha_1 $alpha_n)
    "张成的子空间, 那么"
    (MB (&= $gamma
            (∈ (sum (&= $k $1) $n
                    (orthoproj $beta $alpha_k))
               $W)))
    "是以" $W "中的向量对于" $beta
    "的最佳近似. 并且, 若令" (&= $delta (&- $beta $gamma))
    ", 则" (∈ $delta (^ $W $perp))
    ", 因而" (&= (inner* $gamma $delta) $0) ", 故"
    (MB (deriv0
         (sqrnorm $beta)
         $=
         (&+ (sqrnorm $gamma) (sqrnorm $delta))
         $=
         (&+ (Inner* (sum (&= $k $1) $n
                          (orthoproj $beta $alpha_k))
                     (sum (&= $k $1) $n
                          (orthoproj $beta $alpha_k)))
             (sqrnorm $delta))
         $=
         (&+ (@sum (&= $k $1) $n
                   (~ (&sqr (&abs (inner* $beta $alpha_k)))
                      (sqrnorm $alpha_k)))
             (sqrnorm $delta))
         $>=
         (sum (&= $k $1) $n
              (~ (&sqr (&abs (inner* $beta $alpha_k)))
                 (sqrnorm $alpha_k)))))
    "显然, 此不等式取得等号当且仅当" (&= (sqrnorm $delta) $0)
    ", 即" (&= $beta $gamma) ". 证明就结束了.")
   ((tcomment)
    "Bessel不等式取得等号的一个等价条件为" $beta "在由"
    (&..cm $alpha_1 $alpha_n) "张成的子空间之中.")
   (P "在" (setE $alpha_1 $..h $alpha_n)
      "为规范正交集的特殊情况下, Bessel不等式就变成了"
      (MB (&<= (sum (&= $k $1) $n
                    (&sqr (&abs (inner* $beta $alpha_k))))
               (sqrnorm $beta)) ".")
      "当然, 若" (setE $alpha_1 $..h $alpha_n) "是"
      $V "的一个规范正交基, 那么Bessel不等式总是取等号, 而此时"
      $beta "在有序基" (setE $alpha_1 $..h $alpha_n)
      "下的坐标的第" $k "个分量为"
      (inner* $beta $alpha_k) ".")
   ((example #:n "15")
    "若我们将上述推论应用于例子11中所描述的规范正交集合, 就会发现"
    (Ol #:attr* '((type "a"))
        (Li (MB (&<= (sum (&= $k (&- $n)) $n
                          (&sqr
                           (&Abs
                            (uintegral
                             (&i* (app $f $t)
                                  (^ $e (&- (&i* $2 $pi $i $k $t))))))))
                     (uintegral
                      (&sqr (&abs (app $f $t)))))))
        (Li (MB (&= (uintegral
                     (&sqr
                      (&Abs
                       (sum (&= $k (&- $n)) $n
                            (&i* $c_k
                                 (^ $e (&i* $2 $pi $i $k $t)))))))
                    (sum (&= $k (&- $n)) $n
                         (&sqr (&abs $c_k))))))
        (Li (MB (&= (uintegral
                     (&sqr
                      (@LC (Msqrt $2) (&cos (&i* $2 $pi $t))
                           (Msqrt $2) (&sin (&i* $4 $pi $t)))))
                    (&+ $1 $1) $2)))))
   ((exercise #:n "1")
    "考虑装备了标准内积的" $RR^4 ", 令子空间"
    (MB (&= $W (setI (∈ $gamma $RR^4)
                     (: (&= (inner* $gamma $alpha) $0) "且"
                        (&= (inner* $gamma $beta) $0)))))
    "其中" (&= $alpha (tu0 1 0 -1 1)) "而"
    (&= $beta (tu0 2 3 -1 2)) ", 找出" $W "的一个基.")
   ((exercise #:n "2")
    "应用Gram-Schmidt过程于向量"
    (&= $beta_1 (tu0 1 0 1)) ", "
    (&= $beta_2 (tu0 1 0 -1)) ", "
    (&= $beta_3 (tu0 0 3 4))
    "以得到装备有标准内积的" $RR^3 "的一个规范正交基.")
   ((exercise #:n "3")
    "考虑装备有标准内积的" $CC^3 ", 找出由"
    (&= $beta_1 (tu0 $1 $0 $i)) "和"
    (&= $beta_2 (tu0 $2 $1 (&+ $1 $i)))
    "张成的子空间的一个规范正交基.")
   ((exercise #:n "4")
    "令" $V "是一个内积空间, 两个向量" $alpha "和" $beta
    "之间的" (B "距离") "由"
    (MB (&= (appl $d $alpha $beta)
            (&norm (&- $alpha $beta))))
    "定义, 证明"
    (Ol #:attr* '((type "a"))
        (Li (&>= (appl $d $alpha $beta) $0) ";")
        (Li (&= (appl $d $alpha $beta) $0)
            "当且仅当" (&= $alpha $beta) ";")
        (Li (&= (appl $d $alpha $beta)
                (appl $d $beta $alpha)) ";")
        (Li (&<= (appl $d $alpha $beta)
                 (&+ (appl $d $alpha $gamma)
                     (appl $d $gamma $beta))) ".")))
   ((exercise #:n "5")
    "令" $V "是一个内积空间而" (∈ $alpha $beta $V)
    ", 那么" (&= $alpha $beta) "当且仅当对于每个"
    (∈ $gamma $V) "有"
    (&= (inner* $alpha $gamma)
        (inner* $beta $gamma)) ".")
   ((exercise #:n "6")
    "给定装备有标准内积的" $RR^2 ", 令" $W
    "是由" (tu0 $3 $4) "张成的子空间, " $E
    "是" $RR^2 "在" $W "上的正交投影, 找出"
    (Ol #:attr* '((type "a"))
        (Li (appl $E $x_1 $x_2) "的公式;")
        (Li "标准有序基下" $E "的矩阵;")
        (Li (^ $W $perp) ";")
        (Li "使得" $E "由矩阵"
            (MB (Mat (1 0) (0 0)))
            "表示的一个规范正交基.")))
   ((exercise #:n "7")
    "令" $V "是一个内积空间, 其向量空间为" $RR^2 ", 而其内积的二次形式由"
    (MB (&= (sqrnorm (tu0 $x_1 $x_2))
            (&+ (&sqr (@- $x_1 $x_2))
                (&i* $3 (_^ $x $2 $2)))))
    "定义. 令" $E "是" $V "在由" (tu0 $3 $4) "张成的子空间"
    $W "上的正交投影, 现在回答练习6的四个问题.")
   ((exercise #:n "8")
    "找出" $RR^2 "上的一个内积使得"
    (&= (inner* $epsilon_1 $epsilon_2) $2) ".")
   ((exercise #:n "9")
    "令" $V "是" (&poly $RR) "的次数至多为" $3
    "的多项式构成的子空间, 其上装备的内积为"
    (MB (&= (inner* $f $g)
            (uintegral
             (&i* (app $f $t) (app $g $t)))) ".")
    (Ol #:attr* '((type "a"))
        (Li "找出由所有标量多项式构成的子空间的正交补.")
        (Li "应用Gram-Schmidt过程于基"
            (setE $1 $x $x^2 $x^3) ".")))
   ((exercise #:n "10")
    "令" $V "是向量空间" (^ $CC n*n) ", 设其上的内积为"
    (&= (inner* $A $B) (&tr (&i* $A (&* $B))))
    ", 找出由所有对角矩阵构成的子空间的正交补.")
   ((exercise #:n "11")
    "令" $V "是一个有限维内积空间, "
    (setE $alpha_1 $..h $alpha_n)
    "是" $V "的一个规范正交基, 证明对于任意的"
    (∈ $alpha $beta $V) ", 我们都有"
    (MB (&= (inner* $alpha $beta)
            (sum (&= $k $1) $n
                 (&i* (inner* $alpha $alpha_k)
                      (OverBar
                       (inner* $beta $alpha_k))))) "."))
   ((exercise #:n "12")
    "令" $W "是内积空间" $V "的一个有限维子空间, " $E
    "是" $V "在" $W "上的正交投影, 证明对于所有"
    (∈ $alpha $beta $V) ", "
    (&= (inner* (ap $E $alpha) $beta)
        (inner* $alpha (ap $E $beta))) ".")
   ((exercise #:n "13")
    "令" $S "是内积空间" $V "的一个子集. 证明"
    (&perp (@perp $S)) "包含由" $S
    "张成的子空间. 当" $V "是有限维的时候, 证明"
    (&perp (@perp $S)) "就是由" $S
    "张成的子空间.")
   ((exercise #:n "14")
    "令" $V "是一个有限维内积空间而" basis:def
    "是" $V "的一个规范正交基. 令" $T "是" $V
    "上的一个线性算子而" $A "是在有序基" $BBB
    "下的矩阵. 证明"
    (MB (&= (mref $A $i $j)
            (inner* (ap $T $alpha_j)
                    $alpha_i)) "."))
   ((exercise #:n "15")
    "设" (&= $V (&d+ $W_1 $W_2)) "而"
    $f_1 "和" $f_2 "分别是" $W_1 "和"
    $W_2 "上的内积. 证明存在唯一的" $V
    "上的内积" $f "使得"
    (Ol #:attr* '((type "a"))
        (Li (&= $W_2 (_^ $W $1 $perp)) ";")
        (Li "对于"
            (&cm (∈ $alpha $beta $W_k)
                 (&= $k (&cm $1 $2)))
            ", 有"
            (&= (appl $f $alpha $beta)
                (appl $f_k $alpha $beta)) ".")))
   ((exercise #:n "16")
    "令" $V "是一个内积空间而" $W "是" $V
    "的一个有限维子空间, 一般存在许多以" $W
    "为像的投影. 其中一种当然是" $W
    "上的正交投影, 它具有对于每个" (∈ $alpha $V)
    ", " (&<= (&norm (ap $E $alpha)) (&norm $alpha))
    "的性质. 证明如果" $E "是一个以" $W
    "为像的投影且对于每个" (∈ $alpha $V) "有"
    (&<= (&norm (ap $E $alpha)) (&norm $alpha))
    ", 那么" $E "是" $W "上的正交投影. "
    "[译注: 这个不等式和Bessel不等式差不多.]")
   ((exercise #:n "17")
    "令" $V "是一个实内积空间, 其由区间" (li0 -1 1)
    "上的所有连续实值函数构成, 而内积为"
    (MB (&= (inner* $f $g)
            (integral $-1 $1
                      (&i* (app $f $t)
                           (app $g $t))
                      $t)) ".")
    "令" $W "是所有奇函数构成子空间, 找出" $W
    "的正交补.")
   (H3 "第8.3节 线性泛函和伴随")
   (P "本节的第一部分处理内积空间上的线性泛函以及其与内积的关系. "
      "基本的结果在于有限维内积空间上任意的线性泛函" $f
      "就是&quot;固定一个向量的内积&quot;, 即对于某个固定的"
      (∈ $beta $V) ", " $f "具有"
      (&= (app $f $alpha) (inner* $alpha $beta))
      "的形式. 我们使用这个结果证明了" $V "上的线性算子" $T
      "的&quot;伴随&quot;的存在性, 其是一个对于每个"
      (∈ $alpha $beta $V) "有"
      (&= (inner* (ap $T $alpha) $beta)
          (inner* $alpha (ap (&* $T) $beta)))
      "的线性算子" (&* $T) ". 通过规范正交基的使用, 线性算子上的伴随操作 (从"
      $T "到" (&* $T) ") 就相当于构造一个矩阵的共轭转置. 我们稍微探索了一下"
      "伴随操作和复数的共轭之间的类似之处.")
   (P "令" $V "是任意的内积空间, " (∈ $beta $V)
      "是一个固定的向量, 我们定义从" $V
      "到标量域的函数" $f_beta "为"
      (MB (&= (app $f_beta $alpha)
              (inner* $alpha $beta)) ".")
      "函数" $f_beta "是" $V "上的一个线性泛函, 这是因为根据内积的定义, "
      (inner* $alpha $beta) "作为" $alpha "的函数是线性的. 如果"
      $V "是有限维的, 那么" $V "上的每个线性泛函都可由某个" $beta
      "以这种方式产生.")
   ((theorem #:n "6")
    "令" $V "是一个有限维内积空间, 而" $f "是" $V
    "上的一个线性泛函, 那么存在唯一的向量" (∈ $beta $V)
    "使得对于每个" (∈ $alpha $V) "有"
    (&= (app $f $alpha) (inner* $alpha $beta)) ".")
   ((proof)
    "令" (setE $alpha_1 $alpha_2 $..h $alpha_n) "是" $V
    "的一个规范正交基, 置"
    (MB (&= $beta
            (sum (&= $j $1) $n
                 (&i* (OverBar (app $f $alpha_j))
                      $alpha_j))))
    "令" $f_beta "是由"
    (MB (&= (app $f_beta $alpha) (inner* $alpha $beta)))
    "定义的线性泛函, 那么"
    (MB (&= (app $f_beta $alpha_k)
            (Inner* $alpha_k
                    (sum (&= $j $1) $n
                         (&i* (OverBar (app $f $alpha_j))
                              $alpha_j)))
            (app $f $alpha_k)) ".")
    "既然这对于每个基向量" $alpha_k "成立, 于是" (&= $f $f_beta)
    ". 现在设" (∈ $gamma $V) "满足" (&= $f_gamma $f) ", 那么"
    (eqnderiv (&- (app $f_gamma (&- $gamma $beta))
                  (app $f_beta (&- $gamma $beta)))
              (&- (inner* (&- $gamma $beta) $gamma)
                  (inner* (&- $gamma $beta) $beta))
              (inner* (&- $gamma $beta) (&- $gamma $beta))
              $0)
    "换言之, " (&= (&- $gamma $beta) $0) ", 即"
    (&= $gamma $beta) ". 因此, 恰存在一个向量" $beta
    "按照以上陈述的方式确定了线性泛函" $f ".")
   (P "这个证明可以使用基下的线性泛函的表示的语言稍微重述一下. "
      "如果我们选定了" $V "的一个规范正交基"
      (setE $alpha_1 $..h $alpha_n)
      ", 那么" (&= $alpha (LC0 $x_1 $alpha_1 $x_n $alpha_n))
      "和" (&= $beta (LC0 $y_1 $alpha_1 $y_n $alpha_n))
      "的内积为"
      (MB (&= (inner* $alpha $beta)
              (&..+ (&i* $x_1 (_ (OverBar $y) $1))
                    (&i* $x_n (_ (OverBar $y) $n)))) ".")
      "如果" $f "是" $V "上任意的线性泛函, 那么" $f "具有"
      (MB (&= (app $f $alpha)
              (LC0 $c_1 $x_1 $c_n $x_n)))
      "的形式, 其中" (&..cm $c_1 $c_n)
      "是由基确定的一些固定标量. 当然, "
      (&= $c_j (app $f $alpha_j))
      ". 如果我们希望找到一个向量" (∈ $beta $V)
      "使得对于每个" $alpha "有"
      (&= (inner* $alpha $beta) (app $f $alpha))
      ", 那么显然" $beta "的坐标分量" $y_j
      "必须满足" (&= (_ (OverBar $y) $j) $c_j)
      ", 或者说" (&= $y_j (OverBar (app $f $alpha_j)))
      ". 据此, 可知"
      (MB (&= $beta
              (LC0 (OverBar (app $f $alpha_1)) $alpha_1
                   (OverBar (app $f $alpha_n)) $alpha_n)))
      "就是我们所要的向量.")
   (P "现在应该作出一些更加深刻的评注. 刚才我们所给出的"
      "对于定理6的证明相当简短, 然而它却没能强调一个"
      "基本的几何事实, 即" $beta "位于" $f
      "的零空间的正交补之中. 令" $W "是" $f
      "的零空间, 那么" (&= $V (&d+ $W (&perp $W)))
      ", 并且" $f "完全由其在" (&perp $W)
      "上的值所确定. 实际上, 如果" $P "是" $V "在"
      (&perp $W) "上的正交投影, 那么"
      (MB (&= (app $f $alpha)
              (app $f (ap $P $alpha))))
      "对于每个" (∈ $alpha $V) "成立. 设"
      (&!= $f $0) ", 那么" $f "的秩为" $1 "而"
      (&= (&dim (&perp $W)) $1) ". 如果" $gamma
      "是" (&perp $W) "中任意的非零向量, 那么"
      (MB (&= (ap $P $alpha)
              (orthoproj $alpha $gamma)))
      "对于所有" (∈ $alpha $V) "成立, 因此"
      (eqnderiv (app $f $alpha)
                (app $f (ap $P $alpha))
                (ap $f (pare (orthoproj $alpha $gamma)))
                (&i* (inner* $alpha $gamma)
                     (~ (app $f $gamma) (sqrnorm $gamma)))
                (Inner* $alpha
                        (&i* (~ (OverBar (app $f $gamma))
                                (sqrnorm $gamma))
                             $gamma)))
      "换言之, "
      (&= $beta (&i* (bra0 (&/ (OverBar (app $f $gamma))
                               (sqrnorm $gamma)))
                     $gamma)) ".")
   ((tcomment)
    "前一段的一些基本事实(对于像我这样不够聪明的读者)值得澄清. "
    "首先, 之所以" $beta "位于" $f "的零空间的正交补之中, "
    "是因为若" (&= (app $f $alpha) $0) ", 那么"
    (&= (inner* $alpha $beta) $0) ", 即" $beta
    "正交于" $f "的零空间的每个向量. 其次, 之所以"
    (&= (app $f $alpha) (app $f (ap $P $alpha)))
    ", 是因为根据" $P "的定义, " (&- $alpha (ap $P $alpha))
    "正交于" (&perp $W) "的每个向量, 而我们知道"
    (∈ $beta (&perp $W)) ", 于是就有"
    (eqnderiv
     (inner* (&- $alpha (ap $P $alpha)) $beta)
     (&- (inner* $alpha $beta)
         (inner* (ap $P $alpha) $beta))
     (&- (app $f $alpha) (app $f (ap $P $alpha)))
     $0)
    "即" (&= (app $f $alpha) (app $f (ap $P $alpha))) ".")
   ((example #:n "16")
    "我们应该给出一个例子以表明定理6若缺少" $V
    "是有限维空间的条件则并不成立. 令" $V
    "是复数域上的多项式的向量空间, 而内积为"
    (MB (&= (inner* $f $g)
            (uintegral
             (&i* (app $f $t)
                  (OverBar (app $g $t))))) ".")
    "这个内积也可以被代数地定义. 如果"
    (&= $f (sum (&= $j $0) $l (&i* $a_j $x^j))) "而"
    (&= $g (sum (&= $k $0) $m (&i* $b_k $x^k))) ", 那么"
    (MB (&= (inner* $f $g)
            (sum (&= $j $0) $l
                 (sum (&= $k $0) $m
                      (~ (&i* $a_j (_ (OverBar $b) $k))
                         (&+ $j $k $1))))) ".")
    "令" $z "是一个固定的复数, " $L "是&quot;在" $z
    "处求值&quot;的线性泛函:"
    (MB (&= (app $L $f) (app $f $z)) ".")
    "存在一个多项式" $g "使得对于每个" $f "有"
    (&= (inner* $f $g) (app $L $f))
    "吗? 答案是否定的, 以下是我们的推理. 设存在多项式" $g "满足"
    (MB (&= (app $f $z) (uintegral
                         (&i* (app $f $t)
                              (OverBar (app $g $t))))))
    "对于每个多项式" $f "成立. 令" (&= $h (&- $x $z))
    ", 那么对于任意的" $f "我们有"
    (&= (app (@i* $h $f) $z) $0) ", 于是"
    (MB (&= $0 (uintegral
                (&i* (app $h $t) (app $f $t)
                     (OverBar (app $g $t))))))
    "特别地, 这个等式在" (&= $f (&i* (OverBar $h) $g))
    "时也成立, 以至于"
    (eqnderiv
     (uintegral
      (&i* (app $h $t) (app (@i* (OverBar $h) $g) $t)
           (OverBar (app $g $t))))
     (uintegral
      (&i* (&sqr (&abs (app $h $t)))
           (&sqr (&abs (app $g $t)))))
     (uintegral
      (&sqr (&abs (app (@i* $h $g) $t))))
     (inner* (&i* $h $g) (&i* $h $g))
     $0)
    "这可以推出" (&= (&i* $h $g) $0)
    ". 鉴于" (&!= $h $0) ", 必然有" (&= $g $0)
    ". 可是, " $L "并非零线性泛函, 即这样的"
    $g "不存在.")
   ((tcomment)
    "以上的" (OverBar $h) "是对于" $h
    "的每个系数作复共轭得到的多项式. 在"
    $t "为实数的情况下, "
    (&= (app (OverBar $h) $t)
        (OverBar (app $h $t))) ".")
   (P "读者可以稍微推广一下这个例子. 设我们选定了标量"
      (&..cm $c_1 $c_n) "和不同的复数"
      (&..cm $z_1 $z_n) ", 令"
      (MB (&= (app $L $f)
              (LC0 $c_1 (app $f $z_1)
                   $c_n (app $f $z_n))))
      "那么" $L "是" $V "上的一个线性泛函, 但是除非"
      (&= $c_1 $c_2 $..c $c_n $0)
      ", 并不存在多项式" $g "使得"
      (&= (app $L $f) (inner* $f $g))
      ". 读者只需重复上述的论证以"
      (&= $h (&..i* (@- $x $z_1) (@- $x $z_n))) ".")
   (P "现在我们将注意力转到线性算子的伴随的概念上来.")
   ((theorem #:n "7")
    "对于有限维内积空间" $V "上任意的线性算子" $T
    ", 存在唯一的" $V "上的线性算子" (&* $T)
    "使得对于每个" (∈ $alpha $beta $V) "有"
    (MB (&= (inner* (ap $T $alpha) $beta)
            (inner* $alpha (ap (&* $T) $beta))) "."))
   ((proof)
    "令" $beta "是" $V "中任意的一个向量, 那么"
    (&\|-> $alpha (inner* (ap $T $alpha) $beta))
    "是" $V "上的一个线性泛函. 根据定理6, 存在唯一的"
    (∈ $beta^ $V) "使得对于每个" (∈ $alpha $V) "有"
    (&= (inner* (ap $T $alpha) $beta) (inner* $alpha $beta^))
    ". 令" (&* $T) "是映射" (&\|-> $beta $beta^) ", 我们知道"
    (&= (inner* (ap $T $alpha) $beta)
        (inner* $alpha (ap (&* $T) $beta)))
    "对于所有" (∈ $alpha $beta $V)
    "成立, 那么剩下来的工作就是要验证" (&* $T)
    "的确是一个线性算子. 令" (∈ $beta $gamma $V) "而"
    $c "是一个标量, 对于任意的" $alpha ", 我们有"
    (eqnderiv
     (inner* $alpha (app (&* $T) (LC $c $beta $gamma)))
     (inner* (ap $T $alpha) (LC $c $beta $gamma))
     (LC (OverBar $c) (inner* (ap $T $alpha) $beta)
         (inner* (ap $T $alpha) $gamma))
     (LC (OverBar $c) (inner* $alpha (ap (&* $T) $beta))
         (inner* $alpha (ap (&* $T) $gamma)))
     (inner* $alpha
             (LC $c (ap (&* $T) $beta)
                 (ap (&* $T) $gamma))))
    "因此, "
    (&= (app (&* $T) (LC $c $beta $gamma))
        (LC $c (ap (&* $T) $beta) (ap (&* $T) $gamma)))
    ", 即" (&* $T) "是线性的." (Br)
    (&* $T) "的唯一性是显然的. 对于任意的向量" (∈ $beta $V)
    ", 向量" (ap (&* $T) $beta) "由以下条件所唯一刻画:"
    (MB "对于每个" (∈ $alpha $V) ",&nbsp;"
        (&= (inner* (ap $T $alpha) $beta)
            (inner* $alpha (ap (&* $T) $beta))) "."))
   ((tcomment)
    "以上证明的写法在某种意义上有些颠倒. 实际上, "
    "根据定理6, 满足条件的" (&* $T) "至多只有一个. "
    "接着, 我们仅需要验证这个由定理6确定的映射"
    "的确是我们所要的线性算子即可.")
   ((theorem #:n "8")
    "令" $V "是一个有限维内积空间而" basis:def
    "是" $V "的一个(有序)规范正交基, 令" $T "是" $V
    "上的一个线性算子而" $A "是在有序基" $BBB
    "下的矩阵, 那么"
    (&= (mref $A $k $j)
        (inner* (ap $T $alpha_j) $alpha_k)) ".")
   ((proof)
    "既然" $BBB "是一个规范正交基, 我们有"
    (MB (&= $alpha
            (sum (&= $k $1) $n
                 (&i* (inner* $alpha $alpha_k)
                      $alpha_k))) ".")
    "鉴于矩阵" $A "由"
    (MB (&= (ap $T $alpha_j)
            (sum (&= $k $1) $n
                 (&i* (mref $A $k $j)
                      $alpha_k))))
    "定义, 而"
    (MB (&= (ap $T $alpha_j)
            (sum (&= $k $1) $n
                 (&i* (inner* (ap $T $alpha_j)
                              $alpha_k)
                      $alpha_k))))
    "我们有"
    (&= (mref $A $k $j)
        (inner* (ap $T $alpha_j) $alpha_k)) ".")
   ((corollary)
    "令" $V "是一个有限维内积空间而" $T "是" $V
    "上的一个线性算子, 那么在" $V "的任意的规范正交基下, "
    (&* $T) "的矩阵是" $T "的矩阵的共轭转置.")
   ((proof)
    "令" basis:def "是" $V "的一个规范正交基, 设"
    (&= $A (coordm $T)) "而" (&= $B (coordm (&* $T)))
    ". 根据定理6, 我们有"
    (MB (&= (mref $A $k $j)
            (inner* (ap $T $alpha_j) $alpha_k))
        "和"
        (&= (mref $B $k $j)
            (inner* (ap (&* $T) $alpha_j) $alpha_k)) ".")
    "根据" (&* $T) "的定义, 可以推出"
    (eqnderiv
     (mref $B $k $j)
     (inner* (ap (&* $T) $alpha_j) $alpha_k)
     (OverBar (inner* $alpha_k (ap (&* $T) $alpha_j)))
     (OverBar (inner* (ap $T $alpha_k) $alpha_j))
     (mref (OverBar $A) $j $k)))
   ((example #:n "17")
    "令" $V "是一个有限维内积空间, " $E "是" $V
    "在其一个子空间" $W "上的正交投影, 那么对于任意的"
    (∈ $alpha $beta $V) ", 我们可以推出"
    (eqnderiv
     (inner* (ap $E $alpha) $beta)
     (inner* (ap $E $alpha)
             (&+ (ap $E $beta)
                 (ap (@- $I $E) $beta)))
     (inner* (ap $E $alpha) (ap $E $beta))
     (inner* (&+ (ap $E $alpha)
                 (ap (@- $I $E) $alpha))
             (ap $E $beta))
     (inner* $alpha (ap $E $beta)))
    "根据算子" (&* $E) "的唯一性, 我们知道"
    (&= (&* $E) $E) ". 现在考虑例子14所描述的投影, 那么"
    (MB (&= $A (&i* 1/154
                    (MatR (9 36 -3)
                          (36 144 -12)
                          (-3 -12 1)))))
    "是" $E "在标准规范正交基下的矩阵. 根据之前的推论, "
    "应该有" (&= (&* $A) $A) ", 的确如此. 另一方面, 设"
    (eqn*
     ($alpha_1 $= (tu0 154 0 0))
     ($alpha_2 $= (tu0 145 -36 3))
     ($alpha_3 $= (tu0 -36 10 12)))
    "那么" (setE $alpha_1 $alpha_2 $alpha_3)
    "是一个基, 并且"
    (eqn*
     ((ap $E $alpha_1) $= (tu0 9 36 -3))
     ((ap $E $alpha_2) $= (tu0 0 0 0))
     ((ap $E $alpha_3) $= (tu0 0 0 0)))
    "既然"
    (&= (tu0 9 36 -3) (&- (tu0 154 0 0) (tu0 145 -36 3)))
    ", " $E "在基" (setE $alpha_1 $alpha_2 $alpha_3)
    "下的矩阵为"
    (MB (&= $B (MatR (1 0 0) (-1 0 0) (0 0 0))) ".")
    "在这种情况下, " (&!= (&* $B) $B) ", 而且" (&* $B)
    "也不是" (&* $E) "在基" (setE $alpha_1 $alpha_2 $alpha_3)
    "下的矩阵. 应用以上推论, 我们可以得出"
    (setE $alpha_1 $alpha_2 $alpha_3)
    "不是规范正交基. 当然, 这不论如何都是很显然的.")
   ((definition)
    "令" $T "是内积空间" $V "上的一个线性算子, 那么我们称"
    (B $T "在" $V "上具有一个伴随") ", 如果存在" $V
    "上的一个线性算子" (&* $T) "使得"
    (&= (inner* (ap $T $alpha) $beta)
        (inner* $alpha (ap (&* $T) $beta)))
    "对于所有" (∈ $alpha $beta $V) "成立.")
   (P "根据定理7, 有限维内积空间" $V "上的每个线性算子"
      $T "都在" $V "上具有伴随. 在无限维的情形下, "
      "并不总是如此. 但是, 不论如何, 至多只有一个这样的算子"
      (&* $T) ". 当它存在时, 我们将其称为" $T "的"
      (B "伴随") ".")
   (P "关于有限维的情形, 有两点评注值得一说."
      (Ol (Li $T "的伴随不仅依赖于" $T ", 也依赖于内积的定义.")
          (Li "正如例子17所显示的那样, 对于任意而非规范正交的有序基"
              $BBB ", " (coordm $T) "和" (coordm (&* $T))
              "之间的关系要比以上推论所描述的更加复杂.")))
   ((example #:n "18")
    "令" $V "是" (^ $CC n*1) "而内积为"
    (&= (inner* $X $Y) (&i* (&* $Y) $X))
    ". 如果" (∈ $A (^ $CC n*n)) ", 那么线性算子"
    (&\|-> $X (&i* $A $X)) "的伴随是线性算子"
    (&\|-> $X (&i* (&* $A) $X)) ", 因为"
    (MB (&= (inner* (&i* $A $X) $Y)
            (&i* (&* $Y) $A $X)
            (&i* (&* (@i* (&* $A) $Y)) $X)
            (inner* $X (&i* (&* $A) $Y))) ".")
    "读者应该发现这是前述推论的一个特殊情形.")
   ((example #:n "19")
    "这个例子类似于例子18. 令" $V "是" (^ $CC n*n) "而内积为"
    (&= (inner* $A $B) (&tr (&i* $A (&* $B))))
    ". 令" (∈ $M (^ $CC n*n)) ", 那么左乘" $M "的伴随是左乘"
    (&* $M) ". 当然, 左乘" $M "指的是线性算子"
    (&= (app $L_M $A) (&i* $M $A)) "."
    (eqnderiv
     (inner* (app $L_M $A) $B)
     (&tr (&i* $M $A (&* $B)))
     (&tr (&i* $A (&* $B) $M))
     (&tr (&i* $A (&* (@i* (&* $M) $B))))
     (inner* $A (app (_ $L (&* $M)) $B)))
    "因此, " (&= (&* (@ $L_M)) (_ $L (&* $M)))
    ". 以上计算中, 我们用到了迹函数的一个特有性质: "
    (&= (&tr (&i* $A $B)) (&tr (&i* $B $A))) ".")
   ((tcomment)
    "本书还没有证明过" (&= (&tr (&i* $A $B)) (&tr (&i* $B $A)))
    ", 现在我们来证明一下:"
    (eqnderiv
     (&tr (&i* $A $B))
     (sum (&= $j $1) $n
          (mref (@i* $A $B) $j $j))
     (sum (&= $j $1) $n
          (sum (&= $k $1) $n
               (&i* (mref $A $j $k) (mref $B $k $j))))
     (sum (&= $k $1) $n
          (sum (&= $j $1) $n
               (&i* (mref $B $k $j) (mref $A $j $k))))
     (sum (&= $k $1) $n
          (mref (@i* $B $A) $k $k))
     (&tr (&i* $B $A))))
   ((example #:n "20")
    "令" $V "是复数域上的多项式的向量空间, 而其上的内积为"
    (MB (&= (inner* $f $g)
            (uintegral
             (&i* (app $f $t)
                  (OverBar (app $g $t))))) ".")
    "考虑线性算子&quot;乘以" $f "&quot;, 即"
    (&= (app $M_f $g) (&i* $f $g))
    ", 那么这个算子具有一个伴随, 即乘以" (OverBar $f)
    ", 这是因为"
    (eqnderiv
     (inner* (app $M_f $g) $h)
     (inner* (&i* $f $g) $h)
     (uintegral
      (&i* (app $f $t) (app $g $t)
           (OverBar (app $h $t))))
     (uintegral
      (&i* (app $g $t)
           (OverBar
            (bra0 (&i* (OverBar (app $f $t))
                       (app $h $t))))))
     (uintegral
      (&i* (app $g $t)
           (OverBar (app (@i* (OverBar $f) $h) $t))))
     (inner* $g (&i* (OverBar $f) $h))
     (inner* $g (app (_ $M (OverBar $f)) $h)))
    "于是" (&= (&* (@ $M_f)) (_ $M (OverBar $f))) ".")
   ((example #:n "21")
    "在例子20里, 我们看到某些无限维向量空间上的线性算子"
    "的确也有伴随. 正如之前所说, 这种线性算子并不总是"
    "具有伴随. 令" $V "是例子20中的内积空间, 而" $D
    "是" (&poly $CC) "上的形式微分算子, 那么分部积分表明"
    (MB (&= (inner* (ap $D $f) $g)
            (&- (&i* (app $f $1) (app (OverBar $g) $1))
                (&i* (app $f $0) (app (OverBar $g) $0))
                (inner* $f (ap $D $g)))) ".")
    "让我们固定" $g ", 并检视何时存在一个多项式"
    (ap (&* $D) $g) "使得对于所有的" $f "都有"
    (&= (inner* (ap $D $f) $g) (inner* $f (ap (&* $D) $g)))
    ". 如果这样的一个" (ap (&* $D) $g) "存在的话, 我们有"
    (MB (&= (inner* $f (ap (&* $D) $g))
            (&- (&i* (app $f $1) (app (OverBar $g) $1))
                (&i* (app $f $0) (app (OverBar $g) $0))
                (inner* $f (ap $D $g)))))
    "或者"
    (MB (&= (inner* $f (&+ (ap (&* $D) $g) (ap $D $g)))
            (&- (&i* (app $f $1) (app (OverBar $g) $1))
                (&i* (app $f $0) (app (OverBar $g) $0)))) ".")
    "在" $g "固定的情况下, "
    (&= (app $L $f)
        (&- (&i* (app $f $1) (app (OverBar $g) $1))
            (&i* (app $f $0) (app (OverBar $g) $0))))
    "就成为例子16中所考虑的那种类型的线性泛函. 除非" (&= $L $0)
    ", 其就不可能具有" (&= (app $L $f) (inner* $f $h))
    "的形式. 如果" (ap (&* $D) $g) "存在, 那么令"
    (&= $h (&+ (ap (&* $D) $g) (ap $D $g))) ", 我们就有"
    (&= (app $L $f) (inner* $f $h)) ", 于是"
    (&= (app $g $0) (app $g $1) $0) ". 也就是说, 适合的"
    (ap (&* $D) $g) "的存在可以推出"
    (&= (app $g $0) (app $g $1) $0)
    ". 反过来, 若" (&= (app $g $0) (app $g $1) $0)
    ", 多项式" (&= (ap (&* $D) $g) (&- (ap $D $g)))
    "满足对于所有的" $f ", "
    (&= (inner* (ap $D $f) $g) (inner* $f (ap (&* $D) $g)))
    ". 如果我们选择了任意的" $g "使得" (&!= (app $g $0) $0)
    "或" (&!= (app $g $1) $0) ", 那么就不可能定义合适的"
    (ap (&* $D) $g) ". 我们总结一下, 即" $D "没有伴随.")
   (P "我们希望这些例子能够加深读者对于线性算子的伴随的理解. "
      "我们看到, 从" $T "到" (&* $T) "的伴随操作表现得"
      "有些类似于复数上的共轭. 以下的定理强调了这种类比.")
   ((theorem #:n "9")
    "令" $V "是一个有限维内积空间, " $T "和" $U
    "是" $V "上的线性算子, " $c "是任意的标量, 那么"
    (Ol #:attr* '((type "i"))
        (Li (&= (&* (@+ $T $U))
                (&+ (&* $T) (&* $U))) ";")
        (Li (&= (&* (@i* $c $T))
                (&i* (OverBar $c) (&* $T))) ";")
        (Li (&= (&* (@i* $T $U))
                (&i* (&* $U) (&* $T))) ";")
        (Li (&= (&* (@ (&* $T))) $T) ".")))
   ((proof)
    "为了证明i, 令" (∈ $alpha $beta $V) ", 那么"
    (eqnderiv
     (inner* (ap (@+ $T $U) $alpha) $beta)
     (inner* (&+ (ap $T $alpha) (ap $U $alpha)) $beta)
     (&+ (inner* (ap $T $alpha) $beta)
         (inner* (ap $U $alpha) $beta))
     (&+ (inner* $alpha (ap (&* $T) $beta))
         (inner* $alpha (ap (&* $U) $beta)))
     (inner* $alpha (&+ (ap (&* $T) $beta) (ap (&* $U) $beta)))
     (inner* $alpha (ap (@+ (&* $T) (&* $U)) $beta)))
    "根据伴随的唯一性, 我们得到了"
    (&= (&* (@+ $T $U)) (&+ (&* $T) (&* $U)))
    ". 我们将ii的证明留给读者. 我们从以下关系"
    (MB (&= (inner* (ap $T (ap $U $alpha)) $beta)
            (inner* (ap $U $alpha) (ap (&* $T) $beta))
            (inner* $alpha (ap (&* $U) (ap (&* $T) $beta)))))
    "和"
    (MB (&= (inner* (ap (&* $T) $alpha) $beta)
            (OverBar (inner* $beta (ap (&* $T) $alpha)))
            (OverBar (inner* (ap $T $beta) $alpha))
            (inner* $alpha (ap $T $beta))))
    "可以得到iii和iv.")
   (P "定理9经常被重述为伴随是一个周期为" $2
      "的共轭线性的反同构. 我们上面提及的伴随与复共轭"
      "的类似之处当然是复共轭具有"
      (preserve OverBar $z_1 &+ $z_2) ", "
      (preserve OverBar $z_1 &i* $z_2) ", "
      (&= (OverBar (OverBar $z)) $z)
      "的性质. 对于乘积的伴随, 读者必须小心顺序是相反的: "
      (&= (&* (@i* $T $U)) (&i* (&* $U) (&* $T)))
      ". 当我们继续研究内积空间上的线性算子时, "
      "我们将提及以上类比的一些扩展. 现在, 我们就要"
      "沿着之前的路线提及一点. 一个复数" $z
      "是实数当且仅当" (&= $z (OverBar $z))
      ". 读者可能会设想满足" (&= $T (&* $T))
      "的线性算子" $T "在某种意义上表现得与实数类似, "
      "实际上的确如此. 例如, 若" $T "是有限维" (Em "复")
      "内积空间上的一个线性算子, 那么"
      (MB (&= $T (Complex $U_1 $U_2)))
      "其中" (&= $U_1 (_^ $U $1 $*)) "而"
      (&= $U_2 (_^ $U $2 $*)) ". 因此, " $T
      "也拥有某种&quot;实部&quot;和&quot;虚部&quot;. "
      "这样的算子" $U_1 "和" $U_2 "是唯一的, 由"
      (MB (&= $U_1 (&i* 1/2 (@+ $T (&* $T)))) "和"
          (&= $U_2 (&i* (~ $1 (&i* $2 $i))
                        (@- $T (&* $T)))))
      "给定.")
   (P "满足" (&= $T (&* $T)) "的线性算子" $T
      "被称为是" (B "自伴的") ", 或者"
      (B "Hermite的") ". 若" $BBB "是" $V
      "的一个规范正交基, 那么"
      (MB (&= (coordm (&* $T)) (_^ (bra0 $T) $BBB $*)) ".")
      "于是, " $T "是自伴算子当且仅当其在每个规范正交基"
      "下的矩阵表示都是自伴的. 自伴算子是重要的, "
      "不仅在于其提供了一般线性算子在某种意义下的"
      "实部和虚部, 还出于以下原因:"
      (Ol (Li "自伴算子具有许多特殊的性质. 例如, 对于"
              "这样的一种线性算子, 存在一个由其特征"
              "向量构成的规范正交基.")
          (Li "许多实践中出现的线性算子都是自伴的."))
      "之后我们将考虑自伴算子的特殊性质.")
   ((exercise #:n "1")
    "令" $V "是带有标准内积的向量空间" $CC^2
    ", " $T "是由" (&= (ap $T $epsilon_1) (tu0 $1 -2))
    "和" (&= (ap $T $epsilon_2) (tu0 $i $-1))
    "定义的线性算子. 如果" (&= $alpha (tu0 $x_1 $x_2))
    ", 找出" (ap (&* $T) $alpha) ".")
   ((exercise #:n "2")
    "令" $T "是" $CC^2 "上的线性算子, 由"
    (&= (ap $T $epsilon_1) (tu0 (&+ $1 $i) $2)) "和"
    (&= (ap $T $epsilon_2) (tu0 $i $i))
    "定义. 使用标准内积, 找出" (&* $T)
    "在标准有序基下的矩阵. " $T "与" (&* $T) "交换吗?")
   ((exercise #:n "3")
    "令" $V "是带有标准内积的" $CC^3 ", " $T "是"
    $V "上的线性算子, 其在标准有序基下的矩阵由"
    (MB (&= (mref $A $j $k) (^ $i (&+ $j $k))))
    "定义, 其中" $i "是虚数单位. 找出" (&* $T)
    "的零空间的一个基.")
   ((exercise #:n "4")
    "令" $V "是一个有限维内积空间, " $T "是" $V
    "上的一个线性算子, 证明" (&* $T) "的像是" $T
    "的零空间的正交补.")
   ((exercise #:n "5")
    "令" $V "是一个有限维内积空间, " $T "是" $V
    "上的一个线性算子. 如果" $T "是可逆的, 证明"
    (&* $T) "也是可逆的, 并且"
    (&= (inv (@ (&* $T))) (&* (@ (inv $T)))) ".")
   ((exercise #:n "6")
    "令" $V "是一个内积空间, 而" $beta "和" $gamma
    "是" $V "中固定的向量. 证明"
    (&= (ap $T $alpha) (&i* (inner* $alpha $beta) $gamma))
    "定义了" $V "上的一个线性算子. 证明" $T
    "具有伴随, 并显式描述" (&* $T) "." (Br)
    "现在设" $V "是带有标准内积的" $CC^n
    ", " (&= $beta (tu0 $y_1 $..h $y_n)) "而"
    (&= $gamma (tu0 $x_1 $..h $x_n))
    ". " $T "在标准有序基下的矩阵的第" $j
    "行" $k "列的元素是什么? 这个矩阵的秩是多少?")
   ((exercise #:n "7")
    "证明两个自伴算子之积是自伴的当且仅当这两个算子交换.")
   ((exercise #:n "8")
    "令" $V "是" $RR "上次数小于等于" $3
    "的多项式构成的向量空间, 而内积为"
    (MB (&= (inner* $f $g)
            (uintegral (&i* (app $f $t) (app $g $t)))) ".")
    "如果" $t "是一个实数, 找出多项式" (∈ $g_t $V) "使得对于每个"
    (∈ $f $V) "都有" (&= (inner* $f $g_t) (app $f $t)) ".")
   ((exercise #:n "9")
    "令" $V "是练习8的内积空间, " $D "是" $V
    "上的形式微分算子, 找出" (&* $D) ".")
   ((exercise #:n "10")
    "令" $V "是" (^ $CC n*n) ", 其上的内积为"
    (&= (inner* $A $B) (&tr (&i* $A (&* $B))))
    ". 令" (∈ $P $V) "是一个固定的可逆矩阵, 而"
    (&= (app $T_P $A) (sim $A)) "是" $V
    "上的线性算子. 找出" $T_P "的伴随.")
   ((exercise #:n "11")
    "令" $V "是一个有限维内积空间, " $E "是" $V
    "上的一个幂等线性算子, 证明" $E "是自伴的当且仅当"
    (commute* $E (&* $E)) ".")
   ((exercise #:n "12")
    "令" $V "是一个有限维" (Em "复") "内积空间, "
    $T "是" $V "上的一个线性算子, 证明" $T
    "是自伴的当且仅当对于每个" (∈ $alpha $V) ", "
    (inner* (ap $T $alpha) $alpha) "是实数.")
   (H3 "第8.4节 酉算子")
   (P "在本节中, 我们将考虑两个内积空间之间的同构的概念. 如果"
      $V "和" $W "是向量空间, 那么从" $V "到" $W
      "的同构是一个从" $V "到" $W "的双射的线性变换, "
      "即&quot;保持&quot;向量空间运算的从" $V
      "到" $W "的一一对应. 既然内积空间不仅包含"
      "包含向量空间, 还具有一个给定的内积, 那么当"
      $V "和" $W "是内积空间时, 我们要求从" $V "到" $W
      "的内积不仅保持线性运算, 还应该保持内积. 内积空间"
      "上的自同构被称为&quot;酉算子&quot;. 我们将考虑"
      "酉算子的各种例子并建立其基本性质.")
   ((definition)
    "令" $V "和" $W "是相同的域上的内积空间, " $T
    "是从" $V "到" $W "的线性变换, 那么我们称" $T
    (B "保持内积") ", 如果对于每个"
    (∈ $alpha $beta $V) "都有"
    (&= (inner* (ap $T $alpha) (ap $T $beta))
        (inner* $alpha $beta))
    ". 从" $V "到" $W "的" (B "同构")
    "是保持内积的从" $V "到" $W "的向量空间的同构.")
   (P "如果" $T "保持内积, 那么"
      (&= (&norm (ap $T $alpha)) (&norm $alpha))
      ", 于是" $T "必然是非奇异的. 因此, 从" $V
      "到" $W "的同构也可以被定义为保持内积的从"
      $V "到" $W "的满射的线性变换. 若" $T "是从"
      $V "到" $W "的同构, 那么" (inv $T) "是从"
      $W "到" $V "的同构. 当这样的一个" $T
      "存在时, 我们就称" $V "和" $W "是" (B "同构的")
      ". 当然, 内积空间之间的同构是一个等价关系.")
   ((theorem #:n "10")
    "令" $V "和" $W "是相同的域上的" $n
    "维内积空间, 如果" $T "是从" $V "到" $W
    "的线性变换, 那么以下条件是等价的."
    (Ol #:attr* '((type "i"))
        (Li $T "保持内积.")
        (Li $T "是一个(内积空间的)同构.")
        (Li $T "将" $V "的每个规范正交基映射为"
            $W "的规范正交基.")
        (Li $T "将" $V "的某个规范正交基映射为"
            $W "的规范正交基.")))
   ((proof)
    "由i推出ii: 如果" $T "保持内积, 那么对于每个" (∈ $alpha $V)
    ", " (&= (&norm (ap $T $alpha)) (&norm $alpha))
    ". 因此, " $T "是非奇异的. "
    )
   ((corollary)
    "令" $V "和" $W "是相同的域上的有限维内积空间, 那么"
    $V "和" $W "同构当且仅当它们具有相等的维数.")
   ((theorem #:n "11")
    "令" $V "和" $W "是相同的域上的内积空间, " $T
    "是从" $V "到" $W "的线性变换, 那么" $T
    "保持内积当且仅当对于每个" (∈ $alpha $V) ", "
    (&= (&norm (ap $T $alpha)) (&norm $alpha)) ".")
   ((definition)
    "一个内积空间上的一个" (B "酉算子")
    "是一个从此空间到自身的同构.")
   ((theorem #:n "12")
    "令" $U "是内积空间" $V "上的一个线性算子, 那么" $U
    "是酉算子当且仅当" $U "的伴随" (&* $U) "存在并且"
    (&= (&i* $U (&* $U)) (&i* (&* $U) $U) $I) ".")
   ((definition)
    "一个" n*n "的复矩阵被称为" (B "酉矩阵")
    ", 如果" (&= (&i* (&* $A) $A) $I) ".")
   
   (H3 "第8.5节 正规算子")
   ))