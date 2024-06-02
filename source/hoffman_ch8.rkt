#lang racket
(provide hoffman_ch8.html)
(require SMathML "linear_algebra_utils.rkt")
(define hoffman_ch8.html
  (TmPrelude
   #:title "线性代数 第8章 内积空间"
   #:css "styles.css"
   (H1 "线性代数")
   (H2 "第8章 内积空间")
   (H3 "第8.1节 内积")
   (P "整个章节我们只考虑实或复向量空间, 即实数域或复数域上的向量空间. 我们的主要目的在于研究"
      "这样的向量空间, 其中讨论向量的长度和两个向量之间的夹角是有意义的. 我们将研究向量序对上的"
      "特定类型的标量值函数, 其被称为内积. 内积的一个例子是" $RR^3
      "中的向量的标量积或者说点积. " $RR^3 "中的向量"
      (MB (&= $alpha (tu0 $x_1 $x_2 $x_3)) "和" (&= $beta (tu0 $y_1 $y_2 $y_3)))
      "的标量积是实数"
      (MB (&= (inner* $alpha $beta)
              (&+ (&i* $x_1 $y_1) (&i* $x_2 $y_2) (&i* $x_3 $y_3))) ".")
      "从几何上说, 这个点积是向量" $alpha "的长度, 向量" $beta
      "的长度, 以及" $alpha "和" $beta "的夹角的余弦的乘积. "
      "因此代数地定义" $RR^3 "中的长度和夹角的几何概念是可能的.")
   (P "向量空间上的内积是一个与" $RR^3 "的点积性质类似的函数, 并且基于它可以定义长度和角度. "
      "我们关于角度的一般概念的讨论将仅限于向量的垂直性 (正交性). 第一节我们将定义什么是内积, 考虑"
      "一些特定的例子, 并建立内积的一些基本性质. 然后我们回到讨论长度和正交性的任务上来.")
   ((definition)
    "令" $F "是实数域或复数域, 并且" $V "是域"
    $F "上的一个向量空间. " $V "上的一个内积是一个函数, 其赋"
    $V "中每对向量" $alpha "和" $beta "以一个" $F
    "中的标量" (inner* $alpha $beta) "满足对于所有" $V "中的"
    (&cm $alpha $beta $gamma) "以及所有标量" $c
    (Ol #:attr* '((type "a"))
        (Li (&= (inner* (&+ $alpha $beta) $gamma)
                (&+ (inner* $alpha $gamma)
                    (inner* $beta $gamma))) ";")
        (Li (&= (inner* (&i* $c $alpha) $beta)
                (&i* $c (inner* $alpha $beta))) ";")
        (Li (&= (inner* $beta $alpha)
                (OverBar (inner* $alpha $beta)))
            ", 横杠代表复数共轭;")
        (Li (&> (inner* $alpha $alpha) $0) "如果"
            (&!= $alpha $0) ".")))
   (P "读者应该观察到条件a, b, c可以推出下列条件e."
      (MB (&= (inner* $alpha (&+ (&i* $c $beta) $gamma))
              (&+ (&i* (OverBar $c) (inner* $alpha $beta))
                  (inner* $alpha $gamma))) ".")
      "另一点值得指出. 当" $F "是实数域" $RR "时, "
      "出现在c和e中的复数共轭是多余的. 然而, 若是在复的情况下, "
      "为了条件的一致性那么共轭就是必须的. 如果没有共轭, 我们"
      "就会得到以下矛盾:"
      (MB (&> (inner* $alpha $alpha) $0) "且"
          (&> (&= (inner* (&i* $i $alpha) (&i* $i $alpha))
                  (&i* $-1 (inner* $alpha $alpha)))
              $0) "."))
   (P "在本章的剩余部分, " $F "要么代表实数域要么代表复数域.")
   ((example #:n "1")
    "在" $F^n "上存在一个被我们称为标准内积的内积. "
    "它由"
    (MB (&= (inner* $alpha $beta)
            (sum $j $
                 (&i* $x_j (_ (OverBar $y) $j)))))
    "定义, 其中" (&= $alpha (tu0 $x_1 $..h $x_n))
    ", " (&= $beta (tu0 $y_1 $..h $y_n)) ". "
    "当" (&= $F $RR) "时, 这也可以写成"
    (MB (&= (inner* $alpha $beta)
            (sum $j $ (&i* $x_j $y_j))))
    "在实的情况下, 标准内积也经常被称为点积或标量积, 记作"
    (&d* $alpha $beta) ".")
   ((example #:n "2")
    "对于" $RR^2 "的向量" (&= $alpha (tu0 $x_1 $x_2)) "和"
    (&= $beta (tu0 $y_1 $y_2)) ", 令"
    (MB (&= (inner* $alpha $beta)
            (&+ (&- (&i* $x_1 $y_1)
                    (&i* $x_2 $y_1)
                    (&i* $x_1 $y_2))
                (&i* $4 $x_2 $y_2)))
        ".")
    "因为"
    (&= (inner* $alpha $alpha)
        (&+ (^ (@- $x_1 $x_2) $2)
            (&i* $3 (_^ $x $2 $2))))
    ", 所以知道如果" (&!= $alpha $0) ", 那么"
    (&> (inner* $alpha $alpha) $0) ". 定义的条件a, b, c很容易验证.")
   ((example #:n "3")
    "令" $V "是" (^ $F n*n) ", 即"
    $F "上所有" n*n "矩阵构成的空间. "
    "那么, " $V "以很自然的方式同构于" (^ $F n*n)
    ". 因此, 由例子1可知"
    (MB (&= (inner* $A $B)
            (sum (&cm $j $k) $
                 (&i* (mref $A $j $k)
                      (_ (OverBar $B) (&cm $j $k))))))
    "定义了一个" $V "上的内积. 而且, 如果我们引入共轭转置矩阵"
    (ctrans $B) ", 由"
    (&= (_^ $B (&cm $k $j) $*) (_ (OverBar $B) (&cm $j $k)))
    "定义, 我们可以基于迹函数定义内积:"
    (MB (&= (inner* $A $B)
            (&tr (&i* $A (ctrans $B)))
            (&tr (&i* (ctrans $B) $A)))
        ".")
    "因为"
    (MB (deriv (&tr (&i* $A (ctrans $B)))
               (sum $j $ (_ (@i* $A (ctrans $B)) (&cm $j $j)))
               (sum $j $
                    (sum $k $
                         (&i* (mref $A $j $k)
                              (_^ $B (&cm $k $j) $*))))
               (sum $j $
                    (sum $k $
                         (&i* (mref $A $j $k)
                              (_ (OverBar $B) (&cm $j $k))))))))
   ((example #:n "4")
    "令" $F^n*1 "是域" $F "上所有" n*1 "的(列)矩阵构成的空间, 令" $Q "是域"
    $F "上的一个" n*n "的可逆矩阵. 对于" (&in (&cm $X $Y) $F^n*1) ", 令"
    (MB (&= (inner* $X $Y) (&i* (ctrans $Y) (ctrans $Q) $Q $X)))
    "我们将右边的" (&c* $1 $1) "矩阵与它唯一的元素视为等同. "
    "当" $Q "是恒等矩阵的时候, 这个内积本质上和例子1是一样的, "
    "我们将其称为" $F^n*1 "上的标准内积. "
    "读者应该注意到术语&quot;标准内积&quot;在两个不同的上下文被使用. "
    "对于一般的域" $F "上的有限维向量空间, 没有明显的内积可被称为标准的.")
   ((example #:n "5")
    "令" $V "是单位区间" (&<= $0 $t $1)
    "上所有连续复值函数构成的向量空间, 令"
    (MB (&= (inner* $f $g)
            (integral $0 $1
                      (&i* (app $f $t)
                           (OverBar (app $g $t)))
                      $t)))
    "读者可能更熟悉单位区间上的连续实值函数构成的空间, 那么此时共轭就可以去掉.")
   ((example #:n "6")
    "这实际上是一类例子. 人们可以通过以下方法从已有的内积构造出新的内积来. 令"
    $V "和" $W "是域" $F "上的向量空间, 而设" (inner* $ $) "是" $W "上的一个内积. "
    "如果" $T "是一个从" $V "到" $W "的非奇异的线性变换, 那么"
    (MB (&= (appl $p_T $alpha $beta) (inner* (ap $T $alpha) (ap $T $beta))))
    "定义了一个" $V "上的内积" $p_T ". 例4的内积是这种情况的一个特殊情形. 以下同样也是特殊情形."
    (Ol #:attr* '((type "a"))
        (Li "令" $V "是一个有限维向量空间, 设" (&= $BBB (en0 $alpha_1 $..h $alpha_n))
            "是" $V "的一个有序基. 令" (&cm $epsilon_1 $..h $epsilon_n)
            "是" $F^n "的标准基向量, 令" (func $T $V $F^n) "是满足"
            (&cm (&= (ap $T $alpha_j) $epsilon_j) (&= $j (&cm $1 $..h $n)))
            "的线性变换. 换句话说, " $T "是由"
            $BBB "决定的从" $V "到" $F^n "的自然同构. 如果我们选择"
            $F^n "上的标准内积, 那么"
            (MB (&= (appl $p_T
                          (sum $j $ (&i* $x_j $alpha_j))
                          (sum $k $ (&i* $y_k $alpha_k)))
                    (sum (&= $j $1) $n
                         (&i* $x_j (_ (OverBar $y) $j)))))
            "因此, 对于任何" $V "上的基存在一个内积满足"
            (&= (inner* $alpha_j $alpha_k)
                (_ $delta (&cm $j $k)))
            ". 实际上, 很容易证明这样的内积是唯一的. 之后我们将会证明"
            "每个" $V "上的内积都可以由某个基" $BBB "按照以上方式决定.")
        (Li "我们再一次观察例5并令" (&= $V $W) ", 即单位区间上的"
            "连续函数构成的空间. 令" $T "是&quot;乘上" $t "&quot;的线性算子, 即"
            (&cm (&= (app (@ap $T $f) $t)
                     (&i* $t (app $f $t)))
                 (&<= $0 $t $1))
            ". 容易看出" $T "是线性的, 而且" $T
            "是非奇异的. 如果" (&= (ap $T $f) $0) ", 那么"
            (&= (&i* $t (app $f $t)) $0) "对于"
            (&<= $0 $t $1) "成立, 因此当" (&> $t $0) "时"
            (&= (app $f $t) $0) ". 鉴于" $f "是连续的, "
            (&= (app $f $0) $0) ", 于是" (&= $f $0) ". "
            "现在使用例子5的内积, 我们可以构造一个新的" $V "上的内积:"
            (MB (deriv (appl $p_T $f $g)
                       (integral
                        $0 $1
                        (&i* (app (@ap $T $f) $t)
                             (OverBar (app (@ap $T $g) $t)))
                        $t)
                       (integral
                        $0 $1
                        (&i* (app $f $t)
                             (OverBar (app $g $t))
                             (^ $t $2))
                        $t))))))
   
   (H3 "第8.2节 内积空间")
   
   (H3 "第8.3节 线性泛函和伴随")
   (H3 "第8.4节 酉算子")
   (H3 "第8.5节 正规算子")
   
   ))