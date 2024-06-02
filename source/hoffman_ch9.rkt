#lang racket
(provide div:hoffman_ch9)
(require SMathML "linear_algebra_utils.rkt")
(define div:hoffman_ch9
  (TmDiv
   (H2 "第9章 内积空间上的算子")
   (H3 "第9.1节 引论")
   (P "我们将第8章所处理的大部分议题视为基础的, "
      "即每个人都应该知道的材料. "
      "本章是面向更加优秀的学生以及那些迫不及待想要"
      "扩展自己关于内积空间上的算子的知识的读者的. "
      "这里呈现的材料更加复杂, 一般牵涉更多的技术, "
      "除了主轴定理, 其基本上就是重述定理18关于"
      "自伴算子的酉/正交对角化的结果, "
      "以及第9.2节中关于形式的其他结果. "
      "我们要求读者更加成熟, 就像第5章和第7章的后半部分那样. "
      "论证和证明以更加凝缩的风格编写, "
      "并且几乎没有多少用以润滑的例子. "
      "然而, 我们已经预见到了这种困难, 所以为读者"
      "提供了大量的练习.")
   (P "起初的三节致力于关于内积空间上的形式以及形式与"
      "线性算子之间的关系的结果. 接下来的一节处理谱论, "
      "即第8章牵涉自伴算子和正规算子的对角化的定理18和22"
      "的推论. 最后一节里, 我们研究实内积空间上的正规算子, "
      "由此我们检视了第6章的准素分解定理之于正规算子的意蕴.")
   (H3 "第9.2节 内积空间上的形式")
   (P "如果" $T "是域" $F "上的有限维内积空间"
      $V "上的一个线性算子, 那么由"
      (MB (&= (appl $f $alpha $beta)
              (inner* (ap $T $alpha) $beta)))
      "定义的函数" (func $f (&c* $V $V) $F)
      "可以被视为" $T "的一种替代物. 诸多关于" $T
      "的问题都等价于关于" $f "的问题. 实际上, 很容易看出来"
      $f "可以确定" $T ". 这是因为, 如果" basis:def "是" $V
      "的一个规范正交基, 那么" $T "在" $BBB "下的矩阵" $A "由"
      (MB (&= (mref $A $j $k)
              (appl $f $alpha_k $alpha_j)))
      "给出. 从更加抽象的角度理解为什么" $f "可以确定" $T
      "是重要的. " $f "的重要性质在以下定义中得以描述.")
   ((definition)
    "一个域" $F " (" $F "是实数域或复数域) 上的向量空间"
    $V "上的" (B "(半双线性)形式") "是一个函数"
    (func $f (&c* $V $V) $F) "满足对于任意的"
    (∈ $alpha $beta $gamma $V) "和任意的标量" $c "有"
    (Ol #:attr* '((type "a"))
        (Li (let ((g (lambda (a)
                       (appl $f a $gamma))))
              (linear g $c $alpha $beta)) ";")
        (Li (let ((h (lambda (b)
                       (appl $f $alpha b))))
              (clinear h $c $beta $gamma)) ".")))
   (P "因此, 半双线性形式" $f "使得" (appl $f $alpha $beta)
      "在固定的" $beta "下是" $alpha "的线性函数, 而在固定的"
      $alpha "下是" $beta "的共轭线性函数. 在实情形下, "
      (appl $f $alpha $beta) "对于每个参数都是线性的. 换言之, "
      $f "是一个" (B "双线性形式") ". 在复情形下, 除非"
      (&= $f $0) ", 否则半双线性形式" $f "不会是双线性形式. "
      "在本章的剩余部分里, 除非确有必要, 否则形容词"
      "&quot;半双线性&quot;一律省略.")
   (P "如果" $f "和" $g "是" $V "上的形式而" $c
      "是任意的标量, 那么很容易验证" (LC $c $f $g)
      "也是一个形式. 换言之, 任意的形式的线性组合仍然是一个形式. "
      "因此, " $V "上的所有形式构成的集合是向量空间" (^ $F (&c* $V $V))
      "的一个子空间, 其中" $F "是向量空间" $V "的标量域.")
   ((theorem #:n "1")
    "令" $V "是一个有限维内积空间, " $f "是" $V
    "上的一个形式, 那么存在唯一的" $V "上的线性算子" $T
    "满足对于任意的" (∈ $alpha $beta $V) "都有"
    (MB (&= (appl $f $alpha $beta)
            (inner* (ap $T $alpha) $beta)) ".")
    "并且, 由此定义的映射" (&\|-> $f $T) "是从形式的空间到"
    (&L $V $V) "的一个同构.")
   ((proof)
    "固定一个向量" (∈ $beta $V) ", 那么"
    (&\|-> $alpha (appl $f $alpha $beta))
    "是" $V "上的一个线性泛函. 根据第8章的定理6, 存在唯一的向量"
    (∈ $beta^ $V) "使得对于每个" $alpha ", 我们有"
    (&= (appl $f $alpha $beta) (inner* $alpha $beta^))
    ". 定义函数" (&: $U (&cm (&-> $V $V) (&\|-> $beta $beta^)))
    ", 那么"
    (eqnderiv
     (appl $f $alpha (LC $c $beta $gamma))
     (inner* $alpha (app $U (LC $c $beta $gamma)))
     (LC (OverBar $c) (appl $f $alpha $beta)
         (appl $f $alpha $gamma))
     (LC (OverBar $c) (inner* $alpha (ap $U $beta))
         (inner* $alpha (ap $U $gamma)))
     (inner* $alpha (LC $c (ap $U $beta) (ap $U $gamma))))
    "对于任意的" (∈ $alpha $beta $gamma $V) "和任意的标量"
    $c "成立. 因此, " $U "是" $V "上的一个线性算子. 令"
    (&= $T (&* $U)) ", 则有对于所有的" (∈ $alpha $beta $V)
    ", " (&= (appl $f $alpha $beta) (inner* (ap $T $alpha) $beta))
    ". 如果我们也有线性算子" $T^ "使得"
    (&= (appl $f $alpha $beta) (inner* (ap $T^ $alpha) $beta))
    ", 那么"
    (MB (&= (inner* (&- (ap $T $alpha) (ap $T^ $alpha))
                    $beta) $0) ".")
    "于是, 对于每个" (∈ $alpha $V) ", "
    (&= (ap $T $alpha) (ap $T^ $alpha)) ". 换言之, 对于每个形式"
    $f ", 存在唯一的线性算子" $T_f "使得对于每个" (∈ $alpha $beta $V)
    ", 我们有"
    (MB (&= (appl $f $alpha $beta)
            (inner* (ap $T_f $alpha) $beta)) ".")
    "如果" $f "和" $g "是形式而" $c "是标量, 那么"
    (eqnderiv
     (appl (@LC $c $f $g) $alpha $beta)
     (inner* (ap (_ $T (LC $c $f $g)) $alpha) $beta)
     (LC $c (appl $f $alpha $beta) (appl $g $alpha $beta))
     (LC $c (inner* (ap $T_f $alpha) $beta)
         (inner* (ap $T_g $alpha) $beta))
     (inner* (ap (@LC $c $T_f $T_g) $alpha) $beta))
    "对于任意的" (∈ $alpha $beta $V) "成立, 因而"
    (MB (&= (_ $T (LC $c $f $g)) (LC $c $T_f $T_g)) ".")
    "换言之, " (&\|-> $f $T_f) "是一个线性映射. 对于每个"
    (∈ $f (&L $V $V)) ", 等式"
    (MB (&= (appl $f $alpha $beta)
            (inner* (ap $T $alpha) $beta)))
    "定义了一个形式" $f "使得" (&= $T_f $T) ". 并且, 如果"
    (&= $T_f $0) ", 那么" (&= $f $0) ". 因此, "
    (&\|-> $f $T_f) "的确是一个同构.")
   ((corollary)
    "等式"
    (MB (&= (inner* $f $g)
            (&tr (&i* $T_f (_^ $T $g $*)))))
    "定义了形式的空间上的一个内积, 并且对于每个" $V
    "的规范正交基" (setE $alpha_1 $..h $alpha_n)
    ", 我们有"
    (MB (&= (inner* $f $g)
            (sum (&= $j $1) $n
                 (sum (&= $k $1) $n
                      (&i* (appl $f $alpha_k $alpha_j)
                           (OverBar
                            (appl $g $alpha_k $alpha_j)))))) "."))
   ((proof)
    "根据第8章的例子3, 很容易推出" (&\|-> (tu0 $T $U) (&tr (&i* $T (&* $U))))
    "是" (&L $V $V) "上的一个内积. 既然" (&\|-> $f $T_f)
    "是一个同构, 第8章的例子6表明"
    (MB (&= (inner* $f $g) (&tr (&i* $T_f (_^ $T $g $*)))))
    "也是一个内积. [译注: 实际上, 前一个内积也是通过第8章的例子6得到的.] "
    "现在设" $A "和" $B "分别是" $T_f "和" $T_g "在规范正交基" basis:def
    "下的矩阵, 那么"
    (MB (&= (mref $A $j $k)
            (inner* (ap $T_f $alpha_k) $alpha_j)
            (appl $f $alpha_k $alpha_j)))
    "而"
    (MB (&= (mref $B $j $k)
            (inner* (ap $T_g $alpha_k) $alpha_j)
            (appl $g $alpha_k $alpha_j)) ".")
    "这可以推出"
    (eqnderiv
     (inner* $f $g)
     (&tr (&i* $T_f (_^ $T $g $*)))
     (&tr (&i* $A (&* $B)))
     (sum (&= $j $1) $n
          (sum (&= $k $1) $n
               (&i* (mref $A $j $k)
                    (mref (OverBar $B) $j $k))))
     (sum (&= $j $1) $n
          (sum (&= $k $1) $n
               (&i* (appl $f $alpha_k $alpha_j)
                    (OverBar
                     (appl $g $alpha_k $alpha_j)))))))
   ((definition)
    "如果" $f "是" $V "上的一个形式而" basis:def
    "是" $V "的一个有序基, 那么由"
    (MB (&= (mref $A $j $k)
            (appl $f $alpha_k $alpha_j)))
    "定义的矩阵" $A "被称为"
    (B $f "在有序基" $BBB "下的矩阵") ".")
   (P "当" $BBB "是一个规范正交基时, " $f "在" $BBB
      "下的矩阵也是线性变换" $T_f "在" $BBB
      "下的矩阵, 但是在一般情况下并非如此.")
   (P "如果" $A "是" $f "在有序基" basis:def
      "下的矩阵, 那么"
      (MB (&= (ap $f (tup (sum (&= $s $1) $n
                               (&i* $x_s $alpha_s))
                          (sum (&= $r $1) $n
                               (&i* $y_r $alpha_r))))
              (sum (&= $r $1) $n
                   (sum (&= $s $1) $n
                        (&i* (_ (OverBar $y) $r)
                             (mref $A $r $s)
                             $x_s)))))
      "对于任意的标量" $x_s "和" $y_r
      "成立. 换言之, 矩阵" $A "具有"
      (MB (&= (appl $f $alpha $beta)
              (&i* (&* $Y) $A $X)))
      "的性质, 其中" $X "和" $Y "分别是" $alpha "和" $beta
      "在有序基" $BBB "下的坐标矩阵.")
   (P $f "在另外一个基"
      (MB (&cm (&= (_^ $alpha $j $prime)
                   (sum (&= $i $1) $n
                        (&i* (mref $P $i $j)
                             $alpha_i)))
               (&<= $1 $j $n)))
      "下的矩阵由式子"
      (MB (&= $A^ (usim $A)))
      "给出, 这是因为"
      (eqnderiv
       (_^ $A (&cm $j $k) $prime)
       (appl $f (_^ $alpha $k $prime)
             (_^ $alpha $j $prime))
       (ap $f (tup (sum (&= $s $1) $n
                        (&i* (mref $P $s $k)
                             $alpha_s))
                   (sum (&= $r $1) $n
                        (&i* (mref $P $r $j)
                             $alpha_r))))
       (sum (&= $r $1) $n
            (sum (&= $s $1) $n
                 (&i* (mref (OverBar $P) $r $j)
                      (mref $A $r $s)
                      (mref $P $s $k))))
       (mref (@ (usim $A)) $j $k))
      "既然对于酉矩阵而言, 我们有" (&= (&* $P) (inv $P))
      ", 因而与酉等价相关的结果也可应用于对形式的研究.")
   ((theorem #:n "2")
    "令" $f "是有限维复内积空间" $V "上的一个形式, 那么存在"
    $V "的一个规范正交基使得其下的" $f "的矩阵是上三角的.")
   ((proof)
    "令" $T "是" $V "上的线性算子, 其满足对于任意的"
    (∈ $alpha $beta $V) "有"
    (&= (appl $f $alpha $beta) (inner* (ap $T $alpha) $beta))
    ". 根据第8章的定理21, 存在" $V "的一个规范正交基" basis:def
    "使得" $T "在其下的矩阵是上三角的. 根据之前的观察, 我们知道此时"
    $f "的矩阵和" $T "的矩阵是相同的. 换言之, " $f "在规范正交基"
    $BBB "下的矩阵是上三角的.")
   ((definition)
    "实或复向量空间" $V "上的形式" $f "被称为" (B "Hermite的")
    ", 如果对于每个" (∈ $alpha $beta $V) "有"
    (MB (&= (appl $f $alpha $beta)
            (OverBar (appl $f $beta $alpha))) "."))
   (P "如果" $T "是有限维内积空间" $V "上的线性算子, 而" $f "是由"
      (MB (&= (appl $f $alpha $beta)
              (inner* (ap $T $alpha) $beta)))
      "定义的形式, 那么"
      (MB (&= (OverBar (appl $f $beta $alpha))
              (inner* $alpha (ap $T $beta))
              (inner* (ap (&* $T) $alpha) $beta)))
      "换言之, " $f "是Hermite的当且仅当" $T "是自伴的.")
   ((tcomment)
    "上述观察, 即便没有有限维的条件, 也同样成立.")
   (P "当" $f "是一个Hermite形式, 那么对于每个向量" $alpha ", "
      (appl $f $alpha $alpha) "是实数. 在复向量空间上, 这个性质"
      "就刻画了Hermite形式.")
   ((theorem #:n "3")
    "令" $V "是一个复向量空间而" $f "是" $V "上的一个形式, 如果对于每个向量"
    (∈ $alpha $V) "有" (appl $f $alpha $alpha)
    "为实数, 那么" $f "是一个Hermite形式.")
   ((proof)
    "令" $alpha "和" $beta "是" $V "中的向量, 我们必须证明"
    (&= (appl $f $alpha $beta) (OverBar (appl $f $beta $alpha)))
    ". 现在我们有"
    (MB (&= (appl $f (&+ $alpha $beta) (&+ $alpha $beta))
            (&+ (appl $f $alpha $alpha)
                (appl $f $alpha $beta)
                (appl $f $beta $alpha)
                (appl $f $beta $beta))) ".")
    "既然" (appl $f (&+ $alpha $beta) (&+ $alpha $beta)) ", "
    (appl $f $alpha $alpha) ", " (appl $f $beta $beta)
    "都是实数, " (&+ (appl $f $alpha $beta) (appl $f $beta $alpha))
    "也应该是实数. 对于" (Complex $alpha $beta)
    "施行相同的论证, 我们又可以得到"
    (LC (&- $i) (appl $f $alpha $beta)
        $i (appl $f $beta $alpha))
    "是实数. 我们知道实数的共轭等于其本身, 于是"
    (eqn*
     ((&+ (appl $f $alpha $beta)
          (appl $f $beta $alpha))
      $=
      (&+ (OverBar (appl $f $alpha $beta))
          (OverBar (appl $f $beta $alpha))))
     ((LC (&- $i) (appl $f $alpha $beta)
          $i (appl $f $beta $alpha))
      $=
      (&- (&i* $i (OverBar (appl $f $alpha $beta)))
          (&i* $i (OverBar (appl $f $beta $alpha))))))
    "给第二个等式乘上" $i ", 然后再加上第一个等式, 我们就得到"
    (MB (&= (&i* $2 (appl $f $alpha $beta))
            (&i* $2 (OverBar (appl $f $beta $alpha)))))
    "即"
    (MB (&= (appl $f $alpha $beta)
            (OverBar (appl $f $beta $alpha))) "."))
   ((corollary)
    "令" $T "是有限维复内积空间" $V "上的一个线性算子, 那么"
    $T "是自伴算子当且仅当对于每个" (∈ $alpha $V) ", "
    (inner* (ap $T $alpha) $alpha) "是实数.")
   ((tcomment)
    "实际上, 即便没有有限维的条件, 以上推论仍然成立.")
   ((theorem #:n "4. 主轴定理")
    "对于有限维内积空间" $V "上的每个Hermite形式" $f
    ", 存在" $V "的一个规范正交基使得" $f
    "在其下由一个实对角矩阵表示.")
   ((proof)
    "根据定理1, 存在唯一的线性算子" $T "使得"
    (&= (appl $f $alpha $beta)
        (inner* (ap $T $alpha) $beta))
    ". 根据之前的观察, 既然" $f "是Hermite形式, 那么"
    $T "是自伴算子. 根据第8章的定理18, 我们知道存在" $V
    "的一个规范正交基" $BBB "使得" $T "由对角矩阵表示. "
    "当然, 根据第8章的定理15, 这个对角矩阵的元素均为实数. "
    "我们知道, " $f "在规范正交基" $BBB "下的矩阵即"
    $T "在" $BBB "下的表示, 所以" $f "在" $BBB
    "下也由实对角矩阵表示.")
   ((corollary)
    "对于有限维内积空间" $V "上的Hermite形式" $f
    ", 存在一个规范正交基" $BBB "使得对于每个"
    (∈ $alpha $beta $V) ", 若" (tu0 $x_1 $..h $x_n)
    "和" (tu0 $y_1 $..h $y_n) "分别是" $alpha "和"
    $beta "在" $BBB "下的坐标, 那么"
    (MB (&= (appl $f $alpha $beta)
            (sum (&= $j $1) $n
                 (&i* $c_j $x_j
                      (_ (OverBar $y) $j)))))
    "其中" (&..cm $c_1 $c_n) "是固定的实数.")
   ((exercise #:n "1")
    "请问下列函数" (func $f (&c* $CC^2 $CC^2) $CC)
    "中哪些是" $CC^2 "上的(半双线性)形式, 其中我们设"
    (&= $alpha (tu0 $x_1 $x_2)) ", "
    (&= $beta (tu0 $y_1 $y_2)) "?"
    (Ol #:attr* '((type "a"))
        (Li (&= (appl $f $alpha $beta) $1) ".")
        (Li (&= (appl $f $alpha $beta)
                (&+ (&sqr (@- $x_1 (_ (OverBar $y) $1)))
                    (&i* $x_2 (_ (OverBar $y) $2)))) ".")
        (Li (&= (appl $f $alpha $beta)
                (&- (&sqr (@+ $x_1 (_ (OverBar $y) $1)))
                    (&sqr (@- $x_1 (_ (OverBar $y) $1))))) ".")
        (Li (&= (appl $f $alpha $beta)
                (&- (&i* $x_1 (_ (OverBar $y) $2))
                    (&i* (_ (OverBar $x) $2) $y_1))) ".")))
   ((exercise #:n "2")
    "令"
    (MB (&= (appl $f (tu0 $x_1 $x_2) (tu0 $y_1 $y_2))
            (&+ (&i* $x_1 $y_1) (&i* $x_2 $y_2))))
    "是" $RR^2 "上的形式, 找出" $f "在以下的每个基下的矩阵:"
    (MB (&cm (setE (tu0 $1 $0) (tu0 $0 $1))
             (setE (tu0 $1 $-1) (tu0 $1 $1))
             (setE (tu0 $1 $2) (tu0 $3 $4))) "."))
   ((exercise #:n "3")
    "令"
    (MB (&= $A (MatR ($1 $i) ((&- $i) $2))))
    "而" (&= (appl $g $X $Y) (&i* (&* $Y) $A $X))
    "是" (^ $CC (&c* $2 $1)) "上的形式, 那么" $g
    "是一个内积吗?")
   ((exercise #:n "4")
    "令" $V "是一个复向量空间而" $f "是" $V
    "上的一个对称的(半双线性)形式, 即"
    (&= (appl $f $alpha $beta)
        (appl $f $beta $alpha))
    ", 那么" $f "是什么呢?")
   ((exercise #:n "5")
    "令"
    (MB (&= (appl $f (tu0 $x_1 $x_2) (tu0 $y_1 $y_2))
            (&+ (&i* $x_1 $y_1)
                (&i* $4 $x_2 $y_2)
                (&i* $2 $x_1 $y_2)
                (&i* $2 $x_2 $y_1))))
    "是" $RR^2 "上的形式, 找到一个有序基使得" $f
    "由一个对角矩阵表示.")
   ((exercise #:n "6")
    "称形式" $f "为(左)非退化的, 如果对于每个向量"
    $beta "有" (&= (appl $f $alpha $beta) $0)
    "可以推出" (&= $alpha $0) ". 令" $f
    "是有限维内积空间" $V "上的一个形式, 证明" $f
    "是非退化的当且仅当其对应的线性算子" $T_f
    " (定理1) 是非奇异的.")
   ((exercise #:n "7")
    "令" $f "是有限维向量空间" $V "上的一个形式. "
    "参考练习6给出的左非退化的概念, 定义右非退化, 并证明"
    $f "是左非退化的当且仅当" $f "是右非退化的.")
   ((exercise #:n "8")
    "令" $f "是有限维向量空间" $V
    "上的一个非退化形式 (练习6和7), " $L "是" $V
    "上的一个线性泛函, 证明存在唯一的" (∈ $beta $V)
    "使得对于每个" (∈ $alpha $V) "有"
    (&= (app $L $alpha) (appl $f $alpha $beta)) ".")
   ((exercise #:n "9")
    "令" $f "是有限维向量空间" $V
    "上的一个非退化形式, 证明每个线性算子"
    $S "都有一个&quot;相对于" $f
    "的伴随&quot;, 即一个线性算子" $S^ "满足对于每个"
    (∈ $alpha $beta $V) "有"
    (&= (appl $f (ap $S $alpha) $beta)
        (appl $f $alpha (ap $S^ $beta))) ".")
   (H3 "第9.3节 正定形式")
   (P "本节我们将讨论非负(半双线性)形式以及"
      "其与向量空间上的给定内积之间的关系.")
   ((definition)
    "给定实或复向量空间" $V ", 其上的形式"
    $f "被称为" (B "非负的") ", 如果" $f
    "是Hermite的并且对于每个" (∈ $alpha $V)
    "有" (&>= (appl $f $alpha $alpha) $0)
    "; 其上的形式" $f "被称为" (B "正定的")
    ", 如果" $f "是Hermite的并且对于每个非零向量"
    (∈ $alpha $V) "有"
    (&> (appl $f $alpha $alpha) $0) ".")
   ((tcomment)
    "&quot;非负&quot;这个术语现在一般被"
    "&quot;半正定&quot;所代替.")
   (P $V "上的正定形式实际上就是" $V
      "上的内积. 非负形式几乎就是内积了, "
      "除了某些非零向量可能&quot;正交&quot;于自身.")
   (P "令" $f "是有限维向量空间" $V
      "上的一个形式, " basis:def "是" $V
      "的一个有序基, " $A "是" $f "在基" $BBB
      "下的矩阵, 即"
      (&= (mref $A $j $k)
          (appl $f $alpha_k $alpha_j))
      ". 如果"
      (&= $alpha (LC0 $x_1 $alpha_1 $x_n $alpha_n))
      ", 那么"
      (eqnderiv
       (appl $f $alpha $alpha)
       (ap $f (tup (sum (&= $j $1) $n
                        (&i* $x_j $alpha_j))
                   (sum (&= $k $1) $n
                        (&i* $x_k $alpha_k))))
       (sum (&= $j $1) $n
            (sum (&= $k $1) $n
                 (&i* $x_j (_ (OverBar $x) $k)
                      (appl $f $alpha_j $alpha_k))))
       (sum (&= $j $1) $n
            (sum (&= $k $1) $n
                 (&i* (_ (OverBar $x) $k)
                      (mref $A $k $j)
                      $x_j))))
      "于是, 我们看出来" $f "是非负形式当且仅当"
      (MB (&= $A (&* $A)))
      "[译注: 这是" $f "为Hermite形式的充要条件] 且"
      (MB (&>= (sum (&= $j $1) $n
                    (sum (&= $k $1) $n
                         (&i* (_ (OverBar $x) $k)
                              (mref $A $k $j)
                              $x_j))) $0)
          "对于任意的标量" (&..cm $x_1 $x_n) "成立.")
      "为了使得" $f "成为正定形式, 以上的不等式必须对于每个"
      (&!= (tu0 $x_1 $..h $x_n) $0)
      "严格成立. 刚才我们推导出的条件说明" $f
      "是" $V "上的一个正定形式当且仅当函数"
      (MB (&= (appl $g $X $Y) (&i* (&* $Y) $A $X)))
      "是列矩阵空间" (^ $F n*1) "上的正定形式, 其中"
      $F "是向量空间" $V "的标量域.")
   ((theorem #:n "5")
    "令" $F "是实数域或者复数域, " $A "是域" $F
    "上的一个" n*n "矩阵, 那么由"
    (MB (&= (appl $g $X $Y) (&i* (&* $Y) $A $X)))
    "定义的函数" $g "是" (^ $F n*1)
    "上的正定形式当且仅当存在一个可逆矩阵" (∈ $P (^ $F n*n))
    "满足" (&= $A (&i* (&* $P) $P)) ".")
   ((proof)
    "对于任意的" n*n "矩阵" $A ", 函数" $g
    "都是列矩阵空间上的(半双线性)形式. 我们想要证明的是, "
    $g "为正定的当且仅当" (&= $A (&i* (&* $P) $P))
    ". 首先, 设" (&= $A (&i* (&* $P) $P)) ", 那么"
    $g "是Hermite的, 并且"
    (MB (deriv0
         (appl $g $X $X) $=
         (&i* (&* $X) (&* $P) $P $X) $=
         (&i* (&* (@i* $P $X)) (&i* $P $X)) $>=
         $0))
    "若" $P "是可逆的, 那么" (&!= $X $0) "时"
    (&!= (&i* $P $X) $0) ", 于是"
    (&> (&i* (&* (@i* $P $X)) (&i* $P $X)) $0) "." (Br)
    "现在, 设" $g "是列矩阵空间上的正定形式, 那么"
    $g "就是一个内积, 因而存在列矩阵" (&..cm $Q_1 $Q_n) "使得"
    (eqnderiv (&delta $j $k)
              (appl $g $Q_j $Q_k)
              (&i* (_^ $Q $k $*) $A $Q_j))
    "但是, 这不过就是在说, 如果" $Q "是以" (&..cm $Q_1 $Q_n)
    "为列的矩阵, 那么" (&= (usim $A $Q) $I) ". 既然"
    (setE $Q_1 $..h $Q_n) "相对于内积" $g
    "是一个规范正交基, 所以" $Q "是可逆的. 令"
    (&= $P (inv $Q)) ", 我们就得到" (&= $A (&i* (&* $P) $P)) ".")
   (P "在实践中, 验证一个给定的矩阵" $A "满足我们到目前为止给出的"
      "正定判则并非易事. 定理5的一个推论是, 若" $g
      "为正定形式, 那么" (&> (&det $A) $0) ", 因为"
      (MB (&= (&det $A) (&det (&i* (&* $P) $P))
              (&i* (&det0 (&* $P)) (&det0 $P))
              (&sqr (&abs (&det $P)))) ".")
      "然而, " (&> (&det $A) $0) "并不足以保证" $g
      "是正定形式. 不过, 存在与" $A "相关联的" $n
      "个行列式具有此性质: 如果" (&= $A (&* $A))
      "且这些行列式均为正数, 那么" $g "是一个正定形式.")
   ((definition)
    "令" $A "是域" $F "上的一个" n*n "矩阵, 那么"
    $A "的" (B "顺序主子式 (principal minor)") "是由"
    (MB (&cm (&= (pminor $A)
                 (ap $det (Mat ((mref $A $1 $1) $..c (mref $A $1 $k))
                               ($..v $ $..v)
                               ((mref $A $k $1) $..c (mref $A $k $k)))))
             (&<= $1 $k $n)))
    "定义的" $n "个标量" (&..cm (pminor $A $1) (pminor $A $n)) ".")
   ((lemma)
    "令" $A "是域" $F "上的一个" n*n "的可逆矩阵, 那么以下陈述是等价的."
    (Ol #:attr* '((type "a"))
        (Li "存在一个主对角线元素全为" $1 "的上三角矩阵" $P "使得"
            (&= $B (&i* $A $P)) "是下三角矩阵.")
        (Li $A "的顺序主子式均异于" $0 ".")))
   ((proof)
    "令" $P "是任意的" n*n "矩阵, 置" (&= $B (&i* $A $P)) ", 那么"
    (MB (&= (mref $B $j $k)
            (sum (&= $r $1) $n
                 (&i* (mref $A $j $r)
                      (mref $P $r $k)))) ".")
    "如果" $P "是一个主对角线均为" $1 "的上三角矩阵, 那么"
    (MB (&= (sum (&= $r $1) (&- $k $1)
                 (&i* (mref $A $j $r)
                      (mref $P $r $k)))
            (&- (mref $B $j $k) (mref $A $j $k))) ".")
    "既然" $B "为下三角矩阵等价于" (&< $j $k) "时有"
    (&= (mref $B $j $k) $0) ", 因而" $B "为下三角矩阵当且仅当"
    (MB (&cm (&= (sum (&= $r $1) (&- $k $1)
                      (&i* (mref $A $j $r)
                           (mref $P $r $k)))
                 (&- (mref $A $j $k)))
             (&< $j $k)) ".")
    "我们可以将以上式子看成是关于" (mref $P $r $k)
    "的线性方程组, 那么陈述a就等价于该方程组有解." (Br)
    "实际上, 我们最好将这个大的线性方程组按照" $k
    "拆分. 对于每个" (&= $k (&..cm $2 $n))
    ", 我们有一个关于未知元"
    (&..cm (mref $P $1 $k) (mref $P (&- $k $1) $k))
    "的具" (&- $k $1) "个方程的线性方程组, 其系数矩阵为"
    (MB (Mat ((mref $A $1 $1) $..c (mref $A $1 (&- $k $1)))
             ($..v $ $..v)
             ((mref $A (&- $k $1) $1)
              $..c (mref $A (&- $k $1) (&- $k $1)))))
    "这个矩阵的行列式即顺序主子式" (pminor $A (&- $k $1))
    ". 若陈述b成立, 那么这些线性方程组都有唯一解. 也就是说, "
    "大的线性方程组也有唯一解. 于是, 陈述a成立, 并且矩阵"
    $P "实际上是唯一的. 因此, 陈述b可以推出陈述a." (Br)
    "现在设a成立, 那么"
    (eqnderiv
     (pminor $B)
     (pminor (&i* $A $P))
     (&i* (pminor $A) (pminor $P))
     (pminor $A)
     (&..i* (mref $B $1 $1) (mref $B $k $k)))
    "其中" (preserve pminor $A &i* $P)
    "利用了" $P "是上三角矩阵的事实. 既然" $A
    "和" $P "均可逆, 那么" $B "也可逆. 鉴于下三角矩阵"
    $B "可逆等价于"
    (&cm (&!= (mref $B $k $k) $0)
         (&= $k (&..cm $1 $n)))
    ", 于是"
    (MB (&cm (&!= (pminor $A) $0)
             (&= $k (&..cm $1 $n))) "."))
   ((theorem #:n "6")
    "令" $f "是有限维向量空间" $V "上的一个形式, "
    $A "是" $f "在" $V "的某个有序基" $BBB
    "下的矩阵, 那么" $f "是正定形式当且仅当"
    (&= $A (&* $A)) "并且" $A "的顺序主子式均为正数.")
   ((proof)
    "让我们先来证明这个定理有趣的一半. 设" (&= $A (&* $A))
    ", 并且" (&cm (&> (pminor $A) $0) (&<= $1 $k $n))
    ". 根据引理, 存在(唯一的)主对角线均为" $1
    "的上三角矩阵" $P "使得" (&= $B (&i* $A $P))
    "是下三角矩阵. 矩阵" (&* $P) "当然是一个下三角矩阵, 于是"
    (&= (&i* (&* $P) $B) (usim $A))
    "也是下三角的. 既然" $A "是自伴的, 那么"
    (&= $D (usim $A)) "也是自伴的. 显然, 自伴的下三角矩阵"
    "必然是一个对角矩阵. 按照前面引理的证明里的类似手法, "
    "我们可以推出"
    (eqnderiv
     (pminor $D)
     (pminor (&i* (&* $P) $B))
     (&i* (pminor (&* $P)) (pminor $B))
     (pminor $B)
     (pminor $A))
    "鉴于" $D "是一个对角矩阵, 其顺序主子式为"
    (MB (&= (pminor $D)
            (&..i* (mref $D $1 $1)
                   (mref $D $k $k))) ".")
    "因为" $A "的顺序主子式均为正数, 所以" $D
    "的顺序主子式也均为正数, 那么我们可以推出"
    (MB (&cm (&> (mref $D $k $k) $0)
             (&<= $1 $k $n)) ".")
    "如果" $A "是形式" $f "在有序基" basis:def
    "下的矩阵, 那么" (&= $D (usim $A))
    "是形式" $f "在有序基"
    (setE (_^ $alpha $1 $prime) $..h
          (_^ $alpha $n $prime))
    "下的矩阵, 其中"
    (MB (&= (_^ $alpha $j $prime)
            (sum (&= $i $1) $n
                 (&i* (mref $P $i $j)
                      $alpha_i))) ".")
    "既然" $D "是主对角线元素均为正数的对角矩阵, 那么显然有"
    (MB (&cm (&> (&i* (&* $X) $D $X) $0)
             (&!= $X $0)) ".")
    "这就说明" $f "是一个正定形式." (Br)
    "现在反过来设" $f "是正定形式. 我们知道"
    (&= $A (&* $A)) ", 但是该怎么说明"
    (&cm (&> (pminor $A) $0) (&<= $1 $k $n))
    "呢? 令" $V_k "是由" (&..cm $alpha_1 $alpha_k)
    "张成的子空间, 而" $f_k "是" $f "在"
    (&c* $V_k $V_k) "上的限制, 那么显然" $f_k
    "是" $V_k "上的正定形式, 且" $f_k "在有序基"
    (setE $alpha_1 $..h $alpha_k) "下的表示为"
    (MB (&= $A_k
            (Mat ((mref $A $1 $1) $..c (mref $A $1 $k))
                 ($..v $ $..v)
                 ((mref $A $k $1) $..c (mref $A $k $k)))) ".")
    "作为定理5的推论, 我们注意到每个正定形式" $f_k
    "的矩阵表示" $A_k "的行列式都应该是正数, 即" $A
    "的每个顺序主子式" (pminor $A) "均为正数.")
   (P "这里有一些我们应该作出的评注, 以完成我们对于"
      "正定形式和正定矩阵之间的关系的讨论. "
      "什么刻画了表示正定形式的矩阵? 如果" $f
      "是有限维复向量空间上的一个形式, " $A
      "是" $f "在某个有序基下的矩阵, 那么" $f
      "是正定的当且仅当" (&= $A (&* $A)) "且"
      (MB (&cm (&> (&i* (&* $X) $A $X) $0)
               (&!= $X $0)) ".")
      "根据定理3, " (&= $A (&* $A))
      "的条件是多余的, 因为"
      (&cm (&> (&i* (&* $X) $A $X) $0)
           (&!= $X $0))
      "可以推出" (&= $A (&* $A)) ". 另一方面, 如果"
      $f "是有限维实向量空间上的形式而" $A
      "是" $f "在某个有序基下的矩阵, 那么"
      $f "是正定的当且仅当" (&= $A $A^t) "且"
      (MB (&cm (&> (&i* $X^t $A $X) $0)
               (&!= $X $0)) ".")
      "我们想要强调的是, 实情形下"
      (&cm (&> (&i* $X^t $A $X) $0) (&!= $X $0))
      "无法推出" (&= $A $A^t)
      ". 然而, 值得注意的是, 如果实矩阵" $A
      "满足" (&= $A $A^t) "和"
      (&cm (&> (&i* $X^t $A $X) $0) (&!= $X $0))
      ", 那么即便对于每个复的列矩阵" $X ", 我们也有"
      (MB (&cm (&> (&i* (&* $X) $A $X) $0)
               (&!= $X $0)) ".")
      "这是因为, 若" (&= $X (Complex $Y $Z))
      ", 其中" (∈ $Y $Z (^ $RR n*1)) ", 那么"
      (eqnderiv
       (&i* (&* (@ (Complex $Y $Z))) $A
            (@ (Complex $Y $Z)))
       (&i* (@- $Y^t (&i* $i $Z^t)) $A
            (@ (Complex $Y $Z)))
       (&+ (&i* $Y^t $A $Y)
           (&i* $Z^t $A $Z)
           (&i* $i (@- (&i* $Y^t $A $Z)
                       (&i* $Z^t $A $Y)))))
      "而在" (&= $A $A^t) "的情况下, 有"
      (&= (&i* $Y^t $A $Z) (&i* $Z^t $A $Y)) ".")
   (P "如果" $A "是一个" n*n "的复矩阵并且满足"
      (MB (&cm (&> (&i* (&* $X) $A $X) $0)
               (&!= $X $0)))
      "那么我们就称" $A "是一个" (B "正定矩阵")
      ". 我们已经知道, 有限维复向量空间上的形式"
      "是正定的当且仅当其在某个有序基下的矩阵是"
      "正定矩阵. (这里的&quot;某个&quot;也可以被"
      "替换为&quot;每个&quot;.) 但是, "
      "刚才的评注告诉我们, 即便是在实情形下, "
      "我们还是可以断言形式正定的充要条件"
      "为其在某个有序基下的矩阵正定. "
      "当然, 我们这里将实矩阵也视为复矩阵. "
      "不过, 读者需要注意的是, 即便是实矩阵, "
      "其正定的条件亦是相对于每个非零的复列矩阵而言的.")
   (P "现在设" $V "是一个有限维内积空间而" $f
      "是" $V "上的一个非负形式, 那么存在唯一的" $V
      "上的一个自伴算子" $T "满足"
      (MB (&= (appl $f $alpha $beta)
              (inner* (ap $T $alpha) $beta)))
      "并且" $T "还具有"
      (&>= (inner* (ap $T $alpha) $alpha) $0)
      "的额外性质.")
   ((definition)
    "设" $V "是一个有限维内积空间. " $V "上的一个线性算子"
    $T "是" (B "非负的") ", 如果" (&= $T (&* $T))
    "且对于每个" (∈ $alpha $V) "有"
    (&>= (inner* (ap $T $alpha) $alpha) $0)
    ". " $V "上的一个线性算子" $T "是" (B "正定的")
    ", 如果" (&= $T (&* $T)) "且对于每个" (&!= $alpha $0)
    "有" (&> (inner* (ap $T $alpha) $alpha) $0) ".")
   (P "如果" $V "是一个有限维的(实或复)向量空间而"
      (inner* $ $) "是" $V "上的一个内积, 那么" $V
      "上有个与之相关联的正定算子类. 通过定理1"
      "所描述的映射, " $V "上所有正定形式构成的集合"
      "与所有正定算子构成的集合之间存在一个双射. "
      "我们将以本节的练习来强调正定算子, 正定形式, "
      "正定矩阵之间的关系. 以下的总结或许是有用的.")
   (P "如果" $A "是一个复数域上的" n*n
      "矩阵, 那么以下陈述是等价的."
      (Ol (Li $A "是正定矩阵, 即对于不全为零的复数"
              (&..cm $x_1 $x_n) ", 我们有"
              (&> (sum (&= $j $1) $n
                       (sum (&= $k $1) $n
                            (&i* (_ (OverBar $x) $k)
                                 (mref $A $k $j)
                                 $x_j))) $0) ".")
          (Li (&= (inner* $X $Y) (&i* (&* $Y) $A $X))
              "是" n*1 "的复矩阵空间上的一个内积.")
          (Li "相对于" n*1 "的复矩阵空间上的标准内积"
              (&= (inner* $X $Y) (&i* (&* $Y) $X))
              ", 线性算子" (&\|-> $X (&i* $A $X))
              "是正定的.")
          (Li "存在某个可逆的" (∈ $P (^ $CC n*n))
              "满足" (&= $A (&i* (&* $P) $P)) ".")
          (Li (&= $A (&* $A)) "且" $A
              "的顺序主子式均为正数."))
      "若" $A "的每个元素均为实数, 那么以上这些又等价于"
      (Ol #:attr* '((start "6"))
          (Li (&= $A $A^t) "且对于不全为零的实数"
              (&..cm $x_1 $x_n) ", 我们有"
              (&> (sum (&= $j $1) $n
                       (sum (&= $k $1) $n
                            (&i* $x_k
                                 (mref $A $k $j)
                                 $x_j))) $0) ".")
          (Li (&= (inner* $X $Y) (&i* $Y^t $A $X))
              "是" n*1 "的实矩阵空间上的一个内积.")
          (Li "相对于" n*1 "的实矩阵空间上的标准内积"
              (&= (inner* $X $Y) (&i* $Y^t $X))
              ", 线性算子" (&\|-> $X (&i* $A $X))
              "是正定的.")
          (Li "存在某个可逆的" (∈ $P (^ $RR n*n))
              "满足" (&= $A (&i* $P^t $P)) ".")))
   ((exercise #:n "1")
    "令" $V "是带有标准内积的" $CC^2 ", 对于什么样的向量"
    (∈ $alpha $V) ", 存在一个正定算子" $T
    "使得" (&= $alpha (ap $T $epsilon_1)) "呢?")
   ((exercise #:n "2")
    "令" $V "是带有标准内积的" $RR^2 ", 如果"
    $theta "是一个实数, 令" $T_theta "是逆时针旋转"
    $theta "的线性算子, 即"
    (MB (&= (appl $T_theta $x_1 $x_2)
            (tu0 (&- (&i* $x_1 (&cos $theta))
                     (&i* $x_2 (&sin $theta)))
                 (&+ (&i* $x_1 (&sin $theta))
                     (&i* $x_2 (&cos $theta))))))
    $theta "为何值时" $T_theta "是正定算子呢?")
   ((exercise #:n "3")
    
    )
   ((exercise #:n "4")
    
    )
   ((exercise #:n "5")
    
    )
   ((exercise #:n "6")
    
    )
   ((exercise #:n "7")
    
    )
   ((exercise #:n "8")
    
    )
   ((exercise #:n "9")
    
    )
   ((exercise #:n "10")
    
    )
   ((exercise #:n "11")
    
    )
   ((exercise #:n "12")
    
    )
   ((exercise #:n "13")
    
    )
   ((exercise #:n "14")
    
    )
   ((exercise #:n "15")
    
    )
   (H3 "第9.4节 更多关于形式的结果")
   (P "本节包含两个结果, 其给出了关于(半双线性)形式的更加详细的信息.")
   ((theorem #:n "7")
    "设" $V "是一个实或复向量空间, " $W "是" $V "的一个有限维子空间并且"
    (setE $alpha_1 $..h $alpha_r) "是其一个有序基. 令" $f "是" $V
    "上的一个形式而" $M "是由"
    (MB (&= (mref $M $j $k) (appl $f $alpha_k $alpha_j)))
    "定义的" (&c* $r $r) "矩阵. 如果"
    (MB (&= $W^ (setI (∈ $beta $V)
                      (: "对于任意的" (∈ $alpha $W)
                         ",&nbsp;" (&= (appl $f $alpha $beta) $0)))))
    "那么" $W^ "是" $V "的一个子空间, 并且" (&= (&cap $W $W^) (setE $0))
    "当且仅当" $M "可逆. 当的确如此时, " (&= $V (&d+ $W $W^)) ".")
   ((proof)
    
    )
   ((theorem #:n "8")
    "设" $V "是一个有限维的实或复向量空间, " $f "是" $V "上的一个形式而"
    $A "是" $f "在" $V "的某个有序基" (setE $alpha_1 $..h $alpha_n)
    "下的矩阵. 如果" $A "的顺序主子式均异于零, 那么存在唯一的主对角线元素全为"
    $1 "的上三角矩阵" $P "使得"
    (MB (&i* (&* $P) $A $P))
    "是一个上三角矩阵.")
   ((proof)
    
    )
   (H3 "第9.5节 谱论")
   (P "本节"
      )
   (H3 "第9.6节 正规算子的更深刻性质")
   ))