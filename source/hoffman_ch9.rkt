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
      (MB (&= $A^ (&i* (&* $P) $A $P)))
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
       (mref (@i* (&* $P) $A $P) $j $k))
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
    (&= (&i* (&* $P) $B) (&i* (&* $P) $A $P))
    "也是下三角的. 既然" $A "是自伴的, 那么"
    (&= $D (&i* (&* $P) $A $P)) "也是自伴的. 显然, 自伴的下三角矩阵"
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
    "下的矩阵, 那么" (&= $D (&i* (&* $P) $A $P))
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
    "令" $V "是" (^ $CC n*1) ", 而其上的内积为"
    (&= (inner* $X $Y) (&i* (&* $Y) $G $X))
    ", 这里的" (∈ $G (^ $CC n*n))
    "要使得该公式的确定义了一个内积. 令" $A
    "是一个" n*n "的矩阵而线性算子"
    (&= (app $T $X) (&i* $A $X))
    ". 找出" (&* $T) ". 如果" $Y "是" $V
    "的一个固定元素, 找出确定了线性泛函"
    (&\|-> $X (&i* (&* $Y) $X))
    "的元素" (∈ $Z $V) ". 换言之, 对于每个" (∈ $X $V)
    "有" (&= (&i* (&* $Y) $X) (inner* $X $Z)) ".")
   ((exercise #:n "4")
    "令" $V "是一个有限维内积空间. 如果" $T "和" $U
    "是" $V "上的正定算子, 证明" (@+ $T $U)
    "也是正定算子. 给出一个例子表明" (&i* $T $U)
    "不必是正定的.")
   ((exercise #:n "5")
    "令"
    (MB (&= $A (Mat ($1 1/2) (1/2 1/3))) ".")
    (Ol #:attr* '((type "a"))
        (Li "证明" $A "是正定的.")
        (Li "令" $V "是" (^ $RR (&c* $2 $1))
            ", 而其上的内积为"
            (&= (inner* $X $Y) (&i* $Y^t $A $X))
            ". 现在定义"
            (MB (&cm (&= $X_1 (Mat ($1) ($0)))
                     (&= $X_2 (Mat ($0) ($1)))))
            "请应用Gram-Schmidt过程以找出" $V
            "的一个规范正交基.")
        (Li "找出一个" 2*2 "的可逆实矩阵" $P
            "使得" (&= $A (&i* $P^t $P)) ".")))
   ((exercise #:n "6")
    "以下哪些矩阵是正定的?"
    (MB (&cm (Mat ($1 $2) ($3 $4))
             (Mat ($1 (&+ $1 $i))
                  ((&- $1 $i) $3))
             (Mat ($1 $-1 $1)
                  ($2 $-1 $1)
                  ($3 $-1 $1))
             (Mat (1 1/2 1/3)
                  (1/2 1/3 1/4)
                  (1/3 1/4 1/5)))))
   ((exercise #:n "7")
    "给出一个" n*n "矩阵的例子, 其所有顺序主子式均为正数, "
    "但是并非正定矩阵.")
   ((exercise #:n "8")
    (&= (inner* (tu0 $x_1 $x_2) (tu0 $y_1 $y_2))
        (&+ (&i* $x_1 (_ (OverBar $y) $1))
            (&i* $2 $x_2 (_ (OverBar $y) $1))
            (&i* $2 $x_1 (_ (OverBar $y) $2))
            (&i* $x_2 (_ (OverBar $y) $2))))
    "定义了" $CC^2 "上的一个内积吗?")
   ((exercise #:n "9")
    "证明正定矩阵的每个主对角线元素均为正数.")
   ((exercise #:n "10")
    "令" $V "是一个有限维内积空间. 如果" $T "和" $U
    "是" $V "上的线性算子, 当" (&- $U $T)
    "为正定算子时我们记" (&< $T $U)
    ". 证明以下断言:"
    (Ol #:attr* '((type "a"))
        (Li (&< $T $U) "和" (&< $U $T)
            "不能同时成立.")
        (Li "如果" (&< $T $U) "且" (&< $U $S)
            ", 那么" (&< $T $S) ".")
        (Li "如果" (&< $T $U) "且" (&< $0 $S)
            ", " (&< (&i* $S $T) (&i* $S $U))
            "不必成立.")))
   ((exercise #:n "11")
    "令" $V "是一个有限维内积空间而" $E "是" $V
    "在其某个子空间上的正交投影."
    (Ol #:attr* '((type "a"))
        (Li "证明对于任意的正数" $c ", 算子"
            (LC $c $I $E) "是正定的.")
        (Li "以" $E "表达满足" (&= $T^2 (&+ $I $E))
            "自伴线性算子" $T ".")))
   ((exercise #:n "12")
    "设" $n "是一个正整数而"
    (MB (&= $A (BigMat ($1 1/2 1/3 $..c (~ $1 $n))
                       (1/2 1/3 1/4 $..c (~ $1 (&+ $n $1)))
                       ($..v $..v $..v $ $..v)
                       ((~ $1 $n)
                        (~ $1 (&+ $n $1)) (~ $1 (&+ $n $2)) $..c
                        (~ $1 (&- (&i* $2 $n) $1))))) ".")
    "证明" $A "是正定的.")
   ((exercise #:n "13")
    "令" $A "是一个自伴的" n*n "矩阵, 证明存在正数"
    $c "使得矩阵" (LC $c $I $A) "是正定的.")
   ((exercise #:n "14")
    "证明两个正定线性算子之积是正定的当且仅当它们交换.")
   ((exercise #:n "15")
    "令" $S "和" $T "是正定算子, 证明" (&i* $S $T)
    "的每个特征值都是正数.")
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
   (let ((g_j (lambda (beta)
                (sum (&= $k $1) $r
                     (&i* (mref $A $j $k)
                          (OverBar
                           (appl $f $alpha_k beta)))))))
     ((proof)
      "如果" (∈ $beta $gamma $W^) "而" $c "是一个标量, 那么对于每个"
      (∈ $alpha $W) ", 我们可以推出"
      (MB (&= (appl $f $alpha (LC $c $beta $gamma))
              (LC (OverBar $c) (appl $f $alpha $beta)
                  (appl $f $alpha $gamma)) $0) ".")
      "因此, " $W^ "的确是" $V "的一个子空间." (Br)
      "现在设"
      (MB (&= $alpha (sum (&= $k $1) $r (&i* $x_k $alpha_k))) "和"
          (&= $beta (sum (&= $j $1) $r (&i* $y_j $alpha_j))))
      "那么"
      (eqnderiv (appl $f $alpha $beta)
                (sum (&= $k $1) $r
                     (sum (&= $j $1) $r
                          (&i* (_ (OverBar $y) $j)
                               (mref $M $j $k)
                               $x_k)))
                (sum (&= $k $1) $r
                     (&i* (@sum (&= $j $1) $r
                                (&i* (_ (OverBar $y) $j)
                                     (mref $M $j $k)))
                          $x_k)))
      "由此可知" (∈ $beta $W^) "当且仅当方程组"
      (MB (&cm (&= (sum (&= $j $1) $r
                        (&i* (_ (OverBar $y) $j)
                             (mref $M $j $k))) $0)
               (&<= $1 $k $r)))
      "成立, 因而" (&!= (&cap $W $W^) (setE $0))
      "当且仅当齐次线性方程组"
      (MB (&cm (&= (sum (&= $j $1) $r
                        (&i* (mref (OverBar $M) $j $k)
                             $y_j)) $0)
               (&<= $1 $k $r)))
      "具有非平凡解. 换言之, " (&= (&cap $W $W^) (setE $0))
      "等价于" (&* $M) "可逆, 但" (&* $M) "可逆当且仅当"
      $M "可逆." (Br)
      "设" $M "可逆并令"
      (MB (&= $A (inv (@* $M)) (&* (@inv $M))))
      "我们定义" $V "上的函数" $g_j "为"
      (MB (&= (app $g_j $beta) (g_j $beta)))
      "那么"
      (eqnderiv (app $g_j (LC $c $beta $gamma))
                (g_j (LC $c $beta $gamma))
                (LC $c (g_j $beta) (g_j $gamma))
                (LC $c (app $g_j $beta) (app $g_j $gamma)))
      "也就是说, 每个" $g_j "的确都是" $V "上的线性泛函. 因此, 我们可以定义"
      $V "上的一个线性算子" $E "为"
      (MB (&= (ap $E $beta)
              (sum (&= $j $1) $r
                   (&i* (app $g_j $beta) $alpha_j))))
      "既然"
      (eqnderiv (app $g_j $alpha_n)
                (g_j $alpha_n)
                (sum (&= $k $1) $r
                     (&i* (mref $A $j $k)
                          (_^ $M (&cm $k $n) $*)))
                (mref (@i* $A (&* $M)) $j $n)
                (&delta $j $n))
      "我们可以推出"
      (MB (&cm (&= (app $E $alpha_n) $alpha_n)
               (&<= $1 $n $r)))
      "换言之, 对于每个" (∈ $alpha $W) ", "
      (&= (ap $E $alpha) $alpha)
      ". 现在我们知道" $E "的像是" $W "并且"
      (&= $E^2 $E) ", 即" $E "是从" $V "到"
      $W "上的投影. 若" $beta "是" $V
      "中任意的一个向量, 那么"
      (eqnderiv
       (appl $f $alpha_n (ap $E $beta))
       (appl $f $alpha_n
             (sum (&= $j $1) $r
                  (&i* (app $g_j $beta)
                       $alpha_j)))
       (sum (&= $j $1) $r
            (&i* (OverBar (app $g_j $beta))
                 (appl $f $alpha_n $alpha_j)))
       (sum (&= $j $1) $r
            (&i* (@sum (&= $k $1) $r
                       (&i* (mref (OverBar $A) $j $k)
                            (appl $f $alpha_k $beta)))
                 (appl $f $alpha_n $alpha_j))))
      "既然" (&= (&* $A) (inv $M)) ", 我们可以推出"
      (eqnderiv
       (appl $f $alpha_n (ap $E $beta))
       (sum (&= $k $1) $r
            (&i* (@sum (&= $j $1) $r
                       (&i* (_^ $A (&cm $k $j) $*)
                            (appl $f $alpha_n $alpha_j)))
                 (appl $f $alpha_k $beta)))
       (sum (&= $k $1) $r
            (&i* (@sum (&= $j $1) $r
                       (&i* (_^ $M (&cm $k $j) $-1)
                            (mref $M $j $n)))
                 (appl $f $alpha_k $beta)))
       (sum (&= $k $1) $r
            (&i* (&delta $k $n)
                 (appl $f $alpha_k $beta)))
       (appl $f $alpha_n $beta))
      "换言之, 对于每个" (∈ $alpha $W) ", 我们有"
      (&= (appl $f $alpha $beta) (appl $f $alpha (ap $E $beta)))
      ", 于是"
      (MB (&= (appl $f $alpha (&- $beta (ap $E $beta))) $0))
      "对于所有" (∈ $alpha $W) "和" (∈ $beta $V)
      "成立. 那么, " (∈ (ap (@- $I $E) $beta) $W^) ", 根据等式"
      (MB (&= $beta (&+ (ap $E $beta) (ap (@- $I $E) $beta))))
      "我们可以断言" (&= $V (&+ $W $W^))
      ". 当然, 依照前面的论证, 这个和是一个直和, 即"
      (&= $V (&d+ $W $W^)) ". 证明的最后, 还有一点值得提及的是, "
      (&- $I $E) "实际上是从" $V "到" $W^ "的投影. 若"
      (∈ $beta $W^) ", 那么" (&= (ap $E $beta) $0)
      ", 因此" (&= (ap (@- $I $E) $beta) $beta)
      ", 即" $W^ "是" (&- $I $E) "的像. 另外, 根据第6章的推理, "
      (&- $I $E) "的确是一个幂等线性算子."))
   (P "证明中构造的投影" $E "可由以下性质刻画: " (&= (ap $E $beta) $alpha)
      "当且仅当" (∈ $alpha $W) "且" (∈ (&- $beta $alpha) $W^)
      ". 因此, " $E "独立于其构造过程中用到的" $W "的基. 因此, 我们可以称"
      $E "是由直和分解"
      (MB (&= $V (&d+ $W $W^)))
      "确定的从" $V "到" $W "上的投影. 注意到" $E "是一个正交投影当且仅当"
      (&= $W^ (&perp $W)) ".")
   ((tcomment)
    "对于以上这段话, 读者应该回忆一下第6章和投影相关的内容. 另外, 译者"
    "觉得这最后一句话有点问题, 因为这个定理的条件并没有说" $V
    "是一个内积空间, 所以这个空间里还没有正交的概念. 但是, "
    "在一般的内积空间中, 这个论断的确是正确的, 并且" $W "无需是有限维的.")
   ((theorem #:n "8")
    "设" $V "是一个有限维的实或复向量空间, " $f "是" $V "上的一个形式而"
    $A "是" $f "在" $V "的某个有序基" (setE $alpha_1 $..h $alpha_n)
    "下的矩阵. 如果" $A "的顺序主子式均异于零, 那么存在唯一的主对角线元素全为"
    $1 "的上三角矩阵" $P "使得"
    (MB (&i* (&* $P) $A $P))
    "是一个上三角矩阵.")
   ((proof)
    "既然" (&= (pminor (&* $A)) (OverBar (pminor $A))) ", " (&* $A)
    "的顺序主子式也都异于零. 因此, 根据定理6的引理, 存在一个主对角线元素全为"
    $1 "的上三角矩阵" $P "满足" (&i* (&* $A) $P) "是一个下三角矩阵. 于是, "
    (&= (&i* (&* $P) $A) (&* (@i* (&* $A) $P))) "是一个上三角矩阵. "
    "既然两个上三角矩阵之积仍然是上三角矩阵, " (&i* (&* $P) $A $P)
    "是一个上三角矩阵. 这表明了" $P "的存在性, 但没有说明" $P
    "的唯一性. 然而, 其实有一个更加几何的论证方法可以同时说明" $P
    "的存在性和唯一性." (Br)
    "令" $W_k "是由" (&..cm $alpha_1 $alpha_k) "张成的子空间, 而"
    (MB (&= (_^ $W $k $prime)
            (setI (∈ $beta $V)
                  (: "对于任意的" (∈ $alpha $W_k) ",&nbsp;"
                     (&= (appl $f $alpha $beta) $0)))))
    "既然" (&!= (pminor $A) $0) ", 那么由"
    (MB (&= (mref $M $i $j) (appl $f $alpha_j $alpha_i) (mref $A $i $j)))
    "定义的" (&c* $k $k) "矩阵" $M "是可逆的. 根据定理7, 我们有"
    (MB (&= $V (&d+ $W_k (_^ $W $k $prime))) ".")
    "令" $E_k "是由这个直和分解决定的从" $V "到" $W_k "上的投影, 并置"
    (&= $E_0 $0) ", 设"
    (MB (&cm (&= $beta_k (&- $alpha_k
                             (ap (_ $E (&- $k $1)) $alpha_k)))
             (&<= $1 $k $n)))
    "那么" (&= $beta_1 $alpha_1) ", 而" (&> $k $1) "时有"
    (∈ (ap (_ $E (&- $k $1)) $alpha_k) (_ $W (&- $k $1)))
    ". 于是, 对于" (&> $k $1) ", 存在唯一的标量" (mref $P $j $k) "使得"
    (MB (&= (ap (_ $E (&- $k $1)) $alpha_k)
            (&- (sum (&= $j $1) (&- $k $1)
                     (&i* (mref $P $j $k) $alpha_j)))) ".")
    "再置" (&= (mref $P $k $k) $1) "以及" (&> $j $k) "时"
    (&= (mref $P $j $k) $0) ", 我们就得到了一个" n*n "的上三角矩阵"
    $P ", 其主对角线元素均为" $1 ", 并且对于"
    (&= $k (&..cm $1 $n)) ", 我们有"
    (MB (&= $beta_k (sum (&= $j $1) $k
                         (&i* (mref $P $j $k) $alpha_j))) ".")
    "设" (&<= $1 $i) "且" (&< $i $k) ", 那么" (∈ $beta_i $W_i)
    "而" (&sube $W_i (_ $W (&- $k $1))) ". 既然"
    (∈ $beta_k (_^ $W (&- $k $1) $prime)) ", 可以推出"
    (MB (&= (appl $f $beta_i $beta_k) $0) ".")
    "令" $B "是" $f "在有序基" (setEnum $beta_1 $beta_n)
    "下的矩阵表示, 那么"
    (MB (&= (mref $B $k $i) (appl $f $beta_i $beta_k)) ".")
    "于是, " (&> $k $i) "时" (&= (mref $B $k $i) $0)
    ", 因而" $B "是一个上三角矩阵. 另一方面, 根据关于形式的基变换的讨论, 我们有"
    (MB (&= $B (&i* (&* $P) $A $P)) ".")
    "反过来, 设" $P "是一个满足我们要求的矩阵, 即" $P "是一个主对角线元素均为"
    $1 "的上三角矩阵使得" (&i* (&* $P) $A $P) "也是上三角的, 置"
    (MB (&cm (&= $beta_k
                 (sum (&= $j $1) $n
                      (&i* (mref $P $j $k) $alpha_j))
                 (sum (&= $j $1) $k
                      (&i* (mref $P $j $k) $alpha_j)))
             (&<= $1 $k $n)))
    "那么" (setEnum $beta_1 $beta_k) "显然是" $W_k "的一个基. 对于"
    (&> $k $1) ", " (setEnum $beta_1 (_ $beta (&- $k $1))) "是"
    (_ $W (&- $k $1)) "的一个基, 而且当" (&< $i $k) "时有"
    (&= (appl $f $beta_i $beta_k) $0) " [译注: 这是因为"
    (&= (appl $f $beta_i $beta_k) (mref (@i* (&* $P) $A $P) $k $i))
    "且" (&i* (&* $P) $A $P) "是一个上三角矩阵], 由此我们可以看出"
    (∈ $beta_k (_^ $W (&- $k $1) $prime))
    ". 定义" $beta_k "的公式告诉我们"
    (MB (&= $alpha_k
            (&+ (&- (@sum (&= $j $1) (&- $k $1)
                          (&i* (mref $P $j $k) $alpha_j)))
                $beta_k)) ".")
    "既然"
    (MB (∈ (&- (@sum (&= $j $1) (&- $k $1)
                     (&i* (mref $P $j $k) $alpha_j)))
           (_ $W (&- $k $1))))
    "而"
    (MB (∈ $beta_k (_^ $W (&- $k $1) $prime)))
    "又鉴于"
    (MB (&= $V (&d+ (_ $W (&- $k $1)) (_^ $W (&- $k $1) $prime))))
    "故"
    (MB (&= (ap (_ $E (&- $k $1)) $alpha_k)
            (&- (@sum (&= $j $1) (&- $k $1)
                      (&i* (mref $P $j $k) $alpha_j)))))
    "这实际上就完全确定了"
    (&..cm (mref $P $1 $k) (mref $P (&- $k $1) $k))
    "的可能性, 进而完全确定了矩阵" $P ". 当然, 我们可以很容易看出这个"
    $P "正是我们之前说明存在性时所构造出来的矩阵.")
   (H3 "第9.5节 谱论")
   (P "本节我们探求牵涉自伴算子和正规算子的对角化的第8章的定理18和22的推论.")
   ((theorem #:n "9. 谱定理")
    "令" $T "是有限维复内积空间" $V "上的一个正规算子, 或者是有限维实内积空间"
    $V "上的一个自伴算子, 设" (&..cm $c_1 $c_k) "是" $T "的不同的特征值, 令"
    $W_j "是特征值" $c_j "所对应的特征空间, " $E_j "是" $V "在" $W_j
    "上的正交投影, 那么不同的" $W_i "和" $W_j "相互正交, " $V "是"
    (&..cm $W_1 $W_k) "的直和, 并且"
    (MB (&= $T (LC0 $c_1 $E_1 $c_k $E_k)) "."))
   ((proof)
    "令" (∈ $alpha $W_j) ", " (∈ $beta $W_i) ", 并设" (&!= $i $j) ", 那么"
    (MB (&= (&i* $c_j (inner* $alpha $beta))
            (inner* (ap $T $alpha) $beta)
            (inner* $alpha (ap (&* $T) $beta))
            (inner* $alpha (&i* (_ (OverBar $c) $i) $beta))
            (&i* $c_i (inner* $alpha $beta))))
    "鉴于" (&!= $c_i $c_j) ", 可以推出" (&= (inner* $alpha $beta) $0)
    ", 即不同的" $W_i "和" $W_j "是相互正交的." (Br)
    "根据" $V "拥有全由" $T "的特征向量构成的规范正交基这一事实 "
    "(见第8章的定理18和22), 立即可以得到"
    (MB (&= $V (&..d+ $W_1 $W_k)) ".")
    "因此, " (&= (&..+ $E_1 $E_k) $I) "并且"
    (eqnderiv $T (&i* $T $I)
              (&i* $T (@ (&..+ $E_1 $E_k)))
              (&..+ (&i* $T $E_1) (&i* $T $E_k))
              (LC0 $c_1 $E_1 $c_k $E_k)))
   ((tcomment)
    "以上证明中使用了第8章的定理19. 另外, 这个证明对于"
    (&= (&..+ $E_1 $E_k) $I)
    "没有任何解释, 其实并非那么平凡. 实际上, 如果"
    (MB (&cm (&= $alpha (&..+ $alpha_1 $alpha_k))
             (∈ $alpha_i $W_i)))
    "当然这种分解是唯一的, 那么我们可以证明"
    (MB (&= (ap $E_i $alpha) $alpha_i))
    "这是因为, 当" (&!= $i $j) "时, "
    (&= (ap $E_i $alpha_j) $0)
    ", 鉴于" $alpha_j "正交于" $W_i "而" $E_i
    "是" $V "在" $W_i "上的正交投影.")
   (P "这个定理中出现的分解, 我们将其称为" $T "的"
      (B "谱分解 (spectral resolution)")
      ". 某些物理应用导致了有限维向量空间上的线性算子的"
      (B "谱 (spectrum)") "被定义为线性算子的特征值的集合, "
      "而这是我们使用谱分解这一术语的部分缘由. 另外, 注意到正交投影"
      (&..cm $E_1 $E_k) "由" $T "唯一确定也是重要的; "
      "实际上, 它们是应用多项式于" $T "得到的结果. "
      "[译注: 也请读者参考第6章的定理11.]")
   ((corollary)
    "如果"
    (MB (&= $e_j (prod (&!= $i $j)
                       (pare (~ (&- $x $c_i)
                                (&- $c_j $c_i))))))
    "那么"
    (MB (&cm (&= $E_j (app $e_j $T))
             (&<= $1 $j $k)) "."))
   ((proof)
    "译者就不翻译这里的证明了, 因为它实际上只是第6章的定理11的"
    "证明之后的讨论的重复.")
   (P "因为" (&..cm $E_1 $E_k) "由" $T "唯一确定并且"
      (MB (&= $I (&..+ $E_1 $E_k)))
      "投影族" (setEnum $E_1 $E_k) "被称为"
      (B "由" $T "定义的单位分解 (resolution of the identity defined by " $T ")")
      ".")
   (P "关于谱定理的证明我们有需要作出的评注. 我们运用关于自伴算子和正规算子对角化的"
      "第8章的定理18和22推导出了这个定理. 实际上还有一个更加代数的证明方法, "
      "其需要先证明正规算子的极小多项式是不同的素因子之积. 然后, 我们以类似于"
      "证明准素分解定理 (第6章的定理12) 的方式进行处理. 下一节我们将会给出这种证明.")
   (P "在各种应用中, 有时知道我们能否计算关于算子或者矩阵的特定函数 (例如平方根) "
      "是必要的, 而这对于可对角化的正规算子而言是简单的.")
   ((definition)
    "令" $T "是有限维内积空间上的一个可对角化正规算子, 并且"
    (MB (&= $T (sum (&= $j $1) $k
                    (&i* $c_j $E_j))))
    "是其谱分解. 如果函数" $f "的定义域包括" $T
    "的谱而取值于标量域, 那么我们定义线性算子" (app $f $T) "为"
    (MB (&= (app $f $T)
            (sum (&= $j $1) $k
                 (&i* (app $f $c_j) $E_j))) "."))
   ((theorem #:n "10")
    "令" $T "是有限维内积空间" $V "上的一个谱为" $S
    "的可对角化正规算子, 设" $f "是一个定义域包含" $S
    "而值取于标量域的函数, 那么" (app $f $T) "是一个谱为"
    (app $f $S) "的可对角化正规算子. 如果" $V^
    "也是一个有限维内积空间而" $U "是一个从" $V "到" $V^
    "的酉映射, 并且" (&= $T^ (&i* $U $T (inv $U))) ", 那么"
    $S "也是" $T^ "的谱而"
    (MB (&= (app $f $T^) (&i* $U (app $f $T) (inv $U))) "."))
   ((proof)
    (app $f $T) "的正规性可以根据定义和"
    (MB (&= (&* (app $f $T))
            (sum (&= $j $1) $k
                 (&i* (OverBar (app $f $c_j))
                      $E_j))))
    "这一事实通过简单的计算推得. 而且, 显然对于每个"
    (∈ $alpha (app $E_j $V)) ", 我们有"
    (MB (&= (ap (app $f $T) $alpha)
            (&i* (app $f $c_j) $alpha)) ".")
    "因此, 集合" (app $f $S) "是" (app $f $T)
    "的谱的子集. 反过来, 设" (&!= $alpha $0) "并且"
    (MB (&= (ap (app $f $T) $alpha) (&i* $b $alpha)))
    "那么根据"
    (MB (&= $alpha (sum (&= $j $1) $k (ap $E_j $alpha))))
    "由此可以推出"
    (eqnderiv
     (ap (app $f $T) $alpha)
     (sum (&= $j $1) $k
          (ap (app $f $T) (ap $E_j $alpha)))
     (sum (&= $j $1) $k
          (&i* (app $f $c_j) (ap $E_j $alpha)))
     (sum (&= $j $1) $k
          (&i* $b (ap $E_j $alpha))))
    "因而"
    (eqnderiv
     (&norm (sum (&= $j $1) $k
                 (&i* (@- (app $f $c_j) $b)
                      (ap $E_j $alpha))))
     (sum (&= $j $1) $k
          (&i* (&sqr (&abs (&- (app $f $c_j) $b)))
               (sqrnorm (ap $E_j $alpha))))
     $0)
    "所以, 我们可以断言" (&= (app $f $c_j) $b) "或者"
    (&= (ap $E_j $alpha) $0) ". 根据假设, " (&!= $alpha $0)
    ", 故存在一个下标" $i "使得" (&!= (ap $E_i $alpha) $0)
    ". 然后我们就可以推出" (&= (app $f $c_i) $b)
    ", 也就是说" (app $f $S) "的确是" (app $f $T)
    "的谱. 实际上, 设"
    (MB (&= (app $f $S) (setEnum $b_1 $b_r)))
    "其中当" (&!= $m $n) "时" (&!= $b_m $b_n) ", 也就是互异, 令"
    (&= $X_m (setI (∈ $i $NN)
                   (: (&<= $1 $i $k) "且" (&= (app $f $c_i) $b_m))))
    ", 置"
    (MB (&= $P_m (sum (∈ $i $X_m) $E_i)))
    "那么" $P_m "是从" $V "到" (app $f $T) "与特征值" $b_m
    "相关联的特征空间的正交投影, 而且"
    (MB (&= (app $f $T) (sum (&= $m $1) $r (&i* $b_m $P_m))))
    "是" (app $f $T) "的谱分解." (Br)
    "现在设" $U "是从" $V "到" $V^ "的酉变换, 并且"
    (&= $T^ (&i* $U $T (inv $U))) ", 那么等式"
    (MB (&= (ap $T $alpha) (&i* $c $alpha)))
    "成立当且仅当"
    (MB (&= (ap $T^ (ap $U $alpha))
            (&i* $c (ap $U $alpha))))
    "因此" $S "是" $T^ "的谱, 并且" $U "将" $T
    "的每个特征空间映射成相对应的" $T^ "的特征空间. 实际上, 根据定义, 我们可以看出"
    (MB (&cm (&= $T^ (sum (&= $j $1) $k
                          (&i* $c_j (_^ $E $j $prime))))
             (&= (_^ $E $j $prime)
                 (&i* $U $E_j (inv $U)))))
    "是" $T^ "的谱分解. 因此, 我们又可以推出"
    (eqnderiv
     (app $f $T^)
     (sum (&= $j $1) $k
          (&i* (app $f $c_j) (_^ $E $j $prime)))
     (sum (&= $j $1) $k
          (&i* (app $f $c_j)
               (&i* $U $E_j (inv $U))))
     (&i* $U
          (@sum (&= $j $1) $k
                (&i* (app $f $c_j) $E_j))
          (inv $U))
     (&i* $U (app $f $T) (inv $U))))
   ((tcomment)
    "以上存在一些需要澄清的地方. 首先, 酉映射其实指的就是内积空间的同构. 其次, 事实"
    (MB (&= (&* (app $f $T))
            (sum (&= $j $1) $k
                 (&i* (OverBar (app $f $c_j))
                      $E_j))))
    "的推出需要伴随的基本性质, " $E_j "是正交投影, 还有正交投影是自伴算子. 之所以"
    $E_j "是正交投影, 实际上是因为我们发现可对角化正规算子的条件就足够"
    "推出定理9的那些结论了, 当然或许读者还需要结合第6章的定理11的讨论看看. "
    "至于证明正交投影是自伴算子, 第8章的例子17实际上已经提供了一个证明. "
    "接着, 为了推出" (&= (app $f $c_j) $b) "或者" (&= (ap $E_j $alpha) $0)
    ", 其实不一定要用勾股定理, 也可以根据直和的性质得到. 最后, 这个证明没有提及"
    (&= $T^ (&i* $U $T (inv $U))) "的正规性, 但是我们可以发现"
    (&= (&* (@ $T^)) (&i* $U (&* $T) (inv $U))) ", 鉴于"
    (eqnderiv
     (inner* (ap $T^ $alpha^) $beta^)
     (inner* (ap (&i* $U $T (inv $U)) $alpha^) $beta^)
     (inner* (ap (&i* $T (inv $U)) $alpha^) (ap (inv $U) $beta^))
     (inner* (ap (inv $U) $alpha^) (ap (&i* (&* $T) (inv $U)) $beta^))
     (inner* $alpha^ (ap (&i* $U (&* $T) (inv $U)) $beta^)))
    "其中" (∈ $alpha^ $beta^ $V^) ", 并且我们用到了酉变换的保持内积的特性.")
   (P "在思考前述的讨论时, 我们一定要记得正规算子" $T "的谱是集合"
      (MB (&= $S (setEnum $c_1 $c_k)))
      "而且这些" $c_j "是互异的. 当" $T "在某个由特征向量构成的基下由"
      "一个对角矩阵表示时, 每个" $c_j "都需要重复相对应的特征空间的维数次. "
      "这是我们在以下结果中改换记号的原因.")
   ((corollary)
    "在定理10的假设下, 设" $T "在某个有序基" basis:def "下由对角矩阵"
    $D "表示, 并且" $D "的对角线为" (&..cm $d_1 $d_n) ", 那么在有序基"
    $BBB "下, " (app $f $T) "由对角矩阵" (app $f $D) "表示, 其对角线为"
    (&..cm (app $f $d_1) (app $f $d_n)) ". 如果"
    (&= $BBB^ (setEnum (_^ $alpha $1 $prime) (_^ $alpha $n $prime)))
    "是任意的有序基并且" $P "是从" $BBB "到" $BBB^ "的基变换矩阵, 即"
    (MB (&= (_^ $alpha $j $prime)
            (sum (&= $i $1) $n
                 (&i* (mref $P $i $j) $alpha_i))))
    "那么" (sim (app $f $D)) "是" (app $f $T) "在基" $BBB^ "下的矩阵.")
   ((proof)
    "对于每个下标" $i ", 存在唯一的" $j " (" (&<= $1 $j $k)
    ") 使得" (∈ $alpha_i (app $E_j $V)) "且" (&= $d_i $c_i)
    ". 因此, 对于每个" $i ", "
    (&= (ap (app $f $T) $alpha_i)
        (&i* (app $f $d_i) $alpha_i))
    ", 并且"
    (eqnderiv
     (ap (app $f $T) (_^ $alpha $j $prime))
     (sum (&= $i $1) $n
          (&i* (mref $P $i $j)
               (ap (app $f $T) $alpha_i)))
     (sum (&= $i $1) $n
          (&i* $d_i (mref $P $i $j) $alpha_i))
     (sum (&= $i $1) $n
          (&i* (mref (@i* $D $P) $i $j)
               $alpha_i))
     (sum (&= $i $1) $n
          (&i* (mref (@i* $D $P) $i $j)
               (@sum (&= $k $1) $n
                     (&i* (_^ $P (&cm $k $i) $-1)
                          (_^ $alpha $k $prime)))))
     (sum (&= $k $1) $n
          (sum (&= $i $1) $n
               (&i* (_^ $P (&cm $k $i) $-1)
                    (mref (@i* $D $P) $i $j)
                    (_^ $alpha $k $prime))))
     (sum (&= $k $1) $n
          (&i* (mref (@ (sim $D)) $k $j)
               (_^ $alpha $k $prime)))))
   (P "由这个结果我们可以构造正规矩阵的特定函数, 以下是论证. 设"
      $A "是一个正规矩阵, 那么存在一个可逆的矩阵"
      $P " (实际上是一个酉矩阵" $P ") 使得"
      (&i* $P $A (inv $P)) "是一个对角矩阵, 设其为" $D
      "而对角线元素分别为" (&..cm $d_1 $d_n)
      ". 令" $f "是一个可以应用到"
      (&..cm $d_1 $d_n) "上的复值函数, 令"
      (app $f $D) "是以" (&..cm (app $f $d_1) (app $f $d_n))
      "为对角线元素的对角矩阵, 那么" (sim (app $f $D))
      "独立于" $D ", 在以下意义上只是" $A
      "的一个函数. 如果" $Q "是另一个可逆矩阵并且"
      (&= $D^ (&i* $Q $A (inv $Q))) "是一个对角矩阵, 那么"
      $f "可以被应用到" $D^ "的对角线元素上且"
      (MB (&= (sim (app $f $D))
              (sim (app $f $D^) $Q)) "."))
   ((tcomment)
    "以上说的矩阵均是复矩阵. 另外, 我们最好解释一下以上这段话的进路. "
    "设" basis:def "是一个有序规范正交基, 并且" $T "是在" $BBB
    "下由" $A "确定的线性算子. 因为" $A "是正规的且"
    $BBB "是规范正交的, 所以" $T "也是正规的. 考虑由" $BBB
    "和基变换矩阵" (inv $P) "确定的有序基"
    (&= $BBB^ (setEnum (_^ $alpha $1 $prime)
                       (_^ $alpha $n $prime)))
    ", 即由"
    (MB (&= (_^ $alpha $j $prime)
            (sum (&= $i $1) $n
                 (&i* (_^ $P (&cm $i $j) $-1)
                      $alpha_i))))
    "确定的有序基, 那么" $T "在" $BBB^
    "下的矩阵即对角矩阵" (&= $D (&i* $P $A (inv $P)))
    ". 我们知道" (app $f $T) "在" $BBB^ "下"
    "由" (app $f $D) "表示. 而且, 由于从" $BBB^
    "到" $BBB "的基变换矩阵是" (&= $P (inv (@inv $P)))
    ", 所以" (app $f $T) "在" $BBB "下的矩阵是"
    (sim (app $f $D)) ". 同理可得, "
    (app $f $T) "在" $BBB "下的矩阵也是"
    (sim (app $f $D^) $Q) ". 因此, 这两个矩阵是相等的.")
   ((definition)
    "在以上条件下, " (app $f $A) "被定义为"
    (sim (app $f $D)) ".")
   ((tcomment)
    (app $f $A) "可以理解成是选定一个任意的规范正交基" $BBB
    "得到一个正规算子" $T ", 然后" (app $f $A)
    "就是" (app $f $T) "在" $BBB "下的矩阵. "
    "根据之前的讨论, 我们知道这个矩阵独立于规范正交基的选择.")
   (P "矩阵" (app $f $A) "也可以用一种不同的方式刻画. "
      )
   (H3 "第9.6节 正规算子的更深刻性质")
   ))