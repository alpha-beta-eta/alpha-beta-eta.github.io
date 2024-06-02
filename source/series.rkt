#lang racket
(provide series.html)
(require SMathML)
(define $sum (Mo "&sum;"))
(define (&sum x_n)
  (: $sum x_n))
(define (set-left x)
  (set-attr* x 'columnalign "left"))
(define LC0
  (case-lambda
    ((k u) (&+ $..c (&i* k u)))
    ((k u . arg*) (&+ (&i* k u) (apply LC0 arg*)))))
(define series.html
  (TnTmPrelude
   #:title "无穷级数的理论和应用"
   #:css "styles.css"
   (H1 "无穷级数的理论和应用")
   (H2 "第1章 实数理论的原理")
   (H2 "第2章 实数序列")
   ((definition #:n "24")
    (Ol (Li "序列" (@ $x_n) "被称为有界的, 如果存在常数" $K "满足不等式"
            (MB (&<= (&abs $x_n) $K))
            "对于每个" $n "均成立.")
        (Li "序列" (@ $x_n) "被称为单调递增的, 如果" (&<= $x_n (_ $x (&+ $n $1)))
            "对于每个" $n "均成立; 单调递减的, 如果" (&>= $x_n (_ $x (&+ $n $1)))
            "对于每个" $n "均成立.")))
   ((definition #:n "25")
    "序列" (@ $x_n) "被称为趋零序列 (null sequence), 如果对于任意正数" $epsilon
    "的选择, 总能找到" (&= $n_0 (app $n_0 $epsilon)) ", 满足不等式"
    (MB (&< (&abs $x_n) $epsilon))
    "对于每个" (&> $n $n_0) "成立.")
   ((theorem #:n "26")
    "如果" (@ $x_n) "是一个趋零序列, 并且另一个序列" (@ (_^ $x $n $prime))
    "的项, 对于大于特定的" $m "的每个" $n "满足条件"
    (&<= (&abs (_^ $x $n $prime)) (&abs $x_n)) ", 或者更一般的条件"
    (MB (&<= (&abs (_^ $x $n $prime)) (&d* $K (&abs $x_n))) ",")
    "其中" $K "是一个任意(但固定)的正数, 那么" (@ (_^ $x $n $prime))
    "也是一个趋零序列. (比较测试.)")
   ((theorem)
    "如果" (@ $x_n) "是一个趋零序列, 而" (@ $a_n) "是任意的有界序列, 那么由"
    (MB (&= (_^ $x $n $prime) (&i* $a_n $x_n)))
    "定义的序列是趋零的.")
   ((theorem #:n "27")
    "如果" (@ $x_n) "是一个趋零序列, 那么" (@ $x_n) "的任意子序列"
    (@ (_^ $x $n $prime)) "也是趋零序列.")
   (H3 "第8节 收敛序列")
   (H4 "极限的Cauchy定理及其推广")
   ((theorem #:n "4")
    "令" (tu0 $x_0 $x_1 $..h) "是一个趋零序列, 设系统"
    (MB (set-left
         (&Table
          ((mref $a $0 $0))
          ((mref $a $1 $0) (mref $a $1 $1))
          ((mref $a $2 $0) (mref $a $2 $1) (mref $a $2 $2))
          ($..c $..c $..c $..c)
          ((mref $a $n $0) (mref $a $n $1) (mref $a $n $2) $..c (mref $a $n $n))
          ($..c $..c $..c $..c $..c $..c))))
    "的系数" (mref $a $mu $nu) "满足以下两个条件:"
    (Ol #:attr* '((type "a"))
        (Li "每一列都是一个趋零序列, 即对于固定的" (&>= $p $0) ", 我们有"
            (MB (&-> (mref $a $n $p) $0) "当" (&-> $n (&+ $inf)) "."))
        (Li "存在常数" $K "满足每一行的项的绝对值之和均小于它, 即对于每个" $n "有"
            (MB (&< (&+ (&abs (mref $a $n $0)) (&abs (mref $a $n $1)) $..c
                        (&abs (mref $a $n $n))) $K) ".")))
    "那么由"
    (MB (&= (_^ $x $n $prime)
            (LC0 (mref $a $n $0) $x_0 (mref $a $n $1) $x_1
                 (mref $a $n $n) $x_n)))
    "定义的序列" (@ (_^ $x $n $prime)) "也是一个趋零序列.")
   ((proof)
    "给定" (&> $epsilon $0) ", 存在" $m "满足对于每个" (&> $n $m)
    "有" (&< (&abs $x_n) (~ $epsilon (&i* $2 $K))) ", 那么"
    (MB (deriv0 (&abs (_^ $x $n $prime))
                $= (&abs (LC0 (mref $a $n $0) $x_0 (mref $a $n $n) $x_n))
                $<= (&+ (&abs (LC0 (mref $a $n $0) $x_0 (mref $a $n $m) $x_m))
                        (LC0 (&abs (mref $a $n (&+ $m $1))) (&abs (_ $x (&+ $m $1)))
                             (&abs (mref $a $n $n)) (&abs $x_n)))
                $<= (&+ (&abs (LC0 (mref $a $n $0) $x_0 (mref $a $n $m) $x_m))
                        (&i* (~ $epsilon (&i* $2 $K))
                             (@+ (&abs (mref $a $n (&+ $m $1))) $..c
                                 (&abs (mref $a $n $n)))))
                $< (&+ (&abs (LC0 (mref $a $n $0) $x_0 (mref $a $n $m) $x_m))
                       (~ $epsilon $2))))
    "根据条件a, 鉴于" $m "是固定的, 可以找到一个" (&> $n_0 $m) "使得对于每个"
    (&> $n $n_0) ", 我们有"
    (&< (&abs (LC0 (mref $a $n $0) $x_0 (mref $a $n $m) $x_m)) (~ $epsilon $2))
    ". 也就是说, 对于每个" (&> $n $n_0) ", " (&< (&abs (_^ $x $n $prime)) $epsilon)
    ", 定理也就得到了证明.")
   ((theorem #:n "5")
    "如果" (&-> $x_n $xi) "并且系数" (mref $a $mu $nu) "满足额外条件"
    (Ol #:attr* '((type "a") (start "3"))
        (Li (MB (&-> (&= (&+ (mref $a $n $0) (mref $a $n $1)
                             $..c (mref $a $n $n)) $A_n) $1) ".")))
    "那么亦有"
    (MB (&-> (&= (_^ $x $n $prime)
                 (LC0 (mref $a $n $0) $x_0 (mref $a $n $1) $x_1
                      (mref $a $n $n) $x_n))
             $xi) "."))
   ((proof)
    "现在我们有"
    (MB (&= (_^ $x $n $prime)
            (LC0 $A_n $xi (mref $a $n $0) (@- $x_0 $xi)
                 (mref $a $n $1) (@- $x_1 $xi)
                 (mref $a $n $n) (@- $x_n $xi))))
    "根据定理4, 可知由"
    (MB (&= (_^ $x $n $Prime)
            (LC0 (mref $a $n $0) (@- $x_0 $xi)
                 (mref $a $n $1) (@- $x_1 $xi)
                 (mref $a $n $n) (@- $x_n $xi))))
    "定义的序列" (@ (_^ $x $n $Prime))
    "是一个趋零序列, 并且我们还知道" (&-> (&i* $A_n $xi) $xi)
    ", 故" (&-> (_^ $x $n $prime) $xi) ".")
   
   (H2 "第3章 正项级数")
   (P "本章我们关心正项级数, 或者至少是非负项级数. 这里两者都被称为正项级数, "
      "也就是说正被理解为非严格的正. 对于这样一个级数" (&sum $a_n)
      "而言, 既然" (&>= $a_n $0) ", 那么我们有"
      (MB (&>= (&= $s_n (&+ (_ $s (&- $n $1)) $a_n)) (_ $s (&- $n $1))) ",")
      "于是部分和序列" (@ $s_n) "是一个单调递增序列, 其行为是特别简单的.")
   (P "第一个主要的判别法. 正项级数要么收敛要么发散至"
      (&+ $inf) ", 并且其收敛当且仅当部分和有界.")
   ((theorem)
    "如果" $p "是正整数, 那么两个级数"
    (MB (sum (&= $n $0) $inf $a_n) "和"
        (sum (&= $n $p) $inf $a_n))
    "同时收敛或发散, 并且在收敛的情况下,"
    (MB (&= (sum (&= $n $0) $inf $a_n)
            (&+ $a_0 $a_1 $..c (_ $a (&- $p $1))
                (sum (&= $n $p) $inf $a_n))) "."))
   ((theorem)
    "如果" (&sum $c_n) "是收敛的正项级数, 那么" (&sum (&i* $gamma_n $c_n))
    "亦收敛, 如果因子" $gamma_n "是(非严格)正但有界的数字.")
   
   (H2 "第4章 任意项级数")
   (H2 "第5章 幂级数")
   (H2 "第6章 所谓初等函数的展开")
   (H2 "第7章 无穷积")
   (H2 "第8章 ")
   ))