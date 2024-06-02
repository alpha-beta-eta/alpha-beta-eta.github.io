#lang racket
(provide combinatorics_notes.html)
(require SMathML)
(define $phiv (Mi "&phiv;"))
(define (set-font x variant)
  (set-attr* x 'mathvariant variant))
(define @/ (@lize &/))
(define @compose (@lize &compose))
(define (set-left d)
  (set-attr* d 'columnalign "left"))
(define (set-right d)
  (set-attr* d 'columnalign "right"))
(define $forall (Mo "&forall;"))
(define (∀ c P)
  (&cm (: $forall c) P))
(define $darr (Mo "&darr;"))
(define $larr (Mo "&larr;"))
(define (power-series a0 a1 . a*)
  (let iter ((i 2) (rest a*) (result (list (&i* a1 $x) a0)))
    (if (null? rest)
        (apply &+ (reverse (cons $..c result)))
        (iter (+ i 1) (cdr rest)
              (cons (&i* (car rest)
                         (^ $x (Mn (number->string i))))
                    result)))))
(define-infix*
  (&->0 $->0)
  )
(define (fib n)
  (let iter ((a 1) (b 1) (c 0))
    (if (= n c)
        a
        (iter b (+ a b) (+ c 1)))))
(define (fib* n)
  (let iter ((a 1) (b 1) (c 0) (r '()))
    (if (= n c)
        (reverse (cons a r))
        (iter b (+ a b) (+ c 1) (cons a r)))))
(define (make-frame . x*)
  (Mtable #:attr* '((frame "solid") (displaystyle "true"))
          (Mtr (apply Mtd x*))))
(define combinatorics_notes.html
  (TmPrelude
   #:title "组合学笔记"
   #:css "styles.css"
   (H1 "组合学笔记")
   (P "偶然间发现了所谓的de Bruijn的组合学的课堂笔记, 我有点好奇, 遂读之.")
   (H2 "第1章 引论")
   (H2 "第2章 生成函数")
   (P "组合学中一个非常古老的想法是生成函数, 其可以追溯至Laplace.")
   (P "给定一无穷数列"
      (MB (&cm $a_0 $a_1 $a_2 $..h))
      "其生成函数为以下级数"
      (MB (&= (app $A $x) (power-series $a_0 $a_1 $a_2))))
   (P "生成函数这种技术的想法在于"
      (MB (&Table
           ((make-frame "关于序列的知识")
            (^^ $-> "向前转换")
            (make-frame "关于" (app $A $x) "的知识"))
           ($ $ (: $darr "各种操作"))
           ((make-frame "更多关于序列的知识")
            (__ $larr "向后转换")
            (make-frame "更多关于" (app $A $x) "的知识"))))
      "这有点像是Laplace积分"
      (MB (&= (app $F $x)
              (integral $0 $inf
                        (&i* (^ $e (&i* $x $t))
                             (app $f $t))
                        $t)))
      "其想法在于"
      (MB (&Table
           ((make-frame "关于" $f "的微分方程")
            (^^ $-> "Laplace变换")
            (make-frame "关于" $F "的代数方程"))
           ($ $ (: $darr "求解代数方程"))
           ((make-frame "关于" $f "的微分方程的解")
            (__ $larr "向后转换")
            (make-frame "关于" $F "的代数方程的解")))))
   ((notation)
    "为了避免可能的歧义, 我们定义"
    (MB (&= $NN_0 (setE $0 $1 $2 $..h)))
    "以及"
    (MB (&= $NN_1 (setE $1 $2 $3 $..h))))
   ((example)
    "Fibonacci数列定义如下"
    (MB (&\; (&= $a_0 $1) (&= $a_1 $1)
             (&cm (&= (_ $a (&+ $n $1)) (&+ $a_n (_ $a (&- $n $1))))
                  (∈ $n $NN_1))))
    "Fibonacci数列的生成函数为"
    (MB (&= (app $A $x) (apply power-series (fib* 6))))
    "基于其系数的递推关系, 我们发现"
    (MB (&= (&i* $x (@+ $x $1) (app $A $x)) (&- (app $A $x) $1)))
    "于是"
    (MB (&= (app $A $x) (~ $-1 (&+ $-1 $x $x^2))))
    "将该分式拆分可以得到"
    (MB (&= (app $A $x)
            (&i* (pare (&- (~ $1 (&- $x $alpha_1))
                           (~ $1 (&- $x $alpha_2))))
                 (~ $-1 (&- $alpha_1 $alpha_2)))))
    "其中" $alpha_1 "和" $alpha_2 "是" (&+ $-1 $x $x^2) "的两根." (Br)
    "若是我们将这些分式再写成幂级数的形式, 然后合并同类项, 就可以得到"
    (let* ((√5 (Msqrt $5))
           (1+√5/2 (~ (&+ $1 √5) $2))
           (1-√5/2 (~ (&- $1 √5) $2)))
      (MB (&= $a_n
              (&i* (~ $1 √5)
                   (brac (&- (^ (pare 1+√5/2) (&+ $n $1))
                             (^ (pare 1-√5/2) (&+ $n $1)))))))))
   (P "我们可用三种方式为这把戏 (hocus-pocus) 进行辩护"
      (Ol (Li "作为启发式 (heuristic). 反正我们得到了结果, 不论"
              "是怎么得到的. 之后我们可以验证其正确与否, 比如说"
              "这种情况下我们可以使用数学归纳法.")
          (Li "通过收敛幂级数的理论. 我们可以将" $A
              "视为一个定义在" $CC "平面中的零点的某个小邻域上的"
              "解析函数. 这可以澄清我们对于" $A
              "施行的计算. 问题在于我们需要检视其收敛性, 即其具有"
              "某个正的收敛半径, 虽然就这个例子而言并不是什么困难. "
              "我们可以用归纳法轻易地验证"
              (MB (∀ (∈ $n $NN_0) (&<= (&abs $a_n) $2^n)))
              "根据Cauchy–Hadamard定理, 我们知道收敛半径大于等于"
              (&/ $1 $2) ".")
          (Li "通过建立形式幂级数的理论. 这可以做得相当形式化, "
              "就是非常无聊而已. 当然, 也不会有什么新东西.")))
   (H4 "形式幂级数的理论概览.")
   (P "我们将给出复系数幂级数的理论, 虽然仅需很少的调整其也"
      "可以对于环成立. 你应该将形式幂级数看成是具有无限次数的"
      "多项式. 或者说, 实际上你应该将确定了形式幂级数的序列"
      "当成是形式幂级数自身. 也就是说, 我们的形式幂级数"
      "不过就是一个映射"
      (MB (&cm (&-> $NN_0 $CC) (&\|-> $n $a_n)))
      "加法被定义为逐项之和. 令"
      (MB (set-left
           (&Table
            ((&= (app $A $x) (power-series $a_0 $a_1 $a_2)))
            ((&= (app $B $x) (power-series $b_0 $b_1 $b_2))))))
      "那么"
      (MB (&= (app (@+ $A $B) $x)
              (power-series (@+ $a_0 $b_0)
                            (@+ $a_1 $b_1)
                            (@+ $a_2 $b_2))))
      "乘法被定义为所谓的Cauchy乘积"
      (MB (&= (app (@i* $A $B) $x)
              (power-series (@i* $a_0 $b_0)
                            (@+ (&i* $a_0 $b_1) (&i* $a_1 $b_0))
                            (@+ (&i* $a_0 $b_2) (&i* $a_1 $b_1)
                                (&i* $a_2 $b_0)))))
      $x^n "的系数为" (sum (&= $j $0) $n (&i* $a_j (_ $b (&- $n $j))))
      ". 如果" (&!= $b_0 $0) ", 那么" (app (@/ $A $B) $x) "也是有定义的. 如果"
      (&= $b_0 $0) ", 那么" (app (@compose $A $B) $x)
      "也是有定义的, 甚至我们可以确定一个" (app $C $x) "使得"
      (&= (app (@compose $C $B) $x) $x) ". 我们可以形式化地对于幂级数进行微分. "
      "在特定条件下, 我们可以定义幂级数的无穷级数和无穷乘积.")
   ((remark)
    "我们的一个重要想法在于, 如果一个操作对于每个系数的计算只涉及有限多的"
    "非平凡步骤, 那么其也应该是可行的, 哪怕这个操作实际上是无穷的. 什么是"
    "平凡的呢, 比如说加上" $0 ", 乘上" $0 ", 乘上" $1 "这种." (Br)
    "让我们考虑一个例子"
    (MB (&= (app (@compose $A $B) $x)
            (&+ $a_0 (&i* $a_1 (app $B $x))
                (&i* $a_2 (^ (app $B $x) $2))
                (&i* $a_3 (^ (app $B $x) $3)) $..c)))
    "当" (&!= $b_0 $0) "时, 每一项" (&i* $a_n (^ (app $B $x) $n))
    "都包含一个非零的常数项, 将其合并就得到"
    (MB (&+ $a_0 (&i* $a_1 $b_0) (&i* $a_2 (_^ $b $0 $2)) $..c))
    "这是我们所不允许的无限形式的操作." (Br)
    "再看另外一个例子. 对于" (∈ $k $NN_0) ", 我们将" (app $A_k $x)
    "定义为从" (&i* $a_k $x^k) "开始的幂级数. 那么, 我们可以定义"
    (sum (&>= $k $0) $ (app $A_k $x)) ": 对于第" $k "项, 我们仅需加起"
    (&+ $k $1) "个项.")
   (H3 "第2.1节 零钱问题")
   (P "如果你需要付" 67 "分钱买一杯咖啡, 而只能以"
      (&cm $1 $5 10 25) "分钱的硬币付款, 假定每种硬币的数量都是足够多的, "
      "那么有多少种付款的方式呢? 我们可以先尝试枚举一些具体的方案."
      (MB (&Table
           ((@ 1) (@ 5) (@ 10) (@ 25))
           (2 2 3 1)
           (7 2 0 2)
           (7 0 1 2)))
      "换言之, 一种方案完全由其频率函数"
      (MB (func $f (setE $1 $5 10 25) $NN_0))
      "刻画. 我们的问题是存在多少个总值为" 67 "的这样的频率函数. 频率函数"
      $f "的总值即"
      (MB (&+ (&c* $1 (app $f $1))
              (&c* $5 (app $f $5))
              (&c* 10 (app $f 10))
              (&c* 25 (app $f 25)))))
   (H4 "解法.")
   (P "刚才我们并不成系统. 实际上, 答案是"
      (MB (set-left
           (&Table
            ((@+ $1 $x $x^2 $x^3 $..c) $c*)
            ((@+ $1 $x^5 (^ $x 10) (^ $x 15) $..c) $c*)
            ((@+ $1 (^ $x 10) (^ $x 20) (^ $x 30) $..c) $c*)
            ((@+ $1 (^ $x 25) (^ $x 50) (^ $x 75) $..c)))))
      "中" (^ $x 67) "的系数, 即" 87 ".")
   (P "如果我们用" (_ $C:normal $E) "代表&quot;" $E
      "的系数&quot;, 那么问题的答案可以记为"
      (MB (ap (_ $C:normal (^ $x 67))
              (~ $1 (&i* (@- $1 $x) (@- $1 $x^5)
                         (@- $1 (^ $x 10))
                         (@- $1 (^ $x 25))))))
      "以这种方式, 我们可以找出其他系数, 或者研究" (&->0 $n $inf)
      "时" (_ $C:normal $x^n) "的渐近性质.")
   (P "这种方法的正确性是不言自明的. 在合并幂级数乘积的同类项之前, "
      "每一项的系数都是" $1 ", 而且其都对应于从每一行的级数里"
      "各选出一个项来, 这就从概念上指出了一种挑选方式. 比如说, 项"
      (&i* $x^2 (^ $x 10) (^ $x 30) (^ $x 25))
      "对应于使用" $2 "枚" $1 "分硬币, " $2 "枚" $5 "分硬币, "
      $3 "枚" 10 "分硬币, " $1 "枚" 25 "分硬币的方案. "
      "(译注: 当然, 更严格地说, 这里我们考虑的是项的内涵, 因为"
      "从外延看, 我们无法区分次数相同的项.) "
      "在合并同类项之后, " $x^n "的系数显然就等于付" $n
      "分钱的方式数目.")
   (H3 "第2.2节 一个求和公式")
   (P "我们猜读者已经熟悉用" (&abs $S) "表示集合" $S
      "的元素数目的记号了. 如果" $R "和" $D "是集合, 那么类型为"
      (&-> $D $R) "的所有映射构成的集合, 我们记作" $R^D
      ". 公式" (&= (&abs $R^D) (^ (&abs $R) (&abs $D)))
      "是很好的助记方法.")
   ((theorem)
    "令" $K "是一个含幺交换环, " $D "和" $R "是集合. 对于映射"
    (func $phiv (&c* $D $R) $K) ", 我们有"
    (MB (&= (sum (∈ $f $R^D) $
                 (prod (∈ $d $D) $
                       (appl $phiv $d (app $f $d))))
            (prod (∈ $d $D) $
                  (sum (∈ $r $R) $
                       (appl $phiv $d $r))))))
   ((proof)
    "显然, 或许唯一值得注意的是边角情况. 若" (&= $D $empty)
    ", 那么左右皆为" $1 ". 若" (&!= $D $empty) "且"
    (&= $R $empty) ", 那么左右皆为" $0 ".")
   (P "作为定理的应用, 令" $K "是" $ZZ "上的形式幂级数环, "
      (&= $D (setE $1 $5 10 25)) ", " (&= $R $NN_0) ", "
      (&= (appl $phiv $d $r) (^ $x (&i* $d $r))) ", 那么"
      (MB (&= (prod (∈ $d $D) $
                    (appl $phiv $d (app $f $d)))
              (^ $x (@ $f "的总值"))))
      )
   ))