#lang racket
(provide analysis1_amann.html)
(require SMathML)
(define (H4. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #f] #:auto? [auto? #f] . html*)
  `(,(build-%heading #:present h4-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (let ((n (list-ref (reverse section) 2)))
         (format "~s.~s" n index))))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B (format "~a~a. " name num))
        (B (format "~a. " name)))))
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
    (Cite name `(a ((href ,href)) ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Example "例子" "example")
  (Theorem "定理" "theorem")
  (Definition "定义" "definition")
  (Lemma "引理" "lemma")
  (Corollary "推论" "corollary")
  (Proposition "命题" "proposition")
  (Remark "评注" "remark")
  (RemarkExample "评注和例子" "remark_example")
  
  )
(define $NN^* (^ $NN $c*))
(define $RR^+ (^ $RR $+))
(define $Num (Mi "Num"))
(define (&Num X)
  (app $Num X))
(define $dummy (Mi "&sdot;"))
(define $pointw (Mtext "pointw"))
(define (&pointw f_n f)
  (: f_n (__ $-> $pointw) f))
(define $unf (Mtext "unf"))
(define (&unf f_n f)
  (: f_n (__ $-> $unf) f))
(define |$[| (Mo "["))
(define |$]| (Mo "]"))
(define |$(| (Mo "("))
(define |$)| (Mo ")"))
(define (|[]| a b)
  (: |$[| a $cm b |$]|))
(define (|[)| a b)
  (: |$[| a $cm b |$)|))
(define (|(]| a b)
  (: |$(| a $cm b |$]|))
(define (inv0 x)
  (&/ $1 x))
(define (MBL label . exp*)
  (MB (Mtable
       #:attr*
       '((columnalign "left center right")
         (width "100%"))
       (Mtr (Mtd (Mphantom label))
            (apply Mtd exp*)
            (Mtd label)))))
(define (Dabs a b)
  (&abs (&- a b)))
(define (Dd a b)
  (appl $d a b))
(define analysis1_amann.html
  (TnTmPrelude
   #:title "分析一 (Amann &amp; Escher)"
   #:css "styles.css"
   (H1. "分析一 (Amann &amp; Escher)")
   (P "失去的时间不可复得, 只能加倍努力.")
   (H2. "基础")
   (H3. "逻辑基础")
   (H3. "集合")
   (H3. "函数")
   (H3. "关系和运算")
   (H3. "自然数")
   (H3. "可数性")
   (H3. "群和同态")
   (H3. "环, 域和多项式")
   (H3. "有理数")
   (H3. "实数")
   (H3. "复数")
   (H3. "向量空间, 仿射空间和代数")
   (H2. "收敛" #:id "convergence")
   (P "本章我们终于进入了分析学的领域. "
      "这一数学分支在很大程度上是建立在收敛的概念之上的, "
      "其允许我们在某种意义下将无限多个数字 (或者向量) 相加. "
      "这种考虑无限运算的能力是分析和代数的本质区别.")
   (P "对于数列收敛的朴素想法的公理化尝试自然地产生了距离, "
      "点的邻域, 度量空间的概念&mdash;&mdash;这是"
      (Ref "convergence-of-sequences")
      "的主题. 在序列的项为数字的特殊情况下, "
      "我们可以利用" $KK "的向量空间结构. "
      "对于这种情况下的证明的分析表明, "
      "大部分结果也可应用于向量的序列, "
      "只要某种绝对值的类似物是可用的. "
      "因此, 我们自然地被导向了赋范向量空间的定义, "
      "特别重要的一类度量空间.")
   (P "在赋范向量空间之中, 内积空间以其结构的丰富性和"
      "其几何非常类似于我们所熟悉的平面的Euclid几何这一事实脱颖而出. "
      "诚然如此, 对于初等分析学而言, 最重要的内积空间种类为"
      $m "维Euclid空间" $RR^m "和" $CC^m ".")
   (P "在" (Ref "monotone-sequences") "和"
      (Ref "infinite-limits") "之中, 我们回到了最为简单的情况, 即"
      $RR "中的收敛. 通过使用序结构, 特别是" $RR
      "的序完备性, 我们推导出了我们的第一条具体的收敛准则. "
      "这允许我们计算许多重要的序列极限. "
      "除此以外, 根据" $RR "的序完备性, "
      "我们还推导出了一条基础性的存在性原理, "
      "即Bolzano-Weierstrass定理.")
   (P (Ref "completeness")
      "致力于度量空间的完备性的概念. "
      "将其特化于赋范向量空间之上则得到了Banach空间的定义. "
      "这种空间的基本例子是" $KK^m
      ", 但是我们也表明有界函数的集合是Banach空间.")
   (P "Banach空间在分析学中无处不在, "
      "因而在我们的呈现中扮演了重要的角色. "
      "即便如此, Banach空间的结构仍然足够简单, "
      "以至于初学者从理解实数到理解Banach空间途中也不会经历多少困难. "
      "而且及早引入这些空间使得之后章节中的简短而优雅的证明成为可能.")
   
   (H3. "序列的收敛" #:id "convergence-of-sequences")
   (P "本节我们考虑定义于自然数上的函数, "
      "因而其只会取可数多个值. "
      "对于这样一个函数" (func $phi $NN $X)
      ", 我们对于" (Q $n "趋于无穷")
      "时值" (app $phi $n) "的行为特别感兴趣. "
      "鉴于我们只能对于" $phi
      "求值有限多次, 也就是说我们不可能"
      (Q "抵达无穷") ", 那么我们必须建立方法以使得我们能够证明关于"
      (Q "接近无穷") "的无限多个函数值的陈述. "
      "这样的方法构成了收敛序列的理论, "
      "也就是本节我们所要呈现的内容.")
   (H4. "序列")
   (P "令" $X "是一个集合. (" $X "中的)一个"
      (B "序列") "不过就是一个从"
      $NN "到" $X "的函数. 如果" (func $phi $NN $X)
      "是一个序列, 我们也将其记为"
      (MB (&cm (@ $x_n)
               (_ (@ $x_n) (∈ $n $NN))
               (tu0 $x_0 $x_1 $x_2 $..h)))
      "其中" (&:= $x_n (app $phi $n)) "是序列"
      (&= $phi (tu0 $x_0 $x_1 $x_2 $..h))
      "的第" $n "个" (B "项") ".")
   (P $KK "中的序列被称为" (B "数列") ", 而"
      (B "由所有数列构成的" $KK "-向量空间" $KK^NN)
      "记作" $s "或者" (app $s $KK)
      " (见例子??). 更精确的是, 若" (&= $KK $RR)
      "我们称" (@ $x_n) "是一个" (B "实序列")
      "; 若" (&= $KK $CC) "我们称" (@ $x_n)
      "是一个" (B "复序列") ".")
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li "将一个序列" (@ $x_n) "与其像"
            (setI $x_n (∈ $n $NN))
            "进行区分是很重要的. 例如, 若对于所有的"
            $n "都有" (&= $x_n (∈ $x $X))
            ", 也就是说" (@ $x_n)
            "是一个常序列, 那么"
            (∈ (&= (@ $x_n) (tu0 $x $x $x $..h)) $X^NN)
            "而" (setI $x_n (∈ $n $NN))
            "是一个单元素集合" (setE $x) ".")
        (Li "令" (@ $x_n) "是" $X "中的序列, 而"
            $E "是某个性质. 那么, 我们称" $E
            "对于" (@ $x_n) "的" (B "几乎所有")
            "项成立, 如果存在某个" (∈ $m $NN)
            "使得对于所有的" (&>= $n $m)
            "都有" (app $E $x_n) "为真. 换言之, "
            $E "对于" $x_n "中除了有限多个项之外的所有项成立. "
            "当然了, " (app $E $x_n)
            "也可对于数个 (或者全部的) " (&< $n $m)
            "为真. 若存在一个子集" (&sube $N $NN)
            "满足" (&= (&Num $N) $inf) "且对于每个"
            (∈ $n $N) ", " (app $E $x_n)
            "都为真, 那么我们称" $E "对于"
            (B "无限多个") "项成立. 例如, 实序列"
            (MB (tup -5 4 -3 2 -1 0 -1/2 1/3 -1/4
                     1/5 $..h (&- (~ $1 (&i* $2 $n)))
                     (~ $1 (&+ (&i* $2 $n) $1)) $..h))
            "具有无限多个正项, 无限多个负项, "
            "并且对于几乎所有项而言绝对值都小于" $1 ".")
        (Li "对于" (∈ $m $NN^*) ", 一个函数"
            (func $psi (&+ $m $NN) $X)
            "也被称为" $X "中的一个序列. 也就是说, "
            (&= (_ (@ $x_j) (&>= $j $m))
                (tu0 $x_m (_ $x (&+ $m $1))
                     (_ $x (&+ $m $2)) $..h))
            "是" $X "中的一个序列, 即便索引不从"
            $0 "开始. "
            )
        )
    )
   (H4. "度量空间")
   (P "令" $X "是一个集合. 一个函数" (func $d (&c* $X $X) $RR^+)
      "被称为" $X "上的一个" (B "度量") ", 如果以下条件成立:"
      (Ol (Li (&<=> (&= (Dd $x $y) $0)
                    (&= $x $y)) ";")
          (Li (&= (Dd $x $y) (Dd $y $x)) " (对称性);")
          (Li (&<= (Dd $x $y)
                   (&+ (Dd $x $z) (Dd $z $y)))
              " (三角不等式)."))
      "如果" $d "是" $X "上的一个度量, 那么" (tu0 $X $d)
      "被称为一个" (B "度量空间")
      ". 当度量在上下文中是显然的时候, "
      "我们将" (tu0 $X $d) "简记为" $X
      ". 最后, 我们将" (Dd $x $y)
      "称为度量空间" $X "中"
      (B "点") $x "和" $y
      "之间的" (B "距离") ".")
   
   (H3. "实序列和复序列")
   (H3. "赋范向量空间")
   (H3. "单调序列" #:id "monotone-sequences")
   (H3. "无穷极限" #:id "infinite-limits")
   (H3. "完备性" #:id "completeness")
   (H3. "级数")
   (H3. "绝对收敛")
   (H3. "幂级数")
   (H2. "连续函数" #:id "continuous_functions")
   (H3. "连续性")
   (H3. "拓扑基础")
   (H3. "紧致性")
   (H3. "连通性")
   (P "从直觉上来说显然" $RR "中的一个开区间是" (Q "连通的")
      ", 而如果我们去除了其中的一个点它又会变成" (Q "不连通的")
      ". 本节我们要将连通性的这种直觉性概念精确化. "
      "在达成目的的过程之中, 我们又一次发现拓扑学扮演了本质性的角色.")
   (H4. "定义和基本性质")
   (P "一个度量空间" $X "被称为是" (B "连通的")
      ", 如果" $X "不能被表示为两个无交非空开集之并. "
      "因此, " $X "是连通的当且仅当"
      
      )
   (H3. $RR "上的函数")
   (H3. "指数函数和相关函数")
   (H2. "单变元微分")
   (P "在" (Ref "convergence") "之中, 我们探索了极限的概念, "
      "这是分析学最为基础和本质的概念之一. "
      "我们建立了极限的计算方法, 并呈现了其许多重要的应用. 在"
      (Ref "continuous_functions")
      "之中, 我们仔细考虑了分析学的拓扑基础和连续性的概念. "
      
      )
   
   (H3. "可微性")
   (H3. "中值定理及其应用")
   (H3. "Taylor定理")
   (H3. "迭代过程")
   (H2. "函数序列")
   (P "在本章中, 近似又一次成为了我们兴趣的中心. 正如"
      (Ref "convergence") ", 我们也将研究序列和级数. "
      "不同之处在于这里我们所考虑的是项为函数的序列这种更为复杂的情况. "
      "在这种环境下存在着两种视角: "
      "我们可以局部地考虑这样的序列, 即逐点地, 或者是全局地考虑. "
      "在第二种情形下, 将序列的项考虑为某个函数空间的元素是很自然的, "
      "于是我们又回到了" (Ref "convergence")
      "的情况. 如果序列中的函数均为有界的, "
      "那么我们就有了一个有界函数的Banach空间中的序列, "
      "于是我们可以应用我们在第二章之中所建立的所有关于序列和级数的结果. "
      "这种方法是非常富有成效的, 允许简短而优雅的证明, "
      "并且第一次刻画了于其中我们建立了分析基础的抽象框架的优势.")
   (P "第一节我们研究了出现于对于函数序列的研究之中的各种收敛概念. "
      "其中最为重要的是一致收敛, 而这不过就是有界函数空间中的收敛. "
      "本节的主要结果是Weierstrass优级数判别法, "
      "然而这不过就是将第二章所建立的优级数判别法应用于有界函数的Banach空间.")
   (P "第二节致力于研究函数序列的连续性, 可微性, 以及收敛之间的关联. "
      "就我们具体的Banach空间的储备而言, "
      "这里我们加入了一个极其重要且自然的例子: "
      "紧度量空间上的连续函数的空间.")
   (P "紧接着的一节里我们继续检视之前我们所研究的幂级数并研究所谓的解析函数, "
      "其可由幂级数所局部表示. 特别地, 我们又一次分析了Taylor级数并"
      "推导了一些经典的幂级数表示. 对于优美而重要的解析函数理论的"
      "更为深刻的探究必须要推迟到我们有了积分的概念.")
   (P "最后一节考虑了通过多项式对于连续函数进行逼近. "
      "尽管Taylor多项式提供了局部的逼近, "
      "这里我们所感兴趣的是一致逼近. "
      "主要的结果是Stone-Weierstrass定理. "
      "除此以来, 我们还稍微观察了一下周期函数的行为, "
      "并证明了以" (&i* $2 $pi) "为周期的连续函数的Banach代数同构于"
      "单位圆上的连续函数的Banach代数. "
      "由此事实我们可以直接得到周期函数的Weierstrass逼近定理.")
   (H3. "一致收敛")
   (P "对于函数序列而言, 各种不同的收敛是可能的, "
      "这依赖于我们是对所牵涉的函数的逐点行为还是" (Q "全局")
      "行为感兴趣. 本节我们同时引入了逐点收敛和一致收敛, "
      "并研究了它们之间的关系. "
      "本节我们所推导出的结果构成了一切对于分析学的更深探究的基础.")
   (P "对于整个本节而言, " $X "是一个集合而"
      (&:= $E (tu0 $E (&abs $dummy)))
      "是" $KK "上的一个Banach空间.")
   (H4. "逐点收敛")
   (P "一个" (B $X "上的" $E "-值函数序列") "不过就是"
      $E^X "中的一个序列" (@ $f_n)
      ". 如果对于" $X "和" $E "的选取在上下文中是清晰的 "
      "(或者完全无关紧要), 那么我们就简单地称"
      (@ $f_n) "是一个" (B "函数序列") ".")
   (P "函数序列" (@ $f_n) (B "逐点收敛") "于"
      (∈ $f $E^X) ", 如果对于每个" (∈ $x $X)
      ", " $E "中序列" (@ (app $f_n $x))
      "收敛至" (app $f $x) ". 在这种情况下我们记"
      (&pointw $f_n $f) "或是"
      (&-> $f_n $f) " (pointw), 并称" $f
      "为" (@ $f_n) "的" (B "(逐点)极限")
      "或者" (B "(逐点)极限函数") ".")
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li "设" (@ $f_n) "逐点收敛, 那么极限函数是唯一的."
            ((proof)
             
             ))
        (Li "以下陈述是等价的:"
            (Ol #:attr* '((type "i"))
                (Li (&-> $f_n $f) " (pointw).")
                (Li "对于每个" (∈ $x $X) "和" (&> $epsilon $0)
                    ", 存在一个自然数" (&= $N (appl $N $x $epsilon))
                    "满足对于" (&>= $n $N) "有"
                    (&< (Dabs (app $f_n $x) (app $f $x))
                        $epsilon) ".")
                (Li "对于每个" (∈ $x $X) ", " (@ (app $f_n $x))
                    "在" $E "中是一个Cauchy序列."))
            ((proof)
             
             ))
        (Li "若" $E "被替换为一个任意的度量空间, "
            "以上的定义仍然是有意义的.")))
   ((Example #:id "pointwise_examples")
    (Ol #:attr* '((type "a"))
        (Li "令" (&:= $X (|[]| $0 $1)) ", " (&:= $E $RR)
            ", 以及" (&:= (app $f_n $x) (^ $x (&+ $n $1)))
            ", 那么" (@ $f_n) "逐点收敛于函数"
            (func $f (|[]| $0 $1) $RR) ", 其定义为"
            (MB (&:= (app $f $x)
                     (Choice0
                      ($0 $cm (∈ $x (|[)| $0 $1)))
                      ($1 $cm (&= $x $1)))) "."))
        (Li "令" (&:= $X (|[]| $0 $1)) ", "
            (&:= $E $RR) ", 以及"
            (MB (&:= (app $f_n $x)
                     (Choice0
                      ((&i* $2 $n $x) $cm (∈ $x (|[]| $0 (inv0 (&i* $2 $n)))))
                      ((&- $2 (&i* $2 $n $x))
                       $cm (∈ $x (|(]| (inv0 (&i* $2 $n)) (inv0 $n))))
                      ($0 $cm (∈ $x (|(]| (inv0 $n) $1))))) ",")
            "那么" (@ $f_n) "逐点收敛于" $0 ".")
        (Li "令" (&:= $X $RR) ", " (&:= $E $RR) ", 以及"
            (MB (&:= (app $f_n $x)
                     (Choice0
                      ((inv0 (@+ $n $1)) $cm (∈ $x (|[)| $n (&+ $n $1))))
                      ($0 $cm "否则的话"))) ",")
            "那么在这种情况下" (@ $f_n) "也逐点收敛于" $0 ".")))
   (P "在" (Ref "pointwise_examples")
      "的a中, 我们发现即便序列的每个项都是可微的, "
      "极限函数甚至都可以不是连续的. "
      "因此, 对于诸多目的而言, 逐点收敛太弱了, "
      "于是我们需要定义更强的一种收敛, "
      "其可以保证序列中的函数的性质与极限函数共享.")
   (H4. "一致收敛")
   (P "一个函数序列" (@ $f_n) (B "一致收敛") "于"
      $f ", 如果对于每个" (&> $epsilon $0) ", 存在某个"
      (∈ (&= $N (app $N $epsilon)) $NN) "满足"
      (MBL "(1.1)"
           (&cm (&< (Dabs (app $f_n $x) (app $f $x)) $epsilon)
                (&>= $n $N) (∈ $x $X)) ".")
      "在这种情况下我们记" (&unf $f_n $f) "或者"
      (&-> $f_n $f) " (unf).")
   (P "逐点收敛和一致收敛的本质区别在于, 对于一致收敛而言, "
      $N "依赖于" $epsilon "但不依赖于" (∈ $x $X)
      ", 而对于逐点收敛来说, 就一个给定的" $epsilon
      "而言, 一般" (appl $N $epsilon $x)
      "要随着点的改变发生变化. "
      "对于一致收敛而言, 不等式(1.1)相对于"
      (∈ $x $X) (Em "一致地") "成立.")
   ((RemarkExample)
    (Ol #:attr* '((type "a"))
        (Li "任何一致收敛的函数序列都逐点收敛, 即"
            (&-> $f_n $f) " (unf) 可以推出"
            (&-> $f_n $f) " (pointw).")
        (Li "a的逆命题是错的, "
            "也就是存在逐点收敛的函数序列并不一致收敛."
            ((proof)
             )
            )
        (Li ""
            )
        )
    )
   ((Proposition)
    "(一致收敛的Cauchy准则) 以下陈述是等价的:"
    (Ol #:attr* '((type "i"))
        (Li "函数序列" (@ $f_n) "一致收敛.")
        (Li ""
            )
        )
    )
   (H3. "函数序列的连续性和可微性")
   (H3. "解析函数")
   (H3. "多项式逼近")
   ))