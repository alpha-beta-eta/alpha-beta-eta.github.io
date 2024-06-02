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
(define analysis1_amann.html
  (TnTmPrelude
   #:title "分析一 (Amann &amp; Escher)"
   #:css "styles.css"
   (H1. "分析一 (Amann &amp; Escher)")
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
   (H3. "序列的收敛")
   (H3. "实序列和复序列")
   (H3. "赋范向量空间")
   (H3. "单调序列")
   (H3. "无穷极限")
   (H3. "完备性")
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