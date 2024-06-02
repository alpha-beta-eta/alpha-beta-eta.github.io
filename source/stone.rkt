#lang racket
(provide stone.html)
(require SMathML)
(define (symdiff a b)
  (&join (@meet a (&complement b))
         (@meet b (&complement a))))
(define ((tcomment #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "tcomment")))
   (B (format "译者注记~a." n)) " " x*))
(define-syntax eqn*
  (syntax-rules ()
    ((_ (x ...) ...)
     (MB (set-attr*
          (&Table (x ...) ...)
          'columnalign "right center left left")))))
(define (powerset X)
  (ap $P X))
(define (∀ domain statement)
  (: $forall domain (@ statement)))
(define $Join (Mo "&Vee;"))
(define Join (make-bigop $Join))
(define $Meet (Mo "&Wedge;"))
(define Meet (make-bigop $Meet))
(define $complement $neg)
(define (&complement x)
  (ap $complement x))
(define $join $disj)
(define $meet $conj)
(define-infix*
  (&join $disj)
  (&meet $conj))
(define-@lized-op*
  (@join &join)
  (@meet &meet))
(define (format-num section index)
  (cond ((eq? (car section) '*)
         (if index
             (format "~a" index)
             #f))
        (else
         (if index
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     index)
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     "*")))))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
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
    (if num
        (Cite `(a ((href ,href)) ,name ,num))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Lemma "引理" "lemma")
  (Definition "定义" "definition")
  (Theorem "定理" "theorem")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise")
  (Remark "评注" "remark")
  (Corollary "推论" "corollary"))
(define stone.html
  (TnTmPrelude
   #:title "Stone空间笔记"
   #:css "styles.css"
   (H1. "Stone空间笔记")
   (H2 "前言")
   (P "1977年夏天的时候我第一次意识到并不存在一本单独的书籍"
      "使得人们能够获得对于自Stone表示定理流出的所有数学推论"
      "的平衡看法, 而存在这样的一种书籍是很有用的. "
      "然而, 那时相对来说我并没有太刻意地追求这个想法; "
      "我唯一做的事情只是写下了一份暂定的章名列表, "
      "而这和最终面世的书籍相比并没有什么类似之处. "
      
      )
   (H2 "读者建议")
   (P "就和数学领域诸多研究级别的书籍一样, 本书是学生的教科书和专家的参考书之间的一种紧张的妥协. "
      "专家想必不需要帮助也可以从书本中找到他们想要的东西 (如果的确存在的话), "
      "因此这些注记主要是面向学生的, 或者是那些考虑将本书用作某个研究生课程的基础的讲师.")
   (H2 "引论: 历史视角下的Stone定理")
   (P "本书是关于某个特定定理 (即Boole代数的Stone表示定理) 以及由此45年来所发展的一些数学结果的. "
      "准备以此方式描绘某个数学想法发展的书籍的作者总是无可避免地面对着两种方法之间的妥协的必然性: "
      "一种是历史性方法, 其或多或少试图以年代顺序追逐每条线索的发展 "
      "(但是可能会忽略不同线索之间的某些内在联系); 另一种是逻辑性方法, 或者[Mac Lane 1980]"
      "所说的基因性方法, 其以事后诸葛的方式采取最为经济和无痛的路线到达主要结果 "
      "(但是因而可能会失去一些关于这些结果何以被视为重要的洞察).")
   (P "我所采取的实际妥协在于正文本身中全心全意沿着逻辑性方法的进路 "
      "(我们所采用的路线将于第2.4节抵达Stone定理的证明, "
      "即便保守来说也会给予那些历史脑读者以极大的震撼), "
      "但是以这样的引论作为本书的开始, 其将表示定理置于Stone证明它的实际历史上下文之中, "
      "并指出其后的那些发展是什么, "
      )
   (H2. "预备")
   (H3. "格")
   ((Definition)
    "令" $A "是一个集合. " $A "上的一个" (Em "偏序")
    "是一个二元关系" $<= ", 其满足"
    (Ol #:attr* '((type "i"))
        (Li "自反性: 对于所有的" (∈ $a $A)
            ", " (&<= $a $a) ";")
        (Li "传递性: 如果" (&<= $a $b) "且" (&<= $b $c)
            ", 那么" (&<= $a $c) ";")
        (Li "反对称性: 如果" (&<= $a $b) "且" (&<= $b $a)
            ", 那么" (&= $a $b) "."))
    "一个偏序集是一个装备了某个偏序的集合.")
   ((Definition)
    "令" $A "是一个偏序集, " $S "是" $A "的一个子集. "
    "我们称某个元素" (∈ $a $A) "是" $S "的一个"
    (Em "join") ", 或者说" (Em "最小上界")
    ", 并记作" (&= $a (Join $S)) ", 如果"
    (Ol #:attr* '((type "i"))
        (Li $a "是" $S "的一个上界, 即对于所有的"
            (∈ $s $S) "都有" (&<= $s $a) ";")
        (Li "如果" (∀ (∈ $s $S) (&<= $s $b))
            ", 那么" (&<= $a $b) "."))
    "反对称公理确保了" $S "的join一旦存在则是唯一的. "
    "如果" $S "是一个具有两个元素的集合"
    (setE $s $t) ", 那么我们对于" (Join (setE $s $t))
    "记" (&join $s $t) ". {译注: 实际上, 即便"
    (&= $s $t) ", 我们仍然可以使用这种记号.} "
    "如果" $S "是空集" $empty
    ", 那么我们对于" (Join $empty)
    "记" $0 "&mdash;&mdash;显然" $0
    "是" $A "的最小元素.")
   ((Proposition)
    "令" $A "是一个偏序集且其每个有限子集均有一个join, "
    "那么上述的二元运算" $join "以及元素" $0 "满足等式"
    (Ol #:attr* '((type "i"))
        (Li (&= (&join $a $a) $a))
        (Li (&= (&join $a $b) (&join $b $a)))
        (Li (&= (&join $a (@join $b $c))
                (&join (@join $a $b) $c)))
        (Li (&= (&join $a $0) $a)))
    "其中" (∈ $a $b $c $A)
    ". 简而言之, 我们可以说" (tu0 $A $join $0)
    "是一个交换幺半群, 且其中每个元素都是幂等的. "
    "{译注: 或者直接说交换幂等幺半群.} "
    "反过来, 我们有")
   ((Theorem #:auto? #f)
    "令" (tu0 $A $join $0) "是一个交换幺半群, "
    "且其中每个元素都是幂等的, 那么" $A
    "上存在唯一的偏序使得" (&join $a $b)
    "是" $a "和" $b "的join, 且" $0
    "是最小元.")
   ((proof)
    "显然, 如果这样一个偏序存在, 我们必然有"
    (&<= $a $b) "当且仅当" (&= (&join $a $b) $b)
    ". {译注: 这说明了若该偏序存在则唯一.} "
    "反过来, 取这个作为" $<= "的定义, 那么"
    $<= "的自反性可由等式i直接推得, "
    "而反对称性可由定义的形式得到. "
    "为了证明传递性, 设" (&<= $a $b)
    "且" (&<= $b $c) ", 那么"
    (eqn*
     ((&join $a $c) $= (&join $a (@join $b $c)) (: "因为" (&<= $b $c)))
     ($             $= (&join (@join $a $b) $c) "根据等式iii")
     ($             $= (&join $b $c)            (: "因为" (&<= $a $b)))
     ($             $= $c                       (: "因为" (&<= $b $c))))
    "于是" (&<= $a $c) "." (Br)
    "现在令" (&cm $a $b) "是" $A "的任意两个元素, 那么"
    (&= (&join $a (@join $a $b))
        (&join (@join $a $a) $b)
        (&join $a $b))
    ", 即" (&<= $a (&join $a $b))
    ", 类似地可以证明 (使用等式ii) "
    (&<= $b (&join $a $b))
    ". 如果" (&<= $a $c) "且" (&<= $b $c) ", 那么"
    (&= (&join (@join $a $b) $c)
        (&join $a (@join $b $c))
        (&join $a $c)
        $c)
    ", 即" (&<= (&join $a $b) $c)
    ". 因此, " (&join $a $b) "是" $a
    "和" $b "的join. 最后, 等式iv立即告诉我们"
    $0 "是" $A "的最小元.")
   (P "一个集合若是带有上面定理中所描述的结构则被称为一个"
      (Em "半格") "或者说" (Em "join半格")
      ". 这个定理是说半格的概念既可以基于序关系定义, "
      "也可以基于join运算定义; "
      "但是当我们开始考虑同态 (保持结构的映射) 时, "
      "两者存在重要的差异. "
      "一个半格同态" (func $f $A $B)
      " (即保持突出元素" $0 "和运算" $join
      "的映射) 必然是一个保序映射, "
      "但是半格之间的保序映射不一定是同态. "
      "{译注: 保序映射即偏序集之间的同态, 也称单调映射.}")
   ((Exercise #:auto? #f)
    
    )
   ((Definition)
    "对偶地, 在任意偏序集中我们可以考虑" (Em "meet")
    "或者说" (Em "最大下界") "的概念, "
    "其定义是通过反转join定义中的所有不等式得到的. 参照"
    (&cm (Join $S) (&join $a $b) $0)
    "我们可以定义" (&cm (Meet $S) (&meet $a $b) $1)
    ". 一个" (Em "格") "是一个偏序集" $A
    "满足其每个有限子集均有join和meet. "
    "根据前述定理, 这等价于说" $A
    "是一个装备了两个二元运算" $join "和" $meet
    "以及两个突出元素" $0 "和" $1 "的集合, 并且"
    (tu0 $A $join $0) "和" (tu0 $A $meet $1)
    "都是半格, 而且由这两个半格结构所导出的"
    $A "上的两个偏序是对立的.")
   ((Proposition #:auto? #f)
    "设" (tu0 $A $join $0) "和" (tu0 $A $meet $1)
    "都是半格, 那么" (tu0 $A $join $meet $0 $1)
    "是一个格当且仅当" (Em "吸收律")
    (MB (&cm (&= (&meet $a (@join $a $b)) $a)
             (&= (&join $a (@meet $a $b)) $a)))
    "对于所有" (∈ $a $b $A) "成立.")
   ((proof)
    "设吸收律成立, 那么" (&= (&join $a $b) $b) "可以推出"
    (&= (&meet $a $b) (&meet $a (@join $a $b)) $a)
    ", 反之亦然. 也就是说, 两个" $A "上的偏序是相合的. "
    "相反方向的证明是容易的.")
   ((tcomment)
    "这个命题本身的陈述稍有些模糊. 实际上, "
    (tu0 $A $join $meet $0 $1)
    "是一个格的更为精确的表述为由"
    (tu0 $A $join $0) "导出的偏序和"
    (tu0 $A $meet $1) "导出的偏序是相同的. "
    "然而, 这里的导出的含义和前面的定理又不完全相同. "
    "我说的是" (tu0 $A $meet $1)
    "的情形, 此时应该定义" (&<= $a $b)
    "为" (&= (&meet $a $b) $a)
    ". 若是仍然死板地按照之前的定理, "
    "那么原文也有提到, 这两种导出偏序应该恰好相反.")
   (P "因此, 我们对于格的形式定义为"
      (Q "一个集合带有两个二元运算" $join "和"
         $meet ", 以及两个突出元素" $0 "和" $1
         ", 其满足" $join "是结合的交换的幂等的, "
         "且" $0 "是其单位元, " $meet
         "也是结合的交换的幂等的, 且"
         $1 "是其单位元, 另外" $join
         "和" $meet "还满足吸收律."))
   ((Remark #:auto? #f)
    
    )
   ((Definition)
    "在绝大多数我们将会遇到的格里, 运算"
    $join "和" $meet "还会满足一个额外的等式, 即"
    (Em "分配律")
    (MB (&= (&meet $a (@join $b $c))
            (&join (@meet $a $b)
                   (@meet $a $c))))
    "如果我们将" $meet "想成乘法而" $join
    "想成加法, 那么这就是通常算术的分配律.")
   ((Lemma #:auto? #f)
    "如果一个格中分配律成立, 那么其对偶也成立, 即等式"
    (MB (&= (&join $a (@meet $b $c))
            (&meet (@join $a $b)
                   (@join $a $c)))))
   ((proof)
    
    )
   ((Proposition)
    "令" (&cm $a $b $c) "是某个分配格" $A
    "的三个元素, 那么至多存在一个" (∈ $x $A)
    "满足" (&= (&meet $x $a) $b) "且"
    (&= (&join $x $a) $c) ".")
   ((proof)
    "设" $x "和" $y "均满足这个条件, 那么"
    )
   (P "在任何格中, 一个元素" $x "若满足"
      (&= (&meet $x $a) $0) "且"
      (&= (&join $x $a) $1) "则被称为"
      $a "的一个" (Em "补")
      ". 这个命题告诉我们分配格中补若存在则唯一. "
      "一个" (Em "Boole代数") "是一个分配格" $A
      ", 其装备了一个额外的幺元运算"
      (func $complement $A $A) "使得"
      (&complement $a) "是" $a
      "的补. 既然" $complement
      "由定义中的其他数据唯一确定, "
      "那么可以推得Boole代数之间的任何格同态"
      (func $f $A $B) "实际上都是Boole代数同态, 即"
      $f "与" $complement "可以交换.")
   ((tcomment)
    "如果" (func $f $A $B) "是两个Boole代数之间的格同态, 并且"
    (∈ $a $b $A) "满足" (&= (&join $a $b) $1_A)
    "和" (&= (&meet $a $b) $0_A) ", 那么"
    (&= (&join (app $f $a) (app $f $b)) $1_B) "且"
    (&= (&meet (app $f $a) (app $f $b)) $0_B)
    ". 换言之, " (&= $a (&complement $b)) "而"
    (&= (app $f $a) (&complement (app $f $b))) ", 亦即"
    (&= (app $f (&complement $b))
        (&complement (app $f $b))) ".")
   ((Example)
    "是时候举一些例子了."
    (Ol #:attr* '((type "a"))
        (Li "对于任意的集合" $X ", 幂集" (powerset $X)
            "是一个格, 其中" $<= "被解释为包含关系, "
            $join "和" $meet "分别是子集的并和交, "
            $0 "和" $1 "分别是空集和整个" $X
            ". 而且, " (powerset $X)
            "是分配的, 因为对于" $X "的子集"
            (&cm $A $B $C) ", 我们有"
            
            )
        )
    )
   ((Definition)
    "接下来, 我们要草绘Boole代数和Boole环之间的等价性. "
    "在任意的Boole代数" $A "之中, 我们定义"
    (Em "对称差") "运算" $+ "为"
    (MB (&= (&+ $a $b)
            (&join (@meet $a (&complement $b))
                   (@meet $b (&complement $a)))) "."))
   ((Lemma #:auto? #f)
    "分配律"
    (&= (&meet $a (@+ $b $c))
        (&+ (@meet $a $b) (@meet $a $c)))
    "成立.")
   ((proof)
    
    )
   (P "我们将对于结合律"
      (MB (&= (&+ $a (@+ $b $c))
              (&+ (@+ $a $b) $c)))
      "的验证留给读者作为练习. 现在对于任意的"
      $a ", 我们有"
      (MB (&= (&+ $a $a)
              (&join (@meet $a (&complement $a))
                     (@meet $a (&complement $a)))
              (&join $0 $0)
              $0))
      "以及"
      (MB (&= (&+ $a $0)
              (symdiff $a $0)
              (&join $a $0)
              $a))
      "因此, " (tu0 $A $+ $0) "是一个群 "
      "(而且, 根据" $+ "的定义, 这个群显然是交换的), "
      (tu0 $A $+ $meet $0 $1)
      "是一个含幺交换环. "
      "{译注: 这当然也是幂等环.}")
   ((Definition)
    "反过来, 令" $A "是一个带有幺元" $1
    "的环, 且满足" (&= $a^2 $a)
    " (此时我们称" $A "为一个Boole环), 那么")
   ((Lemma #:auto? #f)
    (Ol #:attr* '((type "i"))
        (Li $A "是交换环.")
        (Li "对于每个" (∈ $a $A) ", "
            (&= (&+ $a $a) $0) ".")))
   ((tcomment)
    "之前受到Halmos误导, 他将满足" (&= (&+ $a $a) $0)
    "的环称为特征为二的环. 现在我想了一下, "
    "实际上特征为二这个性质在含幺环上才有意义. "
    "不过, 的确对于含幺环而言, "
    (&= (&+ $a $a) $0) "和" (&= (&+ $1 $1) $0)
    "是等价的. 当然, 鉴于两者涵义的确并不完全相同, "
    "所以我想还是不要混淆为好.")
   ((proof)
    
    )
   (P "故乘性结构" (tu0 $A $d* $1)
      "是一个半格 (换言之, 幂等交换幺半群), "
      "其偏序被定义为" (&<= $a $b) "当且仅当"
      (&= (&i* $a $b) $a)
      ". {译注: 原文要读者参考之前的定理, "
      "不过实际上是之前定理的对偶版本.} "
      "并且, 显然" $0 "在此偏序之下是"
      $A "中的最小元素.")
   (P "现在考虑" (&+ $a $b (&i* $a $b)) ". 我们有"
      (MB (&= (&i* $a (@+ $a $b (&i* $a $b)))
              (&+ $a (&i* $a $b) (&i* $a $b))
              $a))
      "以及"
      (MB (&= (&i* $b (@+ $a $b (&i* $a $b)))
              (&+ (&i* $b $a) $b (&i* $a $b))
              $b))
      "于是" (&+ $a $b (&i* $a $b))
      "是" $a "和" $b "的一个上界. "
      "但是, 如果" $c "是" $a "和" $b
      "的一个上界, 那么"
      (MB (&= (&i* (@+ $a $b (&i* $a $b)) $c)
              (&+ (&i* $a $c) (&i* $b $c)
                  (&i* $a $b $c))
              (&+ $a $b (&i* $a $b))))
      "故" (&+ $a $b (&i* $a $b))
      "实际上是最小上界. 如果我们定义"
      (&join $a $b) "为" (&+ $a $b (&i* $a $b))
      ", 那么我们就有了一个格结构"
      (tu0 $A $join $d* $0 $1)
      ". 而且, 根据类似于引理1.8的论证, "
      "我们可以验证" $d* "对于" $join
      "分配; 并且, 也很容易验证"
      (&+ $1 $a) "是" $a "的一个补. "
      "因此, " $A "是一个布尔代数.")
   (P "这个布尔代数中的对称差运算是什么? 我们有"
      
      )
   (H3. "理想和滤子")
   (H3. "一些范畴概念")
   (H3. "自由格")
   (H2. "locale导引")
   (H3. "frame和locale")
   (H2. "紧Hausdorff空间")
   (H2. "连续实值函数")
   (H2. "环的表示")
   (H2. "profiniteness和对偶")
   (H2. "连续格")
   ))