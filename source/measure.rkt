#lang racket
(provide measure.html)
(require SMathML)
(define (\]\[ a b)
  (: $rb a $cm b $lb))
(define Δ $Delta:normal)
(define $Sup (Mi "sup"))
(define (Sup c e)
  (ap (__ $Sup c) e))
(define $Inf (Mi "inf"))
(define (Inf c e)
  (ap (__ $Inf c) e))
(define (\[\] a b)
  (li0 a b))
(define (∀ . x*)
  (: $forall (apply ∈ x*)))
(define (∃ . x*)
  (: $exists (apply ∈ x*)))
(define Σ $Sigma:normal)
(define Σ_0 (_ Σ $0))
(define Σ_A (_ Σ $A:script))
(define $\\ (Mo "\\"))
(define $power $P:script)
(define (&power X)
  (ap $power X))
(define (Seq E)
  (_ (ang0 (_ E $n)) (∈ $n $NN)))
(define Seq:E (Seq $E))
(define $cup $union)
(define-infix*
  (&\\ $\\)
  (&Δ Δ)
  (&cup $cup)
  
  )
(define-@lized-op*
  (@\\ &\\)
  (@union &union)
  (@power &power)
  (@cap &cap)
  (@cup &cup)
  
  )
(define (format-section section)
  (apply string-append
         (add-between
          (map number->string (reverse section))
          ".")))
(define (h1-present %heading attr* . html*)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  `(h1 ,(attr*-set attr* 'id id)
       ,(format "第~a卷 " (format-section section))
       . ,html*))
(define (H1. #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present h1-present #:level 1)
    ,attr* . ,html*))
(define (h2-present %heading attr* . html*)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  `(h2 ,(attr*-set attr* 'id id)
       ,(format "第~a章 " (format-section section))
       . ,html*))
(define (H2. #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present h2-present #:level 2)
    ,attr* . ,html*))
(define (h3-present %heading attr* . html*)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  `(h3 ,(attr*-set attr* 'id id)
       ,(format "第~a节 " (format-section section))
       . ,html*))
(define (H3. #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present h3-present #:level 3)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (format "~a.~a"
               (format-section section)
               (format-index index))))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B (format "~a. " num) name ". ")
        (B name ". "))))
(define (format-index index)
  (define (integer->upper i)
    (integer->char (+ i 64)))
  (let iter ((rest index) (result '()))
    (if (= rest 0)
        (list->string result)
        (iter (quotient (- rest 1) 26)
              (cons (integer->upper
                     (add1 (remainder (- rest 1) 26)))
                    result)))))
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
    (Cite `(a ((href ,href)) ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Remark "评注" "remark")
  
  )
(define (entry name)
  (Entry name "entry"))
(define measure.html
  (TnTmPrelude
   #:title "测度论"
   #:css "styles.css"
   (H1. "不可归约的最小")
   (H2. "测度空间")
   (H3. $sigma "-代数")
   ((Definition #:id "sigma-algebra")
    "令" $X "是一个集合. 一个"
    (B $X "的子集的" $sigma "-代数")
    " (有时也被称为" (B $sigma "-域")
    ") 是一个" $X "的子集的族" Σ ", 满足"
    (Ol #:attr* '((type "i"))
        (Li (∈ $empty Σ) ";")
        (Li "对于每个" (∈ $E Σ) ", 其于" $X "中的补"
            (&\\ $X $E) "属于" Σ ";")
        (Li "对于" Σ "中的每个序列" Seq:E ", 其并"
            (Union (∈ $n $NN) $E_n) "属于" Σ ".")))
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li "几乎所有数学主题的学习都是从定义开始的. "
            "在这个阶段, 没有替代死记硬背的学习方法. "
            "这些定义包裹了诸多人物数年 (有时甚至是数个世纪) "
            "的巧思, 你不能期望它们总是可以与你熟悉的想法对应起来.")
        (Li "然而, 你永远应该立即去寻求使新的定义变得更加具体的方法, "
            "一般是藉由你已有的数学经验寻找例子. 在这里"
            (Q $sigma "-代数") "的情况下, 以下所要描述的真正的例子"
            "本质上来说是全新的&mdash;&mdash;也就是说, "
            "你需要从根本上阅读本章. 然而, 你应该立即能够想到两个例子, "
            "而之后你应该将这两个例子铭记在心:"
            (Ol #:attr* '((type "i"))
                (Li "对于任意的" $X ", " (&= Σ (setE $empty $X))
                    "是" $X "的子集的一个" $sigma "-代数.")
                (Li "对于任意的" $X ", " (&power $X)
                    ", 即" $X "的所有子集构成的集合, 是"
                    $X "的子集的一个" $sigma "-代数."))
            "这些当然是" $X "的子集的" $sigma "-代数中最小的和最大的. "
            "而且, 尽管我们不会在这两个例子身上花多少时间, "
            "实际上它们仍然是重要的.")
        (Li "术语" (B "可测空间") "经常用来指代一个序对"
            (tu0 $X Σ) ", 其中" $X "是一个集合而"
            Σ "是" $X "的子集的一个" $sigma "-代数. "
            "然而, 就我个人而言, 除非时间紧迫, 否则我将避免使用这个术语, "
            "因为实际上这种对象的许多最有趣的例子并无有用的测度与之关联.")))
   (((entry "无穷并和交"))
    "如果你还没有见过无穷并, 那么值得驻足观察一下式子" (Union (∈ $n $NN) $E_n)
    ". 这是属于集合" $E_n "中的一个或多个的点构成的集合; 我们可以将其写为"
    (MB (deriv (Union (∈ $n $NN) $E_n)
               (setI $x (&cm (∃ $n $NN) (∈ $x $E_n)))
               (&union $E_0 $E_1 $E_2 $..c)))
    "(我以" $NN "代表自然数集" (setE $0 $1 $2 $3 $..h)
    ".) 以相同的方式, 记"
    (MB (deriv (Cap (∈ $n $NN) $E_n)
               (setI $x (&cm (∈ $x $E_n) (∀ $n $NN)))
               (&cap $E_0 $E_1 $E_2 $..c)))
    "测度空间的基本理论的一个特征在于, 与你之前的经验相比, "
    "它可能需要更多地利用集合操作" $union ", " $cap
    ", " $\\ " (" (Q "集合差") ": "
    (&= (&\\ $E $F) (setI $x (&cm (∈ $x $E) (&!in $x $F))))
    "), " Δ " (" (Q "对称差") ": "
    (&= (&Δ $E $F) (&union (@\\ $E $F) (@\\ $F $E))
        (&\\ (@cup $E $F) (@cap $E $F)))
    "), 并带有无穷并和交所增添的复杂. 我强烈建议在某个时间点花些时间做一做"
    (Ref "basicex11") "的练习a.")
   (((Entry (Span $sigma "-代数的基本性质") "entry"))
    "如果" Σ "是" $X "的子集的一个" $sigma "-代数, 那么其具有以下性质."
    (Ol #:attr* '((type "a"))
        (Li "对于任意的" (∈ $E $F Σ) ", " (∈ (&union $E $F) Σ)
            ". " $P:bold-script " 因为如果" (∈ $E $F Σ)
            ", 置" (&= $E_0 $E) ", 而" (&>= $n $1) "时"
            (&= $E_n $F) ", 那么" Seq:E "是" Σ "中的一个序列, 而"
            (∈ (&= (&union $E $F) (Union (∈ $n $NN) $E_n)) Σ)
            ". " $Q:bold-script)
        (Li "对于任意的" (∈ $E $F Σ) ", " (∈ (&cap $E $F) Σ)
            ". " $P:bold-script " 根据" (Ref "sigma-algebra")
            "的定义之ii, " (∈ (&\\ $X $E) Σ) "且"
            (∈ (&\\ $X $F) Σ) "; 根据本条目之a, "
            (∈ (&union (@\\ $X $E) (@\\ $X $F)) Σ)
            "; 再次根据" (Ref "sigma-algebra") "之ii, "
            (∈ (&\\ $X (@union (@\\ $X $E) (@\\ $X $F))) Σ)
            "; 但这就是" (∈ (&cap $E $F) Σ) ". " $Q:bold-script)
        (Li "对于任意的" (∈ $E $F Σ) ", " (∈ (&\\ $E $F) Σ)
            ". " $P:bold-script " "
            (∈ (&= (&\\ $E $F) (&cap $E (@\\ $X $F))) Σ)
            ". " $Q:bold-script)
        (Li "现在设" Seq:E "是" Σ "中的一个序列, 并考虑"
            (MB (deriv (Cap (∈ $n $NN) $E_n)
                       (setI $x (&cm (∈ $x $E_n) (∀ $n $NN)))
                       (&cap $E_0 $E_1 $E_2 $..c)
                       (&\\ $X (Union (∈ $n $NN) (@\\ $X $E_n)))))
            "其也属于" Σ ".")))
   (((entry "更多关于无穷并和交的讨论"))
    (Ol #:attr* '((type "a"))
        (Li "到目前为止, 我们只在由自然数集" $NN "索引的序列"
            Seq:E "的上下文中考虑了无穷并和交. "
            "在前方的内容中, 诸多其他的形式也会以或多或少自然的方式出现. "
            "例如, 可以考虑具有以下形式的集合"
            (MB (&= (Union (&>= $n $4) $E_n)
                    (&union $E_4 $E_5 $E_6 $..c)))
            (MB (&= (Union (∈ $n $ZZ) $E_n)
                    (setI $x (&cm (∃ $n $ZZ) (∈ $x $E_n)))
                    (&union $..c (_ $E $-2) (_ $E $-1)
                            $E_0 $E_1 $E_2 $..c)))
            (MB (&= (Union (∈ $q $QQ) $E_q)
                    (setI $x (&cm (∃ $q $QQ) (∈ $x $E_q)))))
            "其中我以" $ZZ "代表由所有整数构成的集合, " $QQ
            "代表由所有有理数构成的集合. 如果每个"
            (&cm $E_n $E_q) "属于一个" $sigma "-代数"
            Σ ", 那么这些并也属于. 另一方面, 以下情形"
            (MB (&= (Union (∈ $t (\[\] $0 $1)) $E_t)
                    (setI $x (&cm (∃ $t (\[\] $0 $1))
                                  (∈ $x $E_t)))))
            "在一个" $sigma "-代数包含每个" $E_t
            "的情况下却可能并不属于该" $sigma "-代数. "
            "读者有必要对于特定的指标集建立直觉, 例如"
            (&cm $NN $ZZ $QQ) ", 其在" $sigma
            "-代数的上下文中是" (Q "安全")
            "的, 也要记得那些并不安全的例子.")
        (Li "我希望你已经见过Cantor关于无限集合的理论了, "
            "那么以下内容不过是对于熟悉材料的重述; "
            "但如果没有, 我希望它可以作为对于这些想法的"
            "一个初次但非常不完整的导引. 要义在于, "
            "对于(上一段的)前三个例子而言, "
            "我们可以将所牵涉的集合族重排为简单的集合序列. "
            "对于第一个例子, 这是相当初等的; 对于" (∈ $n $NN)
            ", 令" (&= (_^ $E $n $prime) (_ $E (&+ $n $4)))
            ", 那么可以看到"
            (∈ (&= (Union (&>= $n $4) $E_n)
                   (Union (∈ $n $NN) (_^ $E $n $prime)))
               Σ)
            ". 对于其他两个例子, 我们则需要了解一点关于集合"
            $ZZ "和" $QQ "的知识. 实际上, 我们可以找到整数的序列"
            (Seq $k) "和有理数的序列" (Seq $q)
            "使得每个整数都作为一个" $k_n "出现(至少一次), "
            "而每个有理数都作为一个" $q_n "出现(至少一次); "
            "换言之, 函数"
            (&cm (&\|-> $n $k_n) (&-> $NN $ZZ)) "和"
            (&cm (&\|-> $n $q_n) (&-> $NN $QQ))
            "都是满射的. " $P:bold-script
            " 存在许多不同的方式可以达成这点; 其中一种如下, 置"
            (MB (&= $k_n
                    (Choice0
                     ((~ $n $2) $cm $n "为偶数")
                     ((&- (~ (&+ $n $1) $2))
                      $cm $n "为奇数"))))
            (MB (&= $q_n
                    (~ (&- $n $m^3 $m^2)
                       (&+ $m $1)))
                ", 如果" (∈ $m $NN) "且"
                (&< (&<= $m^3 $n)
                    (^ (@+ $m $1) $3)))
            "(你应该仔细检查这些公式以确保它们的确能做到我所声明的事情.) "
            $Q:bold-script " 现在, 为了处理" (Union (∈ $n $ZZ) $E_n)
            ", 我们可以对于每个" (∈ $n $NN) "置"
            (MB (∈ (&= (_^ $E $n $prime)
                       (_ $E $k_n))
                   Σ))
            "那么就有"
            (MB (∈ (&= (Union (∈ $n $ZZ) $E_n)
                       (Union (∈ $n $NN) (_ $E $k_n))
                       (Union (∈ $n $NN) (_^ $E $n $prime)))
                   Σ))
            "如法炮制, 我们也有"
            (MB (∈ (&= (Union (∈ $q $QQ) $E_q)
                       (Union (∈ $n $NN) (_ $E $q_n)))
                   Σ))
            "注意到第一个例子" (Union (&>= $n $4) $E_n)
            "也可以想成是相同原理的一个应用; 映射"
            (&\|-> $n (&+ $n $4)) "是从" $NN
            "到" (setE $4 $5 $6 $7 $..h) "的一个满射.")))
   (((entry "可数集合"))
    (Ol #:attr* '((type "a"))
        (Li "集合" (setI $n (&>= $n $4)) ", " $ZZ
            ", " $QQ "使得这样的过程能够成立的共同特征在于它们都是"
            (Q "可数的") ". "
            )
        )
    )
   (((entry "Borel集合"))
    "这里我可以描述一类非平凡的" $sigma "-代数; 其构造是抽象的, "
    "但是这样的技术是重要的, 并且这个术语也是测度论的基本词汇的一部分."
    (Ol #:attr* '((type "a"))
        (Li "令" $X "是一个集合, 令" $S:fraktur "是任意的" $X
            "的子集的" $sigma "-代数的非空的族. (因此, "
            $S:fraktur "的每个" (Em "成员") "本身就是一个集合的"
            (Em "族") "; " (&sube $S:fraktur (&power (@power $X)))
            ".) 那么"
            (MB (&= (Cap $S:fraktur)
                    (setI $E (&cm (∈ $E Σ) (∀ Σ $S:fraktur)))))
            "即所有属于" $S:fraktur "的" $sigma "-代数之交, 是"
            $X "的子集的一个" $sigma "-代数. " $P:bold-script
            (Ol #:attr* '((type "i"))
                (Li "根据假设, " $S:fraktur "非空; 取"
                    (∈ Σ_0 $S:fraktur) "; 那么"
                    (&sube (Cap $S:fraktur) Σ_0 (&power $X))
                    ", 故" (Cap $S:fraktur) "的每个成员都是"
                    $X "的一个子集.")
                (Li "因为对于每个" (∈ Σ $S:fraktur)
                    ", " (∈ $empty Σ) ", 故"
                    (∈ $empty (Cap $S:fraktur)) ".")
                (Li "如果" (∈ $E (Cap $S:fraktur))
                    ", 那么对于每个" (∈ Σ $S:fraktur) "有"
                    (∈ $E Σ) ", 故对于每个" (∈ Σ $S:fraktur)
                    "有" (∈ (&\\ $X $E) Σ) ", 因而"
                    (∈ (&\\ $X $E) (Cap $S:fraktur)) ".")
                (Li "令" Seq:E "是" (Cap $S:fraktur)
                    "中的任意序列, 那么对于每个" (∈ Σ $S:fraktur)
                    ", " Seq:E "都是" Σ "中的一个序列, 故"
                    (∈ (Union (∈ $n $NN) $E_n) Σ)
                    "; 鉴于" Σ "是任意的, 所以"
                    (∈ (Union (∈ $n $NN) $E_n) (Cap $S:fraktur))
                    ". " $Q:bold-script)))
        (Li "现在令" $A:script "是任意的一个" $X "的子集的族, 考虑"
            (MB (&= $S:fraktur
                    (setI Σ (: Σ "是" $X "的子集的一个" $sigma "-代数, 而且"
                               (&sube $A:script Σ)))) ".")
            "根据定义, " $S:fraktur "是" $X "的子集的"
            $sigma "-代数的一个族; 而且, 其是非空的, 因为"
            (∈ (&power $X) $S:fraktur) ". 因此, "
            (&= Σ_A (Cap $S:fraktur)) "是" $X "的子集的一个"
            $sigma "-代数. 鉴于对于每个" (∈ Σ $S:fraktur)
            "都有" (&sube $A:script Σ) ", " (&sube $A:script Σ_A)
            "; 因此, " Σ_A "自身就属于" $S:fraktur
            "; 其为包含" $A:script "的最小的" $X "的子集的"
            $sigma "-代数. 我们称" Σ_A "是" (B "由" $A:script "生成")
            "的" $X "的子集的" $sigma "-代数. 以下是两个例子."
            (Ol #:attr* '((type "i"))
                (Li "对于任意的集合" $X ", 由" $empty
                    "生成的" $X "的子集的" $sigma "-代数为"
                    (setE $empty $X) ".")
                (Li "由" (setI (setE $n) (∈ $n $NN))
                    "生成的" $NN "的子集的" $sigma "-代数是"
                    (&power $NN) ".")))
        (Li (Ol #:attr* '((type "i"))
                (Li "我们称一个集合" (&sube $G $RR) "是" (B "开")
                    "的, 如果对于每个" (∈ $x $G) ", 存在"
                    (&> $delta $0) "使得开区间"
                    (\]\[ (&- $x $delta) (&+ $x $delta))
                    "被包含于" $G ".")
                (Li "类似地, 对于任意的" (&>= $r $1)
                    ", 我们称一个集合" (&sube $G $RR^r)
                    "在" $RR^r "中是" (B "开") "的, 如果对于每个"
                    (∈ $x $G) ", 存在" (&> $delta $0) "使得"
                    (&sube (setI $y (&< (&norm (&- $y $x)) $delta)) $G)
                    ", 其中对于"
                    (∈ (&= $z (tu0 $zeta_1 $..h $zeta_r)) $RR^r)
                    ", 我记"
                    (&= (&norm $z)
                        (Msqrt (sum (&= $i $1) $r
                                    (^ (&abs $zeta_i) $2))))
                    "; 因此, " (&norm (&- $y $x)) "不过就是从" $y "到" $x
                    "的通常的Euclid距离.")))
        (Li "现在" $RR "的" (B "Borel集合") "不过就是由" $RR
            "的所有开集构成的族所生成的" $RR "的子集的" $sigma "-代数的成员; "
            $sigma "-代数本身则被称为" (B "Borel " $sigma "-代数")
            ". " $RR^r "的Borel集合和Borel " $sigma
            "-代数以类似的方式定义.")
        (Li "一些读者可能会感到这里的构建并没有给出一个Borel集合到底长什么样子的想法. "
            "(开集要远为简单; 见" (Ref "furtherex11") "的练习e.) "
            "实际上, 这个概念的重要性在很大程度上来源于"
            "存在另外更加显式且在某种意义上更加具体的描述Borel集合的方式. "
            "我将于第4卷的第4.2章回到这个话题上来.")));@@@
   (((entry "基本练习") #:id "basicex11")
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (((entry "深入练习") #:id "furtherex11")
    (Ol #:attr* '((type "a"))
        (Li "在" $RR^r "中, 其中" (&>= $r $1) ", 表明当"
            (&sube $G $RR^r) "是开的且" (∈ $a $RR^r)
            "时, " (&= (&+ $G $a) (setI (&+ $x $a) (∈ $x $G)))
            "也是开的. 基于这个结果, 证明当" (&sube $E $RR^r)
            "是一个Borel集合且" (∈ $a $RR^r) "时, "
            (&+ $E $a) "也是一个Borel集合. (提示: 表明"
            (setI $E (: (&+ $E $a) "是一个Borel集合"))
            "是一个包含所有开集的" $sigma "-代数.)")
        (Li "令" $X "是一个集合, " Σ "是一个" $X "的子集的"
            $sigma "-代数, 而" $A "是" $X "的任意一个子集. 证明"
            (setI (&union (@cap $E $A) (@\\ $F $A))
                  (∈ $E $F Σ))
            "是一个" $X "的子集的" $sigma "-代数, 而且是由"
            (&union Σ (setE $A)) "生成的" $sigma "-代数.")
        (Li "令" (&sube $G $RR^2) "是一个开集. 证明" $G
            "的所有水平截线和垂直截线"
            (MB (&cm (setI $xi (∈ (tu0 $xi $eta) $G))
                     (setI $xi (∈ (tu0 $eta $xi) $G))))
            "都是" $RR "的开子集.")
        (Li ""
            )
        )
    )
   (((entry "注记和评论"))
    
    )
   (H3. "测度空间")
   (P "我希望我们已经准备好迎来第二个定义了, 这个定义是此专著所有工作之基础.")
   ((Definition)
    "一个" (B "测度空间") "是一个三元组" (tu0 $X Σ $mu) ", 其中"
    (Ol #:attr* '((type "i"))
        (Li $X "是一个集合;")
        (Li Σ "是" $X "的子集的一个" $sigma "-代数;")
        (Li (func $mu Σ (\[\] $0 $inf))
            "是一个函数, 满足"
            (Ol #:attr* '((style "list-style-type: lower-greek;"))
                (Li (&= (ap $mu $empty) $0) ";")
                (Li "如果" Seq:E "是" Σ "中的一个互不相交 (disjoint) 的序列, 那么"
                    (&= (app $mu (Union (∈ $n $NN) $E_n))
                        (sum (&= $n $0) $inf (ap $mu $E_n))) "."))))
    "在此上下文之中, " Σ "的成员被称为" (B "可测") "集合, 而" $mu
    "被称为" (B $X "上的一个测度") ".")
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (((entry "测度空间的基本性质"))
    "令" (tu0 $X Σ $mu) "是一个测度空间."
    (Ol #:attr* '((type "a"))
        (Li "如果" (∈ $E $F Σ) "且" (&= (&cap $E $F) $empty) ", 那么"
            (&= (app $mu (&union $E $F)) (&+ (ap $mu $E) (ap $mu $F))) ".")
        (Li "如果" (∈ $E $F Σ) "且" (&sube $E $F) ", 那么"
            (&<= (ap $mu $E) (ap $mu $F)) ".")
        (Li "对于任意的" (∈ $E $F Σ) ", "
            (&<= (app $mu (&union $E $F)) (&+ (ap $mu $E) (ap $mu $F))) ".")
        (Li "如果" Seq:E "是" Σ "中任意的序列, 那么"
            (&<= (app $mu (Union (∈ $n $NN) $E_n))
                 (sum (&= $n $0) $inf (ap $mu $E_n))) ".")
        (Li "如果" Seq:E "是" Σ "中的一个非降序列 (也就是说, 对于每个"
            (∈ $n $NN) ", " (&sube $E_n (_ $E (&+ $n $1)))
            "), 那么"
            (MB (&= (app $mu (Union (∈ $n $NN) $E_n))
                    (lim $n $inf (ap $mu $E_n))
                    (Sup (∈ $n $NN) (ap $mu $E_n))) "."))
        (Li "如果" Seq:E "是" Σ "中的一个非升序列 (也就是说, 对于每个"
            (∈ $n $NN) ", " (&sube (_ $E (&+ $n $1)) $E_n)
            "), 并且某个" (ap $mu $E_n) "是有限的, 那么"
            (MB (&= (app $mu (Cap (∈ $n $NN) $E_n))
                    (lim $n $inf (ap $mu $E_n))
                    (Inf (∈ $n $NN) (ap $mu $E_n))) "."))))
   ((proof)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (H3. "外测度和Carathéodory构造")
   (P "这里我将引入构造测度的最重要方法.")
   ((Definition)
    "现在我们来到本章的第三个基本定义." (Br)
    "令" $X "是一个集合. " $X "上的一个" (B "外测度") "是一个函数"
    (func $theta (&power $X) (\[\] $0 $inf)) ", 满足"
    (Ol #:attr* '((type "i"))
        (Li (&= (ap $theta $empty) $0) ";")
        (Li "如果" (&sube $A $B $X) ", 那么"
            (&<= (ap $theta $A) (ap $theta $B)) ";")
        (Li "对于" $X "的子集的每个序列" (Seq $A) ", "
            (&<= (app $theta (Union (∈ $n $NN) $A_n))
                 (sum (&= $n $0) $inf (ap $theta $A_n))) ".")))
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (H1. "广阔的基础")
   (H1. "测度代数")
   (H1. "拓扑测度空间")
   (H1. "集合论式测度论")
   (H1. "随机分析")
   ))