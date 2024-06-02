#lang racket
(provide measure.html)
(require SMathML)
(define todo.svg
  (Svg
   #:attr* '((width "320")
             (height "160")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Path #:attr* '((x "0")
                   (y "0")
                   (d "M 0 0 h 320 v 160 h -320 z")
                   (fill "none")))
   (Text #:attr* '((x "130") (y "80")) "欠一张图")))
(define (len I) (ap $lambda I))
(define $G_n+1 (_ $G (&+ $n $1)))
(define $E_n+1 (_ $E (&+ $n $1)))
(define $a.e. (Mi "a.e."))
(define $=_a.e. (_ $= $a.e.))
(define $<=_a.e. (_ $<= $a.e.))
(define $>=_a.e. (_ $>= $a.e.))
(define $dom (Mi "dom"))
(define (&dom f) (ap $dom f))
(define $cap:id (Mi "&cap;"))
(define $.* (Mo "."))
(define $.*:id (Mi "."))
(define $+:id (Mi "+"))
(define $infimum (Mi "inf"))
(define $supremum (Mi "sup"))
(define infimum
  (case-lambda
    ((X) (ap $infimum X))))
(define supremum
  (case-lambda
    ((X) (ap $supremum X))))
(define $~ (Mo "~"))
(define (\]\[ a b)
  (: $rb a $cm b $lb))
(define (\[\[ a b)
  (: $lb a $cm b $lb))
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
(define Σ_D (_ Σ $D))
(define $\\ (Mo "\\"))
(define $power $P:script)
(define (&power X)
  (ap $power X))
(define Seq
  (case-lambda
    ((E) (_ (ang0 (_ E $n)) (∈ $n $NN)))
    ((E i I) (_ (ang0 (_ E i)) (∈ i I)))))
(define Seq:E (Seq $E))
(define $cup $union)
(define $s- (Mo "&Delta;"))
(define-infix*
  (&\\ $\\)
  (&Δ Δ)
  (&cup $cup)
  (&~ $~)
  (&.* $.*)
  (&s- $s-)
  (&=_a.e. $=_a.e.)
  (&<=_a.e. $<=_a.e.)
  (&>=_a.e. $>=_a.e.)
  
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
(define (h3-cite %heading)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  (Cite "第"
        (A #:attr* `((href ,(string-append "#" id)))
           (format-section section))
        "节"))
(define (H3. #:attr* [attr* '()] #:id [id #f] . html*)
  `(,(build-%heading
      #:present h3-present #:cite h3-cite #:id id #:level 3)
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
  (Example "例子" "example")
  (Lemma "引理" "lemma")
  (Proposition "命题" "proposition")
  
  )
(define (entry name)
  (Entry name "entry"))
(define measure.html
  (TnTmPrelude
   #:title "测度论"
   #:css "styles.css"
   (H1. "不可归约的最小")
   (H2. "测度空间")
   (H3. $sigma "-代数" #:id "sigma_algebras")
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
   (((Entry (Span $sigma "-代数的基本性质") "entry") #:id "sigma-algebra-basic-properties")
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
            (Q "可数的") ". 为了我们这里的目的, "
            "对于可数性的最自然定义如下: 一个集合" $K
            "是" (B "可数的") ", 要么其为空, 要么存在一个从"
            $NN "到" $K "的满射. "
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
        (Li "练习无穷并和交的代数, 直至你能够自信地解释诸如"
            
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
        (Li "令" (&sube $E $RR^2) "是一个Borel集合. 证明" $E
            "的所有水平截线和垂直截线"
            (MB (&cm (setI $xi (∈ (tu0 $xi $eta) $E))
                     (setI $xi (∈ (tu0 $eta $xi) $E))))
            "都是" $RR "的Borel子集. "
            "(提示: 证明由所有截线均为Borel集合的" $RR^2
            "的子集构成的族是一个包含所有开集的" $RR^2
            "的子集的" $sigma "-代数.)")
        (Li "令" (&sube $G $RR) "是一个开集. 证明"
            $G "可以唯一地表示为开区间 (" $G
            "的" (Q "分量") ") 的一个可数族"
            $I:script " (可能为空) 之并, "
            "其中我们要求这个族内的诸开区间两两不相交. "
            "(提示: 对于" (∈ $x $y $G) ", 称"
            (&~ $x $y) "如果" $x "和" $y
            "之间的每个点都属于" $G ". 表明"
            $~ "是一个等价关系. 令" $I:script
            "是其等价类的集合.)")))
   (((entry "注记和评论"))
    
    )
   (H3. "测度空间")
   (P "我希望我们已经准备好迎来第二个定义了, 这个定义是此专著所有工作之基础.")
   ((Definition #:id "measure-space-definition")
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
        (Li (B $inf "的使用: ")
            "在以上定义的iii之中, 我声明" $mu
            "是一个取值于" (Q (\[\] $0 $inf))
            "之中的函数, 此即由非负实数构成的集合但又加入了"
            (Q $inf) ". 我期望你已经在分析学中遇到对于符号"
            $inf "的各种各样的运用了; 我希望你意识到"
            "这个符号在不同的上下文之中有着相当不同的意思, "
            "而每次使用都有必要建立清晰的约定. "
            (Q "测度的" $inf) "对应于无限长度或者面积或者体积的概念. "
            "我们需要于其上执行的基本操作是加法: 对于"
            (∈ $a (\[\[ $0 $inf))
            " (也就是对于每个实数" (&>= $a $0) "), "
            (&= (&+ $inf $a) (&+ $a $inf) $inf)
            ", 另外" (&= (&+ $inf $inf) $inf)
            ". 这将" (\[\] $0 $inf) "渲染为了一个加法下的半群. "
            "声明对于每个" (∈ $a $RR) "都有"
            (&= (&- $inf $a) $inf) "是相当安全的; "
            "但是我们必须绝对抵制解释公式" (&- $inf $inf)
            ". 至于乘法, 实际上对于" (&> $a $0) "而言通常将公式"
            (&d* $inf $inf) ", " (&d* $a $inf) ", "
            (&d* $inf $a) "都解释为" $inf
            "是正确的, 而一般来说" (&= (&d* $0 $inf) (&d* $inf $0))
            "可以取为" $0 "." (Br)
            (\[\] $0 $inf) "上我们也有一个自然的全序, 对于每个"
            (∈ $a (\[\[ $a $inf)) "记" (&< $a $inf)
            ". 这给出了" (\[\] $0 $inf)
            "的任意(非空)子集的上确界和下确界的想法; "
            "并且将" (infimum $empty) "解释为" $inf
            "常常是正确的, 但是每次相关情况时我都将尽量"
            "提示读者以这条特别的约定. 我们也有极限的概念; 如果"
            (Seq $u) "是" (\[\] $0 $inf) "中的一个序列, "
            "那么其收敛至" (∈ $u (\[\] $0 $inf)) ", 如果"
            (Ul (Li "对于每个" (&< $v $u) ", 存在一个"
                    (∈ $n_0 $NN) "使得对于每个"
                    (&>= $n $n_0) "都有" (&<= $v $u_n) ";")
                (Li "对于每个" (&> $v $u) ", 存在一个"
                    (∈ $n_0 $NN) "使得对于每个"
                    (&>= $n $n_0) "都有" (&>= $v $u_n) "."))
            "当然, 如果" (&= $u $0) "或者" (&= $u $inf)
            ", 其中之一的条件将虚空地成立.")
        (Li "我应该平淡地言称何谓" (Q "不相交")
            "序列: 一个序列" Seq:E "是" (B "不相交的 (disjoint)")
            ", 如果没有点属于超过一个" $E_n
            ", 即对于所有不同的" (∈ $m $n $NN)
            "有" (&= (&cap $E_m $E_n) $empty)
            ". 注意这里我们并没有阻止" $E_n
            "中的一个或者多个为空集." (Br)
            "类似地, 如果" (Seq $E $i $I)
            "是由一个任意的集合" $I
            "所索引的集合族, 那么其为" (B "不相交的")
            ", 如果对于所有不同的" (∈ $i $j $I)
            "有" (&= (&cap $E_i $E_j) $empty) ".")
        (Li "在解释以上定义的iii的" $beta
            "部分时, 我们需要对于" (\[\] $0 $inf)
            "中的任意序列" (Seq $u) "为和"
            (sum (&= $n $0) $inf $u_n)
            "赋值. 自然的方法是言称"
            (&= (sum (&= $n $0) $inf $u_n)
                (lim $n $inf
                     (sum (&= $m $0) $n $u_m)))
            ", 这也要用到a里所草绘之定义. 如果"
            $u_m "的其中一个本身就是无限的, 例如"
            (&= $u_k $inf) ", 那么对于每个" (&>= $n $k)
            "都有" (&= (sum (&= $m $0) $n $u_m) $inf)
            ", 故当然" (&= (sum (&= $n $0) $inf $u_n) $inf)
            ". 如果所有的" $u_m "都是有限的, "
            "鉴于它们都是非负的, 部分和序列"
            (_ (ang0 (sum (&= $m $0) $n $u_m)) (∈ $n $NN))
            "是单调非降的, 要么其具有一个有限的极限"
            (∈ (sum (&= $n $0) $inf $u_n) $RR)
            ", 要么其发散至" $inf
            "; 在后一种情况下我们又一次将"
            (sum (&= $n $0) $inf $u_n)
            "解释为了" $inf ".")
        (Li "又一次, 测度空间的重要例子要等到之后的"
            (Ref "lebesgue_on_RR") "和" (Ref "lebesgue_on_RR^r")
            ". 然而, 现在我就可以描述特定的一类测度空间, 其总应该铭记在心, "
            "尽管它没有为这个主题最重要也最有趣的部分提供良好的图景. 令"
            $X "是任意的集合, 并且令" (func $h $X (\[\] $0 $inf))
            "是任意的函数. 对于每个" (&sube $E $X) "我们记"
            (&= (ap $mu $E) (sum (∈ $x $E) (app $h $x)))
            ". 为了解释这个和, 首先注意到对于有限的集合" $E
            "而言这是没有困难的 (令"
            (&= (sum (∈ $x $empty) (app $h $x)) $0)
            "), 而对于无限的集合" $E ", 我们可以取"
            (&= (sum (∈ $x $E) (app $h $x))
                (supremum
                 (setI (sum (∈ $x $I) (app $h $x))
                       (: (&sube $I $E) "有限"))))
            ", 因为每个" (app $h $x) "都是非负的. "
            "(你第一次可能更倾向于思考" (&= $X $NN)
            "的情况, 那么"
            (&= (sum (∈ $n $E) (app $h $n))
                (lim $n $inf
                     (sum (&cm (∈ $m $E) (&<= $m $n))
                          (app $h $m))))
            "; 但是我希望通过些许思考你会发现一般情形, 即便"
            $X "甚至可以不可数, 实际上也并不更加困难.) 现在"
            (tu0 $X (&power $X) $mu) "是一个测度空间." (Br)
            "我们离准备好用于描述各种不同的测度空间的专门语汇还非常遥远, "
            "但是到时候我将会称这种测度为"
            (B "点支撑的 (point-supported)") "." (Br)
            "两种特定情形经常出现, 以至于值得赋予名字. 如果对于每个"
            (∈ $x $X) "有" (&= (app $h $x) $1) ", 那么" (ap $mu $E)
            "在" $E "有限的情况下不过就是" $E "的点的数目, 而若"
            $E "无限" (ap $mu $E) "则为" $inf
            ". 我们称其为" $X "上的" (B "计数测度")
            ". 如果" (∈ $x_0 $X) ", 那么我们可以置"
            (&= (app $h $x_0) $1) "而对于" (∈ $x (&\\ $X (setE $x_0)))
            "则令" (&= (app $h $x) $0) "; 然后就有若"
            (∈ $x_0 $E) "则" (&= (app $mu $E) $1)
            ", 否则就为" $0 ". 我们称其为"
            (B "集于" $x_0 "处的" $X "上的Dirac测度")
            ". 再举一个简单例子, 令" (&= $X $NN) "而"
            (&= (app $h $n) (^ $2 (&- (@+ $n $1))))
            "; 然后有"
            (&= (ap $mu $X) (&+ 1/2 1/4 $..h) $1) ".")
        (Li "如果" (tu0 $X Σ $mu) "是一个测度空间, 那么"
            Σ "是" $mu "的定义域而" $X "是" Σ
            "的最大成员. 因此, 我们可以通过其最后的分量"
            $mu "来还原整个三元组" (tu0 $X Σ $mu)
            ". 这个阶段还不值得玩这种游戏. 然而, "
            "有时引入一个测度而不立即为其定义域取名也是很方便的, "
            "而当我这么做了的时候我或许会说"
            (Q $mu "测度了" $E) "或者"
            (Q $E "由" $mu "所测度")
            "来表明" (ap $mu $E) "有定义, 即"
            $E "属于" $sigma "-代数" (&dom $mu)
            ". " (B "警告!") " 许多作者使用术语"
            (Q $mu "-可测集合")
            "来指代和我这里所正在讨论的有些许不同的东西.")))
   (((entry "测度空间的基本性质") #:id "measure-space-basic-properties")
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
        (Li "置" (&= $E_0 $E) ", " (&= $E_1 $F)
            ", 而" (&>= $n $2) "时" (&= $E_n $empty)
            "; 那么" Seq:E "是" Σ "中的一个不相交序列, 而"
            (&= (Union (∈ $n $NN) $E_n) (&union $E $F))
            ", 于是"
            (MB (&= (app $mu (&union $E $F))
                    (sum (&= $n $0) $inf
                         (ap $mu $E_n))
                    (&+ (ap $mu $E) (ap $mu $F))))
            "鉴于" (&= (ap $mu $empty) $0) ".")
        (Li "鉴于" (∈ (&\\ $F $E) Σ) " (见"
            (Ref "sigma-algebra-basic-properties")
            "的c) 和" (&>= (app $mu (&\\ $F $E)) $0)
            " (因为" $mu "的值一定在" (\[\] $0 $inf)
            "之中), 我们有 (使用a)"
            (MB (&>= (&= (ap $mu $F)
                         (&+ (ap $mu $E)
                             (app $mu (&\\ $F $E))))
                     (ap $mu $E)) "."))
        (Li "根据a得到"
            (&= (app $mu (&union $E $F))
                (&+ (ap $mu $E)
                    (app $mu (&\\ $F $E))))
            ", 根据b得到"
            (&<= (app $mu (&\\ $F $E))
                 (ap $mu $F)) ".")
        (Li "置" (&= $F_0 $E_0) ", 而"
            (&>= $n $1) "时"
            (&= $F_n (&\\ $E_n (Union (&< $i $n) $E_i)))
            "; 那么" (Seq $F) "是" Σ
            "中的一个不相交序列, 并且有"
            (&= (Union (∈ $n $NN) $F_n)
                (Union (∈ $n $NN) $E_n))
            "和对于每个" $n ", " (&sube $F_n $E_n)
            ". 根据以上的b, 对于每个" $n "有"
            (&<= (ap $mu $F_n) (ap $mu $E_n))
            "; 于是"
            (MB (&<= (&= (app $mu (Union (∈ $n $NN) $E_n))
                         (app $mu (Union (∈ $n $NN) $F_n))
                         (sum (&= $n $0) $inf
                              (ap $mu $F_n)))
                     (sum (&= $n $0) $inf
                          (ap $mu $E_n))) "."))
        (Li "置" (&= $F_0 $E_0) ", 而" (&>= $n $1) "时"
            (&= $F_n (&\\ $E_n (_ $E (&- $n $1))))
            "; 那么" (Seq $F) "是" Σ "中的不相交序列, 并且"
            (&= (Union (∈ $n $NN) $F_n)
                (Union (∈ $n $NN) $E_n))
            ". 因此, "
            (&= (app $mu (Union (∈ $n $NN) $E_n))
                (sum (&= $n $0) $inf
                     (ap $mu $F_n)))
            ". 不过, 根据" $n "上的简单归纳, "
            "其中归纳步骤要使用a, 可以证明"
            (&= (ap $mu $E_n)
                (sum (&= $m $0) $n
                     (ap $mu $F_m)))
            ", 故"
            (MB (&= (sum (&= $n $0) $inf
                         (ap $mu $F_n))
                    (lim $n $inf
                         (sum (&= $m $0) $n
                              (ap $mu $F_m)))
                    (lim $n $inf
                         (ap $mu $E_n))) ".")
            "最后, 根据b可知"
            (_ (ang0 (ap $mu $E_n)) (∈ $n $NN))
            "是非降的, 故"
            (&= (lim $n $inf (ap $mu $E_n))
                (Sup (∈ $n $NN) (ap $mu $E_n))) ".")
        (Li "设" (&< (ap $mu $E_k) $inf)
            ". 对于" (∈ $n $NN) ", 置"
            (&= $F_n (&\\ $E_k (_ $E (&+ $k $n))))
            ", 然后令" (&= $F (Union (∈ $n $NN) $F_n))
            "; 那么" (Seq $F) "是" Σ
            "中的一个非降序列, 于是根据e有"
            (&= (ap $mu $F)
                (lim $n $inf (ap $mu $F_n)))
            ". 另外, "
            (&= (&+ (ap $mu $F_n)
                    (ap $mu (_ $E (&+ $k $n))))
                (ap $mu $E_k))
            "; 鉴于" (&< (ap $mu $E_k) $inf)
            ", 我们可以安全地写下"
            (&= (ap $mu $F_n)
                (&- (ap $mu $E_k)
                    (ap $mu (_ $E (&+ $k $n)))))
            ", 于是"
            (MB (&= (ap $mu $F)
                    (lim $n $inf
                         (@- (ap $mu $E_k)
                             (ap $mu (_ $E (&+ $k $n)))))
                    (&- (ap $mu $E_k)
                        (lim $n $inf (ap $mu $E_n)))))
            "接着, 因为" (&sube $F $E_k) ", 故"
            (&= (&+ (ap $mu $F)
                    (app $mu (&\\ $E_k $F)))
                (ap $mu $E_k))
            ", 于是 (又因为" (ap $mu $E_k) "有限) "
            (&= (ap $mu $F)
                (&- (ap $mu $E_k)
                    (app $mu (&\\ $E_k $F))))
            ". 因此, 我们必然有"
            (&= (app $mu (&\\ $E_k $F))
                (lim $n $inf (ap $mu $E_n)))
            ". 但是, " (&\\ $E_k $F) "正是"
            (Cap (∈ $n $NN) $E_n) "." (Br)
            "最后, 因为"
            (_ (ang0 (ap $mu $E_n)) (∈ $n $NN))
            "是非升的, 所以有"
            (&= (lim $n $inf (ap $mu $E_n))
                (Inf (∈ $n $NN) (ap $mu $E_n))) ".")))
   ((Remark #:auto? #f)
    "观察到上述的f中条件" (Q "存在某个" (ap $mu $E_n) "有限")
    "是本质性的. {译注: 原文似乎有笔误, 已修正.} "
    
    )
   (((entry "可忽略集合"))
    "令" (tu0 $X Σ $mu) "是任意的测度空间."
    (Ol #:attr* '((type "a"))
        (Li "一个集合" (&sube $A $X) "是"
            (B "可忽略的") " (或者说" (B "null")
            "), 如果存在一个集合" (∈ $E Σ)
            "满足" (&sube $A $E) "且"
            (&= (ap $mu $E) $0)
            ". (若是对于涉及的是哪一个测度存疑, "
            "我会写下" (B $mu "-可忽略的") ".)")
        (Li "令" $N:script "是" $X
            "的可忽略子集的族, 那么i. "
            (∈ $empty $N:script)
            ". ii. 如果" (&sube $A (∈ $B $N:script))
            ", 那么" (∈ $A $N:script)
            ". iii. 如果" (Seq $A) "是"
            $N:script "中的任意序列, 那么"
            (∈ (Union (∈ $n $NN) $A_n) $N:script)
            ". " $P:bold-script
            " i. " (&= (ap $mu $empty) $0)
            ". ii. 存在一个" (∈ $E Σ) "满足"
            (&= (ap $mu $E) $0) "且"
            (&sube $B $E) "; 现在" (&sube $A $E)
            ". iii. 对于每个" (∈ $n $NN)
            ", 选择一个" (∈ $E_n Σ) "满足"
            (&sube $A_n $E_n) "且"
            (&= (ap $mu $E_n) $0) ". 现在"
            (&= $E (∈ (Union (∈ $n $NN) $E_n) Σ))
            "而"
            (&sube (Union (∈ $n $NN) $A_n)
                   (Union (∈ $n $NN) $E_n))
            ", 并且根据"
            (Ref "measure-space-basic-properties")
            "的d有"
            (&<= (app $mu (Union (∈ $n $NN) $E_n))
                 (sum (&= $n $0) $inf (ap $mu $E_n)))
            ", 故"
            (&= (app $mu (Union (∈ $n $NN) $E_n)) $0)
            ". " $Q:bold-script (Br)
            "我将称" $N:script "为" $mu "的"
            (B "零理想 (null ideal)")
            ". (满足这里条件i-iii的一个集族被称为一个集合的"
            (B $sigma "-理想") ".)")
        (Li "一个集合" (&sube $A $X) "是" (B "余可忽略")
            "的, 如果" (&\\ $X $A) "是可忽略的; "
            "换言之, 存在一个可测集合" (&sube $E $A)
            "使得" (&= (app $mu (&\\ $X $E)) $0)
            ". 注意到i. " $X "是余可忽略的. ii. 如果"
            (&sube $A $B $X) "而" $A "是余可忽略的, 那么"
            $B "是余可忽略的. iii. 如果" (Seq $A)
            "是余可忽略集合的序列, 那么"
            (Cap (∈ $n $NN) $A_n) "是余可忽略的.")
        (Li "使用一些和可忽略集合相关的非形式化语言是方便的, "
            "而且这也是一种惯例. 如果" (app $P $x)
            "是某个可以应用于集合" $X "的元素" $x
            "的断言, 我们称"
            (MB "对于几乎每个" (∈ $x $X) "都有" (app $P $x))
            "或者"
            (MB (app $P $x) "&nbsp;a.e.&nbsp;" (@ $x))
            "或者"
            (MB $P "几乎处处(成立)")
            "或者"
            (MB $P "&nbsp;a.e.")
            "或者, 若有必要刻画所牵涉的测度, 那么"
            (MB "对于" $mu "-几乎每个" $x "都有" (app $P $x))
            "或者"
            (MB (app $P $x) "&nbsp;" $mu "-a.e." (@ $x))
            "或者"
            (MB $P "&nbsp;" $mu "-a.e.")
            "以陈述"
            (setI $x (&cm (∈ $x $X) (app $P $x)))
            "在" $X "中是余可忽略的这一事实. 换言之, 即"
            (setI $x (&cm (∈ $x $X)
                          (: (app $P $x) "为假")))
            "是可忽略的. 例如, 若" (func $f $X $RR)
            "是一个函数, 那么" (Q (&> $f $0) " a.e.")
            "的含义是" (setI $x (&<= (app $f $x) $0))
            "是可忽略的.")
        (Li ""
            )
        (Li "我应该提请你注意到这一事实, 根据我们的定义, "
            "一个可忽略集合自身不必是可测的, "
            "尽管其必然囊括于某个可测的可忽略集合之中. "
            "(如果一个测度空间的所有可忽略集合都是可测的, "
            "那么其就被称为" (B "完备的")
            ". 我们将于" (Ref "taxnomy-definitions")
            "回到这个问题上来.)")
        (Li "当" $f "和" $g "是定义在某个测度空间的余可忽略子集上的实值函数时, "
            "我们分别记" (&=_a.e. $f $g) ", " (&<=_a.e. $f $g) ", "
            (&>=_a.e. $f $g) "来表示"
            (MB (&= $f $g) "&nbsp;a.e., 即"
                (setI $x
                      (&cm (∈ $x (&cap (&dom $f) (&dom $g)))
                           (&= (app $f $x) (app $g $x))))
                "是余可忽略的")
            (MB (&<= $f $g) "&nbsp;a.e., 即"
                (setI $x
                      (&cm (∈ $x (&cap (&dom $f) (&dom $g)))
                           (&<= (app $f $x) (app $g $x))))
                "是余可忽略的")
            (MB (&>= $f $g) "&nbsp;a.e., 即"
                (setI $x
                      (&cm (∈ $x (&cap (&dom $f) (&dom $g)))
                           (&>= (app $f $x) (app $g $x))))
                "是余可忽略的"))))
   (((entry "基本练习"))
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (((entry "深入练习"))
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (((entry "注记和评论"))
    
    )
   (H3. "外测度和Carathéodory构造")
   (P "这里我将引入构造测度的最重要方法.")
   ((Definition #:id "outer-measure-definition")
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
   ((Remark #:id "outer-measure-remarks")
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        (Li "和之前一样的是, 最重要的外测度需要等到"
            (Ref "lebesgue_on_RR") "和"
            (Ref "lebesgue_on_RR^r")
            ". 一个集合" $A "的" (Q "外")
            "测度的理念在于其应该是" $A
            "的可能的测度的某种上界. "
            "如果我们足够幸运, 或许外测度实际上就是"
            $A "的测度; 但是这一般只是对于拥有足够光滑的边界的集合成立.")
        (Li "将定义的i和iii连在一起看, 我们会发现如果" $theta
            "是" $X "上的一个外测度而" (&cm $A $B)
            "是" $X "的两个子集, 那么"
            (&<= (app $theta (&union $A $B))
                 (&+ (ap $theta $A) (ap $theta $B)))
            "; 请将其与" (Ref "measure-space-basic-properties")
            "的a和c进行比较.")))
   (((entry "Carathéodory的方法: 定理"))
    "令" $X "是一个集合而" $theta "是" $X "上的一个外测度, 置"
    (MB (&= Σ (setI $E
                    (: (&sube $E $X)
                       ", 对于每个" (&sube $A $X)
                       ", 都有"
                       (&= (ap $theta $A)
                           (&+ (app $theta (&cap $A $E))
                               (app $theta (&\\ $A $E))))))))
    "那么" Σ "是" $X "的子集的一个" $sigma "-代数. "
    "通过对于" (∈ $E Σ) "记" (&= (ap $mu $E) (ap $theta $E))
    "定义" (func $mu Σ (\[\] $0 $inf)) "; 那么"
    (tu0 $X Σ $mu) "是一个测度空间.")
   ((proof)
    (Ol #:attr* '((type "a"))
        (Li "第一步是注意到对于任何" (&sube (&cm $E $A) $X)
            ", 根据" (Ref "outer-measure-remarks")
            "的c, 我们有"
            (&>= (&+ (app $theta (&cap $A $E))
                     (app $theta (&\\ $A $E)))
                 (ap $theta $A))
            "; 于是"
            (MB (&= Σ
                    (setI $E
                          (: (&sube $E $X)
                             ", 对于每个" (&sube $A $X)
                             ", 都有"
                             (&>= (ap $theta $A)
                                  (&+ (app $theta (&cap $A $E))
                                      (app $theta (&\\ $A $E)))))))))
        (Li "显然" (∈ $empty Σ) ", 因为"
            (MB (&= (&+ (app $theta (&cap $A $empty))
                        (app $theta (&\\ $A $empty)))
                    (&+ (ap $theta $empty)
                        (ap $theta $A))
                    (ap $theta $A)))
            "对于每个" (&sube $A $X) "都成立. 如果"
            (∈ $E Σ) ", 那么" (∈ (&\\ $X $E) Σ)
            ", 因为"
            (MB (&= (&+ (app $theta (&cap $A (@\\ $X $E)))
                        (app $theta (&\\ $A (@\\ $X $E))))
                    (&+ (app $theta (&\\ $A $E))
                        (app $theta (&cap $A $E)))
                    (ap $theta $A)))
            "对于每个" (&sube $A $X) "都成立.")
        (Li "现在设" (∈ $E $F Σ) "而" (&sube $A $X) ", 那么"
            todo.svg
            (MB (deriv
                 (&+ (app $theta (&cap $A (@union $E $F)))
                     (app $theta (&\\ $A (@union $E $F))))
                 (&+ (app $theta (&cap (@cap $A (@union $E $F))
                                       $E))
                     (app $theta (&\\ (@cap $A (@union $E $F))
                                      $E))
                     (app $theta (&\\ $A (@union $E $F))))
                 (&+ (app $theta (&cap $A $E))
                     (app $theta (&cap (@\\ $A $E) $F))
                     (app $theta (&\\ (@\\ $A $E) $F)))
                 (&+ (app $theta (&cap $A $E))
                     (app $theta (&\\ $A $E)))
                 (ap $theta $A)))
            "鉴于" $A "是任意的, 故" (∈ (&union $E $F) Σ) ".")
        (Li "因此, " Σ "在简单并和补下封闭, 并且包含" $empty
            ". 现在设" (Seq $E) "是" Σ "中的一个序列, 并且"
            (&= $E (Union (∈ $n $NN) $E_n)) ", 置"
            (MB (&= $G_n (Union (&<= $m $n) $E_m)) ";")
            "那么根据" $n "上的归纳, 对于每个" (∈ $n $NN)
            "有" (∈ $G_n Σ) ". 置"
            (MB (&cm (&= $F_0 $G_0 $E_0)
                     (&= $F_n (&\\ $G_n (_ $G (&- $n $1)))
                         (&\\ $E_n (_ $G (&- $n $1)))))
                "&nbsp;for&nbsp;" (&>= $n $1) ";")
            "那么"
            (&= $E (Union (∈ $n $NN) $F_n)
                (Union (∈ $n $NN) $G_n))
            "." (Br)
            "取任意的" (&>= $n $1) "和任意的"
            (&sube $A $X) ", 那么"
            (MB (deriv
                 (app $theta (&cap $A $G_n))
                 (&+ (app $theta (&cap $A $G_n (_ $G (&- $n $1))))
                     (app $theta (&\\ (&cap $A $G_n) (_ $G (&- $n $1)))))
                 (&+ (app $theta (&cap $A (_ $G (&- $n $1))))
                     (app $theta (&cap $A $F_n)))))
            "在" $n "上施行归纳可以表明对于每个" (&>= $n $0) "都有"
            (MB (&= (app $theta (&cap $A $G_n))
                    (sum (&= $m $0) $n
                         (app $theta (&cap $A $F_m)))) ".")
            "设" (&sube $A $X) ", 那么"
            (&= (&cap $A $E)
                (Union (∈ $n $NN) (&cap $A $F_n)))
            ", 于是"
            (MB (deriv0
                 (app $theta (&cap $A $E))
                 $<=
                 (sum (&= $n $0) $inf
                      (app $theta (&cap $A $F_n)))
                 $=
                 (lim $n $inf
                      (sum (&= $m $0) $n
                           (app $theta (&cap $A $F_m))))
                 $=
                 (lim $n $inf
                      (app $theta (&cap $A $G_n)))))
            "另一方面, 我们有"
            (MB (deriv0
                 (app $theta (&\\ $A $E))
                 $=
                 (app $theta (&\\ $A (Union (∈ $n $NN) $G_n)))
                 $<=
                 (Inf (∈ $n $NN) (app $theta (&\\ $A $G_n)))
                 $=
                 (lim $n $inf (app $theta (&\\ $A $G_n)))))
            "这里我们用到了" (Ref "outer-measure-definition")
            "的ii以看出"
            (_ (ang0 (app $theta (&\\ $A $G_n)))
               (∈ $n $NN))
            "是一个非升序列, 以及对于每个" $n "有"
            (MB (&<= (app $theta (&\\ $A $E))
                     (app $theta (&\\ $A $G_n))) ".")
            "由上述结果我们可以得到"
            (MB (deriv0
                 (&+ (app $theta (&cap $A $E))
                     (app $theta (&\\ $A $E)))
                 $<=
                 (&+ (lim $n $inf
                          (app $theta (&cap $A $G_n)))
                     (lim $n $inf
                          (app $theta (&\\ $A $G_n))))
                 $=
                 (lim $n $inf
                      (@+ (app $theta (&cap $A $G_n))
                          (app $theta (&\\ $A $G_n))))
                 $=
                 (lim $n $inf (ap $theta $A))
                 $=
                 (ap $theta $A)))
            "这里用到了对于每个" $n "都有" (∈ $G_n Σ)
            ", 故对于每个" $n "都有"
            (&= (&+ (app $theta (&cap $A $G_n))
                    (app $theta (&\\ $A $G_n)))
                (ap $theta $A))
            ". 鉴于" $A "是任意的, 根据以上的a, 我们知道"
            (∈ $E Σ) "." (Br)
            "由于" Seq:E "是任意的, 所以说"
            (Ref "sigma-algebra")
            "的iii得以满足. 综合以上, " Σ
            "是" $X "的子集的一个" $sigma "-代数.")
        (Li "现在让我们将注意力转到" $mu
            ", 其为" $theta "于" Σ
            "上的限制. 现在我们根据测度空间的定义来检视"
            $mu "是否合乎要求. 首先, 当然有"
            (&= (ap $mu $empty) (ap $theta $empty) $0)
            ". 其次, 令" Seq:E "是" Σ
            "中不相交的序列. 置"
            (&= $G_n (Union (&<= $m $n) $E_n))
            ", 这和d是一样的, 以及令"
            (MB (&= $E
                    (Union (∈ $n $NN) $E_n)
                    (Union (∈ $n $NN) $G_n)) ".")
            "基于和d类似的推理, 我们有"
            (MB (deriv
                 (ap $mu $G_n+1)
                 (ap $theta $G_n+1)
                 (&+ (app $theta (&cap $G_n+1 $E_n+1))
                     (app $theta (&\\ $G_n+1 $E_n+1)))
                 (&+ (ap $theta $E_n+1)
                     (ap $theta $G_n))
                 (&+ (ap $mu $E_n+1)
                     (ap $mu $G_n))))
            "根据归纳, 可知对于每个" $n "有"
            (MB (&= (ap $mu $G_n)
                    (sum (&= $m $0) $n
                         (ap $mu $E_m))) ".")
            "既然"
            (MB (ap $mu $E) $=
                (ap $theta $E) $<=
                (sum (&= $n $0) $inf (ap $theta $E_n)) $=
                (sum (&= $n $0) $inf (ap $mu $E_n)))
            "又有对于每个" $n "可以推出"
            (MB (ap $mu $E) $=
                (ap $theta $E) $>=
                (ap $theta $G_n) $=
                (ap $mu $G_n) $=
                (sum (&= $m $0) $n
                     (ap $mu $E_m)))
            "那么"
            (MB (&>= (ap $mu $E)
                     (sum (&= $n $0) $inf
                          (ap $mu $E_n))))
            "最终可以得到"
            (MB (&= (ap $mu $E)
                    (sum (&= $n $0) $inf
                         (ap $mu $E_n))))
            "鉴于" Seq:E "是任意的, 所以说"
            (Ref "measure-space-definition")
            "的iii的" $beta "得以满足, 故"
            (tu0 $X Σ $mu) "是一个测度空间.")))
   ((Remark)
    
    )
   (((entry "基本练习"))
    
    )
   (((entry "深入练习"))
    
    )
   (((entry "注记和评论"))
    
    )
   (H3. $RR "上的Lebesgue测度" #:id "lebesgue_on_RR")
   ((Definition)
    (Ol #:attr* '((type "a"))
        (Li "对于本节的目的而言, " $RR "中的一个"
            (B "半开区间") "是具有形式"
            (&= (\[\[ $a $b)
                (setI $x (: $a $<= $x $< $b)))
            "的一个集合, 其中" (∈ $a $b $RR)
            ". 注意到这个公式里我允许"
            (&<= $b $a) ", 此时"
            (&= (\[\[ $a $b) $empty) ".")
        (Li "如果" (&sube $I $RR) "是一个半开区间, 那么要么"
            (&= $I $empty) ", 要么"
            (&= $I (\[\[ (infimum $I) (supremum $I)))
            ", 于是其端点是良定义的. 因此, 我们定义半开区间"
            $I "的" (B "长度") (len $I) "为"
            (MB (&cm (&= (len $empty) $0)
                     (: "若" (&< $a $b) "则"
                        (&= (len (\[\[ $a $b))
                            (&- $b $a)))) "."))))
   ((Lemma)
    "如果" (&sube $I $RR) "是一个半开区间, " (Seq $I $j $NN)
    "是一个能够覆盖" $I "的半开区间的序列, 那么"
    (MB (&<= (len $I)
             (sum (&= $j $0) $inf
                  (len $I_j))) "."))
   ((proof)
    (Ol #:attr* '((type "a"))
        (Li "如果" (&= $I $empty) ", 那么当然有"
            (&<= (&= (len $I) $0)
                 (sum (&= $j $0) $inf
                      (len $I_j)))
            ". 不然的话, 取" (&= $I (\[\[ $a $b))
            ", 其中" (&< $a $b) ". "
            )
        )
    )
   ((Definition)
    "从现在开始, 到本节末为止, 定义"
    (func $theta (&power $RR) (\[\] $0 $inf))
    "为"
    (MB (&= (app $theta $A)
            (infimum
             (setI (sum (&= $j $0) $inf (len $I_j))
                   (: (Seq $I $j $NN)
                      "是一个半开区间的序列, 其满足"
                      (&sube $A (Union (∈ $j $NN) $I_j)))))) ".")
    "注意到每个" $A "都可以由某个半开区间序列所覆盖, 例如"
    (&sube $A (Union (∈ $n $NN) (\[\[ (&- $n) $n)))
    "; 因此, 如果"
    )
   ((Proposition)
    (Ol #:attr* '((type "a"))
        (Li $theta "是" $RR "上的一个外测度.")
        (Li "对于每个半开区间" (&sube $I $RR)
            ", " (&= (app $theta $I) (len $I)) ".")))
   ((proof)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   ((Remark #:auto? #f)
    
    )
   ((Definition)
    "鉴于Lebesgue外测度的确是一个外测度, 我们可以使用它来构造一个测度"
    $mu ", 通过Carathéodory方法. 这个测度即" (B $RR "上的Lebesgue测度")
    ". " $mu "所能测度的集合" $E "被称为是" (B "Lebesgue可测的")
    ", 也就是说, 对于每个" (&sube $A $RR) "有"
    (&= (&+ (app $theta (&cap $A $E))
            (app $theta (&\\ $A $E)))
        (app $theta $A))
    ". 对于" $mu "而言的可忽略集合被称为是" (B "Lebesgue可忽略的")
    "; "
    )
   ((Lemma)
    "令" (∈ $x $RR) ", 那么"
    (&= $H_x (\]\[ (&- $inf) $x))
    "是Lebesgue可测的.")
   ((proof)
    
    )
   (H3. $RR^r "上的Lebesgue测度" #:id "lebesgue_on_RR^r")
   
   (H2. "积分")
   (P "如果你沿着你的大学图书馆的特定书架观察, 你会发现" (Q "测度")
      "和" (Q "积分") "就像连体双胞胎一样总是一起出现. "
      
      )
   (H3. "可测函数")
   (P "本节我将退后一步以建立和集合的" $sigma "-代数相关的想法, 沿着"
      (Ref "sigma_algebras") "; 除了练习, 这里将不会提及" (Q "测度")
      ". 我们的目的在于建立" (Q "可测函数") "的概念以及各种相关技术. "
      
      )
   ((Lemma)
    "令" $X "是一个集合而" Σ "是一个" $X "的子集的"
    $sigma "-代数. 令" $D "是" $X "的任意子集而记"
    (MB (&= Σ_D (setI (&cap $E $D) (∈ $E Σ))) ".")
    "那么" Σ_D "是一个" $D "的子集的" $sigma "-代数.")
   ((proof)
    (Ol #:attr* '((type "i"))
        (Li (∈ (&= $empty (&cap $empty $D)) Σ_D)
            ", 鉴于" (∈ $empty Σ) ".")
        (Li "如果" (∈ $F Σ_D) ", 那么存在一个" (∈ $E Σ)
            "满足" (&= $F (&cap $E $D)) "; 现在"
            
            )
        )
    )
   (H3. "积分的定义")
   
   (H3. "收敛定理")
   
   (H2. "补充内容")
   
   (H3. "可测子空间")
   
   (H3. "由测度构造外测度")
   
   (H3. "更宽泛的积分概念")
   
   (H3. "更多关于Lebesgue测度的内容")
   
   (H3. "扩展实轴")
   
   (H3. "单调类定理")
   
   (H2. "附录")
   (H3. "集合论")
   (H3. $RR^r "中的开集和闭集")
   (H1. "广阔的基础")
   (H2. "测度空间的分类学")
   (H3. "定义" #:id "taxnomy-definitions")
   (H3. "完备空间")
   (H3. "半无限空间, 局部确定空间和可局部化空间")
   (H3. "子空间")
   
   (H1. "测度代数")
   (H2. "Boole代数")
   (H3. "Boole代数")
   ((Definition)
    (Ol #:attr* '((type "a"))
        (Li "一个" (B "Boole环") "是一个环"
            (tu0 $A:fraktur $+:id $.*:id)
            ", 其中对于每个" (∈ $a $A:fraktur)
            "有" (&= $a^2 $a) ".")
        (Li "一个" (B "Boole代数") "是一个Boole环, "
            "其带有一个乘法恒元" (&= $1 (_ $1 $A:fraktur))
            "; 这个上下文里我们允许" (&= $1 $0) ".")))
   ((Remark #:auto? #f)
    
    )
   ((Example)
    (Ol #:attr* '((type "a"))
        (Li "对于任意的集合" $X ", " (tu0 (&power $X) Δ $cap:id)
            "是一个Boole代数; 其零元是" $empty "而乘法恒元是" $X
            ". " $P:bold-script " 我们需要检验以下事实, "
            "它们都是很容易建立的, 可以使用Venn图或者其他手段:"
            
            )
        (Li "回忆一下, 一个" (Q $X "的子集的代数")
            
            )
        )
    )
   ((Proposition)
    "令" $A:fraktur "是一个Boole环."
    (Ol #:attr* '((type "a"))
        (Li "对于每个" (∈ $a $A:fraktur) ", "
            (&= (&+ $a $a) $0) ", 即"
            (&= $a (&- $a)) ".")
        (Li "对于所有的" (∈ $a $b $A:fraktur)
            ", " (&= (&i* $a $b) (&i* $b $a))
            ". {译注: 即Boole环一定是交换环.}")))
   ((proof)
    (Ol #:attr* '((type "a"))
        (Li "如果" (∈ $a $A:fraktur) ", 那么"
            (MB (&= (&+ $a $a)
                    (&i* (@+ $a $a) (@+ $a $a))
                    (&+ $a^2 $a^2 $a^2 $a^2)
                    (&+ $a $a $a $a)))
            "于是我们必然有" (&= $0 (&+ $a $a)) ".")
        (Li "现在对于任意的" (∈ $a $b $A:fraktur)
            ", 我们有"
            (MB (&= (&+ $a $b)
                    (&i* (@+ $a $b) (@+ $a $b))
                    (&+ $a^2 (&i* $a $b) (&i* $b $a) $b^2)
                    (&+ $a (&i* $a $b) (&i* $b $a) $b)))
            "于是"
            (MB (&= $0 (&+ (&i* $a $b) (&i* $b $a))
                    (&+ (&i* $a $b) (&i* $a $b))))
            "那么" (&= (&i* $a $b) (&i* $b $a)) ".")))
   ((Lemma)
    "令" $A:fraktur "是一个Boole环, " $I
    "是" $A:fraktur "的一个理想, 以及"
    (∈ $a (&\\ $A:fraktur $I))
    ", 那么存在一个环同态"
    
    )
   (((entry "M.H.Stone的定理: 第一形式"))
    
    )
   ((Remark)
    
    )
   (((entry (Span "一个Boole环上的" $union ", " $\\ ", " $s- "运算")))
    
    )
   (((entry "一个Boole环上的序结构"))
    
    )
   (((entry "一个Stone空间的拓扑: 定理"))
    
    )
   ((Proposition)
    
    )
   (H3. "同态")
   (H1. "拓扑测度空间")
   (H1. "集合论式测度论")
   (H1. "随机分析")
   ))