#lang racket
(provide tapl.html)
(require SMathML)
(define (format-num section index)
  (and index
       (format "~a.~a"
               (apply string-append
                      (add-between
                       (map number->string
                            (cdr (reverse section))) "."))
               index)))
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
  (Exercise "练习" "exercise")
  
  )
(define $Nat (Mi "Nat"))
(define $Top (Mi "Top"))
(define $Bool (Mi "Bool"))
(define $List (Mi "List"))
(define (&List T) (app $List T))
(define (powerset U)
  (app $P:script U))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define $. (Mo "."))
(define (∀ x P)
  (: $forall x $. P))
(define (RuleLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define S<:T (Code "S &lt;: T"))
(define -- "&mdash;&mdash;")
(define $space:2ex (&space 2))
(define (RcdType . x*)
  (apply setE (map2 &: x*)))
(define (AbsRcdType l T i n)
  (setE (^ (&: (_ l i) (_ T i))
           (∈ i (&cm $1 $..h n)))))
(define (Record . x*)
  (apply setE (map2 &= x*)))
(define (APP . x*)
  (apply : (add-between x* $space:2ex)))
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign "left center right")
                (width "100%"))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))
(define $11 (Mn "11"))
(define $12 (Mn "12"))
(define $T_11 (_ $T $11))
(define $T_12 (_ $T $12))
(define Γ $Gamma:normal)
(define $vdash (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (let ((b (car b*)))
      (: (apply &cm a*) $vdash b))))
(define (G!- . x*)
  (apply !- Γ x*))
(define $<: (Mo "&lt;:"))
(define-infix*
  (&<: $<:))
(define tapl.html
  (TnTmPrelude
   #:title "类型和编程语言"
   #:css "styles.css"
   (H1. "类型和编程语言")
   (P "这是一本历久弥新的编程语言入门教科书. "
      "这本书的特点在于深入浅出, 几乎不假设读者具有任何数学知识. "
      "另外, 这本书还使用了OCaml语言编写的程序来帮助读者理解, "
      "避免自然语言的一些可能的歧义和模糊.")
   (H2. "引论")
   (H2. "数学预备")
   (H2. "无类型算术表达式")
   (H2. "算术表达式的一个ML实现")
   (H2. "无类型lambda演算")
   (H2. "项的匿名表示")
   (H2. "lambda演算的一个ML实现")
   (H2. "类型化的算术表达式")
   (H3. "类型")
   (H3. "定型关系")
   (H3. "安全=进展+保持")
   (H2. "简单类型lambda演算")
   (H3. "函数类型")
   (H3. "定型关系")
   (H3. "定型的性质")
   (H3. "Curry-Howard对应")
   (H3. "擦除和可定型性")
   (H3. "Curry风格和Church风格的对比")
   (H2. "简单类型的一个ML实现")
   (H2. "简单扩展")
   (H2. "规范化")
   (P "这一章我们将考虑纯简单类型lambda演算的另一个基础的理论性质: "
      )
   (H3. "简单类型的规范化")
   (H3. "注记")
   (H2. "引用")
   (H2. "异常")
   (H2. "子定型")
   (P "我们已经花了数章时间在简单类型lambda演算的框架之下研究了诸多语言特性的类型行为. "
      "这一章所面对的是一种更为基本的扩展: " (Em "子定型") " (有时也被称为" (Em "子类型多态")
      "). 和我们之前所研究的特性不同的是, 前面的特性或多或少算是正交的, "
      "而子定型是一个横切的扩展, 与大多数其他语言特性以非平凡的方式进行交互. "
      "{译注: 也就是说, 加入子定型会影响语言的全局.}")
   (P "子定型总是可以作为" (Em "面向对象") "语言的特征而找到, "
      "其经常被认为是面向对象风格的一个本质特性. "
      "我们将会在第18章仔细探索这种关联, "
      "然而现在我们将会在只有函数和记录这一更为经济的场景下呈现子定型, "
      "不过最有趣的问题已然出现. 第15.5节讨论了子定型与之前我们所见过的一些其他特性的组合. "
      "在最后一节 (也就是15.6), 我们考虑了子定型的一种更为细化的语义, "
      "其中子定型的使用对应于运行时" (Em "强制") "的插入.")
   (H3. "涵摄 (subsumption)")
   (P "简单类型lambda演算没有子定型, 其规则有时可能恼人地顽固. "
      "类型系统对于参数类型和函数的定义域类型必须精确匹配的坚持"
      "将导致类型检查器会拒绝许多对于程序员而言显然良行为的程序. "
      "例如, 回忆一下函数应用的定型规则:"
      (MBL (RuleLabel "(T-App)")
           (&rule
            (G!- (&: $t_1 (&-> $T_11 $T_12)))
            (G!- (&: $t_2 $T_11))
            (G!- (&: (APP $t_1 $t_2) $T_12))))
      "根据这条规则, 良行为的项"
      (CodeB "(λr:{x:Nat}. r.x) {x=0,y=1}")
      "是不可定型的, 因为其参数的类型为"
      (Code "{x:Nat,y:Nat}")
      ". 但是显然, 这个函数只是要求其参数应该是一个带有域"
      (Code "x") "的记录. 它并不在意参数有还是没有其他的域. "
      "而且, 我们从函数的类型就足以看出来这一点" --
      "我们不需要观察其体来验证它没有使用任何" (Code "x")
      "之外的域这一事实. 将具有类型" (Code "{x:Nat,y:Nat}")
      "的参数传递给期待类型" (Code "{x:Nat}")
      "的函数" (Em "总是") "安全的.")
   (P "子定型的目的在于对定型规则进行细化, "
      "以使得类型系统可以接受如上所描述的项. "
      "我们达成所用的方式在于形式化某些类型比其他类型更有信息 "
      "(informative) 这一直觉: 我们称" (Code "S")
      "是" (Code "T") "的一个" (Em "子类型")
      ", 记作" S<:T ", 来表达任何具有类型"
      (Code "S") "的项都可以安全地用在期待具有类型"
      (Code "T") "的项的上下文之中这一事实. "
      "这种看待子定型的观念被称为"
      (Em "安全替换原则") ".")
   (P "更简单的直觉在于将" S<:T "读作"
      (Q "每个由" (Code "S") "所描述的值也可由"
         (Code "T") "所描述")
      ". 也就是说, "
      (Q (Code "S") "的元素 (复数) 是" (Code "T")
         "的元素 (复数) 的一个子集")
      ". 我们将会在第15.6节看到其他的偶尔有用的更为细化的对于子定型的解释, "
      "但是当前这种" (Em "子集语义") "对于绝大多数目的而言已经足够了.")
   (P "定型关系和子类型关系之间的桥梁是通过添加一条新的定型规则提供的, "
      "即所谓的" (Em "涵摄") "规则:"
      (MBL (RuleLabel "(T-Sub)")
           (&rule
            (G!- (&: $t $S))
            (&<: $S $T)
            (G!- (&: $t $T))))
      "这条规则告诉我们, 如果" S<:T
      ", 那么" (Code "S") "的每个元素" (Code "t")
      "也是" (Code "T") "一个元素. "
      "例如, 如果我们定义某种子类型关系使得"
      (Code "{x:Nat,y:Nat} &lt;: {x:Nat}")
      ", 那么我们就可以使用规则"
      (RuleLabel "T-Sub") "来推导出"
      (Code "⊢ {x=0,y=1} : {x:Nat}")
      ", 这是我们为了使得我们的启发性例子通过类型检查所需要的.")
   (H3. "子类型关系")
   (P "子类型关系可以形式化为一集用于推导具有形式"
      S<:T "的陈述的推理规则, " S<:T "应该读作"
      (Q (Code "S") "是" (Code "T") "的一个子类型") "或者"
      (Q (Code "T") "是" (Code "S") "的一个超类型")
      ". 我们分别考虑每种形式的类型 (函数类型, 记录类型, 等等); "
      "对于每种情况, 我们引入一或多条规则来形式化"
      "在期望某个类型的地方何时可以安全地允许使用"
      "具有这种形式的另一类型的元素.")
   (P "在讨论特定类型构造子的(子定型)规则之前, "
      "我们作出两条一般性约定:" (Br)
      "首先, 子定型规则应该是自反的, 即"
      (MBL (RuleLabel "(S-Refl)")
           (&<: $S $S))
      "其次, 它应该是传递的, 即"
      (MBL (RuleLabel "(S-Trans)")
           (&rule
            (&<: $S $U)
            (&<: $U $T)
            (&<: $S $T)))
      "这些规则可以直接根据安全替换的直觉推得.")
   (P "对于记录类型, 现在我们已经明白了我们想要考虑的是将类型"
      (Code "S = {k1:S1...km:Sm}") "作为"
      (Code "T = {l1:T1...ln:Tn")
      "的一个子类型, 如果" (Code "T")
      "相较于" (Code "S") "有着更少的域. "
      "作为一种特别情形, " (Q "忘记")
      "某个记录类型的末端的一些域是安全的. 所谓的"
      (Em "宽度子定型") "规则捕获了这种直觉:"
      (MBL (RuleLabel "(S-RcdWidth)")
           (&<: (AbsRcdType $l $T $i (&+ $n $k))
                (AbsRcdType $l $T $i $n)))
      "或许会令人惊讶的是, " (Q "更小") "的类型" --
      "即子类型" -- "反而有着" (Em "更多") "的域. "
      "理解这种现象的最简单方式在于如我们在第11.8节"
      "那样采用对待记录类型的一种更自由的观念, "
      "将一个记录类型" (Code "{x:Nat}") "描述为"
      (Q "由所有" (Em "至少") "拥有类型为"
         (Code "Nat") "的域" (Code "x")
         "的记录构成的集合")
      ". 诸如" (Code "{x=3}") "和" (Code "{x=5}")
      "这样的值是这个类型的元素, 诸如" (Code "{x=3,y=100}")
      "和" (Code "{x=3,a=true,b=true}")
      "这样的值也是这个类型的元素. 类似地, 记录类型"
      (Code "{x:Nat,y:Nat}") "描述了" (Em "至少")
      "拥有类型都为" (Code "Nat") "的域" (Code "x")
      "和" (Code "y") "的记录. 诸如"
      (Code "{x=3,y=100}") "和"
      (Code "{x=3,y=100,z=true}")
      "这样的值是这个类型的成员, 但是"
      (Code "{x=3}") "不是, "
      (Code "{x=3,a=true,b=true}")
      "也不是. 因此, 属于第二个类型的值构成的集合"
      "是属于第一个类型的值构成的集合的一个真子集. "
      "更长的记录类型构成了要求更多" -- "即更有信息"
      -- "的刻画, 故其描述了更小的值集合.")
   (P "宽度子定型规则只适用于共同的域等同的记录类型. "
      "允许单独的域的类型发生变化也是安全的, "
      "只要两个记录类型里每个相应的域的类型处于子类型关系. "
      (Em "深度子定型") "规则表达了这种直觉:"
      (MBL (RuleLabel "(S-RcdDepth)")
           (&rule
            (∀ $i (&<: $S_i $T_i))
            (&<: (AbsRcdType $l $S $i $n)
                 (AbsRcdType $l $T $i $n)))))
   (P "下列子定型推导联合使用了" (RuleLabel "S-RcdWidth")
      "和" (RuleLabel "S-RcdDepth") "来表明嵌套的记录类型"
      (Code "{x:{a:Nat,b:Nat},y:{m:Nat}}") "是"
      (Code "{x:{a:Nat},y:{}}") "的一个子类型:"
      (MB (&rull
           (RuleLabel "S-RcdDepth")
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RcdType $a $Nat $b $Nat)
                 (RcdType $a $Nat)))
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RcdType $m $Nat)
                 (RcdType)))
           (&<: (RcdType
                 $x (RcdType $a $Nat $b $Nat)
                 $y (RcdType $m $Nat))
                (RcdType
                 $x (RcdType $a $Nat)
                 $y (RcdType)))))
      "如果我们想要使用" (RuleLabel "S-RcdDepth")
      "来仅仅细化单独一个记录域 "
      "(而不是细化每个域, 如以上例子), "
      "我们可以使用" (RuleLabel "S-Refl")
      "来对于其他的域获得平凡子定型的推导."
      (MB (&rull
           (RuleLabel "S-RcdDepth")
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RcdType $a $Nat $b $Nat)
                 (RcdType $a $Nat)))
           (&rull
            (RuleLabel "S-Refl")
            (&<: (RcdType $m $Nat)
                 (RcdType $m $Nat)))
           (&<: (RcdType
                 $x (RcdType $a $Nat $b $Nat)
                 $y (RcdType $m $Nat))
                (RcdType
                 $x (RcdType $a $Nat)
                 $y (RcdType $m $Nat)))))
      "我们也可以使用传递性规则" (RuleLabel "S-Trans")
      "以将宽度子定型和深度子定型结合起来. 例如, "
      "我们可以藉由抛弃某个域时提升另一个域的类型来得到一个超类型:"
      (MB (&rull
           (RuleLabel "S-Trans")
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RcdType
                  $x (RcdType $a $Nat $b $Nat)
                  $y (RcdType $m $Nat))
                 (RcdType
                  $x (RcdType $a $Nat $b $Nat))))
           (&rull
            (RuleLabel "S-RcdDepth")
            (&rull
             (RuleLabel "S-RcdWidth")
             (&<: (RcdType $a $Nat $b $Nat)
                  (RcdType $a $Nat)))
            (&<: (RcdType
                  $x (RcdType $a $Nat $b $Nat))
                 (RcdType
                  $x (RcdType $a $Nat))))
           (&<: (RcdType
                 $x (RcdType $a $Nat $b $Nat)
                 $y (RcdType $m $Nat))
                (RcdType
                 $x (RcdType $a $Nat))))))
   (P "我们最后的记录子定型规则来源于这样的观察: "
      "记录的域的顺序对于如何安全使用记录不会造成任何影响, "
      "因为我们对于记录能" (Em "做") "的事情" --
      "即对于域进行投影" -- "对于域的顺序是不敏感的."
      (MBL (RuleLabel "(S-RcdPerm)")
           (&rule
            (: (AbsRcdType $k $S $j $n)
               "是"
               (AbsRcdType $l $T $i $n)
               "的一个置换")
            (&<: (AbsRcdType $k $S $j $n)
                 (AbsRcdType $l $T $i $n))))
      "例如, " (RuleLabel "S-RcdPerm") "告诉我们"
      (RcdType $c $Top $b $Bool $a $Nat) "是"
      (RcdType $a $Nat $b $Bool $c $Top)
      "的一个子类型, 反之亦然. (这暗示了子类型关系"
      (Em "并非") "反对称的.)")
   (P (RuleLabel "S-RcdPerm") "可以与"
      (RuleLabel "S-RcdWidth") "和"
      (RuleLabel "S-Trans")
      "进行结合以从一个记录类型的任何位置抛弃域, "
      "而不是只能抛弃末端的域.")
   ((Exercise)
    "绘制一个推导以表明" (RcdType $x $Nat $y $Nat $z $Nat)
    "是" (RcdType $y $Nat) "的一个子类型.")
   (P (RuleLabel "S-RcdWidth") ", "
      (RuleLabel "S-RcdDepth") ", "
      (RuleLabel "S-RcdPerm")
      "这三条规则每条都体现了记录在使用方面的一种不同的灵活性. "
      "出于讨论的目的, 将其呈现为三条单独的规则是有用的. "
      "具体来说, 存在着语言允许其中一些规则而不允许其他的规则; "
      "例如, Abadi和Cardelli的" (Em "对象演算")
      " (1996) 的大多数变体都省略了宽度子定型. "
      "然而, 出于实现的目的, 将这三条规则的所作所为"
      "融合成为一条大型规则是更为方便的. "
      "这条规则在下一章进行了讨论.")
   (P "既然我们在高阶语言的上下文中进行讨论, "
      "不仅是数字和记录, 函数也可以作为参数传递给其他函数, "
      "因而我们必须赋予函数类型以子定型规则" --
      "即我们必须要刻画在期望某个函数类型的上下文之中, "
      "何种情况下可以安全地使用不同类型的函数."
      (MBL (RuleLabel "(S-Arrow)")
           (&rule
            (&<: $T_1 $S_1)
            (&<: $S_2 $T_2)
            (&<: (&-> $S_1 $S_2)
                 (&-> $T_1 $T_2))))
      "注意到子类型关系对于参数类型是逆变的 (contravariant), "
      "对于结果类型是协变的 (covaraint). "
      "直觉在于, 如果我们有了一个类型为" (&-> $S_1 $S_2)
      "的函数" $f ", 那么我们知道" $f "接受类型为" $S_1
      "的元素; 显然, " $f "也接受" $S_1 "的任何子类型"
      $T_1 "的元素. " $f "的类型也告诉我们, 其会返回类型为"
      $S_2 "的元素; 我们也可以将这些结果视为属于" $S_2
      "的任何超类型" $T_2 ". 也就是说, 任何具有类型"
      (&-> $S_1 $S_2) "的函数" $f "也可以视为具有类型"
      (&-> $T_1 $T_2) ".")
   (P "另一种观念是若要在期望类型" (&-> $T_1 $T_2)
      "的上下文之中安全使用类型为" (&-> $S_1 $S_2)
      "的函数, 只需可能传递给该上下文中的函数的参数"
      "不会对函数本身造成惊吓 (故" (&<: $T_1 $S_1)
      "), 并且该函数返回的结果也不会对上下文造成惊吓 (故"
      (&<: $S_2 $T_2) ").")
   (P "最后, 拥有一个每个类型的超类型是方便的. "
      "我们引入一个新的类型常量" $Top
      ", 外加一条规则使得" $Top
      "成为子类型关系的一个最大元素."
      (MBL (RuleLabel "(S-Top)")
           (&<: $S $Top))
      "第15.4节更深入地讨论了" $Top
      "类型.")
   (P "从形式化角度而言, 子类型关系是在我们所给定的规则下封闭的最小关系. "
      
      )
   ((Exercise)
    )
   
   (H2. "子定型的元理论")
   (P "前一章的带有子定型的简单类型lambda演算并不直接适用于实现. "
      "和其他之前我们所见的演算不同的是, 这个系统的规则并非" (Em "句法导向的")
      -- "其无法" (Q "自底而上阅读") "以产生一个类型检查算法. "
      "罪魁祸首是定型关系里涵摄规则的" (RuleLabel "T-Sub")
      "和子定型关系里的传递性" (RuleLabel "S-Trans") ".")
   (P "之所以" (RuleLabel "T-Sub") "是有问题的, "
      )
   (H3. "算法性子定型")
   (H3. "算法性定型")
   
   (H2. "子定型的一个ML实现")
   (H2. "案例研究: 命令式对象")
   (H2. "案例研究: Featherweight Java")
   (H2. "递归类型")
   (P "在第11.12节里我们看到了如何扩展一个简单类型系统以囊括一个类型构造子"
      (&List $T) ", 其元素是列表项为类型" $T "的元素的列表. "
      "列表不过是一类常见结构的一个例子而已" --
      "这类结构也包括队列, 二叉树, 标签树, 抽象句法树, 等等" --
      
      )
   (H2. "递归类型的元理论")
   (H3. "归纳和余归纳")
   ((Definition)
    "一个函数" (∈ $F (&-> (powerset $U:script) (powerset $U:script)))
    "是" (Em "单调的") ", 如果" (&sube $X $Y) "可以推出"
    (&sube (app $F $X) (app $F $Y))
    ". (回忆以下, " (powerset $U:script) "是" $U:script
    "的幂集.)")
   (P "以下我们默认" $F "是" (powerset $U:script)
      "上的某个单调函数. 我们经常将" $F "称为一个"
      (Em "生成函数") ".")
   ((Definition)
    "令" $X "是" $U:script "的一个子集."
    (Ol (Li $X "是" (Em $F "-封闭的")
            ", 如果" (&sube (app $F $X) $X) ".")
        (Li $X "是" (Em $F "-一致的")
            ", 如果" (&sube $X (app $F $X)) ".")
        (Li $X "是" $F "的一个" (Em "不动点")
            ", 如果" (&= (app $F $X) $X) ".")))
   (P "这些定义的一个有用直觉在于将" $U:script
      "的元素想成是某种陈述或者断言, "
      )
   (H3. "有限和无限类型")
   (H3. "子定型")
   (H2. "类型重构")
   (H2. "全称类型")
   (H2. "存在类型")
   (H2. "System F的一个ML实现")
   (H2. "有界量化")
   (H2. "案例研究: 命令式对象, Redux")
   (H2. "有界量化的元理论")
   
   ))