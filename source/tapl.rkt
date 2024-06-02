#lang racket
(provide tapl.html)
(require SMathML)
(define $Nat (Mi "Nat"))
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
(define (RecordType . x*)
  (apply setE (map2 &: x*)))
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
           (&<: (setE (^ (&: $l_i $T_i)
                         (∈ $i (&cm $1 $..h (&+ $n $k)))))
                (setE (^ (&: $l_i $T_i)
                         (∈ $i (&cm $1 $..h $n))))))
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
            (&<: (setE (^ (&: $l_i $S_i)
                          (∈ $i (&cm $1 $..h $n))))
                 (setE (^ (&: $l_i $T_i)
                          (∈ $i (&cm $1 $..h $n))))))))
   (P "下列子定型推导联合使用了" (RuleLabel "S-RcdWidth")
      "和" (RuleLabel "S-RcdDepth") "来表明嵌套的记录类型"
      (Code "{x:{a:Nat,b:Nat},y:{m:Nat}}") "是"
      (Code "{x:{a:Nat},y:{}}") "的一个子类型:"
      (MB (&rull
           (RuleLabel "S-RcdDepth")
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RecordType $a $Nat $b $Nat)
                 (RecordType $a $Nat)))
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RecordType $m $Nat)
                 (RecordType)))
           (&<: (RecordType
                 $x (RecordType $a $Nat $b $Nat)
                 $y (RecordType $m $Nat))
                (RecordType
                 $x (RecordType $a $Nat)
                 $y (RecordType)))))
      "如果我们想要使用" (RuleLabel "S-RcdDepth")
      "来仅仅细化单独一个记录域 "
      "(而不是细化每个域, 如以上例子), "
      "我们可以使用" (RuleLabel "S-Refl")
      "来对于其他的域获得平凡子定型的推导."
      (MB (&rull
           (RuleLabel "S-RcdDepth")
           (&rull
            (RuleLabel "S-RcdWidth")
            (&<: (RecordType $a $Nat $b $Nat)
                 (RecordType $a $Nat)))
           (&rull
            (RuleLabel "S-Refl")
            (&<: (RecordType $m $Nat)
                 (RecordType $m $Nat)))
           (&<: (RecordType
                 $x (RecordType $a $Nat $b $Nat)
                 $y (RecordType $m $Nat))
                (RecordType
                 $x (RecordType $a $Nat)
                 $y (RecordType $m $Nat)))))
      "我们也可以将传递性规则" (RuleLabel "S-Trans")
      "与宽度和深度子定型规则结合起来使用. "
      
      )
   (H2. "子定型的元理论")
   (H2. "子定型的一个ML实现")
   (H2. "案例研究: 命令式对象")
   (H2. "案例研究: Featherweight Java")
   (H2. "递归类型")
   (H2. "递归类型的元理论")
   (H2. "类型重构")
   (H2. "全称类型")
   (H2. "存在类型")
   (H2. "System F的一个ML实现")
   (H2. "有界量化")
   (H2. "案例研究: 命令式对象, Redux")
   (H2. "有界量化的元理论")
   
   ))