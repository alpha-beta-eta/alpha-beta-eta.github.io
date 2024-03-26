#lang racket
(provide curry-howard.html)
(require SMathML "curry-howard-utils.rkt")
(define curry-howard.html
  (TnTmPrelude
   #:title "Curry-Howard同构讲义"
   #:css "styles.css"
   (H1 "Curry-Howard同构讲义")
   (Heading0 #:auto? #f "前言")
   (P "Curry-Howard同构, 也被称为&quot;命题作为类型&quot;范式, 陈述了形式逻辑系统与计算性演算"
      "之间惊人的联系. 它自这样的观察开始, 一个推论" (&-> $A $B) "与一类从" $A "到" $B
      "的函数有关 [译注: 这里强调的是一个推论和一类有关, 一类也就是一个类型], 因为从" (&-> $A $B)
      "和" $A "推出" $B "可以被视为将第一个假设应用于第二个, 正如将一个从" $A "到" $B "的函数应用于"
      $A "的一个元素产生" $B "的一个元素. 类似地, 可以说通过实际从" $B "推出" $A "来证明推论"
      (&-> $A $B) "就像构造一个函数, 其将定义域" $A "的任意元素都映射至" $B "的元素.")
   (P "实际上, 这是一个很老的想法了, 应该归功于Brouwer, Kolmogorov和Heyting, 即从" $A "到"
      $B "的推论的一个构造性证明, 是一个将" $A "的证明变换为" $B "的证明的过程. Curry-Howard同构"
      "形式化了这个想法, 例如命题直觉主义逻辑中的证明可以被表示为简单类型" $lambda "项. "
      "可证明的定理不外乎是非空类型.")
   (P "这种类比, 最初由Haskell Brooks Curry于1930年代发现, 也适用于其他的逻辑系统. 似乎所有和"
      "证明相关的概念都可以基于计算来解释, 而各种lambda演算和相近系统的一切句法特征都可以用"
      "证明论的语言来陈述. 例如, 谓词逻辑中的量化对应于依赖积, 二阶逻辑与多态有关, 而古典逻辑中的"
      "反证法是控制运算符 (例如异常) 的近亲. 而且, 各种逻辑形式化 (Hilbert风格, 自然演绎, 相继式演算) "
      "可由相对应的计算模型 (组合子逻辑, " $lambda "演算, 显式替换演算) 来模拟.")
   (P "自William Howard的1969年的工作起人们理解到这种命题作为类型的对应不仅仅是意外得来的珍品, 而且是"
      "一种根本性的原理. 证明正规化和切消是另一种计算的模型, 等价于" $beta "规约. 证明论和"
      "计算理论不过是一体两面而已.")
   (P ""
      )
   (Heading0 #:id "untyped_lambda-calculus" "无类型" $lambda "演算")
   (P $lambda "演算是一种计算的模型, 它在另一种这样的模型Turing机器出现数年之前被发明出来. "
      "对于后者而言, 计算被表达为读写纸带, 以及根据纸带的内容施行操作. "
      "Turing机器就像命令式语言 (比如Java或C) 的程序." (Br)
      "与之相对地, 在" $lambda "演算中, 人们关心函数, 而函数可以将函数作为参数, 以及返回"
      "函数作为结果. 用编程的术语来说, " $lambda "演算是一个极其简单的高阶函数式编程语言." (Br)
      "本章我们只讨论无类型" $lambda "演算, 之后我们将引入诸多变体, 其中" $lambda
      "项被归为各种类型.")
   (Heading0 #:level 3 "温和的引入")
   (P "在" $lambda "演算之中计算以" $lambda "项表达. 它们类似于数学中使用的匿名函数记号"
      (&\|-> $n $n^2) ". 然而, 数学家使用这样的记号来指称作为数学对象的函数 (被定义为序对的集合). "
      "与之相对地, " $lambda "项是形式表达式 (字符串), 从直觉上说, 以最纯粹的形式表达了函数和"
      "函数的应用. 因此, 一个" $lambda "项是如下形式之一:"
      (Ul (Li "一个变量;")
          (Li "一个抽象" (&lam $x $M) ", 其中" $x "是一个变量, " $M "是一个" $lambda "项;")
          (Li "一个应用" (ap $M $N) " (" $M "应用于" $N "), 其中" $M "和" $N "是" $lambda "项."))
      "在一个抽象" (&lam $x $M) "中, 变量" $x "代表函数参数 (或者说形式参数), 它可能出现在函数的体"
      $M "之中, 但不必总是如此. 在一个应用" (ap $M $N) "之中, 运算符" $M "和参数" $N
      "的形状没有限制, 它们都可以是任意的" $lambda "项.")
   (P "举个例子, " $lambda "项" (&= $I:bold (&lam $x $x)) "从直觉上表示了一个将任意的参数映射至自身的"
      "函数, 即恒等函数. 再举一个, " (&= $K:bold (&lam0 $x (&lam0 $y $x))) "代表了这样的一个函数, "
      "其将任意的参数" $x "映射至总是返回" $x "的常函数. 最后一个例子, " (ap $I:bold $K:bold)
      "表达了将函数" $I:bold "应用于参数" $K:bold ".")
   (P "在数学中, 我们通常将函数应用的参数写在括号里, 例如若函数是" $f ", 参数是" $4
      ", 那么就记成" (app $f $4) ". 在" $lambda "演算中, 我们则会将其记成" (ap $f $4)
      ". 尽管如此, 括号的运用并不能被完全消除. 例如, 记号" (: $lambda $x $x $y) "是有歧义的. "
      "如果我们想要表达的是将" $I:bold "应用于" $y ", 那么就应该写成" (ap (@lam0 $x $x) $y)
      ". 或者, 可以用" (&lam0 $x (@ (ap $x $y))) "来表示一个" $x "上的抽象, 其体为" (ap $x $y)
      ". 后一种情况下, 按照惯例是使用点记号的, 即写作" (&lam1 $x (ap $x $y)) ". 类似地, 我们也需要"
      "使用括号来消除应用的歧义. 例如, " (ap $I:bold (@ (ap $K:bold $K:bold))) "表达了将" $I:bold
      "应用于" (ap $K:bold $K:bold) ".")
   (P "如果" (&lam0 $x $M) "代表一个函数, " $N "代表一个参数, 那么应用" (ap (@lam0 $x $M) $N)
      "的&quot;值&quot;可以通过将" $M "中的" $x "替换为" $N "计算得到. 这样一个替换的结果以"
      Mx:=N "表示, 而我们可以用" $beta "规约规则来形式化这个计算: "
      (&->beta (ap (@lam0 $x $M) $N) Mx:=N) ". 例如,"
      (MB (&->beta (&= (ap (@ (ap $I:bold $K:bold)) $z)
                       (ap (@ (ap (@lam0 $x $x) $K:bold)) $z))
                   (&= (ap (subst $x $x $K:bold) $z)
                       (ap $K:bold $z)
                       (ap (@lam0 $y (&lam0 $x $y)) $z))
                   (&lam0 $x $z)) ".")
      "这个计算表达式的值的过程与通常的数学实践是类似的. 如果" (&= (app $f $n) $n^2)
      ", 那么" (&= (app $f $4) (^ $4 $2)) ", 并且我们是通过在" $f "定义的体中将" $n
      "替换为" $4 "从应用" (app $f $4) "得到结果" (^ $4 $2) "的. 编程语言上的类比即"
      "按名调用的参数传递机制, 其中过程的形式参数被全部替换以实际的参数表达式.")
   (P "一个" $lambda "抽象" (&lam0 $x $M) "中的变量" $x "在" $M "中被绑定 (或者说局部于), "
      "就非常类似于一个过程的形式参数被认为局部于该过程. 与之相对的是, 一个变量" $y
      ", 若没有与其对应的抽象, 则被称为自由的 (或者全局的), 这就类似于大多数编程语言中的"
      "全局变量. 因此, 在" (&lam1 $x (ap $x $y)) "之中, " $x "是绑定的, 而" $y "是自由的.")
   (P "当绑定变量和自由变量重名时会引起一些混乱. 例如, 在" (ap $x (@ (&lam1 $x (ap $x $y))))
      "中, 显然有两个不同的" $x ": 自由的 (全局的) " $x "和绑定的 (局部的) " $x ", 绑定的"
      $x "&quot;遮住了&quot;体内自由的" $x ". [译注: 换言之, 若体内的" $x "没有" $lambda
      "绑定的包裹, 则会是一个自由变量.] 若我们转而考虑" $lambda "项"
      (ap $x (@lam1 $z (ap $z $y))) ", 那就没有歧义了. 再举另外一种引起混乱的例子, "
      (subst (@lam1 $x (ap $x $y)) $y $x) "应该将" (@lam1 $x (ap $x $y)) "中的"
      $y "替换为自由变量" $x ", 但是" (&lam1 $x (ap $x $x)) "并非预期的结果. 在后一个项中, "
      "我们失去了形式参数" $x "和自由变量" $x "之间的区别 (自由变量已被lambda捕获). "
      "如果我们使用一个绑定变量" $z ", 那么混乱就消失了: "
      (&= (subst (@lam1 $z (ap $z $y)) $y $x) (&lam1 $z (ap $z $x))) ".")
   (P "过程的局部变量总可以被换名而不影响程序的含义. 类似地, 在" $lambda "演算中我们也不"
      "在乎绑定变量的名字. " $lambda "项" (&lam0 $x $x) "和" (&lam0 $y $y) "都代表着"
      "恒等函数. 正因如此, 通常假定只有绑定变量不同的项是等同的. 这给了我们选取绑定变量"
      "以避免混乱 (比如说变量捕获) 的自由.")
   (Heading0 #:level 3 "预项和" $lambda "项")
   (P "现在我们定义预项的概念, 并将" $lambda "项作为预项的等价类引入. 本节相当乏味, "
      "但对于使得我们的形式化精确而言是必要的. 然而, 为了理解本书的大部分内容, "
      "前一节对于" $lambda "项的非形式化理解就足够了.")
   ((Definition #:id "pre-term")
    "令" $Upsilon:normal "代表一个可数无穷的符号集合, 之后就将其称为变量 "
    "(当其他种类的变量可能造成歧义的时候, 也将其称为对象变量或者" $lambda "变量). "
    "我们通过归纳定义预项的概念如下:"
    (Ul (Li "每个变量都是一个预项.")
        (Li "如果" $M ", " $N "是预项, 那么" (@ap $M $N) "也是一个预项.")
        (Li "如果" $x "是一个变量而" $M "是一个预项, 那么" (@lam0 $x $M)
            "也是一个预项."))
    "所有预项构成的集合记作" Lambda- ".")
   ((Remark #:auto? #f)
    "这个定义可以被总结为以下语法:"
    (MB (&::= $M (&vert $x (@ (ap $M $M)) (@lam0 $x $M))) ".")
    "在本书的剩余部分里, 我们将时常使用这种简短的定义风格.")
   (let ((lam (lambda (x M) (@ $lambda x M)))
         (app (lambda (M N) (@ (ap M N)))))
     (P "预项, 正如上面所定义的那样, 是完全括号化的. 如预项"
        (lam $f (app (lam $u (app $f (app $u $u)))
                     (lam $v (app $f (app $v $v)))))
        "所呈现的, 括号的重度使用是相当笨拙的. 因此, 我们引入一些记号上的约定, "
        "每当不致引起歧义时我们非正式地使用它们."))
   ((Convention)
    (Ol #:attr* '((type "i"))
        (Li "一个项最外部的括号被省略.")
        (Li "应用向左结合: " (@ap* $P $Q $R) "被缩略为" (@ (ap* $P $Q $R)) ".")
        (Li "抽象向右结合: " (@lam0 $x (@lam0 $y $P)) "被缩略为"
            (@lam0 $x (&lam0 $y $P)) ".")
        (Li "一个抽象的序列" (@lam0 $x_1 (@lam0 $x_2 (: $..h (@lam0 $x_n $P) $..h)))
            "可以被记成" (@ (&lam $x_1 $x_2 $..h $x_n $P)) ", 在这种情形之下" $P
            "最外面的括号 (如果有的话) 通常被省略. [原注: 这个点代表了一个辖域尽可能"
            "向右延伸的左括号.]")))
   ((Example #:auto? #f)
    (Ul (Li "根据i, " (@lam0 $v (@ap $v $v)) "可以被缩略为"
            (&lam0 $v (@ap $v $v)) ".")
        (Li "根据i和ii, " (@ap* (@lam0 $x $x) (@lam0 $y $y) (@lam0 $z $z))
            "可以被缩略为" (ap* (@lam0 $x $x) (@lam0 $y $y) (@lam0 $z $z)) ".")
        (Li "根据i和iii, " (@lam0 $x (@lam0 $y (@ap $x $y))) "被记为"
            (&lam0 $x (&lam0 $y (@ap $x $y))) ", 或者根据i和iv记作"
            (&lam $x $y (ap $x $y)) ".")
        (Li (@lam0 $f (@ap (@lam0 $u (@ap $f (@ap $u $u))) (@lam0 $v (@ap $f (@ap $v $v))))) "记作"
            (&lam1 $f (ap (@lam1 $u (ap $f (@ap $u $u))) (@lam1 $v (ap $f (@ap $v $v))))) ".")))
   ((Definition)
    "定义" $M "的自由变量的集合" (&FV $M) "如下."
    (MB (set-attr*
         (&Table
          ((&FV $x) $= (: (setE $x) ";"))
          ((&FV (&lam0 $x $P)) $= (: (&- (&FV $P) (setE $x)) ";"))
          ((&FV (ap $P $Q)) $= (: (&union (&FV $P) (&FV $Q)) ".")))
         'columnalign "left")))
   ((Example #:auto? #f)
    "令" (&cm $x $y $z) "是不同的变量, 那么"
    (&= (&FV (ap (@lam0 $x $x) (@lam1 $y (ap* $x $y $z)))) (setE $x $z))
    ". 这里有两种" $x "的出现: 一个在" (: $lambda $x) "下, 一个在" (: $lambda $y)
    "下. " $M "中一个" $x "的出现被称为绑定的, 如果它在形状为" (&lam0 $x $L)
    "的" $M "的一部分中, 反之则被称为自由的. 于是, " (&in $x (&FV $M))
    "当且仅当" $M "中存在" $x "的一个自由出现.")
   ((Definition)
    "将" $M "中的" $x "替换为" $N ", 记作" Mx:=N
    ", 是有定义的当且仅当不存在" $M "中" $x "的自由出现满足其在形式为"
    (&lam0 $y $L) "的" $M "的一部分中, 并且" (&in $y (&FV $N))
    ". [译注: 直觉就是" $N "中的自由变量不能被" $y "捕获.] "
    "在这样的情况下, " Mx:=N "由下列等式给出:"
    (MB (Tstyle1
         (&Table
          ((subst $x $x $N) $= (: $N ";"))
          ((subst $y $x $N) $= (: $y ", 如果" (&!= $x $y) ";"))
          ((subst (@ (ap $P $Q)) $x $N)
           $= (: (ap (subst $P $x $N) (subst $Q $x $N)) ";"))
          ((subst (@lam0 $x $P) $x $N) $= (: (&lam0 $x $P) ";"))
          ((subst (@lam0 $y $P) $x $N)
           $= (: (&lam0 $y (subst $P $x $N)) ", 如果" (&!= $x $y) ".")))))
    "[原注: 在我们的元记号中, 替换绑定得比其他任何东西都要强, 所以说在第三行, "
    "最右边的替换是应用于" $Q "的, 而不是应用于" (ap (subst $P $x $N) $Q)
    "的. (译注: 补充说一句, 真要是后一种情况, 那这个定义是否良定都很可疑了, 因为"
    (ap (subst $P $x $N) $Q) "不比" (ap $P $Q) "小.)]" (Br)
    "[译注: 读者应该注意到在这个定义里, 如果" (subst (@ (ap $P $Q)) $x $N)
    "有定义, 那么" (subst $P $x $N) "和" (subst $Q $x $N) "也就有定义, 并且如果"
    (subst (@lam0 $y $P) $x $N) "有定义, 其中" (&!= $x $y) ", 那么"
    (subst $P $x $N) "也就有定义. 否则的话, 替换就不是良定的了.]")
   ((Remark #:auto? #f)
    "在最后一行, " (&!in $y (&FV $N)) "或者" (&!in $x (&FV $P)) ".")
   ((Lemma)
    (Ol #:attr* '((type "i"))
        (Li "如果" (&!in $x (&FV $M)) ", 那么" Mx:=N
            "有定义, 并且" (&= Mx:=N $M) ".")
        (Li "如果" Mx:=N "有定义, 那么" (&in $y (&FV Mx:=N))
            "当且仅当要么" (&in $y (&FV $M)) "且" (&!= $x $y) ", 要么"
            (&in $y (&FV $N)) "且" (&in $x (&FV $M)) ".")
        (Li "替换" (subst $M $x $x) "有定义, 且" (&= (subst $M $x $x) $M) ".")
        (Li "如果" (subst $M $x $y) "有定义, 那么" (subst $M $x $y) "与" $M
            "拥有相同的长度.")))
   ((proof)
    "在" $M "上施行归纳. 作为例子, 我们细致地证明i. 显然" Mx:=N
    "是有定义的. 为了证明" (&= Mx:=N $M) "我们考虑以下情形. 如果"
    $M "是一个变量" $y ", 那么必有" (&!= $y $x) ", 并且" (&= (subst $y $x $N) $y)
    ". 如果" (&= $M (ap $P $Q)) ", 那么" (&!in $x (&FV $P)) "且"
    (&!in $x (&FV $Q)) ", 于是根据归纳假设有" (&= (subst $P $x $N) $P) "且"
    (&= (subst $Q $x $N) $Q) ", 因此"
    (&= (subst (@ (ap $P $Q)) $x $N)
        (ap (subst $P $x $N) (subst $Q $x $N))
        (ap $P $Q))
    ". 最后, 如果" $M "是一个抽象, 要么" (&= $M (&lam0 $x $P)) "要么"
    (&= $M (&lam0 $y $P)) ", 其中" (&!= $x $y) ". 在前一种情况下, "
    (&= (subst (@lam0 $x $P) $x $N) (&lam0 $x $P)) ". 在后一种情况下, 我们有"
    (&!in $x (&FV $P)) ", 于是根据归纳假设, "
    (&= (subst (@lam0 $y $P) $x $N)
        (&lam0 $y (subst $P $x $N))
        (&lam0 $y $P)) ".")
   ((Lemma)
    "假定" Mx:=N "是有定义的, 并且" (subst $N $y $L) "和"
    (subst* $M $x $N $y $L) "也都是有定义的, 其中" (&!= $x $y) ". 如果"
    (&!in $x (&FV $L)) "或者" (&!in $y (&FV $M)) ", 那么" (subst $M $y $L)
    "是有定义的, " (subst* $M $y $L $x (subst $N $y $L)) "也是有定义的, 并且"
    (MB (&= (subst* $M $x $N $y $L) (subst* $M $y $L $x (subst $N $y $L))) "."))
   ((proof)
    
    )
   ((Lemma)
    
    )
   ((Definition)
    "关系" $=alpha " (" $alpha "变换) 是" Lambda- "上满足下列条件的"
    "最小的传递自反关系:"
    (Ul (Li "如果" (&!in $y (&FV $M)) "并且" (subst $M $x $y) "有定义, 那么"
            (&=alpha (&lam0 $x $M) (&lam1 $y (subst $M $x $y))) ".")
        (Li "如果" (&=alpha $M $N) ", 那么对于所有的变量" $x ", "
            (&=alpha (&lam0 $x $M) (&lam0 $x $N)) ".")
        (Li "如果" (&=alpha $M $N) ", 那么"
            (&=alpha (ap $M $Z) (ap $N $Z)) ".")
        (Li "如果" (&=alpha $M $N) ", 那么"
            (&=alpha (ap $Z $M) (ap $Z $N)) "."))
    "[译注: 后三条是在说" $=alpha "是一个兼容关系.]")
   ((example)
    "令" (&cm $x $y) "是不同的变量, 那么"
    (&=alpha (&lam $x $y (ap $x $y)) (&lam $y $x (ap $y $x))) ", 但是"
    (: (&lam1 $x (ap $x $y)) (_ $!= $alpha) (&lam1 $y (ap $y $x))) ".")
   ((Lemma)
    $alpha "变换的关系是一个等价关系.")
   ((Lemma)
    "如果" (&=alpha $M $N) ", 那么" (&= (&FV $M) (&FV $N)) ".")
   ((proof)
    
    )
   
   ((definition #:n "1.2.14")
    "定义" $lambda "项的集合" $Lambda:normal "为" $=alpha "的商集:"
    (MB (&= $Lambda:normal (setI (eqc $M $alpha) (&in $M Lambda-))) ",")
    "其中" (&= (eqc $M $alpha) (setI (&in $N Lambda-) (&=alpha $M $N))) ".")
   ((example)
    
    )
   ((definition #:n "1.2.15")
    "一个" $lambda "项" $M "的自由变量" (&FV $M) "定义如下. "
    "令" (&= $M (eqc $M^ $alpha)) ", 那么"
    (MB (&= (&FV $M) (&FV $M^)) ".")
    "如果" (&= (&FV $M) $empty) ", 那么就称" $M "是封闭的或者它是一个组合子." (Br)
    "引理1.2.10确保了任何对于" $M^ "的选择都会产生相同的结果.")
   ((definition #:n "1.2.16")
    
    )
   ((notation #:n "1.2.17")
    
    )
   ((lemma #:n "1.2.18")
    "以下等式是合理的:"
    (MB (set-attr*
         (&Table
          ((&FV $x) $= (: (setE $x) ";"))
          ((&FV (&lam0 $x $P)) $= (: (&- (&FV $P) (setE $x)) ";"))
          ((&FV (ap $P $Q)) $= (: (&union (&FV $P) (&FV $Q)) ".")))
         'columnalign "left")))
   ((lemma #:n "1.2.19")
    
    )
   ((lemma #:n "1.2.20")
    
    )
   ((definition #:n "1.2.21")
    
    )
   
   (Heading0 #:level 3 "规约")
   ((Definition)
    "一个" $Lambda:normal "上的关系" i.sc "是兼容的当且仅当它对于所有的"
    (&in (&cm $M $N $Z) $Lambda:normal) "满足下列条件."
    (Ol #:attr* '((type "i"))
        (Li "对于所有的变量" $x ", 如果" (&sc $M $N) ", 那么"
            (&sc (&lam $x $M) (&lam $x $N)) ".")
        (Li "如果" (&sc $M $N) ", 那么" (&sc (ap $M $Z) (ap $N $Z)) ".")
        (Li "如果" (&sc $M $N) ", 那么" (&sc (ap $Z $M) (ap $Z $N)) "."))
    "[译注: 兼容的直觉就是上下文中的关系.]")
   ((Definition)
    $Lambda:normal "上满足"
    (MB (&->beta (ap (@lam1 $x $P) $Q) (subst $P $x $Q)))
    "的最小兼容关系" $->beta "被称为" $beta "规约. 具有形式"
    (ap (@lam0 $x $P) $Q) "的项被称为" $beta "可规约项, 而"
    (subst $P $x $Q) "被称为是由收缩这个可规约项产生的. 一个项" $M "是"
    $beta "规范形式 (记号" (&in $M NF_beta) ") 当且仅当不存在这样的" $N
    "满足" (&->beta $M $N) ", 即" $M "并不包含一个" $beta "可规约项.")
   ((Definition)
    (Ol #:attr* '((type "i"))
        (Li "关系" $->>beta " (多步" $beta "规约) 是" $->beta "的传递且自反闭包. "
            $->beta "的传递闭包记作" $->beta+ ".")
        (Li "关系" $=beta " (被称为" $beta "等价或者" $beta "变换) 是包含"
            $->beta "的最小等价关系.")
        (Li "一个" $beta "规约序列是一个有限或无限的序列"
            (MB (&->beta $M_0 $M_1 $M_2 $..c)))))
   ((Remark)
    
    )
   ((example #:n "1.3.5")
    (Ol #:attr* '((type "i"))
        (Li (M (ap (@lam1 $x (ap $x $x)) (@lam0 $z $z)) $->beta
               (subst (@ (ap $x $x)) $x (&lam0 $z $z)) $=
               (ap (@lam0 $z $z) (@lam0 $y $y))) ".")
        (Li (M (ap (@lam0 $z $z) (@lam0 $y $y)) $->beta
               (subst $z $z (&lam0 $y $y)) $=
               (&lam0 $y $y)) ".")
        (Li (&->>beta (ap (@lam1 $x (ap $x $x)) (@lam0 $z $z))
                      (&lam0 $y $y)) ".")
        (Li (&=beta (ap* (@lam0 $x $x) $y $z)
                    (ap $y (@ (ap (@lam0 $x $x) $z)))) ".")))
   ((example #:n "1.3.6")
    (B "(一些常见的" $lambda "项).")
    (Ol #:attr* '((type "i"))
        (Li "令" (&= $I:bold (&lam0 $x $x)) ", " (&= $K:bold (&lam $x $y $x))
            "以及" (&= $S:bold (&lam $x $y $z (ap* $x $z (@ap $y $z))))
            ", 那么" (&->>beta (ap* $S:bold $K:bold $K:bold) $I:bold) ".")
        (Li "令" (&= $omega (&lam1 $x (ap $x $x))) "以及"
            (&= $Omega:bold (ap $omega $omega)) ", 那么"
            (&->beta $Omega:bold $Omega:bold $Omega:bold $..c) ".")
        (Li "令" (&= $Y:bold (&lam1 $f (ap (@lam1 $x (ap $f (@ap $x $x))) (@lam1 $x (ap $f (@ap $x $x))))))
            ", 那么" (&->beta $Y:bold (&prime $Y:bold) (&Prime $Y:bold) $..c) ", 其中"
            (&cm $Y:bold (&prime $Y:bold) (&Prime $Y:bold) $..c) "各不相同.")))
   ((remark #:n "1.3.7")
    
    )
   ((lemma #:n "1.3.8")
    (Ol #:attr* '((type "i"))
        (Li ""
            )
        )
    )
   ((definition #:n "1.3.9")
    
    )
   ((proposition #:n "1.3.10")
    "令" $=ext "是满足下列条件的最小关系:"
    (Ul (Li "如果" (&=beta $M $N) ", 那么" (&=ext $M $N) ";")
        (Li "如果" (&=ext (ap $M $x) (ap $N $x)) "且"
            (&!in $x (&union (&FV $M) (&FV $N))) ", 那么"
            (&=ext $M $N) ";")
        (Li "如果" (&=ext $P $Q) ", 那么" (&=ext (ap $P $Z) (ap $Q $Z))
            "且" (&=ext (ap $Z $P) (ap $Q $P)) "."))
    "那么, " (&=ext $M $N) "当且仅当" (&=beta-eta $M $N)
    ". [译注: " $=ext "即所谓的外延相等的概念.]")
   ((proof)
    
    )
   ((lemma #:n "1.3.11")
    
    )
   
   (H3 "第1.4节 Church-Rosser定理")
   (P "既然一个" $lambda "项" $M "可能包含数个" $beta "可规约项, 那么可能存在多个" $N
      "满足" (&->beta $M $N) ". 例如, "
      (&->beta (ap $K:bold (@ (ap $I:bold $I:bold))) (&lam1 $x (ap $I:bold $I:bold)))
      "和" (&->beta (ap $K:bold (@ (ap $I:bold $I:bold))) (ap $K:bold $I:bold))
      ". "
      )
   ((definition #:n "1.4.1")
    "令" $=>beta "是满足下列条件的" $Lambda:normal "上的最小关系:"
    (Ul (Li "对于所有变量" $x ", " (&=>beta $x $x) ".")
        (Li "如果" (&=>beta $P $Q) ", 那么"
            (&=>beta (&lam1 $x $P) (&lam1 $x $Q)) ".")
        (Li "如果" (&=>beta $P_1 $Q_1) "且" (&=>beta $P_2 $Q_2)
            ", 那么" (&=>beta (ap $P_1 $P_2) (ap $Q_1 $Q_2)) ".")
        (Li "如果" (&=>beta $P_1 $Q_1) "且" (&=>beta $P_2 $Q_2) ", 那么"
            (&=>beta (ap (@lam1 $x $P_1) $P_2)
                     (subst $Q_1 $x $Q_2)) ".")))
   ((lemma #:n "1.4.2")
    (Ol #:attr* '((type "i"))
        (Li "如果" (&->beta $M $N) ", 那么" (&=>beta $M $N) ".")
        
        )
    )
   ((proof)
    
    )
   ((definition #:n "1.4.3")
    "令" (&* $M) " (" $M "的complete development) 定义为:"
    (MB
     (set-attr*
      (&Table
       ((&* $x) $= (: $x ";"))
       ((&* (@lam0 $x $M)) $= (: (&lam0 $x (&* $M)) ";"))
       ((&* (@ap $M $N)) $= (: (ap (&* $M) (&* $N)) ", 如果" $M "不是一个抽象;"))
       ((&* (@ap (@lam0 $x $M) $N)) $= (: (subst (&* $M) $x (&* $N)) ".")))
      'columnalign "left"))
    
    )
   (H3 "第1.5节 最左规约是正规化")
   ((definition #:n "1.5.1")
    "一个项" $M "是正规化的 (记号" (&in $M $WN_beta) ") 当且仅当存在一个从" $M
    "开始的规约序列终于正规形式" $N ", 那么我们称" $M "有正规形式" $N
    ". 一个项" $M "是强正规化的 (" (&in $M $SN_beta) "或者仅仅" (&in $M $SN)
    ") 如果从" $M "开始的规约序列都是有限的. 如果" (&!in $M $SN_beta) ", 那么我们记"
    (&in $M $inf_beta) ". 类似的记号也用于其他规约概念.")
   (P "任何强正规化的项也是正规化的, 反之则不然, 如" (ap* $K:bold $I:bold $Omega:bold)
      ". 但是, 定理1.5.8陈述了一个正规形式, 如果存在的话, 总是可由重复规约最左可规约项得到, "
      "最左可规约项即其" $lambda "向左走得最远的可规约项. 以下记号和定义对于证明"
      "定理1.5.8而言是方便的.")
   (P (B "向量记号.") " 令" (&>= $n $0) ". 如果" (&= (&vec $P) (&cm $P_1 $..h $P_n))
      ", 那么我们记" (ap* $M $P_1 $..h $P_n) "为" (ap $M (&vec $P)) ". 特别地, 如果"
      (&= $n $0) ", 即" (&vec $P) "为空, 那么" (ap $M (&vec $P)) "就是" $M ". 类似地, 如果"
      (&= (&vec $z) (&cm $z_1 $..h $z_n)) ", 那么我们记" (&lam $z_1 $..h $z_n $M) "为"
      (&lam1 (&vec $z) $M) ". 又一次, 如果" (&= $n $0) ", 即" (&vec $z) "为空, 那么"
      (&lam1 (&vec $z) $M) "就是" $M ".")
   ((remark)
    "任何项都恰具有以下两种形式之中的一种: " (&lam1 (&vec $z) (ap $x (&vec $R))) "或者"
    (&lam1 (&vec $z) (ap* (@lam0 $x $P) $Q (&vec $R))) ", 在后一种情况下"
    (ap (@lam0 $x $P) $Q) "被称为头部可规约项 (在前一种情况下, 不存在头部可规约项). "
    )
   ((definition #:n "1.5.2")
    "对于不是正规形式的项" $M ", 我们记"
    (Ul (Li (&l->beta $M $N) "如果" $N "是通过" $M "收缩最左可规约项得到的.")
        (Li (&h->beta $M $N) "如果" $N "是通过" $M "收缩头部可规约项得到的.")
        (Li (&i->beta $M $N) "如果" $N "是通过" $M "收缩内部可规约项得到的.")))
   ((lemma #:n "1.5.3")
    (Ol #:attr* '((type "i"))
        (Li "如果" (&h->beta $M $N) ", 那么" (&h->beta (&lam0 $x $M) (&lam0 $x $N)) ".")
        
        )
    )
   ((definition #:n "1.5.4")
    
    )
   (H3 "第1.6节 永恒规约和守恒定理")
   (H3 "第1.7节 可表达性和不可判定性")
   (P "无类型" $lambda "演算是如此之简单, 以至于它的强大是令人惊讶的. 本节我们将展示实际上"
      $lambda "演算可以被视为递归论的另一种形式化.")
   (P "我们可以使用" $lambda "项表示各种各样的构造, 例如真值:"
      (MB (&= $true lam:true) ";&nbsp;"
          (&= $false lam:false) ";&nbsp;"
          (&= (&if $P $Q $R) (ap* $P $Q $R)) ".")
      "很容易看出来"
      (MB (&->>beta (&if $true $P $Q) $P) ";&nbsp;"
          (&->>beta (&if $false $P $Q) $Q) ".")
      "另一种有用的构造是序对"
      (MB (Tstyle1
           (&Table
            ((tupa0 $M $N) $= (&lam1 $x (ap* $x $M $N)))
            ($pi_1 $= (&lam1 $p (ap $p (@ lam:true))))
            ($pi_2 $= (&lam1 $p (ap $p (@ lam:false)))))))
      "正如所期望的我们有"
      (MB (&->>beta (ap $pi_i (tupa0 $M_1 $M_2)) $M_i)))
   ((definition #:n "1.7.1")
    "在" $lambda "中我们可以将自然数表示为Church数码:"
    (MB (&= (_ $c:bold $n) (&lam $f $x (app $f^n $x))))
    "其中" (app $f^n $x) "是" (app $f (app $f $..c (@ $x) $..c))
    "的缩略形式, " $f "出现了" $n "次. 有时我们将" (_ $c:bold $n)
    "记成" $n:bold ", 于是"
    (MB (Tstyle1
         (&Table
          ($0-bold $= (&lam $f $x $x))
          ($1-bold $= (&lam $f $x (ap $f $x)))
          ($2-bold $= (&lam $f $x (ap $f (@ (ap $f $x)))))))))
   ((definition #:n "1.7.2")
    "一个部分函数" (func $f $NN^k $NN) "是" $lambda
    "可定义的当且仅当存在" (&in $F $Lambda:normal) "满足:"
    (Ul (Li "如果" (&= (appl $f $n_1 $..h $n_k) $m) ", 那么"
            (&=beta (ap* $F (_ $c:bold $n_1) $..h (_ $c:bold $n_k))
                    (_ $c:bold $m)) ".")
        (Li "如果" (appl $f $n_1 $..h $n_k) "是未定义的, 那么"
            (ap* $F (_ $c:bold $n_1) $..h (_ $c:bold $n_k))
            "没有规范形式."))
    "我们称项" $F "定义了函数" $f ".")
   ((example #:n "1.7.3")
    "下列项定义了一些常见函数."
    (Ul (Li "后继: " (&= i.succ (&lam $n $f $x (ap $f (@ (ap* $n $f $x))))) ".")
        (Li "加法: " (&= i.add (&lam $m $n $f $x (ap* $m $f (@ (ap* $n $f $x))))) ".")
        (Li "乘法: " (&= i.mult (&lam $m $n $f $x (ap* $m (@ (ap $n $f)) $x))) ".")
        (Li "幂: " (&= i.exp (&lam $m $n $f $x (ap* $m $n $f $x))) ". "
            "[注记: 不知是笔误还是有意为之, 实际上这里的幂的定义的参数顺序与通行认知相反. "
            "并且, 其实" $f "和" $x "也可以不用写出来即可定义幂.]")
        (Li "常零函数: " (&= i.zero (&lam1 $m $0-bold)) ".")
        (Li $k "元参数的第" $i "投影: " (&= (_^ $Pi:normal $i $k) (&lam $m_1 $..h $m_k $m_i)) ".")))
   ((proposition #:n "1.7.4")
    "原始递归函数是" $lambda "可定义的.")
   
   (H3 "第1.8节 注记")
   (P $lambda "演算和与之相关的组合子逻辑是在1930年左右分别由Alonzo Church "
      (Cite0 "postulates_for_the_foundation_of_logic1" "postulates_for_the_foundation_of_logic2")
      "和Haskell B. Curry引入的. "
      "起初, 这种演算是某意图作为逻辑基础的系统的一部分. 不幸的是, Church的学生Kleene和Rosser发现原始的"
      "系统是不一致的, 而Curry简化了这个结果, 其现在被称为Curry悖论. 最终, 仅处理" $lambda
      "项, 规约以及变换的子系统, 也就是我们称之为" $lambda "演算的东西, 被单独进行研究.")
   (P $lambda "绑定和" $alpha "变换的概念在直觉上是非常清晰的, 然而在第1.2节我们看到为了正确地处理它们, "
      "许多技术性的困难必须被克服. 当人们遇到实际实现的问题时, 这个事情就变得尤为重要了. 一种古典的"
      "解决方案是使用变量的匿名表示 (所谓的de Bruijn索引). 想要了解更多此方面内容和相关主题, 见例如"
      (Cite0 "higher-order_abstract_syntax" "first-order_theory_of_names_and_binding"
             "closure_under_alpha-conversion") ".")
   
   (H3 "第1.9节 练习")
   ((exercise #:n "1.2")
    "修改定义1.2.4以使得操作" Mx:=N "对于所有的" $M ", " $N "和" $x "都有定义, 然后证明"
    (&=alpha Mx:=N (subst $M^ $x $N^)) "对于所有的" (&=alpha $M $M^) "和" (&=alpha $N $N^)
    "均成立 (参照引理1.2.11).")
   
   (Heading0 #:id "intuitionistic_logic" "直觉主义逻辑")
   (P "&quot;逻辑&quot;一词具有多种含义, 从日常推理到复杂的形式系统. 在大多数情况下, 逻辑被用来"
      "将陈述划分为&quot;真&quot;和&quot;假&quot;. 也就是说, 人们所说的逻辑通常指的是二值古典逻辑"
      "的诸多变种之一.")
   (P ""
      )
   (H3 "第2.1节 BHK解释")
   (P "直觉主义命题逻辑, 也被称为直觉主义命题演算 (缩写为IPC), 其语言和古典命题逻辑相同 "
      "[译注: 即句法相同].")
   ((definition #:n "2.1.1")
    "给定一个命题变量的无限集合" $PV ", 我们将公式的集合" $Phi:normal
    "定义为满足下列条件的最小集合:"
    (Ul (Li "每个命题变量和常量" $bottom "都在" $Phi:normal "中;")
        (Li "如果" (&in (&cm $phi $psi) $Phi:normal) ", 那么"
            (&in (&cm (@-> $phi $psi) (@ (&disj $phi $psi)) (@ (&conj $phi $psi)))
                 $Phi:normal) "."))
    "变量和" $bottom "被称为原子公式. 一个公式" $phi "的子公式是" $phi
    "的一部分 (未必proper), 其自身是也是一个公式.")
   (P "也就是说, 我们基本的联结词是: 推出" i.-> ", 析取" i.disj ", 合取" i.conj
      ", 以及常量" $bottom " (谬). 否定" i.neg "和等价" i.<-> "是缩略形式, 常量"
      $top " (真) 也是如此:"
      (Ul (Li (&neg $phi) "是" (&-> $phi $bottom) "的缩略;")
          (Li (&<-> $phi $psi) "是" (&conj (@-> $phi $psi) (@-> $psi $phi))
              "的缩略;")
          (Li $top "是" (&-> $bottom $bottom) "的缩略.")))
   (P "Brouwer-Heyting-Kolmogorov解释:"
      (Ul (Li (&conj $phi_1 $phi_2) "的一个构造由" $phi_1 "的一个构造和"
              $phi_2 "的一个构造构成;")
          (Li (&disj $phi_1 $phi_2) "的一个构造由一个指示子" (&in $i (setE $1 $2))
              "和" $phi_i "的一个构造构成;")
          (Li (&-> $phi_1 $phi_2) "的一个构造是一个将每个" $phi_1
              "的构造转换为" $phi_2 "的一个构造的方法 (函数);")
          (Li "不存在" $bottom "的构造.")))
   ((example #:n "2.1.3")
    "考虑以下公式:"
    (Ol #:attr* '((type "i"))
        (Li (&-> $bottom $p) ";")
        (Li (&-> $p $q $p) ";")
        (Li (&-> (@-> $p $q $r) (@-> $p $q) $p $r) ";")
        (Li (&-> $p (&neg (&neg $p))) ";")
        (Li (&-> (&neg (&neg (&neg $p))) (&neg $p)) ";")
        (Li (&-> (@-> $p $q)
                 (@-> (&neg $q) (&neg $p))) ";")
        (Li (&<-> (&neg (@ (&disj $p $q)))
                  (@ (&conj (&neg $p) (&neg $q)))) ";")
        (Li (&<-> (@-> (@ (&conj $p $q)) $r)
                  (@-> $p (@-> $q $r))) ";")
        (Li (&neg (&neg (@ (&disj $p (&neg $p))))) ";")
        (Li (&-> (@ (&disj $p (&neg $p)))
                 (&neg (&neg $p)) $p) "."))
    "以上公式都可被赋予一个BHK解释. "
    )
   ((example #:n "2.1.4")
    "以下每个公式都是一个古典重言, 但是并不存在一个构造, 尽管其中一些"
    "与前一个例子中的公式类似或者&quot;对偶&quot;."
    (Ol #:attr* '((type "i"))
        (Li (&-> (@-> (@-> $p $q) $p) $p) ";")
        (Li (&disj $p (&neg $p)) ";")
        (Li (&-> (&neg (&neg $p)) $p) ";")
        (Li (&-> (@-> (&neg $q) (&neg $p)) (@-> $p $q)) ";")
        (Li (&<-> (&neg (@ (&conj $p $q)))
                  (@ (&disj (&neg $p) (&neg $q)))) ";")
        (Li (&-> (@-> $p $q) (@-> (&neg $p) $q) $q) ";")
        (Li (&<-> (@ (&<-> (@ (&<-> $p $q)) $r))
                  (@ (&<-> $p (@ (&<-> $q $r))))) ";")
        (Li (&<-> (@-> $p $q)
                  (@ (&disj (&neg $p) $q))) ";")
        (Li (&disj (@-> (&disj $p $q) $p)
                   (@-> (&disj $p $q) $q)) ";")
        (Li (&-> (@-> (&neg (&neg $p)) $p)
                 (&disj $p (&neg $p))) "."))
    
    )
   (H3 "第2.2节 自然演绎")
   (P "为了形式化直觉主义命题逻辑, 我们定义了一个被称为自然演绎的证明系统, 用"
      (appl $NJ i.-> $bottom i.conj i.disj) "表示, 或者就简单地记作" $NJ ".")
   ((definition #:n "2.2.1")
    (Ol #:attr* '((type "i"))
        (Li "自然演绎中的一个判断是一个序对, 被记作" (&entailL $G:env $phi)
            " (读作&quot;" $G:env "证明了" $phi "&quot;), 其由一个"
            "有限的公式集合" $G:env "和一个公式" $phi "构成.")
        (Li "我们在书写判断的时候使用了多种简化, 例如记" (&-> (&cm $phi_1 $phi_2) $psi)
            "而不是" (&-> (setE $phi_1 $phi_2) $psi) ", 记"
            (&cm $G:env $Delta:normal) "而不是"
            (&union $G:env $Delta:normal) ", 记" (&cm $G:env $phi)
            "而不是" (&union $G:env (setE $phi)) ". 特别地, 记号"
            (&entailL $ $phi) "代表" (&entailL $empty $phi) ".")
        (Li (&entailL $G:env $phi) "的一个形式证明或推导是一个有限的判断的树, "
            "满足以下条件:"
            (Ul (Li "根标签是" (&entailL $G:env $phi) ";")
                (Li "所有的叶子都是公理, 例如具有形式"
                    (&entailL (&cm $G:env $phi) $phi) "的判断;")
                (Li "每个母结点的标签都由其女之标签由图2.1的规则得到."))
            "如果这样一个证明存在, 那么我们称" (&entailL $G:env $phi)
            "是可证明的或可推导的, 记作" (: $G:env (_ $entailL $N) $phi)
            ". 对于无限的" $G:env ", " (: $G:env (_ $entailL $N) $phi)
            "的意思是对于某个" $G:env "的有限子集" (_ $G:env $0)
            "有" (: (_ $G:env $0) (_ $entailL $N) $phi) ".")
        (Li "通常我们省略" (_ $entailL $N) "中的标记" (_ $ $N)
            ". 注意到若是如此那么记号" (&entailL $G:env $phi)
            "就过载了. 它既表达一个判断的可证明性, 也代表判断本身. 然而, "
            "其用意在上下文中总是明确的.")
        (Li "如果" (&entailL $ $phi) ", 那么我们称" $phi "是一个定理."))
    
    )
   (H3 "第2.3节 古典逻辑的代数语义")
   ((definition #:n "2.3.1")
    "一个格是一个偏序" (tu0 $A i.<=) ", 其满足对于每个" $A
    "的二元素子集" (setE $a $b) ", " $A "中存在它的最小上界和最大下界. "
    "我们记" (ap (_ (Mi "sup") $A) (setE $a $b)) "为" (&sqcup $a $b) ", "
    (ap (_ (Mi "inf") $A) (setE $a $b)) "为" (&sqcap $a $b)
    ". 与集合论的运算相类比, 我们称" $sqcup "为并 (或者join), "
    $sqcap "为交 (或者meet). 格的top (相应地, bottom) 若存在, "
    "通常被记为" $1 " (相应地, " $0 ").")
   ((lemma #:n "2.3.2")
    "在一个格中, 以下条件等价:"
    (Ol #:attr* '((type "i"))
        (Li (&<= $a $b) ";")
        (Li (&= (&sqcap $a $b) $a) ";")
        (Li (&= (&sqcup $a $b) $b) ".")))
   ((example #:n "2.3.3")
    "每个线序, 例如真值的集合" $BB ", 是一个格. 每个在集合的并与交下"
    "封闭的集族也是一个格. 但是, 相对于" i.cup "和" i.cap
    "的封闭性并不是集族成为格的必要条件. 一个例子是Euclid平面的"
    "所有凸子集的族. (一个集合" $A "是凸的当且仅当对于所有的"
    (&in (&cm $a $b) $A) ", 连接" $a "与" $b "的线段被包含在" $A
    "中.)")
   ((lemma #:n "2.3.4")
    "以下等式在每个格中都是成立的:"
    (Ol #:attr* '((type "i"))
        (Li (&= (&sqcup $a $a) $a) "和" (&= (&sqcap $a $a) $a) ";")
        (Li (commute &sqcup $a $b) "和" (commute &sqcap $a $b) ";")
        (Li (associate &sqcup $a $b $c) "和"
            (associate &sqcap $a $b $c) ";")
        (Li (&= (&sqcap (@ (&sqcup $a $b)) $a) $a) "和"
            (&= (&sqcup (@ (&sqcap $a $b)) $a) $a) ".")))
   ((definition #:n "2.3.5")
    (Ol #:attr* '((type "i"))
        (Li "格" $A "被称为分配的, 如果以下等式在" $A "中成立:"
            (Ol (Li (distributeL $a &sqcup $b &sqcap $c) ";")
                (Li (distributeL $a &sqcap $b &sqcup $c) ".")))
        (Li "假定格" $A "拥有top元素" $1 "和bottom元素" $0 ", 我们称" $b
            "是" $a "的补当且仅当" (&= (&sqcup $a $b) $1)
            "且" (&= (&sqcap $a $b) $0) ".")))
   ((lemma #:n "2.3.6")
    "令" $b "是" $a "在某分配格" $A "中的补, 那么" $b "是" $A "中满足"
    (&= (&sqcap $a $b) $0) "的最大元素. 特别地, " $a "最多只有一个补.")
   ((proof)
    "设" (&= (&sqcap $a $c) $0) ", 那么" (&<= $c $b) ", 因为"
    (MB (&= $c (&sqcap $1 $c) (&sqcap (@ (&sqcup $a $b)) $c)
            (&sqcup (@ (&sqcap $a $c)) (@ (&sqcap $b $c)))
            (&sqcup $0 (@ (&sqcap $b $c))) (&sqcap $b $c)) "."))
   ((definition #:n "2.3.7")
    "一个布尔代数是一个带有top和bottom元素的分配格" $B ", 其满足每个" $B
    "的元素" $a "都有一个补 (记作" (&- $a) ").")
   (P "布尔代数经常以形式为" (&= $BBBB (tupa0 $B i.sqcup i.sqcap i.- $0 $1))
      "的代数结构呈现. 在这种情况下, 偏序可以在" (&<= $a $b) "和"
      (&= (&sqcap $a $b) $a) "的等价性 (见引理2.3.2) 的帮助之下被重新构造出来.")
   ((example #:n "2.3.8")
    "令" $X "是任意的集合. 一个集合域 (" $X "上的) 是一个非空的" $X
    "的子集族" $RRRR ", 其在集合论的并, 交, 补 (相对于" $X ") 下封闭. "
    "每个集合域都是一个布尔代数. 集合域的例子如下:"
    (Ol #:attr* '((type "i"))
        (Li (powset $X) ", " $X "的幂集;")
        (Li (setE $empty $X) ";")
        (Li (setI (&sube $A $X) (: $A "有限或者" (&- $X $A) "有限")) "."))
    "注意到真值的代数" $BB " (暂时忘却" i.-> ") 同构于ii.")
   ((theorem #:n "2.3.9")
    "每个布尔代数同构于一个集合域.")
   ((definition #:n "2.3.10")
    "布尔代数" (&= $BBBB (tupa0 $B i.sqcup i.sqcap i.- $0 $1))
    "中的一个赋值是任意的从命题变量" $PV "到" $B
    "的映射" $v ". 一个公式" $phi "相对于赋值" $v
    " (在" $BBBB "中) 的值由归纳定义."
    (MB (set-attr*
         (&Table
          ((val $p) $= (: (app $v $p) ", 对于" (&in $p $PV) ";"))
          ((val $bottom) $= (: $0 ";"))
          ((val (&disj $phi $psi)) $= (: (&sqcup (val $phi) (val $psi)) ";"))
          ((val (&conj $phi $psi)) $= (: (&sqcap (val $phi) (val $psi)) ";"))
          ((val (&-> $phi $psi)) $= (: (&sqcup (&- (val $phi)) (val $psi)) ".")))
         'columnalign "left"))
    "当" (&= (val $phi) $1) "时, 记" (&entailS (&cm $BBBB $v) $phi) ". 若对于所有的" $v
    "有" (&entailS (&cm $BBBB $v) $phi) ", 记" (&entailS $BBBB $phi) ".")
   (P "显然, 这种布尔代数语义是通常二值语义的一种泛化. 的确, 一个公式" $phi
      "是一个古典重言当且仅当" (&entailS $BB $phi) ". 实际上, 这种泛化不是本质性的.")
   ((theorem #:n "2.3.11")
    "一个命题公式" $phi "是一个古典重言当且仅当对于所有的布尔代数" $BBBB
    "有" (&entailS $BBBB $phi) ".")
   ((proof)
    "自右向左是即得的. 为了证明另一个方向, 我们假设对于某个" $BBBB "有"
    (&!entailS $BBBB $phi) ". 根据Stone表示定理我们不妨设" $BBBB
    "是某个" $X "上的一个集合域." (Br)
    "既然" (&!entailS $BBBB $phi) ", 那么存在某个" $BBBB "中的赋值" $v
    "满足" (&!= (val $phi) $X) ". 因此, 存在" (&in $x $X) "满足"
    (&!in $x (val $phi)) ". 定义一个二元赋值" $w " (即" $BB
    "中的一个赋值) 满足" (&= (app $w $p) $1) "当且仅当" (&in $x (val $p))
    ". 根据归纳, 可以证明对于所有的公式" $psi ":"
    (MB (&= (val0 $psi $w) $1) "当且仅当" (&in $x (val $psi)) ".")
    "那么, " (&!= (val0 $phi $w) $1) ".")
   (H3 "第2.4节 Heyting代数")
   (P "现在我们将建立直觉主义命题逻辑的一种语义. 出于这样的目的我们检视了公式相对于"
      "可证明性的代数性质. 我们首先观察到可证明的推论表现得几乎就像一个公式上的序关系, "
      "即它是自反的和传递的. 更确切地说, 对于每个" $G:env "我们有:"
      (Ul (Li (G!- (&-> $phi $phi)) ";")
          (Li "如果" (G!- (&-> $phi $psi)) "且" (G!- (&-> $psi $thetav))
              ", 那么" (G!- (&-> $phi $thetav)) "."))
      "然而, 这个关系并不是反对称的. 但是, 我们可以通过将等价的公式视为等同的来将其"
      "转化为一个偏序关系. 为了使之精确, 我们令" $G:env "是一个固定的命题公式集合 "
      "(特别地, " $G:env "可以为空). 我们按照以下方式定义一个关系" $~G ":"
      (MB (&~G $phi $psi) "当且仅当" (G!- (&-> $phi $psi))
          "且" (G!- (&-> $psi $phi)) ".")
      "不难看出" $~G "是所有公式构成的集合" $Phi:bold "上的一个等价关系, 并且有 ("
      "我们省略" $~G "中的下标" (_ $ $G:env) "):"
      (MB (&= (eqc $bottom $~) (setI $phi (G!- (&neg $phi)))) "且"
          (&= (eqc $top $~) (setI $phi (G!- $phi))) ".")
      
      )
   (H3 "第2.5节 Kripke语义")
   ((definition #:n "2.5.1")
    "一个Kripke模型是一个具有形式"
    (MB (&= $CCCC (tupa0 $C i.<= i.Vdash)))
    "的三元组, 其中" $C "是一个非空集合, 它的元素被称为状态或者可能世界, "
    i.<= "是一个" $C "中的偏序, 而" i.Vdash "是" $C "的元素和命题变量之间"
    "的一个二元关系. 关系" i.Vdash " (读作&quot;力迫&quot;) 必须"
    "满足以下单调性条件."
    (MB "如果" (&<= $c $c^) "且" (&Vdash $c $p) "那么" (&Vdash $c^ $p) ".")
    "直觉在于模型的元素代表了知识的状态. 关系" i.<= "与通过获得更多知识"
    "来扩展状态有关. 关系" i.Vdash "决定了在一个给定的状态下哪些命题变量"
    "被认为是真的. 以下, 我们延拓这个关系来为命题公式提供含义.")
   ((definition #:n "2.5.2")
    "如果" (&= $CCCC (tupa0 $C i.<= i.Vdash)) "是一个Kripke模型, 那么"
    (Ul (Li (&Vdash $c (&disj $phi $psi)) "当且仅当" (&Vdash $c $phi)
            "或" (&Vdash $c $psi) ";")
        (Li (&Vdash $c (&conj $phi $psi)) "当且仅当" (&Vdash $c $phi)
            "且" (&Vdash $c $psi) ";")
        (Li (&Vdash $c (&-> $phi $psi)) "当且仅当对于所有满足"
            (&Vdash $c^ $phi) "的" (&>= $c^ $c) "有"
            (&Vdash $c^ $psi) ";")
        (Li (&Vdash $c $bottom) "并不成立."))
    "注意到上述定义推出了以下否定的法则:"
    (Ul (Li (&Vdash $c (&neg $phi)) "当且仅当对于所有的"
            (&>= $c^ $c) "有" (&nVdash $c^ $phi) "."))
    "记号" i.Vdash "可以按照多种方式被运用. 有时我们记"
    (&Vdash (&cm $CCCC $c) $phi) "以明确使用了哪个模型. "
    "当对于所有的" (&in $c $C) "有" (&Vdash $c $phi) "时, 我们记"
    (&Vdash $CCCC $phi) ". 记号" (&Vdash $c $G:env)
    "的意思是对于所有的" (&in $phi $G:env) "有"
    (&Vdash $c $phi) ", 对于记号" (&Vdash $CCCC $G:env)
    "也是类似的. 最后, 若对于每个Kripke模型" $CCCC "和每个"
    $CCCC "的状态" $c ", 条件" (&Vdash (&cm $CCCC $c) $G:env)
    "能够推出" (&Vdash (&cm $CCCC $c) $phi) ", 那么记"
    (&Vdash $G:env $phi) ".")
   ((lemma #:n "2.5.3")
    "如果" (&<= $c $c^) "且" (&Vdash $c $phi) "那么"
    (&Vdash $c^ $phi) ".")
   ((example #:n "2.5.4")
    "令" (&= $C (setE $c $c^ $c^^)) ", 其中" (&<= $c $c^) "而"
    $c^ "和" $c^^ "是不可比较的. 一个Kripke模型"
    (&= $CCCC (tupa0 $C i.<= i.Vdash)) ", 其中" 
    )
   (H3 "第2.6节 推论片段")
   (H3 "第2.7节 注记")
   (P "数学中的构造主义深深地根植于19世纪, 甚至是更加遥远的过去. 例如, 直觉主义者他们自己"
      "也承认受到Immanuel Kant (1724-1804) 的哲学的启发. 根据Kant, 数学认知的领域诸如"
      "时间和空间可直达人类的直觉而并非经验上所&quot;观察&quot;到的. 因此, 数学可被视为"
      "纯粹的心灵上的构造.")
   (P "Leopold Kronecker (1823-1891) 通常被引为第一个显式将构造主义的想法应用于数学的"
      "作者. 根据Kronecker, 一个数字的正确定义, 可在有限步骤之内被验证. 而一个存在性"
      "陈述的证明, 应该提供一个能够见证这个陈述的显式对象.")
   (P "为了寻找数学的坚实基础, 许多其他人也加入了Kronecker. 19世纪下半叶, 数学正发生着"
      "相当重要的改变. 随着新分支的发展, 包括数理逻辑, 数学研究的主题变得愈发抽象而与"
      "现实经验无关. 数学家的活动从探索&quot;真实&quot;世界的性质转移到了创造一个抽象"
      "世界. 这引发了关于数学基础的重要问题. 随着悖论的发现, 特别是Russell发现的知名悖论, "
      "这些问题变得紧急起来.")
   (P ""
      )
   (H3 "第2.8节 练习")
   (H2 "第3章 简单类型" $lambda "演算")
   (P "在逻辑学中, 判断一个公式相对于某个特定的语义是否有效 (valid) 总是核心议题. "
      "或者, 更一般地, 一集假设是否在所有模型中都蕴涵一个公式. "
      "在古典逻辑的&quot;语义传统&quot;之中, 这种问题总是最重要的主题, 而可靠 (sound) 且"
      "完备 (complete) 的证明系统的构造主要被视为判断有效性的工具. 在这种情况下, "
      "公式和判断的可证明性是和证明相关的问题之中唯一令人感兴趣的. 人们会问证明是否"
      "存在, 但是不必问是哪一个证明.")
   (P "在证明论中, 视角变得有所不同. 我们想要研究证明的结构, 比较各种证明, "
      "从中选出一些, 以与其他的证明进行区分. 这对于构造性逻辑而言是尤为重要的, "
      "其中证明 (构造) 而不是语义是最终的标准.")
   (P "因此, 寻求一种简便的证明记号是非常自然的. 例如, 我们可以用" (&: $M $phi)
      "来表示" $M "是" $phi "的一个证明. 若有额外假设" $G:env
      ", 那么我们或许可以扩展这个记号为"
      (MB (G!- (&: $M $phi)))
      "现在, 如果" $M "和" $N "分别是" (&-> $phi $psi) "和" $phi "的证明, 那么由"
      (@ i.-> "E") "得到的" $psi "的证明可以表示为" (&: (appl (Mi "@") $M $N) $psi)
      ", 或者就简单地记作" (&: (ap $M $N) $psi) ". 这给出了一种&quot;带注解的&quot;"
      (@ i.-> "E") "规则"
      (MB (&rule (G!- (&: $M (&-> $phi $psi))) (G!- (&: $N $phi))
                 (G!- (&: (ap $M $N) $psi))))
      "当试着为" (@ "Ax") "设计一种带注解的版本时, 读者或许会发现给假设命名"
      "也是很方便的事情, 例如"
      (MB (!- (&: $x $phi) (&: $y $psi) (&: $x $phi)))
      "代表使用第一个假设. 这个想法在我们想要注解" (@ i.-> "I") "规则时也派得上用场, "
      "那么discharge一个证明" $M "中的假设" $x "可以被记成诸如"
      (: $sharp $x $phantom. $M) ", " (: $xi $x $phantom. $M) ", ... 我们为什么"
      "不试试lambda呢?"
      (MB (&rule (G!- (&: $x $phi) (&: $M $psi))
                 (G!- (&: (&lam $x $M) (&-> $phi $psi)))))
      "是的, 我们得到的就是lambda记号. 用一位著名作家的话来说, 虽然是在完全不同的上下文中, "
      "那就是相似并非刻意也并非偶然, 而是不可避免的. 的确, 一个推论的一个证明代表了一个"
      "构造, 而根据BHK解释, 一个推论的一个构造是一个函数.")
   (P "然而, 不是每个lambda项都可以被用作证明的记号. 例如, 自应用" (ap $x $x)
      "似乎并不代表任何命题的证明, 不论由" $x "注解的假设为何. 所以, 在我们探索证明和项的"
      "类似之处 (第4章的事情) 之前, 我们必须寻找合适的lambda演算的子系统.")
   (P "正如我们所说, BHK解释将推论的构造和函数视为等同的. 在数学中, 一个函数" $f "总是定义在一个"
      "特定的定义域 (domain) " $A "上, 而值落在一个陪域 (codomain) " $B "中. 这被记作"
      (func $f $A $B) ". 类似地, 一个公式" (&-> $phi $psi) "的一个构造也只能被应用于指定的"
      "参数上来, 也就是前提 (premise) 的构造. 那么, 其结果是结论 (conclusion) 的构造, 它当然"
      "也只能具有特定的类型.")
   (P "在lambda演算中, 我们可以引入类型来描述项的函数行为. 一个应用" (ap $M $N) "只有当" $M
      "具有形式为" (&-> $sigma $tau) "的函数类型而" $N "具有类型" $sigma "时才是可能的. "
      "其结果具有类型" $tau ". 这是一种相当类似于严格定型的编程语言所具有的类型原则.")
   (P "几乎从一开始表达项的函数性的类型赋予的概念就被纳入了组合子逻辑和lambda演算之中, "
      "自那时起各种类型化演算就得到了全面的研究. 本章我们将引入对于类型概念最基本的"
      "形式化: 系统" lambda-> ".")
   (H3 "第3.1节 Curry风格的简单类型" $lambda "演算")
   (P "我们从Curry风格的简单类型" $lambda "演算开始, 其中我们处理和"
      (Cite1 "untyped_lambda-calculus") "相同的通常的lambda项.")
   ((definition #:n "3.1.1")
    (Ol #:attr* '((type "i"))
        (Li "一个推论命题公式被称为一个简单类型. 所有简单类型构成的集合记作" $Phi-> ".")
        (Li "一个环境是具有形式" (setE (&: $x_1 $tau_1) $..h (&: $x_n $tau_n))
            "的一个有限的序对集合, 其中" $x_i "是不同的" $lambda "变量, " $tau_i
            "是类型. 也就是说, 环境是从变量到类型的有限的部分函数. 因此, 如果"
            (&in (@: $x $tau) $G:env) ", 我们也可以将其写成"
            (&= (&G:env $x) $tau) ". 我们也定义:"
            (MB (&= (&dom $G:env)
                    (setI (&in $x $Upsilon:normal)
                          (: (&in (@: $x $tau) $G:env)
                             ", 对于某个" $tau)))
                ";&nbsp;"
                (&= (&rg $G:env)
                    (setI (&in $tau $Phi->)
                          (: (&in (@: $x $tau) $G:env)
                             ", 对于某个" $x))) "."))))
   (P "在其他文献中, 考虑简单类型lambda演算的一种变体是相当常见的, 其中所有的类型都从唯一的一个"
      "类型变量构造而来 (因而其被称为类型常量). 这样一种lambda演算的计算性质类似于我们的" lambda->
      ". 但是, 从&quot;逻辑&quot;角度而言这种限制于一个类型常量的情况并不是同样有趣的, "
      "参见练习4.10.")
   ((notation)
    (&-> $tau $..c $tau $sigma) "被缩略为" (&-> $tau^n $sigma) ", 其中" $tau "出现了" $n
    "次. 环境" (setE (&: $x_1 $tau_1) $..h (&: $x_n $tau_n)) "经常被简记作"
    (&cm (&: $x_1 $tau_1) $..h (&: $x_n $tau_n)) ". 如果"
    (&= (&cap (&dom $G:env) (&dom $G:env^)) $empty) ", 那么我们也将" (&union $G:env $G:env^)
    "记作" (&cm $G:env $G:env^) ". 特别地, " (&cm $G:env (&: $x $tau)) "代表"
    (&union $G:env (setE (&: $x $tau))) ", 其中假定" (&!in $x (&dom $G:env)) ". 类似的约定也"
    "适用于之后的章节.")
   ((definition #:n "3.1.2")
    "一个判断由一个环境, 一个lambda项和一个类型构成, 记作" GMt
    ". 图3.1中的规则定义了系统" lambda-> "的可推导的规则的概念. (读者必须记住规则"
    (@ "Var") "和" (@ "Abs") "中变量" $x "不在" $G:env "的定义域之中.) "
    "如果" GMt "是可推导的, 那么我们称" $M "在" $G:env
    "中具有类型" $tau ", 记作" (: $G:env (_ $entailL lambda->) (&: $M $tau))
    "或者就记成" GMt " (参考定义2.2.1)."
    (&label
     (set-attr*
      (&Table
       ((@ "Var") (G!- (&: $x $tau) (&: $x $tau)))
       ((@ "Abs") (&rule (G!- (&: $x $sigma) (&: $M $tau))
                         (G!- (&: (@lam $x $M) (&-> $sigma $tau)))))
       ((@ "App") (&rule (G!- (&: $M (&-> $sigma $tau))) (G!- (&: $N $sigma))
                         (G!- (&: (@ap $M $N) $tau)))))
      'frame "solid" 'rowspacing "5.0ex")
     "图3.1: 简单类型lambda演算" lambda->))
   ((example #:n "3.1.3")
    "令" (&cm $sigma $tau $rho) "是任意的类型, 那么:"
    (Ol #:attr* '((type "i"))
        (Li (!- (&: $I:bold (&-> $sigma $sigma))) ";")
        (Li (!- (&: $K:bold (&-> $sigma $tau $sigma))) ";")
        (Li (!- (&: $S:bold (&-> (@-> $sigma $tau $rho) (@-> $sigma $tau) $sigma $rho))) ".")))
   (P "一个形式为" (&: $M (&-> $tau $sigma)) "的类型赋予当然可以被解释为&quot;" $M "是一个以" $tau
      "为定义域" $sigma "为陪域的函数&quot;. 但是我们必须明白这里对于&quot;定义域&quot;和&quot;"
      "陪域&quot;的理解不是集合论性质的. 在Curry风格的类型化演算中, 类型被更适切地描述为(由项满足的)"
      "谓词或者规格 (specification) 而不是集合论性质的函数空间. 在集合论中" (func $f $A $B)
      "的含义为" $f "的参数恰是" $A "的元素, 而所有的值都必须属于" $B ". 与之相对地, "
      (&: $M (&-> $tau $sigma)) "仅意味着将" $M "应用于一个类型为" $tau "的参数必须产生一个类型为"
      $sigma "的结果.")
   ((lemma #:n "3.1.4")
    (Ol #:attr* '((type "i"))
        (Li "如果" GMt ", 那么" (&sube (&FV $M) (&dom $G:env)) ".")
        (Li "如果" (G!- (&: $x $tau) (&: $M $sigma)) "并且"
            (&!in $y (&union (&dom $G:env) (setE $x)))
            ", 那么" (G!- (&: $y $tau) (&: (subst $M $x $y) $sigma)) ".")))
   ((proof)
    
    )
   ((lemma #:n "3.1.5")
    (B "(Generation lemma).") " 设" GMt "."
    (Ol #:attr* '((type "i"))
        (Li "如果" $M "是一个变量" $x ", 那么" (&= (&G:env $x) $tau) ".")
        (Li "如果" $M "是一个应用" (ap $P $Q) ", 那么对于某个" $sigma ", "
            (G!- (&: $P (&-> $sigma $tau))) "且" (G!- (&: $Q $sigma)) ".")
        (Li "如果" $M "是一个抽象" (&lam0 $x $N) "并且" (&!in $x (&dom $G:env))
            ", 那么" (&= $tau (&-> $tau_1 $tau_2)) ", 其中"
            (G!- (&: $x $tau_1) (&: $N $tau_2)) ".")))
   ((proof)
    
    )
   ((lemma #:n "3.1.6")
    (Ol #:attr* '((type "i"))
        (Li "如果" (G!- (&: $M $sigma)) "并且对于所有的" (&in $x (&FV $M))
            "有" (&= (&G:env $x) (app (&prime $G:env) $x))
            ", 那么" (!- (&prime $G:env) (&: $M $sigma)) ".")
        (Li "如果" (G!- (&: $x $tau) (&: $M $sigma)) "并且"
            (G!- (&: $N $tau)) ", 那么" (G!- (&: Mx:=N $sigma)) ".")))
   ((proof)
    "我们对于" $M "的大小进行归纳. "
    )
   ((theorem #:n "3.1.7. (Subject reduction)")
    "如果" (G!- (&: $M $sigma)) "并且" (&->>beta $M $N) ", 那么"
    (G!- (&: $N $sigma)) ".")
   ((proof)
    
    )
   ((definition #:n "3.1.8")
    "将类型" $sigma "中的类型变量" $p "替换为类型" $tau ", 记作"
    (subst $sigma $p $tau) ", 被定义为:"
    (MB (Tstyle1
         (&Table
          ((subst $p $p $tau) $= (: $tau ";"))
          ((subst $q $p $tau) $= (: $q ", 如果" (&!= $q $p) ";"))
          ((subst (@-> $sigma_1 $sigma_2) $p $tau)
           $= (: (&-> (subst $sigma_1 $p $tau) (subst $sigma_2 $p $tau)) ".")))))
    "记号" (subst $G:env $p $tau) "代表"
    (setI (@: $x (subst $sigma $p $tau)) (&in (@: $x $tau) $G:env))
    ", 类似的记号还可应用于等式, 等式的集合, 诸如此类.")
   ((proposition #:n "3.1.9")
    "如果" (G!- (&: $M $sigma)) ", 那么"
    (!- (subst $G:env $p $tau) (&: $M (subst $sigma $p $tau))) ".")
   (H3 "第3.2节 类型重构算法")
   (P "一个项" (&in $M $Lambda:normal) "是可定型的, 如果存在" $G:env "和" $sigma
      "满足" (G!- (&: $M $sigma)) ". 可定型项的集合是所有" $lambda "项的集合的一个真子集. "
      "因此, 判断系统" lambda-> "中到底哪些项可被赋予类型以及如何有效地找出这些类型成了"
      "根本性的问题. 实际上, 从对于三元谓词&quot;" GMt "&quot;的分析之中我们可以提出许多"
      "判定问题. 以下的定义对于每个能够导出具有这种形式的判断的类型赋予系统都是有意义的.")
   ((definition #:n "3.2.1")
    (Ol #:attr* '((type "i"))
        (Li "类型检查问题指的是对于一个给定的环境" $G:env ", 一个项" $M "和一个类型"
            $tau ", 判断" GMt "是否成立.")
        (Li "可定型问题, 又称类型重构问题, 指的是判断一个给定项" $M "是否是可定型的.")
        (Li "类型居留问题, 又称类型是否为空问题, 指的是对于一个给定的类型" $tau
            ", 判断是否存在一个封闭项" $M "满足" (!- (&: $M $tau)) "成立. (那么, 我们称"
            $tau "是非空的, 并拥有居民" $M ".)")))
   (P "类型居留问题将在第4章讨论, 本节我们先考虑可定型问题和类型检查问题. 第一眼看上去, "
      "似乎判断一个给定项在一个给定环境之中是否拥有一个给定的类型可能要比判断它到底是否"
      "拥有类型来得容易. 然而, 这种印象一般而言是错误的. 对于许多类型赋予系统而言, "
      "可定型问题很容易被规约为类型检查问题. 的确, 为了判断一个项" $M "是否是可定型的, 其中"
      (&= (&FV $M) (setE $x_1 $..h $x_n)) ", 我们可以问是否有"
      (MB (!- (&: $x_0 $p) (&: (ap* $K:bold $x_0 (@ (&lam $x_1 $..h $x_n $M))) $p)))
      "而这就将可定型问题化为了类型检查问题. 实际上, 在简单类型的情况下, 这两者是等价的 "
      "(练习3.11), 尽管将后一个问题规约为前一个问题就不那么简单了. 但是, 对于某些类型赋予系统, "
      "这两个问题并非等价: 比较一下命题13.4.3和定理13.4.4.")
   (P "现在我们展示可定型问题是如何能够被规约为仅包含二元函数符号" i.-> "的签名上的"
      "合一 (unification) 的. 这个签名上的项被视为与简单类型是等同的. 对于每个项" $M
      "我们根据归纳定义"
      (Ul (Li "一个方程组" $E_M ";")
          (Li "一个类型" $tau_M "."))
      "想法如下: " $E_M "有解当且仅当" $M "是可定型的, 而" $tau_M "是" $M "的 (非形式化的) "
      "类型的模式. 出现在" $E_M "中的类型变量 (未知元) 分为两种: 其中一些对应于" $M
      "的自由变量" $x "的类型, 记作" $p_x ", 而其他变量是辅助性的.")
   ((definition #:n "3.2.2")
    (Ol #:attr* '((type "i"))
        (Li "如果" $M "是一个变量" $x ", 那么" (&= $E_M $empty) "且" (&= $tau_M $p_x)
            ", 其中" $p_x "是一个新鲜 (fresh) 的类型变量.")
        (Li "令" $M "是一个应用" (ap $P $Q) ". 首先, "
            )
        )
    )
   (H3 "第3.3节 Church风格的简单类型" $lambda "演算")
   ((definition #:n "3.3.5")
    (&label
     (set-attr*
      (&Table
       ((@ "Var")
        (G!- (&: $x $tau) (&: $x $tau)))
       ((@ "Abs")
        (&rule (G!- (&: $x $sigma) (&: $M $tau))
               (G!- (&: (@ (&lam (&: $x $sigma) $M))
                        (&-> $sigma $tau)))))
       ((@ "App")
        (&rule (G!- (&: $M (&-> $sigma $tau)))
               (G!- (&: $N $sigma))
               (G!- (&: (@ (ap $M $N)) $tau)))))
      'frame "solid" 'rowspacing "5.0ex")
     "图3.2: Church风格的简单类型lambda演算")
    )
   (H3 "第3.4节 Church定型 vs Curry定型")
   (H3 "第3.5节 正规化")
   (H3 "第3.6节 Church-Rosser性质")
   (H3 "第3.7节 可表达性")
   (H3 "第3.8节 注记")
   (P "类型经常被视为一种避免无类型世界中因各种各样的自应用而产生的悖论的方法. 无疑, "
      "悖论给予了20世纪之初创造各种类型理论的动力. 然而, 正如"
      (Cite0 "simple_theory_of_types" "types_before_1940")
      "所指出的那样, 在数学中将对象划分为不同的范畴或者&quot;类型&quot;是自然的, "
      "并且远在这些悖论被发现之前.")
   (P "形式类型论的历史起源于Russell. Chwistek, Ramsey, Hilbert等其他人的工作对于该主题的"
      "建立做出了贡献. 对于简单类型论的一份有影响力的呈现由1940年Church的论文"
      (Cite0 "formulation_of_simple_theory_of_types") "给出. 为此Church引入了简单类型"
      "lambda演算, 其为他的类型论的核心语言.")
   (P "组合子逻辑的类型化版本在数年之前由Curry提出, 在1934年的论文"
      (Cite0 "functionality_in_combinatory_logic") "中, 尽管Curry肯定在1928年就已经有"
      "想法了, 见" (Cite0 "Currys_anticipation_of_types" "lambda-calculus_and_combinatory_logic")
      ". Curry完整的&quot;theory of functionality&quot;后来被证明是不一致的"
      (Cite0 "inconsistency_of_full_theory_of_combinatory_functionality") "但是随即就由"
      (Cite0 "consistency_of_theory_of_functionality") "修正. 很快类型就成为组合子理论和"
      "lambda演算中的标准概念.")
   (P "之后类型被证明在编程语言中很有用. 就像无类型的" $lambda "演算为无类型的编程语言"
      "提供了基础, 各种类型化的" $lambda "演算为带有类型的编程语言提供了基础. 实际上, 诸如ML "
      (Cite0 "ML_for_working_programmer") "等语言的设计启发了类型检查问题和可定型问题的研究. "
      "然而, 正如" (Cite "[" (Ref "basic_simple_type_theory") ", pp. 103-104]")
      "所指出的那样, 类型重构算法的主要想法可以追溯至1920年代. "
      )
   (H3 "第3.9节 练习")
   (H2 "第4章 Curry-Howard同构")
   (P "既已在第2章讨论了直觉主义逻辑, 在第3章讨论了带类型的lambda演算, 现在开始我们将关注"
      "这二者之间的关系, 也就是Curry-Howard同构. "
      )
   (H3 "第4.1节 证明和项")
   
   (H3 "第4.2节 类型居留")
   (H3 "第4.3节 并非确切的同构")
   (H3 "第4.4节 证明正规化")
   (H3 "第4.5节 和与积")
   (H3 "第4.6节 证明者怀疑者对话")
   (P "回忆一下第2章的BHK解释. 我们可以将一个公式的构造的建立过程想象成一个证明者和一个怀疑者"
      "之间的对话, 证明者产生构造, 而怀疑者质疑构造是否存在.")
   (P (B "证明者1:") " 我断言" (&-> (@-> (@-> $p $p) (@-> $q $r $q) $s) $s)
      "成立." (Br)
      (B "怀疑者1:") " 真的吗? 让我们假设我给了你一个" (&-> (@-> $p $p) (@-> $q $r $q) $s)
      "的构造, 那么你能给我一个" $s "的构造吗?" (Br)
      (B "证明者2:") " 通过将你的构造应用于" (&-> $p $p) "的一个构造, 然后再将结果应用于"
      (&-> $q $r $q) "的一个构造, 我就能得到你想要的东西." (Br)
      (B "怀疑者2:") " 嗯...你做出了两个新的断言. 我最怀疑第一个. 你确定你有" (&-> $p $p)
      "的一个构造吗? 假设我给你一个" $p "的构造, 你能回我一个" $p "的构造吗?" (Br)
      (B "证明者3:") " 用相同的构造就可以了!")
   (P "对话自证明者作出断言开始. "
      )
   (H3 "第4.7节 带有谬的证明者怀疑者对话")
   (H3 "第4.8节 注记")
   (H3 "第4.9节 练习")
   (H2 "第5章 证明作为组合子")
   (P "之前的章节中我们重点关注了自然演绎证明和" $lambda "项之间的Curry-Howard同构. "
      "正如我们所看到的, 这个同构联系起了两个世界的许多概念. 本章我们呈现一个相关的"
      "另外两种基础概念之间的对应, 即Hilbert风格的证明和组合子逻辑的项.")
   (P ""
      )
   (H3 "第5.1节 Hilbert风格的证明")
   (P "自然演绎在历史上并非形式化证明概念的第一种方式, 它也不是得到最广泛使用的一种. "
      "许多数理逻辑的古典呈现, 例如[], 使用了更传统的Hilbert风格的证明的概念. "
      "通常一个Hilbert风格的证明系统由一集公式 (被称为逻辑公理) 和寥寥几条证明规则构成. "
      "而且, Hilbert风格的证明系统中的形式证明在传统上被定义为公式的序列, 而不是树结构的推导.")
   (P "本章我们考虑的系统, 正如其他大部分命题逻辑的系统, 仅包含一条证明规则, 其被称为"
      "modus ponens或者分离规则:"
      (MB (&rule $phi (&-> $phi $psi) $psi))
      "因此, 一个Hilbert风格的证明系统可以被视为与其逻辑公理的集合等同. 以下的定义适用于"
      "所有这样的命题系统.")
   ((definition #:n "5.1.1")
    "一个公式" $phi "的一个证明序列 (或者说一个Hilbert风格的证明) 是一个公式的有限序列"
    (&cm $psi_1 $psi_2 $..h $psi_n) "满足" (&= $psi_n $phi) ", 并且对于所有的"
    (&= $i (&cm $1 $..h $n)) ","
    (Ul (Li "要么" $psi_i "是一条公理,")
        (Li "要么存在" (&< (&cm $j $l) $i) "满足" (&= $psi_j (&-> $psi_l $psi_i))
            " (即" $psi_i "是由" $psi_j "和" $psi_l "通过modus ponens得到的)."))
    "如果存在这样一个证明, 那么公式" $phi "就被称为一个定理.")
   ((definition #:n "5.1.2")
    
    )
   ((definition #:n "5.1.5")
    
    )
   (&label
    (set-attr*
     (&Table
      ((@ "Id") (G!- $phi $phi))
      ((@ "Ax") (: (G!- $phi) ", 如果" $phi "是一条逻辑公理"))
      ((@ "MP") (&rule (!- $G:env_1 $alpha)
                       (!- $G:env_2 (&-> $alpha $beta))
                       (!- $G:env_1 $G:env_2 $beta))))
     'frame "solid" 'columnalign "left" 'rowspacing "5.0ex" 'columnspacing "12.0ex")
    "图5.1: Hilbert风格系统的树变体")
   
   (H3 "第5.2节 组合子逻辑")
   (P "Hilbert风格的Curry-Howard等价物该是什么呢? "
      )

   ((definition #:n "5.2.1")
    "组合子项的集合" $CCCC "定义如下:"
    (Ul (Li "所有的对象变量都在" $CCCC "之中, 即" (&sube $Upsilon:normal $CCCC) ".")
        (Li "常量" $K:sans-serif "和" $S:sans-serif "都在" $CCCC "中.")
        (Li "如果" $G "和" $H "在" $CCCC "中, 那么应用" (@ (ap $G $H))
            "也在" $CCCC "中."))
    
    )
   
   (H3 "第5.3节 带类型的组合子")
   (H3 "第5.4节 组合子 vs lambda项")
   (H3 "第5.5节 外延性")
   (H3 "第5.6节 ")
   (H3 "第5.7节 ")
   (H3 "第5.8节 ")
   (H2 "第6章 古典逻辑和控制运算符")
   (P "在之前的章节中, 我们已经遇到了化身为各种面目的Curry-Howard同构. "
      "每一种都陈述了某个计算性演算和一个形式逻辑系统之间的对应. "
      "到目前为止, 这些形式逻辑系统都是直觉主义的. 例如, 没有一个能够"
      "推导在古典的逻辑中可以看到的双重否定消去原理"
      (&-> (&neg (&neg $phi)) $phi) "." (Br)
      "这并非巧合. 事实上, 到大约1990年为止, 许多人相信&quot;并不存在古典逻辑的"
      "Curry-Howard同构&quot;. 然而, 那个时候Timothy Griffin发现了双重否定消除原理"
      "对应于编程语言理论中众所周知的一个控制运算符的定型. 很快这个想法就经由"
      "Chet Murthy得到了推广和完善." (Br)
      "本章呈现了这个发现以及它的一些推论, 例如Kolmogorov从古典逻辑到直觉主义逻辑的"
      "双重否定嵌入对应于延续传递风格转换.")
   (H3 "第6.1节 古典命题逻辑")
   (&label
    (set-attr*
     (&Table
      ((: (G!- $phi $phi) (@ "Ax")))
      ((: (: (&rule (G!- $phi $psi) (G!- (&-> $phi $psi))) (@ i.-> "I"))
          (&space 32) (: (&rule (G!- (&-> $phi $psi)) (G!- $phi) (G!- $psi)) (@ i.-> "E"))))
      ((: (&rule (G!- (&-> $phi $bottom) $bottom) (G!- $phi)) (@ i.neg "E"))))
     'frame "solid" 'rowspacing "5.0ex")
    "图6.1: 古典命题演算")
   ((example #:n "6.1.2")
    (Ol #:attr* '((type "i"))
        (Li "以下是公式" (&-> (@-> $phi $psi) (@-> (&neg $phi) $psi) $psi)
            " (&quot;分类讨论证明&quot;) 的一个推导, 令"
            (&= $G:env (setE (&-> $phi $psi) (&-> (&neg $phi) $psi) (&neg $psi))) "."
            (MB (&rule*
                 (&rule
                  (G!- (&neg $psi))
                  (&rule
                   (G!- (&-> (&neg $phi) $psi))
                   (&rule
                    (&rule (G!- $phi (&neg $psi))
                           (&rule (G!- $phi (&-> $phi $psi))
                                  (G!- $phi $phi)
                                  (G!- $phi $psi))
                           (G!- $phi $bottom))
                    (G!- (&neg $phi)))
                   (G!- $psi))
                  (G!- $bottom))
                 (!- (&-> $phi $psi) (&-> (&neg $phi) $psi) $psi)
                 (!- (&-> $phi $psi) (&-> (@-> (&neg $phi) $psi) $psi))
                 (!- (&-> (@-> $phi $psi) (@-> (&neg $phi) $psi) $psi)))))
        (Li "以下是Peirce律的推导, 令" (&= $G:env (setE (&-> (@-> $phi $psi) $phi))) "."
            (MB (&rule*
                 (&rule
                  (G!- (&neg $phi) (&neg $phi))
                  (&rule
                   (G!- (&neg $phi) (&-> (@-> $phi $psi) $phi))
                   (&rule* (&rule (G!- (&neg $phi) $phi (&neg $psi) (&neg $phi))
                                  (G!- (&neg $phi) $phi (&neg $psi) $phi)
                                  (G!- (&neg $phi) $phi (&neg $psi) $bottom))
                           (G!- (&neg $phi) $phi $psi)
                           (G!- (&neg $phi) (&-> $phi $psi)))
                   (G!- (&neg $phi) $phi))
                  (G!- (&neg $phi) $bottom))
                 (G!- $phi)
                 (!- (&-> (@-> (@-> $phi $psi) $phi) $phi)))))))
   
   (H3 "第6.2节 " (: $lambda $mu) "演算")
   (H3 "第6.3节 ")
   (H3 "第6.4节 逻辑嵌入和CPS转换")
   (P "在直觉主义逻辑之中解释古典逻辑, 被称为逻辑嵌入, 自1920年代已被研究. "
      "本节我们研究这样一个嵌入, 其被称为Kolmogorov双重否定转换, 它有一个"
      "有趣的计算性解释.")
   ((definition #:n "6.4.1")
    (B "(Kolmogorov转换).") " 按如下方式定义一个转换, 其中" $alpha
    "遍历原子命题:"
    (MB (set-attr*
         (&Table
          ((app $k $alpha) $= (&neg (&neg $alpha)))
          ((app $k (&-> $phi $psi))
           $= (&neg (&neg (@-> (app $k $phi) (app $k $psi))))))
         'columnalign "left"))
    "而且, " (&= (app $k $G:env) (setI (app $k $phi) (&in $phi $G:env))) ".")
   
   (H3 "第6.5节 古典证明者怀疑者对话")
   (H3 "第6.6节 纯推论片段")
   (H3 "第6.7节 合取与析取")
   (H3 "第6.8节 注记")
   (H3 "第6.9节 练习")
   (H2 "第7章 相继式演算")
   (H3 "第7.1节 Gentzen的相继式演算LK")
   (H3 "第7.2节 LK的片段 vs 自然演绎")
   (H3 "第7.3节 Gentzen的Hauptsatz")
   (H3 "第7.4节 切消 vs 正规化")
   (H3 "第7.5节 Lorenzen对话")
   (H3 "第7.6节 注记")
   (H3 "第7.7节 练习")
   (H2 "第8章 一阶逻辑")
   (H3 "第8.1节 一阶逻辑的句法")
   (H3 "第8.2节 非形式化的语义")
   (H3 "第8.3节 证明系统")
   (H3 "第8.4节 古典语义")
   (H3 "第8.5节 直觉主义逻辑的代数语义")
   (H3 "第8.6节 Kripke语义")
   (H3 "第8.7节 lambda演算")
   (H3 "第8.8节 不可判定性")
   (H3 "第8.9节 注记")
   (H3 "第8.10节 练习")
   (H2 "第9章 一阶算术")
   (H3 "第9.1节 算术的语言")
   (H3 "第9.2节 Peano算术")
   (H3 "第9.3节 Gödel的定理")
   (H3 "第9.4节 ")
   (H3 "第9.5节 Heyting算术")
   (H3 "第9.6节 Kleene的可实现性解释")
   (H3 "第9.7节 注记")
   (H3 "第9.8节 练习")
   (H2 "第10章 Gödel的系统T")
   (H3 "第10.1节 从Heyting算术到系统T")
   (H3 "第10.2节 句法")
   (H3 "第10.3节 ")
   (H3 "第10.4节 ")
   (H3 "第10.5节 ")
   (H3 "第10.6节 ")
   
   (H2 "第11章 二阶逻辑和多态")
   
   (H2 "第12章 二阶算术")
   
   (H2 "第13章 依赖类型")
   
   (H2 "第14章 纯类型系统和" $lambda "立方")
   
   (H2 "附录A 数学背景")
   (H3 "第A.1节 集合论")
   (H3 "第A.2节 代数与合一")
   (P "一个签名是一族函数与关系符号, 其中每个都拥有固定的元数 (可能为零). "
      "零元符号被称为常量. 若不加另外说明, 签名则被认为是有限的.")
   (P "假定有一个固定的独立变量的无限集合" (setE $a_0 $a_1 $..h)
      ". 一个签名" $Sigma:normal "上的一个代数项 (或者就称为项) 要么是一个独立变量, "
      "要么是具有形式" (@ $f $t_1 $..h $t_n) "的表达式, 其中" $f "是一个" $n
      "元函数符号, 而" (&cm $t_1 $..h $t_n) "是项. 当书写项的时候, 我们经常省略"
      "最外面的括号. 符号" (&FV $t) "代表了所有出现在项" $t "中的独立变量的集合. "
      "项" $t "是封闭的当且仅当" (&= (&FV $t) $empty) ".")
   (P "一个代数项的形式定义牵扯函数符号的前缀应用. 当然, 从传统上来说一些二元函数符号"
      "是以中缀风格书写的, 正常情况下我们就这么做. 我们最喜爱的签名仅包含(中缀)箭头"
      "作为唯一的符号. 不难看出这个签名上的代数项可以被视为与简单类型等同, 或者你喜欢"
      "的话也可以说是与推论公式等同.")
   (P "本节我们仅考虑代数签名, 即不包含关系符号的签名. 这样一个签名"
      (&= $Sigma:normal (setE $f_1 $..h $f_n)) " (其中" $f_i "的元数是" $r_i
      ") 上的一个代数是一个集合" $A "以及函数" (func (_^ $f $i $A) (^ $A $r_i) $A)
      ", 即一个系统" (&= $AAAA (tupa0 $A (_^ $f $1 $A) $..h (_^ $f $n $A))) ". "
      )
   (H3 "第A.3节 部分递归函数")
   
   (H2 "附录B 部分练习的解答和提示")
   
   (H2 "参考文献")
   (Bib #:id "explicit_substitutions"
        "M. Abadi, L. Cardelli, P.-L. Curien, and J.-J. Levy. Explicit substitutions. "
        "Journal of Functional Programming, 1(4):375-416, 1991.")
   (Bib #:id "predicative_strong_normalisation_proof_for_lambda-calculus_with_interleaving_inductive_types"
        "A. Abel and T. Altenkirch. A predicative strong normalisation proof for "
        "a " $lambda "-calculus with interleaving inductive types. In T. Coquand, P. Dybjer, "
        "B. Nordstr&ouml;m, and J. Smith, editors, Types for Proofs and Programs, "
        "International Workshop TYPES'99, volume 1956 of Lecture Notes in Computer "
        "Science, pages 21-40. Springer-Verlag, 2000.")
   (Bib #:id "sicp"
        "H. Abelson and G.J. Sussman with J. Sussman. The Structure and Interpretation "
        "of Computer Programs. MIT Press, second edition, 1996.")
   (Bib #:id "computational_interpretations_of_linear_logic"
        "S. Abramsky. Computational interpretations of linear logic. Theoretical "
        "Computer Science, lll(l-2):3-57, 1993.")
   (Bib #:id "games_and_full_completeness_for_multiplicative_linear_logic"
        "S. Abramsky and R. Jagadeesan. Games and full completeness for "
        "multiplicative linear logic. Journal of Symbolic Logic, 59(2):543-574, 1994.")
   (Bib #:id "logic_of_mathematics"
        "Z. Adamowicz and P. Zbierski. Logic of Mathematics. A Modern Course of "
        "Classical Logic. John Wiley & Sons, 1997.")
   (Bib #:id "compiling_with_continuations"
        "A. Appel. Compiling with Continuations. Cambridge University Press, 1992.")
   (Bib #:id "types_for_proofs_and_programs"
        "H. Barendregt and T. Nipkow, editors. Types for Proofs and Programs, "
        "International Workshop TYPES'93, volume 806 of Lecture Notes in Computer "
        "Science. Springer-Verlag, 1994.")
   (Bib #:id "postulates_for_the_foundation_of_logic1"
        "A. Church. A set of postulates for the foundation of logic. "
        "Annals of Mathematics, 33(2):346-366, 1932.")
   (Bib #:id "postulates_for_the_foundation_of_logic2"
        "A. Church. A set of postulates for the foundation of logic. (Second paper.) "
        "Annals of Mathematics, 34(4):839-864, 1933.")
   (Bib #:id "simple_theory_of_types"
        "R.O. Gandy. The simple theory of types. In R.O. Gandy and J.M.E. Hyland, "
        "editors, Logic Colloquium 76, volume 87 of Studies in Logic and the "
        "Foundations of Mathematics, pages 173-181. North-Holland, 1977.")
   (Bib #:id "types_before_1940"
        "F. Kamareddine, T. Laan, and R. Nederpelt. Types in logic and mathematics "
        "before 1940. Bulletin of Symbolic Logic, 8(2):185-245, 2002.")
   (Bib #:id "formulation_of_simple_theory_of_types"
        "A. Church. A formulation of the simple theory of types. "
        "Journal of Symbolic Logic, 5(2):56-68, 1940.")
   (Bib #:id "functionality_in_combinatory_logic"
        "H.B. Curry. Functionality in combinatory logic. Proceedings of the National "
        "Academy of Science USA, 20(ll):584-590, 1934.")
   (Bib #:id "inconsistency_of_full_theory_of_combinatory_functionality"
        "H.B. Curry. The inconsistency of the full theory of combinatory functionality. "
        "Journal of Symbolic Logic, 20(1):91, 1955.")
   (Bib #:id "consistency_of_theory_of_functionality"
        "H.B. Curry. Consistency of the theory of functionality. Journal of Symbolic "
        "Logic, 21(1):110, 1956.")
   (Bib #:id "ansi_common_lisp"
        "P. Graham. ANSI Common Lisp. Prentice-Hall, 1995.")
   (Bib #:id "higher-order_abstract_syntax"
        "F. Pfenning and C. Elliot. Higher-order abstract syntax. "
        "In Programming Language Design and Implementation, pages 199-208. ACM Press, 1988.")
   (Bib #:id "first-order_theory_of_names_and_binding"
        "A. Pitts. Nominal logic: A first order theory of names and binding. "
        "Information and Computation, 186(2):165-193, 2003.")
   (Bib #:id "Currys_anticipation_of_types"
        "J.P. Seldin. Curry's anticipation of the types used in programming languages. "
        "In Proceedings of the Annual Meeting of the Canadian Society for History and "
        "Philosophy of Mathematics, Toronto, 24-26 May 2002, pages 148-163, 2003.")
   (Bib #:id "lambda-calculus_and_combinatory_logic"
        "J.P. Seldin. Church and Curry: The lambda calculus and combinatory logic. "
        "In Gabbay and Woods [159]. To appear.")
   (Bib #:id "basic_simple_type_theory"
        "J.R. Hindley. Basic Simple Type Theory, volume 42 of Cambridge Tracts in "
        "Theoretical Computer Science. Cambridge University Press, 1997.")
   (Bib #:id "ML_for_working_programmer"
        "L.C. Paulson. ML for the Working Programmer. Cambridge University Press, "
        "second edition, 1996.")
   (Bib #:id "closure_under_alpha-conversion"
        "R. Pollack. Closure under alpha-conversion. In Barendregt and Nipkow "
        (Cite0 "types_for_proofs_and_programs")
        ", pages 313-332.")
   ))