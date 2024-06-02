#lang racket
(provide automated_ch3_part1)
(require (except-in
          SMathML
          H3. H4. $!= &~ Lemma Corollary Definition
          Remark Theorem Example Proposition Warning)
         "automated_utils.rkt")
(define automated_ch3_part1
  (Tm*
   (H2. "一阶逻辑")
   (P "我们现在从命题逻辑移至更为丰富的一阶逻辑, "
      "其中命题可以牵涉全称或者存在量化的非命题变量. "
      "我们将会展现一阶逻辑之中的证明是如何可以"
      "藉由Herbrand定理被朴素地机械化的. "
      "接着我们将会引入诸多改进, "
      "特别是合一 (unification), "
      "其可以使得自动化证明更加高效.")
   (H3. "一阶逻辑及其实现")
   (P "命题逻辑只允许我们从原始命题构建公式, "
      "原始命题本身可以独立地为真或者为假. "
      "然而, 这太过受限, 很难捕获命题的真假依赖于"
      (Em "非命题") "变量的值这种情况的推理模式. "
      "例如, 一个典型的关于数字的命题是" (Q (&< $m $n))
      ", 而其真值依赖于" $m "和" $n
      "的值. 如果我们只是为每个这样的命题引入一个不同的命题变量, "
      "那么我们就失去了根据其所含变量将不同实例相互关联的能力, "
      "例如断言" (&neg (@conj (&< $m $n) (&< $n $m)))
      ". {译注: 也就是说, 例如这里的" $m "和" $n
      "是在两个命题之间共享的, 由此建立联系.} "
      (Em "一阶(谓词)逻辑")
      "以两种方式扩展了命题逻辑以适应这种需求:"
      (Ul (Li "原子命题可以使用函数和谓词由非命题变量和常量构建而成;")
          (Li "非命题变量可以被" (Em "量词") "所" (Em "绑定") ".")))
   (P "我们对于" (Em "公式") " (从直觉上来说意在为真为假) 和"
      (Em "项") " (意在指称进行推理的论域中的" (Q "对象")
      ") 进行句法上的区分. 项是由(对象指称)变量通过函数构建的. "
      "{译注: 更准确地说, 应该是函数符号.} "
      "在讨论中, 我们使用记号" (appl $f $s $t $u)
      "代表由子项" (&cm $s $t $u) "使用函数" $f
      "构建的项, 或者有时使用中缀记号" (&+ $s $t) "而非"
      (appl $+ $s $t) ", 如果中缀记号更自然或者令人熟悉. "
      "所有这些符号都只应该理解为对于项的潜在抽象句法的呈现, "
      "这里的项要么是一个变量, 要么是一个函数应用于任意数目的其他"
      (Q "参数") "项:"
      (CodeB "type term = Var of string
          | Fn of string * term list;;"))
   (P "函数可以拥有任意数目的参数, 而这个数目被称为是函数的"
      (Em "元数(arity)") " (from a pun on the words unary, "
      "binary, ternary, quaternary, etc.). "
      "特别地, 我们可以将诸如" $1 "或者" $pi
      "这样的常量归为" (Em "零元")
      "函数, 即具有零个参数的函数. "
      "绝大多数数学表达式都可以相当直接地形式化为项, 例如"
      (Msqrt (&- $1 (app (^ $cos $2) (&+ $x $y))))
      "可以翻译为:"
      (CodeB "Fn(&quot;sqrt&quot;,[Fn(&quot;-&quot;,[Fn(&quot;1&quot;,[]);
                   Fn(&quot;power&quot;,[Fn(&quot;cos&quot;,[Fn(&quot;+&quot;,[Var &quot;x&quot;; Var &quot;y&quot;])]);
                               Fn(&quot;2&quot;,[])])])]);;"))
   (P "命题逻辑的所有逻辑联结词都可以延续到一阶逻辑之中. "
      "然而, 现在每个原子命题都被分析为了一个命名的"
      (Em "谓词") "或者说" (Em "关系")
      "应用于任意有限数目的项. 又一次, 对于谓词" $P
      "应用于参数" $s "和" $t "我们记" (appl $P $s $t)
      ", 但是有时也使用中缀记号" (&< $s $t) "而非"
      (appl $< $s $t) ", 如果这看起来更为自然. "
      "我们创建一个新的类型" (Code "fol")
      ", 其表示一阶原子命题, "
      "由此对于一阶公式我们有了自然的"
      (Code "fol formula") "类型:"
      (CodeB "type fol = R of string * term list;;")
      "例如, " (&< (&+ $x $y) $z)
      "可以形式化为以下的原子公式:"
      (CodeB "Atom(R(&quot;&lt;&quot;,[Fn(&quot;+&quot;,[Var &quot;x&quot;; Var &quot;y&quot;]); Var &quot;z&quot;]))"))
   (P "一个谓词可以只有零个参数, 这对应于简单命题变量的情况. "
      "我们将具有一个参数的函数和谓词称为" (Em "单元的(unary或monadic)")
      ", 具有两个参数的称为" (Em "二元的(binary或dyadic)")
      ", 而具有" $n "个参数的称为" $n "元的.")
   (P "在特定上下文中, 我们会考虑受限" (Em "语言")
      "中的项和/或类型. 形式化地说, 我们将" (Em "签名")
      "定义为一对集合, 其中一个集合是函数的列表, "
      "另一个集合是谓词的列表, 函数和谓词都是由名字和元数构成的序对, "
      "而签名所对应的" (Em "语言")
      "是所有只使用签名里的函数和谓词构建而成的项和公式的集合 "
      "(不过变量可以是任意的). "
      "例如, 我们在第7章所使用的" (Em "算术语言") "具有以下签名:"
      (let ((sym (lambda (s) (Ms (string-append "&quot;" s "&quot;")))))
        (MB (tu0 (setE (tu0 (sym "0") $0)
                       (tu0 (sym "S") $1)
                       (tu0 (sym "+") $2)
                       (tu0 (sym "*") $2))
                 (setE (tu0 (sym "=") $2)
                       (tu0 (sym "<") $2)
                       (tu0 (sym "<=") $2))) "."))
      "于是诸如" (&+ $x (app $S $0)) "这样的项和诸如"
      (&< (app $S (app $S $0)) (&+ $x $y))
      "这样的公式都在该语言之中, 但是"
      (&+ $1 $x) "和" (app $P $0 $x)
      "不在. 对于" (Q "语言") "和" (Q "签名")
      "的确切形式化定义是不重要的 "
      "(这些随着材料的不同而发生变化, "
      "也有的作者会将语言和签名视为等同的), "
      "只要受限语言中的项和公式的概念清晰明确即可.")
   (H4. "量词")
   (P "现在我们来到另一和命题逻辑相较而言的主要改变: 量词的引入."
      (Ul (Li "公式" (∀ $x $p) ", 或者以OCaml表述是" (Code "Forall(x,p)")
              ", 其中" $x "是一个变量而" $p "是任意的公式, "
              "从直觉上来说意思是"
              (Q "对于" $x "的" (Em "所有") "值, " $p "为真")
              ". 出于这种理由, " $forall "被称为" (Em "全称量词")
              ", 这个符号是由" (Q "all") "的首字母颠倒得来的. "
              "{译注: 原文是" (Code "Forall(&quot;x&quot;,p)")
              ", 但是个人认为我的修改版本更为合理, 因为这里的"
              $x "是变量的元变量, 而不是一个具体的变量.}")
          (Li "与之可以类比的公式" (∃ $x $p)
              ", 或者以OCaml表述是" (Code "Exists(x,p)")
              ", 从直觉上来说意思是"
              (Q "存在一个" $x "使得" $p "为真")
              ", 即"
              (Q $p "对于" $x "的" (Em "某个") "值为真")
              ". 出于这种理由, " $exists "被称为"
              (Em "存在量词") ", 这个符号是由"
              (Q "exists") "的首字母颠倒得来的.")))
   (P "在公式" (∀ $x (App $P $x)) "和" (∃ $x (App $P $x))
      "里, 子公式" (App $P $x) "被称为是相应量词的"
      (Em "作用域(scope)")
      ". (在非形式化的讨论里, 我们经常用"
      (App $P $x) "这样的表达式代表"
      (Q "某个可能牵涉" (Q $x) "的任意公式")
      "). 这样的量词被称为是" (Em "绑定(bind)")
      "了其作用域内的" $x "的实例, "
      "而这些变量被称为是" (Em "绑定的(bound)")
      ". 不在某个量词的作用域内的变量实例被称为是" (Em "自由的(free)")
      ". 注意到相同的变量在同一公式里既自由出现又绑定出现, 例如"
      (&conj (appl $R $x $a) (∀ $x (appl $R $y $x)))
      ", 其中变量有一次自由出现, 还有一次绑定出现.")
   (P "从直觉上来说, 一个绑定变量只是一个回指对应绑定操作的占位符, "
      "而非通常意义下的独立变量. "
      "绑定变量可以与英语里的代词进行类比, "
      "代词回指在句子开头建立的某个特定名词: "
      (Q "Although the money was missing, John denied that he stole it")
      ". 绑定在数学记号里相当常见, 例如"
      (sum (&= $n $1) $inf (&/ $1 $n^2))
      "中的变量" $n ", "
      (integral (&- $inf) $inf (^ $e (&- $x^2)) $x)
      "中的变量" $x ", 以及"
      (setI $k^2 (∈ $k $NN))
      "中的变量" $k
      ". 它们也出现在编程语言里, 例如对于OCaml而言, 定义"
      (CodeB "let f(x) = 2 * x")
      "里的" (Code "x") ", 以及表达式"
      (CodeB "let a = 2 in a * a * a")
      "里的" (Code "a")
      ". 和逻辑学一样, 数学里的变量有时也在同一表达式里"
      "既自由出现又绑定出现, 例如在"
      (integral $0 $x (&i* $2 $x) $x)
      "里, 变量既有自由出现 (作为积分的上极限), "
      "又有绑定出现 (在积分的体里). "
      "类似地, " $x "真的在" (&d/d (@ $x^2) $x)
      "中既有自由出现又有绑定出现, "
      "尽管常规记号模糊了这一事实. "
      "我们可以将其分析为"
      (&\|-> $x $x^2) " (其中" $x "是绑定的) "
      "的导函数在点" $x "处求值 (这里的" $x
      "是一个自由变量). {译注: 然后, 根据惯例, "
      (&i* $2 $x) "又被理解为"
      (&\|-> $x (&i* $2 $x)) ".}")
   (P "在我们的具体句法里, 量词的作用域尽可能向右延伸, 例如"
      (∀ $x (&=> (app $P $x) (app $Q $x)))
      "代表" (∀ $x (@=> (app $P $x) (app $Q $x)))
      "而非" (&=> (@∀ $x (app $P $x)) (app $Q $x))
      ". (许多文献, 特别是较老的文献, 使用相反的约定, "
      "使得量词绑定得比联结词更为紧密. "
      "在查阅文献时读者应该记住这一点.) "
      "如果我们将全称量词或者存在量词相继应用于数个变量, "
      "那么我们通常只会写下一个量词符号, 例如"
      (∀ $x $y $z (&= (&+ $x (@+ $y $z)) (&+ (@+ $x $y) $z)))
      "而非"
      (∀ $x (∀ $y (∀ $z (&= (&+ $x (@+ $y $z)) (&+ (@+ $x $y) $z)))))
      ". 而且, 有时断言恰存在一个" $x "满足" $p
      "为真也是有用的. 我们将此记为" (∃! $x (App $P $x))
      ", 然后将其理解为"
      (∃ $x (&conj (App $P $x)
                   (∀ $y (&=> (App $P $y) (&= $y $x))))) ".")
   (P "从直觉上来说, 同种量词 (全为全称或者全为存在) "
      "的序列的排序不太重要: "
      (Q "对于" $x ", 对于" $y ", ...")
      "和"
      (Q "对于" $y ", 对于" $x ", ...")
      "意思应该是相同的. "
      "当我们之后精确定义逻辑等价时, 读者应该能够确认这一直觉. "
      "然而, 当不同种类的量词嵌套时, 或者导出量词" $exists!
      "牵涉其中时 (见练习3.1), 顺序往往就非常重要了. "
      "例如, 如果我们将" (&loves $x $y) "想成是"
      (Q $x " loves " $y) ", 公式"
      (∀ $x (∃ $y (&loves $x $y)))
      "断言了每个人都喜欢某个人, 而公式"
      (∃ $y (∀ $x (&loves $x $y)))
      "断言了某个人被所有人喜欢. "
      "举一个更为数学的例子, 考虑函数"
      (func $f $RR $RR)
      "的连续和一致连续的" $epsilon "-" $delta
      "定义. 连续性断言了给定" (&> $epsilon $0)
      ", 对于每个" $x ", 存在一个" (&> $delta $0)
      "使得每当" (&< (&abs (&- $x^ $x)) $delta) "时, 我们也有"
      (&< (&abs (&- (app $f $x^) (app $f $x))) $epsilon)
      " {译注: 原文的epsilon使用不太一致, 一会儿"
      $epsilon ", 一会儿" $epsilonv "}:"
      (MB (∀ $epsilon
             (&=> (&> $epsilon $0)
                  (∀ $x (∃ $delta
                           (&conj (&> $delta $0)
                                  (∀ $x^ (&=> (&< (&abs (&- $x^ $x)) $delta)
                                              (&< (&abs (&- (app $f $x^) (app $f $x)))
                                                  $epsilon)))))))) "."))
   (P "另一方面, 一致连续断言了给定" (&> $epsilon $0)
      ", 存在一个" (&> $delta $0) (Em "独立于") $x
      "使得对于任意的" $x "和" $x^ ", 每当"
      (&< (&abs (&- $x^ $x)) $delta) "时, 我们也有"
      (&< (&abs (&- (app $f $x^) (app $f $x))) $epsilon) ":"
      (MB (∀ $epsilon
             (&=> (&> $epsilon $0)
                  (∃ $delta
                     (&conj (&> $delta $0)
                            (∀ $x (∀ $x^ (&=> (&< (&abs (&- $x^ $x)) $delta)
                                              (&< (&abs (&- (app $f $x^) (app $f $x)))
                                                  $epsilon)))))))) "."))
   (P "请注意量词的顺序变化如何从根本上改变了其所断言的性质. (例如, "
      (&= (app $f $x) $x^2) "在实轴上是连续的, 但并非一致连续的.) "
      "一致连续性的概念在分析的算术化 (arithmetization) 过程中很晚才被明确提出, "
      "而若干早期的" (Q "证明") "表面上只需要连续性, 实际上却需要一致连续性. "
      "或许使用形式语言本可以更早地澄清许多概念上的困难.")
   (P "{原注: 即便使用形式语言, 要理解"
      (Q $forall) "和" (Q $exists)
      "量词反复交替出现的含义往往仍然很困难. "
      "正如我们将在第7章中看到的, "
      "量词交替的次数是衡量一个公式" (Q "数学复杂性")
      "的一个重要指标. 甚至有人提出, "
      "复数和拓扑空间等整套数学概念与结构, "
      "主要只是一种隐藏更多量词交替的手段, "
      "从而使它们更容易为我们的直觉所理解.}")
   (P "名字" (Q "一阶逻辑") "的由来在于量词只能应用于指称对象的变量, "
      "而不能是函数或者谓词. 允许对于函数和谓词进行量化的逻辑 (例如"
      (∃ $f (∀ $x (Appl $P $x (app $f $x))))
      ") 被称为是" (Em "二阶的") "或是" (Em "高阶的")
      ". 但是, 我们自限于一阶量词: "
      "接下来定义的句法分析器会将这样的字符串里的第一个" $f
      "当作通常的对象变量, 而第二个" $f
      "会被当成一个幺元函数, 它们只是恰好同名.")
   (H3. "句法分析和打印")
   
   (H3. "一阶逻辑的语义")
   (P "与命题公式一样, 一阶公式的意义是递归定义的, "
      "并且依赖于赋予其各组成部分的基本意义. "
      "在命题逻辑中, 唯一的组成部分是命题变量, "
      "但在一阶逻辑中, 变量, 函数符号和谓词符号都需要被解释. "
      "通常的做法是将这些关注点分开处理, "
      "相对于一个解释和一个赋值来定义项或公式的含义, "
      "其中解释刻画函数符号和谓词符号的解释, 赋值刻画变量的意义. "
      "从数学上讲, 一个解释" $M "由三个部分组成."
      (Ul (Li "一个非空集合" $D ", 其被称为解释的" (Em "论域")
              ". 意图在于所有的项都取值于" $D ".")
          (Li "一个映射, 将每个" $n "元函数符号" $f
              "映射为一个函数" (func $f_M $D^n $D) ".")
          (Li "一个映射, 将每个" $n "元谓词符号" $P
              "映射为一个布尔函数"
              (func $P_M $D^n (setE $false $true))
              ". 等价地, 我们可以将这种解释想成是一个子集"
              (&sube $P_M $D^n) ".")))
   (P "我们基于一个特定的解释" $M "和赋值" $v
      "来定义一个项的值, 只需注意变量是如何被"
      $v "所解释的以及函数符号是如何被" $M
      "所解释的:"
      (eqn*
       ((&termval $M $v $x) $= (app $v $x))
       ((&termval $M $v (appl $f $t_1 $..h $t_n))
        $=
        (appl $f_M (&termval $M $v $t_1) $..h
              (&termval $M $v $t_n)))))
   (P "一个公式是否在某个特定的解释" $M "和赋值"
      $v "下成立 (也就是具有真值" (Q $true)
      "), 可以类似地递归定义 (Tarski 1936), "
      "并且大部分都遵循着命题逻辑所建立的模式. "
      "主要附加的复杂度是刻画量词的意义. "
      "我们意在使得" (∀ $x (App $P $x))
      "在一个特定解释" $M "和赋值" $v
      "下成立恰当其体" (App $P $x)
      "对于变量" $x "的" (Em "任何")
      "解释都为真, 换言之, 不论我们怎样修改赋值"
      $v "在" $x "上的值.")
   (eqn*
    ((&holds $M $v $bottom) $= $false)
    ((&holds $M $v $top) $= $true)
    ((&holds $M $v (appl $R $t_1 $..h $t_n))
     $=
     (appl $R_M (&termval $M $v $t_1) $..h
           (&termval $M $v $t_n)))
    ((&holds $M $v (@neg $p))
     $=
     (&not (&holds $M $v $p)))
    ((&holds $M $v (@conj $p $q))
     $=
     (&and (&holds $M $v $p)
           (&holds $M $v $q)))
    ((&holds $M $v (@disj $p $q))
     $=
     (&or (&holds $M $v $p)
          (&holds $M $v $q)))
    ((&holds $M $v (@=> $p $q))
     $=
     (&or (&not (&holds $M $v $p))
          (&holds $M $v $q)))
    ((&holds $M $v (@<=> $p $q))
     $=
     (@= (&holds $M $v $p)
         (&holds $M $v $q)))
    ((&holds $M $v (@∀ $x $p))
     $=
     (: "对于所有的" (∈ $a $D) ",&nbsp;"
        (&holds $M (@ext $v $x $a) $p)))
    ((&holds $M $v (@∃ $x $p))
     $=
     (: "存在某个" (∈ $a $D) ",&nbsp;"
        (&holds $M (@ext $v $x $a) $p))))
   (P "解释里的论域" $D "是约定非空的, "
      "但只要非空即可, 其可以具有任意有限或者无限的基数 "
      "(例如, 集合" (setE $0 $1) "或者实数集" $RR
      "), 并且函数和谓词可由任意的(可能不可计算的)数学函数所解释. "
      "对于无限的" $D ", 我们无法直接在OCaml中实现" $holds
      "函数, 因为解释一个量词牵涉在" $D
      "的所有元素上运行测试. 不过, "
      "我们将会实现一个只对于有限论域成立的弱化版本.")
   (P "一个解释是由一个论域, 函数解释, 谓词解释的三元组表示的. "
      "(为了使得解释有意义, 论域" $D "应该是非空的, "
      "并且每个" $n "元函数符号" $f "应该被解释为一个将"
      $D "的元素的" $n "元组映射至" $D "的一个函数" $f_M
      ". 以下的OCaml函数只是假定参数" (Code "m")
      "在这种解读下是有意义的.) "
      "赋值被表示为一个有限部分函数 (见附录2). "
      "然后, 项的语义可以遵循我们之前所给出的抽象描述进行递归定义:"
      (CodeB "let rec termval (domain,func,pred as m) v tm =
  match tm with
    Var(x) -> apply v x
  | Fn(f,args) -> func f (map (termval m v) args);;")
      "而公式的语义如下:"
      (CodeB "let rec holds (domain,func,pred as m) v fm =
  match fm with
    False -> false
  | True -> true
  | Atom(R(r,args)) -> pred r (map (termval m v) args)
  | Not(p) -> not(holds m v p)
  | And(p,q) -> (holds m v p) &amp; (holds m v q)
  | Or(p,q) -> (holds m v p) or (holds m v q)
  | Imp(p,q) -> not(holds m v p) or (holds m v q)
  | Iff(p,q) -> (holds m v p = holds m v q)
  | Forall(x,p) -> forall (fun a -> holds m ((x |-> a) v) p) domain
  | Exists(x,p) -> exists (fun a -> holds m ((x |-> a) v) p) domain;;"))
   (P "为了澄清概念, 让我们尝试一些解释公式的例子, "
      "这些公式牵涉零元函数符号" (Q $0) ", " (Q $1)
      ", 二元函数符号" (Q $+) "和" (Q $d*)
      ", 以及二元谓词符号" (Q $=)
      ". {译注: 从某种意义上来说, "
      "以下的程序暗示了名字和元数是要打包在一起出现的, "
      "同名但不同元数的函数符号或谓词符号被视为(截然)不同的. "
      "这点之后在原文中也有显式强调.} "
      "我们考虑一种Boole本人式解释, 其中"
      (Q $+) "被解读为不可兼的或:"
      (CodeB "let bool_interp =
  let func f args =
    match (f,args) with
      (&quot;0&quot;,[]) -> false
    | (&quot;1&quot;,[]) -> true
    | (&quot;+&quot;,[x;y]) -> not(x = y)
    | (&quot;*&quot;,[x;y]) -> x &amp; y
    | _ -> failwith &quot;uninterpreted function&quot;
  and pred p args =
    match (p,args) with
      (&quot;=&quot;,[x;y]) -> x = y
    | _ -> failwith &quot;uninterpreted predicate&quot; in
  ([false; true],func,pred);;"))
   (P "另一种解释是对于某个任意的正整数" $n
      "的模" $n "算术:"
      (CodeB "let mod_interp n =
  let func f args =
    match (f,args) with
      (&quot;0&quot;,[]) -> 0
    | (&quot;1&quot;,[]) -> 1 mod n
    | (&quot;+&quot;,[x;y]) -> (x + y) mod n
    | (&quot;*&quot;,[x;y]) -> (x * y) mod n
    | _ -> failwith &quot;uninterpreted function&quot;
  and pred p args =
    match (p,args) with
      (&quot;=&quot;,[x;y]) -> x = y
    | _ -> failwith &quot;uninterpreted predicate&quot; in
  (0--(n-1),func,pred);;"))
   (P "如果所有的变量都由量词所绑定, "
      "那么赋值就对于一个公式是否成立不产生任何影响. "
      "(很快我们将会以更精确的方式陈述和证明这个结果.) "
      "在这样的情形下, 我们可以就使用"
      (Code "undefined") "进行实验. 例如, "
      (∀ $x (&disj (&= $x $0) (&= $x $1)))
      "在解释" (Code "bool_interp") "和"
      (Code "mod_interp 2") "下均成立, 但在"
      (Code "mod_interp 3") "下不成立:"
      (CodeB "# holds bool_interp undefined &lt;&lt;forall x. (x = 0) \\/ (x = 1)>>;;
- : bool = true
# holds (mod_interp 2) undefined &lt;&lt;forall x. (x = 0) \\/ (x = 1)>>;;
- : bool = true
# holds (mod_interp 3) undefined &lt;&lt;forall x. (x = 0) \\/ (x = 1)>>;;
- : bool = false"))
   (P "考虑以下断言, 即论域里的每个非零对象均有一个乘法逆元."
      (CodeB "# let fm = &lt;&lt;forall x. ~(x = 0) ==> exists y. x * y = 1>>;;"))
   (P "对于了解一些数论的读者应该可以预料到, 这在"
      (Code "mod_interp n") "中成立恰当" (Code "n")
      "是一个素数, 或者是平凡情形为" $1 ":"
      (CodeB "# filter (fun n -> holds (mod_interp n) undefined fm) (1--45);;
- : int list = [1; 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43]"))
   (P "读者可以确认, 这个公式在" (Code "bool_interp")
      "下也是成立的. (实际上, 即便基于不同的论域, "
      (Code "mod_interp 2") "和" (Code "bool_interp") "是"
      (Em "同构的") ", 即本质上相同, 这是会在第4.2节有所解释的概念.")
   (P "以下是个人Scheme版本的实现:"
      (CodeB "(define (forall? pred lst)
  (cond ((null? lst) #t)
        ((pred (car lst))
         (forall? pred (cdr lst)))
        (else #f)))
(define (exists? pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else
         (exists? pred (cdr lst)))))
;&lt;term> ::= &lt;var> | (&lt;func> &lt;term>*)
;&lt;predicate> ::= (&lt;pred> &lt;term>*)
;&lt;exp> ::= &lt;bool>
;       |  &lt;predicate>
;       |  (not &lt;exp>)
;       |  (and &lt;exp> &lt;exp>)
;       |  (or &lt;exp> &lt;exp>)
;       |  (=> &lt;exp> &lt;exp>)
;       |  (&lt;=> &lt;exp> &lt;exp>)
;       |  (forall &lt;var> &lt;exp>)
;       |  (exists &lt;var> &lt;exp>)
(struct interp (domain func pred))
(define ((termval m v) term)
  (match term
    (,x (guard (symbol? x)) (v x))
    ((,func . ,term*)
     (apply (funcval m func)
            (map (termval m v) term*)))))
(define (funcval m f)
  ((interp-func m) f))
(define (predval m p)
  ((interp-pred m) p))
(define (extend v x a)
  (lambda (y)
    (if (eq? y x)
        a
        (v y))))
(define (holds? m v exp)
  (match exp
    (,b (guard (boolean? b)) b)
    ((not ,e) (not (holds? m v e)))
    ((and ,e1 ,e2) (and (holds? m v e1)
                        (holds? m v e2)))
    ((or ,e1 ,e2) (or (holds? m v e1)
                      (holds? m v e2)))
    ((=> ,e1 ,e2) (or (not (holds? m v e1))
                      (holds? m v e2)))
    ((&lt;=> ,e1 ,e2) (eq? (holds? m v e1)
                        (holds? m v e2)))
    ((forall ,x ,e)
     (forall? (lambda (a)
                (holds? m (extend v x a) e))
              (interp-domain m)))
    ((exists ,x ,e)
     (exists? (lambda (a)
                (holds? m (extend v x a) e))
              (interp-domain m)))
    ((,pred . ,term*)
     (apply (predval m pred)
            (map (termval m v) term*)))))")
      "模逆的例子的确十分有趣."
      (CodeB "(define (mod-interp n)
  (interp (range n)
          (lambda (f)
            (case f
              ((zero) (lambda () 0))
              ((one) (lambda () (modulo 1 n)))
              ((+) (lambda (a b)
                     (modulo (+ a b) n)))
              ((*) (lambda (a b)
                     (modulo (* a b) n)))
              (else (error 'mod-interp &quot;unknown func symbol ~s&quot; f))))
          (lambda (p)
            (case p
              ((=) (lambda (a b) (= a b)))
              (else (error 'mod-interp &quot;unknown pred symbol ~s&quot; p))))))
(define undefined
  (lambda (x)
    (error 'undefined &quot;unknown variable ~s&quot; x)))
(define modulo_inverse_existence
  '(forall x (=> (not (= x (zero)))
                 (exists y (= (* x y) (one))))))")
      (CodeB "> (filter (lambda (n)
            (holds? (mod-interp n) undefined
                    modulo_inverse_existence))
          (range 1 46))
'(1 2 3 5 7 11 13 17 19 23 29 31 37 41 43)"))
   (H4. "自由变量的集合")
   (P "对于一个项" $t "所牵涉的所有变量的集合我们记"
      (&FVT $t) ", 例如"
      (&= (&FVT (appl $f (&+ $x $y) (&+ $y $z)))
          (setE $x $y $z))
      ", 其可以在OCaml中递归实现如下:"
      (CodeB "let rec fvt tm =
  match tm with
    Var x -> [x]
  | Fn(f,args) -> unions (map fvt args);;"))
   (P "一个项" $t "被称为是" (Em "ground")
      "的, 如果其不含有变量, 即"
      (&= (&FVT $t) $empty)
      ". 正如我们所期望的, "
      "一个项的语义只依赖于赋值在实际出现在项里的变量上的动作, "
      "所以说作为特殊情形, ground项的语义和赋值无关.")
   ((Theorem #:id "term-var-agree")
    "如果赋值" $v "和" $v^ "在某个项里的所有变量上都相合, "
    "即对于所有的" (∈ $x (&FVT $t)) ", 我们都有"
    (&= (app $v $x) (app $v^ $x)) ", 那么"
    (&= (&termval $M $v $t) (&termval $M $v^ $t)) ".")
   ((proof)
    "根据" $t "的结构上的归纳. 如果" $t
    "只是一个变量" $x ", 那么"
    (&= (&FVT $t) (setE $x))
    ", 于是根据题设有"
    (&= (&termval $M $v $x)
        (app $v $x)
        (app $v^ $x)
        (&termval $M $v^ $x))
    "." (Br)
    "若" $t "具有形式" (appl $f $t_1 $..h $t_n)
    ", 那么根据题设, " $v "和" $v^ "在集合"
    (&FVT (appl $f $t_1 $..h $t_n))
    "上相合, 因而其也在每个" (&FVT $t_i)
    "上相合. 根据归纳假设, 对于每个" $t_i "有"
    (&= (&termval $M $v $t_i)
        (&termval $M $v^ $t_i))
    ", 据此可以推得"
    (&= (&termval $M $v (appl $f $t_1 $..h $t_n))
        (&termval $M $v^ (appl $f $t_1 $..h $t_n)))
    ".")
   (P "下列函数返回出现在一个公式里的所有变量的集合."
      (CodeB "let rec var fm =
   match fm with
    False | True -> []
  | Atom(R(p,args)) -> unions (map fvt args)
  | Not(p) -> var p
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> union (var p) (var q)
  | Forall(x,p) | Exists(x,p) -> insert x (var p);;"))
   (P "和项一样, 一个公式" $p "被称为是" (Em "ground")
      "的, 如果其不包含变量, 即" (&= (&var $p) $empty)
      ". 然而, 我们通常对于公式的自由变量集合"
      (&FV $p) "更有兴趣, 忽略那些只会绑定出现的变量. "
      "在这种情况下, 当我们经过一个量词时, "
      "我们需要从其体的自由变量集合里去除量化了的变量而不是加上它:"
      (CodeB "let rec fv fm =
  match fm with
    False | True -> []
  | Atom(R(p,args)) -> unions (map fvt args)
  | Not(p) -> fv p
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) -> union (fv p) (fv q)
  | Forall(x,p) | Exists(x,p) -> subtract (fv p) [x];;"))
   (P "诚然如此, 在将以上定理从项推广至公式的过程里, "
      "自由变量集合的概念是重要的:")
   ((Theorem #:id "formula-var-agree")
    "如果两个赋值" $v "和" $v^ "在一个公式" $p
    "的所有自由变量上都相合, 即对于所有的"
    (∈ $x (&FV $p)) "我们都有"
    (&= (app $v $x) (app $v^ $x))
    ", 那么"
    (&= (&holds $M $v $p)
        (&holds $M $v^ $p)) ".")
   ((proof)
    "根据" $p "的结构上的归纳. 若" $p "为" $bottom "或" $top
    ", 那么该定理平凡为真. 如果" $p "具有形式"
    (appl $R $t_1 $..h $t_n) ", 那么既然" $v "和" $v^
    "在" (&FV (appl $R $t_1 $..h $t_n))
    "上相合, 因而其也在每个" (&FVT $t_i)
    "上相合. " (Ref "term-var-agree")
    "表明, 对于每个" $t_i ", 我们有"
    (&= (&termval $M $v $t_i)
        (&termval $M $v^ $t_i))
    ", 故"
    (&= (&holds $M $v (appl $R $t_1 $..h $t_n))
        (&holds $M $v^ (appl $R $t_1 $..h $t_n)))
    "." (Br)
    "如果" $p "具有形式" (&neg $q)
    ", 那么既然根据定义有" (&= (&FV $p) (&FV $q))
    ", 归纳假设给出了"
    (&= (&holds $M $v $p)
        (&not (&holds $M $v $q))
        (&not (&holds $M $v^ $q))
        (&holds $M $v^ $p))
    ". 类似地, 如果" $p "具有形式" (&conj $q $r)
    ", 那么既然"
    (&= (&FV (&conj $q $r))
        (&union (&FV $q) (&FV $r)))
    ", 归纳假设保证了"
    (&= (&holds $M $v $q) (&holds $M $v^ $q)) "和"
    (&= (&holds $M $v $r) (&holds $M $v^ $r))
    ", 于是"
    (&= (&holds $M $v (@conj $q $r))
        (&holds $M $v^ (@conj $q $r)))
    ". 对于其他二元联结词情形的论证几乎都是一样的." (Br)
    "如果" $p "具有形式" (∀ $x $q)
    ", 那么根据题设, 我们有对于每个" (∈ $y (&FV $p))
    ", " (&= (app $v $y) (app $v^ $y)) ". 既然"
    (&= (&FV (∀ $x $q)) (&- (&FV $q) (setE $x)))
    ", 这意味着对于每个" (∈ $y (&FV $q))
    ", 除了" (&= $y $x) "的情况, 都有"
    (&= (app $v $y) (app $v^ $y))
    ". 但是, 这就保证了对于论域" $M
    "里的任意元素" $a ", 对于" (Em "所有") "的"
    (∈ $y (&FV $q)) ", 我们都有"
    (&= (app (@ext $v $x $a) $y)
        (app (@ext $v^ $x $a) $y))
    ". 因此, 根据归纳假设, 对于所有这样的" $a ", 我们有"
    (&= (&holds $M (@ext $v $x $a) $q)
        (&holds $M (@ext $v^ $x $a) $q))
    ". 根据定义, 这意味着"
    (&= (&holds $M $v $p) (&holds $M $v^ $p))
    ". 对于存在量词情形的论证也是类似的.")
   (P "一个公式" $p "被称为是一个" (Em "句子(sentence)")
      ", 如果其没有自由变量, 即" (&= (&FV $p) $empty)
      ". 一个ground公式也是一个句子, 但是一个句子可以含有变量, "
      "只要其所有的实例都有绑定即可, 例如"
      (∀ $x (∃ $y (appl $P $x $y))) ".")
   ((Corollary)
    "如果" $p "是一个句子, 那么对于任意的解释" $M
    "和任意的赋值" $v "和" $v^ ", 我们都有"
    (&= (&holds $M $v $p) (&holds $M $v^ $p))
    ". {译注: 当然, 这种解释需要合理才行.}")
   ((proof)
    "如果" (&= (&FV $p) $empty)
    ", 那么不论赋值怎样, 它们都在"
    (&FV $p) "上是相合的.")
   (H4. "有效性和可满足性")
   (P "类比于命题逻辑, 一个一阶公式被称为是" (Em "逻辑有效的")
      ", 如果其在所有解释和所有赋值下都成立. "
      "并且, 如果" (&<=> $p $q) "是逻辑有效的, 那么我们称"
      $p "和" $q "是" (Em "逻辑等价的")
      ". 有效公式是命题重言的一阶类比, "
      "并且对于一阶情形我们有时也使用" (Q "重言")
      "这个词汇. 的确如此, 每个命题重言都可以给出 (give rise to) "
      "相应的有效一阶公式 (见之后的推论3.13). "
      "一个牵涉量词的有效公式是"
      (&=> (@∀ $x (App $P $x)) (App $P $a))
      ", 其断言了如果" $P "对于所有的" $x
      "均为真, 那么其也对于任何特定的常量"
      $a "为真. 另外, 这个量词的存在和作用域都是关键的; "
      (&=> (App $P $x) (App $P $a)) "和"
      (∀ $x (&=> (App $P $x) (App $P $a)))
      "都不是有效的. {译注: 这里说的不是有效的, "
      "指的是不总是有效的, 但是不排除存在有效的情况. "
      "这些记号都是元记号, " $x "是代表变量的元变量, "
      $a "是代表常量 (零元函数应用) 的元变量, "
      $P "是公式模式, 可以理解为根据一个项参数构造公式的方法.} "
      "例如, 后者在某些解释下成立但是在其他解释下又不成立:"
      (CodeB "# holds (mod_interp 3) undefined &lt;&lt;(forall x. x = 0) ==> 1 = 0>>;;
- : bool = true
# holds (mod_interp 3) undefined &lt;&lt;forall x. x = 0 ==> 1 = 0>>;;
- : bool = false")
      "{译注: 这里对于例子的描述和实际给出的例子是不一致的, "
      "实际上给出的例子说的是在某个固定的解释下, "
      (&=> (@∀ $x (App $P $x)) (App $P $a)) "的某个具体实例成立, "
      "但是" (∀ $x (&=> (App $P $x) (App $P $a)))
      "按照相同方式进行实例化得到的公式却不成立.}")
   (P "一个相当令人意外的逻辑有效公式或许是"
      (∃ $x (∀ $y (&=> (app $P $x) (app $P $y))))
      ". {译注: 这个公式应该理解为字面上的而不是元的.} "
      "从直觉上来说, 要么" $P "对于一切都为真, "
      "此时后件" (app $P $y) "总是为真, 要么存在某个"
      $x "使得前件" (app $P $x) "为假. 不论是哪种情况, "
      "这整个推出式都是为真的. "
      "(这经常被称为" (Q "酒鬼悖论")
      ", 因为其可以想成是断言某人" $x
      "的存在性, 其满足如果" $x "喝酒了, "
      "那么所有人都喝酒了.)")
   (P "我们称一个解释" $M (Em "满足")
      "一个一阶公式" $p ", 或者说" $p
      "在解释" $M "下成立, 如果对于所有的赋值" $v
      ", 我们都有" (&= (&holds $M $v $p) $true)
      ". 类似地, 我们称" $M "满足一个公式的集合" $S
      ", 或者说" $S "在" $M "下成立, 如果" $M
      "满足集合里的每个公式. 我们称一个一阶公式"
      "或者一个一阶公式集合是" (Em "可满足的")
      ", 如果存在某个能够满足它的解释. "
      "注意到在可满足性定义里解释和赋值的非对称性: "
      "存在某个解释" $M "使得对于所有的赋值" $v
      "我们都有" (&holds $M $v $p)
      "; 这看起来有点令人惊讶, "
      "但是却使得之后的材料从技术上更为简单. "
      "在任何情况下, 当我们考虑句子时, "
      "这种不对称就消失了, 因为赋值不会造成任何影响. "
      "很容易看出来, 一个句子" $p "是有效的当且仅当"
      (&neg $p) "是不可满足的, 这和命题逻辑是一样的. "
      "然而, 对于具有自由变量的公式而言, 这不再正确. "
      "例如, " (&disj (app $P $x) (&neg (app $P $y)))
      "不是有效的, 然而否定形式"
      (&conj (&neg (app $P $x)) (app $P $y))
      "仍是不可满足的, 因为其要被" (Em "所有")
      "赋值满足, 包括那些给" $x "和" $y
      "指派了相同对象的赋值.")
   (P "满足了一个公式集合" Γ "的一个解释被称为是"
      Γ "的一个" (Em "模型") ". 记号" (G!= $p) "的意思是"
      (Q $p "在" Γ "的所有模型里均成立")
      ", 并且我们通常将" (!= $empty $p) "记为"
      (!= $p) ". 特别地, " Γ "是不可满足的当且仅当"
      (G!= $bottom) " (既然" $bottom
      "永远不能成立, 必然不存在" Γ "的模型). "
      "然而, 和命题逻辑不同的是, 即便当"
      (&= Γ (setE $p_1 $..h $p_n))
      "有限时, 也不必满足"
      (!= (setE $p_1 $..h $p_n) $p) "等价于"
      (!= (&=> (&conj $p_1 $..c $p_n) $p))
      ". 原因在于赋值上的量化在不同的位置发生. "
      "例如, " (!= (setE (app $P $x)) (app $P $y))
      "为真, 但是" (!= (&=> (app $P $x) (app $P $y)))
      "不为真. 然而, 如果每个" $p_i "都是一个句子 "
      "(也就是没有自由变量), 那么这两个会是等价的. "
      "{译注: 对于非句子的情况, 右边可以推出左边, "
      "但是左边无法推出右边.} "
      "我们偶尔会使用记号" (!=_M Γ $p)
      "来表达, 如果" (Em "特定") "的" $M "是"
      Γ "的模型, 那么" $p "也在该模型中成立. "
      "{译注: 原文没有对于单个公式定义模型, "
      "但实际上这里的确可以说" $M "是" $p "的模型.} "
      "于是, " (!=_M $p) "的意思就是"
      $M "满足" $p ".")
   (P "{译注: 前一段的讨论默认了"
      (&cm $p_1 $..h $p_n $p)
      "都是具有某个特定签名的语言里的公式, "
      "并且解释应该与签名相匹配. 换言之, "
      "恰好应该提供对于签名所描述的函数符号和谓词符号的解释, "
      "并且元数也要与签名相符.} "
      "{再次译注: 实际上根据后文来看, "
      "作者的倾向不是假定特定隐式签名, "
      "而是解释需要解释一切函数符号和谓词符号.}")
   (P "正如我们已经指出的, "
      "我们不可能直接基于语义来实现有效性或可满足性的检验. "
      "我们完全无法评估一个公式在具有无限论域的解释下是否成立. "
      "虽然我们可以检验它在某个有限解释下是否成立, "
      "但我们无法检验它在所有这样的解释下是否成立, 因为有限解释有无穷多个. "
      "请注意这与命题逻辑的对比: 在命题逻辑中, "
      "命题变量的取值范围是一个有限的(" $2 "-元素的)集合, "
      "因此可以穷举遍历, 而且也不存在单独的解释这一概念. "
      "{译注: 并且, 一个命题公式的不同原子数目是有限的, "
      "而命题公式的真值只依赖于其所牵涉的原子上的真值.}")
   (P "然而, 这并不意味着先验地排除了以更巧妙的方式检验一阶有效性的一切希望. "
      "实际上, 我们将以一种更间接的方式来解决有效性检验问题: "
      "首先将一阶公式转化为一组命题公式, "
      "使得这组命题公式可满足当且仅当原本的公式可满足. "
      "因此, 我们将首先考虑如何对公式进行变换, "
      "使量词移到最外层, 然后再将其彻底消去. "
      "不过, 在着手这项工作之前, "
      "我们需要精确地处理一些相当乏味的句法问题.")
   (H3. "句法操作")
   (P "我们经常想要取一个一阶公式, 然后对于其所有的自由变量进行全称量化, 例如从"
      (∃ $y (&< $x (&+ $y $z))) "到" (∀ $x (∃ $y (&< $x (&+ $y $z))))
      ". {译注: 这里只是演示了对于部分自由变量进行全称量化而非全部, "
      "全称闭包指的是对于所有的自由变量进行全称量化. "
      "从语义角度来看, 全称量化的次序的确不重要.} "
      "注意到这种" (Q "泛化") "或者说" (Q "全称闭包")
      "是有效的当且仅当原本的公式是有效的, "
      "因为不论哪种情况我们都要求在对于那个变量指派任意的论域元素时, "
      "核心公式成立. (更为形式化地说, 使用" (Ref "formula-var-agree")
      "以表明{对于所有的赋值" $v "和" (∈ $a $D) ", 我们有"
      (&holds $M (@ext $v $x $a) $p) "}当且仅当{对于所有的赋值"
      $v ", 我们有" (&holds $M $v $p) "}.) "
      "{译注: 原文括号里的补充实际上是更强一些的, "
      "它说明了解释" $M "满足新的公式当且仅当" $M
      "满足原本的公式. 并且, 可以看到实际上" $x
      "不一定要是" (&FV $p) "的元素, 任意的变量都具有这种性质. "
      "不过, 后文指出了如果" (&!in $x (&FV $p))
      ", 那么" (∀ $x $p) "和" $p "甚至是逻辑等价的. "
      "逻辑等价性和这里所说的性质的区别在于, "
      "逻辑等价需要逐模型逐赋值的真值相等, "
      "但是这里只是逐模型的可满足性相同, "
      "所以逻辑等价是更强的性质.} "
      "不过, 与句子打交道更为方便; "
      "例如, 如果所有牵涉的公式都是句子, 那么"
      (!= (setE $p_1 $..h $p_n) $q) "当且仅当"
      (!= (&=> (&conj $p_1 $..c $p_n) $q))
      ", 并且" $p "的有效性和" (&neg $p)
      "的不可满足性是相同的, "
      "这两个和命题逻辑的情况保持一致. "
      "以下是一个对于全称泛化的OCaml实现:"
      (CodeB "let generalize fm = itlist mk_forall (fv fm) fm;;")
      "{译注: " (Code "generalize")
      "将任意的一阶公式转化为一个句子, 但是保持有效性不变. "
      "更细致地说, 相对于每个特定模型的可满足性不变.}")
   (H4. "项中的替换")
   (P "我们需要定义的另一关键操作是将项或者公式里的变量替换为项, "
      "例如将" (&=> (&< $x $2) (&<= $x $y)) "中的" $x
      "替换为" $1 "可以得到" (&=> (&< $1 $2) (&<= $1 $y))
      ". 我们将会把意图的变量指派或者说" (Em "实例化")
      "描述为一个从变量到项的有限部分函数, "
      "对于我们不想改变的变量, 这个函数可以是未定义的或者将"
      (Code "x") "映射为" (Code "Var(x)")
      ". 给定这样一种指派" (Code "sfn")
      ", 项上的替换可以递归定义如下:"
      (CodeB "let rec tsubst sfn tm =
  match tm with
    Var x -> tryapplyd sfn x tm
  | Fn(f,args) -> Fn(f,map (tsubst sfn) args);;"))
   (P "对于这个概念, 我们可以观察到一些重要的性质. "
      "首先, 被替换后的项中的变量是可以预料的:")
   ((Lemma #:id "tsubst-free")
    "对于任意的项" $t "和实例化" $i
    ", 被替换后的项中的自由变量恰好是用于替换"
    $t "中的自由变量的那些项里的自由变量, 即"
    (MB (&= (&FVT (&tsubst $i $t))
            (Union (∈ $y (&FVT $t))
                   (&FVT (app $i $y)))) ".")
    "{译注: 这里的" $i "是从变量到项的(完全)函数, "
    "但是我们可以将有限部分函数嵌入到完全函数里. "
    "或许这里作者的想法就是嵌入, 所以说" $i
    "并非所有可能的完全函数. 然而, 实际上像"
    (Code "tsubst") "和之前的" (Code "termval")
    "和" (Code "holds") "这样用到有限部分函数的过程"
    "可以自然地推广至完全函数的版本. "
    "并且, 这些论证也没有用到有限部分函数的特殊性. "
    "所以说, 论证也不是错的, "
    "并且在某种意义上证明了一个稍强的版本. "
    "不过, 值得注意一下的是, 数学记号里"
    $\|-> "这个操作可以应用于完全函数, "
    "但是OCaml代码里" (Code "|->")
    "只会应用于所谓的有限部分函数.}")
   ((proof)
    "根据项的结构上的归纳. 如果" $t
    "是一个变量" $z ", 那么"
    (MB (&= (&FVT (&tsubst $i $t))
            (&FVT (app $i $z))
            (Union (∈ $y (setE $z))
                   (&FVT (app $i $y)))) ".")
    "既然" (&= (&FVT $z) (setE $z))
    ", 于是推出了想要的结果." (Br)
    "若" $t "具有形式" (appl $f $t_1 $..h $t_n)
    ", 那么根据归纳假设, 对于每个"
    (&= $k (&cm $1 $..h $n)) ", 我们有:"
    (MB (&= (&FVT (&tsubst $i $t_k))
            (Union (∈ $y (&FVT $t_k))
                   (&FVT (app $i $y)))) ".")
    "由此可以推出:"
    (MB (deriv^
         (&FVT (&tsubst $i (appl $f $t_1 $..h $t_n)))
         (&FVT (appl $f (&tsubst $i $t_1) $..h
                     (&tsubst $i $t_n)))
         (Union (&= $k $1) $n
                (&FVT (&tsubst $i $t_k)))
         (Union (&= $k $1) $n
                (Union (∈ $y (&FVT $t_k))
                       (&FVT (app $i $y))))
         (Union (∈ $y (Union (&= $k $1) $n
                             (&FVT $t_k)))
                (&FVT (app $i $y)))
         (Union (∈ $y (&FVT (appl $f $t_1 $..h $t_n)))
                (&FVT (app $i $y))))))
   (P "以下结果给出了关于替换后的项的解释的一个简单性质. "
      "经过反思不难发现, 它相当符合预期. "
      "{译注: 第2章有一个非常类似的定理, "
      "只不过那里是对于单独一个变量进行替换, "
      "而这里是同时对于所有变量进行替换. "
      "并且, 第2章和第3章的变量在层次上也并不相同, "
      "第2章的变量差不多就是原子公式本身, "
      "而第2章的替换是从变量到(命题)公式的映射; "
      "第3章的变量只是项的一种, 原子公式的一部分, "
      "而第3章的替换是从变量到项的映射.}")
   ((Lemma #:id "tsubst-value")
    "对于任意的项" $t "和实例化" $i ", 在任意的解释" $M "和赋值" $v
    "下, 替换后的项的值和原本的项在修饰了的赋值"
    (&compose (@termval $M $v) $i)
    "下所得到的值是相同的, 即"
    (MB (&= (&termval $M $v (@tsubst $i $t))
            (&termval $M (@compose (@termval $M $v) $i) $t)) ".")
    "{译注: 原文说的是原本的公式, 这只是一个笔误.}")
   ((proof)
    "如果" $t "是一个变量" $x ", 那么"
    (MB (deriv^
         (&termval $M $v (@tsubst $i $x))
         (&termval $M $v (app $i $x))
         (app (@compose (@termval $M $v) $i) $x)
         (&termval $M (@compose (@termval $M $v) $i) $x)))
    "这正是预期的结果. 若" $t "具有形式"
    (appl $f $t_1 $..h $t_n)
    ", 那么根据归纳假设, 对于每个"
    (&= $k (&cm $1 $..h $n)) ", 我们有:"
    (MB (&= (&termval $M $v (@tsubst $i $t_k))
            (&termval $M (@compose (@termval $M $v) $i) $t_k)))
    "于是:"
    (MB (deriv^
         (&termval $M $v (@tsubst $i (appl $f $t_1 $..h $t_n)))
         (&termval $M $v (appl $f (&tsubst $i $t_1) $..h
                               (&tsubst $i $t_n)))
         (appl $f_M (&termval $M $v (@tsubst $i $t_1)) $..h
               (&termval $M $v (@tsubst $i $t_n)))
         (appl $f_M (&termval $M (@compose (@termval $M $v) $i) $t_1) $..h
               (&termval $M (@compose (@termval $M $v) $i) $t_n))
         (&termval $M (@compose (@termval $M $v) $i)
                   (appl $f $t_1 $..h $t_n)))))
   (H4. "公式中的替换")
   (P "乍看之下, 我们似乎可以通过类似的结构递归来定义公式中的替换操作. "
      "然而, 绑定变量的存在使问题变得复杂许多.")
   (P "我们已经观察到, 绑定变量只是占位符, "
      "用于指示绑定变量与其绑定实例之间的对应关系, "
      "因此不应对它们进行替换. 例如, 对于" $x "进行替换不应对公式"
      (∀ $x (&= $x $x)) "产生任何影响, 因为其中每个" $x
      "都被量词所绑定. 此外, 即使避免对绑定变量本身进行替换, "
      "我们仍然面临一个风险: "
      "替换进去的项的自由变量可能被外部的变量绑定操作所捕获. "
      "例如, 如果我们直接将公式" (∃ $x (&= (&+ $x $1) $y))
      "中的" $y "替换为" $x ", 那么得到的公式"
      (∃ $x (&= (&+ $x $1) $x)) "并不是我们所想要的, "
      "因为替换进去了的变量" $x "被绑定了. "
      "我们想要做的事情是" (Em "alpha变换")
      ", 即对于绑定变量进行重命名, 例如这里将"
      $x "重命名为" $z ". 然后我们可以安全地进行替换以得到"
      (∃ $z (&= (&+ $z $1) $x))
      ", 这既按照要求替换了自由变量, 又维护了正确的绑定对应. "
      "为了实现这一点, 我们首先编写一个函数, "
      "通过不断向变量名添加撇号字符来发明一个变量名的" (Q "变体")
      ", 直到它与给定的需要避免的变量列表中的所有变量都不同为止; "
      "这将在必要时用于重命名绑定变量:"
      (CodeB "let rec variant x vars =
  if mem x vars then variant (x^&quot;'&quot;) vars else x;;")
      "例如:"
      (CodeB "# variant &quot;x&quot; [&quot;y&quot;; &quot;z&quot;];;
- : string = &quot;x&quot;
# variant &quot;x&quot; [&quot;x&quot;; &quot;y&quot;];;
- : string = &quot;x'&quot;
# variant &quot;x&quot; [&quot;x&quot;; &quot;x'&quot;];;
- : string = &quot;x''&quot;"))
   (P "现在, 替换的定义从一系列直接的结构递归开始. "
      "然而, 量化公式" (∀ $x $p) "和" (∃ $x $p)
      "这两种微妙情形由一个互递归的函数"
      (Code "substq") "处理:"
      (CodeB "let rec subst subfn fm =
  match fm with
    False -> False
  | True -> True
  | Atom(R(p,args)) -> Atom(R(p,map (tsubst subfn) args))
  | Not(p) -> Not(subst subfn p)
  | And(p,q) -> And(subst subfn p,subst subfn q)
  | Or(p,q) -> Or(subst subfn p,subst subfn q)
  | Imp(p,q) -> Imp(subst subfn p,subst subfn q)
  | Iff(p,q) -> Iff(subst subfn p,subst subfn q)
  | Forall(x,p) -> substq subfn mk_forall x p
  | Exists(x,p) -> substq subfn mk_exists x p"))
   (P "这个" (Code "substq") "函数会检查如果绑定变量"
      $x "没有重命名的话是否会出现变量捕获. "
      "它进行检测的方法是, 判断是否会出现"
      (&FV $p) "中的" (&!= $y $x)
      "使得应用替换于" $y "产生一个具有"
      $x "的自由出现的项. "
      "{译注: 当然了, 项压根没有绑定结构, 所以只要出现即可.} "
      "如果的确出现的变量捕获, 那么我们会挑选一个新的绑定变量"
      $x^ ", 其不会与对于" $p "进行替换产生的结果发生(变量)冲突; "
      "否则的话, 直接置" (&= $x^ $x)
      ". 整体的结果是应用带有额外映射" (&\|-> $x $x^)
      "的替换于体" $p "得到的. "
      "注意到在不需要换名的情况下, 其会阻止对于" $x
      "进行(非平凡的)替换, 这正是预期的行为."
      (CodeB "and substq subfn quant x p =
  let x' = if exists (fun y -> mem x (fvt(tryapplyd subfn y (Var y))))
                     (subtract (fv p) [x])
           then variant x (fv(subst (undefine x subfn) p)) else x in
  quant x' (subst ((x |-> Var x') subfn) p);;")
      "例如:"
      (CodeB "# subst (&quot;y&quot; |=> Var &quot;x&quot;) &lt;&lt;forall x. x = y>>;;
- : fol formula = &lt;&lt;forall x'. x' = x>>
# subst (&quot;y&quot; |=> Var &quot;x&quot;) &lt;&lt;forall x x'. x = y ==> x = x'>>;;
- : fol formula = &lt;&lt;forall x' x''. x' = x ==> x' = x''>>"))
   (P "我们希望这种重命名的微妙至少看上去还算合理. "
      "不过, 若要最终澄清我们的定义, 实际上我们需要表明"
      (Code "subst") "满足与相对于" (Code "tsubst")
      "而言的" (Ref "tsubst-free") "和" (Ref "tsubst-value")
      "类似的性质, 尽管建立这些性质要远为困难.")
   ((Lemma)
    "对于任意的公式" $p "和实例化" $i
    ", 替换后的项中的自由变量恰好是那些用于替换"
    $p "的自由变量的项中自由出现的变量, 即"
    (MB (&= (&FV (&subst $i $p))
            (Union (∈ $y (&FV $p))
                   (&FVT (app $i $y)))) "."))
   ((proof)

    )
   ((Theorem #:id "subst-value")
    "对于任意的公式" $p ", 实例化" $i
    ", 解释" $M "以及赋值" $v ", 我们有"
    (MB (&= (&holds $M $v (@subst $i $p))
            (&holds $M (@compose (@termval $M $v) $i) $p)) "."))
   ((proof)

    )
   (P "一个直接的推论如下, "
      "如果我们将自由变量想成是隐式全称量化的, "
      "那么这个结果的确不足为奇:")
   ((Corollary)
    "如果一个公式是有效的, 那么其任何替换实例也是有效的.")
   ((proof)
    "令" $p "是一个逻辑有效的公式. 对于任意的实例化"
    $i ", 我们有"
    (MB (&= (&holds $M $v (@subst $i $p))
            (&holds $M (@compose (@termval $M $v) $i) $p)
            $true) ".")
    "这是因为, 既然对于任意的赋值" $v
    "都有" (&= (&holds $M $v $p) $true)
    ", 那么" (&compose (@termval $M $v) $i)
    "作为赋值也不会例外. "
    "{译注: 一个微妙之处在于" $v "是依赖于" $M
    "的. 另外, 所有这些牵涉的对象都依赖于一个隐式的签名.} "
    "{再次译注: 我读到后面才意识到, "
    "作者在一般情况下应该都是假定解释/模型提供了一切函数符号和谓词符号的解释, "
    "并非总是假定存在一个隐式的签名. 不过, "
    "基本上所有结果也都可以在假定隐式签名的情况下成立, "
    "只不过有的或许牵涉签名的扩张等修改, "
    "所以说作者选择了这种解释一切的做法, "
    "因为会比较方便, 无需特殊说明.}")
   (P "替换的定义及其关键性质的证明相当乏味无聊. "
      "一种替代方案是将自由变量和绑定变量分为不同的句法范畴, "
      "从而使捕获不可能发生. "
      "一种特别流行的方案由de Bruijn (1972) 提出, "
      "它使用数值索引来表示绑定变量的嵌套深度. "
      "然而, 这种方法本身也有一些缺点.")
   (H3. "前束范式")
   (P "一个一阶公式被称为是具有"
      (Em "前束范式(prenex normal form, PNF)")
      ", 如果其所有的量词都出现在外部, "
      "而体 (或者说" (Q "matrix")
      ") 里只用到了命题联结词. 例如, "
      (∀ $x (∃ $y (∀ $z (&=> (&conj (app $P $x)
                                    (app $P $y))
                             (app $P $z)))))
      "具有PNF形式, 而"
      (&=> (@∃ $x (app $P $x))
           (∃ $y (&conj (app $P $y)
                        (∀ $z (app $P $z)))))
      "则不具有PNF形式, "
      "因为量化了的子公式使用了命题联结词进行组合. "
      "本节我们将会展示如何将任意的一阶公式"
      "转换为一个与之逻辑等价的PNF形式.")
   (P "当我们在命题逻辑中实现析取范式时 (第2.6节), "
      "我们考虑了两种方法, 一种基于真值表, "
      "另一种则是不断应用重言性变换, 例如"
      (&--> (&conj $p (@disj $q $r))
            (&disj (@conj $p $q)
                   (@conj $p $r)))
      ". 在一阶逻辑里, 我们没有和真值表手段类似的方法, "
      "但是我们仍然可以通过反复将子公式转换为与之逻辑等价的形式, "
      "从而将量词逐渐外拉, 最终将一个公式转换为前束范式. "
      "鉴于并无方便的手段能够将量词从逻辑等价式中拉出来, "
      "所以我们最好提前消去它们, 就像前一章里的命题否定范式. "
      "实际上, 如果我们遵循与早前DNF变换类似的模式, 会使问题得到简化:"
      (Ul (Li "将" (Code "False") ", " (Code "True")
              ", 空虚量化等东西化简消除;")
          (Li "消去推出式和等价式, 将否定下推;")
          (Li "将量词外拉.")))
   (P "化简阶段的处理方式和之前消去命题公式里的"
      (Code "False") "和" (Code "True")
      "时大致相同. 但是, 我们也会消除" (Em "空虚量词")
      ", 也就是量化变量没有在体中自由出现的情况.")
   ((Theorem)
    "如果" (&!in $x (&FV $p)) ", 那么"
    (∀ $x $p) "逻辑等价于" $p ".")
   ((proof)
    "公式" (∀ $x $p) "在模型" $M "和赋值" $v
    "下成立当且仅当对于" $M "的论域里的每个" $a
    ", " $p "在" $M "和赋值" (ext $v $x $a)
    "下成立. 然而, 既然" $x "并没有在" $p
    "中自由出现, 这种情况成立恰当" $p
    "在" $M "和" $v "下成立, 鉴于论域是非空的.")
   (P "类似地, 如果" (&!in $x (&FV $p))
      ", 那么" (∃ $x $p) "逻辑等价于" $p
      ". 因此, 我们可以看到下列化简函数"
      "总是返回一个逻辑等价的公式:"
      (CodeB "let simplify1 fm =
  match fm with
    Forall(x,p) -> if mem x (fv p) then fm else p
  | Exists(x,p) -> if mem x (fv p) then fm else p
  | _ -> psimplify1 fm;;")
      "然后我们可以将其深入地反复应用:"
      (CodeB "let rec simplify fm =
  match fm with
    Not p -> simplify1 (Not(simplify p))
  | And(p,q) -> simplify1 (And(simplify p,simplify q))
  | Or(p,q) -> simplify1 (Or(simplify p,simplify q))
  | Imp(p,q) -> simplify1 (Imp(simplify p,simplify q))
  | Iff(p,q) -> simplify1 (Iff(simplify p,simplify q))
  | Forall(x,p) -> simplify1(Forall(x,simplify p))
  | Exists(x,p) -> simplify1(Exists(x,simplify p))
  | _ -> fm;;")
      "例如:"
      (CodeB "# simplify &lt;&lt;true ==> (p &lt;=> (p &lt;=> false))>>;;
- : fol formula = &lt;&lt;p &lt;=> ~p>>
# simplify &lt;&lt;exists x y z. P(x) ==> Q(z) ==> false>>;;
- : fol formula = &lt;&lt;exists x z. P(x) ==> ~Q(z)>>
# simplify &lt;&lt;(forall x y. P(x) \\/ (P(y) /\\ false)) ==> exists z. Q>>;;
- : fol formula = &lt;&lt;(forall x. P(x)) ==> Q>>")
      "{译注: 这个过程" (Code "simplify")
      "的输出有一个很好的性质, 就是逻辑常量只能单独存在. "
      "实际上, 这也是我们对于第2章的对应过程的要求.}")
   (P "接着, 我们通过消去推出式和等价式并将否定下推以将公式转换为NNF形式. "
      "回忆一下De Morgan律, 其可以反复使用以获得逻辑等价的公式 "
      "{译注: 对于以下这些符号" (&cm $p_1 $p_2 $..h $p_n)
      ", 我们应该将其理解为代表(一阶)公式的元变量, "
      "而不应该像前一章那样将其理解为对象语言里的原子变量. "
      "当然了, 硬要说的话, 将它们理解为零元谓词也不是不行, "
      "但是没有必要采用这种别扭的角度}:"
      (eqn*
       ((&neg (@conj $p_1 $p_2 $..c $p_n))
        $<=>
        (&disj (&neg $p_1) (&neg $p_2) $..c (&neg $p_n)))
       ((&neg (@disj $p_1 $p_2 $..c $p_n))
        $<=>
        (&conj (&neg $p_1) (&neg $p_2) $..c (&neg $p_n)))))
   (P "根据类比, 我们有以下对于量词而言的" (Q "无穷De Morgan律")
      ". 这里的逻辑等价性应该相当清晰; "
      "例如, 如果不是对于所有的" $x "都有" (app $P $x)
      "成立的情形, 那么必然存在某个" $x "使得" (app $P $x)
      "不成立, 反之亦然:"
      (eqn*
       ((&neg (@∀ $x $p)) $<=> (∃ $x (&neg $p)))
       ((&neg (@∃ $x $p)) $<=> (∀ $x (&neg $p)))))
   (P "这些澄清了将否定下推经过量词的额外变换, "
      "以补充已在命题情形所使用了的变换, "
      "由此我们定义:"
      (CodeB "let rec nnf fm =
  match fm with
    And(p,q) -> And(nnf p,nnf q)
  | Or(p,q) -> Or(nnf p,nnf q)
  | Imp(p,q) -> Or(nnf(Not p),nnf q)
  | Iff(p,q) -> Or(And(nnf p,nnf q),And(nnf(Not p),nnf(Not q)))
  | Not(Not p) -> nnf p
  | Not(And(p,q)) -> Or(nnf(Not p),nnf(Not q))
  | Not(Or(p,q)) -> And(nnf(Not p),nnf(Not q))
  | Not(Imp(p,q)) -> And(nnf p,nnf(Not q))
  | Not(Iff(p,q)) -> Or(And(nnf p,nnf(Not q)),And(nnf(Not p),nnf q))
  | Forall(x,p) -> Forall(x,nnf p)
  | Exists(x,p) -> Exists(x,nnf p)
  | Not(Forall(x,p)) -> Exists(x,nnf(Not p))
  | Not(Exists(x,p)) -> Forall(x,nnf(Not p))
  | _ -> fm;;")
      "例如:"
      (CodeB "# nnf &lt;&lt;(forall x. P(x))
        ==> ((exists y. Q(y)) &lt;=> exists z. P(z) /\\ Q(z))>>;;
- : fol formula =
&lt;&lt;(exists x. ~P(x)) \\/
  (exists y. Q(y)) /\\ (exists z. P(z) /\\ Q(z)) \\/
  (forall y. ~Q(y)) /\\ (forall z. ~P(z) \\/ ~Q(z))>>"))
   (P "现在我们来到前束范式真正独特的部分, 即将量词拉出来. "
      "到目前为止, 我们已经进行了化简和NNF变换, "
      "任何不在外部的量词必然是由" (Q $conj) "或"
      (Q $disj) "所连接的, 因为否定已经下推至原子公式, "
      "而其他命题联结词已经被消除了. "
      "因此, 症结在于将诸如" (&conj $p (∃ $x $q))
      "这样的公式中的联结词上拉. "
      "又一次, 根据DNF的分配规则:"
      (MB (&<=> (&conj $p (@disj $q_1 $..c $q_n))
                (&disj (&conj $p $q_1) $..c
                       (&conj $p $q_n))))
      "我们可以作出无限情形的类比, "
      "似乎以下公式(模式)应该是逻辑有效的:"
      (MB (&<=> (&conj $p (@∃ $x $q))
                (∃ $x (&conj $p $q))) "."))
   (P "这几乎是正确的, 但是若" $x "自由出现于"
      $p ", 我们需要当心变量捕获. "
      "例如, 以下公式不是逻辑有效的:"
      (MB (&<=> (&conj (app $P $x)
                       (@∃ $x (app $Q $x)))
                (∃ $x (&conj (app $P $x)
                             (app $Q $x)))) "."))
   (P "如有必要, 我们总是可以通过对于绑定变量换名来避免这样的问题, "
      "即将" $x "重命名为某个" $y ", 而" $y
      "在" $p "或" $q "中都没有自由出现:"
      (MB (&<=> (&conj $p (@∃ $x $q))
                (∃ $y (&conj $p (@subst (@\|=> $x $y) $q)))) "."))
   (P "这种等价可以使用前一节的定理进行严格澄清. "
      "根据定义, 在模型" $M " (其论域为" $D ") 和赋值" $v
      "下, 公式" (&conj $p (@∃ $x $q)) "成立, 如果"
      (&holds $M $v $p) "且存在某个" (∈ $a $D)
      "使得" (&holds $M (@ext $v $x $a) $q) ". 公式"
      (∃ $y (&conj $p (@subst (@\|=> $x $y) $q)))
      "成立, 如果存在一个" (∈ $a $D) "使得"
      (&holds $M (@ext $v $y $a) $p) "且"
      (&holds $M (@ext $v $y $a)
              (@subst (@\|=> $x $y) $q))
      ". 然而, 既然根据构造, " $y "没在" $p
      "中自由出现, " (Ref "formula-var-agree")
      "表明" (&holds $M (@ext $v $y $a) $p)
      "等价于" (&holds $M $v $p) ". 至于"
      (&holds $M (@ext $v $y $a)
              (@subst (@\|=> $x $y) $q))
      ", 根据" (Ref "subst-value") ", 其等价于"
      (&holds $M (@compose (@termval $M (@ext $v $y $a))
                           (@subst (@\|=> $x $y)))
              $q)
      ", 因而又等价于"
      (&holds $M (@ext $v $x $a) $q)
      ", 这正是我们所要的. "
      "{译注: 这最后的两个等价于只要进行细致分析并不难理解. "
      "不过, 我还是要指明这里的符号和语义滥用, "
      "而归根结底这又应该算是程序和数学之间的gap. "
      "在之前的数学论述和证明里, 我们把" $subst
      "的第一个参数, 即所谓的实例化" $i
      ", 都是当作一个从变量到项的完全函数. "
      "但是在代码里, " $subst
      "的第一个参数是在OCaml里实现的所谓有限部分函数. "
      "这之间的语义差异, 是通过未定义变量会被映射至"
      "等同于自身的项这一操作而抹平的. "
      "但是, 从本质上来说, " $subst
      "的代码定义的确也能用在完全函数上, "
      "这里没有任何微妙之处. 此处证明的有趣之处在于, "
      "数学论述里" (Ref "subst-value")
      "需要在" $subst "的第一个参数是完全函数时才能应用, "
      "但是这里的" (@\|=> $x $y) "显然不是一个完全函数. "
      "那么, 作者其实是通过等式"
      (&= (&subst (@\|=> $x $y) $q)
          (&subst (@subst (@\|=> $x $y)) $q))
      "把替换延拓成了一个完全函数. "
      "而且细心的读者会发现, " $subst
      "之前在数学论述里只会接受完全函数, "
      "但这里打破了约定. 另外, "
      (@subst (@\|=> $x $y))
      "其实还用上了currying, "
      "并且本来可以接受任意的项, "
      "但这里作为替换只会接受变量. "
      "另外还可以说一点无聊的, 原文要求"
      $y "既不在" $p "又不在" $q
      "中自由出现, 但是最宽松的要求其实是"
      $y "不在" (&conj $p (@∃ $x $q))
      "中自由出现. 当然了, 原文也说了如有必要, "
      "那么说明" $x "已经在" $p
      "中自由出现, 所以" $y
      "的确不会是" $x
      ". (读者还可以看出经过了前面的化简之后, "
      "到了这里" $x "必然要在" $q
      "中自由出现, 不会落空.)} "
      "全然类似的论证允许我们将全称或存在量词经过合取或析取拉出来. "
      "如果读者对于其中任何逻辑等价保有疑问, "
      "那么的确它们可以按照类似的方法进行澄清:"
      (eqn*
       ((&conj (@∀ $x $p) $q)
        $<=>
        (∀ $y (&conj (@subst (@\|=> $x $y) $p) $q)))
       ((&conj $p (@∀ $x $q))
        $<=>
        (∀ $y (&conj $p (@subst (@\|=> $x $y) $q))))
       ((&disj (@∀ $x $p) $q)
        $<=>
        (∀ $y (&disj (@subst (@\|=> $x $y) $p) $q)))
       ((&disj $p (@∀ $x $q))
        $<=>
        (∀ $y (&disj $p (@subst (@\|=> $x $y) $q))))
       ((&conj (@∃ $x $p) $q)
        $<=>
        (∃ $y (&conj (@subst (@\|=> $x $y) $p) $q)))
       ((&conj $p (@∃ $x $q))
        $<=>
        (∃ $y (&conj $p (@subst (@\|=> $x $y) $q))))
       ((&disj (@∃ $x $p) $q)
        $<=>
        (∃ $y (&disj (@subst (@\|=> $x $y) $p) $q)))
       ((&disj $p (@∃ $x $q))
        $<=>
        (∃ $y (&disj $p (@subst (@\|=> $x $y) $q))))))
   (P "{再次译注: 非常不妙的是, 前一段中的译注有问题. "
      "不过, 容我自我辩白一下, 其实原文也有问题, "
      "所以我受到了误导. 译注里我说的等式"
      (MB (&= (&subst (@\|=> $x $y) $q)
              (&subst (@subst (@\|=> $x $y)) $q)))
      "实际上是错误的. " $subst
      "的第一个参数是所谓的实例化, "
      "其应该是从变量到项的映射, "
      "但是curry化了的" (@subst (@\|=> $x $y))
      "是一个从公式到公式的映射. "
      "其实, 正确的等式应该是"
      (MB (&= (&subst (@\|=> $x $y) $q)
              (&subst (@tsubst (@\|=> $x $y)) $q)))
      "所以原文相应的地方应该将" $subst
      "改成" $tsubst ". 不过, 从更为形式化的角度来看, "
      "这仍然并不正确. 更准确地说, 实例化是从"
      (Em "变量名") "到项的映射, 但是变量名并不是项, "
      "给变量名加上" $Var "之后才变成项. 因此, "
      "更为正确的等式其实是"
      (MB (&= (&subst (@\|=> $x $y) $q)
              (&subst (@compose (@tsubst (@\|=> $x $y))
                                $Var)
                      $q)))
      "然后, 原文的"
      (MB (&holds $M (@compose (@termval $M (@ext $v $y $a))
                               (@subst (@\|=> $x $y)))
                  $q))
      "实际上应该是"
      (MB (&holds $M (@compose (@termval $M (@ext $v $y $a))
                               (@tsubst (@\|=> $x $y))
                               $Var)
                  $q))
      "之后有一处译注我也犯了同样的错误.}")
   (P "在立即子公式都为量化公式的特殊情形下, "
      "我们有时可以运用逻辑等价产生具有更少量词的结果, "
      "其中的" $z "要求不在原本的公式里自由出现即可."
      (eqn*
       ((&conj (@∀ $x $p) (@∀ $y $q))
        $<=>
        (∀ $z (&conj (@subst (@\|=> $x $z) $p)
                     (@subst (@\|=> $y $z) $q))))
       ((&disj (@∃ $x $p) (@∃ $y $q))
        $<=>
        (∃ $z (&disj (@subst (@\|=> $x $z) $p)
                     (@subst (@\|=> $y $z) $q)))))
      "{译注: 个人喜欢将其理解为两个量化变量相同时的逻辑等价的衍生结果.}")
   (P "然而, 以下并不逻辑有效 "
      "{译注: 这里不是双重否定, 只是正常否定一次}:"
      (eqn*
       ((&disj (@∀ $x $p) (@∀ $y $q))
        $<=/=>
        (∀ $z (&disj (@subst (@\|=> $x $z) $p)
                     (@subst (@\|=> $y $z) $q))))
       ((&conj (@∃ $x $p) (@∃ $y $q))
        $<=/=>
        (∃ $z (&conj (@subst (@\|=> $x $z) $p)
                     (@subst (@\|=> $y $z) $q))))))
   (P "例如, 对于上面第一条, 如果这有这种等价的话, 那么"
      (&disj (@∀ $n (&Even $n)) (@∀ $n (&Odd $n)))
      "应该逻辑等价于"
      (∀ $n (&disj (&Even $n) (&Odd $n)))
      ". 可是在基于整数的奇偶性的显然解释下, "
      "前者为假而后者为真. "
      "类似地, 如果第二条正确, 那么"
      (&conj (@∃ $n (&Even $n)) (@∃ $n (&Odd $n)))
      "应该逻辑等价于"
      (∃ $n (&conj (&Even $n) (&Odd $n)))
      ", 然而在同样的显然解释下, 前者为真而后者为假.")
   (P "现在为了将所有出现于合取或者析取的立即子公式里的量词拉出来, "
      "我们在OCaml中实现了以下变换:"
      (CodeB "let rec pullquants fm =
  match fm with
    And(Forall(x,p),Forall(y,q)) ->
                          pullq(true,true) fm mk_forall mk_and x y p q
  | Or(Exists(x,p),Exists(y,q)) ->
                          pullq(true,true) fm mk_exists mk_or x y p q
  | And(Forall(x,p),q) -> pullq(true,false) fm mk_forall mk_and x x p q
  | And(p,Forall(y,q)) -> pullq(false,true) fm mk_forall mk_and y y p q
  | Or(Forall(x,p),q) ->  pullq(true,false) fm mk_forall mk_or x x p q
  | Or(p,Forall(y,q)) ->  pullq(false,true) fm mk_forall mk_or y y p q
  | And(Exists(x,p),q) -> pullq(true,false) fm mk_exists mk_and x x p q
  | And(p,Exists(y,q)) -> pullq(false,true) fm mk_exists mk_and y y p q
  | Or(Exists(x,p),q) ->  pullq(true,false) fm mk_exists mk_or x x p q
  | Or(p,Exists(y,q)) ->  pullq(false,true) fm mk_exists mk_or y y p q
  | _ -> fm")
      "其中为了经济性, 各种各样类似的子情形都由一个互递归函数"
      (Code "pullq") "处理, 其会调用主函数" (Code "pullquants")
      "以在体上进一步拉出更多的量词:"
      (CodeB "and pullq(l,r) fm quant op x y p q =
  let z = variant x (fv fm) in
  let p' = if l then subst (x |=> Var z) p else p
  and q' = if r then subst (y |=> Var z) q else q in
  quant z (pullquants(op p' q'));;"))
   (P "整体的前束函数将量化公式的量词留下, "
      "对于合取与析取则递归地将其立即子公式前束化, "
      "然后再应用" (Code "pullquants") ":"
      (CodeB "let rec prenex fm =
  match fm with
    Forall(x,p) -> Forall(x,prenex p)
  | Exists(x,p) -> Exists(x,prenex p)
  | And(p,q) -> pullquants(And(prenex p,prenex q))
  | Or(p,q) -> pullquants(Or(prenex p,prenex q))
  | _ -> fm;;")
      "将其与NNF与化简阶段组合, 我们就得到:"
      (CodeB "let pnf fm = prenex(nnf(simplify fm));;")
      "例如:"
      (CodeB "# pnf &lt;&lt;(forall x. P(x) \\/ R(y))
        ==> exists y z. Q(y) \\/ ~(exists z. P(z) /\\ Q(z))>>;;
- : fol formula =
&lt;&lt;exists x. forall z. ~P(x) /\\ ~R(y) \\/ Q(x) \\/ ~P(z) \\/ ~Q(z)>>"))
   (H3. "Skolem化")
   (P "前束范式将量词和命题部分 (或称" (Q "matrix") ") 分离开来, "
      "但是量词前缀仍然可能包含任意复杂度的全称和存在量词嵌套. "
      "我们可以更进一步, 消去存在量词而只保留全称量词, "
      "这用到了一种叫做" (Em "Skolem化")
      "的技术, 其以Thoraf Skolem (1928) 的名字而命名. "
      "注意到以下陈述一般被认为是数学等价的:"
      (Ol (Li "对于所有的" (∈ $x $D) ", 存在一个" (∈ $y $D)
              "使得" (Appl $P $x $y) ";")
          (Li "存在一个" (func $f $D $D) "使得对于所有的"
              (∈ $x $D) ", " (Appl $P $x (app $f $x)) ".")))
   (P "其中一个方向是相当简单的: 如果(2)成立, 那么取"
      (&= $y (app $f $x)) ", 我们可以看到(1)也成立. "
      "另一方向比较微妙: 即便对于每个" $x
      "都存在至少一个" $y "使得" (Appl $P $x $y)
      ", 然而也可能存在许多这样的元素, "
      "于是为了得到一个函数" $f ", 对于每个"
      $x "我们需要自限于一个特定的" $y
      ". 一般而言, 对于逐" $x "得" $y
      "的这样一种选择总是存在的断言 "
      "(即便我们无法写下挑选的方法), "
      "即是著名的选择公理 (Axiom of Choice, AC) "
      "(Moore 1982; Jech 1973). "
      "为了与通常的数学实践保持一致, "
      "我们简单地选择假定该公理成立, "
      "尽管这只是一种方便的做法, "
      "而且如有必要则可以避免.")
   (P "{原注: 选择公理在论域" $D
      "良序时是毫无疑义的, 而可数是其一种特殊情形, "
      "这是因为我们可以将" (app $f $x)
      "定义为满足" (Appl $P $x $y) "的最小的"
      $y ". 由向下Löwenheim–Skolem定理3.49可知, "
      "对于我们的可数语言, 本质上可以将注意力限制在可数模型上. "
      "尽管我们对该结论的证明使用了Skolem化方法, "
      "但Henkin (1949) 提出了一种更为精巧的方法来避免这一点, "
      "该方法代之以在可数个阶段中不断向语言添加新常量. "
      "Enderton (1972) 等若干教材以此方式证明了完备性定理.}")
   (P "即便接受了(1)和(2)的等价, "
      "后者也并不对应于某个一阶公式的语义. "
      "如果我们允许对于函数符号进行存在量化, "
      "以一种从直觉上来说看起来较为合理的方式对于语义的概念进行扩展, "
      "那么这种等价意味着以下公式(模式)应该是逻辑有效的:"
      (MB (&<=> (@∀ $x (∃ $y (Appl $P $x $y)))
                (@∃ $f (∀ $x (Appl $P $x (app $f $x))))))
      "更为一般地:"
      (MB (&<=> (@∀ $x_1 $..h $x_n
                    (∃ $y (Appl $P $x_1 $..h $x_n $y)))
                (@∃ $f (∀ $x_1 $..h $x_n
                          (Appl $P $x_1 $..h $x_n
                                (appl $f $x_1 $..h $x_n)))))))
   (P "在一个合适的" (Em "二阶逻辑")
      "系统之中, 这些的确是逻辑等价, "
      "并且我们可以使用它们对于前束公式的量词前缀进行变换"
      "以使得所有的存在量词都出现在所有的全称量词之前, 例如"
      (MB (deriv^0
           (@∀ $x (∃ $y (∀ $u (∃ $v (Appl $P $u $v $x $y)))))
           $<=>
           (@∃ $f (∀ $x $u (∃ $v (Appl $P $u $v $x (app $f $x)))))
           $<=>
           (@∃ $f $g (∀ $x $u (Appl $P $u (appl $g $x $u) $x (app $f $x)))))))
   (P "我们注意到, 不论是变换等价还是最终的结果, "
      "都无法以一阶公式表达, 所以我们不能确切遵循这个过程. "
      "但是, 我们可以大致取得相同的效果, "
      "如果我们能够接受作为结果的公式和原本的公式不是逻辑等价, "
      "而仅仅是" (Em "等可满足") " (第2.8节). "
      "要义在于函数上的存在量化已然在可满足性断言中隐式存在了: "
      "一个公式是可满足的, 如果" (Em "存在")
      "某个论域和对于函数与谓词符号的解释能够满足它. "
      "因此, 进行" (Em "Skolem化") "的这一行为得到了澄清, "
      "Skolem化指的是进行同样的变换, 但是不对于函数进行显式量化, "
      "例如我们将公式"
      (MB (∀ $x (∃ $y (∀ $u (∃ $v (Appl $P $u $v $x $y))))))
      "转换为"
      (MB (∀ $x $u (Appl $P $u (appl $g $x $u) $x (app $f $x))))
      "其中的" $f "和" $g "是不在原本公式里的不同函数符号. "
      "{译注: 其实无需强调不同, 因为这里的" $f "和" $g
      "不是元函数符号, 而是具体的函数符号.} "
      "另外, 既然对于自由变量的全称量化"
      "在可满足性的定义里是隐式存在的, "
      "我们还可以继续将其变换为"
      (MB (Appl $P $u (appl $g $x $u) $x (app $f $x)) "."))
   (P "虽然这些公式中没有任何两个是逻辑等价的, 但它们都是等可满足的. "
      "因此, 如果我们想判断第一个公式是否可满足, "
      "只需考虑最后一个公式即可, 因为它完全不含显式量词. "
      "我们将在下一节中看到, 此类无量词公式的可满足性问题"
      "可以借助命题逻辑中的技术加以解决. 但在此之前, "
      "让我们对主要的 Skolem 化变换给出更为细致严格的说明, "
      "并在此过程中定义实际实现中所用到的若干辅助概念.")
   (P "有必要引入称为" (Em "Skolem函数")
      "的新函数符号 (在零元情形下称为"
      (Em "Skolem常量") "), 且这些符号不得出现在原公式中. "
      "因此, 首先我们需要定义一个过程, "
      "用于获取一个项和一个公式中已经存在的函数, "
      "从而避免与之产生命名冲突. 这一实现是直接的, "
      "值得注意的是, 我们以名称和元数的序对来标识函数, "
      "因为同名但元数不同的函数被视为不同的函数."
      (CodeB "let rec funcs tm =
  match tm with
    Var x -> []
  | Fn(f,args) -> itlist (union ** funcs) args [f,length args];;

let functions fm =
  atom_union (fun (R(p,a)) -> itlist (union ** funcs) a []) fm;;"))
   (P "正如" (&holds $M $v $p) "只依赖于对于"
      (∈ $x (&FV $p)) "而言的取值" (app $v $x)
      " (" (Ref "formula-var-agree")
      "), 其也只依赖于实际出现于" $p
      "中的那些函数符号在" $M "下的解释. ("
      (Ref "formula-var-agree")
      "的证明可以经过调整移用; "
      "而且, 事情在某种意义上变得更简单了, "
      "因为变量绑定不发挥作用.) "
      "自现在起, 当我们言称"
      (Q $p "并不牵涉" $n "元函数符号" $f)
      "时, 从形式化的角度来看我们指的是"
      (&!in (tu0 $f $n) (&functions $p)) ".")
   ((Theorem)
    "如果" $p "是一个并不牵涉" $n
    "元函数符号" $f "的公式, 并且"
    (&= (&FV (∃ $y $p))
        (setE $x_1 $..h $x_n))
    " (其中" $x_i "互异, 但是顺序无关紧要), "
    "那么对于任意的解释" $M
    ", 存在另一个解释" $M^
    ", 其和" $M "只有对于" $f
    "的解释(可能)不同, 使得在所有赋值" $v "下:"
    (MB (&= (&holds $M $v (@∃ $y $p))
            (&holds $M^ $v
                    (@subst (@\|=> $y (appl $f $x_1 $..h $x_n))
                            $p))))
    "并且我们也有"
    (MB (&= (&holds $M $v (@∃ $y $p))
            (&holds $M^ $v (@∃ $y $p))))
    "因为" $p "并不牵涉" $f ".")
   (P "{译注: 这里的函数符号" $f "是和其元数" $n
      "绑定在一起的, 不只是名字.}")
   ((proof)
    "我们将" $M^ "定义为按照以下方式将对于" $f
    "的解释修改为了" (_ $f $M^) "的" $M
    ". 对于" (∈ $a_1 $..h $a_n $D)
    ", 如果存在某个" (∈ $b $D) "使得"
    (MB (&holds $M (tu0 (&\|=> $x_1 $a_1) $..h
                        (&\|=> $x_n $a_n)
                        (&\|=> $y $b))
                $p))
    "那么" (appl (_ $f $M^) $a_1 $..h $a_n)
    "就是某个这样的" $b ", 否则的话就选取任意一个"
    (∈ $b $D) ". 这个定义的要义在于对于一个任意的赋值"
    $v ", 断言"
    (MB (&holds $M^
                (@ext $v $y
                      (appl (_ $f $M^)
                            (app $v $x_1) $..h
                            (app $v $x_n)))
                $p))
    "和"
    (MB (存在某个
         (∈ $b $D)
         (&holds $M (@ext $v $y $b) $p)))
    "是等价的, 这是因为如果的确存在这样一个" $b
    ", 那么" (_ $f $M^) "就会挑选一个这样的元素. "
    "{译注: 这里还用到了" $n "元函数符号"
    $f "没有在" $p "中出现.} "
    "使用" (Ref "subst-value") "和这个等价, "
    "我们可以推出"
    (MB (deriv^
         (&holds $M^ $v
                 (@subst (@\|=> $y (appl $f $x_1 $..h $x_n)) $p))
         (&holds $M^
                 (@compose
                  (@termval $M^ $v)
                  (@subst (@\|=> $y (appl $f $x_1 $..h $x_n))))
                 $p)
         (&holds $M^
                 (@ext $v $y
                       (&termval $M^ $v
                                 (appl $f $x_1 $..h $x_n)))
                 $p)
         (&holds $M^
                 (@ext $v $y
                       (appl (_ $f $M^)
                             (app $v $x_1) $..h
                             (app $v $x_n)))
                 $p)
         (存在某个
          (∈ $b $D)
          (&holds $M (@ext $v $y $b) $p))
         (&holds $M $v (@∃ $y $p))))
    "{译注: 原文这里本来用的不是"
    (@subst (@\|=> $y (appl $f $x_1 $..h $x_n)))
    "而是" (@\|=> $y (appl $f $x_1 $..h $x_n))
    ", 为了与前文保持一致, "
    "我认为还是修改了的版本比较好.} "
    "这正是我们所要的.")
   (P "{再次译注: 根据之前的修正, 实际上"
      (MB (&holds $M^
                  (@compose
                   (@termval $M^ $v)
                   (@subst (@\|=> $y (appl $f $x_1 $..h $x_n))))
                  $p))
      "应该是"
      (MB (&holds $M^
                  (@compose
                   (@termval $M^ $v)
                   (@tsubst (@\|=> $y (appl $f $x_1 $..h $x_n)))
                   $Var)
                  $p))
      "非常抱歉.}")
   (P "因为这个等价对于所有的赋值成立, "
      "所以当子公式被替换时, "
      "等价可以沿着公式的结构向上传播, "
      "这是由于在" $termval "和" $holds
      "的递归定义里只有赋值改变了. "
      "{译注: 这句话十分令人难以理解, "
      "但是个人认为它表达的不过就是相对于某个特定解释/模型而言, "
      "如果两个公式逐赋值的真值相等, "
      "那么这个性质可以在任意的句法构造操作下保持. "
      "实际上, 这或许应该算是逻辑有效性在句法操作下保持的变体.} "
      "因此, 上述定理建立了以下结果: "
      "如果我们取某个任意的解释" $M "和一个具有子公式"
      (∃ $y $q) "的公式" $p ", 那么只要" $f
      "没有在整个公式" $p "里出现, 那么我们就能够以"
      $f "对于这个子公式进行Skolem化, 然后得到一个新的公式"
      $p^ ", 还有一个新的模型" $M^ ", 其和" $M
      "只在对于" $f "的解释上有所不同, "
      "其使得对于所有的赋值" $v ":"
      (MB (&= (&holds $M $v $p)
              (&holds $M^ $v $p^)) ".")
      "{译注: 读者应该还记得, 这里所说的函数符号"
      $f ", 是要考虑具体元数的, "
      "细节在之前的定理里已经详细表述过了.}")
   (P "这种操作可以反复进行, 替换所有的存在量化子公式, "
      "每个阶段选取某个还未出现在"
      "到目前为止的整个公式里的函数(符号). "
      "从初始公式" $p "和某个解释" $M
      "开始, 我们得到了一系列公式"
      (&cm $p_1 $..h $p_m) "和解释"
      (&cm $M_1 $..h $M_m)
      ", 其使得每个" (_ $M (&+ $k $1)) "仅是修改了"
      $M_k "对于一个新的Skolem函数得到的, 并且"
      (MB (&= (&holds $M_k $v $p_k)
              (&holds (_ $M (&+ $k $1)) $v
                      (_ $p (&+ $k $1)))) ".")
      "根据归纳, 对于所有的解释" $M
      "和所有的赋值" $v ", 我们有:"
      (MB (&= (&holds $M $v $p)
              (&holds $M_m $v $p_m)))
      "其中" $p_m "不含有存在量词. "
      "因此, 如果原本的公式" $p
      "是可满足的, 例如由某个模型" $M
      "满足, 那么Skolem化了的公式" $p_m
      "则由" $M_m "满足.")
   (P "这并不依赖于任何种类的预先规范形式变换; "
      "我们可以自由地应用Skolem化于任意的存在量化子公式, "
      "并且如果原本的公式是可满足的, "
      "那么Skolem化的结果也是可满足的. "
      "{译注: 这里说的就是指对于子公式"
      "进行Skolem化之后得到的整个公式.} "
      "反过来, 一个存在公式的Skolem化形式可以推出原本的公式 "
      "{译注: 这里的存在公式即公式本身就是存在量化, "
      "并且我们就是对于整个公式而非子公式进行Skolem化}, 所以"
      (Em "只要") "所有的Skolem化子公式以肯定方式出现 (以第2.5节之意), "
      "那么整个Skolem化了的公式就能逻辑推出原本的公式, "
      "因而它们是等可满足的. "
      "{译注: 这里说的" (Q "推出") "和" (Q "逻辑推出")
      ", 比如说" (Q $p "推出" $q) ", 大概指的是"
      (&=> $p $q) "是逻辑有效的, 展开来说就是对于任意的解释"
      $M "和任意的赋值" $v ", 如果" (&holds $M $v $p)
      "(为真), 那么" (&holds $M $v $q) "(为真).} "
      "在没有这个条件的情况下, 我们不能期望等可满足性质成立; "
      "例如, 如果我们对于不可满足公式"
      (&conj (@∃ $y (app $P $y))
             (&neg (@∃ $x (app $P $x))))
      "的第二个存在子公式进行Skolem化, "
      "那么我们就得到了可满足的"
      (&conj (@∃ $y (app $P $y))
             (&neg (app $P $c)))
      ". {译注: 这里的" $c "是零元函数, 或者说常量, "
      "但绝对不是变量. 在句法上它的确和一般变量没什么区别, "
      "我觉得这算是一个记号约定失误, 因为的确"
      (app $c) "是更一致和更好的选择.}")
   (P "{译注: 上一段对于" (Q "occur positively")
      "语焉不详, 实际上的确需要小心谨慎. "
      "不过, 鉴于后文在进行Skolem化之前会将公式变为NNF形式, "
      "所以说我们可以仅讨论为什么在NNF形式下"
      (Q "局部Skolem化") "是保持可满足性的. "
      "实际上, 我们只需要证明以下几点就够了. "
      (Ol (Li "如果" (&=> $p $q) "是逻辑有效的, 那么"
              (&=> (&conj $p $r) (&conj $q $r)) "和"
              (&=> (&conj $r $p) (&conj $r $q))
              "都是逻辑有效的;")
          (Li "如果" (&=> $p $q) "是逻辑有效的, 那么"
              (&=> (&disj $p $r) (&disj $q $r)) "和"
              (&=> (&disj $r $p) (&disj $r $q))
              "都是逻辑有效的;")
          (Li "如果" (&=> $p $q) "是逻辑有效的, "
              "那么对于任意的变量" $x ", "
              (&=> (@∀ $x $p) (@∀ $x $q)) "和"
              (&=> (@∃ $x $p) (@∃ $x $q))
              "都是逻辑有效的."))
      "不过, 这三点都足够显然, 所以留给读者也没问题. "
      "否定在NNF形式中只会应用于原子公式, 所以无需讨论.}")
   (P "因此, 先将公式转换为NNF形式是相当合理的, "
      "由此我们可以识别肯定和否定子公式, "
      "然后可以直接Skolem化掉所有的存在量词, "
      "此时它们都总是肯定出现. "
      "我们可以进一步先将公式置于PNF形式, "
      "不过往往先应用Skolem化更好, "
      "因为PNF变换可能会将更多的自由变量"
      "引入存在量词的作用域之中, "
      "使得Skolem函数需要更多参数. 例如, "
      (∀ $x $z (&disj (&= $x $z)
                      (∃ $y (&= (&d* $x $y) $1))))
      "可以被直接Skolem化为"
      (∀ $x $z (&disj (&= $x $z)
                      (&= (&d* $x (app $f $x)) $1)))
      ", 而如果我们先将其化为前束范式"
      (∀ $x $z (∃ $y (&disj (&= $x $z)
                            (&= (&d* $x $y) $1))))
      ", 那么继而Skolem化会给出"
      (∀ $x $z (∃ $y (&disj (&= $x $z)
                            (&= (&d* $x (appl $f $x $z))
                                $1))))
      ". 出于相同的理由, 似乎先对于外层量词进行Skolem化更为明智, "
      "因为这同样也会减少自由变量的数目, 例如"
      (MB (&--> (∃ $x $y (&= (&d* $x $y) $1))
                (∃ $y (&= (&d* $c $y) $1))
                (&= (&d* $c $d) $1)))
      "而非"
      (MB (&--> (∃ $x $y (&= (&d* $x $y) $1))
                (∃ $x (&= (&d* $x (app $f $x)) $1))
                (&= (&d* $c (app $f $c)) $1)) "."))
   (P "那么, 对于整体的Skolem化函数, "
      "我们直接对于公式进行递归下降, "
      "Skolem化任何遇到的存在公式, "
      "然后继续处理子公式. "
      "我们维护一个既已存在于公式中的函数符号列表" (Code "fns")
      ", 于是我们可以避免使用它们作为Skolem函数. "
      "(我们甚至保守地避免了使用同名不同元数的函数, "
      "这在逻辑上并无必要, 但有时可以避免阅读结果时产生误解. "
      "另一方向的优化可以是对于等同的Skolem公式复用相同的Skolem函数; "
      "一些对于Skolem化主要定理的思考表明这是可行的.)"
      (CodeB "let rec skolem fm fns =
  match fm with
    Exists(y,p) ->
        let xs = fv(fm) in
        let f = variant (if xs = [] then &quot;c_&quot;^y else &quot;f_&quot;^y) fns in
        let fx = Fn(f,map (fun x -> Var x) xs) in
        skolem (subst (y |=> fx) p) (f::fns)
  | Forall(x,p) -> let p',fns' = skolem p fns in Forall(x,p'),fns'
  | And(p,q) -> skolem2 (fun (p,q) -> And(p,q)) (p,q) fns
  | Or(p,q) -> skolem2 (fun (p,q) -> Or(p,q)) (p,q) fns
  | _ -> fm,fns"))
   (P "在处理二元联结词时, 用以避免重名的函数符号集"
      "需要在处理另一个公式之前先更新第一个公式所引入的Skolem函数, "
      "由此我们定义了辅助函数" (Code "skolem2") ":"
      (CodeB "and skolem2 cons (p,q) fns =
  let p',fns' = skolem p fns in
  let q',fns'' = skolem q fns' in
  cons(p',q'),fns'';;"))
   (P (Code "skolem") "函数意在于NNF变换之后应用, "
      "因而对于否定式, 推出式, 等价式, 以及原子公式保持不变. "
      "{译注: 其实还有可能是" $bottom "和" $top
      ", 不过实际上除了这两个逻辑常量之外, "
      "我们只会遇到原子公式和对于原子公式的否定, "
      "其他情况都被NNF变换消除了.} "
      "对于总体上的Skolem化函数, 我们先化简, "
      "然后将其转换为NNF形式, 接着应用" (Code "skolem")
      "以一个合适的初始避免重名函数符号集:"
      (CodeB "let askolemize fm =
  fst(skolem (nnf(simplify fm)) (map fst (functions fm)));;")
      "{译注: 鉴于函数符号有同名不同元数的情况, "
      "所以个人感觉这里应该使用"
      (Code "image") "而非" (Code "map")
      ". 另外, NNF变换并不改变公式所具有的函数符号.}")
   (P "往往我们想要将结果转化为PNF形式并省略全称量词, "
      "这可以给出一个没有显式量词的等可满足公式. "
      "最后一步需要一个新的函数, 尽管相当简单:"
      (CodeB "let rec specialize fm =
  match fm with
    Forall(x,p) -> specialize p
  | _ -> fm;;")
      "然后我们可以将这些碎片拼在一起:"
      (CodeB "let skolemize fm = specialize(pnf(askolemize fm));;")
      "{译注: 个人之见, 使用" (Code "prenex")
      "就完全足够了, 无需使用" (Code "pnf")
      ", 因为" (Code "askolemize") "能够保持NNF的结构.} "
      "例如:"
      (CodeB "# skolemize &lt;&lt;exists y. x &lt; y ==> forall u. exists v. x * u &lt; y * v>>;;
- : fol formula = &lt;&lt;~x &lt; f_y(x) \\/ x * u &lt; f_y(x) * f_v(u,x)>>
# skolemize
 &lt;&lt;forall x. P(x)
             ==> (exists y z. Q(y) \\/ ~(exists z. P(z) /\\ Q(z)))>>;;
- : fol formula = &lt;&lt;~P(x) \\/ Q(c_y) \\/ ~P(z) \\/ ~Q(z)>>"))
   (P "尽管在实践中我们通常对于Skolem化掉"
      "一个公式或者一集公式里的所有存在量词感兴趣, "
      "但是需要指出的是我们不是非得这么做. "
      "如果我们对于一个公式" $p "进行Skolem化得到了"
      (&* $p) ", 不仅是这两个公式为等可满足的, "
      "而且只要新引入的那些Skolem函数没有出现在另一个公式" $q
      "中, 那么" (&conj $p $q) "和" (&conj (&* $p) $q)
      "也是等可满足的, 只需将相同的推理应用于"
      (&conj $p $q) "并保留" $q "中的存在量词. "
      "{译注: 这里所说的Skolem化的含义和之前有所不同, "
      "实际上应该相当于应用" (Code "askolemize")
      "或者" (Code "skolemize")
      "函数. 这是因为, 如果只是朴素地对于" $p
      "应用局部Skolem化, 前文已经告诉我们可满足性无法保持. "
      "不过, 读者不能忽略细节问题, 应该意识到"
      (Code "skolemize") "在此处之所以适用, "
      "需要分成两步论证, 第一步和" (Code "askolemize")
      "完全相同, 第二步则用到了对于任意公式" $p
      "和任意变量" $x ", " $p "和" (∀ $x $p)
      "逐模型的可满足性相同.} "
      "这进一步推出对于句子" $p "和" $q
      ", 我们有" (!= (&=> $p $q)) "当且仅当"
      (!= (&=> (&* $p) $q)) ", 只要" $q
      "并不牵涉任何的Skolem函数, 这是因为"
      (!= (&=> $p $q)) "当且仅当"
      (&conj $p (&neg $q)) "是不可满足的. "
      "{译注: 这里的Skolem化的含义或许应该说又和前文不同了, "
      "要义在于" (&* $p) "也必须要是一个句子, 即没有自由变量. "
      (Code "simplify") "和" (Code "nnf")
      "实际上都可以保持自由变量集不变, " (Code "askolemize")
      "也可以, 因为原始的对于存在量化公式进行的Skolem化"
      "的确也保持自由变量集不变. 所以说, "
      (Code "askolemize") "能够胜任这里言称的Skolem化. "
      "接着, 我们分析" (Code "pnf") "函数, "
      "然后会发现因为量词上拉的过程总是伴随着精心的避免捕获的重命名, "
      "所以说" (Code "pnf") "也能够保持自由变量集不变. "
      "问题出在最后的" (Code "specialize")
      "上, 简单去除有可能导致自由变量增加, "
      "在这里则实际上是一定增加. 换言之, "
      (Code "skolemize") "不能胜任, 但是"
      "如果只是" (Code "askolemize") "后面跟着"
      (Code "pnf") "则是可以的.} "
      "我们通过言称Skolem化是" (Em "保守的")
      "以表达这一事实: 如果" $q
      "由一个Skolem化了的公式推出, "
      "那么其必然也可由未Skolem化的公式推出, 只要"
      $q "并不牵涉任何Skolem函数.")
   (P "以一个不同的方向, 我们可以直接论证推出以下定理, "
      "尽管直接证明也不困难:")
   ((Theorem)
    "一个公式" $p "是有效的当且仅当" $p^
    "是有效的, 其中" $p^ "是将" $p
    "中的所有自由变量替换为没有出现在" $p
    "中的不同常量. {译注: 常量即零元函数.}")
   ((proof)
    "对于所有的自由变量进行泛化, 然后作否定, "
    "接着应用Skolem化于那些外部的量化变量.")
   (P "{译注: 补充一下细节, 设"
      (&cm $x_1 $..h $x_n)
      "是" $p "的所有不同的自由变量, 那么"
      $p "是有效的当且仅当"
      (∀ $x_1 $..h $x_n $p)
      "是有效的, 当且仅当"
      (MB (&= (&neg (@∀ $x_1 $..h $x_n $p))
              (∃ $x_1 $..h $x_n (&neg $p))))
      "是不可满足的, 当且仅当" (&neg $p^)
      "是不可满足的, 当且仅当" $p^
      "是有效的.}")
   (P "Skolem函数似乎看上去只是纯粹的形式逻辑人工制品, "
      "但运用函数而非量词嵌套来表示依赖关系实际上在数学中相当常见, "
      "即便有时是无意识的, 而且只是半形式化的. "
      "例如, Burkill和Burkill (1970) 等分析学教材在处理具有形式"
      (Q (∀ $epsilon (&=> (&> $epsilon $0) (∃ $delta $..h))))
      "的典型" $epsilon "-" $delta "逻辑断言时, 有时会将其写成"
      (Q "对于所有的" (&> $epsilon $0) ", 存在一个"
         (&> (app $delta $epsilon) $0) "使得...")
      ", 这以记号" (app $delta $epsilon) "强调了"
      $delta "(可能)依赖于" $epsilon
      ". 本节开头的讨论表明, "
      "这样的函数式记号可以按照字面来理解, 通过将"
      $delta "视为一个Skolem函数, 其是由对于"
      (∀ $epsilon (∃ $delta (Appl $P $epsilon $delta)))
      "进行Skolem化而得到"
      (∃ $delta (∀ $epsilon (Appl $P $epsilon (app $delta $epsilon))))
      "的过程中产生的. {译注: 准确来说, 这不是Skolem化, "
      "而是Skolem化的想法起源.} "
      "实际上, Skolem函数所能表达的依赖关系比一阶量词更为精细, "
      "这促使人们去研究更一般的" (Q "分支") "量词 (Hintikka 1996).")
   (H3. "canonical模型")
   (P "一个无量词公式可以视为命题逻辑公式. "
      "现在我们有的不是" (Code "prop")
      "作为命题变量的原始集合, "
      "而是关系应用于项, 这对应于我们的OCaml类型"
      (Code "fol") ", 但是这不会造成本质性的区别, "
      "因为理论结果几乎不依赖于潜在集合的性质. "
      "特别是一个给定的一阶公式只能牵涉有限多个变量, 函数和谓词, "
      "故原子命题的集合是可数的, "
      "因而我们对于命题紧致性 (定理2.13) 的证明也能够施行. "
      "{译注: 个人感觉这句话的因果关系有点奇怪. "
      "就我个人而言, 如果变量集合, 函数符号集合, "
      "谓词符号集合都是可数的, "
      "那么原子命题的集合显然是可数的.} "
      "我们将会使用命题性求值" $eval
      "的微小变体, 其中为了方便, 一个命题性赋值"
      (Code "d") "将原子公式自身映射至真值. 函数"
      $pholds "确定了一个公式是否"
      "在命题逻辑的意义下对于这种赋值概念成立. "
      "(如若该函数应用于一个包含量词的公式, 则会失败.)"
      (CodeB "let pholds d fm = eval fm (fun p -> d(Atom p));;")
      "{译注: 本书通过精致的设计安排使得第2章命题逻辑的"
      $eval "也能用在第3章的一阶情形, 不过"
      $pholds "和" $eval
      "的差异几乎完全只是技术性的, 即它会把"
      $eval "脱去的" (Code "Atom")
      "又加回来, 仅此而已.}")
   (P "这个修饰了的赋值概念纯粹只是形式上的调整, "
      "这是为了避免" (Code "Atom")
      "映射反复出现在我们的定理之中, 不过与"
      (Code "Atom") "的复合定义了其与命题赋值的原本概念"
      "之间的一个自然双射, 所以一个无量词公式" (Code "p")
      "在命题逻辑的意义下是有效的当且仅当对于所有的赋值"
      (Code "d") "都有" (Code "pholds d p")
      ", " (Code "p") "是可满足的当且仅当对于某个赋值"
      (Code "d") "有" (Code "pholds d p")
      ". {译注: 这里也就是说, 在某种意义上忽略谓词的细致结构, "
      "只是为每种谓词指派一个真值.} "
      "现在我们也要证明一个无量词公式"
      "在一阶逻辑的意义下是有效的当且仅当"
      "其在命题逻辑的意义下是有效的, "
      "这是通过设置一阶解释和赋值与"
      "相应的命题赋值之间的对应关系完成的. "
      "其中一个方向相当直接. "
      "每个解释" $M "和赋值" $v
      "以自然的方式定义了一个与之对应的"
      "原子公式的命题赋值, 即" (&holds $M $v)
      ". 那么, 我们有:")
   ((Theorem #:id "Mv2d")
    "如果" $p "是一个无量词一阶公式, "
    "那么对于所有的解释"
    $M "的赋值" $v ", 我们有"
    (MB (&= (&pholds (@holds $M $v) $p)
            (&holds $M $v $p)) "."))
   ((proof)
    "直接对于" $p "的结构应用结构归纳, "
    "因为对于无量词一阶公式而言, " $holds
    "和" $pholds "具有相同的递归模式, "
    "而对于原子公式而言, 结果根据定义即成立.")
   ((Corollary #:id "prop->first")
    "如果一个无量词一阶公式是一个重言, 那么其也是一阶有效的.")
   ((proof)
    "在任意的解释" $M "和赋值" $v
    "下, 我们在之前的定理中已经表明了"
    (MB (&= (&holds $M $v $p)
            (&pholds (@holds $M $v) $p)) ".")
    "然而, 如果" $p "是一个命题重言, "
    "右边就只会是" (Q $true) "而已.")
   (P "现在我们转向相反的方向: "
      "给定一个原子公式上的命题性赋值" $d
      ", 构造解释" $M "和赋值" $v "使得"
      (MB (&= (&holds $M $v $p)
              (&pholds $d $p)) ".")
      "又一次, 我们仅需保证其对于原子公式为真, "
      "因为在对于" (Ref "Mv2d") "的证明中, "
      "我们注意到" $holds "和" $pholds
      "对于无量词公式的递归(模式)恰好相同. "
      "所有的(一阶)原子公式都具有形式"
      (appl $R $t_1 $..h $t_n)
      ", 而根据定义, 我们有"
      (MB (&= (&holds $M $v (appl $R $t_1 $..h $t_n))
              (appl $R_M (&termval $M $v $t_1) $..h
                    (&termval $M $v $t_n))) "."))
   (P "我们想要调制一种解释" $M "和赋值" $v
      "使得这与" (&pholds $d (appl $R $t_1 $..h $t_n))
      "是相等的. 实际上只要构造函数的解释以及赋值使得不同的项元组"
      (tu0 $t_1 $..h $t_n) "映射至不同的论域元素的元组"
      (tu0 (&termval $M $v $t_1) $..h
           (&termval $M $v $t_n))
      "就够了, 这是因为然后我们就可以按照要求"
      "选择对于谓词符号的解释以匹配命题赋值"
      $d ". (如果"
      (&!= (app $d (appl $R $s_1 $..h $s_n))
           (app $d (appl $R $t_1 $..h $t_n)))
      "而这两个项元组有着相同的解释的话, "
      "选择是不可能的.)")
   (P "{译注: 将不同的项元组映射至"
      "不同的论域元素元组这一条件等价于"
      (@termval $M $v) "是一个单射. "
      "这相当显然, "
      "如果不同的项元组能够映射至不同的论域元素元组, "
      "那么它也能推出不同的单元组要映射至不同的单元组, "
      "这和" (@termval $M $v) "为单射是等价的. "
      "现在论证另一个方向, 若"
      (@termval $M $v) "是一个单射, 那么如果"
      (&!= (tu0 $t_1 $..h $t_n) (tu0 $s_1 $..h $s_n))
      ", 这相当于至少存在一个" $i "使得"
      (&!= $t_i $s_i) ", 那么也就有"
      (&!= (&termval $M $v $t_i)
           (&termval $M $v $s_i))
      ", 由此映射至的论域元组不同是显然的事实.}")
   (P "这种条件可以用各种各样的方法达成, "
      "但是或许最直接的方式在于将模型的论域"
      "取为项集合本身的某个子集. 公式"
      $p "的一个" (Em "canonical解释")
      "是这样的, 其论域是项集合的某个子集, "
      "并且出现于" $p "中的每个" $n
      "元函数(符号)" $f
      "都以自然的方式解释为一个句法构造子, 即"
      (&= (appl $f_M $t_1 $..h $t_n)
          (appl $f $t_1 $..h $t_n))
      ", 或者基于我们的OCaml实现严格来说是"
      (&Fn $f $t_1 $..c $t_n)
      ". 既然对于函数符号的解释需要将"
      $D^n "映射至" $D
      ", 我们要求论域在出现于" $p
      "中的函数符号的(句法)应用下封闭, 即若"
      (∈ $t_1 $..h $t_n $D) ", 那么"
      (∈ (appl $f $t_1 $..h $t_n) $D)
      ". 作为特殊情形, 对于" $p
      "中的每个常量 (零元函数) 都有"
      (∈ $c $D) ". 一种可能性是将" $D
      "取为" (Em "所有") "项之集合. "
      "现在给定一个命题性赋值" $d
      ", 我们可以构造一个相应的canonical解释"
      $M_d ", 根据强制要求我们将函数符号解释为:"
      (MB (&= (appl (_ $f $M_d) $t_1 $..h $t_n)
              (appl $f $t_1 $..h $t_n)))
      "并且谓词要按照以下方式解释:"
      (MB (&= (appl (_ $R $M_d) $t_1 $..h $t_n)
              (app $d (appl $R $t_1 $..h $t_n))) "."))
   (P "现在我们有了所要的对应, 至少对于恒等赋值"
      $Var "而言, 其将一个变量" (Q "映射至自身")
      ". 这给出了以下毫不令人意外的性质, 即"
      (&termval $M_d $Var) "是恒等映射:")
   ((Lemma)
    "对于所有的项" $t ", "
    (&= (&termval $M_d $Var $t) $t) ".")
   ((proof)
    "对于" $t "的结构施行归纳. 如果" $t
    "是一个变量" (&Var $x) ", 那么"
    (&= (&termval $M_d $Var (@Var $x)) (&Var $x))
    ", 这是根据定义得到的. "
    "{译注: 或许有点tricky, 虽然" (&Var $x)
    "可由" $Var "应用于" $x
    "得到, 但是这里的" (&Var $x)
    "读者最好理解为某种意义上的字面结构.} "
    "不然的话, 若" $t "具有形式"
    (appl $f $t_1 $..h $t_n)
    ", 根据归纳假设, 对于每个"
    (&= $k (&cm $1 $..h $n))
    ", 我们有"
    (&= (&termval $M_d $Var $t_k) $t_k)
    ", 于是"
    (MB (deriv^
         (&termval $M_d $Var (appl $f $t_1 $..h $t_n))
         (appl (_ $f $M_d)
               (&termval $M_d $Var $t_1) $..h
               (&termval $M_d $Var $t_n))
         (appl (_ $f $M_d) $t_1 $..h $t_n)
         (appl $f $t_1 $..h $t_n)
         $t))
    "这正是我们所要的.")
   ((Theorem #:id "d2Mv")
    "如果" $d "是一个对于原子公式的赋值, "
    "那么对于任意的无量词公式" $p
    ", 我们有:"
    (MB (&= (&holds $M_d $Var $p)
            (&pholds $d $p)) "."))
   ((proof)
    "对于" $p "的结构施行归纳. 对于原子公式而言:"
    (MB (deriv^
         (&holds $M_d $Var (appl $R $t_1 $..h $t_n))
         (appl (_ $R $M_d)
               (&termval $M_d $Var $t_1) $..h
               (&termval $M_d $Var $t_n))
         (appl (_ $R $M_d) $t_1 $..h $t_n)
         (app $d (appl $R $t_1 $..h $t_n))
         (&pholds $d (appl $R $t_1 $..h $t_n))))
    "其他情形都是直截了当的, 因为对于无量词公式而言, "
    $holds "和" $pholds "有着相同的递归模式.")
   (P "这允许我们证明一阶有效性和命题有效性是一样的.")
   ((Corollary #:id "prop=first")
    "一个无量词一阶公式是一个命题重言当且仅当其是一阶有效的.")
   ((proof)
    "自左向右的方向已经在" (Ref "prop->first")
    "中得到了证明. 反过来, 设" $p
    "是一阶有效的, 那么对于任意的命题性赋值"
    $d ", 我们根据上述定理有"
    (&= (&pholds $d $p)
        (&holds $M_d $Var $p))
    ". 然而, 既然" $p "是一阶有效的, "
    "其在所有解释和赋值下皆成立, "
    "故等式右侧为" (Q $true) ".")
   ((Corollary #:auto? #f)
    "这个推论是译者插入的, 不属于原文的一部分. "
    "两个无量词一阶公式是命题逻辑等价的"
    "当且仅当其是一阶逻辑等价的.")
   (P "这是一个有趣的结果, 但是就我们的总体计划而言, "
      "我们对于可满足性的类似结果更为感兴趣, "
      "因为Skolem化 (使用我们的方法可以抵达一个无量词公式) "
      "是可满足性保持的但并非有效性保持的. 对于"
      (Em "ground") "公式而言, 一切都很简单:")
   ((Corollary)
    "一个ground公式是命题有效的当且仅当是一阶有效的, "
    "是命题可满足的当且仅当是一阶可满足的. "
    "{译注: 根据定义, ground公式必然是无量词公式.}")
   ((proof)
    "第一部分是" (Ref "prop=first") "的一种特殊情形. "
    "第二部分则是根据以下事实推出的: "
    "对于ground公式" $p "而言, " $p
    "的命题有效性和" (&neg $p)
    "的命题不可满足性是相同的, "
    $p "的一阶有效性和" (&neg $p)
    "的一阶不可满足性是相同的.")
   (P "{译注: 原文的证明叙述实际上有点绕, "
      "我将其改写为了更为易读的形式. "
      "不过, 这和最终结果仍然有差距, "
      "第二部分的证明实际上是这样的. "
      "对于ground公式" $p "而言, "
      (&neg $p) "的命题有效性等价于"
      (&neg (&neg $p)) "的命题不可满足性, "
      "等价于" $p "的命题不可满足性; "
      (&neg $p) "的一阶有效性等价于"
      (&neg (&neg $p)) "的一阶不可满足性, "
      "等价于" $p "的一阶不可满足性, "
      "这是由于ground公式没有自由变量. "
      "既然我们已经知道" (&neg $p)
      "的命题有效性和一阶有效性是等价的, 那么" $p
      "的命题不可满足性和一阶不可满足性也是等价的. "
      "又因为实际上命题不可满足性是对于命题可满足性的否定, "
      "一阶不可满足性是对于一阶可满足性的否定, "
      "所以说" $p "的命题可满足性和一阶可满足性是等价的. "
      "另外多说一句, 之所以这里要求的是ground公式, "
      "是因为没有自由变量的无量词公式这一条件"
      "恰好就和ground公式是等价的.}")
   (P "由此我们澄清了以下事实: 对于ground公式, "
      "我们可以自由地在命题和一阶有效性之间切换, "
      "也可以自由地在命题和一阶可满足性之间切换. "
      "那么对于一般情况下的无量词公式而言呢? "
      "又一次, 其中一个方向是直截了当的:")
   ((Corollary)
    "如果一个无量词一阶公式是一阶可满足的, "
    "那么其也是命题可满足的.")
   ((proof)
    "如果" $p "不是命题可满足的, 那么"
    (&neg $p) "是命题有效的, 根据"
    (Ref "prop=first")
    ", 其也是一阶有效的, 于是"
    $p "不可能是一阶可满足的.")
   (P "{译注: 我感觉这个证明有点绕, "
      "实际上可以直接证明. "
      "若无量词一阶公式" $p
      "是一阶可满足的, 那么存在解释"
      $M "使得对于任意的(一阶)赋值" $v
      "都有" (&holds $M $v $p)
      ". 我们选取其中一个赋值" $v_0
      ", 注意论域的非空性保证了赋值的存在性. "
      "根据本节开头的" (Ref "Mv2d")
      ", 命题性赋值" (@holds $M $v_0)
      "可使得公式" $p
      "在命题逻辑意义下得到满足.}")
   (P "然而, 一点反思表明反向的关系没有那么简单. 例如, "
      (&conj (app $P $x) (&neg (app $P $y)))
      "作为一个命题公式是可满足的, "
      "这是因为原子公式" (app $P $x) "和" (app $P $y)
      "是不同的, 故可以分别解释为" (Q $true)
      "和" (Q $false) ". 然而, "
      "其作为一阶公式是不可满足的, "
      "因为其模型需要使得该公式在" (Em "所有")
      "赋值下成立, 一个特殊情形是指派"
      $x "和" $y "以相同的论域值. "
      "{译注: 那么, 这种情况下无论如何都不可能成立.}")
   (P "我们的处理方式是首先对于" (Ref "d2Mv")
      "进行泛化. 注意到canonical模型下的赋值是从变量名到项的映射, "
      "于是其可以视为实例化.")
   ((Lemma #:id "valM=subst")
    "如果" $M "是任意的canonical解释而" $v "是任意的赋值, "
    "那么对于任意的项" $t ", 我们有"
    (MB (&= (&termval $M $v $t)
            (&tsubst $v $t)) "."))
   ((proof)
    (&termval $M) "和" $tsubst
    "的定义在任意的canonical模型下都是相同的, 因为每个"
    $f_M "不过就是作为句法构造子的" $f ".")
   (P "{译注: 其实严格来说, 它们并不全然相同. "
      $termval "要求的赋值是完全函数, 而" $tsubst
      "要求的实例化可以是部分函数. "
      "不过, 既然这里的" $v "是完全函数, "
      "所以说此时它们的确没有什么区别.}")
   (P "我们首先注意到一个简单的推论, "
      "尽管直接证明也相当容易.")
   ((Corollary #:id "icompose")
    "如果" $i "和" $j "是两个实例化而" $t
    "是任意的项, 那么"
    (MB (&= (&tsubst $i (@tsubst $j $t))
            (&tsubst (@compose (@tsubst $i) $j) $t)) "."))
   ((proof)
    "选取一个任意的canonical解释" $M
    " (例如将所有的关系都解释为恒假). 根据"
    (Ref "valM=subst") ", 这个声明等价于"
    (MB (&= (&termval $M $i (@tsubst $j $t))
            (&termval $M (@compose (@termval $M $i) $j) $t)))
    "其恰好就是" (Ref "tsubst-value") ".")
   (P "不过, 我们的主要结果如下.")
   ((Theorem #:id "thm3-21")
    "如果" $p "是一个无量词公式, " $d
    "是一个原子公式的命题性赋值, " $M
    "是某个对于" $p "而言的canonical解释, "
    "并且该解释满足"
    (&= (appl $R_M $t_1 $..h $t_n)
        (app $d (appl $R $t_1 $..h $t_n)))
    ", 那么对于任意的赋值" $v ", 我们有:"
    (MB (&= (&holds $M $v $p)
            (&pholds $d (@subst $v $p))) "."))
   ((proof)
    "对于" $p "的结构施行归纳. 对于原子公式:"
    (MB (deriv^
         (&holds $M $v (appl $R $t_1 $..h $t_n))
         (appl $R_M (&termval $M $v $t_1) $..h
               (&termval $M $v $t_n))
         (appl $R_M (&tsubst $v $t_1) $..h
               (&tsubst $v $t_n))
         (app $d (appl $R (&tsubst $v $t_1) $..h
                       (&tsubst $v $t_n)))
         (app $d (&subst $v (appl $R $t_1 $..h $t_n)))
         (&pholds $d (@subst $v (appl $R $t_1 $..h $t_n)))))
    "而对于其他种类的公式, " $holds "和" $pholds
    "的递归模式和之前一样是契合的.")
   (P "出于实用目的, 使得canonical模型的论域尽可能小比较方便. "
      "一个特定的一阶语言的" (Em "Herbrand宇宙") "或者说"
      (Em "Herbrand论域") "是该语言的所有" (Em "ground")
      "项之集合, 即所有可由语言的常量和函数符号"
      "在不使用变量的情况下所能构造出来的所有项, "
      "除非这个语言没有常量, 那么我们会添加一个常量"
      $c "以使得Herbrand宇宙非空. "
      "在接下来的部分里通常我们所感兴趣的会是一个单独公式"
      $p "的语言, 而我们会说" $p "的Herbrand宇宙, "
      "意即" $p "的语言的Herbrand宇宙. "
      "{译注: 这里所说的" $p "的语言, 大概指的是由" $p
      "牵涉的函数符号和谓词符号确定的签名所对应的语言.} "
      "我们可以取得一个公式的所有函数符号之集合, "
      "将其分为零元和非零元符号两类, "
      "并在没有零元符号时作出如前所述的添加常量的调整, "
      "整个函数如下:"
      (CodeB "let herbfuns fm =
  let cns,fns = partition (fun (_,ar) -> ar = 0) (functions fm) in
  if cns = [] then [&quot;c&quot;,0],fns else cns,fns;;")
      "{译注: 首先, 这里原文把公式误写作了项. "
      "另外, 这里的代码似乎有错, "
      (Code "[&quot;c&quot;,0]") "实际上应该是"
      (Code "[(&quot;c&quot;,0)]") ".}")
   (P "注意到" $p "的Herbrand宇宙为无穷集合恰当"
      $p "牵涉非零元函数(符号); 例如, "
      "以单独的一个常量" $c "和一个幺元函数符号"
      $f "而言, 其Herbrand宇宙为"
      (setE $c (app $f $c) (app $f (app $f $c))
            (app $f (app $f (app $f $c))) $..h)
      ". 一个" (Em "Herbrand解释")
      "是一个canonical解释, "
      "其论域是对于某个适切语言而言的Herbrand宇宙 "
      "(通常是出现于一个或多个公式中的符号所确定的语言), "
      "而公式集合的一个" (Em "Herbrand模型")
      "则是该公式集合的模型且模型本身是一个Herbrand解释. "
      "如果实例化" $i "的像都落入Herbrand宇宙, "
      "那么我们将会称" (&subst $i $p) "为" $p
      "的一个" (Em "ground实例") ".")
   (P "{译注: 或许时机算是有些晚了, "
      "但我想在这个地方写一点关于canonical解释的注记. "
      "canonical是对于解释的论域和函数符号的解释的限制, "
      "但对于谓词符号的解释没有要求. "
      "就函数符号的解释而言, "
      "它可能是对于所有函数符号的解释有所限制, "
      "也可能是对于部分所需的函数符号解释进行限制. "
      "而论域其实比较随意, "
      "限制其实就是能够满足函数符号解释的需要, "
      "其实也就是某种封闭性. "
      "另外, 有的时候我们的解释是相对于不确定的语言而言的, "
      "有的时候则是相对于一个特定的语言签名而言的. "
      "原文没有特别显式强调这一点, "
      "因为一般来说所有的定理在这两种情况下都成立. "
      "而对于Herbrand解释而言, 它的确要相对于一个特定签名, "
      "也就是由出现了的函数符号和谓词符号确定的签名, "
      "这样处理比较自然妥帖, 无需乱打补丁.}")
   ((Theorem)
    "一个Herbrand解释" $H "满足一个无量词公式" $p
    "当且仅当其满足由所有ground实例"
    (&subst $i $p) "构成的集合.")
   ((proof)
    "如果" $H "满足" $p ", 那么其也满足所有的ground实例, "
    "这是因为根据" (Ref "subst-value") ", 我们有"
    (MB (&= (&holds $H $v (@subst $i $p))
            (&holds $H (@compose (@termval $H $v) $i) $p)
            $true) ".")
    "反过来, 设" $H "满足所有的ground实例. "
    "任意的赋值" $v "都是以ground项集为陪域的映射, "
    "于是利用" (Ref "valM=subst") "我们可以得到"
    (&= (&compose (@termval $H $v) $v)
        (&compose (@tsubst $v) $v) $v)
    ". 但是然后根据" (Ref "subst-value")
    ", 我们有"
    (MB (&= (&holds $H $v $p)
            (&holds $H (@compose (@termval $H $v) $v) $p)
            (&holds $H $v (@subst $v $p))
            $true) ".")
    "{译注: 原文这里最后一行的公式存在笔误, 将"
    $subst "误写为了" $tsubst ".}")
   (P "诚然如此, 不过同样种类的结果不仅对于"
      "特定Herbrand模型下的满足性成立, "
      "也对于作为整体的可满足性成立.")
   ((Theorem #:id "Herbrand1")
    "一个无量词公式" $p "是一阶可满足的当且仅当"
    "由其所有ground实例构成的集合是命题可满足的.")
   ((proof)
    "如果" $p "是(一阶)可满足的, 那么其在某个模型"
    $M "下对于所有的赋值均成立. 令" $i
    "是任意的ground实例化, 即从变量(名)到Herbrand宇宙的映射. "
    "{译注: Herbrand宇宙也可以理解为所有ground项之集合, "
    "所以它才叫做ground实例化.} 使用" (Ref "subst-value")
    "和" (Ref "Mv2d") ", 我们可以推出, 对于任意的赋值"
    $v ", 都有:"
    (MB (deriv^
         (&pholds (@holds $M $v) (@subst $i $p))
         (&holds $M $v (@subst $i $p))
         (&holds $M (@compose (@termval $M $v) $i) $p)
         $true))
    "于是命题性赋值" (&holds $M $v)
    "可以同时满足" $p "的所有ground实例. "
    "{译注: " $v "的存在性依赖于论域非空.}" (Br)
    "反过来, 如果某个命题性赋值" $d
    "满足所有的ground实例, "
    "定义一个Herbrand解释" $H "满足"
    (&= (appl $R_H $t_1 $..h $t_n)
        (app $d (appl $R $t_1 $..h $t_n)))
    ". 根据" (Ref "thm3-21")
    ", 对于任意的赋值/ground实例化" $i
    ", 我们都有"
    (MB (&= (&holds $H $i $p)
            (&pholds $d (@subst $i $p))
            $true))
    "于是" $H "满足" $p ".")
   (P "这个重要结果通常被称为" (Em "Herbrand定理")
      ", 尽管这实际上是一种误称. "
      "根据本质上相同的证明, "
      "我们也可以推出以下重要的等价, "
      "这是藉由命题步骤绕路完成的.")
   ((Theorem)
    "一个无量词公式有一个模型 (即是可满足的) "
    "当且仅当其有一个Herbrand模型.")
   ((proof)
    "自右向左是立即的, "
    "因为一个Herbrand模型当然也是一个模型. "
    "对于另外一个方向, 我们可以复用"
    (Ref "Herbrand1") "的证明, "
    "通过注意到其中构造出来的模型的确是一个Herbrand模型. "
    "也就是说, 如果" $p "有一个模型, "
    "那么其所有ground实例之集合是命题可满足的, "
    "因而其有一个Herbrand模型.")
   (P "请注意这一推理仅涵盖了无量词公式或者全称性公式的情况. "
      "{译注: 这里原文所谓的universal formula"
      "应该指的是仅含有全称量词的一阶公式, "
      "这和前一节提及的existential formula是不一样的, "
      "那指的是以存在量词开头的一阶公式, 仅此而已.} "
      "举个例子, "
      (&conj (app $P $c)
             (∃ $x (&neg (app $P $x))))
      "是可满足的 (例如, 置论域为自然数集, "
      $P "为" (Q "是偶数") ", " $c
      "为零), 但是其没有Herbrand模型, "
      "因为其Herbrand宇宙仅是" (setE $c)
      ", 而这个公式不可能有单元素模型. "
      "出于相同的理由, "
      )
   (H3. "机械化Herbrand定理")
   (P "在诸多工作之后, "
      "我们成功地将一阶可满足性归约为了命题可满足性. "
      "然而, 我们的胜利却因以下事实而蒙上了阴影: "
      "我们需要测试由" (Em "所有")
      "ground实例构成的集合的命题可满足性, "
      "而这些实例的数目通常是无穷的. "
      "然而, 命题逻辑的紧致性"
      (Ref "prop-compactness")
      "拯救了我们.")
   ((Theorem)
    "一个无量词公式是一阶可满足的当且仅当"
    "其所有的有限ground实例集合都是(命题)可满足的.")
   ((proof)
    "这可由Herbrand" (Ref "Herbrand1")
    "和命题逻辑的紧致性 ("
    (Ref "prop-compactness") ") 立即推出.")
   ((Corollary)
    "一个无量词公式" $p "是一阶不可满足的当且仅当"
    "存在某个有限的ground实例集合是(命题)不可满足的.")
   ((proof)
    "前一条定理的逆否.")
   (P "这产生了一种过程, "
      "我们可以利用它来验证一个公式"
      $p "是不可满足的. "
      "我们可以直接枚举越来越大的ground实例集合, "
      "然后测试它们的命题可满足性. "
      "只要每个ground实例最终都可以出现于枚举之中, "
      "我们就可以保证若" $p "是不可满足的, "
      "那么我们终将抵达一个有限的命题不可满足集合. "
      "如果" $p "实际上是可满足的, "
      "那么这个过程永远也不会终止, "
      "所以这只是一个半判定过程. "
      "但是, 我们会在第7.6节看到, "
      "这已经是我们在一般情况下所能期望的最好结果了.")
   (P "1950年代后半期, 或许是受到Robinson (1957) "
      "在1957年康奈尔大学的Summer Institute for Symbolic Logic"
      "上某个建议的启发, 出现了若干按照这一思路实现的定理证明系统, "
      "其中最早的系统之一应归功于Gilmore (1960). "
      "{译注: 原文将会议误作为1954年举行的了.} "
      "Gilmore枚举越来越大的ground实例集合, "
      "每个阶段都会检查矛盾性, "
      "方法是将其置于析取范式, "
      "然后检查每个析取分量是否具有互补文字. "
      "{译注: 如果每个析取分量都有互补文字, "
      "那么说明该析取范式是不可满足的, 即矛盾.} "
      "让我们遵循这一方法以理解其实际运行的效果.")
   (P "我们需要设置一种合适的ground实例枚举方法, "
      "或者更准确地说, 其实是枚举ground项的"
      $m "元组, 而" $m "是公式中的自由变量的数目. "
      "如果我们想要保证每个不可满足公式最终都会被证明是不可满足的, "
      "那么枚举必然要最终能够囊括每个可能的ground实例. "
      "{译注: 这句话我感觉和进路不太一致, 实际上或许应该是"
      (Q "若要保证存在有限不可满足集合则一定能被发现的话")
      ", 并且我们要注意不可满足集合的超集也是不可满足的.} "
      "一种合理的方法是先生成所有不牵涉函数的"
      $m "元组 (即仅是常量项的组合), "
      "然后接着生成所有仅共牵涉一个函数的"
      $m "元组, 之后则是两个, 三个, 依此类推. "
      "{译注: 这里说的常量是零元函数, "
      "函数则是元数大于等于一的函数.} "
      "每个元组最终都会出现, 而"
      (Q "更简单") "的可能性会优先进行尝试. "
      "我们可以通过两个互递归的函数来设置枚举, "
      "它们都会取常量项集" (Code "cntms")
      "和函数(连带着元数)集" (Code "funcs")
      "为参数.")
   (P "函数" (Code "groundterms")
      "枚举了所有牵涉" (Code "n")
      "个函数的ground项. 如果" (Code "n = 0")
      ", 那么返回的就是常量项. "
      "否则的话, 所有可能的函数都会被尝试, "
      "并且既然我们需要为" (Code "m")
      "元函数的每个参数位置填上总共牵涉"
      (Code "n - 1") "个函数的诸项 "
      "(考虑到已经用了一个函数), "
      "那么我们可以递归地调用"
      (Code "groundtuples") ":"
      (CodeB "let rec groundterms cntms funcs n =
  if n = 0 then cntms else
  itlist (fun (f,m) l -> map (fun args -> Fn(f,args))
                             (groundtuples cntms funcs (n - 1) m) @ l)
          funcs []")
      "而这互递归函数" (Code "groundtuples")
      "会生成所有的总计牵涉" (Code "n")
      "个函数的ground项" (Code "m")
      "元组. {译注: 这是针对于下面的代码而言的.} "
      "对于所有到" (Code "n") "为止的" (Code "k")
      ", 它会尝试占据第一个元组位置以牵涉"
      (Code "k") "个函数的ground项的所有可能方式, "
      "并随之递归地生成余下的牵涉" (Code "n - k")
      "个函数的所有" (Code "m - 1") "元组."
      (CodeB "and groundtuples cntms funcs n m =
  if m = 0 then if n = 0 then [[]] else [] else
  itlist (fun k l -> allpairs (fun h t -> h::t)
                       (groundterms cntms funcs k)
                       (groundtuples cntms funcs (n - k) (m - 1)) @ l)
         (0 -- n) [];;"))
   (P "Gilmore的方法可以被认为是" (Q "Herbrand过程") "族的一员, "
      "该族在某种意义上会测试越来越大的ground实例的合取, "
      "直到检出不可满足的. 我们可以对于可满足性测试完成的方式 ("
      (Code "tfn") ") 和增扩ground实例 (复数) 以新实例的修饰函数 ("
      (Code "mfn") ") 进行一般化, 不论其以何种形式存储. "
      "这种一般化不仅是为了节约代码, "
      "也是为了强调关键的想法是独立于特定的可满足性测试的, "
      "其过程会通过以下循环展开:"
      (CodeB "let rec herbloop mfn tfn fl0 cntms funcs fvs n fl tried tuples =
  print_string(string_of_int(length tried)^&quot; ground instances tried; &quot;^
               string_of_int(length fl)^&quot; items in list&quot;);
  print_newline();
  match tuples with
    [] -> let newtups = groundtuples cntms funcs n (length fvs) in
          herbloop mfn tfn fl0 cntms funcs fvs (n + 1) fl tried newtups
  | tup::tups ->
          let fl' = mfn fl0 (subst(fpf fvs tup)) fl in
          if not(tfn fl') then tup::tried else
          herbloop mfn tfn fl0 cntms funcs fvs n fl' (tup::tried) tups;;"))
   (P "许多参数在循环的过程中是保持不变的: "
      "修饰函数和测试函数, 以某种转换后的列表形式存在的初始公式 ("
      (Code "fl0") ") {译注: 原则上并不绝对如此}, "
      "常量项" (Code "ctms") "和函数" (Code "funcs")
      ", 以及公式的自由变量" (Code "fvs")
      ". 其他的参数还有" (Code "n")
      ", 其代表了接下来要生成的枚举的层级, " (Code "fl")
      ", 到目前为止的ground实例集合 "
      "{译注: 实际上是这个集合之合取的某种表示}, "
      (Code "tried") ", 已经尝试了的实例, 以及"
      (Code "tuples") ", 当前层级的剩余ground实例. 当"
      (Code "tuples") "为空时, 我们就直接生成下一层级, "
      "并为" (Code "n") "加上一. 在其他情况下, "
      "我们会使用修饰函数以另外的实例更新" (Code "fl")
      ". 如果这个东西是不可满足的, "
      "那么我们就返回已经尝试了的实例集合. "
      "否则的话, 继续就可以了. "
      "在Gilmore过程这一特定情形下, "
      (Code "fl0") "和" (Code "fl")
      "都维护以DNF形式, 而修饰函数应用实例化于起始公式"
      (Code "fl0") "并通过分配律来将两个DNF进行组合:"
      (CodeB "let gilmore_loop =
  let mfn djs0 ifn djs =
    filter (non trivial) (distrib (image (image ifn) djs0) djs) in
  herbloop mfn (fun djs -> djs &lt;> []);;"))
   (P "我们通常对于证明有效性而非不可满足性更感兴趣. "
      "对此我们首先对于初始公式进行泛化, 否定和Skolem化, "
      "然后设置合适的自由变量集, 函数以及常量(项). "
      "接着我们开启主循环, 然后在终止时报告尝试了多少个ground实例:"
      (CodeB "let gilmore fm =
  let sfm = skolemize(Not(generalize fm)) in
  let fvs = fv sfm and consts,funcs = herbfuns sfm in
  let cntms = image (fun (c,_) -> Fn(c,[])) consts in
  length(gilmore_loop (simpdnf sfm) cntms funcs fvs 0 [[]] [] []);;"))
   (P "{译注: 对于这些程序的细节, 我们作如下说明. 若"
      (Code "sfm") "没有自由变量或者没有元数大于等于一的函数符号, "
      "那么实际上第" $0 "层级就将ground实例的所有可能全部枚举完毕了. "
      "即便如此, 如果" (Code "fm") "不是有效的, 那么"
      (Code "herbloop") "也不会检测到不可满足性, "
      "所以仍然会继续运行下去, "
      "只是枚举不出来新的ground实例而已. "
      "如果" (Code "fm") "是有效的, "
      "那么如前所述, 终止性可以得到保证, "
      "即便是ground实例的可能性有限的情况下. "
      "综上所述, " (Code "gilmore")
      "能够终止当且仅当" (Code "fm")
      "是一个一阶有效公式.}")
   (P "让我们在一些例子上尝试我们新的一阶证明器. "
      "我们会从很小的公式开始:"
      (CodeB "# gilmore &lt;&lt;exists x. forall y. P(x) ==> P(y)>>;;
...
1 ground instances tried; 1 items in list
- : int = 2"))
   (P "到目前为止, 一切都好. 这应该是一个简单的问题. "
      "然而, 为了澄清内部的工作情况, "
      "有必要追踪这个例子的整个运行过程. "
      "首先这个公式先否定后Skolem化得到的结果是:"
      (CodeB "# let sfm = skolemize(Not &lt;&lt;exists x. forall y. P(x) ==> P(y)>>);;
val sfm : fol formula = &lt;&lt;P(x) /\\ ~P(f_y(x))>>"))
   (P "读者可以通过运行" (Code "gilmore")
      "内部的其他步骤来确认常量项集合仅由一个" (Q "发明出来")
      "的常量" (Code "c") "构成, 并且函数也仅有一个幺元Skolem函数"
      (Code "f_y") ". 第一个被生成的ground实例为"
      (CodeB "P(c) /\\ ~P(f_y(c))"))
   (P "既然这仍然是命题可满足的, 第二个实例也会被生成:"
      (CodeB "P(f_y(c)) /\\ ~P(f_y(f_y(c)))"))
   (P "由于这两个实例的合取是命题不可满足的 "
      "(该合取同时包含" (Code "P(f_y(c))") "及其否定), "
      "过程终止, 并指明使用了两个ground实例, "
      "且该公式如所声称的那样是有效的. "
      "读者不妨以类似的方式逐步推演后续的更多例子, 这将大有裨益. "
      "在本章中, 我们将大量取材于"
      "Pelletier (1986) 给出的一组套题, "
      "以期对不同方法的优劣有所了解. "
      "其中一些例子可由当前程序轻松解决:"
      (CodeB "# let p24 = gilmore
   &lt;&lt;~(exists x. U(x) /\\ Q(x)) /\\
     (forall x. P(x) ==> Q(x) \\/ R(x)) /\\
     ~(exists x. P(x) ==> (exists x. Q(x))) /\\
     (forall x. Q(x) /\\ R(x) ==> U(x))
     ==> (exists x. P(x) /\\ R(x))>>;;
0 ground instances tried; 1 items in list
0 ground instances tried; 1 items in list
val p24 : int = 1"))
   (P "有的则需要更多一点时间, "
      "并且也要尝试数个ground实例, 例如:"
      (CodeB "# let p45 = gilmore
 &lt;&lt;(forall x. P(x) /\\ (forall y. G(y) /\\ H(x,y) ==> J(x,y))
              ==> (forall y. G(y) /\\ H(x,y) ==> R(y))) /\\
   ~(exists y. L(y) /\\ R(y)) /\\
   (exists x. P(x) /\\ (forall y. H(x,y) ==> L(y)) /\\
                      (forall y. G(y) /\\ H(x,y) ==> J(x,y)))
   ==> (exists x. P(x) /\\ ~(exists y. G(y) /\\ H(x,y)))>>;;
4 ground instances tried; 2511 items in list
val p45 : int = 5")
      "{译注: 这段交互疑似漏了省略号.}")
   (P "还有一些问题似乎相当棘手, 运行时间很长, "
      "最终导致机器内存耗尽, "
      "因为所生成的析取分量的数量实在过于庞大."
      (CodeB "let p20 = gilmore
 &lt;&lt;(forall x y. exists z. forall w. P(x) /\\ Q(y) ==> R(z) /\\ U(w))
   ==> (exists x y. P(x) /\\ Q(y)) ==> (exists z. R(z))>>;;"))
   (P "总而言之, 尽管Gilmore过程是"
      "一阶定理证明的一个有前景的开端, "
      "但仍有很大的改进空间. "
      "由于其主要局限似乎在于DNF中析取分量数目的爆炸性增长, "
      "一个自然的思路是保持同样的枚举过程, "
      "但用一种更高效的命题算法来检查目前所生成的"
      "ground实例之合取的命题可满足性.")
   (P "事实上, 正是为了这个目的, "
      "Davis和Putnam (1960) 才开发了他们"
      "用于命题可满足性测试的过程 (见2.9节). "
      "在这种背景下, 子句形式有一个特别的优势 "
      "{译注: 这里所说的子句形式特指以集合之集合 "
      "(或者说列表之列表) 形式存在的合取范式}, "
      "即不存在与析取分量的乘性爆炸相对应的现象. "
      "我们只需把(经否定和Skolem化的)公式化为子句形式, "
      "比如说有" $k "个合取分量, 而每生成一个新的ground实例, "
      "就只是往累积的子句堆里再添加" $k
      "个子句. 但与此相对的是, 当然, "
      "这需要运行一个真正的可满足性测试算法, "
      "而在Gilmore过程中, 这只不过是寻找互补文字的事情. "
      "略带些时代错置地, 我们将使用DPLL过程而非DP过程, "
      "因为我们之前的实验表明它通常更优, "
      "而且它在空间表现上确实更好. "
      "Davis-Putnam程序的结构与Gilmore程序非常类似. "
      "这一次所存储的公式全都是CNF形式而非DNF形式, "
      "并且每当我们纳入一个新实例时, 就用"
      (Code "dpll") "来检查不可满足性:"
      (CodeB "let dp_mfn cjs0 ifn cjs = union (image (image ifn) cjs0) cjs;;

let dp_loop = herbloop dp_mfn dpll;;")
      "外层包装没有变化, 只是公式被化为CNF形式而非DNF形式:"
      (CodeB "let davisputnam fm =
  let sfm = skolemize(Not(generalize fm)) in
  let fvs = fv sfm and consts,funcs = herbfuns sfm in
  let cntms = image (fun (c,_) -> Fn(c,[])) consts in
  length(dp_loop (simpcnf sfm) cntms funcs fvs 0 [] [] []);;"))
   (P "{译注: 译者认为" (Code "dp_mfn")
      "是成问题的, 因为" (Code "cjs0")
      "不存在平凡子句不能保证ground实例化"
      "之后的结果不存在平凡子句. "
      "因此, 去除平凡子句仍然是有必要的.}")
   (P "对于大多数情况, 这段代码的效果要好得多. "
      "例如, 先前颇成问题的" (Code "p20")
      "现在被迅速解决了, 仅使用了" 19
      "个ground实例:"
      (CodeB "# let p20 = davisputnam
   &lt;&lt;(forall x y. exists z. forall w. P(x) /\\ Q(y) ==> R(z) /\\ U(w))
     ==> (exists x y. P(x) /\\ Q(y)) ==> (exists z. R(z))>>;;
0 ground instances tried; 0 items in list
...
18 ground instances tried; 37 items in list
val p20 : int = 19"))
   (P "尽管Davis-Putnam过程避免了那种灾难性的内存使用爆炸 "
      "(这正是Gilmore过程的祸根), "
      "但它仍然常常生成数量极其庞大的ground实例, "
      "并且在每个命题步骤上都变得相当缓慢. "
      "通常, 这些实例中的大多数对最终的反驳并无贡献, "
      "一个小得多的集合就已足够. 总体运行时间 (以及最终的可行性) "
      "取决于在枚举过程中一个足够的集合多快会出现, "
      "而这是相当难以预测的. 假设我们定义一个函数, "
      "它遍历可能需要的实例列表 (" (Code "dunno")
      "), 仅当其余实例是可满足的时, "
      "才把这些实例放到所需实例的列表"
      (Code "need") "上:"
      (CodeB "let rec dp_refine cjs0 fvs dunno need =
  match dunno with
    [] -> need
  | cl::dknow ->
      let mfn = dp_mfn cjs0 ** subst ** fpf fvs in
      let need' =
       if dpll(itlist mfn (need @ dknow) []) then cl::need else need in
      dp_refine cjs0 fvs dknow need';;"))
   (P "{译注: 我不怎么理解" (Code "dp_refine")
      "这一过程的细节, 只知道它是在成功之后减少"
      "在某种意义上并不必要已尝试实例, "
      "这些实例是以用于实例化的元组的形式存在的.}")
   (P "{再次译注: 我稍微想明白了一些" (Code "dp_refine")
      "所做的事情了, 它可以计算一个极小不可满足子集. "
      "也就是说, 这个集合本身是不可满足的, "
      "但是如果去掉任何一个元素, 那么它就会是可满足的了. "
      "证明思路如下, 首先总体的不可满足性的保持是比较容易理解的, "
      "通过直接的循环不变量即可证明. "
      "其次, 如果去掉其中任何一个元素, 根据这个过程的写法, "
      "我们知道剩余元素构成的集合的某个超集会是可满足的, "
      "而可满足集合的任何子集都是可满足的.}")
   (P "我们可以在主循环成功之后使用这个精化过程:"
      (CodeB "let dp_refine_loop cjs0 cntms funcs fvs n cjs tried tuples =
  let tups = dp_loop cjs0 cntms funcs fvs n cjs tried tuples in
  dp_refine cjs0 fvs tups [];;"))
   (P "读者可以验证, 在Davis-Putnam过程中用"
      (Code "dp_refine_loop") "替换" (Code "dp_loop")
      ", 会大幅减少最终实例的数量, 例如在" (Code "p36")
      "的情况下从" 40 "个减少到仅" $3 "个, 在"
      (Code "p29") "的情况下从" 181 "个减少到" $5
      "个. 然而, 虽然像这样削减数量在我们"
      "想把这组ground实例用于某些用途时可能是有益的 "
      "(正如我们将在5.13节中所做的那样), "
      "但它并不能帮助提高该过程本身的效率, "
      "因为它在每次迭代时仍然需要检查迄今为止的整个实例集合. "
      "正如 Davis (1983) 事后承认的那样:"
      (Blockquote
       "...有效地消除了truth-functional可满足性这一障碍, "
       "却只是揭示出了更深层次的问题, "
       "即在Herbrand宇宙中进行无结构搜索时所固有的组合爆炸..."))
   (P "定理证明领域的下一个重大进步, "
      "是一种更智能的实例选择方法, "
      "即挑选出较小的相关实例的集合, "
      "而不是盲目地尝试所有的可能性.")
   (H4. "Herbrand过程的Scheme/Racket之实现")
   (P "以下是译者在Scheme/Racket实现的本节的所谓" (Q "Herbrand过程")
      ", 在诸多细节上和原文并不相同.")
   (CodeB "(define (herbfuns exp)
  (let-values (((c* f*)
                (partition
                 (lambda (p)
                   (= (cadr p) 0))
                 (functions exp))))
    (if (null? c*)
        (values (list (list 'c 0)) f*)
        (values c* f*))))
(define (groundterms ct* f* n)
  (if (= n 0)
      ct*
      (append-map
       (lambda (p)
         (define f (car p))
         (define m (cadr p))
         (map (curry cons f)
              (groundtuples ct* f* (- n 1) m)))
       f*)))
(define (groundtuples ct* f* n m)
  (if (= m 0)
      (if (= n 0) '(()) '())
      (append-map
       (lambda (k)
         (allpairs
          cons
          (groundterms ct* f* k)
          (groundtuples ct* f* (- n k) (- m 1))))
       (range (+ n 1)))))
(define ((herbloop modify test pre post init)
         exp #:refine? [refine? #f])
  (define exp0 (pre exp))
  (define v* (fv exp0))
  (define-values (c* f*)
    (herbfuns exp0))
  (define ct*
    (map (lambda (p) (list (car p))) c*))
  (define template (post exp0))
  ;refine computes a minimal unsatisfiable subset.
  (define (refine tried*)
    (let iter ((need '()) (dunno tried*))
      (if (null? dunno)
          need
          (let ((current (car dunno))
                (dunno (cdr dunno)))
            (if (test (fold-right
                       (lambda (tuple representation)
                         (modify template
                                 (curry subst (fpf v* tuple))
                                 representation))
                       init (append need dunno)))
                (iter (cons current need) dunno)
                (iter need dunno))))))
  (let loop ((current init) (n 0) (tried* '()) (tuple* '()))
    (if (null? tuple*)
        (loop current (+ n 1) tried*
              (groundtuples ct* f* n (length v*)))
        (let* ((tuple (car tuple*))
               (tuple* (cdr tuple*))
               (tried* (cons tuple tried*)))
          (define new
            (modify template
                    (curry subst (fpf v* tuple))
                    current))
          ;This assumes that it takes a list representation.
          (printf &quot;~s ground instances tried; ~s items in list\\n&quot;
                  (length tried*) (length new))
          (if (not (test new))
              (if refine? (refine tried*) tried*)
              (loop new n tried* tuple*))))))
(define (SNG exp)
  (skolemize (Not (generalize exp))))
(define (gilmore_modify template instantiate current)
  (remove-trivial
   (distrib (image (curry image instantiate) template)
            current)))
(define gilmore
  (compose length
           (herbloop gilmore_modify (non null?)
                     SNG simpdnf '(()))))
;The original OCaml version does not have remove-trivial here,
;but I think it is necessary!
;Note that subsume has included remove-trivial.
(define (dp_modify template instantiate current)
  (U (subsume
      (map (curry image instantiate) template))
     current))
(define davis-putnam
  (compose length (herbloop dp_modify dpll SNG simpcnf '())))")
   (P "除了消除了许多冗余的重复, " (Code "dp_modify")
      "和原文也并不相同.")
   ))