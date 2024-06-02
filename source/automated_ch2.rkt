#lang racket
(provide automated_ch2)
(require (except-in
          SMathML
          H3. H4. $!= &~ Lemma Corollary Definition
          Remark Theorem Example Proposition Warning)
         "automated_utils.rkt")
(define automated_ch2
  (Tm*
   (H2. "命题逻辑")
   (P "我们将会仔细研究命题逻辑, 在OCaml之中定义其形式句法, "
      "连带着句法分析和打印支持. "
      "我们将会讨论一些关键性的命题算法, 并证明紧致性定理, "
      "还会指明命题定理证明的丰富应用.")
   (H3. "命题逻辑的句法")
   (P "命题逻辑是" (Ref "algebra-of-logic")
      "所呈现的Boole的命题代数的一种现代版本. "
      "{原注: 诚然如此, 命题逻辑有时被称为" (Q "Boole代数")
      ". 但是, 这容易令人感到困惑, "
      "因为数学家将一切满足特定公理的代数结构都称为Boole代数, "
      "大致上这些公理是通常的代数律连带着" (&= $x^2 $x)
      " (Halmos 1963).} {译注: 这说的是Boole环, 可以被定义为含幺元的幂等环.} "
      "其牵涉被称为" (Em "公式") "的表达式, 而公式的意图是表示命题, "
      "即可以被认为是真或者假的断言. "
      "{原注: 当查阅文献时, 读者或许会发现用的是术语" (Em "合式公式")
      " (缩写为wff) 而非仅仅" (Q "公式")
      ". 这是为了强调在具体句法之中, 我们仅仅关心具有句法合法形式的字符串, "
      "而非任意的符号的(字符)串.} "
      "这些公式可以由常量" (Q "true") "和" (Q "false")
      "以及一些基本的" (Em "原子命题") " (或者说" (Em "原子")
      ") 通过各种逻辑联结词 (" (Q "not") ", " (Q "and")
      ", " (Q "or") ", 等等) 构筑而成. "
      "原子命题类似于通常代数之中的变量, "
      "有时我们将其称为" (Em "命题变量") "或者"
      (Em "Boole变量") ". 正如词汇" (Q "原子")
      "所暗示的, 我们并不会分析其内在结构; "
      "当我们在下一章中处理一阶逻辑时则要进行考虑.")
   (H4. "OCaml中的表示")
   (P "我们使用一个OCaml数据类型来表示命题公式, "
      "这可以类比于" (Ref "symbolic-computation-and-ocaml")
      "里的表达式类型. 我们允许" (Q "常量")
      "命题" (Code "False") "和" (Code "True")
      "以及原子命题" (Code "Atom p")
      ", 并且可以由它们通过使用幺元运算符" (Code "Not")
      "以及二元联结词" (Code "And") ", " (Code "Or")
      ", " (Code "Imp") " (" (Q "implies")
      "), " (Code "Iff") " (" (Q "if and only if")
      ") 构筑公式. 我们将对于这些联结词的确切含义的讨论推后, "
      "先来处理立即有用的部分.")
   (P "原子命题的潜在集合很大程度上是任意的, "
      "尽管对于某些目的而言其应该是无限的, "
      "以避免限制我们所能考虑的公式的复杂度. "
      "在抽象处理之中原始命题往往就是用数字索引的. "
      "我们令原子命题的潜在类型" (Code "'a")
      "为公式类型的定义的一个参数, "
      "由此许多基本函数可以不管该类型为何而一样工作. "
      "当我们考虑扩展至一阶逻辑时, "
      "这种乍看上去空洞的泛化有助于避免重复工作. "
      "出于相同的理由, 我们包含了两个额外的公式类型构造子"
      (Code "Forall") "和" (Code "Exists")
      ". 这些在本章中大致上会被忽略, "
      "不过其作用将会在之后变得清晰."
      (CodeB "type ('a)formula = False
                 | True
                 | Atom of 'a
                 | Not of ('a)formula
                 | And of ('a)formula * ('a)formula
                 | Or of ('a)formula * ('a)formula
                 | Imp of ('a)formula * ('a)formula
                 | Iff of ('a)formula * ('a)formula
                 | Forall of string * ('a)formula
                 | Exists of string * ('a)formula;;"))
   (H4. "具体句法")
   (P "正如我们之前所见, Boole对于逻辑联结词使用了传统的代数符号, 例如"
      (Q $+) ". 这使得许多逻辑事实看起来令人迷惑地熟悉, 例如"
      (MB (&= (&i* $p (@+ $q $r))
              (&+ (&i* $p $q) (&i* $p $r)))))
   (P "但是有些逻辑事实看起来就相当奇怪了, "
      "例如若是将第一个公式中的" (Q "and") "和" (Q "or")
      "系统地交换, 则可以得到以下事实:"
      (MB (&= (&+ $p (&i* $q $r))
              (&i* (@+ $p $q) (@+ $p $r))))
      "{译注: 注意这里遵循惯例, 乘法先于加法进行计算.}")
   (P "以逻辑的伪装这是在说如果" $p "成立或者" $q "和" $r
      "都成立, 那么" $p "或" $q "成立, 且" $p "或" $r
      "成立, 反之亦然. 稍加思考则可令读者确信的确如此; "
      "回忆一下" (Q $p "或" $q) "是可兼的, 即包含同时成立的情况.")
   (P "为了避免困惑或者是由通常代数产生误导性的类比, "
      "我们将会对于联结词使用如今业已标准化了的特殊符号. "
      "下表的每一行我们给出了每种构造的英语读法, "
      "之后跟着的是我们在讨论中所采用的标准符号化, "
      "然后是我们在程序中所支持的ASCII近似化, "
      "相应的抽象句法构造, 以及其他一些可能会用到的符号化. "
      "(最后一列如果只是阅读本书则可以忽略, "
      "但是在参考其他文献时则会很有用.)"
      (let ((TrTd* (lambda x* (apply Tr (map Td x*)))))
        (Table
         #:attr* '((align "center")
                   (border "1")
                   (style "border-collapse: collapse;"))
         (TrTd* "英语" "符号" "ASCII" "OCaml" "其他符号")
         (TrTd* "false" $bottom (Code "false") (Code "False") (&cm $0 $F))
         (TrTd* "true" $top (Code "true") (Code "True") (&cm $1 $T))
         (TrTd* (Span "not " $p) (&neg $p) (Code "~p") (Code "Not p")
                (&cm (OverBar $p) (&- $p) (&~ $p)))
         (TrTd* (Span $p " and " $q) (&conj $p $q)
                (Code "p /\\ q") (Code "And(p,q)")
                (&cm (&i* $p $q) (&& $p $q) (&d* $p $q)))
         (TrTd* (Span $p " or " $q) (&disj $p $q) (Code "p \\/ q")
                (Code "Or(p,q)") (&cm (&+ $p $q) (&\| $p $q)))
         (TrTd* (Span $p " implies " $q) (&=> $p $q)
                (Code "p ==> q") (Code "Imp(p,q)")
                (&cm (&-> $p $q) (&sup $p $q)))
         (TrTd* (Span $p " iff " $q) (&<=> $p $q)
                (Code "p &lt;=> q") (Code "Iff(p,q)")
                (&cm (&<-> $p $q) (&equiv $p $q) (&~ $p $q))))))
   (P "符号" (Q $disj) "来源于拉丁词汇" (Q "vel")
      "的首字母, 其意为可兼或. " $top
      "的形状类似于" (Q "true") "的首字母, 而" $bottom
      "和" $conj "不过是" $top "和" $disj
      "的镜像, 体现了一种对偶原则, "
      "这将在第2.4节中加以解释. "
      "否定的符号与算术取负的符号足够相似, 因此容易记忆. "
      "有些读者可能在非形式化的数学中见过推出和"
      (Q "当且仅当") "的符号.")
   (P "和普通代数一样, 我们需要建立联结词的优先级规则, "
      "并且若有必要可以通过括号来覆写规则. "
      "我们所采用的(颇为标准的)优先级顺序已经在上表中指明, 其中"
      (Q $neg) "是最高的, " (Q $<=>) "是最低的. 例如, "
      (&=> $p (&disj (&conj $q (&neg $r)) $s)) "意思是"
      (&=> $p (@disj (@conj $q (@neg $r)) $s))
      ". 或许赋予" $conj "和" $disj
      "相等的优先级更为合适, 但是只有少数作者这么做 "
      "(Dijkstra和Scholten 1990), "
      "我们还是从众赋予" $conj "更高的优先级.")
   (P "我们所有的二元联结词都以向右结合的方式进行句法分析, 于是"
      (&conj $p $q $r) "的意思是" (&conj $p (@conj $q $r))
      ", 诸如此类. 在非形式化的实践里, 形式如"
      (&=> $p $q $r) "这样的迭代推出经常用作"
      (Q (&=> $p $q) "且" (&=> $q $r))
      "的缩写, 就像" (&<= $x $y $z) "是"
      (Q (&<= $x $y) "且" (&<= $y $z))
      "的缩写. 但是对于我们而言, "
      (&=> $p $q $r) "指的是"
      (&=> $p (@=> $q $r)) ", 不是一个意思.")
   (P "在非形式化的讨论里, 我们不会使得" (Code "Atom")
      "构造子显式化, 但是会使用诸如" (&cm $p $q $r)
      "这样的名字代表一般的公式, 而" (&cm $x $y $z)
      "代表一般的原子. 例如, 当我们讨论"
      (&<=> $x $p) "时, 我们往往指的是具有形式"
      (Code "Iff(Atom(x),p)") "的公式.")
   (P "{译注: 实际情况是, 这个记号约定基本上废的, "
      (&cm $p $q $r) "除了用于公式的元变量, "
      "也一般用作原子命题的名字, "
      "虽然一般不用作原子命题的元变量. "
      "问题在于, 这足够引起混淆了, 例如"
      (&=> $p $q) "到底指的是一个具体的公式, "
      "还是一个公式的形式/模式, "
      "只能通过上下文判断.}")
   (H4. "通用句法分析和打印")
   (P "我们为公式建立了自动化的句法分析和打印支持, "
      "正如我们在第1.7–1.8节中对普通代数表达式所做的那样. "
      "由于具体细节对当前目的而言并不重要, "
      "代码的详细描述推迟到附录3中给出. "
      "不过我们确实想要强调的是, "
      "既然公式的类型是以原子命题的类型为参数的, "
      "那么句法分析和打印函数也同样是参数化的. "
      "函数" (Code "parse_formula") "的类型为:"
      (CodeB "# parse_formula;;
- : (string list -> string list -> 'a formula * string list) *
    (string list -> string list -> 'a formula * string list) ->
    string list -> string list -> 'a formula * string list
= &lt;fun>"))

   (H4. "原始命题")
   (P "尽管许多函数将会是通用的 (generic), "
      "但是如果我们固定在一个确定的原始命题类型上, "
      "对于某些操作进行实验会更加简单. "
      "据此我们定义了以下的原始命题类型, 其由名字索引 (即字符串):"
      (CodeB "type prop = P of string;;")
      "我们定义以下函数来获取一个命题的名字:"
      (CodeB "let pname(P s) = s;;"))
   (P "现在我们只需提供一个原子命题的解析器, 这相当直截了当. "
      "出于附录3中解释的原因, 我们需要检查第一个输入字符是否不是左括号, "
      "除此之外, 我们只需将输入流中的第一个标记作为原始命题的名称即可:"
      (CodeB "let parse_propvar vs inp =
  match inp with
    p::oinp when p &lt;> &quot;(&quot; -> Atom(P(p)),oinp
  | _ -> failwith &quot;parse_propvar&quot;;;")
      "现在我们将其提供给通用的公式解析器, "
      "其中对于目前未使用的中缀原子解析器传入一个总是失败的函数, "
      "对于非命题变量的上下文则传入一个空列表:"

      )

   (H4. "句法操作")
   (P "如果我们能有对应于公式构造子的句法操作"
      "作为正常的OCaml函数可用是很方便的:"
      (CodeB "let mk_and p q = And(p,q) and mk_or p q = Or(p,q)
and mk_imp p q = Imp(p,q) and mk_iff p q = Iff(p,q)
and mk_forall x p = Forall(x,p) and mk_exists x p = Exists(x,p);;"))
   (P "对偶地, 往往能够解构公式而不需要显式的模式匹配也是方便的. "
      "以下这个函数解构了一个" (Em "等价")
      " (或者说" (Em "biimplication") ", 或者说" (Em "biconditional")
      "), 即将具有形式" (&<=> $p $q) "的公式转换为序对" (tu0 $p $q) ":"
      (CodeB "let dest_iff fm =
  match fm with Iff(p,q) -> (p,q) | _ -> failwith &quot;dest_iff&quot;;;"))
   (P "类似地, 以下函数将一个" (Em "合取") "公式" (&conj $p $q)
      "分解为两个" (Em "合取分量(conjunct)") $p "和" $q ":"
      (CodeB "let dest_and fm =
  match fm with And(p,q) -> (p,q) | _ -> failwith &quot;dest_and&quot;;;")
      "而以下函数将一个合取递归地分解为一个合取分量的列表:"
      (CodeB "let rec conjuncts fm =
  match fm with And(p,q) -> conjuncts p @ conjuncts q | _ -> [fm];;"))
   (P "下列类似的函数将一个" (Em "析取") "公式" (&disj $p $q)
      "分解为" (Em "析取分量(disjunct)") $p "和" $q
      ", 一个是在顶层, 一个则是递归的:"
      (CodeB "let dest_or fm =
  match fm with Or(p,q) -> (p,q) | _ -> failwith &quot;dest_or&quot;;;

let rec disjuncts fm =
  match fm with Or(p,q) -> disjuncts p @ disjuncts q | _ -> [fm];;")
      "以下是推出式的一个顶层解构子:"
      (CodeB "let dest_imp fm =
  match fm with Imp(p,q) -> (p,q) | _ -> failwith &quot;dest_imp&quot;;;"))
   (P "一个推出式" (&=> $p $q) "里的公式" $p "和" $q
      "分别被称为其" (Em "前件") "和" (Em "后件")
      ", 而我们也应该定义相应的函数:"
      (CodeB "let antecedent fm = fst(dest_imp fm);;
let consequent fm = snd(dest_imp fm);;"))
   (P "我们经常需要通过对公式进行递归来定义函数, "
      "正如我们在第1.6节中对化简所做的那样. "
      "有两种递归模式似乎足够常见, 因此有必要定义通用函数. "
      "下面这个函数将一个函数应用于公式中的所有原子, 但保持其余结构不变. "
      "例如, 它可以用来将某个特定的原子命题系统地替换为另一个公式:"
      (CodeB "let rec onatoms f fm =
  match fm with
    Atom a -> f a
  | Not(p) -> Not(onatoms f p)
  | And(p,q) -> And(onatoms f p,onatoms f q)
  | Or(p,q) -> Or(onatoms f p,onatoms f q)
  | Imp(p,q) -> Imp(onatoms f p,onatoms f q)
  | Iff(p,q) -> Iff(onatoms f p,onatoms f q)
  | Forall(x,p) -> Forall(x,onatoms f p)
  | Exists(x,p) -> Exists(x,onatoms f p)
  | _ -> fm;;"))
   (P "下面这个函数是列表迭代器"
      (Code "itlist") "之于公式的类比, "
      "它将一个二元函数迭代地作用于公式中的所有原子:"
      (CodeB "let rec overatoms f fm b =
  match fm with
    Atom(a) -> f a b
  | Not(p) -> overatoms f p b
  | And(p,q) | Or(p,q) | Imp(p,q) | Iff(p,q) ->
        overatoms f p (overatoms f q b)
  | Forall(x,p) | Exists(x,p) -> overatoms f p b
  | _ -> b;;"))
   (P "一个特别常见的应用是收集与原子相关联的某种属性的集合; "
      "最简单的情形就是返回所有原子的集合. "
      "我们可以通过将一个函数" $f "连同一个" (Q "append")
      "操作迭代地作用于所有原子来实现这一点, "
      "最后将结果转换为集合以去除重复项. "
      "(我们也可以在过程中使用" (Code "union")
      "来逐步去除重复项, 但当涉及的集合较大时, "
      "当前的实现方式可能更为高效.)"
      (CodeB "let atom_union f fm = setify (overatoms (fun h t -> f(h)@t) fm []);;"))
   (P "我们很快将会看到对于如何使用这些非常一般的函数的刻画.")
   (H3. "命题逻辑的语义")
   (P "既然命题公式意在表示可能为真或者为假的断言, "
      "一个公式的最终含义只是两个" (Em "真值")
      (Q "true") "和" (Q "false") "中的一个. "
      "然而, 正如像" (&+ $x $y $1)
      "这样的一个代数表达式当我们知道变量" $x "和" $y
      "所代表的东西之后只有一个确切的含义, "
      "一个命题公式的含义依赖于被分配给其原子公式的真值. "
      "这种分配在一个" (Em "赋值(valuation)")
      "之中进行编码, 赋值是一个从原子的集合到真值的集合"
      (setE $false $true) "的函数. 给定一个公式" $p "和一个赋值" $v
      ", 然后我们可以根据下列递归定义的函数求得总体的真值:"
      (CodeB "let rec eval fm v =
  match fm with
    False -> false
  | True -> true
  | Atom(x) -> v(x)
  | Not(p) -> not(eval p v)
  | And(p,q) -> (eval p v) &amp; (eval q v)
  | Or(p,q) -> (eval p v) or (eval q v)
  | Imp(p,q) -> not(eval p v) or (eval q v)
  | Iff(p,q) -> (eval p v) = (eval q v);;"))
   (P "这是我们对于命题逻辑的数学" (Em "定义")
      ", 意在作为对于我们的直觉的自然形式化. "
      "{原注: 我们也可以选择将部分求值了的"
      (Code "eval p") ", 即一个从赋值到值的函数, "
      "视为公式" (Code "p") "的语义, 而非将赋值当作额外的参数. "
      "这主要只是一种术语问题.} "
      "(implication的语义并不显然, 之后我们将详细讨论.) "
      "每个逻辑联结词都由OCaml的内置类型" (Code "bool")
      "上的一个相应的运算子所解释. "
      "为了完全明确这些运算子的含义, "
      "我们可以枚举所有可能的输入组合并观察相应的输出, "
      "例如对于" (Code "&amp;") "运算子:"
      (CodeB "# false &amp; false;;
- : bool = false
# false &amp; true;;
- : bool = false
# true &amp; false;;
- : bool = false
# true &amp; true;;
- : bool = true"))
   (P "我们可以将这些信息排列在一张真值表中, "
      "展示如何由一个公式的立即子公式的真值来确定该公式被赋予的真值:"
      (MB (set-attr*
           (&Table
            ($p $q (&conj $p $q) (&disj $p $q) (&=> $p $q) (&<=> $p $q))
            ("false" "false" "false" "false" "true" "true")
            ("false" "true" "false" "true" "true" "false")
            ("true" "false" "false" "true" "false" "false")
            ("true" "true" "true" "true" "true" "true"))
           'frame "solid" 'rowlines "solid" 'columnlines "solid"))
      "当然了, 出于完整性的考量, 我们也应该包括幺元否定的真值表:"
      (MB (set-attr*
           (&Table
            ($p (&neg $p))
            ("false" "true")
            ("true" "false"))
           'frame "solid" 'rowlines "solid" 'columnlines "solid")))
   (P "让我们尝试在一个赋值下对公式" (&=> (&conj $p $q) (&conj $q $r))
      "求值, 其中" (&cm $p $q $r) "分别被设为" (Q "true")
      ", " (Q "false") "和" (Q "true")
      ". (我们不必费心去定义那些未出现在公式中的原子的值, "
      "OCaml会发出我们还没有完成的警告.)"
      (CodeB "# eval &lt;&lt;p /\\ q ==> q /\\ r>>
       (function P&quot;p&quot; -> true | P&quot;q&quot; -> false | P&quot;r&quot; -> true);;
...
- : bool = true"))
   (P "然而在另一个赋值下, 该公式可以求值为" (Q "false")
      "; 读者可能会发现手动验证这些结果是有益的练习:"
      (CodeB "eval &lt;&lt;p /\\ q ==> q /\\ r>>
     (function P&quot;p&quot; -> true | P&quot;q&quot; -> true | P&quot;r&quot; -> false);;"))
   (H4. "机械化了的真值表")
   (P "我们期望对于一个公式的求值独立于赋值如何给没有出现在公式之中的原子分配真值. "
      "让我们通过定义一个提取公式之中所出现的原子命题的集合的函数来使得我们的表述精确化. "
      "以抽象的数学术语来说, 我们将通过公式上的递归定义" $atoms "如下:"
      (eqn*
       ((&atoms $bottom) $= $empty)
       ((&atoms $top) $= $empty)
       ((&atoms $x) $= (setE $x))
       ((&atoms (&neg $p)) $= (&atoms $p))
       ((&atoms (&conj $p $q))
        $=
        (&union (&atoms $p) (&atoms $q)))
       ((&atoms (&disj $p $q))
        $=
        (&union (&atoms $p) (&atoms $q)))
       ((&atoms (&=> $p $q))
        $=
        (&union (&atoms $p) (&atoms $q)))
       ((&atoms (&<=> $p $q))
        $=
        (&union (&atoms $p) (&atoms $q)))))
   (P "作为公式上的结构归纳证明的一个简单例子 "
      "(见附录1和2), 我们将会证明" (&atoms $p)
      "总是有限的, 因而我们可以基于ML的列表来解释它而并没有曲解其含义. "
      "(当然了, 我们需要记住一般情况下列表相等性和集合相等性并不相同.) "
      "{译注: 我的理解大概就是这里可以使用列表表示集合, 仅此而已.}")
   ((Theorem)
    "对于任意的命题公式" $p ", 集合" (&atoms $p) "是有限的.")
   ((proof)
    "对公式的结构进行归纳证明." (Br)
    "若" $p "是" $bottom "或" $top ", 则" (&atoms $p)
    "是空集; 若" $p "是一个原子命题, 则" (&atoms $p)
    "是一个单元素集. 在所有这些情况下, 它们都是有限的." (Br)
    "若" $p "的形式为" (&neg $q) ", 则由归纳假设, "
    (&atoms $q) "是有限的, 且根据定义, "
    (&= (&atoms (&neg $q)) (&atoms $q)) "." (Br)
    "若" $p "的形式为" (&conj $q $r) ", " (&disj $q $r)
    ", " (&=> $q $r) "或" (&<=> $q $r) ", 则"
    (&= (&atoms $p) (&union (&atoms $q) (&atoms $r)))
    ". 由归纳假设, " (&atoms $q) "和" (&atoms $r)
    "都是有限的, 而两个有限集的并集仍然是有限的.")
   (P "类似地, 我们可以形式化地澄清以上所提及的直觉上显然的事实.")
   ((Theorem #:id "same-on-atoms")
    "对于任意的命题公式" $p ", 如果两个赋值" $v "和" $v^
    "在集合" (&atoms $p) "上相合, 即对于每个" (∈ $x (&atoms $p))
    "都有" (&= (app $v $x) (app $v^ $x)) ", 那么"
    (&= (&eval $p $v) (&eval $p $v^)) ".")
   ((proof)
    "对" $p "的结构进行归纳证明." (Br)
    "如果" $p "是" $bottom "或者" $top
    ", 那么其真值解释是独立于赋值的." (Br)
    "如果" $p "是一个原子" $x ", 那么"
    (&= (&atoms $x) (setE $x))
    ", 而根据题设我们有"
    (&= (app $v $x) (app $v^ $x)) ". 因此, "
    (&= (&eval $p $v) (app $v $x)
        (app $v^ $x) (&eval $p $v^))
    "." (Br)
    "如果" $p "具有形式" (&conj $q $r) ", " (&disj $q $r)
    ", " (&=> $q $r) "或" (&<=> $q $r) ", 那么"
    (&= (&atoms $p) (&union (&atoms $q) (&atoms $r)))
    ". 既然赋值在两个集合之并上是相合的, 那么其在"
    (&atoms $q) "和" (&atoms $r) "上更是相合的了. "
    "因此, 我们可以应用归纳假设以得出"
    (&= (&eval $q $v) (&eval $q $v^)) "和"
    (&= (&eval $r $v) (&eval $r $v^))
    ". 既然" $p "的赋值是这些子赋值的函数, 那么"
    (&= (&eval $p $v) (&eval $p $v^)) ".")
   (P "以上" $atoms "的定义可以被直接翻译为一个OCaml函数, "
      "例如对于" (Q $union) "使用" (Code "union") "而对于"
      (Q (setE $x)) "使用" (Code "[x]")
      ". 然而, 我们更倾向于基于既有的迭代子"
      (Code "atom_union") ":"
      (CodeB "let atoms fm = atom_union (fun a -> [a]) fm;;")
      "例如:"
      (CodeB "# atoms &lt;&lt;p /\\ q \\/ s ==> ~p \\/ (r &lt;=> s)>>;;
- : prop list = [P &quot;p&quot;; P &quot;q&quot;; P &quot;r&quot;; P &quot;s&quot;]"))
   (P "鉴于对于一个命题公式" $p
      "的解释只依赖于赋值在有限集合" (&atoms $p)
      " (设其有" $n "个元素) 上的动作, "
      "并且对于每个原子命题只有两种选择可作, "
      "于是最终的真值完全由对于这些原子的总共" $2^n
      "种选择所确定. 因此, 我们可以自然地将真值表形式的枚举"
      "从基本运算推广至任意的公式. "
      "为了在OCaml中实现这个, 我们从定义一个函数开始, "
      "其会测试一个函数" (Code "subfn")
      "是否会在原子" (Code "ats")
      "的所有可能赋值上返回" (Code "true")
      ", 对于其他所有原子则使用既有的赋值" (Code "v")
      ". 所有赋值的空间是通过相继修饰" (Code "v")
      "以设定每个原子" (Code "p") "为" (Q "true")
      "和" (Q "false") "与递归调用探索的:"
      (CodeB "let rec onallvaluations subfn v ats =
  match ats with
    [] -> subfn v
  | p::ps -> let v' t q = if q = p then t else v(q) in
             onallvaluations subfn (v' false) ps &amp;
             onallvaluations subfn (v' true) ps;;"))
   (P "我们可以将其应用于一个函数, "
      "这个函数绘制真值表的一行, 然后返回" (Q "true")
      ". (这个返回值是重要的, 因为" (Q (Code "&amp;"))
      "只有在其第一个参数是" (Code "true")
      "的情况下才会对于第二个参数进行求值.) "
      "这可以用来绘制一个公式的整个真值表:"
      (CodeB "let print_truthtable fm =
  let ats = atoms fm in
  let width = itlist (max ** String.length ** pname) ats 5 + 1 in
  let fixw s = s^String.make(width - String.length s) ' ' in
  let truthstring p = fixw (if p then &quot;true&quot; else &quot;false&quot;) in
  let mk_row v =
     let lis = map (fun x -> truthstring(v x)) ats
     and ans = truthstring(eval fm v) in
     print_string(itlist (^) lis (&quot;| &quot;^ans)); print_newline(); true in
  let separator = String.make (width * length ats + 9) '-' in
  print_string(itlist (fun s t -> fixw(pname s) ^ t) ats &quot;| formula&quot;);
  print_newline(); print_string separator; print_newline();
  let _ = onallvaluations mk_row (fun x -> false) ats in
  print_string separator; print_newline();;")
      "{译注: 老实说我不喜欢这种代码写法, "
      "其将求值和副作用混在了一起. "
      "我宁愿单独再写一个类似的函数专门处理副作用.}")
   (P "请注意, 我们以宽度为" (Code "width")
      "的列进行打印, 列宽足以容纳所有原子的名字以及"
      (Code "true") "和" (Code "false")
      ", 外加一个末尾空格. 这样, "
      "表格中的所有项就能整齐地对齐. 例如:"
      (CodeB "# print_truthtable &lt;&lt;p /\\ q ==> q /\\ r>>;;
p     q     r     | formula
---------------------------
false false false | true
false false true  | true
false true  false | true
false true  true  | true
true  false false | true
true  false true  | true
true  true  false | false
true  true  true  | true
---------------------------
- : unit = ()"))
   (H4. "形式语言和自然语言")
   (P "命题逻辑为我们提供了一种形式化的方式, "
      "来表达英语或其他自然语言中某些复杂的命题. "
      "练习将英语中的复合命题形式化 (即翻译为形式逻辑) 是很有益的. "
      "正如在两种自然语言之间进行翻译一样, "
      "我们不能总是期望逐词对应. "
      "但只要对非形式命题的结构有一定的了解, "
      "通常就可以进行相当直接的形式化.")
   (P "在命题逻辑中, 除了上面给出的优先级规则之外, "
      "我们还可以使用标准的数学括号技术将命题组合在一起, "
      "例如区分" (Q (&conj $p (@disj $q $r)))
      "和" (Q (&disj (@conj $p $q) $r))
      ". 括号在英语和大多数其他语言中的用法截然不同 "
      "(用来插入像这样的旁注). "
      "在英语中表示优先级是一件更加临时且笨拙的事情, "
      "通常通过插入额外的标点符号和" (Q "噪音词")
      "来给短语加括号, 从而消除歧义. "
      "例如, 我们可以将上述两个例子分别表述为"
      (Q $p ", and also either " $q " or " $r) "和"
      (Q "either both " $p " and " $q ", or else " $r)
      ". 对于复杂的命题, 这种方式会变得非常繁琐, "
      "而这实际上也是需要形式语言的部分原因.")
   (P "一般来说, 像" (Q "and") ", " (Q "or") "和" (Q "not")
      "这样的结构可以相当直接地从英语翻译为相应的逻辑联结词. "
      "联结词" (Q "not") "在英语中也可以隐含在前缀如"
      (Q "dis-") "和" (Q "un-") "中, 因此我们可以将"
      (Q "You are either honest and kind, or dishonest, or unkind")
      "翻译为" (Q (&disj (&conj $H $K) (&neg $H) (&neg $K)))
      ". 然而, 有时英语短语暗示着超越纯粹真值函数之外的细微含义. 例如, "
      (Q "and") "常常表示因果关系 (" (Q "he dropped the plate and it broke")
      ") 或时间顺序 (" (Q "she climbed into bed and turned out the light")
      "). " (Q "but") "一词可以说与" (Q "and") "有着相同的真值函数解释, "
      "但它表达的是各组成命题之间以一种出人意料或令人遗憾的方式相连. 同样, "
      (Q "unless") "可以合理地翻译为" (Q "or")
      ", 但由此导致的" (Q $p " unless " $q) "与"
      (Q $q " unless " $p) "之间的对称似乎令人意外.")
   (P "更成问题的是推出式或条件式" (&=> $p $q)
      "与其预期的英语表述" (Q $p " implies " $q)
      "或" (Q "if " $p " then " $q) "之间的关系. "
      "在这一点上表面上的不协调困扰着许多形式逻辑的初学者, "
      "并且至少使一个人永远放弃了这门学科 (Waugh 1991). "
      "事实上, 关于推出式的意义的争论可以追溯到两千多年前的"
      "墨伽拉-斯多噶学派逻辑学家 (Bochénski 1961). "
      "据Sextus Empiricus记载, "
      "公元前二世纪亚历山大图书馆的馆长Callimachus曾说"
      (Q "连屋顶上的乌鸦都在争论哪些条件句是真的") ".")
   (P "首先, 让我们明确一点: 如果我们对" (&=> $p $q)
      "采用" (Em "任何") "真值函数语义, 即根据"
      $p "和" $q "的真值来定义" (&=> $p $q)
      "的真值, 那么我们所选择的语义是唯一合理的. "
      "按照直觉理解, 推出最基本的原则是: 如果"
      $p "和" (&=> $p $q) "都为真, 那么" $q "也为真; "
      "因此, 如果" $p "为真而" $q "为假, 则" (&=> $p $q)
      "必须为假. 此外, " (&=> (&conj $p $q) $p)
      "总是为真这一点也是合理的, "
      "而只有我们所选择的语义才能在"
      $p "和" $q "取任意真值的情况下使之为真.")

   (H3. "有效性, 可满足性, 重言")
   (P "我们称一个赋值" $v (Em "满足")
      "一个公式" $p ", 如果" (&eval $p $v)
      "为真. 一个公式被称为是:"
      (Ul (Li (Em "重言") "或者" (Em "逻辑有效的")
              ", 如果其被" (Em "所有")
              "公式满足, 或者等价地说, "
              "如果其真值表的每一行的值都为真;")
          (Li (Em "可满足的")
              ", 如果其被某个赋值满足, "
              "即其真值表至少有一行的值为真;")
          (Li (Em "不可满足的") "或者说" (Em "矛盾")
              ", 如果没有赋值可以满足它, "
              "即其真值表的每一行的值都为假.")))
   (P "注意到一个重言也是可满足的, 另外正如名字所暗示的, "
      "一个公式是不可满足的恰当其不是可满足的. "
      "而且, 在任何赋值中, " (&eval (@neg $p) $v)
      "为假当且仅当" (&eval $p $v)
      "为真. 因此, " $p "是一个重言当且仅当"
      (&neg $p) "是不可满足的.")
   (P "最简单的重言是" (Q $top)
      "; 一个稍微有趣点的重言例子是"
      (&=> (&conj $p $q) (&disj $p $q)) " ("
      (Q "如果" $p "和" $q "都为真, 那么" $p "和" $q "至少有一个为真")
      "), 而一个许多人第一眼看上去会比较惊讶的例子是" (Q "Peirce律")
      (&=> (@=> (@=> $p $q) $p) $p) ":"
      (CodeB "# print_truthtable &lt;&lt;((p ==> q) ==> p) ==> p>>;;
p     q     | formula
---------------------
false false | true
false true  | true
true  false | true
true  true  | true
---------------------"))
   (P "对于我们首先在OCaml中生成其真值表的公式"
      (&=> (&conj $p $q) (&conj $q $r))
      ", 它是可满足的, 因为其真值表的最后一列有"
      (Q "true") ", 但它不是重言, 因为其也有"
      (Q "false") ". 最简单的矛盾是" (Q $bottom)
      ", 另一个简单的矛盾是" (Q (&conj $p (&neg $p)))
      " (" (Q $p "既为真又为假") "):"
      (CodeB "# print_truthtable &lt;&lt;p /\\ ~p>>;;
p     | formula
---------------
false | false
true  | false
---------------"))
   (P "从直觉上来说, 重言" (Q "总是为真")
      ", 可满足公式" (Q "有时(但可能并不总是)为真")
      ", 矛盾" (Q "总是为假")
      ". 的确如此, 重言意在形式化地捕获引论章节以非技术性方式"
      "所讨论的逻辑真性 (logical truth) 的概念, "
      "到目前为止的话我们是在命题逻辑中定义了重言. "
      "重言恰可以类比于诸如"
      (&= (&- $x^2 $y^2) (&i* (@+ $x $y) (@- $x $y)))
      "这样的代数等式, 其构成变量的值不论为何都是"
      "一般为真的 (universally true). "
      "可满足公式可以类比于至少有解的等式, "
      "但不必总是有效, 例如"
      (&= (&+ $x^2 $2) (&i* $3 $x))
      ". {译注: 英语里等式和方程都是equation就是了.} "
      "矛盾可以类比于无解的等式, 例如"
      (&= (&d* $0 $x) $1)
      ". {译注: 这里当然都是在实数域上讨论方程.}")
   (P "将(不)可满足性的想法从公式推广至公式的集合是有用的: "
      "公式的集合" Γ "被称为是可满足的, "
      "如果存在一个赋值" $v "能够" (Em "同时")
      "满足集合里的所有公式. 注意到" (Q "同时") ": "
      (setE (&conj $p (&neg $q)) (&conj (&neg $p) $q))
      "是不可满足的, 即便其每个公式都是可满足的. "
      "当所关心的集合有限时, 设"
      (&= Γ (setE $p_1 $..h $p_n))
      ", " Γ "的可满足性等价于单一公式"
      (&conj $p_1 $..c $p_n)
      "的可满足性, 读者可从定义看出来. "
      "然而, 在我们之后的工作里, "
      "考虑无限的公式集合的可满足性也是必要的, "
      "其就无法直接归约为单一公式的可满足性了. "
      "我们也会使用记号" (G!= $q) "来表达"
      (Q "对于所有使得每个" (∈ $p Γ) "都为真的赋值, "
         "该赋值也使得" $q "为真")
      ". 注意到在有限的" (&= Γ (setE $p_1 $..h $p_n))
      "这种情况下, " (G!= $q) "等价于断言"
      (&=> (&conj $p_1 $..c $p_n) $q)
      "是一个重言. 在" (&= Γ $empty)
      "的情况下, " (!= $empty $q) "一般记为"
      (!= $q) ", 意即" $q "是一个重言. "
      "{译注: " (G!= $q)
      "一般读作" Γ "语义蕴涵" $q ".}")
   (H4. "重言和可满足性检查")
   (P "虽然我们可以通过检查真值表来确定公式的状态, "
      "但让计算机完成所有工作会更简单. "
      "以下函数通过检查公式是否在所有赋值下均求值为"
      (Q (Code "true")) "来测试该公式是否为重言式."
      (CodeB "let tautology fm =
  onallvaluations (eval fm) (fun s -> false) (atoms fm);;"))
   (P "注意到一旦任意的求值碰到" (Q (Code "false"))
      ", 那么根据" (Code "onallvaluations")
      "的编写方式, 其会立即返回, "
      "而不是坚持对于所有可能的赋值进行求值. "
      "{译注: 说立即返回其实稍有不准确之处, "
      "因为鉴于它是(非尾)递归的, "
      "所以返回时还需要走过之前途径了的"
      (Code "and") ". 这一点其实可以优化, 但就"
      (Code "onallvaluations")
      "这个函数而言没有必要优化.}"
      (CodeB "# tautology &lt;&lt;p \\/ ~p>>;;
- : bool = true
# tautology &lt;&lt;p \\/ q ==> p>>;;
- : bool = false
# tautology &lt;&lt;p \\/ q ==> q \\/ (p &lt;=> q)>>;;
- : bool = false
# tautology &lt;&lt;(p \\/ q) /\\ ~(p /\\ q) ==> (~p &lt;=> q)>>;;
- : bool = true"))
   (P "使用之前注意到的其间关系, "
      "我们可以基于重言定义可满足性和不可满足性:"
      (CodeB "let unsatisfiable fm = tautology(Not fm);;

let satisfiable fm = not(unsatisfiable fm);;"))
   (H4. "替换")
   (P "和代数恒等式一样, "
      "我们期望能够将一个重言中的原子命题一致地替换为其他公式, "
      "然后仍然得到一个重言. "
      "我们可以将这种用公式去替换原子的函数定义如下, 其中"
      (Code "subfn") "是一个有限部分函数 (见附录2):"
      (CodeB "let psubst subfn = onatoms (fun p -> tryapplyd subfn p (Atom p));;"))
   (P "例如, 使用替换函数" (&\|=> $p (&conj $p $q))
      ", 其将" $p "映射为" (&conj $p $q)
      ", 但在其他情况下则是未定义的, 我们得到:"
      (CodeB "# psubst (P&quot;p&quot; |=> &lt;&lt;p /\\ q>>) &lt;&lt;p /\\ q /\\ p /\\ q>>;;
- : prop formula = &lt;&lt;(p /\\ q) /\\ q /\\ (p /\\ q) /\\ q>>")
      "{译注: 原文" $\|=> "这个符号竖线和箭头是连在一起的, 类似于"
      $\|-> ", 但是因为HTML entity里面没有这个符号, "
      "我只能退而求其次使用这种记法了.}")
   (P "我们将会证明重言中的替换将会产生重言, "
      "这是通过一个更为一般的结果完成的, "
      "这个更为一般的结果可以利用公式上的结构归纳直接证明:")
   ((Theorem #:id "subst-eval")
    "对于任意的原子命题" $x "与任意的公式" $p "和" $q
    ", 以及任意的赋值" $v ", 我们有"
    (MB (&= (&eval (@psubst (@\|=> $x $q) $p) $v)
            (&eval $p (@ext $v $x (&eval $q $v)))) "."))
   (P "{原注: 记号" (ext $v $x $a) "表示这样一个函数" $v^
      ", 其映射" (&= (app $v^ $x) $a) "而对于" (&!= $y $x)
      "则映射" (&= (app $v^ $y) (app $v $y))
      "; 记号" (&\|=> $x $a) "则表示这样的函数, "
      "其将" $x "映射为" $a ", 对于其他参数则是未定义的. (见附录1) "
      "在我们的OCaml实现里对于有限部分函数使用对应的运算符"
      (Q (Code "|->")) "和" (Q (Code "|=>")) "; 见附录2.}")
   ((proof)
    "根据" $p "的结构上的归纳. 如果" $p "是" $bottom "或"
    $top ", 那么赋值并不发挥作用, 于是等式显然成立. 如果"
    $p "是一个原子" $y ", 那么我们需要区分两种情况. 如果"
    (&= $y $x) ", 那么使用替换和求值的定义, 我们发现:"
    (MB (deriv
         (&eval (@psubst (@\|=> $x $q) $x) $v)
         (&eval $q $v)
         (&eval $x (@ext $v $x (&eval $q $v)))))
    "从另一方面而言, 如果" (&!= $y $x) ", 那么:"
    (MB (deriv
         (&eval (@psubst (@\|=> $x $q) $y) $v)
         (&eval $y $v)
         (&eval $y (@ext $v $x (&eval $q $v)))))
    "对于其他种类的公式, 求值和替换都遵循着公式的结构, "
    "故结果很容易根据归纳假设推出. 例如, 如果" $p
    "具有形式" (&neg $r) ", 那么根据定义并使用" $r
    "的归纳假设可知:"
    (MB (deriv
         (&eval (@psubst (@\|=> $x $q) (@neg $r)) $v)
         (&eval (@neg (@psubst (@\|=> $x $q) $r)) $v)
         (&not (&eval (@psubst (@\|=> $x $q) $r) $v))
         (&not (&eval $r (@ext $v $x (&eval $q $v))))
         (&eval (@neg $r) (@ext $v $x (&eval $q $v)))))
    "二元联结词的情况都遵循着相同的本质模式, "
    "只不过有两个不同的公式" $r "和" $s
    ", 而非只有" $r ".")
   ((Corollary)
    "如果" $p "是一个重言, " $x "是任意的原子(命题), "
    $q "是任意的公式, 那么" (&psubst (@\|=> $x $q) $p)
    "也是一个重言.")
   ((proof)
    "根据之前的定理, 对于任意的赋值" $v ", 我们有:"
    (MB (&= (&eval (@psubst (@\|=> $x $q) $p) $v)
            (&eval $p (@ext $v $x (&eval $q $v)))))
    "但是既然" $p "是一个重言, 那么其对于任何赋值都会求值为真, "
    "包括出现在这个等式右边的赋值. 因此, "
    (&= (&eval (@psubst (@\|=> $x $q) $p) $v) $true)
    ". 既然" $v "是任意的, 这意味着该公式也是重言.")
   (P "请注意, 这一结果仅适用于对原子进行替换, 而非对任意命题进行替换. "
      "例如, " (&=> (&conj $p $q) (&conj $q $p))
      "是一个重言, 但是如果我们将" (&conj $p $q)
      "替换为" (&disj $p $q) ", 就不复如此了. "
      "这与普通代数中的情况完全一样, "
      "而我们的替换函数是一个从原子名字出发的函数, "
      "这一点有助于强制施加这样的限制. 不过, "
      "主要结果可以很容易地推广到同时对多个原子进行替换的情形. "
      "这些替换总是可以通过逐个重复执行单个替换来完成, "
      "但可能需要引入额外的替换来更换变量, "
      "以避免后续替换对先前替换产生虚伪的影响. "
      "例如, 我们期望能够在" (&conj $x $y)
      "中同时将" $y "替换为" $x "和" $x "替换为" $y
      ", 从而得到" (&conj $y $x)
      ". 然而, 如果我们按顺序依次执行这些替换, 就会得到:"
      (MB (deriv^
           (&psubst (@\|=> $x $y)
                    (@psubst (@\|=> $y $x)
                             (@conj $x $y)))
           (&psubst (@\|=> $x $y) (@conj $x $x))
           (&conj $y $y))))
   (P "然而, 通过使用替换对于变量进行适切的重命名, "
      "这样的问题总是可以避免的. 例如:"
      (MB (deriv^
           (&psubst
            $z $y
            (@psubst
             $y $x
             (@psubst
              $x $z (@conj $x $y))))
           (&psubst
            $z $y
            (@psubst
             $y $x (@conj $z $y)))
           (&psubst
            $z $y
            (@conj $z $x))
           (&conj $y $x))))
   (P "通过列举一些常见的重言式来培养对命题逻辑的直觉是很有帮助的. "
      "其中一些简单而直观, 例如排中律" (Q (&disj $p (&neg $p)))
      ", 它表明每个命题非真即假. "
      "一个更令人意外的重言式" -- "这无疑是因为" (Q $=>)
      "与直觉上的推出概念之间存在较大出入" -- "为:"
      (CodeB "# tautology &lt;&lt;(p ==> q) \\/ (q ==> p)>>;;
- : bool = true"))
   (P "如果" (&=> $p $q) "是一个重言 {译注: 这里的" $p
      "和" $q "是一般公式的元变量}, 即任何满足" $p
      "的赋值也都满足" $q ", 那么我们称" $q
      "是" $p "的一个" (Em "逻辑推论")
      ". 如果" (&<=> $p $q) "是一个重言, "
      "即一个赋值满足" $p "当且仅当其满足" $q
      ", 那么我们称" $p "和" $q "是" (Em "逻辑等价的")
      ". {译注: 这里引入的逻辑推论和逻辑等价都是语义而非句法概念.} "
      "许多重要的重言都具有后者的形式, 并且如果" $p
      "是一个重言, 那么平凡地" (&<=> $p $top)
      "也是一个重言, 读者很容易确认这一事实. "
      "在代数里, 给定一个合法的等式如" (&= (&i* $2 $x) (&+ $x $x))
      ", 那么我们可以在任意的其他表达式里将" (&i* $2 $x)
      "替换为" (&+ $x $x) "而不改变其值. "
      "类似地, 如果一个赋值满足" (&<=> $p $q)
      ", 那么我们可以在另一个公式" $r
      "里将" $p "替换为" $q
      "或者反过来而不改变这个赋值是否满足" $r
      ", 即便" $p "或" $q "并非原子. "
      "{译注: 不改变" $r "在这个赋值下的解释结果.} "
      "既然我们还没有形式化地定义非原子的替换, "
      "我们可以想象通过使用" (Q "模式")
      "项的某个原子" $x "来将替换位置确定下来.")
   ((Theorem #:id "eval-subst")
    "对于任意的赋值" $v "和公式" $p "与" $q "满足"
    (&= (&eval $p $v) (&eval $q $v))
    ", 对于任意的原子" $x "和公式" $r
    ", 我们有"
    (MB (&= (&eval (@psubst $x $p $r) $v)
            (&eval (@psubst $x $q $r) $v)) "."))
   ((proof)
    "根据" (Ref "subst-eval") ", 我们有"
    (MB (subst-eval $x $p $r $v))
    "和"
    (MB (subst-eval $x $q $r $v))
    "但是既然根据题设有"
    (&= (&eval $p $v) (&eval $q $v))
    ", 这些当然都是相等的.")
   ((Corollary)
    "如果" $p "和" $q "是逻辑等价的, 那么"
    (MB (&= (&eval (@psubst $x $p $r) $v)
            (&eval (@psubst $x $q $r) $v)) ".")
    "特别地, " (&psubst $x $p $r)
    "是一个重言当且仅当" (&psubst $x $q $r)
    "是一个重言.")
   ((proof)
    "既然" $p "和" $q "是逻辑等价的, 我们有"
    (&= (&eval $p $v) (&eval $q $v))
    "对于任意的赋值" $v
    "成立, 而这个结果可由之前的定理直接推出.")
   (H4. "一些重要的重言")
   (P "闲话少说, 下面列出一些重言式. "
      "其中许多如果用Boole本人的符号重写, "
      "就对应于普通代数的定律, 例如"
      (&<=> (&conj $p $bottom) $bottom)
      "对应于" (&= (&d* $p $0) $0) ".")
   (eqn*
    ((&neg $top) $<=> $bottom)
    ((&neg $bottom) $<=> $top)
    ((&neg (&neg $p)) $<=> $p)
    ((&conj $p $bottom) $<=> $bottom)
    ((&conj $p $top) $<=> $p)
    ((&conj $p $p) $<=> $p)
    ((&conj $p (&neg $p)) $<=> $bottom)
    ((&conj $p $q) $<=> (&conj $q $p))
    ((&conj $p (@conj $q $r))
     $<=>
     (&conj (@conj $p $q) $r))
    ((&disj $p $bottom) $<=> $p)
    ((&disj $p $top) $<=> $top)
    ((&disj $p $p) $<=> $p)
    ((&disj $p (&neg $p)) $<=> $top)
    ((&disj $p $q) $<=> (&disj $q $p))
    ((&disj $p (@disj $q $r))
     $<=>
     (&disj (@disj $p $q) $r))
    ((&conj $p (@disj $q $r))
     $<=>
     (&disj (@conj $p $q) (@conj $p $r)))
    ((&disj $p (@conj $q $r))
     $<=>
     (&conj (@disj $p $q) (@disj $p $r)))
    ((&=> $bottom $p) $<=> $top)
    ((&=> $p $top) $<=> $top)
    ((&=> $p $bottom) $<=> (&neg $p))
    ((&=> $p $p) $<=> $top)
    ((&=> $p $q)
     $<=>
     (&=> (&neg $q) (&neg $p)))
    ((&=> $p $q)
     $<=>
     (@<=> $p (&conj $p $q)))
    ((&=> $p $q)
     $<=>
     (@<=> $q (&disj $p $q)))
    ((@<=> $p $q) $<=> (@<=> $q $p))
    ((@<=> $p (@<=> $q $r))
     $<=>
     (@<=> (@<=> $p $q) $r)))
   (P "以上最后几个重言式或许尤其令人惊讶, "
      "因为我们在日常数学中并不习惯" (Q "等式中嵌套等式")
      "的情形. 实际上, 它们表明" (Q $<=>)
      "是一个对称且具有结合律的运算符 (类似于算术中的" (Q $+)
      "), 即迭代等价 (iterated equivalences) "
      "的顺序和结合方式在逻辑上没有任何差别. "
      "Dijkstra和Scholten (1990) 给出了一些涉及等价的其他重言式, "
      "这些重言式可以在OCaml中加以验证; 他们将其中第二个重言式称为"
      (Q "黄金法则") "."
      (CodeB "# tautology &lt;&lt;p \\/ (q &lt;=> r) &lt;=> (p \\/ q &lt;=> p \\/ r)>>;;
- : bool = true
# tautology &lt;&lt;p /\\ q &lt;=> ((p &lt;=> q) &lt;=> p \\/ q)>>;;
- : bool = true"))
   (P "我们的重言式列表里还有一个对应于逆否原理的重言, 即"
      (&=> $p $q) "和其逆否式" (&=> (&neg $q) (&neg $p))
      "的等价, 或者" (&=> $p (&neg $q)) "和"
      (&=> $q (&neg $p)) "的等价. (例如, "
      (Q "those who mind don't matter") "和"
      (Q "those who matter don't mind")
      "是逻辑等价的.) "
      "与之形成对比的是, 我们可以确认" (&=> $p $q) "和"
      (&=> $q $p) "并非等价, 这是常见谬误:"
      (CodeB "# tautology &lt;&lt;(p ==> q) &lt;=> (~q ==> ~p)>>;;
- : bool = true
# tautology &lt;&lt;(p ==> ~q) &lt;=> (q ==> ~p)>>;;
- : bool = true
# tautology &lt;&lt;(p ==> q) &lt;=> (q ==> p)>>;;
- : bool = false"))
   (H3. "De Morgan律, 充足性, 对偶性")
   (P "以下重要的重言被称为" (Em "De Morgan律")
      ", 其是以Augustus De Morgan的名字命名的, "
      "他和Boole几乎是同时代的人物, "
      "对于逻辑领域作出了重要贡献."
      (MB (&<=> (&neg (@disj $p $q))
                (&conj (&neg $p) (&neg $q))))
      (MB (&<=> (&neg (@conj $p $q))
                (&disj (&neg $p) (&neg $q)))))
   (P "第一个重言的一个日常例子是"
      (Q "I can not speak either Finnish or Swedish")
      "和"
      (Q "I can not speak Finnish and I can not speak Swedish")
      "意思相同. 第二个重言的一个例子是"
      (Q "I am not a wife and mother")
      "和"
      (Q "either I am not a wife or I am not a mother (or both)")
      "是相同的. De Morgan律的变体, 同样也很容易看出来是重言, 为:"
      (MB (&<=> (&disj $p $q)
                (&neg (@conj (&neg $p) (&neg $q)))))
      (MB (&<=> (&conj $p $q)
                (&neg (@disj (&neg $p) (&neg $q))))))
   (P "这些重言之所以是有趣的, 是因为它们展示了对于联结词"
      $conj "和" $disj ", 如何用其中一个表达另外一个. "
      "根据之前关于替换的定理, 这意味着比如说我们可以"
      (Q "重写") "任意公式为一个与之逻辑等价但并不包含"
      (Q $disj) "的公式, 只需系统地将具有形式"
      (&disj $q $r) "的子公式替换为"
      (&neg (@conj (&neg $q) (&neg $r)))
      ". 关于用某些逻辑联结词表达其他逻辑联结词, "
      "还有许多其他选项. 例如, 使用下列等价, "
      "对于任意的公式我们都可以找出一个与之等价的"
      "仅仅使用原子公式, " $conj ", " $neg
      "的公式. 以术语来说, " (setE $conj $neg)
      "是联结词的一个" (Em "充足集合(adequate set)") "."
      (eqn*
       ($bottom $<=> (&conj $p (&neg $p)))
       ($top $<=> (&neg (@conj $p (&neg $p))))
       ((&disj $p $q)
        $<=>
        (&neg (@conj (&neg $p) (&neg $q))))
       ((&=> $p $q)
        $<=>
        (&neg (@conj $p (&neg $q))))
       ((@<=> $p $q)
        $<=>
        (&conj (&neg (@conj $p (&neg $q)))
               (&neg (@conj (&neg $p) $q)))))
      "{译注: 所谓的逻辑常量" $bottom "和"
      $top "是零元联结词.}")
   (P "类似地, 下列经OCaml验证的等价表明"
      (setE $=> $bottom) "也是充足的:"
      (CodeB "forall tautology
 [&lt;&lt;true &lt;=> false ==> false>>;
  &lt;&lt;~p &lt;=> p ==> false>>;
  &lt;&lt;p /\\ q &lt;=> (p ==> q ==> false) ==> false>>;
  &lt;&lt;p \\/ q &lt;=> (p ==> false) ==> q>>;
  &lt;&lt;(p &lt;=> q) &lt;=> ((p ==> q) ==> (q ==> p) ==> false) ==> false>>];;
- : bool = true"))
   (P "单独一个联结词是否足以表达所有其他联结词? "
      "对于我们已经引入的联结词而言, 答案是否定的. "
      "我们至少需要一个二元联结词, 否则我们永远无法引入涉及多个变量 "
      "(从而依赖于多个变量的赋值) 的公式. "
      "而且事实上, 即使是整个集合"
      (setE $top $conj $disj $=> $<=>)
      ", 如果没有" $neg "或" $bottom
      ", 也不构成一个充足集, "
      "因此更不用说其中任何单个二元联结词了. "
      "要看出这一点, 注意所有这些二元联结词在两个参数均为"
      (Q "真") "时都给出结果" (Q "真")
      ". (换言之, 它们各自真值表的最后一行, 最终列都是"
      (Q "真") ".) 因此, 由这些组件构建的任何公式, "
      "在将所有原子都映射为" (Q "真")
      "的赋值下, 必定求值为" (Q "真")
      ", 所以否定是不可表达的.")
   (P "然而, 对于二元真值函数而言, 存在"
      (&= (^ $2 (^ $2 $2)) 16)
      "种真值表, 而常规的二元联结词只覆盖了其中四种情况. "
      "(真值表有" (&= $2^2 $4)
      "行, 每一行可以在两种真值里选择一个.) "
      "或许某个以其他" 12 "种函数之一为其真值表的联结词可以是充足的? "
      "正如以上所论证的, 任意单独的充足联结词都必须以"
      (Q "假") "作为其真值表最后一行的值, 不然的话否定无法表达. "
      "根据类似的论证, 我们也可以看出来真值表第一行的值必然为"
      (Q "真") ". 那么, 留给我们的选择自由就只剩中间两行的情况了, "
      "总计有四种可能性. 其中有两种可能是平凡的, "
      "因为它们只是对于其中一个参数进行否定, "
      "故无法用来构建求值依赖于多于一个单一原子的值的表达式. "
      "不过, 其他两种情况单独都是充足的: "
      "一种是" (Q "not and") "运算"
      (&= (&NAND $p $q) (&neg (@conj $p $q)))
      ", 另一种是" (Q "not or") "运算"
      (&= (&NOR $p $q) (&neg (@disj $p $q)))
      ". 这两个的真值表如下:"
      (MB (set-attr*
           (&Table
            ($p $q (&NAND $p $q) (&NOR $p $q))
            ("false" "false" "true" "true")
            ("false" "true" "true" "false")
            ("true" "false" "true" "false")
            ("true" "true" "false" "false"))
           'frame "solid" 'rowlines "solid" 'columnlines "solid")))
   (P "例如, 我们可以通过"
      (&= (&neg $p) (&NAND $p $p))
      "来表达否定, 然后得到"
      (&= (&conj $p $q) (&neg (@NAND $p $q)))
      ", 不过我们已经知道" (setE $conj $neg)
      "是充足的了; " $NOR "以类似的方式成立. "
      "实际上, 一旦我们有了一个联结词的充足集合, "
      "那么我们也可以找出各种公式, "
      "其语义分别对应于其他" 12 "种真值函数. "
      "当我们于第2.6节讨论析取范式时, "
      "这会变得清晰起来.")
   (P "联结词" $NAND "和" $NOR
      "的单独充足性对于电子设计师而言是众所周知的: "
      "对应的门是数字电路的基本构建块 (见第2.7节). "
      "在纯粹逻辑学家之间, 这两个联结词其中之一会被习惯上记为"
      (&\| $p $q) ", 而" (Q $\|) "会被称为"
      (Q "Sheffer竖线") " (Sheffer 1913).")
   (P "{原注: 如今人们通常将竖线解释为" $NAND
      ", 但Sheffer最初用他的竖线表示的是" $NOR
      ", 并且Nicod (1917) 将其用于命题逻辑的一个精简表述中. "
      "这一思想早在30年前就已为Peirce所熟知. "
      "Schönfinkel (1924) 将其扩展为一种" (Q "量词竖线")
      ", 其中" (qstroke $x (app $phiv $x) (app $psi $x)) "意为"
      (&neg (∃ $x (&conj (app $phiv $x) (app $psi $x))))
      ", 这进而引发了人们对将同样的精简方法"
      "应用于更一般的数学表达式的兴趣, "
      "并由此推动了他对组合子的发展.}")
   (H4. "对偶性")
   (P "在第1.4节里我们注意到我们需要在" (Q "或") "的"
      (Q "可兼") "和" (Q "不可兼") "解读之间作出选择. "
      "无疑" (Q "and") "和" (Q "inclusive or")
      "之间的令人满意的对称是选择可兼解读的一个强烈动机. "
      "设我们现在有一个公式, 只牵涉联结词"
      (&cm $bottom $top $conj $disj)
      ". {译注: 漏了一个联结词, 即"
      $neg ".} 当我们言称其" (Em "对偶(dual)")
      "时, 我们指的是系统交换" (Q $conj) "和"
      (Q $disj) "以及" (Q $top) "和" (Q $bottom)
      "所得到的结果, 因而有以下定义:"
      (CodeB "let rec dual fm =
  match fm with
    False -> True
  | True -> False
  | Atom(p) -> fm
  | Not(p) -> Not(dual p)
  | And(p,q) -> Or(dual p,dual q)
  | Or(p,q) -> And(dual p,dual q)
  | _ -> failwith &quot;Formula involves connectives ==> or &lt;=>&quot;;;")
      "例如:"
      (CodeB "# dual &lt;&lt;p \\/ ~p>>;;
- : prop formula = &lt;&lt;p /\\ ~p>>"))
   (P "稍加思索即可发现" (&= (&dual (@dual $p)) $p)
      ". 对偶的关键语义性质如下:")
   ((Theorem #:id "dual-theorem")
    "对于任意的赋值" $v ", "
    (&= (&eval (@dual $p) $v)
        (&not (&eval $p (@compose $not $v)))) ".")
   ((proof)
    "这个结果可以由公式上的形式结构归纳证明 (见练习2.5), "
    "但是或许基于De Morgan律使用更为直接的推理来得比较简单. 令"
    (&* $p) "是对于公式" $p "的所有原子取否定, "
    "并且将" $bottom "替换为" (&neg $top) ", 将"
    $top "替换为" (&neg $bottom) "所得到的结果. "
    "然后, 我们有"
    (&= (&eval $p (@compose $not $v))
        (&eval (&* $p) $v))
    ". {译注: 这一结果若想严格说明, 还是用结构归纳比较好.} "
    "现在使用De Morgan律, 我们可以不断将" (&* $p)
    "中新引入的否定从原子那里拉出来, "
    "从而给出一个逻辑等价的形式:"
    (eqn*
     ((&conj (&neg $p) (&neg $q))
      $<=>
      (&neg (@disj $p $q)))
     ((&disj (&neg $p) (&neg $q))
      $<=>
      (&neg (@conj $p $q))))
    "通过这种做法, 我们交换了" (Q $conj) "和" (Q $disj)
    ", 并且使得新引入的否定符号浮上来了, "
    "最后我们在顶层恰有一个额外的否定符号, "
    "这就是" (&neg (@dual $p))
    ", 由此得出定理.")
   ((Corollary)
    "如果" $p "和" $q "是逻辑等价的, 那么"
    (&dual $p) "和" (&dual $q)
    "也是逻辑等价的. 如果" $p
    "是一个重言, 那么" (&neg (@dual $p))
    "也是一个重言.")
   ((proof)
    (&= (&eval (@dual $p) $v)
        (&not (&eval $p (@compose $not $v)))
        (&not (&eval $q (@compose $not $v)))
        (&eval (@dual $q) $v))
    ". 如果" $p "是一个重言, 那么" $p
    "和" $top "是逻辑等价的. 因此, "
    (&dual $p) "和" (&= (&dual $top) $bottom)
    "也是逻辑等价的, 由此得出结果.")
   (P "例如, 既然" (&conj $p (@disj $q $r)) "和"
      (&disj (@conj $p $q) (@conj $p $r))
      "是等价的, 那么" (&disj $p (@conj $q $r))
      "和" (&conj (@disj $p $q) (@disj $p $r))
      "也是等价的. 既然" (&disj $p (&neg $p))
      "是一个重言, 那么" (&neg (@conj $p (&neg $p)))
      "也是一个重言.")
   (H3. "化简和否定范式")
   (P "在普通代数中, 将表达式系统地变换为"
      "等价的标准形式或范式是很常见的做法. "
      "其中一种方法涉及展开与消去, 例如从"
      (&+ (&i* (@+ $x $y) (@- $y $x)) $y $x^2)
      "得到范式" (&+ $y^2 $y)
      ". 通过将表达式化为范式, "
      "我们有时可以看出表面上不同的表达式实际上是等价的. "
      "此外, 如果范式选取得当, 它还能提供有价值的信息. "
      "例如, 观察" (&+ $y^2 $y) "我们可以看出" $x
      "的值是无关紧要的, 而从初始形式来看这一点完全不明显. "
      "在逻辑中, 公式的范式具有极其重要的地位, "
      "并且正如在代数中一样, 范式往往能够提供重要的信息.")
   (P "在开始正式构建规范形式之前, "
      "最好先对于公式进行常规的化简以消除(不必要的)命题常量"
      (Q $bottom) "和" (Q $top)
      ", 这完全可以类比于第1.6节的代数例子. 每当"
      (Q $bottom) "和" (Q $top)
      "以组合形式出现, 总是存在一个重言"
      "澄清其与某种更为简单的公式之间的等价性, 例如"
      (&<=> (&conj $bottom $p) $bottom) ", "
      (&<=> (&disj $bottom $p) $p) ", "
      (&<=> (&=> $p $bottom) (&neg $p))
      ". 此外, 我们也会消除双重否定" (&neg (&neg $p))
      ". {译注: 这里的" $p "你将其理解为对象语言里的变量 "
      "(即原子(命题), 但不是代表原子的元变量), "
      "或者元语言里代表命题公式的元变量, 其实都说得通. "
      "不过这里作者应该更倾向于元变量的理解吧.} "
      "以下这段代码只是通过模式匹配逐情形考虑每种可能:"
      (CodeB "let psimplify1 fm =
  match fm with
    Not False -> True
  | Not True -> False
  | Not(Not p) -> p
  | And(p,False) | And(False,p) -> False
  | And(p,True) | And(True,p) -> p
  | Or(p,False) | Or(False,p) -> p
  | Or(p,True) | Or(True,p) -> True
  | Imp(False,p) | Imp(p,True) -> True
  | Imp(True,p) -> p
  | Imp(p,False) -> Not p
  | Iff(p,True) | Iff(True,p) -> p
  | Iff(p,False) | Iff(False,p) -> Not p
  | _ -> fm;;")
      "{原注: 注意到对于" (&=> $p $bottom) ", "
      (&<=> $p $bottom) ", " (&<=> $bottom $p)
      "导致" (&neg $p) "的子句都被置于了相应的组的最下方, "
      "这是为了使得例如" (&=> $bottom $bottom)
      "被化简为" $top ", 而不是" (&neg $bottom)
      ", 后者在同一层次仍需进一步化简.} "
      "{译注: 不过, 作者在这里仍然出现了疏漏, "
      (&<=> $bottom $bottom) "根据这个定义会被化简为"
      (&neg $bottom) ", 而不是" $top ".} "
      )
   (P "然后我们以递归的自底而上方式应用化简:"
      (CodeB "let rec psimplify fm =
  match fm with
    Not p -> psimplify1 (Not(psimplify p))
  | And(p,q) -> psimplify1 (And(psimplify p,psimplify q))
  | Or(p,q) -> psimplify1 (Or(psimplify p,psimplify q))
  | Imp(p,q) -> psimplify1 (Imp(psimplify p,psimplify q))
  | Iff(p,q) -> psimplify1 (Iff(psimplify p,psimplify q))
  | _ -> fm;;")
      "{译注: 在勘误里, 对于" (Code "psimplify1")
      "的补救措施是添加对于" (&<=> $bottom $bottom)
      "的特殊处理, 然而我觉得这仍然不那么令人满意. "
      "比如说, 对于公式" (&=> (@=> $p $bottom) $bottom)
      "和" (&<=> (@<=> $p $bottom) $bottom)
      " (其中" $p "是字面上的原子命题, 而不是代表公式的元变量), "
      "目前的过程" (Code "psimplify") "会将其都化简为"
      (&neg (&neg $p)) ", 然而我们知道这个结果又可以化简为"
      $p ". 个人认为更好的解决之道是让返回" (&neg $p)
      " (这里则是元变量) 的子句应该考虑" (Code "psimplify1")
      "里面对于" $neg "部分的化简方式, "
      "而这又迫使我们最好重新组织代码, "
      "对于每种联结词都编写一个函数, 如此复用比较方便, "
      "我编写了下列Scheme函数以说明我的想法:"
      (CodeB "(define (psimplify exp)
  (define (Not exp)
    (match exp
      (,b (guard (boolean? b)) (not b))
      ((not ,e1) e1)
      (,else `(not ,exp))))
  (define (And e1 e2)
    (cond ((eq? e1 #f) #f)
          ((eq? e2 #f) #f)
          ((eq? e1 #t) e2)
          ((eq? e2 #t) e1)
          (else `(and ,e1 ,e2))))
  (define (Or e1 e2)
    (cond ((eq? e1 #t) #t)
          ((eq? e2 #t) #t)
          ((eq? e1 #f) e2)
          ((eq? e2 #f) e1)
          (else `(or ,e1 ,e2))))
  (define (Imp e1 e2)
    (cond ((eq? e1 #f) #t)
          ((eq? e2 #t) #t)
          ((eq? e1 #t) e2)
          ((eq? e2 #f) (Not e1))
          (else `(=> ,e1 ,e2))))
  (define (Iff e1 e2)
    (cond ((eq? e1 #t) e2)
          ((eq? e2 #t) e1)
          ((eq? e1 #f) (Not e2))
          ((eq? e2 #f) (Not e1))
          (else `(&lt;=> ,e1 ,e2))))
  (define (simp exp)
    (match exp
      ((not ,e1) (Not (simp e1)))
      ((and ,e1 ,e2) (And (simp e1) (simp e2)))
      ((or ,e1 ,e2) (Or (simp e1) (simp e2)))
      ((=> ,e1 ,e2) (Imp (simp e1) (simp e2)))
      ((&lt;=> ,e1 ,e2) (Iff (simp e1) (simp e2)))
      (,else exp)))
  (simp exp))"))
   (P "例如:"
      (CodeB "# psimplify &lt;&lt;(true ==> (x &lt;=> false)) ==> ~(y \\/ false /\\ z)>>;;
- : prop formula = &lt;&lt;~x ==> ~y>>"))
   (P "如果我们先应用这个化简函数, 就几乎可以忽略命题常量, 这会使事情更加方便. "
      "然而, 我们需要记住两个平凡的例外情况: "
      "虽然在化简后的公式中" (Q $bottom) "和" (Q $top)
      "不能以组合的形式出现, 但整个公式本身可能就是其中之一, 例如:"
      (CodeB "# psimplify &lt;&lt;((x ==> y) ==> true) \\/ ~false>>;;
- : prop formula = &lt;&lt;true>>")
      "{译注: 通过简单的结构归纳可以证明, 经过" (Code "psimplify")
      "化简得到的公式, 除非本身就是" $bottom "或者" $top
      ", 否则就不可能含有这两个逻辑常量.}")
   (P "一个" (Em "文字(literal)") "要么是一个原子公式, "
      "要么是原子公式的否定. 我们称一个文字是" (Em "否定性的(negative)")
      ", 如果其具有形式" (&neg $p) ", 否则就将其称为"
      (Em "肯定性的(positive)") ". 这可由以下OCaml函数进行测试, "
      "注意这两个函数都只应该应用于文字:"
      (CodeB "let negative = function (Not p) -> true | _ -> false;;

let positive lit = not(negative lit);;"))
   (P "当我们之后言及" (Em "否定") "一个文字" $l
      "时, 记作" (&- $l) ", 我们指的是当文字为肯定性时应用否定, "
      "而当其为否定性时" (Em "移除")
      "否定 (而非要双重否定, 因为这样的话它就并非文字了). "
      "两个文字被称为" (Em "互补的")
      ", 如果一个是另一个的否定:"
      (CodeB "let negate = function (Not p) -> p | p -> Not p;;"))
   (P "一个公式被称为具有" (Em "否定范式")
      " (NNF), 如果其只由文字通过使用二元联结词" (Q $conj)
      "和" (Q $disj) "构造而成, 或者其为退化情形"
      (Q $bottom) "和" (Q $top) "之一. 换言之, "
      "其并不牵涉二元联结词" (Q $=>) "和" (Q $<=>)
      ", 而且" (Q $neg) "只能应用于原子公式. "
      "NNF公式的例子包括" $bottom ", " $p ", " (&conj $p $q) ", "
      (&disj $p (@disj (&conj $q (@neg $r)) $s))
      ", 而非NNF公式的例子包括" (&=> $p $p)
      " (牵涉其他二元联结词), 也包括" (&neg (&neg $p))
      "和" (&conj $p (&neg (@disj $q $r)))
      " (牵涉非原子公式的否定).")
   (P "我们可以将任意的公式转换为逻辑等价的NNF公式. "
      "和上一节一样, 我们可以基于其他联结词消去"
      (Q $=>) "和" (Q $<=>)
      ", 然后我们可以不断应用De Morgan律和双重否定律:"
      (eqn*
       ((&neg (@conj $p $q))
        $<=>
        (&disj (&neg $p) (&neg $q)))
       ((&neg (@disj $p $q))
        $<=>
        (&conj (&neg $p) (&neg $q)))
       ((&neg (&neg $p)) $<=> $p))
      "将否定下推至原子公式, 这恰好是"
      (Ref "dual-theorem")
      "的证明里所考虑的变换的反向. "
      "(当前的变换可以类比于如下普通代数里的过程: "
      "先将减法替换为其定义"
      (&= (&- $x $y) (&+ $x (&- $y)))
      ", 然后使用"
      (&= (&- (@+ $x $y)) (&+ (&- $x) (&- $y))) ", "
      (&= (&- (@i* $x $y)) (&i* (@- $x) $y)) "和"
      (&= (&- (@- $x)) $x)
      "系统地将负号 (negation) 下推.) "
      "这在OCaml里编程是相当直接的, "
      "而且实际上在我们递归地将否定下推时可以一并消去"
      (Q $=>) "和" (Q $<=>) ", 无需单独步骤 (phase)."
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
  | _ -> fm;;"))
   (P "这段代码对于" (Q $=>) "和" (Q $<=>)
      "的消去, 不论肯定还是否定, 是由以下重言所澄清的:"
      (eqn*
       ((&=> $p $q) $<=> (&disj (&neg $p) $q))
       ((&neg (@=> $p $q))
        $<=>
        (&conj $p (&neg $q)))
       ((@<=> $p $q)
        $<=>
        (&disj (&conj $p $q)
               (&conj (&neg $p) (&neg $q))))
       ((&neg (@<=> $p $q))
        $<=>
        (&disj (&conj $p (&neg $q))
               (&conj (&neg $p) $q))))
      "尽管出于某些目的我们可能更倾向于其他变式, 例如"
      (eqn*
       ((@<=> $p $q)
        $<=>
        (&conj (@disj $p (&neg $q))
               (@disj (&neg $p) $q)))
       ((&neg (@<=> $p $q))
        $<=>
        (&conj (@disj $p $q)
               (@disj (&neg $p) (&neg $q))))))
   (P "为了完结, 我们重新定义" (Code "nnf")
      "以包含一个初始的化简, "
      "然后再调用刚才的定义. "
      "(这不是递归定义, 而是使用之前的定义对于"
      (Code "nnf") "进行重定义, 因为这里没有"
      (Code "rec") "关键词.)"
      (CodeB "let nnf fm = nnf(psimplify fm);;"))
   (P "NNF公式(可能)要比本来的公式显著更大. "
      "诚然如此, 因为每次一个公式" (Q (&<=> $p $q))
      "扩展的时候, 公式" $p "和" $q
      "都会得到复制, 最坏情况下具有" $n
      "个联结词的公式可以扩展为具有超过"
      $2^n "个联结词的NNF" -- "见之后的练习2.6. "
      "这种指数膨胀似乎在保持逻辑等价时很难避免, "
      "但是我们至少可以避免进行指数量的" (Em "计算")
      ", 通过以更高效的方式重写" (Code "nnf")
      "函数 (练习2.7). 如果目标只是将否定下推到原子层次, "
      "我们也可以保留" (Q $<=>) "以避免潜在的指数膨胀, "
      "使用诸如"
      (&<=> (&neg (@<=> $p $q))
            (@<=> (&neg $p) $q))
      "这样的重言:"
      (CodeB "let rec nenf fm =
  match fm with
    Not(Not p) -> nenf p
  | Not(And(p,q)) -> Or(nenf(Not p),nenf(Not q))
  | Not(Or(p,q)) -> And(nenf(Not p),nenf(Not q))
  | Not(Imp(p,q)) -> And(nenf p,nenf(Not q))
  | Not(Iff(p,q)) -> Iff(nenf p,nenf(Not q))
  | And(p,q) -> And(nenf p,nenf q)
  | Or(p,q) -> Or(nenf p,nenf q)
  | Imp(p,q) -> Or(nenf(Not p),nenf q)
  | Iff(p,q) -> Iff(nenf p,nenf q)
  | _ -> fm;;")
      "化简也还是要加进去的:"
      (CodeB "let nenf fm = nenf(psimplify fm);;")
      "{译注: 实际上对于" $<=> "的否定使用的重言是"
      (&<=> (&neg (@<=> $p $q))
            (@<=> $p (&neg $q))) ".}")
   (P "这个函数当然有着自身的用途. "
      "不过, NNF的诱人之处在于我们可以区分原子公式的"
      (Q "肯定性") "和" (Q "否定性")
      "出现. 联结词" (Q $conj) "和" (Q $disj)
      "和联结词" (Q $neg) ", " (Q $=>) ", "
      (Q $<=>) "的不同之处在于其是" (Em "单调的")
      ", 意即其真值函数" $f "具有性质"
      (&=> (&conj (&<= $p $p^) (&<= $q $q^))
           (&<= (appl $f $p $q)
                (appl $f $p^ $q^)))
      ", 其中" (Q $<=) "是" (Q $=>)
      "的真值函数. {译注: 换言之, 是满足"
      (&<= $false $true) "的偏序.} "
      "换种说法, 也就是以下的公式都是重言:"
      (CodeB "# tautology &lt;&lt;(p ==> p') /\\ (q ==> q') ==> (p /\\ q ==> p' /\\ q')>>;;
- : bool = true
# tautology &lt;&lt;(p ==> p') /\\ (q ==> q') ==> (p \\/ q ==> p' \\/ q')>>;;
- : bool = true"))
   (P "这导致了, 如果一个原子" $x "在某个NNF公式"
      $p "中只以肯定形式出现, "
      "那么我们可以推出对于整个公式而言的一个相应的单调性质:"
      (MB (&=> (@=> $x $x^)
               (@=> $p (&psubst $x $x^ $p))))
      "而如果该原子只以否定形式出现, 那么我们有一个反单调性, 因为"
      (&=> (@=> $p $p^) (@=> (&neg $p^) (&neg $p)))
      "是一个重言:"
      (MB (&=> (@=> $x $x^)
               (@=> (&psubst $x $x^ $p) $p))))
   (H3. "析取范式和合取范式" #:id "dnf-cnf")
   (P "一个公式被称为是具有" (Em "析取范式")
      " (DNF), 当其具有以下形式:"
      (MB (&disj $D_1 $D_2 $..c $D_n))
      "而每个析取分量" $D_i "都具有形式:"
      (MB (&conj (_cm $l $i $1) (_cm $l $i $2)
                 $..c (_cm $l $i $m_i)))
      "并且每个" (_cm $l $i $j) "都是一个文字. "
      "因此, DNF公式也具有NNF形式, "
      "只是添加了其为" (Q "合取的析取")
      "这一额外限制, 不能使得" (Q $conj)
      "和" (Q $disj) "任意交错. "
      "{译注: DNF应该也和NNF一样具有" $bottom
      "和" $top "两种退化情形.} "
      "这全然类似于代数里全然展开的"
      (Q "积之和") "表达式, 例如"
      (&+ $x^3 (&i* $x^2 $y) (&i* $x $y) $z) ".")
   (P "对偶地, 一个公式被称为是具有" (Em "合取范式")
      " (CNF), 当其具有形式:"
      (MB (&conj $C_1 $C_2 $..c $C_n))
      "而每个合取分量" $C_i "都具有形式:"
      (MB (&disj (_cm $l $i $1) (_cm $l $i $2)
                 $..c (_cm $l $i $m_i)))
      "并且每个" (_cm $l $i $j) "都是一个文字. "
      "因此, CNF公式也具有NNF形式, "
      "只是添加了其为" (Q "析取的合取")
      "这一额外限制. 这类似于普通代数里全然分解了的"
      (Q "和之积") "形式, 例如"
      (&i* (@+ $x $1) (@+ $y $2) (@+ $z $3))
      ". 在普通代数里, 我们总是可以将表达式展开为积之和, "
      "但一般并不总是能分解为和之积 (例如考虑"
      (&- (&+ $x^2 $y^2) $1)
      "). 这种非对称性并不存在于逻辑中, "
      "这我们可以从" $conj "和" $disj
      "的对偶性中料想得到. "
      "首先我们将会展示如何将一个公式变换为一个等价的DNF, "
      "然后这个变换很容易调整为产生一个等价的CNF.")
   (H4. "通过真值表得到DNF")
   (P "如果一个公式牵涉原子" (setE $p_1 $..h $p_n)
      ", 那么其真值表的每一行都确定了对于"
      (setE $p_1 $..h $p_n)
      "的一个特定真值指派方式, "
      "由此又确定了一类赋值, "
      "其对于该原子集合有着相同的指派 "
      "(我们并不关心这些赋值对于其他原子的指派是什么). "
      "现在给定任意的赋值" $v ", 考虑公式:"
      (MB (&conj $l_1 $..c $l_n))
      "其中"
      (MB (&= $l_i
              (Choice0
               ($p_i ", 如果" (&= (app $v $p_i) $true))
               ((&neg $p_i) ", 如果" (&= (app $v $p_i) $false))))))
   (P "根据构造, 一个赋值" $w "满足" (&conj $l_1 $..c $l_n)
      "当且仅当" $w "和" $v "在" (&cm $p_1 $..h $p_n)
      "上的值是相合的. 现在原本公式的真值表里那些最后一列为"
      (Q $true) "的行恰好确定了满足该公式的所有类. "
      "{译注: 这个类可以理解为等价类, 划分方式就是"
      (&cm $p_1 $..h $p_n) "上的指派.} "
      "据此, 对于" $k "个" (Q "真") "行的每一个, "
      "我们都可以挑选一个相对应的赋值" $v_i
      " (为了确定起见, 可以令除了"
      (setE $p_1 $..h $p_n)
      "之外的所有变量都映射为" (Q $false)
      "), 然后构造上述公式:"
      (MB (&= $D_i (&conj (_cm $l $i $1)
                          $..c
                          (_cm $l $i $n))) "."))
   (P "既然析取" (&disj $D_1 $..c $D_k)
      "和原本的公式恰好由相同的那些赋值满足, "
      "那么因而其是逻辑等价的; "
      "而且, 根据构造的方式, 其又必然具有DNF形式.")
   (P "为了在OCaml中实现这个过程, "
      "我们从函数" (Code "list_conj")
      "和" (Code "list_disj")
      "开始, 其分别将一个公式列表"
      (|[;]| $p_1 $..h $p_n)
      "映射为迭代的合取"
      (&conj $p_1 $..c $p_n)
      "和迭代的析取"
      (&disj $p_1 $..c $p_n)
      ". 在列表为空的特殊情形下, 我们分别返回"
      $top "和" $bottom
      ". 这些选择避免了之后的一些特殊情形区分, "
      "并且也是自然的, 如果我们将这些公式想成是"
      (Q "所有的" (&cm $p_1 $..h $p_n) "都为真")
      " (如果没有任何" $p_i ", 那就是空虚为真) 和"
      (Q "某个" (&cm $p_1 $..h $p_n) "为真")
      " (如果没有任何" $p_i ", 那就必然为假)."
      (CodeB "let list_conj l = if l = [] then True else end_itlist mk_and l;;

let list_disj l = if l = [] then False else end_itlist mk_or l;;"))
   (P "接下来我们有一个函数" (Code "mk_lits")
      ", 其对于一个公式列表" (Code "pvs")
      ", 制作一个由这些公式或其否定构成的合取, "
      "否定与否是按照每个公式是否由赋值" $v
      "满足所决定的."
      (CodeB "let mk_lits pvs v =
  list_conj (map (fun p -> if eval p v then p else Not p) pvs);;"))
   (P "现在我们定义" (Code "allsatvaluations")
      ", 其和" (Code "allvaluations")
      "结构很像, 但是作用在于将满足" (Code "subfn")
      "的赋值收集为一个列表:"
      (CodeB "let rec allsatvaluations subfn v pvs =
  match pvs with
    [] -> if subfn v then [v] else []
  | p::ps -> let v' t q = if q = p then t else v(q) in
             allsatvaluations subfn (v' false) ps @
             allsatvaluations subfn (v' true) ps;;"))
   (P "使用这个函数, 我们可以挑选出满足公式的赋值列表, "
      "然后对于它用" (Code "make_lits")
      "进行map, 然后将结果收集为一个迭代析取. "
      "注意到在公式没有变量或者不可满足的情况下, "
      "这个过程会适切地返回" $bottom "或者" $top "."
      (CodeB "let dnf fm =
  let pvs = atoms fm in
  let satvals = allsatvaluations (eval fm) (fun s -> false) pvs in
  list_disj (map (mk_lits (map (fun p -> Atom p) pvs)) satvals);;")
      "{译注: 这些函数里的" (Code "pvs") "的含义并不完全相同, "
      "有的是原子名字的列表, 有的是原子公式的列表. "
      "不过, 我感到这里的" (Code "mk_lits")
      "有小小的低效之处. 其实它也可以设计为接受原子名字的列表, "
      "这样的话就不需要使用" (Code "eval") "而可以直接应用赋值本身了. "
      (Code "eval") "的工作其实也是要拆掉外面的" (Code "Atom")
      "然后喂给赋值. 以下是我在Scheme中重新实现的版本:"
      (CodeB "(define ((make-lits x*) v)
  (make-conj
   (map (lambda (x)
          (if (v x) x `(not ,x)))
        x*)))
(define (allsat f v a*)
  (if (null? a*)
      (if (f v) (list v) '())
      (let ((a (car a*))
            (a* (cdr a*)))
        (define ((v^ b) x)
          (if (eq? x a) b (v x)))
        (append (allsat f (v^ #f) a*)
                (allsat f (v^ #t) a*)))))
(define (dnf exp)
  (let* ((a* (atoms exp))
         (v* (allsat (curry ev exp)
                     (lambda (x) #f)
                     a*)))
    (make-disj
     (map (make-lits a*) v*))))"))
   (P "例如:"
      (CodeB "# let fm = &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>;;
val fm : prop formula = &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>
# dnf fm;;
- : prop formula = &lt;&lt;~p /\\ q /\\ r \\/ p /\\ ~q /\\ ~r \\/ p /\\ q /\\ ~r>>"))
   (P "不出所料, 这个结果的析取分量自然地对应于真值表里产生"
      (Q $true) "的三行, 由此确定了三类赋值:"
      (CodeB "# print_truthtable fm;;
p     q     r     | formula
---------------------------
false false false | false
false false true  | false
false true  false | false
false true  true  | true
true  false false | true
true  false true  | false
true  true  false | true
true  true  true  | false
---------------------------"))
   (P "这种方法无需初始化简或者预规范化, "
      "并且强调了DNF和真值表之间的关系. "
      "我们现在可以确认第2.4节中提出的论断: 给定任意一个"
      $n "元真值函数, 我们可以将其视为一个包含"
      $n "个原子命题和" $2^n "行的真值表, 并直接构造一个公式 "
      "(以析取范式的形式), 使得该真值函数就是它的解释. "
      "另一方面, 当" $n " (即原公式中原子命题的数量) 较大时, "
      "需要考虑所有" $2^n "个赋值这一点是相当不理想的. "
      "例如, 下面这个公式本身已经是一个简洁的析取范式, "
      "却会被膨胀成一个复杂得多的变体:"
      (CodeB "# dnf &lt;&lt;p /\\ q /\\ r /\\ s /\\ t /\\ u \\/ u /\\ v>>;;
..."))
   (H4. "通过变换得到DNF")
   (P "一种创建等价DNF形式的替代方法是将其与普通代数进行类比. "
      "在普通代数中, 为了得到完全展开的形式, 我们可以反复应用分配律"
      (disR $x $y $z) "和" (disL $x $y $z)
      ". 类似地, 从NNF形式的命题公式开始, "
      "我们可以通过基于以下重言将其反复重言以得到DNF形式:"
      (eqn*
       ((&conj $p (@disj $q $r))
        $<=>
        (&disj (&conj $p $q) (&conj $p $r)))
       ((&conj (@disj $p $q) $r)
        $<=>
        (&disj (&conj $p $r) (&conj $q $r)))))
   (P "为了将其编码为一个有效的OCaml函数而不过多次遍历公式树, "
      "我们需要一些小心. 我们从一个反复应用分配律的函数开始, "
      "假定其直接子公式已经具有DNF形式:"
      (CodeB "let rec distrib fm =
  match fm with
    And(p,(Or(q,r))) -> Or(distrib(And(p,q)),distrib(And(p,r)))
  | And(Or(p,q),r) -> Or(distrib(And(p,r)),distrib(And(q,r)))
  | _ -> fm;;"))
   (P "现在, 当输入公式为一个合取或析取时, "
      "我们首先递归地将其直接子公式转换为DNF, "
      "然后若有必要则使用之前的函数进行"
      (Q "分配 (distribute)") ":"
      (CodeB "let rec rawdnf fm =
  match fm with
    And(p,q) -> distrib(And(rawdnf p,rawdnf q))
  | Or(p,q) -> Or(rawdnf p,rawdnf q)
  | _ -> fm;;")
      "{译注: 证明" (Code "rawdnf") "和" (Code "distrib")
      "的正确性需要稍微细致一些的分析, "
      "鉴于这本书对于NNF和DNF(以及CNF)"
      "的定义和其他材料比起来也稍微细一些, "
      "以下是我在Scheme中编写程序时写下的关于句法的注释:"
      (CodeB ";&lt;literal> ::= &lt;var> | (not &lt;var>)
;&lt;nnf> ::= &lt;bool> | &lt;nnf0>
;&lt;nnf0> ::= &lt;literal>
;        |  (and &lt;nnf0> &lt;nnf0>)
;        |  (or &lt;nnf0> &lt;nnf0>)
;&lt;dnf> ::= &lt;bool> | &lt;dnf0>
;&lt;dnf0> ::= &lt;conj> | (or &lt;dnf0> &lt;dnf0>)
;&lt;conj> ::= &lt;literal> | (and &lt;conj> &lt;conj>)")
      "当然了, 证明的手段终归还是结构归纳.}")
   (P "例如:"
      (CodeB "# rawdnf &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>;;
- : prop formula =
&lt;&lt;(p /\\ ~p \\/ (q /\\ r) /\\ ~p) \\/ p /\\ ~r \\/ (q /\\ r) /\\ ~r>>"))
   (P "虽然这已然是DNF, 但是仍然很难阅读, "
      "因为迭代合取和析取混合结合在了一起. "
      "而且, 一些析取分量完全是冗余的: "
      (&conj $p (&neg $p)) "和"
      (&conj (@conj $q $r) (&neg $r))
      "都与" $bottom "逻辑等价, "
      "故可以省略而不破坏逻辑等价性.")
   (H4. "基于集合的表示")
   (P "为了使结合问题不再显得那么令人困扰, "
      "以及使得通过列表操作进行化简更为容易, "
      "将DNF表示为文字集合的集合是比较方便的, "
      "例如使用"
      (setE (setE $p $q)
            (setE (&neg $p) $r))
      "表示"
      (&disj (&conj $p $q)
             (&conj (&neg $p) $r))
      ". 既然DNF的逻辑结构总是合取的析取, "
      "并且析取与合取(的语义)都是结合的, 交换的, 幂等的, "
      "在这种翻译里没有什么本质性的东西会丢失, "
      "而且也容易映射回实际的公式. "
      "现在我们可以按如下方式写下DNF函数, "
      "使用OCaml列表表示集合, "
      "但是要注意在构造时避免重复:"
      (CodeB "let distrib s1 s2 = setify(allpairs union s1 s2);;

let rec purednf fm =
  match fm with
    And(p,q) -> distrib (purednf p) (purednf q)
  | Or(p,q) -> union (purednf p) (purednf q)
  | _ -> [[fm]];;")
      "{译注: 注意到在这种表示下, 单独的" (Code "True")
      "和" (Code "False") "会分别变成" (Code "[[True]]")
      "和" (Code "[[False]]")
      ". 虽然这可能是作者意料之中的事情, "
      "但我觉得还是有必要说一下. "
      "(后面有单独判断, 那没事了.) "
      "另外, 我不太清楚这里的" (Code "setify")
      "能否在列表元素顺序不同的情况下有效清理冗余列表. "
      "(看了一眼源代码, " (Code "union") "和" (Code "setify")
      "都会对于列表进行排序, 并且似乎用于比较的函数"
      (Code "Pervasives.compare")
      "对于列表会进行字典序比较, 那么其实就没有问题了.) "
      "虽然即便完全不清理, 得到的结果也是合理的. "
      "但是有一点我们可以证明, "
      "就是作为DNF的每个析取分量的合取里面不会有重复文字.}")
   (P "本质的结构是相同的; 这次的" (Code "distrib")
      "只是取两个集合的集合, "
      "然后返回所有从中可能取出的集合序对之并. "
      "如果我们将其应用于相同的例子, "
      "会得到相同的结果, modulo新的表示:"
      (CodeB "# purednf &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>;;
- : prop formula list list =
[[&lt;&lt;p>>; &lt;&lt;~p>>]; [&lt;&lt;p>>; &lt;&lt;~r>>]; [&lt;&lt;q>>; &lt;&lt;r>>; &lt;&lt;~p>>];
 [&lt;&lt;q>>; &lt;&lt;r>>; &lt;&lt;~r>>]]"))
   (P "但是, 得益于列表表示, 现在化简所得公式变得相当容易. "
      "首先我们定义一个函数" (Code "trivial")
      ", 用于检查同一列表中是否存在形如"
      $p "和" (&neg $p) "这样的互补文字. "
      "我们通过将文字划分为肯定文字和否定文字两组来实现这一点, "
      "然后检查肯定文字的集合与否定文字取反后的集合之间是否存在公共元素:"
      (CodeB "let trivial lits =
  let pos,neg = partition positive lits in
  intersect pos (image negate neg) &lt;> [];;")
      "现在我们可以通过过滤只留下无矛盾的析取分量, 例如"
      (CodeB "# filter (non trivial) (purednf &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>);;
- : prop formula list list = [[&lt;&lt;p>>; &lt;&lt;~r>>]; [&lt;&lt;q>>; &lt;&lt;r>>; &lt;&lt;~p>>]]"))
   (P "这已经给出了更小的DNF. "
      "另一种在许多场合下值得应用的优化是基于"
      (Em "subsumption") "的. 注意到如果"
      (&sube (setE (_prime $l $1) $..h (_prime $l $m))
             (setE $l_1 $..h $l_n))
      ", 那么每个满足"
      (&= $D (&conj $l_1 $..c $l_n))
      "的赋值也满足"
      (&= $D^ (&conj (_prime $l $1) $..c (_prime $l $m)))
      ". 因此, 析取" (&disj $D $D^) "逻辑等价于"
      $D^ ". 这种情况下, 我们称" $D^ " subsumes " $D
      ", 或者" $D " is subsumed by " $D^
      ". 以下是我们总体的函数, 其接受一个已经是NNF形式的公式, "
      "产生一个与之等价的DNF公式, 使用集合的集合表示, "
      "它首先会得到未经化简的DNF, 然后过滤掉矛盾和subsumed的析取分量:"
      (CodeB "let simpdnf fm =
  if fm = False then [] else if fm = True then [[]] else
  let djs = filter (non trivial) (purednf(nnf fm)) in
  filter (fun d -> not(exists (fun d' -> psubset d' d) djs)) djs;;")
      "{译注: 这里的" (Code "psubset")
      "是一个判断是否为真子集的谓词. "
      "个人认为还有一种可能效率更高的写法, "
      "那就是按照列表长度对于析取分量从大到小进行排序, "
      "然后判断析取分量是否冗余就只需要看它之后的项即可:"
      (CodeB "(define (simpdnf exp)
  (cond ((eq? exp #f) '())
        ((eq? exp #t) '(()))
        (else
         (let rec ((rest (sort (filter (compose not trivial?)
                                       (purednf (nnf exp)))
                               > #:key length)))
           (cond ((null? rest) '())
                 ((memf (lambda (d)
                          (subset? d (car rest)))
                        (cdr rest))
                  (rec (cdr rest)))
                 (else (cons (car rest)
                             (rec (cdr rest)))))))))"))
   (P "注意我们对" (Q $bottom) "和" (Q $top)
      "进行了特殊处理, 分别返回空列表和包含一个空合取式的单元素列表. "
      "此外, 在主代码中, 剔除矛盾析取项也可能产生空列表. "
      "如果所有析取分量确实都是矛盾的, 那么该公式在逻辑上必然等价于"
      (Q $bottom) ", 这与我们之前定义的" (Code "list_disj")
      "函数对空列表的解释是一致的. 要将所有内容转换回公式, 我们只需执行:"
      (CodeB "let dnf fm = list_disj(map list_conj (simpdnf fm));;"))
   (P "我们可以检验, 尽管我们的构造相当复杂, "
      "但是的确返回了逻辑等价的公式:"
      (CodeB "# let fm = &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>;;
val fm : prop formula = &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>
# dnf fm;;
- : prop formula = &lt;&lt;p /\\ ~r \\/ q /\\ r /\\ ~p>>
# tautology(Iff(fm,dnf fm));;
- : bool = true"))
   (P "注意到一个DNF公式是可满足的, 当且仅当其中某个析取分量是可满足的, 这直接由析取的语义可知. "
      "而每个析取分量本身是文字的合取, 它是可满足的当且仅当其中不包含两个互补文字 "
      "(并且当不包含时, 我们可以像使用真值表求DNF时那样找到一个满足的赋值). "
      "{译注: 实际上, 肯定文字对应的原子取真, 否定文字对应的原子取假就够了.} "
      "因此, 将一个公式转换为等价的DNF之后, 我们就能快速高效地判断它是否可满足. "
      "(事实上, 我们最新的DNF函数已经剔除了所有矛盾的析取分量, "
      "所以一个公式是可满足的当且仅当简化后的DNF中仍然包含至少一个析取分量.) "
      "{译注: 换言之, 最终的" (Code "dnf") "产生的结果不为"
      $bottom "就说明一定是可满足的.} "
      "然而, 这种方法并不一定优于真值表方法, 因为等价的DNF可能是指数级大的.")
   (H4. "CNF")
   (P "对于CNF而言, 我们将会使用和之前一样使用基于列表的表示, "
      "只不过现在隐式的解释为析取的合取而已. "
      "注意到根据De Morgan律, 我们有: 如果"
      (MB (&<=> (&neg $p)
                (Disj (&= $i $1) $m
                      (Conj (&= $j $1) $n
                            (_cm $p $i $j)))))
      "那么"
      (MB (&<=> $p
                (Conj (&= $i $1) $m
                      (Disj (&= $j $1) $n
                            (&- (_cm $p $i $j))))))
      "{译注: 这里的" $n "是常量, 然而实际上根据上下文来看, "
      "其应该是变量才对, 所以把" $n "改成" $n_i "更为合理.}")
   (P "因此, 以列表表示, 我们可以按照以下方式产生等价的CNF公式: "
      "首先对于初始公式进行否定 (并将其置于NNF形式), "
      "然后产生其DNF形式, 最后再对于所有的文字进行否定:"
      (CodeB "let purecnf fm = image (image negate) (purednf(nnf(Not fm)));;"))
   (P "基于形式列表操作, 消除CNF的冗余和subsumed的合取分量的代码是相同的, "
      "尽管解释是不同的. 例如, 现在平凡的合取分量表示"
      "包含某个文字和其否定的析取, 故等价于" $top
      "; 既然" (&<=> (&conj $top $C) $C)
      ", 将其排除于最终的合取之外也是同等合理的. "
      "只有两个退化情形需要以不同的方式处理:"
      (CodeB "let simpcnf fm =
  if fm = False then [[]] else if fm = True then [] else
  let cjs = filter (non trivial) (purecnf fm) in
  filter (fun c -> not(exists (fun c' -> psubset c' c) cjs)) cjs;;")
      "现在我们只需映射回作为公式的正确解释:"
      (CodeB "let cnf fm = list_conj(map list_disj (simpcnf fm));;")
      "例如:"
      (CodeB "# let fm = &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>;;
val fm : prop formula = &lt;&lt;(p \\/ q /\\ r) /\\ (~p \\/ ~r)>>
# cnf fm;;
- : prop formula = &lt;&lt;(p \\/ q) /\\ (p \\/ r) /\\ (~p \\/ ~r)>>
# tautology(Iff(fm,cnf fm));;
- : bool = true"))
   (P "正如我们可以快速检测一个DNF公式的可满足性一样, "
      "我们也可以快速检测一个CNF公式的有效性. "
      "{译注: 这里有效和重言同义.} "
      "事实上, 一个合取式" (&conj $C_1 $..c $C_n)
      "是有效的, 当且仅当每个" $C_i "都是有效的. "
      "而由于每个" $C_i "是文字的析取, "
      "它是有效的当且仅当它包含某个文字与其否定的析取; "
      "否则, 我们可以构造一个不满足它的赋值. "
      "再一次, 使用我们的简化CNF, 事情会更加简单: "
      "一个公式是有效的, 当且仅当其简化CNF恰好是" $top
      ". 同样, 这未必是一个好的实用算法, "
      "因为转换为CNF的过程中可能产生指数级的膨胀.")
   (H4. "原书的微妙bug")
   (P "鉴于我是事后才发现这些bug的, 所以我感觉单开一小节也不为过. "
      "事实上, 我之前的一些注记和注记里的程序也有错误.")
   (P "这些错误归根结底是因为忘了考虑" (Code "nnf")
      "的输出也可能是" (Code "True") "或者"
      (Code "False") ", 由此引发了意料之外的问题. 鉴于"
      (Q "垃圾进, 垃圾出")
      "原则, 在这里进行事后分析恐怕只是浪费时间, "
      "不如直接重写.")
   (CodeB "(define (subsume exp)
  (let rec ((rest (sort (filter (compose not trivial?) exp)
                        > #:key length)))
    (cond ((null? rest) '())
          ((memf (lambda (d)
                   (subset? d (car rest)))
                 (cdr rest))
           (rec (cdr rest)))
          (else (cons (car rest)
                      (rec (cdr rest)))))))
(define (purednf0 exp)
  (define (distrib s1 s2)
    (setlize (allpairs U s1 s2)))
  (define (dnf exp)
    (match exp
      ((and ,e1 ,e2)
       (distrib (dnf e1) (dnf e2)))
      ((or ,e1 ,e2)
       (U (dnf e1) (dnf e2)))
      (,else `((,exp)))))
  (cond ((eq? exp #f) '())
        ((eq? exp #t) '(()))
        (else (dnf exp))))
(define purednf
  (compose purednf0 nnf))
(define (simpdnf exp)
  (subsume (purednf exp)))
(define (dnf exp)
  (make-disj
   (map make-conj (simpdnf exp))))
(define (purecnf exp)
  (map (curry map negate)
       (purednf `(not ,exp))))
(define (simpcnf exp)
  (subsume (purecnf exp)))
(define (cnf exp)
  (make-conj
   (map make-disj (simpcnf exp))))")
   (P "因为译者更熟悉Scheme, 所以这里的代码也是用Scheme写成的. "
      "不过, 我也说明一下应该改什么. " (Code "purednf")
      "应该接受任意的公式输入, 在内部调用" (Code "nnf")
      ", 并且处理两个逻辑常量的特别情况. "
      "这么做的一个很大的好处就是现在" (Code "purednf")
      "和" (Code "purecnf") "是完全对称的过程了, "
      "它们都接受任意的公式, 分别产生DNF和CNF, "
      "但是都是以列表之列表的形式. "
      (Code "simpdnf") "和" (Code "simpcnf")
      "现在都无需再处理特别情形, "
      "实际上只是消除具有互补文字的冗余, "
      "并根据subsumption规则进行化简. "
      "这在DNF和CNF情况下都是完全一样的, "
      "所以我们将这个简化过程抽象为" (Code "subsume")
      ", 由此我们可以看到高度的对称之美.")
   (P "测试一下:"
      (CodeB "> (define peirce '(=> (=> (=> p q) p) p))
> (simpdnf peirce)
'(((not p)) (p))
> (dnf peirce)
'(or (not p) p)
> (simpcnf peirce)
'()
> (cnf peirce)
#t
> (define e0 '(&lt;=> p (&lt;=> q r)))
> (simpdnf e0)
'((p q r) (p (not q) (not r)) ((not p) q (not r)) ((not p) (not q) r))
> (simpcnf e0)
'(((not p) (not q) r) ((not p) q (not r)) (p (not q) (not r)) (p q r))
> (tautology? `(&lt;=> ,e0 ,(dnf e0)))
#t
> (tautology? `(&lt;=> ,e0 ,(cnf e0)))
#t"))
   (H3. "命题逻辑的应用")
   (P "我们已经完成了命题逻辑的基础学习, 确定了后续将要使用的主要概念, "
      "并将各种操作机械化, 包括重言式的识别. "
      "从某种角度来看, 我们的工作已经完成了. "
      "但对于许多更复杂的公式而言, 这些识别重言式的方法并不实用, "
      "在后续章节中我们将介绍更高效的算法. "
      "要测试这些算法, 甚至要证明其必要性, "
      "如果没有一批非平凡的命题公式作为储备, 是相当困难的. "
      "虽然在Pelletier (1986) 等文献集中有各种现成的命题问题可供使用, "
      "但我们将开发一些方法, 从简洁的描述出发生成整类有趣的命题问题.")
   (H4. "Ramsey定理")
   (P "我们首先考虑Ramsey的组合定理的一些特殊情形 "
      "(Ramsey 1930; Graham, Rothschild和Spencer 1980). "
      "一个简单的Ramsey型结果是: 在任何一个六人聚会中, "
      "必然存在三个人彼此都认识, 或者三个人彼此都不认识. "
      "习惯上我们基于图 (graph) 思考这样的问题, "
      "即一个顶点 (vertex) 集" $V
      "连带着由边 (edge) 所连接的特定点对, "
      "而边是从一个集合" $E "中取出的. "
      "一种对于" (Q "六人聚会") "结果的泛化 "
      "(尽管仍然远没有Ramsey定理一般) 是:")
   (P "{原注: 请参见第5.5节, "
      "了解Ramsey在引入其定理时正在解决的逻辑问题. "
      "另一个其与逻辑的联系是, 第一个" (Q "自然的")
      "独立于一阶Peano算术的命题 (Paris和Harrington 1991) "
      "本质上是一个Ramsey型结果的数值编码.}")
   ((Theorem)
    "对于每个" (∈ $s $t $NN) ", 存在某个" (∈ $n $NN)
    "使得任意具有" $n "个顶点的图要么有一个大小为" $s
    "的完全连通子图, 要么有一个大小为" $t
    "的完全不连通子图 (可兼的或). 而且, 如果"
    (Q "Ramsey数") (Ramsey $s $t)
    "代表对于给定的" $s "和" $t
    "而言的最小的这样的" $n ", 那么我们有:"
    (MB (&<= (Ramsey $s $t)
             (&+ (Ramsey (&- $s $1) $t)
                 (Ramsey $s (&- $t $1)))) "."))
   ((proof)
    "对于" (&+ $s $t) "施行完全归纳. "
    "我们可以根据归纳假设假定结果对于任意的"
    $s^ "和" $t^ "满足" (&< (&+ $s^ $t^) (&+ $s $t))
    "成立, 然后我们需要证明" $s "和" $t "的情形." (Br)
    "考虑具有大小"
    (&= $n (&+ (Ramsey (&- $s $1) $t)
               (Ramsey $s (&- $t $1))))
    "的任意图. 取一个任意的顶点" $v
    ". 要么存在至少" (Ramsey (&- $s $1) $t)
    "个顶点与" $v "相连, 要么存在至少"
    (Ramsey $s (&- $t $1)) "个顶点不与"
    $v "相连, 否则的话图的总大小至多只有"
    (&= (&+ (@- (Ramsey (&- $s $1) $t) $1)
            (@- (Ramsey $s (&- $t $1)) $1)
            $1)
        (&- $n $1))
    ", 这违背了假设. 我们假设前一种情形成立, "
    "后一种情形的论证是对称的." (Br)
    "考虑基于与" $v "相连的顶点集的子图, "
    "其大小至少为" (Ramsey (&- $s $1) $t)
    ". 根据归纳假设, 要么其有一个大小为"
    (&- $s $1) "的完全连通子图, 要么其有一个大小为"
    $t "的完全不连通子图. 如果是前者, 包括"
    $v "的话就能给出主图的一个大小为" $s
    "的完全连通子图, 那么我们就结束了. "
    "如果是后者, 那么我们已经有了一个大小为"
    $t "的完全不连通子图, 这正是我们所要的. "
    "于是, 任何大小为" $n
    "的图要么有一个大小为" $s
    "的完全连通子图, 要么有一个大小为"
    $t "的完全不连通子图. 换言之, "
    (&<= (Ramsey $s $t) $n) ".")
   (P "{译注: 这个定理的陈述及其论证比看上去要复杂. "
      "并且, 它的陈述还有一个小错, 即"
      (&= $s $1) "且" (&= $t $1)
      "时这个不等式并不成立. "
      "不过除了这个特殊情形之外, 它的确对于任意的"
      (&>= $s $1) "和" (&>= $t $1)
      "成立. 所以说, 论证里其实也存在一个小错, "
      "就是在这个特殊情形下我们没法从那个图里取出一个顶点, "
      "因为此时压根就没有顶点. 不过, 在"
      (&= $s $1) "且" (&>= $t $2) ", 或者"
      (&>= $s $2) "且" (&= $t $1)
      "的边缘情形, 原文对于相连不相连的中间结果虽然正确, "
      "但是论证相当于牵涉了负数目, 所以不太具有意义. "
      "原文这个对于不等式的论证的适用范围是"
      (&>= $s $2) "且" (&>= $t $2) "的情形. "
      "为了清晰起见, 实际上最好把它拆成两个定理. "
      "一个说明Ramsey数是良定的, 另一个说明不等式成立. "
      "不过, 不等式的证明本质上不需要归纳法, "
      "所以说原文证明里的" (Q "根据归纳假设")
      "的确令人困惑.}")
   (P "对于任意特定的正整数" (&cm $s $t $n)
      ", 我们可以表述一个命题公式, 其为重言恰当"
      (&<= (Ramsey $s $t) $n)
      ". 我们用整数" $1 "到" $n
      "为顶点编号, 计算所有的" $s "个元素和"
      $t "个元素的子集, 然后对于这些集合, "
      "计算所有的" $2 "元素子集. "
      "{译注: " $2 "元素子集代表的是边.} "
      "我们想要表达这样的事实, "
      "即对于所有的" $s "个元素子集, 有一个的每对元素都是连通的, "
      "或者对于所有的" $t "个元素子集, 有一个的每对元素都是不连通的. "
      "以下的局部定义" (Code "e[m;n]") "产生一个原子公式"
      (Code "p_m_n") ", 我们将其想成是"
      (Q $m "和" $n "是连通的") ", 或者说"
      (Q $m "和" $n "互相认识") ", 诸如此类:"
      (CodeB "let ramsey s t n =
  let vertices = 1 -- n in
  let yesgrps = map (allsets 2) (allsets s vertices)
  and nogrps = map (allsets 2) (allsets t vertices) in
  let e[m;n] = Atom(P(&quot;p_&quot;^(string_of_int m)^&quot;_&quot;^(string_of_int n))) in
  Or(list_disj (map (list_conj ** map e) yesgrps),
     list_disj (map (list_conj ** map (fun p -> Not(e p))) nogrps));;")
      "{译注: 实际上这个函数的参数"
      (Code "s") ", " (Code "t") ", " (Code "n")
      "都可以是自然数, 此时生成的命题公式仍然具有意义.}" (Br)
      "例如:"
      (CodeB "# ramsey 3 3 4;;
- : prop formula =
&lt;&lt;(p_1_2 /\\ p_1_3 /\\ p_2_3 \\/
   p_1_2 /\\ p_1_4 /\\ p_2_4 \\/
   p_1_3 /\\ p_1_4 /\\ p_3_4 \\/ p_2_3 /\\ p_2_4 /\\ p_3_4) \\/
  ~p_1_2 /\\ ~p_1_3 /\\ ~p_2_3 \\/
  ~p_1_2 /\\ ~p_1_4 /\\ ~p_2_4 \\/
  ~p_1_3 /\\ ~p_1_4 /\\ ~p_3_4 \\/ ~p_2_3 /\\ ~p_2_4 /\\ ~p_3_4>>"))
   (P "我们可以确认数字" $6 "是开始的聚会例子里最好的结果, 即"
      (&= (Ramsey $3 $3) $6) ":"
      (CodeB "# tautology(ramsey 3 3 5);;
- : bool = false
# tautology(ramsey 3 3 6);;
- : bool = true"))
   (P "然而, 后一个例子已经需要相当长的时间, 而即使稍大一些的输入参数, "
      "也会产生远超我们目前所述方法在合理时间内所能解决的命题问题. "
      "事实上, 已知的精确Ramsey数非常少, 截至撰写本书时, 即便是"
      (Ramsey $5 $5) "也只知道其值介于" 43 "到" 49 "之间.")
   (P "{译注: 以下是Scheme版本的实现:"
      (CodeB "(define ((allsets n) s)
  (define (a n s l)
    (cond ((= n 0) '(()))
          ((&lt; l n) '())
          ((= l n) (list s))
          (else
           (append (map (curry cons (car s))
                        (a (- n 1) (cdr s) (- l 1)))
                   (a n (cdr s) (- l 1))))))
  (a n s (length s)))
(define (ramsey s t n)
  (define v* (range n))
  (define (sub x)
    (map (allsets 2)
         ((allsets x) v*)))
  (define yes* (sub s))
  (define no* (sub t))
  (define (e p)
    (string->symbol
     (apply format &quot;p_~s_~s&quot; p)))
  (make-disj
   (append (map (lambda (p*)
                  (make-conj (map e p*)))
                yes*)
           (map (lambda (p*)
                  (make-conj (map (lambda (p)
                                    `(not ,(e p)))
                                  p*)))
                no*))))")
      "一个例子:"
      (CodeB "> (simpdnf (ramsey 3 3 4))
'((p_0_1 p_0_2 p_1_2)
  (p_0_1 p_0_3 p_1_3)
  (p_0_2 p_0_3 p_2_3)
  (p_1_2 p_1_3 p_2_3)
  ((not p_0_1) (not p_0_2) (not p_1_2))
  ((not p_0_1) (not p_0_3) (not p_1_3))
  ((not p_0_2) (not p_0_3) (not p_2_3))
  ((not p_1_2) (not p_1_3) (not p_2_3)))"))
   (H4. "数字电路")
   (P "数字计算机使用只能占据有限数量电压等级之一的电信号来运行. "
      "(相比之下, 在模拟计算机中, 电压等级可以连续变化.) "
      "几乎所有现代计算机都是二进制的, "
      "即只使用两个等级, 按惯例称为" $0 " (" (Q "低") ")和" $1
      " (" (Q "高") "). 在任何特定时刻, "
      "我们可以将二进制数字计算机中的每根内部或外部导线视为具有一个布尔值"
      -- $0 "对应" (Q "假") ", " $1 "对应" (Q "真") --
      "并将每个电路元件视为一个布尔函数, "
      "对其输入导线上的值进行运算, "
      "从而在输出导线上产生一个值. "
      "(当然, 采用这种观点时, 我们忽略了许多重要的物理层面, "
      "但我们在这里只关注逻辑结构.)")
   (P "数字电路的关键构建块, 即" (Em "逻辑门")
      ", 基本上对应于通常的逻辑联结词. 例如, 一个"
      (Q "AND门") "是对应于联结词" (Q "and")
      " (" $conj ") 的电路元素: 其有两个输入和一个输出, "
      "并且输出导线是高电平的 (为真), 恰当两个输入都是高电平的. "
      "类似地, 一个" (Q "NOT门") ", 或者说" (Em "反相器")
      ", 有一个输入导线和一个输出导线, "
      "并且当输入为低时输出为高, 输入为高时输出为低, "
      "因而对应于" (Q "not") "联结词. "
      "因此, 数字电路和公式之间存在着紧密的对应, "
      "其可以大致总结如下:"
      (MB (set-attr*
           (&Table
            ("数字设计" "命题逻辑")
            ("电路" "公式")
            ("逻辑门" "命题联结词")
            ("输入导线" "原子")
            ("内部导线" "子表达式")
            ("电平" "真值"))
           'columnalign "left"
           'frame "solid" 'rowlines "solid" 'columnlines "solid")))
   (P "例如, 以下逻辑电路对应于命题公式"
      (&disj (&conj (&neg $s) $x)
             (&conj $s $y))
      ". 一个具有此行为的复合电路元件被称为一个"
      (Em "选择器(multiplexer)")
      ", 因为其输出要么是输入" $x ", 要么是输入" $y
      ", 取决于" $s "是低电平还是高电平."
      todo.svg)
   (P "一个显著的区别是, 在电路中我们只需将导线一分为二就能复制输入"
      $s ", 而在表达式中则需要将" $s "书写两次. "
      "对于较大的子表达式, 这一差异会更加明显: "
      "在公式中我们可能需要将其书写多次, "
      "而在电路中只需从相应的电路元件引出多条导线即可. "
      "在第2.8节中, 我们将为公式发展出一种类似的技术.")
   (H4. "加法")
   (P "鉴于计算机采用两电平电路结构, "
      "数字在计算机中的主要表示方式自然是二进制位置表示法, "
      "而非十进制或其他方案. "
      "一个二进制数位, 即一个比特, "
      "可以用单根导线上的值来表示. 具有"
      $n "个二进制数位的较大数字可以用" $n
      "个比特的有序序列来表示, 并以" $n
      "根导线组成的阵列 (array) 来实现. "
      "(对于特定大小的阵列有专门的名称, "
      "例如字节或octet表示八个比特的序列.) "
      "我们在学校学到的多位算术的常规算法可以直接改写为二进制形式; "
      "事实上, 它们往往会变得更加简单.")
   (P "假设我们想要将两个二进制数相加, 每个数由一组" $n
      "个比特表示. 这意味着每个数的范围是从" $0 "到"
      (&- $2^n $1) ", 于是和的范围是从" $0 "到"
      (&- (^ $2 (&+ $n $1)) $2) ", 可能需要" (&+ $n $1)
      "个比特来存储. 我们只需像十进制一样从右到左逐位相加. "
      "当某一位的和" (&>= $ $2) "时, 我们将其减去" $2
      ", 并向下一个比特位产生一个值为" $1 "的" (Q "进位")
      ". 下面是一个例子, 对应于十进制的"
      (&= (&+ 179 101) 280) ":"
      (CodeB "       1  0  1  1  0  0  1  1
 +     0  1  1  0  0  1  0  1
------------------------------
 =  1  0  0  0  1  1  0  0  0"))
   (P "为了将" $n "位数的加法实现为电路或命题公式, "
      "最简单的方法是利用算法的规律性, 通过将一个"
      $1 "位加法器复制" $n "次来构建加法器, "
      "并在每对相邻元件之间传播进位. "
      "第一个任务是构建一个" $1 "位加法器, 这并不太难. "
      "我们可以将两个数字相加所产生的" (Q "和位")
      " (" $s ") 与" (Q "进位") " (" $c
      ") 视为两个独立的布尔函数, 其真值表如下所示, "
      "其中我们使用" $0 "和" $1 "而非" (Q $false)
      "和" (Q $true) "来强调其与算术的联系:"
      (MB (set-attr*
           (&Table
            ($x $y $c $s)
            ($0 $0 $0 $0)
            ($0 $1 $0 $1)
            ($1 $0 $0 $1)
            ($1 $1 $1 $0))
           'frame "solid" 'rowlines "solid" 'columnlines "solid")))
   (P "进位的真值表可能看起来很熟悉: 它就是" (Q "and")
      "运算" (&conj $x $y) ". 至于和位, 它是" (Q "or")
      "运算的不可兼版本, 可以表示为" (&neg (@<=> $x $y))
      "或者" (&<=> $x (&neg $y)) ", 缩写为XOR. "
      "我们可以在OCaml中实现与这些运算对应的函数, 如下所示:"
      (CodeB "let halfsum x y = Iff(x,Not y);;

let halfcarry x y = And(x,y);;")
      "现在我们可以将" (Em "半加器")
      "的输入导线和输出导线之间的关系断言如下:"
      (CodeB "let ha x y s c = And(Iff(s,halfsum x y),Iff(c,halfcarry x y));;"))
   (P "之所以使用" (Q "半") ", 是因为强调这只是我们所需要的一部分. "
      "除了最右的数位, 其他情况我们需要将三个比特加起来, "
      "而不只是两个, 因为还有传入的进位. 一个" (Em "全加器")
      "将三个比特相加, 因为答案" (&<= $ $3)
      ", 所以仍然可以只是将结果返回为一个和位和一个进位. "
      "其真值表如下:"
      (MB (set-attr*
           (&Table
            ($x $y $z $c $s)
            ($0 $0 $0 $0 $0)
            ($0 $0 $1 $0 $1)
            ($0 $1 $0 $0 $1)
            ($0 $1 $1 $1 $0)
            ($1 $0 $0 $0 $1)
            ($1 $0 $1 $1 $0)
            ($1 $1 $0 $1 $0)
            ($1 $1 $1 $1 $1))
           'frame "solid" 'rowlines "solid" 'columnlines "solid"))
      "并且一种作为门的可能实现如下:"
      (CodeB "let carry x y z = Or(And(x,y),And(Or(x,y),z));;

let sum x y z = halfsum (halfsum x y) z;;

let fa x y z s c = And(Iff(s,sum x y z),Iff(c,carry x y z));;"))
   (P "现在将多个全加器组合成一个" $n
      "位加法器是很容易的事情了, "
      "并且其在最低端允许一个进位传入, "
      "而在最高端传出第" (&+ $n $1)
      "位. {译注: 最后半句有点令我迷惑, "
      "不过有无这个进位输入都需要"
      (&+ $n $1) "个位来输出.} "
      "相应的OCaml函数期望用户提供函数"
      (Code "x") ", " (Code "y") ", " (Code "out")
      ", " (Code "c") ", 这些函数接受一个索引, "
      "生成一个合适的新(原子)变量. "
      (Code "x") "和" (Code "y")
      "所返回的变量用于诸输入位, "
      (Code "out") "返回的则用于输出位, 而"
      (Code "c") "返回的诸变量是在内部用作进位的, 其中"
      (Code "c(0)") "是进位入, 而" (Code "c(n)")
      "是进位出. {译注: 从概念上来说, 除了两端之外, "
      "这些用于进位的变量既是输入又是输出.}"
      (CodeB "let conjoin f l = list_conj (map f l);;

let ripplecarry x y c out n =
  conjoin (fun i -> fa (x i) (y i) (c i) (out i) (c(i + 1)))
          (0 -- (n - 1));;"))
   (P "例如, 以下是使用带索引的风格化名称作为输入, "
      "生成一个" $3 "位加法器:"
      (CodeB "let mk_index x i = Atom(P(x^&quot;_&quot;^(string_of_int i)))
and mk_index2 x i j =
  Atom(P(x^&quot;_&quot;^(string_of_int i)^&quot;_&quot;^(string_of_int j)));;
val mk_index : string -> int -> prop formula = &lt;fun>
val mk_index2 : string -> int -> int -> prop formula = &lt;fun>
# let [x; y; out; c] = map mk_index [&quot;X&quot;; &quot;Y&quot;; &quot;OUT&quot;; &quot;C&quot;];;
...")
      "我们得到:"
      (CodeB "# ripplecarry x y c out 2;;
- : prop formula =
&lt;&lt;((OUT_0 &lt;=> (X_0 &lt;=> ~Y_0) &lt;=> ~C_0) /\\
   (C_1 &lt;=> X_0 /\\ Y_0 \\/ (X_0 \\/ Y_0) /\\ C_0)) /\\
  (OUT_1 &lt;=> (X_1 &lt;=> ~Y_1) &lt;=> ~C_1) /\\
  (C_2 &lt;=> X_1 /\\ Y_1 \\/ (X_1 \\/ Y_1) /\\ C_1)>>"))
   (P "如果我们对最低端的进位输入不感兴趣, "
      "可以修改结构, 在该位的位置仅使用半加器. "
      "一个更简单但粗糙的替代方案是, 直接输入"
      (Code "False") " (即" $0 ") 并化简所得公式:"
      (CodeB "let ripplecarry0 x y c out n =
  psimplify
   (ripplecarry x y (fun i -> if i = 0 then False else c i) out n);;"))
   (P "之所以使用术语" (Q "波纹进位 (ripple-carry)")
      "加法器, 是因为进位从右向左依次流过各个全加器. "
      "在实际电路中, 门的输入发生变化到输出相应变化之间存在传播延迟. "
      "在极端情况下 (例如" (&+ (Mn "11111...11") $1)
      "), 最终输出位只有在进位经过" $n
      "个阶段传播之后才能得到, 大约需要" (&i* $2 $n)
      "个门延迟. 当" $n "相当大时, 比如" 64
      ", 这种延迟可能是无法接受的, 因此需要采用不同的设计. "
      "例如, 在进位选择加法器中, " $n "位输入被分成若干个" $k
      "位的块, 对应的" $k "位块被加法运算两次, "
      "一次假设进位输入为" $0 ", 一次假设进位输入为" $1
      ". 然后可以通过多路复用 (multiplexing) 来决定正确答案, "
      "以来自前一阶段的实际进位输入作为选择信号. "
      "这样, 进位只需在" (&/ $n $k)
      "个块中传播, 每个块仅有少量门延迟. "
      "为了实现这样的加法器, 我们需要另一个元件来补充"
      (Code "ripplecarry0") ", 这次强制进位输入为" $1 ":"
      (CodeB "let ripplecarry1 x y c out n =
  psimplify
   (ripplecarry x y (fun i -> if i = 0 then True else c i) out n);;")
      "当我们使用多路复用器 (选择器) 进行进位传播时, "
      "将在两种方案之间进行选择:"
      (CodeB "let mux sel in0 in1 = Or(And(Not sel,in0),And(sel,in1));;"))
   (P "现在可以递归地实现整体函数, "
      "使用一个辅助函数对于位阵列中的索引进行偏移:"
      (CodeB "let offset n x i = x(n + i);;"))
   (P "假设我们处理的是整体" $n "位中的第"
      (&cm $0 $..h (&- $k $1))
      "位. 我们分别在假定进位是" $0 "和" $1
      "的情况下将" $k "位相加, 这分别给出了输出"
      (Code "c0") ", " (Code "s0") "和"
      (Code "c1") ", " (Code "s1")
      ". 最终的和位与进位出位是由一个多路复用器选择的, "
      "而选择子为" (Code "c(0)")
      ". 剩余的" (&- $n $k) "个比特位可以通过递归处理, "
      "但是所有的比特向量需要都需要偏移" $k
      "位, 因为我们每次都是从" $0
      "开始. 额外需要注意的点在于" $n
      "可能不是" $k "的精确倍数, "
      "所以说实际上我们每次使用的都是" $k^
      ", 它要么是" $k ", 要么是比特位总数"
      $n ", 不论如何选择较小的那个:"
      (CodeB "let rec carryselect x y c0 c1 s0 s1 c s n k =
  let k' = min n k in
  let fm =
    And(And(ripplecarry0 x y c0 s0 k',ripplecarry1 x y c1 s1 k'),
        And(Iff(c k',mux (c 0) (c0 k') (c1 k')),
            conjoin (fun i -> Iff(s i,mux (c 0) (s0 i) (s1 i)))
                    (0 -- (k' - 1)))) in
  if k' &lt; k then fm else
  And(fm,carryselect
            (offset k x) (offset k y) (offset k c0) (offset k c1)
            (offset k s0) (offset k s1) (offset k c) (offset k s)
            (n - k) k);;")
      "{译注: 个人感觉这里的条件测试应该改成"
      (Code "(= (- n k^) 0)")
      ", 当然这是Scheme写法, 不过读者应该可以理解.}")
   (P "{译注: 关于为什么原文稍有冗余的版本也正确, "
      "我实际上思考了一段时间, 但是没有抓住头绪. "
      "后来我发现我忽略了一个关键的事实, "
      "也就是每个块里面进位输入为假所对应的进位输出, "
      "一定是小于等于进位输入为真所对应的进位输出的. "
      "这是一种单调性.}")
   (P "电路设计中的一个问题是验证某些效率优化没有对所计算的函数造成任何逻辑改变. "
      "因此, 如果从波纹进位加法器转变为进位选择加法器的优化是正确的, "
      "那么以下内容应当始终生成重言式. 它表明, 如果相同的输入向量"
      (Code "x") "和" (Code "y") "由两种不同的方法相加 (使用不同的内部变量), "
      "则每种方法中所有的和位输出与进位输出应当相同."
      (CodeB "let mk_adder_test n k =
  let [x; y; c; s; c0; s0; c1; s1; c2; s2] = map mk_index
      [&quot;x&quot;; &quot;y&quot;; &quot;c&quot;; &quot;s&quot;; "
             "&quot;c0&quot;; &quot;s0&quot;; &quot;c1&quot;; &quot;s1&quot;; &quot;c2&quot;; &quot;s2&quot;] in
  Imp(And(And(carryselect x y c0 c1 s0 s1 c s n k,Not(c 0)),
          ripplecarry0 x y c2 s2 n),
      And(Iff(c n,c2 n),
          conjoin (fun i -> Iff(s i,s2 i)) (0 -- (n - 1))));;"))
   (P "{译注: 我并没有完全理解这个程序, 不过从直觉上来说, "
      "它的确应该有一个可以改进的地方. " (Code "ripplecarry0")
      "只是默认了进位输入是" $0 ", 而对于进位选择加法器, "
      "生成的命题公式则是断言了进位输入是" $0
      ". 所以说, 原本的程序在" (Code "n") "为零时不是重言. "
      "一种改进方式是为波纹进位加法器添加关于进位输入的约束, "
      "当然此时我们只需要使用" (Code "ripplecarry") ".}")
   (P "这是一个实用的重言生成器. "
      "它也展示了计算机设计里的实际问题是如何由命题方法所解决的.")
   (H4. "乘法")
   (P "既然我们可以将" $n "位数字相加, "
      "那么我们就可以使用反复加法将数字相乘. "
      "又一次, 这里可以应用传统算法. "
      "考虑将两个" $4 "位数字" $A "和" $B
      "相乘. 我们将会使用记号" $A_i "和" $B_i
      "代表" $A "和" $B "的第" $i
      "位, 其中最低重要位 (LSB) 自零开始数, "
      "于是第" $i "位相当于隐式乘以了" $2^i
      ". 正和我们手工进行十进制算术时一样, "
      "我们可以将数字布局如下, 其中乘积项"
      (&i* $A_i $B_j) "若具有相等的"
      (&+ $i $j) "则被安排在同一列, "
      "然后将它们全都加起来:"
      todo.svg)
   (P "之后对于" (&i* $A_i $B_j) "我们将会记" (_cm $X $i $j)
      "; 每个这样的乘积项都可以通过单个AND门由输入位得到. "
      
      )
   (H4. "素性和因数分解")
   (H4. "命题逻辑的力量")
   (P "本节仅仅初步展示了某些问题如何可以归约为" (Q "SAT")
      ", 即命题公式的可满足性检验. Cook (1971) 的著名工作表明, "
      "包括SAT本身在内的一大类组合问题, 在精确意义上彼此具有完全相同的难度. "
      "(粗略地说, 求解其中任何一个问题的算法都可以产生求解其他任何一个问题的算法, "
      "运行时间至多增加一个多项式因子.) "
      "这类NP完全问题如今已知包含许多看起来非常困难且具有重大实际意义的问题 "
      "(Garey和Johnson 1979).")
   (P "我们的" (Code "tautology") "或" (Code "satisfiable")
      "函数在最坏情况下可能需要关于输入公式规模的指数级时间, "
      "因为它们可能需要在其" $n "个原子命题的全部"
      $2^n "个赋值上对公式求值. "
      "我们后续将要开发的算法在实践中要高效得多, "
      "但其最坏情况复杂度同样是指数级的. "
      "若能找到SAT或任何其他NP完全问题的多项式时间算法, "
      "就能由此得到所有NP完全问题的多项式时间算法. "
      "由于迄今为止尚未找到这样的算法, 人们普遍相信这是不可能的, "
      "但在撰写本书时这一点尚未被证明. 这就是著名的P=NP问题, "
      "它或许是离散数学和计算机科学中最突出的开放性问题. "
      "Baker, Gill和Solovay (1975) 给出了一些理由, "
      "说明为何许多看似可行的攻克该问题的方法不太可能奏效.")
   (P "尽管如此, 许多其他问题可归约为SAT这一事实也具有积极的意义. "
      "人们已经在SAT算法及其高效实现上投入了大量精力. "
      "实践中常常发现, 将问题仔细归约为SAT, "
      "然后使用这些工具之一来求解, "
      "其效果优于除最精良的专用算法之外的所有方法.")
   (H3. "定义性CNF")
   (P "我们已经观察到, 对CNF公式进行重言式检查是容易的, "
      "对DNF公式进行可满足性检验同样如此 ("
      (Ref "dnf-cnf") "). 遗憾的是, "
      "将公式转换为这两种范式中任一种的逻辑等价形式这一简单操作, "
      "都可能导致公式规模呈指数级膨胀. 这并非我们特定实现的缺陷, "
      "而是原则上不可避免的 (Reckhow 1976).")
   (P "然而, 如果我们要求一种比逻辑等价更弱的性质, 就可以做得好得多. "
      "我们将会展示如何将任意的公式" $p "转换为某个CNF公式" $p^
      ", 其使得" $p^ "最差也只会是" $p "的数倍大, "
      "并且" $p "和" $p^ "是" (Em "等可满足的(equisatisfiable)")
      ", 即" $p^ "是可满足的当且仅当" $p
      "是可满足的, 即便一般它们并不逻辑等价. "
      "我们可以对偶化这个过程以给出一个DNF公式, "
      "其和原本的公式是" (Em "等有效的(equivallid)")
      ", 即其是一个重言当且仅当原本的公式是一个重言. "
      "这两种过程都不能直接产生平凡的重言或者可满足性测试, "
      "因为CNF和DNF恰好颠倒了. 不过, "
      "它们至少为更高级的算法提供了一个有用的简化起点.")
   (P "其基本思想最初由Tseitin (1968) 提出, "
      "后来又以多种方式加以改进 (Wilson 1990), "
      "核心是引入新的原子命题作为子公式的缩略或者说" (Q "定义")
      ", 因此得名" (Q "定义性CNF")
      ". 理解这一方法的最佳途径大概是考察一个简单的典型例子. "
      "假设我们想将以下公式转换为CNF:"
      (MB (&conj (@disj $p (@conj $q (&neg $r))) $s) "."))
   (P "我们引入一个新的原子" $p_1 "来对于" (&conj $q (&neg $r))
      "进行缩略, 这个原子没有在公式的其他地方用过. "
      "然后我们将缩略了的公式与" $p_1 "的"
      (Q "定义") "结合在一起:"
      (MB (set-attr*
           (&Table
            ((&conj (@<=> $p_1 (&conj $q (&neg $r))) $))
            ((&conj (@disj $p $p_1) $s)))
           'columnalign "left")))
   (P "接着我们继续施行相同种类的步骤, 引入另一个原子"
      $p_2 "作为对于" (&disj $p $p_1) "的缩略:"
      (MB (set-attr*
           (&Table
            ((&conj (@<=> $p_1 (&conj $q (&neg $r))) $))
            ((&conj (@<=> $p_2 (&disj $p $p_1)) $))
            ((&conj $p_2 $s)))
           'columnalign "left"))
      "然后引入" $p_3 "作为对于"
      (&conj $p_2 $s) "的缩略:"
      (MB (set-attr*
           (&Table
            ((&conj (@<=> $p_1 (&conj $q (&neg $r))) $))
            ((&conj (@<=> $p_2 (&disj $p $p_1)) $))
            ((&conj (@<=> $p_3 (&conj $p_2 $s)) $))
            ($p_3))
           'columnalign "left"))
      "最后, 我们使用传统方法将每个合取分量转换为CNF形式:"
      (MB (set-attr*
           (&Table
            ((&conj (@disj (&neg $p_1) $q)
                    (@disj (&neg $p_1) (&neg $r))
                    (@disj $p_1 (&neg $q) $r) $))
            ((&conj (@disj (&neg $p_2) $p $p_1)
                    (@disj $p_2 (&neg $p))
                    (@disj $p_2 (&neg $p_1)) $))
            ((&conj (@disj (&neg $p_3) $p_2)
                    (@disj (&neg $p_3) $s)
                    (@disj $p_3 (&neg $p_2) (&neg $s)) $))
            ($p_3))
           'columnalign "left")))
   (P "我们可以看出来作为结果的公式只能比原本的公式"
      "在规模上大上一个较小的常量因子. "
      "引入的定义性合取分量的数目是由"
      "原本公式里的联结词数目所划定上界的. "
      "并且, 最终将每个合取分量变为CNF的展开只会导致较小的膨胀, "
      "因为这些分量的形式都比较简单. "
      "即便是最坏的情况下, 也就是" (&<=> $p (@<=> $q $r))
      ", 其等价的CNF形式里也只有" 11 "个二元联结词:"
      (CodeB "# cnf &lt;&lt;p &lt;=> (q &lt;=> r)>>;;
- : prop formula =
&lt;&lt;(p \\/ q \\/ r) /\\
  (p \\/ ~q \\/ ~r) /\\ (q \\/ ~p \\/ ~r) /\\ (r \\/ ~p \\/ ~q)>>"))
   (P "由此我们关于公式大小的声明得到了澄清. "
      "对于等可满足性, 我们只需表明每一定义性步骤都是保持可满足性的, "
      "因为总体上这个变换是一系列这样的步骤之后跟着一个逻辑等价的变换.")
   ((Theorem)
    "如果" $x "没有出现在" $q "里, 那么公式"
    (&psubst $x $q $p) "和" (&conj (@<=> $x $q) $p)
    "是等可满足的.")
   ((proof)
    "如果" (&psubst $x $q $p) "是可满足的, 比如说由一个赋值"
    $v "所满足, 那么根据" (Ref "subst-eval")
    ", 经过修饰的赋值" (&= $v^ (ext $v $x (&eval $q $v)))
    "能够满足" $p ". 这个赋值也能够满足" (&<=> $x $q)
    ", 因为根据构造, 我们有" (&= (app $v^ $x) (&eval $q $v))
    ", 并且鉴于" $x "没有出现在" $q
    "之中, 这个值又和" (&eval $q $v^)
    "相同 (" (Ref "same-on-atoms")
    "). 因此, " $v^ "满足" (&conj (@<=> $x $q) $p)
    ", 于是该公式也是可满足的." (Br)
    "反过来, 设某个赋值" $v "满足" (&conj (@<=> $x $q) $p)
    ". 因为其满足第一个合取分量, 故有"
    (&= (app $v $x) (&eval $q $v)) ", 因而"
    (ext $v $x (&eval $q $v)) "就是" $v
    ". 根据" (Ref "subst-eval") ", 赋值" $v "满足"
    (&psubst $x $q $p) ".")
   (P "这个证明的第二部分实际上说明了从右到左的推出"
      (&=> (&conj (@<=> $x $q) $p)
           (&psubst $x $q $p))
      "是一个重言. {译注: 并且, 无需" $x
      "不能出现在" $q "中的条件.} "
      "然而, 相反方向的推出是不行的, "
      "所以我们没有逻辑等价. "
      "这是因为, 若某个赋值" $v "满足"
      (&psubst $x $q $p) ", 鉴于" $x
      "没有出现在" $q "中, 所以"
      (&= $v^ (ext $v $x (&not (app $v $x))))
      "也能够满足" (&psubst $x $q $p)
      ". 但是, " $v "和" $v^
      "之中必然有不能满足"
      (&<=> $x $q) "的赋值.")
   (H4. "定义性CNF的实现")
   (P "对于新的命题变量, 我们将使用具有形式"
      (Code "p_n") "的风格化名称. "
      "下面的函数返回这样一个原子, "
      "同时返回递增后的索引以备下次使用. "
      "{译注: 对于Scheme和许多其他Lisp方言而言, "
      "我们有" (Code "gensym") "这种便利设施可用, "
      "无需费心于freshness问题. 不过, " (Code "gensym")
      "的语义可能相当微妙, 使用起来需要小心谨慎, "
      "并且可能不具备可移植性. 就译者使用的Racket而言, "
      "它会产生所谓的uninterned符号, 所以冲突的确是得以避免的, "
      "但是它并不能保证这些产生的符号在字面上就能和其他符号区别开来, "
      "所以说简单复制REPL输出再喂给其他过程作为输入在特定情况下可能造成问题, "
      "不过这种有问题的现象在实际使用时应该罕有发生. "
      "当然了, 若考虑到实际阅读和演示的需求, "
      "原文的做法比起使用" (Code "gensym") "是更正确的更好的.}"
      (CodeB "let mkprop n = Atom(P(&quot;p_&quot;^(string_of_num n))),n +/ Int 1;;")
      "为简单起见, 假设初始公式已经通过" (Code "nenf")
      "进行了预简化, 使得否定仅作用于原子, 并且推出式已被消除. "
      "主递归函数" (Code "maincnf")
      "接受一个三元组作为输入, "
      "其由一个要被转换的公式, "
      "一个给出了已经作出的" (Q "定义") "的有限部分函数, "
      "以及当前变量索引计数器的值构成. "
      "这个函数会返回一个类似的三元组, "
      "由被转换了的公式, 增长了的定义, "
      "以及跨越了已在定义中使用过了的变量的新计数器构成. "
      "其做的一切就是将顶层的二元联结词分解为类型构造器和其直接子公式, "
      "然后将其作为参数" (Code "op") "和" (Code "(p,q)")
      "传递给另一个一般函数" (Code "defstep")
      ", 这个函数进行了主要的工作. "
      "{译注: 这里说的类型构造器不是严格意义上的.} "
      "(两个函数" (Code "maincnf") "和" (Code "defstep")
      "是互递归的, 因而我们将其作为一个整体输入: "
      "注意到以下的代码没有双分号.)"
      (CodeB "let rec maincnf (fm,defs,n as trip) =
  match fm with
    And(p,q) -> defstep mk_and (p,q) trip
  | Or(p,q) -> defstep mk_or (p,q) trip
  | Iff(p,q) -> defstep mk_iff (p,q) trip
  | _ -> trip"))
   (P "在" (Code "defstep") "内部, 对于" (Code "maincnf")
      "的递归调用将左子公式" (Code "p") "进行转换, "
      "返回被转换了的公式" (Code "fm1")
      ", 增长了的定义列表" (Code "defs1")
      ", 以及计数器" (Code "n1")
      ". 右子公式" (Code "q")
      "连带着新的定义列表和计数器用在了另一个递归调用里, "
      "给出了一个被转换的公式" (Code "fm2")
      "以及进一步修饰了的定义" (Code "defs2")
      "和计数器" (Code "n2")
      ". 然后, 我们通过应用传入的构造器" (Code "op")
      "来构造合适的复合公式" (Code "fm'")
      ". 接着, 我们检查是否已经有定义对应于该公式. "
      "若是如此, 则返回定义变量. "
      "否则的话, 我们创建一个新的变量并插入一个新的定义, "
      "之后返回该变量作为简化了的公式, 当然调用"
      (Code "mkprop") "所得到的新计数器也要返回."
      (CodeB "and defstep op (p,q) (fm,defs,n) =
  let fm1,defs1,n1 = maincnf (p,defs,n) in
  let fm2,defs2,n2 = maincnf (q,defs1,n1) in
  let fm' = op fm1 fm2 in
  try (fst(apply defs2 fm'),defs2,n2) with Failure _ ->
  let v,n3 = mkprop n2 in (v,(fm'|->(v,Iff(v,fm'))) defs2,n3);;")
      "{译注: 这个优化的正确性实际上并非完全显然, 而是需要证明. "
      "这里我们需要用到" (Ref "eval-subst")
      ". 首先我们要把这Tseitin变换的中间步骤的整个表达式 (包括定义) "
      "的当前焦点部分挖去, 换上新鲜变量" $y
      ", 然后我们设这个模板是" $r
      ", 优化所使用的定义里的变量和公式分别为" $x "和" $q
      ", 那么前后两个表达式分别为"
      (MB (&psubst $y $q $r) "和" (&psubst $y $x $r))
      "如果赋值" $v "满足其中任何一个公式, "
      "那么因为定义是合取分量之一, 所以"
      (&= (&eval $x $v) (&eval $q $v))
      ". 由此可知, 根据" (Ref "eval-subst") ", 我们有"
      (MB (&= (&eval (@psubst $y $q $r) $v)
              (&eval (@psubst $y $x $r) $v)))
      "换言之, 若" $v "满足其中一个表达式, "
      "那么必然" $v "也满足另一个表达式. "
      "也就是说, 优化前后的两个表达式是逻辑等价的, "
      "这比等可满足还要强.}")
   (P "我们需要保证我们新引入的原子没有出现在初始公式里. "
      "{译注: 更准确地说, 对于这里的情况, 以及后面可能出现的情况, "
      "新要引入的原子一般要保证和初始公式里的每个原子以及"
      "已经引入的新原子都不同. 许多过程的正确性都依赖于这一点. "
      "就这里的情况而言, freshness是为了保证新原子能够"
      "唯一地标识Tseitin变换的中间步骤的位置, "
      "这里说的中间步骤是包括所有已经引入的定义的.} "
      "这种繁琐的工作在后续还会多次出现, "
      "因此我们现在实现一个更通用的解决方案. "
      (Code "max_varindex") "函数返回参数" (Code "n")
      "和可能的" (Code "m") "中更大的那个, "
      "如果字符串参数" (Code "s") "是" (Code "pfx")
      "后面跟着对应于数字" (Code "m") "的字符串的话:"
      (CodeB "let max_varindex pfx =
  let m = String.length pfx in
  fun s n ->
    let l = String.length s in
    if l &lt;= m or String.sub s 0 m &lt;> pfx then n else
    let s' = String.sub s m (l - m) in
    if forall numeric (explode s') then max_num n (num_of_string s')
    else n;;")
      "{译注: 原文对于" (Code "max_varindex")
      "的描述真是非常绕, 实际上就是看看字符串" (Code "s")
      "是不是恰好为" (Code "pfx") "后面跟着数码的形式. "
      "如果是, 那就试图得到这些数码所表示的数字, "
      "然后比大小. 如果不是这样的形式, 就不用比较了, "
      "直接返回" (Code "n") "即可.}")
   (P "现在我们可以实现整体的函数了. "
      "首先对公式进行化简并将否定下推, 得到" (Code "fm'")
      ". 然后, 我们使用这个公式以选择合适的起始变量索引, "
      "具体做法是看看既有的形式为" (Q (Code "p_n"))
      "的所有变量, 然后给其中最大的" $n "加上一. "
      "然后我们调用主函数, 这里我们将其保留为参数"
      (Code "fn") "以允许未来的修改, "
      "而主函数的参数是从没有定义和之前选取的变量索引开始的. "
      "然后我们以集合之集合表示返回作为结果的CNF:"
      (CodeB "let mk_defcnf fn fm =
  let fm' = nenf fm in
  let n = Int 1 +/ overatoms (max_varindex &quot;p_&quot; ** pname) fm' (Int 0) in
  let (fm'',defs,_) = fn (fm',undefined,n) in
  let deflist = map (snd ** snd) (graph defs) in
  unions(simpcnf fm'' :: map simpcnf deflist);;"))
   (P "我们的第一个定义性CNF函数知识将其应用于" (Code "maincnf")
      ", 然后将结果转换回公式的形式:"
      (CodeB "let defcnf fm = list_conj(map list_disj(mk_defcnf maincnf fm));;"))
   (P "在示例公式上进行测试, 得到了预期的结果, "
      "与上面手工推导得到的结果一致, "
      "只是合取分量的顺序以及每个合取分量内文字的顺序有所不同."
      (CodeB "# defcnf &lt;&lt;(p \\/ (q /\\ ~r)) /\\ s>>;;
- : prop formula =
&lt;&lt;(p \\/ p_1 \\/ ~p_2) /\\
  (p_1 \\/ r \\/ ~q) /\\
  (p_2 \\/ ~p) /\\
  (p_2 \\/ ~p_1) /\\
  (p_2 \\/ ~p_3) /\\
  p_3 /\\
  (p_3 \\/ ~p_2 \\/ ~s) /\\ (q \\/ ~p_1) /\\ (s \\/ ~p_3) /\\ (~p_1 \\/ ~r)>>"))
   (P "与其将每个定义单独转换为CNF, 我们本可以先形成最终的合取式, "
      "然后调用一次旧的CNF函数. 这样编程会稍微简单一些, "
      "并且能消除更多被subsumed的合取分量, "
      "例如这个例子里的" (&disj $p_3 (&neg $p_2) (&neg $s))
      " is subsumed by " $p_3
      ". 然而, 对于非常大的公式, subsumption测试会变得极其缓慢, "
      "因为(在我们的简单实现里)其对于大小为" $n
      "的公式大约要执行" $n^2 "次运算. "
      "{译注: 我感觉可能没有那么糟糕, "
      "因为作者会进行预排序. 对于排序好了的集合, "
      "subsumption测试是线性的. 不过, 排序总是反复进行, "
      "然而我期望作者的实现里对于已经排序好的列表进行排序应该不太耗费代价. "
      "不过, 鉴于作者的实现里每个公式都要对于所有公式进行一次"
      "subsumption测试, 我感觉这种计算量不会很小.}")
   (P "{译注: 以下是个人以Scheme实现的Tseitin变换, 其大致相当于原文"
      (Code "maincnf") "和" (Code "nenf")
      "功能复合, 不过没有实现原文的小优化 "
      "(为了简单和可读性起见):"
      (CodeB "(define (fresh-var)
  (gensym 'p_))
(define (tseitin exp)
  (define (tseitin exp d*)
    (match exp
      ((,op ,e1 ,e2)
       (let*-values (((l1 d*) (tseitin e1 d*))
                     ((l2 d*) (tseitin e2 d*)))
         (let ((x (fresh-var))
               (e `(,op ,l1 ,l2)))
           (values x (cons `(&lt;=> ,x ,e) d*)))))
      (,else (values exp d*))))
  (tseitin (nenf exp) '()))")
      "这里的变量名" (Code "l1") "和" (Code "l2")
      "暗示了内部的" (Code "tseitin")
      "所返回的第一个值是" (Q "文字") ".}")
   (H4. "优化")
   (P "我们可以通过避免一些明显冗余的定义来优化该过程. "
      "首先, 当处理初始公式里的迭代合取时, "
      "我们可以只是将合取分量分别置于CNF形式, "
      "然后将它们连接起来. "
      "{原注: 注意到这里最初要调用的" (Code "nenf")
      "带来了收益, 因为它可以暴露原本隐藏在嵌套否定之下的CNF结构, "
      "例如" (&neg (@disj $p (&conj $q $r)))
      "在这变换之后就已然变成了CNF形式.} "
      "并且, 如果合取分量本身又是析取, "
      "那么我们可以忽略析取分量是文字的情况, "
      "只对于其他子公式引入定义.")
   (P "编码相当简单: "
      "首先我们向下历经任意多层嵌套的合取, "
      "然后再向下历经任意多层嵌套的析取, "
      "在我们开始定义性工作之前. "
      "然而, 我们仍然需要将公式的不同部分的定义性变换链接起来, "
      "于是我们维持了和之前相同的具有三个参数的整体结构. 函数"
      (Code "subcnf") "和" (Code "defstep")
      "的结构大致相同, 除了它只是处理链接记录事务 (linkage housekeeping) "
      "而不引入新的定义, 并且其还有要被递归调用的函数作为额外的参数"
      (Code "sfn") ":"
      (CodeB "let subcnf sfn op (p,q) (fm,defs,n) =
  let fm1,defs1,n1 = sfn(p,defs,n) in
  let fm2,defs2,n2 = sfn(q,defs1,n1) in (op fm1 fm2,defs2,n2);;"))
   (P "首先这用来定义第一个函数, 其递归地向下历经析取, "
      "然后对于析取分量执行定义性变换 "
      "{译注: 根据前文可知, 定义性变换对于文字不会引入新的定义}:"
      (CodeB "let rec orcnf (fm,defs,n as trip) =
  match fm with
    Or(p,q) -> subcnf orcnf mk_or (p,q) trip
  | _ -> maincnf trip;;")
      "当然我们还需要一个函数, 其向下历经合取, "
      "然后对于合取分量调用" (Code "orcnf") ":"
      (CodeB "let rec andcnf (fm,defs,n as trip) =
  match fm with
    And(p,q) -> subcnf andcnf mk_and (p,q) trip
  | _ -> orcnf trip;;"))
   (P "现在整体的函数是相同的, 除了" (Code "andcnf")
      "被用作代替" (Code "maincnf")
      ". 我们将实际从集合之集合表示重构公式单独分离为一个函数, "
      "因为之后拦截中间结果是有用的. "
      "{译注: 意即之后会用到中间结果, 也就是集合之集合表示.}"
      (CodeB "let defcnfs fm = mk_defcnf andcnf fm;;

let defcnf fm = list_conj (map list_disj (defcnfs fm));;")
      "这对于之前我们的运行示例的确给出了远为简单的结果:"
      (CodeB "# defcnf &lt;&lt;(p \\/ (q /\\ ~r)) /\\ s>>;;
- : prop formula =
&lt;&lt;(p \\/ p_1) /\\ (p_1 \\/ r \\/ ~q) /\\ (q \\/ ~p_1) /\\ s /\\ (~p_1 \\/ ~r)>>"))
   (P "如果再仔细一些, 可以设计出一种定义性CNF过程, "
      "使其输出的大小始终至少不逊于朴素算法 (Boy de la Tour 1990). "
      "不过, 我们现在得到的函数" (Code "defcnf")
      "已经相当不错, 完全能够满足我们的需要. "
      "关于一种可能的优化, 参见练习2.11.")
   (P "{译注: 以下是我在Scheme中实现的优化了的定义性CNF过程:"
      (CodeB "(define (tseitin exp)
  (define (lookup e d*)
    (match d*
      (((&lt;=> ,x ,p) . ,d*)
       (if (equal? e p)
           x
           (lookup e d*)))
      (,else #f)))
  (define (Exp step op e1 e2 d*)
    (let*-values (((e1 d*) (step e1 d*))
                  ((e2 d*) (step e2 d*)))
      (values `(,op ,e1 ,e2) d*)))
  (define (CNF exp d*)
    (match exp
      ((and ,e1 ,e2) (Exp CNF 'and e1 e2 d*))
      (,else (Conjunct exp d*))))
  (define (Conjunct exp d*)
    (match exp
      ((or ,e1 ,e2) (Exp Conjunct 'or e1 e2 d*))
      (,else (Literal exp d*))))
  (define (Literal exp d*)
    (match exp
      ((,op ,e1 ,e2)
       (let-values (((e d*) (Exp Literal op e1 e2 d*)))
         (define x (lookup e d*))
         (if x
             (values x d*)
             (let ((x (fresh-var)))
               (values x (cons `(&lt;=> ,x ,e) d*))))))
      (,else (values exp d*))))
  (CNF (nenf exp) '()))")
      "这里的变量命名遵循我个人的编程习惯. "
      (Code "CNF") "代表返回的表达式具有CNF形式, "
      (Code "Conjunct") "代表返回的表达式具有CNF的合取分量形式, "
      "而其本身应该是文字的析取. "
      (Code "Literal") "代表返回的表达式是一个文字. "
      "上述陈述存在一个漏洞, "
      "实际上逻辑常量需要谨慎单独对待. "
      "不过, 就这里的情形而言, "
      "逻辑常量不会造成任何问题, 所以我也没有特意处理. "
      "但是读者必须记住, 这是例外情况, 必须总是关心.}")
   (H4. "3-CNF")
   (P "注意到在之前未经优化的定义性CNF转换之后, "
      "作为结果的公式具有" (Q "3-CNF")
      "形式, 也就是说每个合取分量"
      (Em "至多拥有三个")
      "文字. 读者可以通过确认以下事实来验证这一点: "
      "对于每个联结词" (Q $o*) "而言, 定义" (&<=> $p (&o* $q $r))
      "在CNF转换下得到结果里每个合取分量至多只能拥有三个文字. "
      "{译注: 记号" (&<=> $p (&o* $q $r)) "里, "
      $p "是原子的元变量, " $q "和" $r
      "是文字的元变量, " $o* "只能是" $conj
      ", " $disj ", " $<=> "里面选一个.} "
      "然而, 最后的优化破坏了这一性质, "
      "因为它选择保留了已然是CNF的结构. "
      "如果3-CNF被认为是重要的, "
      "可以在仍然分别处理各个合取分量的同时恢复这一性质. "
      "一种粗糙但够用的方法是简单地省略中间函数" (Code "orcnf")
      " {译注: 这种方法相当于令每个合取分量只有一个文字}:"
      (CodeB "let rec andcnf3 (fm,defs,n as trip) =
  match fm with
    And(p,q) -> subcnf andcnf3 mk_and (p,q) trip
  | _ -> maincnf trip;;

let defcnf3 fm = list_conj (map list_disj(mk_defcnf andcnf3 fm));;")
      "{译注: 这里的代码作者搞错了, 可能是之前的版本, "
      "我已根据官网代码进行修正.}")
   (P "本节的结果表明, 我们可以将SAT问题" --
      "即测试任意公式的可满足性" --
      "归约为测试一个仅大数倍的CNF公式的可满足性. "
      "事实上, 根据上述内容, 我们只需要能够测试" (Q "3-SAT")
      ", 即3-CNF公式的可满足性. "
      "正因如此, 许多实用算法假设输入为CNF, "
      "而理论结果也往往只考虑CNF或3-CNF公式.")
   (H3. "Davis-Putnam过程")
   (P "Davis-Putnam过程是一种用于判定"
      "具有合取范式的命题公式的可满足性的方法. "
      "实际上有两种显著不同的算法都被常称为" (Q "Davis-Putnam")
      ", 但我们将分别讨论它们, 并尽量在术语上加以区分. "
      "Davis和Putnam (1960) 提出的原始算法将简称为"
      (Q "Davis-Putnam") " (DP), 而由Davis, Logemann和Loveland (1962) "
      "发展出的后来更为流行的变体将被称为"
      (Q "Davis-Putnam-Loveland-Logemann")
      " (DPLL). 按照历史顺序, 我们首先考虑DP.")
   (P "我们发现" (Q "集合的集合") "表示法在将公式转换为CNF时非常有用, "
      "我们也将在DP和DPLL过程中使用这种表示法. CNF公式的隐式"
      (Q "集合的集合") "表示通常被称为子句形式, "
      "每个合取分量被称为一个子句. 前面的辅助函数" (Code "simpcnf")
      "已经将公式置于子句形式, 而" (Code "defcnfs")
      "做了类似的事情, 只不过使用的是定义性CNF. 我们将直接使用后者, "
      "避免从集合的集合表示重新构造公式的最后一步. "
      "在我们的讨论中, 我们书写子句的时候仍会包括隐式的逻辑联结词, "
      "但应理解我们实际上执行的是集合操作.")
   (P "退化情形应该铭记在心: 包含空子句的列表对应于公式"
      (Q $bottom) ", 而子句的空列表则对应于" (Q $top)
      "; 这种解释在后文中经常使用. "
      "DP过程通过一系列其他的过程相继对于子句形式的公式进行变换, "
      "保持子句形式以及其与原本公式的等可满足性. "
      "这个过程在子句形式包含空子句 (这种情况下原本的公式必然是不可满足的) "
      "或者子句形式本身为空 (这种情况下原本的公式必然是可满足的) "
      "时会终止. 以下是DP过程所使用的三种基本的可满足性保持变换:"
      (Ul (Li "(I) 1-文字规则,")
          (Li "(II) 肯定否定规则,")
          (Li "(III) 消除原子公式的规则.")))
   (P "规则I和II总是使公式变得更简单, 减少文字的总数. "
      "因此它们总是被尽可能多地应用, 而第三条规则" --
      "它可能大幅增加公式的大小" --
      "只在前两条规则都不适用时才使用. "
      "然而, 从逻辑的角度来看, 我们可以将I视为III的特殊情形, "
      "因此我们将复用III保持可满足性的论证来证明I也具有同样的性质.")
   (H4. "1-文字规则")
   (P "这条规则适用于某个子句是" (Em "单元子句(unit clause)")
      "的情形, 也就是说, 该子句只是一个单独的文字, "
      "而非超过一个文字的析取. 如果" $p "是这样一个单元子句, "
      "那么我们可以通过以下方法得到一个新的公式:"
      (Ul (Li "移除其他子句里的所有" (&- $p) "实例,")
          (Li "移除所有包含" $p "的子句, 包含该单元子句本身.")))
   (P "之后我们将会证明这种变换保持可满足性. "
      "1-文字规则本身也被称为" (Em "单元传播")
      ", 因为它将" $p "为真的信息传播至其他子句里. "
      "为了以列表之列表的表示实现这一过程, "
      "我们寻找一个单元子句, 即长度为一的列表, "
      "然后令" (Code "u") "是其中单独的文字, 而"
      (Code "u'") "是其否定. 然后, 我们首先移除所有包含"
      (Code "u") "的子句, 然后从剩余的子句里移除"
      (Code "u'") "."
      (CodeB "let one_literal_rule clauses =
  let u = hd (find (fun cl -> length cl = 1) clauses) in
  let u' = negate u in
  let clauses1 = filter (fun cl -> not (mem u cl)) clauses in
  image (fun cl -> subtract cl [u']) clauses1;;"))
   (P "如果不存在单元子句, " (Code "find")
      "的应用会抛出异常. 这使得我们可以很方便地反复应用"
      (Code "one_literal_rule")
      "来消除多个单元子句, 直到失败表明已经没有更多的单元子句为止. "
      "注意, 即使初始公式中只有一个单元子句, "
      "规则的应用也可能通过删除其他文字而产生新的单元子句.")
   (H4. "肯定否定规则")
   (P "这个规则有时也被称为" (Em "纯文字")
      "规则, 其利用了某些文字" (Em "仅肯定出现")
      "或者" (Em "仅否定出现") "的事实, "
      "然后我们可以删除所有包含这种文字的子句, "
      "同时仍能保持可满足性. "
      "对于实现而言, 我们从将所有文字收集在一起开始, "
      "然后将其划分为肯定 (" (Code "pos")
      ") 和否定 (" (Code "neg'")
      "). 从这些值我们得到" (Code "pure")
      ", 其包含了所有只肯定出现或者只否定出现的文字, "
      "然后据此我们消除所有含有其中任意文字的子句. "
      "如果没有纯文字了, 我们会选择失败, "
      "这样更适合整体的过程."
      (CodeB "let affirmative_negative_rule clauses =
  let neg',pos = partition negative (unions clauses) in
  let neg = image negate neg' in
  let pos_only = subtract pos neg and neg_only = subtract neg pos in
  let pure = union pos_only (image negate neg_only) in
  if pure = [] then failwith &quot;affirmative_negative_rule&quot; else
  filter (fun cl -> intersect cl pure = []) clauses;;"))
   (P "如果任意某个赋值可以满足原本的子句集合, "
      "那么其也必然满足新的集合, 因为新的集合是本来的集合的子集. "
      "反过来, 如果某个赋值" $v "满足新的集合, "
      "那么我们对于所有仅肯定文字" $p "置"
      (&= (app $v^ $p) $true)
      ", 对于所有仅否定文字" (&neg $n) "置"
      (&= (app $v^ $n) $false)
      ", 对于其他所有原子则置"
      (&= (app $v^ $a) (app $v $a))
      ". 根据构造方式, 其会满足被删除的子句, "
      "并且既然" $v^ "不会改变" $v
      "对于所有出现在新集合的子句里的原子的指派, "
      "其当然也满足这些没有被删除的子句, 故"
      $v^ "能够满足原本的子句集合.")
   (H4. "消除原子公式的规则")
   (P "这条规则是唯一一条可能使公式规模增大的规则, "
      "而且在最坏情况下, 这种增大可能是相当可观的. "
      "然而, 它能够彻底消除某个特定原子, "
      "而不需要对包含该原子的子句施加任何特殊要求. "
      "该规则以一个文字" $p "为参数, 要求" $p
      "在至少一个子句中肯定出现, "
      "并且在至少一个子句中否定出现. "
      "(如果纯文字规则已经被应用过, "
      "那么剩余的任何文字都满足这一性质. "
      "事实上, 如果我们还过滤掉了平凡的, 即重言的子句, "
      "那么没有任何文字会在同一个子句中既肯定出现又否定出现, "
      "但我们在陈述和证明下一个定理时不会依赖这一点.)")
   ((Theorem)
    "给定一个文字" $p ", 将子句集合" $S
    "划分为三类: 仅以正面方式包含" $p ", 仅以反面方式包含"
    $p ", 以及两者都不为真的:"
    (MB (&= $S (&union (setI (&disj $p $C_i)
                             (&<= $1 $i $m))
                       (setI (&disj (&- $p) $D_j)
                             (&<= $1 $j $n))
                       $S_0)))
    "其中" $C_i "和" $D_j "中没有任何一个应该包含文字" $p
    "或其否定, 并且如果" $p "或者" (&- $p)
    "出现在" $S_0 "的任意子句里, 那么两者都应该出现. 然后, "
    $S "是可满足的当且仅当" $S^ "是可满足的, 其中:"
    (MB (&= $S^ (&union (setI (&disj $C_i $D_j)
                              (&cm (&<= $1 $i $m)
                                   (&<= $1 $j $n)))
                        $S_0)) ".")
    "{译注: 仅以正面方式包含" $p
    ", 换成更容易理解的方式, "
    "其实就是包含" $p "但不包含" (&- $p)
    ". 仅以反面方式包含" $p ", 其实就是包含" (&- $p)
    "但不包含" $p ".}")
   ((proof)
    "不失一般性, 我们可以假定" $p "为肯定文字, "
    "即一个原子公式, 否则的话相同的论证也可以适用于"
    (&- $p) "." (Br)
    "如果一个赋值" $v "满足" $S ", 那么存在两种可能性. 如果"
    (&= (app $v $p) $false) ", 既然每个" (&disj $p $C_i)
    "都得到满足而" $p "没有被满足, 那么每个" $C_i
    "都得到了满足, 那么每个" (&disj $C_i $D_j)
    "就更被满足了. 如果" (&= (app $v $p) $true)
    ", 既然每个" (&disj (&- $p) $D_j) "都得到满足而"
    (&- $p) "没有被满足, 那么每个" $D_j
    "都得到了满足, 因而" (&disj $C_i $D_j)
    "亦是如此. " $S_0 "里的公式已经在原本的" $S
    "里了, 所以说仍然被" $v "所满足." (Br)
    "反过来, 设一个赋值" $v "满足" $S^
    ". 我们声明, " $v "要么满足所有的" $C_i
    ", 要么满足所有的" $D_j
    ". 诚然如此, 若该赋值没有满足某个特定的" $C_k
    ", 那么然而赋值能够对于" (&<= $1 $j $n)
    "满足所有的" (&disj $C_k $D_j)
    "这一事实立即表明了这个赋值满足所有的"
    $D_j "; 类似地, 如果该赋值不能满足某个"
    $D_l ", 那么其必然满足所有的" $C_i
    ". 现在, 若" $v "满足所有的" $C_i
    ", 那么我们置" (&= (app $v^ $p) $false)
    "而对于其他所有原子置"
    (&= (app $v^ $a) (app $v $a))
    ". 所有的" (&disj $p $C_i) "都被"
    $v^ "满足, 因为所有的" $C_i
    "都被" $v^ "满足. 所有的"
    (&disj (&- $p) $D_j)
    "也都被" $v^ "满足, 因为"
    (&- $p) "被" $v^ "满足. "
    "既然" $S_0 "的公式要么不牵涉" $p
    ", 要么就是重言, 所以它们仍然被" $v^
    "所满足. 另一情形全然是对称的: "
    "如果" $v "满足所有的" $D_j
    ", 那么置" (&= (app $v^ $p) $true)
    ", 推理是类似的. {译注: 原文是"
    (&= (app $v $p) $true)
    ", 可能是一个笔误.}")
   (P "规则III也常被称为" (Em "归结(resolution)")
      "规则, 我们将会在第3章里对其进行详细研究. "
      "相应地, 子句" (&disj $C_i $D_j)
      "被称为是子句" (&disj $p $C_i) "和"
      (&disj (&- $p) $D_j) "的一个" (Em "resolvent")
      ", 并且我们说它是由" (Em "归结")
      "得到的, 或者更准确地说是" (Em $p "上的归结")
      ". 在实现里, 我们也在最后把平凡 (或者说重言) "
      "的子句过滤掉:"
      (CodeB "let resolve_on p clauses =
  let p' = negate p and pos,notpos = partition (mem p) clauses in
  let neg,other = partition (mem p') notpos in
  let pos' = image (filter (fun l -> l &lt;> p)) pos
  and neg' = image (filter (fun l -> l &lt;> p')) neg in
  let res0 = allpairs union pos' neg' in
  union other (filter (non trivial) res0);;")
      "{译注: 这里的输入实际上已经默认了, 或者说要求, "
      "所有的平凡子句已被去除. 否则的话, 使用"
      (Code "(mem p)") "进行筛选并不正确. "
      "另外, 最后的过滤是因为归结可能产生新的平凡子句, "
      "所以说从维护这一性质的角度考虑, "
      "过滤不是可选的而是必要的.}")
   (P "从理论上说, 我们可以将1-文字规则应用于单元子句"
      $p "视为先利用subsumption化简, 然后再施行" $p
      "上的归结. 这就推出了之前我们说要证明的东西:")
   ((Corollary)
    "1-文字规则保持可满足性.")
   ((proof)
    "如果原本的集合" $S "包含单元子句" (setE $p)
    ", 那么根据subsumption规则, 集合" $S
    "里所有其他牵涉" $p
    "的子句都可以被移除而不改变可满足性, "
    "设这个操作给出了" $S^
    ". {译注: subsumption实际上保持逻辑等价性.} "
    "现在根据上述定理, 由" $p
    "上的归结所得到的新集合是等可满足的, "
    "而这个操作相当于移除了这个单元子句本身, "
    "以及" (&- $p) "的所有实例.")
   (P "{译注: 如果严格遵循定义, "
      "那么这里存在一个微妙的问题. "
      $S^ "中可能并不含有" (&- $p)
      "这个文字, 此时并不匹配归结规则的条件. "
      "不过, 我认为原文对于归结适用条件的表述"
      "还有其他作者未预料到的问题, 即其应该是"
      $p "至少在一个子句里以单独正面形式出现, "
      "又至少在一个子句里以单独反面形式出现. "
      "不然的话, 一个关于" $p
      "的平凡子句也能满足条件, "
      "但这恐怕是作者想要排除的情况. "
      "回到正题, 对于慵懒的读者, "
      "他可以认为没有" (&- $p)
      "相当于" $p "是纯文字, "
      "所以可以运用肯定否定规则. "
      "(当然, 这和一次性想要消除所有纯文字的代码并不一致, "
      "不过显然也完全是正确的.) "
      "对于积极的读者, 他可能会想要思考若"
      $C_i "和" $D_j "有一方不存在会发生什么. "
      "藉由肯定否定规则, 我们可以先去除平凡子句, "
      "然后去除纯文字, 然后再把平凡子句加回来. "
      "不过, 这看起来总是有点搞笑. "
      "读者也可以稍微细致地考察关于归结规则的证明, "
      "然后会发现其实它也适用于有一方缺席的情形. "
      "甚至, 恐怕两方都缺席也是对的. "
      "我不知道作者是不是已然预料到了这一情况, "
      "毕竟定理本身的表述里没有对于" $p "进行限制.}")
   (P "在实践中, 我们只会在应用1-文字规则和肯定否定规则之后应用归结规则. "
      "在这种情况下, 我们可以假定" (Em "任何")
      "还在的文字既有正面出现又有反面出现, "
      "并且我们面临着要挑哪一个文字进行归结的选择. "
      "给定一个文字" $l ", 我们可以预测由" $l
      "上的归结所导致的子句数目变化:"
      (CodeB "let resolution_blowup cls l =
  let m = length(filter (mem l) cls)
  and n = length(filter (mem (negate l)) cls) in
  m * n - m - n;;")
      "{译注: 其实肯定否定规则 (纯文字规则) "
      "可以视为依次对于每个纯文字进行归结. "
      "注意, 我这里说的其实" (B "并不符合归结的定义")
      ", 但是可以从形式上理解.}")
   (P "我们将会挑选最小化这个膨胀的文字. "
      "(尽管这看起来很有说服力, 但实际上是过分简单化了的; "
      "远为复杂的启发式是可能的, 并且或许也是更好的.)"
      (CodeB "let resolution_rule clauses =
  let pvs = filter positive (unions clauses) in
  let p = minimize (resolution_blowup clauses) pvs in
  resolve_on p clauses;;"))
   (H4. "DP过程")
   (P "DP主过程是递归定义的. 如果子句集合为空 (返回"
      (Code "true") ", 因为该集合是平凡可满足的), "
      "或者子句集合包含了一个空子句 (返回"
      (Code "false") ", 因为不可满足性), "
      "那么这个过程就会终止. 否则的话, "
      "它会依次试图不断应用规则I, II, III, "
      "然后在新的子句集合上递归地继续. "
      "这个递归过程必然会终止, "
      "因为每条规则要么会降低不同原子的数目 "
      "(在III的情形下, 我们假定了平凡子句总是会事先去除), "
      "要么保持原子数目不变但降低了子句的总大小. "
      "{译注: 关于规则III, 在之前的译注里我们已有所讨论. "
      "另外, 这里的论述并不正确. "
      "实际上, 每条规则在适用时都必然能够减少不同原子的数目. "
      "这才是递归终止的根本保证.}"
      (CodeB "let rec dp clauses =
  if clauses = [] then true else if mem [] clauses then false else
  try dp (one_literal_rule clauses) with Failure _ ->
  try dp (affirmative_negative_rule clauses) with Failure _ ->
  dp(resolution_rule clauses);;")
      "{译注: 这个过程经过我的思考, 发现并不像看上去那么简单. "
      "我们需要厘清这三条规则之中哪一个可以为哪一个创造条件, "
      "而实际上最终我发现厘清的过程异常地微妙. "
      "单文字规则可以为肯定否定规则创造条件, "
      "肯定否定规则是应用归结规则的前提. "
      "(不过, 即便没有肯定否定规则, "
      "归结规则在形式上仍然正确. "
      "如果将其应用于某个纯文字, "
      "实质上相当于消除了含有该纯文字的子句.) "
      "归结规则因为要消除新生成的平凡子句, "
      "所以可以为肯定否定规则创造条件. "
      "但是, 最微妙的情况是其实归结规则可能为单文字规则创造条件. "
      "乍看上去, 似乎归结规则生成的子句的大小都大于等于二, "
      "但是实际上因为可能出现重复文字, "
      "所以说单文字子句也是可能因为归结而出现的. "
      "另一个同等微妙的情况是, 肯定否定规则应用之后, "
      "肯定否定规则仍然可能适用, 例如"
      (MB (setE (setE $p (&neg $r))
                (setE $p $r)
                (setE $q $r)
                (setE (&neg $q) $r)))
      "在第一轮应用后会得到"
      (MB (setE (setE $q $r)
                (setE (&neg $q) $r)))
      "然而, 即便我们之前说过在纯文字上进行归结"
      "在形式上和肯定否定规则是等效的, "
      "就归结规则的这里的实际实现而言, "
      "迭代应用肯定否定规则直至不能再应用却仍然是必要的. "
      "这是因为归结规则假定了其输入公式里"
      "所有原子必有作为肯定和否定文字出现的版本. "
      "这是可以证明的性质, "
      "而且它还依赖于输入公式里起码至少要有一个原子, "
      "但实际上可以证明此时公式至少有两个不同的原子. "
      "唯一稍显冗余的只是应用肯定否定规则之后, "
      "单文字规则是不可能适用的. "
      "或许最后我还想强调一下, 这部分内容非常微妙, "
      "我实际上反复修改了无数遍 (大于等于五遍), "
      "因为每次都发现我的陈述存在想当然的漏洞.}")
   (P "代码可以用作进行可满足性和重言检查:"
      (CodeB "let dpsat fm = dp(defcnfs fm);;

let dptaut fm = not(dpsat(Not fm));;")
      "令人振奋的是, " (Code "dptaut") "证明公式"
      (Code "prime 11") "要比" (Code "tautology")
      "函数快得多:"
      (CodeB "# tautology(prime 11);;
- : bool = true
# dptaut(prime 11);;
- : bool = true"))
   (P "{译注: 以下是Scheme版本的DP过程:"
      (CodeB "(define (dp clauses)
  (cond ((null? clauses) #t)
        ((member '() clauses) #f)
        (else
         (let ((new (one-literal-rule clauses)))
           (if new (dp new)
               (let ((new (affirmative-negative-rule clauses)))
                 (if new (dp new)
                     (dp (resolution-rule clauses)))))))))")
      "这和原文的OCaml版本基本一致, "
      "除了失败是通过返回" (Code "#f")
      "而不是抛出异常进行通知的. "
      "本来我设想了一些优化, "
      "最后还是发现原文的写法最好! "
      "这是因为, 最终我发现这些优化绝大多数都存在漏洞. "
      "原文的代码风格相当偏向于正确性的易于证明, "
      "不仅是这里, 其他地方也是如此.}")
   (H4. "DPLL过程")
   (P "对于更具挑战性的问题, DP过程中生成的子句数量和规模可能会急剧增长, "
      "并可能在得出结论之前耗尽可用内存. "
      "在DP算法开发时期的早期计算机上, 这一问题尤为突出, "
      "这促使Davis, Logemann和Loveland (1962) 用一种"
      (Em "分裂规则") "来取代归结规则III. "
      "如果规则I和规则II均不适用, 则选取某个文字" $p
      ", 那么一个子句集合" Δ "的可满足性可以被归约为"
      (&union Δ (setE (&- $p))) "和" (&union Δ (setE $p))
      "的可满足性, 这可以分别测试. "
      "注意到这保持可满足性: " Δ "是可满足的当且仅当"
      (&union Δ (setE (&- $p))) "和" (&union Δ (setE $p))
      "中至少有一个是可满足的, 因为任何赋值必然满足"
      $p "和" (&- $p) "其中之一. "
      "{译注: 注意到这句话里的" $p "和" (&- $p)
      "所表示的其实是单元子句而非文字. "
      "似乎本书只有一处用了" (setE $p)
      "记号来表示单元子句, 其他时候都是使用" $p "记号.} "
      "新加的单元子句可以立即被1-文字规则利用起来以简化子句集合. "
      "因为这一步约简了(不同)原子的数目, "
      "所以说过程的终止性仍然能得到保证.")
   (P "一种对于分裂文字的合理选择方式似乎是挑选最频繁出现的文字 "
      "(不论以肯定方式还是以否定方式), "
      "这样的话之后的单元传播过程可以产生最大程度的化简. "
      "{原注: 实际上精确地说, 分裂变量的最优选择比"
      "解决可满足性问题本身还要困难 (Liberatore 2000).} "
      "据此想法, 我们可以定义一个类似于DP过程的"
      (Code "resolution_blowup") "的函数:"
      (CodeB "let posneg_count cls l =
  let m = length(filter (mem l) cls)
  and n = length(filter (mem (negate l)) cls) in
  m + n;;"))
   (P "现在的算法基本上和之前是如出一辙的, "
      "只不过归结规则被换成了分裂规则 (case-split):"
      (CodeB "let rec dpll clauses =
  if clauses = [] then true else if mem [] clauses then false else
  try dpll(one_literal_rule clauses) with Failure _ ->
  try dpll(affirmative_negative_rule clauses) with Failure _ ->
  let pvs = filter positive (unions clauses) in
  let p = maximize (posneg_count clauses) pvs in
  dpll (insert [p] clauses) or dpll (insert [negate p] clauses);;")
      "又一次, 代码可以用作进行可满足性和重言检查:"
      (CodeB "let dpllsat fm = dpll(defcnfs fm);;

let dplltaut fm = not(dpllsat(Not fm));;")
      "并且此时对于相同的例子而言, DPLL过程比DP过程甚至更好:"
      (CodeB "# dplltaut(prime 11);;
- : bool = true"))
   (P "{译注: 以下是Scheme版本的DPLL过程:"
      (CodeB "(define (dpll clauses)
  (cond ((null? clauses) #t)
        ((member '() clauses) #f)
        (else
         (let ((new (one-literal-rule clauses)))
           (if new (dpll new)
               (let ((new (affirmative-negative-rule clauses)))
                 (if new (dpll new)
                     (splitting-rule clauses))))))))
(define (splitting-rule clauses)
  (define p*
    (filter positive? (apply U* clauses)))
  (define p
    (maximize (posneg-count clauses) p*))
  (or (dpll (cons (list p) clauses))
      (dpll (cons (list (negate p)) clauses))))"))
   (H4. "迭代DPLL")
   (P "对于规模非常大的问题, "
      "我们已经呈现了的简单递归形式的DPLL过程"
      "可能需要不切实际大小的内存, "
      "这是由于分裂嵌套时中间状态的存储. "
      "大多数现代实现转而采用尾递归 (迭代) 的控制结构, "
      "使用显式的" (Em "踪迹(trail)") "来保存关于递归分裂的信息. "
      "我们将这个踪迹就实现为序对的列表, "
      "每个序对的第一个成员是我们正在假定的文字, "
      "第二个则是一个标志 (flag), "
      "其指明了这个文字到底是仅作为情形分裂的一半而假设的 ("
      (Code "Guessed") "), 还是说它是由之前假设的文字根据单元传播而推出的 ("
      (Code "Deduced") "). 踪迹以逆序存储, "
      "也就是说列表的头部是最近才假设或者推出的文字. "
      "并且, 标志取自以下这个枚举类型:"
      (CodeB "type trailmix = Guessed | Deduced;;"))
   (P "一般来说, 我们在探索情形分裂时不再修改输入问题中的子句, 而是保留原始公式, "
      "仅在踪迹中记录我们进一步的 (通常是临时的) 假设. "
      "踪迹中的所有文字都被视为在当前探索阶段成立. "
      "为了找到可用于情形分裂的潜在原子公式, "
      "我们使用以下方法来标识问题中那些"
      "在踪迹中未有(正或负)指派的原子公式, "
      "无论该文字是猜测得来的还是推导得来的:"
      (CodeB "let unassigned =
  let litabs p = match p with Not q -> q | _ -> p in
  fun cls trail -> subtract (unions(image (image litabs) cls))
                            (image (litabs ** fst) trail);;"))
   (P "为了执行单元传播, 能够" (Em "内部")
      "修改问题子句" (Code "cls") "是方便的, "
      "并且为了更为高效的查找, 我们也会将踪迹"
      (Code "trail") "处理为一个有限部分函数"
      (Code "fn") ". 这都是在下列子函数里实现的, "
      "其执行单元传播, 直至没有进展可以作出:"
      (CodeB "let rec unit_subpropagate (cls,fn,trail) =
  let cls' = map (filter ((not) ** defined fn ** negate)) cls in
  let uu = function [c] when not(defined fn c) -> [c] | _ -> failwith &quot;&quot; in
  let newunits = unions(mapfilter uu cls') in
  if newunits = [] then (cls',fn,trail) else
  let trail' = itlist (fun p t -> (p,Deduced)::t) newunits trail
  and fn' = itlist (fun u -> (u |-> ())) newunits fn in
  unit_subpropagate (cls',fn',trail');;")
      "{译注: 原文还说了或者遇到了空子句, "
      "不过代码并没有检查这一点, 也不需要检查这一点.}")
   (P "然后这用在了整体的函数里, "
      "既返回修改了的子句, 又返回踪迹, "
      "尽管前者只是为了方便而使用的, "
      "并不会在主循环中进行保留:"
      (CodeB "let unit_propagate (cls,trail) =
  let fn = itlist (fun (x,_) -> (x |-> ())) trail undefined in
  let cls',fn',trail' = unit_subpropagate (cls,fn,trail) in cls',trail';;"))
   (P "当我们遇到一个矛盾或者说" (Em "冲突")
      "时, 我们需要进行回溯以最近分裂的另一分支. "
      "这就是" (Em "决策文字(decision literal)")
      " (标记以" (Code "Guessed") "的文字) "
      "和踪迹里的其他文字有所区别的地方了: "
      "我们从踪迹里移除诸项, 直至遇到最近的决策文字, "
      "或者什么也不剩的情况."
      (CodeB "let rec backtrack trail =
  match trail with
    (p,Deduced)::tt -> backtrack tt
  | _ -> trail;;"))
   (P "现在我们要用这种迭代改良重述经典的DPLL算法. "
      (Code "dpli") "的参数是原本问题的子句" (Code "cls")
      ", 其在递归调用的过程中保持不变, "
      "还有一个参数是当前的" (Code "trail")
      ". 首先我们执行穷竭性的单元传播以获得新的子句集合"
      (Code "cls'") "和新的踪迹" (Code "trail'")
      ". (我们没有考虑肯定否定规则, 尽管可以毫不费力地将其加入.) "
      "如果我们推导出了空子句, 那么就回溯至最近的决策文字. "
      "如果没有剩下来的决策文字, 我们就结束了: 该公式不可满足. "
      "否则的话, 我们选取最近的决策文字, 将其否定置于踪迹中, "
      "标记以" (Code "Deduced")
      "以指出其是由之前在踪迹里假定的文字所推出的. "
      "{译注: 实际上的意思就是不用回溯了.} "
      "(从操作角度而言, "
      "这意味着在下一次冲突时不会再次对其取反而陷入循环.) "
      "如果没有冲突, 那么就和递归版本一样, 我们选取一个未指派的文字"
      (Code "p") ", 启动情形分裂, 而若不存在未指派的文字, "
      "那么该公式就是可满足的了."
      (CodeB "let rec dpli cls trail =
  let cls',trail' = unit_propagate (cls,trail) in
  if mem [] cls' then
    match backtrack trail with
      (p,Guessed)::tt -> dpli cls ((negate p,Deduced)::tt)
    | _ -> false
  else
      match unassigned cls trail' with
        [] -> true
      | ps -> let p = maximize (posneg_count cls') ps in
              dpli cls ((p,Guessed)::trail');;"))
   (P "与往常一样, 我们可以将其转化为对任意公式的可满足性检验和重言式检验:"
      (CodeB "let dplisat fm = dpli (defcnfs fm) [];;

let dplitaut fm = not(dplisat(Not fm));;"))
   (P "它的运行效果与递归实现一样好, 尽管通常会慢一些, "
      "原因在于我们朴素的数据结构不支持高效的查找和单元传播. "
      "但当我们考虑进一步的优化时, 迭代结构的优势便真正得以体现.")
   (H4. "回跳和学习")
   (P "对于一个不可满足的子句集合, 在递归情形分裂足够多次之后, "
      "我们总是能得到空子句, 其表明了某种特定的文字指派组合是不一致的. "

      )
   (H3. "Stålmarck方法")
   (H3. "二元决策图")
   (H3. "紧致性")
   (P "我们现在建立命题逻辑的一个关键理论性质, "
      "这一性质在下一章中将被重要地使用, "
      "它涉及一个无穷公式集的可满足性. "
      "回顾一下, 一个命题公式的集合" Γ "被称为是可满足的, "
      "如果存在一个赋值能同时满足其中所有的公式. "
      "紧致性定理陈述如下:")
   (P "{原注: 这个名称源于与点集拓扑学的联系 (Engelking 1989; Kelley 1975). "
      "赋予所有赋值的集合" $BB^NN
      "基于离散拓扑的积拓扑, 其中"
      (&= $BB (setE $false $true))
      ". (这有时被称为Cantor空间.) 对于任意公式" $p
      ", 满足它的赋值集合" $V_p "在这个拓扑中是闭的 (实际上也是开的), "
      "因为每个公式只涉及有限多个命题变量. 由于" $BB
      "是紧致的, 根据Tychonoff定理, " $BB^NN
      "也是紧致的. 根据假设, 来源于集族" (setI $V_p (∈ $p Γ))
      "的所有有限交都是非空的, 因此由紧致性的定义, "
      "它们全部的交也是非空的, 这正是所需要的. "
      "假定选择公理成立, 若将" $NN
      "替换为任意原子集合, Tychonoff定理仍然成立, "
      "从而给出了紧致性定理在一般情形下的证明.}")
   ((Theorem #:id "prop-compactness")
    "对于任意的命题公式集合" Γ
    ", 如果其每个有限子集" (&sube Δ Γ)
    "都是可满足的, 那么" Γ
    "本身也是可满足的.")
   ((proof)

    )
   ((Corollary)
    "如果一个任意的命题公式集合" Γ
    "是不可满足的, 那么存在某个有限子集"
    (&sube Δ Γ) "是不可满足的.")
   ((proof)
    "假设每个有限子集" (&sube Δ Γ)
    "都是可满足的. 根据紧致性定理, " Γ
    "是可满足的, 这与题设矛盾.")
   ((Corollary)

    )
   ((proof)

    )

   (H4. "无限图的着色")
   (H3. "深入阅读")
   (P "关于Boolean代数的一般理论, 包括对Boole原始系统的命题, 集合论及其他解释, "
      "可参见例如Abian (1976), Davey和Priestley (1990) 以及Halmos (1963). "
      "许多逻辑教科书中都有关于Boolean代数的讨论, 如Bell和Slomson (1969), "
      "其中一些我们稍后会因其他技术主题而加以推荐. 最后, "
      "Halmos和Givant (1998) 以现代方式处理逻辑, 但采用了更为明确的代数风格.")
   (P "命题逻辑在许多标准逻辑教材中都有涉及, "
      "例如Church (1956), van Dalen (1994), Enderton (1972), Goodstein (1971), "
      "Hilbert和Ackermann (1950), Hodges (1977), Johnstone (1987), "
      "Kreisel和Krivine (1971), Mates (1972), Quine (1950) 以及Tarski (1941); "
      "其中许多也证明了紧致性定理. "
      "大多数关于自动定理证明的书籍也讨论了命题逻辑和经典判定方法, "
      "如Davis–Putnam方法, 不过往往在命题逻辑上着墨不多, "
      "便转向一阶逻辑 (即我们下一章的内容). Davis, Sigal和Weyuker (1994) "
      "将理论逻辑与自动定理证明相结合, 同时也是一本关于可计算性与复杂性的教科书. "
      "更侧重于自动定理证明的有Bibel (1987), Chang和Lee (1973), Duffy (1991), "
      "Fitting (1990), Loveland (1978), Newborn (2001) 以及"
      "Wos, Overbeek, Lusk和Boyle (1992).")
   (P "回跳和学习最早在DPLL中被应用于SAT求解器"
      "GRASP (Marques-Silva和Sakallah 1996) 和rel_sat (Bayardo和Schrag 1997). "
      "一些较新的基于DPLL的系统, 按大致的开发时间顺序排列, "
      "有SATO (Zhang 1997), Chaff (Moskewicz, Madigan, Zhao, Zhang和Malik 2001), "
      "BerkMin (Goldberg和Novikov 2002) 以及MiniSat (Eén和Sörensson 2003). "
      "描述这些系统的论文是了解基本DPLL算法各版本及巧妙实现技巧的宝贵信息来源. "
      "Nieuwenhuis, Oliveras和Tinelli (2006) 以及Krstić和Goel (2007) "
      "通过非确定性的抽象规则序列来描述迭代DPLL, "
      "使得具体实现可以被视为部署这些规则的方式. "
      "Kroening和Strichman (2008) 也讨论了" (Q "工业级")
      "SAT求解器的架构, 并讨论了命题逻辑的诸多扩展及其在应用中的使用. "
      "其中一些主题将在本书后面讨论, 但有些则不会, 特别是量化Boolean公式 (QBF), "
      "其中公式可以对原子进行量化. (这与下一章描述的一阶逻辑不同, "
      "一阶逻辑中的量化是针对论域中的元素, 而非命题.)")
   (P "我们讨论过的一些主题在通用教科书中尚未被广泛涵盖, "
      "读者必须查阅更专业的专著或研究论文. Stålmarck算法尤其如此, "
      "不过Sheeran和Stålmarck (2000) 对该理论及其成功的实际应用进行了综述. "
      "递归学习的思想 (Kunz和Pradhan 1994) 与Stålmarck方法有重要的共通之处.")
   (P "Bryant (1992) 的综述文章和Kropf (1999) "
      "的教科书讨论了BDD及其在形式化硬件验证自动方法中的作用. "
      "最引人注目的是, 当McMillan等人 (Coudert, Berthet和Madre 1989; "
      "Burch, Clarke, McMillan, Dill和Hwang 1992; Pixley 1990) "
      "将BDD表示与时序逻辑模型检查 (Clarke和Emerson 1981; "
      "Queille和Sifakis 1982) 相结合时, 后者经历了一场小型革命. "
      "关于模型检查的详细介绍, 可参见Clarke, Grumberg和Peled (1999), "
      "以及一些关于计算机科学中的逻辑的书籍, 如Huth和Ryan (1999).")

   (H3. "练习")
   ))
