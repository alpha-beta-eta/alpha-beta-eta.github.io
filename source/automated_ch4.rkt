#lang racket
(provide automated_ch4)
(require (except-in SMathML H3. H4. $!=)
         "automated_utils.rkt")
(define automated_ch4
  (Tm*
   (H2. "相等性")
   (P "到目前为止, 相等性只是被处理为一个普通的二元谓词, 其可以被任意地解释. "
      "然而, 相等性的地位是如此中心性的, 以至于我们往往只想要考虑使得"
      (Q "相等意味着相等") "的解释. 之前的逻辑理论和编程的证明过程"
      "可以通过轻松的修改以支持新的情况, "
      "但是也存在着更为有效和特化的方式用于处理相等性.")
   (H3. "相等性公理")
   (P "在逻辑的诸多应用之中, 特别是对于数学推理的应用, "
      "相等性扮演着一个中心的角色. "
      "我们对此已有所意识, 所以支持了常规的中缀记号"
      (Q (&= $s $t)) "而不是使用" (Q (appl $= $s $t))
      ". 而且, 我们还可以定义各种趁手的句法操作用于"
      "测试一个公式是否是一个等式, "
      "以及创建和分解等式, 例如"
      (CodeB "let is_eq = function (Atom(R(&quot;=&quot;,_))) -> true | _ -> false;;

let mk_eq s t = Atom(R(&quot;=&quot;,[s;t]));;

let dest_eq fm =
  match fm with
    Atom(R(&quot;=&quot;,[s;t])) -> s,t
  | _ -> failwith &quot;dest_eq: not an equation&quot;;;

let lhs eq = fst(dest_eq eq) and rhs eq = snd(dest_eq eq);;"))
   (P "但是, 从逻辑上讲, 相等性至此仅被当作了一个任意的二元谓词进行处理; "
      "我们在对于逻辑有效性问题进行判定时所考虑的解释, "
      "也包括那些对于" (Q $=) "的解释与真正的相等关系大相径庭的情况. "
      "鉴于相等性所具有的中心地位, "
      "研究将模型的类限制于" (Q "相等意味着相等") "的情形是非常自然的, "
      "因为这正是我们在考虑诸如抽象代数之类的领域时心中通常会想到的东西. "
      "我们称一个解释 (或者特定句子集合的模型) 是" (Em "规范的(normal)")
      ", 如果相等性谓词" (Q $=) "被解释为其论域上的相等.")
   (P "任何规范解释都必须满足断言相等是一个等价关系的公式, "
      "即其是自反, 对称, 传递的:"
      (MB (set-attr*
           (&Table
            ((∀ $x (&= $x $x)))
            ((∀ $x $y (&<=> (&= $x $y) (&= $y $x))))
            ((∀ $x $y $z
                (&=> (&conj (&= $x $y) (&= $y $z))
                     (&= $x $z)))))
           'columnalign "left"))
      "并且其还需要满足断言" (Em "congruence")
      "的公式, 对于所考虑的语言的每个"
      $n "元函数(符号)" $f "而言:"
      (MB (∀ $x_1 $..h $x_n
             $y_1 $..h $y_n
             (&=> (&conj (&= $x_1 $y_1) $..c
                         (&= $x_n $y_n))
                  (&= (appl $f $x_1 $..h $x_n)
                      (appl $f $y_1 $..h $y_n)))))
      "对于每个" $n "元谓词(符号)" $R "也是类似的:"
      (MB (∀ $x_1 $..h $x_n
             $y_1 $..h $y_n
             (&=> (&conj (&= $x_1 $y_1) $..c
                         (&= $x_n $y_n))
                  (&=> (appl $R $x_1 $..h $x_n)
                       (appl $R $y_1 $..h $y_n))))))
   (P "对于一个给定的一阶公式集合" Δ ", 我们记"
      (&eqaxioms Δ) " (" (Q Δ "的相等性公理")
      ") 以表示等价关系公式连带着之于出现在"
      Δ "里的所有函数符号" $f "和谓词符号"
      $R "的congruence公式.")
   (P "我们观察到任意的规范解释都满足" (&eqaxioms Δ)
      ", 但是满足" (&eqaxioms Δ) "的解释未必是规范的. "
      "例如, 考虑仅具有两个二元函数符号" (Q $+) "和"
      (Q $d*) "以及两个常量" $0 "和" $1
      "的语言. 如果我们将所有这些符号以" $ZZ
      "中的通常方式进行解释, 但是却把相等性解释为关系"
      (mod= $x $y $2) ", 那么相等性公理仍然都得到满足, "
      "即便这种解释不是规范的. "
      "实际上, 并不存在公式集合能够限制其模型为规范的. "
      "这是因为, 对于任意的规范模型, "
      "我们总能创造一个非规范模型, "
      "通过挑选论域里的某个" $a
      ", 然后给论域添加任意多个额外元素" (∈ $b_i $B)
      ", 并将所有的" $b_i "以和" $a
      "相同的方式进行解释. 尽管如此, "
      "我们仍然有以下的关键结果.")
   ((Theorem)
    "任意的一阶公式集合" Δ
    "拥有一个规范模型当且仅当集合"
    (&union Δ (&eqaxioms Δ))
    "拥有一个模型.")
   ((proof)
    "其中一个方向是简单的: 如果" $M
    "是" Δ "的一个规范模型, 那么显然"
    (&eqaxioms Δ) "在其中成立; "
    "因此, 对于" Δ "的任意规范模型, "
    (&union Δ (&eqaxioms Δ))
    "也在其中成立." (Br)
    "反过来, 设" (&union Δ (&eqaxioms Δ))
    "拥有一个模型" $M ". 在" $M "的论域" $D
    "上通过置" (&~ $a $b) "恰当" (appl $=_M $a $b)
    "来定义一个关系" (Q $~)
    ", 即这个关系是基于解释" $=_M "的" (Q "相等")
    ". 因为等价公理在" $M
    "下成立, 所以这是一个等价关系. "
    "于是, 我们可以将" $D
    "划分为等价类, 其中每个"
    (∈ $a $D) "属于等价类:"
    (MB (&= (equivc $a) (setI (∈ $b $D) (&~ $b $a))))
    "并且" (&= (equivc $a) (equivc $b))
    "当且仅当" (&~ $a $b)
    ". 我们将会使用等价类的集合"
    (&= $D^ (setI (equivc $a) (∈ $a $D)))
    "作为新模型" $M^ "的论域, "
    "并且按照如下方式解释每个" $n
    "元函数符号" $f ":"
    (MB (&= (appl (_ $f $M^) (equivc $a_1) $..h (equivc $a_n))
            (equivc (appl $f_M $a_1 $..h $a_n))) ".")
    "注意到这个是良定义的, 即独立于每个等价类的特定代表元, "
    "因为如果对于" (&= $i (&cm $1 $..h $n)) "有"
    (&~ (_prime $a $i) $a_i) ", 我们也有"
    (&~ (appl $f_M (_prime $a $1) $..h (_prime $a $n))
        (appl $f_M $a_1 $..h $a_n))
    ", 这恰是由于函数congruence公理在" $M
    "下成立. 类似地, 我们以"
    (MB (&= (appl (_ $R $M^) (equivc $a_1) $..h (equivc $a_n))
            (appl $R_M $a_1 $..h $a_n)))
    "来解释每个" $n "元谓词符号" $R
    ". 当然, 这也是独立于等价类代表元的特定选择的, "
    "因为谓词congruence公理在" $M "下成立." (Br)
    "特别地, 我们有" (appl $=_M^ (equivc $a) (equivc $b))
    "恰当" (&~ $a $b) ", 于是恰当"
    (&= (equivc $a) (equivc $b))
    ". 因此, " $M^ "是一个规范解释. "
    "为了看出来为什么它满足" Δ "中的所有公式, "
    "我们本质上需要表明我们可以将等价类构成运算"
    (Q "提升") "至公式的语义层面. 首先, 注意到:"
    (MB (&= (&termval $M^ $delta^ $t)
            (equivc (&termval $M $delta $t))))
    "其中对于每个变量" $x "有"
    (&= (app $delta^ $x) (equivc (app $delta $x)))
    ". 为了证明这一点, 只需要施行" $t
    "上的结构归纳. 如果" $t "是变量" $x
    ", 那么我们有"
    (MB (deriv^
         (&termval $M^ $delta^ $x)
         (app $delta^ $x)
         (equivc (app $delta $x))
         (equivc (&termval $M $delta $x))))
    "而若" (&= $t (appl $f $s_1 $..h $s_n))
    ", 那么使用归纳假设和" (_ $f $M^)
    "的定义, 我们得到:"
    (MB (deriv^
         (&termval $M^ $delta^ (appl $f $s_1 $..h $s_n))
         (appl (_ $f $M^) (&termval $M^ $delta^ $s_1) $..h
               (&termval $M^ $delta^ $s_n))
         (appl (_ $f $M^) (equivc (&termval $M $delta $s_1))
               $..h (equivc (&termval $M $delta $s_n)))
         (equivc (appl $f_M (&termval $M $delta $s_1) $..h
                       (&termval $M $delta $s_n)))
         (equivc (&termval $M $delta (appl $f $s_1 $..h $s_n)))))
    "现在我们声称对于任意的公式" $p ", 我们都有"
    (&= (&holds $M^ $delta^ $p)
        (&holds $M $delta $p))
    ". 又一次, 证明是通过结构归纳进行的. 若" $p
    "为" $bottom "或" $top ", 证明是平凡的. 而若"
    $p "是一个原子公式, 这个结果根据" (_ $R $M^)
    "的定义而成立. 命题运算显然保持这个性质, "
    "剩下量化了的公式作为有趣的情形. 注意到:"
    (MB (deriv^
         (&holds $M^ $delta^ (@∀ $x $p))
         (对于所有的
          (∈ $A $D^)
          (&holds $M^ (@ext $delta^ $x $A) $p))
         (对于所有的
          (∈ $a $D)
          (&holds $M^ (@ext $delta^ $x (equivc $a)) $p))
         (对于所有的
          (∈ $a $D)
          (&holds $M^ (&prime (@ext $delta $x $a)) $p))
         (对于所有的
          (∈ $a $D)
          (&holds $M (@ext $delta $x $a) $p))
         (&holds $M $delta (@∀ $x $p))))
    "{译注: 本书之所以不在这里直接使用数学符号" (Q $forall)
    "而是使用文字" (Q "对于所有的 (for all)")
    ", 可能是因为想要避免因为" $forall
    "在这里具有多重用途 (一个句法, 一个语义) 而引起混淆. "
    "不过, 似乎直接使用符号是更多逻辑学书籍的选择.} "
    "论证对于存在量词也是类似的. 因此, 既然每个"
    (∈ $p Δ) "在" $M "下对于所有的赋值" $delta
    "成立, 那么它也在" $M^ "下对于所有的赋值" $epsilonv
    "成立, 因为" $epsilonv "必然对于某个" $M
    "中的赋值" $delta "具有形式" $delta^
    " (只需令" (app $delta $x) "为"
    (app $epsilonv $x) "的任何一个成员).")
   (P "{译注: 这个论证其实有一个问题, 就是"
      (&eqaxioms Δ) "实际上只会限制对于出现在"
      Δ "里的函数符号和谓词符号以及相等符号" $=
      "的解释. 因此, 论证里的商构造只能对于这些符号进行. "
      "当然了, 因为" Δ "里压根没有出现这些符号, "
      "所以随便什么解释都是成立的. "
      "但是, 我们必须要注意, "
      "原文的结构归纳所论证的东西并不是"
      "对于任意的项和任意的公式成立的, "
      "项只能包含" Δ "既有的函数符号, "
      "公式只能包含" Δ "既有的谓词符号以及相等符号. "
      "另外, 我觉得原文其实有一个细节"
      "稍微值得向读者说明一下, 就是"
      (&eqaxioms Δ) "在其中存在二元谓词" (Q $=)
      "时要不要包括对于" $= "的congruence公式. "
      "实际上, 不论选择包括还是不包括都可以, "
      "因为表达等价性的公式蕴涵" $=
      "的congruence公式.}")
   (P "在我们的实际应用里, 我们只会关心单个公式. 定义"
      (&eqaxiom $p) "为相等性公理" (&eqaxioms (setE $p))
      "里的所有公式的合取 (这些公式的数目必然是有限的), 那么:")
   ((Corollary)
    "任意的公式" $p "可被一个规范模型所满足当且仅当"
    (&conj $p (&eqaxiom $p)) "是可满足的.")
   ((proof)
    "根据合取的语义的定义, 一个解释满足"
    (&conj $p (&eqaxiom $p)) "当且仅当其满足"
    $p "和" (&eqaxioms (setE $p)) ".")
   (P "{译注: 一个解释满足集合"
      (setE $p_1 $..h $p_n) "当且仅当其满足"
      (&conj $p_1 $..c $p_n) ".}")
   (P "我们有以下对于有效性的对偶结果.")
   ((Corollary #:id "normal-validity")
    "一个公式" $p "在所有的规范模型中成立当且仅当"
    (&=> (&eqaxiom $p) $p) "在所有模型中成立.")
   ((proof)
    "因为" $p "在某个模型下成立当且仅当其全称闭包在该模型下成立, "
    "我们可以不失一般性地假定" $p "为封闭公式. "
    "因此, " $p "在所有规范模型下成立当且仅当"
    (&neg $p) "没有规范模型, 于是当且仅当"
    (&conj (&neg $p) (&eqaxiom (&neg $p)))
    "没有模型. {译注: 原文将某个iff错写成了if.} "
    "但是, 既然"
    (&= (&eqaxiom (&neg $p)) (&eqaxiom $p))
    ", 那么"
    (&conj (&neg $p) (&eqaxiom (&neg $p)))
    "逻辑等价于"
    (&neg (@disj $p (&neg (&eqaxiom $p))))
    ", 而这又逻辑等价于"
    (&neg (@=> (&eqaxiom $p) $p))
    ". 这个公式 (句子) 是不可满足的当且仅当"
    (&=> (&eqaxiom $p) $p) "是有效的.")
   (let* (($closure (Mi "closure"))
          (&closure (λ (p) (app $closure p))))
     (P "{译注: 个人对于这里的" (Q "不失一般性")
        "不太满意, 所以想要补全这里的细节. 设"
        (&closure $p) "是" $p "的全称闭包. "
        "我们已经知道, " $p
        "在所有的规范模型中成立当且仅当" (&closure $p)
        "在所有的规范模型中成立, 当且仅当"
        (&=> (&eqaxiom (&closure $p)) (&closure $p))
        "在所有模型中成立. 于是, 如果我们能够说明"
        (&=> (&eqaxiom (&closure $p)) (&closure $p))
        "在所有模型中成立当且仅当"
        (&=> (&eqaxiom $p) $p)
        "在所有模型中成立, 就结束了. "
        "当然了, 我们还应该注意到"
        (&= (&eqaxiom (&closure $p)) (&eqaxiom $p))
        ". 不过, 我们可以证明稍强一些的结论, "
        "即对于所有可能的解释" $M ", " $M "满足"
        (&=> (&eqaxiom (&closure $p)) (&closure $p))
        "当且仅当" $M "满足" (&=> (&eqaxiom $p) $p)
        ". 我们知道这两个推出式的前件实际上是相同的, "
        "并且它们都是封闭公式, 即句子. 若"
        (&eqaxiom $p) "不被" $M
        "满足, 那么对于任意的赋值" $v
        ", " (&holds $M $v (&eqaxiom $p))
        "都为假, 于是" $M "同时满足这两个推出式. "
        "如果" (&eqaxiom $p) "被" $M
        "满足, 那么对于任意的赋值" $v "都有"
        (MB (&= (&holds $M $v (@=> (&eqaxiom (&closure $p)) (&closure $p)))
                (&holds $M $v (&closure $p))))
        "并且"
        (MB (&= (&holds $M $v (@=> (&eqaxiom $p) $p))
                (&holds $M $v $p)))
        "也就是说, 这两个推出式是否同时被" $M
        "所满足或不满足的问题可以归约为"
        (&closure $p) "和" $p "是否被" $M
        "满足的问题. 然而, 我们知道既然"
        (&closure $p) "是" $p "的全称闭包, "
        "那么其相对于每个特定模型的可满足性必然是相同的. "
        "综上所述, 这两个推出式相对于每个模型的可满足性都相同, "
        "我们也就补全了gap.}"))
   (P "在以上的抽象处理之中, "
      "相等性公理包括了对于相等符号本身的谓词congruence性质:"
      (MB (∀ $x_1 $x_2 $y_1 $y_2
             (&=> (&conj (&= $x_1 $y_1)
                         (&= $x_2 $y_2))
                  (&=> (&= $x_1 $x_2)
                       (&= $y_1 $y_2)))) ".")
      "{译注: 严格说来, 这需要公式集里有" (Q $=) "的出现.}")
   (P "不过我们可以接受省略这一条公式, "
      "因为其是等价性公理的逻辑推论. "
      "{译注: 或者说等价性公理语义蕴涵这一条公式.} "
      "甚至我们可以更加经济一些, 只使用两条公理刻画等价性, "
      "分别是自反性公式和传递性公式的一个变种, 即"
      (∀ $x $y $z (&=> (&conj (&= $x $y)
                              (&= $x $z))
                       (&= $y $z)))
      ". (对称性可以通过实例化该公理以使得" $x
      "和" $z "相同, 然后再使用自反性得到.)")
   (H4. "OCaml实现")
   (P "在Skolem化里, 我们使用了" (Code "functions")
      "来寻找一个项中的所有函数符号; "
      "类似地, 以下函数找出所有出现的谓词(符号), "
      "仍然以名称和元数的序对形式:"
      (CodeB "let rec predicates fm = atom_union (fun (R(p,a)) -> [p,length a]) fm;;"))
   (P "我们通过产生合适数目的参数" (&cm $x_1 $..h $x_n) "和"
      (&cm $y_1 $..h $y_n) "并构造公式"
      (MB (∀ $x_1 $..h $x_n
             $y_1 $..h $y_n
             (&=> (&conj (&= $x_1 $y_1) $..c
                         (&= $x_n $y_n))
                  (&= (appl $f $x_1 $..h $x_n)
                      (appl $f $y_1 $..h $y_n)))))
      "来对于每个函数符号制造一个congruence公理. "
      "{译注: 原文的公式存在一个笔误, 漏了一个合取符号.}")
   (P "我们会返回一个列表, 其通常具有一个元素, "
      "但在零元函数 (即单独的常量) 的情形下会为空:"
      (CodeB "let function_congruence (f,n) =
  if n = 0 then [] else
  let argnames_x = map (fun n -> &quot;x&quot;^(string_of_int n)) (1 -- n)
  and argnames_y = map (fun n -> &quot;y&quot;^(string_of_int n)) (1 -- n) in
  let args_x = map (fun x -> Var x) argnames_x
  and args_y = map (fun x -> Var x) argnames_y in
  let ant = end_itlist mk_and (map2 mk_eq args_x args_y)
  and con = mk_eq (Fn(f,args_x)) (Fn(f,args_y)) in
  [itlist mk_forall (argnames_x @ argnames_y) (Imp(ant,con))];;")
      "例如:"
      (CodeB "# function_congruence (&quot;f&quot;,3);;
- : fol formula list =
[&lt;&lt;forall x1 x2 x3 y1 y2 y3.
     x1 = y1 /\\ x2 = y2 /\\ x3 = y3 ==> f(x1,x2,x3) = f(y1,y2,y3)>>]
# function_congruence (&quot;+&quot;,2);;
- : fol formula list =
[&lt;&lt;forall x1 x2 y1 y2. x1 = y1 /\\ x2 = y2 ==> x1 + x2 = y1 + y2>>]"))
   (P "针对谓词而言的类似函数几乎是相同的, "
      "只不过在后件里我们用的是公式的推出而非项的相等:"
      (CodeB "let predicate_congruence (p,n) =
  if n = 0 then [] else
  let argnames_x = map (fun n -> &quot;x&quot;^(string_of_int n)) (1 -- n)
  and argnames_y = map (fun n -> &quot;y&quot;^(string_of_int n)) (1 -- n) in
  let args_x = map (fun x -> Var x) argnames_x
  and args_y = map (fun x -> Var x) argnames_y in
  let ant = end_itlist mk_and (map2 mk_eq args_x args_y)
  and con = Imp(Atom(R(p,args_x)),Atom(R(p,args_y))) in
  [itlist mk_forall (argnames_x @ argnames_y) (Imp(ant,con))];;")
      "正如之前所计划的, 我们使用以下等价性质变体:"
      (CodeB "let equivalence_axioms =
  [&lt;&lt;forall x. x = x>>; &lt;&lt;forall x y z. x = y /\\ x = z ==> y = z>>];;"))
   (P "现在我们定义一个函数, 其对于输入公式" $p
      "返回" (&=> (&eqaxiom $p) $p)
      ". 如果" $p "压根不牵涉相等符号, "
      "那么就保留" $p "本来的样子, "
      "因为其规范和非规范模型之间没有区别. "
      "{译注: 规范模型本质上只是对于相等符号的解释的约束. "
      "不含有相等符号的" $p
      "若在所有的规范解释下成立, "
      "那么对于任意的解释, "
      "我们可以修改其对于相等符号的解释, "
      "然后就知道" $p "在这个修改版本下成立. "
      "可是, 因为" $p "不含有相等符号, "
      "所以成立与否和相等符号的解释无关, "
      "由此也就知道了" $p "在所有解释下都成立.}"
      (CodeB "let equalitize fm =
  let allpreds = predicates fm in
  if not (mem (&quot;=&quot;,2) allpreds) then fm else
  let preds = subtract allpreds [&quot;=&quot;,2] and funcs = functions fm in
  let axioms = itlist (union ** function_congruence) funcs
                      (itlist (union ** predicate_congruence) preds
                              equivalence_axioms) in
  Imp(end_itlist mk_and axioms,fm);;"))
   (P (Ref "normal-validity")
      "的影响在于我们可以通过在常规一阶逻辑里测试"
      (&equalitize $p) "的有效性来测试" $p
      "在具有相等概念的一阶逻辑里的有效性. "
      "因此, 我们可以将" $equalitize
      "应用为既存证明过程的预处理步骤. "
      
      )
   (H3. "范畴性和初等等价")
   (H3. "等式逻辑和完备性")
   (H3. "congruence闭包")
   (H3. "重写")
   (H3. "终止排序")
   (H3. "Knuth-Bendix补全")
   (H3. "等式消去")
   (H3. "paramodulation")
   (H3. "深入阅读")
   (P "专注于等式逻辑的模型论分支也被称为泛代数 (universal algebra), "
      "相关教材有Cohn (1965) 以及Burris和Sankappanavar (1981) 等. "
      "上一章所引用的几乎所有模型论书籍中也都涉及这里所描述的理论内容. "
      "Corcoran (1980) 对范畴性 (categoricity) 概念提供了更多信息, "
      "包括历史背景等方面. 关于" $kappa
      "-范畴性还有两个难度更高的定理: 其一是Morley定理, "
      "断言一个在某个不可数基数上范畴的理论在所有不可数基数上均范畴; "
      "其二是Ryll-Nardzewski定理, 给出了" $aleph_0
      "-范畴理论一个简洁的代数刻画. "
      "这两个定理均可在Hodges (1993b) 中找到.")
   (P "关于基于重写技术的纯等式推理, 请参阅Baader和Nipkow (1998) 的专著, "
      "以及Huet和Oppen (1980), Klop (1992) 和Plaisted (1993) 的综述文章. "
      "Dershowitz关于化简序 (simplification order) 是停机的这一结论, "
      "通常由Kruskal定理的(一个简单情形)推导得出 "
      "(Kruskal 1960; Nash-Williams 1963); "
      "Baader和Nipkow (1998) 中有易于理解的阐述. "
      "在实现LPO时我们未考虑效率问题, "
      "但Löchner (2006) 对此作了细致的分析.")
   (P "判定带等式逻辑中全称公式有效性的方法在验证领域有重要应用 (Burch和Dill 1994). "
      "这推动了对合同闭包 (congruence closure) 之外的各种替代算法的探索. "
      "关于基于Ackermann规约方法的进一步改进, "
      "可参见Goel, Sajid, Zhou, Aziz和Singhal (1998), Velev和Bryant (1999) "
      "以及Lahiri, Bryant, Goel和Talupur (2004).")
   (P "已提到的一些自动定理证明教材中讨论了超模归结 (paramodulation), "
      "包括Chang和Lee (1973) 以及Loveland (1978). 此外, "
      "Argonne小组的Wos, Overbeek, Lusk和Boyle (1992) "
      "等著作涵盖了利用超模归结求解非平凡问题的方法. "
      "Bachmair和Ganzinger (1994) 是一篇关于超模归结及相关思想的综述, "
      "Degtyarev和Voronkov (2001) 则综述了"
      "如tableau等自顶向下自由变量演算中的等式推理.")
   (P "TPTP问题库 (Sutcliffe和Suttner 1998) 包含大量等式问题, "
      "并提供工具以便不直接处理等式的证明器添加等式公理. "
      "自动推理在等式逻辑这一大领域中有若干最令人印象深刻的应用. "
      "最著名的例子是Robbins猜想" --
      "该猜想曾令包括Tarski在内的众多杰出数学家的证明尝试均告失败"
      -- "最终由McCune (1997)使用EQP证明器自动求解. "
      "这只是自动推理程序回答开放性问题的一个特别广为人知的案例. "
      "更多案例可见于McCune和Padmanabhan (1996) "
      "以及Wos和Pieper (2003) 的专著, 以及互联网上的相关资源.")
   (H3. "练习")
   ))
