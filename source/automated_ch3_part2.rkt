#lang racket
(provide automated_ch3_part2)
(require (except-in SMathML H3. H4. $!=)
         "automated_utils.rkt")
(define automated_ch3_part2
  (Tm*
   (H3. "合一")
   (P (Code "gilmore") "和" (Code "davisputnam")
      "过程基本遵循了相同的模式. "
      "命题逻辑的判定方法 "
      "(分别是析取范式方法和Davis-Putnam方法) "
      "被用来和ground实例的系统枚举一起使用. "
      "一个更为精巧的想法最早由"
      "Prawitz, Prawitz和Voghera (1960) 采用, "
      "即直接对" (Em "未实例化") "的公式执行命题运算, "
      "或者至少只对它们进行恰到好处的智能实例化, "
      "以便在命题推理上取得进展. "
      "Prawitz的工作被J. A. Robinson (1965b) 加以扩展, "
      "他给出了一个有效的句法过程, "
      "称为合一 (unification), "
      "用于判定应当采取何种适当的实例化, "
      "从而使项正确地匹配起来. "
      "例如, 假设我们在Davis–Putnam"
      "方法中有如下未实例化的子句:"
      (MB (&cm (&disj (appl $P $x (app $f $y))
                      (appl $Q $x $y))
               (&neg (appl $P (app $g $u) $v))) "."))
   (P "与其盲目枚举, 我们可以选择"
      "两个子句中的变量的实例化以使得"
      (appl $P $x (app $f $y)) "和"
      (&neg (appl $P (app $g $u) $v))
      "成为互补的, 例如置"
      (&= $x (app $g $u)) "而"
      (&= $v (app $f $y))
      ". 在实例化之后, 我们有了以下子句:"
      (MB (&cm (&disj (appl $P (app $g $u)
                            (app $f $y))
                      (appl $Q (app $g $u) $y))
               (&neg (appl $P (app $g $u)
                           (app $f $y)))) ".")
      "于是我们能够使用归结规则来推导出一个新的子句:"
      (MB (appl $Q (app $g $u) $y) ".")
      "相比之下, 在基于枚举的方法中, "
      "我们必须一直等到那些允许执行"
      "同一种归结步骤的实例被生成出来为止, "
      "而到那个时候, 我们可能早已被其它 "
      "(往往是无关的) 实例所淹没.")
   ((Definition)
    "给定一个项的序对之集合"
    (MB (&= $S (setE (tu0 $s_1 $t_1) $..h
                     (tu0 $s_n $t_n))))
    "集合" $S "的一个unifier是一个实例化"
    $sigma "使得"
    (MB (&= (&tsubst $sigma $s_i)
            (&tsubst $sigma $t_i)))
    "对于每个" (&= $i (&cm $1 $..h $n))
    "成立. 在单独一对项的情况下, "
    "我们往往言称"
    (Q $s "和" $t "的unifier")
    ", 其指的是" (setE (tu0 $s $t))
    "的unifier.")
   (P "对于一个项的序对之集合进行合一可以类比于对于方程组进行求解, "
      "例如普通代数里的" (&= (&+ (&i* $2 $x) $y) $3) "和"
      (&= (&- $x $y) $6) ", 并且我们会在以下讨论中强调这种对应. "
      "就像方程组可能是无解的, 合一问题也可能是无解的. "
      "首先, 不存在" (app $f $x) "和" (app $g $y)
      "的unifier, 其中" $f "和" $g "是不同的函数符号, "
      "因为不论以何项替换" $x "和" $y
      ", 实例化之后的项在顶层所具有的仍然是不同的函数. "
      "更加微妙的情况是, " $x "和" (app $f $x)
      "也没有unifier, 或者更一般地, " $x "和任意牵涉"
      $x "为真子项的项也没有unifier, "
      "因为不论对于" $x "采取何种实例化, "
      "一个项永远都会是另一个项的真子项, "
      "由此不会是相等的. "
      "这和在普通代数里试图求解" (&= $x (&+ $x $1))
      "形成了恰到好处的类比. "
      "这种循环性的一个更为复杂的例子是"
      (setE (tu0 $x (app $f $y))
            (tu0 $y (app $g $x)))
      "的合一问题, 其可以类比于求解方程组"
      (&= $x (&+ $y $1)) "和"
      (&= $y (&+ $x $2)) ".")
   (P "另一方面, 如果一个合一问题具有一个解, "
      "那么其总是具有无限多个解, 因为若"
      $sigma "是" $s_i "和" $t_i
      "的一个unifier, "
      "那么对于任意的实例化" $tau
      ", 使用" (Ref "icompose") ":"
      (MB (deriv^
           (&tsubst (@compose (@tsubst $tau) $sigma) $s_i)
           (&tsubst $tau (@tsubst $sigma $s_i))
           (&tsubst $tau (@tsubst $sigma $t_i))
           (&tsubst (@compose (@tsubst $tau) $sigma) $t_i))))
   (P "例如, 与其通过置" (&= $x (app $g $u)) "和"
      (&= $v (app $f $y)) "来对于"
      (appl $P $x (app $f $y)) "和"
      (appl $P (app $g $u) $v)
      "进行合一, 我们本也可以使用其他变量或者是任意程度复杂的项, 例如"
      (&= $x (app* $g $f $g $y)) ", "
      (&= $u (app* $f $g $y))
      "和" (&= $v (app $f $y))
      ". 不过, 实际上我们总是可以找到一个" (Q "最为一般")
      "的unifier, 其保持实例化项尽可能" (Q "简单") ".")
   (P "我们称一个实例化" $sigma "比另一个实例化" $tau
      "更为一般, 记作" (&<= $sigma $tau)
      ", 如果存在某个实例化" $delta "使得"
      (MB (&= (&tsubst $tau)
              (&compose (@tsubst $delta)
                        (@tsubst $sigma))) "."))
   (P "我们称" $sigma "是" $S "的一个"
      (Em "最为一般的unifier")
      " (MGU), 如果 (i) 它是" $S
      "的一个unifier且 (ii) 对于" $S
      "的每个unifier " $tau
      ", 我们都有" (&<= $sigma $tau)
      ". 最一般unifier未必是唯一的. 例如, 集合"
      (setE (tu0 $x $y)) "有两个不同的MGU, "
      "一个maps " (&\|=> $x $y) ", 另一个maps "
      (&\|=> $y $x) ". {译注: 这里的" (Q "maps")
      "是动词, 例如" (Q "maps " (&\|=> $x $y))
      "应该理解为描述了将变量" $x "映射为" $y
      "而将其他变量都映射为自身的实例化.} "
      "然而, 我们可以轻松地证明一个给定集合" $S
      "的两个MGU, 其不同之处相当于一个对于变量的置换. "
      "(假定我们限制了unifier只能是影响有限数目变量的实例化.) "
      "{译注: 即我们仅考虑这样的实例化" $sigma
      ", 集合" (setI $x (&!= (app $sigma $x) $x))
      "是有限的.}")
   (H4. "一个合一算法")
   (P "现在让我们考虑解决合一问题或者判定其无解的通用方法. "
      "我们的主函数" (Code "unify") "是递归的, 其有两个参数: "
      (Code "env") ", 其是一个从变量到项的有限部分函数, "
      "以及" (Code "eqs") ", 其是一个要进行合一的项-项序对列表. "
      "合一函数本质上是在应用一些变换于" (Code "eqs")
      ", 并将得到的变量-项映射融入" (Code "env")
      ". 这个" (Code "env") "并非最终的合一映射本身, "
      "因为其会将变量映射到包含自身被指派的变量的项, 例如"
      (&\|-> $x $y) "和" (&\|-> $y $z) "而非直接"
      (&\|-> $x $z) ". 但是, 我们会要求" (Code "env")
      "没有" (Em "环路(cycle)") ". 我们记"
      (&--> $x $y) "以指明" (Code "env")
      "中存在指派" (&\|-> $x $t) "满足"
      (∈ $y (&FVT $t)) ". 使用环路一词, "
      "我们指的是一个导回起点的非空有限序列:"
      (MB (&--> $x_0 $x_1 $..c $x_p $x_0) ".")
      "{译注: 换言之, " $env "存在环路当且仅当存在变量"
      $x "满足" (&-->+ $x $x) ".}")
   (P "我们的主合一算法只会融入使得" (Code "env")
      "能够保持无环路性质的新条目" (&\|-> $x $t)
      ". 实际上, 确保以下条件就足够了:"
      (Ol (Li (Code "env") "中不存在既有指派"
              (&\|-> $x $s) ";")
          (Li "不存在变量" (∈ $y (&FVT $t))
              "满足" (&-->* $y $x)
              ", 此即一个由零或多个"
              $--> "步骤构成的从" $y
              "到" $x "的序列; 特别地, "
              (&!in $x (&FVT $t)) ".")))
   (P "为了看出若" (Code "env")
      "是无环路的且这些性质成立, 那么"
      (ext $env $x $t)
      "也是无环路的, 注意到如果对于新的关系"
      $-->^ "而言真的存在一个环路:"
      (MB (&-->^ $z $x_1 $..c $x_p $z))
      "那么必然存在具有以下形式之一的环路:"
      (MB (&--> $z $x_1
                (&-->^ $x $y)
                $..c $x_p $z))
      "其中" (∈ $y (&FVT $t))
      ". 这是因为必然要留下给新的指派"
      (&\|-> $x $t) "发挥的余地, 鉴于"
      $env "本来是无环路的, "
      "而且如果存在多于一个" $x "的实例, "
      "我们总是能将第一个实例和"
      "最后一个实例之间的中间步骤全部砍掉. "
      "{译注: 更准确地说, 其实不是" $x
      "的实例, 而是" (&-->^ $x $y)
      "的实例, 其中" $y "可以是" (&FVT $t)
      "的任意元素. 即便" $x
      "在这环路里出现了多于一次, "
      "但是形如" (&-->^ $x $y)
      "的步骤也可能只出现了一次, 原因在于"
      $x "可以恰好出现在开头和结尾.} "
      "然而, 具有上述形式的环路也会给出以下环路, "
      "这与假设(2)相矛盾:"
      (MB (&--> $y $..c $x_p $z $x_1 $x) ".")
      "{译注: 这就像将项链从不同位置剪开一样.} "
      "{译注: 我更喜欢这样表述: 对于无环路的"
      $env "而言, 如果" $env "中不存在对于"
      $x "的指派, 那么" (ext $env $x $t)
      "具有环路当且仅当存在" (∈ $y (&FVT $t))
      "使得" (&-->* $y $x) ".}")
   (P "以下函数会在上述条件(2)对于新的指派"
      (&\|-> $x $t) "成立时返回" (Q "假")
      ". 如果条件(2)不成立, 那么它会失败, "
      "除了在" (&= $t $x) "的情形下其会返回真, "
      "这指明该指派是" (Q "平凡的") "."
      (CodeB "let rec istriv env x t =
  match t with
    Var y -> y = x or defined env y &amp; istriv env x (apply env y)
  | Fn(f,args) -> exists (istriv env x) args &amp; failwith &quot;cyclic&quot;;;"))
   (P "{译注: 这段程序虽然只有四行, 但却异常微妙和难以理解. "
      "因此, 我们需要证明其正确性. "
      "首先, 我们需要明确前条件: " $env "没有环路, "
      $env "中没有对于" $x "的指派. "
      "然后, 我们需要明确要证明的结论:"
      (Blockquote
       (Code "istriv env x t")
       "能够在有限时间内停止, "
       "不论是返回一个值还是抛出异常, 并且"
       (Q (Code "istriv env x t")
          "返回" (Code "false"))
       "当且仅当"
       (Q "对于每个" (∈ $y (&FVT $t))
          ", 都有" (&neg (@-->* $y $x)))
       "; "
       (Q (Code "istriv env x t")
          "返回" (Code "true"))
       "当且仅当"
       (Q (&= (&walk $env $t) $x))
       "; "
       (Q (Code "istriv env x t")
          "抛出异常")
       "当且仅当"
       (Q "存在" (∈ $y (&FVT $t))
          "满足" (&-->* $y $x)
          ", 另外"
          (&!= (&walk $env $t) $x))
       ". 其中, " $walk
       "是我们的辅助函数, 其定义如下:"
       (MB (&= (&walk $env $t)
               (Choice0
                ((&walk $env (&apply $env $y))
                 ", 如果" (&= $t $y) "且"
                 $y "在" $env "中存在指派")
                ($y ", 如果" (&= $t $y) "且"
                    $y "在" $env "中没有指派")
                ((appl $f $t_1 $..h $t_n)
                 ", 如果" (&= $t (appl $f $t_1 $..h $t_n)))))))
      "我们注意到"
      (&= (&walk $env $t) $x) "蕴涵了"
      (Q "存在" (∈ $y (&FVT $t))
         "满足" (&-->* $y $x))
      ". 因此, 这三种条件显然是互斥的, "
      "并且它们的析取等价于" $top
      ". 现在我们开始证明, 使用良基归纳. 如果"
      (&= $t $y) ", 那么"
      (Ol (Li (&= $y $x) ", 此时返回" (Code "true")
              ", 并且我们知道其满足条件"
              (&= (&walk $env $t) $x) ";")
          (Li (&!= $y $x) ", 且" $y
              "在" $env "中没有指派, "
              "此时返回" (Code "false")
              ", 并且我们知道不存在"
              (∈ $y (&FVT $t)) "使得"
              (&-->* $y $x) ";")
          (Li (&!= $y $x) ", 且" $y
              "在" $env "中存在指派, "
              "此时有三种可能性:"
              (Ol (Li "返回" (Code "true")
                      ", 说明"
                      (&= (&walk $env (&apply $env $y)) $x)
                      ", 于是"
                      (&= (&walk $env $t) $x) ";")
                  (Li "返回" (Code "false")
                      ", 说明对于每个"
                      (∈ $z (&FVT (&apply $env $y)))
                      ", 都有" (&neg (@-->* $z $x))
                      ", 若" (&-->* $y $x)
                      ", 此时肯定不可能是" $0
                      "阶情形, 于是起始的步骤会是"
                      (&--> $y $z) ", 由此推出"
                      (&-->* $z $x) ", 然而这与"
                      (&neg (@-->* $z $x))
                      "矛盾了, 于是"
                      (&neg (@-->* $y $x)) ";")
                  (Li "抛出异常, 说明存在"
                      (∈ $z (&FVT (&apply $env $y)))
                      "满足" (&-->* $z $x)
                      ", 而且"
                      (MB (&!= (&walk $env (&apply $env $y))
                               $x) ",")
                      "于是"
                      (&--> $y $z) ", 由此可知"
                      (&-->* $y $x) "而"
                      (&!= (&walk $env $t) $x) "."))))
      "如果" (&= $t (appl $f $t_1 $..h $t_n)) ", 那么"
      (Ol (Li "返回" (Code "false")
              ", 说明对于每个" $t_i "都有"
              (Q "对于每个" (∈ $y (&FVT $t_i))
                 "都有" (&neg (@-->* $y $x)))
              ", 故对于每个"
              (∈ $y (&FVT (appl $f $t_1 $..h $t_n)))
              "都有" (&neg (@-->* $y $x)) ";")
          (Li "抛出异常, 此时有两种可能性. "
              "但是, 不论对于哪种可能性, 都存在"
              (∈ $y (&FVT (appl $f $t_1 $..h $t_n)))
              "使得" (&-->* $y $x) ", 另外因为"
              $t "不是变量, 所以也显然有"
              (&!= (&walk $env $t) $x) "."))
      "证明完毕.}")
   (P "这有效地计算了" $--> "的自反传递闭包, "
      "尽管其本也可以用远为高效的方式完成. "
      "然而, 这个简单的递归算法通常足够快速了, "
      "并且一定会终止, 恰因既有环境" $env
      "是无环的.")
   (P "现在我们来到合一的主函数. 它只是对于序对列表"
      (Code "eqs") "进行变换, 将头部转换为"
      (tu0 $x $t) "的形式. 如果在" $env
      "中已有定义" (&\|-> $x $s)
      ", 那么序对会被扩展为" (tu0 $s $t)
      ", 继续递归处理. 否则的话, "
      "我们知道条件(1)成立, 于是"
      (&\|-> $x $t) "可以成为融入" $env
      "的候选. 如果存在良性的环路, 那么"
      (Code "istriv env x t")
      "为真而" $env "保持不变. "
      "任何其他种类的环路都会导致失败, "
      "失败会传播出去. 否则的话, "
      "条件(2)成立, " (&\|-> $x $t)
      "融入" $env "之中以供下一次递归调用."
      (CodeB "let rec unify env eqs =
  match eqs with
    [] -> env
  | (Fn(f,fargs),Fn(g,gargs))::oth ->
        if f = g &amp; length fargs = length gargs
        then unify env (zip fargs gargs @ oth)
        else failwith &quot;impossible unification&quot;
  | (Var x,t)::oth | (t,Var x)::oth ->
        if defined env x then unify env ((apply env x,t)::oth)
        else unify (if istriv env x t then env else (x|->t) env) oth;;")
      "{译注: 这个版本和原书稍有不同, 来源于官方代码库.}")
   (P "让我们将" $env "中的指派" (&\|-> $x_i $t_i)
      "和" (Code "eqs") "中的序对" (tu0 $s_j (_prime $s $j))
      "合起来视为一个序对的集合"
      (MB (&= $S (setE $..h (tu0 $x_i $t_i) $..h
                       (tu0 $s_j (_prime $s $j)) $..h)) ".")
      (Code "unify") "是尾递归的, "
      "而关键的观察在于相继的递归调用过程之中, 参数"
      $env "和" (Code "eqs") "满足两个性质:"
      (Ul (Li "有限部分函数" $env "是无环的;")
          (Li "将" $env "和" (Code "eqs") "结合起来的集合"
              $S "和原本的问题" (Em "恰好有着相同的")
              "unifier集合.")))
   (P "第一个声称之所以成立的原因如下: 新的指派" (&\|-> $x $t)
      "只在环境中不存在既有指派" (&\|-> $x $s)
      "的情况下才会考虑加入到环境之中, 这确保了条件(1)满足, "
      "并且在" (Code "istriv env x t") "返回"
      (Code "false") "时才会真的加入到环境之中, "
      "这确保了条件(2)满足. "
      "{译注: 原文将" (Code "istriv env x t")
      "误作为了" (Code "defined env x") ".} "
      "为了验证另一声称, 我们考虑可能会导致递归调用的分支情形. "
      "第二个分支情形只会在" (Code "eqs") "的头部具有形式"
      (MB (tu0 (appl $f $s_1 $..h $s_n)
               (appl $f $t_1 $..h $t_n)))
      "时才会触发递归调用, 而声称之所以成立, 是因为"
      (MB (&union (setE (tu0 (appl $f $s_1 $..h $s_n)
                             (appl $f $t_1 $..h $t_n)))
                  $E))
      "和"
      (MB (&union (setE (tu0 $s_1 $t_1) $..h
                        (tu0 $s_n $t_n))
                  $E))
      "恰好有着相同的unifier (复数). "
      "原因在于任意的实例化能够对于"
      (appl $f $s_1 $..h $s_n) "和"
      (appl $f $t_1 $..h $t_n)
      "进行合一当且仅当其能够对于每个对应的序对"
      $s_i "和" $t_i "进行合一. "
      "当头部的序对为" (tu0 $x $t)
      "并且已然存在一个指派" (&\|-> $x $s)
      "时, 我们递归调用的方式在于将"
      (tu0 $x $t) "换成" (tu0 $s $t)
      ". 这仍然保持我们声称的性质, 因为"
      (&union (setE (tu0 $x $t)
                    (tu0 $x $s))
              $E)
      "和"
      (&union (setE (tu0 $s $t)
                    (tu0 $x $s))
              $E)
      "恰好有着相同的unifier (复数). "
      "最后一个分支情形只是反转了头部序对的顺序, "
      "而这个顺序对于unifier而言是无关紧要的. "
      "{译注: 原本的代码有第四个分支情形, "
      "但是现在被整合到第三个分支情形里去了.} "
      "因此, 声称得到了验证.")
   (P "任何失败都会指出其中一个中间问题是不可解的, "
      "要么是其牵涉了不兼容的顶层函数, 例如序对"
      (tu0 (app $f $s) (app $g $t))
      ", 要么是其牵涉了循环, 其中unifier要对于"
      (tu0 $x $t) "进行合一而" (∈ $x (&FVT $t))
      "且" (&!= $x $t) ". "
      )
   (H4. "使用合一")
   (H3. "tableau")
   (H3. "归结")
   (H3. "subsumption和replacement")
   (H3. "对于归结的改进")
   (H3. "Horn子句和Prolog")
   (H3. "模型消去")
   (H3. "更多的一阶元定理")
   (H3. "深入阅读")
   (P "这里的基本理论结果可在大多数入门逻辑教材中找到, "
      "例如Enderton (1972), Mendelson (1987), Boolos和Jeffrey (1989), "
      "Goodstein (1971), Kreisel和Krivine (1971) 以及Andrews (1986), "
      "而在模型论的高级教材中则有更深入的发展, "
      "如Bell和Slomson (1969), Chang和Keisler (1992), Hodges (1993b), "
      "Marcja和Toffalori (2003) 以及Poizat (2000). "
      "Davis, Sigal和Weyuker (1994) 以更侧重机械化的视角涵盖了这些内容. "
      "提供数理逻辑发展之更多历史与哲学背景的书籍包括Bochénski (1961), "
      "Dumitriu (1977) 以及Kneale和Kneale (1962), "
      "而Kneebone (1963) 则将哲学与技术性结果融为一体. "
      "Van Heijenoort (1967) 是该领域经典论文的选集, "
      "其中包括Löwenheim, Skolem, Gödel和Herbrand的奠基性工作, "
      "这些工作构成了本章大多数方法的基础. "
      "关于Skolem化和归约为子句范式的详细研究, "
      "着重探讨与自动证明相关的效率问题, "
      "可参见Nonnengart和Weidenbach (2001).")
   (P "一阶逻辑有若干推广形式, 我们不作深入探讨. "
      "最为彻底的推广是高阶逻辑 (HOL), 其中允许对函数和谓词进行量化; "
      "在上述教材中, Andrews (1986) 是唯一对高阶逻辑有详尽论述的, "
      "但Boolos和Jeffrey (1989) 以及Enderton (1972) 也有所提及. "
      "一种较为适度的推广允许量词具有分支辖域 (branching scope), "
      "这可视为高阶逻辑的一种受限形式. Hintikka (1996) 认为, "
      "在某种意义上这种" (Q "独立友好")
      "逻辑 (independence friendly logic) 比普通一阶逻辑更为基本, "
      "但IF逻辑或HOL的有效性问题甚至不再是半可判定的 (semidecidable).")
   
   (H3. "练习")
   ))