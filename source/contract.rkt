#lang racket
(provide contract.html)
(require SMathML)
(define $Contract (Mi "Contract"))
(define $Date (Mi "Date"))
(define $Currency (Mi "Currency"))
(define $Double (Mi "Double"))
(define $zcb (Mi "zcb"))
(define $GBP (Mi "GBP"))
(define $:: (Mo "::"))
(define App (&split 2))
(define $mkDate (Mi "mkDate"))
(define $String (Mi "String"))
(define $quot (Mo "&quot;"))
(define (&quot x)
  (: $quot x $quot))
(define $and (Mi "and"))
(define $squot (Mo "'"))
(define (&squot x)
  (: $squot x $squot))
(define $qand (&squot $and))
(define $give (Mi "give"))
(define (&give c)
  (App $give c))
(define $andGive (Mi "andGive"))
(define $qandGive (&squot $andGive))
(define $zero (Mi "zero"))
(define $one (Mi "one"))
(define (&one k)
  (App $one k))
(define $or (Mi "or"))
(define $Obs (Mi "Obs"))
(define $Bool (Mi "Bool"))
(define (&Obs x)
  (App $Obs x))
(define $cond (Mi "cond"))
(define (&cond o c1 c2)
  (App $cond o c1 c2))
(define $if (Mi "if"))
(define (&if q a e)
  (appl $if q a e))
(define $scale (Mi "scale"))
(define $qscale (&squot $scale))
(define $when (Mi "when"))
(define (&when o c)
  (App $when o c))
(define $anytime (Mi "anytime"))
(define $until (Mi "until"))
(define $qor (&squot $or))
(define $konst (Mi "konst"))
(define (&konst x)
  (App $konst x))
(define $lift (Mi "lift"))
(define (&lift f o)
  (App $lift f o))
(define $lift_2 (_ $lift $2))
(define (&lift_2 f o1 o2)
  (App $lift_2 f o1 o2))
(define $date (Mi "date"))
(define $instance (Mi "instance"))
(define $Num (Mi "Num"))
(define Eval
  (case-lambda
    ((c k) (ap (_ $E:script k)
               (&db0 c)))
    ((c) (Eval c $k))))
(define (Val o)
  (ap $V:script (&db0 o)))
(define $exch (Mi "exch"))
(define &exch
  (case-lambda
    ((k1 k2) (app (_ $exch k1) k2))
    ((k2) (&exch $k k2))))
(define $PR
  (Mi "PR" #:attr* '((mathvariant "script"))))
(define (&PR t)
  (App $PR t))
(define $RV
  (Mi "RV" #:attr* '((mathvariant "script"))))
(define (&RV t)
  (App $RV t))
(define $dummy $d*)
(define $disc (Mi "disc"))
(define &disc
  (case-lambda
    ((k b r) (appl (_ $disc k) b r))
    ((b r) (&disc $k b r))))
(define-infix*
  (&qscale $qscale)
  (&qor $qor)
  (&:: $::)
  (&qand $qand)
  (&qandGive $qandGive))
(define-@lized-op*
  (@lift_2 &lift_2)
  (@lift &lift)
  (@konst &konst)
  (@Obs &Obs)
  (@-> &->)
  (@qor &qor)
  (@App App)
  (@give &give)
  (@qand &qand))
(define contract.html
  (TnTmPrelude
   #:title "如何编写金融合约"
   #:css "styles.css"
   (H1. "如何编写金融合约")
   (H2. "引论")
   (H2. "开始")
   (P "本节我们将会非形式化地引入我们对于合约的记号, "
      "并展示我们如何根据较为简单的合约构建更为复杂的合约. "
      "整个文章我们都会使用函数式语言Haskell.")
   (H3. "一个简单合约")
   (P "考虑以下简单合约, 或许可以充当Richard的礼物: "
      (Q "receive £100 on 13th February 2003")
      ". (这种形式的合约在业界被称为"
      (Em "零息折扣债券 (zero-coupon discount bond)")
      ".) 我们可以描述这个合约, 称之为" $c_1 ", 如下:"
      (eqn*
       ($c_1 $:: $Contract)
       ($c_1 $= (App $zcb $t_1 100 $GBP)))
      "图1总结了整篇文章我们对于变量所使用的记号约定, "
      "例如这个定义里的" $c_1 "和" $t_1 ".")
   (P $c_1 "定义中所使用的组合子" $zcb "具有如下类型:"
      (MB (&:: $zcb (&-> $Date $Double $Currency $Contract))))
   (P $zcb "的第一个参数是一个" $Date
      ", 其刻画了时间方面的一个特定时刻 (即日期和时间). "
      "我们提供了一个函数" $mkDate
      ", 其将表达以一个友好的字符串的date转换为一个"
      $Date "."
      (MB (&:: $mkDate (&-> $String $Date)))
      "现在我们可以定义Richard的2003年和2004年的生日如下:"
      (eqn*
       ((&cm $t_1 $t_2) $:: $Date)
       ($t_1 $= (App $mkDate (&quot "0800 GMT 13 Feb 2003")))
       ($t_2 $= (App $mkDate (&quot "0800 GMT 13 Feb 2004")))))
   (H3. "对于合约进行组合")
   (P "所以说, " $zcb "让我们能够构建一个简单合约. "
      "我们也可以组合合约以制作更大的合约. "
      "这样一种组合形式的良好例子是" $and
      ", 其类型为:"
      (MB (&:: $and (&-> $Contract $Contract $Contract)))
      "使用" $and "我们可以定义" $c_3
      ", 一个牵涉两个支付的合约:"
      (eqn*
       ((&cm $c_2 $c_3) $:: $Contract)
       ($c_2 $= (App $zcb $t_2 200 $GBP))
       ($c_3 $= (&qand $c_1 $c_2)))
      "也就是说, 如果Richard持有合约" $c_3
      ", 那么他会在2003年生日受益于£100的支付, "
      "在2004年生日受益于£200的支付.")
   (P "一般而言, 我们可以描述的合约是两方之间的, 即合约的"
      (Em "持有者") ", 以及" (Em "交易对手")
      ". 尽管有圣经教导在先 (Acts 20.35), "
      "默认情况下还是合约的持有者接受支付, "
      "并作出选择, 这在合约中进行描述. "
      "这种情况可由" $give "组合子反转:"
      (MB (&:: $give (&-> $Contract $Contract)))
      "合约" (App $give $c) "不过就是" $c
      "的权利和义务反转, "
      "而其精确含义我们将会在第4.2节揭晓. "
      "的确, 当两方就合约达成一致时, "
      "一方获得了合约" $c
      ", 而另一方获得了合约" (App $give $c)
      "; 每一方都是另一方的对手方. "
      "例如, " $c_4 "是一个合约, 其持有者在时间"
      $t_1 (Em "接受") "£100, 而在时间" $t_2
      (Em "支付") "£200:"
      (MB (&= $c_4 (&qand $c_1 (@give $c_2))))
      "到目前为止, 我们的每个定义都在定义新的"
      (Em "合约") " (" $c_1 ", " $c_2
      ", 等等). 定义新的" (Em "组合子")
      " (构建合约的函数) 也是相当容易的. "
      "例如, 我们可以按照如下方式定义"
      $andGive ":"
      (eqn*
       ($andGive
        $:: (&-> $Contract $Contract $Contract))
       ((App $andGive $c $d)
        $=
        (&qand $c (@give $d))))
      "现在我们给出" $c_4 "的另外一个定义 (比之前更加简单):"
      (MB (&= $c_4 (&qandGive $c_1 $c_2)))
      "这种定义新的组合子, 然后将其像内置函数一样使用的能力, "
      "对于函数式程序员是相当常规的, "
      "但对于金融工程师并非如此.")
   (H2. "构建合约")
   (P (eqn*
       ($zero  $:: $Contract)
       ($ $ (: $zero "是一个没有权利且没有义务的合约."))
       ($one   $:: (&-> $Currency $Contract))
       ($ $ (: "如果你获得了" (@App $one $k)
               ", 那么你立即可以收到货币"
               $k "的一个单位."))
       ($give  $:: (&-> $Contract $Contract))
       ($ $ (: "获得" (@give $c)
               "就是获得所有的" $c
               "的权利作为义务, 所有的义务作为权利."))
       ($ $ (: "注意到对于"
               $A "方和" $B "方之间的双边合约"
               $q "而言,&nbsp;" $A "获得" $q "可以推出"
               $B "获得" (@give $q) "."))
       ($and   $:: (&-> $Contract $Contract $Contract))
       ($ $ (: "如果你获得了" (@qand $c_1 $c_2)
               ", 那么你立即获得了" $c_1 "和" $c_2 "."))
       ($or    $:: (&-> $Contract $Contract $Contract))
       ($ $ (: "如果你获得了" (@qor $c_1 $c_2)
               ", 那么你必须立即从" $c_1 "和" $c_2
               "中选择一个获得."))
       ($cond  $:: (&-> (&Obs $Bool)
                        $Contract $Contract $Contract))
       
       ($scale $:: (&-> (&Obs $Double)
                        $Contract $Contract))
       ($when  $:: (&-> (&Obs $Bool)
                        $Contract $Contract))
       ($anytime $:: (&-> (&Obs $Bool)
                          $Contract $Contract))
       ($until $:: (&-> (&Obs $Bool)
                        $Contract $Contract))
       )
      (B "图2. 定义合约的原语"))
   (P "我们已经完成了我们的非正式介绍. "
      "本章我们将给出完整的原语集, "
      "并展示各种各样的合约是如何通过这些原语构建的. "
      "处于参考的目的, 图2给出了所有合约上的原始组合子; "
      "我们将会按需引入这些原语.")
   (H3. "获取日期和可观察量")
   (P (eqn*
       ($konst  $:: (&-> $a (&Obs $a)))
       ($ $ (: (@konst $x) "是一个在任何时间都具有值"
               $x "的量."))
       ($lift   $:: (&-> (@-> $a $b)
                         (&Obs $a) (&Obs $b)))
       ($ $ (: (@lift $f $o) "是可观察量, 其值为应用" $f
               "于可观察量" $o "的值的结果."))
       ($lift_2 $:: (&-> (@-> $a $b $c)
                         (&Obs $a) (&Obs $b) (&Obs $c)))
       ($ $ (: (@lift_2 $f $o_1 $o_2)
               "是可观察量, 其值为应用" $f
               "于可观察量" $o_1 "和" $o_2
               "的值之结果."))
       ($date   $:: (&Obs $Date))
       ($ $ (: "日期为" $s "时可观察量" $date
               "的值就是" $s "."))
       ((App $instance (App $Num $a))
        $=> (App $Num (@Obs $a)))
       ($ $ (: "所有的数值运算都可以提升至"
               $Obs "类型. 实现颇为简单, 使用"
               $lift "和" $lift_2 "即可.")))
      (B "图3. 可观察量上的原语"))
   (P "图2给出了对于每个组合子的自然语言描述, 尽管相当精确. "
      "为了做到这一点, 它使用了两个我们必须预先引入的概念: "
      "获取日期和可观察量.")
   (P "我们的语言描述了什么是一个合约. 然而, "
      "合约对于持有者而言的后效依赖于合约获得的日期, "
      "也就是" (Em "获取日期 (acquisition date)")
      ". (使用" (Q "持有者的后效")
      ", 我们指的是合约施加于合约持有者的支付, 权利和义务.) "
      "例如, 合约"
      (Q "receive £100 on 1 Jan 2000 and "
         "receive £100 on 1 Jan 2001")
      "若获取时间晚于2000年1月1日则会价值低上不少, "
      "因为根据定义, 任何在获取日期之前"
      "截止的权利和义务应该直接抛弃.")
   (P "第二个基础概念是" (Em "可观察量")
      ". 一个真实合约往往依赖于可测量的量. "
      "例如, 一个合约可能会说"
      (Q "receive an amount in dollars equal to "
         "the noon Centigrade temperature "
         "in Los Angeles")
      "; 或者是"
      
      )
   (H3. "折扣债券")
   (H3. "可观察量和缩放")
   (H3. "期权合约")
   (H3. "限制合约 (limit contracts)")
   (H3. "总结")
   (H2. "估值")
   (P (MB (set-attr*
           (&Table
            ($ (Eval $dummy) $: (&-> $Contract (App $PR $RR)))
            ("(E1)" (Eval $zero) $= (app $K:script $0))
            ("(E2)" (Eval (&one $k_2)) $= (&exch $k_2))
            ("(E3)" (Eval (&give $c)) $= (&- (Eval $c)))
            ("(E4)" (Eval (&qscale $o $c)) $=
                    (&* (Val $o) (Eval $c)))
            ("(E5)" (Eval (&qand $c_1 $c_2)) $=
                    (&+ (Eval $c_1) (Eval $c_2)))
            ("(E6)" (Eval (&qor $c_1 $c_2)) $=
                    (&max (Eval $c_1) (Eval $c_2)))
            ("(E7)" (Eval (&cond $o $c_1 $c_2)) $=
                    (&if (Val $o) (Eval $c_1) (Eval $c_2)))
            ("(E8)" (Eval (&when $o $c)) $=
                    (&disc (Val $o) (Eval $c)))
            ("(E9)")
            ("(E10)")
            )
           'columnalign "left right center left"))
      (B "图4. 合约的复合性估值语义"))
   (P "我们现在拥有一种丰富的语言, 可用于描述金融合约. "
      "这在人与人之间的交流中已经非常有用" --
      "金融业界正缺乏这样一种精确的表示法. "
      "但除此之外, 精确的描述还适用于各种自动处理. "
      "我们有望从单一的合约描述中, 生成法律文件, "
      "图表, 日程表以及更多其他内容. "
      "然而, 人们可能会对合约提出的最直接的问题是: "
      "它价值几何? 也就是说, 为了拥有这份合约, "
      "我愿意支付多少钱? 这正是我们现在要转向的问题.")
   (P "我们将通过两个" (Q "层次") "来表达合约估值:")
   (P (B "抽象估值语义. ")
      "首先, 我们将展示如何将使用我们的语言编写的任意合约, "
      "转化为一个" (Em "价值过程 (value process)")
      ", 并要展示对于这些过程的一组操作. "
      "这些过程与金融专家所使用的数学和随机机制直接对应.")
   (P (B "具体实现. ")
      "过程是一个抽象的数学值. "
      "为了让计算机利用过程进行计算, "
      "我们必须以某种方式对其进行表示" --
      "这是从抽象语义向具体实现迈出的一步. "
      "一个实现将由一个金融模型以及"
      "某种离散的数值方法组成. "
      "如今有大量不同的金融模型被投入使用 "
      "(例如Black-Scholes, Ho-Lee, 等等); "
      "但在业界, 只有三大类数值方法被广泛使用: "
      "偏微分方程 [Willmot et al., 1993], "
      "蒙特卡洛 [Boyle et al., 1997] "
      "和格方法 [Cox et al., 1997].")
   (P "这种方法强烈让人联想到编译器典型的构建方式. "
      "程序首先被翻译成一种低级但与机器无关的中间语言; "
      "许多优化都会在这个层面上进行; "
      "然后程序被进一步翻译成目标处理器 "
      "(Pentium, Sparc, 诸如此类) 的指令集.")
   (P "以类似的方式, 我们可以将一个合约转换为一个价值过程. "
      "在计算该过程的价值之前, "
      "对这个中间表示应用保持语义不变的优化转换. "
      "最后的这个步骤可以通过解释执行的方式来完成, "
      "或者也可以设想生成特化的代码, 在运行时执行估值.")
   (P "事实上, 我们的抽象语义可以作为判断两个合约是否"
      (Q "相同") "的参考模型. 例如, 存在两个声明:"
      (eqn*
       ((&qand $c_1 (@qor $c_2 $c_3))
        $=
        (&qor (@qand $c_1 $c_2)
              (@qand $c_1 $c_3)))
       ((&give (@qor $c_1 $c_2))
        $=
        (&qor (@give $c_1) (@give $c_2))))
      "实际上, 第一个声明为真, 而第二个声明为假, "
      "但是我们该如何确切地知道呢? "
      "答案: 我们对比它们的估值语义, "
      "将见于第4.6节.")
   
   (H3. "价值过程")
   ((definition #:n "1")
    "类型" $a "上的价值过程" $p
    "是一个从时间到类型为" $a
    "的随机变量的(完全)函数. "
    "随机变量" (app $p $t)
    "描述了" $p "在时间" $t
    "处的可能值. 我们写下非正式的类型定义"
    (MB (&= (&PR $a) (&-> $Date (&RV $a))))
    "(对于语义层次的类型我们使用花体.) "
    "因为我们需要与相同" (Q "潜在空间")
    " (从技术上说, 是filtration) "
    "上的不同过程打交道, "
    "这样的价值过程的更精确描述是"
    (Em "给定filtration的适应随机过程")
    ". 此类过程配备了一套复杂的数学理论 "
    "[Revuz and Yor, 1991, Musiela and Rutkowski, 1997], "
    "但计算机科学家可能对其并不熟悉, "
    "因此我们仅提供非正式的直观的概念. "
    "我们通常将" (Q "价值过程")
    "简称为" (Q "过程")
    ". 不过需要注意的是: "
    (Q "过程") "和" (Q "变量")
    "的含义与它们常规的计算机科学含义完全不同.")
   (P "合约和可观察量都被建模为过程. "
      "其潜在的直觉如下:"
      (Ul (Li "可观察量" $o "的价值过程将时间" $t
              "映射为随机变量, "
              "该随机变量描述了" $o "在" $t
              "时的可能值. 例如, 可观察量"
              (Q "IBM stock price in US$")
              "是一个函数, 其将时间映射为实值随机变量, "
              "该随机变量描述了IBM股价以美元计的可能值.")
          (Li "以货币" $k "表达的合约" $c
              "的价值过程是一个从时间" $t
              "到随机变量的函数, "
              "其描述了于时间" $t
              "获取合约" $c "以货币" $k
              "计的价值."))
      "这些直觉对于理解本文的剩余内容是必要的.")
   (H3. "从合约到过程")
   (P "那么,我们该如何由合约和可观察量得到过程呢? "
      "图4给出了从合约到过程的完整转换, "
      "而图5对可观察量做了同样的处理. "
      "这些图看起来并不令人印象深刻, "
      "但这也正是要义所在! "
      "到目前为止的一切都在为这一点做铺垫; "
      "我们的整个设计都是围绕着提供一个简单, "
      "易处理, 模块化的估值语义这一愿望展开的. "
      "让我们更仔细地看一下图4.")
   
   ))