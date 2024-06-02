#lang racket
(provide prot.html)
(require SMathML)
(define $def (Mi "def"))
(define $<=> (Mo "&hArr;"))
(define $def= (Mover $= $def))
(define ap*
  (case-lambda
    ((f x) (ap f x))
    ((f x . x*) (apply ap* (ap f x) x*))))
(define @ap* (@lize ap*))
(define Type:Int (Mi "Int" #:attr* '((mathvariant "sans-serif"))))
(define Type:Bool (Mi "Bool" #:attr* '((mathvariant "sans-serif"))))
(define @-> (@lize &->))
(define Int:Zero $O:sans-serif)
(define Int:Succ $S:sans-serif)
(define Int:Elim $R:sans-serif)
(define Bool:T $T:sans-serif)
(define Bool:F $F:sans-serif)
(define Bool:Elim $D:sans-serif)
(define Bool:Neg (Mi "neg" #:attr* '((mathvariant "sans-serif"))))
(define Bool:Disj (Mi "disj" #:attr* '((mathvariant "sans-serif"))))
(define Bool:Conj (Mi "conj" #:attr* '((mathvariant "sans-serif"))))
(define Int:Null (Mi "null" #:attr* '((mathvariant "sans-serif"))))
(define Int:It (Mi "it" #:attr* '((mathvariant "sans-serif"))))
(define Int:Iter (Mi "It" #:attr* '((mathvariant "sans-serif"))))
(define (Cite0 #:attr* [attr* '()] . id*)
  `(cite ,attr* . ,(fenced (map Ref id*))))
(define (default-entry:bib-present %entry:bib attr* . xml*)
  (define index (%entry-index %entry:bib))
  (define numbering (format "[~s] " index))
  (define id (%entry-id %entry:bib))
  (keyword-apply
   Div '(#:attr*)
   (list (attr*-set attr* 'class "bibliography" 'id id))
   numbering xml*))
(define (default-entry:bib-cite %entry:bib)
  (define id (%entry-id %entry:bib))
  (define index (%entry-index %entry:bib))
  (define numbering (format "~s" index))
  (define href (string-append "#" id))
  `(a ((href ,href)) ,numbering))
(define (build-%entry:bib id)
  (build-%entry #:local? #f #:class "bibliography" #:id id
                #:present default-entry:bib-present
                #:cite default-entry:bib-cite))
(define (Bib #:id [id #f] #:attr* [attr* '()] . xml*)
  `(,(build-%entry:bib id) ,attr* . ,xml*))
(define $=> (Mo "&rArr;"))
(define $forall (Mo "&forall;"))
(define (set-compact op)
  (set-attr* op 'lspace "0" 'rspace "0"))
(define $=>:compact (set-compact $=>))
(define $=>I (: $=>:compact $I:script))
(define $disj:compact (set-compact $disj))
(define $conj:compact (set-compact $conj))
(define $conjI (: $conj:compact $I:script))
(define $exist (Mo "&exist;"))
(define $forallI (: $forall $I:script))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define (&forall x P)
  (: $forall x $. P))
(define $~> (Mo "&zigrarr;"))
(define $\|- (Mo "&vdash;"))
(define $UnderBar (Mo "&UnderBar;"))
(define (UnderBar x) (__ x $UnderBar))
(define-infix*
  (&disj $disj)
  (&=> $=>)
  (&conj $conj)
  (&~> $~>)
  (&\|- $\|-)
  (&def= $def=)
  (&<=> $<=>)
  
  )
(define LX (: $L:script $X:sans-serif))
(define RX (: $R:script $X:sans-serif))
(define LW (: $L:script $W:sans-serif))
(define RW (: $R:script $W:sans-serif))
(define LC (: $L:script $C:sans-serif))
(define RC (: $R:script $C:sans-serif))
(define (LAM var term)
  (: $lambda var $. term))
(define @LAM (@lize LAM))
(define (subst termA termB var)
  (: termA (bra0 (&/ termB var))))
(define &split:16 (&split 16))
(define (&rule:labelled label . P*)
  (: (apply &rule P*) label))
(define (colmat . x*)
  (&table (map list x*)))
(define $conj1E (: $conj:compact $1 $E:script))
(define $conj2E (: $conj:compact $2 $E:script))
(define $=>E (: $=>:compact $E:script))
(define $forallE (: $forall $E:script))
(define @=> (@lize &=>))
(define prot.html
  (TnTmPrelude
   #:title "证明与类型"
   #:css "styles.css"
   (H1 "证明与类型")
   (H2 "前言")
   (P "这本小书来源于1986至1987年秋季学期于Universit&eacute; Paris VII"
      "开设的一门关于类型" $lambda "演算的短期研究生课程. 它并不意图是"
      "百科全书式的, 例如Church-Rosser定理就没有证明, 而且主题的选取是"
      "相当随意的.")
   (P "关于逻辑的基本常识是必要的, 然而我们也并不会陷入乏味的细节之中. "
      
      )
   (H2 "第1章 涵义, 指称和语义")
   (P "理论计算尚非科学. 许多基本概念亟待澄清, "
      "并且当前该领域的研究遵循一种&quot;婚礼蛋糕&quot;"
      "范式: 例如, 语言设计让人想到Ptolemy天文学&mdash;&mdash;"
      "不断需要更加深入的修正. 然而, 也存在一些有限的主题, "
      "例如复杂度理论和指称语义学, 它们相当远离这种批判.")
   (P "在这样的情况下, 方法论式的评论极其重要, 因为我们不得不"
      "将方法论视为" (Em "战略") "而将具体的结果视为具有"
      (Em "战术") "性质.")
   (P "我们尤其感兴趣的东西可在1900年代的逻辑漩涡的源头找到, "
      "由Frege, Löwenheim, Gödel等名字刻画. 不熟悉逻辑学史"
      "的读者应该参考" (Cite0 "vanHeijenoort") ".")
   (H3 "第1.1节 逻辑中的涵义和指称")
   (P "让我们从一个例子开始. 存在一个乘法的标准过程, 它由输入"
      (Mn "27") "和" (Mn "37") "产生结果" (Mn "999")
      ". 对于这个事实我们可以言称什么?")
   (P "最初的尝试是言称我们拥有了一个" (Em "等式")
      (MB (&= (&c* (Mn "27") (Mn "37")) (Mn "999")))
      "这个等式在数学主流中以言称两边指称相同的整数且" $c*
      "是Cantor的图的意义下的一个" (Em "函数")
      "而获得了含义. (译注: 这里&quot;整数&quot;的原文是"
      "&quot;integer&quot;, 又有原注, 全文的" (Em "integer")
      "将表示" (Em "natural number") ": "
      (&cm $0 $1 $2 $..h) ")")
   (P "这是指称性的方面, 无疑是正确的, 然而它忽略了基本的点.")
   (P "存在一个有限的" (Em "计算") "过程表明这两个指称是相等的. 言称"
      (&c* (Mn "27") (Mn "37")) "等于" (Mn "999")
      "是一种滥用 (这并非什么廉价的哲学&mdash;&mdash;而是一个具体的问题), "
      "因为如果我们所拥有的这两个东西真是" (Em "相同") "的, 那么我们就不会"
      "感到陈述它们的相等性的需要了. 具体地说, 我们在问一个" (Em "问题")
      ", " (&c* 27 37) ", 然后得到了一个答案, " 999 ". 这两个表达式"
      "具有不同的" (Em "涵义") ", 而我们必须" (Em "做") "些什么 (编制"
      "证明或者进行计算, 或是至少查询百科全书) 来表明这两个" (Em "涵义")
      "具有相同的" (Em "指称") ".")
   (P "关于" $c* ", 将其称为一个(作为图的)函数是不正确的, 因为加载了乘法程序"
      "的机器无法容纳下一个无限的图. (译注: 这句话是说" (Em "实无限")
      "是不可能容纳于一个经典计算机器之中的, 当然" (Em "潜无限")
      "的确是可以的.) 因此, 我们不得不总结道, 我们面对的是与这个涵义之问"
      "相关的一种" (Em "有限") "的动力学.")
   (P "尽管指称在很早的阶段就被建模, 涵义则被推向了" (Em "主观主义")
      ", 导致当前的数学对于涵义的处理或多或少沦为了" (Em "句法")
      "操作. 这在我们所要讨论的主题的本质之下并非" (Em "先验")
      ", 而我们可以期待在接下来的几十年里找到一种对于计算的处理, 它结合了"
      "指称语义学 (数学的清晰性) 和句法 (有限的动力学) 的优点. 本书显然坐落于传统之上, "
      "这种传统基于不幸的当前状况: 在" (Em "无限的静态的指称")
      "与" (Em "有限的动态的涵义") "的对立之中, 指称性的一方要远比另一方先进.")
   (P "于是乎, 逻辑中由Frege指出的最根本的一个区分是: 给定一个句子"
      $A ", 存在两种看待它的方式:"
      (Ul (Li "作为" (Em "指令") "的序列, 确定了其" (Em "涵义") ", 例如"
              (&disj $A $B) "的意思是&quot;" $A "或" $B "&quot;, 等等.")
          (Li "作为由这些操作找到的" (Em "理想结果") ": 此即其" (Em "指称") "."))
      "&quot;指称 (denotation)&quot;与&quot;记号 (notation)&quot;相对, 是"
      (Em "被指称") "的什么, 而不是" (Em "进行指称") "的什么. 例如一个逻辑句子的是"
      $t:bold " (true, 真) 或者" $f:bold " (false, 假), 而" (&disj $A $B)
      "的指称可由" $A "和" $B "的指称通过析取的真值表得到.")
   (P "拥有相同涵义的两个句子当然拥有相同的指称, 这是显然的; "
      "但是两个拥有相同指称的句子很少拥有相同的涵义. "
      "例如, 取一个复杂的数学等价" (&<=> $A $B)
      ". 两个句子拥有相同的指称 (它们同时为真), 但肯定拥有不同的涵义; "
      "不然的话, 表明这种等价的意义何在?")
   (P "这个例子允许我们引入一些成组的想法:"
      (Ul (Li "涵义, 句法, 证明;")
          (Li "指称, 真值, 语义, 代数操作."))
      "这是逻辑中的根本对立. 虽然话是这么说, 两方的地位完全不对称!")
   (H4 "第1.1.1小节 代数传统")
   (P "这个传统 (早在Frege的时代之前就由Boole开始) 基于对Ockham剃刀的激进应用: "
      "我们相当轻易地舍弃了涵义, 只考虑指称. 澄清这种对于逻辑的肢解的合理性的是其可操作的一面: "
      (Em "it works!"))
   (P "建立了这种传统的主导性地位的基本转折点在于1916年的"
      "L&ouml;wenheim定理. 如今, 人们可以将模型论视为这种"
      "业已古老的认识论选择所带来的丰富回报. 实际上, "
      "从指称的角度, 即从操作的" (Em "结果") "的角度来看, "
      "如此考虑逻辑, 我们发现了一种有些特殊的代数, "
      "但它允许我们去检视对于更加传统的代数而言并不熟悉的操作. "
      "实际上, 避免局限于" (Em "等式性") "变体而考虑一般的"
      (Em "可定义") "结构也是可以的. 因此, "
      "模型论常以fruitful的方式为代数的想法和方法注入了活力.")
   (H4 "第1.1.2小节 句法传统")
   (P "另一方面, " (Q "全然忘记指称而专注于涵义")
      "是不可能的, 这出于简单的原因, 即涵义包含指称, "
      "至少是隐式地包含. 因此, 这并非对称的情况. "
      "实际上, 几乎不存在统一的句法观点, 因为我们从未能够赋予神秘的"
      (Em "涵义") "以一种操作性的涵义. 关于涵义唯一可感知的现实"
      "在于其被写下来的方式, 即形式化; 但是, 形式化仍然是一种"
      "不够理想的研究对象, 不具备真切的结构, 就像一片"
      (Em "soft camembert") ".")
   (P "这难道意味着纯粹句法的方法毫无讨论的价值吗? "
      "当然不是, 1934年Gentzen的著名定理表明在句法层面上"
      "逻辑具有某些深远的对称性 (由" (Em "切消")
      "表达). 然而, 这些对称被句法的不完美之处掩盖了. "
      "换句话说, 它们不是句法的对称, 而是涵义的对称. "
      "但是, 要想更进一步, 我们必须要将那些对称表达为"
      "句法的性质, 而结果并不是很美丽.")
   (P "那么, 总结我们对于这种传统的观点, 它总是在寻找其根本概念, "
      "也就是说, 涵义和句法之间的操作性区别. 或者把话说得更具体些, "
      "它意在寻找深刻的句法的几何形状上的" (Em "不变量")
      ": 其中可以找到涵义.")
   (P "被称为" (Q "句法性") " (因为没有更加高贵的名字了) "
      "的传统, 从没能达到其对手的高度. 近些年来, 也就是说代数传统"
      "繁荣发展的时期, 句法传统不值一提, 并且无疑可能将"
      "因为缺少问题和方法论而在一二十年内消失. "
      "这个灾难因为计算机科学 (伟大的句法操纵装置) "
      "得以避免, 其提出了一些非常重要的理论问题.")
   (P "其中一些问题 (例如关于算法复杂度的) 似乎更多地需要"
      "逻辑的字面而非逻辑的灵魂. 另一方面, 一切和程序的"
      "正确性和模块性有关的问题都深刻诉诸于句法传统, 诉诸于"
      (Em "证明论") ". 我们被引导至从可追溯到1930年的"
      "Herbrand的根本性定理开始对于证明论进行修订. "
      "这个修订给那些一度被认为永远固定下来了的领域带来了新的光亮, "
      "那里曾在很长一段时间内盛行着墨守成规.")
   (P "在句法逻辑传统与计算机科学之间的交流中, "
      "人们可以在计算的一侧等待着新的语言和新的机器. "
      "但是, 在逻辑的一侧 (也就是本书的主要作者所在的领域), "
      "人们终于可以期望用上一直被残忍忽视的概念基础了.")
   (H3 "第1.2节 两种语义传统")
   (H4 "第1.2.1小节 Tarski")
   (P "这种传统以极端的陈词滥调为人所知: 联结词"
      (Q $disj) "被翻译为" (Q "或") ", 诸如此类. "
      "这种解释没有告诉我们关于逻辑联结词的特别突出的东西: "
      "它显然的抱负缺乏是其可操作性的潜在理由. "
      "我们只关心句法的句子 (封闭表达式) 的指称, "
      $t:bold "或" $f:bold ".")
   (Ol (Li "对于原子句子, 我们假定其指称已然知晓; 例如:"
           (Ul (Li (&= (&+ $3 $2) $5) "具有指称"
                   $t:bold ".")
               (Li (&= (&+ $3 $3) $5) "具有指称"
                   $f:bold ".")))
       (Li "表达式"
           )
       )
   (H2 "第2章 自然演绎")
   (P "正如我们之前所言, 句法性的观点展现了逻辑学的某种深远的对称. Gentzen的"
      "相继式演算以特别令人满意的方式完成了这种观点. 不幸的是, "
      "其计算上的意义在某种程度上被其句法上的复杂性所掩盖, 尽管这种复杂"
      "与本质无关, 但是从来没有真正被克服. 这就是为什么我们在处理相继式演算之前"
      "要先呈现Prawitz的自然演绎.")
   (P "自然演绎在某种意义上是有点悖论性的系统. 其局限于直觉主义的情形 "
      "(在古典情形下没有什么良好的性质), 但是它也只是对于语言的"
      (tu0 $conj:compact $=>:compact $forall) "部分令人满意而已: "
      "我们将对于" $disj "和" $exist "的考虑推迟到第10章. "
      "尽管如此, 析取和存在是两种最" (Em "典型") "的直觉主义的联结词!")
   (P "自然演绎的基本想法是一种不对称: 一个证明大致上是一个树状的结构, "
      "其有一个或多个假设 (也可能没有), 但是只有一个结论. 这种演算的"
      "深刻对称在于相互精准匹配的" (Em "引入") "和" (Em "消去")
      "规则. 我们应该顺便观察到这样的事实, 对于这样一种树状结构, "
      "我们总是可以唯一地确定哪一条规则是" (Em "最后") "被使用的. "
      "如果有多个结论的话, 那是无法做到的.")
   (H3 "第2.1节 演算")
   (P "我们将使用记号"
      (MB (&Table ($..v) ($A)))
      "来指对于" $A "的一个" (Em "演绎(deduction)") ", 即停止于" $A
      ". 这个演绎会被写成一个有限的树的形式, 并且树的叶子会被标记以"
      "句子. [译注: 这个指的不是带标签的树 (labelled tree).] "
      "对于这些句子, 存在两种可能的状态, " (Em "死") "或"
      (Em "生") ". [译注: 更准确地说, 是对于这些叶子.]")
   (P "在通常状态下, 一个句子是活着的, 也就是说其在证明中"
      "还处于活跃的地位: 我们称其是一个" (Em "假设(hypothesis)")
      ". 典型的情况由自然演绎的第一条规则刻画, 其允许我们构造"
      "一个仅包含一个句子的演绎:"
      (MB $A)
      "这里的" $A "既是叶子也是根; 从逻辑上说, 我们推出了" $A
      ", 但这只是因为" $A "是被假定成立的!")
   (P "现在位于叶子位置的句子是可以死去的, 当其不再于证明中活跃时. "
      "死去的句子可以通过杀死活着的句子得到. 最典型的例子是"
      $=> "的引入规则:"
      (MB (&rule:labelled
           $=>I
           (colmat (bra0 $A) $..v $B)
           (&=> $A $B)))
      "以上的演绎必须按照如下方式理解: 我们从" $B
      "的一个演绎开始, 在这个" $B "的演绎中, 我们选取了特定数目的"
      $A "的出现作为" (Em "假设") " (这个数字可以是任意的: "
      (&cm $0 $1 250 $..h) "), 然后我们构造了一个新的演绎, 其结论是"
      (&=> $A $B) ", 但是其中的这些选定的" $A "的出现都会被"
      (Em "discharged") ", 即被杀死. 可能还存在其他" $A
      "的出现我们选择不去discharge.")
   (P "这条规则很好地刻画了树状的记号的样貌: 知道" (Em "何时")
      "一个假设被discharged是重要的, 因而有必要记录这种信息. "
      "但是如果在以上的例子中这样做, 这意味着我们需要将被叉掉的"
      $A "和" $=>I "规则那行连起来; 但是连起来之后这个东西"
      "就不再是之前我们所考虑的真正的树结构了!")
   (H4 "第2.1.1小节 规则")
   (Ul (Li (Em "假设") ": " $A)
       (Li (Em "引入") ":"
           (MB (&split:16
                (&rule:labelled
                 $conjI
                 (colmat $..v $A)
                 (colmat $..v $B)
                 (&conj $A $B))
                (&rule:labelled
                 $=>I
                 (colmat (bra0 $A) $..v $B)
                 (&=> $A $B))
                (&rule:labelled
                 $forallI
                 (colmat $..v $A)
                 (&forall $xi $A)))))
       (Li (Em "消去") ":"
           (MB (&split:16
                (&rule:labelled
                 $conj1E
                 (colmat $..v (&conj $A $B))
                 $A)
                (&rule:labelled
                 $conj2E
                 (colmat $..v (&conj $A $B))
                 $B)
                (&rule:labelled
                 $=>E
                 (colmat $..v $A)
                 (colmat $..v (&=> $A $B))
                 $B)
                (&rule:labelled
                 $forallE
                 (&forall $xi $A)
                 (subst $A $a $xi))))
           "在传统上" $=>E "被称为" (Em "modus ponens") "."))
   (P "一些注记:" (Br)
      "所有的规则, 除了" $=>I ", 都保持the stock of hypotheses: "
      "例如, 以上以" $=>E "作结的演绎, 其假设是两个立即子演绎的假设." (Br)
      "出于众所周知的逻辑原因, 有必要将" $forallI "限制于变量" $xi
      "不能在任何假设中自由出现的情形 (但是, 从另一方面说, 其可以在"
      "死去的叶子中自由出现)." (Br)
      "这个系统的基础对称在于" (Em "引入/消去") "对称, 其代替了"
      "不能在这种上下文中被实现的" (Em "假设/结论") "对称.")
   (P "原注: 变量" $xi "属于" (Em "对象语言")
      " (其可能代表一个数字, 一个数据记录, 一个事件). 我们为"
      $lambda "演算的变量保留符号" (&cm $x $y $z)
      ", 下一节我们将引入这个概念. [译注: 元语言和对象语言是相对的概念.]")
   (H3 "第2.2节 计算上的意义")
   (P "我们将以Heyting语义的角度重新检视自然演绎系统. "
      "我们固定原子公式的解释以及量词的范围. 一个公式" $A
      "将被看成是其所有可能的演绎构成的集合; 不说&quot;" $delta
      "证明了" $A "&quot;, 我们说&quot;" (∈ $delta $A) "&quot;.")
   (P "然后自然演绎的规则就以构造函数的特别方式显现: 以"
      (&cm $B_1 $..h $B_n) "为假设的一个演绎" $A
      ", 可以被看成是一个函数" (ap $t (li0 $x_1 $..h $x_n))
      ", 其联系" (∈ $b_i $B_i) "以结果"
      (∈ (ap $t (li0 $b_1 $..h $b_n)) $A)
      ". 实际上, 为了使得这种对应精确, 我们需要处理"
      (Em "parcels of hypotheses")
      ": 相同的公式" $B "可能在假设中出现多次, 而在相同的parcel"
      "中的两次" $B "的出现应该对应于相同的变量.")
   (P "以上的内容听起来似乎有些神秘, 但是通过一些例子, "
      "其很快就会变得清晰起来.")
   (H4 "第2.2.1小节 规则的解释")
   (Ol (Li "仅由一个假设" $A "构成的演绎由表达式" $x
           "表示, 其中" $x "是代表" $A "的一个元素的变量. 之后, 若还有"
           $A "的出现, 我们将选择相同的" $x ", 或者另外一个变量, "
           "这取决于这些出现是否在相同的parcel之中.")
       (Li "如果一个演绎由两个演绎通过" $conjI "得到, 并且这两个演绎分别对应于"
           (ap $u (li0 $x_1 $..h $x_n)) "和"
           (ap $v (li0 $x_1 $..h $x_n)) ", 那么我们联系该演绎以序对"
           (tupa0 (ap $u (li0 $x_1 $..h $x_n)) (ap $v (li0 $x_1 $..h $x_n)))
           ", 这不过是因为对于合取的证明是一个" (Em "序对")
           ". 我们刚才使得" $u "和" $v "依赖于相同的变量; "
           "的确, 对于" $u "和" $v "的变量的选择是相互关联的, "
           "因为某些parcels of hypotheses应该是同一个.")
       (Li "如果一个演绎以" $conj1E "作结, 并且" (ap $t (li0 $x_1 $..h $x_n))
           "与立即子演绎相对应, 那么我们就将我们的证明与"
           (ap $pi^1 (ap $t (li0 $x_1 $..h $x_n)))
           "相关联. 此即" (Em "第一投影") ", 这不过是因为" $t
           "作为对于合取的证明应该是一个序对. 类似地, " $conj2E
           "和" (Em "第二投影") $pi^2 "有关." (Br)
           "尽管不是很形式化, 但是考虑以下基础等式是必要的:"
           (MB (&cm (&= (ap $pi^1 (tupa0 $u $v)) $u)
                    (&= (ap $pi^2 (tupa0 $u $v)) $v)
                    (&= (tupa0 (ap $pi^1 $t) (ap $pi^2 $t)) $t)))
           "这些等式是逻辑学和计算机科学之间的对应的本质.")
       (Li "如果一个演绎以" $=>I "作结, 令" $v "是与立即子演绎相关联的项; "
           "这个立即子演绎在parcels of hypotheses的层次上是被无歧义地"
           "确定的, 这是在说一整个" $A "-parcel都被discharged了. 如果"
           $x "是一个与该parcel相关联的变量, 那么我们就有了一个函数"
           (ap $v (li0 $x $x_1 $..h $x_n))
           ". 我们将我们的演绎与函数" (ap $t (li0 $x_1 $..h $x_n))
           "相关联, 其将每个" $A "的参数" $a "映射至"
           (ap $v (li0 $a $x_1 $..h $x_n)) ". 我们所用的记号是"
           (LAM $x (ap $v (li0 $x $x_1 $..h $x_n)))
           ", 其中" $x "是被绑定的变量." (Br)
           "我们应该观察到" (Em "绑定(binding)")
           "对应于" (Em "discharge") ".")
       (Li "以" $=>E "作结的演绎的情况由考虑两个函数"
           (ap $t (li0 $x_1 $..h $x_n)) "和"
           (ap $u (li0 $x_1 $..h $x_n))
           "处理, 其分别对应于两个立即子演绎. "
           )
       )
   (H2 "第3章 Curry-Howard同构")
   (P "我们已经看到Heyting的想法在自然演绎的框架之下执行得非常好. "
      "我们将建立一个类型化项 (typed term) 的" (Em "形式")
      "系统以讨论藏在证明背后的泛函对象. 这个系统的意义是"
      "藉由我们已经写下的泛函方程给出的. 实际上, "
      "这些方程可以用两种不同的方式阅读, 这再次强化了"
      "涵义和指称之间的二分:"
      (Ul (Li "作为定义了项的相等性的" (Em "等式")
              ", 换言之即指称的相等 (" (Em "静态")
              "观点).")
          (Li "作为" (Em "重写") "规则, 其允许我们通过规约至规范形式来计算项. "
              "这是一种操作性的, " (Em "动态")
              "的观点, 而且对于逻辑学的此方面而言, "
              "这是唯一真正带来丰富结果 (fruitful) 的观点.")))
   (P "当然, 第二种观点和第一种比起来有欠发展, 至少对于逻辑学是这样! 例如, 程序的"
      (Em "指称") "语义 (例如, Scott语义) 有很多: 对于这种语义, 程序的执行过程"
      "中没有东西会发生变化. 另一方面, 几乎没有任何打磨精致的程序的" (Em "操作")
      "语义 (我们排除只是粗糙重述迈向规范形式的步骤的" (Em "ad hoc")
      "语义). 建立真正的算法的操作语义或许是计算机科学中最重要的问题.")
   (P "类型和命题之间的对应在" (Cite0 "Howard") "之中建立起来.")
   (H3 "第3.1节 lambda演算")
   (H4 "第3.1.1小节 类型")
   (P "当我们以Heyting的精神思考证明, 公式就成为了" (Em "类型")
      ". 具体来说, 类型如下:"
      (Ol (Li "原子类型" (&cm $T_1 $..h $T_n) "是类型.")
          (Li "如果" $U "和" $V "是类型, 那么"
              (&c* $U $V) "和" (&-> $U $V) "是类型.")
          (Li "(暂时)仅有的类型都是通过1和2得到的.")))
   (P "这与命题演算的" (tu0 $conj:compact $=>:compact)
      "相对应: 原子命题写作" $T_i ", " $conj "成为" $c*
      ", " $=> "成为" $-> ".")
   (H4 "第3.1.2小节 项")
   (P "证明就成了" (Em "项") ". 更准确地说, " $A
      "的一个证明 (" $A "作为公式) 成为一个"
      (Em "具有类型" $A "的项") " (" $A
      "作为类型). 具体来说, 项如下:"
      (Ol (Li "变量" (&cm (_^ $x $0 $T) $..h (_^ $x $n $T) $..h)
              "是具有类型" $T "的项.")
          (Li "如果" $u "和" $v "分别是具有类型" $U "和" $V
              "的项, 那么" (tupa0 $u $v) "是具有类型" (&c* $U $V)
              "的项.")
          (Li "如果" $t "是具有类型" (&c* $U $V) "的项, 那么"
              (ap $pi^1 $t) "和" (ap $pi^2 $t)
              "分别是具有类型" $U "和" $V "的项.")
          (Li "如果" $v "是具有类型" $V "的一个项并且"
              (_^ $x $n $U) "是一个具有类型" $U "的变量, 那么"
              (LAM (_^ $x $n $U) $v) "是一个具有类型"
              (&-> $U $V) "的项. 一般而言, 我们将假定我们已经"
              "解决了绑定变量的选取以及替换问题, 通过这样或那样的方法, "
              "这允许我们不需要考虑绑定变量的名字, 想法在于"
              "绑定变量没有individuality.")
          (Li "如果" $t "和" $u "分别是具有类型" (&-> $U $V)
              "和" $U "的项, 那么" (ap $t $u) "是具有类型"
              $V "的项.")))
   (H3 "第3.2节 指称上的意义")
   (P "类型代表了某种正在被讨论的对象. 例如, 一个具有类型"
      (&-> $U $V) "的对象是一个从" $U "到" $V "的函数, 一个具有类型"
      (&c* $U $V) "的对象是一个序对, 由一个" $U "的对象和一个"
      $V "的对象构成. 原子类型的含义是不重要的, 其依赖于上下文.")
   (H3 "第3.3节 操作性的意义")
   (P "一般来说, " (Em "项") "代表" (Em "程序")
      ". 程序的目的在于计算其指称, 或者至少是将指称置于一种便利的形式. 程序的"
      (Em "类型") "被视为一种" (Em "描述(specification)")
      ", 即程序(抽象地)做了什么. 先验地说这是一种具有形式&quot;"
      "这个程序计算两个整数之和&quot;的评论.")
   (H3 "第3.4节 转换")
   (P "一个项被称为" (Em "规范的") ", 如果没有其子项具有形式:"
      (MB (&split:16
           (ap $pi^1 (tupa0 $u $v))
           (ap $pi^2 (tupa0 $u $v))
           (ap (@LAM (_^ $x $n $U) $v) $u)))
      
      )
   (H3 "第3.5节 对于同构的描述")
   (P ""
      )
   (H2 "第4章 规范化定理 (正则化定理)")
   (P "本章关心的是确保类型化" $lambda
      "演算能够在计算上表现良好的两个结果. "
      (Em "规范化定理") "提供了规范形式的存在性, 而与此同时"
      (Em "Church-Rosser") "性质保证了其唯一性. 实际上, "
      "对于后者而言, 我们仅是简单陈述其内容但不加以证明, "
      "因为其确非类型论的内容, 而且在许多文献中都有"
      "很好的讲解了, 例如" (Cite0 "Barendregt") ".")
   (P "规范化定理具有两种形式:"
      (Ul (Li (Em "弱形式") " (存在" (Em "某个")
              "规范化的能够终止的策略), 这将在本章中进行证明.")
          (Li (Em "强形式") " (" (Em "所有可能的")
              "规范化策略都将终止), 这在第6章中证明.")))
   (H3 "第4.1节 Church-Rosser性质")
   (P "这个性质表达了规范形式的唯一性, 这独立于其存在性. "
      "实际上, 它对于其他演算来说也有意义, 例如" (Em "无类型")
      $lambda "演算, 在无类型的" $lambda "演算里规范化定理不成立.")
   ((theorem)
    "如果" (&~> $t (&cm $u $v)) ", 那么我们可以找到" $w
    "使得" (&~> (&cm $u $v) $w) ".")
   ((corollary)
    "一个项" $t "至多只有一个规范形式.")
   ((proof)
    "如果" (&~> $t (&cm $u $v)) ", 其中" $u "和" $v
    "是规范形式, 那么存在某个" $w "使得"
    (&~> (&cm $u $v) $w) ", 但是既然" $u "和" $v
    "已经是规范的了, 它们就不可能被规约至除了自身以外的项, 所以"
    (&= $u $w $v) ".")
   (P "Church-Rosser定理"
      )
   (H3 "第4.2节 弱规范化定理")
   (P "这个结果陈述了每个项的规范形式的" (Em "存在性")
      ", 当然它必然也是唯一的. 其立即的推论在于指称相等的"
      (Em "可判定性") ". "
      )
   (H3 "第4.3节 弱规范化定理的证明")
   
   (H3 "第4.4节 强规范化定理")
   
   (H2 "第5章 相继式演算")
   (P "归功于Gentzen, " (Em "相继式演算") "是对于逻辑学的对称的最漂亮刻画. "
      "其与自然演绎有着诸多类似之处, 但是不局限于直觉主义的情形.")
   (P "这种演算一般来说被计算机科学家所忽略, 尽管其刻画了某些基础性的想法: "
      "例如, Prolog这种语言是对于相继式演算的部分实现, 而自动定理证明领域使用的"
      "&quot;tableaux&quot;方法不过是这种演算的一个特殊情形. 换言之, "
      "它浑然不觉地被许多人使用, 但是混杂了" (Em "控制")
      "特性, 即编程设备 (programming device). 使得这一切运作的是"
      "相继式演算及其深刻的对称性, 而不是什么特别的技巧. 所以说, "
      "若是不知道相继式演算的微妙之处, 是很难考虑, 例如, Prolog的理论的.")
   (P "从算法的角度来说, 相继式演算没有" (Em "Curry-Howard同构")
      ", 这是因为太多书写相同证明的方式. 这阻止了我们将其当作类型化"
      $lambda "演算使用, 尽管我们瞥见了某种类似物的深层结构, 可能与"
      "并行有关. 但是, 它需要对于句法的新方法, 例如带有多个结论的自然演绎.")
   (H3 "第5.1节 演算")
   (H4 "第5.1.1小节 相继式")
   (P "一个" (Em "相继式") "是一个表达式" (&\|- (UnderBar $A) (UnderBar $B))
      ", 其中" (UnderBar $A) "和" (UnderBar $B) "是公式的有限序列"
      (&cm $A_1 $..h $A_n) "和" (&cm $B_1 $..h $B_m) ".")
   (P "幼稚的(指称性)解释在于" $A_i "的合取推出了" $B_j "的析取, 特别地"
      (Ul (Li "如果" (UnderBar $A) "为空, 那么相继式就断言了" $B_j "的析取;")
          (Li "如果" (UnderBar $A) "为空并且" (UnderBar $B) "就是" $B_1
              ", 那么它断言了" $B_1 ";")
          (Li "如果" (UnderBar $B) "为空, 那么它断言了"
              $A_i "的合取的否定;")
          (Li "如果" (UnderBar $A) "和" (UnderBar $B)
              "均为空, 那么它断言了一个矛盾.")))
   (H4 "第5.1.2小节 结构规则")
   (P "这些规则, 似乎什么也没说, 强制规定了管理&quot;槽&quot;的一种特定方式, "
      "在槽中人们书写公式. 规则如下, 它们是:"
      (Ol (Li (Em "交换") "规则"
              (MB (&split:16
                   (&rule:labelled
                    LX
                    (&\|- (&cm (UnderBar $A) $C $D (UnderBar $A^))
                          (UnderBar $B))
                    (&\|- (&cm (UnderBar $A) $D $C (UnderBar $A^))
                          (UnderBar $B)))
                   (&rule:labelled
                    RX
                    (&\|- $A (&cm (UnderBar $B) $C $D (UnderBar $B^)))
                    (&\|- $A (&cm (UnderBar $B) $D $C (UnderBar $B^))))))
              "这些规则表达了逻辑的" (Em "交换性")
              ", 通过允许符号" $\|- "每一边的公式进行置换.")
          (Li (Em "削弱") "规则"
              (MB (&split:16
                   (&rule:labelled
                    LW
                    (&\|- (UnderBar $A) (UnderBar $B))
                    (&\|- (&cm (UnderBar $A) $C) (UnderBar $B)))
                   (&rule:labelled
                    RW
                    (&\|- (UnderBar $A) (UnderBar $B))
                    (&\|- (UnderBar $A) (&cm $C (UnderBar $B))))))
              "正如名字所暗示的那样, 其允许将相继式代替以一个更弱的相继式.")
          (Li (Em "收缩") "规则"
              (MB (&split:16
                   (&rule:labelled
                    LC
                    (&\|- (&cm (UnderBar $A) $C $C) (UnderBar $B))
                    (&\|- (&cm (UnderBar $A) $C) (UnderBar $B)))
                   (&rule:labelled
                    RC
                    (&\|- (UnderBar $A) (&cm $C $C (UnderBar $B)))
                    (&\|- (UnderBar $A) (&cm $C (UnderBar $B))))))
              "表达了合取与析取的幂等性.")))
   (P "事实上, 与流行的认知相反, 这些规则才是整个演算里最重要的部分. "
      "这是因为, 在还没有写下任何一个逻辑符号之前, 我们实际上就已经"
      "能够确定逻辑操作的未来行为. 尽管这些规则从指称的角度来看是显然的, "
      "但是我们应该从操作的角度仔细检视这些规则, 特别是" (Em "收缩") ".")
   (P ""
      )
   (H4 "第5.1.3小节 直觉主义的情形")
   (H4 "第5.1.4小节 &quot;相等&quot;群")
   (Ol (Li ""
           )
       )
   (H4 "第5.1.5小节 逻辑规则")
   (P "传统上人们认为逻辑是一种形式游戏, 一连串或多或少有些随意的公理和规则. "
      "相继式演算 (当然自然演绎也是) 表明实际上完全不是这么一回事: "
      "人们可以自娱自乐地发明他们自己的逻辑操作, 但是这些逻辑操作不得不"
      "尊重左/右对称, 否则的话他们发明的就只是一种毫无趣味的逻辑暴行. "
      "具体来说, 对称指的是我们可以" (Em "消除") "切规则.")
   (Ol (Li ""
           )
       )
   (H3 "第5.2节 无切系统的一些性质")
   (H2 "第6章 强规范化定理")
   (P "本章我们将证明简单类型" $lambda "演算的强规范化定理, 但是既然"
      )
   (H2 "第7章 G&ouml;del的系统T")
   (H3 "第7.1节 演算")
   (H4 "第7.1.1小节 类型")
   (P "第3章中我们允许额外的常量类型; 现在我们将描述两种这样的类型, 分别是"
      Type:Int " (整数) 和" Type:Bool " (布尔).")
   (P "译注: 根据第1章的原注, 本书的整数都是指自然数.")
   (H4 "第7.1.2小节 项")
   (P "除了常见的五种, 对于" Type:Int "和" Type:Bool "存在特定的scheme. "
      "我们保持了使用" (Em "引入/消去") "术语, 因为这些scheme还将出现在之后的"
      $F:bold "之中.")
   (Ol (Li Type:Int (Em "引入") ":"
           (Ul (Li Int:Zero "是一个具有类型" Type:Int "的常量;")
               (Li "如果" $t "具有类型" Type:Int ", 那么"
                   (ap Int:Succ $t) "具有类型" Type:Int ".")))
       (Li Type:Int (Em "消去") ": 如果" (&cm $u $v $t)
           "分别具有类型" (&cm $U (&-> $U (@-> Type:Int $U)) Type:Int)
           ", 那么" (ap* Int:Elim $u $v $t) "具有类型" $U ".")
       (Li Type:Bool (Em "引入") ": " Bool:T "和" Bool:F
           "具有类型" Type:Bool ".")
       (Li Type:Bool (Em "消去") ": 如果" (&cm $u $v $t)
           "分别具有类型" (&cm $U $U Type:Bool)
           ", 那么" (ap* Bool:Elim $u $v $t) "具有类型" $U "."))
   (H4 "第7.1.3小节 意图的含义")
   (Ol (Li Int:Zero "和" Int:Succ "分别是零元和后继函数.")
       (Li Int:Elim "是递归算子: " (&= (ap* Int:Elim $u $v $0) $u) ", "
           (&= (ap* Int:Elim $u $v (@+ $n $1))
               (ap* $v (@ap* Int:Elim $u $v $n) $n)) ".")
       (Li Bool:T "和" Bool:F "是真值.")
       (Li Bool:Elim "是&quot;if ... then ... else&quot;: "
           (&= (ap* Bool:Elim $u $v Bool:T) $u) ", "
           (&= (ap* Bool:Elim $u $v Bool:F) $v) "."))
   (P "译注: 在某种意义上这里有点符号滥用.")
   (H4 "第7.1.4小节 转换")
   (P "除了经典的redex, 我们加入了:"
      (MB (&~> (ap* Int:Elim $u $v Int:Zero) $u))
      (MB (&~> (ap* Int:Elim $u $v (@ap Int:Succ $t))
               (ap* $v (@ap* Int:Elim $u $v $t) $t)))
      (MB (&~> (ap* Bool:Elim $u $v Bool:T) $u))
      (MB (&~> (ap* Bool:Elim $u $v Bool:F) $v)))
   (H3 "第7.2节 规范化定理")
   (P "在" $T:bold "中, 所有的规约序列都是有限的, 并将导向相同的规范形式.")
   ((proof)
    
    )
   (H3 "第7.3节 表达力: 例子")
   (H4 "第7.3.1小节 布尔")
   (P "典型的例子是逻辑联结词:"
      (MB (&split:16
           (&= (app Bool:Neg $u)
               (ap* Bool:Elim Bool:F Bool:T $u))
           (&= (appl Bool:Disj $u $v)
               (ap* Bool:Elim Bool:T $v $u))
           (&= (appl Bool:Conj $u $v)
               (ap* Bool:Elim $v Bool:F $u))))
      "例如, " (&~> (appl Bool:Disj Bool:T $x) Bool:T)
      "和" (&~> (appl Bool:Disj Bool:F $x) $x)
      ". 但是, 从另一方面来说, 如果遇到了表达式"
      (appl Bool:Disj $x Bool:T)
      ", 那么我们就不知道该做什么了.")
   (P (B "问题.") " 有没有一种可能定义另外一种析取, 但它是对称的?")
   (P "在第9.3.1小节的时候, 我们将看到, 根据语义方法, 可以说明不存在具有类型"
      (&-> (&cm Type:Bool Type:Bool) Type:Bool) "的项" $G "满足"
      (MB (&split:16
           (&~> (ap $G (tupa0 Bool:T $x)) Bool:T)
           (&~> (ap $G (tupa0 $x Bool:T)) Bool:T)
           (&~> (ap $G (tupa0 Bool:F Bool:F)) Bool:F))))
   (H4 "第7.3.2小节 整数")
   (P "首先我们必须表示整数: 选择用"
      (&= (OverBar $n) (ap (^ Int:Succ $n) Int:Zero))
      "来表示整数" $n "是显然的.")
   (P "经典的函数可以用简单的递推关系定义 [虽然译者觉得应该说是递归?]. "
      "让我们给出加法的例子: 我们需要从我们已经熟知的定义方程开始:"
      (MB (&split:16
           (&= (&+ $x Int:Zero) $x)
           (&= (&+ $x (ap Int:Succ $y))
               (app Int:Succ (&+ $x $y))))))
   (P "考虑"
      (&= (ap $t (li0 $x $y))
          (ap* Int:Elim $x
               (@LAM (^ $z Type:Int)
                     (LAM (^ $z^ Type:Int)
                          (ap Int:Succ $z)))
               $y))
      ":"
      (MB (&split:16
           (&~> (ap $t (li0 $x Int:Zero)) $x)
           (&~> (ap $t (li0 $x (ap Int:Succ $y)))
                (ap* (@LAM (^ $z Type:Int)
                           (LAM (^ $z^ Type:Int)
                                (ap Int:Succ $z)))
                     (@ap $t (li0 $x $y)) $y)
                (ap Int:Succ (ap $t (li0 $x $y))))))
      "这表明我们可以将" (ap $t (li0 $x $y))
      "当作" (&+ $x $y) "的一个定义.")
   (P "通过类似这种风格的简单练习, 我们可以自娱自乐地"
      "定义乘法, 幂, 前继, 等等.")
   (P "整数上的谓词当然也可以被定义, 例如"
      (MB (&split:16
           (&= (app Int:Null Int:Zero) Bool:T)
           (&= (app Int:Null (ap Int:Succ $x)) Bool:F)))
      "给出了"
      (MB (&def= (app Int:Null $x)
                 (ap* Int:Elim Bool:T
                      (@LAM (^ $z Type:Bool)
                            (LAM (^ $z^ Type:Int)
                                 Bool:F)) $x)))
      "这允许我们将一个特征函数 (类型" Type:Int
      ") 转换为一个谓词 (类型" Type:Bool ").")
   (P "这些例子都没有严肃地运用高阶类型. 然而, 随着在递归中使用的类型的增加, "
      "越来越多的函数变得可以表达. 例如, 如果" $f "具有类型"
      (&-> Type:Int Type:Int) ", 我们可以定义类型为"
      (&-> Type:Int Type:Int) "的" (app Int:It $f) "为"
      (MB (&= (ap (app Int:It $f) $x)
              (ap* Int:Elim (OverBar $1)
                   (@LAM (^ $z Type:Int)
                         (LAM (^ $z^ Type:Int)
                              (ap $f $z))) $x)))
      "那么" (ap (app Int:It $f) (OverBar $n)) "就是"
      (ap $f^n (OverBar $1)) "了. 作为具有类型"
      (&-> (@-> Type:Int Type:Int) (@-> Type:Int Type:Int))
      "的对象, 函数" Int:It "是:"
      (MB (LAM (^ $x (&-> Type:Int Type:Int))
               (app Int:It $x))))
   (P "很容易看出来, 通过某个合理的函数" $f_0
      "的有限迭代, 我们可以超越每个原始递归函数. 例如, 给定"
      $n "返回" (ap (^ Int:It $n) $f_0)
      "的函数 (Ackermann函数) 就要比所有原始递归函数增长得更快.")
   (P "译注: 上面这段话不是很理解, 因为我不知道什么是" $f_0 ".")
   (P "这种函数在" $T:bold "中很容易定义, 只要我们使用一个复杂类型上的递归, 例如"
      (&-> Type:Int Type:Int) ": 取"
      (ap* Int:Elim $f_0
           (@LAM (^ $x (&-> Type:Int Type:Int))
                 (LAM (^ $z Type:Int)
                      (app Int:It $x))) $y)
      ", 其对于" (&= $y Int:Zero) "将被规范化为" $f_0
      ", 对于" (OverBar $n) "将被规范化为"
      (ap (^ Int:It $n) $f_0) ".")
   (P "为了给本小节作结, 我们应该指出" (ap* Int:Elim $u $v $t)
      "中的" $v "的第二个参数实际上经常是不用的. 可能有人更倾向于使用迭代子"
      Int:Iter "而不是" Int:Elim ", 其应用于具有类型" $T "的" $u
      ", 具有类型" (&-> $T $T) "的" $v ", 以及具有类型" Type:Int
      "的" $t ", 而规则是:"
      (MB (&~> (ap* Int:Iter $u $v (@ap Int:Succ $t))
               (ap $v (@ap* Int:Iter $u $v $t)))))
   (P "满足等式"
      )
   (H3 "第7.4节 表达力: 结果")
   
   (H2 "第8章 coherence空间")
   (P "指称语义学领域最早的工作是由[Scott69]对于无类型" $lambda
      "演算完成的, 自那时起已经又有很多内容了. 他的方法由"
      (Em "连续性") "刻画, 即保持有向join. "
      "[译注: 有很多人将join翻译成并, 但是我感觉这不是很好.] "
      "本章介绍了一种新型的domain论 "
      "[译注: 有人将domain论翻译成论域论, 我觉得也容易造成误解], "
      "其中我们也有上有界的meet (" (Em "拉回")
      "), 并且meet也得到保持. 这种性质, 被称为" (Em "稳定性")
      ", 最初是由[Berry]引入的, 其试图给出" (Em "顺序")
      "算法的语义刻画. 我们将发现这种语义很适合系统" $F:bold
      ", 并且它会将我们引向线性逻辑.")
   (H3 "第8.1节 一般想法")
   (P "指称语义的基本想法在于解释规约 (一种动态概念) 以相等性 "
      "(一种静态概念). 换言之, 我们对于演算的不变量进行建模. "
      "这是在说, 存在着模型和模型: 自G&ouml;del (1930) 起, "
      "如何将模型构造为极大一致扩张就已经是众所周知的了. "
      "这当然不是我要说的东西, 因为这种方法没有给出" (Em "信息") ".")
   (P "我们心中已经有毋须用纸笔写下的幼稚解释, 即类型" (&-> $U $V)
      "的一个对象是一个从" $U "到" $V "的函数, 现在让我们来"
      "看看能否赋予词汇" (Q "函数") "以一个合理的含义. "
      "以这种方式, 我们尽力去避免贪大求全, 而是去寻找简单的几何想法.")
   (P "第一个想到的会是以下内容:"
      (Ul (Li "类型 = 集合.")
          (Li (&-> $U $V) "是所有从" $U "到" $V
              "的(集合论意义上的)函数的集合."))
      "这种解释当然很好, 但是什么也没解释. "
      "计算上令人感兴趣的对象在集合论式函数的海洋中淹死了. "
      "函数空间也会很快变得庞大无比.")
   (P "Kreisel有着以下的想法 (hereditarily effective operation):"
      (Ul (Li "类型 = " $NN "上的部分等价关系.")
          (Li (&-> $U $V) "是满足以下条件的部分递归函数" $f
              "(的代码)的集合, 如果" (: $x $U $y)
              ", 那么" (: (app $f $x) $V (app $f $y))
              ". 这个集合服从于以下等价关系:"
              (MB (: $f (@-> $U $V) $g)
                  "当且仅当"
                  (: $forall (&cm $x $y)
                     (@=> (: $x $U $y)
                          (: (app $f $x) $V (app $g $y)))))))
      "这离我们所寻求模拟的计算范式更近了, 但是似乎又太近了, "
      "因为实际上它没有做比它自己解释句法更多的事情了, "
      "除开一些无聊的编码的话.")
   (P "Scott的想法就好多了:"
      (Ul (Li "类型 = 拓扑空间")
          (Li (&-> $U $V) " = 从" $U "到" $V "的连续函数."))
      "现在众所周知的是拓扑并不意味着我们就能知道如何去构造函数空间. "
      "(Now it is well known that the topology does not lend "
      "itself well to the construction of function spaces.) "
      "何时我们称函数序列收敛呢, 逐点还是说某种一致的收敛? (原注1)")
   (P "原注1. 这个问题最常见的回答 (但是完全不意味着是放之四海而皆准的回答) "
      "是使用" (Em "紧开") "拓扑. 在这种拓扑里, 函数位于一个基本的开集中, "
      "如果该函数限制于某个预先刻画的紧集时, 其值位于某个预先刻画的开集里. "
      "这种拓扑只在空间是局部紧 (每个点都有紧邻域基) 时表现良好, "
      "即便如此函数空间也不必自身是局部紧的.")
   (P "为了解决这些问题, Scott"
      )
   (H2 "第9章 T的指称语义")
   (H2 "第10章 自然演绎中的和")
   (H2 "第11章 系统F")
   (H2 "第12章 和的coherence语义")
   (H2 "第13章 切消 (Hauptsatz)")
   (H2 "第14章 F的强规范化性质")
   (H2 "第15章 表示定理")
   (H2 "附录A: 系统F的语义")
   (H2 "附录B: 什么是线性逻辑?")
   (H2 "参考文献")
   (Bib #:id "Barendregt"
        "H. Barendregt, The lambda-calculus: its syntax and semantics, "
        "North-Holland (1980).")
   (Bib #:id "HinSel"
        "J.R. Hindley and J.P. Seldin, To H.B. Curry: Essays on combinatory logic, "
        "Lambda Calculus and Formalism, Academic Press (1980).")
   (Bib #:id "Howard"
        "W.A. Howard, The formulae-as-types notion of construction, in "
        (Cite0 "HinSel") ".")
   (Bib #:id "vanHeijenoort"
        "J. van Heijenoort, From Frege to G&ouml;del, "
        "a source book in mathematical logic, 1879–1931, "
        "Harvard University Press (1967)")
   ))