#lang racket
(provide automated.html)
(require SMathML)
(define (format-num section index)
  (and index
       (format "~a.~a" (cadr (reverse section)) index)))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
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
    (Cite `(a ((href ,href)) ,name ,num)))
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
  (Theorem "定理" "theorem")
  (Warning "警告" "warning")
  (Example "例子" "example")
  (Proposition "命题" "proposition"))
(define (H3. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #f] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h3-present #:cite heading-cite
                     #:level 3 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (H4. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #f] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h4-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define $space:2ex (&space 2))
(define (@@ . x*)
  (apply @ (add-between x* $space:2ex)))
(define (APP . x*)
  (apply : (add-between x* $space:2ex)))
(define $atoms (Ms "atoms"))
(define (&atoms p)
  (app $atoms p))
(define $eval (Ms "eval"))
(define (&eval p v)
  (APP $eval p v))
(define $true (Mi "true"))
(define $false (Mi "false"))
(define $~ (Mo "~"))
(define &~
  (make-op $~
    (err0 '&~)
    (lambda (x) (: $~ x))))
(define $& (Mo "&amp;"))
(define $\| (Mo "|"))
(define $<-> (Mo "&harr;"))
(define-infix*
  (&conj $conj)
  (&disj $disj)
  (&& $&)
  (&\| $\|)
  (&=> $=>)
  (&<-> $<->))
(define automated.html
  (TnTmPrelude
   #:title "实用逻辑和自动推理手册"
   #:css "styles.css"
   (H1. "实用逻辑和自动推理手册")
   (H2 "前言")
   (P "本书是关于可以执行" (Em "自动推理")
      "的计算机程序的. "
      )
   (H2. "引论")
   (P "本章我们引入了逻辑推理和机械化它的想法, "
      "简要触及了重要的历史性发展. "
      "我们通过讨论逻辑学中的一些最为基础的想法"
      "以及刻画符号方法是如何在计算机上实现的"
      "来为后续内容奠定基础.")
   (H3. "什么是逻辑推理?")
   (P "存在许多理由相信某个东西为真. "
      "它或许似乎是显而易见的, "
      "或许至少第一眼看上去是令人信服的, "
      "可能我们的父母之前告诉过我们, "
      "也可能是其与相关科学实现的结果达成了惊人的一致性. "
      "尽管往往是可靠的, 但是这些判断的方法也并非万无一失, "
      "其也曾被用于说服人们地球是平的, 圣诞老人是存在的, "
      "原子不可被进一步划分为更小的粒子.")
   (P (Em "逻辑") "推理相较于其他推理的不同之处在于"
      "其会避免任何未经澄清的假设, "
      "并将自身限制于不会出错且超越理性置辩的推理. "
      "为了避免作出任何无法保证的假设, "
      "逻辑推理不能依赖于要被推理的对象或者概念的任何特殊性质. "
      "这意味着逻辑推理必须从所有这样的特殊特征之中抽象出来, "
      "并在应用于其他领域时同等有效 (valid). "
      "论证之所以被接受为逻辑性的(论证), "
      "在于其与某种一般" (Em "形式")
      "的相合, 而非因为其所处理的特定" (Em "内容")
      ". 例如, 将以下的传统例子:"
      (Blockquote
       (Pre "所有人都是要死的
Socrates是一个人
因此Socrates是要死的"))
      "与下列从数学之中抽出的推理进行比较:"
      (Blockquote
       (Pre "所有正整数都是四个整数的平方之和
" 15 "是一个正整数
因此" 15 "是四个整数的平方之和"))
      "这两个论证都是正确的, 并且具有共同的模式:"
      (Blockquote
       (Pre "所有的" $X "都是" $Y "
" $a "是" $X "
因此" $a "是" $Y)))
   (P "这个推理模式是逻辑有效的, 因为其有效性并不依赖于内容: "
      (Q "正整数") "和" (Q "要死的") "的含义是无关紧要的. "
      "我们可以将" $X ", " $Y ", " $a
      "替换为任何我们喜欢的东西, 只要尊重语法范畴即可, "
      "而语句仍然保持有效. "
      "与之相对的是, 考虑以下推理:"
      (Blockquote
       (Pre "所有的雅典人都是希腊人
Socrates是一个雅典人
因此Socrates是要死的")))
   (P "尽管这个结论是全然正确的, 但是这个论证并非逻辑有效的, "
      "因为其依赖于所牵涉的项的内容. "
      "其他的具有同样的似是而非形式的论证当然也可能是假的, 例如"
      (Blockquote
       (Pre "所有的雅典人都是希腊人
Socrates是一个雅典人
因此Socrates没有胡须")))
   (P "然而, 第一个论证可以转变为一个逻辑有效的论证, "
      "通过将隐式的假设" (Q "所有的希腊人都是要死的")
      "显式化. 现在这个论证是以下一般的逻辑有效的形式的一个实例:"
      (Blockquote
       (Pre "所有的" $G "都是" $M "
所有的" $A "都是" $G "
" $s "是" $A "
因此" $s "是" $M)))
   (P "第一眼看上去, 这种对于推理的法医鉴识式分析似乎并不令人印象非常深刻. "
      "逻辑有效的推理从未告诉过我们任何关于世界的本质上新颖的东西"
      "&mdash;&mdash;正如Wittgenstein (1922) 所言, "
      (Q "当我知道天气要么下雨要么不下雨时, 我对于天气一无所知")
      ". 换言之, 如果我们" (Em "的确")
      "从推理的链条之中学到了关于世界的什么新东西, "
      "那么它必然包含" (Em "并非") "纯粹逻辑的步骤. "
      "Schilpp (1944) 中引用Russell所言:"
      (Blockquote
       "Hegel从纯粹逻辑之中推导出了世界的全部本质, "
       "包括小行星的不存在性, 其之所以能够做到这点, "
       "只是因为他逻辑无能.")
      "{原注: 为了对于Hegel公平一些, 我必须要说词汇" (Em "逻辑")
      "直到相当的最近都常以更为宽泛的含义使用, "
      "而我们所考虑的逻辑在那时应该被称为" (Em "演绎逻辑")
      ", 用以和" (Em "归纳逻辑") "进行区分, "
      "后者从观察到的数据之中得出结论, "
      "如在物质科学 (physical sciences) 里的那样.}")
   (P "但是逻辑分析可以清晰地揭示关于真实世界的"
      (Em "事实") "之间的必要关系, "
      "直接地表明何处掺入了或许不能保证的假设. "
      "例如, 根据" (Q "如果刚刚下过雨, 那么地面是潮湿的")
      "可以逻辑推出" (Q "如果地面并非超市, 那么刚刚就不可能下过雨")
      ". 这是被称为" (Em "逆否") "的一般原理的一个实例: 从"
      (Q "如果" $P "那么" $Q) "可以推出"
      (Q "如果非" $Q "那么非" $P)
      ". 然而, 从" (Q "如果" $P "那么" $Q) "到"
      (Q "如果" $Q "那么" $P) "一般" (Em "并非")
      "有效, 在这种情况下就是我们发现我们不能推出"
      (Q "如果地面是潮湿的, 那么刚刚下过雨")
      ", 因为也可能是由于爆裂的水管或者灌溉设施什么的才导致了潮湿.")
   (P "或许正如Locke (1689) 所言, 这样的例子可能是"
      (Em "琐碎的") ", 不过这种初等的逻辑谬误我们也经常会遇到. "
      "更为重要的是, 数学之中的演绎远非琐碎所能概括, "
      "而是一直使人类历史之中的一些伟大智者深深着迷, "
      "经常也使得他们感到挫败. "
      "从简单而不可辩驳的假设出发, 经过漫长而复杂的逻辑演绎链条, "
      "可以通往复杂且违反直觉的定理, 正如Hobbes所发现的 (Aubrey 1898):"
      (Blockquote
       "在一位绅士的私人书房里, Euclid的原本摊开着, 上面写着"
       
       )
      )
   (P "的确, Euclid的开创性作品" (Em "几何原本")
      "建立了一种特定的推理风格, "
      "其经完善之后构成了如今数学的脊骨. "
      
      )
   (H3. "Calculemus!")
   (P (Q "推理就是算账 (reckoning).")
      " 在本书的卷首语中我们引用了Hobbes关于逻辑推理和数值计算之间的相似性的阐述. "
      "尽管Hobbes应该因为使得这个想法更为广为人知而受到嘉奖, "
      "这个想法本身即便在1651年也不是全新的. "
      "的确由Plato和Aristotle所使用以代表推理或者逻辑思维的希腊词汇"
      (Em "logos") "在其他上下文中也可能表示计算或者算账. "
      "当古希腊哲学家的作品在中世纪的欧洲广为人知的时候, " (Em "logos")
      "一般被翻译为" (Em "ratio") ", 这是代表算账的拉丁词汇 "
      "(因而有了英语词汇rational, ratiocination, 等等). "
      "即便在如今的英语里, 我们有时也能听到"
      (Q "I reckon that ...") ", 这里的" (Q "reckon")
      "指的是某种推理而并非字面意义上的要去计算.")
   (P "然而, 推理和算账之间的联系在Gottfried Wilhelm von Leibniz (1646–1716) "
      "的作品之前几乎只是一种暗示性的口号. "
      "Leibniz相信一个根据计算进行推理的系统必须包含两个基本组件:"
      (Ul (Li "一种通用语言 (" (Em "characteristica universalis")
              "), 由此可以表达任意的东西;")
          (Li "一种推理演算 (" (Em "calculus ratiocinator")
              "), 用于判定以" (Em "characteristica")
              "表达的断言的真假.")))
   (P "Leibniz梦想着有朝一日不能达成一致意见的双方不会陷入徒劳的争辩, "
      "而是将他们的不一致转换为" (Em "characteristica")
      ", 然后彼此言称" (Q "calculemus") " (让我们计算吧). "
      
      )
   (H3. "符号化")
   (P "Leibniz将注意力放在建立合适的语言这一基本的首要步骤上是正确的. "
      "然而, 他太过雄心壮志以至于想要表达人类思维的所有方面. "
      "最终的进步来自于扩展已经在数学中所使用的符号记号的应用范围. "
      "例如, 如今我们会说" (Q (&<= $x^2 (&+ $y $z))) "而非"
      (Q $x "乘上自身小于等于" $y "与" $z "之和")
      ". 随着时间推移, 越来越多的数学开始以形式符号记号表达, "
      "取代了自然语言渲染. 对此我们可以找到几个坚实的理由.")
   
   (H3. "Boole的逻辑代数" #:id "algebra-of-logic")
   (P "词汇" (Em "algebra") "来源于阿拉伯语的" (Q "al-jabr")
      ", "
      )
   (H4. "机械化")
   (H4. "逻辑形式")
   (H3. "句法和语义")
   (H4. "对象语言和元语言")
   (H4. "抽象和具体句法")
   (H3. "符号计算和OCaml" #:id "symbolic-computation-and-ocaml")
   (P "现代计算的早期人们普遍相信计算机基本上是用来进行数值计算的设备 (Ceruzzi 1983). "
      "其输入和输出设备确实也在某种程度上偏向于这一方向: "
      "当Samuels在1948年于IBM写下第一个跳棋程序时, "
      "他不得不将输出编码为一个数字, 因为这就是唯一可以打印的内容了. "
      "然而, 即便在"
      )
   (P "第一个任务是定义一个数据类型以表示代数表达式的抽象句法. "
      "我们允许表达式由诸如" (Code "0") ", " (Code "1") ", "
      (Code "33") "这样的数值常量以及诸如" (Code "x") "和"
      (Code "y") "这样的命名变量通过加法 (" (Q (Code "+"))
      ") 和乘法 (" (Q (Code "*")) ") 运算构筑而成. "
      "以下是相应的递归数据类型声明:"
      (CodeB "type expression =
   Var of string
 | Const of int
 | Add of expression * expression
 | Mul of expression * expression;;"))
   
   (H3. "句法分析 (parsing)")
   (H4. "词法分析")
   (H4. "句法分析")
   (H3. "美观打印 (prettyprinting)")
   (H3. "深入阅读")
   (H3. "练习")
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
      "的首字母, 其意为可兼或. "
      )
   (H4. "Generic parsing and printing")
   (H4. "原始命题")
   (P "尽管许多函数将会是通用的 (generic), "
      "但是如果我们固定在一个确定的原始命题类型上, "
      "对于某些操作进行实验会更加简单. "
      "据此我们定义了以下的原始命题类型, 其由名字索引 (即字符串):"
      (CodeB "type prop = P of string;;")
      "我们定义以下函数来获取一个命题的名字:"
      (CodeB "let pname(P s) = s;;"))
   
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
      "上的一个相应运算子所解释. "
      
      )
   (H4. "机械化了的真值表")
   (P "我们期望对于一个公式的求值独立于赋值如何给没有出现在公式之中的原子分配真值. "
      "让我们通过定义一个提取公式之中所出现的原子命题的集合的函数来使得我们的表述精确化. "
      "以抽象的数学术语来说, 我们将通过公式上的递归定义" (Code "atoms") "如下:"
      (let* (($atoms (Ms "atoms"))
             (atoms (curry app $atoms)))
        (eqn*
         ((atoms $bottom)   $= $empty)
         ((atoms $top)      $= $empty)
         ((atoms $x)        $= (setE $x))
         ((atoms (&neg $p)) $= (atoms $p))
         ((atoms (&conj $p $q))
          $=
          (&union (atoms $p) (atoms $q)))
         ((atoms (&disj $p $q))
          $=
          (&union (atoms $p) (atoms $q)))
         ((atoms (&=> $p $q))
          $=
          (&union (atoms $p) (atoms $q)))
         ((atoms (&<=> $p $q))
          $=
          (&union (atoms $p) (atoms $q))))))
   (P "作为公式上的结构归纳证明的一个简单例子 "
      "(见附录1和2), 我们将会证明" (&atoms $p)
      "总是有限的, 因而我们可以基于ML的列表来解释它而并没有曲解其含义. "
      "(当然了, 我们需要记住一般情况下列表相等性和集合相等性并不相同.) "
      "{译注: 我的理解大概就是这里可以使用列表表示集合, 仅此而已.}")
   ((Theorem)
    "对于任意的命题公式" $p ", 集合" (&atoms $p) "是有限的.")
   ((proof)
    
    )
   (P "类似地, 我们可以形式化地澄清以上所提及的直觉上显然的事实.")
   ((Theorem)
    "对于任意的命题公式" $p ", 如果两个赋值" $v "和" $v^
    "在集合" (&atoms $p) "上相合, 即对于每个" (∈ $x (&atoms $p))
    "都有" (&= (app $v $x) (app $v^ $x)) ", 那么"
    (&= (&eval $p $v) (&eval $p $v^)) ".")
   ((proof)
    
    )
   (P "以上" (Code "atoms") "的定义可以被直接翻译为一个OCaml函数, "
      "例如对于" (Q $union) "使用" (Code "union") "而对于"
      (Q (setE $x)) "使用" (Code "[x]")
      ". 然而, 我们更倾向于基于既有的迭代子"
      (Code "atom_union") ":"
      (CodeB "let atoms fm = atom_union (fun a -> [a]) fm;;")
      "例如:"
      (CodeB "# atoms &lt;&lt;p /\\ q \\/ s ==> ~p \\/ (r &lt;=> s)>>;;
- : prop list = [P &quot;p&quot;; P &quot;q&quot;; P &quot;r&quot;; P &quot;s&quot;]"))
   (P "鉴于对于一个命题公式" $p "的解释"
      )
   (H4. "形式语言和自然语言")
   
   (H3. "有效性, 可满足性, 重言")
   (H3. "De Morgan律, 充分性, 对偶性")
   (H3. "化简和否定范式")
   (H3. "析取范式和合取范式")
   (H3. "命题逻辑的应用")
   (H2. "一阶逻辑")
   (P "我们现在从命题逻辑移至更为丰富的一阶逻辑, "
      "其中命题可以牵涉全称或者存在量化的非命题变元. "
      "我们将会展现一阶逻辑之中的证明是如何可以"
      "藉由Herbrand定理被朴素地机械化的. "
      "接着我们将会引入诸多改进, "
      "特别是合一 (unification), "
      "其可以使得自动化证明更加高效.")
   (H3. "一阶逻辑及其实现")
   (P "命题逻辑只允许我们从原始命题构建公式, "
      "原始命题本身或许是独立地为真或者为假. "
      
      )
   (H3. "句法分析和打印")
   (H3. "一阶逻辑的语义")
   (H3. "句法操作")
   
   (H2. "相等性")
   (H2. "可判定问题")
   (H2. "交互式定理证明")
   (H2. "限制")
   ))