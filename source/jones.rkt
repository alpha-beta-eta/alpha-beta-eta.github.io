#lang racket
(provide jones.html)
(require SMathML)
;; (define ((tcomment #:n [n ""]) . x*)
;;   (keyword-apply
;;    Div '(#:attr*) '(((class "tcomment")))
;;    (B (format "译者注记~a." n)) " " x*))
(define &card &abs)
(define (H4. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #f] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h4-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (format-num section index)
  (cond ((and section index)
         (format "~a.~a"
                 (apply string-append
                        (add-between
                         (map number->string
                              (take (cdr (reverse section)) 2))
                         "."))
                 index))
        (section
         (format "~a.~a"
                 (apply string-append
                        (add-between
                         (map number->string
                              (cdr (reverse section))) "."))
                 "*"))
        (index (format "~a" index))
        (else #f)))
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
    (if num
        (Cite `(a ((href ,href)) ,name ,num))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Theorem "定理" "theorem")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise"))
(define (make-identifier str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define-syntax-rule (define-identifier* (x s) ...)
  (begin
    (define x (make-identifier s))
    ...))
(define-identifier*
  (:d "d")
  (:e "e")
  (:f "f")
  (:p "p")
  (:q "q")
  (:C "C")
  (:D "D")
  (:E "E")
  (:F "F")
  (:L "L")
  (:M "M")
  (:P "P")
  (:V "V")
  (:X "X")
  (:Y "Y")
  (:Z "Z")
  (:WHILE "WHILE")
  (:I "I")
  (::= ":=")
  (:\; ";")
  (:while "while")
  (:var "var")
  (:quote "quote")
  (:cons "cons")
  (:hd "hd")
  (:tl "tl")
  (:=? "=?")
  (:nil "nil")
  (:Vars "Vars")
  (:Expressions "Expressions")
  (:Commands "Commands")
  (:Programs "Programs")
  (:do "do")
  (:read "read")
  (:write "write"))
(define :WHILE-programs
  (: :WHILE
     (Mtext "-程序"
            #:attr*
            '((style "font-family: KaiTi;")))))
(define :WHILE-data
  (: :WHILE
     (Mtext "-数据"
            #:attr*
            '((style "font-family: KaiTi")))))
(define :L-programs
  (: :L (Mtext "-程序"
               #:attr*
               '((style "font-family: KaiTi;")))))
(define :L-data
  (: :L (Mtext "-数据"
               #:attr*
               '((style "font-family: KaiTi")))))
(define :L-data_⊥
  (_ :L-data $bottom))
(define :M-programs
  (: :M (Mtext "-程序"
               #:attr*
               '((style "font-family: KaiTi;")))))
(define :M-data
  (: :M (Mtext "-数据"
               #:attr*
               '((style "font-family: KaiTi")))))
(define deno
  (case-lambda
    ((x) (&db0 x))
    ((x L) (^ (&db0 x) L))))
(define $bull (Mi "&bull;"))
(define $dummy $bull)
(define-@lized-op*
  (@-> &->))
(define $simeq (Mo "&simeq;"))
(define $ni (Mo "&ni;"))
(define-infix*
  (&simeq $simeq)
  (&ni $ni))
(define pairl
  (Mo "(" #:attr* '((mathvariant "monospace"))))
(define pairr
  (Mo ")" #:attr* '((mathvariant "monospace"))))
(define pair.
  (Mo "." #:attr* '((mathvariant "monospace")
                    (lspace "0") (rspace "0"))))
(define (Pair a d)
  (: pairl a pair. d pairr))
(define Nat
  (Mi "N" #:attr* '((mathvariant "double-struck")
                    (style "font-style: italic;"))))
(define (enum . x*)
  (apply : (add-between
            x* (&space 2))))
#;
(define jones.html
  (TmPrelude
   #:title "可计算性和计算复杂度"
   #:css "styles.css"
   (H1 "可计算性和计算复杂度")
   (P "虽然很多人都知道, 但是第一个将这样的想法写成书籍的恐怕是Neil Jones. "
      "也就是说, Turing机器和G&ouml;del配数其实可以看成是很难用的编程语言, "
      "没有必要局限于此, 我们可以使用真正的编程语言刻画想法, 进行证明.")
   (H2 "前言")
   (P "本书是对于可计算理论和复杂度理论的一般性介绍. 它应该对于想要了解"
      "可计算理论和复杂度理论的编程语言研究者而言是有趣的, 或许反过来也是一样.")
   (H3 "俯瞰主题")
   (P "与绝大多数计算机科学领域都不相同, 可计算理论和复杂度理论既处理综合也处理"
      "分析, 并且与某些显然带有绝对性质的概念打交道. 过去近一个世纪的逻辑学和"
      "递归函数论的进展已经相当精确地刻画了有效过程 (effective procedure) , "
      "可判定问题 (decidable problem) 和半可判定问题 (semi-decidable problem) "
      "的概念和性质, 并且以本质上相对于所用计算设备和逻辑理论不变的方式建立了它们.")
   (P "令人惊讶的是, 一些类似的不变概念也从有界资源计算中诞生: "
      "多项式时间 (作为判定问题的输入规模的函数), 多项式存储, 带或不带非确定性的计算 "
      "(非确定性即一种&quot;猜&quot;的能力), 以及仅有&quot;只读&quot;"
      "数据访问能力的计算.")
   (P "不仅是理论研究者会对于可计算理论和复杂度理论感兴趣, "
      "编程实践者也应该关心它们. 例如, &quot;复杂度下界&quot;"
      "的角色就类似于工程中的信道容量: 不管编码有多聪明, 界就不可能被克服.")
   (P "不幸的是, 人们对于这种领域的印象就是不可理解的基础定义, "
      "定理证明, 甚至对于定理和问题定义的陈述也不那么简单. "
      "以我之见, 这在某种程度上是一种历史债. 若是我们能够从面向Turing机器和"
      "G&ouml;del配数的古典方法转向更多使用编程语言概念的道路, "
      "想必这能将可计算理论和复杂度理论的传统结果渲染得更平易近人, "
      "而且也使得那些非常强的定理更加容易从直觉上理解, 更加容易应用到实际问题上来.")
   (P "本书包含了计算的古典模型和可计算理论与复杂度理论里的核心结果. "
      "然而, 在以下两个方面上, 其与传统文献迥然相异:"
      (Ol (Li "本书显然更加容易理解, 但并不牺牲精确性, 这是因为我们呈现"
              "可计算理论和复杂度理论的方式运用了编程技术, 并且"
              "受到了编程语言理论的启发.")
          (Li "本书缓解了复杂度理论中的特定结果与日常编程实践之间的"
              "长久以来的一种紧张感. 这种更好的匹配是通过一种新颖的计算模型"
              "完成的, 其与传统模型在特定的重要方面有所不同."))
      "而且, 许多古典理论中繁琐的 (baroque) 构造在编程的上下文中"
      "变得异常简单, 有时甚至将我们引向更强的结果, 其后果是"
      "许多通常只能草绘大概的构造现在可用更加精确和令人信服的方式完成.")
   (H3 "The perspective of the book")
   (P "对于那些已经熟悉可计算理论和复杂度理论的人而言, "
      "以上两点可以阐述得更加具体.")
   (P "对于第一点, 我引入了一个非常简单的编程语言WHILE, 其基本上就是"
      "Pascal或者LISP的一个很小的子集. WHILE语言似乎是表达力和简单性"
      "的精当混合. 当我们将程序作为数据对象处理时, "
      (Em "表达力") "是重要的. WHILE的数据结构特别适合于编码"
      "程序文本, 这避免了" (Em "G&ouml;del配数")
      "的技术性灾难, 而几乎所有以前的文献都使用了G&ouml;del配数. "
      (Em "简单性") "对于证明关于程序及其性质的定理而言也是重要的, "
      "因为太过复杂的语言难以全然理解.")
   (P "从更一般的角度来说, 我认为可计算理论和复杂度理论以及"
      "编程语言和语义学之间有很多可以提供给对方的洞察. "
      "从一个方向上说, 可计算理论和复杂度理论的广度, 深度, 一般性"
      "是目前的编程语言理论中尚且很难见到的, 并且一个传统在于"
      "提出整个学界都感兴趣的精确定义的" (Em "开放问题")
      ". 更进一步来说, 和解决特定关于程序的问题的程序的" (Em "内蕴")
      "不可行性有关的一些问题对于编程语言研究者而言应该也是很有趣的. "
      "例如, 许多出现在程序分析和转换领域中的问题都被证明是"
      "不可判定的或者具有无法克服的高复杂度.")
   (P "从另一个方向上来说, 编程语言界对于设计, 呈现, 实现算法"
      "具有坚实的理解, 并且建立了诸多框架以精确地刻画"
      "各种编程语言概念的语义, 这些编程语言概念包括"
      "函数式编程, 逻辑编程, 命令式编程, 控制运算符, "
      "通信和并发, 面向对象等等. 而且, 编程语言所构造的计算模型"
      "往往在某些特定的重要方面比那些传统的计算模型更加切实.")
   (P "以下是可计算理论和编程语言之间的一个具体联系. "
      "自1930年代起, 可计算理论中的枯燥无味的&quot;s-m-n定理&quot;"
      "就已经为人所知, 但是似乎只是在特定证明中有用的"
      "一个技巧而已. 然而, 令许多人惊讶的是, 在过去的20年间, "
      "s-m-n定理以" (Em "部分求值") "或者说" (Em "程序特化")
      "的别名证明了其自身的价值: 当部分求值器被有效实现时, "
      "其可以用来进行实际" (Em "编译") ", 而当其应用于自身时, "
      "其也可以用来" (Em "生成程序生成器") ".")
   ((tcomment)
    "所谓的生成程序生成器, 实际上指的是第三二村投影, 即"
    "将部分求值器应用于自身时, 可以得到一个编译器生成器, "
    "其以解释器作为输入, 而以编译器作为输出.")
   (P "可计算理论的另一块基石, 即&quot;通用机器&quot;, "
      "无非就是一个" (Em "自解释器")
      "而已, 这在编程语言界是众所周知的概念. 而且, "
      "在可计算理论和复杂度理论的导论里, &quot;模拟&quot;"
      "通常是藉由非形式化的编译器或者有时是解释器达成的.")
   (P "对于前文的第二点, 长久以来可计算理论和复杂度理论与"
      "&quot;实际计算&quot;之间的紧张, 至少有一部分原因是"
      "在于所谓的" (Em "Turing机器加速定理")
      ", 这是复杂度理论中最早被证明的结果之一. "
      "这个定理断言了一个反直觉但却正确的事实: "
      (Em "任意的") "Turing机器程序, 若能在超线性时间"
      "内运行完成, 则其可被替换以一个等价的程序在极限意义下"
      "以两倍的速度完成. 编程语言理论中" (Em "有效")
      "自解释器的存在性则将我们引向了相反的结果: 一种"
      (Em "层次") "定理表明, 对于一种比Turing机器"
      "更加实际的计算模型而言, "
      (Em "常量时间因子的确是重要的")
      ". 更精确地说, 给定时间界" (app $f $n) ", 其中"
      $n "度量了问题输入的规模, 那么存在问题可在时间"
      (&i* (@+ $1 $epsilon) (app $f $n))
      "内解决但是却不能在时间" (app $f $n)
      "内解决. 因此, 给可用时间乘上一个常量因子的确"
      (Em "真扩大(properly increase)")
      "了可被解决的问题类.")
   (P "这样或那样使用编程语言概念的例子产生了可计算理论"
      "和复杂度理论领域中更加容易理解的定理陈述与证明 "
      "(至少对于计算机科学家而言), 甚至是更强的结果. "
      "这些更深刻的结果包括对于著名的问题类"
      'LOGSPACE "和" 'PTIME "的&quot;内蕴&quot;刻画, "
      "其只依赖于程序的句法本身, 而无需借助于任何"
      "外部指定的空间或者时间界.")
   (P "最后我们想说的是, 在这种角度下, "
      "许多旧有的可计算理论和计算复杂度问题重获新生, "
      "而一些新的问题又自然而然出现了. 一类重要的"
      "新问题 (尽管尚没能完全解决) 是: 我们所采取的"
      (Em "编程风格") ", 例如函数式, 命令式, 对于"
      "我们所写的程序的" (Em "效率") "有何影响?")
   (H3 "如何阅读本书")
   (H3 "预备要求")
   (P "预期的读者是已经学习过某种理论的研究生新生, 或者是具备良好数学成熟度的高年级本科生. "
      "具体来说, 本书自由地运用了集合, 函数, 图, 递归定义等概念. 虽然这些概念"
      "在附录里都有解释, 然而其太过紧凑而不适合用作导引. 熟悉某种编程语言"
      "是必要的, 然而至于是何种语言则并无所谓.")
   (H3 "简而言之, 本书的新颖之处")
   (P "本书关于古典可计算理论的结果包括停机问题的不可解性, 以及一些其他自然的问题, 诸如"
      "上下文无关歧义和Hilbert第十问题; Rice关于所有非平凡外延性程序性质均不可判定的结果; "
      "对于递归函数, 递归集合, 递归可枚举集合的刻画; Kleene的s-m-n定理, 第二递归定理, "
      "正规形式定理; 基于不动点的递归; Rogers的同构定理; G&ouml;del的不完备定理.")
   (P "古典复杂度理论的结果则包括"
      )
   (P "和关于可计算理论与复杂度理论的传统书籍相比, 我们的处理具有以下特色:"
      (Ol (Li (Code "WHILE") "语言, 带有类似LISP的数据. 优点: "
              "在牵扯将程序作为数据的构造上既方便又可读; "
              "免于存储管理的烦扰.")
          (Li "与大家熟知的计算机科学概念的强烈关联: "
              "编译 (模拟), 解释 (通用程序), 程序特化 (s-m-n定理), "
              "最优程序的存在与否.")
          (Li "自应用和编译器自举的关系.")
          (Li "程序特化以加速程序运行的部分求值的形式出现, 或者以"
              "通过特化解释器来编译或者生成编译器的形式出现.")
          (Li "对于基础概念的&quot;健壮性&quot;的更简单的构造, "
              "包括函数式语言和lambda演算.")
          (Li "一种证明Kleene第二递归定理的构造, 其给出的"
              "程序远比传统证明有效.")
          (Li ""
              )
          )
      )
   (H2 "第一部分 迈向理论")
   (H3 "第1章 导引")
   (P "本书是关于" (Em "可计算理论") "和" (Em "复杂度理论")
      "的. 在这第一章里, 我们想要去传达 (convey) 可计算理论和复杂度理论的"
      "疆界和技巧. 本章我们有意写得非形式化; 甚至在某些时候我们将会"
      "引入不甚严格的定义或者命题, 它们依赖于特定的直觉性想法.")
   
   (H4 "第1.1节 可计算理论的疆界和目标")
   (P (Em "可计算理论") "会问这样的问题: 是否存在任何" (Em "有效过程")
      "均不可解的" (Em "问题") "? 或者更不严格地说, 是否存在这样的一个问题, "
      "其在任何计算机上以任何可以想象到的编程语言的任何程序都无法解决?")
   (P "我们的编程直觉或许会给出否定的答案, 因为我们的经验告诉我们, "
      "一旦一个问题被以specification的形式精确定义, 那么编写满足"
      "specification的程序就或多或少成为了一个常规的任务. "
      "诚然如此, 类似的一种直觉也占据了Hilbert关于数学基础的工作, "
      "见第1.6节: 他们预设一切的数学都可以被公理化. 然而, "
      "我们将发现这些直觉都错得离谱. 的确有不能被有效过程解决的问题.")
   (P "为了证明这点, 我们必然需要精确定义什么是有效过程, 什么是问题. "
      "任何对于有效过程的单一形式化是否就足够了呢, 这并非先验. "
      "似乎, 任何特定的选择都太过狭隘, 因为其会排除具有特定能力的"
      "其他计算设备. 因此, 不同的选择或许会导致不同的可计算理论. "
      "然而, 可计算理论领域里的最重要的一个洞察在于1930年代的人们"
      "逐步认识到的一个事实, 即对于有效过程的概念的形式化的"
      "任何理智的选择都会在某种意义上导致相同的理论. 此即所谓的"
      (Em "Church-Turing论题") ", 这是因为Alonzo Church和"
      "Alan M. Turing最早陈述并证明了这种洞察的一个版本. "
      "[译注: 与其说这是一个命题, 不如说这是一种指导思想.] "
      "解释为何不同的形式化会导向相同的理论本身就是可计算理论的"
      "重要主题之一, 因而我们在这方面也花了很多精力.")
   (P "在有对于问题和有效过程的精确定义的保证下, 可计算理论关心的是"
      "可计算和不可计算之间的分野, 并试图解决以下问题:"
      (Ul (Li "每个被精确陈述的问题都可以由某个有效过程解决吗?")
          (Li "可由有效过程解决的问题的类是什么? 这个类具有什么基本性质?")
          (Li "不可由有效过程解决的诸多问题之间有什么关系?"))
      "如果一个问题可被一个有效过程解决, 那么我们称其是" (Em "有效可解的")
      ", 或者就说是" (Em "可解的") ". 一个问题的" (Em "不可解性")
      "并非完全负面的结果, 因为这意味着寻找解决问题的有效过程的行为"
      "只可能是徒劳.")
   (P "在接下来的两节里, 我们将讨论有效过程和问题的概念的形式化. 在此之后, "
      "我们将非形式化地呈现可计算理论的一些初等结果, 其中包含两个"
      "被精确陈述的不可解问题.")
   (H4 "第1.2节 什么是有效过程?")
   (P "为了形式化有效过程的概念, 我们可以采取各种各样的策略. "
      "当然, 我们可以按照我们自己的喜好自由地定义概念, 只是"
      "这个定义必须捕获对于有效过程的直觉性想法. 例如, 不应该出现"
      "以下情况: 某个问题根据我们的理论是不可解的, 然而在一个真实世界里的"
      "计算机上却是可以被解决的. 因此, 这个形式化所刻画的有效过程"
      "恰好也应该是我们直觉上有效的过程.")
   (H5 "第1.2.1小节 Alan Turing对于计算的分析")
   (H5 "第1.2.2小节 Church-Turing论题")
   (P "在Turing的分析中所提及的机器被称为" (Em "Turing机器")
      ". "
      )
   (P "除了Turing机器, 还有其他对于有效过程概念的形式化, 例如"
      (Ul (Li "由Kleene [98]定义的" (Em "递归函数") ".")
          (Li "归功于Church [22, 23]的函数定义的"
              (Em "lambda演算") "方法.")
          (Li (Em "随机存取机器") " [163].")
          (Li (Em "Markov算法") " [115].")))
   (P "尽管它们有着相当的不同, 但其实也共享一些特征 [155]:"
      (Ol (Li "一个有效过程是通过一集有限长度的指令给出的, "
              "只有有限多个不同的指令.")
          (Li "计算是以离散的按步风格执行的, "
              "不使用连续方法或者模拟设备.")
          (Li "计算是以确定的方式执行的, "
              "不采取随机方法或者随机设备, 例如骰子.")
          (Li "对于存储空间和时间没有" (Em "先验的")
              "固定边界, 但是终止的计算不能依赖于"
              "无限的空间或者无限的时间.")
          (Li "每个计算步骤只牵涉有限数量的数据.")))
   (P "以上这些有限过程的概念的形式化实际上是等价的. "
      "因此, 有时Church-Turing论题以如下形式表达:"
      (Ol (Li "所有对于有效过程的直觉概念的合理形式化都是等价的;")
          (Li "Turing机器可计算性是对于有效可计算性的一个合理形式化.")))
   (P "为了支持这个观点, 之后的章节里我们会考虑许多不同的形式化并且证明"
      "它们都是等价的. 不过, 对于本章的剩余部分, 有效过程或者"
      (Em "算法") "的概念将维持直觉性的观念.")
   (H5 "第1.2.3小节 算法是硬件还是软件?")
   (P "讨论算法是硬件还是软件的问题有点类似于讨论先有鸡还是先有蛋, "
      "但是这的确是值得的, 因为关于可计算理论和复杂度理论的材料, "
      "特别是复杂度理论的材料, 都隐含地偏向其中一种观点. "
      "例如, &quot;Turing机器&quot;携带了硬件的弦外之音, 而Turing的论证中的"
      "&quot;心灵的状态&quot;似乎对应于机器的状态.")
   (P (Em "硬件观点") "认为算法是实现意图的计算的机能装置的一部分. &quot;指令集&quot;"
      "是对于其架构的刻画. 在任何一个时间点整个机器的状态由其正在执行的指令和其"
      "存储状态构成. 更大的算法对应于更大的硬件.")
   (P "不限制存储的量的问题可以用多种方法解决:"
      (Ul (Li "假定给定了无穷的分立的存储单元, 例如Turing的&quot;纸带&quot;;")
          (Li "假定拥有一个无限可扩展的理想硬件, 尽管在任何时间点只会使用有限的存储;")
          (Li "使用有限机器的无限族" (&cm $M_1 $M_2 $..h)
              ", 于是更大的输入数据由更大的机器处理. "))
      "最后一种方法与所谓的" (Em "电路复杂度") "有关. 人们通常要求序列"
      (&cm $M_1 $M_2 $..h) "是" (Em "一致的") ", 于是更大的数据不会由"
      "迥然相异的机器处理.")
   (P (Em "软件观点") "认为算法是指令的集合或者序列. 例如, 一个算法可以是"
      "以某人最爱的编程语言写成的程序. &quot;计算代理&quot;然后解释算法; "
      "这可以是一块硬件, 但也可以是软件的: 以某个低层次语言写成的解释器程序. "
      "从操作的角度来看, 一个解释器维护了一个到当前指令的指针, 以及对于"
      "算法的当前存储状态的表示. 更大的算法对应于更大的被解释程序, "
      "但是解释器本身仍然是固定的, 不论是硬件还是程序.")
   (P "最初完全自动的计算机器是von Neumann的&quot;存储程序&quot;机器. "
      )
   (H3 "第2章 " (Code "WHILE") "语言")
   (P "导引章节的概念 (例如&quot;有效可计算&quot;) 是不精确的, 因为它们依赖于对于"
      "&quot;有效过程&quot;的直觉性理解. 现在我们呈现一个计算模型, 或者说编程语言, "
      "其被称为" (Code "WHILE") ". 之后我们将精确地定义前一章的直觉性概念, 通过将"
      "&quot;有效过程&quot;和&quot;" (Code "WHILE") "程序&quot;等同起来.")
   (H4 "第2.1节 " (Code "WHILE") "数据和程序的句法")
   (H5 "第2.1.1小节 作为数据值的二叉树")
   ((definition #:n "2.1.1")
    "树的集合" $DD "定义如下:"
    (Ol (Li "原子" (Code "nil") "是" $DD "的一个元素;")
        (Li "每当" (Code "d" (Sub "1")) "和" (Code "d" (Sub "2"))
            "是" $DD "的元素, 那么"
            (Code "(d" (Sub "1") ",d" (Sub "2") ")")
            "也是; 并且")
        (Li $DD "是满足前两点的最小集合.")))
   ((definition #:n "2.1.2")
    "函数" 
    )
   (H5 "第2.1.2小节 " (Code "WHILE") "程序的句法")
   
   (H3 "第3章 程序作为数据对象")
   (H2 "第二部分 可计算性导论")
   (H3 "第4章 自解释: " (Code "WHILE") "和" (Code "I") "的通用程序")
   (H3 "第5章 可计算理论基础")
   (H3 "第5.1节 可计算性, 可判定性, 可枚举性")
   ((definition #:n "5.1.1")
    "一个部分函数" (func $f $DD (_ $DD $bottom)) "是" (Code "WHILE")
    "可计算的当且仅当存在一个" (Code "WHILE") "程序" (Code "p")
    "使得" (&= $f (&db0 (Ms "p"))) ", 即对所有" (∈ (Ms "d") (Ms "e") $DD) ":"
    (Ol (Li "如果" (&= (app $f (Ms "d")) $bottom) ", 那么"
            (&= (app (&db0 (Ms "p")) (Ms "d")) $bottom) ".")
        (Li "如果" (&= (app $f (Ms "d")) (&in (Ms "e") $DD)) ", 那么"
            (&= (app (&db0 (Ms "p")) (Ms "d")) (Ms "e")) ".")))
   (P "集合" $A "是可判定的, 如果关于" $A "的成员资格问题可由一个总是终止的程序回答. "
      ""
      )
   (H3 "第6章 元编程, 自解释和编译器生成")
   (H3 "第7章 计算的其他顺序模型")
   
   ))
(define jones.html
  (TnTmPrelude
   #:title "可计算性和计算复杂度"
   #:css "styles.css"
   (H1. "可计算性和计算复杂度")
   (H2. "引论")
   (H2. "WHILE语言")
   (H3. "WHILE数据和程序的句法")
   (H4. "二叉树作为数据值")
   ((Definition)
    "树的集合" $DD "定义如下:"
    (Ol (Li "原子" :nil "是" $DD "的一个元素;")
        (Li "每当" (_ :d $1) "和" (_ :d $2)
            "是" $DD "的元素, 那么"
            (Pair (_ :d $1) (_ :d $2))
            "也是; 并且")
        (Li $DD "是满足前述两点的最小集合.")))
   ((Definition)
    "函数" (func (&card $bull) $DD Nat) "定义如下:"
    (MB (&= (&card :d)
            (Choice0
             ($1 ", 如果" (∈ :d $A))
             ((&+ (&card (_ :d $1))
                  (&card (_ :d $2)))
              ", 如果"
              (&= :d (Pair (_ :d $1) (_ :d $2)))))))
    "其代表了一个数据值" (∈ :d $DD)
    "的" (Em "大小(size)") ".")
   (H4. "WHILE程序的句法")
   ((Definition)
    "令" (&= :Vars (setE (_ :V $0) (_ :V $1) $..h))
    "是不同的变量. 我们使用约定"
    (∈ :d :e :f $..h $DD) "和"
    (∈ :X :Y :Z $..h :Vars)
    ". 那么, WHILE的句法由以下语法给出:"
    (MB (set-attr*
         (&Table
          (:Expressions $ni (&cm :E :F) $::= :X
                        (@ "对于" (∈ :X :Vars)))
          ($ $ $ $lv :d
             (@ "对于原子" :d))
          ($ $ $ $lv (enum :cons :E :F))
          ($ $ $ $lv (enum :hd :E))
          ($ $ $ $lv (enum :tl :E))
          ($ $ $ $lv (enum :=? :E :F))
          (:Commands $ni (&cm :C :D) $::= (enum :X ::= :E))
          ($ $ $ $lv (enum :C :\; :D))
          ($ $ $ $lv (enum :while :E :do :C))
          (:Programs $ni :P $::=
                     (enum :read :X :\;
                           :C :\;
                           :write :Y)))
         'columnalign
         "left center left center left left"))
    "这里的" :X "和" :Y "是不需要相异的"
    (Em "输入变量") "和" (Em "输出变量") ".")
   (P "我们使用缩进来指明" :while "和其他命令的作用域. "
      "例如, 考虑以下两个命令:"
      (CodeB "while E do
  C;
D")
      (CodeB "while E do
  C;
  D")
      
      )
   (H4. "非形式化的语义")
   (H4. "真值和if-then-else")
   (H4. "列表")
   (H4. "数字")
   (H4. "句法糖: 一些有用的宏记号")
   (H3. "WHILE程序的语义")
   (H4. "存储")
   (H4. "对于表达式的求值")
   (H4. "命令的执行")
   (H4. "WHILE程序的语义")
   (H4. "计算语义值")
   (H3. "相等性与原子相等性的对比")
   (H4. "更多的句法糖")
   (H3. "练习" #:auto? #f)
   (H3. "引用" #:auto? #f)
   (H2. "程序作为数据对象")
   (H3. "编程语言和模拟")
   ((Definition)
    "一个" (Em "编程语言") :L "由以下资料构成:"
    (Ol (Li "两个集合, " :L-programs "和" :L-data ";")
        (Li "一个函数"
            (func (deno $dummy :L)
                  :L-programs
                  (@-> :L-data :L-data_⊥)) "."))
    "这里" (deno $dummy :L) "是" :L
    "的" (Em "语义函数") ", 其将每个"
    :L "-程序" (∈ :p :L-programs)
    "与一个相应的部分函数"
    (func (deno :p :L) :L-data :L-data_⊥)
    "联系起来." (Br)
    "如果" (&sube :L-programs :L-data)
    ", 那么我们称" :L "具有"
    (Em "程序作为数据(programs-as-data)")
    "的性质. 另外, 如果"
    (&sube (&c* :L-data :L-data) :L-data)
    ", 那么我们称" :L "具有"
    (Em "结对(pairing)") "性质.")
   ((Definition)
    "设" (&= :L-data :M-data)
    ". 语言" :M (Em "可以模拟") :L
    ", 如果对于每个" (∈ :p :L-programs)
    ", 存在一个" :M "-程序" :q "使得对于每个"
    (∈ :d :L-data) ", 我们有"
    (MB (&simeq (app (deno :p :L) :d)
                (app (deno :q :M) :d)) ".")
    "等价地: " :M "可以模拟" :L
    "当且仅当存在一个完全函数"
    (func $f :L-programs :M-programs)
    "满足对于每个" :L "-程序" :p "都有"
    (MB (&= (deno :p :L)
            (deno (app $f :p) :M)) ".")
    "语言" :L (Em "等价于") "语言" :M
    ", 记作" (&equiv :L :M) ", 如果语言"
    :L "和语言" :M "可以互相模拟.")
   (P "这个定义表达了" :L "和" :M
      "可以计算相同函数的事实; "
      "但是其并没有断言存在任何" (Em "构造性")
      "的方式来获得一个等价于给定" :L "-程序的"
      :M "-程序. 本章的剩余部分关心的是"
      "模拟是如何计算性地完成的, 要么通过转换 "
      "(应用一个编译函数), 要么是通过解释. "
      "然而, 首先我们需要一种方式来将程序视为数据对象.")
   (H3. "在" $DD "中表示" :WHILE "程序")
   (P "之前我们已经给过了" :WHILE-programs "和" :WHILE-data
      "的句法. 假设我们想要将一个WHILE程序喂给另外一个WHILE程序作为输入. "
      "目前这是不可行的, 仅仅是因为" :WHILE-programs
      "的元素并非" :WHILE-data "中的对象. 因此, "
      "我们现在需要对于WHILE程序给出一种"
      (Em "程序作为数据表示") ".")
   ((Definition)
    "令" (setE ::= :\; :while :var :quote :cons :hd :tl :=? :nil)
    "代表" $DD "的" 10 "个相异元素. WHILE程序" :p
    "的表示"
    )
   (H2. "自解释: " :WHILE "和" :I "的通用程序")
   (H2. "可计算理论的基本")
   (H2. "元编程, 自应用, 和编译器生成")
   (H2. "其他顺序计算模型")
   (H2. "可计算概念的健壮性")
   (H2. "函数式语言所施行的计算")
   (H2. "一些自然的不可解问题")
   (H2. "Hilbert第十问题")
   (H2. "推理系统和G&ouml;del不完备性定理")
   (H2. "基于数字的可计算理论")
   (H2. "更多可计算性的抽象方式")
   ))