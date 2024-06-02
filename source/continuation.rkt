#lang racket
(provide continuation.html)
(require SMathML)
(define (MiB str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define (LAM var exp)
  (: $lambda var $. exp))
(define (make-blank n)
  (Mspace #:attr* `((width ,(format "~spx" n)))))
(define blank:4px (make-blank 4))
(define (APP e1 e2)
  (: e1 blank:4px e2))
(define @APP (@lize APP))
(define APPL
  (case-lambda
    ((e1 e2) (APP e1 e2))
    ((e1 e2 . e*)
     (apply APPL (APP e1 e2) e*))))
(define diagram.svg
  (Svg
   #:attr* '((width "320")
             (height "160")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs (Path #:attr*
               '((id "piece")
                 (d "M 0 0 h 40 l 20 -20 l 20 20 h 40 l -60 -60 z")))
         (Path #:attr*
               '((id "triangle")
                 (d "M 0 0 l 20 -20 l 20 20 z"))))
   (Use #:attr* '((x "5") (y "65") (href "#piece") (fill "none")))
   (Use #:attr* '((x "5") (y "125") (href "#piece")))
   (Use #:attr* '((x "45") (y "145") (href "#triangle") (fill "none")))
   (Text #:attr* '((x "125") (y "125"))
         "CONTINUATION")))
(define continuation.html
  (TmPrelude
   #:title "延续和自然语言"
   #:css "styles.css"
   (H1 "延续和自然语言")
   (H3 "记号约定")
   (P (Table
       (Tr (Td (&-> $e:monospace $t:monospace))
           (Td "类型: 我们不将从类型为" $tau
               "的对象到类型为" $sigma "的对象的函数的类型记为"
               (tupa0 $tau $sigma)
               ", 而是遵循计算机科学的约定, 写成"
               (&-> $tau $sigma)
               ". 因此, 一个外延性verb phrase的类型为"
               (&-> $e:monospace $t:monospace) "."))
       
       ))
   (H3 "缩写表")
   (P (Table
       (Tr (Td "AR")
           (Td "Argument Raising")
           (Td "参数提升"))
       (Tr (Td "CBN")
           (Td "call-by-name Continuation Passing Style")
           (Td "按名调用的延续传递风格"))
       (Tr (Td "CBV")
           (Td "call-by-value Continuation Passing Style")
           (Td "按值调用的延续传递风格"))
       (Tr (Td "CL")
           (Td "Combinatory Logic")
           (Td "组合子逻辑"))
       (Tr (Td "CPS")
           (Td "Continuation Passing Style")
           (Td "延续传递风格"))
       
       ))
   (H3 "引论")
   (P "本书是关于延续的. 其论证了延续对于完整理解自然语言的含义而言是一个基本的组件."
      (Ol (Li (B "延续假设 (continuation hypothesis): ")
              "一些自然语言表达代表 (denote, 或许也可以翻译为指称) "
              "了其延续上的函数, 即以其语义上下文为参数的函数."))
      "我们为此假设辩护的主要方式在于提供对于各种各样的自然语言现象的分析, "
      "而其直觉依赖于对延续的显式引用.")
   (H4 "什么是一个延续?")
   (P "一个" (B "延续") "是围绕一个表达式的上下文的一个部分."
      (Ol #:attr* '((start "2"))
          (Li "John said [" (B "Mary called ")
              "everyone" (B " yesterday")
              "] with relief."))
      "在2这个句子里, 表达式" (Em "everyone")
      "相对于由括号包裹的嵌入从句的延续是将这个嵌入从句的"
      (Em "everyone") "移除之后的剩余部分. "
      "其材料包括词项 (lexical item) " (Em "Mary")
      ", " (Em "called") ", 还有" (Em "yesterday") ".")
   (P "根据字符串表示, 这个延续似乎是一个非连续的对象, "
      "但是延续的连续 (contiguous) 本性在我们考虑"
      "句法结构树 (syntactic phrase structure tree) 时立刻变得显然起来."
      (Ol #:attr* '((start "3"))
          (Li (CodeB "(S John
   (VP (VP said
           " (B "(S Mary
              (VP (VP called "
                (Span #:attr* '((style "font-weight: normal;"))
                      "everyone")
                ")
                  yesterday)))") "
       (PP with relief)))")))
      "[注记: 绘制树结构对于我而言太困难了, 还是用S-exp来表示吧. "
      "另外, 加粗的部分在原图中亦有加粗以示强调.] 在这个树里, "
      (Em "everyone") "相对于嵌入从句的延续是统领 (dominate) "
      (Em "Mary") ", " (Em "called") ", " (Em "yesterday")
      ", 但是把" (Em "everyone") "移除了的树的连续部分. "
      "若以图解表示, 则有以下图示:"
      (Ol #:attr* '((start "4"))
          (Li diagram.svg))
      "在手头上的这个例子里, 上部未涂黑的缺口三角形对应于较小的这个从句"
      "所嵌入的结构的部分, 包括" (Em "John") ", " (Em "said")
      ", 以及" (Em "with relief") ". 中间涂黑的缺口三角形对应于"
      "scope-taker " (Em "everyone") "所take scope的材料"
      "&mdash;&mdash;即其延续. 而最小的未涂黑无缺口三角形对应于"
      "scope-taker " (Em "everyone") ".")
   (P "我们将在整本书中不时使用这样的示意图. 我们将其称为"
      (Em "七巧板图(tangram diagram)")
      ", 七巧板是一种智力游戏, 其中一集平面几何形状"
      "被重新排列组合为各种各样更大的形状.")
   (P "既然我们主要关心的是含义, 我们将会专注于" (Em "语义")
      "上下文而非, 比如说语音上下文或者句法上下文. "
      "那么, 在以上的例子里, " (Em "everyone")
      "相对于括起来的从句的语义延续是抽象出了" (Em "everyone")
      "的贡献的该从句的含义, 即being called yesterday by Mary"
      "这个性质, 其可以被渲染为"
      (LAM $x (APPL (MiB "yesterday")
                    (@APP (MiB "called")
                          $x)
                    (MiB "m")))
      ".")
   (H4 "延续何以基本?")
   (P "2中所识别的语义延续是量词" (Em "everyone")
      "用作其语义参数的东西, 即其" (Q "核作用域 (nuclear scope)")
      ". 一般而言, 识别一个scope-taking表达式的语义参数和"
      "识别出(其某一个)延续是相同的事情. "
      "因此, scope-taking是延续在自然语言中最令人信服的应用.")
   (P "然而, 这对于我们而言还不足以表明延续提供了一种"
      "概念化scope-taking的优雅方式, "
      "考虑到也有其他并不显式牵涉延续但能够有效处理scope-taking的策略, "
      "例如量词提升 (Quantifier Raising), 灵活Montague语法 "
      "(Flexible Montague Grammar), 诸如此类. "
      "为了有力地论证延续何以基本, "
      "我们必须说明延续提供了哪些其他方法所不能提供的洞察.")
   (P "我们从邻近的领域中寻找这种洞察的线索. "
      "在计算机编程语言中, 延续已经是在某种程度上深入探索的想法了, "
      "其已经被用来(在许多应用中)刻画计算机程序中的表达式的"
      (B "求值顺序 (order of evaluation)")
      ", 第12.1节解释了这种应用. "
      "一般而言, 延续的一个突出优点在于其提供了"
      "推理计算展开顺序的显式手段.")
   (P "我们将会论证诸多自然语言的现象都依赖于求值顺序. "
      "这些包括量化绑定, 交叉效应 (crossover), reconstruction, "
      "negative polarity licensing, 以及驴照应 (donkey anaphora).")
   (P "具体来说, 中心结果之一是基于求值顺序的对于crossover的健壮解释."
      (Ol #:attr* '((start "5"))
          (Li (Ol #:attr* '((type "a"))
                  (Li "Everyone" (Sub $i) " loves his" (Sub $i) " mother.")
                  (Li "*His" (Sub $i) " mother loves everyone" (Sub $i) "."))))
      "当量词" (Em "everyone") "出现在代词" (Em "his") "之前时, "
      )
   (H2 "第I部分. 塔: 作用域和求值顺序")
   (H3 "第1章. 作用域和塔")
   (H4 "第1.1节. 作用域")
   
   ))
