#lang racket
(provide ski.html)
(require SMathML)
(define $zigrarr (Mo "&zigrarr;"))
(define-infix*
  (&zigrarr $zigrarr))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define (LAM X M)
  (: $lambda X $. M))
(define $space:1 (&space 1))
(define (APP M . N*)
  (apply : (add-between (cons M N*)
                        $space:1)))
(define-@lized-op*
  (@LAM LAM)
  (@APP APP))
(define ski.html
  (TnTmPrelude
   #:title "lambda到SKI"
   #:css "styles.css"
   (H1. $lambda "到SKI")
   (H2. "引论")
   (P "既然Curry [2]已经确定性且构造性地刻画了任何lambda表达式"
      "都可以被转换为SKI组合子, 那么似乎无需重访这个议题. "
      "尽管如此, 它还是持续性地产生吸引力: "
      "对于数学家而言, 检视lambda项和图之间的联系, "
      "对于理论计算机科学家而言, 研究这个过程的复杂度 [4].")
   (P "一个令人意外的转变是, 从lambda项到SKI组合子的转换, "
      "原先认为是纯粹学术性的, 实际上是相当实用的. "
      "David Turner使用这种转换作为其函数式语言SASL的编译技术 [12, 13], "
      "以及后来的Miranda. 这种转换或者说编译的令人熟悉的呈现方式, "
      "其被称为" (Q "括号抽象 (bracket abstraction)")
      ", 应该归功于Schoenfinkel [8]. Turner打磨并优化了这个转换, "
      "使其实用化并流行开来. Peyton Jones关于实现函数式语言的书籍有一整章 "
      "[7, Chap. 16]致力于SKI转换, 其将它形容为"
      (Q "有吸引力的, 鉴于其导出了一个极其简单的归约机器")
      ". 这本书提及了两个围绕SKI规约设计的物理机器: "
      "Cambridge SKIM machine [11]和Burroughs NORMA.")
   (P "不过, 括号抽象是一颗珍珠. 在作为背景的第2章进行形式化呈现之前, "
      "我们也无法忍住一探其光辉. 这个转换将lambda项转换为S, K, I"
      "组合子的应用, 而组合子本身的归约呈现于图1的左列.")
   (P (MB (&Table
           ($ "归约" "编译规则")
           ($I (&zigrarr (ap $I $x) $x)
               (&\|-> (LAM $x $x) $I))
           ($K (&zigrarr (APP $K $y $x) $y)
               (&\|-> (LAM $x $e) (ap $K $e)))
           ($S (&zigrarr (APP $S $f $g $x)
                         (ap (@ap $f $x) (@ap $g $x)))
               (&\|-> (LAM $x (APP $e_1 $e_2))
                      (APP $S (@LAM $x $e_1) (@LAM $x $e_2))))))
      (B "图1. ")
      "SKI组合子的归约和编译规则. 在" $K "的编译规则之中, "
      $e "是一个组合子或者异于" $x "的变量. "
      "{译注: 这里说的组合子指的就是在编译过程中会添加的"
      $S ", " $K ", " $I ", 而非封闭的lambda表达式之意.}")
   (P "这个转换是以图1的右列的三条简单规则为基础的重写. "
      $I "规则和" $K "规则是对于其归约的重述; "
      "如果注意到一个可能含有自由变量" $x "的项" $e
      "等于" (ap (@LAM $x $e) $x) ", 那么" $S
      "规则也是显然的. 现在来看一个例子, "
      (LAM $x (LAM $y (APP $y $x)))
      ". 对于这个表达式, 我们只能对于内层的"
      "lambda抽象使用" $S "规则, 得到"
      (LAM $x (APP $S (@LAM $y $y) (@LAM $y $x)))
      ". 对于这个, 我们可以应用" $K "规则和" $S
      "规则, 给出"
      (LAM $x (APP (@ap $S $I) (@ap $K $x)))
      ". 再使用三次" $S "规则, 最后可以得到"
      (APP $S (@APP $S (@ap $K $S) (@ap $K $I))
           (@APP $S (@ap $K $K) $I))
      ". 这个结果比最初的项还要更大: "
      "我们将要在第6章解决大小爆炸问题.")
   (P "我们呈现了另外一个从lambda项到SKI组合子的转换珍珠, "
      "并炫耀了其facets. 它来源于一个非常不同的生蚝. "
      "{译注: 前面这两句话使用了比喻.} "
      "我们的转换不基于 (句法性的, 从本质上说) 重写. "
      "转而, 我们基于组合子定义了一个(可能是开的)"
      "lambda项的语义模型 {译注: 开 (open) 是形容lambda项的}, "
      "沿着以可复合的方式计算项的含义的道路, "
      "也就是说一个项的语义是由其直接孩子 (immediate children) "
      "的含义得到的. 一个封闭lambda项的含义被设计为其相应的SKI项. "
      "我们的转换的优点在于避免了检查变量相等性或者自由出现这样的操作. "
      "{译注: 所谓检查自由出现, 指的是判断一个变量是否自由出现于一个项之中.} "
      "尽管括号抽象"
      )
   (H2. $lambda "演算和SKI演算和括号抽象")
   (P "本章重述了lambda演算和组合子演算, "
      "以及经典的括号抽象. "
      "其主要是引入了论文剩余部分会用到的记号.")
   (P "图2呈现了演算的句法, 还有记号约定. "
      "对于带名lambda演算的表达式, 我们记以" $e
      "; 而对于使用de Bruijn索引的lambda演算的表达式, "
      "我们记以" $e^0 ", 但并不绝对, "
      "当根据上下文没有歧义时, 也将其记为" $e
      ". 小写字母 (可能装饰以下标或者上标) "
      "总是变量. 我们既考虑无类型演算也考虑简单类型演算. "
      "在后一种情况下, 类型 (指称以元变量"
      (&cm $alpha $beta $gamma $sigma $tau)
      ") 是基类型和箭头类型, 而没有类型变量. "
      $Gamma "代表一个可能为空的类型序列, 而"
      (^ $Gamma $+) "代表一个非空的类型序列. "
      
      )
   (H2. "语义转换")
   (P "本章呈现了我们的转换, 先是直觉性的, 然后是形式化的. "
      
      )
   ))