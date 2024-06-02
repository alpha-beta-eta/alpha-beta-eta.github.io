#lang racket
(provide judgmental.html)
(require SMathML)
(define App (&split 2))
(define split16 (&split 16))
(define $impl $sup)
(define $diamond (Mo "&diamond;"))
(define (&diamond x)
  (ap $diamond x))
(define $box (Mo "&square;"))
(define (&box x)
  (ap $box x))
(define $○
  (set-compact (Mo "○")))
(define (&○ M) (ap $○ M))
(define (&rull label . x*)
  (: (apply &rule x*) label))
(define $conj:id (Mi "&and;"))
(define $conjF (: $conj:id $F))
(define $conjI (: $conj:id $I))
(define $conjE_L (: $conj:id $E_L))
(define $conjE_R (: $conj:id $E_R))
(define $prop (Mi "prop"))
(define (&prop A)
  (App A $prop))
(define (&true A)
  (App A $true))
(define-infix*
  (&impl $impl))
(define-@lized-op*
  (@box &box)
  (@diamond &diamond)
  (@○ &○))
(define judgmental.html
  (TnTmPrelude
   #:title "模态逻辑的判断性重构"
   #:css "styles.css"
   (H1. "模态逻辑的判断性重构")
   (P "我们遵循Martin-Löf区分判断与命题的方法论, "
      "重新考察模态逻辑的基础. "
      "我们为必然性与可能性给出了构造性的意义解释, "
      "由此得到一个简单而统一的直觉主义模态逻辑的自然演绎系统, "
      "该系统不会出现其他方案中所存在的反常现象. "
      "我们还给出了lax逻辑的一种新表述, "
      "并发现lax模态已经可以用可能性与必然性表达出来. "
      "通过对模态逻辑中证明的计算解释, "
      "我们进一步得到了Moggi的单子元语言 "
      "(monadic metalanguage) 的一种新形式.")
   (H2. "引论")
   (P "在本文中, 我们遵循Martin-Löf (1996) "
      "区分判断与命题的方法论, "
      "重新考察模态逻辑的基础. "
      "我们为必然性 (" $box ") 与可能性 ("
      $diamond ") 给出构造性的意义解释. "
      "这项工作产生了一个简单而统一的"
      "直觉主义模态逻辑自然演绎系统, "
      "它不具有其他方案中出现的那些反常现象. "
      "我们还给出了lax逻辑 "
      "(Fairtlough和Mendler, 1997) 的一种新表述, "
      "并发现: 只要把lax模态" (&○ $A)
      "分解为" (&diamond (&box $A))
      ", 把lax推出" (&=> $A $B)
      "分解为" (&impl (@box $A) $B)
      ", 它其实已经包含在模态逻辑之中. "
      "通过对模态逻辑中证明的计算解释, "
      "我们进一步得到了Moggi的单子元语言 "
      "(Moggi, 1998; 1989; 1991) 的一种新表述, "
      "它综合并系统化了S. Kobayashi (1997) 以及"
      "Benton, Bierman和de Paiva (1998) 此前的工作.")
   (P "在判断这一层面上, 上述展开所需的原始概念少得令人惊讶. "
      "具体而言, 我们只需要假言判断 (hypothetical judgment) 来解释推出, "
      "只需要直言判断 (categorical judgment) 来解释诸模态. "
      "至此, 我们为模态逻辑及其计算解释的构造性理解获得了一个令人满意的基础.")
   (H2. "判断和命题")
   (P "在1983年的锡耶纳讲座 (最终于1996年出版) 中, "
      "Martin-Löf基于对判断与命题这两个概念的清晰区分, 为逻辑给出了一个基础. "
      "他的论证是: 判断即知道, 而一个自明的 (evident) 判断就是一个知识的对象. "
      "证明则是使判断成为自明的东西. 在逻辑中, 我们作出诸如"
      (Q $A "是一个命题") "或" (Q $A "为真")
      "这样的具体判断, 其中后者预设了我们已经知道" $A
      "是一个命题. 知道" (Q $A "是一个命题")
      "意味着知道什么算作" $A "的一个验证 (verification), 而知道"
      (Q $A "为真") "则意味着知道如何去验证" $A
      ". 用他自己的话说 (Martin-Löf, 1996, 第27页):"
      (Blockquote
       "一个命题的意义由 [...] 什么算作对它的验证所决定."))
   (P "这一进路带来了一种明确的概念上的优先次序: "
      "我们首先需要理解判断以及判断之证据 (evidence) 这两个概念, "
      "然后再通过命题以及命题之验证这两个概念来理解真.")
   (P "作为例子, 我们考虑合取的解释. "
      "我们知道, 若" $A "与" $B "都是命题, 则" (&conj $A $B)
      "是一个命题. 作为一条推理规则 (称为合取的形成规则):"
      (MB (&rull $conjF
                 (&prop $A) (&prop $B)
                 (&prop (&conj $A $B)))))
   (P "其意义则通过陈述什么算作" (&conj $A $B)
      "的一个验证来给出. 我们说, 当我们同时拥有"
      $A "与" $B "的验证时, 我们就拥有了" (&conj $A $B)
      "的一个验证. 作为一条推理规则:"
      (MB (&rull $conjI
                 (&true $A) (&true $B)
                 (&true (&conj $A $B))))
      "其中我们预设" $A "与" $B "已知都是命题. "
      "这被称为引入规则 (introduction rule), "
      "该术语来自 Gentzen (1935), "
      "他最早表述了一个自然演绎系统. "
      "反过来, 如果我们知道"(&conj $A $B)
      "为真, 我们又知道了什么? 既然"
      (&conj $A $B) "的一个验证由"
      $A "与" $B "二者的验证构成, 我们便知道"
      $A "必定为真且" $B "必定为真. "
      "表述为推理规则 (称为合取的消去规则):"
      (MB (split16
           (&rull $conjE_L
                  (&true (&conj $A $B))
                  (&true $A))
           (&rull $conjE_R
                  (&true (&conj $A $B))
                  (&true $B)))))
   (P "由上面的解释可以清楚地看出, "
      "这两条消去规则是可靠的 (sound): "
      "如果我们用合取的引入规则来定义其意义, 那么当"
      (&conj $A $B) "为真时断定" $A
      "为真就是完全有理据的, 第二条规则同理.")
   (P "可靠性保证了消去规则不会过强. "
      "只要我们对前提中的判断拥有充分的证据, "
      "我们对结论中的判断就也拥有充分的证据. "
      "这一点由一个局部归约 (local reduction) 所见证, "
      "它从前提的证据构造出结论的证据."
      
      )
   (H2. "假言判断和推出")
   
   ))