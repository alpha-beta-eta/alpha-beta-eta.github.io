#lang racket
(provide hott.html)
(require SMathML)
(define (prop= a b A)
  (: a (_ $= A) b))
(define (judg= a b A)
  (&: (&≡ a b) A))
(define (Cite #:attr* [attr* '()] id)
  `(cite ,attr* ,(Ref id)))
(define (&label x . t*)
  (Table #:attr* '((class "label") (align "center"))
         (Tr (Td x))
         (Tr (keyword-apply
              Td '(#:attr*) '(((align "center")))
              t*))))
(define (footnote-cite %entry)
  (define id (%entry-id %entry))
  (define href (string-append "#" id))
  (define index (%entry-index %entry))
  `(sup () (a ((href ,href)) ,(format "~a" index))))
(define (footnote-present %entry attr* . html*)
  (define index (%entry-index %entry))
  (define id (%entry-id %entry))
  `(p ,(attr*-set attr* 'class "footnote" 'id id)
      ,(Sup (format "~a" index)) . ,html*))
(define (Footnote #:id [id #f] #:attr* [attr* '()] . html*)
  `(,(build-%entry #:local? #f #:class "footnote"
                   #:id id #:present footnote-present
                   #:cite footnote-cite)
    ,attr* . ,html*))
(define (table-cite %entry)
  (define id (%entry-id %entry))
  (define href (string-append "#" id))
  (define index (%entry-index %entry))
  `(a ((href ,href)) ,(format "~a" index)))
(define (table-present %entry attr* table . html*)
  (define index (%entry-index %entry))
  (define id (%entry-id %entry))
  (set-attr*
   (if (null? html*)
       (&label table (format "表格~a" index))
       (apply &label table (format "表格~a: " index)
              html*))
   'class "table" 'id id))
(define (LabelledTable #:id [id #f] . html*)
  `(,(build-%entry #:local? #f #:class "table"
                   #:id id #:present table-present
                   #:cite table-cite)
    () . ,html*))
(define (heading-present %heading attr* . html*)
  (define level (%heading-level %heading))
  (define auto? (%heading-auto? %heading))
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  (cond ((= level 4) `(h4 ,(attr*-set attr* 'id id)
                          ,(format "第~a小节 " (format-section section))
                          . ,html*))
        ((= level 3) `(h3 ,(attr*-set attr* 'id id)
                          ,(format "第~a节 " (format-section section))
                          . ,html*))
        ((= level 2) `(h2 ,(attr*-set attr* 'id id)
                          ,(format "第~a章 " (format-section section))
                          . ,html*))
        ((= level 1) `(h1 ,attr* . ,html*))
        (else (error 'heading-present "invalid level ~s" level))))
(define (format-section section)
  (define sec (cdr (reverse section)))
  (apply string-append
         (add-between (map number->string sec) ".")))
(define (H1 #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present heading-present #:level 1)
    ,attr* . ,html*))
(define (H2 #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present heading-present #:level 2)
    ,attr* . ,html*))
(define (H2: #:attr* [attr* '()] . html*)
  `(,(build-%heading #:level 2 #:auto? #f)
    ,attr* . ,html*))
(define (H3 #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present heading-present #:level 3)
    ,attr* . ,html*))
(define (H3: #:attr* [attr* '()] . html*)
  `(,(build-%heading #:level 3 #:auto? #f)
    ,attr* . ,html*))
(define (H4 #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present heading-present #:level 4)
    ,attr* . ,html*))
(define (Td* . x*)
  (apply Tr (map Td x*)))
(define Type:Void (set-attr* $0 'mathvariant "bold"))
(define Type:Unit (set-attr* $1 'mathvariant "bold"))
(define $=> (Mo "&rArr;"))
(define $≡ (Mo "&equiv;"))
(define $:≡ (Mo ":&equiv;"))
(define-infix*
  (&disj $disj)
  (&conj $conj)
  (&=> $=>)
  (&≡ $≡)
  (&:≡ $:≡)
  
  )
(define-@lized-op*
  (@= &=)
  (@: &:)
  
  )
(define hott.html
  (TnTmPrelude
   #:title "同伦类型论"
   #:css "styles.css"
   (H1 "同伦类型论")
   (H2: "引论")
   (LabelledTable
    #:id "correspondence"
    (Table
     (Tr #:attr* '((align "left")) (Th "类型") (Th "逻辑") (Th "集合") (Th "同伦"))
     (Td* $A "命题" "集合" "空间")
     (Td* (&: $a $A) "证明" "元素" "点")
     (Td* (app $B $x) "谓词" "集合族" "纤维")
     (Td* (&: (app $b $x) (app $B $x)) "条件证明" "元素族" "截断")
     (Td* (&cm Type:Void Type:Unit) (&cm $bottom $top) (&cm $empty (setE $empty))
          (&cm $empty (Mi "&#8270;")))
     (Td* (&+ $A $B) (&disj $A $B) "无交并" "余积")
     (Td* (&c* $A $B) (&conj $A $B) "序对的集合" "积空间")
     (Td* (&-> $A $B) (&=> $A $B) "函数的集合" "函数空间")
     
     )
    "类型论操作的不同观点之对比"
    )
   (H2 "类型论")
   (H3 "类型论和集合论的对比")
   (P "同伦类型论是数学的一种基础性语言, 即Zermelo-Fraenkel集合论的一种替代品. "
      "然而, 其表现与集合论存在诸多重要的相异之处, 而这需要一些适应. "
      "仔细解释这些不同之处需要我们比本书的其余部分更加形式化. "
      "正如在引论中所言, 我们的目的在于" (Em "非形式化") "地写下类型论; "
      "但是对于习惯于集合论的数学家而言, 最初更多的精确性有助于避免一些"
      "常见的误解和错误.")
   (P "我们注意到一个集合论式的基础拥有两个" (Q "层次") ": "
      "一阶逻辑的演绎系统, 以及在这个系统内所刻画的某个特定理论 (例如ZFC) 的公理. "
      "因此, 集合论不仅是关于集合的, 更是关于集合 (第二层次的对象) 和"
      "命题 (第一层次的对象) 的交互作用的.")
   (P "与之相对的是, 类型论是其自身的演绎系统: 它不需要在其中刻画任何的超越结构, "
      "例如一阶逻辑. 集合论拥有两个基本概念, 集合和命题, 而类型论只有一个基本概念: "
      (Em "类型") ". 命题 (可以对其进行证明, 证伪, 假设, 否定等行为的陈述"
      (Cite "prop") ") 被等同于特定的类型, 通过表格" (Cite "correspondence")
      "所展示的对应关系. 因此, " (Em "证明一个定理") "这样的数学活动被等同于"
      (Em "构造一个对象") "这样的数学活动的一种特殊情况, "
      "这个对象即代表一个命题的类型的一个居民.")
   (Footnote
    #:id "prop"
    "令人困惑的是, 习惯上 (可以追溯至Euclid) 术语" (Q "命题 (proposition)")
    "和" (Q "定理 (theorem)") "近乎同义. 我们将自限于逻辑学家使用术语的方式, "
    (Em "命题") "是" (Em "可以进行证明活动(susceptible to proof)")
    "的陈述, 而" (Em "定理") " (或者" (Q "引理") "或者" (Q "推论")
    ") 是" (Em "已经被证明") "的陈述. 因此, " (Q (&= $0 $1)) "及其否定"
    (Q (&neg (@= $0 $1))) "都是命题, 但只有后者才是定理.")
   (P "这将我们导向了另外一个类型论和集合论的不同之处, 但是为了解释这个不同, "
      "我们必须说点关于一般演绎系统的事情. 非形式化地说, 一个演绎系统是用以推导被称为"
      (B "判断") "的东西的一集" (B "规则") ". 如果我们将演绎系统当成是形式游戏, "
      "那么判断就是游戏中我们根据游戏规则抵达的" (Q "位置") ". 我们也可以将"
      "演绎系统想成是某种代数理论, 在这种情况下判断是元素 (就像是一个群的元素) 而"
      "演绎规则是运算 (类似于群的乘法). 从逻辑角度来看, 判断可以被认为是" (Q "外部")
      "陈述, 活在元理论之中, 与理论本身的" (Q "内部") "陈述相对.")
   (P "在一阶逻辑 (集合论基于此) 的演绎系统之中, 只存在一种判断: "
      "一个给定的命题拥有一个证明. 也就是说, 每个命题" $A "都产生了一个判断"
      (Q $A "拥有一个证明") ", 而且所有的判断都具有这种形式. "
      (Q "从" $A "和" $B "可以推出" (&conj $A $B)) "这样的一阶逻辑规则"
      "实际上是一种" (Q "证明构造") "规则, 其是在说给定判断" (Q $A "拥有一个证明")
      "和" (Q $B "拥有一个证明") ", 我们可以推出" (Q (&conj $A $B) "拥有一个证明")
      ". 注意到判断" (Q $A "拥有一个证明") "存在于和" (Em "命题") $A
      "不同的层次, 其是理论的内部陈述.")
   (P "类型论的基本判断, 同" (Q $A "拥有一个证明") "类似, 写作" (Q (&: $a $A))
      ", 读作" (Q "项" $a "具有类型" $A) ", 或者更不严谨地说, "
      (Q $a "是" $A "的一个元素") " (或者, 在同伦类型论中, "
      (Q $a "是" $A "的一个点") "). 当" $A "是一个代表命题的类型时, 那么"
      $a "可以被称为" $A "的可证明性的" (Em "见证者") ", 或者说" $A
      "的真性的" (Em "证据") " (或者甚至说是" $A "的一个" (Em "证明")
      ", 但是我们将会尽力避免使用这个令人困惑的术语). 在这种情况下, 判断"
      (&: $a $A) "在类型论中是可推导的 (对于某个" $a ") 恰当类似的判断"
      (Q $A "拥有一个证明") "在一阶逻辑中是可推导的 (抛开假定的公理和"
      "数学的编码上的差异, 这我们将在整本书中进行讨论).")
   (P "从另一方面来说, 如果类型" $A "以更近于集合而非命题的方式对待 "
      "(尽管我们将看到, 区别有时是模糊的), 那么" (Q (&: $a $A))
      "就类似于集合论式的陈述" (Q (∈ $a $A)) ". 然而, 这里存在一个本质性的不同, 在于"
      (Q (&: $a $A)) "是" (Em "判断") "而" (Q (∈ $a $A)) "是" (Em "命题")
      ". 具体来说, 当位于类型论的内部进行处理的时候, 我们不能作出例如"
      (Q "如果" (&: $a $A) ", 那么就不会有" (&: $b $B)) "这样的陈述, 也不能"
      (Q "证伪") "判断" (Q (&: $a $A)) ".")
   (P "一种思考该点的好方法是, 在集合论中, " (Q "成员资格 (membership)")
      "是一种关系, 可能也可能不在两个预先存在的对象" (Q $a) "和" (Q $A)
      "之间成立, 而在类型论中, 我们不能单独讨论一个元素" (Q $a)
      ": 每个元素, " (Em "究其本质而言") ", 都是某个类型的元素, 并且"
      "这个类型 (一般而言) 是唯一确定的. 因此, 当我们非形式化地说"
      (Q "令" $x "是一个自然数") "时, 在集合论中这是"
      (Q "令" $x "是某个东西并假定" (∈ $x $NN)) "的缩写, 而在类型论中, "
      (Q "令" (&: $x $NN)) "是一个原子陈述: 我们不能引入一个变量但不刻画其类型.")
   (P "第一眼看上去, 这似乎是令人不快的限制, 但是或许也可以说这更接近于"
      (Q "令" $x "是一个自然数") "在数学上的直觉含义. 在实践中, 似乎每当我们实际上"
      (Em "需要") (Q (∈ $a $A)) "是一个命题而非判断时, 总是存在一个包覆"
      $A "的集合" $B ", 此时我们知道" $a "是" $B "的一个元素并且" $A
      "是" $B "的一个子集. 这种情况在类型论中也是容易表示的, 取" $a
      "是类型" $B "的一个元素, 而" $A "是" $B "上的一个谓词; 见第3.5节.");@@@
   (P "类型论和集合论之间的最后一点不同在于对于相等的处理. "
      "数学中为人熟知的相等概念是一种命题: 例如, 我们可以证伪一个相等性或者"
      "将一个相等性当作假设. 既然在类型论中, 命题也是类型, 这意味着"
      "一个相等性也是一个类型: 对于元素" (&: (&cm $a $b) $A)
      " (即" (&: $a $A) "且" (&: $b $A) ") 而言, 我们有类型"
      (Q (prop= $a $b $A)) ". (当然, 在" (Em "同伦")
      "类型论中, 这种相等命题的表现可能相当不同寻常: 见第1.12节和第2章, 以及全书的剩余部分.) 当"
      (prop= $a $b $A) "存在居民 (is inhabited) 时, 我们称" $a "和" $b
      "是" (B "(命题)相等的") ".");@@@
   (P "然而, 在类型论中, 也有相等" (Em "判断") "的需要, 其存在平行于判断"
      (Q (&: $x $A)) "的层次. 这被称为" (B "判断相等") "或者" (B "定义相等")
      ", 我们将其记为" (judg= $a $b $A) "或者就简记作" (&≡ $a $b)
      ". 将这种相等想成是" (Q "根据定义相等") "比较好. 例如, 如果我们根据等式"
      (&= (app $f $x) $x^2) "定义了一个函数" (func $f $NN $NN)
      ", 那么" (Em "根据定义(by definition)") "表达式" (app $f $3)
      "等于" $3^2 ". 在理论内部, 否定或者假定一个定义相等性是没有意义的; 我们不能说"
      (Q "如果根据定义" $x "等于" $y ", 那么根据定义" $z "不等于" $w)
      ". 两个表达式根据定义相等或者不相等这种判断只和展开定义有关; "
      "具体来说, 这是算法上可判定的 (尽管此算法必然是元理论性质的, 而非位于理论内部).")
   (P "随着类型论变得愈发复杂, 判断相等也可能变得愈发微妙起来, 但以上的直觉是良好的起点. "
      "从另外一种角度看, 如果我们将演绎系统当作代数理论, 那么判断相等不过就只是"
      "这个理论之中的相等, 类似于一个群的元素之间的相等. 唯一可能造成混乱的地方在于"
      "演绎系统的" (Em "内部") "同样" (Em "也") "存在着从内部表现为" (Q "相等")
      "的概念对象, 即类型" (Q (&= $a $b)) ".")
   (P "我们" (Em "想要") "相等的判断概念的原因在于它可以控制其他形式的判断, "
      (Q (&: $a $A)) ". 例如, 设给定了一个" (&= $3^2 $9) "的证明, 即对于某个" $p
      "推导出了" (&: $p (@= $3^2 $9)) ", 那么相同的见证者" $p
      "也应该可以算作" (&= (app $f $3) $9) "的证明, 因为" (Em "根据定义")
      (app $f $3) "就是" $3^2 ". 表示这种想法的最好方式在于通过一个规则言称, "
      "给定判断" (&: $a $A) "和" (&≡ $A $B) ", 我们可以推导出判断" (&: $a $B) ".")
   (P "因此, 对于我们来说, 类型论会是基于以下两种形式的判断的演绎系统:"
      (Table
       #:attr* '((style "margin: auto;"))
       (Tr (Th "判断") (Th "意义"))
       (Td* (&: $a $A) (Q $a "是具有类型" $A "的一个对象"))
       (Td* (judg= $a $b $A) (Q $a "和" $b "是定义上相等的具有类型" $A "的对象")))
      "当引入一个定义相等时, 即定义某个东西与另一个相等时, 我们将使用符号"
      (Q $:≡) ". 因此, 上述对于函数" $f "的定义将会写成"
      (&:≡ (app $f $x) $x^2) ".")
   (P "因为判断不能放在一起以构造更复杂的陈述, 符号" (Q $:) "和" (Q $≡)
      "的绑定比其他任何符号都更松散. 因此, 例如" (Q (&: $p (&= $x $y)))
      ", 应该被理解为" (Q (&: $p (@= $x $y))) ", 这是有意义的, 因为"
      (Q (&= $x $y)) "是一个类型, 但是不能被理解为" (Q (&= (@: $p $x) $y))
      ", 这是无意义的, 因为" (Q (&: $p $x))
      "是一个判断, 其不能等于任何东西. 类似地, "
      (Q (&≡ $A (&= $x $y))) "只能被理解为" (Q (&≡ $A (@= $x $y)))
      ", 尽管在这样极端的情况下, 我们应该加上括号以辅助阅读理解. "
      "而且, 之后我们还将采用通常的相等链式记号, 例如写下"
      (&= $a $b $c $d) "以表达"
      (Q (&= $a $b) ", " (&= $b $c) ", " (&= $c $d) ", 因而" (&= $a $d))
      ". 并且, 我们也将包括判断相等于这样的链中. 上下文总是能够使得我们的意图清晰.")
   (P "这里或许也是提及另外一种常见数学记号" (Q (func $f $A $B))
      "的适合地方了, 其表达了" $f "是从" $A "到" $B "的一个函数的事实, "
      "可以被视为一个类型判断, 因为我们使用" (Q (&-> $A $B))
      "作为从" $A "到" $B "的函数的类型 (这是类型论的标准实践; 见第1.4节).");@@@
   (P "判断可以依赖于具有形式" (&: $x $A) "的" (Em "假设")
      ", 其中" $x "是一个变量而" $A "是一个类型. 例如, 我们可以"
      )
   (H3 "函数类型")
   (H3 "宇宙和族")
   (H3 "依赖函数类型 (" $Pi:normal "类型)")
   (H3 "积类型")
   (H3 "依赖序对类型 (" $Sigma:normal "类型)")
   (H3 "余积类型")
   (H3 "布尔的类型")
   (H3 "自然数")
   (H3 "模式匹配和递归")
   (H3 "命题作为类型")
   (H3 "相等类型")
   (H3: "注记")
   (H3: "练习")
   (H2 "同伦类型论")
   (H3 "类型是高阶群胚")
   (H3 "函数是函子")
   (H3 "类型族是纤维")
   (H3 "同伦和等价")
   (H2 "集合和逻辑")
   (H2 "等价")
   (H2 "归纳")
   (H2 "高阶归纳类型")
   (H2 "同伦" $n "类型")
   
   ))