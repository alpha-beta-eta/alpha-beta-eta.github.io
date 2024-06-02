#lang racket
(provide hott.html)
(require SMathML)
(define (_cm A . x*)
  (_ A (apply &cm x*)))
(define $★ (Mi "&Star;"))
(define $prod (Mo "&prod;"))
(define &split:16 (&split 16))
(define (Pi x A Bx #:style [style #f])
  (set-attr*
   (prod (@: x A) Bx)
   'displaystyle style))
(define $Fin (Mi "Fin" #:attr* '((mathvariant "sans-serif"))))
(define (&Fin n) (app $Fin n))
(define $fmax (Mi "fmax" #:attr* '((mathvariant "sans-serif"))))
(define (&fmax n) (app $fmax n))
(define $id (Mi "id" #:attr* '((mathvariant "sans-serif"))))
(define (&id x #:type [A #f])
  (if (eq? A #f)
      (app $id x)
      (app (_ $id A) x)))
(define $swap (Mi "swap" #:attr* '((mathvariant "sans-serif"))))
(define Universe $U:script)
(define Universe_inf (_ Universe $inf))
(define Universe_0 (_ Universe $0))
(define Universe_1 (_ Universe $1))
(define Universe_2 (_ Universe $2))
(define Universe_i (_ Universe $i))
(define Universe_i+1 (_ Universe (&+ $i $1)))
(define UnitType
  (Mi "1" #:attr* '((mathvariant "bold"))))
(define ((tcomment #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "tcomment")))
   (B (format "译者注记~a." n)) " " x*))
(define @-> (@lize &->))
(define $blank (Mi "-"))
(define @\|-> (@lize &\|->))
(define (prop= a b A)
  (: a (_ $= A) b))
(define (judg= a b A)
  (&: (&≡ a b) A))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define lam
  (case-lambda
    ((var type exp) (: $lambda (@: var type) $. exp))
    ((var exp) (: $lambda var $. exp))))
(define @lam (@lize lam))
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
(define (heading-cite %heading)
  (define id (%heading-id %heading))
  (define href (string-append "#" id))
  (define section (%heading-section %heading))
  `(a ((href ,href)) ,(format-section section)))
(define (H1 #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present heading-present #:level 1)
    ,attr* . ,html*))
(define (H2 #:attr* [attr* '()] #:id [id #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite #:level 2 #:id id)
    ,attr* . ,html*))
(define (H2: #:attr* [attr* '()] . html*)
  `(,(build-%heading #:level 2 #:auto? #f)
    ,attr* . ,html*))
(define (H3 #:attr* [attr* '()] #:id [id #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite #:level 3 #:id id)
    ,attr* . ,html*))
(define (H3: #:attr* [attr* '()] . html*)
  `(,(build-%heading #:level 3 #:auto? #f)
    ,attr* . ,html*))
(define (H4 #:attr* [attr* '()] #:id [id #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite #:level 4 #:id id)
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
(define (format-num section index)
  (define sec (cdr (reverse (cons index section))))
  (define numbering
    (apply string-append
           (add-between (map number->string sec) ".")))
  numbering)
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B (format "~a~a. " name num))
        (B (format "~a. " name)))))
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
    (Cite name `(a ((href ,href)) ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Remark "评注" "remark")
  
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
      "将一个相等性当作前提. 既然在类型论中, 命题也是类型, 这意味着"
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
      ", 其中" $x "是一个变量而" $A "是一个类型. 例如, 我们可以在"
      (&: (&cm $m $n) $NN) "的假设下构造一个对象" (&: (&+ $m $n) $NN)
      ". 另外一个例子是假设" $A "是一个类型, " (&: (&cm $x $y) $A)
      ", 以及" (&: $p (prop= $x $y $A)) ", 那么我们可以构造一个元素"
      (&: (inv $p) (prop= $y $x $A)) ". 所有这样的假设构成的合集被称为"
      (B "上下文 (context)") "; 从拓扑的角度来看, 其可以被想成是一个"
      (Q "参数空间") ". 实际上, 从技术上来说上下文必须是假设的有序列表, "
      "因为后面的假设可以依赖于之前的假设: 假设" (&: $x $A) "只能在假定任意的变量在"
      $A "中出现" (Em "之后") "作出. [译注: 这里译者也没看明白原文的含义.]")
   (P "如果一个假设" (&: $x $A) "中的类型" $A "代表一个命题, 那么这个假设是类型论版本的"
      (Em "前提(hypothesis)") ": 我们假定命题" $A "成立. 当类型被视为命题时, "
      "我们可以省略证明的名字. 因此, 在上面的第二个例子里, 我们也可以说假定"
      (prop= $x $y $A) ", 我们可以证明" (prop= $y $x $A)
      ". 然而, 鉴于我们正在做" (Q "证明相关") "的数学, 所以我们也将经常"
      "回头指明证明为对象. 例如, 在上面的例子里, 我们想要建立这样的想法, 即"
      (inv $p) "连带着传递性和自反性的证明就表现得如同一个群胚; 见第"
      (Cite "chapter_hott") "章.")
   (P "注意到在词汇" (Em "假设(assumption)") "的含义下, 我们可以假定一个命题相等性 "
      "(通过假设一个变量" (&: $p (&= $x $y)) "), 但是我们不能假定一个判断相等性"
      (&≡ $x $y) ", 因为这不是一个可以拥有元素的类型. 然而, 我们可以做些其他的事情, "
      "看起来就像是假定了一个判断相等性: 如果我们有一个牵涉变量" (&: $x $A)
      "的类型或者元素, 那么我们可以将" $x (Em "替换") "为任意具体的元素"
      (&: $a $A) "以得到一个更加特定的类型或者元素. 有时我们使用语言"
      (Q "现在假设" (&≡ $x $a)) "以指这种替换过程, 即便这并非前文所引入的技术含义上的"
      (Em "假设") ".")
   (P "根据相同的理由, 我们也不能" (Em "证明") "一个判断相等性, 因为这并非我们可以"
      "展示见证人的类型. 然而, 我们有时会陈述判断相等性作为定理的一部分, 例如"
      (Q "存在" (func $f $A $B) "满足" (&≡ (app $f $x) $y))
      ". 这应该被视为分别作出了两个判断: 首先我们作出了对于某个元素" $f
      "有" (func $f $A $B) "这一判断, 然后我们作出了"
      (&≡ (app $f $x) $y) "这一额外判断.")
   (P "本章的剩余部分, 我们将试图给出类型论的非形式化呈现, 这对于本书的目的而言是足够了; "
      "在附录A中我们给出了一个更加形式化的版本. 除了一些特别显然的规则 "
      "(例如判断相等的东西总是可以相互替换这一事实), 类型论的规则可以按照"
      (Em "类型形成子(type former)") "组织. 每个类型形成子都由一种构造类型的方式 "
      "(可能利用之前已经构造了的类型), 以及关于该类型的元素的构造和行为的规则构成. "
      "大多数情况下, 这些规则遵循着显而易见的模式, 但是现在我们还不会精确刻画这个想法; "
      "然而读者可以见第1.5节的开头和第5章.");@@@
   (P "本章所呈现的类型论的一个重要方面在于其仅由" (Em "规则")
      "构成, 没有任何" (Em "公理") ". 在对基于判断的演绎系统的描述里, "
      (Em "规则") "允许我们从一集其他的判断中推导 (conclude) 出一个判断, 而" (Em "公理")
      "是我们最初就拥有的判断. 如果我们将演绎系统想成是形式游戏, 那么规则就是"
      "游戏的规则, 而公理是起始位置. 如果我们将演绎系统想成是代数理论, 那么"
      "规则是理论的运算而公理是对于理论的某个特定自由模型而言的" (Em "生成元") ".")
   (P "在集合论中, 仅有的规则就是一阶逻辑的规则 (例如允许我们从" (Q $A "有一个证明")
      "和" (Q $B "有一个证明") "推导出" (Q (&conj $A $B) "有一个证明")
      "的规则): 关于集合的行为的所有信息都被包含在公理之中了. 与之相对的是, "
      "在类型论中, 通常是" (Em "规则") "涵盖了所有的信息, 而公理是没有必要的. 例如, 在第"
      (Cite "section_product_type") "节中我们将看到存在一条规则允许我们从"
      (Q (&: $a $A)) "和" (Q (&: $b $B)) "推导出判断" (Q (&: (tu0 $a $b) (&c* $A $B)))
      ", 而在集合论中类似的陈述将会是配对公理(的一个实例).")
   (P "只使用规则刻画类型论的优点在于规则是" (Q "过程性的") ". 具体来说, 这种性质使得"
      "类型论的良好计算性质成为可能 (但并不自动保证), 例如" (Q "canonicity")
      ". 然而, 尽管这种风格对于传统类型论有效, 我们还不全然理解以这种方式刻画对于"
      (Em "同伦") "类型论而言必要的一切. 具体地说, 在第2.9节和第2.10节以及第6章中, "
      "我们将引入额外的公理来增强本章所呈现的类型论规则, 特别是"
      (Em "泛等公理(univalence axiom)") ". 然而, 本章我们将自限于基于规则的传统类型论.")
   (H3 "函数类型")
   (P "给定类型" $A "和" $B ", 我们可以构造以" $A "为定义域而" $B "为陪域的"
      (B "函数") "的类型" (&-> $A $B) ". 有时我们也称函数为" (B "映射")
      ". 和集合论不同的是, 函数并不被定义为函数关系; 反而它们是类型论中的原始概念. "
      "我们通过描述可以对于函数做些什么, 如何构造函数, 以及函数所导出的相等性来解释函数类型.")
   (P "给定一个函数" (func $f $A $B) "和定义域的一个元素" (&: $a $A)
      ", 我们可以" (B "应用") "函数以得到陪域" $B "的一个元素, 记作" (app $f $a)
      ", 称为" $f "在" $a "处的" (B "值") ". 在类型论中省略括号是普遍的, 例如可以将"
      (app $f $a) "简记为" (ap $f $a) ", 有时我们也将这么做.")
   (P "但是我们该如何构造" (&-> $A $B) "的元素呢? 存在两种等价的方式: "
      "要么通过直接的定义, 要么使用" $lambda "抽象. 通过定义引入函数意味着"
      "我们在定义函数时赋予了其名字, 比如说" $f ", 我们可以说我们通过等式"
      (MB (&:≡ (app $f $x) $Phi:normal))
      "定义了一个函数" (func $f $A $B) ", 其中" $x "是一个变量而" $Phi:normal
      "是一个可以使用" $x "的表达式. 为了使得这个定义具有意义, 我们需要验证假定"
      (&: $x $A) "时可以推出" (&: $Phi:normal $B)
      ". [译注: 在某种意义上而言, 这里的假定不是假定.]")
   (P "现在我们可以通过替换" $Phi:normal "里的变量" $x "为" $a
      "来计算" (app $f $a) ". 作为一个例子, 考虑由"
      (&:≡ (app $f $x) (&+ $x $x)) "定义的函数" (func $f $NN $NN)
      ". (我们将于第" (Cite "section_nat") "节定义" $NN "和" $+
      ".) 那么, " (app $f $2) "判断相等于" (&+ $2 $2) ".")
   (P "如果我们不希望为函数引入名字, 我们可以使用" (B $lambda "抽象")
      ". 给定一个类型" $B "的表达式" $Phi:normal ", 其可以和上面一样使用"
      (&: $x $A) ", 我们记" (lam $x $A $Phi:normal)
      "以指和之前的表达式所定义的相同的函数. 因此, 我们有"
      (MB (&: (@lam $x $A $Phi:normal) (&-> $A $B)) ".")
      "对于前一节的例子, 我们有类型判断"
      (MB (&: (@lam $x $NN (&+ $x $x)) (&-> $NN $NN)) ".")
      "作为另外一个例子, 对于任意的类型" $A "和" $B "以及任意的元素"
      (&: $y $B) ", 我们有一个" (B "常函数")
      (&: (@lam $x $A $y) (&-> $A $B)) ".")
   (P "我们一般会省略" $lambda "抽象里变量" $x "的类型而记"
      (lam $x $Phi:normal) ", 因为定型判断" (&: $x $A)
      "可从函数" (lam $x $Phi:normal) "具有类型"
      (&-> $A $B) "这一判断推出. 根据惯例, 变量绑定"
      (Q (M $lambda $x $.)) "的" (Q "作用域")
      "是表达式的整个剩余部分, 除非被括号隔断. 因此, "
      (lam $x (&+ $x $x)) "应该被理解为"
      (lam $x (@+ $x $x)) "而不是" (&+ (@lam $x $x) $x)
      ", 当然后者不论如何都是病态类型 (ill-typed) 的.")
   (P "另一种等价的记号是"
      (MB (&: (@\|-> $x $Phi:normal) (&-> $A $B)) ".")
      "有时我们也可以在表达式" $Phi:normal
      "中用空白" (Q $blank) "代替变量以指称隐式的" $lambda
      "抽象. 例如, " (appl $g $x $blank) "是另一种书写"
      (lam $y (appl $g $x $y)) "的方式.")
   (P "现在" $lambda "抽象是函数, 于是我们可以将其应用于参数"
      (&: $a $A) ". 然后我们会有以下" (B "计算规则") (Cite "beta-conversion")
      ", 其是定义相等性:"
      (MB (&≡ (app (@lam $x $Phi:normal) $a)
              (&prime $Phi:normal)))
      "其中" (&prime $Phi:normal) "是将所有" $x
      "的出现替换为" $a "了的" $Phi:normal
      ". [译注: 其实是所有" $x "的自由出现, 而且需要避免意外的变量捕获.] "
      "继续上面的例子, 我们有"
      (MB (&≡ (app (@lam $x (&+ $x $x)) $2) (&+ $2 $2)) ".")
      "注意到由任意的函数" (func $f $A $B) ", 我们可以构造一个"
      $lambda "抽象" (lam $x (app $f $x))
      ". 因为根据定义这是" (Q "将" $f "应用于其参数的函数")
      ", 我们将其视为定义相等于" $f (Cite "eta-conversion") ":"
      (MB (&≡ $f (@lam $x (app $f $x))) ".")
      "这种相等性是" (B "函数类型的唯一性原则")
      ", 因为" $f "由其值唯一确定. "
      "[译注: 当然是在所有可能的元素上的值, 集合论学家会认为这是外延公理的推论.]")
   (Footnote
    #:id "beta-conversion"
    "这种相等性的运用经常被称为" (B $beta "变换") "或者" (B $beta "规约") ".")
   (Footnote
    #:id "eta-conversion"
    "这种相等性的运用经常被称为" (B $eta "变换") "或者" (B $eta "规约") ".")
   (P "通过带有显式参数的定义引入的函数也可以被规约为使用" $lambda "抽象的简单定义: "
      "例如, 我们可以将通过"
      (MB (&:≡ (app $f $x) $Phi:normal))
      "定义的函数" (func $f $A $B) "读作"
      (MB (&:≡ $f (lam $x $Phi:normal)) "."))
   (P "当进行牵涉变量的计算时, 我们必须在替换变量以同样牵涉变量的表达式时小心谨慎, "
      "因为我们想要保持表达式的绑定结构. 我们所说的" (Em "绑定结构")
      "指的是由绑定子 (例如" (&cm $lambda $Pi:normal $Sigma:normal)
      ", 很快我们就要遇到) 生成的在变量引入处与变量使用处之间的无形连接. "
      "作为一个例子, 考虑由"
      (MB (&:≡ (app $f $x) (lam $y (&+ $x $y))))
      "定义的函数" (func $f $NN (@-> $NN $NN))
      ". 现在如果我们已经在某处假定了" (&: $y $NN)
      ", 那么" (app $f $y) "是什么呢? 只是朴素地 (naively) 将表达式"
      (Q (lam $y (&+ $x $y))) "中的所有" $x "替换为" $y
      "是错误的, 因为这意味着" $y "会被" (B "捕获")
      ". 在替换之前, 被替换进去的" $y "指的是我们的假定, 但是现在它指的是"
      $lambda "抽象的参数. 因此, 朴素的替换将会破坏绑定结构, 允许我们执行"
      "语义上unsound的计算.")
   (P "但是这个例子中的" (app $f $y) "应该是什么呢? 我们注意到绑定 "
      "(或者" (Q "哑") ") 变量, 例如表达式" (lam $y (&+ $x $y))
      "中的" $y ", 只具有局部的意义, 可以被一致地替换为其他变量, "
      "仍然保持绑定结构. 诚然, " (lam $y (&+ $x $y)) "被声明为判断相等"
      (Cite "alpha-conversion") "于"
      (lam $z (&+ $x $z)) ". 然后, 我们知道" (app $f $y) "判断相等于"
      (lam $z (&+ $y $z)) ". (不止是" $z ", 任意不同于" $y
      "的变量都可以使用, 产生相等的结果.)")
   (Footnote
    #:id "alpha-conversion"
    "这种相等性的运用经常被称为" (B $alpha "变换") ".")
   (P "当然, 这些对于数学家而言都应该是熟悉的: 如果"
      (&:≡ (app $f $x) (integral $1 $2 (~ $1 (&- $x $t)) $t))
      ", 那么" (app $f $t) "不是"
      (integral $1 $2 (~ $1 (&- $t $t)) $t)
      "而是"
      (integral $1 $2 (~ $1 (&- $t $s)) $s)
      ", 这与之前的现象本质上是相同的. " $lambda
      "抽象绑定哑变量的方式与积分完全一致.")
   (P "我们已经看到该如何定义单变量的函数了. 定义多变量函数的一种方式是使用笛卡尔积, "
      "笛卡尔积将在之后引入: 参数的类型为" $A "和" $B "而结果的类型为"
      $C "的函数将被赋予类型" (&-> (&c* $A $B) $C) ". 然而, 还存在另外一种"
      "避免使用积类型的选择, 其被称为" (B "currying")
      " (以数学家Haskell Curry的名字命名).")
   ((tcomment)
    "currying虽然以逻辑学家Haskell Curry的名字命名, 但是并不是Curry本人发明的, "
    "这是他自己说的. 似乎许多人都发明过这种技术, 但具体应该归功于谁已不可考.")
   (P "currying的想法是表示具有两个输入" (&: $a $A) "和" (&: $b $B)
      "的函数以一个函数, 其接受" (Em "一个") "输入" (&: $a $A)
      ", 然后返回" (Em "另一个函数") ", 其将接受第二个输入"
      (&: $b $B) "并返回结果. 也就是说, 我们将两个变量的函数想成是"
      "属于一个迭代的函数类型, " (&: $f (&-> $A (@-> $B $C)))
      ". 我们也可以省略括号, 将其写成" (&: $f (&-> $A $B $C))
      ", 默认约定向右结合. 那么, 给定" (&: $a $A) "和" (&: $b $B)
      ", 我们可以应用" $f "于" $a ", 然后应用结果于" $b
      ", 得到" (&: (app (app $f $a) $b) $C)
      ". 为了避免括号大量增殖, 我们允许我们自己将"
      (app (app $f $a) $b) "写成" (appl $f $a $b)
      ", 甚至其实这里并没有积的参与. 当整个省略函数参数周围的括号时, 我们记"
      (ap (ap $f $a) $b) "或者" (ap (@ap $f $a) $b)
      ", 默认约定向左结合, 这就使得" $f "以正确的顺序应用于其参数.")
   (P "我们对于显式参数定义的记号也可以扩展到此类情况: 我们可以通过等式"
      (MB (&:≡ (appl $f $x $y) $Phi:normal))
      "以定义一个命名函数" (&: $f (&-> $A $B $C))
      ", 其中假定" (&: $x $A) "和" (&: $y $B) "时有" (&: $Phi:normal $C)
      ". 使用" $lambda "抽象, 这对应于"
      (MB (&:≡ $f (lam $x (lam $y $Phi:normal))))
      "其也可以写作"
      (MB (&:≡ $f (&\|-> $x $y $Phi:normal)))
      "我们也可以隐式地通过多个空白以作出多个变量的抽象, 例如"
      (appl $g $blank $blank) "的意思是"
      (lam $x (lam $y (appl $g $x $y)))
      ". currying三个或者更多参数的函数是对于我们之前"
      "所描述的方法的直截了当的扩展.")
   (H3 "宇宙和族" #:id "universes")
   (P "到目前为止, 我们只是在非形式化地使用表达式"
      (Q $A "是一个类型") ". 现在我们要将其变得更加精确, 通过引入"
      (B "宇宙 (universe)") ". 一个宇宙是一个其元素是类型的类型. "
      "就和在朴素集合论中一样, 或许我们希望拥有一个所有类型的宇宙"
      Universe_inf ", 其包含自身, 即" (&: Universe_inf Universe_inf)
      ". 然而, 就和在集合论中一样, 这是unsound的, 即我们可以从中推导出"
      "每个类型, 包括表示命题False的空类型 (见第"
      (Cite "section_coproduct_types") "节), 都拥有居民. 例如, "
      "通过将集合表示为树, 我们可以直接编码Russell悖论 [Coq92a].");@@@
   (P "为了避免悖论, 我们引入一个宇宙的层次结构"
      (MB (&: Universe_0 Universe_1 Universe_2 $..c))
      "其中每个宇宙" Universe_i "都是下一个宇宙" Universe_i+1
      "的元素. 而且, 我们假定我们的宇宙是" (B "累积性的")
      ", 即第" $i "个宇宙的所有元素也都是第" (&+ $i $1)
      "个宇宙的元素, 例如若" (&: $A Universe_i) "则也有"
      (&: $A Universe_i+1) ". 这是方便的, 但也会导致元素不再具有唯一的类型"
      "这一令人不太愉快的后果, 在其他方面也会有点tricky, 但是"
      "在这里还不至于让我们烦心; 见本章末的注记.")
   (P "当我们说" $A "是一个类型时, 我们的意思是其居于某个宇宙"
      Universe_i "之中. 我们通常想要避免显式提及层级" $i
      ", 并假定层级可以按照一致的方式被赋予; 因此, 我们或许可以记"
      (&: $A Universe) "而省略层级. 以这种方式, 我们甚至可以写下"
      (&: Universe Universe) ", 其应该读作"
      (&: Universe_i Universe_i+1) ", 它有意地将层级留作隐式. "
      "这种写下宇宙的风格常被称为" (B "典型的歧义 (typical ambiguity)")
      ". 它很方便, 但是也有点危险, 因为其允许我们写下表面上看起来合理的证明, "
      "但是实际上却重演了自指的悖论. 如果对于某个论证的正确性保有任何的疑问, "
      "那么验证这个证明的方法在于尝试为其中出现的每个宇宙一致地赋予层级. "
      "当假定了某个宇宙" Universe "时, 我们可以称属于" Universe
      "的类型为" (B "小类型 (small types)") ".")
   (P "为了模拟在给定类型" $A "上变动的一个类型合集, 我们使用函数"
      (&: $B (&-> $A Universe)) ", 其陪域是一个宇宙. 这些函数被称为"
      (B "类型族") " (或者有时也被称为" (Em "依赖类型")
      "); 它们对应于集合论中使用的集合族.")
   (P "类型族的一个例子是有限集合的族" (&: $Fin (&-> $NN Universe))
      ", 其中" (&Fin $n) "是一个恰具有" $n "个元素的类型. (现在我们还不能"
      (Em "定义") "族" $Fin ", 因为我们还没有引入定义域" $NN
      ", 但是很快就可以了; 见练习1.9.) 我们可以将" (&Fin $n)
      "的元素记为" (&cm $0_n $1_n $..h (_ (@- $n $1) $n))
      ", 这里的下标是为了强调在" $n "不同于" $m "时, "
      (&Fin $n) "和" (&Fin $m) "的元素是不同的, 并且它们也都不同于"
      "一般的自然数 (我们将在第" (Cite "section_nat") "节引入).")
   (P "一种更平凡(但是也非常重要)的类型族的例子是在某个类型"
      (&: $B Universe) "处的" (B "常") "类型族, 其当然是常函数"
      (&: (@lam $x $A $B) (&-> $A Universe)) ".")
   (P "作为一个" (Em "non") "-example, 在我们的版本的类型论中, "
      "并没有类型族" (Q (lam $i $NN Universe_i))
      ". 诚然如此, 并没有足够大的宇宙作为其陪域. 而且, "
      "甚至我们也不会将宇宙" Universe_i "的下标" $i
      "和类型论的自然数" $NN "等同起来 (后者在第"
      (Cite "section_nat") "节引入).")
   (H3 "依赖函数类型 (" $Pi:normal "类型)")
   (P "在类型论中, 我们经常使用一种更加一般的函数类型的版本, 其可以称为"
      (B $Pi:normal "类型") "或者" (B "依赖函数类型")
      ". 我们使用名字" (Q $Pi:normal "类型")
      "的原因在于这种类型也可以被视为给定类型上的笛卡尔积.")
   (P "给定类型" (&: $A Universe) "和族" (&: $B (&-> $A Universe))
      ", 我们可以构造依赖函数的类型"
      (&: (Pi $x $A (app $B $x)) Universe)
      ". 这种类型的记号可以有诸多变体, 例如"
      (MB (&split:16
           (Pi $x $A (app $B $x) #:style "false")
           (Pi $x $A (app $B $x))
           (set-attr*
            (: $prod (&cm (@: $x $A) (app $B $x)))
            'displaystyle "false")) ".")
      "如果" $B "是一个常族, 那么依赖积类型就变成了通常的函数类型:"
      (MB (&≡ (Pi $x $A $B #:style "false") (@-> $A $B)) ".")
      "的确, 所有" $Pi:normal "类型的构造其实是"
      "通常函数类型上的相应构造的一种推广.")
   (P "我们可以通过隐式定义来引入依赖函数: 为了定义"
      (&: $f (Pi $x $A (app $B $x)))
      ", 其中" $f "是要被定义的函数的名字, "
      "我们需要一个表达式" (&: $Phi:normal (app $B $x))
      ", 其可能牵涉变量" (&: $x $A) ", 而我们记"
      (MB (&:≡ (app $f $x) $Phi:normal)
          ", 对于" (&: $x $A) ".")
      "或者, 我们也可以使用" (B $lambda "抽象")
      (MB (&: (lam $x $Phi:normal)
              (Pi $x $A (app $B $x))) ".")
      "就和非依赖的函数一样, 我们可以" (B "应用")
      "一个依赖函数" (&: $f (Pi $x $A (app $B $x)))
      "于一个参数" (&: $a $A) "以获得一个元素"
      (&: (app $f $a) (app $B $a))
      ". 相等性就和通常函数类型是一样的, 即我们有着以下计算规则, 给定"
      (&: $a $A) ", 我们有" (&≡ (app $f $a) (&prime $Phi:normal)) "和"
      (&≡ (app (@lam $x $Phi:normal) $a) (&prime $Phi:normal))
      ", 其中" (&prime $Phi:normal) "由将" $Phi:normal
      "中的所有" $x "的出现替换为" $a "得到 (避免变量捕获, 总是如此). "
      "类似地, 我们拥有唯一性原则, 对于任意的"
      (&: $f (Pi $x $A (app $B $x))) ", "
      (&≡ $f (@lam $x (app $f $x))) ".")
   (P "作为一个例子, 我们回忆一下第" (Cite "universes")
      "节里有一个类型族" (&: $Fin (&-> $NN Universe))
      ", 其值是标准的有限集合, 而(集合的)元素为"
      (&: (&cm $0_n $1_n $..h (_ (@- $n $1) $n)) (&Fin $n))
      ". 那么, 存在着一个函数" (&: $fmax (Pi $n $NN (&Fin (&+ $n $1))))
      ", 其返回每个非空有限类型的" (Q "最大") "元素, "
      (&:≡ (&fmax $n) (_ $n (&+ $n $1))) ". 就和" $Fin
      "本身的情况一样, 我们还不能定义" $fmax ", 但是很快就可以了; 见练习1.9.");@@@
   (P "另一类重要的函数类型 (我们现在就可以定义) 是于一个给定宇宙上"
      (B "多态 (polymorphic)") "的函数. 一个多态函数接受类型作为其参数之一, "
      "然后作用于该类型的元素 (或者是由该类型构造而来的其他类型的元素). "
      "一个例子是多态恒等函数" (&: $id (Pi $A Universe (&-> $A $A)))
      ", 我们通过" (&:≡ $id (lam $A Universe (lam $x $A $x)))
      "定义. (和" $lambda "抽象一样, " $Pi:normal "自动地管辖 (scope over) "
      "表达式的剩余部分, 除非另有截断; 因此"
      (&: $id (Pi $A Universe (&-> $A $A))) "的意思是"
      (&: $id (Pi $A Universe (@-> $A $A))) ". 这个约定尽管在数学中并不常见, "
      "但是在类型论中却是常见的.)")
   (P "有时我们将一个依赖函数的某些参数写成下标形式. 例如, 我们或许会等价地通过"
      (&:≡ (&id #:type $A $x) $x) "来定义多态恒等函数. 而且, "
      "如果某个参数可以通过上下文推导出来, 那么我们可能会整个省略该参数. "
      "例如, 如果" (&: $a $A) ", 那么" (&id $a) "就是没有歧义的, 因为"
      $id "必然意味着" (_ $id $A) "以使其能够应用于" $a ".")
   (P "另一个(不那么平凡的)多态函数的例子是" (Q "swap")
      "操作, 其交换一个(curry化了的)二参数函数的参数顺序:"
      (MB (&: $swap (Pi $A Universe
                        (Pi $B Universe
                            (Pi $C Universe
                                (&-> (@-> $A $B $C)
                                     (@-> $B $A $C)))))) ".")
      "我们可以通过"
      (MB (&:≡ (appl $swap $A $B $C $g)
               (lam $b (lam $a (app (app $g $a) $b)))))
      "定义" $swap ", 也可以等价地将类型参数写成下标形式:"
      (MB (&:≡ (appl (app (_cm $swap $A $B $C) $g) $b $a)
               (appl $g $a $b)) "."))
   (P "注意到正如我们之前对于通常函数所做的, 我们也可以使用currying"
      "来定义多参数的依赖函数 (例如" $swap
      "). 然而, 在依赖的情形之下, 第二个参数的定义域可以依赖于第一个参数, "
      "而陪域可以依赖于这两个参数. 也就是说, 给定" (&: $A Universe)
      "以及类型族" (&: $B (&-> $A Universe)) "和"
      (&: $C (&-> (Pi $x $A (app $B $x)) Universe))
      ", 我们可以构造具有两个参数的函数的类型"
      (Pi $x $A (Pi $y (app $B $x) (appl $C $x $y)))
      ". 在" $B "是常量且等于" $A "的情况下, 我们可以使用紧凑记号, 写下"
      (Pi (&cm $x $y) $A (appl $C $x $y))
      ". 例如, " $swap "的类型也可以写成是"
      (MB (&: $swap (Pi (&cm $A $B $C) Universe
                        (&-> (@-> $A $B $C)
                             (@-> $B $A $C)))) ".")
      "最后, 给定" (&: $f (Pi $x $A (Pi $y (app $B $x) (appl $C $x $y))))
      "以及参数" (&: $a $A) "和" (&: $b (app $B $a)) ", 我们有"
      (&: (app (app $f $a) $b) (appl $C $a $b))
      ". 就和之前一样, 我们将其写成"
      (&: (appl $f $a $b) (appl $C $a $b)) ".")
   (H3 "积类型" #:id "section_product_type")
   (P "给定类型" (&: (&cm $A $B) Universe) ", 我们可以引入类型"
      (&: (&c* $A $B) Universe) ", 其被称为它们的"
      (B "笛卡尔积 (cartesian product)")
      ". 我们也引入了一个零元积类型, 其被称为" (B "单位类型 (unit type)")
      ", " (&: UnitType Universe) ". 我们期望" (&c* $A $B)
      "的元素是序对" (&: (tu0 $a $b) (&c* $A $B)) ", 其中"
      (&: $a $A) "且" (&: $b $B) ", 而" UnitType
      "的唯一元素是某个特定的对象" (&: $★ UnitType)
      ". 然而, 和集合论不同的是, 类型论中的序对是一种原始概念, "
      "和类型论中的函数一样, 而不是说定义序对为什么特定的集合, "
      "并将它们收集起来以形成笛卡尔积.")
   ((Remark)
    
    )
   (H3 "依赖序对类型 (" $Sigma:normal "类型)")
   (H3 "余积类型" #:id "section_coproduct_types")
   (H3 "布尔的类型")
   (H3 "自然数" #:id "section_nat")
   (H3 "模式匹配和递归")
   (H3 "命题作为类型")
   (H3 "相等类型")
   (H3: "注记")
   (H3: "练习")
   (H2 "同伦类型论" #:id "chapter_hott")
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