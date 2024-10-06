#lang racket
(provide hott.html)
(require SMathML)
(define (Cite #:attr* [attr* '()] id)
  `(cite ,attr* ,(Ref id)))
(define (&label x . t*)
  (Table #:attr* '((class "label") (align "center"))
         (Tr (Td x))
         (Tr (keyword-apply
              Td '(#:attr*) '(((align "center")))
              t*))))
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
(define-infix*
  (&disj $disj)
  (&conj $conj)
  (&=> $=>)
  
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
      (Em "类型") ". 命题 (可以对其进行证明, 证伪, 假设, 否定等行为的陈述) "
      "被等同于特定的类型, 通过表格" (Cite "correspondence")
      "所展示的对应关系. 因此, " (Em "证明一个定理") "这样的数学活动被等同于"
      (Em "构造一个对象") "这样的数学活动的一种特殊情况, "
      "这个对象即代表一个命题的类型的一个居民.")
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
   (P "从另一方面来说, 如果"
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