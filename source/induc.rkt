#lang racket
(provide induc.html)
(require SMathML)
(define $& (Mo "&amp;"))
(define-infix*
  (&& $&))
(define $Th (Mi "Th"))
(define (&Th H)
  (app $Th H))
(define $Induc (Mi "I"))
(define (&Induc P)
  (app $Induc P))
(define-@lized-op*
  (@-> &->))
(define (format-num section index)
  (and index
       (format "~a.~a"
               (apply string-append
                      (add-between
                       (map number->string
                            (cdr (reverse section))) "."))
               index)))
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
  (Proposition "命题" "proposition")
  (Note "注意" "note"))
(define induc.html
  (TnTmPrelude
   #:title "归纳定义导论"
   #:css "styles.css"
   (H1. "归纳定义导论")
   (H2. "引论" #:auto? #f)
   (P "对于集合的归纳定义经常被非形式化地呈现为给出生成这个集合的元素的规则, "
      "然后添加一个对象在这个集合之中仅当其由规则生成的陈述. "
      "等价的描述是刻画这个集合为在规则之下封闭的最小集合.")
   (P "当然了, 归纳定义的基本例子是生成自然数的例子. "
      "但是, 长久以来其被证明是呈现形式语言的句法的有用设备. "
      
      )
   (H2. "什么是归纳定义?")
   (H3. "作为广义形式系统的归纳定义")
   (P "当逻辑学家描述语言的句法时, 归纳定义总是被不断地使用. "
      "例如, 一阶语言的项被定义为包含变量和常量, 并且在项的构造规则:"
      (Blockquote
       "如果" (&cm $t_1 $..h $t_n) "是项而" $f
       "是语言的" $n "元函数符号, 那么表达式"
       (appl $f $t_1 $..h $t_n) "也是一个项.")
      "之下封闭的最小的表达式的集合.")
   (P "类似地, 一阶语言的公式被定义为包含原子公式的在逻辑符号"
      (&cm $neg $disj $conj $-> (: $exists $x) (: $forall $x))
      "的诸构成规则下封闭的最小表达式集合.")
   (P "对于我们的目的而言, 要考虑的最有用的例子是"
      "对于形式系统的定理的类的定义. "
      "考虑一阶逻辑的Hilbert风格系统" $H:bold
      ", " $H:bold "的定理集" (&Th $H:bold)
      "因而是基于" $H:bold "的" (Q "证明")
      "概念定义的. 但是, " (&Th $H:bold)
      "也可以被刻画为包含公理且在推理规则之下封闭的"
      "最小公式集合. 每个推理规则的实例都具有以下形式:"
      (Blockquote
       "根据" $X "中的前提" $theta ", 推出结论" $psi ".")
      "在modus ponens的情况下, " $X
      "由两个前提构成, 即" $phi "和" (@-> $phi $psi)
      ". 泛化规则的实例只有一个前提. "
      "将axiom scheme当作推理规则的特殊形式是方便的, "
      "其中每个实例的前提集都是空集. "
      "使用这个约定, 形式系统" $H:bold
      "确定了一个序对" (tu0 $X $psi)
      "的集合" (_ $Phi $H:bold)
      ", 其中每个元素都是" $H:bold
      "的推理规则的实例. {译注: 也就是说, "
      $X "是前提集, " $psi "是结论.} "
      "然后, " (&Th $H:bold)
      "不过就是在" (∈ (tu0 $X $psi) (_ $Phi $H:bold))
      "下封闭的最小集合.")
   (P "推广之后我们得到了以下定义.")
   ((Definition)
    (Ol #:attr* '((type "i"))
        (Li "一个" (Em "规则(rule)") "是一个序对"
            (tu0 $X $x) ", 其中" $X "是一个集合, 其被称为"
            (Em "前提") "集, 而" $x "是" (Em "结论")
            ". 通常我们将规则记为" (&-> $X $x) ".")
        (Li "如果" $Phi "是一个规则的集合 (以下也称为"
            (Em "规则集(rule set)")
            "), 那么一个集合是" (Em $Phi "封闭")
            "的, 如果对于每个" $Phi
            "中的规则, 若其前提在" $A "之中, 那么结论也在"
            $A "之中. 我们记" (&: $Phi (&-> $X $x))
            "来表示规则" (&-> $X $x) "在" $Phi
            "之中. 于是, " $A "是" $Phi "封闭的, 如果"
            (&& (&: $Phi (&-> $X $x)) (&sube $X $A))
            "可以推出" (∈ $x $A) ".")
        (Li "如果" $Phi "是一个规则集, 那么"
            (&Induc $Phi) ", 即"
            (Em "由" $Phi "归纳定义的集合")
            ", 其定义为"
            (&= (&Induc $Phi)
                (Cap (setI $A (: $A "是" $Phi "封闭的")))) ".")))
   ((Note #:auto? #f)
    $Phi "封闭集合的确存在, 只需要取" $Phi
    "中的每个规则的结论构成的集合即可. "
    "另外, 由" $Phi "封闭集合构成的任意族的交也是"
    $Phi "封闭的. 特别地, " (&Induc $Phi)
    "是" $Phi "封闭的, 因而" (&Induc $Phi)
    "是最小的" $Phi "封闭集合.")
   (P "回到我们之前的例子上来, 我们看到"
      (&= (&Th $H:bold) (&Induc (_ $Phi $H:bold)))
      ". 类似地, "
      )
   ((Example)
    
    )
   ((Definition)
    
    )
   ((Proposition)
    
    )
   (H3. "一个关系的良基部分")
   ((Proposition)
    
    )
   (H3. "作为算子的归纳定义")
   (H3. "单调归纳的" (Q "证明") "的概念")
   (H3. "内核&mdash;&mdash;归纳定义的对偶")
   (H3. "经典数学中的一些归纳的例子")
   (H2. "递归论中的归纳")
   (H3. "递归可枚举关系")
   (H3. (_^ $Pi:normal $1 $1) "关系")
   (H3. "可表示性")
   (H2. "归纳定义的类")
   (H3. "一般框架")
   
   ))