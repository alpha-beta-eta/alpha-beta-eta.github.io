#lang racket
(provide nlab.html)
(require SMathML)
(define (format-num section index)
  (cond ((and section index)
         (format "~a.~a"
                 (apply string-append
                        (add-between
                         (map number->string
                              (cdr (reverse section))) "."))
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
  (Corollary "推论" "corollary")
  (Lemma "引理" "lemma")
  (Remark "评注" "remark")
  (Exercise "练习" "exercise"))
(define &forall
  (case-lambda
    ((x t p)
     (&cm (: $forall (@: x t)) p))
    ((q p)
     (&cm (: $forall q) p))))
(define $pr (Mo "&pr;"))
(define %pr (Mi "&pr;"))
(define $lArr (Mo "&lArr;"))
(define-infix*
  (&lArr $lArr)
  (&=> $=>)
  (&conj $conj)
  (&disj $disj)
  (&pr $pr))
(define-@lized-op*
  (@: &:)
  (@disj &disj)
  (@forall &forall))
(define nlab.html
  (TnTmPrelude
   #:title "nlab翻译计划"
   #:css "styles.css"
   (H1. "nlab翻译计划")
   (P "就看到什么翻什么吧.")
   (P "注意, " (Q "either ... or ...")
      "在数学的上下文里通常含义等同于可兼或, "
      "这与日常不同. 因此, 我将其翻译为"
      (Q "要么..., 要么...")
      "时读者不应产生误解.")
   (H2. "良基关系 (well-founded relation)")
   (H3. "想法")
   (P "一个集合" $S "上的一个二元关系" $pr
      "被称为是良基的, 如果我们可以在" $S
      "上对于" $pr "施行归纳.")
   (H3. "定义")
   (P "令" $S "是一个集合, 而" $pr
      "是" $S "上的一个二元关系. "
      $S "的一个子集" $A "是"
      $pr "-归纳的, 如果"
      (MB (&forall
           $x $S
           (&=> (@forall
                 $t $S
                 (&=> (&pr $t $x)
                      (∈ $t $A)))
                (∈ $x $A))) ".")
      "关系" $pr "是良基的, 如果" $S "仅有的"
      $pr "-归纳子集就是" $S "本身.")
   (P "注意到这恰是澄清" $pr "上的归纳所必要的: "
      "如果我们可以表明一个陈述关于" $S
      "的一个元素" $x "是真的每当其关于所有先于 "
      "(precede, " $pr ") " $x "的元素是真的, "
      "那么其必然关于" $S "中的所有元素都是真的. "
      "若排中律在场, 那么其等价于其他常见的定义; "
      "见以下经典逻辑中的刻画.")
   (H4 "经典逻辑中的刻画")
   (P "尽管上述定义遵循了一个良基关系的一般用法 "
      "(即为了利用归纳证明" $S "的元素所具有的性质), "
      "其是复杂的. 以下我们给出两种替代性的刻画:"
      (Ol (Li "关系" $pr "没法无穷递降"
              " (通常归功于Pierre de Fermat), 如果"
              $S "中不存在序列"
              (&pr $..c $x_2 $x_1 $x_0)
              ". (这样的一个序列被称为一个无穷递降序列.)")
          (Li "关系" $pr "是经典良基的, "
              "如果" $S "的每个寓居子集" $A
              "都有一个成员" (∈ $x $A)
              "满足不存在" (∈ $t $A)
              "使得" (&pr $t $x)
              ". (这样的一个" $x
              "被称为" $A "的一个极小元素.)"))
      "在经典数学中, 这两个条件都等价于良基的条件. "
      "在构造性数学中, 我们可以证明一个良基关系没法无穷递降, "
      "但是反过来不行. 另外, 可以证明一个经典良基关系是良基的, "
      "但是反过来也不行.")
   (P "良基的经典概念将经典逻辑强加于我们, 意义如下.")
   ((Proposition #:id "classical-well-foundedness-implies-lem")
    "如果" (tu0 $X %pr) "是一个经典意义下的寓居良基关系, "
    "那么不加限制的排中律成立.")
   ((proof)
    "设存在" $x "和" $y "满足" (&pr $y $x)
    ", 令" $Q "是一个任意的命题. 考虑一个集合"
    (&sub $P $X) ", 其被定义为"
    (&= $P (&union (setE $x)
                   (setI $a (&conj (&pr $a $x) $Q))))
    ". 显然, 集合" $P "是寓居的, "
    "因而根据经典良基性其有一个极小元素"
    $x_0 ". 根据直觉主义推理, 要么" $x_0
    "在" (setE $x) "之中, 即" (&= $x_0 $x)
    ", 要么" (∈ $x_0 (setI $a (&conj (&pr $a $x) $Q)))
    ", 即" (&conj (&pr $x_0 $x) $Q)
    ". 在后一种情形下, 我们立即看出" $Q
    "成立. 那么, 设" (&= $x_0 $x) "是" $P
    "的极小元素; 我们将要证明" (&neg $Q)
    "成立. 这是因为假设" $Q "成立; 那么"
    (∈ $y $P) "且" (: $y $pr $x $= $x_0)
    ", 其违反了" $x_0 "是" $P "的一个极小元素的条件."
    (P "既然" $Q "是一个任意的命题, 我们可以推出"
       (&forall $Q (@disj $Q (&neg $Q))) "."))
   ((Remark)
    "我们注意到经典良基性对于构造性 (即直觉主义) "
    "数学而言实在是太强了, 根据"
    (Ref "classical-well-foundedness-implies-lem")
    ". 从另一方面来说, 无穷递降条件又太弱了, "
    "以至于在构造性数学中没什么用处. "
    "良基的归纳概念是恰好合适的.")
   (P "然而, 注意到在谓词性数学中, "
      "良基的定义甚至无法陈述, "
      "那么其他两个概念就是更可取的了, "
      "只要使用经典逻辑.")
   (H2. "排中 (excluded middle)")
   (H3. "想法")
   (P "在逻辑学中, 排中原理陈述了每个真值 (truth value) "
      "要么为真要么为假 (Aristotle, MP1011b24). "
      "(这有时被称为排中" (Q "公理") "或者"
      (Q "律") ", 要么是为了强调其是可选的, "
      "要么是为了强调其并非可选; " (Q "原理")
      "是相对中性的术语.) 经典逻辑的诸多含义之一"
      "是为了强调这条原理的成立; "
      "与之相对的是, 它在直觉主义逻辑中并不成立.")
   (P "排中原理 (从这里之后以PEM指代), "
      "作为关于真值本身的陈述, "
      "几乎为所有数学家接受 (经典数学); "
      "那些怀疑或者否定它的人是显然的少数, "
      "即构造主义者. 然而, "
      "当一个人试图在除了集合范畴之外的范畴之中内化数学时, "
      "无疑排中经常从内部 (internally) 失败. "
      "见列举于内部逻辑条目下的例子. "
      "(那些其中排中成立的范畴被称为布尔的; "
      "一般而言, 形容词" (Q "布尔的 (Boolean)")
      "经常被用来指明PEM的可用.)")
   (P "尽管术语" (Q "排中")
      " (有时甚至是" (Em "排三(excluded third)")
      ") 暗示了这条原理不能应用于多值逻辑之中, "
      "but that is not the point; "
      "多值逻辑" (Em "从外部") "是多值的, "
      "但是仍然可能" (Em "从内部") "是二值的. "
      "以范畴逻辑的语言来说, 一个范畴"
      "是否恰有两个亚终对象 (subterminal object) "
      "一般而言是独立于其是否是布尔的; "
      
      )
   (H3. "在集合论中")
   (H3. "在类型论中")
   (H3. "与选择公理的关系")
   (H3. "其他等价陈述")
   (H3. "双重否定PEM")
   (H2. "笛卡尔闭范畴 (cartesian closed category)")
   (H3. "定义")
   (H3. "例子")
   (H2. "Stone空间")
   (H3. "定义")
   (H3. "相关概念")
   (H2. "闭包算子 (closure operator)")
   (H2. "幂等 (idempotent)")
   (H3. "想法")
   (P "一个范畴之中的幂等态射的概念一般化了"
      "线性代数的上下文中的投影子 (projector) 的概念: "
      
      )
   (H2. "Galois连接 (Galois connection)")
   (H3. "想法")
   (H2. "双向类型检查 (bidirectional typechecking)")
   (H3. "想法")
   (P "双向类型检查既是一种呈现类型论的风格, "
      "也是一种实现类型论的算法. "
      "想法在于基本的判断" (&: $t $A)
      " (项" $t "属于类型" $A ") 被分为两种判断:"
      (Ul (Li (&lArr $t $A) ": 项" $t
              "被检查具有类型" $A ".")
          (Li (&=> $t $A) ": 根据项" $t
              "可以推导出或者合成出其具有类型" $A "."))
      "区别在于对于" (&lArr $t $A) "而言, " $t
      "和" $A "均被视为" (Q "给定") "或者说"
      (Q "输入") ", 而对于" (&=> $t $A)
      ", 我们仅将" $t "视为" (Q "输入")
      ", " $A "则是由" $t "经过" (Q "计算")
      "得到的" (Q "输出") ". 当类型论作为纯粹演绎系统呈现时, "
      (Q "输入/输出模式") "并不具有任何意义, 但当作为算法实现时, "
      "这意味着我们有了两个互递归的函数:"
      (Ul (Li (appl 'check $t $A) "简单返回成功或者失败.")
          (Li (app 'infer $t) "要么返回一个类型" $A
              ", 要么失败."))
      "见逻辑编程.")
   (H2. "Lawvere不动点定理")
   
   ))