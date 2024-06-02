#lang racket
(provide constructive_logic_notes.html)
(require SMathML)
(define (type-of var env)
  (cond ((assoc var env) => cdr)
        (else (error 'type-of "unknown variable ~s" var))))
(define (extend-env var type env)
  (cons (cons var type) env))
(define (reify t)
  (define (reify t)
    (match t
      ((-> ,t1 ,t2) (&impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (&conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (&disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (define (@reify t)
    (match t
      ((-> ,t1 ,t2) (@impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (@conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (@disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (reify t))
(struct result (t c) #:transparent)
(define (VAR u)
  (lambda (env)
    (define t (type-of u env))
    (result t (assume u (&true (reify t))))))
(define (GIVEN l t)
  (lambda (env)
    (result t (label l (&true (reify t))))))
(define (CONS a b)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match-define (result tb cb) (b env))
    (define type `(conj ,ta ,tb))
    (result type
            (&rull $conjI ca cb
                   (&true (reify type))))))
(define (CAR a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((conj ,t1 ,t2)
       (result t1 (&rull $conjE1 ca
                         (&true (reify t1))))))))
(define (CDR a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((conj ,t1 ,t2)
       (result t2 (&rull $conjE2 ca
                         (&true (reify t2))))))))
(define (LAM u t body)
  (lambda (env)
    (match-define (result tbody cbody)
      (body (extend-env u t env)))
    (define type `(-> ,t ,tbody))
    (result type
            (&rull (&implI u) cbody
                   (&true (reify type))))))
(define (APP a b)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match-define (result tb cb) (b env))
    (match ta
      ((-> ,t1 ,t2)
       (unless (equal? t1 tb)
         (error 'APP "type mismatch"))
       (result t2 (&rull $implE ca cb
                         (&true (reify t2))))))))
(define (INL tb a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (define type `(disj ,ta ,tb))
    (result type (&rull $disjI1 ca
                        (&true (reify type))))))
(define (INR ta b)
  (lambda (env)
    (match-define (result tb cb) (b env))
    (define type `(disj ,ta ,tb))
    (result type (&rull $disjI2 cb
                        (&true (reify type))))))
(define (CASE a u1 b1 u2 b2)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((disj ,t1 ,t2)
       (match-define (result tb1 cb1)
         (b1 (extend-env u1 t1 env)))
       (match-define (result tb2 cb2)
         (b2 (extend-env u2 t2 env)))
       (unless (equal? tb1 tb2)
         (error 'CASE "branch type mismatch"))
       (result tb1 (&rull (&disjE u1 u2) ca cb1 cb2
                          (&true (reify tb1))))))))
(define (ND proof)
  (result-c (proof '())))
(define $D:script^
  (&prime $D:script))
(define (assume u t)
  (&rull u t))
(define $ver (Mi "&uarr;"))
(define (&ver A) (: A $ver))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (let ((b (car b*)))
      (: (apply &cm a*) $vdash b))))
(define (Sequent . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (let ((b (car b*)))
      (: (apply &cm a*) $==> b))))
(define (label l d)
  (Mtable (Mtr (Mtd l)) (Mtr (Mtd d))))
(define (walk . x*)
  (&table (map list x*)))
(define (parallel . x*)
  (&table (list x*)))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define &split16 (&split 16))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define √2 (Msqrt $2))
(define √2^√2 (^ √2 √2))
(define $neg:id (Mi "&not;"))
(define (&negI u)
  (: $neg:id (^ $I u)))
(define $negE (: $neg:id $E))
(define $falsehood $bottom)
(define $falsehoodE (: $falsehood $E))
(define $truth $top)
(define $truthI (: $truth $I))
(define $disj:id (Mi "&or;"))
(define $conj:id (Mi "&and;"))
(define $impl:id (Mi "&sup;"))
(define $disjI1 (: $disj:id $I_1))
(define $disjI2 (: $disj:id $I_2))
(define $? (Mo "?"))
(define $disjE1? (: $disj:id $E_1 $?))
(define $conjI (: $conj:id $I))
(define $conjE1 (: $conj:id $E_1))
(define $conjE2 (: $conj:id $E_2))
(define $conjL (: $conj:id $L))
(define $conjR (: $conj:id $R))
(define $implI (: $impl:id $I))
(define (&implI u)
  (: $impl:id (^ $I u)))
(define $implE
  (: $impl:id $E))
(define (&disjE u w)
  (: $disj:id (^ $E (&cm u w))))
(define $disjE?
  (: $disj:id $E $?))
(define (make-sans str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define (make-italic str)
  (Mi str #:attr* '((mathvariant "italic"))))
(define (make-bold str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define $fst (make-bold "fst"))
(define (&fst p)
  (ap $fst p))
(define $snd (make-bold "snd"))
(define (&snd p)
  (ap $snd p))
(define $unit (make-bold "1"))
(define $name (make-italic "name"))
(define $true (make-italic "true"))
(define $false (make-italic "false"))
(define &split2 (&split 2))
(define (&true A)
  (&split2 A $true))
(define (&false A)
  (&split2 A $false))
(define $not_type (make-sans "not_type"))
(define (&not_type e t)
  (appl $not_type e t))
(define $type (make-sans "type"))
(define (&type e t)
  (appl $type e t))
(define $path (make-sans "path"))
(define (&path p u v)
  (appl $path p u v))
(define $lfloor (Mo "&lfloor;"))
(define $rfloor (Mo "&rfloor;"))
(define (&floor x)
  (: $lfloor x $rfloor))
(define $even (make-sans "even"))
(define $odd (make-sans "odd"))
(define (&even x) (app $even x))
(define (&odd x) (app $odd x))
(define $def= (^^ $= $Delta:normal))
(define (subst d x A)
  (: (bra0 (&/ d x)) A))
(define $impl $sup)
(define $==>R (_ $==> $R))
(define $==>E (_ $==> $E))
(define-infix*
  (&==>R $==>R)
  (&==>E $==>E)
  (&def= $def=)
  (&impl $impl)
  (&<-> $<->))
(define (Lam x M)
  (: $lambda x $. M))
(define (∀ x A)
  (: $forall x $. A))
(define (∃ x A)
  (: $exists x $. A))
(define-@lized-op*
  (@impl &impl)
  (@∃ ∃))
(define &impl*
  (case-lambda
    ((x y) (&impl x y))
    ((x y . z*) (&impl x (@ (apply &impl* y z*))))))
(define constructive_logic_notes.html
  (TnTmPrelude
   #:title "构造性逻辑讲义"
   #:css "styles.css"
   (H1. "构造性逻辑讲义")
   (P "这基本上是对于Frank Pfenning的构造性逻辑课程的讲义的翻译.")
   (H2. "何为(构造性)逻辑?")
   (H3. "引论")
   (P "根据维基百科, 逻辑学是研究有效推理和论证的学科. "
      "从这个定义的广度可以立即看出, "
      "逻辑学在哲学和数学领域构成了一个重要的研究方向. "
      "逻辑工具和方法在计算机硬件和软件的设计, "
      "描述和验证中也发挥着至关重要的作用. "
      "本课程将重点关注逻辑学在计算机科学中的这些应用. "
      "为了正确理解逻辑学及其与计算机科学的相关性, "
      "我们需要大量借鉴哲学和数学中更为古老的逻辑学传统. "
      "我们将在这些讲义中讨论逻辑学的一些相关历史, "
      "并提供进一步阅读的指引. "
      "在这次的介绍里, 我们只是给出了一个简要的引论. "
      "至于深入的一般性阅读材料, "
      "我们推荐Troelstra and van Dalen [1988]的引论部分, "
      "以及斯坦福哲学百科全书 (Stanford Encyclopedia of Philosophy) 中的"
      (A "直觉主义逻辑"
         #:attr* '((href "https://plato.stanford.edu/entries/logic-intuitionistic/")))
      "和"
      (A "数学哲学中的直觉主义"
         #:attr* '((href "https://plato.stanford.edu/entries/intuitionism/")))
      "词条.")
   (H3. "Russell悖论")
   (P "在19世纪末期和20世纪初期, "
      "数学处于一种可以称之为基础危机的状态, "
      "这场危机是由日益抽象的数学所引发的. "
      "逻辑悖论的发现加深了这场危机. "
      "Russell悖论 (发现于1901年) 尤其沉重打击了Frege试图"
      "发展一种可以用来进行所有数学推理的通用逻辑的努力 [Frege, 1879]. "
      "以现代语言, Russell悖论或许最容易通过集合来理解. "
      "Russell将" $R "定义为由所有不包含自身的集合构成的集合."
      (MB (&= $R (setI $x (&!in $x $x))))
      "那么, " $R "是" $R "的一个元素吗? "
      "如果是的话, 那么根据" $R "的定义必然有" (&!in $R $R)
      ". 如果不是的话, 那么又一次根据" $R "的定义必然有"
      (&in $R $R) ". 于是, 我们发现"
      (MB (&<-> (&in $R $R) (&!in $R $R)))
      "因而我们有了一个悖论性的命题, "
      "这个命题为真当且仅当其为假. "
      "Russell的解决方案是引入" (Em "类型")
      "以使得集合" $R "因为一个集合不能是其自身的元素"
      "而不能被构造出来. Whitehead和Russell的分歧类型论 "
      "(ramified type theory) Whitehead and Russell [1910–13]"
      ", 之后由Church [1940]演化成为他的" (Em "简单类型论")
      ", 解决了这个特定的问题. "
      "{译注: 从历史上来看, "
      "英年早逝的Frank Ramsey也对于简单类型论有重要的贡献.} "
      "其他的解决方案存在于集合论之中.")
   (H3. "直觉主义")
   (P "除了对于某个特定的系统中的某个问题进行提问, "
      "我们还可以进行更为一般的提问: "
      "当我们做数学的时候, 我们是在做什么呢? "
      "从柏拉图主义的角度来看, "
      "诸如整数, 有理数, 或者实数这样的数学对象具有客观的存在, "
      "而数学家只是试图理解这种真实性 (reality). "
      "Brouwer [1907], 作为直觉主义的创立者, "
      "转而将数学考虑为施行心智中的构造. "
      "由此数学更多与构造和知识有关, "
      "而非(一些难以捉摸的)绝对真性 (truth). "
      "从Brouwer的观念来看, 仅当我们知道"
      $A "为真或者" $B "为真的时候才能断言" (&disj $A $B)
      ". 特别地, 在一般情况下我们不能必然断言"
      (Em "排中律") (&disj $A (&neg $A))
      ", 因为我们可能既不知道" $A "的一个证明, "
      "也不知道" $A "的一个反驳. "
      "一个例子是哥德巴赫猜想 (让我们称其为" $G
      "), 即每个大于二的偶数都是两个素数之和. "
      "既然哥德巴赫猜想已经悬而未决将近三百年了, "
      "我们并不知道其一个证明, 也不知道其一个反例. "
      "因此, 我们无法言称" (&disj $G (&neg $G))
      "为真. 但是, 我们也不能排除之后或许我们就可以知道的可能性. "
      "{译注: 这句话让人想到Kripke语义.}")
   (P "我们将不会纠结于Brouwer关于直觉主义的特定形式的细节, "
      "但是我们可以从中提取出以下直觉主义的真性定义 "
      "(这个定义有些刻意地非形式化) "
      "[Troelstra and van Dalen, 1988]:"
      (Ul (Li "一个对于" (&conj $A $B)
              "的证明是通过呈现一个对于" $A
              "的证明和一个对于" $B
              "的证明而给出的.")
          (Li "一个对于" (&disj $A $B)
              "的证明是通过呈现一个对于" $A
              "的证明或一个对于" $B
              "的证明而给出的.")
          (Li "一个对于" (&impl $A $B)
              "的证明是一个构造, 其允许我们将一个对于"
              $A "的证明转换为一个对于" $B "的证明.")
          (Li "不存在对于" $falsehood
              "的证明. 另外, 一个对于" (&neg $A)
              "的证明是一个构造, 其将一个(假想的)对于"
              $A "的证明转换为一个矛盾.")
          (Li "一个对于" (∃ $x $A)
              "的证明是通过提供一个量化的论域里的见证者 (witness) "
              $d "和一个对于" (subst $d $x $A)
              "的证明给出的.")
          (Li "一个对于" (∀ $x $A)
              "的证明是一个构造, "
              "其取量化的论域里的任意一个元素"
              $d ", 然后给出对于" (subst $d $x $A)
              "的一个证明. {译注: 也就是说, "
              "对于论域的每个元素都需要给出证明. "
              "这其实和对于推出" (&-> $A $B) "的证明有些类似, "
              "在类型论中这将会更加明显.}"))
      "这个定义里提及" (Em "构造")
      "的地方, 我们可以将定义向计算机科学倾斜, 将其想成是"
      (Em "函数") ", 其必须是有效可计算的 "
      "(于是构造可以被实际施行). "
      "在这样一种解释下, 直觉主义证明对应于函数, "
      "而特定的直觉主义推理系统则会导致特定的编程语言.")
   (H3. "第一个例子")
   (P "我们称一个接受排中律的逻辑为" (Em "古典的(classical)")
      ". 作为古典证明和直觉主义证明的区别的例子, "
      "我们考虑下列定理和证明.")
   ((theorem #:n "1")
    "存在两个无理数" $a "和" $b "使得" $a^b "是有理数.")
   ((proof)
    "考虑" √2^√2 ", 存在两种情形." (Br)
    (B "情形: " √2^√2 "是有理数. ")
    "那么, " (&= $a $b √2) "就满足声明." (Br)
    (B "情形: " √2^√2 "是无理数. ")
    "那么, " (&= $a √2^√2) "和" (&= $b √2) "满足声明, 因为"
    (&= $a^b (^ (@ √2^√2) √2) (^ √2 $2) $2) ".")
   (P "在这个点, 古典数学家相当 (profoundly) 高兴, "
      "因为这是对于一个看起来非平凡的定理的"
      "一个极其简短而优雅的证明. "
      "直觉主义者相当不高兴, "
      "因为它并没有实际披露满足" $a^b
      "为有理数的无理数见证者" $a "和" $b
      ". 这样的见证者可能是" (&= $a $b √2)
      ", 也可能是" (&= $a √2^√2) "和" (&= $b √2)
      ". 因此, 直觉主义者拒斥这个证明.")
   (P "这里实际上并不正确的步骤是假定存在两种情形 "
      "(要么" √2^√2 "是有理数, 要么" √2^√2
      "不是有理数) 而不知道哪一种情形成立, "
      "这是(被直觉主义者拒斥的)排中律的一个实例.")
   (P "然而, 并无所失! 作为一个直觉主义者, "
      "我看了看上面的证明, 然后说"
      (Blockquote
       "啊, 我理解了你的证明, 但是它所证明的是另外一个不同的定理! "
       "实际上你证明的是:"
       ((theorem)
        "如果" √2^√2 "是有理数或者不是有理数, "
        "那么存在两个无理数" $a "和" $b
        "使得" $a^b "是有理数."))
      "令人惊讶的是, 只要我们坚持于纯粹逻辑, 或者或许是自然数的理论, "
      "任何古典证明都可以被重新解释为对于某个不同的定理的直觉主义证明! "
      "这暗示了, 一旦我们接受了直觉主义实际上可以被形式化的事实, "
      "那么直觉主义逻辑和古典逻辑就不再冲突了. "
      "转而, 直觉主义逻辑是古典逻辑的一种" (Em "泛化")
      ", 意即直觉主义逻辑拥有一个构造性的存在量化子和一个构造性的析取, "
      "而这在古典逻辑中是缺失的. 与此同时, "
      "所有的古典定理和证明都可以一致地在某种转换下导入直觉主义逻辑. "
      "{译注: 也就是所谓的双重否定转换 (double negation translation).}")
   (H3. "另一个悖论")
   (P "或许超越了直觉主义, 我们称某个逻辑是" (Em "构造性的")
      ", 如果其证明描述了有效的构造. 这里所强调的是" (Em "有效")
      ", 其是在说由证明所传达的构造可以实际被机械化地执行. "
      "换言之, 构造性证明描述了算法. "
      "第一眼看上去似乎所有描述了构造的证明都具有这种形式, "
      "而从历史来说很长一段时间的确如此. "
      "在19世纪的某个时间点似乎我们失去了这种数学和计算的直接联系, "
      "这导致了直觉主义和其他更为严格的数学推理形式的诞生. "
      "尽管如此, 时至今日绝大多数数学家仍然接受排中律, "
      "是计算机科学复兴了人们对于构造性逻辑和数学的兴趣.")
   (P "为了更好地理解这种区分, 我们考虑以下所谓的"
      (Em "Banach-Tarski悖论") ".")
   ((theorem #:n "2")
    "给定三维空间中的一个实心球, "
    "存在一种将球分解为有限数目的不相交子集的方法, "
    "然后其可以重新组装产生两个与原本的球等同的复制. "
    "而且, 重新组装的过程只牵涉移动和旋转碎片, "
    "不需要改变它们的形状. "
    "这种重构仅需要与五块碎片打交道, 但不能更少了.")
   (P "这被认为是悖论性的, 因为显然我们不能施行这种分解. "
      "中间的碎片实际上是不可测的点的无限散落. "
      "这种分解依赖于集合论中的选择公理, "
      "其是高度非构造性的.")
   (H3. "关于证明和程序之间的联系")
   (P "现在我们给出一个关于算术中的直觉主义证明和程序"
      "之间的联系的具体例子. 我们定义"
      (eqn*
       ((&even $x) $def= (∃ $y (&= $x (&i* $2 $y))))
       ((&odd $x)  $def= (∃ $y (&= $x (&+ (&i* $2 $y) $1))))))
   ((theorem #:n "3")
    (∀ $x (&disj (&even $x) (&odd $x))))
   ((proof)
    "根据" $x "上的归纳." (Br)
    (B "情形: " (&= $x $0) ". ")
    "那么" (&even $x) ", 鉴于对于" (&= $y $0)
    "我们有" (&= $x (&i* $2 $y)) "." (Br)
    (B "情形: " (&= $x (&+ $x^ $1)) ". ")
    "那么根据" $x^ "上的归纳假设有"
    (&disj (&even $x^) (&odd $x^))
    ". 我们分情形处理."
    (Ul (Li (B "子情形: " (&even $x^) ". ")
            "那么对于某个" $y^ "有"
            (&= $x^ (&i* $2 $y^)) ". 然后"
            (&= $x (&+ $x^ $1) (&+ (&i* $2 $y^) $1))
            ", 于是" (&odd $x) ".")
        (Li (B "子情形: " (&odd $x^) ". ")
            "那么对于某个" $y^ "有"
            (&= $x^ (&+ (&i* $2 $y^) $1)) ". 然后"
            (&= $x (&+ $x^ $1)
                (&+ (@+ (&i* $2 $y^) $1) $1)
                (&i* $2 (@+ $y^ $1)))
            ", 于是" (&even $x) ".")))
   (P "现在让我们从这个证明中提取出一个函数. "
      "根据真性的直觉主义解释, "
      "这应该是一个从自然数到该数字为奇为偶的指示的函数. "
      "我们可以使用布尔值, 不过为了清晰起见, "
      "我们为此定义了一个特定的类型. "
      "我们在ML中编写我们的函数, "
      "但是当前其可以转写为任意的函数式语言. "
      "鉴于ML内置的只有整数, "
      "让我们像Peano一样由零和后继函数定义自然数 (类型"
      (Code "nat") ")."
      (CodeB "datatype nat = zero | succ of nat
datatype eo = ev | od

(* even_or_odd : nat -> eo *)
fun even_or_odd x = case x of
    zero => ev
  | succ(x') => case even_or_odd x'
                  of ev => od
                   | od => ev")
      "我们看到一个诉诸于归纳假设对应于一个递归调用. "
      "情形区分转换为了" (Code "case") "构造.")
   (P "直觉主义者应该将归纳接纳为有效的推理原则吗? "
      "或者说, 另一个相关的问题是, 直觉主义者应该接纳"
      (Code "even_or_odd")
      "函数为一个有效的构造吗? 答案是肯定的, "
      "因为显然这个函数对于每个自然数都是良定的, 因为 "
      "(a) 其对于" $0 "提供了一个情形, 且 (b) "
      "其在递归调用中将参数从" (&+ $x^ $1)
      "归约为了" $x^ ". 这是自然数上的"
      (Em "原始递归") "的一般模式的一个例子. "
      "我们称一个函数" $f "由原始递归定义, "
      "如果其定义具有形式"
      (eqn*
       ((app $f $0) $= $c)
       ((app $f (&+ $x^ $1))
        $=
        (appl $g $x^ (app $f $x^))))
      "这意味着任何" $f "的递归调用都必然在" $x^
      "上. 一般而言, 由使用归纳的直觉主义证明所提取出来的函数"
      "都会具有原始递归的形式.")
   (P "回看原本的证明, 我们发现我们沿途丢失了一些信息. "
      "如果我们展开" $even "和" $odd
      "的定义, 我们会看到一个存在量化子. "
      "并且, 原本的证明给出了关于存在量化的见证者的信息. 在"
      $x^ "为偶数的情形 (见证者为" $y^
      "), 对于" (&+ $x^ $1) "为奇数这一事实的见证者是相同的"
      $y^ ". 在" $x^ "为奇数的情形 (见证者为" $y^
      "), 对于" (&+ $x^ $1) "为偶数这一事实的见证者是"
      (&+ $y^ $1) ". 为了将这样的信息容纳于函数之中, "
      "我们需要修改" (Code "eo") "数据类型来携带见证者."
      (CodeB "datatype nat = zero | succ of nat
datatype eo = ev of nat | od of nat")
      "然后这个信息可以按照原本证明所描述的方式携带进函数里."
      (CodeB "(* even_or_odd : nat -> eo *)
fun even_or_odd x = case x of
    zero => ev(zero)
  | succ(x') => case even_or_odd x'
                  of ev(y') => od(y')
                   | od(y') => ev(succ(y'))")
      "现在函数不仅实际会计算参数" $x
      "是奇是偶的指示, 还会返回"
      (&= $y (&floor (&/ $x $2))) ".")
   (P "我们想要强调一下, 我们并不是写了一个程序然后证明了其正确性 "
      "(虽然当然我们也可以这么做), 而是提供了一个构造性证明, "
      "然后从中提取出了一个程序. "
      "实际上也可以按照相反方向进行: "
      "给定一个程序, 我们或许可以从中读出其所表示的证明. "
      "这种证明和程序之间的联系还会在本课程的第一阶段"
      "占据相当长的时间.")
   (H3. "隐藏计算性内容")
   (P "通常 (尽管并不绝对必要, 这我们会在之后的作业里进行探索) "
      "我们通过推出和谬 (falsehood) 来定义否定:"
      (MB (&def= (&neg $A) (&impl $A $falsehood)))
      "现在从直觉主义来说, 一个对于" (&impl $A $falsehood)
      "的证明是一个函数, 其接受一个对于" $A
      "的证明, 返回一个对于" $falsehood
      "的证明. 然而, 既然我们有了对于" $A
      "的一个反驳, 并不存在对于" $A
      "的证明, 所以说这样的一个函数永远也不可能应用. "
      "非正式的结论是对于" (&neg $A)
      "的证明没有计算性内容. 这意味着我们可以通过将"
      $A "替换为" (&neg (&neg $A)) "来"
      (Q "隐藏") "计算性内容. 例如, "
      "如果我们并不想要见证者而只是想要知道某个数字"
      $x "是奇是偶, 我们或许可以描述:"
      (MB (∀ $x (&disj (&neg (&neg (&even $x)))
                       (&neg (&neg (&odd $x))))))
      "但是我们不得不非常小心, "
      "因为这可能从根本上改变证明的结构. "
      "特别地, 如果我们试图直接通过归纳来证明这个陈述, "
      "归纳假设立即就会变得弱得多.")
   (P "以下是两个类似的(非正式的)例子, "
      "这个课程之后我们还要回到这些例子上来."
      (MB (∀ (&cm $u $v)
             (&disj
              (@∃ $p (&path $p $u $v))
              (&neg (∃ $p (&path $p $u $v))))))
      "假如量化的论域得当, 并且图中路径" $p
      "连接的" $u "和" $v "时谓词"
      (&path $p $u $v)
      "为真, 那么这个命题可以充当对于图可达性的刻画. "
      "我们的意思是对于这个命题的一个" (Em "构造性证明")
      "必然包含一个算法, 其确定了给定的顶点" $u "和"
      $v "之间是否存在一条路径, "
      "并且若是存在则要返回某条路径. "
      "我们可以使用诸多方式证明这个命题, "
      "这将会导致诸多不同的算法 "
      "(通过路径长度上的归纳, "
      "通过未访问结点集合上的归纳, "
      "通过路径的边集上的归纳, 等等). "
      "如果我们不想要路径本身, "
      "只需要对于路径是否存在的指示, "
      "那么我们可以在第一个存在量化子"
      "之前加上一个双重否定.")
   (P "另一个角度是, " (Em "从古典逻辑来说")
      ", 这是一个全然平凡的陈述, 因为其有着"
      (∀ (&cm $u $v)
         (&disj (appl $A $u $v)
                (&neg (appl $A $u $v))))
      "的形式, 故其是排中律的一个实例.")
   (P "最后, 在编程语言领域我们可以将一个类型检查器描述为"
      (MB (∀ (&cm $e $tau)
             (&disj (&type $e $tau)
                    (&neg (&type $e $tau)))))
      "其中若表达式" $e "具有类型" $tau
      "则谓词" (&type $e $tau)
      "成立. 这个主题的一个变种是" (Em "类型推导") ":"
      (MB (∀ $e
             (&disj (@∃ $tau (&type $e $tau))
                    (&neg (∃ $tau (&type $e $tau))))))
      "从实现的角度来说, 这两个刻画都不那么尽如人意, "
      "因为当程序并非良类型时我们不能获得任何信息. "
      "为了解决这个问题, 我们可以定义另外一个谓词"
      (&not_type $e $tau) "使得其证明携带了所需的信息. "
      "这有点类似于我们的奇数/偶数例子的逆, "
      "因为我们本可以说"
      (∀ $x (&disj (&even $x) (&neg (&even $x))))
      ", 但是我们转而选择了使用" $odd "的一个显式定义.")
   (H3. "一些命题的地位 (status)")
   (P "讲座里有一些关于特定的命题是否拥有直觉主义逻辑中的证明的讨论. "
      "在下次讲座之后, 我们会有些工具来证明其中一些命题, "
      "但是直到数次讲座之后我们才能证明其中一些命题不是直觉主义可证的. "
      "以下是一些我所能回忆起来的命题:")
   (P "我们有" (&impl $A (&neg (&neg $A)))
      ": 如果" $A "为真, 那么" $A "一定不为假. "
      "从另一个方向来说, " (&neg (&neg $A))
      "在一般情况下不能推出" $A ", 因为在" (&neg (&neg $A))
      "的证明中我们或许没有足够的信息. 不过, 我们有"
      (&<-> (&neg $A) (&neg (&neg (&neg $A))))
      ", 对于更深的否定迭代也是类似的. "
      "从直觉上来说, 这是因为对于" (&neg $A)
      "的证明没有计算性内容.")
   (P "正如我们之前所言, 直觉主义者并不接受排中律"
      (&disj $A (&neg $A)) ", 但是我们可以证明其并不为假: "
      (&neg (&neg (@disj $A (&neg $A))))
      "是直觉主义为真的.")
   (P "排中律和非直接证明的规则有些联系, 后者也是直觉主义所拒斥的. "
      "非直接证明允许通过假定" (&neg $A) "然后推导出一个矛盾来证明"
      $A ". 但是, 我们要小心! 通过假定" $A "然后推导出一个矛盾来证明"
      (&neg $A) "本质上是直觉主义的. 从古典逻辑的角度来说, "
      "这两种形式的证明在某种意义上是不可区分的, "
      "但是从直觉主义来说它们是截然不同的 (一个有效, 另一个无效).")
   (P "这是一种常见的现象: 在进行构造性推理时, "
      "我们不得不对于留心于对于定理进行精确的表述, "
      "因为许多古典等价的命题并非直觉主义等价的.")
   (H3 "参考文献")
   
   (H2. "自然演绎")
   (H3. "引论")
   (P "这一章的目的在于建立逻辑学的两种概念, 即" (Em "命题")
      "和" (Em "证明") ". 关于这些概念的正确基础并无统一的意见. "
      "其中一种在计算机科学应用中取得巨大成功的方法, "
      "在于藉由命题的证明来理解命题的意义. "
      "用Martin-Löf [1983, 第27页]的话来说:"
      (Blockquote
       "命题的意义是由[...]什么算作对其的验证而确定的."))
   (P "一个" (Em "验证") "或许可以理解为一种特定的证明, "
      "其只会检视一个命题的组成部分. "
      "这在Dummett [1991]中得到了远为细致的分析, "
      "尽管其和计算机科学的联系更不那么直接了. "
      "这种观念所导致的推理规则系统是" (Em "自然演绎")
      ", 其由Gentzen [1935]提出而由"
      "Prawitz [1965]深入研究.")
   (P "本章我们将会应用Martin-Löf的方法来解释基本的命题联结词, "
      "这种方法遵循着丰富的哲学传统. "
      "之后我们将会看到, 全称和存在量化子与"
      "诸如自然数, 列表, 或者树这样的类型"
      "能够自然地融入相同的框架之中.")
   (P "我们将会藉由规则来定义命题逻辑的通常联结词 (合取, 推出, 析取) 的意义, "
      "这些规则允许我们推理它们何时应该为真, 即所谓的" (Em "引入规则")
      ". 根据这些规则, 我们可以推导出使用这些命题的规则, 即所谓的"
      (Em "消去规则") ". 我们所得到的系统是" (Em "自然演绎")
      ", 其是直觉主义逻辑的基础, 与函数式编程和逻辑编程有着直接的联系.")
   (H3. "判断和命题")
   (P "逻辑的Martin-Löf基础的基石是对于判断和命题的清晰区分. 一个"
      (Em "判断") "是某种我们或许知道的东西, 也就是某种知识对象. "
      "如果我们实际知道某个判断, 那么这个判断是"
      (Em "显然的(evident)") ".")
   (P "我们之所以作出诸如" (Q "天正在下雨")
      "的判断, 是因为我们有其证据. "
      "在日常生活中, 这样的证据往往是直接的: "
      "我们可以向窗外看看, 发现的确正在下雨. "
      "在逻辑学中, 我们关心的是证据间接的情况: "
      "我们根据其他显然的判断通过作出正确推理"
      "来推演出这判断.")
   (P "逻辑学中最重要的判断形式是" (Q $A "为真")
      ", 其中" $A "是一个命题. "
      "当然也有其他许多得到仔细研究的判断形式, 例如"
      (Q $A "为假") ", " (Q $A "于时刻" $t "为真")
      " (来源于时态逻辑), " (Q $A "必然为真")
      " (来源于模态逻辑), " (Q "程序" $M "具有类型" $tau)
      " (来源于编程语言), 等等.")
   (P "回到(前一段的)最后一个判断上来, "
      "让我们试着解释合取的含义. 对于判断"
      (Q $A "为真") " (预设" $A "为一个命题) 我们记"
      (&true $A) ". 对于命题" $A "和" $B
      ", 我们可以构成复合命题" (Q $A "且" $B)
      ", 更形式化地记作" (&conj $A $B)
      ". 但是, 我们尚未刻画合取的" (Em "含义")
      ", 也就是什么算作对于" (&conj $A $B)
      "的一个验证. 这是由以下的推理规则所完成的:"
      (MB (&rull
           $conjI
           (&true $A) (&true $B)
           (&true (&conj $A $B))))
      "这里的名字" $conjI "代表"
      (Q "合取引入 (conjunction introduction)")
      ", 鉴于合取的确在结论里被引入了.")
   (P "这条规则规则允许我们在已经知道" (&true $A)
      "和" (&true $B) "的情况下断言"
      (&true (&conj $A $B))
      ". 在这条推理规则之中, " $A "和" $B
      "是" (Em "模式变量(schematic variable)")
      ", 而" $conjI "是规则的名字.")
   (P "从直觉上来说, " $conjI "规则是说对于"
      (&true (&conj $A $B))
      "的一个证明由对于" (&true $A)
      "的一个证明连带着对于" (&true $B)
      "的一个证明构成. 这恰是我们在将第一次讲座里"
      "所提及的合取的直觉主义意义.")
   (P "推理规则的一般形式为"
      (MB (&rull
           $name
           $J_1 $..h $J_n
           $J))
      "其中判断" (&cm $J_1 $..h $J_n)
      "被称为" (Em "前提(premise)")
      ", 判断" $J "被称为"
      (Em "结论(conclusion)")
      ". 一般而言, 我们将会使用字母" $J
      "代表判断, 而" (&cm $A $B $C)
      "留给命题使用.")
   (P "我们将合取引入视为完整地描述了" (&conj $A $B)
      "的意义. 那么, 如果我们知道" (&conj $A $B)
      "为真, 什么可以推演 (deduce) 呢? "
      "根据以上的规则, 拥有对于" (&conj $A $B)
      "的一个验证意味着拥有对于" $A "和" $B
      "的验证. 因此, 以下两条规则得到了澄清:"
      (MB (&split16
           (&rull
            $conjE1
            (&true (&conj $A $B))
            (&true $A))
           (&rull
            $conjE2
            (&true (&conj $A $B))
            (&true $B))))
      "名字" $conjE1 "代表"
      (Q "第一合取消去 (first conjunction elimination)")
      ", 因为前提中的合取在结论中已被消去. "
      "类似地, " $conjE2 "代表" (Q "第二合取消去") ".")
   (P "之后我们将会看到为了保证一个联结词的构成, 引入, 消去"
      "能够正确适配到一起所必要的精确条件. "
      "暂时我们将只能非形式化地论述消去规则的正确性, "
      "如我们刚才对于合取消去规则所做的.")
   (P "作为第二个例子, 我们考虑记作" $truth "的命题"
      (Q "真 (truth)") ". 命题真应该永远为真, "
      "这意味着其引入规则没有前提."
      (MB (&rull
           $truthI
           (&true $truth)))
      "因此, 当我们知道" (&true $truth)
      "时, 我们并不拥有什么信息, "
      "所以这里没有消去规则.")
   (P "两个命题的合取是由带有两个前提的引入规则和"
      "两条相对应的消去规则所刻画的. "
      "我们可以将(命题)真想成是零个命题的合取. "
      "根据类比, 它应该具有一条带有零个前提的引入规则, "
      "以及零条消去规则. 这恰是我们上面所写下的内容.")
   (H3. "假言判断")
   (P "考虑以下演绎, 对于任意的命题" (&cm $A $B $C) ":"
      (MB (&rull
           $conjE1
           (&rull
            $conjE2
            (&true (&conj $A (@conj $B $C)))
            (&true (&conj $B $C)))
           (&true $B)))
      "这里我们实际证明了任何东西吗? "
      "乍看上去似乎不是这么回事: "
      $B "是一个任意的命题; "
      "显然我们不应该能够证明其为真. "
      "再仔细检视一遍我们发现所有的推理都是正确的, "
      "但是第一个判断" (&true (&conj $A (@conj $B $C)))
      "没有得到澄清. 我们可以从这个演绎中提取出如下知识:"
      (Blockquote
       "根据" (&conj $A (@conj $B $C)) "为真的假设, "
       "我们可以推演出" $B "必然为真."))
   (P "这是" (Em "假言判断(hypothetical judgment)")
      "的一个例子, 而以上的图形是一个"
      (Em "假言演绎(hypothetical deduction)")
      ". 一般而言, 我们可以拥有多于一个假设, "
      "所以一个假言演绎具有形式"
      (MB (walk (parallel $J_1 $..c $J_n)
                $..v $J))
      "其中判断" (&cm $J_1 $..h $J_n)
      "是未经证明的判断, 而判断" $J
      "是结论. 推理规则的所有实例也都是假言判断, "
      "即便在推理规则没有前提的情况下可能只有"
      $0 "个假设. {译注: 译者的一个疑问是, "
      "推理规则本身算作假言判断吗? "
      "尽管推理规则总是模式性的, 或者说含有元变量, "
      "但是实际上推理规则的实例也总是含有元变量. "
      "译者认为显然应该是算的.}")
   (P "推理的许多错误都是由于忽略了对某些隐藏假设的依赖而导致的. "
      "当我们需要显式化时, 我们会将其上由假言演绎所建立的假言判断写成"
      (!- $J_1 $..h $J_n $J) ". 我们可以将" (&cm $J_1 $..h $J_n)
      "称为假言判断的前件, 而" $J "称为假言判断的后件. 例如, 假言判断"
      (!- (&true (&conj $A (@conj $B $C))) (&true $B))
      "是由之前的假言演绎所证明的, 即" (&true $B)
      "的确可由假设" (&true (&conj $A (@conj $B $C)))
      "通过推理规则推出.")
   (P "假言判断的关键性质, 或者说"
      (Em "定义性质(defining property)")
      ", 是我们总是可以将对于某个假设" (&true $A)
      "的所有使用替换为对于" $A "的一个证明. "
      "形式化证明替换这个概念并不全然是直接的, "
      "但是它应该是相当直觉性的, "
      "以至于现在我们将其作为一种原始概念. "
      "{译注: 这里所称证明是元层次上的, "
      "Martin-Löf称之为demonstration.}")
   (P "根据假言判断, 我们现在可以解释推出 (implication) "
      (Q $A "推出" $B) "或者" (Q "如果" $A "那么" $B)
      " (更形式化地, " (&impl $A $B)
      ") 的意义了. 其引入规则读作: " (&impl $A $B)
      "为真, 如果在" $A "为真的假设下" $B "为真."
      (MB (&rull
           (&implI $u)
           (walk
            (&rull $u (&true $A))
            $..v
            (&true $B))
           (&true (&impl $A $B))))
      "这条规则的微妙之处在于标签" $u "和那条横杠. "
      "如果我们省略这个注解 (annotation), "
      "那么这个规则将会变成"
      (MB (&rull
           $implI
           (walk (&true $A) $..v (&true $B))
           (&true (&impl $A $B))))
      "这是不正确的: 这看起来像是根据"
      (&true $A) "的假设对于" (&true (&impl $A $B))
      "的推导. 但是, 假设" (&true $A) "是在证明"
      (&true (&impl $A $B)) "的过程之中引入的, "
      "结论不应该依赖于它! {译注: 也就是不依赖于"
      (&true $A) "这个假设.}")
   (P "推出" (&impl $A $B) "是否为真当然是独立于"
      $A "自身是否实际为真的问题的. 因此, "
      "我们以一个新的名字" $u "标记了假设的使用, "
      "并且对于将这个假设引入推导的推理, "
      "我们标记以相同的名字" $u ".")
   (P "我们应该注意到证明中以" $u
      "标记的假设只在推理" (&implI $u) "的"
      (Em "上方") "可用. 我们很快就会发现, "
      "如果我们允许违反标签" $u
      "的作用域, 那么自然演绎会变成" (Em "不一致的")
      ". 另外, 一个证明中的标签应该是不同的, "
      "这是为了我们能够毫无歧义地指出一个假设引入的位置. "
      "{译注: 实际上译者认为其实标签是可以重复的, "
      "但是终归标签的目的是为了无误指明哪些假设要解除 (discharge).}")
   (P "在我们施行一些示例证明之前, "
      "我们考虑一下推出的消去规则应该要说什么. "
      "根据其唯一的引入规则, 拥有对于" (&true (&impl $A $B))
      "的一个证明意味着我们根据" (&true $A)
      "有了一个对于" (&true $B) "的假言性证明. "
      "通过替换原理, 如果我们有了对于" (&true $A)
      "的一个证明, 那么我们就得到了对于" (&true $B)
      "的一个证明."
      (MB (&rull
           $implE
           (&true (&impl $A $B))
           (&true $A)
           (&true $B)))
      "这就完成了和推出相关的规则. "
      "{译注: 这里所谓的替换原理本质上来说"
      "就是第一次讲座的对于推出的直觉主义解释, "
      "虽然假言判断是外部性的, 推出是内部性的.} "
      "{译注: 这里我不是想进行什么循环论证, "
      "我只是想说它们的确是一致的.}")
   (H3. "构造证明")
   (P "在移至其他联结词之前, "
      "让我们构造一些(直觉主义)自然演绎中的示例证明. "
      "和通常数学一样, 往往是证明构造的过程"
      "而非最终产物提供了更多的洞察. "
      "尽管在纸面上很难呈现这种传达直觉的随时间变化方面, "
      "但我们会尽力为之.")
   (P "我们从对于推出的最简单使用开始, 即" (&true (&impl $A $A))
      ". 在呈现里, 我们用竖着的点来指出我们正在试图填充一个缺失的证明. "
      "假设 (若是存在的话) 则在更之上出现."
      (MB (walk $..v (&true (&impl $A $A))))
      "既然我们没有可用的假设, 我们从一个推出引入开始."
      (MB (&rull
           (&implI $u)
           (walk
            (&rull $u (&true $A))
            $..v
            (&true $A))
           (&true (&impl $A $A))))
      "在这个时间点我们看到我们唯一的假设和我们试图证明的东西是匹配的, "
      "所以说我们就将gap闭合了."
      (MB (&rull
           (&implI $u)
           (&rull
            $u
            (&true $A))
           (&true (&impl $A $A))))
      "合法性检查: " $u "只在" (&implI $u)
      "推理(线)的上方用到, 所以作用域是正确的.")
   (P "接着, 我们试着证明稍微复杂一些的东西, 即"
      (&true (&impl* $A $B $A))
      ". 从直觉上来说, 这之所以是真的是因为我们可以从第一个假设"
      $A "就可以得到结论" $A ", 而并不需要第二个假设" $B
      ". 以下是怎么将证明写出来的过程."
      (MB (walk $..v (&true (&impl* $A $B $A))))
      "我们又一次从推出引入开始."
      (MB (&rull
           (&implI $u)
           (walk
            (&rull $u (&true $A))
            $..v
            (&true (&impl $B $A)))
           (&true (&impl* $A $B $A))))
      "我们再一次使用推出引入, 以一个新的标签" $w "."
      (MB (&rull
           (&implI $u)
           (&rull
            (&implI $w)
            (walk
             (parallel
              (&rull $u (&true $A))
              (&rull $w (&true $B)))
             $..v
             (&true $A))
            (&true (&impl $B $A)))
           (&true (&impl* $A $B $A))))
      "这里我们有两个可用的假设, 但是我们只需要用第一个以"
      $u "标记的假设来闭合gap和完成证明."
      (MB (&rull
           (&implI $u)
           (&rull
            (&implI $w)
            (&rull $u (&true $A))
            (&true (&impl $B $A)))
           (&true (&impl* $A $B $A))))
      "这是对于我们并不一定需要使用假设这个事实的刻画, "
      "如果一个假设并不需要, 那么其就不用出现在最终证明里. "
      "{译注: " (&implI $w)
      "这一步是所谓的vacuous discharge, "
      "曾令译者相当困惑. 不过, 如果我们将其理解为"
      $lambda "绑定了变量" $w ", 似乎就容易接受了. "
      "discharge均可以理解为绑定, 只不过不一定是"
      $lambda "绑定, 还可能是其他的, "
      "例如后文的析取消去就相当于一种"
      "case distinction式的绑定.}")
   (P "然后, 让我们展示一个需要使用假设不止一次的例子, "
      "并且其也需要使用消去规则进行推理."
      (MB (walk $..v
                (&true
                 (&impl
                  (@conj $A $B)
                  (@conj $B $A)))))
      "和之前一样, 我们从推出引入开始."
      (MB (&rull
           (&implI $u)
           (walk
            (&rull $u (&true (&conj $A $B)))
            $..v
            (&true (&conj $B $A)))
           (&true
            (&impl
             (@conj $A $B)
             (@conj $B $A)))))
      "鉴于我们要证明一个合取, "
      "所以说我们可以将其归约为证明其两个合取分量 (conjunct)."
      (MB (&rull
           (&implI $u)
           (&rull
            $conjI
            (walk
             (&rull $u (&true (&conj $A $B)))
             $..v
             (&true $B))
            (walk
             (&rull $u (&true (&conj $A $B)))
             $..v
             (&true $A))
            (&true (&conj $B $A)))
           (&true
            (&impl
             (@conj $A $B)
             (@conj $B $A)))))
      "这里我们将假设" $u "写了两次, "
      "因为其在两个子证明中都是可用的. "
      "在讲座里我们使用了稍微不同的形式, "
      "其中我们有一块" (Q "草稿区域")
      "用于记录假设, 所以那时记录一次就够了.")
   (P "让我们来看第一个子目标, 即由" (&true (&conj $A $B))
      "证明" (&true $B) ". 这里我们无法使用引入规则来处理 "
      "(因为" $B "是任意的), 但是我们通过消去规则以利用"
      (&true (&conj $A $B)) "这个知识."
      (MB (&rull
           (&implI $u)
           (&rull
            $conjI
            (walk
             (&rull
              $conjE2
              (&rull $u (&true (&conj $A $B)))
              (&true $B))
             $..v
             (&true $B))
            (walk
             (&rull $u (&true (&conj $A $B)))
             $..v
             (&true $A))
            (&true (&conj $B $A)))
           (&true
            (&impl
             (@conj $A $B)
             (@conj $B $A)))))
      "现在我们可以闭合这gap, 因为我们有了一个对于"
      (&true $B) "的证明, 由消去规则所构造."
      (MB (&rull
           (&implI $u)
           (&rull
            $conjI
            (&rull
             $conjE2
             (&rull $u (&true (&conj $A $B)))
             (&true $B))
            (walk
             (&rull $u (&true (&conj $A $B)))
             $..v
             (&true $A))
            (&true (&conj $B $A)))
           (&true
            (&impl
             (@conj $A $B)
             (@conj $B $A)))))
      "对于" $conjI "的第二个前提的证明完全是对称的, "
      "所以说我们不必细致写下每一个单独步骤."
      (MB (&rull
           (&implI $u)
           (&rull
            $conjI
            (&rull
             $conjE2
             (&rull $u (&true (&conj $A $B)))
             (&true $B))
            (&rull
             $conjE1
             (&rull $u (&true (&conj $A $B)))
             (&true $A))
            (&true (&conj $B $A)))
           (&true
            (&impl
             (@conj $A $B)
             (@conj $B $A)))))
      "这完成了证明. 让我们确保"
      $u "的所有使用都在引入"
      $u "的规则 (" (&implI $u)
      ") 之上, 事实也确实如此. "
      "因此不存在作用域的违反, 证明是有效的.")
   (P "最后, 对于本节, 我们构造一个" (Q "反例")
      ", 它显示了作用域限制的必要性. "
      )
   (H3. "析取和谬")
   (P "到目前为止, 我们已经解释了合取, (命题)真, 推出的意义. 析取"
      (Q $A "或" $B) " (记作" (&disj $A $B)
      ") 更为复杂, 但并不需要新的判断形式. "
      "析取是由两条引入规则刻画的: " (&disj $A $B)
      "为真, 如果" $A "或者" $B "为真."
      (MB (&split16
           (&rull
            $disjI1
            (&true $A)
            (&true (&disj $A $B)))
           (&rull
            $disjI2
            (&true $B)
            (&true (&disj $A $B)))))
      "诸如以下的消去规则是不正确的"
      (MB (&rull
           $disjE1?
           (&true (&disj $A $B))
           (&true $A)))
      "因为即便我们知道" (&disj $A $B)
      "为真, 我们也不知道到底" $A "和"
      $B "哪一个析取分量 (disjunct) 为真. "
      "从具体方面来说, "
      "使用这一规则按照以下方式推导出任意的命题"
      $A "为真:"
      (MB (&rull
           $disjE1?
           (&rull
            $disjI2
            (&rull
             (&implI $u)
             (&rull $u (&true $B))
             (&true (&impl $B $B)))
            (&true (&disj $A (@impl $B $B))))
           (&true $A))))
   (P "因此, 我们需要换种不同的方法. 如果我们知道"
      (&disj $A $B) "为真, 我们必须要考虑两种情形: "
      (&true $A) "和" (&true $B)
      ". 如果我们可以在两种情形下都证明结论"
      (&true $C) ", 那么" $C "必然为真! "
      "我们将其写成一条推理规则:"
      (MB (&rull
           (&disjE $u $w)
           (&true (&disj $A $B))
           (walk (&rull $u (&true $A))
                 $..v
                 (&true $C))
           (walk (&rull $w (&true $B))
                 $..v
                 (&true $C))
           (&true $C)))
      "如果在" (&true (&disj $A $B)) "是因为" $A "为真和"
      (&true (&disj $A $B)) "是因为" $B
      "为真这两种情形下我们都能推出" (&true $C)
      ", 那么若我们知道了" (&true (&disj $A $B))
      "也就知道了" (&true $C)
      ". 注意到我们又使用了假言判断的机制. "
      "在第二个前提的证明里我们可以使用标记了"
      $u "的假设" (&true $A)
      ", 在第三个前提的证明里我们可以使用标记了"
      $w "的假设" (&true $B)
      ". 这两个假设都在析取消去时被解除了.")
   (P "让我们更为显式地澄清这条规则的结论. "
      "根据第一个前提我们知道" (&true (&disj $A $B))
      ". 其两个引入规则的前提分别是" (&true $A)
      "和" (&true $B) ". 在" (&true $A)
      "情形下我们根据替换原理和第二个前提作出了结论"
      (&true $C) ": 我们将假言推导中对于标记了"
      $u "的假设的所有使用替换为了对于" (&true $A)
      "的证明. " (&true $B) "的情形是对称的, "
      "使用的是第三个前提的假言推导.")
   (P "在讲座里我们原本所提出的是如下规则:"
      (MB (&rull
           $disjE?
           (&true (&disj $A $B))
           (&true (&impl $A $C))
           (&true (&impl $B $C))
           (&true $C)))
      "这与我们所选择的规则紧密关联, 并且实际上是"
      (Em "可推导的(derivable)")
      ", 因而是正确的. 但是它有着一个缺陷: "
      "在我们的验证主义纲领中, "
      "我们希望完全彼此独立地解释各个联结词的意义. "
      "也就是说, " (&disj $A $B)
      "的意义应当只依赖于" $A "和" $B
      "的意义, 而不依赖于任何其他联结词. "
      "上面这条规则不满足这一原则, "
      "因为它在前提中使用了推出 (implication). "
      "虽然这看起来像是一个吹毛求疵的小问题, "
      "但一旦我们开始研究证明构造的原理, "
      "允许这样一条规则作为消去规则将会带来严重的负面后果. "
      "幸运的是, 这并不要紧: 任何想使用" $disjE?
      "的人都可以放心地使用它, 因为它是可以被导出的.")
   (P "课上我们证明了"
      (MB (&true
           (&impl (@impl (@disj $A $B) $C)
                  (@conj (@impl $A $C)
                         (@impl $B $C)))))
      "我们逐步再走一遍."
      (MB (walk $..v
                (&true
                 (&impl (@impl (@disj $A $B) $C)
                        (@conj (@impl $A $C)
                               (@impl $B $C))))))
      "我们将最初的两个步骤合并, "
      "其是推出引入后面跟着合取引入."
      (MB (&rull (&implI $u)
                 (&rull $conjI
                        (walk
                         (assume $u (&true (&impl (@disj $A $B) $C)))
                         $..v
                         (&true (&impl $A $C)))
                        (walk
                         (assume $u (&true (&impl (@disj $A $B) $C)))
                         $..v
                         (&true (&impl $B $C)))
                        (&true (&conj (@impl $A $C)
                                      (@impl $B $C))))
                 (&true
                  (&impl (@impl (@disj $A $B) $C)
                         (@conj (@impl $A $C)
                                (@impl $B $C))))))
      "我们处理第一个未完成的子目标, 再一次应用推出引入."
      (MB (&rull (&implI $u)
                 (&rull $conjI
                        (&rull
                         (&implI $w)
                         (walk
                          (parallel
                           (assume $u (&true (&impl (@disj $A $B) $C)))
                           (assume $w (&true $A)))
                          $..v
                          (&true $C))
                         (&true (&impl $A $C)))
                        (walk
                         (assume $u (&true (&impl (@disj $A $B) $C)))
                         $..v
                         (&true (&impl $B $C)))
                        (&true (&conj (@impl $A $C)
                                      (@impl $B $C))))
                 (&true
                  (&impl (@impl (@disj $A $B) $C)
                         (@conj (@impl $A $C)
                                (@impl $B $C))))))
      "此时我们在第一个子目标中无法再用引入规则向上推进了, "
      "因为结论" (&true $C) "是一般性的. 但我们有一个标签为"
      $u "的假设, 它是一个推出, 所以可以尝试推出消去."
      (MB (&rull (&implI $u)
                 (&rull $conjI
                        (&rull
                         (&implI $w)
                         (&rull
                          $implE
                          (assume $u (&true (&impl (@disj $A $B) $C)))
                          (walk
                           (parallel
                            (assume $u (&true (&impl (@disj $A $B) $C)))
                            (assume $w (&true $A)))
                           $..v
                           (&true (&disj $A $B)))
                          (&true $C))
                         (&true (&impl $A $C)))
                        (walk
                         (assume $u (&true (&impl (@disj $A $B) $C)))
                         $..v
                         (&true (&impl $B $C)))
                        (&true (&conj (@impl $A $C)
                                      (@impl $B $C))))
                 (&true
                  (&impl (@impl (@disj $A $B) $C)
                         (@conj (@impl $A $C)
                                (@impl $B $C))))))
      "在剩下的第一个子目标中, 我们不再需要标签为" $u
      "的假设了, 因为假设" (&true $A) " (标签为" $w
      ") 已足以用两条析取引入规则之一来完成证明的这一部分."
      (MB (&rull (&implI $u)
                 (&rull $conjI
                        (&rull
                         (&implI $w)
                         (&rull
                          $implE
                          (assume $u (&true (&impl (@disj $A $B) $C)))
                          (&rull
                           $disjI1
                           (assume $w (&true $A))
                           (&true (&disj $A $B)))
                          (&true $C))
                         (&true (&impl $A $C)))
                        (walk
                         (assume $u (&true (&impl (@disj $A $B) $C)))
                         $..v
                         (&true (&impl $B $C)))
                        (&true (&conj (@impl $A $C)
                                      (@impl $B $C))))
                 (&true
                  (&impl (@impl (@disj $A $B) $C)
                         (@conj (@impl $A $C)
                                (@impl $B $C))))))
      "唯一剩下的子目标与我们已经证明过的那个是对称的, "
      "所以我们直接把它补上."
      (MB (&rull (&implI $u)
                 (&rull $conjI
                        (&rull
                         (&implI $w)
                         (&rull
                          $implE
                          (assume $u (&true (&impl (@disj $A $B) $C)))
                          (&rull
                           $disjI1
                           (assume $w (&true $A))
                           (&true (&disj $A $B)))
                          (&true $C))
                         (&true (&impl $A $C)))
                        (&rull
                         (&implI $x)
                         (&rull
                          $implE
                          (assume $u (&true (&impl (@disj $A $B) $C)))
                          (&rull
                           $disjI2
                           (assume $x (&true $B))
                           (&true (&disj $A $B)))
                          (&true $C))
                         (&true (&impl $B $C)))
                        (&true (&conj (@impl $A $C)
                                      (@impl $B $C))))
                 (&true
                  (&impl (@impl (@disj $A $B) $C)
                         (@conj (@impl $A $C)
                                (@impl $B $C))))))
      "我们应当把这个证明再检查一遍, "
      "以确保没有作用域方面的违反; 可以看到, 的确"
      (&cm $u $w $x) "都只出现在把它引入证明的那条推理之上.")
   ((tcomment)
    "这个证明约相当于"
    (CodeB "(λ (u)
  (cons (λ (w) (u (inl w)))
        (λ (x) (u (inr x)))))"))
   (P "关于析取的讨论到此结束. falsehood (写作"
      $falsehood ", 有时称为absurdity) "
      "是一个不应有任何证明的命题! 因此它没有引入规则.")
   (P "既然不可能存在" (&true $falsehood)
      "的证明, 那么如果我们知道" (&true $falsehood)
      ", 推出任意命题为真就是可靠的. "
      "这就澄清了以下的消去规则:"
      (MB (&rull
           $falsehoodE
           (&true $falsehood)
           (&true $C)))
      "我们也可以把falsehood看作是在零个选择项之间的析取. "
      "与二元析取类比, 我们因而有零条引入规则, "
      "以及一条需要考虑零种情形的消去规则. "
      "这恰好就是上面的" $falsehoodE "规则.")
   (P "由此看来, falsehood似乎毫无用处: "
      "我们永远无法证明它. 这是对的, "
      "只不过我们可能会从相互矛盾的假设出发进行推理! "
      "在讨论否定时我们会看到一些例子, "
      "因为我们可以把命题" (Q "not " $A)
      " (写作" (&neg $A) ") 理解为"
      (&impl $A $falsehood)
      ". 换句话说, " (&neg $A)
      "为真, 恰恰意味着假设" (&true $A)
      "是矛盾的, 因为由它我们能导出"
      (&true $falsehood) ".")
   (P "让我们定义"
      (MB (&def= (&neg $A)
                 (&impl $A $falsehood)))
      "并证明" $A "与" (&neg $A) "不可能同时为真."
      (MB (walk $..v
                (&true (&impl (@conj $A (&neg $A))
                              $falsehood))))
      "照例我们从推出引入开始."
      (MB (&rull
           (&implI $u)
           (walk (assume $u (&true (&conj $A (&neg $A))))
                 $..v
                 (&true $falsehood))
           (&true (&impl (@conj $A (&neg $A))
                         $falsehood))))
      "现在我们从假设出发, 用消去规则来推理. "
      "我们先把合取的两个分量都取出来."
      (MB (&rull
           (&implI $u)
           (walk (parallel
                  (&rull $conjE2
                         (assume $u (&true (&conj $A (&neg $A))))
                         (&true (&neg $A)))
                  (&rull $conjE1
                         (assume $u (&true (&conj $A (&neg $A))))
                         (&true $A)))
                 $..v
                 (&true $falsehood))
           (&true (&impl (@conj $A (&neg $A))
                         $falsehood))))
      "但我们在我们的语言中做了一个定义, 所以"
      (&neg $A) "实际上等同于"
      (&impl $A $falsehood)
      ". 因此下面我们得到完全相同的不完整证明, "
      "只不过是以等价的方式书写."
      (MB (&rull
           (&implI $u)
           (walk (parallel
                  (&rull $conjE2
                         (assume $u (&true (&conj $A (&neg $A))))
                         (&true (&impl $A $falsehood)))
                  (&rull $conjE1
                         (assume $u (&true (&conj $A (&neg $A))))
                         (&true $A)))
                 $..v
                 (&true $falsehood))
           (&true (&impl (@conj $A (&neg $A))
                         $falsehood))))
      "现在很容易看出, 我们可以用推出消去规则来完成这个证明."
      (MB (&rull
           (&implI $u)
           (&rull $implE
                  (&rull $conjE2
                         (assume $u (&true (&conj $A (&neg $A))))
                         (&true (&impl $A $falsehood)))
                  (&rull $conjE1
                         (assume $u (&true (&conj $A (&neg $A))))
                         (&true $A))
                  (&true $falsehood))
           (&true (&impl (@conj $A (&neg $A))
                         $falsehood))))
      $u "的所有使用都在作用域内, 因此这是一个有效的证明. "
      "它说明了即便" $falsehood
      "永远不为真, 我们仍然可以从矛盾的假设中证明它.")
   (H3. "总结: 自然演绎")
   (P "我们到目前为止定义的判断, "
      "命题和推理规则共同构成了一个自然演绎系统. "
      "它是由Gentzen [1935] 引入并由"
      "Prawitz [1965] 深入研究的系统的一个小变体. "
      "Gentzen的主要动机之一是设计尽可能直接地建模数学推理的规则, "
      "尽管显然比典型的数学论证要详细得多.")
   (P "对于这些规则潜在的真性判断的特定解释是直觉主义的或者说构造性的. "
      "这与真性的经典或者说布尔解释不同. 例如, 经典逻辑接受命题"
      (&disj $A (@impl $A $B)) "对任意" $A "和" $B
      "为真, 尽管在我们目前介绍的系统中这没有证明. "
      "经典逻辑基于每个命题必须为真或为假的原则. "
      "如果我们接受这种区分 (为真为假), 就会看到"
      (&disj $A (@impl $A $B))
      "应当被接受, 因为当" $A
      "为真时左析取支成立; 当" $A
      "为假时右析取支成立. 相比之下, "
      "直觉主义逻辑基于显式证据, "
      "析取的证据要求提供其中一个析取支的证据. "
      "我们将在后面回到经典逻辑及其与直觉主义逻辑的关系; "
      "目前我们的推理保持直觉主义的, 因为正如我们将看到的, "
      "它与函数式计算有直接的联系, 而经典逻辑则缺少这种联系.")
   (P "我们将到目前为止引入的真性判断的推理规则总结在图1中.")
   (H3. "记号性定义")
   (P "到目前为止, 我们通过引入规则来定义逻辑联结词的意义, "
      "这就是所谓的验证主义方法. "
      "另一种定义逻辑联结词的常见方式是通过记号定义. "
      "记号性定义用另一个已经定义了意义的命题"
      "来给出一个命题的一般形式的含义. "
      "例如, 我们可以定义逻辑等价为"
      (&conj (@impl $A $B) (@impl $B $A))
      ", 记作" (&<-> $A $B)
      ". 这个定义是合理的, 因为我们已经理解了推出和合取.")
   (P "如前所述, 直觉主义逻辑中另一个常见的记号性定义是"
      (&def= (&neg $A) (&impl $A $falsehood))
      ". 对于直觉主义否定的其他一些更为直接的定义也是存在的, "
      "本课程之后我们将会见到其中一些. "
      "或许其中最为直觉性的定义是说"
      (&true (&neg $A)) "若" (&false $A)
      ", 但是这需要新的关于falsehood的判断.")
   (P "记号定义可以很方便, 但有时也可能有些繁琐. "
      "我们有时会给出一个记号性定义, "
      "然后为该联结词推导出引入规则和消去规则. "
      "我们应当理解, 这些规则即便可能被称为引入规则或消去规则, "
      "也与那些定义了联结词的规则具有不同的地位. "
      "在这一特定的情形, 我们得到如下导出规则:"
      (MB (&split16
           (&rull
            (&negI $u)
            (walk
             (assume $u (&true $A))
             $..v
             (&true $falsehood))
            (&true (&neg $A)))
           (&rull
            $negE
            (&true (&neg $A))
            (&true $A)
            (&true $falsehood))))
      "你应当说服自己, 在" (&neg $A)
      "的记号性定义下, 这些确实是导出规则. "
      "它们几乎具有引入规则和消去规则的形式, "
      "只不过我们使用了" $falsehood "来定义" (&neg $A)
      ", 而此前我们避免在定义一个联结词时使用其他联结词.")
   (H3. "更多的例子")
   (P "我们再展示几个证明的例子, 从"
      (&impl $A (@impl $B (@conj $A $B))) "开始."
      (MB (&rull
           (&implI $u)
           (&rull
            (&implI $w)
            (&rull
             $conjI
             (assume $u (&true $A))
             (assume $w (&true $B))
             (&true (&conj $A $B)))
            (&true (&impl $B (@conj $A $B))))
           (&true (&impl $A (@impl $B (@conj $A $B))))))
      "注意这个推导不是假言性的 (它不依赖于任何假设). 标记为"
      $u "的假设" (&true $A)
      "在最后一步推理中被discharge, 标记为"
      $w "的假设" (&true $B)
      "在倒数第二步推理中被discharge. "
      "关键的一点是, 被discharge的假设不再可用于推理, "
      "并且推导中引入的所有标签必须互不相同. "
      "{译注: 标签相当于变量, 相同的标签相当于相同的变量, "
      "当然这也要考虑作用域问题就是了.}")
   ((tcomment)
    "我厌倦了无聊乏味手工拼装自然演绎的过程, "
    "做了一个无聊的EDSL原型, 以下是一些例子."
    (CodeB "(MB (ND (LAM $u $A
             (LAM $w $B
                  (CONS (VAR $u) (VAR $w))))))")
    (MB (ND (LAM $u $A
                 (LAM $w $B
                      (CONS (VAR $u) (VAR $w))))))
    (CodeB "(MB (ND (LAM $u `(conj ,$A ,$B)
             (CONS (CDR (VAR $u))
                   (CAR (VAR $u))))))")
    (MB (ND (LAM $u `(conj ,$A ,$B)
                 (CONS (CDR (VAR $u))
                       (CAR (VAR $u))))))
    (CodeB "(MB (ND (LAM $u `(-> (disj ,$A ,$B) ,$C)
             (CONS (LAM $w $A
                        (APP (VAR $u)
                             (INL $B (VAR $w))))
                   (LAM $x $B
                        (APP (VAR $u)
                             (INR $A (VAR $x))))))))")
    (MB (ND (LAM $u `(-> (disj ,$A ,$B) ,$C)
                 (CONS (LAM $w $A
                            (APP (VAR $u)
                                 (INL $B (VAR $w))))
                       (LAM $x $B
                            (APP (VAR $u)
                                 (INR $A (VAR $x))))))))
    (CodeB "(MB (ND (LAM $u `(conj (-> ,$A ,$C) (-> ,$B ,$C))
             (LAM $w `(disj ,$A ,$B)
                  (CASE (VAR $w)
                        $x (APP (CAR (VAR $u)) (VAR $x))
                        $y (APP (CDR (VAR $u)) (VAR $y)))))))")
    (MB (ND (LAM $u `(conj (-> ,$A ,$C) (-> ,$B ,$C))
                 (LAM $w `(disj ,$A ,$B)
                      (CASE (VAR $w)
                            $x (APP (CAR (VAR $u)) (VAR $x))
                            $y (APP (CDR (VAR $u)) (VAR $y)))))))
    (CodeB "(let ((Neg (lambda (A) `(-> ,A bot))))
  (MB (ND (LAM $u (Neg `(disj ,$A ,(Neg $A)))
               (APP (LAM $v (Neg $A)
                         (APP (VAR $u)
                              (INR $A (VAR $v))))
                    (LAM $w $A
                         (APP (VAR $u)
                              (INL (Neg $A) (VAR $w)))))))))")
    (let ((Neg (lambda (A) `(-> ,A bot))))
      (MB (ND (LAM $u (Neg `(disj ,$A ,(Neg $A)))
                   (APP (LAM $v (Neg $A)
                             (APP (VAR $u)
                                  (INR $A (VAR $v))))
                        (LAM $w $A
                             (APP (VAR $u)
                                  (INL (Neg $A) (VAR $w)))))))))
    "译注结束.")
   (P "当我们构造这样的推导时, "
      "通常采用自底向上和自顶向下相结合的方式. "
      "下一个例子是一个分配律, "
      "它允许我们将推出移入合取之中. "
      "这一次, 我们在每一步中展示部分证明. "
      "当然, 其他的证明构造步骤顺序也是可行的."
      (MB (walk $..v
                (&true (&impl (@impl $A (@conj $B $C))
                              (@conj (@impl $A $B)
                                     (@impl $A $C)))))))
   
   (P (MB (ND (LAM $u `(disj ,$A ,$B)
                   (CASE (VAR $u)
                         $v (INR $B (VAR $v))
                         $w (INL $A (VAR $w)))))))
   (H3 "参考文献")
   
   (H2. "和谐")
   (H3. "引论")
   (P "在通过引入规则对逻辑联结词进行验证主义定义时, "
      "我们曾简要地论证了消去规则的合理性. 在本讲中, "
      "我们将更深入地研究引入规则与消去规则之间的平衡关系. "
      "关于这一问题, Dummett [1991] 的讲义是一份很好的参考资料.")
   (P "我们将详细阐述验证主义的观点, "
      "即逻辑联结词是由其引入规则所定义的. "
      "我们将证明, 对于目前所介绍的直觉主义逻辑而言, "
      "消去规则与引入规则是和谐的, 即消去规则既不过强也不过弱. "
      "我们将分别通过局部归约和局部扩展来展示这一点.")
   (H3. "局部可靠性和局部完备性")
   (P "为了证明引入规则与消去规则是和谐的, "
      "我们需要建立两个性质: 局部可靠性和局部完备性.")
   (P "局部可靠性表明消去规则不会过强: "
      "无论我们如何对引入规则的结果施加消去规则, "
      "都不会获得任何新信息. "
      "我们通过以下方式来证明这一点: 对于消去规则的结论, "
      "我们总能找到一个比先引入再消去相关联结词更为直接的证明. "
      "这一点由对给定引入及其后续消去的局部归约所见证.")
   (P "局部完备性表明消去规则不会过弱: "
      "我们总能找到一种施加消去规则的方式, "
      "使得从其结果出发, 通过应用引入规则, "
      "可以重新构造出原命题的证明. "
      "这一点由将任意给定推导扩展为引入主联结词的推导的局部扩展所见证.")
   (P "从验证主义的视角来看, "
      "引入规则与消去规则在局部可靠且完备的意义上和谐的联结词, 才是被正当定义的. "
      "否则, 所提议的联结词就应当受到质疑. "
      "我们还希望统一适用的另一个准则是, "
      "引入规则和消去规则都不应涉及其他命题常项或联结词 (除了我们正在定义的那个), "
      "因为这可能在各个联结词之间产生危险的相互依赖. "
      "在给出正确定义的同时, 我们偶尔也会举一些反例, "
      "以说明违反有效推理模式背后原则所带来的后果.")
   (P "在下面对各个联结词的讨论中, 我们用记号"
      (MB (&==>R (label $D:script (&true $A))
                 (label $D:script^ (&true $A))))
      "表示把推导" $D:script
      "局部归约为关于同一判断" (&true $A)
      "的另一个推导" $D:script^
      ". 事实上, " $==>R
      "本身可以看作关联两个证明"
      $D:script "与" $D:script^
      "的更高层次的判断, "
      "不过我们不会直接利用这一观点. 类似地,"
      (MB (&==>E (label $D:script (&true $A))
                 (label $D:script^ (&true $A))))
      "是把" $D:script "局部展开为"
      $D:script^ "的记号.")
   (P (B "合取. ")
      "我们从局部可靠性开始, "
      "也就是对一个刚刚被引入的合取施加消去并作局部归约. "
      "由于存在两条消去规则和一条引入规则, "
      "我们需要考虑两种情形, 因为在" $conjI
      "引入规则之后可以跟着两条不同的消去规则"
      $conjE1 "与" $conjE2
      ". 在两种情形下我们都可以轻易地归约."
      (let ((A (GIVEN $D:script $A))
            (B (GIVEN $E:script $B)))
        (MB (&==>R (ND (CAR (CONS A B)))
                   (ND A))))
      (let ((A (GIVEN $D:script $A))
            (B (GIVEN $E:script $B)))
        (MB (&==>R (ND (CDR (CONS A B)))
                   (ND B))))
      "这两个归约说明: 当我们刚刚由" (&true $A)
      "的证明" $D:script "和" (&true $B)
      "的证明" $E:script "经引入规则" $conjI
      "证明了合取" (&conj $A $B)
      "为真之后, 借助消去规则所能取回的, "
      "只能是我们放进" (&true (&conj $A $B))
      "的证明中的东西. 这使得"
      $conjE1 "与" $conjE2
      "是局部可靠的, 因为我们取出来的只有"
      (&true $A) " (它已有直接证明" $D:script
      ") 以及" (&true $B)
      " (它已有直接证明" $E:script
      "). 上述两个归约使"
      $conjE1 "与" $conjE2
      "成为局部可靠的.")
   (P "局部完备性则确认我们不会因消去规则而损失信息. "
      "局部完备性要求我们对" (&true (&conj $A $B))
      "的任意一个证明施加消去规则, "
      "并使得我们能由其结果重新构造出"
      (&conj $A $B) "的证明."
      (let ((PAIR (GIVEN $D:script `(conj ,$A ,$B))))
        (MB (&==>E (ND PAIR)
                   (ND (CONS (CAR PAIR)
                             (CDR PAIR))))))
      "这个局部展开表明: 消去规则"
      $conjE1 "与" $conjE2
      "合在一起, 从判断" (&true (&conj $A $B))
      "中抽取出了用引入规则" $conjI
      "重新证明" (&true (&conj $A $B))
      "所需的全部信息. 请记住, 假设"
      (&true (&conj $A $B))
      "一旦可用, 就可以被多次使用, "
      "这一点在局部展开中体现得非常明显: "
      (&true (&conj $A $B))
      "的证明" $D:script
      "可以在左前提和右前提上分别重复使用.")
   (P "作为局部完备性失效的一个例子, 考虑我们"
      (Q "忘记") "了合取的第二条 (右) 消去规则"
      $conjE2 "的情形. 剩下的那条规则仍然是局部可靠的, "
      "因为它所证明的东西正是被放进"
      (&true (&conj $A $B))
      "的证明之中的; 但它不是局部完备的, "
      "因为我们无法从假设" (&conj $A $B)
      "中抽取出" $B "的证明. 于是, 举例来说, "
      "我们将无法证明"
      (&impl (@conj $A $B) (@conj $B $A))
      ", 尽管这显然应当为真.")
   (P (B "替换原理. ")
      "在讨论推出 (implication) 之前, 我们需要假言判断的定义性质. "
      
      )
   (H2 "Rec 1: Dcheck与和谐")
   (H2. "证明作为程序")
   (H3. "引论")
   (P "在本次讲座中, 我们将检视构造性证明的一种计算解释, "
      "并将其与函数式编程联系起来. "
      "在命题逻辑的层面上, "
      "这被称为Curry-Howard同构 [Howard, 1969]. "
      "从构造性逻辑与构造性数学发展的最初阶段起, "
      "一个核心思想就是: 证明应当表示构造. "
      "Curry-Howard同构只是这一思想的一个尤为深刻而优美的实现. "
      "在后来一篇极具影响力的论文中, "
      "Martin-Löf [1980] "
      "将其进一步发展为一种表达力更强的演算, 称为"
      (Em "直觉主义类型论") ".")
   (H3. "命题作为类型")
   (P "为了阐明证明与程序之间的关系, 我们引入一个新的判断:"
      (Blockquote
       (&: $M $A) ", " $M "是命题" $A "的一个证明项")
      "当我们写下这个判断时, 我们预设" $A
      "是一个命题. 我们也将" (&: $M $A)
      "解读为" (Q $M "是类型为" $A "的一个程序")
      ". 对同一判断的这两种对偶解读正是Curry-Howard同构的核心. "
      "我们既可以将" $M "视为表示" (&true $A)
      "的证明的一个句法项, 也可以将" $A "视为程序" $M
      "的类型. 在讨论每个逻辑联结词时, "
      "我们给出规则的两种解读, 以强调这种类比.")
   (P "我们期望: 若" (&: $M $A) ", 则" (&true $A)
      ". 反过来, 若" (&true $A)
      ", 则存在某个适当的证明项" $M
      "使得" (&: $M $A)
      ". 但我们还想要更多: " (&: $M $A)
      "的每一个推导都应当与" (&true $A)
      "的一个具有相同结构的推导相对应, 反之亦然. "
      "换言之, 我们用证明项来标注自然演绎的推理规则. "
      "这样一来, 上述性质就应当是显然的. 如此, "
      (&: $M $A) "中的证明项" $M "将直接对应于"
      (&true $A) "的相应证明.")
   (P "在术语上, 我们将" (&true $A)
      "称为综合判断 (synthetic judgment), "
      "因为我们需要以自然演绎的形式为其提供外部证据. 另一方面, "
      (&: $M $A) "是一个分析判断 (analytic judgment), "
      "因为它自身包含了证据, 我们可以有效地验证它. "
      "Martin-Löf [1994] 进一步详细阐述了这一重要区分.")
   (P (B "合取. ")
      "从构造性的角度, 我们将" (&true (&conj $A $B))
      "的证明看作一对证明: 一个用于" (&true $A)
      ", 一个用于" (&true $B)
      ". 因此, 若" $M "是" $A "的证明且"
      $N "是" $B "的证明, 则序对" (tupa0 $M $N)
      "是" (&conj $A $B) "的证明."
      (MB (&rull $conjI
                 (&: $M $A) (&: $N $B)
                 (&: (tupa0 $M $N)
                     (&conj $A $B)))))
   (P "消去规则对应于从序对到其第一个和第二个分量的投影, 即从序对"
      $M "中取回各个独立的证明."
      (MB (&split16
           (&rull $conjE1
                  (&: $M (&conj $A $B))
                  (&: (&fst $M) $A))
           (&rull $conjE2
                  (&: $M (&conj $A $B))
                  (&: (&snd $M) $B))))
      "因此, 合取" (&conj $A $B)
      "作为命题, 对应于积类型" (&c* $A $B)
      ". 而且, 函数式编程语言中的积类型确实具有与合取"
      (&conj $A $B) "相同的性质. 构造一个类型为"
      (&c* $A $B) "的序对" (tupa0 $M $N)
      "需要一个类型为" $A "的程序" $M
      "和一个类型为" $B "的程序" $N
      " (如" $conjI "所示). 给定一个类型为"
      (&c* $A $B) "的序对" $M ", 其类型为"
      $A "的第一个分量可以通过投影" (&fst $M)
      "获得 (如" $conjE1 "所示), 其类型为" $B
      "的第二个分量可以通过投影" (&snd $M)
      "获得 (如" $conjE2 "所示).")
   (P "一般而言, 一个逻辑联结词的引入规则"
      "对应于相应类型的构造子 (constructor). "
      "反之, 消去规则对应于解构子 (destructor).")
   (P (B "Truth. ")
      "从构造性的角度, 我们将" (&true $truth)
      "的证明看作一个不携带任何信息的单位元素."
      (MB (&rull $truthI
                 (&: (tupa0) $truth)))
      "因此, " $truth
      "对应于只有一个元素的单位类型" $unit
      ". 它没有消去规则, "
      "因而也没有进一步的证明项构造用于truth. "
      "{译注: 指用于解构的构造.} "
      "事实上, 我们在通过" $truthI "构造" (tupa0)
      "时没有放入任何信息, "
      "所以也不能指望通过消去规则从中取出任何信息.")
   (P (B "推出. ")
      "从构造性的角度, 我们将" (&true (&impl $A $B))
      "的证明看作一个将" (&true $A) "的证明变换为"
      (&true $B) "的证明的函数.")
   (P "在数学和许多编程语言中, 我们通过写"
      (&= (app $f $x) $..h)
      "来定义一个关于变量" $x "的函数" $f
      ", 其中右端的" (Q $..h) "依赖于" $x
      ". 例如, 我们可以写"
      (&= (app $f $x) (&- (&+ $x^2 $x) $1))
      ". 在函数式编程中, 我们可以改写为"
      (&= $f (Lam $x (&- (&+ $x^2 $x) $1)))
      ", 也就是说, 我们通过对一个变量 (在此例中为"
      $x ") 进行" $lambda
      "-抽象来显式地构成一个函数对象.")
   (P "在Standard ML语言的具体句法中, "
      (Lam $x $M) "写作" (Code "fn x => M")
      ", 但我们将使用Church and Rosser [1936] "
      "提出的通用且原始的记法. "
      "一般而言, 我们用一个点号 (" (Q $.)
      ") 将所谓的绑定变量与其作用域分隔开来. "
      "这与自然演绎中假设的辖域概念完全相同.")
   
   (H2. "验证")
   (H3. "引论")
   (P "验证主义 (verificationist) 的观点在本课程前面已经介绍过, 它主张: "
      "一个逻辑联结词的意义应当由它的引入规则来确定. "
      "从这一意义出发, 我们推导出消去规则, 并检验其可靠性与完备性. 这些"
      (Q "局部") "的检查每次只涉及单个联结词.")
   (P "在这一观点下, 一个由多个逻辑联结词构成的命题, 其意义又是什么呢? "
      "我们说, 一个命题的意义由它的诸多验证所确定 [Martin-Löf, 1983]. "
      "为了与对联结词的解释保持一致, 一个验证因此应当通过引入规则来进行. "
      "然而, 我们同样需要把消去规则考虑在内, "
      "因为它们不可避免地会出现在命题的证明之中.")
   (P "从直觉上来说, 一个验证应当是这样一种证明: 它只分析命题自身的构成成分. "
      "这种对全部可能证明所构成之空间的限制是必要的, 它使得该定义是良基的. "
      "例如, 如果我们允许所有的证明, 那么为了理解" $A
      "的意义, 我们就不得不去理解" (&impl $B $A) "与" $B
      "的意义, 于是整个验证主义进路便岌岌可危, 因为"
      $B "可能是一个包含" $A "的命题. 而这样一来, "
      $A "的意义反过来又依赖于" $A "的意义, 从而造成恶性循环.")
   (P "在本节中, 我们将使验证的结构更加明确. 我们用"
      (&ver $A) "表示判断" (Q $A "有一个验证")
      ". 自然地, 这应当意味着" $A
      "为真, 并且其证据具有某种特殊的形式. "
      "最终我们还将确立其逆命题: 若" $A "为真, 则" $A
      "有一个验证. 验证在证明搜索中也起着有益的作用, 因为"
      (&ver $A) "限制了" $A "的证明可以被如何构造.")
   (P "从证明搜索的角度看, "
      "验证这一概念被称为插入 "
      "(intercalation) [Sieg and Byrnes, 1998]. "
      "这个名称暗示了: 在构造一个推导时, 我们只从下方使用引入规则, "
      "只从上方使用消去规则, 从而向中间靠拢.")
   (P "除了理解命题的意义和刻画一种搜索策略之外, "
      "研究验证还有第三个重要理由: "
      "从程序设计语言的角度看, 它们催生了重要的双向类型检查 "
      "(bidirectional typechecking) 算法 "
      "[Dunfield and Krishnaswami, 2022].")
   (P "实际上, 以上这三者" --
      "验证, 插入, 以及双向定型" --
      "是同一回事!")
   (H3. "验证主义者的验证")
   (P "合取容易理解. " (&conj $A $B)
      "的一个验证应当由" $A "的一个验证和"
      $B "的一个验证组成."
      (MB (&rull $conjI
                 (&ver $A) (&ver $B)
                 (&ver (&conj $A $B))))
      "我们在此沿用引入规则的名称, "
      "因为这条规则与合取为真的引入规则是严格类似的.")
   (P "然而, 推出 (implication) 引入了一个新的假设, "
      "该假设并非由某条引入规则显式地证成, "
      "而仅仅是一个新的标签. 例如, 在如下证明中"
      
      )
   (H2 "Rec 2: 验证和证明项")
   (H2. "规则作为算法")
   (H3. "引论")
   (P "到目前为止, 我们主要使用推理规则来定义逻辑中的基本概念, "
      "例如(直觉主义)truth " (&true $A)
      "的概念或验证" (&ver $A)
      "的概念. 推理规则还有另一个非常重要的作用, "
      "即在非常高的抽象层次上描述算法. 事实上, "
      "将推理规则解释为定义某些算法的方式不止一种, "
      "我们将在课程后续部分进一步研究这一点. "
      "其中一种特定的方式是将规则视为描述如何自底向上地构造推导.")
   (P "这一观点的一个重要应用是双向类型检查, "
      "它以一种完全有原则的方式从验证的概念中自然产生. "
      "但它究竟如何与类型检查的算法联系起来呢? "
      "这就是今天这节课的主题. 不过, "
      "让我们先来看看我们的基本判断, "
      "并确定可以围绕它们提出哪些问题."
      (Ul (Li (&: $M $A) ", 其中" $A
              "已给定. 这是" (Em "定理证明")
              ". 换言之, 我们需要找到"
              (MB (∀ $A (&disj (@∃ $M (&: $M $A))
                               (&neg (@∃ $M (&: $M $A))))))
              "的一个构造性证明. 这里, 我们将判断" (&: $M $A)
              "实际上当作我们数学元语言中的一个命题来使用, "
              "在这个元语言中我们讨论判断的各种性质, "
              "例如局部可靠性或完备性. "
              "在经典数学中, 这个陈述是平凡的; "
              "而在直觉主义数学中, 一个证明需要包含一个算法, "
              "该算法能判定任意命题" $A
              "是否有证明 (若有则返回该证明), "
              "或者是否不存在这样的证明 (若不存在则给出相应的证据). "
              "一个提取出的函数可能具有如下类型:"
              (CodeB "decide_true : prop -> term option")
              "其中" (Code "prop") "是命题的类型, "
              (Code "term") "是证明项的类型. "
              )
          )
      )
   (H2. "相继式演算")
   (H3. "引论")
   (P "在本讲中, 我们转向一种不同的证明演算呈现方式. "
      "我们将相继式演算发展为自然演绎中证明搜索的形式系统. "
      "除了能够帮助我们理解证明搜索之外, "
      "相继式演算还使得证明过程中假设作用域的管理更加透明, "
      "并且允许我们进行更多的证明论研究, 即关于证明的性质的证明.")
   (P "相继式演算最初由Gentzen [1935] 引入, "
      "主要作为证明谓词逻辑一致性的技术工具. "
      "我们描述自然演绎证明搜索过程的目标, "
      "使我们倾向于采用Kleene [1952] 提出的一种称为"
      $G_3 "的形式化表述.")
   (P "我们的相继式演算旨在精确捕捉第5讲中引入的验证概念. "
      "回忆一下, 验证是自底向上构造的, 使用引入规则从结论到前提; "
      "而使用是自顶向下构造的, 使用消去规则从假设到结论. "
      "两者在中间交汇, 在那里我们从假设推导出的命题可以被用作验证. "
      "在相继式演算中, 这两个步骤都是自底向上进行的, "
      "这最终使我们能够证明局部可靠性和完备性性质的全局版本.")
   (H3. "相继式")
   
   (H2 "Rec 3: 相继式演算")
   (H2. "切消")
   (H3. "引论")
   
   (H2. "证明和验证")
   (H2 "Rec4: 规则归纳")
   (H2. "从证明系统到编程语言")
   (H2. "自然数")
   (H2 "Rec 5: 编程语言的动态语义")
   (H2. "延续")
   (H2. "谓词演算")
   (H2 "期中考试")
   (H2. "命题定理证明")
   (H2 "Rec6: 古典逻辑和谓词演算")
   (H2. "逆转")
   (H3. "引论")
   (P "无收缩相继式演算可以被视为描述了"
      "直觉主义命题逻辑的一个判定过程. "
      "这很好! 但一旦我们有一个包含许多前件的相继式, "
      "就会面临许多可以应用的规则选择. "
      "除非我们以某种方式进行" (Q "优化")
      ", 否则对于每一个试图证明的相继式, "
      "我们都不得不尝试所有的选择. 事实证明, "
      "除了非常小的例子之外, 这样做是不可行的.")
   (P "幸运的是, 有些规则具有这样的性质: "
      "我们可以始终应用它们而无需考虑其他替代方案. "
      "粗略地说, 这是因为只要结论是可证的, 所有前提也都是可证的. "
      "由于规则可以有许多不同的表述形式, "
      "我们不将此视为规则的性质, 而是联结词的性质. "
      "我们说一个联结词在右侧是可逆的, "
      "如果我们总是可以对该联结词应用右规则而无需考虑其他替代方案, "
      "并且在搜索过程中仍然保持完备性. 对称地, "
      "如果我们总是可以应用一个联结词的左规则并从相继式中移除主公式, "
      "则称该联结词在左侧是可逆的.")
   (P "因此, 策略是以确定性的方式分解相继式右侧和左侧所有可逆的联结词, "
      "直到我们遇到一个最终必须做出选择的情况. "
      "当存在多个选择时, 我们可能需要按某种顺序逐一尝试, 并在必要时进行回溯.")
   (P "正如我们在本课程中大多数时候所做的那样, "
      "我们通过一组限制性规则来捕捉这一策略. "
      "当尝试自底向上地用这些规则构造推导时, "
      "我们应当被迫遵循这一策略.")
   (P "我们将逐步发展这一内容, "
      "以便了解判断的各个组成部分从何而来. "
      "为简单起见, 我们不使用无收缩相继式演算, "
      "而是使用不含蕴涵 (implication) 精化的限制性相继式演算.")
   (H3. "限制性相继式演算")
   
   (H2. "certification")
   (H3. "引论")
   (P "我们在本讲开始时实现了一个"
      "基于第15讲中开发的带循环检测的逆转演算的判定过程. "
      "相应的实时代码可以在loop.sml中找到. "
      "这里没有本质上的新概念, "
      "只是一些关于将规则转化为代码这一过程的即兴思考, "
      "这些内容很难在讲义中传达.")
   (P "本讲的概念部分涉及认证. 在过去几十年中, "
      "谓词演算及更丰富的类型论的可证明性判定过程和定理证明器, "
      "采用了越来越有效和高效的实现方法. "
      "因此, 它们变得越来越难以证明其正确性, "
      "相应地也越来越不值得信赖. "
      "尤其在定理证明领域, 这是一个不可容忍的局面, "
      "因为我们依赖逻辑恰恰是为了获得保证!")
   (P "解决这一困境的途径是认证. "
      "我们设计定理证明器, 使其不仅返回是或否的答案, "
      "还返回一个可以独立检验的证书. "
      "在最简单的情况下, 这个证书是一个证明, "
      "我们可以为其编写一个直截了当的证明检查器, "
      "希望它比证明器本身简单得多. "
      "如果证明器返回" (Q "否")
      ", 情况就更加复杂了, "
      "因为证明不存在证明比展示一个证明要困难得多. "
      "对于广泛使用的系统, 如SAT求解器, "
      "这已经成为一个丰富而有趣的研究方向. "
      "在更一般的系统中, "
      "这一问题已在逻辑框架中得到研究 [Pfenning, 2001]. "
      "事实上, ML (即元语言) 的类型系统的起源, "
      "正是为了为定理证明器提供一个小型的可信核心 "
      "[Gordon et al., 1978].")
   (P "在本讲中, 我们考察如何对相继式演算进行增强, "
      "使其能够计算自然演绎的证明项, "
      "我们将自然演绎作为" (Q "金标准")
      ", 并且这些证明项可以由一个"
      "小型且简单的双向类型检查器来检验. "
      "各种改进 (如逆转演算, 聚焦演算, "
      "无收缩相继式演算, 以及它们的组合和进一步的改进) "
      "都可以基于相同的思想产生证明对象. "
      "你将在未来的作业中探索这一内容.")
   
   (H2 "Rec 7: 逆转")
   (H2. "聚焦")
   (H2. "逻辑编程")
   (H3. "引论")
   (P "判断及其定义性推理规则扮演着多重角色. "
      "其一, 我们用它们来定义真性和验证等基本概念. "
      "其二, 我们用它们来表达基本的算法思想, "
      "例如双向类型检查或直觉主义命题逻辑的判定过程. "
      "在后一种情形中, 我们通常自底向上地解读规则, "
      "并从中提取出函数式实现. 更形式化地说, "
      "这种函数式实现就是关于该判断及其规则的直觉主义证明的计算内容.")
   (P "现在我们将第二个角色进一步推进: 我们定义一种编程语言, "
      "其中按照推理规则构造推导即为计算的基本概念. "
      "证明的构造必须遵循一种固定的策略, 否则程序的计算行为将不可预测. "
      "这与证明归约和计算步骤之间的关系相呼应: "
      "后者同样必须遵循固定的策略才能产生可预测的结果.")
   (P "通过证明搜索来运行的这类语言被称为逻辑编程语言. "
      "它们大致可分为反向链接 "
      "(如Prolog [Kowalski, 1988, Colmerauer and Roussel, 1993]) "
      "和正向链接 (如Datalog [Maier et al., 2018]) 两类. "
      "这两者具有相当不同的特征, 我们将依次加以考察. "
      "在本讲中, 我们讨论反向链接与Prolog.")
   (P "现在我们来考察如何将一组推理规则解释为程序, "
      "其中涉及的各种设计决策.")
   (H3. "目标导向的证明构造")
   
   (H2 "Rec8: Prolog")
   (H2. "一个元循环解释器")
   (H2. "目标栈")
   (H3. "引论")
   (P "我们已经强调了高层规则描述的核心作用: "
      "一方面, 它们足够抽象, 可以支持正确性的形式化证明; "
      "另一方面, 它们又足够具体, 可以支持实现 "
      "(无论是直接用逻辑编程还是间接用函数式编程). "
      "今天, 我们将遇到这方面的另一个例子, "
      "同时也会练习规则归纳.")
   (P "当我们把证明归约作为计算的基本机制来研究时, "
      "我们从证明论所提供的局部归约出发, "
      "仔细地发展了一套小步归约的概念, "
      "使其满足保持性, 进展性和确定性. "
      "第19讲中的元循环解释器并不完全满足这些性质, "
      "因为对象逻辑中证明构造的细节"
      "仍然依赖于元逻辑中证明构造的细节. "
      "而两者都是Horn子句! 剩下的选择有:"
      (Ol (Li "子目标排序: 我们以什么顺序求解"
              (&conj $G_1 $G_2)
              "? 非形式化的策略是先推导"
              $G_1 ", 再推导" $G_2 ".")
          (Li "回溯: 我们以什么顺序尝试程序子句? "
              "非形式化的策略是按照它们给出的顺序依次尝试.")
          (Li "合一: 当量词被实例化时, 我们如何避免猜测项? "
              "非形式化的策略是找到由原子命题匹配所施加的等式的最一般解."))
      "在本讲中, 我们处理子目标排序的问题; "
      "在下一讲中, 我们来研究合一.")
   (H3. "延续作为目标栈")
   
   (H2 "Rec9: 无收缩相继式演算")
   (H2. "合一 (unification)")
   (H2. "逆方法")
   (H3. "引论")
   (P "我们通常解释推理规则的标准方式是自底向上的, "
      "即从结论读到前提. "
      "这对于目标导向的搜索以及命题情形下的判定过程 "
      "(如循环检测或无收缩相继式演算) 来说是很自然的. "
      "目标导向搜索中一个采用固定策略的重要例子是"
      "基于Horn子句的逻辑编程 (即Prolog风格的). "
      "前向链接则沿相反方向工作, "
      "但我们能否将其作为一般证明搜索或判定过程的基础呢? "
      "并且, 我们能否将其用作一种编程语言的基础?")
   (P "在今天的课程中, 我们来看第一个问题: "
      "如果我们关心的是一般性的定理证明 "
      "(以及可能的命题情形下的判定过程), "
      "我们能否以一种实用的方式使用前向推理和饱和? 答案是"
      (Q "可以") ", 这一洞见最初归功于Maslov [1964]. "
      "我们今天针对命题情形来展开这一方法, "
      "它同样完全适用于谓词演算. Maslov将他的方法称为"
      (Q "逆方法") ", 因为它与我们通常进行证明搜索的方向相反. "
      "这与作为减少证明搜索中非确定性的"
      "一般技术的逆转 (inversion) 无关.")
   (H3. "相继式演算的问题")
   (P "与归结 [Robinson, 1965] 相比, "
      "逆方法的一个巨大优势在于它是一种适用于"
      "(无切)相继式演算的通用技术, "
      "而归结法在其最初的构想中是相当特化于经典逻辑的. "
      "但是, 当我们审视相继式演算的规则时, "
      "它们从根本上是按照从结论到前提的方向来分解命题的. "
      "这似乎完全不适合自底向上的证明构造, "
      "因为我们可能会无限地应用诸如以下的规则"
      (MB (&rull $conjR
                 (Sequent Γ $A)
                 (Sequent Γ $B)
                 (Sequent Γ (&conj $A $B))))
      "从而推导出越来越大的合取式. 证明搜索将永远无法达到饱和.")
   (P "Maslov利用了Gentzen的洞见, 即在相继式演算的推导中, "
      "推导中出现的任何命题都是我们总目标相继式的子公式. "
      "于是, 我们只有在" (&conj $A $B)
      "是目标的子公式时才应用诸如" $conjR
      "之类的规则! 如果任何相继式都只允许由子公式组成, "
      "那么在命题情形下 (没有量化), "
      "当我们将前件视为集合 "
      "(即隐式地或显式地应用收缩) 时, "
      "相继式的数量是有限的. 这样, "
      "前向搜索在命题情形下必然达到饱和. "
      "在谓词逻辑的情形下, 如果我们有一个形如"
      (∀ $x (app $P $x))
      
      )
   (H2 "Rec 10: 逆转方法的例子")
   (H2. "运行时代码生成")
   (P "这次没有现成的讲义, 但是有推荐的阅读材料. "
      "第一个阅读材料是作者的论文"
      "Modal Types as Staging Specifications for Run-time Code Generation "
      "(模态类型作为运行时代码生成的阶段化描述). "
      "这篇论文探索和讨论了模态逻辑和多阶段编程之间的联系. "
      
      )
   (H2 "Rec11: lambda盒子")
   (H2. "线性逻辑")
   (H2. "同步消息传递")
   (H3. "引论")
   (P "在上一讲中, 我们引入了直觉主义线性逻辑, "
      "作为一种刻画资源推理的方式. "
      "我们以相继式演算而非自然演绎的形式来呈现该逻辑, "
      "因为资源的管理 (从逻辑角度看, 即前件的管理) "
      "在相继式演算中更加显式, 也更易于推理. "
      "我们没有使用局部归约和局部扩展, "
      "而是使用切割归约和恒等扩展来检验所有连接词的右规则和左规则是否具有和谐性. "
      "这直接关联到切割可容许性和恒等可容许性这两个全局性质, "
      "我们曾提及但未正式陈述或证明它们.")
   (P "自然演绎通过Curry-Howard对应为我们提供了函数式计算. "
      "在本讲中, 我们探讨线性相继式演算在计算层面带给我们什么. "
      "结果表明, 它对应的是同步消息传递. "
      "这一观察最为明确地出现在Caires和Pfenning [2010] "
      "以及Caires等人 [2016] 的工作中.")
   (H3. "证明作为进程")
   
   (H2 "Rec12: 线性逻辑")
   ))