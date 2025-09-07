#lang racket
(provide catlog.html)
(require SMathML)
(define (format-num section index)
  (and index
       (string-append
        (apply string-append
              (add-between
               (map number->string (cdr (reverse section)))
               "."))
        (format ".~a" index))))
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
  (Proposition "命题" "proposition"))
(define (distributeL x + y * z)
  (&= (* (@ (+ x y)) z)
      (+ (* x z) (* y z))))
(define (distributeR z * x + y)
  (&= (* z (@ (+ x y)))
      (+ (* z x) (* z y))))
(define $. (Mo "."))
(define (∀ R P)
  (: $forall R $. P))
(define (∃ R P)
  (: $exists R $. P))
(define-infix*
  (&conj $conj))
(define-@lized-op*
  (@conj &conj))
(define (make-cat str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define-syntax-rule (define-cat* (id str) ...)
  (begin
    (define id (make-cat str))
    ...))
(define-cat*
  (CatSet "Set")
  )
(define catlog.html
  (TnTmPrelude
   #:title "范畴逻辑引论"
   #:css "styles.css"
   (H1. "范畴逻辑引论")
   (H2. "代数理论")
   (P "代数理论 (algebraic theory) 是对于结构的描述, "
      "其完全基于操作 (operation) 和等式 (equation). "
      "所有这样的代数概念都共有着一些相当深刻而广泛的性质, "
      "从自由代数的存在性到Lawvere的对偶理论. "
      "这些之中最基本的内容在本章呈现. "
      "这里的发展也充当着" (Em "函子语义(functorial semantics)")
      "的一般模式 (general scheme) 的第一个例子和模板, "
      "之后的章节里也要用于其他逻辑概念.")
   (H3. "句法和语义")
   (P "我们从诸如群, 环, 格之类的代数结构的一般方法开始. "
      "这些是由公理化 (axiomization) 所刻画的, "
      "其只牵涉单一种类 (sort) 的变量和常量, "
      "操作, 以及等式. 操作在每一处都有定义是重要的, "
      "这排除了两个重要的例子: 域, 鉴于" $0
      "的(乘)逆没有定义, 以及范畴, 鉴于复合只对特定的态射序对有定义.")
   (P "让我们从相当基础的代数理论开始: 群的理论. "
      "在一阶逻辑之中, 一个群可以被描述为一个集合" $G
      "带有一个二元操作" (func $d* (&c* $G $G) $G)
      ", 其满足两条一阶公理:"
      (MB (∀ (∈ $x $y $z $G)
             (associate &d* $x $y $z)))
      (MB (∃ (∈ $e $G)
             (∀ (∈ $x $G)
                (∃ (∈ $y $G)
                   (@conj
                    (&= (&d* $e $x)
                        (&d* $x $e)
                        $x)
                    (&= (&d* $x $y)
                        (&d* $y $x)
                        $e))))))
      "更仔细地观察这些公理的逻辑形式, "
      "我们会发现第二条公理 "
      "(其表达了单位元和逆元的存在性) "
      "有点不尽如人意, 因为它牵涉嵌套的量词. "
      "不仅这会使得解释复杂化, "
      "而且其也并非真正必要, "
      "鉴于一个群中的单位元和逆元是唯一确定的. "
      "因此, 我们可以将它们添加到结构之中"
      "并重新表述如下. 单位元以一个特别 (distinguished) "
      "的" (Em "常量") (∈ $e $G)
      "表示, 而逆则是一个幺元操作"
      (func (inv $) $G $G)
      ". 然后, 我们可以得到一个等价的表述, "
      "其中所有的公理都可以被表达为(全称量化的)"
      (Em "等式") ":"
      (MB (associate &d* $x $y $z))
      (MB (set-attr*
           (&Table
            ((&= (&d* $x $e) $x)
             (&= (&d* $e $x) $x))
            ((&= (&d* $x (inv $x)) $e)
             (&= (&d* (inv $x) $x) $e)))
           'columnalign "left"
           'columnspacing "3em"))
      "全称量词" (: $forall (∈ $x $G)) ", "
      (: $forall (∈ $y $G))
      "在陈述公理时不再必要, "
      "因为我们可以将所有的变量解释为遍历"
      $G "的每个元素 (因为我们限制于考虑完全定义操作). "
      "在描述中显式提及特定的集合" $G
      "也无真正的必要. 最后, 既然常量" $e
      "可以被视为一个零元操作, 即一个函数" (func $e $1 $G)
      ", 对于群的概念的描述可以仅有操作和等式构成. "
      "这将我们引向了以下对于代数理论的一般定义.")
   ((Definition)
    "一个代数理论的一个" (Em "签名(signature)") $Sigma:normal
    "由一个集合族" (_ (setE (_ $Sigma:normal $k)) (∈ $k $NN))
    "构成. " (_ $Sigma:normal $k) "的元素被称为"
    (Em $k "元操作(" $k "-ary operation)")
    ". 特别地, " (_ $Sigma:normal $0) "的元素被称为"
    (Em "零元操作") "或者" (Em "常量") "." (Br)
    "一个签名" $Sigma:normal "的" (Em "项(term)")
    "是由以下规则归纳构造的表达式:"
    (Ol (Li "变量" (&cm $x $y $z $..h) "是项,")
        (Li "如果" (&cm $t_1 $..h $t_k)
            "是项而" (∈ $f (_ $Sigma:normal $k))
            "是一个" $k "元操作, 那么"
            (appl $f $t_1 $..h $t_k) "是一个项.")))
   ((Definition)
    "(参见定义1.2.10). 一个" (Em "代数理论")
    (&= $TT (tu0 (_ $Sigma:normal $TT) $A_TT))
    "由一个签名" (_ $Sigma:normal $TT) "和一个"
    (Em "公理") "集" $A_TT
    "给出, 其中公理是项之间的等式 (形式化地, 项的序对).")
   (P "代数理论也被称为" (Em "等式理论(equational theory)")
      ". 我们并不假定集合" (_ $Sigma:normal $k) "或者"
      $A_TT "有限, 但是单独的项和等式只牵涉有限多个变量.")
   ((Example)
    "一个含幺交换环的理论是一个代数理论. "
    "存在两个零元操作 (常量) " $0 "和" $1
    ", 一个幺元操作" $- ", 还有两个二元操作"
    $+ "和" $d* ". 等式为:"
    (MB
     (&Table
      ((associate &+ $x $y $z)
       (associate &d* $x $y $z))
      ((&= (&+ $x $0) $x)
       (&= (&d* $x $1) $x))
      ((&= (&+ $0 $x) $x)
       (&= (&d* $1 $x) $x))
      ((&= (&+ $x (@- $x)) $0)
       (distributeL $x &+ $y &d* $z))
      ((&= (&+ (@- $x) $x) $0)
       (distributeR $z &d* $x &+ $y))
      ((commute &+ $x $y)
       (commute &d* $x $y)))))
   ((Example)
    "既没有操作也没有等式的" (Q "空") "或者平凡理论"
    $TT_0 "是一个集合的理论.")
   ((Example)
    "只有一个常量而没有等式的理论是一个"
    (Em "带点集合") "的理论, 参见例子A.4.11.")
   ((Example)
    "令" $R "是一个环. 存在一个左" $R "模的理论. "
    "其有一个常量" $0 ", 一个幺元操作" $-
    ", 一个二元操作" $+ ", 并且对于每个"
    (∈ $a $R) "有一个操作" (OverBar $a)
    ", 其被称为" (Em "乘上" $a "的标量乘法")
    ". 以下等式成立:"
    (MB (&Table
         ((associate &+ $x $y $z)
          (commute &+ $x $y))
         ((&= (&+ $x $0) $x)
          (&= (&+ $0 $x) $x))
         ((&= (&+ $x (@- $x)) $0)
          (&= (&+ (@- $x) $x) $0))))
    "对于每个" (∈ $a $b $R) ", 我们也有等式"
    (MB (&cm (&= (app (OverBar $a) (&+ $x $y))
                 (&+ (ap (OverBar $a) $x)
                     (ap (OverBar $a) $y)))
             (&= (app (OverBar $a)
                      (ap (OverBar $b) $x))
                 (app (OverBar (@i* $a $b)) $x))
             (&= (app (OverBar (@+ $a $b)) $x)
                 (&+ (ap (OverBar $a) $x)
                     (ap (OverBar $b) $x)))))
    "乘上" $a "的标量乘法通常记作" (&d* $a $x)
    "而非" (ap (OverBar $a) $x)
    ". 如果我们将环" $R "替换为一个域" $FF
    ", 那么我们就得到了" $FF "上的一个向量空间的代数理论 "
    "(尽管域的理论并非代数的!). "
    "{译注: 译者感觉向量空间的代数理论还需要另外的一条公理保证"
    "域的幺元和向量空间的元素的标量乘法仍然得到相同的这个向量空间元素.}")
   ((Example)
    "在计算机科学之中, 归纳数据类型是代数理论的例子. "
    "例如, 带有以整数标记的叶子的二叉树或许可以在某个编程语言中定义如下:"
    (CodeB "type tree = Leaf of int | Node of tree * tree")
    "这对应于对于每个整数" $n "都有一个常量" (ap (Ms "Leaf") $n)
    "且还有一个二元操作" (Ms "Node") "的代数理论. "
    "实际上, 当计算机科学家定义像这样的一个数据类型时, "
    "他们在心中有一个特定的理论模型, 称为" (Em "自由(free)") "模型.")
   ((Example)
    "一个显然的非例 (non-example) 是偏序集的理论, "
    )
   (H4. "代数理论的模型" #:switch? #f)
   (P "让我们考虑代数理论的" (Em "模型") ", 即" (Em "代数")
      ". 经典地, 一个群可以由一个集合" $G ", 一个元素"
      (∈ $e $G) ", 一个函数" (func $m (&c* $G $G) $G)
      ", 一个函数" (func $i $G $G) "给出, 其满足群公理:"
      (eqn*
       ((appl $m $x (appl $m $y $z))
        $=
        (appl $m (appl $m $x $y) $z))
       ((appl $m $x (ap $i $x))
        $=
        (&= (appl $m (ap $i $x) $x) $e))
       ((appl $m $x $e)
        $=
        (&= (appl $m $e $x) $x)))
      "对于任意的" (∈ $x $y $z $G)
      "成立. 然而, 观察到这种概念可以被轻易地一般化"
      "以使得我们可以在异于" CatSet
      "的范畴之中讨论群论的模型. "
      
      )
   (H2. "命题逻辑")
   (H2. "一阶逻辑")
   (H2. "类型论")
   (H2. "依赖类型论")
   ))