#lang racket
(provide measure.html)
(require SMathML)
(define $Sup (Mi "sup"))
(define (Sup c e)
  (ap (__ $Sup c) e))
(define $Inf (Mi "inf"))
(define (Inf c e)
  (ap (__ $Inf c) e))
(define (\[\] a b)
  (li0 a b))
(define (∀ . x*)
  (: $forall (apply ∈ x*)))
(define Σ $Sigma:normal)
(define $\\ (Mo "\\"))
(define $power $P:script)
(define (&power X)
  (ap $power X))
(define (Seq E)
  (_ (ang0 (_ E $n)) (∈ $n $NN)))
(define Seq:E (Seq $E))
(define-infix*
  (&\\ $\\)
  
  )
(define-@lized-op*
  (@\\ &\\)
  (@union &union)
  (@power &power)
  
  )
(define (format-section section)
  (apply string-append
         (add-between
          (map number->string (reverse section))
          ".")))
(define (h1-present %heading attr* . html*)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  `(h1 ,(attr*-set attr* 'id id)
       ,(format "第~a卷 " (format-section section))
       . ,html*))
(define (H1. #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present h1-present #:level 1)
    ,attr* . ,html*))
(define (h2-present %heading attr* . html*)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  `(h2 ,(attr*-set attr* 'id id)
       ,(format "第~a章 " (format-section section))
       . ,html*))
(define (H2. #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present h2-present #:level 2)
    ,attr* . ,html*))
(define (h3-present %heading attr* . html*)
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  `(h3 ,(attr*-set attr* 'id id)
       ,(format "第~a节 " (format-section section))
       . ,html*))
(define (H3. #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present h3-present #:level 3)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (format "~a.~a"
               (format-section section)
               (format-index index))))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B (format "~a. " num) name ". ")
        (B name ". "))))
(define (format-index index)
  (define (integer->upper i)
    (integer->char (+ i 64)))
  (let iter ((rest index) (result '()))
    (if (= rest 0)
        (list->string result)
        (iter (quotient (- rest 1) 26)
              (cons (integer->upper
                     (add1 (remainder (- rest 1) 26)))
                    result)))))
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
    (Cite `(a ((href ,href)) ,num)))
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
  
  )
(define (entry name)
  (Entry name "entry"))
(define measure.html
  (TnTmPrelude
   #:title "测度论"
   #:css "styles.css"
   (H1. "不可归约的最小")
   (H2. "测度空间")
   (H3. $sigma "-代数")
   ((Definition #:id "sigma-algebra")
    "令" $X "是一个集合. 一个"
    (B $X "的子集的" $sigma "-代数")
    " (有时也被称为" (B $sigma "-域")
    ") 是一个" $X "的子集的族" Σ ", 满足"
    (Ol #:attr* '((type "i"))
        (Li (∈ $empty Σ) ";")
        (Li "对于每个" (∈ $E Σ) ", 其于" $X "中的补"
            (&\\ $X $E) "属于" Σ ";")
        (Li "对于" Σ "中的每个序列" Seq:E ", 其并"
            (Union (∈ $n $NN) $E_n) "属于" Σ ".")))
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li "几乎所有数学主题的学习都是从定义开始的. "
            "在这个阶段, 没有替代死记硬背的学习方法. "
            "这些定义包裹了诸多人物数年 (有时甚至是数个世纪) "
            "的巧思, 你不能期望它们总是可以与你熟悉的想法对应起来.")
        (Li "然而, 你永远应该立即去寻求使新的定义变得更加具体的方法, "
            "一般是藉由你已有的数学经验寻找例子. 在这里"
            (Q $sigma "-代数") "的情况下, 以下所要描述的真正的例子"
            "本质上来说是全新的&mdash;&mdash;也就是说, "
            "你需要从根本上阅读本章. 然而, 你应该立即能够想到两个例子, "
            "而之后你应该将这两个例子铭记在心:"
            (Ol #:attr* '((type "i"))
                (Li "对于任意的" $X ", " (&= Σ (setE $empty $X))
                    "是" $X "的子集的一个" $sigma "-代数.")
                (Li "对于任意的" $X ", " (&power $X)
                    ", 即" $X "的所有子集构成的集合, 是"
                    $X "的子集的一个" $sigma "-代数."))
            "这些当然是" $X "的子集的" $sigma "-代数中最小的和最大的. "
            "而且, 尽管我们不会在这两个例子身上花多少时间, "
            "实际上它们仍然是重要的.")
        (Li "术语" (B "可测空间") "经常用来指代一个序对"
            (tu0 $X Σ) ", 其中" $X "是一个集合而"
            Σ "是" $X "的子集的一个" $sigma "-代数. "
            "然而, 就我个人而言, 除非时间紧迫, 否则我将避免使用这个术语, "
            "因为实际上这种对象的许多最有趣的例子并无有用的测度与之关联.")))
   (((entry "无穷并和交"))
    "如果你还没有见过无穷并, 那么值得驻足观察一下" (Union (∈ $n $NN) $E_n)
    ". "
    )
   (((Entry (Span $sigma "-代数的基本性质") "entry"))
    "如果" Σ "是" $X "的子集的一个" $sigma "-代数, 那么其具有以下性质."
    (Ol #:attr* '((type "a"))
        (Li "对于任意的" (∈ $E $F Σ) ", " (∈ (&union $E $F) Σ)
            ". " $P:bold-script " 因为如果" (∈ $E $F Σ)
            ", 置" (&= $E_0 $E) ", 而" (&>= $n $1) "时"
            (&= $E_n $F) ", 那么" Seq:E "是" Σ "中的一个序列, 而"
            (∈ (&= (&union $E $F) (Union (∈ $n $NN) $E_n)) Σ)
            ". " $Q:bold-script)
        (Li "对于任意的" (∈ $E $F Σ) ", " (∈ (&cap $E $F) Σ)
            ". " $P:bold-script " 根据" (Ref "sigma-algebra")
            "的定义之ii, " (∈ (&\\ $X $E) Σ) "且"
            (∈ (&\\ $X $F) Σ) "; 根据本条目之a, "
            (∈ (&union (@\\ $X $E) (@\\ $X $F)) Σ)
            "; 再次根据" (Ref "sigma-algebra") "之ii, "
            (∈ (&\\ $X (@union (@\\ $X $E) (@\\ $X $F))) Σ)
            "; 但这就是" (∈ (&cap $E $F) Σ) ". " $Q:bold-script)
        (Li "对于任意的" (∈ $E $F Σ) ", " (∈ (&\\ $E $F) Σ)
            ". " $P:bold-script " "
            (∈ (&= (&\\ $E $F) (&cap $E (@\\ $X $F))) Σ)
            ". " $Q:bold-script)
        (Li "现在设" Seq:E "是" Σ "中的一个序列, 并考虑"
            (MB (deriv (Cap (∈ $n $NN) $E_n)
                       (setI $x (&cm (∈ $x $E_n) (∀ $n $NN)))
                       (&cap $E_0 $E_1 $E_2 $..c)
                       (&\\ $X (Union (∈ $n $NN) (@\\ $X $E_n)))))
            "其也属于" Σ ".")))
   (((entry "更多关于无穷并和交的讨论"))
    
    )
   (((entry "可数集合"))
    
    )
   (((entry "Borel集合"))
    "这里我可以描述一类非平凡的" $sigma "-代数; 其构造是抽象的, "
    "但是这样的技术是重要的, 并且这个术语也是测度论的基本词汇的一部分."
    (Ol #:attr* '((type "a"))
        (Li "令" $X "是一个集合, 令" $S:fraktur "是任意的" $X
            "的子集的" $sigma "-代数的非空的族. (因此, "
            $S:fraktur "的每个" (Em "成员") "本身就是一个集合的"
            (Em "族") "; " (&sube $S:fraktur (&power (@power $X)))
            ".) 那么"
            (MB (&= (Cap $S:fraktur)
                    (setI $E (&cm (∈ $E Σ) (∀ Σ $S:fraktur)))))
            "即所有属于" $S:fraktur "的" $sigma "-代数之交, 是"
            $X "的子集的一个" $sigma "-代数. " $P:bold-script
            
            )
        )
    )
   (H3. "测度空间")
   (P "我希望我们已经准备好迎来第二个定义了, 这个定义是此专著所有工作之基础.")
   ((Definition)
    "一个" (B "测度空间") "是一个三元组" (tu0 $X Σ $mu) ", 其中"
    (Ol #:attr* '((type "i"))
        (Li $X "是一个集合;")
        (Li Σ "是" $X "的子集的一个" $sigma "-代数;")
        (Li (func $mu Σ (\[\] $0 $inf))
            "是一个函数, 满足"
            (Ol #:attr* '((style "list-style-type: lower-greek;"))
                (Li (&= (ap $mu $empty) $0) ";")
                (Li "如果" Seq:E "是" Σ "中的一个互不相交 (disjoint) 的序列, 那么"
                    (&= (app $mu (Union (∈ $n $NN) $E_n))
                        (sum (&= $n $0) $inf (ap $mu $E_n))) "."))))
    "在此上下文之中, " Σ "的成员被称为" (B "可测") "集合, 而" $mu
    "被称为" (B $X "上的一个测度") ".")
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (((entry "测度空间的基本性质"))
    "令" (tu0 $X Σ $mu) "是一个测度空间."
    (Ol #:attr* '((type "a"))
        (Li "如果" (∈ $E $F Σ) "且" (&= (&cap $E $F) $empty) ", 那么"
            (&= (app $mu (&union $E $F)) (&+ (ap $mu $E) (ap $mu $F))) ".")
        (Li "如果" (∈ $E $F Σ) "且" (&sube $E $F) ", 那么"
            (&<= (ap $mu $E) (ap $mu $F)) ".")
        (Li "对于任意的" (∈ $E $F Σ) ", "
            (&<= (app $mu (&union $E $F)) (&+ (ap $mu $E) (ap $mu $F))) ".")
        (Li "如果" Seq:E "是" Σ "中任意的序列, 那么"
            (&<= (app $mu (Union (∈ $n $NN) $E_n))
                 (sum (&= $n $0) $inf (ap $mu $E_n))) ".")
        (Li "如果" Seq:E "是" Σ "中的一个非降序列 (也就是说, 对于每个"
            (∈ $n $NN) ", " (&sube $E_n (_ $E (&+ $n $1)))
            "), 那么"
            (MB (&= (app $mu (Union (∈ $n $NN) $E_n))
                    (lim $n $inf (ap $mu $E_n))
                    (Sup (∈ $n $NN) (ap $mu $E_n))) "."))
        (Li "如果" Seq:E "是" Σ "中的一个非升序列 (也就是说, 对于每个"
            (∈ $n $NN) ", " (&sube (_ $E (&+ $n $1)) $E_n)
            "), 并且某个" (ap $mu $E_n) "是有限的, 那么"
            (MB (&= (app $mu (Cap (∈ $n $NN) $E_n))
                    (lim $n $inf (ap $mu $E_n))
                    (Inf (∈ $n $NN) (ap $mu $E_n))) "."))))
   ((proof)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (H3. "外测度和Carathéodory构造")
   (P "这里我将引入构造测度的最重要方法.")
   ((Definition)
    "现在我们来到本章的第三个基本定义." (Br)
    "令" $X "是一个集合. " $X "上的一个" (B "外测度") "是一个函数"
    (func $theta (&power $X) (\[\] $0 $inf)) ", 满足"
    (Ol #:attr* '((type "i"))
        (Li (&= (ap $theta $empty) $0) ";")
        (Li "如果" (&sube $A $B $X) ", 那么"
            (&<= (ap $theta $A) (ap $theta $B)) ";")
        (Li "对于" $X "的子集的每个序列" (Seq $A) ", "
            (&<= (app $theta (Union (∈ $n $NN) $A_n))
                 (sum (&= $n $0) $inf (ap $theta $A_n))) ".")))
   ((Remark)
    (Ol #:attr* '((type "a"))
        (Li ""
            )
        )
    )
   (H1. "广阔的基础")
   (H1. "测度代数")
   (H1. "拓扑测度空间")
   (H1. "集合论式测度论")
   (H1. "随机分析")
   ))