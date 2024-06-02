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
(define-infix*
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
   
   ))