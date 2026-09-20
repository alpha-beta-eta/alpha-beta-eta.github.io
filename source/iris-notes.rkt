#lang racket
(provide iris-notes.html)
(require SMathML)
(define $later (Mo "▷"))
(define (Later P)
  (ap $later P))
(define (Meta str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $persistent (Meta "persistent"))
(define (Persistent P)
  (app $persistent P))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define (Box #:attr* [attr* '()] . xml*)
  `(menclose ,(attr*-set attr* 'notation "box")
             . ,xml*))
(define (DBox . xml*)
  `(mrow ((style "border: 0.8px dashed black; padding: 0px 4px;"))
         . ,xml*))
(define (Inv N P)
  (^ (Box P) N))
(define (Own γ a)
  (^ (DBox a) γ))
(define $? (Mi "?"))
(define (&? x) (^ x $?))
(define $dummy (Mi "&minus;"))
(define (Core a) (&abs a))
(define $⇝ (Mo "⇝"))
(define $impl $=>)
(define $Valid (OverBar $V:script))
(define (Valid a) (app $Valid a))
(define $prcue (Mo "&prcue;"))
(define $exclusive (Meta "exclusive"))
(define (Exclusive a)
  (app $exclusive a))
(define $Auth (Mi "Auth"))
(define (Auth M) (app $Auth M))
(define $• (Mo "•"))
(define (• a) (ap $• a))
(define $◦ (Mo "◦"))
(define (◦ a) (ap $◦ a))
(define-infix*
  (&prcue $prcue)
  (Bind $.)
  (&⇝ $⇝)
  (&impl $impl))
(define-@lized-op*
  (@impl &impl))
(define iris-notes.html
  (TnTmPrelude
   #:title "iris笔记"
   #:css "styles.css"
   (H1. "iris笔记")
   (H2. "资源代数")
   (H3. "核 (core)")
   (MB (&def= (&⇝ $a $b)
              (∀ (∈ (&? $c) (&? $M))
                 (&impl (Valid (&d* $a (&? $c)))
                        (Valid (&d* $b (&? $c)))))))
   (P $a "能转移到" $b ", 如果与" $a
      "相兼容的资源也与" $b "相兼容.")
   (P (∈ $bottom (&? $M)) "代表没有资源, "
      "所以与没有资源复合就没有作用.")
   (P "排他资源因为无法与其他资源复合, "
      "因而可以转移至任意的(合法)资源.")
   (MBL "(RA-CORE-ID)"
        (∀ $a (&impl (∈ (Core $a) $M)
                     (&= (&d* (Core $a) $a) $a))))
   (MBL "(RA-CORE-IDEM)"
        (∀ $a (&impl (∈ (Core $a) $M)
                     (&= (Core (Core $a))
                         (Core $a)))))
   (P "如果" (∈ $a $M) "满足" (∈ (Core $a) $M)
      ", 那么" (∈ (Core (Core $a)) $M) ", 于是"
      (&= (&d* (Core (Core $a))
               (Core $a))
          (Core $a))
      ". 又因为" (&= (Core (Core $a)) (Core $a))
      ", 所以" (&= (&d* (Core $a) (Core $a)) (Core $a))
      ". 即" (Core $a) "是复合运算" $d*
      "的幂等元, 如果" (Core $a) "有定义的话.")
   (MBL "(RA-CORE-IDEM0)"
        (∀ $a (&impl (∈ (Core $a) $M)
                     (&= (&d* (Core $a) (Core $a))
                         (Core $a)))))
   (P "从直觉上来说, 这表达了" (Core $a)
      "是可复制的. 并且, 即便"
      (&= (Core $a) $bottom)
      ", 由于我们对于复合进行了延拓, 故"
      (&= (&d* $bottom $bottom) $bottom)
      ", 所以仍然有"
      (&= (&d* (Core $a) (Core $a)) (Core $a))
      ", 即没有资源当然也可以复制. "
      "不过, 更准确地说, " (Core $a)
      "代表了" (Q "知识") ", 它从"
      $a "中将排他的部分剥离出去. "
      "排他资源本身就是排他的, "
      "所以核只能为" $bottom "."
      (MB (&def= (Exclusive $a)
                 (∀ $c (&neg (Valid (&d* $a $c)))))))
   (P "另外, 根据资源代数上的预序关系的定义, "
      "我们还能知道"
      (MB (∀ (∈ $a $M)
             (&impl (∈ (Core $a) $M)
                    (&prcue (Core $a) $a))))
      "这也是相当直觉化的, 即从" $a
      "中提取的知识" (Core $a)
      "应当比" $a "更小.")
   (P "核的公理还包括"
      (MBL "(RA-CORE-MONO)"
           (∀ $a $b
              (&impl (&conj (∈ (Core $a) $M)
                            (&prcue $a $b))
                     (&conj (∈ (Core $b) $M)
                            (&prcue (Core $a) (Core $b))))))
      "这里的直觉是资源越多, 知识越多, "
      "至少知识不应该变少.")
   (P "如果" (&= (Core $a) $a) ", 那么"
      $a "都是知识, 故iris有规则"
      (MBL "(PERSISTENT-GHOST)"
           (&rule (&= (Core $a) $a)
                  (Persistent (Own $gamma $a)))))
   (H3. "权威资源代数 (auth RA)")
   (P "以下给出权威资源代数的构造. "
      "对于单位资源代数"
      (tu0 $M $epsilon $Valid (Core $dummy) $d*)
      ", 权威资源代数" (Auth $M) "定义如下:"
      (Ul (Li "载体: "
              (&c* (_cm $M $bottom $top) $M))
          (Li "复合运算:"
              (MB (&= (&d* (tu0 $x $a) (tu0 $y $b))
                      (Choice0
                       ((tu0 $y (&d* $a $b))
                        ", 如果" (&= $x $bottom))
                       ((tu0 $x (&d* $a $b))
                        ", 如果" (&= $y $bottom))
                       ((tu0 $top (&d* $a $b))
                        ", 否则的话")))))
          (Li "核:"
              (MB (&= (Core (tu0 $x $a))
                      (tu0 $bottom (Core $a)))))
          (Li "合法性:"
              (MB (&= (Valid (tu0 $x $a))
                      (&disj (@conj (&= $x $bottom)
                                    (Valid $a))
                             (@conj (∈ $x $M)
                                    (Valid $x)
                                    (&prcue $a $x))))))
          (Li "我们定义"
              (&= (• $x) (tu0 $x $epsilon)) "而"
              (&= (◦ $a) (tu0 $bottom $a))
              ", 于是" (tu0 $bottom $epsilon)
              "是" (Auth $M) "的单位元.")))
   (H2. "later模态")
   (H3. "语义")
   (P "将iris命题" $P "近似地看作序对" (tu0 $k $r)
      "的集合, 其中" $k "是一个自然数而" $r
      "是一个资源.")
   (P "将" $k "想成是步数索引, "
      "即多少归约步骤之内我们知道"
      $r "在" $P "中.")
   (P "如果" (∈ (tu0 $k $r) $P) "且"
      (&<= $m $k) ", 那么也有"
      (∈ (tu0 $m $r) $P) ".")
   (P "步骤索引可以解释" $later ":"
      (MB (&= (Later $P)
              (&union
               (setI (tu0 (&+ $m $1) $r)
                     (∈ (tu0 $m $r) $P))
               (setI (tu0 $0 $r)
                     (∈ $r $R:script))))))
   (P "以下我们证明L&ouml;b的soundness, "
      "也就是我们要证明"
      (&sube (@impl (Later $P) $P) $P)
      ". 当然, 这两边其实是解释. "
      "另外, 我们需要定义"
      (MB (&= (@impl $P $Q)
              (setI (tu0 $k $r)
                    (∀ (&<= $m $k)
                       (&impl (∈ (tu0 $m $r) $P)
                              (∈ (tu0 $m $r) $Q))))))
      "令命题" (app $P:script $k) "为"
      (Q "对于每个资源" $r ", 如果"
         (∈ (tu0 $k $r) (@impl (Later $P) $P))
         ", 那么" (∈ (tu0 $k $r) $P))
      ".")
   (P (app $P:script $0) "是平凡的, "
      "因为" (∈ (tu0 $0 $r) (@impl (Later $P) $P))
      "等价于" (∈ (tu0 $0 $r) $P) ".")
   (P "假设" (app $P:script $n) "成立, 考察"
      (app $P:script (&+ $n $1)) ". 如果"
      (∈ (tu0 (&+ $n $1) $r) (@impl (Later $P) $P))
      ", 那么" (∈ (tu0 $n $r) (@impl (Later $P) $P))
      ". 根据归纳假设, " (∈ (tu0 $n $r) $P)
      ". 于是, " (∈ (tu0 (&+ $n $1) $r) (Later $P))
      ". 由此可知, " (∈ (tu0 (&+ $n $1) $r) $P) ".")
   
   ))