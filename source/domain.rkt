#lang racket
(provide domain.html)
(require SMathML)
(define (H4. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #f] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h4-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (let ((xsec (reverse section)))
         (format "~s.~s.~s" (cadr xsec) (caddr xsec) index))))
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
  (Example "例子" "example")
  (Theorem "定理" "theorem")
  (Definition "定义" "definition")
  (Lemma "引理" "lemma")
  (Corollary "推论" "corollary")
  (Proposition "命题" "proposition")
  (Remark "评注" "remark")
  
  )
(define $sqsube (Mo "&sqsube;"))
(define $sqsube:id (Mi "&sqsube;"))
(define $sqsupe (Mo "&sqsupe;"))
(define $==> (Mo "&xrArr;"))
(define-infix*
  (&sqsube $sqsube)
  (&sqsupe $sqsupe)
  (&==> $==>)
  (&conj $conj)
  (&disj $disj))
(define domain.html
  (TnTmPrelude
   #:title "domain论"
   #:css "styles.css"
   (H1. "domain论")
   (P "也有人将domain theory翻译为论域论, "
      "不过既然这里的domain和逻辑学中的论域并不相同, "
      "我认为这个翻译方式是不合理的.")
   (H2. "引论和概览")
   (H3. "起源")
   (H3. "我们的方法")
   (H3. "概览")
   (H2. "单独看待domain")
   (P "我们将从引入domain论的基本语言开始.")
   (H3. "收敛")
   (H4. "偏序集和预序")
   ((Definition)
    "一个集合" $P "连带着一个二元关系" $sqsube
    "被称为一个偏序集, 如果对于所有的"
    (∈ $x $y $z $P) "有以下条件成立:"
    (Ol (Li (&sqsube $x $x) " (自反性)")
        (Li (&==> (&conj (&sqsube $x $y)
                         (&sqsube $y $z))
                  (&sqsube $y $z))
            " (传递性)")
        (Li (&==> (&conj (&sqsube $x $y)
                         (&sqsube $y $x))
                  (&= $x $y))
            " (反对称性)")))
   (P "小的有限偏序集可以画成线图, 即所谓的Hasse图. "
      )
   ((Proposition)
    
    )
   (H4. "序论里的记号")
   (P "下列概念构成了序论的核心语言.")
   ((Definition)
    "令" (tupa0 $P $sqsube:id) "是一个有序集."
    (Ol (Li $P "的一个子集" $A
            "是一个上集 (upper set), 如果"
            (∈ $x $A) "可以推出对于所有的"
            (&sqsupe $y $x) "都有" (∈ $y $A)
            ". 我们以"
            )
        )
    )
   (H3. "近似")
   (H3. "拓扑")
   (H2. "看待多个domain")
   (H3. "比较domain")
   (H3. "有限构造")
   (H3. "无限构造")
   (H2. "domain的笛卡尔闭范畴")
   (H2. "递归domain方程")
   (H2. "等式理论")
   (H2. "domain和逻辑")
   (H2. "未来的方向")
   ))