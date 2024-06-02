#lang racket
(provide kerodon.html)
(require SMathML)
(define (\[\] a b)
  (: $lb0 a $cm b $rb0))
(define $sime (Mo "&sime;"))
(define $pi_<=1 (_ $pi (&<= $ $1)))
(define-infix*
  (&sime $sime)
  )
(define (format-num section index)
  (and index
       (let ((l (length section)))
         (cond
           ((= l 2) (format "~s.0.0.~s" (car section) index))
           ((= l 3) (format "~s.~s.0.~s" (cadr section) (car section) index))
           ((= l 4) (format "~s.~s.~s.~s"
                            (caddr section) (cadr section)
                            (car section) index))
           (else (error 'format-num "unknown format of section: ~s" section))))))
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
  (Question "问题" "question")
  )
(define kerodon.html
  (TnTmPrelude
   #:title "Kerodon"
   #:css "styles.css"
   (H1. "Kerodon")
   (H2. $inf "范畴的语言")
   (P "代数拓扑的主要目的在于通过代数和组合不变量理解拓扑空间. "
      "让我们考虑一些基本的例子.")
   (Ul (Li "对于任意的拓扑空间" $X ", 我们可以联系" $X
           "的" (Em "路径分量") "的集合" (app $pi_0 $X)
           ". 这是" $X "除以等价关系" $sime "的商, 其中"
           (&sime $x $y) ", 如果存在一条连续路径"
           (&: $p (&-> (\[\] $0 $1) $X)) "满足"
           (&= (app $p $0) $x) "且" (&= (app $p $1) $y) ".")
       (Li "对于任意装备有一个基点" (∈ $x $X) "的拓扑空间" $X
           ", 我们可以联系" (Em "基本群") (appl $pi_1 $X $x)
           ". 这是一个群, 其元素是满足" (&= (app $p $0) $x (app $p $1))
           "的连续路径" (&: $p (&-> (\[\] $0 $1) $X)) "的同伦等价类."))
   (P "出于诸多目的, 将集合" (app $pi_0 $X) "与基本群"
      (_ (setE (appl $pi_1 $X $x)) (∈ $x $X))
      "组合为一个单独的数学对象是有用的. 对于任意的拓扑空间" $X
      ", 我们可以联系一个不变量" (app $pi_<=1 $X) ", 其被称为"
      $X "的" (Em "基本群胚") ". 基本群胚" (app $pi_<=1 $X)
      "是一个范畴, 其对象是" $X "的点, 而从点" (∈ $x $X)
      "到点" (∈ $y $X) "的一个态射是满足"
      (&= (app $p $0) $x) "且" (&= (app $p $1) $y)
      "的连续路径" (&: $p (&-> (\[\] $0 $1) $X))
      "的一个同伦等价类. 然后, 路径分量的集合" (app $pi_0 $X)
      "可以被恢复为范畴" (app $pi_<=1 $X)
      "的对象的同构类的集合, 并且每个基本群" (appl $pi_1 $X $x)
      "可以被等同为作为范畴" (app $pi_<=1 $X) "的对象的点" $x "的自同构群. "
      "范畴论这一形式化允许我们汇集路径分量和基本群的信息为一个方便的整体.")
   (P "基本群胚" (app $pi_<=1 $X) "是拓扑空间" $X
      "的一个非常重要的不变量, 但远非一个完备的不变量. "
      "特别是, 它没有包含任何关于" (Em "高阶") "同伦群"
      (_ (setE (appl $pi_n $X $x)) (&>= $n $2))
      "的信息. 因此, 我们提出以下问题:")
   ((Question)
    "令" $X "是一个拓扑空间. "
    )
   (H3. "单纯集")
   (H3. "从拓扑空间到单纯集")
   (H3. "从范畴到单纯集")
   (H3. $inf "范畴")
   ))