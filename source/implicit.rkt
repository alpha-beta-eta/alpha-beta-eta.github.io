#lang racket
(provide implicit.html)
(require SMathML)
(define todo.svg
  (Svg
   #:attr* '((width "320")
             (height "160")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Path #:attr* '((x "0")
                   (y "0")
                   (d "M 0 0 h 320 v 160 h -320 z")
                   (fill "none")))
   (Text #:attr* '((x "130") (y "80")) "欠一张图")))
(define (format-num section index)
  (and index
       (apply string-append
              (add-between
               (map number->string
                    (cdr (reverse (cons index section))))
               "."))))
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
(define (MBL label . exp*)
  (MB (Mtable
       #:attr*
       '((columnalign "left center right")
         (width "100%"))
       (Mtr (Mtd (Mphantom label))
            (apply Mtd exp*)
            (Mtd label)))))
(define $cos (Mi "cos"))
(define (&cos x)
  (app $cos x))
(define $~ (Mo "~"))
(define implicit.html
  (TnTmPrelude
   #:title "隐函数定理"
   #:css "styles.css"
   (H1. "隐函数定理")
   (H2. "隐函数定理引论")
   (H3. "隐函数")
   (P "对于微积分的初学者而言, 函数由诸如"
      (MBL "(1.1)"
           (&= (app $f $x)
               (&- (&+ $x^3 (&i* $2 $x^2))
                   $x $3)))
      (MBL "(1.2)"
           (&= (app $g $y)
               (Msqrt (&+ $y^2 $1))))
      (MBL "(1.3)"
           (&= (app $h $t)
               (&cos (&i* $2 $pi $t))))
      "这样的解析表达式给出. "
      "实际上, 250年前这是L&eacute;onard Euler (1707-1783) "
      "所采取的方法 (见Euler [EB88]):"
      (Blockquote
       "A function of a variable quantity is an analytic expression "
       "composed in any way whatsoever of the variable quantity "
       "and numbers or constant quantities."))
   (P "几乎是在同一时间, 有人发现" (Q "由公式给出函数")
      "这一概念对于微积分的目的而言太过具有限制性了. 例如,"
      (MBL "(1.4)"
           $y^5 $+ (&i* 16 $y)
           $- (&i* 32 $x^3)
           $+ (&i* 32 $x)
           $= $0)
      todo.svg
      "的轨迹定义了一个很好的" $RR^2 "的子集, 草绘于图1.1之中. "
      "这个图片 (figure) 让我们怀疑这个轨迹是不是" $y "作为" $x
      "的函数的图 (graph). 但是, 并没有可以用来描述这个函数的公式存在.")
   (P "与将函数当作公式的朴素定义相比, "
      "函数的现代的集合论式的定义基于函数的图 (graph) 陈述. "
      "精确地说, 定义域为" $X "而陪域为" $Y
      "的一个函数" $f "是笛卡尔积"
      (MB (&= (&c* $X $Y)
              (setI (tu0 $x $y)
                    (&cm (∈ $x $X)
                         (∈ $y $Y)))))
      "的一个子集, 其具有性质(i)对于每个" (∈ $x $X)
      ", 存在一个元素" (∈ (tu0 $x $y) $f)
      "; (ii)如果" (∈ (tu0 $x $y) $f) "且"
      (∈ (tu0 $x (^^ $y $~)) $f) ", 那么"
      (&= $y (^^ $y $~)) ". 在这两个性质成立的情况下, "
      (∈ $x $X) "的选择确定了唯一使得"
      (∈ (tu0 $x $y) $f) "的" $y
      "; 藉着唯一性, 我们发现简记"
      (MB (&= $y (app $f $x)))
      "以表达" (∈ (tu0 $x $y) $f) "比较方便.")
   ((Example)
    
    )
   (H3. "隐函数定理的一个非正式版本")
   ((Example)
    
    )
   ))