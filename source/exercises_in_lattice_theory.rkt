#lang racket
(provide exercises_in_lattice_theory.html)
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
  (Exercise "练习" "exercise"))
(define $<_P (_ $< $P))
(define $<=_P (_ $<= $P))
(define $<=_Q (_ $<= $Q))
(define $<=:id (Mi "&le;"))
(define $<=:id_P (_ $<=:id $P))
(define $<=:id_Q (_ $<=:id $Q))
(define $<==> (Mo "&DoubleLongLeftRightArrow;"))
(define-infix*
  (&<_P $<_P)
  (&<=_P $<=_P)
  (&<=_Q $<=_Q)
  (&<==> $<==>))
(define exercises_in_lattice_theory.html
  (TnTmPrelude
   #:title "格论练习和笔记"
   #:css "styles.css"
   (H1. "格论练习和笔记")
   (P "注意, 这本书默认偏序集是非空的.")
   (H2. "偏序集与格")
   (H3. "偏序集")
   (H3. "格与完备格")
   (H3. "序同构和格同构")
   (H3. "分配格和Boole代数")
   (H3. "理想和滤子")
   (H3. "格中的特殊元素")
   (H3. "习题" #:auto? #f)
   ((Exercise)
    )
   ((Exercise)
    "设" (tu0 $P $<=:id_P) "和" (tu0 $Q $<=:id_Q)
    "是两个偏序集, 在笛卡尔积" (&c* $P $Q)
    "上定义二元关系如下:"
    (MB (&<==>
         (&<= (tu0 $x_1 $y_1) (tu0 $x_2 $y_2))
         (: (&<_P $x_1 $x_2) "或"
            (&cm (&= $x_1 $x_2)
                 (&<=_Q $y_1 $y_2)))) ".")
    "证明: " $<= "是" (&c* $P $Q)
    "上的一个偏序, 其被称为" (&c* $P $Q)
    "上的字典序 (lexicographic order), 且"
    $<= "是全序当且仅当" $<=_P
    "和" $<=_Q "都是全序.")
   ((proof)
    "对于每个" (∈ (tu0 $x $y) (&c* $P $Q))
    ", 我们有" (&= $x $x) "和"
    (&<=_Q $y $y) ", 故"
    (&<= (tu0 $x $y) (tu0 $x $y))
    ", 这就说明了自反性. "
    "对于任意的" (&<= (tu0 $x_1 $y_1) (tu0 $x_2 $y_2))
    "和" (&<= (tu0 $x_2 $y_2) (tu0 $x_3 $y_3))
    ", 鉴于" $<= "的两个条件是互斥的, "
    "所以说其实就是四种情况. "
    )
   (H2. "Galois伴随和Galois连接")
   (H3. "Galois伴随")
   ((Definition)
    "设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的两个保序映射, 如果对于任意的"
    (&cm (∈ $a $P) (∈ $b $Q)) "都有"
    (MB (&<==> (&<= (app $f $a) $b)
               (&<= $a (app $g $b))))
    "那么则称序对" (tu0 $f $g) "是从" $P
    "到" $Q "的一个Galois伴随 (Galois correspondence或"
    "Galois adjunction), 记作"
    
    )
   ))