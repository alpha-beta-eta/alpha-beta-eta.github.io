#lang racket
(provide exercises_in_lattice_theory.html)
(require SMathML)
(define (∃ Q P)
  (: $exists Q $cm P))
(define (Mprescripts #:attr* [attr* '()] . mathml*)
  `(mprescripts ,attr* . ,mathml*))
(define (^-> X) (^ X $->))
(define (^<- X) (^ X $<-))
(define (powerset X)
  (app $P:script X))
(define $id (Mi "id"))
(define (&id x) (_ $id x))
(define $Idl (Mi "Idl"))
(define $Fil (Mi "Fil"))
(define (&Idl P)
  (app $Idl P))
(define (&Fil P)
  (app $Fil P))
(define $darr (Mo "&darr;"))
(define $uarr (Mo "&uarr;"))
(define (↓ x)
  (: $darr x))
(define (↑ x)
  (: $uarr x))
(define (distributeR a * b + c)
  (&= (* a (@ (+ b c)))
      (+ (@ (* a b)) (@ (* a c)))))
(define $Im (Mi "Im"))
(define (&Im f) (app $Im f))
(define ((answer #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "answer")))
   (B (format "解答~a." n)) " " x*))
(define (format-num section index)
  (cond ((eq? (car section) '*)
         (if index
             (format "~a" index)
             #f))
        (else
         (if index
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     index)
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     "*")))))
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
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite
                          #:class class)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Theorem "定理" "theorem")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise")
  (Remark "评注" "remark")
  (Corollary "推论" "corollary"))
(define $<_P (_ $< $P))
(define $<=_P (_ $<= $P))
(define $<=_Q (_ $<= $Q))
(define $<=:id (Mi "&le;"))
(define $<=:id_P (_ $<=:id $P))
(define $<=:id_Q (_ $<=:id $Q))
(define $<==> (Mo "&DoubleLongLeftRightArrow;"))
(define $dashv (Mo "&dashv;"))
(define $RightVector (Mo "&RightVector;"))
(define-infix*
  (&<_P $<_P)
  (&<=_P $<=_P)
  (&<=_Q $<=_Q)
  (&<==> $<==>)
  (&meet $conj)
  (&join $disj)
  (&dashv $dashv)
  (&⊣ $dashv)
  (&RightVector $RightVector)
  (&=> $=>))
(define-@lized-op*
  (@meet &meet)
  (@join &join))
(define exercises_in_lattice_theory.html
  (TnTmPrelude
   #:title "格论练习和笔记"
   #:css "styles.css"
   (H1. "格论练习和笔记")
   (P "注意, 这本书默认偏序集是非空的.")
   (H2. "偏序集与格")
   (H3. "偏序集")
   
   (H3. "格与完备格")
   ((Definition)
    )
   ((Remark)
    )
   ((Definition)
    )
   ((Theorem)
    )
   ((Example)
    )
   ((Theorem)
    )
   ((Theorem)
    )
   ((Definition)
    )
   ((Example)
    )
   ((Definition)
    )
   ((Theorem)
    )
   ((Corollary)
    )
   ((Definition)
    )
   ((Theorem)
    )
   ((Definition)
    "设" $P "是一个偏序集, "
    (func $f $P $P)
    "是一个自映射. 如果"
    (Ol (Li "单调性/保序性: "
            (&<= $x $y) "可以推出"
            (&<= (app $f $x) (app $f $y))
            ", 对于任意的"
            (∈ $x $y $P) ";")
        (Li "增值性: "
            (&<= $x (app $f $x))
            ", 对于任意的"
            (∈ $x $P) ";")
        (Li "幂等性: "
            (&= (&compose $f $f) $f) ","))
    "那么则称" $f "是" $P
    "上的一个闭包算子 (closure operator).")
   (H3. "序同构和格同构")
   (H3. "分配格和Boole代数")
   ((Theorem)
    "设" $L "是一个格, 那么下面两个有限分配律是等价的:"
    (Ol (Li "对于任意的" (∈ $x $y $z $L) ", "
            (distributeR $x &meet $y &join $z) ";")
        (Li "对于任意的" (∈ $x $y $z $L) ", "
            (distributeR $x &join $y &meet $z) ".")))
   ((proof)
    )
   ((Definition)
    
    )
   (H3. "理想和滤子")
   (P "设" $P "是一个偏序集, " (&sube $S $P)
      ". 如果" $S "非空, 且对于任意的"
      (∈ $x $y $S) "都存在" (∈ $z $S)
      "使得" (&<= (&cm $x $y) $z) ", 则称"
      $S "是" $P "的有向子集/定向子集 (directed subset); 如果"
      $S "非空且对于任意的" (∈ $x $y $S)
      "都存在" (∈ $z $S) "使得"
      (&<= $z (&cm $x $y)) ", 则称"
      $S "是" $P "的滤过子集/可滤子集 (filtered subset).")
   ((Definition)
    "设" $P "是一个偏序集, " (&sube (&cm $I $F) $P) "."
    (Ol (Li "如果" $I "是一个定向下集, 则称" $I
            "是" $P "的理想 (ideal);")
        (Li "如果" $F "是一个滤过上集, 则称" $F
            "是" $P "的滤子 (filter).")))
   (P "如果" $S "是" $P "的一个理想 (respectively, 滤子), 且"
      (&!= $S $P) ", 那么称" $S "是" $P
      "的真理想 (proper ideal) (respectively, 真滤子). "
      "对于任意的" (∈ $x $P) ", 子集" (↓ $x)
      " (respectively, " (↑ $x) ") 是一个理想 "
      " (respectively, 滤子), 其被称为关于" $x
      "的主理想 (principal ideal) "
      "(respectively, 主滤子 (principal filter)). "
      "我们记" (&Idl $P) "为" $P "的所有理想构成的集合, "
      (&Fil $P) "为" $P "的所有滤子构成的集合.")
   (H3. "格中的特殊元素")
   (H3. "习题" #:auto? #f)
   ((Exercise)
    "找出所有的" $4 "元偏序集和" $5 "元格.")
   ((answer)
    
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
   ((Exercise)
    "设" $P "是一个偏序集, "
    (func (&cm $f $g) $P $P) "是闭包算子, "
    "证明下列条件等价:"
    (Ol (Li (&<= $f $g) ";")
        (Li (&= (&compose $f $g) $g) ";")
        (Li (&= (&compose $g $f) $g) ";")
        (Li (&sube (&Im $g) (&Im $f)) ".")))
   ((proof)
    (Ol (Li "由1推出2: "
            "根据" $f "的增值性, "
            (&<= (app $g $x) (app $f (app $g $x)))
            ". 根据" (&<= $f $g) ", "
            (&<= (app $f (app $g $x))
                 (app $g (app $g $x)))
            ". 又根据" $g "的幂等性, "
            (&<= (app $f (app $g $x))
                 (app $g $x))
            ". 综上所述, "
            (&= (app $f (app $g $x))
                (app $g $x))
            ". 鉴于" $x "的任意性, 故"
            (&= (&compose $f $g) $g) ".")
        (Li "由2推出3: "
            "根据" $f "的增值性, "
            (&<= $x (app $f $x))
            ". 根据" $g "的单调性, "
            (&<= (app $g $x) (app $g (app $f $x)))
            ". 根据" (&= (&compose $f $g) $g)
            "和" $g "的幂等性, 可以推出"
            (&= (app $g (app $f (app $g $x)))
                (app $g (app $g $x))
                (app $g $x))
            ". 根据" $g "的增值性, "
            (&<= $x (app $g $x))
            ". 根据" $g "和" $f "的单调性, "
            (&<= (app $g (app $f $x))
                 (app $g (app $f (app $g $x))))
            ", 即"
            (&<= (app $g (app $f $x))
                 (app $g $x))
            ". 综上所述, "
            (&= (app $g (app $f $x)) (app $g $x))
            ". 鉴于" $x "的任意性, 故"
            (&= (&compose $g $f) $g) ".")
        (Li "由3推出1: "
            "根据" $g "的增值性, "
            (&<= (app $f $x) (app $g (app $f $x)))
            ". 又根据"
            (&= (&compose $g $f) $g)
            ", 可以得到"
            (&<= (app $f $x) (app $g $x))
            ". 鉴于" $x "的任意性, 有"
            (&<= $f $g) "."))
    "根据以上推理, 可知1, 2, 3是等价的."
    (Ol (Li "由2推出4: "
            "根据"
            (&= (&compose $f $g) $g)
            ", 可知"
            (&= (&Im (&compose $f $g))
                (&Im $g))
            ". 又因为"
            (&sube (&Im (&compose $f $g))
                   (&Im $f))
            ", 故"
            (&sube (&Im $g) (&Im $f)) ".")
        (Li "由4推出2: "
            "因为"
            (&sube (&Im $g) (&Im $f))
            ", 所以"
            (&in (app $g $x) (&Im $f))
            ". 于是, 存在" (∈ $y $P)
            "使得"
            (&= (app $g $x) (app $f $y))
            ". 那么, 根据" $f
            "的幂等性, 可以推出"
            (&= (app $f (app $g $x))
                (app $f (app $f $y))
                (app $f $y)
                (app $g $x))
            ". 鉴于" $x "的任意性, 可知"
            (&= (&compose $f $g) $g) ".")))
   ((Exercise)
    )
   ((Exercise)
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
    (MB (&: (&dashv $f $g)
            (&RightVector $P $Q)) ".")
    "如果" (&= $P $Q) ", 则称" (tu0 $f $g)
    "是" $P "上的一个Galois伴随.")
   ((Example)
    (Ol (Li "设" (&sube $R (&c* $X $Y)) "是一个二元关系, 分别定义"
            (func (^-> $R) (powerset $X) (powerset $Y)) "和"
            (func (^<- $R) (powerset $Y) (powerset $X)) "为"
            (MB (&= (app (^-> $R) $A)
                    (setI (∈ $y $Y)
                          (∃ (∈ $x $A)
                             (∈ (tu0 $x $y) $R)))))
            (MB (&= (app (^<- $R) $B)
                    (setI (∈ $x $X)
                          (&=> (∈ (tu0 $x $y) $R)
                               (∈ $y $B)))))
            "则"
            (&: (&dashv (^-> $R) (^<- $R))
                (&RightVector (powerset $X) (powerset $Y)))
            ". {译注: 以上两个操作可以视为映射所诱导的image和preimage概念"
            "在关系上的推广.}")
        (Li "设" (&sube $R (&c* $X $Y)) "是一个二元关系, 分别定义"
            (func (Mmultiscripts $R (Mprescripts) $ $->)
                  (powerset $X) (powerset $Y))
            "和"
            (func (Mmultiscripts $R (Mprescripts) $ $<-)
                  (powerset $Y) (powerset $X))
            "为"
            (MB (&= (app (Mmultiscripts $R (Mprescripts) $ $->) $A)
                    (setI (∈ $y $Y)
                          (&=> (∈ (tu0 $x $y) $R)
                               (∈ $x $A)))))
            (MB (&= (app (Mmultiscripts $R (Mprescripts) $ $<-) $B)
                    (setI (∈ $x $X)
                          (∃ (∈ $y $B)
                             (∈ (tu0 $x $y) $R)))))
            "则"
            (&: (&dashv (Mmultiscripts $R (Mprescripts) $ $<-)
                        (Mmultiscripts $R (Mprescripts) $ $->))
                (&RightVector (powerset $Y) (powerset $X)))
            ".")
        (Li "设" (func $f $X $Y) "是一个映射, 将" $f "视为二元关系"
            (setI (tu0 $x (app $f $x)) (∈ $x $X)) ", 那么"
            
            )
        )
    )
   ((Theorem)
    "设" (func $f $P $Q) "和" (func $g $Q $P)
    "是偏序集之间的两个保序映射, 则下列条件等价:"
    (Ol (Li (&: (&dashv $f $g)
                (&RightVector $P $Q)) ";")
        (Li (&<= (&id $P) (&i* $g $f)) ", "
            (&<= (&i* $f $g) (&id $Q)) ".")))
   ((proof)
    )
   ((Theorem)
    "设" (&: (&dashv $f $g) (&RightVector $P $Q))
    ", 那么"
    (Ol (Li (&= (&i* $f $g $f) $f) ", "
            (&= (&i* $g $f $g) $g) ";")
        (Li (func (&i* $g $f) $P $P)
            "是闭包算子, "
            (func (&i* $f $g) $Q $Q)
            "是内部算子.")))
   
   ))