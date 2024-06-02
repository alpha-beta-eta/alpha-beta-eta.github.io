#lang racket
(provide cat_awodey.html)
(require SMathML)
(define (Quotient X R) (&/ X R))
(define eqc
  (case-lambda
    ((x) (bra0 x))
    ))
(define-syntax-rule (define-simple* (&id $id str) ...)
  (begin
    (define $id (Mi str))
    ...
    (define (&id x) (app $id x))
    ...))
(define-simple*
  (&ker $ker "ker")
  (Aut $Aut "Aut")
  (&dom $dom "dom")
  (&cod $cod "cod")
  (&range $range "range")
  
  )
(define $~ (Mo "&Tilde;"))
(define Func
  (case-lambda
    ((A f B) (: A (^^ $-> f) B))
    ((A f B . arg*)
     (: A (^^ $-> f) (apply Func B arg*)))))
(define-syntax-rule (eqn* (x ...) ...)
  (MB (set-attr*
       (&Table (x ...) ...)
       'columnalign "right center left"
       'displaystyle "true")))
(define $->-> (Mo "&rrarr;"))
(define (set-compact op)
  (set-attr* op 'lspace "0" 'rspace "0"))
(define $~:compact (set-compact $~))
(define X/~ (Quotient $X $~:compact))
(define (Cat:Arrow Cat)
  (^ Cat $->))
(define $op (Mi "op"))
(define (&op C) (^ C $op))
(define $Hom (Mi "Hom"))
(define Hom
  (case-lambda
    ((A B) (appl $Hom A B))
    ((C A B) (appl (_ $Hom C) A B))))
(define Arrow
  (case-lambda
    ((X f Y) (: X (^^ $-> f) Y))
    ((X f Y . Z*)
     (: X (^^ $-> f) (apply Arrow Y Z*)))))
(define Cat:Pos (Mi "Pos" #:attr* '((mathvariant "bold"))))
(define Cat:Rel (Mi "Rel" #:attr* '((mathvariant "bold"))))
(define (make-cat str) (Mi str #:attr* '((mathvariant "bold"))))
(define-syntax-rule (define-cat* (id str) ...)
  (begin
    (define id (make-cat str))
    ...))
(define-cat*
  (Cat:Sets "Sets")
  (Cat:C "C")
  (Cat:D "D")
  (Cat:Mon "Mon")
  (Cat:Top "Top")
  )
(define str:empty (Mi "-"))
(define @compose (@lize &compose))
(define $iso (Mo "&cong;"))
(define $d*M (_ $d* $M))
(define $d*N (_ $d* $N))
(define $=> (Mo "&Implies;"))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define (&exist x p)
  (: $exist x $. p))
(define $and (Mo "&amp;"))
(define-infix*
  (&iso $iso)
  (&d*M $d*M)
  (&d*N $d*N)
  (&=> $=>)
  (&and $and)
  (&~ $~)
  
  )
(define (forget x) (&abs x))
(define (fforget f A B)
  (apply func (map forget (list f A B))))
(define @d* (@lize &d*))
(define marker0
  (Marker
   #:attr*
   '((id "arrow")
     (viewbox "0 0 10 10")
     (refX "5")
     (refY "5")
     (markerWidth "6")
     (markerHeight "6")
     (orient "auto-start-reverse"))
   (Path #:attr* '((d "M 0 2 L 6 5 L 0 8 z")))))
(define marker1
  (Marker
   #:attr*
   '((id "tail")
     (markerWidth "10")
     (markerHeight "10")
     (refX "5")
     (refY "5")
     (orient "auto"))
   (Path #:attr* '((d "M 0,5 A 5,5 0 1,1 10,5")
                   (fill "none")
                   (stroke-width "2")))))
(define (:arrc start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (marker-start "url(#tail)")
          (stroke-width "1.2px"))))
(define (:arrowc start end #:prop [prop 0.8] #:offset [vec vec:zero])
  (define t (/ (- 1 prop) 2))
  (define p1 ((lerp t) (pt+ start vec) (pt+ end vec)))
  (define p2 ((lerp (- 1 t)) (pt+ start vec) (pt+ end vec)))
  (:arrc p1 p2))
(define (n2s n)
  (format "~s" (exact-round n)))
(define (:dot cx cy #:r [r 2])
  (Circle
   #:attr*
   `((cx ,(n2s cx)) (cy ,(n2s cy)) (r ,(n2s r)))))
(define (::dot pt #:r [r 2])
  (:dot (pt-x pt) (pt-y pt) #:r r))
(define (make-pt x y) (vector 'pt x y))
(define (pt-x pt) (vector-ref pt 1))
(define (pt-y pt) (vector-ref pt 2))
(define (make-vec x y) (vector 'vec x y))
(define (vec-x vec) (vector-ref vec 1))
(define (vec-y vec) (vector-ref vec 2))
(define (pt+ p v)
  (make-pt (+ (pt-x p) (vec-x v))
           (+ (pt-y p) (vec-y v))))
(define (vec* k v)
  (make-vec (* k (vec-x v)) (* k (vec-y v))))
(define (vec+ u v)
  (make-vec (+ (vec-x u) (vec-x v))
            (+ (vec-y u) (vec-y v))))
(define vec:zero (make-vec 0 0))
(define vec:down (make-vec 0 1))
(define vec:up (make-vec 0 -1))
(define vec:left (make-vec -1 0))
(define vec:right (make-vec 1 0))
(define offset:down (make-vec -7 -4))
(define offset:up (make-vec -7 -13))
(define offset:left (make-vec -14 -9))
(define offset:right (make-vec -2 -9))
(define (:FO pos #:offset [offset 'down] #:scale [scale 10] . x*)
  (define offset-vec
    (case offset
      ((down) (vec+ (vec* scale vec:down) offset:down))
      ((up) (vec+ (vec* scale vec:up) offset:up))
      ((left) (vec+ (vec* scale vec:left) offset:left))
      ((right) (vec+ (vec* scale vec:right) offset:right))
      ((none) (make-vec 0 0))))
  (define position (pt+ pos offset-vec))
  (keyword-apply
   ForeignObject
   '(#:attr*)
   `(((x ,(n2s (pt-x position)))
      (y ,(n2s (pt-y position)))
      (width "100")
      (height "30")))
   x*))
(define (pt- p1 p2)
  (make-vec
   (- (pt-x p1) (pt-x p2))
   (- (pt-y p1) (pt-y p2))))
(define ((lerp t) p1 p2)
  (pt+ p1 (vec* t (pt- p2 p1))))
(define (:line start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (stroke-width "1.2px"))))
(define (:arr start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (stroke-width "1.2px"))))
(define (:darr start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (marker-start "url(#arrow)")
          (stroke-width "1.2px"))))
(define (:arrow start end #:prop [prop 0.8] #:offset [vec vec:zero])
  (define t (/ (- 1 prop) 2))
  (define p1 ((lerp t) (pt+ start vec) (pt+ end vec)))
  (define p2 ((lerp (- 1 t)) (pt+ start vec) (pt+ end vec)))
  (:arr p1 p2))
(define (set-dotted line)
  (set-attr* line 'stroke-dasharray "2 2"))
(define UMP:coequalizer.svg
  (let ((A (make-pt 30 30))
        (B (make-pt 160 30))
        (Q (make-pt 290 30))
        (Z (make-pt 290 130)))
    (Svg
     #:attr* '((width "320")
               (height "160")
               (stroke "black")
               (style "display: block; margin: auto;"))
     (Defs marker0)
     (:FO A $A)
     (:FO B $B)
     (:FO Q $Q)
     (:FO Z $Z)
     (:arrow A B #:offset (make-vec 0 7))
     (:arrow A B #:offset (make-vec 0 19))
     (:arrow B Q #:offset (make-vec 0 12))
     (:arrow B Z #:offset (make-vec 0 12))
     (set-dotted (:arrow Q Z #:offset (make-vec 0 12)))
     (:FO (pt+ ((lerp 1/2) A B) (make-vec 0 -20)) $f)
     (:FO (pt+ ((lerp 1/2) A B) (make-vec 0 12)) $g)
     (:FO (pt+ ((lerp 1/2) B Q) (make-vec 0 -15)) $q)
     (:FO (pt+ ((lerp 1/2) Q Z) (make-vec 10 -5)) $u)
     (:FO (pt+ ((lerp 1/2) B Z) (make-vec -3 3)) $z)
     )))
(define UMP:equalizer.svg
  (let ((E (make-pt 30 30))
        (Z (make-pt 30 130))
        (A (make-pt 160 30))
        (B (make-pt 290 30)))
    (Svg
     #:attr* '((width "320")
               (height "160")
               (stroke "black")
               (style "display: block; margin: auto;"))
     (Defs marker0)
     (:FO E $E)
     (:FO Z $Z)
     (:FO A $A)
     (:FO B $B)
     (:arrow A B #:offset (make-vec 0 7))
     (:arrow A B #:offset (make-vec 0 17))
     (:arrow E A #:offset (make-vec 0 12))
     (:arrow Z A #:offset (make-vec 0 12))
     (:FO (vec+ ((lerp 1/2) A B) (make-vec 0 -22)) $f)
     (:FO (vec+ ((lerp 1/2) A B) (make-vec 0 13)) $g)
     (:FO (vec+ ((lerp 1/2) E A) (make-vec 0 -17)) $e)
     (:FO (vec+ ((lerp 1/2) Z A) (make-vec 10 10)) $z)
     (set-attr*
      (:arrow Z E #:offset (make-vec 0 12) #:prop 0.7)
      'stroke-dasharray "2 2")
     (:FO (vec+ ((lerp 1/2) E Z) (make-vec -7 -5)) $u)
     )))
(define UMP:free-monoid.svg
  (Svg
   #:attr* '((width "640")
             (height "320")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs marker0)
   (:FO (make-pt 30 0)
        (Em "在" (Mi "Mon" #:attr* '((mathvariant "bold"))) "之中:"))
   (set-attr* (:arrow (make-pt 240 60) (make-pt 400 60) #:prop 0.7)
              'stroke-dasharray "4 4")
   (:FO (make-pt 240 60) #:offset 'left (app $M $A))
   (:FO (make-pt 400 60) #:offset 'left #:scale 3 $N)
   (:FO (make-pt 320 60) #:offset 'up #:scale 15 (OverBar $f))
   (:FO (make-pt 30 100)
        (Em "在" (Mi "Sets" #:attr* '((mathvariant "bold"))) "之中:"))
   (:arrow (make-pt 240 150) (make-pt 400 150) #:prop 0.7)
   (:FO (make-pt 240 150) #:offset 'left #:scale 15 (forget (app $M $A)))
   (:FO (make-pt 400 150) #:offset 'left #:scale 3 (forget $N))
   (:arrow (make-pt 240 290) (make-pt 240 150))
   (:FO (make-pt 240 290) #:scale 0 $A)
   (:arrow (make-pt 240 290) (make-pt 400 150))
   (:FO (make-pt 240 220) #:offset 'left #:scale 2 $i)
   (:FO ((lerp 0.6) (make-pt 240 290) (make-pt 400 150)) $f)
   (:FO (make-pt 320 150) #:offset 'up (forget (OverBar $f)))))
(define cat_awodey.html
  (TmPrelude
   #:title "范畴论笔记"
   #:css "styles.css"
   (H1 "范畴论笔记")
   (H2 "第1章 范畴")
   (H3 "第1.1节 引入")
   (P "范畴论在某种意义上可以被视为" (Em "函数的代数学") ".")
   (H3 "第1.2节 集合的函数")
   (P "令" $f "是从集合" $A "到集合" $B "的函数, 记为" (func $f $A $B)
      ". 若用" (&range $f) "表示" $f "的值域, 那么" (&sube (&range $f) $B)
      ". 现在设我们还有一个函数" (func $g $B $C) ", 那么我们可以构造复合"
      (func (&compose $g $f) $A $C) ", 其由"
      (MB (&cm (&= (app (@compose $g $f) $a)
                   (app $g (app $f $a))) (∈ $a $A)))
      "给定. 函数复合" $compose "是结合性的, 即若再有一个函数"
      (func $h $C $D) ", 那么"
      (MB (&assoc &compose $h $g $f) ".")
      "这里的函数相等显然是外延相等 (体现为逐点的函数值相等), 即对于每个" (∈ $a $A) "有"
      (MB (&= (app (@compose (@compose $h $g) $f) $a)
              (app (@compose $h (@compose $g $f)) $a)
              (app $h (app $g (app $f $a)))) ".")
      "对于任意的集合" $A ", 存在一个恒等函数" (func $1_A $A $A) ", 其由"
      (&= (app $1_A $a) $a) "定义. 恒等函数在某种意义上是函数复合" $compose "的单位元, 即"
      (MB (&= (&compose $f $1_A) (&compose $1_B $f) $f) ".")
      "对于函数的概念进行抽象或许提供了定义范畴的动机.")
   (H3 "第1.3节 范畴的定义")
   ((definition #:n "1.1")
    "一个" (Em "范畴") "由以下资料构成:"
    (Ul (Li "对象: " (&cm $A $B $C $..h))
        (Li "箭头: " (&cm $f $g $h $..h))
        (Li "对于每个箭头" $f ", 存在两个(箭头所内蕴的)对象"
            (MB (&dom $f) "和" (&cod $f))
            "其被称为" $f "的定义域 (domain) 和陪域 (codomain). 我们记"
            (MB (func $f $A $B))
            "以指明" (&= $A (&dom $f)) "和" (&= $B (&cod $f)) ".")
        (Li "给定箭头" (func $f $A $B) "和" (func $g $B $C) ", 即"
            (MB (&= (&cod $f) (&dom $g)))
            "存在与之对应的箭头"
            (MB (func (&compose $g $f) $A $C))
            "其被称为" $f "和" $g "的复合.")
        (Li "对于每个对象" $A ", 存在与之对应的箭头"
            (MB (func $1_A $A $A))
            "其被称为" $A "的恒等箭头."))
    "以上这些资料需要满足以下法则."
    (Ul (Li "结合律: 对于所有的"
            (&cm (func $f $A $B) (func $g $B $C) (func $h $C $D)) "有"
            (MB (&assoc &compose $h $g $f) "."))
        (Li "单位元: 对于所有的" (func $f $A $B) "有"
            (MB (&= (&compose $f $1_A) (&compose $1_B $f) $f) "."))))
   (P "范畴的定义是全然抽象的, 对象不必是集合, 箭头不必是函数.")
   (H3 "第1.4节 范畴的例子")
   (Ol (Li ""
           )
       (Li "另外一类在数学中常见的例子是" (Em "带结构集合")
           "的范畴, 即带有结构的集合和保持结构的函数, "
           "这些概念在某种意义上是以相对独立的方式确定的. "
           "读者可能熟悉的例子有"
           (Ul (Li "群和群同态;")
               (Li "向量空间和线性映射;")
               (Li "图和图同态;")
               (Li "实数域" $RR "和连续函数" (&-> $RR $RR) ";")
               (Li "开集" (&sube $U $RR) "和定义于其间的连续函数"
                   (func $f $U (&sube $V $RR)) ";")
               (Li "拓扑空间和连续映射;")
               (Li "可微流形和光滑映射;")
               (Li "自然数集" $NN "和递归函数" (&-> $NN $NN)
                   ", 或者也可以像上面的连续函数的例子一样取定义于子集"
                   (&sube $U $NN) "上的部分递归函数;")
               (Li "偏序集和单调函数."))
           "如果你不熟悉其中某些例子, 也不要紧张. 之后我们将更仔细地"
           "检视其中一些. 暂时, 让我们讨论一下以上例子中的最后一个.")
       (Li "偏序集即装备了偏序关系的集合, 偏序集之间的箭头即单调映射. 如果"
           (MB (func $m $A $B))
           "是一个从偏序集" $A "到偏序集" $B "之间的单调映射, 那么"
           (MB (&cm (&=> (: $a (_ $<= $A) $a^)
                         (: (app $m $a) (_ $<= $B) (app $m $a^)))
                    (∈ $a $a^ $A)) ".")
           "那么, 偏序集和单调映射何以成为范畴呢? 我们需要知道"
           (func $1_A $A $A) "是单调的, 但这是显然的, 因为"
           (&=> (: $a (_ $<= $A) $a^) (: $a (_ $<= $A) $a^))
           ". 我们也需要知道如果" (func $f $A $B) "和"
           (func $g $B $C) "是单调的, 那么" (func (&compose $g $f) $A $C)
           "是单调的. 这当然也是成立的, 因为"
           (MB (&=> (&<= $a $a^)
                    (&<= (app $f $a) (app $f $a^))
                    (&<= (app $g (app $f $a)) (app $g (app $f $a^)))
                    (&<= (app (@compose $g $f) $a)
                         (app (@compose $g $f) $a^))) ".")
           "因此, 我们有由偏序集和单调函数构成的范畴" Cat:Pos ".")
       (Li "到目前为止我们已经考虑了的范畴是所谓" (Em "具体范畴")
           "的例子. 不严格地说, 这些范畴的对象是可能装备有某种结构的集合, "
           "而箭头是特定的函数, 比如说保持结构的函数. 我们将在之后看到"
           "这个概念并非全然一致的, 见评注1.7. 但是, 理解范畴论的一种"
           "方式在于&quot;doing without elements&quot;, 将函数"
           "替换成箭头. 让我们现在来看看一些例子以明白这种观念并非可选, "
           "而是基础性的." (Br)
           "令" Cat:Rel "是以下范畴: 取集合为对象, 而取二元关系为箭头. "
           "也就是说, 一个箭头" (func $f $A $B) "是一个任意的子集"
           (&sube $f (&c* $A $B)) ". 集合" $A "上的恒等箭头为恒等关系, 即"
           (MB (&= $1_A (&sube (setI (∈ (tu0 $a $a) (&c* $A $A))
                                     (∈ $a $A))
                               (&c* $A $A))) ".")
           "给定" (&sube $R (&c* $A $B)) "和" (&sube $S (&c* $B $C))
           ", 我们定义复合" (&compose $S $R) "为"
           (MB (∈ (tu0 $a $c) (&compose $S $R)) "当且仅当"
               (&exist $b (&and (∈ (tu0 $a $b) $R)
                                (∈ (tu0 $b $c) $S))) ".")
           "此即" $S "和" $R "的&quot;relative product&quot;. "
           "我们将验证" Cat:Rel "在事实上是一个范畴的工作留给读者. "
           "(要做什么?)" (Br)
           "现在我们要举另一个箭头并非函数的范畴的例子, 令对象是有限集合"
           (&cm $A $B $C) "而箭头" (func $F $A $B) "是由自然数构成的矩阵"
           (&= $F (_ (@ (_ $n (&cm $i $j))) (&cm (&< $i $a) (&< $j $b))))
           ", 其中" (&= $a (&abs $A)) "且" (&= $b (&abs $B))
           ", 这个记号的含义是集合的元素个数, 也就是基数或者说势. "
           "箭头的复合即通常的矩阵乘法, 恒等箭头即通常的单位矩阵. "
           "对象在这里的作用仅仅是为了确保矩阵乘法有定义, 但是矩阵并非"
           "对象之间的函数.")
       (Li (Em "有限范畴") (Br)
           "当然, 范畴的对象不必是集合, 以下是一些非常简单的例子:"
           
           )
       )
   ((definition #:n "1.2")
    "一个范畴" $C:bold "和" $D:bold "之间的" (Em "函子")
    (MB (func $F $C:bold $D:bold))
    "是从对象到对象和从箭头到箭头的映射, 其满足"
    (Ol #:attr* '((type "a"))
        (Li (&= (app $F (func $f $A $B))
                (func (app $F $f) (app $F $A) (app $F $B))) ";")
        (Li (&= (app $F $1_A) (_ $1 (app $F $A))) ";")
        (Li (&= (app $F (&compose $g $f))
                (&compose (app $F $g) (app $F $f))) ".")))
   (Ol #:attr* '((start "7"))
       (Li "一个" (Em "预序") "是一个装备有满足自反性和传递性的二元关系的集合. 若将"
           "集合的元素视为对象, 而两个对象之间存在唯一的箭头当且仅当其满足二元关系, "
           "则预序可以被视为范畴.")
       (Li ""
           )
       (Li "拓扑学一例: 令" $X "是一个拓扑空间, 其开集族为" (app $O:script $X)
           ". 根据包含关系进行排序, " (app $O:script $X)
           "就成了一个poset category. 而且, " $X "上我们也可以通过"
           (Em "specialization") "定义一个预序, 即将关系" (&<= $x $y)
           "定义为对于每个开集" (∈ $U (app $O:script $X)) ", "
           (∈ $x $U) "可以推出" (∈ $y $U) ". 若" $X "满足" $T_1
           "公理, 那么这个预序只会是平凡的. 但是如果不是, 这个预序可能会是"
           "相当有趣的, 代数几何和指称语义中都有这样的空间的例子. "
           "我们将对于以下事实的证明留作练习, " $T_0
           "空间在specilization序下实际上是偏序集. "
           "[译注: 使用反证法.]")
       (Li "逻辑学一例: 给定一个逻辑演绎系统, 存在一个与之相关的"
           (Em "证明的范畴") ", 其对象是公式"
           (MB (&cm $phi $psi $..h))
           "从" $phi "到" $psi "的箭头是从(uncanceled)假设" $phi
           "开始的推出" $psi "的一个演绎."
           (MB (&rule* $phi $..v $psi))
           "箭头的复合不过就是将演绎以显然的方式放在一起, "
           "因而当然是结合的. 我们应该看出从" $phi "到" $psi
           "的箭头可以有很多个, 因为证明可以有很多个. "
           "这种范畴实际上具有丰富的结构, 之后我们将和" $lambda
           "演算一起讨论它.")
       (Li "计算机科学一例: 给定一个函数式编程语言" $L
           ", 存在一个与之相关的范畴, 其对象是" $L
           "的数据类型, 而箭头是" $L "的可计算函数. 两个程序"
           (Arrow $X $f $Y $g $Z) "的复合显然是将" $g
           "应用于" $f "的输出, 还有一种写法是"
           (MB (&= (&compose $g $f) (&\; $f $g)) ".")
           "恒等箭头当然是&quot;什么也不做&quot;的程序." (Br)
           "这种范畴对于编程语言的指称语义的想法而言是基本的. 例如, 如果"
           (app $C:bold $L) "是我们刚才定义的范畴, 那么以Scott domain的范畴"
           $D:bold "作为解释的" $L "的指称语义实际上不过就是一个函子"
           (MB (func $S (app $C:bold $L) $D:bold))
           "因为" $S "赋予" $L "的类型以domain, " $L
           "的程序以domain之间的连续函数. 这个例子和前一个例子"
           "都和所谓的笛卡尔闭范畴 (CCC) 有关, 之后我们将讨论CCC.")
       (Li "令" $X "是一个集合, 那么我们可以将" $X "当作一个范畴"
           (app (Mi "Dis" #:attr* '((mathvariant "bold"))) $X)
           ", 其对象是" $X "的元素而箭头就只有必要的恒等箭头. "
           "这样的范畴被称为离散范畴, 实际上我们应该注意到"
           "离散范畴不过就是非常特殊的偏序集.")
       (Li "一个幺半群 (monoid, 偶尔会说semigroup with unit) 是一个集合"
           $M "装备了一个二元运算" (func $d* (&c* $M $M) $M)
           ", 并且这个运算是结合的, 即对于" (∈ $x $y $z $M) ", 我们有"
           (MB (&= (&d* $x (@d* $y $z))
                   (&d* (@d* $x $y) $z)))
           "另外, 这个运算还有一个幺元, 即存在" (∈ $u $M)
           "使得对于每个" (∈ $x $M) "都有"
           (MB (&= (&d* $u $x) (&d* $x $u) $x))
           "等价地说, 其实一个幺半群是一个仅有一个对象的范畴. 这个范畴的"
           "箭头是幺半群的元素. 恒等箭头即幺元" $u ". 箭头的复合不过就是"
           "幺半群的二元运算而已." (Br)
           "幺半群太过常见了. 例如, " (&cm $NN $QQ $RR) "相对于加法是幺半群, "
           "其幺元 (可能用加法的单位元更好) 是" $0
           ", 相对于乘法也是, 其幺元是" $1 ". 另外, 对于任意的集合" $X
           ", 从" $X "到" $X "的所有函数构成的集合, 即"
           (MB (Hom Cat:Sets $X $X))
           "在函数复合下也是一个幺半群. 更一般地, 对于任意的范畴" Cat:C
           "中的任意的对象" $C ", 从" $C "到" $C "的箭头的集合"
           (Hom Cat:C $C $C) "在范畴" Cat:C "的箭头复合运算下是一个幺半群." (Br)
           "既然幺半群也是所谓的带结构的集合, 那么存在这样一个范畴" Cat:Mon
           ", 其对象是幺半群, 而箭头是保持幺半群结构的函数. 更细致地说, 从幺半群"
           $M "到幺半群" $N "的一个同态是一个函数" (func $h $M $N)
           "满足对于任意的" (∈ $m $n $M) ", 有"
           (MB (&= (app $h $m (_ $d* $M) $n)
                   (: (app $h $m) (_ $d* $N) (app $h $n))))
           "并且"
           (MB (&= (app $h $u_M) $u_N))
           "我们应该观察到, 从" $M "到" $N "的同态可以被视为函子, 若是将"
           $M "和" $N "视为范畴. 在这种意义下, 范畴是一般化了的幺半群, "
           "函子是一般化了的同态."))
   (H3 "第1.5节 同构")
   ((definition #:n "1.3")
    "在任意的集合" $C:bold "中, 称箭头" (func $f $A $B) "为" (Em "同构")
    ", 如果存在箭头" (func $g $B $A) "满足"
    (MB (&= (&compose $g $f) $1_A) "且" (&= (&compose $f $g) $1_B) ".")
    "这样的逆显然是唯一的, 我们记" (&= $g (inv $f)) ". 我们称" $A
    "同构于" $B ", 如果其间存在同构, 此时记" (&iso $A $B) ".")
   ((definition #:n "1.4")
    "一个群" $G "是一个幺半群, 并且每个元素" $g "都有一个逆元"
    (inv $g) ". 换言之, " $G "是一个只有一个对象的范畴, "
    "并且其每个箭头都是同构. [译注: 箭头都是同构的范畴被称为群胚.]")
   (P $NN "在加法下并非一个群, " $ZZ "在加法下的确是一个群, 正有理数集合"
      (^ $QQ $+) "在乘法下是一个群. 对于任意的集合" $X ", " $X
      "上的所有自同构, 或者说置换, 构成了群" (Aut $X)
      ". 所谓的" (Em "置换群") "是子群" (&sube $G (Aut $X))
      ". 因此, " $G "必须满足以下性质:"
      (Ol (Li "恒等函数" $1_X "在" $G "之中.")
          (Li "如果" (∈ $g $g^ $G) ", 那么"
              (∈ (&compose $g $g^) $G) ".")
          (Li "如果" (∈ $g $G) ", 那么" (∈ (inv $g) $G) ".")))
   (P "两个群之间的同态" (func $h $G $H) "实际上是幺半群的同态, "
      "而且其也必然保持逆元运算.")
   (P "以下是关于抽象群的基本的经典的结果.")
   ((theorem #:n ". Cayley") "每个群都同构于一个置换群.")
   ((proof)
    (Ol (Li "首先, 对于群" $G ", 定义其Cayley表示" (OverBar $G)
            "为以下的集合" $G "的一个置换群: 对于每个" (∈ $g $G)
            ", 我们有置换" (func (OverBar $g) $G $G)
            ", 其定义为"
            (MB (&\|-> $h (&d* $g $h)) ".")
            "这的确是一个置换, 因为" (OverBar (inv $g)) "是其逆.")
        (Li "接着, 定义同态" (func $i $G (OverBar $G)) "为"
            (&\|-> $g (OverBar $g)) ", " (func $j (OverBar $G) $G)
            "为" (&\|-> (OverBar $g) (app (OverBar $g) $u_G)) ".")
        (Li "最后, 证明" (&= (&compose $i $j) (_ $1 (OverBar $G)))
            "和" (&= (&compose $j $i) $1_G) ".")))
   (P "Cayley定理是说任何的抽象群都可以由一个&quot;具体&quot;的"
      "置换群表示. 这个定理可以被推广为, 任何&quot;不太大&quot;"
      "的范畴都可以被表示为一个&quot;具体&quot;的范畴, 即一个由"
      "集合和函数构成的范畴. (第1.8节讨论了一些关于基础的技术细节.)")
   ((theorem #:n "1.6")
    "每个范畴" Cat:C "都同构于这样的一个范畴, 其对象是集合, 其箭头是函数.")
   ((proof)
    "定义" Cat:C "的Cayley表示" (OverBar Cat:C) "为以下具体范畴:"
    (Ul (Li "对象是具有形式"
            (MB (&= (OverBar $C)
                    (setI (∈ $f Cat:C)
                          (&= (&cod $f) $C))))
            "的集合, 其中" (∈ $C Cat:C) ".")
        (Li "对于" Cat:C "中的箭头" (func $g $C $D)
            ", 我们有一个" (OverBar Cat:C) "中的箭头"
            (MB (func (OverBar $g) (OverBar $C) (OverBar $D)))
            "对于" (OverBar $C) "中的每个箭头" (func $f $X $C)
            ", 我们定义"
            (MB (&= (app (OverBar $g) $f)
                    (&compose $g $f)) "."))))
   (P "这向我们表明了集合和函数的&quot;具体&quot;范畴的朴素概念有什么"
      (Em "错误") ": 尽管不是每个范畴都以集合为对象而函数为箭头, "
      "但是每个范畴都同构于一个这样的具体范畴. 因此, 这样的范畴"
      "可能具有的什么特殊性质是和范畴论无关的, 例如不以任何方式"
      "影响到箭头的对象的那些性质 (就像通过Dedekind分割或者"
      "Cauchy序列构造的实数之间的区别). 更好的捕获极其模糊的"
      "&quot;具体&quot;范畴的想法的尝试是这样的, 任意的箭头"
      (func $f $C $D) "都完全由其和箭头们" (func $x $T $C)
      "的复合确定, 这里的" $T "是某种&quot;测试对象&quot;. "
      "这句话的意思是若对于所有这样的" $x "都有"
      (&= (&i* $f $x) (&i* $g $x)) ", 那么" (&= $f $g)
      ". 之后我们将看到, 这相当于考虑由" $T "确定的范畴的"
      "一个具体表示. "
      )
   (H3 "第1.6节 范畴上的构造")
   (P "现在我们有了一些能与之打交道的范畴, 所以我们可以开始"
      "考虑一些从旧的范畴产生新的范畴的构造.")
   (Ol (Li "两个范畴" Cat:C "和" Cat:D "的" (Em "积") ", 记作"
           (MB (&c* Cat:C Cat:D))
           "其对象具有形式" (tu0 $C $D) ", 其中" (∈ $C Cat:C)
           "而" (∈ $D Cat:D) ", 并且箭头具有形式"
           (MB (func (tu0 $f $g) (tu0 $C $D) (tu0 $C^ $D^)))
           "其中" (∈ (func $f $C $C^) Cat:C) "而"
           (∈ (func $g $D $D^) Cat:D) ". 复合是按分量定义的, 即"
           (MB (&= (&compose (tu0 $f^ $g^) (tu0 $f $g))
                   (tu0 (&compose $f^ $f)
                        (&compose $g^ $g))))
           "恒等箭头显然是"
           (MB (&= (_ $1 (tu0 $C $D)) (tu0 $1_C $1_D)))
           "存在两个显然的" (Em "投影函子")
           (MB Cat:C (^^ $<- $pi_1) (&c* Cat:C Cat:D)
               (^^ $-> $pi_2) Cat:D)
           "其定义为" (&= (ap $pi_1 (tu0 $C $D)) $C)
           "和" (&= (ap $pi_1 (tu0 $f $g)) $f)
           ", " $pi_2 "的定义是类似的." (Br)
           "若是读者熟悉群, 那么对于群" $G "和" $H
           ", 若将它们当成范畴, 则其积范畴" (&c* $G $H)
           "不过就是通常的群的直积.")
       (Li "一个范畴" Cat:C "的" (Em "反")
           "范畴, 或者说对偶范畴, 其对象和" Cat:C
           "无异, 但是" (&op Cat:C) "中的箭头"
           (func $f $C $D) "是" Cat:C "中的箭头"
           (func $f $D $C) ". 换言之, " (&op Cat:C)
           "其实就是箭头调转方向的" Cat:C "." (Br)
           "最好能有记号让我们区分" $C "和" (&op Cat:C)
           "中的对象和箭头. 因此, 让我们对于" Cat:C
           "中的" (func $f $C $D) "记"
           (MB (func (&* $f) (&* $D) (&* $C)))
           "这是" (&op Cat:C)
           "中的相应箭头. 以此记号, 我们可以基于"
           Cat:C "中的相应操作来定义" (&op Cat:C)
           "中的复合和单位元, 即"
           (MB (&cm (&= (_ $1 (&* $C)) (&* (@ $1_C)))
                    (&= (&compose (&* $f) (&* $g))
                        (&* (@compose $g $f)))))
           "数学的许多&quot;对偶&quot;定理实际上不过表达了这样的事实, "
           "一个范畴是另一个范畴的反范畴 (或者是这个反范畴的一个子范畴). "
           "一个这样的例子是我们将在之后证明的" Cat:Sets
           "对偶于完备原子布尔代数的范畴.")
       (Li "一个范畴" Cat:C "的" (Em "箭头范畴") (Cat:Arrow Cat:C)
           "以" Cat:C "的箭头为对象, 并且" (Cat:Arrow Cat:C)
           "中从对象" (func $f $A $B) "到" (func $f^ $A^ $B^)
           "的一个箭头" $g "是一个&quot;交换正方形&quot;"
           (Svg
            #:attr* '((width "160")
                      (height "160")
                      (stroke "black")
                      (style "display: block; margin: auto;"))
            (Defs marker0)
            (:FO (make-pt 30 30) $A)
            (:FO (make-pt 100 26) $A^)
            (:FO (make-pt 30 100) $B)
            (:FO (make-pt 100 96) $B^)
            (:arrow (make-pt 30 44) (make-pt 100 44) #:prop 0.6)
            (:arrow (make-pt 30 114) (make-pt 100 114) #:prop 0.6)
            (:arrow (make-pt 30 44) (make-pt 30 114) #:prop 0.6)
            (:arrow (make-pt 100 44) (make-pt 100 114) #:prop 0.6)
            (:FO (make-pt 20 65) $f)
            (:FO (make-pt 110 61) $f^)
            (:FO (make-pt 65 16) $g_1)
            (:FO (make-pt 65 110) $g_2))
           "其中" $g_1 "和" $g_2 "是" Cat:C
           "中的箭头. 换句话说, 这样的一个箭头" $g "是" Cat:C
           "中的箭头序对" (tu0 $g_1 $g_2) "并且满足"
           (MB (&= (&compose $g_2 $f)
                   (&compose $f^ $g_1)) ".")
           "一个对象" (func $f $A $B) "上的恒等箭头是序对"
           (tu0 $1_A $1_B) ". 箭头的复合是按分量计算的:"
           (MB (&= (&compose (tu0 $h_1 $h_2)
                             (tu0 $g_1 $g_2))
                   (tu0 (&compose $h_1 $g_1)
                        (&compose $h_2 $g_2))))
           "读者应该画出合适的交换图以验证这个定义的确合理." (Br)
           "观察到有两个函子:"
           (MB Cat:C (^^ $<- $dom) (Cat:Arrow Cat:C)
               (^^ $-> $cod) Cat:C))
       (Li "一个范畴" Cat:C "在对象" (∈ $C Cat:C)
           "上的" (Em "切片范畴") "如下"
           (Ul (Li "对象: 所有满足" (&= (&cod $f) $C)
                   "的箭头" (∈ $f Cat:C))
               (Li "箭头: 从对象" (func $f $X $C)
                   "到对象" (func $f $X^ $C) "的一个箭头"
                   $a "是一个" Cat:C "中的箭头"
                   (func $a $X $X^) "使得"
                   (&= (&compose $f^ $a) $f)
                   ", 如以下交换图所示:"
                   (Svg
                    #:attr* '((width "160")
                              (height "160")
                              (stroke "black")
                              (style "display: block; margin: auto;"))
                    (Defs marker0)
                    (:FO (make-pt 30 30) $X)
                    (:FO (make-pt 100 26) $X^)
                    (:FO (make-pt 65 90) $C)
                    (:arrow (make-pt 30 42) (make-pt 100 42) #:prop 0.6)
                    (:arrow (make-pt 30 42) (make-pt 65 102)  #:prop 0.6)
                    (:arrow (make-pt 65 102) (make-pt 100 42)  #:prop 0.6)
                    (:FO (make-pt 40 60) $f)
                    (:FO (make-pt 90 56) $f^)
                    (:FO (make-pt 65 18) $a))
                   "恒等箭头和箭头复合都继承自范畴" Cat:C
                   ", 这和之前的箭头范畴的情况差不多. "
                   )
               )
           )
       )
   (H3 "第1.7节 自由范畴")
   (P (B "自由幺半群. ") "我们从字母表" $A "开始. 一个" $A
      "上的词是来自于" $A "的字母构成的有限序列. " $A
      "的Kleene闭包" (&* $A) "被定义为所有" $A "上的词构成的集合. "
      (&* $A) "上我们可以定义所谓的连接运算" $*
      ". 这个运算显然是结合的, 并且长度为零的空序列是其单位元. 因此, "
      (&* $A) "形成了一个幺半群, 其被称为集合" $A
      "上的" (Em "自由幺半群") ". 元素" (∈ $a $A)
      "可以被视为长度为一的词, 即我们有一个函数"
      (MB (&: $i (&cm (&-> $A (&* $A)) (&\|-> $a $a))))
      "虽然说以上的定义算是一种符号滥用 (abuse of notation) 吧. "
      $A "的元素在某种意义上&quot;生成&quot;了这个自由幺半群, 即每个"
      (∈ $w (&* $A)) "都是" $A "的元素的" $* "积. 也就是说, 对于某些"
      (∈ $a_1 $a_2 $..h $a_n $A) "有"
      (&= $w (&* $a_1 $a_2 $..c $a_n)) ".")
   (P "到底何谓&quot;自由&quot;呢? 我们称一个幺半群" $M
      "是由其子集" $A  (Em "自由生成") "的, 如果"
      (Ol (Li "每个" (∈ $m $M) "都可以被写成" $A "的元素之积:"
              (MB (&cm (&= $m (&d*M $a_1 $..c $a_n))
                       (∈ $a_i $A)) "."))
          (Li $M "中不存在&quot;非平凡&quot;的关系, 即如果"
              (&= (&i* $a_1 $..h $a_j)
                  (&i* (_^ $a $1 $prime) $..h (_^ $a $k $prime)))
              ", 那么这是幺半群的公理所要求的."))
      "第一个条件有时被称为&quot;没有垃圾&quot;, 第二个条件有时被称为"
      "&quot;没有噪音&quot;. 因此, " $A "上的自由幺半群是包含" $A
      "的没有垃圾也没有噪音的幺半群.")
   (P "每个幺半群" $N "都拥有一个作为基础的集合" (forget $N)
      ", 而每个幺半群同态" (func $f $N $M) "都可以导出一个集合之间的函数"
      (func (forget $f) (forget $N) (forget $M)) ". 很容易看出来这是一个函子, "
      "即所谓的&quot;遗忘函子&quot;. 集合" $A "上的自由幺半群" (app $M $A)
      "是满足以下" (Em "泛性质") "的&quot;唯一&quot;的那个幺半群. "
      "(本书里将泛性质 (universal property) 称为泛映射性质 "
      "(universal mapping property, UMP), 这两者是一回事.)")
   (P "存在函数" (func $i $A (forget (app $M $A)))
      ", 对于任意的幺半群" $N "和函数" (func $f $A (forget $N))
      ", 有唯一的幺半群同态" (func (OverBar $f) (app $M $A) $N)
      "满足" (&= (&compose (forget (OverBar $f)) $i) $f)
      ", 这可以画成以下交换图表:"
      UMP:free-monoid.svg)
   ((proposition #:n "1.9")
    (&* $A) "具有" $A "上的自由幺半群的泛性质.")
   ((proof)
    "对于函数" (func $f $A (forget $N)) ", 我们通过"
    (MB (&= (app (OverBar $f) str:empty) $u_N))
    "其中" str:empty "是空字符串而" $u_N "是幺半群" $N "的单位元, 以及"
    (MB (&= (app (OverBar $f) (&i* $a_1 $..h $a_i))
            (&d*N (app $f $a_1) $..c (app $f $a_i))))
    "来定义" (func (OverBar $f) (&* $A) $N)
    ", 其显然是一个同态, 并且满足对于每个" (∈ $a $A) "有"
    (MB (&= (app (OverBar $f) $a) (app $f $a)) ".")
    "这仍然是符号滥用, 更准确地说, 左边的" $a "实际上是" (app $i $a)
    ". 如果同态" (func $g (&* $A) $N) "也满足对于每个" (∈ $a $A)
    "有" (&= (app $g $a) (app $f $a)) ", 那么对于所有"
    (∈ (&i* $a_1 $..h $a_i) (&* $A)) ":"
    (MB (deriv (app $g (&i* $a_1 $..h $a_i))
               (app $g (&* $a_1 $..c $a_i))
               (&d*N (app $g $a_1) $..c (app $g $a_i))
               (&d*N (app $f $a_1) $..c (app $f $a_i))
               (&d*N (app (OverBar $f) $a_1) $..c (app (OverBar $f) $a_i))
               (app (OverBar $f) (&* $a_1 $..c $a_i))
               (app (OverBar $f) (&i* $a_1 $..h $a_i))))
    "于是, " (&= $g (OverBar $f)) ", 证明就结束了. 怎么说呢, " (&* $A)
    "是自由幺半群的玄机藏在似乎平凡的事实"
    (&= (&* $a_1 $..c $a_i) (&i* $a_1 $..h $a_i))
    "之中. " (&i* $a_1 $..h $a_i)
    "在某种意义上其实是一种双关, 它暗示了只有唯一一种将其表示为乘积"
    (&* $a_1 $..c $a_i) "的方式.")
   (P "现在我们回头重新以泛性质来检视旧有的定义, 或者说思考泛性质是怎样捕获"
      "naive定义的想法的. 这个泛性质的存在性部分捕获了&quot;没有噪音&quot;"
      "的概念, 因为任意的生成元的代数组合之间的等式也对于其所映射至的东西成立, "
      "也就是所有地方都成立. 唯一性部分捕获了&quot;没有垃圾&quot;的想法, 因为任何不是"
      "生成元的组合的元素, 其所映射至的东西可以是任意的值.")
   (P "使用泛性质, 很容易表明自由幺半群" (app $M $A) "在同构意义下是唯一的.")
   
   (H3 "第1.8节 基础问题: 大, 小, 局部小")
   (P "让我们首先区分以下两种东西:"
      (Ol #:attr* '((type "i"))
          (Li "数学的范畴论基础;")
          (Li "范畴论的数学基础."))
      "对于第一点而言, 人们有时会听说范畴论可以用来提供&quot;数学的基础&quot;, "
      "作为集合论的替代物. 实际上的确如此, 但是这不是我们这里要做的事情. "
      "在集合论中, 人们经常从存在性公理开始, 例如&quot;存在一个无穷集合&quot;, "
      "然后通过某些公理来导出更多的集合, 例如&quot;每个集合都有一个幂集&quot;, "
      "由此人们构筑了一个数学对象 (即集合) 的宇宙, 从原则上说它对于"
      "&quot;所有的数学&quot;而言应该是足够了. 我们的公理&quot;每个箭头都有一个"
      "domain和一个codomain&quot;不应该和集合论公理&quot;每个集合都有一个幂集"
      "&quot;以相同的方式理解! 区别在于, 在集合论中 (至少是一般意义上的集合论), "
      "这些公理被认为是指 (或者说确定) 一个单一的由集合构成的宇宙. 而在范畴论中, "
      "与之相对的是, 这些公理是某种东西的" (Em "定义")
      ", 即范畴的定义. 这就像群论或者拓扑学, 其公理是为了定义"
      "需要检视的对象的, 而这些对象又被认为是存在于某种&quot;背景&quot;或者"
      "&quot;基础&quot;系统之中, 例如集合论 (或者类型论). "
      "而集合论本身又可能使用范畴论确定, 或者以其他某种方式.")
   (P "这将我们带至第二点: 我们假定我们的范畴是由集合和函数构成的, "
      "以这样或那样的方式, 就和其他绝大多数数学对象一样, "
      "然后开始考虑范畴论 (或者其他什么理论) 作为基础的可能性. "
      "但是, 在范畴论中, 我们经常在通常的实践中遇到集合论的困难. "
      "大多数这些问题是和&quot;大小&quot;有关的; 一些范畴&quot;太大&quot;"
      "以至于我们没法按照寻常集合论的方式妥当地进行处理. 在第1.5节里"
      "考虑Cayley表示时, 我们就已经遇到了这种问题. 那里我们要求考虑的范畴"
      ""
      )
   (H3 "第1.9节 练习")
   
   (H2 "第2章 抽象结构")
   (H3 "第2.1节 满态射和单态射")
   (P "给定函数" (func $f $A $B) ", 其被称为单射的, 如果对于所有的"
      (∈ $a $a^ $A) ", " (&= (app $f $a) (app $f $a^)) "可以推出"
      (&= $a $a^) "; 其被称为满射的, 如果对于每个" (∈ $b $B)
      ", 存在" (∈ $a $A) "使得" (&= (app $f $a) $b) ".")
   ((definition #:n "2.1")
    "在任意的范畴" $C:bold "中, 给定箭头" (func $f $A $B)
    ", 其被称为是一个" (Em "单态射(monomorphism)") ", 如果对于任意的"
    (func (&cm $g $h) $C $A) ", " (&= (&i* $f $g) (&i* $f $h))
    "可以推出" (&= $g $h) "; 其被称为是一个" (Em "满态射(epimorphism)")
    ", 如果对于任意的" (func (&cm $i $j) $B $D) ", "
    (&= (&i* $i $f) (&i* $j $f)) "可以推出" (&= $i $j) ".")
   (P "如果" $f "是一个单态射, 那么我们记" (&: $f (&>-> $A $B))
      ". 如果" $f "是一个满态射, 那么我们记" (&: $f (&->> $A $B)) ".")
   ((proposition)
    "一个集合之间的函数" (func $f $A $B) "是单态射恰当其为单射.")
   ((proof)
    "设" (&: $f (&>-> $A $B)) ". 令" (∈ $a $a^ $A) "满足"
    (&!= $a $a^) ", 并令" (setE $x) "是任意的一个单元素集. "
    )
   ((example #:n "2.3")
    "在许多类似于幺半群的" (Q "结构化集合") "的范畴中, 单态射就恰为"
    (Q "单射的同态") ". 更精确地说, 一个幺半群的同态" (func $h $M $N)
    "是单态射恰当其基础函数" (fforget $h $M $N) "是单态射, "
    "即前文的单射. 为了证明这点, 设" $h "是一个单态射并取两个不同的"
    (Q "元素") (func (&cm $x $y) $1 (forget $M))
    ", 其中" (&= $1 (setE (set-compact $*))) "是任意的单元素集合. "
    
    )
   (H3 "第2.2节 始对象和终对象")
   (P "现在我们考虑对于范畴" Cat:Sets "中的空集和单元素集的抽象刻画, "
      "而这可以推广至一般范畴中从结构上考虑类似的对象.")
   ((definition #:n "2.9")
    "在任何范畴" Cat:C "中, 对象" $0 "被称为始对象, 如果对于任意的对象"
    (∈ $C Cat:C) ", 存在唯一的态射" (&-> $0 $C) "; 对象" $1
    "被称为终对象, 如果对于任意的对象" (∈ $C Cat:C) ", 存在唯一的态射"
    (&-> $C $1) ".")
   (P "就像单态射和满态射一样, 我们应该注意到这定义中存在某种"
      "&quot;对偶性&quot;. 精确地说, " Cat:C "中的终对象恰是"
      (&op Cat:C) "中的始对象. 我们在第3章中系统地考虑了对偶性.")
   (P "首先, 我们应该注意到始对象和终对象的概念显然是泛性质, "
      "这样的对象在同构意义下是唯一的, 就和自由幺半群一样.")
   ((proposition #:n "2.10")
    "始对象在同构下是唯一的, 终对象也是.")
   
   (H2 "第3章 对偶")
   
   (H3 "第3.1节 对偶原理")
   
   (H3 "第3.2节 余积")
   
   (H3 "第3.3节 等化子")
   ((definition #:n "3.13")
    "在任何范畴" Cat:C "中, 给定平行的箭头"
    (MB $A (__^^ $->-> $g $f) $B)
    $f "和" $g "的一个" (Em "等化子") "由一个对象" $E
    "和一个箭头" (func $e $E $A) "构成, universal such that"
    (MB (&= (&compose $f $e) (&compose $g $e)) ".")
    "此即是说, 给定任意的" (func $z $Z $A) "满足"
    (MB (&= (&compose $f $z) (&compose $g $z)))
    "存在" (Em "唯一") "的" (func $u $Z $E) "使得"
    (&= (&compose $e $u) $z) ", 如图所示则为"
    UMP:equalizer.svg)
   (P "让我们考虑一些简单的例子.")
   ((example #:n "3.14")
    "设我们有函数" (&: (&cm $f $g) (&->-> $RR^2 $RR)) ", 其中"
    (eqn*
     ((appl $f $x $y) $= (&+ $x^2 $y^2))
     ((appl $g $x $y) $= $1))
    "我们取等化子, 比如说在" Cat:Top "范畴中. 这是子空间"
    (MB (&= $S
            (&@-> (setI (∈ (tu0 $x $y) $RR^2)
                        (&= (&+ $x^2 $y^2) $1))
                  $RR^2)))
    "即平面中的单位圆. 这是因为, 给定任意的" (Q "泛化元素")
    (func $z $Z $RR^2) ", 通过与两个投影进行复合, "
    "我们就得到了一对这样的" (Q "元素")
    (func (&cm $z_1 $z_2) $Z $RR) ", "
    (&= $z (tupa0 $z_1 $z_2)) ", 对于这些我们然后有"
    (MB (deriv0 (&= (app $f $z) (app $g $z))
                "iff"
                (&= (&+ (_^ $z $1 $2) (_^ $z $2 $2)) $1)
                "iff"
                (: "&quot;"
                   (&= (tupa0 $z_1 $z_2)
                       (∈ $z $S))
                   "&quot;")))
    "其中最后一行实际上意味着" $z "可以通过嵌入"
    (&: $i (&@-> $S $RR^2)) "分解为"
    (&= $z (&compose $i (OverBar $z)))
    " [译注: 这里原文似乎有笔误, 将顺序颠倒了, 但未见于勘误], "
    "如以下交换图表所示 [译注: 这里从" $S "到"
    $RR^2 "实际上应该是嵌入箭头, 但是译者嫌麻烦, 还没有画]:"
    (let ((E (make-pt 30 30))
          (Z (make-pt 30 130))
          (A (make-pt 160 30))
          (B (make-pt 290 30)))
      (Svg
       #:attr* '((width "320")
                 (height "160")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       (:FO E $S)
       (:FO Z $Z)
       (:FO A $RR^2)
       (:FO B $RR)
       (:arrow A B #:offset (make-vec 0 7))
       (:arrow A B #:offset (make-vec 0 17))
       (:arrow E A #:offset (make-vec 0 12))
       (:arrow Z A #:offset (make-vec 0 12))
       (:FO (vec+ ((lerp 1/2) A B) (make-vec -23 -22)) (&+ $x^2 $y^2))
       (:FO (vec+ ((lerp 1/2) A B) (make-vec 0 13)) $1)
       (:FO (vec+ ((lerp 1/2) E A) (make-vec 0 -17)) $i)
       (:FO (vec+ ((lerp 1/2) Z A) (make-vec 10 10)) $z)
       (set-attr*
        (:arrow Z E #:offset (make-vec 0 12) #:prop 0.7)
        'stroke-dasharray "2 2")
       (:FO (vec+ ((lerp 1/2) E Z) (make-vec -7 -5)) (OverBar $z))
       ))
    "既然嵌入" $i "是单态射, 这样的分解在存在的情况下必然是唯一的, 因而"
    (&@-> $S $RR^2) "的确是" $f "和" $g "的等化子.")
   ((example #:n "3.15")
    "类似地, 在" Cat:Sets "中, 给定任意的函数" (&: (&cm $f $g) (&->-> $A $B))
    ", 其等化子是由等式定义的子集"
    (MB (&@-> (setI (∈ $x $A) (&= (app $f $x) (app $g $x))) $A))
    "到" $A "的嵌入. 其论证基本上和刚才给出的是一模一样的.")
   (P "现在让我们暂停一下, 观察到一个基本事实. 实际上, 每个子集都具有这种"
      (Q "等式性") "的形式, 即每个子集都是某对函数的等化子. 的确, "
      "我们可以用非常canonical的方式构造出来. 首先, 让我们置"
      (MB (&= $2 (setE $top $bottom)))
      "将其想成是" (Q "真值") "的集合, 然后考虑" (Em "特征函数")
      (MB (&= (app $chi_U $x)
              (Choice0
               ($top ",&nbsp;" (∈ $x $U))
               ($bottom ",&nbsp;" (&!in $x $U)))))
      "因此, 我们有"
      (MB (&= $U (setI (∈ $x $A)
                       (&= (app $chi_U $x) $top))))
      "于是, 以下是一个等化子:"
      (MB $U $-> $A
          (__^^ $->-> $chi_U (&i* $top $!))
          $2)
      "其中"
      (&= (&i* $top $!)
          (&: (&compose $top $!)
              (Func $A $! $1 $top $2)))
      ". [译注: 原文是" (Func $U $! $1 $top $2)
      ", 但是译者认为这是作者的笔误.]")
   (P "而且, 对于每个函数"
      (MB (func $phi $A $2))
      "我们可以构造" (Q "variety") "(即等式性子集)"
      (MB (&= $V_phi
              (setI (∈ $x $A)
                    (&= (app $phi $x) $top))))
      "作为等化子, 以同样的方式. (将" $phi
      "想成是定义于" $A "上的一个" (Q "命题函数")
      ", 子集" (&sube $V_phi $A) "是由分离公理提供的"
      $phi "的" (Q "外延") ".) [译注: 所谓命题函数, "
      "也可以称为谓词.]")
   (P "现在, 很容易看出来操作" $chi_U "和" $V_phi
      "是互逆的 [译注: 我更愿意省略dummy variable而说" $chi
      "和" $V "是互逆的, 当然这只是风格问题]:"
      (MB (deriv
           (_ $V $chi_U)
           (setI (∈ $x $A)
                 (&= (app $chi_U $x) $top))
           (setI (∈ $x $A) (∈ $x $U))
           $U))
      "对于任意的" (&sube $U $A) "成立. 并且, 给定任意的"
      (func $phi $A $2) ", 我们有"
      (MB (deriv
           (app (_ $chi $V_phi) $x)
           (Choice0
            ($top ",&nbsp;" (∈ $x $V_phi))
            ($bottom ",&nbsp;" (&!in $x $V_phi)))
           (Choice0
            ($top ",&nbsp;" (&= (app $phi $x) $top))
            ($bottom ",&nbsp;" (&= (app $phi $x) $bottom)))
           (app $phi $x)))
      "因此, 我们有熟悉的同构"
      (MB (&iso (Hom $A $2) (app $P $A)))
      "mediated by taking equalizers.")
   (P "函数的等化子可以取子集这样一个事实实际上是一种更一般的现象的特殊情形.")
   ((proposition #:n "3.16")
    "在任何范畴中, 如果" (func $e $E $A) "是某对箭头的一个等化子, 那么"
    $e "是一个单态射.")
   ((proof)
    "[译注: 这个证明近乎于重复了一遍定义.] 考虑图表"
    (let ((E (make-pt 30 30))
          (Z (make-pt 30 130))
          (A (make-pt 160 30))
          (B (make-pt 290 30)))
      (Svg
       #:attr* '((width "320")
                 (height "160")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       (:FO E $E)
       (:FO Z $Z)
       (:FO A $A)
       (:FO B $B)
       (:arrow A B #:offset (make-vec 0 7))
       (:arrow A B #:offset (make-vec 0 17))
       (:arrow E A #:offset (make-vec 0 12))
       (:arrow Z A #:offset (make-vec 0 12))
       (:FO (vec+ ((lerp 1/2) A B) (make-vec 0 -22)) $f)
       (:FO (vec+ ((lerp 1/2) A B) (make-vec 0 13)) $g)
       (:FO (vec+ ((lerp 1/2) E A) (make-vec 0 -17)) $e)
       (:FO (vec+ ((lerp 1/2) Z A) (make-vec 10 10)) $z)
       (:arrow Z E #:offset (make-vec -5 13) #:prop 0.7)
       (:arrow Z E #:offset (make-vec 5 13) #:prop 0.7)
       (:FO (vec+ ((lerp 1/2) E Z) (make-vec -12 -5)) $x)
       (:FO (vec+ ((lerp 1/2) E Z) (make-vec 17 -5)) $y)))
    "其中我们假定" $e "是" $f "和" $g "的等化子. 设"
    (&= (&i* $e $x) (&i* $e $y)) ", 我们想要证明" (&= $x $y)
    ". 置" (&= $z (&i* $e $x) (&i* $e $y)) ", 那么"
    (&= (&i* $f $z) (&i* $f $e $x) (&i* $g $e $x) (&i* $g $z))
    ", 于是存在" (Em "唯一") "的" (func $u $Z $E) "使得"
    (&= (&i* $e $u) $z) ". 因此, 根据" (&= (&i* $e $x) $z)
    "和" (&= (&i* $e $y) $z) "可以推出" (&= $x $u $y) ".")
   ((example #:n "3.17")
    "在许多范畴中, 例如偏序集和幺半群的范畴, 一个平行箭头序对"
    (&: (&cm $f $g) (&->-> $A $B)) "的等化子可以通过取"
    "作为基础函数的等化子按照以上方式构造出来, 即子集"
    (&sube (app $A (&= $f $g)) $A) ", 其指的是满足"
    (&= (app $f $x) (app $g $x)) "的所有元素" (∈ $x $A)
    "构成的集合. 例如, 在偏序集中我们取从" $A "限制到"
    (app $A (&= $f $g)) "的序, 而在拓扑空间中, 我们取子空间拓扑." (Br)
    "在幺半群中, 子集" (app $A (&= $f $g)) "在" $A
    "的运算下也是一个幺半群, 因此这个嵌入是一个同态. 这个的原因是 "
    "[译注: 当然十分显然] " (&= (app $f $u_A) $u_B (app $g $u_A))
    ", 并且如果" (&= (app $f $a) (app $g $a)) "且"
    (&= (app $f $a^) (app $g $a^)) ", 那么"
    (&= (app $f (&d* $a $a^))
        (&d* (app $f $a) (app $f $a^))
        (&d* (app $g $a) (app $g $a^))
        (app $g (&d* $a $a^)))
    ". 于是, " (app $A (&= $f $g))
    "包含单位元并在乘积运算下封闭." (Br)
    "在Abel群中, 我们可以有对于等化子的另外的描述方式, 使用以下事实"
    (MB (&= (app $f $x) (app $g $x)) "当且仅当"
        (&= (app (@- $f $g) $x) $0))
    "因此, " $f "和" $g "的等化子就等于同态" (@- $f $g) "和零同态"
    (func $0 $A $B) "的等化子. 于是, 只需要对于任意的同态"
    (func $h $A $B) "考虑特殊形式的等化子" (&>-> (appl $A $h $0) $A)
    "就够了. 这个" $A "的子群被称为" $h "的" (Em "核") ", 记作"
    (&ker $h) ". 因此, 我们有等化子"
    (MB (&ker (&- $f $g)) $@-> $A
        (__^^ $->-> $g $f) $B ".")
    "同态的核在对于群的研究中具有根本的重要性, "
    "在第4章中我们将进一步考虑这个概念.")
   (H3 "第3.4节 余等化子")
   (P "余等化子是对于由等价关系取商的泛化, 所以让我们从回顾这个概念开始, "
      "当然我们已经用过很多次了. 首先回忆一下, 一个集合" $X
      "上的一个" (Em "等价关系") "是一个二元关系" (&~ $x $y) ", 其满足"
      (Ul (Li "自反性: " (&~ $x $x) ";")
          (Li "对称性: " (&~ $x $y) "可以推出" (&~ $y $x) ";")
          (Li "传递性: " (&~ $x $y) "和" (&~ $y $z) "可以推出"
              (&~ $x $z) "."))
      "给定这样一个关系, 定义一个元素" (∈ $x $X) "的" (Em "等价类") "为"
      (MB (&= (eqc $x) (setI (∈ $y $X) (&~ $x $y))) ".")
      "所有不同的等价类然后构成了" $X "的一个" (Em "划分")
      ", 意思是每个元素" $y "都恰好位于其中一个集合中, 即"
      (eqc $y) " (证明一下!).")
   (P "有时人们将等价关系想成是由具有某种相同性质 (例如具体相同的颜色) "
      "的等价元素所产生的. 我们可以将等价类" (eqc $x)
      "当成是性质, 在这种意义下作为" (Q "抽象对象")
      " (比如说颜色红色, 蓝色, 等等, 它们本身). 这有时被称为"
      (Q "definition by abstraction") ", 举例来说, 它描述了"
      "实数是如何从有理数的Cauchy序列构造出来的, 或者"
      "有限基数是如何从有限集合构造出来的.")
   (P "所有等价类构成的集合"
      (MB (&= X/~ (setI (eqc $x) (∈ $x $X))))
      "或许可以被称为" $X "除以" $~ "的" (Em "商")
      ". 当人们想要" (Q "抽象出") "等价元素" (&~ $x $y)
      "之间的差异时 [译注: 也就是忽略等价元素之间的差异], "
      "就是使用" X/~ "代替" $X ", 即在" X/~
      "中这样的元素 (也仅是这样的元素) 会被等同起来, 因为"
      (MB (&= (eqc $x) (eqc $y)) "当且仅当" (&~ $x $y) ".")
      "观察到" (Em "商映射")
      (MB (func $q $X X/~))
      "其取" $x "至" (eqc $x) ", 具有这样的性质, 即一个映射"
      (func $f $X $Y) " extends along " $q ","
      (let ((X (make-pt 30 30))
            (Q (make-pt 130 30))
            (Y (make-pt 130 130)))
        (Svg
         #:attr* '((width "160")
                   (height "160")
                   (stroke "black")
                   (style "display: block; margin: auto;"))
         (Defs marker0)
         (:FO X $X)
         (:FO Q X/~)
         (:FO Y $Y)
         (:arrow #:offset (make-vec 0 12) X Q)
         (set-dotted (:arrow #:offset (make-vec 0 12) Q Y))
         (:arrow #:offset (make-vec 0 12) X Y)
         (:FO (pt+ ((lerp 1/2) X Q) (make-vec 0 -12)) $q)
         (:FO (pt+ ((lerp 1/2) X Y) (make-vec -5 5)) $f)
         ))
      "只要" $f "尊重等价关系, 意即" (&~ $x $y)
      "可以推出" (&= (app $f $x) (app $f $y)) ".")
   (P "现在让我们来考虑对偶于等化子的概念, 即所谓的余等化子.")
   ((definition #:n "3.18")
    "对于某个范畴" Cat:C "中任意的平行箭头" (func (&cm $f $g) $A $B)
    " [译注: 老实说, 这本书的很多记号都不太一致, 但是理解意思就行了], 一个"
    (Em "余等化子") "由" $Q "和" (func $q $B $Q)
    "构成, universal with the property " (&= (&i* $q $f) (&i* $q $g))
    ", 如下图所示"
    UMP:coequalizer.svg
    "也就是说, 给定任意的" $Z "和" (func $z $B $Z) ", 如果"
    (&= (&i* $z $f) (&i* $z $g)) ", 那么存在唯一的态射"
    (func $u $Q $Z) "使得" (&= (&i* $u $q) $z) ".")
   (P "首先观察到, 根据对偶性, 我们知道范畴" Cat:C
      "中的这样一个余等化子是范畴" (&op Cat:C)
      "中的一个等化子, 这个等化子根据命题3.16是单态射, 所以在"
      Cat:C "中这个余等化子应该是满态射.")
   ((proposition #:n "3.19")
    "如果" (func $q $B $Q) "是某对箭头的一个余等化子, 那么"
    $q "是一个满态射.")
   (P "因此, 我们可以将一个余等化子" (&: $q (&->> $B $Q))
      "想成是" $B "的一种" (Q "坍缩")
      ", 通过将所有序对" (&= (app $f $a) (app $g $a)) (Q "等同起来")
      " (speaking as if there were such " (Q "elements")
      " " (∈ $a $A) "). 而且, 我们以" (Q "最小")
      "的方式这么做, 也就是说, 尽可能少地打扰" $B
      ", 在于我们总是可以将" $Q "映射到任何其他的"
      $Z ", 只要其中所有这样的等同起来都是成立的.")
   ((example #:n "3.20")
    "令" (&sube $R (&c* $X $X)) "是集合" $X "上的一个等价关系, 并考虑图"
    (MB $R (__^^ $->-> $r_2 $r_1) $X)
    "其中" $r_1 "和" $r_2 "是嵌入 (inclusion) "
    (&sube $R (&c* $X $X)) "的两个投影, 即"
    (let ()
      (Svg
       #:attr* '((width "320")
                 (height "160")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       
       ))
    )
   (H2 "第4章 群和范畴")
   (H2 "第5章 极限和余极限")
   
   ))