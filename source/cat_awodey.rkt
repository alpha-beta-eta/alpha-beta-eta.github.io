#lang racket
(provide cat_awodey.html)
(require SMathML)
(define (Cat:Arrow Cat)
  (^ Cat $->))
(define $<- (Mo "&larr;"))
(define $->> (Mo "&Rarr;"))
(define $>-> (Mo "&rarrtl;"))
(define $op (Mi "op"))
(define (&op C) (^ C $op))
(define $Aut (Mi "Aut"))
(define (Aut X) (app $Aut X))
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
(define Cat:Sets (make-cat "Sets"))
(define Cat:C (make-cat "C"))
(define Cat:D (make-cat "D"))
(define Cat:Mon (make-cat "Mon"))
(define str:empty (Mi "-"))
(define $range (Mi "range"))
(define (&range f) (app $range f))
(define @compose (@lize &compose))
(define $dom (Mi "dom"))
(define $cod (Mi "cod"))
(define (&dom f) (app $dom f))
(define (&cod f) (app $cod f))
(define $iso (Mo "&cong;"))
(define $d*M (_ $d* $M))
(define $d*N (_ $d* $N))
(define $=> (Mo "&Implies;"))
(define $. (Mo "." #:attr* '((lspace "0"))))
(define $exist (Mo "&exist;"))
(define (&exist x p)
  (: $exist x $. p))
(define $and (Mo "&amp;"))
(define-infix*
  (&iso $iso)
  (&d*M $d*M)
  (&d*N $d*N)
  (&=> $=>)
  (&and $and)
  (&->> $->>)
  (&>-> $>->)
  
  )
(define (forget x) (&abs x))
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
(define (:arrow start end #:prop [prop 0.8])
  (define t (/ (- 1 prop) 2))
  (define p1 ((lerp t) start end))
  (define p2 ((lerp (- 1 t)) start end))
  (:arr p1 p2))
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
      "的没有垃圾也没有噪音的幺半群. (第二个条件在某种意义上已经"
      "足够审慎而精当, 然而仍然不容易理解, 甚至并不足够清晰以向不懂的人"
      "传达. 笔者也没有这个能力, 自由这个想法, 怎么说呢, 这里的话就是"
      (&= (&d*M $a_1 $..c $a_j)
          (&d*M (_^ $a $1 $prime) $..c (_^ $a $k $prime)))
      "当且仅当" (&= $j $k) "且"
      (&cm (&= $a_i (_^ $a $i $prime))
           (&= $i (&cm $1 $..h $j))) ".)")
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
   (H2 "第4章 群和范畴")
   (H2 "第5章 极限和余极限")
   
   ))