#lang racket
(provide cat_awodey.html)
(require SMathML)
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
(define-infix*
  (&iso $iso)
  (&d*M $d*M)
  (&d*N $d*N)
  
  )
(define (forget x) (&abs x))
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
   ((example)
    "一个" (Em "预序") "是一个装备有满足自反性和传递性的二元关系的集合. 若将"
    "集合的元素视为对象, 而两个对象之间存在唯一的箭头当且仅当其满足二元关系, "
    "则预序可以被视为范畴.")
   
   (H3 "第1.5节 同构")
   ((definition #:n "1.3")
    "在任意的集合" $C:bold "中, 称箭头" (func $f $A $B) "为" (Em "同构")
    ", 如果存在箭头" (func $g $B $A) "满足"
    (MB (&= (&compose $g $f) $1_A) "且" (&= (&compose $f $g) $1_B) ".")
    "这样的逆显然是唯一的, 我们记" (&= $g (inv $f)) ". 我们称" $A
    "同构于" $B ", 如果其间存在同构, 此时记" (&iso $A $B) ".")
   ((theorem #:n ". Cayley") "每个群都同构于一个置换群.")
   
   (H3 "第1.6节 范畴上的构造")
   (Ol (Li ""
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
   ((proposition)
    "一个集合之间的函数" (func $f $A $B) "是单态射恰当其为单射.")
   
   (H3 "第2.2节 始对象和终对象")
   
   (H2 "第3章 对偶")
   (H2 "第4章 群和范畴")
   (H2 "第5章 极限和余极限")
   
   ))