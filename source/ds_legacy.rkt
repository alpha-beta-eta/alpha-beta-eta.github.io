#lang racket
(provide ds.html)
(require SMathML)
(define $. (Mo "." #:attr* '((lspace "0") (rspace "0"))))
(define $UnderBar (Mo "&UnderBar;"))
(define (UnderBar x) (__ x $UnderBar))
(define $sqsube (Mo "&sqsube;"))
(define $sqcup (Mo "&sqcup;"))
(define-infix*
  (&sqsube $sqsube)
  (&sqcup $sqcup)
  
  )
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
(define (:dot cx cy #:r [r 4])
  (Circle
   #:attr*
   `((cx ,(n2s cx)) (cy ,(n2s cy)) (r ,(n2s r)))))
(define (::dot pt #:r [r 4])
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
      ((right) (vec+ (vec* scale vec:right) offset:right))))
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
(define pre:ds.html
  (TnTmPrelude
   #:title "指称语义学"
   #:css "styles.css"
   (H1 "指称语义学")
   (H2 "第1章 引论")
   (H2 "第2章 指称语义学")
   (H2 "第3章 二进制数码")
   (H2 "第4章 第一级函数")
   (H2 "第5章 " $lambda "演算")
   (H3 "第5.1节 " $lambda "演算")
   
   (H3 "第5.2节 递归定义")
   
   (H2 "第6章 格与域")
   (P "前一章最后的讨论已经指出如果我们想要成功地找到一个" $lambda "演算的语义理论, 那么"
      "我们就必须找出一种限制我们能够考虑的函数类的方法. 否则的话, 就太多了而不能找到任何"
      "具有同构函数空间的非平凡的空间.")
   (P "逻辑学家的通常解决方案是压根不与函数打交道, 换而与函数的表示打交道. 简而言之, "
      "他们用哥德尔配数. 他们通常处理整数上的函数, 在这种情况下哥德尔配数操作将"
      "可用有限长度的算法表达的函数 (构成了一个可数集) 映射至整数. 当他们想要考虑以函数"
      $B "作为参数的函数" $A "时, 他们可以形式化地给" $A "提供" $B "的哥德尔配数.")
   (P ""
      )
   (H3 "第6.1节 近似序")
   (P "不使用算法的哥德尔配数, 那么我们将采用另一种方式, 这种方式更加关注于函数或者说映射本身. "
      "让我们通过观察我们的问题的另一方面来获得一点直觉, 即多重不动点的麻烦. 考虑递归函数:"
      (MB ((Ttable (lambda (d i j)
                     (if (= j 1)
                         (set-attr* d 'columnalign "left")
                         d)))
           (&Table
            ((: $f $= $lambda $x $.)
             (: "if&nbsp;"
                (&= $x $0)
                "&nbsp;then&nbsp;"
                $1))
            ($ (: "else if&nbsp;"
                  (&= $x $1)
                  "&nbsp;then&nbsp;"
                  (app $f $3)))
            ($ (: "else&nbsp;" (app $f (&- $x $2)) ".")))))
      "我们知道我们在寻找"
      (MB ((Ttable (lambda (d i j)
                     (if (= j 1)
                         (set-attr* d 'columnalign "left")
                         d)))
           (&Table
            ((: $lambda $f $. $lambda $x $.)
             (: "if&nbsp;"
                (&= $x $0)
                "&nbsp;then&nbsp;"
                $1))
            ($ (: "else if&nbsp;"
                  (&= $x $1)
                  "&nbsp;then&nbsp;"
                  (app $f $3)))
            ($ (: "else&nbsp;" (app $f (&- $x $2)) ".")))))
      "的不动点. 我们对于递归函数的实际经验或许会将我们引向解"
      (MB (&= (app $f^ $x)
              (Choice0
               ($1 (@ $x "&nbsp;even"))
               ("undefined" (@ $x "&nbsp;odd")))))
      "但是"
      (MB (&= (app $f_1 $x) $1))
      "或者更一般的"
      (MB (&= (app $f_a $x)
              (Choice0
               ($1 (@ $x "&nbsp;even"))
               ($a (@ $x "&nbsp;odd")))))
      "也是解, 其中" $a "是任意的常数. 那么为什么" $f^
      "是实践中的计算机实现会提供的函数呢?")
   (P "答案在于其他的解都包含了我们不希望实现去决定的任意的信息 ("
      $a "的值). 另一方面, " $f^ "仅包含递归定义实际传达的信息. "
      "它是我们所需要的最小的解, 即包含最少信息量的解. 让我们现在来"
      "形式化这个根据信息量对于值进行排序的想法.")
   (P "为了看明白这个序是如何运作的, 让我们先来看看一个比函数更简单的"
      "值的集合. 考虑一种对于实数进行近似的可能方式, 即闭区间, 记作"
      (li0 (UnderBar $x) (OverBar $x)) "其中" (UnderBar $x) "和"
      (OverBar $x) "是实数, 并且" (&<= (UnderBar $x) (OverBar $x))
      ". 想法在于绝对精确的值介于这两值之间.")
   (P "令" (&= $x (li0 (UnderBar $x) (OverBar $x))) "而"
      (&= $y (li0 (UnderBar $y) (OverBar $y))) ". 如果"
      (&<= (UnderBar $y) (UnderBar $x)) "且"
      (&<= (OverBar $x) (OverBar $y)) ", 那么" $x "与" $y
      "一致, 但是更加精确. 我们或许可以将其记作" (&sqsube $y $x)
      ", 意思在于" $x "比" $y "更精确 (或者说至少不更不精确), "
      $x "比" $y "包含更多 (或者相等) 的信息, 但是" $x
      "中没有任何会与" $y "相冲突的信息."
      (Svg
       #:attr* '((width "640")
                 (height "270")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       (:darr (make-pt 260 90) (make-pt 380 90))
       (:darr (make-pt 140 120) (make-pt 380 120))
       (:darr (make-pt 260 150) (make-pt 500 150))
       (:FO #:offset 'left (make-pt 260 90) $z)
       (:FO #:offset 'left (make-pt 140 120) $y)
       (:FO #:offset 'left (make-pt 260 150) $x)
       (set-attr* (:line (make-pt 260 60) (make-pt 260 180)) 'stroke-dasharray "8 4")
       (set-attr* (:line (make-pt 380 60) (make-pt 380 180)) 'stroke-dasharray "8 4")
       (:line (make-pt 30 210) (make-pt 610 210))
       (:FO (make-pt 30 210) (&- $inf))
       (:FO (make-pt 610 210) (&+ $inf)))
      "我们看到在以上图片中, 有" (&sqsube $x $z) "和" (&sqsube $y $z)
      ", 但是" $x "和" $y "是不可比较的. 实际上, " $z "是将" $y "和" $z
      "的所有信息合并了: 我们称" $z "是" $x "和" $y "的最小上界, 记作"
      (&= $z (&sqcup $y $z)) ".")
   (P "存在一些特殊的情形. 一种是区间" (li0 (&- $inf) $inf)
      ", 整个实轴. (?或许应该称为扩展实轴) 这压根就没有给出任何信息, 它比任何其他"
      "可能给出的信息都要弱, 我们称其为" $bottom ". 另一种特别的情况是包含不一致的信息, 例如"
      (&sqcup (li0 $0 $1) (li0 $2 $3)) ", 我们称其为" $top
      ". 因此, " $bottom "代表信息的缺失, 而" $top "代表信息太多, 以至于产生了矛盾的地步. "
      "其他某些值集可能更容易根据信息进行排序. 例如, 两个真值之间是无法比较的, 于是"
      "如果我们添加" $bottom "和" $top "以代表欠定和过定的真值, 那么我们就得到了下图所呈现的内容."
      (Svg
       #:attr* '((width "640")
                 (height "270")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       (:line (make-pt 320 30) (make-pt 200 135))
       (:line (make-pt 320 30) (make-pt 440 135))
       (:line (make-pt 320 240) (make-pt 200 135))
       (:line (make-pt 320 240) (make-pt 440 135))
       (:dot 320 30) (:dot 200 135)
       (:dot 440 135) (:dot 320 240)
       (:FO (make-pt 320 30) #:offset 'up $top)
       (:FO (make-pt 200 135) #:offset 'left #:scale 36 (M "false"))
       (:FO (make-pt 440 135) #:offset 'right (M "true"))
       (:FO (make-pt 320 240) $bottom))
      
      )
   
   (H3 "第6.2节 偏序集")
   ((definition)
    "一个集合" $P "是在" $sqsube "下的偏序集, 如果对于每个"
    (∈ $x $y $z $P) ", 以下性质成立:"
    (Ol (Li "(自反性) " (&sqsube $x $x) ".")
        (Li "(反对称) " (&sqsube $x $y) "且"
            (&sqsube $y $x) "可以推出"
            (&= $x $y) ".")
        (Li "(传递性) " (&sqsube $x $y) "且"
            (&sqsube $y $z) "可以推出"
            (&sqsube $x $z) ".")))
   (P "下图呈现了一些例子."
      (Svg
       #:attr* '((width "640")
                 (height "270")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       (:dot 50 135)
       (:FO (make-pt 50 135) #:offset 'left 0)
       (:dot 50 110)
       (:FO (make-pt 50 110) #:offset 'left 1)
       (:dot 50 85)
       (:FO (make-pt 50 85) #:offset 'left 2)
       (:dot 50 60)
       (:FO (make-pt 50 60) #:offset 'left 3)
       (:FO (make-pt 55 60) #:offset 'up #:scale 20 $..v)
       (:dot 50 160)
       (:FO (make-pt 50 160) #:offset 'left #:scale 22 -1)
       (:dot 50 185)
       (:FO (make-pt 50 185) #:offset 'left #:scale 22 -2)
       (:dot 50 210)
       (:FO (make-pt 50 210) #:offset 'left #:scale 22 -3)
       (:FO (make-pt 54.5 210) #:scale 18 $..v)
       (:line (make-pt 50 60) (make-pt 50 210))
       (:FO (make-pt 47 239) "(1)")

       (:dot 100 135)
       (:FO (make-pt 100 135) #:offset 'left 3)
       (:dot 100 110)
       (:FO (make-pt 100 110) #:offset 'left 4)
       (:dot 100 85)
       (:FO (make-pt 100 85) #:offset 'left 5)
       (:dot 100 60)
       (:FO (make-pt 100 60) #:offset 'left 6)
       (:FO (make-pt 105 60) #:offset 'up #:scale 20 $..v)
       (:dot 100 160)
       (:FO (make-pt 100 160) #:offset 'left 2)
       (:dot 100 185)
       (:FO (make-pt 100 185) #:offset 'left 1)
       (:dot 100 210)
       (:FO (make-pt 100 210) #:offset 'left 0)
       (:line (make-pt 100 60) (make-pt 100 210))
       (:FO (make-pt 97 239) "(2)")

       (:dot 240 60)
       (:dot 180 85)
       (:dot 300 85)
       (:dot 180 185)
       (:dot 300 185)
       (:dot 240 210)
       (:line (make-pt 240 60) (make-pt 180 85))
       (:line (make-pt 240 60) (make-pt 300 85))
       (:line (make-pt 180 85) (make-pt 180 185))
       (:line (make-pt 300 85) (make-pt 300 185))
       (:line (make-pt 240 210) (make-pt 180 185))
       (:line (make-pt 240 210) (make-pt 300 185))
       (:line (make-pt 180 85) (make-pt 300 185))
       (:FO (make-pt 243 210) $1)
       (:FO (make-pt 180 185) #:offset 'left #:scale 4 $3)
       (:FO (make-pt 300 185) #:offset 'right $2)
       (:FO (make-pt 180 85) #:offset 'left #:scale 4 $6)
       (:FO (make-pt 300 85) #:offset 'right $4)
       (:FO (make-pt 238 60) #:offset 'up 12)
       (:FO (make-pt 237 239) "(3)")

       (:dot 480 60)
       (:dot 400 100)
       (:dot 480 100)
       (:dot 560 100)
       (:dot 400 185)
       (:dot 480 185)
       (:dot 560 185)
       (:dot 480 225)
       (:line (make-pt 480 225) (make-pt 400 185))
       (:line (make-pt 480 225) (make-pt 480 185))
       (:line (make-pt 480 225) (make-pt 560 185))
       (:line (make-pt 400 185) (make-pt 480 100))
       (:line (make-pt 400 185) (make-pt 560 100))
       (:line (make-pt 480 185) (make-pt 400 100))
       (:line (make-pt 480 185) (make-pt 560 100))
       (:line (make-pt 560 185) (make-pt 400 100))
       (:line (make-pt 560 185) (make-pt 480 100))
       (:line (make-pt 400 100) (make-pt 480 60))
       (:line (make-pt 480 100) (make-pt 480 60))
       (:line (make-pt 560 100) (make-pt 480 60))
       (:FO (make-pt 460 60) #:offset 'up (setE $a $b $c))
       (:FO (make-pt 479 225) (setE))
       (:FO (make-pt 400 185) #:offset 'left #:scale 20 (setE $a))
       (:FO (make-pt 560 185) #:offset 'right (setE $c))
       (:FO (make-pt 480 185) #:offset 'left #:scale 20 (setE $b))
       (:FO (make-pt 400 100) #:offset 'left #:scale 32 (setE $b $c))
       (:FO (make-pt 480 100) #:offset 'right (setE $a $c))
       (:FO (make-pt 560 100) #:offset 'right (setE $a $b))
       (:FO (make-pt 477 242) "(4)"))
      (Ol (Li "整数集, 序为" $<= ".")
          (Li "非负整数集, 序为" $<= ".")
          (Li 12 "的因子集, 以整除性排序.")
          (Li "三元素集的幂集, 序为" $sube ".")))
   ((definition)
    "对于偏序集" $P "而言, 称其为链, 如果对于任意的"
    (∈ $x $y $P) ", " (&sqsube $x $y) "或者"
    (&sqsube $y $x) ".")
   ((corollary)
    "链的每个子集都是链.")
   ((theorem)
    "每个非空的有限链" $P "都可以写成"
    (setE $p_1 $p_2 $..h $p_n)
    "的形式, 其中当" (&<= $i $j) "时有"
    (&sqsube $p_i $p_j) ".")
   ((notation)
    
    )
   ((theorem)
    "对于任意的" (&sube $X $P) ", 至多存在一个"
    (∈ $a $P) "使得对于每个" (∈ $p $P)
    ", " (&sqsube $a $p) "当且仅当"
    (&sqsube $X $p) ". " $a "若存在, 其被称为"
    $X "的最小上界.")
   
   (H3 "第6.3节 完全格")
   (H3 "第6.4节 格上的函数")
   (H3 "第6.5节 一个不动点算子")
   (H2 "第7章 自反域")
   (H2 "第8章 " $lambda "演算的形式语义")
   
   ))
(define Tds
  (T `((mtext
        *preorder*
        ,(lambda (tag attr* . xml*)
           `(mtext ,(attr*-set attr* 'mathvariant "monospace")
                   . ,xml*))))))
(define ds.html
  (Tds pre:ds.html))