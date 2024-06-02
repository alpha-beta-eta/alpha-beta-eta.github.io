#lang racket
(provide denotational_semantics_notes.html)
(require SMathML)
(define @= (@lize &=))
(define (preserve f g . x*)
  (&= (f (apply g x*))
      (apply g (map f x*))))
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
      (width "150")
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
(define (_cm x . i*)
  (_ x (apply &cm i*)))
(define (set-compact x)
  (set-attr* x 'lspace "0" 'rspace "0"))
(define $LUB (Mo "&bigsqcup;"))
(define LUB
  (case-lambda
    ((d x) (: (__ $LUB d) x))
    ((d l x) (: (__^^ $LUB d l) x))
    ((x) (: $LUB x))))
(define @LUB (@lize LUB))
(define $fix (Mi "fix"))
(define (&fix f) (app $fix f))
(define $dom (Mi "dom"))
(define (&dom f) (app $dom f))
(define $graph (Mi "graph"))
(define (&graph f) (app $graph f))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define $forall (Mo "&forall;"))
(define (forall domain statement)
  (: $forall domain $. statement))
(define $d^^ (&Prime $d))
(define $true (Mi "true"))
(define $false (Mi "false"))
(define $=> (Mo "&Implies;"))
(define eqnderiv (compose MB deriv))
(define $w_inf (_ $w $inf))
(define $! (Mo "!"))
(define (&fact n) (: $! n))
(define (@fact n) (@ $! n))
(define (make-map m)
  (apply li0 (map (lambda (p) (apply &\|-> p)) m)))
(define map:0 (make-map `((,$X ,$x) (,$Y ,$y))))
(define $State (Mi "State"))
(define $sqsube (Mo "&sqsube;"))
(define $sqsube:compact
  (set-compact $sqsube))
(define $sqsube_1 (_ $sqsube $1))
(define $sqsube_2 (_ $sqsube $2))
(define (set-style x style)
  (set-attr* x 'style style))
(define $def (Mi "def"))
(define $def= (Mover $= $def))
(define @> (@lize &>))
(define $. (Mo "." #:attr* '((lspace "0") #;(rspace "0"))))
(define (lam var term)
  (: $lambda var $. term))
(define (set-left d)
  (set-attr* d 'columnalign "left"))
(define ((tcomment #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "tcomment")))
   (B (format "译者注记~a." n)) " " x*))
(define @-> (@lize &->))
(define align:thin
  (Ttable
   (lambda (d i j)
     (cond ((= j 0) (set-attr* d 'columnalign "right"))
           ((>= j 2) (set-attr* d 'columnalign "left"))
           (else d)))))
(define-syntax-rule (eqn* (x ...) ...)
  (MB (set-attr*
       (align:thin
        (&Table (x ...) ...))
       'displaystyle "true")))
(define (make-slide title . content*)
  (keyword-apply
   Div '(#:attr*) '(((class "slide")))
   (Div #:attr* '((align "center")) (B title))
   (Hr) content*))
(define (&label x . t*)
  (Table #:attr* '((class "label") (align "center"))
         (Tr (Td ;#:attr* '((align "center"))
              x))
         (Tr (keyword-apply
              Td '(#:attr*) '(((align "center")))
              t*))))
(define (deno P) (&db0 P))
(define (&A:deno x) (ap $A:script (deno x)))
(define (&B:deno x) (ap $B:script (deno x)))
(define $::= (Mo "&Colone;"))
(define $\| (Mo "|"))
(define $RightVector (Mo "&RightVector;"))
(define $dArr (Mo "&dArr;"))
(define $UnderBar (Mo "&UnderBar;"))
(define (UnderBar x)
  (Munder x $UnderBar))
(define $conj (Mo "&amp;"))
(define $def<=> (^^ $<=> $def))
(define-infix*
  (&::= $::=)
  (&\| $\|)
  (&RightVector $RightVector)
  (&dArr $dArr)
  (&def= $def=)
  (&sqsube $sqsube)
  (&=> $=>)
  (&and $conj)
  (&or $disj)
  (&def<=> $def<=>)
  (&sqsube_1 $sqsube_1)
  (&sqsube_2 $sqsube_2))
(define @and (@lize &and))
(define @or (@lize &or))
(define @RightVector (@lize &RightVector))
(define $p-> $RightVector)
(define &p-> &RightVector)
(define @p-> @RightVector)
(define (make-if P A E)
  (: (Ms "if") "&nbsp;" P "&nbsp;"
     (Ms "then") "&nbsp;" A "&nbsp;"
     (Ms "else") "&nbsp;" E))
(define (make-while P B)
  (: (Ms "while") "&nbsp;" P "&nbsp;"
     (Ms "do") "&nbsp;" B))
(define denotational_semantics_notes.html
  (TmPrelude
   #:title "指称语义学讲义"
   #:css "styles.css"
   (H1 "指称语义学讲义")
   (H2 "前言")
   (P "我们的目的在于介绍domain论和指称语义, 并展示其是如何为推理程序行为提供数学基础的.")
   (H3 "推荐书目")
   (Ul (Li "Winskel, G. (1993). "
           (I "The Formal Semantics of Programming Languages. ")
           "MIT Press." (Br)
           "这是一本操作语义和指称语义的极好导论. 就本课程而言, 相关的章节是"
           "5, 8, 9, 10 (第1节和第2节), 以及11. [译注: 有中文译本, 名为程序设计语言的形式语义.]")
       (Li "Tennent, R. D. (1991). "
           (I "Semantics of Programming Languages. ")
           "Prentice-Hall." (Br)
           "部分I和II与本讲义有关."))
   (H3 "深入阅读")
   (Ul (Li "Gunter, C. A. (1992). "
           (I "Semantics of Programming Languages. Structures and Techniques. ")
           "MIT Press." (Br)
           "这是一本研究生水平的教材, 包含有诸多本讲义未能涵盖的材料. "
           "就讲义本身而言, 相关的章节是第1章, 第2章, 以及第4到6章.")
       (Li "Streicher, T. (2006). "
           (I "Domain-Theoretic Foundations of Functional Programming. ")
           "World Scientific Publishing Co. ISBN 981-270-142-7" (Br)
           "一本关于本讲义后半部分所涉及的PCF语言的研究生水平教材."))
   (H2 "第1章 引论")
   (P "幻灯片1提示了给出编程语言的形式语义的几种方法. "
      "操作性方法于Part IB课程" (B "Semantics of Programming Languages")
      "中介绍, 而公理性方法在Part II课程" (B "Hoare Logic")
      "中刻画. 本课程讲义给出了指称性方法的一些技巧的简要导引. "
      "指称语义学的目的之一在于以尽可能抽象且实现无关的方式描述"
      "编程语言的构造: 通过这种方法, 我们有可能获得对于某些概念的洞察, "
      "而这些概念构成了编程语言以及其间关系的基础, 甚至有时还能理解"
      "在语言设计中实现这些概念的新方式. 当然, 验证指称性描述可以被"
      "实现是重要的. 换言之, 即将指称语义和操作语义联系起来: "
      "我们将在之后刻画如何施行此事.")
   (&label
    (make-slide
     "形式语义的风格"
     (P (B "操作的.")
        "程序片段的意义基于程序执行过程中其可以施行的"
        (Em "计算步骤") "定义.")
     (P (B "公理的.")
        "程序片段的意义间接地通过程序性质的某种逻辑的"
        (Em "公理和规则") "定义.")
     (P (B "指称的.")
        "关心给出编程语言的" (Em "数学模型")
        ". 程序片段的意义抽象地定义为某种适当数学结构的元素."))
    "幻灯片1")
   (&label
    (make-slide
     "指称语义的特征性质"
     (Ul (Li "每个程序片段" $P "被赋予一个" (Em "指称")
             (deno $P) ", 这是一个数学对象, 其代表了"
             $P "对于完整程序的意义的贡献.")
         (Li "一个程序片段的意义只由其子片段决定, 或者说"
             "指称语义是" (Em "可复合的") ".")))
    "幻灯片2")
   (H3 "第1.1节 指称语义的基本例子")
   (P "考虑基本的编程语言IMP" (Sup $-)
      ", 其相当于带有控制结构的算术和布尔表达式, "
      "而这里的控制结构是由赋值, 顺序, 条件刻画的, "
      "见幻灯片3.")
   (&label
    (set-attr*
     (make-slide
      "指称语义的基本例子 (I):"
      (Div #:attr* '((align "center"))
           "IMP" (Sup $-) "的句法")
      (P "算术表达式: "
         (MB (&::= (∈ $A (Ms "Aexp"))
                   (&\| (UnderBar $n) $L
                        (&+ $A $A) $..h)))
         "其中" $n "是整数, 而" (∈ $L $LL)
         ", " $LL "是给定的" (Em "位置")
         "的集合.")
      (P "布尔表达式: "
         (MB (&::= (∈ $B (Ms "Bexp"))
                   (&\| (Ms "true") (Ms "false")
                        (&= $A $A) (&neg $B)
                        $..h))))
      (P "命令: "
         (eqn*
          ((∈ $C (Ms "Comm"))
           $::=
           (&\| (Ms "skip") (&:= $L $A) (&\; $C $C)))
          ($ $\| (make-if $B $C $C)))))
     'style "height: 300px")
    "幻灯片3")
   ((tcomment)
    (UnderBar $n) "是代表整数" $n "的句法.")
   (P "为了给出一个编程语言的" (Em "指称语义")
      ", 我们需要赋予每种程序片段的句法范畴以一个解释的domain "
      "(domain of interpretation), 然后复合性地描述各种"
      "形成程序片段 (phrase-forming) 的构造所对应的语义函数. 对于IMP"
      (Sup $-) ", 幻灯片4到10给出了其指称语义, 并且这个语义"
      "也很容易在SML中实现.")
   (&label
    (make-slide
     "指称语义的基本例子 (II)"
     (Div #:attr* '((align "center")) "语义函数")
     (MB (set-left
          (&Table
           ((&: $A:script
                (&-> (Ms "Aexp") (@-> $State $ZZ))))
           ((&: $B:script
                (&-> (Ms "Bexp") (@-> $State $BB))))
           ((&: $C:script
                (&-> (Ms "Comm") (@p-> $State $State)))))))
     "其中"
     (eqn*
      ($ZZ $= (setE $..h $-1 $0 $1 $..h))
      ($BB $= (setE $true $false))
      ($State $= (@-> $LL $ZZ))))
    "幻灯片4")
   ((tcomment)
    $p-> "的含义是部分函数 (partial function). 另外, "
    (Ms "true") "和" $true "是不同的, 前者是句法, "
    "而后者是一个数学对象.")
   (&label
    (make-slide
     "指称语义的基本例子 (III)"
     (Div #:attr* '((align "center"))
          "语义函数" $A:script)
     (eqn*
      ((&A:deno (UnderBar $n))
       $= (lam (∈ $s $State) $n))
      ((&A:deno $L)
       $= (lam (∈ $s $State) (app $s $L)))
      ((&A:deno (&+ $A_1 $A_2))
       $= (lam (∈ $s $State)
               (&+ (app (&A:deno $A_1) $s)
                   (app (&A:deno $A_2) $s))))))
    "幻灯片5")
   (&label
    (make-slide
     "指称语义的基本例子 (IV)"
     (Div #:attr* '((align "center"))
          "语义函数" $B:script)
     (eqn*
      ((&B:deno (Ms "true"))
       $= (lam (∈ $s $State) $true))
      ((&B:deno (Ms "false"))
       $= (lam (∈ $s $State) $false))
      ((&B:deno (&= $A_1 $A_2))
       $= (lam (∈ $s $State)
               (appl 'eq
                     (app (&A:deno $A_1) $s)
                     (app (&A:deno $A_2) $s))))
      ($ $ (: "其中"
              (&= (appl 'eq $a $a^)
                  (Choice0
                   ($true ", 如果" (&= $a $a^))
                   ($false ", 如果" (&!= $a $a^))))))
      ((&B:deno (&neg $B))
       $= (lam (∈ $s $State)
               (&neg (@ (app (&B:deno $B) $s)))))))
    "幻灯片6")
   (&label
    (make-slide
     "指称语义的基本例子 (V)"
     (Div #:attr* '((align "center"))
          "语义函数" $C:script)
     (MB (&= (deno (Ms "skip"))
             (lam (∈ $s $State) $s)))
     (P "注记: 从现在开始, 语义函数的名字都将省略."))
    "幻灯片7")
   (&label
    (set-attr*
     (make-slide
      "可复合性一例"
      (P "给定部分函数"
         (&: (&cm (deno $C) (deno $C^)) (&p-> $State $State))
         "以及函数"
         (&: (deno $B) (&-> $State (setE $true $false)))
         ", 我们可以定义"
         (MB (&= (deno (make-if $B $C $C^))
                 (lam (∈ $s $State)
                      (appl 'if
                            (app (deno $B) $s)
                            (app (deno $C) $s)
                            (app (deno $C^) $s)))))
         "其中"
         (MB (&= (appl 'if $b $x $x^)
                 (Choice0
                  ($x ", 如果" (&= $b $true))
                  ($x^ ", 如果" (&= $b $false)))))))
     'style "width: 500px")
    "幻灯片8")
   (&label
    (make-slide
     "指称语义的基本例子 (VI)"
     (Div #:attr* '((align "center"))
          "语义函数" $C:script)
     (MB (&= (deno (&:= $L $A))
             (lam (∈ $s $State)
                  (lam (∈ $l $LL)
                       (appl 'if
                             (&= $l $L)
                             (app (deno $A) $s)
                             (app $s $l)))))))
    "幻灯片9")
   (&label
    (set-attr*
     (make-slide
      "顺序复合的指称语义"
      (P "两个命令的顺序复合" (&\; $C $C^) "的指称为"
         (MB (&= (deno (&\; $C $C^))
                 (&compose (deno $C^) (deno $C))
                 (lam (∈ $s $State)
                      (app (deno $C^)
                           (app (deno $C) $s)))))
         "这实际上就是命令的指称 (即部分函数"
         (&: (&cm (deno $C) (deno $C^)) (&p-> $State $State))
         ") 的复合而已.")
      (Hr)
      (P "与之相对的是, 顺序复合的操作语义为"
         (MB (&rule (&cm $C (&dArr $s $s^))
                    (&cm $C^ (&dArr $s^ (&Prime $s)))
                    (&cm (&\; $C $C^)
                         (&dArr $s (&Prime $s)))))))
     'style "width: 500px; height: 250px")
    "幻灯片10")
   (H3 "第1.2节 例子: 作为不动点的" (Ms "while") "循环")
   (P "幻灯片2所提及的" (Em "可复合性")
      "的要求是相当tough的. 我们用以赋予程序片段指称的"
      "数学对象必须足够丰富, 因为需要支持建模所讨论编程语言的"
      "一切形成程序片段的构造. 某些形成程序片段的构造"
      "是容易处理的, 而其他一些可能就不那么容易了. 例如, "
      "牵涉状态改变命令的条件表达式 [译注: 更准确地说, "
      "应该是条件命令] 可以基于应用相应的分支函数于"
      "立即子表达式的指称给出其指称语义: 见幻灯片8. "
      "类似地, 命令的顺序复合的指称语义可由从状态到状态的"
      "部分函数的复合操作得到, 如幻灯片10.")
   (P "现在我们来考虑基本编程语言IMP的指称语义, 其扩展了IMP"
      (Sup $-) "以" (Ms "while") "循环:"
      (MB (&::= (∈ $C (Ms "Comm"))
                (&\| $..h (make-while $B $C))))
      "然而, 这种循环构造并不容易以可复合的方式解释!")
   (P (Ms "while") "循环的转换语义为"
      (MB (&-> (tupa0 (make-while $B $C) $s)
               (tupa0 (make-if $B
                               (&\; $C
                                    (@ (make-while $B $C)))
                               (Ms "skip"))
                      $s)))
      "这暗示了其作为从状态到状态的部分函数的指称应该满足"
      (MB (&= (deno (make-while $B $C))
              (deno (make-if $B
                             (&\; $C
                                  (@ (make-while $B $C)))
                             (Ms "skip")))))
      "我们应该注意到这不能直接用来定义"
      (deno (make-while $B $C))
      ", 因为右边恰恰包含有一个我们想要定义其指称的子片段. "
      "使用顺序复合和" (Ms "if") "的指称语义, 以及"
      (Ms "skip") "的指称为恒等函数" (lam (∈ $s $State) $s)
      "的事实, 上面这条等式是在说" (deno (make-while $B $C))
      "应该是幻灯片11中给出的" (Em "不动点方程") "的一个解.")
   (&label
    (set-attr*
     (make-slide
      (: (deno (make-while $B $C)) "的不动点性质")
      (MB (&= (deno (make-while $B $C))
              (app (_ $f (&cm (deno $B) (deno $C)))
                   (deno (make-while $B $C)))))
      "其中, 对于每个" (&: $b (&-> $State (setE $true $false)))
      "和" (&: $c (&p-> $State $State)) ", 我们定义"
      (MB (&: (_ $f (&cm $b $c))
              (&-> (@p-> $State $State)
                   (@p-> $State $State))))
      "为"
      (MB (&= (_ $f (&cm $b $c))
              (lam (∈ $w (@p-> $State $State))
                   (lam (∈ $s $State)
                        (appl 'if (app $b $s)
                              (app $w (app $c $s))
                              $s)))))
      (Hr)
      (Ul (Li "为什么"
              (&= $w (app (_ $f (&cm (deno $B) (deno $C))) $w))
              "有解?")
          (Li "若此方程具有多解, 那么选取哪一个作为"
              (deno (make-while $B $C)) "呢?")))
     'style "width: 500px; height: 250px")
    "幻灯片11")
   (P "在赋予带有递归特性的编程语言以指称语义时, 这样的不动点方程经常出现. "
      "自Dana Scott于60年代晚期的先驱性研究始, 一种被称为" (Em "domain论")
      "的数学理论建立起来以提供一种背景环境, 其中我们不仅总是可以找到"
      "因指称语义而生的不动点方程的解, 而且还能选出在某种适切意义下"
      "最小的解, 而这实际上保证了指称语义和操作语义之间的协调配合. "
      "关键的想法在于考虑用作指称的数学对象之间的一种偏序, "
      "此偏序表达了这样的事实, 一个对象由另一个对象" (Em "近似")
      ", 或者说比另一个对象" (Em "携带了更多的信息")
      ", 或者说比另一个对象" (Em "更加良定") ". 然后, 不动点方程"
      "的最小解可以被构造为对于解的近似升链的极限. 在下一章里, "
      "这些想法将会变得从数学角度来说更加精确和一般, 但是目前先让我们"
      "具体地阐明该如何运用此想法解决幻灯片11中的特定问题.")
   (P "为了确定起见, 让我们考虑以下特定的" (Ms "while") "循环"
      (MB (make-while
           (&> $X $0)
           (@ (&\; (&:= $Y (&* $X $Y))
                   (&:= $X (&- $X $1))))))
      "其中" $X "和" $Y "是两个不同的整数存储位置 (变量), 而位置的集合"
      (&= $LL (setE $X $Y)) ".")
   ((tcomment)
    "在某种意义上说, 将" $LL "的元素既用作句法也用作讨论语义时"
    "所牵涉的概念对象是一种(司空见惯的)滥用. 但是, 只要满足目的就好.")
   (P "在这种情形之下, 我们可以就取状态为赋值"
      map:0 ", 其中" (∈ $x $y $ZZ) ", 这记录了位置" $X "和" $Y
      "的当前内容. 因此, " (&= $State (@-> $LL $ZZ)) ".")
   ((tcomment)
    "实际上, 幻灯片4中就已经定义" $State "为" (@-> $LL $ZZ) "了.")
   (P "我们正在试着将这个" (Ms "while") "循环的指称定义为一个部分函数"
      (MB (&: $w (&p-> $State $State)))
      "其应该是幻灯片11上的不动点方程"
      (MB (&= $w (app (_ $f (&cm (deno (&> $X $0))
                                 (deno (&\; (&:= $Y (&* $X $Y))
                                            (&:= $X (&- $X $1)))))) $w)))
      "的一个解.")
   (P "对于特定的布尔表达式" (&= $B (@> $X $0)) "和命令"
      (&= $C (@ (&\; (&:= $Y (&* $X $Y))
                     (&:= $X (&- $X $1)))))
      ", 函数" (_ $f (&cm (deno $B) (deno $C)))
      "恰好与幻灯片12上定义的函数" $f "相同.")
   (&label
    (set-style
     (make-slide
      (deno
       (make-while
        (&> $X $0)
        (@ (&\; (&:= $Y (&* $X $Y))
                (&:= $X (&- $X $1))))))
      "令"
      (eqn*
       ($State $def= (@-> $LL $ZZ))
       ($D $def= (@p-> $State $State)))
      "对于"
      (∈ (deno
          (make-while
           (&> $X $0)
           (@ (&\; (&:= $Y (&* $X $Y))
                   (&:= $X (&- $X $1))))))
         $D)
      ", 我们寻求" (&= $w (app $f $w))
      "的一个最小的解, 其中" (func $f $D $D)
      "被定义为"
      (MB (&= (app (app $f $w) map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ((app $w (li0 (&\|-> $X (&- $x $1))
                             (&\|-> $Y (&* $x $y))))
                ", 如果" (&> $x $0))))))
     "width: 500px")
    "幻灯片12")
   (&label
    (make-slide
     (: $D "上的偏序")
     (Ul (Li $D "上的偏序" $sqsube ":" (Br)
             (&sqsube $w $w^) "当且仅当对于每个"
             (∈ $s $State) ", 如果" $w "在" $s
             "上有定义, 那么" $w^ "也在" $s
             "上有定义, 并且" (&= (app $w $s) (app $w^ $s))
             ". 另外一种描述是, " $w "的图包含于" $w^ "的图.")
         (Li "最小元" (∈ $bottom $D) " w.r.t. " $sqsube ":" (Br)
             $bottom "即全然未定义的部分函数, 或者说图为空的部分函数, "
             "其满足对于每个" (∈ $w $D) ", " (&sqsube $bottom $w) ".")))
    "幻灯片13")
   (P "考虑幻灯片13上给出的" (&= $D (@p-> $State $State))
      "的元素之间的偏序" $sqsube ". 注意到" $sqsube
      "实际上就是前文所提及的&quot;信息序&quot;的具体化身: 如果"
      (&sqsube $w $w^) ", 那么" $w^ "在" $w "有定义的地方都保持和"
      $w "的一致, 但是它可能在其他一些参数上也有定义. 我们还应该注意到的是, "
      $D "包含一个相对于此偏序的最小元: 对于全然未定义的部分函数, 我们将其记作"
      $bottom ", 它满足对于任意的" (∈ $w $D) "都有" (&sqsube $bottom $w) ".")
   (P "自" $bottom "开始, 我们反复应用函数" $f "以构造一个部分函数的序列"
      (&cm $w_0 $w_1 $w_2 $..h) ":"
      (MB (&\; (&def= $w_0 $bottom)
               (&def= (_ $w (&+ $n $1))
                      (app $f $w_n))) ".")
      "使用幻灯片12上的" $f "的定义, 我们发现"
      (MB (&= (ap $w_1 map:0)
              (ap (app $f $bottom) map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ('undefined
                ", 如果" (&>= $x $1)))))
      (MB (&= (ap $w_2 map:0)
              (ap (app $f $w_1) map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ((make-map '((X 0) (Y y)))
                ", 如果" (&= $x $1))
               ('undefined ", 如果" (&>= $x $2)))))
      (MB (&= (ap $w_3 map:0)
              (ap (app $f $w_2) map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ((make-map '((X 0) (Y y)))
                ", 如果" (&= $x $1))
               ((make-map `((X 0) (Y ,(&* $2 $y))))
                ", 如果" (&= $x $2))
               ('undefined ", 如果" (&>= $x $3)))))
      (MB (&= (ap $w_4 map:0)
              (ap (app $f $w_3) map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ((make-map '((X 0) (Y y)))
                ", 如果" (&= $x $1))
               ((make-map `((X 0) (Y ,(&* $2 $y))))
                ", 如果" (&= $x $2))
               ((make-map `((X 0) (Y ,(&* $6 $y))))
                ", 如果" (&= $x $3))
               ('undefined ", 如果" (&>= $x $4)))))
      "并且, 在" (&>= $n $1) "的一般情况下, 我们有"
      (MB (&= (ap $w_n map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ((make-map `((X 0) (Y ,(&* (@fact $x) $y))))
                ", 如果" (&< $0 $x $n))
               ('undefined ", 如果" (&>= $x $n)))))
      "其中" (&fact $x) "是" $x "的阶乘.")
   (P "因此, 我们得到了一个部分函数的递增序列"
      (MB (&sqsube $w_0 $w_1 $w_2 $..h $w_n $..h))
      "所有这些部分函数之并是元素" (∈ $w_inf $D) ", 其为"
      (MB (&= (ap $w_inf map:0)
              (Choice0
               (map:0 ", 如果" (&<= $x $0))
               ((make-map `((X 0) (Y ,(&* (@fact $x) $y))))
                ", 如果" (&> $x $0)))))
      "注意到" $w_inf "是" $f "的一个不动点, 因为对于每个"
      map:0 ", 我们有"
      (eqnderiv
       (ap (app $f $w_inf) map:0)
       (: (Choice0
           (map:0 ", 如果" (&<= $x $0))
           ((ap $w_inf
                (make-map
                 `((X ,(&- $x $1))
                   (Y ,(&* $x $y)))))
            ", 如果" (&> $x $0)))
          (&space 6)
          "(根据" $f "的定义)")
       (: (Choice0
           (map:0 ", 如果" (&<= $x $0))
           ((make-map `((X 0) (Y ,(&* $1 $y))))
            ", 如果" (&= $x $1))
           ((make-map `((X 0) (Y ,(&* (&fact (@- $x $1))
                                      $x $y))))
            ", 如果" (&> $x $1)))
          (&space 6)
          "(根据" $w_inf "的定义)")
       (ap $w_inf map:0))
      "实际上, 我们可以表明" $w_inf "是" $f "的" (Em "最小")
      "不动点, 意即对于任意的" (∈ $w $D) ", 有"
      (MB (&=> (&= $w (app $f $w))
               (&sqsube $w_inf $w)) ".")
      "我们取这个最小不动点" $w_inf "作为"
      (MB (make-while
           (&> $X $0)
           (@ (&\; (&:= $Y (&* $X $Y))
                   (&:= $X (&- $X $1))))))
      "的指称, 其构造方式是下一章要证明的Tarski不动点定理的一个实例. "
      "我们也应该注意到, " $w_inf "的确就是命令"
      (make-while
       (&> $X $0)
       (@ (&\; (&:= $Y (&* $X $Y))
               (&:= $X (&- $X $1)))))
      "的结构操作语义所给出的从状态到状态的函数, 见Part IB课程"
      (B "Semantics of Programming Languages") ".")
   (H3 "第1.3节 练习")
   ((exercise #:n "1")
    "在SML中实现IMP" (Sup $-) "的指称语义.")
   ((exercise #:n "2")
    "考虑幻灯片11上定义的函数"
    (MB (&: (_ $f (&cm $b $c))
            (&-> (@p-> $State $State)
                 (@p-> $State $State))))
    (Ol #:attr* '((type "i"))
        (Li "根据" $n "上的归纳证明"
            (MB (&= (app (_^ $f (&cm $b $c) $n) $bottom)
                    (lam (∈ $s $State)
                         (Choice0
                          ((app $c^k $s)
                           ", 如果存在" (: $0 $<= $k $< $n)
                           "满足对于每个" (: $0 $<= $i $< $k))
                          ($ "有" (&= (app $b (app $c^i $s)) $true)
                             "而" (&= (app $b (app $c^k $s)) $false))
                          ('undefined
                           ", 如果对于每个" (: $0 $<= $i $< $n) "有"
                           (&= (app $b (app $c^i $s)) $true)))))))
        (Li "令" (&: (_ $w (&cm $b $c)) (&p-> $State $State)) "是由"
            (MB (&def=
                 (_ $w (&cm $b $c))
                 (lam (∈ $s $State)
                      (Choice0
                       ((app $c^k $s)
                        ", 如果存在" (&>= $k $0)
                        "满足对于每个" (: $0 $<= $i $< $k))
                       ($ "有" (&= (app $b (app $c^i $s)) $true)
                          "而" (&= (app $b (app $c^k $s)) $false))
                       ('undefined
                        ", 如果对于每个" (&>= $i $0) "有"
                        (&= (app $b (app $c^i $s)) $true))))))
            "定义的部分函数, 证明" (_ $w (&cm $b $c)) "满足不动点方程"
            (MB (&= (_ $w (&cm $b $c))
                    (app (_ $f (&cm $b $c))
                         (_ $w (&cm $b $c)))) "."))
        (Li "对于" (&= $b (deno (Ms "true")) (lam (∈ $s $State) $true))
            "和" (&= $c (deno (Ms "skip")) (lam (∈ $s $State) $s))
            ", 描述函数" (_ $f (&cm $b $c)) ". 什么样的从状态到状态的部分函数是"
            (_ $f (&cm $b $c)) "的不动点呢? 相对于" $sqsube
            "的最小不动点是什么呢? 这个最小不动点和"
            (make-while (Ms "true") (Ms "skip"))
            "的操作语义所确定的从状态到状态的部分函数是一致的吗?")))
   ((exercise #:n "3")
    "说明幻灯片13上的关系" $sqsube
    "的确是一个偏序, 而且" $bottom "是最小元.")
   ((exercise #:n "4")
    "证明" $w_inf "的确是" $f "的最小不动点. 更一般地, "
    "根据幻灯片13和练习2的定义, 证明对于任意的"
    (∈ $w (@p-> $State $State)) ", 有"
    (MB (&=> (&= $w (app (_ $f (&cm $b $c)) $w))
             (&sqsube (_ $w (&cm $b $c)) $w)) "."))
   (H2 "第2章 最小不动点")
   (P "本章介绍了被称为" (Em "domain论") "的数学理论, 其为构造各种"
      "编程语言特性的指称语义中所用到的最小不动点提供了一个一般性的框架. "
      "该理论是由Dana Scott提出的.")
   (H3 "第2.1节 偏序集和单调函数")
   (&label
    (make-slide
     "论点"
     (Ul (Li "所有计算的domain都是带有最小元的偏序集.")
         (Li "所有可计算函数都是单调的.")))
    "幻灯片14")
   (H4 "第2.1.1小节 偏序集")
   (P "domain论使用满足特定完备性质的偏序集. 我们在幻灯片15中回顾了"
      (Em "偏序") "的定义. " $D "被称为偏序集" (tu0 $D $sqsube:compact)
      "的" (Em "基础集(underlying set)") ". 大部分时候, 我们"
      "只以基础集的名字引用偏序集, 而以相同的符号" $sqsube
      "代表不同偏序集的偏序.")
   (&label
    (make-slide
     "偏序集"
     "集合" $D "上的二元关系" $sqsube "是一个偏序, 当且仅当它是"
     (Ul (Li "自反的: " (forall (∈ $d $D) (&sqsube $d $d)) ";")
         (Li "传递的: "
             (forall (∈ $d $d^ $d^^ $D)
                     (&=> (&sqsube $d $d^ $d^^)
                          (&sqsube $d $d^^))) ";")
         (Li "反对称的: "
             (forall (∈ $d $d^ $D)
                     (&=> (&sqsube $d $d^ $d)
                          (&= $d $d^))) "."))
     "序对" (tu0 $D $sqsube:compact) "被称为一个" (Em "偏序集") ".")
    "幻灯片15")
   ((tcomment)
    (&sqsube $d $d^ $d^^) "是" (&and (&sqsube $d $d^) (&sqsube $d^ $d^^))
    "的缩写.")
   (&label
    (make-slide
     "偏序集公理: 规则形式"
     (MB (&rule (&sqsube $x $x)))
     (Br)
     (MB (&rule (&sqsube $x $y) (&sqsube $y $z)
                (&sqsube $x $z)))
     (Br)
     (MB (&rule (&sqsube $x $y) (&sqsube $y $x)
                (&= $x $y))))
    "幻灯片16")
   (&label
    (make-slide
     (: "部分函数的domain,&nbsp;" (&p-> $X $Y))
     (Ul (Li "基础集: 由所有定义域" (&sube (&dom $f) $X)
             "且取值于" $Y "的部分函数" $f "构成.")
         (Li "偏序: " (&sqsube $f $g) "当且仅当"
             (&sube (&dom $f) (&dom $g)) "且"
             (forall (∈ $x (&dom $f))
                     (&= (app $f $x) (app $g $x)))
             ", 或者说"
             (&sube (&graph $f) (&graph $g)) ".")))
    "幻灯片17")
   ((tcomment)
    "实在是一点可有可无且无聊的注记. 若" (&sube $A $X)
    "而" (&sube $B $Y) ", 那么应该将" (&: $f (&p-> $A $B))
    "也视为" (&p-> $X $Y) "的元素吗 (假设排除" (&= $A $X) "且"
    (&= $B $Y) "的平凡情形)? 这是微妙的, 往往取决于具体的上下文.")
   ((example #:n "1")
    "从集合" $X "到集合" $Y "的所有部分函数构成的集合"
    (@p-> $X $Y) "可以看成是一个偏序集, 如幻灯片17那样. "
    "前一章里, 我们取这个domain在" (&= $X $Y $State)
    " (某个状态集合) 情形下的实例作为命令的指称集.")
   (H4 "第2.1.2小节 单调函数")
   (P "幻灯片18中给出了偏序集之间的单调映射的概念.")
   (&label
    (make-slide
     "单调性"
     "两个偏序集之间的函数" (func $f $D $E) "是"
     (Em "单调的") ", 如果"
     (MB (forall (∈ $d $d^ $D)
                 (&=> (&sqsube $d $d^)
                      (&sqsube (app $f $d) (app $f $d^)))) ".")
     (Br)
     (MB (&rule (&sqsube $x $y)
                (&sqsube (app $f $x) (app $f $y)))
         (&space 8)
         "(" $f "单调)"))
    "幻灯片18")
   ((example #:n "2")
    "给定偏序集" $D "和" $E ", 显然常函数"
    (func (lam (∈ $d $D) $e) $D $E) "是单调的.")
   ((example #:n "3")
    "当" $D "是部分函数的domain " (@p-> $State $State)
    " (见幻灯片17) 时, 幻灯片11上定义的与" (Ms "while")
    "循环的指称语义有关的函数"
    (func (_ $f (&cm $b $c)) $D $D)
    "是一个单调函数. 我们将其验证留作练习.")
   (H3 "第2.2节 最小元和前不动点 (pre-fixed point)")
   ((definition #:n "1")
    "设" $D "是一个偏序集, " $S "是" $D "的一个子集, "
    (∈ $d $S) "被称为" $S "的" (Em "最小") "元, 如果其满足"
    (MB (forall (∈ $x $S) (&sqsube $d $x)) "."))
   (P "注意到因为" $sqsube "是反对称的, 所以" $S
      "至多拥有一个最小元. 我们也应该注意到, 有的偏序集是没有最小元的. "
      "例如, 带有通常偏序的" $ZZ ".")
   (P "函数" (func $f $D $D) "的一个" (Em "不动点")
      ", 根据定义, 是满足" (&= (app $f $d) $d) "的一个元素"
      (∈ $d $D) ". 如果" $D "是一个偏序集, 我们可以考虑一个更弱的概念, 即"
      (Em "前不动点") ", 见幻灯片19.")
   (&label
    (make-slide
     "前不动点"
     "令" $D "是一个偏序集而" (func $f $D $D) "是一个函数." (Br)
     "一个元素" (∈ $d $D) "是" $f "的一个" (Em "前不动点")
     ", 如果其满足" (&sqsube (app $f $d) $d) "." (Br)
     $f "的" (Em "最小前不动点") ", 如果存在的话, 记作"
     (MB (&fix $f) ".")
     "因此, 最小前不动点由以下两条性质(唯一地)刻画:"
     (Ul (Li "(lpf1): " (&sqsube (app $f (&fix $f)) (&fix $f)) ";")
         (Li "(lpf2): "
             (forall (∈ $d $D)
                     (&=> (&sqsube (app $f $d) $d)
                          (&sqsube (&fix $f) $d))) ".")))
    "幻灯片19")
   (&label
    (make-slide
     "证明原理"
     (Ol (Li (MB (&rule (&sqsube (app $f (&fix $f)) (&fix $f)))))
         (Li "令" $D "是一个偏序集, " (func $f $D $D)
             "是一个带有最小前不动点" (∈ (&fix $f) $D)
             "的函数." (Br)
             "对于任意的" (∈ $x $D) ", 为了证明"
             (&sqsube (&fix $f) $x) ", 只需要证明"
             (&sqsube (app $f $x) $x) "."
             (MB (&rule (&sqsube (app $f $x) $x)
                        (&sqsube (&fix $f) $x))))))
    "幻灯片20")
   ((proposition #:n "2")
    "设" $D "是一个偏序集而" (func $f $D $D)
    "是一个带有最小前不动点" (&fix $f)
    "的函数. 只要" $f "是单调的, 那么" (&fix $f)
    "实际上就是" $f "的一个不动点, 因此也是" $f
    "的最小不动点.")
   ((proof)
    "因为" (&fix $f) "是" $f "的前不动点, 所以"
    (&sqsube (app $f (&fix $f)) (&fix $f))
    ". 如果" $f "是单调的, 那么"
    (MB (&sqsube (app $f (app $f (&fix $f)))
                 (app $f (&fix $f))) ".")
    "换言之, " (app $f (&fix $f)) "也是" $f
    "的一个前不动点. 但是, 鉴于" (&fix $f)
    "是最小的前不动点, 我们可以推出"
    (MB (&sqsube (&fix $f)
                 (app $f (&fix $f))) ".")
    "根据偏序的反对称性, 我们可以断言"
    (MB (&= (&fix $f) (app $f (&fix $f))))
    "即" (&fix $f) "是" $f "的一个不动点. "
    "而且, 考虑到偏序的自反性, 不动点也是前不动点. "
    "对于任意的" (∈ $d $D) "满足" (&= $d (app $f $d))
    ", 我们有" (&sqsube (&fix $f) $d)
    ". 换言之, " (&fix $f) "是" $f "的最小不动点.")
   (H3 "第2.3节 完全偏序 (cpo) 和连续函数")
   (&label
    (make-slide
     "论点*"
     (Ul (Li "所有计算的domain都是带有最小元的完全偏序.")
         (Li "所有可计算函数都是连续的.")))
    "幻灯片21")
   (H4 "第2.3.1小节 domain")
   ((definition #:n "1")
    (Ol #:attr* '((type "i"))
        (Li "若存在, 我们将偏序集" $D "的最小元记为"
            (_ $bottom $D) ". 若" $D
            "在上下文中是已知的, 写成" $bottom
            "就可以了. 因此, " $bottom "由性质"
            (MB (forall (∈ $d $D) (&sqsube $bottom $d)))
            "唯一确定. 偏序集的最小元有时也被称为其"
            (Em "底(bottom)") "元素.")
        (Li "偏序集" $D "中的一个可数的升" (Em "链")
            "是由" $D "的元素构成的一个序列满足"
            (MB (&sqsube $d_0 $d_1 $d_2 $..h))
            "这样的链的一个" (Em "上界") "是任意满足"
            (forall (∈ $n $NN) (&sqsube $d_n $d))
            "的" (∈ $d $D) ". 若链的" (Em "最小上界(lub)")
            "存在, 我们将其记为"
            (MB (LUB (&>= $n $0) $d_n) ".")
            "因此, 根据定义:"
            (Ul (Li (forall (∈ $m $NN)
                            (&sqsube $d_m (LUB (&>= $n $0) $d_n))) ".")
                (Li "对于任意的" (∈ $d $D) ", 如果"
                    (forall (∈ $m $NN) (&sqsube $d_m $d))
                    ", 那么"
                    (&sqsube (LUB (&>= $n $0) $d_n) $d) ".")))))
   ((tcomment)
    "所谓的链, 指的是偏序集的全序子集. 不过, 本讲义实际上只考虑"
    "以(通常的)序列面目出现的可数的升链.")
   ((remark #:n "2")
    "以下是读者应该注意的点."
    (Ol #:attr* '((type "i"))
        (Li "我们不需要考虑偏序集中不可数的链, 或者降链: "
            "因此, &quot;链&quot;将总是指可数的升链.")
        (Li "就和偏序集的任意子集的最小元一样, 链的最小上界"
            "若存在则唯一. 当然, 链可以没有最小上界, 例如"
            $NN "中的" (&<= $0 $1 $2 $..h) ", 不过它连上界也没有.")
        (Li "最小上界有时也被称为" (Em "上确界") ". "
            (LUB (&>= $n $0) $d_n) "的一些其他常见替代记号为"
            (MB (LUB (&= $n $0) $inf $d_n) "和"
                (LUB (setI $d_n (&>= $n $0))) "."))
        (Li "链的元素不必是互异的. 实际上, 我们称一个链"
            (&sqsube $d_0 $d_1 $d_2 $..h)
            (Em "终至恒常") ", 如果存在" (∈ $N $NN)
            "使得" (forall (&>= $n $N) (&= $d_n $d_N))
            ". 注意到此时"
            (&= (LUB (&>= $n $0) $d_n) $d_N) ".")
        (Li "如果我们丢弃链的开头任意有限数目的元素, "
            "也并不会影响其上界集和最小上界:"
            (MB (forall (∈ $N $NN)
                        (&= (LUB (&>= $n $0) $d_n)
                            (LUB (&>= $n $0)
                                 (_ $d (&+ $N $n))))) "."))))
   (&label
    (set-style
     (make-slide
      "cpo和domain"
      "一个" (Em "链完备偏序集(chain-complete poset)")
      ", 或者说缩写为" (Em "cpo") ", 是一个偏序集"
      (tu0 $D $sqsube:compact) "满足其中的每个链"
      (&sqsube $d_0 $d_1 $d_2 $..h) "都具有最小上界"
      (LUB (&>= $n $0) $d_n) ":"
      (Ul (Li "(lub1): "
              (forall (&>= $m $0)
                      (&sqsube $d_m (LUB (&>= $n $0) $d_n))) ";")
          (Li "(lub2): "
              (forall (∈ $d $D)
                      (&=> (@ (forall (&>= $m $0) (&sqsube $d_m $d)))
                           (&sqsube (LUB (&>= $n $0) $d_n) $d))) "."))
      "一个" (Em "domain") "是一个带有最小元" $bottom "的cpo:"
      (MB (forall (∈ $d $D) (&sqsube $bottom $d)) "."))
     "width: 500px; height: 300px")
    "幻灯片22")
   ((tcomment)
    "幻灯片22的两个冒号后面的内容, 只是为了解释什么是最小上界和最小元. "
    "另外, 链完备偏序集也被称为完全偏序, complete partial order.")
   (&label
    (make-slide
     "最小元和最小上界的定义: 规则形式"
     (MB (&rule (&sqsube $bottom $x)))
     (Br)
     (MB (&rule (&sqsube $x_i (LUB (&>= $n $0) $x_n)))
         (&space 4)
         "(" (&>= $i $0) "而" (ang0 $x_n) "是一个链)")
     (Br)
     (MB (&rule (forall (&>= $m $0) (&sqsube $x_m $x))
                (&sqsube (LUB (&>= $n $0) $x_n) $x))
         (&space 4)
         "(" (ang0 $x_n) "是一个链)"))
    "幻灯片23")
   (P "本讲义里我们关心的是具有特定完备性质的偏序集, "
      "见幻灯片22. 读者应该注意的是, 在有关的指称语义的文献中, "
      "术语&quot;domain&quot;的含义是相当宽泛的: "
      "存在各种各样的domain, 它们可能具有各种各样的序论性质, "
      "而不仅仅是满足链完备性质和拥有最小元.")
   ((example #:n "3")
    "从集合" $X "到集合" $Y "的所有部分函数的集合"
    (@p-> $X $Y) "上可以赋予一个偏序成为domain, 见幻灯片24. "
    "在第1.2节, 我们使用了" (&= $X $Y $State)
    "的特殊情形作为命令的指称集. 我们应该注意到, 声称是链"
    (&sqsube $f_0 $f_1 $f_2 $..h)
    "的最小上界的" $f "的确是一个良定的部分函数, "
    "因为在有定义的地方, 每个" $f_n "的值都是一致的. "
    "至于验证" $f "的确是偏序集"
    (tu0 (&p-> $X $Y) $sqsube:compact)
    "中的" (&sqsube $f_0 $f_1 $f_2 $..h)
    "的最小上界, 我们将其留给读者作为练习.")
   (&label
    (set-style
     (make-slide
      (: "部分函数的domain,&nbsp;" (&p-> $X $Y))
      (B "基础集: ")
      "由所有满足以下条件的部分函数" $f "构成, 其定义域"
      (&sube (&dom $f) $X) "而取值于" $Y "." (Br)
      (B "偏序: ")
      (&sqsube $f $g) "当且仅当"
      (&sube (&dom $f) (&dom $g)) "且"
      (forall (∈ $x (&dom $f))
              (&= (app $f $x) (app $g $x)))
      ", 或者说"
      (&sube (&graph $f) (&graph $g)) "." (Br)
      (B "链的最小上界: ")
      "链" (&sqsube $f_0 $f_1 $f_2 $..h)
      "的最小上界是部分函数" $f ", 其"
      (&= (&dom $f) (Cup (&>= $n $0) (&dom $f_n)))
      "而"
      (MB (&= (app $f $x)
              (Choice0
               ((app $f_n $x)
                ", 如果存在某个" (∈ $n $NN)
                "使得" (∈ $x (&dom $f_n)))
               ('undefined ", 否则的话"))))
      (B "最小元素: ")
      $bottom "是全然未定义的部分函数.")
     "height: 250px; width: 500px")
    "幻灯片24")
   ((example #:n "4")
    "对于任意的偏序集" (tu0 $D $sqsube:compact)
    "而言, 如果" $D "是有限的, 那么该偏序集是"
    "一个cpo. 这是因为, 在这样的偏序集中, "
    "任何链都将终至恒常, 因而拥有最小上界. "
    "当然, 有限的偏序集也不一定拥有最小元, "
    "即不是一个domain. 例如, 考虑以下Hasse图"
    "所描述的偏序集."
    (Svg
     #:attr* '((width "320")
               (height "150")
               (stroke "black")
               (style "display: block; margin: auto;"))
     (Defs marker0)
     (:dot 60 120)
     (:dot 260 120)
     (:dot 160 30)
     (:arrow (make-pt 60 120) (make-pt 160 30) #:prop 0.7)
     (:arrow (make-pt 260 120) (make-pt 160 30) #:prop 0.7))
    "一个偏序集的" (Em "Hasse图") "是一个有向图, 其顶点是"
    "偏序集的基础集的元素, 而从顶点" $x "到顶点" $y
    "有一条边当且仅当" (&!= $x $y) "且"
    (forall
     $z (&=> (@and (&sqsube $x $z) (&sqsube $z $y))
             (@or (&= $z $x) (&= $z $y)))) ".")
   (&label
    (Svg
     #:attr* '((width "500")
               (height "400")
               (stroke "black")
               (style "display: block; margin: auto;"))
     (Defs marker0)
     (:FO (make-pt 30 30) "&quot;扁平自然数集&quot;" (_ $NN $bottom) ":")
     (:FO (make-pt 60 60) $0)
     (:FO (make-pt 120 60) $1)
     (:FO (make-pt 180 60) $2)
     (:FO (make-pt 240 60) $..c)
     (:FO (make-pt 300 60) $n)
     (:FO (make-pt 360 60) (&+ $n $1))
     (:FO (make-pt 450 60) $..c)
     (:FO (make-pt 240 120) $bottom)
     (:arrow (make-pt 240 130) (make-pt 60 80))
     (:arrow (make-pt 240 130) (make-pt 120 80))
     (:arrow (make-pt 240 130) (make-pt 180 80))
     (:arrow (make-pt 240 130) (make-pt 300 80))
     (:arrow (make-pt 240 130) (make-pt 360 80))
     (:FO (make-pt 30 150) "&quot;垂直自然数集&quot;" $Omega:normal ":")
     (:FO (make-pt 240 180) $omega)
     (:FO (make-pt 225 220) (&+ $n $1))
     (:FO (make-pt 240 260) $n)
     (:FO (make-pt 240 300) $2)
     (:FO (make-pt 240 340) $1)
     (:FO (make-pt 240 380) $0)
     (:arrow (make-pt 237 385) (make-pt 237 364))
     (:arrow (make-pt 237 345) (make-pt 237 324))
     (:FO (make-pt 243 280) $..v)
     (:arrow (make-pt 237 265) (make-pt 237 244))
     (:FO (make-pt 243 200) $..v))
    "图1")
   (P "图1展示两个非常简单但却无限的domain, "
      "而以下是两个并非cpo的偏序集的例子.")
   ((example #:n "5")
    "装备有通常偏序" $<= "的自然数集"
    (&= $NN (setE $0 $1 $2 $..h))
    "不是一个cpo, 因为链" (&<= $0 $1 $2 $..h)
    "在" $NN "中没有上界.")
   ((example #:n "6")
    "考虑上图的第二个例子的一种修改版本, 其中我们为"
    $NN "添加了两个不同的上界" (&!= $omega_1 $omega_2)
    ". 换言之, 我们考虑的是"
    (&def= $D (&union $NN (setE $omega_1 $omega_2)))
    ", 而其上的偏序" $sqsube "为"
    (MB (&def<=>
         (&sqsube $d $d^)
         (Choice0
          ((&<= $d $d^)
           ", 如果" (∈ $d $d^ $NN))
          ((&or (∈ $d $NN)
                (&= $d $d^))
           ", 如果" (∈ $d^ (setE $omega_1 $omega_2))))))
    "然后, " $D "中的链" (&sqsube $0 $1 $2 $..h)
    "拥有两个上界, 即" $omega_1 "和" $omega_2
    ", 但是没有最小的上界, 因为" $omega_1 "和"
    $omega_2 "不可比较. 因此, "
    (tu0 $D $sqsube:compact) "不是一个cpo.")
   (&label
    (make-slide
     "链的最小上界的一些性质"
     "设" $D "是一个cpo."
     (Ol (Li "对于任意的" (∈ $d $D) ", "
             (&= (LUB (&>= $n $0) $d) $d) ".")
         (Li "对于" $D "中的每个链"
             (&sqsube $d_0 $d_1 $..h $d_n $..h)
             ", 我们有"
             (MB (&= (LUB (&>= $n $0) $d_n)
                     (LUB (&>= $n $0) (_ $d (&+ $N $n)))))
             "对于任意的" (∈ $N $NN) "成立.")))
    "幻灯片25")
   (&label
    (make-slide
     "链的最小上界的一些性质 (续)"
     (Ol #:attr* '((start "3"))
         (Li "对于" $D "中的两条链"
             (&sqsube $d_0 $d_1 $..h $d_n $..h)
             "和"
             (&sqsube $e_0 $e_1 $..h $e_n $..h)
             ", 如果对于每个" (∈ $n $NN) "有"
             (&sqsube $d_n $e_n) ", 那么"
             (MB (&sqsube (LUB (&>= $n $0) $d_n)
                          (LUB (&>= $n $0) $e_n)) ".")))
     (MB (&rule (forall (∈ $n $0) (&sqsube $x_n $y_n))
                (&sqsube (LUB (&>= $n $0) $x_n)
                         (LUB (&>= $n $0) $y_n)))
         (&space 4)
         "(" (ang0 $x_n) "和" (ang0 $y_n) "俱是链)"))
    "幻灯片26")
   (&label
    (set-style
     (make-slide
      "双链的对角化"
      ((lemma)
       "令" $D "是一个cpo, 设双下标索引的元素"
       (∈ (_ $d (&cm $m $n)) $D)
       " (其中" (&>= (&cm $m $n) $0)
       ") 构成的族满足"
       (MB (&=> (&and (&<= $m $m^)
                      (&<= $n $n^))
                (&sqsube
                 (_ $d (&cm $m $n))
                 (_ $d (&cm $m^ $n^)))))
       "那么我们有"
       (MB (&sqsube (LUB (&>= $n $0) (_cm $d $0 $n))
                    (LUB (&>= $n $0) (_cm $d $1 $n))
                    (LUB (&>= $n $0) (_cm $d $2 $n))
                    $..h))
       "以及"
       (MB (&sqsube (LUB (&>= $m $0) (_cm $d $m $0))
                    (LUB (&>= $m $0) (_cm $d $m $1))
                    (LUB (&>= $m $0) (_cm $d $m $2))
                    $..h))
       "而且"
       (MB (&= (LUB (&>= $m $0)
                    (@LUB (&>= $n $0) (_cm $d $m $n)))
               (LUB (&>= $k $0) (_cm $d $k $k))
               (LUB (&>= $n $0)
                    (@LUB (&>= $m $0) (_cm $d $m $n)))) ".")))
     "height: 300px")
    "幻灯片27")
   ((proof)
    "我们利用了定义链的最小上界的性质, 即幻灯片22上的(lub1)和"
    "(lub2). 首先, 注意到如果" (&<= $m $m^) ", 那么"
    (MB (deriv0 (_cm $d $m $n) $sqsube
                (_cm $d $m^ $n) $sqsube
                (LUB (&>= $n^ $0)
                     (_cm $d $m^ $n^))))
    "对于每个" (&>= $n $0) "成立. 因此, "
    (&sqsube (LUB (&>= $n $0) (_cm $d $m $n))
             (LUB (&>= $n^ $0) (_cm $d $m^ $n^)))
    ". 于是, 我们的确可以得到一条由最小上界构成的链"
    (MB (&sqsube (LUB (&>= $n $0) (_cm $d $0 $n))
                 (LUB (&>= $n $0) (_cm $d $1 $n))
                 (LUB (&>= $n $0) (_cm $d $2 $n))
                 $..h))
    "并且我们可以构造其最小上界"
    (LUB (&>= $m $0) (LUB (&>= $n $0) (_cm $d $m $n)))
    ". 运用两次(lub1), 我们有"
    (MB (&sqsube (_cm $d $k $k)
                 (LUB (&>= $n $0) (_cm $d $k $n))
                 (LUB (&>= $m $0)
                      (LUB (&>= $n $0) (_cm $d $m $n)))))
    "对于每个" (&>= $k $0) "成立, 那么根据(lub2)可以得到"
    (MB (&sqsube (LUB (&>= $k $0) (_cm $d $k $k))
                 (LUB (&>= $m $0)
                      (LUB (&>= $n $0) (_cm $d $m $n)))))
    "反过来, 对于每个" (&>= (&cm $m $n) $0) ", 我们注意到"
    (MB (deriv0 (_cm $d $m $n) $sqsube
                (_cm $d (&max $m $n) (&max $m $n)) $sqsube
                (LUB (&>= $k $0) (_cm $d $k $k))))
    "因而再应用两次(lub2)就可以推出"
    (MB (&sqsube (LUB (&>= $m $0)
                      (LUB (&>= $n $0) (_cm $d $m $n)))
                 (LUB (&>= $k $0) (_cm $d $k $k))))
    "根据" $sqsube "的反对称性, 我们就得出了想要的等式. "
    "剩余的结果也可按照相同的论证手法得到, 只需要交换"
    $m "和" $n "的角色.")
   (H4 "第2.3.2小节 连续函数")
   (&label
    (make-slide
     "连续性和严格性"
     (Ul (Li "如果" $D "和" $E "是cpo, 函数" $f
             "是" (Em "连续的") "当且仅当"
             (Ol (Li $f "是单调的;")
                 (Li $f "保持链的最小上界, 即对于" $D
                     "中的每条链" (&sqsube $d_0 $d_1 $..h)
                     ", 我们有"
                     (MB (preserve
                          (lambda (x) (app $f x))
                          (lambda (x_n)
                            (LUB (&>= $n $0) x_n))
                          $d_n) "."))))
         (Li "如果" $D "和" $E "都拥有最小元, 那么称函数"
             $f "是" (Em "严格的") ", 如果"
             (&= (app $f $bottom) $bottom) ".")))
    "幻灯片28")
   ((remark #:n "7")
    "我们注意到如果" (func $f $D $E) "是单调的, 并且"
    (&sqsube $d_0 $d_1 $d_2 $..h) "是" $D
    "中的一个链, 那么应用" $f "就可以得到" $E "中的一个链"
    (&sqsube (app $f $d_0) (app $f $d_1) (app $f $d_2) $..h)
    ". 而且, 如果" $d "是第一条链的一个上界, 那么" (app $f $d)
    "是第二条链的一个上界. 换言之, 如果" (func $f $D $E)
    "是cpo之间的单调函数, 我们总有"
    (MB (&sqsube (LUB (&>= $n $0) (app $f $d_n))
                 (app $f (LUB (&>= $n $0) $d_n))))
    "因此, 根据" $sqsube "的固有性质, 给定cpo之间的单调函数"
    (func $f $D $E) ", " $f "是连续的等价于对于" $D
    "中的每条链" (&sqsube $d_0 $d_1 $d_2 $..h)
    ", " $E "中"
    (MB (&sqsube (app $f (LUB (&>= $n $0) $d_n))
                 (LUB (&>= $n $0) (app $f $d_n))))
    "成立.")
   ((example #:n "8")
    "给定cpo " $D "和" $E ", 对于每个" (∈ $e $E) "而言, 常函数"
    (func (lam (∈ $d $D) $e) $D $E) "是连续的.")
   ((example #:n "9")
    "当" $D "是部分函数的domain " (@p-> $State $State)
    "时 (见幻灯片24), 定义于幻灯片11的与" (Ms "while")
    "循环的指称语义有关的函数" (func (_cm $f $b $c) $D $D)
    "是一个连续函数. 我们将其验证留作练习.")
   ((example #:n "10")
    "令" $Omega:normal "是垂直自然数的domain, 那么由"
    (MB (&= (app $f $x)
            (Choice0
             ($0 ", 如果" (∈ $x $NN))
             ($omega ", 如果" (&= $x $omega)))))
    "定义的函数" (func $f $Omega:normal $Omega:normal)
    "既是单调的也是严格的, 但是并非连续, 因为"
    (MB (&= (app $f (LUB (&>= $n $0) $n))
            (app $f $omega)
            $omega))
    "然而"
    (MB (&= (LUB (&>= $n $0) (app $f $n))
            (LUB (&>= $n $0) $0)
            $0)))
   (H3 "第2.4节 Tarski不动点定理")
   (&label
    (make-slide
     "Tarski不动点定理"
     "令" (func $f $D $D) "是domain " $D
     "上的一个连续函数, 那么"
     (Ul (Li $f "具有最小前不动点, 由"
             (MB (&= (&fix $f)
                     (LUB (&>= $n $0)
                          (app $f^n $bottom))))
             "给出.")
         (Li "于是, " (&fix $f) "也是" $f
             "的不动点, 因而是" $f "的"
             (Em "最小不动点") ".")))
    "幻灯片29")
   (P "幻灯片29给出了关于domain上的连续函数的关键结果, "
      "其允许我们赋予牵涉递归特性的程序以指称语义. 幻灯片上所用的记号"
      (app $f^n $bottom) "是递归定义的:"
      (MB (&\; (&def= (app $f^0 $bottom) $bottom)
               (&def= (app (^ $f (&+ $n $1)) $bottom)
                      (app $f (app $f^n $bottom)))) ".")
      "注意到既然" (forall (∈ $d $D) (&sqsube $bottom $d)) ", 我们有"
      (&sqsube (&= (app $f^0 $bottom) $bottom) (app $f^1 $bottom))
      "; 而根据单调性, 又可以推出"
      (MB (&=> (&sqsube (app $f^n $bottom)
                        (app (^ $f (&+ $n $1)) $bottom))
               (&sqsube (&= (app (^ $f (&+ $n $1)) $bottom)
                            (app $f (app $f^n $bottom)))
                        (&= (app $f (app (^ $f (&+ $n $1)) $bottom))
                            (app (^ $f (&+ $n $2)) $bottom)))) ".")
      "因此, 通过对于" (∈ $n $NN) "进行归纳, 我们可以得到"
      (MB (forall (∈ $n $NN)
                  (&sqsube (app $f^n $bottom)
                           (app (^ $f (&+ $n $1)) $bottom))) ".")
      "换言之, 元素" (app $f^n $bottom) "构成了" $D
      "中的一条链. 所以说, 既然" $D "是cpo, 那么幻灯片29上用到的"
      (LUB (&>= $n $0) (app $f^n $bottom)) "的确是有意义的.")
   ((proof)
    "首先我们注意到"
    (eqn*
     ((app $f (&fix $f))
      $= (app $f (LUB (&>= $n $0) (app $f^n $bottom))))
     ($ $= (LUB (&>= $n $0) (app $f (app $f^n $bottom)))
        (: "根据" $f "的连续性"))
     ($ $= (LUB (&>= $n $0) (app (^ $f (&+ $n $1)) $bottom))
        "根据函数的幂次的定义")
     ($ $= (LUB (&>= $n $0) (app $f^n $bottom))
        "根据第2.3节的评注2")
     ($ $= (&fix $f)))
    "因此, " (&fix $f) "的确是" $f "的一个不动点, 当然也就满足"
    "幻灯片19上的条件(lpf1). 为了验证(lpf2), 即前不动点的最小性, 我们设"
    (∈ $d $D) "满足" (&sqsube (app $f $d) $d)
    ", 那么既然" $bottom "在" $D "是最小的, 可以得到"
    (MB (&sqsube (&= (app $f^0 $bottom) $bottom) $d))
    "并且"
    (MB (&=> (&sqsube (app $f^n $bottom) $d)
             (&sqsube (&= (app (^ $f (&+ $n $1)) $bottom)
                          (app $f (app $f^n $bottom)))
                      (app $f $d) $d)))
    "根据归纳, 我们可以推出"
    (forall (∈ $n $NN) (&sqsube (app $f^n $bottom) $d))
    ". 换言之, " $d "是链的一个上界, 所以它大于等于最小上界, 即"
    (MB (&sqsube (&= (&fix $f)
                     (LUB (&>= $n $0) (app $f^n $bottom)))
                 $d))
    "这就是我们想要的(lpf2)了.")
   ((example #:n "1")
    "定义于幻灯片11上的函数" (_cm $f (deno $B) (deno $C))
    "是domain " (@p-> $State $State)
    "上的一个连续函数, 因而我们可以应用Tarski不动点定理, 将"
    (deno (make-while $B $C)) "定义为"
    (&fix (_cm $f (deno $B) (deno $C)))
    ". 实际上, 第1.2节中我们构造部分函数" $w_inf
    "的方法不过就是不动点定理的一个实例而已.")
   (&label
    (set-style
     (make-slide
      (deno (make-while $B $C))
      (eqnderiv
       (deno (make-while $B $C))
       (&fix (_cm $f (deno $B) (deno $C)))
       (LUB (&>= $n $0)
            (app (_^ $f (&cm (deno $B) (deno $C)) $n)
                 $bottom))
       (lam (∈ $s $State)
            (Choice0
             ((app (^ (deno $C) $k) $s)
              ", 如果存在" (&>= $k $0)
              "满足对于每个" (: $0 $<= $i $< $k) "有")
             ($ (&= (app (deno $B) (app (^ (deno $C) $i) $s)) $true)
                "而" (&= (app (deno $B) (app (^ (deno $C) $k) $s)) $false))
             ('undefined
              ", 如果对于每个" (&>= $i $0) "有"
              (&= (app (deno $B) (app (^ (deno $C) $i) $s)) $true))))))
     "width: 650px")
    "幻灯片30")
   (H3 "第2.5节 练习")
   ((exercise #:n "1")
    "验证幻灯片24上的断言.")
   ((exercise #:n "2")
    "证明幻灯片25和27中的声明.")
   ((exercise #:n "3")
    "验证例子9中" (_cm $f $b $c) "是连续函数的断言. 何时"
    (_cm $f $b $c) "是严格的?")
   ((tcomment)
    "以上练习皆相当平凡.")
   (H2 "第3章 domain上的构造")
   (P "本节我们将给出诸多构造domain和连续函数的方式, "
      "实际上专注于PCF编程语言的指称语义所需的构造, "
      "PCF是本讲义的后半部分的研究对象. 注意到为了"
      "描述一个cpo, 我们必须先" (Em "定义")
      "一个装备有某二元关系的集合, 然后" (Em "证明")
      (Ol #:attr* '((type "i"))
          (Li "这个关系是一个偏序;")
          (Li "对于这个偏序集中所有的链, 其最小上界存在."))
      "另外, 为了使得cpo成为一个domain, 我们还需要说明"
      (Ol #:attr* '((type "i") (start "3"))
          (Li "存在最小的元素."))
      "注意到既然链的最小上界以及最小元若存在则唯一, "
      "那么cpo和domain完全由其基础集和偏序决定. "
      "[译注: 意味不明.] "
      "之后我们将给出各种各样构造cpo和domain的方法, "
      "而将验证i, ii, iii成立的任务留作练习.")
   (H3 "第3.1节 扁平domain")
   (P "为了模拟PCF的基本类型 (ground type) " 'nat
      "和" 'bool ", 我们将使用幻灯片31给出的"
      (Em "扁平domain") "的概念.")
   (&label
    (set-style
     (make-slide
      "离散cpo和扁平domain"
      "对于任意的集合" $X ", 相等关系"
      (MB (&def<=> (&sqsube $x $x^) (&= $x $x^)))
      "使得" (tu0 $X $sqsube:compact)
      "成为一个cpo, 其被称为以" $X
      "为基础集的" (Em "离散") "cpo." (Br)
      "令" (&def= (_ $X $bottom) (&union $X (setE $bottom)))
      ", 其中" $bottom "是某个不在" $X "中的元素, 那么"
      (MB (&def<=> (&sqsube $d $d^)
                   (&or (@= $d $d^) (@= $d $bottom))))
      "使得" (tu0 (_ $X $bottom) $sqsube:compact)
      "成为一个以" $bottom "为最小元的domain, "
      "其被称为由" $X "确定的" (Em "扁平") "domain.")
     "height: 250px")
    "幻灯片31")
   (P "自然数的扁平domain " (_ $NN $bottom)
      ", 上一章的图1中我们就已描绘过其图像. 至于布尔值的扁平domain "
      (_ $BB $bottom) ", 其Hasse图为"
      (Svg
       #:attr* '((width "320")
                 (height "150")
                 (stroke "black")
                 (style "display: block; margin: auto;"))
       (Defs marker0)
       (:FO (make-pt 60 30) 'true)
       (:FO (make-pt 260 30) 'false)
       (:FO (make-pt 164 120) $bottom)
       (:arrow (make-pt 164 134) (make-pt 60 48))
       (:arrow (make-pt 164 134) (make-pt 275 48)))
      "以下牵涉扁平domain的连续函数实例对于PCF的"
      "指称语义而言也是必要的, 我们将其验证留给读者作为练习.")
   ((proposition #:n "1")
    "令" (&: $f (&p-> $X $Y)) "是两个集合之间的部分函数, 那么"
    (MB (&def= (app (_ $f $bottom) $d)
               (Choice0
                ((app $f $d)
                 ", 如果" (∈ $d $X) "且" $f
                 "定义于" $d)
                ($bottom
                 ", 如果" (∈ $d $X) "而" $f
                 "在" $d "上没有定义")
                ($bottom
                 ", 如果" (&= $d $bottom)))))
    "定义了相应扁平domain之间的连续函数"
    (func (_ $f $bottom) (_ $X $bottom) (_ $Y $bottom)) ".")
   (H3 "第3.2节 domain的积")
   (&label
    (set-style
     (make-slide
      "cpo和domain的二元积"
      "两个cpo " (tu0 $D_1 (_ $sqsube:compact $1)) "和"
      (tu0 $D_2 (_ $sqsube:compact $2))
      "之" (Em "积") "的基础集为"
      (MB (&= (&c* $D_1 $D_2)
              (setI (tu0 $d_1 $d_2)
                    (&and (∈ $d_1 $D_1)
                          (∈ $d_2 $D_2)))))
      "而其上的偏序" $sqsube "定义如下"
      (MB (&def<=> (&sqsube (tu0 $d_1 $d_2)
                            (tu0 (_^ $d $1 $prime) (_^ $d $2 $prime)))
                   (&and (&sqsube_1 $d_1 (_^ $d $1 $prime))
                         (&sqsube_2 $d_2 (_^ $d $2 $prime)))))
      "链的最小上界可以按照分量进行计算:"
      (MB (&= (LUB (&>= $n $0) (tu0 (_cm $d $1 $n) (_cm $d $2 $n)))
              (tu0 (LUB (&>= $i $0) (_cm $d $1 $i))
                   (LUB (&>= $j $0) (_cm $d $2 $j)))))
      "若" (tu0 $D_1 (_ $sqsube:compact $1)) "和"
      (tu0 $D_2 (_ $sqsube:compact $2)) "都是domain, 那么"
      (tu0 (&c* $D_1 $D_2) $sqsube:compact)
      "也是一个domain, 并且"
      (MB (&= (_ $bottom (&c* $D_1 $D_2))
              (tu0 (_ $bottom $D_1)
                   (_ $bottom $D_2)))))
     "height: 300px")
    "幻灯片32")
   ((proposition #:n "1. 投影和配对")
    "令" $D_1 "和" $D_2 "是cpo, 那么投影"
    (MB (func:def $pi_1 (&c* $D_1 $D_2) $D_1
                  (tu0 $d_1 $d_2) $d_1))
    "和"
    (MB (func:def $pi_2 (&c* $D_1 $D_2) $D_2
                  (tu0 $d_1 $d_2) $d_2))
    "是连续函数. 如果" (func $f_1 $D $D_1)
    "和" (func $f_2 $D $D_2) "是连续函数, 其中"
    $D "是一个cpo, 那么"
    (MB (func:def (tupa0 $f_1 $f_2) $D (&c* $D_1 $D_2)
                  $d (tu0 (app $f_1 $d) (app $f_2 $d))))
    "是连续的.")
   ((proof)
    "这些函数的连续性可由幻灯片32上对于" (&c* $D_1 $D_2)
    "中的链的最小上界的刻画直接推出.")
   ((proposition #:n "2")
    "对于每个domain " $D ", 函数"
    (MB (func:def 'if (&c* (_ $BB $bottom) (@ (&c* $D $D))) $D
                  (tu0 $x (tu0 $d $d^))
                  (Choice0
                   ($d ", 如果" (&= $x $true))
                   ($d^ ", 如果" (&= $x $false))
                   ((_ $bottom $D) ", 如果" (&= $x $bottom)))))
    "是连续的.")
   (P "我们将会需要以下更一般的积构造.")
   ((definition #:n "3. 依赖积")
    "给定集合" $I ", 设对于每个" (∈ $i $I) "我们有一个cpo "
    (tu0 $D_i (_ $sqsube:compact $i)) ", 那么这个cpo族之"
    (Em "积") "为"
    (Ul (Li "基础集是集合" $D_i "的" $I "重笛卡尔积"
            (prod (∈ $i $I) $D_i) ", 其由所有这样的函数"
            $p "构成, " $p "定义在" $I "上, 而" $p "在每个"
            (∈ $i $I) "处的值" (∈ (app $p $i) $D_i) ";")
        (Li "偏序" $sqsube "为"
            (MB (&def<=> (&sqsube $p $p^)
                         (forall (∈ $i $I)
                                 (: (app $p $i)
                                    (_ $sqsube $i)
                                    (app $p^ $i)))) ".")))
    "就和二元积的情况一样, "
    (tu0 (prod (∈ $i $I) $D_i) $sqsube:compact)
    "中的链的最小上界也可以逐分量计算: 如果"
    (&sqsube $p_0 $p_1 $p_2 $..h)
    "是积cpo中的一个链, 那么其最小上界是将每个"
    (∈ $i $I) "映射至" $D_i "中的链"
    (&sqsube (app $p_0 $i) (app $p_1 $i) (app $p_2 $i) $..h)
    "的最小上界的函数, 即"
    (MB (&cm (&= (app (@LUB (&>= $n $0) $p_n) $i)
                 (LUB (&>= $n $0) (app $p_n $i)))
             (∈ $i $I)) ".")
    "而且, 对于每个" (∈ $i $I) ", 第" $i "投影函数"
    (MB (func:def $pi_i (prod (∈ $j $I) $D_j) $D_i
                  $p (app $p $i)))
    "是连续的. 如果每个" $D_i "都是domain, 那么它们的积"
    "也是domain, 并且其最小元是将每个" (∈ $i $I)
    "映射至" $D_i "的最小元的函数.")
   (&label
    (set-style
     (make-slide
      "两个参数的连续函数"
      ((proposition)
       "令" (&cm $D $E $F) "是cpo, 那么函数"
       (func $f (&c* $D $E) $F) "是单调的当且仅当"
       "其对于每个参数分别都是单调的:"
       (MB (forall (&cm (∈ $d $d^ $D) (∈ $e $E))
                   (&=> (&sqsube $d $d^)
                        (&sqsube (appl $f $d $e)
                                 (appl $f $d^ $e)))))
       (MB (forall (&cm (∈ $d $D) (∈ $e $e^ $E))
                   (&=> (&sqsube $e $e^)
                        (&sqsube (appl $f $d $e)
                                 (appl $f $d $e^)))))
       "而且, 其是连续的当且仅当其对于每个参数分别都是连续的 "
       "[译注: 在单调的基础之上, 也就是对于每个参数分别都是"
       "保持链的最小上界的]:"
       (MB (&= (appl $f (LUB (&>= $n $0) $d_n) $e)
               (LUB (&>= $n $0) (appl $f $d_n $e))))
       (MB (preserve
            (lambda (e) (appl $f $d e))
            (lambda (e_n) (LUB (&>= $n $0) e_n))
            $e_n))))
     "height: 300px")
    "幻灯片33")
   (&label
    (make-slide
     "两个参数的连续函数: 推导规则"
     (Ul (Li "在" $f "单调的情况下, 我们有"
             (MB (&rule (&sqsube $x $x^)
                        (&sqsube $y $y^)
                        (&sqsube (appl $f $x $y)
                                 (appl $f $x^ $y^)))))
         (Li "在" $f "连续的情况下, 我们有"
             (MB (&rule
                  (&= (appl $f (LUB (&>= $m $0) $x_m)
                            (LUB (&>= $n $0) $y_n))
                      (LUB (&>= $k $0)
                           (appl $f $x_k $y_k))))))))
    "幻灯片34")
   ((proof)
    "&quot;仅当&quot;的方向是直接的, 其证明依赖于简单的观察, 即"
    (&=> (&sqsube $d $d^) (&sqsube (tu0 $d $e) (tu0 $d^ $e))) "和"
    (preserve (lambda (d) (tu0 d $e))
              (lambda (d_n) (LUB (&>= $n $0) d_n))
              $d_n)
    ", 以及它们之于右参数的对偶版本. 对于&quot;当&quot;的方向, "
    "首先设" $f "对于每个参数分别都是单调的, 那么如果"
    (&c* $D $E) "中有" (&sqsube (tu0 $d $e) (tu0 $d^ $e^))
    ", 根据二元积的定义, 我们可以推出" $D "中有"
    (&sqsube $d $d^) "而" $E "中有" (&sqsube $e $e^) ", 因此"
    (eqn*
     ((appl $f $d $e) $sqsube (appl $f $d^ $e) "根据对于第一个参数的单调性")
     ($ $sqsube (appl $f $d^ $e^) "根据对于第二个参数的单调性"))
    "于是, " (&sqsube (appl $f $d $e) (appl $f $d^ $e^))
    ", 即" $f "是单调函数." (Br)
    "现在设" $f "对于每个参数分别都是连续的, 那么如果"
    (&sqsube (tu0 $d_0 $e_0) (tu0 $d_1 $e_1) (tu0 $d_2 $e_2) $..h)
    "是二元积中的一个链, 我们有"
    (eqn*
     ((app $f (LUB (&>= $n $0) (tu0 $d_n $e_n)))
      $= (appl $f (LUB (&>= $i $0) $d_i) (LUB (&>= $j $0) $e_j))
      "见幻灯片32")
     ($ $= (LUB (&>= $i $0) (appl $f $d_i (LUB (&>= $j $0) $e_j)))
        "根据对于第一个参数的连续性")
     ($ $= (LUB (&>= $i $0) (LUB (&>= $j $0) (appl $f $d_i $e_j)))
        "根据对于第二个参数的连续性")
     ($ $= (LUB (&>= $n $0) (appl $f $d_n $e_n))
        "根据幻灯片27上的引理"))
    "而这就说明了" $f "的连续性.")
   (H3 "第3.3节 函数domain")
   (P "两个cpo/domain之间的所有连续函数的集合可以赋予一个偏序"
      "而成为一个cpo/domain, 见幻灯片35. 有时我们也用术语"
      "&quot;指数cpo/domain (exponential cpo/domain)&quot;"
      "而不是&quot;函数cpo/domain&quot;.")
   (&label
    (set-style
     (make-slide
      "函数cpo和domain"
      "给定cpo " (tu0 $D (_ $sqsube:compact $D)) "和"
      (tu0 $E (_ $sqsube:compact $E)) ", 函数cpo "
      (tu0 (&-> $D $E) $sqsube:compact)
      "的基础集为 [译注: 以下符号有点过载]"
      (MB (&def= (@-> $D $E)
                 (setI (func $f $D $E) (: $f "连续"))))
      "而偏序为"
      (MB (&def<=> (&sqsube $f $f^)
                   (forall (∈ $d $D)
                           (: (app $f $d)
                              (_ $sqsube $E)
                              (app $f^ $d)))))
      "推导规则:"
      (MB (&rule (: $f (_ $sqsube (@-> $D $E)) $g)
                 (: $x (_ $sqsube $D) $y)
                 (: (app $f $x)
                    (_ $sqsube $E)
                    (app $g $y)))))
     "height: 250px")
    "幻灯片35")
   (&label
    (make-slide
     "函数cpo和domain (续)"
     "链的最小上界可以逐参数计算:"
     (MB (&= (LUB (&>= $n $0) $f_n)
             (lam (∈ $d $D)
                  (LUB (&>= $n $0)
                       (app $f_n $d)))))
     "推导规则:"
     (MB (&rule
          (&= (app (@LUB (&>= $n $0) $f_n)
                   (LUB (&>= $n $0) $x_n))
              (LUB (&>= $k $0) (app $f_k $x_k)))))
     "如果" $D "和" $E "还是domain, 那么" (&-> $D $E)
     "也成为一个domain, 并且最小元为"
     (MB (func:def (_ $bottom (&-> $D $E))
                   $D $E $d (_ $bottom $E)) "."))
    "幻灯片36")
   ((proof)
    
    )
   (H2 "第4章 Scott归纳")
   (H2 "第5章 PCF")
   (H2 "第6章 PCF的指称语义")
   (H2 "第7章 将指称语义和操作语义联系起来")
   (H2 "第8章 完全抽象")
   ))