#lang racket
(provide sewpr.html)
(require SMathML)
(define (make-entry name class)
  (lambda (#:n [n ""])
    (lambda x*
      (keyword-apply
       Div '(#:attr*) `(((class ,class)))
       (B (format "~a~a." name n)) " " x*))))
(define tcomment (make-entry "译者注记" "tcomment"))
(define $FV (Mi "FV" #:attr* '((mathvariant "script"))))
(define (&FV M) (app $FV M))
(define (subst M X N)
  (: M (bra0 (&<- X N))))
(define $uarr (Mo "&uarr;"))
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
(define $lceil (Mo "&lceil;"))
(define $rceil (Mo "&rceil;"))
(define (numeral n)
  (: $lceil n $rceil))
(define $beta_v:sans-serif
  (_ $beta $v:sans-serif))
(define (APP . M*)
  (apply @ M*))
(define $. (Mo "." #:attr* '((lspace "0") (rspace "0"))))
(define (LAM X M)
  (@ $lambda X $. M))
(define $<- (Mo "&larr;"))
(define $-- (Mo "-"))
(define $or (Mo "&or;"))
(define $dot (Mo "&bull;"))
(define $Implies (Mo "&Implies;"))
(define $CupCap (Mo "&CupCap;"))
(define $refl_r (_ $CupCap $r:bold))
(define $t:ms $t:monospace)
(define $f:ms $f:monospace)
(define o.r (Mo "r" #:attr* '((mathvariant "bold"))))
(define $asymp (Mo "&asymp;"))
(define $asymp_r (_ $asymp $r:bold))
(define (attach exp label)
  (Mrow exp "&nbsp;" (bra0 label)))
(define (&rulea label . r*)
  (attach (apply &rule r*) label))
(define o.rr (Mo "&ropf;" #:attr* '((mathvariant "bold"))))
(define $->r (_ $-> $r:bold))
(define $->> (Mo "&Rarr;"))
(define $->>r (_ $->> $r:bold))
(define $=r (_ $= $r:bold))
(define $cup (Mo "&cup;"))
(define $->n (_ $-> $n:bold))
(define $->>n (_ $->> $n:bold))
(define $=n (_ $= $n:bold))
(define $->α (_^ $-> $n:bold $alpha))
(define $->β (_^ $-> $n:bold $beta))
(define $->η (_^ $-> $n:bold $eta))
(define-infix*
  (&dot $dot)
  (&Implies $Implies)
  (&CupCap $CupCap)
  (&asymp $asymp)
  (&disj $or)
  (&asymp_r $asymp_r)
  (&rr o.rr)
  (&->r $->r)
  (&r o.r)
  (&->>r $->>r)
  (&refl_r $refl_r)
  (&=r $=r)
  (&<- $<-)
  (&cup $cup)
  (&->n $->n)
  (&->>n $->>n)
  
  )
(define (@dot . x*)
  (@ (apply &dot x*)))
(define (set-left d)
  (set-attr* d 'columnalign "left"))
(define (set-right d)
  (set-attr* d 'columnalign "right"))
(define $eval (Mi "eval"))
(define $B:eval (_ $eval $r:bold))
(define (&B:eval B)
  (app $B:eval B))
(define $B:eval0
  (_^ $eval $r:bold $->>r))
(define (&B:eval0 B)
  (app $B:eval0 B))
(define $B:eval1
  (_^ $eval $r:bold $=r))
(define (&B:eval1 B)
  (app $B:eval1 B))
;bad
(define (Bexp e)
  (match e
    (t $t:ms)
    (f $f:ms)
    ((,label . ,e*)
     (guard (string? label))
     (apply &rulea label (map Bexp e*)))
    ((or ,e1 ,e2) (@dot (Bexp e1) (Bexp e2)))
    ((asymp ,e1 ,e2) (&asymp_r (Bexp e1) (Bexp e2)))
    ((== ,e1 ,e2) (&asymp_r (Bexp e1) (Bexp e2)))
    ((r ,e1 ,e2) (&r (Bexp e1) (Bexp e2)))
    ((rr ,e1 ,e2) (&rr (Bexp e1) (Bexp e2)))
    ((-> ,e1 ,e2) (&->r (Bexp e1) (Bexp e2)))
    ((->> ,e1 ,e2) (&->>r (Bexp e1) (Bexp e2)))
    (,metavar metavar)))
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
(define (:arr start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (stroke-width "1.2px"))))
(define (:arrow start end #:prop [prop 0.8])
  (define t (/ (- 1 prop) 2))
  (define p1 ((lerp t) start end))
  (define p2 ((lerp (- 1 t)) start end))
  (:arr p1 p2))
(define sewpr.html
  (TnTmPrelude
   #:title "语义工程和PLT Redex"
   #:css "styles.css"
   (H1 "语义工程和PLT Redex")
   (H2 "第1部分 规约语义")
   (H3 "第1章 语义via句法")
   (P "描述编程语言自句法始. 正如每个程序员所知, 语言的句法总以BNF (Backus-Naur Form) "
      "语法的某个变种形式出现, 其枚举了符合语法的词汇和句子. 困难之处实际上在于刻画"
      "程序的意义, 即程序是如何进行计算的.")
   (P "在这本书的第一部分, 我们建立了一个基于句法的语义描述方法. 我们从这样的观察开始, "
      "即计算 (computation) 是对于算术 (calculation) 的一般化, 一个孩子所接受的"
      "算术训练从&quot;" $1 "加" $1 "等于" $2 "&quot;这样的材料开始. 诀窍在于"
      "看出这种形式的算术也可应用到程序上来.")
   (P "程序的算术意味着观察一个表达式或者语句的句法, 然后将其与另一个表达式或者语句"
      "联系起来, 通常假定是更简单的. 对于表达式" (&+ $1 $1) "而言, 前面的声明是"
      "容易理解的. 它等于" $2 ", 即" (&+ $1 $1) "和" $2 "关联起来. 即便是函数应用于"
      "参数值也可以这种方式表达 [译注: 虽然" $+ "本身就是一个二元函数]:"
      (MB (&cm (&= (app $f $4) (&+ (&* $2 $4) 55))
               (: "如果定义" (&= (app $f $x) (&+ (&* $2 $x) 55)))) "."))
   (P "用数学的语言来说, 我们是将编程语言的语义描述为其句法上的关系. 对于学过函数式编程语言的"
      "人而言, 这种声明并不令人意外. 我们知道函数式编程并不超出将七年级代数转换为编程语言"
      "太多, 而代数定律是将代数表达式互相联系起来的等式. [译注: 在某种意义上来说, 这句话是一种虚张声势.] "
      "但令人惊讶的是, 这种方法具有描述(几乎)所有编程语言的语义的可能性, 即便是包含"
      "命令式副作用的语言.")
   (P "现在我们来介绍这种想法, 从将句法定义为集合开始, 这是必要的数学元知识.")
   (H4 "第1.1节 定义集合")
   (P "BNF语法可有多种用途. 一种含义是字符串的集合. 另一种解释是&quot;树&quot;"
      "的集合, 其常被称为抽象句法(树). 本书我们总是指后者.")
   (P "对于本章和下一章而言, 我们使用下面的BNF语法作为例子:"
      (MB (deriv0 $B $= $t:ms
                  $lv $f:ms
                  $lv (@dot $B $B)))
      "我们将其当作下列施加于抽象句法树集合" $B "上的约束的缩写:"
      (MB (&Table
           ((&in $t:ms $B))
           ((&in $f:ms $B))
           ((&Implies (: (&in $a $B) "&nbsp;且&nbsp;" (&in $b $B))
                      (&in (@dot $a $b) $B)))))
      "从技术上说, " $B "是满足以上约束的最小集合. 为了构造这个集合, "
      "我们先容纳基本元素" $t:ms "和" $f:ms
      ", 然后归纳地将其中的东西组合成复合元素.")
   (P "记号: 我们有时用&quot;"$B"&quot;表示&quot;集合"$B"&quot;, 但有时&quot;"
      $B"&quot;也代表&quot;"$B"的任意一个元素&quot;. 从上下文来看, 含义总是明确的. "
      "有时, 我们将下标或一撇附加在集合的名字上来集合的任意元素, 例如&quot;"$B_1
      "&quot;或者&quot;"$B^"&quot;. 因此, 以上约束也可以写成"
      (MB
       (&Table
        ((&in $t:ms $B) (bra0 "a"))
        ((&in $f:ms $B) (bra0 "b"))
        ((&in (@dot $B_1 $B_2) $B) (bra0 "c")))))
   (P "在有限的空间之中枚举出" $B "的所有元素显然是不可能的:"
      (MB (&= $B (setE $t:ms $f:ms (@dot $t:ms $t:ms) (@dot $t:ms $f:ms) $..h)))
      "然而, 给定某个树, 我们可以通过论证它满足约束来证明其属于" $B ". 例如, "
      (@dot $t:ms (@dot $f:ms $t:ms)) "就在" $B "中:"
      (MB (set-attr*
           (&Table
            ("1." (&in $t:ms $B) (: "by&nbsp;" (bra0 "a")))
            ("2." (&in $f:ms $B) (: "by&nbsp;" (bra0 "b")))
            ("3." (&in $t:ms $B) (: "by&nbsp;" (bra0 "a")))
            ("4." (&in (@dot $f:ms $t:ms) $B) (: "by 2, 3, and&nbsp;" (bra0 "c")))
            ("5." (&in (@dot $t:ms (@dot $f:ms $t:ms)) $B)
                  (: "by 1, 4, and&nbsp;" (bra0 "c"))))
           'columnalign "left"))
      "通常, 这样的论证也可以被安排成所谓的证明树的形式:"
      (MB (attach
           (&rule (attach (&in $t:ms $B) "a")
                  (attach
                   (&rule (attach (&in $f:ms $B) "b") (attach (&in $t:ms $B) "a")
                          (&in (@dot $f:ms $t:ms) $B))
                   "c")
                  (&in (@dot $t:ms (@dot $f:ms $t:ms)) $B))
           "c"))
      "绝大多数时候, 证明树以没有标签的形式出现, 因为每一步通常都是显然的:"
      (MB (&rule (&in $t:ms $B)
                 (&rule (&in $f:ms $B) (&in $t:ms $B)
                        (&in (@dot $f:ms $t:ms) $B))
                 (&in (@dot $t:ms (@dot $f:ms $t:ms)) $B))))
   ((exercise #:n "1.1")
    "以下哪些是" $B "的元素?"
    (Ol (Li $t:ms ";")
        (Li $dot ";")
        (Li (@dot (@dot $f:ms $t:ms) (@dot $f:ms $f:ms)) ";")
        (Li (@dot (@ $f:ms) (@ $t:ms)) ";")
        (Li (M "&quot;hello&quot;") "."))
    "若是, 则提供一个证明树.")
   (H4 "第1.2节 关系")
   (P "关系是序对的集合. 例如, 我们可以将关系" $R:bold
      "定义成将" $B "的每个元素与自身匹配的关系:"
      (MB (&Implies (&in $a $B)
                    (&in (tupa0 $a $a) $R:bold)))
      "对于像" $R:bold "这样的二元关系, 与其记" (&in (tupa0 $a $a) $R:bold)
      ", 不如写成" (: $a $R:bold $a) ":"
      (MB (&Implies (&in $a $B)
                    (: $a $R:bold $a)))
      "甚至可以更简单"
      (MB $B_1 $R:bold $B_1)
      "只要其被理解为" $R:bold "的定义. 实际上, 关系" $R:bold
      "是自反的, 对称的, 传递的, 即其满足以下三个约束条件:"
      (MB (set-attr*
           (&Table
            ((: "一个关系" $R:bold "是自反的")
             "当且仅当" (: (: $a $R:bold $a) "&nbsp;(对于任意的" $a "而言)"))
            ((: "一个关系" $R:bold "是对称的")
             "当且仅当" (&Implies (: $a $R:bold $b) (: $b $R:bold $a)))
            ((: "一个关系" $R:bold "是传递的")
             "当且仅当"
             (&Implies (: (: $a $R:bold $b) "且" (: $b $R:bold $c)) (: $a $R:bold $c))))
           'columnalign "left"))
      "如果一个关系是自然的, 对称的, 传递的, 那么其就被称为一个等价关系 (equivalence). "
      "关系的特定名字, 例如" $= ", 暗示了该关系是一个等价关系.")
   (P "以下定义了一个关系" o.r ", 它既不自反, 对称, 也不传递."
      (MB (&Table
           ((@dot $f:ms $B_1) o.r $B_1 (bra0 "a"))
           ((@dot $t:ms $B_1) o.r $t:ms (bra0 "b"))))
      "在规约语义的上下文中, 这样的关系被称为" (B "规约的概念")
      ". 对于该定义的一个小小的修饰可以产生一个自反关系" $refl_r ":"
      (MB (&Table
           ((@dot $f:ms $B_1) $refl_r $B_1 (bra0 "a"))
           ((@dot $t:ms $B_1) $refl_r $t:ms (bra0 "b"))
           ($B_1 $refl_r $B_1 (bra0 "c"))))
      "另一种定义" $refl_r "的方法在于扩展" o.r
      "并显式约束新的关系为自反的:"
      (MB (&Table
           ($B_1 $refl_r $B_2 (: "如果" $B_1 o.r $B_2) (bra0 "ab"))
           ($B_1 $refl_r $B_1 $ (bra0 "c"))))
      "因此关系" $refl_r "是" o.r "的" (B "自反闭包")
      ". 我们还可以通过添加对称和传递约束来定义另一个关系:"
      (MB ((Ttable (lambda (d i j)
                     (if (= j 3)
                         (set-attr* d 'columnalign "left")
                         d)))
           (&Table
            ($B_1 $asymp_r $B_2 (: "如果" $B_1 o.r $B_2) (bra0 "ab"))
            ($B_1 $asymp_r $B_1 $ (bra0 "c"))
            ($B_2 $asymp_r $B_1 (: "如果" $B_1 $asymp_r $B_2) (bra0 "d"))
            ($B_1 $asymp_r $B_3
                  (: "如果" (: $B_1 $asymp_r $B_2) "且"
                     (: $B_2 $asymp_r $B_3)) (bra0 "e")))))
      "关系" $asymp_r "是" $refl_r "的" (B "对称传递闭包")
      ", 并且它也是" o.r "的" (B "自反对称传递闭包") "或者说" (B "等价闭包") ".")
   (H4 "第1.3节 作为等价关系的语义")
   (P $B "和" o.r "的实际例子暗示了一个编程语言是如何通过句法以及句法上的关系定义的, "
      "或者更确切地说, 作为一个抽象句法树上的集合" $B "以及该集合上的一个关系" o.r
      ". 实际上, 或许机敏的读者可能开始怀疑" $B "是布尔表达式的语法, 其中" $t:ms
      "代表" 'true ", " $f:ms "代表" 'false ", " $dot "代表&quot;或&quot;运算符. 关系"
      $asymp_r "将具有相同(布尔)值的" $B "表达式等同起来.")
   (P "的确, 使用上面的约束, 我们可以表明" (Bexp '(asymp (or f t) (or t t)))
      ", 正如" (&= (&disj 'false 'true) (&disj 'true 'true)) ":"
      (MB (Bexp '("e" ("a" (== (or f t) t))
                      ("d" ("b" (== (or t t) t))
                           (== t (or t t)))
                      (== (or f t) (or t t)))))
      "然而, 这并不能直接得出" $dot "和布尔&quot;或&quot;表现得完全一致. "
      "如果我们希望建立这种联系, 那么我们不得不证明关于" $dot "的一般声明, "
      "例如对于任意的表达式" $B_1 "有" (Bexp `(asymp (or ,$B_1 t) t)) ".")
   (P "换言之, 编程语言的语义和我们想要知道的这个语义的性质之间存在一般性的gap. "
      "出于许多目的, 语义的性质和其将表达式或程序联系至的值一样重要. 例如, 如果"
      $dot "的确满足&quot或&quot;的法则, 那么一个编译器或许可以安全地将"
      (Bexp `(or ,$B_1 t)) "优化为" $t:ms ". 类似地, 如果编程语言的语义可以保证"
      "数字不会被加到任何非数字的东西上去, 那么语义的实现就无需检查加法运算的"
      "参数以保证它们都是数字.")
   (H4 "第1.4节 语义via规约")
   (P "关系" $asymp_r "让人想到小学里从算术和代数中学到的" $=
      ". 正如在这种情况下我们教授学生出于各种目的都使用这样的等式推理, 我们可以使用"
      $asymp_r "关系来证明特定表达式的等价性. 尽管如此, 在一般情况下, 这个关系"
      "并没有指明该如何从一个任意的" $B "得到" $t:ms "或" $f:ms "&mdash;&mdash;"
      "这是我们在构建一个语义的解释器时所真正需要的东西.")
   (P "在这种意义下, 关系" o.r "实际上比" $asymp_r "更有用. 在" o.r
      "的定义的两种情况下, 一个表达式都与另一个更小的表达式联系起来. 而且, 对于任何表达式"
      $B ", 要么" $B "是" $t:ms "或" $f:ms ", 要么" o.r "将" $B
      "与至多一个另外的表达式联系起来. 因此, 我们可以将" o.r "想成是一个"
      (B "单步规约") ", 与解释器采取单一的求值步骤迈向最终的值对应起来.")
   (P "使用" o.r ", 我们可以定义其自反传递闭包" $rr ":"
      (MB (set-attr*
           (&Table
            ($B_1 o.rr $B_1)
            ($B_1 o.rr $B_2 "如果" (&r $B_1 $B_2))
            ($B_1 o.rr $B_2 "如果" (&rr $B_1 $B_3) "且" (&rr $B_3 $B_2)))
           'columnalign "left"))
      "这产生了一个" (B "多步规约") "关系. 特别地, 多步关系" o.rr
      "将一个表达式映射至许多其他的表达式, 但是" $t:ms "和" $f:ms
      "之中最多只能有一个.")
   (P "关系" o.r "和" o.rr "被有意设计为非对称的, 这强调了求值"
      "具有特定的方向性. 例如, 给定表达式" (Bexp '(or f (or f (or t f))))
      ", 我们可以表明存在一个从它到" $t:ms "的" (B "规约") "序列:"
      (MB (deriv0 (Bexp '(or f (or f (or t f)))) o.r (Bexp '(or f (or t f)))
                  o.r (Bexp '(or t f))
                  o.r $t:ms))
      "左列的空白隐式地被前一行的右列的表达式所填充. 于是, 每一行都是对于"
      (Bexp '(rr (or f (or f (or t f))) t)) "的论证中的一步.")
   ((exercise #:n "1.2")
    "通过构造基于单步关系" o.r "的规约序列证明"
    (Bexp '(rr (or f (or f (or f f))) f)) ".")
   (H4 "第1.5节 上下文中的规约")
   (P "表达式" (Bexp '(or (or f t) f)) "该怎样规约呢? 根据" o.r "或者"
      o.rr ", 它压根就不能规约. 从直觉上来说, " (Bexp '(or (or f t) f))
      "应该被规约为" (Bexp '(or t f)) ", 即根据" (Bexp '(r (or f t) t))
      "来简化第一个子表达式. 然而, " o.r "的定义中没有能够匹配"
      (Bexp '(or (or f t) f)) "作为源表达式的规则. 也就是说, 我们只能规约具有形式"
      (@dot $f:ms $B) "或者" (@dot $t:ms $B) "的表达式. 尽管最外层的" $dot
      "的右侧表达式可以是任意的, 但左侧的表达式必须是" $t:ms "或者" $f:ms
      "才行 [译注: 即才能规约].")
   (P "如果我们希望将这样的" $B "表达式规约为答案, 那么我们必须扩展" o.r
      "关系为另一个能够支持子表达式规约的关系."
      (MB ((Ttable (lambda (d i j)
                     (cond ((= j 0) (set-right d))
                           ((= j 2) (set-left d))
                           ((= j 4) (set-left d))
                           (else d))))
           (&Table
            ($B_1 $->r $B_2 "如果" (: $B_1 o.r $B_2) (bra0 "a"))
            ((@dot $B_1 $B_2) $->r (@dot (_^ $B $1 $prime) $B_2)
                              "如果" (&->r $B_1 (_^ $B $1 $prime)) (bra0 "b"))
            ((@dot $B_1 $B_2) $->r (@dot $B_1 (_^ $B $2 $prime))
                              "如果" (&->r $B_2 (_^ $B $2 $prime)) (bra0 "c")))))
      "关系" $->r "是" o.r "的" (B "兼容闭包") ". 类似于" o.r ", "
      $->r "是一个单步规约关系, 但是" $->r "允许对于表达式的任意子表达式进行规约. "
      "可规约表达式 (reducible expression) 被称为" (B "redex") ", 而一个redex周围的文本被称为其"
      (B "上下文") ".")
   (P "特别地, 关系" $->r "包含" (Bexp '(-> (or (or f t) f) (or t f)))
      ". 我们可以用以下证明树来说明这个包含:"
      (MB (Bexp '("b" ("a" (r (or f t) t)
                           (-> (or f t) t))
                      (-> (or (or f t) f)
                          (or t f)))))
      "继续使用" $-> ", 我们可以将" (Bexp '(or (or f t) f)) "规约至" $t:ms ":"
      (MB (deriv0 (Bexp '(or (or f t) f)) $->r (@dot $t:ms $f:ms)
                  $->r $t:ms))
      "最后, 如果我们将" $->>r "定义为" $->r "的自反传递闭包, 那么我们就得到了"
      (Bexp '(->> (or (or f t) f) t)) ". 因此, " $->>r "是由" o.r
      "生成的" (B "自然规约关系") ".")
   (P "一般来说, 仅仅关系" o.r "的自反闭包" $refl_r ", 等价闭包" $asymp_r
      ", 以及自反传递闭包" o.rr "不是很有趣. 我们往往最感兴趣的是兼容闭包" $->r
      "及其自反传递闭包" $->>r ". 这两种关系与表达式求值和解释的典型概念相对应. "
      "而且, " $->r "的等价闭包" $=r "也是有趣的, 因为其将产生相同结果的"
      "表达式联系起来.")
   ((exercise #:n "1.3")
    "解释为什么" (&!in (tupa0 (Bexp '(or f (or (or t f) f))) $t:ms) o.rr) ".")
   ((exercise #:n "1.4")
    "通过基于" $->r "的规约序列来证明"
    (Bexp '(->> (or f (or (or t f) f)) t)) ".")
   (H4 "第1.6节 求值函数")
   (P $->>r "使我们接近了求值的有用概念, 但是我们还没有抵达那里. 尽管"
      (Bexp '(->> (or (or f t) f) t)) ", 但是我们也有"
      (Bexp '(->> (or (or f t) f) (or t f))) "和"
      (Bexp '(->> (or (or f t) f) (or (or f t) f)))
      ". 然而, 对于一个求值器而言, 我们仅关心一个" $B
      "是否能求值到一个结果以及这个结果是" $f:ms "还是" $t:ms
      ". 其他一切都是无关紧要的.")
   (P "我们使用两个定义来形式化地陈述这个想法. 首先, 我们需要刻画我们所认为的"
      $B "&quot;程序&quot;的结果" $R "是什么. "
      (MB (deriv0 $R $= $t:ms $lv $f:ms))
      "显然, " $R "是" $B "的一个子集, 因为结果也是我们的&quot;编程语言&quot;"
      "的表达式.")
   (P "我们的第二个定义将求值刻画为" $B:eval0 "关系, 其将每个表达式映射至一个结果."
      (MB (func:def $B:eval0 $B $R $B
                    (Choice0
                     ($f:ms "如果" (&->>r $B $f:ms))
                     ($t:ms "如果" (&->>r $B $t:ms)))))
      "这里我们使用了另外的记号来定义关系, 这暗示了其为" (B "函数")
      ", 即将每个元素映射至最多一个元素的关系. [译注: 这是所谓的partial function.] "
      "我们使用函数记号的原因在于, 若" $B:eval0 "作为求值器要make sense的话, "
      "那么它就必然是一个函数 (至少对于确定性的编程语言而言).")
   (P "这个关系的名字既有下标也有上标. 自然地, 前者只是在说这个函数基于关系"
      o.r ", 而后者是在告诉我们求值的定义基于关系" $->>r ". 我们用这两者"
      "来装饰函数的名字, 因为存在许多其他变种定义. 例如, 以下定义使用基于" o.r
      "的等价关系而不是自然规约关系."
      (MB (&= (&B:eval1 $B)
              (Choice0
               ($f:ms "如果" (&=r $B $f:ms))
               ($t:ms "如果" (&=r $B $t:ms)))))
      "等价关系" $=r "当然是" o.r "的兼容, 自反, 传递, 对称闭包. "
      "[译注: 更确切地说, 是兼容闭包的等价闭包, 因为这两个运算并不可以交换.] "
      "通过" $=r "来定义一个求值函数表明了程序的计算真的就只是一般化了"
      "来源于代数的运算概念.")
   ((exercise #:n "1.5")
    "关系" (&cm o.r $refl_r o.rr $->r $->>r $=r) "中哪些是函数? "
    "对于非函数的关系, 找到一个表达式和另外两个与其关联的表达式.")
   ((exercise #:n "1.6")
    "使用上述定义来找出" (&B:eval1 (Bexp '(or (or f t) t))) "和"
    (&B:eval0 (Bexp '(or (or f t) f))) "的结果.")
   (H4 "第1.7节 记号总结")
   (P "以下表格总结了我们到目前为止引入的概念和记号."
      (MB (set-left
           (&Table
            ("名字" "定义" "直觉")
            ($-- "表达式语法的成员上的基本关系"
                 "没有上下文的单一&quot;规约&quot;步骤")
            ((_ $-> $--) (: "相对于表达式语法的" $-- "的兼容闭包")
                         "上下文中的单步规约")
            ((_ $->> $--) (: (_ $-> $--) "的自反传递闭包")
                          "复数求值步骤 (零或更多)")
            ((_ $= $--) (: (_ $->> $--) "的对称传递闭包")
                        "将产生相同结果的表达式等同起来")
            ((_^ $eval $-- $--) "投影于某个范围的关系"
                                (: "基于" (_ $->> $--) "或者"
                                   (_ $= $--) "的完全求值")))))
      "[译注: 如果仅是对称闭包, 一般情况下并不保持传递性, 所以需要对称传递闭包.] "
      "最终, 我们希望得到" (_ $eval $--) ", 即一般的求值关系.")
   (H3 "第2章 分析句法性的语义")
   (P "一旦我们有了一个编程语言的句法和语义, 我们可以提出问题, 进行实验, 以及"
      "考虑变体. 在本部分中, 我们将看看编程语言理论家会提出的最基本的问题, 并研究"
      "如何回答它们. 在本书的第二部分, 我们将引入对于句法和语义进行实验的工具, "
      "通常其将有助于提出猜想和问题.")
   (P "现在我们使用第一章引入的句法和语义以刻画我们可以提出什么样的问题以及如何"
      "严格地回答这些问题. 第一节展现了如何用数学术语提出关于语言的问题. "
      "第二节用数学定理和证明刻画了回答, 引入了对于本书的剩余部分而言"
      "关键的证明技术.")
   (H4 "第2.1节 从问题到数学声明")
   (P "第一章定义了数个求值器, 包括" $B:eval0 ". 从编程语言实现者的角度来看, "
      "这个" $B:eval0 "函数使用了某种类似于机器的东西, 这个机器的初状态, 中间状态和"
      "终状态是" $B "表达式, 而指令是" $->>r "关系. 它启动程序, 等待直至机器到达一个"
      "终状态 (" $t:ms "或" $f:ms "), 然后报告这个最终结果.")
   (P "一个显然的问题是, 这个求值器是否对于某个固定的程序总是产生恰好一个结果. "
      "[译注: 从上下文看, 这里更确切的说法是产生至多一个结果, 因为产生恰好一个结果"
      "是更强的条件, 是说这个关系是一个total function.] "
      "用数学术语来说, 我们是在问这个求值器是否是一个函数. 如果的确如此, 那么若我们"
      "观察到对于相同的程序产生了两个不同的结果, 那么就知道这个求值器的实现是错的.")
   (P "现在回忆一下, 关系和函数都是序对的集合. 每个序对将一个输入和一个输出结合起来. "
      "关系和函数的不同之处在于, 后者对于任何输入最多只包含一个相关的序对. 因此, "
      "我们的第一个问题是在问以下声明是否成立:"
      (Blockquote
       "对于所有的" $B_0 ", " (&in (tu0 $B_0 $R_0) $B:eval1) "且"
       (&in (tu0 $B_0 $R_1) $B:eval1) "可以推出" (&= $R_0 $R_1) ".")
      "用函数记号的话, 那就变成了"
      (Blockquote
       "对于所有的" $B_0 ", " (&= (&B:eval1 $B_0) $R_0) "且"
       (&= (&B:eval1 $B_0) $R_1) "可以推出" (&= $R_0 $R_1) ".")
      "[译注: 读者应该将这种函数记号当成关系记号的等价物, 而不是函数真的一定产生了一个值, "
      "因为我们尚不知道这个关系对于每个输入是否都存在至少一个输出. 或者, 读者可以将"
      (&= (&B:eval1 $B_0) $R_0) "解释为" $B:eval1 "对于" $B_0 "有定义且" $R_0
      "是一个相应的输出. 另外, 之前的" $B:eval0 "到这里变成了" $B:eval1
      ", 译者感到行文有点不太连贯, 虽然这并不影响后面的呈现的正确性.]")
   (P "第1章不止定义了一个求值器, 实际上定义了两个. 从理想来说, 这两个定义应该引入的"
      "是相同的函数. 也就是说, 这两个求值器对于相同的程序应该产生同样的结果. 证明"
      "这样一个声明将允许我们根据需要交换地使用这两个定义. 例如, 当与一个数学老师"
      "争辩时, 我们可以使用" $B:eval1 "来刻画程序执行泛化了七年级代数. 而当与负责"
      $B "编程语言实现的软件工程师讨论" $B "的语义时, 或许我们应该使用" $B:eval0 ".")
   (P "因此, 我们的第二个问题关心的是等式求值器" $B:eval1 "和有向求值器" $B:eval0
      "之间的关系. 更确切地说, 我们想要知道它们是否是相同的函数:"
      (MB (&= $B:eval0 $B:eval1))
      "若将函数视为序对的集合, 那么问题就变成了这两个集合是否包含相同的元素:"
      (Blockquote
       "对于所有的" $B_0 "和" $R_0 ", " (&in (tu0 $B_0 $R_0) $B:eval1)
       "当且仅当" (&in (tu0 $B_0 $R_0) $B:eval0))
      "这个陈述一个大致上的翻译为"
      (Blockquote
       "对于所有的" $B_0 "和" $R_0 ", " (&= (&B:eval1 $B_0) $R_0)
       "当且仅当" (&= (&B:eval0 $B_0) $R_0))
      "[译注: 同样地, 我们应该将这种记号当作关系记号的等价形式.] 这个翻译的意思是说如果"
      $B:eval1 "对于" $B_0 "有定义 [译注: 有定义即存在以" $B_0 "为输入的相应序对] 而输出就是"
      $R_0 " [译注: 第一个问题解决了的话, 那么" $R_0 "就是唯一的], 那么" $B:eval0
      "对于" $B_0 "也有定义, 并且输出也是" $R_0 ", 反方向也该这么解释. "
      "[译注: 如果这个问题解决了的话, 那么" $B:eval1 "是(部分)函数就可以推出" $B:eval0
      "也是函数.]")
   (P "自然地, &quot;有定义&quot;这个表达暗示了另外一个问题, 即是否求值器对于所有可能的输入"
      "都产生一个输出. 这个陈述有一个直接的数学表达, 即"
      (Blockquote
       "对于所有的" $B ", 存在一个" $R "满足" (&in (tu0 $B_0 $R_0) $B:eval))
      "注意到, 这里我们丢掉了上标, 因为使用哪一个是无关紧要的.")
   (H4 "第2.2节 作为定理的回答")
   (P "既然我们已经理解了关于编程语言的句法和语义我们所能提问的最简单问题, 那么"
      "其回答显然应该是关于相应数学模型的定理. 我们的第一个定理是在陈述" $B:eval1
      "是一个(部分)函数.")
   ((theorem #:n "2.1")
    "如果" (&= (&B:eval1 $B_0) $R_1) "并且" (&= (&B:eval1 $B_0) $R_2)
    ", 那么" (&= $R_1 $R_2) ".")
   (P "从证明" $B:eval1 "的定理开始, 我们遵循历史路线, 首先建立了等式演算的"
      (B "一致性") ". 一旦我们证明了这两个定义描述了相同的关系, 那么我们就知道"
      $B:eval0 "也是一个函数. 同样根据传统, 在显然的时候我们去掉了定理中的"
      "量词前缀 (对于所有, 存在), 并且我们使用函数式记号来表达结果存在且"
      "是一个特定的值.")
   ((proof #:n "2.1")
    "为了证明该定理, 我们假设对于某" (&cm $B_0 $R_1 $R_2) "有"
    (&= (&B:eval1 $B_0) $R_1) "且" (&= (&B:eval1 $B_0) $R_2)
    ". 基于假设, 现在我们试图证明" (&= $R_1 $R_2) ". 根据" $B:eval1
    "的定义, 我们知道" (&=r $B_0 $R_1) "且" (&=r $B_0 $R_2)
    ". 注意一下, 这里是" $=r "而不是" $= ". 因为(根据定义)" $=r
    "是一个等价关系, " (&=r $R_1 $R_2) ". 为了到达我们想要的结果"
    (&= $R_1 $R_2) ", 即" $R_1 "和" $R_2 "是等同的, 我们必须研究"
    "这个等价关系的性质和计算的性质. (未完待续)")
   (P "前述证明的最后论证要求我们研究对于" (∈ $M $N $B) "而言的"
      (&=r $M $N) "的证明 (或者说证明树) 的形状. 既然" $=r
      "是单步规约关系" $->r "的自反, 对称, 传递闭包, 证明"
      (&=r $M $N) "的计算包含有一系列两个方向上的这样的单步规约:"
      (MB (&<- (&-> $M $L_1 $L_2)
               (&-> $L_3 $L_4) $..c $L_5 $N))
      "[译注: 原谅我将本来二维的图片用这样的单行的数学符号表达, 但是图片的意图应该清晰地传达了.] "
      "在这张图片里, 每个表达式" (∈ $L_i $B) "而每个箭头都表示序列中的相邻项之间的一个" $->r
      "关系. 形式地, " (&-> $L_i $L_j) "与" (&->r $L_i $L_j) "相对应.")
   (P "来源于这张图片的关键性的洞察在于或许可以重塑这个计算, 使得所有的规约步骤都是从" $M
      "到某个" $L "以及从" $N "到相同的" $L ". 换言之, 如果" (&=r $M $N) ", 那么存在一个表达式"
      $L "满足" (&->>r $M $L) "且" (&->>r $N $L) ":"
      (MB (&<- (&-> $M $dot $dot $..c $L)
               $..c $dot $dot $N))
      "如果我们能够证明对于两个等价的项总是存在这样一个" $L ", 那么一致性的证明就结束了.")
   ((proof #:n "2.1")
    "(剩下的证明) 回忆一下, 我们有"
    (MB (&=r $R_1 $R_2))
    "根据(尚未证明的)声明, 必然存在表达式" $L "满足"
    (MB (&->>r $R_1 $L) "且" (&->>r $R_2 $L))
    "但是, " $R "的元素仅有" $t:ms "和" $f:ms ", 它们不可能被规约为除了自身以外的"
    "任何其他表达式 [译注: 严格说来, 这也需要证明, 但是使用简单的结构归纳即可], 因此"
    (&= $L $R_1) "且" (&= $L $R_2) ", 这意味着" (&= $R_1 $R_2) ".")
   (P "根据前面这样的推理, 我们将" $B:eval1 "的一致性证明转换为了关于证明树的形状的声明, 即建立"
      (&=r $M $N) "之后我们能够重塑它的证明树. 这个关键性的洞察应该归功于Church和Rosser, "
      "它们运用这个想法分析了被称为" $lambda "演算的语言的一致性 (这是下一章的主题). "
      "据此, 这样的引理以他们的名字命名.")
   ((lemma #:n "2.2")
    (B "[" $=r "的一致性]:") " 如果" (&=r $M $N) ", 那么存在表达式" $L "满足"
    (&->>r $M $L) "且" (&->>r $N $L) ".")
   ((proof #:n "2.2")
    "既然给定了" (&=r $M $N) ", 以及" $=r "的定义归纳性地扩展" $->>r
    "为其对称传递闭包, 我们根据" (&=r $M $N) "的推导的结构 (也就是其证明树的结构) "
    "上的归纳来证明该引理:"
    (Ul (Li "基本情形:"
            (Ul (Li (B (&->>r $M $N) "情形") (Br)
                    "令" (&= $L $N) ", 然后声明成立.")))
        (Li "归纳情形:"
            (Ul (Li (B (&=r $M $N) "因为" (&=r $N $M) "情形") (Br)
                    "根据归纳, 存在" (&=r $N $M) "的一个" $L
                    ", 此即我们想要的" $L ".")
                (Li (B (&=r $M $N) "因为" (&=r $M $L_0) "且" (&=r $L_0 $N) "情形") (Br)
                    "根据归纳, 存在" $L_1 "满足" (&->>r $M $L_1) "且"
                    (&->>r $L_0 $L_1) ". 同样根据归纳, 存在" $L_2 "满足"
                    (&->>r $N $L_2) "且" (&->>r $L_0 $L_2)
                    ". 用图像表示的话, 我们有:"
                    (let ((M (make-pt 60 50))
                          (N (make-pt 420 50))
                          (L0 (make-pt 240 50))
                          (L1 (make-pt 150 200))
                          (L2 (make-pt 330 200)))
                      (Svg
                       #:attr* '((width "480")
                                 (height "270")
                                 (stroke "black")
                                 (style "display: block; margin: auto;"))
                       (Defs marker0)
                       (::dot M) (::dot N) (::dot L0) (::dot L1) (::dot L2)
                       (:FO M #:offset 'up $M) (:FO N #:offset 'up $N)
                       (:FO L0 #:offset 'up $L_0)
                       (:FO L1 $L_1) (:FO L2 $L_2)
                       (:FO (make-pt 150 50) #:offset 'up $=r)
                       (:FO (make-pt 330 50) #:offset 'up $=r)
                       (:arrow M L1) (:arrow L0 L1)
                       (:arrow N L2) (:arrow L0 L2)))
                    "现在假设每当" $L_0 "可以被规约为" $L_1 "和" $L_2
                    "时, 存在某个表达式" $L_3 "满足" (&->>r $L_1 $L_3) "且"
                    (&->>r $L_2 $L_3) ", 那么我们想要证明的声明的确成立, 因为"
                    (&->>r $M $L_3) "且" (&->>r $N $L_3) ".")))))
   (P "又一次, 我们基于 (modulo) 另外一个关于规约系统的声明的证明完成了证明. "
      "更确切地说, 我们假定如果遇到了左边的情况, 那么可以找到一个项" (&in $L^ $B)
      "使得我们能够构造右边的情况:"
      (let* ((L1 (make-pt 160 100))
             (M1 (make-pt 50 200))
             (N1 (make-pt 270 200))
             (offset-vec (make-vec 320 -75))
             (L2 (pt+ L1 offset-vec))
             (M2 (pt+ M1 offset-vec))
             (N2 (pt+ N1 offset-vec))
             (L^ (pt+ L2 (make-vec 0 200))))
        (Svg
         #:attr* '((width "640")
                   (height "270")
                   (stroke "black")
                   (style "display: block; margin: auto;"))
         (Defs marker0)
         (G #:attr* '((id "half"))
            (::dot L1) (::dot M1) (::dot N1)
            (:FO L1 #:offset 'up $L)  (:FO M1 #:offset 'left $M) (:FO N1 #:offset 'right $N)
            (:arrow L1 M1) (:arrow L1 N1))
         (:FO (make-pt 320 135) #:offset 'up (Mo "&DoubleLongRightArrow;"))
         (Use #:attr* '((href "#half") (transform "translate(320 -75)")))
         (::dot L^) (:FO L^ $L^) (:arrow M2 L^) (:arrow N2 L^)))
      "这个性质被称为" (B "菱形性质") ", 因为这张图片需要&quot;规约分支&quot;"
      "能够被补全为菱形的形状. 当规约概念 (例如" o.r ") 的兼容闭包的自反传递闭包"
      "满足这样的性质时, 其被称为" (B "Church-Rosser") "的.")
   ((lemma #:n "2.3")
    (B "[" $->>r "的Church-Rosser]:") " 如果" (&->>r $L $M) "且" (&->>r $L $N)
    ", 那么存在表达式" $L^ "满足" (&->>r $M $L^) "且" (&->>r $N $L^) ".")
   (P "既然" (&->>r $L $M) "由一系列" $->r "证明步骤构成, 那么检查后者是否也满足"
      "菱形性质是很自然的事情. 如果的确如此, 那么我们可以将" $->r "的小菱形组合起来获得"
      $->>r "的大菱形. 尽管菱形性质对于" $->r "并不那么成立, "
      "一个足够强的性质的确可以满足.")
   ((lemma #:n "2.4")
    (B "[" $->r "的类菱形性质]:") " 如果" (&->r $L $M) "且" (&->r $L $N) "对于"
    (&!= $M $N) "成立, 那么以下三个条件恰有一条可以满足:"
    (Ul (Li (&->r $M $N) ";")
        (Li (&->r $N $M) ";")
        (Li "存在" $L^ "使得" (&->r $M $L^) "且" (&->r $N $L^) "."))
    "换句话说, &quot;规约分支&quot;可以被补全为菱形, 或者存在&quot;三角形&quot;"
    "风格的补全方式.")
   ((proof #:n "2.4")
    "为了证明这条引理, 我们回忆一下" $->r "被归纳性地定义为" o.r
    "的兼容闭包. 因此, 自然的处理方式为假定" (&->r $L $M) "并于证明的结构上施行结构归纳:"
    (Ul (Li "基本情形:" (Br)
            (Ul (Li (B (&r $L $M) "情形") (Br)
                    "根据" o.r "的定义, 存在两种子情形."
                    (Ul (Li (B (&= $L (@dot $f:ms $B_0)) "且" (&= $M $B_0) "情形") (Br)
                            
                            )
                        (Li (B (&= $L (@dot $t:ms $B_0)) "且" (&= $M $t:ms) "情形") (Br)
                            
                            )
                        )
                    )
                )
            )
        (Li "对于归纳情形, 不失一般性, 假定" (&!in (tupa0 $L $N) $r:bold)
            ", 否则可以交换" $N "和" $M "." (Br)
            (Ul (Li ""
                    )
                )
            ))
    )
   (H3 "第3章 " $lambda "演算")
   (H4 "第3.1节 函数和" $lambda "演算")
   (P $lambda "演算的句法提供了一种简单而规整的方法来待应用的函数, 抑或是"
      "作为其他函数的输入和输出的函数. 在" $lambda "演算之中, 对于这种函数的"
      "刻画专注于从参数到结果的规则, 而忽略了对于函数的命名, 以及其定义域和"
      "陪域. 例如, 一位数学家会将某个集合" $A "上的恒等函数写成是"
      (MB (&: $f (Choice
                  ($A $-> $A)
                  ($x $\|-> $x))))
      "而若用" $lambda "演算的句法的话, 则为"
      
      )
   (H4 "第3.2节 " $lambda "演算: 句法和规约")
   (eqn* 
    ((&FV $X) $= (setE $X))
    ((&FV (LAM $X $M)) $= (&- (&FV $M) (setE $X)))
    ((&FV (APP $M_1 $M_2))
     $= (&cup (&FV $M_1) (&FV $M_2))))
   (P "我们给出" $FV "的一些例子:"
      (eqn*
       ((&FV $x) $= (setE $x))
       ((&FV (APP $x (APP $y $x)))
        $= (setE $x $y))
       ((&FV (LAM $x (APP $x $y)))
        $= (setE $y))
       ((&FV (APP $z (LAM $z $z)))
        $= (setE $z))))
   (eqn* 
    ((subst $X_1 $X_1 $M) $= $M)
    ((subst $X_2 $X_1 $M)
     $= (: $X_2 ", 其中" (&!= $X_1 $X_2)))
    ((subst (LAM $X_1 $M_1) $X_1 $M_2)
     $= (LAM $X_1 $M_1))
    ((subst (LAM $X_1 $M_1) $X_2 $M_2)
     $= (LAM $X_3 (subst (subst $M_1 $X_1 $X_3) $X_2 $M_2)))
    ($ $ (: "其中" (eqn* ($X_1 $!= $X_2)
                       ($X_3 $!in (&FV (LAM $X_1 $M_1)))
                       ($X_3 $!in (&FV $M_2)))))
    ((subst (APP $M_1 $M_2) $X $M_3)
     $= (APP (subst $M_1 $X $M_3) (subst $M_2 $X $M_3))))
   ((tcomment)
    "这里的想法在于避免" (Em "意外捕获")
    ", 最主要的是上面第四个分支, 也就是最复杂的那个, 它有三个条件. "
    "第一个条件是为了限制适用的情况, 和第三个分支进行区别. "
    "第二个条件和第三个条件才是实质性的, 它们需要联合看待. "
    "为了解决捕获问题 (虽然你可能还不知道这是什么?), "
    "但是解决捕获问题的方法, 正如第四个分支所示, 即换名. "
    "根据数学的经验, 我们已经知道换名不会改变函数的意义, 而"
    $lambda "演算中的" $alpha "变换正是为了形式化换名操作. "
    "这里换名的目的是为了避免" $lambda "绑定的新变量"
    $X_3 "捕获替换进去的" $M_2 "里的自由变量, 所以说要求"
    (&!in $X_3 (&FV $M_2)) ". 另外, 我们还需要保证"
    (&!in $X_3 (&FV (LAM $X_1 $M_1))) ", 这是" $alpha
    "变换本身的要求, 为了避免" $X_3 "去捕获"
    $M_1 "的自由变量, 当然" $X_1 "本身有没有被捕获是无所谓的, 所以"
    (&!in $X_3 (&FV (LAM $X_1 $M_1))) "即可. 在"
    (&!in $X_1 (&FV $M_2)) "的情况下, 令" $X_3
    "为" $X_1 "也可满足要求, 此时实质上并没有进行换名.")
   (P "显然, 替换的定义告诉我们替换不是一个函数. "
      "[译注: 有的材料区分预项和" $lambda "项, 通过"
      $alpha "等价. 在这种情况下, " $lambda
      "项上的替换的确会是函数.]")
   (P "最终, 为了定义" $lambda "演算的一般规约概念"
      $n:bold ", 我们首先定义三种简单的规约概念"
      (&cm $alpha $beta $eta) "."
      (eqn*
       ((LAM $X_1 $M) $alpha (LAM $X_2 (subst $M $X_1 $X_2)))
       ($ $ (: "其中" (&!in $X_2 (&FV $M))))
       ((APP (LAM $X $M_1) $M_2)
        $beta (subst $M_1 $X $M_2))
       ((LAM $X (APP $M $X)) $eta $M)
       ($ $ (: "其中" (&!in $X (&FV $M)))))
      (Ul (Li $alpha "更换了一个函数的形式参数的名字. "
              "它形式化了以下的想法: 形式参数的名字可以是任意的, "
              "只要其使用是一致的. 例如, " (LAM $x $x)
              "和" (LAM $y $y) "作为函数是不可区分的, "
              "它们都是接受一个参数然后直接返回这个参数.")
          (Li $beta "是主要的规约关系, 它形式化了函数应用于"
              "参数时的行为.")
          (Li $eta "编码了这样的事实, 如果一个函数" $f
              "取其参数并立即应用" $g "于该参数, 那么"
              $f "基本上和" $g "就是一样的."))
      "接着, 我们定义"
      (MB (&= $n:bold (&cup $alpha $beta $eta)))
      "这对于" $lambda "演算而言是最一般的规约概念.")
   (P "和往常一样, 我们定义" $->n "为" $n:bold
      "的兼容闭包, " $->>n "为" $->n
      "的自反传递闭包, " $=n "为" $->>n
      "的对称传递闭包. [译注: 原文误为对称闭包, 但的确是不对的.] "
      "另外, 我们定义" $->α ", " $->β ", " $->η
      "分别是" $alpha ", " $beta ", " $eta "的兼容闭包.")
   
   (H4 "第3.3节 编码布尔")
   (H4 "第3.4节 编码序对")
   (H4 "第3.5节 编码数字")
   (H4 "第3.6节 编码和错误")
   (H4 "第3.7节 递归")
   (H4 "第3.8节 一致性和正规形式")
   (P "任何等式系统的定义都会提出所谓的" (Q "一致性挑战")
      ". 一致性的意思是等式是有意义的. 特别是, 我们不应该能够"
      "证明任意两个项的相等性.")
   (P "原注: "
      )
   (H4 "第3.9节 正规形式和规约策略")
   (H4 "第3.10节 历史")
   
   (H3 "第4章 ISWIM")
   (P "在1950年代晚期和1960年代早期, 人们发现"
      "编程语言和" $lambda "演算的诸多方面"
      "之间存在联系. 这里的动机是多样的, "
      "有的只是想要刻画Algol 60的语义, "
      "而有的是试图通过已经得到良好理解的数学系统"
      "来理解整个编程语言领域的面貌. "
      "实际上, 人们希望学会如何系统设计编程语言.")
   (P "Landin是这群研究者之中的新星. "
      "他设计了一种被称为ISWIM的基于" $lambda
      "演算的语言, 并探索了其通过抽象或者说虚拟机器的"
      "应用和实现. 不严格地说, ISWIM几乎是一个函数式编程语言, "
      "Scheme编程语言是其最近的亲戚. "
      "[译注: 当然, 有的人可能认为Standard ML之类的语言比"
      "Scheme更加接近于ISWIM. 另外, ISWIM其实是一类语言, "
      "而不是一个语言.]")
   (P "虽然人们欣赏ISWIM的表达力并将其当作灵感的源泉, "
      "人们也意识到ISWIM和" $lambda "演算之间的关系"
      "并不那么直接. Plotkin最终设计了" $lambda
      "演算的一个变种, 其与ISWIM有着直接而自然的对应关系. "
      "他也表明了, 这种对应是一般性的原则. "
      "他将这原则具体化为了ISWIM的两个变种: "
      "call-by-name和call-by-value.")
   (P "这一章和接下来的一章里我们呈现了ISWIM, 其演算, 以及"
      "ISWIM和演算之间的关系. 具体来说, 本章呈现了ISWIM的"
      "句法和ISWIM的&quot;算术&quot;系统. 这包含一个求值器"
      "的定义以及对于这个求值器所定义的最广泛等式系统的探索. "
      "接下来的一章展现了" $lambda "演算的一个元定理"
      "是如何自然地定义了ISWIM的一个高层次的抽象机器的.")
   (H4 "第4.1节 ISWIM表达式")
   (P "ISWIM是一族编程语言, 根据字面常量和原始运算符"
      "而有所不同. ISWIM族的每一语言都扩展了" $lambda
      "演算的句法以基本常量的集合" $b "和一族多元函数"
      $o^n ".")
   (MB (deriv0 (&cm $M $N $L $K) $= $X
               $lv (LAM $X $M)
               $lv (APP $M $M)
               $lv $b
               $lv (APP $o^n $M $..h $M)))
   (P "这里的" $o^n "里的" $n "不是确定的, 其代表了原始运算符的"
      "参数数目 (元数), 并且我们额外限制这个应用的实际参数的数目"
      "必须等于相应的原始运算符的元数.")
   (P "为了明确起见, 我们使用"
      (eqn* ($b $= (setI (numeral $n) (∈ $n $ZZ)))
            ($o^1 $= (setE (Ms "add1") (Ms "sub1") (Ms "iszero")))
            ($o^2 $= (setE $+ $- $* $uarr)))
      "其中" (numeral $n) "是表示数字" $n "的numeral.")
   (P $FV "和" (subst $d* $d* $d*) "可以自然地扩展至新的句法上来."
      (eqn* ((&FV $b) $= $empty)
            ((&FV $X) $= (setE $X))
            ((&FV (LAM $X $M)) $= (&- (&FV $M) (setE $X)))
            ((&FV (APP $M_1 $M_2))
             $= (&cup (&FV $M_1) (&FV $M_2)))
            ((&FV (APP $o^n $M_1 $..h $M_n))
             $= (&cup (&FV $M_1) $..c (&FV $M_n))))
      (eqn* ((subst $b $X $M) $= $b)
            ((subst $X_1 $X_1 $M) $= $M)
            ((subst $X_2 $X_1 $M)
             $= (: $X_2 ", 其中" (&!= $X_1 $X_2)))
            ((subst (LAM $X_1 $M_1) $X_1 $M_2)
             $= (LAM $X_1 $M_1))
            ((subst (LAM $X_1 $M_1) $X_2 $M_2)
             $= (LAM $X_3 (subst (subst $M_1 $X_1 $X_3) $X_2 $M_2)))
            ($ $ (: "其中" (eqn* ($X_1 $!= $X_2)
                               ($X_3 $!in (&FV (LAM $X_1 $M_1)))
                               ($X_3 $!in (&FV $M_2)))))
            ((subst (APP $M_1 $M_2) $X $M_3)
             $= (APP (subst $M_1 $X $M_3) (subst $M_2 $X $M_3)))
            ((subst (APP $o^n $M_1 $..h $M_n) $X $M)
             $= (APP $o^n (subst $M_1 $X $M) $..h
                     (subst $M_n $X $M)))))
   (H4 "第4.2节 使用ISWIM进行计算")
   (P "受到Algol 60的call-by-value参数传递机制的启发, Landin以类似的方式"
      "设计了ISWIM的函数. 也就是说, 在ISWIM之中, 一个函数应用在函数取得控制之前"
      "对于其实际参数求值. 这么做可以消除在函数体中对于一个参数多次求值或者"
      "追踪一个参数是否已经被求值过了的开销. 这也排除了纯粹" $lambda
      "演算中的一些incongruities, 例如表达式"
      (MB (APP (LAM $x (numeral $1))
               (APP (Ms "sub1") (LAM $y (LAM $x $y)))))
      "将" (Ms "sub1") "应用于一个并非numeral的东西 [译注: 从句法上说], "
      "会被call-by-value的参数传递机制发现. 而在纯粹" $lambda
      "演算之中, 这种错误应用 (mis-application) 因为可以直接施行" $beta
      "规约而不会被发现.")
   (P "因为ISWIM的函数只接受被求值了的参数, 那么我们必须首先规定何谓"
      (B "值") ". 从直觉上说, 值是对其求值可以立即知道结果的项. "
      "[译注: 一般我们在操作语义之中采用这种观点, 在其他场合, 值还有另外的含义.] "
      "或者说, 值是任意的表达式在规约过程之中的目的地. 我们定义值的集合如下:"
      (MB (deriv0 (&cm $V $U $W) $= $b
                  $lv $X
                  $lv (LAM $X $M)))
      "基本的常量当然是值, " $lambda "抽象也是值, 不论其体的形状如何. "
      "因此, Landin将函数作为第一级对象的想法引入了编程语言的研究之中. "
      "[译注: 至于第一级对象这个想法或者说术语, 最早提出者应该是Christopher Strachey, "
      "他也算是指称语义学的奠基人之一.]")
   (P "应用不可能是一个值, 因为它就像刻画计算的发动机一样. 变量也是值, "
      "不过这值得额外的解释. " $lambda "演算中的变量有且只有一种目的: "
      "作为形式参数 (formal argument) 或者说" (B "parameter")
      ". 对于一个程序员而言, parameter的作用在于为函数的实际参数充当占位符号 "
      "(place holder). 既然ISWIM的函数只能消化值, 那么变量就总是代表值. "
      "因此, 我们暂时也将变量看成是值. 第4.7节回顾了这个决定.")
   (P "基于对于值的刻画, 现在我们可以定义ISWIM的基本规约概念."
      (eqn* ((APP (LAM $X $M) $V)
             $beta_v:sans-serif
             (subst $M $X $V)))
      "这个关系有别于" $beta ", 因为参数必须是" $V "的成员. "
      "在ISWIM中, 你不能将任意的项替换进函数的体里.")
   (P "限制参数必须是一个值迫使参数表达式的规约在对于函数应用求值之前进行. 例如, "
      (APP (LAM $X (numeral $1)) (APP (Ms "sub1") (LAM $y $y)))
      "就不能被规约至" (numeral $1) ", 因为"
      (&!in (APP (Ms "sub1") (LAM $y $y)) $V) ".")
   (P "除了函数应用, 计算ISWIM的系统还必须考虑原始运算. "
      "既然ISWIM是随着常量集和原始运算符集的改变而变化的"
      "一族语言, 那么对于这些集合我们需要一个足够一般的"
      "规约概念, 因而我们引入了一个部分" $delta "函数:"
      (MB (func $delta (&c* $o^n $b $..c $b) $V))
      "也就是说, " $delta "将" $o^n "的元素和" $n
      "个基本常量映射至一个值. 添加" $delta
      "至演算引入了第二种规约概念.")
   (P "对于我们之前约定好了的常量和运算符, 我们选择了以下具体的"
      $delta "函数."
      (eqn*
       ((appl $delta (Ms "add1") (numeral $m))
        $= (numeral (&+ $m $1)))
       ((appl $delta (Ms "sub1") (numeral $m))
        $= (numeral (&- $m $1)))
       ((appl $delta (Ms "iszero") (numeral $0))
        $= (LAM $x (LAM $y $x)))
       ((appl $delta (Ms "iszero") (numeral $n))
        $= (LAM $x (LAM $y $y)) (: ",&nbsp;" (&!= $n $0)))
       ((appl $delta $+ (numeral $m) (numeral $n))
        $= (numeral (&+ $m $n)))
       ((appl $delta $- (numeral $m) (numeral $n))
        $= (numeral (&- $m $n)))
       ((appl $delta $* (numeral $m) (numeral $n))
        $= (numeral (&d* $m $n)))
       ((appl $delta $uarr (numeral $m) (numeral $n))
        $= (numeral $m^n)))
      
      )
   (H4 "第4.3节 " $alpha ", " $eta "和商")
   
   (H3 "第5章 抽象句法机器")
   (P "ISWIM求值器的定义是优雅的, 但是难于实现. "
      "为了确定一个程序的结果, 程序员可以自由地"
      "应用等式系统的规则以任意的顺序, 以任意的方向, "
      "在程序的任意位置. 然而, 这种灵活性并非将"
      "求值器实现为(元)程序的良好基础. 根据一致性定理, "
      
      )
   (H3 "第6章 抽象寄存器机器")
   (P "依照所处的上下文, 求值器的效率可能并不重要. 例如, 如果"
      )
   (H4 "第6.1节 CC机器")
   (P "前一章的句法机器的每个求值循环中都需要执行三个步骤. "
      "首先, 其需要判断程序是否是一个值; 如果是的话, 那么"
      "机器就停止了. 其次, 或者说程序并非一个值, 那么机器"
      "会将程序分成一个求值上下文" $E "和一个应用"
      (APP $U $V) "或者" (APP $o^n $V_1 $..h $V_n)
      ". 最后, 如果这个应用是一个" $beta_v:sans-serif
      "或者" $delta "可规约表达式, 那么机器会构造contractum "
      $M ", 然后填充求值上下文" $E "以" $M
      ". 这一步的结果是下一个机器状态, 于是求值循环又重新再来.")
   (P "这个机器显见的低效之处在于反复将程序分成一个求值上下文和"
      "一个可规约表达式. "
      )
   (H4 "第6.2节 SCC机器")
   
   (H4 "第6.3节 CK机器")
   
   (H4 "第6.4节 CEK机器")
   
   (H4 "第6.5节 历史")
   
   (H3 "第7章 尾调用和更多的空间节省")

   (H3 "第8章 控制: 错误, 异常和延续")

   (H3 "第9章 状态: 命令式赋值")

   (H3 "第10章 简单类型ISWIM")

   
   ))