#lang racket
(provide cat_awodey.html)
(require SMathML)
(define (n2s n)
  (~r n #:precision 2))
(define arrow-head
  (Marker
   #:attr*
   '((id "arrow-head")
     (viewbox "0 0 10 10")
     (refX "5")
     (refY "5")
     (markerWidth "6")
     (markerHeight "6")
     (orient "auto-start-reverse"))
   (Path #:attr* '((d "M 0 2 L 6 5 L 0 8 z")))))
(define-struct vec2
  (x y)
  #:transparent)
(define-struct pt2
  (x y)
  #:transparent)
(define (vec2* k v)
  (make-vec2
   (* k (vec2-x v))
   (* k (vec2-y v))))
(define (pt2+ p v)
  (make-pt2
   (+ (pt2-x p) (vec2-x v))
   (+ (pt2-y p) (vec2-y v))))
(define (pt2- p1 p2)
  (make-vec2
   (- (pt2-x p1) (pt2-x p2))
   (- (pt2-y p1) (pt2-y p2))))
(define ((lerp t) p1 p2)
  (pt2+ p1 (vec2* t (pt2- p2 p1))))
(define zero-vec2
  (make-vec2 0 0))
(define (:FO position #:offset [offset zero-vec2] #:width [width 100] #:height [height 30] . x*)
  (define pos
    (pt2+ position offset))
  (keyword-apply
   ForeignObject
   '(#:attr*)
   `(((x ,(n2s (pt2-x pos)))
      (y ,(n2s (pt2-y pos)))
      (width ,(n2s width))
      (height ,(n2s height))))
   x*))
(define (:line start end)
  (define x1 (n2s (pt2-x start)))
  (define y1 (n2s (pt2-y start)))
  (define x2 (n2s (pt2-x end)))
  (define y2 (n2s (pt2-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1)
          (x2 ,x2)
          (y2 ,y2))))
(define (:arrow start end)
  (set-attr*
   (:line start end)
   'marker-end
   "url(#arrow-head)"))
(define (:arrow-prop start end #:prop [prop 0.7])
  (define t (/ (- 1 prop) 2))
  (define start^ ((lerp t) start end))
  (define end^ ((lerp (- 1 t)) start end))
  (:arrow start^ end^))
(define (set-dotted line)
  (set-attr* line 'stroke-dasharray "2 2"))
(define (concat . x*)
  (apply : x*))
(define (forget x)
  (&abs x))
(define (free-monoid A)
  (app $M A))
(define $A^* (&* $A))
(define $dummy (Mi "&minus;"))
(define $empty-word (Mi "-"))
(define $*:id (Mi "&#8270;"))
(define $g_* (_ $g $*))
(define (long-arrow length)
  (^^ $-> (Mspace #:attr* `((width ,length)))))
(define pad
  (case-lambda
    ((x)
     (: (Mspace #:attr* `((width "2em")))
        x
        (Mspace #:attr* `((width "2em")))))
    ((x length)
     (: (Mspace #:attr* `((width ,length)))
        x
        (Mspace #:attr* `((width ,length)))))))
(define (slice Cat C)
  (&/ Cat C))
(define (coslice C Cat)
  (&/ C Cat))
(define $op (Mi "op"))
(define (&op C)
  (^ C $op))
(define $<=_A (_ $<= $A))
(define $<=_B (_ $<= $B))
(define $& (Mo "&amp;"))
(define $Star (Mo "&Star;"))
(define $rlarr (Mo "&rlarr;"))
(define $\; (Mo ";"))
(define $d*_M (_ $d* $M))
(define $d*_N (_ $d* $N))
(define $cong (Mo "&cong;"))
(define $rrarr (Mo "&rrarr;"))
(define-infix*
  (&<=_A $<=_A)
  (&<=_B $<=_B)
  (&& $&)
  (&rlarr $rlarr)
  (&\; $\;)
  (&d*_M $d*_M)
  (&d*_N $d*_N)
  (&cong $cong)
  (&rrarr $rrarr))
(define $QQ^+ (^ $QQ $+))
(define (powerset X)
  (app $P:script X))
(define $.
  (Mo "." #:attr* '((lspace "0"))))
(define (card A)
  (&abs A))
(define (open X)
  (app $O:script X))
(define (∃ Q P)
  (: $exists Q $. P))
(define (_cm A . x*)
  (_ A (apply &cm x*)))
(define (_prime A x)
  (_^ A x $prime))
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
(define (Arrow f A B)
  (&: f (&-> A B)))
(define (map-toggle flag proc lst)
  (cond ((null? lst) '())
        (flag (cons (proc (car lst))
                    (map-toggle (not flag) proc (cdr lst))))
        (else (cons (car lst)
                    (map-toggle (not flag) proc (cdr lst))))))
(define (ARROW . arg*)
  (apply : (map-toggle
            #f
            (lambda (f) (^^ $-> f))
            arg*)))
(define (Functor F C D)
  (&: F (&-> C D)))
(define $Hom (Mi "Hom"))
(define Hom
  (case-lambda
    ((X Y) (appl $Hom X Y))
    ((C X Y) (appl (_ $Hom C) X Y))))
(define $Aut (Mi "Aut"))
(define Aut
  (case-lambda
    ((X) (app $Aut X))
    ((C X) (app (_ $Aut C) X))))
(define app*
  (case-lambda
    ((f x) (app f x))
    ((f g . arg*) (app f (apply app* g arg*)))))
(define-syntax-rule (define-simple* (&id $id str) ...)
  (begin
    (define $id (Mi str))
    ...
    (define (&id x) (app $id x))
    ...))
(define-simple*
  (&range $range "range")
  (&dom $dom "dom")
  (&cod $cod "cod")
  (&darr $darr "&darr;"))
(define-@lized-op*
  (@compose &compose)
  (@_cm _cm))
(define (make-cat str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define-syntax-rule (define-cat* (id str) ...)
  (begin
    (define id (make-cat str))
    ...))
(define-cat*
  (Sets "Sets")
  (Pos "Pos")
  (Rel "Rel")
  (Cat0 "0")
  (Cat1 "1")
  (Cat2 "2")
  (Cat3 "3")
  (CatC "C")
  (CatD "D")
  (Cat "Cat")
  (Dis "Dis")
  (Mon "Mon")
  (Groups "Groups")
  (CatP "P")
  (Graphs "Graphs"))
(define (free-category G)
  (app CatC G))
(define $fin (Mi "fin"))
(define Sets_fin (_ Sets $fin))
(define Sets_* (_ Sets $*))
(define CatC^-> (^ CatC $->))
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign "left center right")
                (width "100%"))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))
(define (H3. #:attr* [attr* '()] #:id [id #f] #:switch? [switch? #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite
                     #:level 3 #:id id #:switch? switch?)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (format "~a.~a" (cadr section) index)))
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
(define UMP:free-monoid.svg
  (Svg
   #:attr* '((width "480")
             (height "300")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs arrow-head)
   (:FO (make-pt2 0 0) (Em "在" Mon "之中:"))
   (:FO (make-pt2 120 30) (free-monoid $A))
   (:FO (make-pt2 360 30) $N)
   (set-dotted
    (:arrow-prop (pt2+ (make-pt2 120 30)
                       (make-vec2 20 10))
                 (pt2+ (make-pt2 360 30)
                       (make-vec2 20 10))))
   (:FO ((lerp 1/2) (make-pt2 120 30) (make-pt2 360 30))
        #:offset (make-vec2 10 -10)
        (OverBar $f))
   (:FO (make-pt2 0 60) (Em "在" Sets "之中:"))
   (:FO (make-pt2 120 90)
        #:offset (make-vec2 -4 0)
        (forget (free-monoid $A)))
   (:FO (make-pt2 360 90)
        #:offset (make-vec2 -4 0)
        (forget $N))
   (:FO (make-pt2 120 280)
        #:offset (make-vec2 14 0)
        $A)
   (:arrow-prop (pt2+ (make-pt2 120 90)
                      (make-vec2 20 10))
                (pt2+ (make-pt2 360 90)
                      (make-vec2 20 10)))
   (:arrow-prop #:prop 0.8
                (pt2+ (make-pt2 120 280)
                      (make-vec2 20 8))
                (pt2+ (make-pt2 120 90)
                      (make-vec2 20 8)))
   (:arrow-prop #:prop 0.85
                (pt2+ (make-pt2 120 280)
                      (make-vec2 18 10))
                (pt2+ (make-pt2 360 90)
                      (make-vec2 18 10)))
   (:FO ((lerp 1/2) (make-pt2 120 90) (make-pt2 360 90))
        #:offset (make-vec2 8 -10)
        (forget (OverBar $f)))
   (:FO ((lerp 1/2) (make-pt2 120 90) (make-pt2 120 280))
        #:offset (make-vec2 6 0)
        $i)
   (:FO ((lerp 1/2) (make-pt2 120 280) (make-pt2 360 90))
        #:offset (make-vec2 18 10)
        $f)))
(define cat_awodey.html
  (TnTmPrelude
   #:title "范畴论笔记"
   #:css "styles.css"
   (H1. "范畴论笔记")
   (P "现在的是重写的版本, 但在某种意义上更接近于翻译"
      "Category Theory (Steve Awodey) 而非笔记了.")
   (H2 "翻译术语讨论")
   (P "翻译这样一本关于范畴论的书籍, 不论如何在术语方面都值得斟酌再三, "
      "我将我的一些选择写在这里.")
   (P "对于collection, 在某种意义上它其实就是集合论里的class, "
      "只不过在使用时作者又不想太过形式化而已. 也就是说, collection可能指的是set, "
      "也可能指的是proper class. 我有意将collection翻译成了" (Q "合集")
      ", 许多书籍将其同set一样也翻译为集合, 这不免有时给读者带来疑惑. "
      "当然, 在本书的第一章, 作者Steve Awodey是有意避免一开始就变得technical的, "
      "然而还是不免在第一章的最后讨论了一些数学基础方面的事情.")
   (P "英文里某些简单表达在中文里没有那么一致的对应, 我有点倾向于机械化翻译, "
      "以使读者能够辨识出来. 当然了, 不机械化的翻译也有很多. "
      "首先是" (Q "either...or...") ", 虽然我将其翻译为了"
      (Q "要么..., 要么...") ", 但是这并不意味着这二者只居其一, "
      "实际上就是" (Q "或者") "的意思. 其次是" (Q "in particular, ...")
      ", 它的实际用法相当于接下来要举一个符合情况的但是具体且特殊的例子, "
      "我以前总是将其翻译为" (Q "特别地, ...") ", 但是并不通顺, 而且也不容易理解. "
      "现在有时我也会意译, 例如将其翻译为" (Q "举一个例子, ...") ".")
   (P (Q "underlying set") "是容易理解的, 大概就是某个结构或者范畴的集合部分, "
      "或者说遗忘了结构而剩下的集合. 不过, 我觉得并不容易翻译妥当, 最终我选择翻译为"
      (Q "潜在集") ", 我想这不至于引起什么误解. 类似地, 一个结构化集合之间的同态, "
      "在遗忘了结构之后, 可以成为一个" (Q "潜在函数") ".")
   (H2 "前言")
   (P "当我们已经有了Mac Lane的" (Em "Categories for the Working Mathematician")
      ", 为什么还要写一本新的范畴论教科书呢? "
      "简而言之, 因为Mac Lane的书是为工作的数学家准备的. "
      "在范畴论渗透进各种各样的领域并出现在课程里30年之后, "
      "现在需要的是一本供所有人阅读的书籍.")
   (P "这本书成长于我过去十年间在CMU教授的范畴论课程. "
      "在这段时间里, 我已经为计算机科学, 数学, 逻辑学的本科生和研究生"
      "教授了无数的讲座课程或者高级讨论班. "
      "基于本书材料的讲座课程由15个星期每周两节的90分钟讲座构成. "
      "这些讲座(材料)的胚芽是我自己的研究生笔记, "
      "来源于Mac Lane在芝加哥大学所教授的范畴论课程. "
      "在教授我自己的课程时, 我很快发现CMU这里的混合的学生群体"
      "和芝加哥大学的数学研究生的需求非常不同, "
      "而我寻找满足这些需求的合适教科书的过程表明"
      "现有的文献和学生的需求之间存在严重的沟壑. "
      "我的讲义随着时间逐渐演化以填补这个沟壑, "
      "补充并最终替代了我尝试使用的诸多教材.")
   (P "我的课程的学生往往没有什么数学背景, "
      "只上过一门离散数学, 以及一些微积分或者线性代数"
      "或者是一两门逻辑学课程. 然而, 当学生最终成为计算机科学或者逻辑学的研究者时, "
      "许多人都需要熟悉范畴论的基本概念, 而且是在没有接受许多深入的数学训练的情况下. "
      "数学系的本科生其实也是类似的: 在数学上很有天分, "
      "并且因其与他们之后所需要学习的东西有着明显的联系而备受鼓舞, "
      "尽管如此还是不能跟着Mac Lane的书, 因为他们仍然缺乏必要的数学准备. "
      "我的大多数学生甚至还不知道自由群是什么, "
      "所以说当他们知道这是伴随的一个例子时也不会受到启发.")
   (P "因此, 这本书意在成为范畴论的教科书和参考, "
      "不仅面向数学系学生, 而且也面向计算机科学, 逻辑学, 语言学, 认知科学, 哲学"
      "以及任何需要用到范畴论的领域的研究者和学生. "
      "对于我来说挑战在于能够使得基本定义, 定理, 以及证明技术被这样的读者群体理解, "
      "因此无法假定读者熟悉范畴论在代数学和拓扑学中的主要 (或者说至少是原初的) 应用. "
      "然而这不意味着我会在真空中建立主题, 简单跳过例子和应用. "
      "这种抽象层次的材料在没有应用和例子使之变得鲜活的情况下直接无法理解.")
   (P "面对这种两难的境地, 我采取了这样的策略. 首先从最开始细致地建立几个基本的例子, "
      "例如偏序集和幺半群, 然后带着这些例子一起, 并在整本书里一直使用它们. "
      "这在教学上有诸多值得提及的优点: 偏序集和幺半群本身就是特殊种类的范畴, "
      "并且在某种意义上代表了一个一般性的范畴所具有的两个" (Q "维度")
      " (对象和箭头). 诸多范畴里出现的现象当作来源于偏序集或者幺半群"
      "的什么东西的泛化来理解是最好的. 从另一个角度来说, "
      "偏序集 (和单调映射) 和幺半群 (和同态) 的范畴提供了两个更进一步的"
      "而且又相当不同的范畴的例子, 其可以用来考虑各种各样的概念. "
      "例如, 极限的概念既可以在一个给定的偏序集里考虑, 也可以在偏序集的范畴里考虑.")
   (P "当然了, 许多偏序集和幺半群之外的其他例子也有处理. "
      "例如, 关于群和范畴的一章建立了群论的最初几步, 直至核, 商群, 以及同态定理, "
      "作为等化子和余等化子的例子. 这里, 以及偶尔别的什么地方 (例如与Stone对偶联系起来), "
      "我有意涵盖了更多的一点数学, 超出了用于刻画手头概念所严格必要的范围. "
      "我的想法在于当一些学生将要参加更加高等的数学课程时, "
      "这些或许是离他们最近的, 因此他们应该能够通过收获一些低垂的果实"
      "来从学习范畴论的努力中获益.")
   (P "尽管所需要的数学预备知识比Mac Lane的书要低得多, "
      "(我希望)标准的严格性并没有妥协. "
      "所有重要命题和定理的完整证明都已给出, "
      "只是偶尔常规的引理会留作练习 (然后它们通常被列在一章的最后). "
      "材料的选取是容易的, 存在必须涵盖的标准核心: "
      "范畴, 函子, 自然变换, 等价, 极限和余极限, 函子范畴, "
      "可表, Yoneda引理, 伴随, 以及单子. "
      "这近乎填满了一个课程. 这里所涵盖的唯一算是" (Q "可选")
      "的话题是笛卡尔闭范畴和" $lambda "演算, "
      "但是这对于计算机科学家, 逻辑学家, 语言学家而言又是必要的. "
      "一些显然更加深入的话题被有意地忽略了: 2-范畴, 意象 (topos) (任何深度上), "
      "幺半范畴. 这些话题在Mac Lane中得到处理, "
      "而学生在完整参与这个课程之后应该有能力阅读此书.")
   (H2. "范畴")
   (H3. "引论")
   (P (Em "什么是范畴论?") " 作为第一次近似, "
      "我们可以说范畴论是对于" (Em "函数的代数")
      "的研究. 正如群论是对于集合的置换或者"
      "几何对象的对称变换这一类系统的思想的抽象, "
      "范畴论来源于一些对象之间的函数所构成的系统的理念.")
   (P "我们将复合" (&compose $g $f) "想成是某种函数"
      $f "和" $g "的" (Q "积") ", 然后考虑由函数的合集所引申出的那种抽象"
      (Q "代数") ". 一个范畴正是这样一种" (Q "代数")
      ", 其由对象" (&cm $A $B $C $..h) "和箭头"
      (&cm (Arrow $f $A $B) (Arrow $g $B $C) $..h)
      "构成, 而且箭头在函数复合之下封闭并满足一些对于函数复合而言"
      "十分典型的特定条件. 精确的定义在本章的之后给出.")
   todo.svg
   (P "作为抽象代数的分支, 范畴论的发明遵循着Felix Klein的"
      (Em "Erlanger纲领") "的传统, 作为一种基于数学结构之间的"
      (Q "可容许 (admissible) 变换") "来对于不同数学结构进行研究和刻画的方法. "
      "范畴这种一般性的概念提供了对于" (Q "保持结构的变换")
      "这一想法的刻画, 并由此提供了对于承载这样的变换的一类结构的刻画.")
   (P "这个学科的历史发展大致如下:"
      (Ul (Li "1945: Eilenberg和Mac Lane的"
              (Q "General theory of natural equivalences")
              "是原初的论文, 这是对于范畴论的最早描述.")
          (Li "1940年代末: 主要的应用最初是代数拓扑, "
              "特别是同调论和抽象代数.")
          (Li "1950年代: A. Grothendieck et al.开始使用范畴论, "
              "在代数几何领域取得了极大的成功.")
          (Li "1960年代: F. W. Lawvere和其他人开始应用范畴论于逻辑学, "
              "揭示了一些深刻而令人意外的联系.")
          (Li "1970年代: 范畴论的应用已经出现于计算机科学, "
              "语言学, 认知科学, 哲学, 以及其他诸多领域.")))
   (P "关于这个领域的一件非常引人注目的事情在于其有着如此广阔的应用. "
      "事实上, 它就像集合论一样是一种普遍性的数学语言. "
      "由于拥有各种各样的应用, 范畴论往往也能揭示不同领域之间的特定联系"
      "&mdash;&mdash;就像逻辑和几何. 举个例子, " (Em "伴随函子")
      "这一重要概念在逻辑中以存在量化子出现, "
      "在拓扑中则以沿着连续函数取像 (image) 的运算出现. "
      "从范畴的角度而言, 这些本质上是相同的操作.")
   (P "实际上, 伴随函子应该是读者学习本书时所应该掌握的主要东西之一. "
      "这个纯粹范畴论的概念已经被证明是第一级的概念工具&mdash;&mdash;"
      "其重要性可与连续函数的想法比肩.")
   (P "实际上, 正如拓扑空间的想法起源于其与连续函数的联系, "
      "范畴的概念是为了定义什么是函子而产生的, "
      "至少根据范畴论的发明者之一的说法是这样. "
      "故事还可以继续下去, 函子的概念是为了定义自然变换而产生的. "
      "我们或许还可以继续下去, 自然变换是为了定义伴随而生的:"
      (Table #:attr* '((style "margin: auto; text-align: center;"))
             (Tr (Td "范畴"))
             (Tr (Td "函子"))
             (Tr (Td "自然变换"))
             (Tr (Td "伴随")))
      "诚然如此, 而这给出了本书的大纲.")
   (P "在正式学习之前, 或许我们应该问问为什么范畴论有着如此广泛而深远的应用. "
      "既然我们说过其实函数的抽象理论, 所以说答案就是:"
      (Div #:attr* '((style "text-align: center;"))
           (Em "函数无处不在!"))
      "并且哪里有函数, 哪里就有范畴. 的确如此, 也许这个学科应该叫做"
      (Em "抽象函数论") ", 或者更好的名字是: " (Em "箭术(archery)") ".")
   (H3. "集合之间的函数" #:id "functions-of-sets")
   (P "我们从考虑集合之间的函数开始. "
      "我不打算在这里说明什么是函数, 就像我也不会说明什么是集合一样, "
      "转而我们将会默认读者拥有对于这些术语的实际可行的理解. "
      "它们实际上可以使用范畴论来" (Em "定义")
      ", 但是这并非我们在这里的目的.")
   (P "令" $f "是从一个集合" $A "到另一个集合" $B "的一个函数, 我们记作"
      (MB (Arrow $f $A $B) ".")
      "以显式的语言来说, 这意味着" $f "定义在整个的" $A "上而"
      $f "的所有值都在" $B "之中. 以集合论术语而言, 就是"
      (MB (&sube (&range $f) $B) ".")
      "现在设我们也有一个函数" (Arrow $g $B $C) ","
      todo.svg
      "那么存在着一个作为复合的函数"
      (Arrow (&compose $g $f) $A $C) ", 由"
      (MBL "(1.1)"
           (&cm (&= (app (@compose $g $f) $a)
                    (app $g (app $f $a)))
                (∈ $a $A)))
      "给出. 现在这个函数复合的运算" (Q $compose)
      "是结合性的, 在于如果我们还有一个函数"
      (Arrow $h $C $D)
      todo.svg
      "并构成" (&compose $h $g) "和" (&compose $g $f)
      ", 那么我们像以上图表所指示的那样比较"
      (&compose (@compose $h $g) $f) "和"
      (&compose $h (@compose $g $f))
      ". 实际上, 这两个函数总是等同的,"
      (MB (associate &compose $h $g $f))
      "因为对于任意的" (∈ $a $A) ", 我们有"
      (MB (&= (app (@compose (@compose $h $g) $f) $a)
              (app* $h $g $f $a)
              (app (@compose $h (@compose $g $f)) $a)))
      "这使用了(1.1).")
   (P "顺便值得一提的是, 两个函数相等的含义: "
      "对于每个参数, 它们有着相同的值.")
   (P "最后, 注意到每个集合" $A "都拥有一个恒等函数"
      (MB (Arrow $1_A $A $A))
      "其由"
      (MB (&= (app $1_A $a) $a))
      "给出.")
   (P "这些恒等函数表现为复合运算" $compose "的" (Q "单位元")
      ", 抽象代数意义下的. 也就是说, 对于任意的"
      (Arrow $f $A $B) ", 我们有"
      (MB (&= (&compose $f $1_A)
              $f
              (&compose $1_B $f)) "."))
   todo.svg
   (P "所有这些关于集合之间的函数的性质是我们想要对于函数的"
      (Em "抽象") "概念所考虑的: 复合与恒元. "
      "因此, 可以说现在我们想要" (Q "抽象掉")
      "其他所有东西, 这正是由以下定义所完成的.")
   (H3. "范畴的定义")
   ((Definition)
    "一个" (Em "范畴") "由以下数据构成:"
    (Ul (Li (Em "对象") ": " (&cm $A $B $C $..h))
        (Li (Em "箭头") ": " (&cm $f $g $h $..h))
        (Li "对于每个箭头" $f ", 可以给出两个对象"
            (MB (&cm (&dom $f) (&cod $f)))
            "其被称为" $f "的" (Em "定义域(domain)")
            "和" (Em "陪域(codomain)") ". 我们记"
            (MB (Arrow $f $A $B))
            "以指明" (&= $A (&dom $f)) "而"
            (&= $B (&cod $f))
            ". {译注: 定义域和陪域是一个箭头所内蕴的信息.}")
        (Li "给定箭头" (Arrow $f $A $B) "和" (Arrow $g $B $C)
            ", 即满足"
            (MB (&= (&cod $f) (&dom $g)))
            "那么可以给出一个箭头"
            (MB (Arrow (&compose $g $f) $A $C))
            "其被称为" $f "和" $g "的"
            (Em "复合(composite)") ".")
        (Li "对于每个对象" $A ", 可以给出一个箭头"
            (MB (Arrow $1_A $A $A))
            "其被称为" $A "的"
            (Em "恒等箭头(identity arrow)") "."))
    "这些数据必须满足以下法则:"
    (Ul (Li "结合性:"
            (MB (associate &compose $h $g $f))
            "对于所有"
            (&cm (Arrow $f $A $B)
                 (Arrow $g $B $C)
                 (Arrow $h $C $D))
            "成立.")
        (Li "单位元:"
            (MB (&= (&compose $f $1_A)
                    $f
                    (&compose $1_B $f)))
            "对于所有" (Arrow $f $A $B) "成立."))
    "一个范畴可以是满足这个定义的" (Em "任意东西")
    "&mdash;&mdash;并且很快我们就要拥有大量的例子. "
    "暂时我想要强调的是, 和" (Ref "functions-of-sets")
    "中的情况不同, 对象不必是集合, 箭头也不必是函数. "
    "一个范畴在这种意义下是函数或者说" (Q "箭头")
    " (有时也称为" (Q "态射") ") 的一个" (Em "抽象")
    "代数, 以复合运算" (Q $compose)
    "为原语. 如果你熟悉群, 那么你或许可以将一个范畴"
    "想成是某种一般化了的群.")
   (H3. "范畴的例子")
   (Ol (Li "我们已经遇到过了集合和函数的范畴" Sets
           ". 这里也有另一个范畴"
           (MB Sets_fin)
           "其由所有的有限集合以及其间函数构成." (Br)
           "诚然, 还有许多像这样的范畴, "
           "其由限制能够成为对象的集合与能够成为箭头的函数给出. "
           "例如, 取有限集合为对象而单射函数为箭头. "
           "既然单射函数的复合仍是单射函数, "
           "并且恒等函数是单射的, "
           "所以说这也给出了一个范畴." (Br)
           "如果我们取集合为对象而取满足以下条件的函数"
           (Arrow $f $A $B) "为箭头: 对于每个"
           (∈ $b $B) ", 子集"
           (MB (&sube (app (inv $f) $b) $A))
           "至多只有两个元素 (而不是一个), "
           "这仍然是一个范畴吗? "
           "{译注: 显然不是, 因为这样的函数对于复合运算不封闭.} "
           "若取" (app (inv $f) $b) "有限的函数呢? "
           "无限的情况呢? 存在着许多这样的"
           "由集合与函数构成的限制范畴.")
       (Li "另一种我们经常在数学中见到的例子是"
           (Em "结构化集合(structured set)") "的范畴, "
           "即由带有更进一步" (Q "结构") "的集合和"
           (Q "保持这结构") "的函数构成的范畴, "
           "其中这些概念以某种独立的方式确定. "
           "你可能会熟悉的这类例子有"
           (Ul (Li "群和群同态;")
               (Li "向量空间和线性映射;")
               (Li "图和图同态;")
               (Li "实数集" $RR "和连续函数" (&-> $RR $RR) ";")
               (Li "开集" (&sube $U $RR) "和定义于其上的连续函数"
                   (Arrow $f $U (&sube $V $RR)) ";")
               (Li "拓扑空间和连续映射;")
               (Li "可微流形和光滑映射;")
               (Li "自然数集" $NN "和所有的递归函数" (&-> $NN $NN)
                   ", 或者就像连续函数的例子一样, "
                   "我们可以取定义于子集" (&sube $U $NN)
                   "上的部分递归函数;")
               (Li "偏序集和单调函数."))
           "即便你还不熟悉以上的其中一些例子, 也请不要担心, "
           "暂且让我们仅是更细致地考虑以上的最后一个例子.")
       (Li "一个偏序集 (partially ordered set) 或者说"
           (Em "poset") "是一个装备了一个二元关系"
           (&<=_A $a $b) "的集合" $A ", 其对于所有的"
           (∈ $a $b $c $A) "满足以下条件:"
           (Ul (Li "自反性: " (&<=_A $a $a) ";")
               (Li "传递性: 如果" (&<=_A $a $b) "且"
                   (&<=_A $b $c) ", 那么" (&<=_A $a $c) ";")
               (Li "反对称性: 如果" (&<=_A $a $b) "且"
                   (&<=_A $b $a) ", 那么" (&= $a $b) "."))
           "例如, 装备有通常的序关系" (&<= $a $b)
           "的实数集" $RR "构成了一个偏序集, 其也是"
           (Em "线性(linearly)") "序的: 对于任意的"
           (&cm $x $y) ", 要么" (&<= $x $y)
           ", 要么" (&<= $y $x) ". {译注: 亦可兼而有之, "
           "当且仅当" (&= $x $y) "时.}" (Br)
           "从一个偏序集" $A "到一个偏序集" $B
           "的一个箭头是一个函数"
           (MB (Arrow $m $A $B))
           "其为" (Em "单调的(monotone)")
           ", 意即对于所有的" (∈ $a $a^ $A) ", 我们有"
           (MB (&<=_A $a $a^) "可以推出"
               (&<=_B (app $m $a) (app $m $a^)) ".")
           "要使得这成为一个范畴需要满足什么条件呢? "
           "我们需要知道" (Arrow $1_A $A $A)
           "是单调的, 但这是显然的, 因为"
           (&<=_A $a $a^) "可以推出" (&<=_A $a $a^)
           ". 我们也需要知道, 如果" (Arrow $f $A $B)
           "和" (Arrow $g $B $C) "是单调的, 那么"
           (Arrow (&compose $g $f) $A $C)
           "也是单调的. 这同样成立, 因为"
           (&<= $a $a^) "可以推出"
           (&<= (app $f $a) (app $f $a^)) "可以推出"
           (&<= (app* $g $f $a) (app* $g $f $a^)) "可以推出"
           (&<= (app (@compose $g $f) $a)
                (app (@compose $g $f) $a^))
           ". 因此, 我们有偏序集和单调函数的范畴" Pos ".")
       (Li "到目前为止我们已经考虑了的范畴是有时叫做"
           (Em "具体范畴(concrete category)")
           "的东西的例子. 非形式化地说, "
           "存在着这样的范畴, 其对象是集合, "
           "可能装备有某种结构, 而箭头是特定的函数, "
           "可能保持结构 (我们将在之后看到这个想法"
           "并非逻辑一致的; 见评注1.7). "
           "但是, 实际上一种理解范畴论的方式全然在于"
           (Q "不用元素 (doing without elements)") "而代之以箭头. "
           "让我们来看看一些例子, "
           "其中这种观点并不仅是可选的, 而是不可避免的." (Br)
           "令" Rel "是以下范畴: 取集合为对象而二元关系为箭头. "
           "也就是说, 一个箭头" (Arrow $f $A $B)
           "是一个任意的子集" (&sube $f (&c* $A $B))
           ". 一个集合" $A "上的恒等箭头即是恒等关系,"
           (MB (&= $1_A
                   (&sube (setI (∈ (tu0 $a $a)
                                   (&c* $A $A))
                                (∈ $a $A))
                          (&c* $A $A))) ".")
           "给定" (&sube $R (&c* $A $B)) "和"
           (&sube $S (&c* $B $C)) ", 我们可以根据"
           (MB (∈ (tu0 $a $c) (&compose $S $R))
               "当且仅当"
               (∃ $b (&& (∈ (tu0 $a $b) $R)
                         (∈ (tu0 $b $c) $S))))
           "定义复合" (&compose $S $R)
           ", 即" $R "和" $S "的" (Q "关系积 (relative product)")
           ". 我们将表明" Rel "确为一个范畴的工作留作练习. "
           "(要做什么呢?)" (Br)
           "要举另外一个箭头并非" (Q "函数") "的范畴的例子, "
           "令对象为有限集合" (&cm $A $B $C) "而一个箭头"
           (Arrow $F $A $B) "是一个由自然数构成的矩阵"
           (&= $F (_cm (@_cm $n $i $j) (&< $i $a) (&< $j $b)))
           ", 其中" (&= $a (card $A)) "而" (&= $b (card $B))
           ". 符号" (card $C) "表示一个集合" $C "的元素数目. "
           "箭头的复合无非是通常的矩阵乘法, "
           "而恒等箭头即是通常的单位矩阵. "
           "这里的对象的目的仅是为了保证矩阵乘法有定义, "
           "但是矩阵并非对象之间的函数.")
       (Li (Em "有限范畴") (Br)
           "当然了, 范畴的对象也不必是集合. "
           "以下是一些非常简单的例子:"
           (Ul (Li "范畴" Cat1 "模样如下:"
                   (MB $*)
                   "其有一个对象和一个该对象的恒等箭头, "
                   "恒等箭头我们没有画出来.")
               (Li "范畴" Cat2 "模样如下:"
                   (MB $* (long-arrow "6em") $Star)
                   "其有两个对象, 必要的恒等箭头, 以及恰好一个"
                   "(不同)对象之间的箭头.")
               (Li "范畴" Cat3 "模样如下:"
                   todo.svg
                   "其有三个对象, 必要的恒等箭头, "
                   "恰好一个从第一个对象到第二个对象的箭头, "
                   "恰好一个从第二个对象到第三个对象的箭头, 以及"
                   "恰好一个从第一个对象到第三个对象的箭头 "
                   "(因而其为前两个箭头的复合).")
               (Li "范畴" Cat0 "模样如下:"
                   (MB "")
                   "其既没有对象, 也没有箭头."))
           "和上面一样, 从现在开始绘制范畴时我们都会省略恒等箭头." (Br)
           "描述有限范畴是容易的&mdash;&mdash;只需要取一些对象"
           "并在这些对象之间放置箭头, 但是请务必保证放置了"
           "由范畴论的公理所要求的必要的恒等箭头还有复合. "
           "而且, 如果存在任何的环路 (loop) 的话, "
           "那么我们就需要通过等式来截断环路以保持范畴的有限性. "
           "例如, 考虑以下描述:"
           (MB $A (__^^ $rlarr (pad $g) (pad $f)) $B)
           "除非我们明确规定诸如" (&= (&i* $g $f) $1_A)
           "这样一个等式 {译注: 大概还需要规定"
           (&= (&i* $f $g) $1_B)
           "}, 不然我们就会拥有无穷多的箭头: "
           (&cm (&i* $g $f) (&i* $g $f $g $f)
                (&i* $g $f $g $f $g $f) $..h)
           ". 当然, 这仍然是一个范畴, 但并非一个"
           (Em "有限") "范畴. 本章之后讨论自由范畴时, "
           "我们还会回到这个(无限的)情况上来.")
       (Li "范畴论的一个重要口号是"
           (Div #:attr* '((style "text-align: center;"))
                (Em "箭头才是真正重要的东西!"))
           "因此, 我们也应该检视范畴之间的箭头或者"
           (Q "映射") ". " (Q "范畴的同态") "被称为函子."))
   ((Definition)
    "范畴" CatC "和范畴" CatD "之间的一个" (Em "函子")
    (MB (Functor $F CatC CatD))
    "是一个从对象到对象而从箭头到箭头的映射, 其满足"
    (Ol #:attr* '((type "a"))
        (Li (&= (app $F (Arrow $f $A $B))
                (Arrow (app $F $f) (app $F $A) (app $F $B))) ";")
        (Li (&= (app $F $1_A) (_ $1 (app $F $A))) ";")
        (Li (&= (app $F (&compose $g $f))
                (&compose (app $F $g) (app $F $f))) "."))
    "换言之, " $F "保持定义域和陪域, 恒等箭头, 以及复合. "
    "因此, 一个函子" (Functor $F CatC CatD)
    "给出了某种" CatC "于" CatD "中的(可能扭曲的)"
    (Q "图景(picture)") "."
    todo.svg
    "现在, 我们可以轻易地看出函子能够按照预期的方式复合, "
    "而每个范畴" CatC "都拥有一个恒等函子"
    (Functor (_ $1 CatC) CatC CatC)
    ". 于是, 我们有了另一个范畴的例子, 叫做"
    Cat ", 其是由所有的范畴和函子构成的范畴.")
   (Ol #:attr* '((start "7"))
       (Li "一个" (Em "预序(preorder)")
           "是一个装备了一个满足自反性和传递性的二元关系"
           (&<= $p $q) "的集合: " (&<= $a $a)
           ", 并且如果" (&<= $a $b) "且" (&<= $b $c)
           ", 那么" (&<= $a $c) ". 任意的预序" $P
           "都可以被视为一个范畴, 只需要取对象为"
           $P "的元素而两个元素之间唯一可能的箭头是"
           (MBL "(1.2)"
                (&-> $a $b) "当且仅当" (&<= $a $b) ".")
           $<= "的自反和传递条件保证了这的确是一个范畴." (Br)
           "从另一个方向上来说, 如果一个范畴满足其任意两个对象"
           "之间至多只有一个箭头, 那么其就确定了一个预序, "
           "只需要根据(1.2)来定义一个对象上的二元关系"
           $<= "即可.")
       (Li "一个偏序集显然是一个满足额外的反对称条件的预序: "
           "如果" (&<= $a $b) "且" (&<= $b $a) ", 那么"
           (&= $a $b) ". 因此, 一个偏序集也是一个范畴. "
           "这样的" (Em "偏序集范畴(poset category)")
           "很是常见; 例如, 对于任意的集合" $X
           ", 幂集" (powerset $X) "在通常的" $X
           "的子集" (&cm $U $V) "之间的包含关系"
           (&sube $U $V) "下是一个偏序集." (Br)
           "偏序集范畴" $P "和" $Q "之间的函子"
           (Functor $F $P $Q) "是什么样的呢? "
           "其必须满足恒等律和复合律... 显然, "
           "这些只是之前已经考虑过了的单调函数." (Br)
           "将一个范畴想成是某种" (Em "广义偏序集(generalized poset)")
           "常常是有用的, 也就是带有比" (&<= $p $q)
           "带有" (Q "更多结构") "的东西. 因此, "
           "我们也可以将函子想成是某种广义单调映射.")
       (Li (Em "来源于拓扑的一个例子")
           ": 令" $X "是一个以" (open $X)
           "为开集族的拓扑空间. 以包含关系排序, "
           (open $X) "就成了一个偏序集范畴. "
           "而且, " $X "的点可以根据" (Em "特殊化程度(specialization)")
           "进行预序化, 通过设置" (&<= $x $y)
           "当且仅当对于每个开集" $U ", "
           (∈ $x $U) "可以推出" (∈ $y $U)
           ", 也就是" $y "包含于每个包含" $x
           "的开集之中. 如果" $X "是充分分离的 ("
           (Q $T_1) "), 那么这个序关系是平凡的, "
           "但是其他情况下可能会变得相当有趣, "
           "例如对于代数几何和指称语义的空间而言. "
           "作为练习, 请证明" $T_0
           "空间在此特殊化序下实际上是偏序集. "
           "{译注: 特殊化程度可以这么理解, 因为包含"
           $y "的开集比包含" $x "的开集更多, 所以"
           $y "比" $x "更加特殊.}")
       (Li (Em "来源于逻辑的一个例子")
           ": 给定一个逻辑演绎系统, 存在一个与之相关的"
           (Em "证明范畴(category of proofs)")
           ", 其中的对象为公式:"
           (MB (&cm $phi $psi $..h))
           "从" $phi "到" $psi "的一个箭头是一个从"
           "(uncanceled)假设" $phi "到" $psi "的推导."
           (MB (&rule* $phi $..v $psi))
           "箭头的复合即将这样的推导以显然的方式并置, "
           "当然这是结合性的. (恒等箭头" $1_phi
           "应该是什么呢?) 我们观察到可以存在诸多不同的箭头"
           (MB (Arrow $p $phi $psi))
           "因为或许存在着诸多不同的证明. "
           "这种范畴实际上有着丰富的结构, "
           "之后我们将与" $lambda "演算一起考虑.")
       (Li (Em "来源于计算机科学的一个例子")
           ": 给定一个函数式语言" $L
           ", 存在一个与之相关的范畴, 其对象是"
           $L "的数据类型, 而箭头是" $L
           "的可计算函数 (" (Q "进程 (process)")
           ", " (Q "过程 (procedure)") ", "
           (Q "程序 (program)") "). 两个这样的程序的复合"
           (ARROW $X $f $Y $g $Z) "由应用" $g
           "于" $f "的输出得到, 有时亦记作"
           (MB (&= (&compose $g $f) (&\; $f $g)) ".")
           "恒等箭头即" (Q "什么也不做 (do nothing)")
           "的程序." (Br)
           "这样的范畴对于编程语言的指称语义的想法而言是基本的. "
           "例如, 如果" (app CatC $L) "是刚才定义的范畴, "
           "那么语言" $L "于一个Scott domain的范畴" CatD
           "中的指称语义不过就是一个函子"
           (MB (Functor $S (app CatC $L) CatD))
           "因为" $S "赋予了" $L "的类型以domain, "
           "而程序以连续函数. 这个例子和前一个例子都和"
           "我们之后要考虑的"
           (Q "笛卡尔闭范畴 (cartesian closed category)")
           "的概念有关. {译注: 这里的domain有特殊的含义, "
           "其来源于指称语义的理论, "
           "并不是(函数或者箭头的)定义域或者(逻辑学的)论域的意思, "
           "故我倾向于保留原文不动.}")
       (Li "令" $X "是一个集合. 我们可以将" $X
           "当作一个范畴" (app Dis $X) ", 其取"
           $X "的元素为对象而箭头仅是必要的恒等箭头, "
           "也就是对于每个" (∈ $x $X) "都有一个. "
           "这种箭头仅是恒等箭头的范畴被称为是"
           (Em "离散的(discrete)")
           ". 注意到离散范畴非常不过是非常特殊的偏序集.")
       (Li "一个" (Em "幺半群(monoid)")
           " (有时也被称为" (Em "semigroup with unit")
           ") 是一个集合" $M ", 其装备了一个二元运算"
           (Arrow $d* (&c* $M $M) $M) "和一个突出的"
           (Q "单位元") (∈ $u $M) ", 满足对于所有的"
           (∈ $x $y $z $M) ", 都有"
           (MB (associate &d* $x $y $z))
           "和"
           (MB (&= (&d* $u $x) $x (&d* $x $u)))
           "成立. 等价地, 一个幺半群不过就是一个仅有一个对象的范畴. "
           "这个范畴的箭头即是幺半群的元素. "
           "特别地, 恒等箭头即是单位元素" $u
           ". 箭头的复合即是幺半群的二元运算" (&d* $m $n) "." (Br)
           "幺半群是非常普遍的. 这里有数字构成的幺半群, "
           "例如装备了加法和" $0 "或者乘法和" $1 "的"
           $NN ", " $QQ ", 还有" $RR ". 不过, 对于任意的集合"
           $X ", 由所有从" $X "到" $X "的函数构成的集合, 记作"
           (MB (Hom Sets $X $X))
           "在复合运算下也是一个幺半群. 更一般地, 对于任意范畴"
           CatC "中的对象" $C ", 从" $C "到" $C "的箭头的集合, 记作"
           (Hom CatC $C $C) ", 在" CatC "的复合运算下是一个幺半群." (Br)
           "既然幺半群是结构化集合, 那么存在一个范畴" Mon
           ", 其对象是幺半群而箭头是保持幺半群结构的函数. "
           "更细致地说, 一个从幺半群" $M "到幺半群" $N
           "的同态是一个函数" (Arrow $h $M $N)
           ", 其满足对于所有的" (∈ $m $n $M) ", 都有"
           (MB (&= (app $h (&d*_M $m $n))
                   (&d*_N (app $h $m) (app $h $n))))
           "和"
           (MB (&= (app $h $u_M) $u_N))
           "成立. 我们观察到一个从" $M "到" $N
           "的幺半群同态和一个从被视为范畴的" $M
           "到被视为范畴的" $N "的函子是相同的东西. "
           "在这种意义下, 范畴也是广义的幺半群 (generalized monoid), "
           "而函子是广义的同态."))
   (H3. "同构")
   ((Definition)
    "在任意的范畴" CatC "中, 一个箭头" (Arrow $f $A $B)
    "被称为一个" (Em "同构(isomorphism)")
    ", 如果存在另一个箭头" (Arrow $g $B $A) "满足"
    (MB (&= (&compose $g $f) $1_A) "且"
        (&= (&compose $f $g) $1_B) ".")
    "既然逆是唯一的 (请证明!), 我们记" (&= $g (inv $f))
    ". 我们称" $A "同构于" $B ", 记作" (&cong $A $B)
    ", 如果它们之间存在一个同构.")
   (P "同构的定义我们第一个对于重要的概念进行"
      (Em "抽象") "的范畴论式的定义的例子. "
      "抽象的含义在于其只使用了范畴论的概念, "
      "而并不诉诸于关于对象和箭头的一些额外信息. "
      "相较于其他可能的定义, 其优点在于它适用于任意的范畴. "
      "例如, 人们有时将集合 (幺半群, 等等) 的同构定义为"
      (Em "双射(bijective)") "函数 (相应地, 双射的同态), 即满足"
      (Q "1-1和onto") "的东西&mdash;&mdash;这用到了对象的"
      (Em "元素") ". {译注: " (Q "1-1和onto")
      "是英语曾经对于单射和满射的表达, "
      "但是自从Bourbaki学派兴起之后, "
      "一般人都用injective和surjective了.} "
      "在某种情形下, 这" (Em "等价于")
      "我们的定义, 例如集合和幺半群. "
      "但是我们也应该注意到, 例如在" Pos
      "之中, 非同构的偏序集之间也存在着"
      (Q "双射的同态") ". 而且, 在诸多情形之下, "
      (Em "只有") "抽象的定义能够成立 (make sense), "
      "例如将幺半群视为范畴的情形.")
   ((Definition)
    "一个" (Em "群(group)") $G "是一个幺半群, "
    "其每个元素" $g "都有一个逆" (inv $g)
    ". 因此, " $G "是一个只有一个对象的范畴, "
    "其每个箭头都是一个同构.")
   (P "自然数集" $NN "在加法或者乘法下都不能构成一个群, "
      "但是整数集" $ZZ "在加法下能够形成一个群, "
      "正有理数集" $QQ^+ "在乘法下也能够形成一个群. "
      "对于任意的集合" $X ", 我们有由" $X
      "的自同构 (或者说" (Q "置换") ") 构成的群"
      (Aut $X) ", 即由同构" (Arrow $f $X $X)
      "构成的群. (为什么这在" (Q $compose)
      "下封闭?) 一个" (Em "置换群(group of permutations)")
      "是对于某个集合" $X "而言的子群"
      (&sube $G (Aut $X)) ", 也就是由" $X
      "的(某些)自同构构成的群. 因此, 集合"
      $G "必须满足以下条件:"
      (Ol (Li $X "上的恒等函数" $1_X
              "在" $G "之中.")
          (Li "如果" (∈ $g $g^ $G)
              ", 那么" (∈ (&compose $g $g^) $G) ".")
          (Li "如果" (∈ $g $G)
              ", 那么" (∈ (inv $g) $G) ".")))
   (P "一个群的" (Em "同态") (Arrow $h $G $H)
      "不过就是一个幺半群同态, "
      "由此则必然保持逆 (请证明!).")
   (P "现在请让我们考虑以下关于抽象群的基本而经典的结果.")
   ((Theorem #:auto? #f)
    (B "(Cayley). ")
    (Em "每个群" $G "都同构于一个置换群."))
   ((proof)
    "(证明大纲)"
    (Ol (Li "首先, 定义" $G "的Cayley表示"
            (OverBar $G) "为以下的集合的置换的群: "
            "这个集合就是" $G "自身, 而对于每个元素" (∈ $g $G)
            ", 我们有置换" (Arrow (OverBar $g) $G $G)
            ", 其定义于所有" (∈ $h $G) "之上, 相当于"
            (Q "作用于左 (acting on the left)") ":"
            (MB (&= (app (OverBar $g) $h)
                    (&d* $g $h)) ".")
            "这的确是一个置换, 因为其有着" (inv $g)
            "的作用为逆.")
        (Li "接着, 根据" (&= (app $i $g) (OverBar $g))
            "定义同态" (Arrow $i $G (OverBar $G))
            ", 而根据"
            (&= (app $j (OverBar $g))
                (app (OverBar $g) $u))
            "定义同态" (Arrow $j (OverBar $G) $G) ".")
        (Li "最后, 表明" (&= (&compose $i $j) (_ $1 (OverBar $G)))
            "而" (&= (&compose $j $i) $1_G) ".")))
   ((Warning)
    "注意到Cayley定理的证明之中存在着两种不同层次的同构. "
    "我们既有由" $G "的元素构成的集合的置换, 其是" Sets
    "中的同构, 也有" $G "和" (OverBar $G) "之间的同构, "
    "其在由群和群同态构成的范畴" Groups "之中.")
   (P "Cayley定理言称任意的抽象群都可以由某个"
      (Q "具体") "群所表示, 即某个集合的一个置换群. "
      "实际上, 这个定理可以被推广以表明任意不"
      (Q "太大 (too big)") "的范畴都可以由一个"
      (Q "具体") "范畴表示, 即一个由集合和函数构成的范畴. "
      "(不" (Q "太大") "的技术性含义于" (Ref "foundation")
      "引入.)")
   ((Theorem)
    (Em "每个以箭头的集合为态射类的范畴" CatC
        "都同构于一个以集合为对象而函数为箭头的范畴.")
    " {译注: " (Q "每个以箭头的集合为态射类的范畴" CatC)
    "的原文是" (Q "every category " CatC " with a set of arrows")
    ", 这里译者采取了意译而非直译. 换言之, 即范畴"
    CatC "是小范畴.}")
   ((proof)
    "(证明大纲) 定义" CatC "的Cayley表示" (OverBar CatC)
    "为以下具体范畴:"
    (Ul (Li "对象是每个具有形式"
            (MB (&= (OverBar $C)
                    (setI (∈ $f CatC)
                          (&= (&cod $f) $C))))
            "的集合, 其中" (∈ $C CatC) ";")
        (Li "箭头是对于" CatC "中的每个箭头"
            (Arrow $g $C $D)
            "而言的函数"
            (MB (apply Arrow
                       (map OverBar
                            (list $g $C $D))))
            "其定义于" (OverBar $C) "中的任意箭头"
            (Arrow $f $X $C) "之上, 由"
            (&= (app (OverBar $g) $f)
                (&compose $g $f))
            "给出."
            todo.svg)))
   ((Remark)
    "这向我们表明关于由集合和函数所构成的"
    (Q "具体") "范畴的朴素概念的" (Em "错误之处")
    ": 尽管不是每个范畴都以特殊的集合和函数为其"
    "对象和箭头, 然而每个范畴都同构于一个这样的范畴. "
    "因此, 这样的范畴所能拥有的特殊性质仅可能是"
    "那些和范畴无关的性质, 例如对象所不能影响箭头分毫的那种特性 "
    "(就像构造为Dedekind分割或者Cauchy序列的实数系之间的区别). "
    "为了捕获" (Q "具体") "范畴这个相当模糊的想法所意图表达的东西, "
    "更好的尝试为以下的刻画: 任意的箭头" (Arrow $f $C $D)
    "完全由其与箭头们" (Arrow $x $T $C) "的复合确定, "
    "其中的" $T "是某个" (Q "测试对象") ", 意即如果对于所有这样的"
    $x "都有" (&= (&i* $f $x) (&i* $g $x)) ", 那么就可以推出"
    (&= $f $g) ". 我们之后将会看到, 这相当于考虑由" $T
    "所确定的对于范畴的一种特定表示. 那么, "
    "一个范畴被称为是" (Q "具体") "的, 如果这个条件对于某个"
    (Q "终对象") $T "成立, 终对象的含义见" (Ref "initial-and-terminal")
    "; 不过, 也有很好的理由考虑其他的对象" $T
    ", 如我们在" (Ref "abstract-structures") "中所见." (Br)
    "注意到" CatC "的态射类为箭头的" (Em "集合")
    "这个条件是必要的, 其是为了保证合集"
    (setI (∈ $f CatC) (&= (&cod $f) $C))
    "的确是" (Em "集合") "&mdash;&mdash;我们在"
    (Ref "foundation") "还会回到这个问题上来.")
   (H3. "范畴的构造")
   (P "既然我们有了一些可以摆弄的范畴, 现在我们可以考虑一些从旧范畴产生新范畴的构造了."
      (Ol (Li "两个范畴之" (Em "积") ", 记作"
              (MB (&c* CatC CatD))
              "其有着形式为" (tu0 $C $D) "的对象, 其中" (∈ $C CatC) "且"
              (∈ $D CatD) ", 而有着形式为"
              (MB (apply Arrow
                         (map2 tu0
                               (list $f $g $C $D $C^ $D^))))
              "的箭头, 其中" (∈ (Arrow $f $C $C^) CatC) "而"
              (∈ (Arrow $g $D $D^) CatD) ". 复合和单位元都是逐分量定义的, 即"
              (MB (&= (apply &compose
                             (map2 tu0
                                   (list $f^ $g^ $f $g)))
                      (apply tu0
                             (map2 &compose
                                   (list $f^ $f $g^ $g)))))
              (MB (&= (_ $1 (tu0 $C $D))
                      (tu0 $1_C $1_D)))
              "存在着两个显然的" (Em "投影函子(projection functor)")
              (MB CatC (^^ $<- (pad $pi_1)) (&c* CatC CatD)
                  (^^ $-> (pad $pi_2)) CatD)
              "其由" (&= (appl $pi_1 $C $D) $C) "和"
              (&= (appl $pi_1 $f $g) $f) "定义, 而" $pi_2 "是类似的." (Br)
              "熟悉群论的作者应该能够识别出来, 对于群" $G "和" $H
              ", 其积范畴" (&c* $G $H) "不过是通常的群的(直)积.")
          (Li "一个范畴" CatC "的" (Em "相反(opposite)")
              " (或者说" (Q "对偶 (dual)") ") 范畴"
              (&op CatC) "和" CatC "有着相同的对象, 而"
              (&op CatC) "中的一个箭头" (Arrow $f $C $D) "是"
              CatC "中的一个箭头" (Arrow $f $D $C)
              ". 也就是说, " (&op CatC) "不过就是将"
              CatC "中的所有箭头在形式上调转方向而已." (Br)
              "使用一种记号将" CatC "中的对象和" (&op CatC)
              "中的相同对象进行区分是方便的, 箭头亦是如此. "
              "因此, 让我们将" CatC "中的" (Arrow $f $C $D)
              "在" (&op CatC) "中的相应箭头记为"
              (MB (apply Arrow (map &* (list $f $D $C))) ".")
              "以此记号, 我们可以基于" CatC "中的复合和单位元来定义"
              (&op CatC) "中的相应运算, 即"
              (MB (&= (_ $1 (&* $C)) (&* (@ $1_C))))
              (MB (&= (&compose (&* $f) (&* $g))
                      (&* (@compose $g $f))))
              "因此, " CatC "中的一个图表"
              todo.svg
              "在" (&op CatC) "中看起来就像以下图表"
              todo.svg
              "诸多数学的" (Q "对偶性")
              "定理不过是表达了一个范畴是另一个范畴的相反范畴"
              "(的一个子范畴). 这种定理的一个例子是我们之后要证明的"
              Sets "对偶于完备原子布尔代数的范畴.")
          (Li "一个范畴" CatC "的" (Em "箭头范畴") CatC^->
              "以" CatC "的箭头为对象, 而" CatC^->
              "中一个从" (Arrow $f $A $B) "到"
              (apply Arrow (map &prime (list $f $A $B)))
              "的箭头" $g "是一个" (Q "交换方块")
              todo.svg
              "其中的" $g_1 "和" $g_2 "是" CatC "中的箭头. "
              "也就是说, 这样一个箭头是" CatC "中的一对箭头"
              (&= $g (tu0 $g_1 $g_2)) ", 其满足"
              (MB (&= (&compose $g_2 $f)
                      (&compose $f^ $g_1)) ".")
              "一个对象" (Arrow $f $A $B) "上的恒等箭头"
              $1_f "是序对" (tu0 $1_A $1_B)
              ". 箭头的复合是逐分量进行的:"
              (MB (&= (&compose (tu0 $h_1 $h_2)
                                (tu0 $g_1 $g_2))
                      (tu0 (&compose $h_1 $g_1)
                           (&compose $h_2 $g_2))))
              "读者应该绘制相应的交换图表来验证这的确能够成立." (Br)
              "观察到这里存在着两个函子:"
              (MB CatC (^^ $<- (pad $dom)) CatC^->
                  (^^ $-> (pad $cod)) CatC)
              "{译注: 函子" $dom "的定义为" (&= (&dom (Arrow $f $A $B)) $A)
              "而" (&= (&dom $g) $g_1) ", 其中" (&= $g (tu0 $g_1 $g_2))
              "; 函子" $cod "的定义与" $dom "类似.}")
          (Li "一个范畴" CatC "于一个对象" (∈ $C CatC)
              "上的" (Em "切片范畴(slice category)") (slice CatC $C) "拥有"
              (Ul (Li "对象: 所有满足" (&= (&cod $f) $C)
                      "的箭头" (∈ $f CatC) ";")
                  (Li "箭头: 从" (Arrow $f $X $C) "到"
                      (Arrow $f^ $X^ $C) "的一个箭头" $a
                      "是" CatC "中的一个箭头"
                      (Arrow $a $X $X^) ", 其满足"
                      (&= (&compose $f^ $a) $f) ", 如下图所示"
                      todo.svg
                      "恒等箭头和复合继承自" CatC
                      ", 这与箭头范畴的情况类似. "
                      "注意到存在着一个函子"
                      (Functor $U (slice CatC $C) CatC)
                      ", 其" (Q "遗忘了基对象" $C) "." (Br)
                      "如果" (Arrow $g $C $D)
                      "是任意的箭头, 那么存在着一个复合函子"
                      (MB (Functor $g_* (slice CatC $C) (slice CatC $D)))
                      "其由" (&= (app $g_* $f) (&compose $g $f)) "定义"
                      todo.svg
                      "而对于" (slice CatC $C) "中的箭头也是类似的. "
                      "显然, 整个构造是一个函子"
                      (MB (Functor (slice CatC (@ $dummy)) CatC Cat))
                      "读者可以轻易验证这个事实. 和Cayley表示相比, "
                      "这个函子给出了" CatC "作为由范畴和函子构成的范畴的一种"
                      (Q "表示") "&mdash;&mdash;而非由集合与函数构成的范畴. "
                      "当然了, Cayley表示不过就是其接着一个遗忘函子"
                      (Functor $U Cat Sets)
                      ", 其取范畴为它潜在 (underlying) 的对象集. "
                      "{译注: 这个遗忘函子将" Cat "里的箭头, 即范畴间的函子, "
                      "变为潜在对象集之间的函数.}"(Br)
                      "如果" (&= CatC CatP) "是一个偏序集范畴而"
                      (∈ $p CatP) ", 那么"
                      (MB (&cong (slice CatP $p) (&darr $p)))
                      "切片范畴" (slice CatP $p) "不过就是"
                      (Q "主理想") (&darr $p) ", 其由所有满足"
                      (&<= $q $p) "的元素" (∈ $q CatP) "构成. "
                      "我们很快还会遇到更多切片范畴的例子." (Br)
                      "一个范畴" CatC "在一个" CatC "的对象" $C
                      "下的" (Em "余切片范畴(coslice category)")
                      (coslice $C CatC) "以所有满足" (&= (&dom $f) $C)
                      "的" CatC "中箭头" $f "为对象, 而一个从"
                      (Arrow $f $C $X) "到" (Arrow $f^ $C $X^)
                      "的箭头是一个箭头" (Arrow $h $X $X^)
                      ", 其满足" (&= (&compose $h $f) $f^)
                      ". 读者现在应该仿照切片范畴的定义来"
                      "展开剩余的关于余切片范畴的定义. "
                      "如何根据切片范畴和相反构造来定义余切片范畴呢?")))))
   ((Example)
    (Em "带点集合(pointed set)") "的范畴" Sets_*
    "以带有一个突出元素" (∈ $a $A) "的集合" $A
    "为对象, 而箭头" (Arrow $f (tu0 $A $a) (tu0 $B $b))
    "则是保持" (Q "点") "的函数" (Arrow $f $A $B)
    ", 即" (&= (app $f $a) $b) ". 这同构于一个余切片范畴, 即"
    (MB (&cong Sets_* (coslice $1 Sets)))
    "其中" $1 "是任意的单元集" (setE $*:id)
    ". 诚然如此, 函数" (Arrow $a $1 $A) "唯一地与元素"
    (&= (app $a $*:id) (∈ $a $A)) "相对应, 而箭头"
    (Arrow $f (tu0 $A $a) (tu0 $B $b)) "恰与交换三角形"
    todo.svg
    "相对应.")
   (H3. "自由范畴")
   (P (B "自由幺半群 (free monoid).")
      " 我们从一个" (Q "字母") (&cm $a $b $c $..h)
      "的" (Q "表 (alphabet)") $A "开始, 即一个集合"
      (MB (&= $A (setE $a $b $c $..h)) ".")
      $A "上的一个" (Em "词(word)")
      "是一个字母的有限序列:"
      (MB (&cm "thisword"
               "categoriesarefun"
               "asddjbnzzfj"
               $..h))
      "我们将空词记为" (Q $empty-word)
      ". " $A "的" (Q "Kleene闭包")
      "被定义为集合"
      (MB (&= $A^*
              (cur0 (: $A "上的词"))) ".")
      "我们定义一个" $A^* "上的二元运算"
      (Q $*) "为" (&= (&* $w $w^) (concat $w $w^))
      ", 其中词" (∈ $w $w^ $A^*) ". 因此, "
      (Q $*) "不过就是" (Em "拼接(concatenation)")
      ". 于是, 运算" (Q $*) "是结合性的, 并且空词"
      (Q $empty-word) "是其单位元. "
      "所以说, " $A^* "是一个幺半群&mdash;&mdash;"
      "其被称为集合" $A "上的"
      (Em "自由幺半群(free monoid)")
      ". 元素" (∈ $a $A) "可以被视为长度为一的词, "
      "也就是说我们有了一个函数"
      (MB (Arrow $i $A $A^*))
      "其定义是" (&= (app $i $a) $a)
      ", 并被称为" (Q "生成元的嵌入 (insertion of generators)")
      ". 我们说" $A "的元素" (Q "生成") "了这自由范畴, "
      "其意在于每个" (∈ $w $A^*) "都是" $a
      "的一个" $* "积, 即对于" $A "中的某些"
      (&cm $a_1 $a_2 $..h $a_n) "我们有"
      (&= $w (&* $a_1 $a_2 $..c $a_n))
      ". {译注: 嵌入是自动的, 没有以额外的符号标示.}")
   (P "现在我们想问的是这里的" (Q "自由")
      "是什么意思? 能猜一下吗? "
      "有时读者会在" (Q "宝宝代数 (baby algebra)")
      "书籍中看到其定义以如下文字呈现:")
   (P "一个幺半群" $M "由" $M "的一个子集" $A
      (Em "自由生成") ", 如果其满足以下条件:"
      (Ol (Li "每个元素" (∈ $m $M) "都可以写成" $A "的元素之积:"
              (MB (&cm (&= $m (&d*_M $a_1 $..h $a_n))
                       (∈ $a_i $A)) "."))
          (Li "没有" (Q "非平凡 (nontrivial)")
              "的关系可以在" $M "中成立, 即若"
              (&= (&i* $a_1 $..h $a_j)
                  (&i* (_^ $a $1 $prime)
                       $..h
                       (_^ $a $k $prime)))
              ", 那么这是由幺半群的公理所要求的."))
      "第一个条件有时被称为" (Q "没有垃圾 (no junk)")
      ", 而第二个条件有时被称为" (Q "没有杂讯 (no noise)")
      ". 因此, " $A "上的自由幺半群是一个包含" $A
      "且既无垃圾也无杂讯的幺半群. "
      "你对于该自由幺半群的定义作何感想呢?")
   (P "我反对这第二个条件所涉及的" (Q "可证明性 (provability)")
      "或者其他什么. {译注: 译者也不太能完全理解这句话, "
      "大概的意思就是第二个条件太过模糊, 编写证明时难以使用.} "
      "欲使其作为定义而成功, 那么其就必须变得更加精确. "
      "在范畴论中, 我们可以给出" (Q "自由")
      "的精确定义&mdash;&mdash;其捕获了上述内容的实质"
      "&mdash;&mdash;但是避免了这种模糊.")
   (P "首先, 每个幺半群" $N "都有一个潜在集"
      (forget $N) ", 并且每个幺半群同态"
      (Arrow $f $N $M) "都有一个潜在函数 (underlying function) "
      (apply Arrow (map forget (list $f $N $M)))
      ". 很容易看出来这是一个函子, 可以称为"
      (Q "遗忘函子") ". 根据定义, 一个集合" $A "上的自由幺半群"
      (free-monoid $A) "是带有以下所谓"
      (Em "泛映射性质(universal mapping property)")
      "或者说UMP的" (Q "那个 (the)") "幺半群!")
   (P (Em (free-monoid $A) "的泛映射性质") (Br)
      "存在着一个函数" (Arrow $i $A (forget (free-monoid $A)))
      ", 对于任意的幺半群" $N "和任意的函数" (Arrow $f $A (forget $N))
      ", 存在" (Em "唯一") "的幺半群同态"
      (Arrow (OverBar $f) (free-monoid $A) $N)
      "使得" (&= (&compose (forget (OverBar $f)) $i) $f)
      ", 如以下图表所示:"
      UMP:free-monoid.svg)
   ((Proposition)
    (Em $A^* "具有" $A "上的自由幺半群的UMP."))
   ((proof)
    "给定函数" (Arrow $f $A (forget $N)) ", 由"
    (MB (&cm (&= (app (OverBar $f) $empty-word) $u_N)
             (: $N "的单位元")))
    (MB (&= (app (OverBar $f) (&i* $a_1 $..h $a_i))
            (&d*_N (app $f $a_1) $..h
                   (app $f $a_i))))
    "定义" (Arrow (OverBar $f) $A^* $N)
    ", 那么" (OverBar $f) "显然是一个满足"
    (MB (&cm (&= (app (OverBar $f) $a)
                 (app $f $a))
             (: "对于所有的" (∈ $a $A))))
    "的同态. 如果" (Arrow $g $A^* $N) "也满足对于所有的"
    (∈ $a $A) "都有" (&= (app $g $a) (app $f $a))
    ", 那么对于所有的" (∈ (&i* $a_1 $..h $a_i) $A^*) ":"
    (MB (deriv (app $g (&i* $a_1 $..h $a_i))
               (app $g (&* $a_1 $..h $a_i))
               (&d*_N (app $g $a_1) $..h (app $g $a_i))
               (&d*_N (app $f $a_1) $..h (app $f $a_i))
               (&d*_N (app (OverBar $f) $a_1)
                      $..h
                      (app (OverBar $f) $a_i))
               (app (OverBar $f) (&* $a_1 $..h $a_i))
               (app (OverBar $f) (&i* $a_1 $..h $a_i))))
    "因此, 正如泛映射性质所要求的, " (&= $g (OverBar $f))
    ". {译注: 这个证明里的某些地方需要诉诸我们对于有限字符序列 (字符串) "
    "的直觉, 但是的确是可以严格形式化的.}")
   (P "请想一想为什么上述的UMP精确捕获了" (Q "没有垃圾")
      "和" (Q "没有杂讯") "的意义. 更确切地说, "
      "UMP的存在性部分捕获了" (Q "没有杂讯")
      "这个模糊的概念 (因为生成元的代数组合之间成立的任意等式"
      "也必然在任意其可以被映射至的地方成立, 也就是所有的地方), "
      "而唯一性的部分精确化了" (Q "没有垃圾")
      "的想法 (因为任意不是由生成元组合而成的额外元素都可以被"
      "自由地映射至" (Em "不同") "的值.")
   (P "使用这个UMP, 我们可以很容易地表明自由幺半群"
      (free-monoid $A) "在同构下唯一确定, 其含义如下:")
   ((Proposition)
    (Em "给定幺半群" $M "和" $N ",以及附带的函数"
        (Arrow $i $A (forget $M)) "和"
        (Arrow $j $A (forget $N)) ",每个都满足"
        $A "上的自由幺半群的UMP,那么存在一个(唯一的)幺半群同构"
        (&: $h (&cong $M $N)) "满足"
        (&= (&i* (forget $h) $i) $j) "且"
        (&= (&i* (forget (inv $h)) $j) $i) "."))
   ((proof)
    "根据" $j "和" $M "的UMP, 我们有" (Arrow (OverBar $j) $M $N)
    ", 其满足" (&= (&i* (forget (OverBar $j)) $i) $j)
    ". 根据" $i "和" $N "的UMP, 我们有" (Arrow (OverBar $i) $N $M)
    ", 其满足" (&= (&i* (forget (OverBar $i)) $j) $i)
    ". 复合给出了一个同态"
    (Arrow (&compose (OverBar $i) (OverBar $j)) $M $M)
    ", 其满足"
    (&= (&i* (forget (&compose (OverBar $i) (OverBar $j))) $i) $i)
    ". {译注: 遗忘是一个函子, 故保持复合.} 鉴于"
    (Arrow $1_M $M $M) "也具有此性质, 根据" $M
    "的UMP的唯一性部分, 我们有"
    (&= (&compose (OverBar $i) (OverBar $j)) $1_M)
    ". 交换" $M "和" $N "的角色可以表明"
    (&= (&compose (OverBar $j) (OverBar $i)) $1_N)
    " {译注: " $h "的唯一性也可由自由幺半群的UMP的唯一性部分直接得到}:"
    todo.svg)
   (P "例如, 任意单元素集合上的自由幺半群很容易看出来与自然数集"
      $NN "在加法下的幺半群同构 (其" (Q "生成元")
      "是数字" $1 "). 因此, 作为一个幺半群, "
      "根据自由幺半群的UMP, " $NN "在同构下唯一确定. "
      "{译注: 和自由幺半群同构的幺半群是自由的.}")
   (P (B "自由范畴 (free category).")
      " 现在, 我们想要对于一般的范畴 (而非仅是幺半群) "
      "进行相同的事情. 范畴拥有的是潜在图 (underlying graph) "
      "而不是潜在集合, 故让我们先回顾图论.")
   (P "一个" (Em "有向图(directed graph)") "由顶点和边构成, "
      "其中每条边都是有向的, 也就是每条边都有一个"
      (Q "源头 (source)") "和一个" (Q "目标 (target)") "顶点. "
      "{译注: 在范畴论中, domain有时也被称为source, "
      "codomain有时也被称为target.}"
      todo.svg
      "我们就像绘制范畴一样绘制图, 但是这里没有边的复合, "
      "也没有恒等边 (identity). "
      "{译注: 绘制范畴的图表时, 我们会默认每个对象上都有一个恒等箭头"
      "而不画出来. 在图 (graph) 中, 我们没有与之对应的恒等边的概念. "
      "不过, 就像一个范畴可以有从某个对象到其自身的非恒等的箭头, "
      "一个图里也可以存在着从某个顶点到其自身的边 (即自环, loop). "
      "另外, 虽然边和边不能复合为新的边, "
      "但是两个由边构成的路径可以复合为新的路径, "
      "这也是下文就要提到的.}")
   (P "因此, 一个图由两个集合" $E " (边) 和"
      $V " (顶点), 以及两个函数" (Arrow $s $E $V)
      " (源头) 和" (Arrow $t $E $V)
      " (目标) 构成. 于是, 在" Sets "中, 一个图不过是具有形式"
      (MB $E (__^^ $rrarr (pad $t) (pad $s)) $V)
      "的一个由对象和箭头构成的配置 (configuration).")
   (P "现在, 每个图" $G "都" (Q "生成") "了一个范畴"
      (free-category $G) ", 即" $G "上的"
      (Em "自由范畴") ". 其取" $G "的顶点为对象, 而"
      $G "中的" (Em "路径(path)") "为箭头, "
      "其中一个路径是一个边的有限序列"
      (&cm $e_1 $..h $e_n) ", 其满足对于每个"
      (&= $i (&cm $1 $..h (&- $n $1))) "都有"
      (&= (app $t $e_i) (app $s (_ $e (&+ $i $1))))
      ". 我们记" (free-category $G) "的箭头以形式"
      (concat $e_n (_ $e (&- $n $1)) $..h $e_1) "."
      ;也可以算作某种形式的concat
      (MB $v_0 (^^ $-> (pad $e_1))
          $v_1 (^^ $-> (pad $e_2))
          $v_2 (^^ $-> (pad $e_3))
          $..c (^^ $-> (pad $e_n))
          $v_n)
      "置"
      (eqn*
       ((app $dom (concat $e_n $..h $e_1))
        $=
        (app $s $e_1))
       ((app $cod (concat $e_n $..h $e_1))
        $=
        (app $t $e_n)))
      "并以拼接定义复合:"
      (MB (&= (&compose
               (concat $e_n $..h $e_1)
               (concat (_^ $e $m $prime)
                       $..h
                       (_^ $e $1 $prime)))
              (concat
               (concat $e_n $..h $e_1)
               (concat (_^ $e $m $prime)
                       $..h
                       (_^ $e $1 $prime)))) ".")
      "对于每个顶点" $v ", 我们都有一个"
      (Q "空路径 (empty path)")
      ", 记作" $1_v ", 其为" $v "处的恒等箭头.")
   (P "我们注意到如果" $G "只有一个顶点, 那么"
      (free-category $G) "不过就是" $G
      "的边的集合上的自由幺半群. 另外, 如果" $G
      "只有顶点(而没有边), 那么" (free-category $G)
      "是" $G "的顶点集合上的离散范畴.")
   (P "之后我们将会拥有一个对于" (Q "自由")
      "的一般性定义, 暂时先让我们看到"
      (free-category $G) "也有一个UMP. "
      "首先, 以显然的方式定义遗忘函子"
      (MB (Functor $U Cat Graphs))
      "一个范畴" CatC "的潜在图"
      )
   (H3. "基础: 大, 小, 以及局部小" #:id "foundation")
   (H3. "练习")
   (H2. "抽象结构" #:id "abstract-structures")
   (H3. "满态射和单态射")
   (H3. "始对象和终对象" #:id "initial-and-terminal")
   (H3. "广义元素")
   (H3. "积")
   (H3. "积的例子")
   (H3. "带有积的范畴")
   (H3. "同态集")
   (H3. "练习")
   (H2. "对偶性")
   (H3. "对偶原理")
   (H3. "余积")
   (H3. "等化子")
   (H3. "余等化子")
   (H3. "练习")
   (H2. "群和范畴")
   (H3. "范畴中的群")
   (H3. "群的范畴")
   (H3. "作为范畴的群")
   (H3. "有限表现范畴")
   (H3. "练习")
   (H2. "极限和余极限")
   (H3. "子对象")
   (H3. "拉回")
   (H3. "拉回的性质")
   (H3. "极限")
   (H3. "极限的保持")
   (H3. "余极限")
   (H3. "练习")
   (H2. "指数")
   (H3. "范畴中的指数")
   (H3. "笛卡尔闭范畴")
   (H3. "Heyting代数")
   (H3. "命题演算")
   (H3. "CCC的等式性定义")
   (H3. $lambda "演算")
   
   (H2. "自然性")
   (H2. "图表的范畴")
   (H2. "伴随")
   (H2. "单子和代数")
   ))