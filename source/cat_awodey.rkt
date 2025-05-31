#lang racket
(provide cat_awodey.html)
(require SMathML)
(define $<=_A (_ $<= $A))
(define $<=_B (_ $<= $B))
(define $& (Mo "&amp;"))
(define-infix*
  (&<=_A $<=_A)
  (&<=_B $<=_B)
  (&& $&))
(define $.
  (Mo "." #:attr* '((lspace "0"))))
(define (card A)
  (&abs A))
(define (∃ Q P)
  (: $exists Q $. P))
(define (_cm A . x*)
  (_ A (apply &cm x*)))
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
  (&cod $cod "cod"))
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
  (Rel "Rel"))
(define $fin (Mi "fin"))
(define Sets_fin (_ Sets $fin))
(define (MBL label exp)
  (MB (Mtable #:attr*
              '((columnalign "left center right")
                (width "100%"))
              (Mtr (Mtd (Mphantom label))
                   (Mtd exp)
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
  (Remark "评注" "remark"))
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
   (H2 "前言")
   (P "当我们已经有了Mac Lane的" (Em "Categories for the Working Mathematician")
      ", 为什么还要写一本新的范畴论教科书呢? "
      "简而言之, 因为Mac Lane的书是为工作的数学家准备的. "
      "在范畴论渗透进各种各样的领域并出现在课程里30年之后, "
      "现在需要的是一本供所有人阅读的书籍.")
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
           )
       )
   (H3. "同构")
   (H3. "范畴的构造")
   (H3. "自由范畴")
   (H3. "基础: 大, 小, 以及局部小")
   (H3. "练习")
   (H2. "抽象结构")
   (H3. "满态射和单态射")
   (H3. "始对象和终对象")
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
   (H2. "极限和余极限")
   (H2. "指数")
   (H2. "自然性")
   (H2. "图表的范畴")
   (H2. "伴随")
   (H2. "单子和代数")
   ))