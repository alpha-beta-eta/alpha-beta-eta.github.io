#lang racket
(provide manifolds.html)
(require SMathML)
(define $~ (Mo "~"))
(define-infix*
  (&~ $~))
(define $sin (Mi "sin"))
(define $cos (Mi "cos"))
(define (&sin x)
  (ap $sin x))
(define (&cos x)
  (ap $cos x))
(define $d:form
  (Mo "d" #:attr* '((lspace "0") (rspace "0"))))
(define (&d x)
  (ap $d:form x))
(define $integral (Mo "&int;"))
(define (integral a b f x)
  (: (_^ $integral a b) f (&d x)))
(define C^inf (^ $C $inf))
(define $partial (Mi "&part;"))
(define &partial
  (case-lambda
    ((x) (ap $partial x))
    ((n x) (ap (^ $partial n) x))))
(define (H4. #:attr* [attr* '()] #:id [id #f] #:switch? [switch? #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?)
    ,attr* . ,html*))
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
  (Proposition "命题" "proposition")
  (Lemma "引理" "lemma")
  (Problem "问题" "problem"))
(define (MBL label . exp*)
  (MB (Mtable
       #:attr*
       '((columnalign "left center right")
         (width "100%"))
       (Mtr (Mtd (Mphantom label))
            (apply Mtd exp*)
            (Mtd label)))))
(define manifolds.html
  (TnTmPrelude
   #:title "流形引论"
   #:css "styles.css"
   (H1. "流形引论")
   (P "这本书被一些人认为是咸鱼之友, 也有一些人说这本书什么内容也没有. "
      "是这样的, 这本书没有太多内容, 但是篇幅也不短, "
      "因为所有的篇幅都花在了向像我这样完全不懂分析的人解释基本概念和基本想法了.")
   (H2 "简介")
   (P "本科微积分从实直线上的函数的微分和积分一路推进至平面和三维空间之中的函数. "
      "然后, 学生会遇到向量值函数并学习曲线和曲面上的积分. "
      "实分析将微分和积分演算从" $RR^3 "扩展至" $RR^n
      ". 本书是关于将微积分从曲线和曲面推广至更高维度的.")
   (P "光滑曲线和曲面的高维类比被称为" (Em "流形")
      ". 向量微积分的构造和定理在流形这样更为一般的环境下变得更加简单了; "
      "梯度, 旋度, 散度都是外导数的特殊情形, 而线积分的基本定理, "
      "Green定理, Stokes定理, 散度定理都是流形版本的一般Stokes定理的不同表现.")
   (P "即便我们只关心我们所寓居的三维空间, 高维流形也会出现. "
      "例如, 如果我们将先旋转后平移称为仿射运动, 那么由" $RR^3
      "中的所有仿射运动构成的集合是一个六维流形. "
      "而且, 这个六维流形不是" $RR^6 ".")
   (P "我们认为两个流形是拓扑相同的, 如果存在一个它们之间的同胚, "
      "即在两个方向上都连续的双射. 流形的拓扑不变量是在同胚下保持不变的性质, "
      "例如紧性. 另外一个例子是流形的连通分量的数目. "
      "令人感兴趣的是, 我们可以使用流形上的微分和积分演算研究流形的拓扑. "
      "我们可以得到一个称为流形的de Rham上同调的更为精细的不变量.")
   (P "我们的计划如下. 首先, 我们以适合于流形理论的方式重新呈现了"
      $RR^n "上的微积分. 我们的做法是赋予符号" (&cm (&d $x) (&d $y) (&d $z))
      "以自足的意义, 即作为微分形式, 而非仅仅是一种本科生微积分中的记号.")
   (P "尽管从逻辑上说在流形理论之前建立" $RR^n "上的微分形式并不必要, "
      "毕竟第5章里流形上的微分形式的理论已经涵盖了" $RR^n
      "上的情形, 但是从教育学的角度来看, 单独处理" $RR^n
      "更好, 因为" $RR^n "上的情况展现了微分形式和外微分本质上的简单性.")
   (H2. "Euclid空间")
   (P "Euclid空间" $RR^n "是所有流形的原型. "
      "不只是因为其是最简单的流形, "
      "而且从局部上每个流形看起来都像" $RR^n ". 对于" $RR^n
      "的良好理解对于将微分和积分演算推广至流形的情况是基本的.")
   (P "Euclid空间的特别之处在于其有着一集标准的全局坐标. "
      "这既是一种赐福, 也是一种障碍. "
      "说是赐福是因为所有" $RR^n "上的构造都可以基于标准坐标定义而"
      "所有的计算都可以显式执行. 说是障碍是因为基于坐标定义的话, "
      "不容易看出来什么概念是内蕴的, 即独立于坐标. "
      "既然一般的流形没有标准坐标可言, "
      "只有独立于坐标的概念才能够在流形上成立. "
      "例如, 实际上" $n "维流形上不能够对于函数进行积分, "
      "因为函数的积分依赖于一集坐标. "
      "能够进行积分的对象是微分形式. "
      "只是因为全局坐标的存在性允许函数和" $RR^n
      "上的微分" $n "-形式等同起来, 由此"
      $RR^n "上的函数的积分才变得可能起来.")
   (P "本章我们的目的在于以适合于推广至流形的无坐标方式重新呈现"
      $RR^n "上的微积分. 为此我们不把切向量视为箭头或者一列数字, "
      "而是当作函数上的导子. 然后我们呈现了Hermann Grassmann"
      "对于向量空间上的交错多线性函数的形式化, "
      "其奠定了微分形式理论的基础. 最后我们引入了" $RR^n
      "上的微分形式, 以及两个基本运算, 楔积和外导数, "
      "并展现了其是如何泛化和简化" $RR^3 "中的向量微积分的.")
   (H3. "Euclid空间上的光滑函数")
   (P "对于 "C^inf "函数的演算将会是我们研究高维流形的主要工具. "
      "出于这个原因, 我们从回顾" $RR^n "上的" C^inf
      "函数开始.")
   (H4. C^inf "和解析函数的对比")
   (P "我们将" $RR^n "上的诸坐标记为" (&cm $x^1 $..h $x^n)
      "并令" (&= $p (tu0 $p^1 $..h $p^n)) "是"
      $RR^n "中的一个开集" $U "的一个点. "
      "为了与微分几何的约定保持一致, 坐标的索引是" (Em "上标")
      "而非下标. 对于上标和下标的规则的解释见于第1.4.7小节.")
   ((Definition)
    "令" $k "是一个非负整数. 一个实值函数" (func $f $U $RR)
    "被称为在" (∈ $p $U) "处是" $C^k
    "的, 如果其所有阶数" (&<= $j $k) "的各偏导数"
    (MB (~ (ap (^ $partial $j) $f)
           (&i* (ap $partial (^ $x $i_1))
                $..c
                (ap $partial (^ $x $i_j)))))
    "均存在且在" $p "处连续. 函数"
    (func $f $U $RR) "在" $p
    "处是" C^inf "的, 如果对于" (&>= $k $0)
    "其都是" $C^k "的; 换言之, 其任意阶数的诸偏导数"
    (&/ (ap (^ $partial $j) $f)
        (&i* (ap $partial (^ $x $i_1))
             $..c
             (ap $partial (^ $x $i_j))))
    "均存在且在" $p "处连续. 一个向量值函数"
    (func $f $U $RR^m) "被称为在" $p "处是"
    $C^k "的, 如果其每个分量函数" (&cm $f^1 $..h $f^k)
    "在" $p "处都是" $C^k "的. 我们称"
    (func $f $U $RR^m) "在" $U "上是"
    $C^k "的, 如果其在每个" $U
    "中的点处都是" $C^k "的. 开集" $U
    "上的" C^inf "函数的定义也是类似的. "
    "我们将术语" (Q C^inf) "和" (Q "光滑")
    "视为同义词. {译注: " $C^k "和" C^inf
    "也可以用可微性而非偏导数来描述就是了.}")
   ((Example)
    (Ol #:attr* '((type "i"))
        (Li $U "上的一个" $C^0 "函数即一个" $U
            "上的连续函数.")
        (Li "令" (func $f $RR $RR) "为"
            (&= (app $f $x) (^ $x (&/ $1 $3)))
            ", 那么"
            (MB (&= (app $f^ $x)
                    (Choice0
                     ((&i* 1/3 (^ $x (&- (&/ $2 $3))))
                      "对于" (&!= $x $0))
                     ("未定义"
                      "对于" (&= $x $0)))))
            "故函数" $f "在" (&= $x $0)
            "处是" $C^0 "的但不是" $C^1 "的.")
        (Li "令" (func $g $RR $RR) "由"
            (MB (&= (app $g $x)
                    (integral $0 $x (app $f $t) $t)
                    (integral $0 $x (^ $t (&/ $1 $3)) $t)
                    (&i* 3/4 (^ $x (&/ $4 $3)))))
            "定义, 那么"
            (&= (app $g^ $x) (app $f $x) (^ $x (&/ $1 $3)))
            ", 于是" (app $g $x) "在" (&= $x $0)
            "处是" $C^1 "的但不是" $C^2 "的. 以相同的方式, "
            "我们可以构造在给定点是" $C^k "的但不是"
            (^ $C (&+ $k $1)) "的函数.")
        (Li "实直线上的多项式函数, 正弦函数, "
            "余弦函数, 指数函数都是" C^inf "的.")))
   (P $RR^n "中的一个点的" (Em "邻域")
      "是一个包含这个点的开集. 函数在" $p "处是"
      (Em "实解析") "的, 如果在" $p "的某个邻域之中, "
      "其等于它在" $p "处的Taylor级数:"
      (eqn*
       ((app $f $x)
        $=
        (&+ (app $f $p)
            (sum $i
                 (&i* (app (~ (&partial $f)
                              (&partial $x^i))
                           $p)
                      (@- $x^i $p^i)))
            (&i* (~ $1 (&fact $2))
                 (sum (&cm $i $j)
                      (&i* (app (~ (&partial $2 $f)
                                   (&i* (&partial $x^i)
                                        (&partial $x^j)))
                                $p)
                           (@- $x^i $p^i)
                           (@- $x^j $p^j))))))
       ($
        $+
        (&+ $..c
            (&i* (~ $1 (&fact $k))
                 (sum (&cm $i_1 $..h $i_k)
                      (&i* (app (~ (&partial $k $f)
                                   (&i* (&partial (^ $x $i_1))
                                        $..c
                                        (&partial (^ $x $i_k))))
                                $p)
                           (@- (^ $x $i_1) (^ $p $i_1))
                           $..c
                           (@- (^ $x $i_k) (^ $p $i_k)))))
            $..c)))
      "其中一般的项的求和布于所有"
      (&<= $1 (&cm $i_1 $..h $i_k) $n)
      "之上.")
   (P "实解析函数必然是" C^inf "的, 因为正如我们在实分析中所学到的, "
      "一个收敛的幂级数在其收敛域内可以被逐项微分. 例如, 如果"
      (MB (&= (app $f $x) (&sin $x)
              (: $x $- (&i* (~ $1 (&fact $3)) $x^3)
                 $+ (&i* (~ $1 (&fact $5)) $x^5)
                 $- $..c)))
      "那么逐项微分将给出"
      (MB (&= (app $f^ $x) (&cos $x)
              (: $1 $- (&i* (~ $1 (&fact $2)) $x^2)
                 $+ (&i* (~ $1 (&fact $4)) $x^4)
                 $- $..c))))
   (P "以下例子表明一个" C^inf "函数不必是实解析的. "
      "想法在于构造一个" $RR "上的" C^inf
      "函数, 其图尽管不是水平的, 但是却在接近" $0
      "时" (Q "非常扁平") ", 意即其所有在" $0
      "处的导数都消失了. {译注: 这里消失 (vanish) "
      "的意思是为零.}")
   todo.svg
   ((Example)
    "(一个在" $0 "处非常扁平的" C^inf "函数). 定义"
    $RR "上的" (app $f $x) "为"
    (MB (&= (app $f $x)
            (Choice0
             ((^ $e (&- (&/ $1 $x)))
              "对于" (&> $x $0))
             ($0
              "对于" (&<= $x $0)))))
    "(见图1.1.) 根据归纳, 我们可以表明" $f
    "在" $RR "上是" C^inf "的, 并且导数"
    (app (^ $f (@ $k)) $0) "对于"
    (&>= $k $0) "都等于" $0
    " (问题1.2).")
   (P "这个函数原点处的Taylor级数在其原点的任意邻域内都恒为零, "
      "因为所有的导数" (app (^ $f (@ $k)) $0) "都等于"
      $0 ". 因此, " (app $f $x) "不可能等于其Taylor级数, "
      (app $f $x) "在" $0 "处不是实解析的.")
   (H4. "带余项的Taylor定理")
   (P "尽管" C^inf "函数不必等于其Taylor级数, "
      "但是对于我们的目的而言, " C^inf
      "函数的带余项的Taylor定理往往就足够好用了. "
      "在以下的引理之中, 我们证明了最初的情形, "
      "其中的Taylor级数仅由常数项" (app $f $p)
      "构成.")
   (P "我们称" $RR^n "的一个子集" $S "相对于某个"
      $S "中的点" $p "是" (Em "星形")
      "的, 如果对于每个" $S "中的" $x
      ", 从" $p "到" $x "的线段都落于"
      $S "之中 (图1.2).")
   todo.svg
   ((Lemma)
    "(带余项的Taylor定理). 令" $f
    "在" $RR^n "的一个开子集" $U
    "上是一个" C^inf "函数, 并且"
    $U "相对于" $U "中的某个点"
    (&= $p (tu0 $p^1 $..h $p^n))
    "是星形的, 那么存在函数"
    (∈ (app $g_1 $x) $..h (app $g_n $x)
       (app C^inf $U))
    "使得"
    
    )
   (H4. "问题" #:switch? #t)
   ((Problem)
    
    )
   ((Problem)
    
    )
   (H3. $RR^n "中的切向量作为导子")
   (P "在初等微积分中, 我们通常将在" $RR^3
      "中的某个点" $p "处的向量代数地表示为一列数字"
      (MB (&= $v (Mat ($v^1) ($v^2) ($v^3))))
      "或者几何地表示为一个从" $p "出发的箭头 (图2.1)."
      todo.svg)
   (P "回忆一下, " $RR^3 "中的一个曲面的一个截平面"
      "是一个由曲面的三个点所确定的平面. "
      "当这三个点接近于曲面上的一个点" $p
      "时, 如果相对应的截平面接近于一个极限位置, "
      "那么作为截平面极限位置的平面就被称为曲面在"
      $p "点处的切平面. 从直觉上来说, "
      "曲面于" $p "处的切平面是" $RR^3
      "中恰好与曲面在" $p "处" (Q "接触")
      "的平面. 一个点" $p "处的向量切于"
      $RR^3 "中的曲面, 如果其落于" $p
      "处的切平面之中 (图2.2).")
   todo.svg
   (P "这样定义曲面的切向量预先假定了曲面是嵌入于"
      "一个Euclid空间之中的, "
      )
   (H4. "方向导数")
   
   (H4. "Germs of Functions")
   (P "一个集合" $S "上的一个" (Em "关系")
      "是" (&c* $S $S) "的一个子集" $R
      ". 对于" $S "中的" (&cm $x $y)
      ", 我们记" (&~ $x $y) "当且仅当"
      (∈ (tu0 $x $y) $R) ". 关系" $R
      "是一个" (Em "等价关系")
      ", 如果其对于所有的" (∈ $x $y $z $S)
      "满足以下三条性质:"
      
      )
   (H3. "多重余向量的外代数")
   (H3. $RR^n "上的微分形式")
   ))