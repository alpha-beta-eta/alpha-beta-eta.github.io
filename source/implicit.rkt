#lang racket
(provide implicit.html)
(require SMathML)
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
       (apply string-append
              (add-between
               (map number->string
                    (cdr (reverse (cons index section))))
               "."))))
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
(define (MBL label . exp*)
  (MB (Mtable
       #:attr*
       '((columnalign "left center right")
         (width "100%"))
       (Mtr (Mtd (Mphantom label))
            (apply Mtd exp*)
            (Mtd label)))))
(define $cos (Mi "cos"))
(define (&cos x)
  (app $cos x))
(define $~ (Mo "~"))
(define (Δ x) (ap $Delta:normal x))
(define $partial (Mi "&part;"))
(define (∂ x) (ap $partial x))
(define (∂/∂ f x)
  (&/ (∂ f) (∂ x)))
(define (∂//∂ f x)
  (ap (~ $partial (∂ x)) f))
(define $\| (Mo "|"))
(define (evaluate f x)
  (: f (_ $\| x)))
(define implicit.html
  (TnTmPrelude
   #:title "隐函数定理"
   #:css "styles.css"
   (H1. "隐函数定理")
   (H2. "隐函数定理引论")
   (H3. "隐函数")
   (P "对于微积分的初学者而言, 函数由诸如"
      (MBL "(1.1)"
           (&= (app $f $x)
               (&- (&+ $x^3 (&i* $2 $x^2))
                   $x $3)))
      (MBL "(1.2)"
           (&= (app $g $y)
               (Msqrt (&+ $y^2 $1))))
      (MBL "(1.3)"
           (&= (app $h $t)
               (&cos (&i* $2 $pi $t))))
      "这样的解析表达式给出. "
      "实际上, 250年前这是L&eacute;onard Euler (1707-1783) "
      "所采取的方法 (见Euler [EB88]):"
      (Blockquote
       "A function of a variable quantity is an analytic expression "
       "composed in any way whatsoever of the variable quantity "
       "and numbers or constant quantities."))
   (P "几乎是在同一时间, 有人发现" (Q "由公式给出函数")
      "这一概念对于微积分的目的而言太过具有限制性了. 例如,"
      (MBL "(1.4)"
           $y^5 $+ (&i* 16 $y)
           $- (&i* 32 $x^3)
           $+ (&i* 32 $x)
           $= $0)
      todo.svg
      "的轨迹定义了一个很好的" $RR^2 "的子集, 草绘于图1.1之中. "
      "这个图片 (figure) 让我们怀疑这个轨迹是不是" $y "作为" $x
      "的函数的图 (graph). 但是, 并没有可以用来描述这个函数的公式存在.")
   (P "与将函数当作公式的朴素定义相比, "
      "函数的现代的集合论式的定义基于函数的图 (graph) 陈述. "
      "精确地说, 定义域为" $X "而陪域为" $Y
      "的一个函数" $f "是笛卡尔积"
      (MB (&= (&c* $X $Y)
              (setI (tu0 $x $y)
                    (&cm (∈ $x $X)
                         (∈ $y $Y)))))
      "的一个子集, 其具有性质(i)对于每个" (∈ $x $X)
      ", 存在一个元素" (∈ (tu0 $x $y) $f)
      "; (ii)如果" (∈ (tu0 $x $y) $f) "且"
      (∈ (tu0 $x (^^ $y $~)) $f) ", 那么"
      (&= $y (^^ $y $~)) ". 在这两个性质成立的情况下, "
      (∈ $x $X) "的选择确定了唯一使得"
      (∈ (tu0 $x $y) $f) "的" $y
      "; 藉着唯一性, 我们发现简记"
      (MB (&= $y (app $f $x)))
      "以表达" (∈ (tu0 $x $y) $f) "比较方便.")
   ((Example)
    "由(1.4)所定义的轨迹具有这样的性质, 对于"
    (∈ $x $RR) "的每个选择, 都存在唯一的" (∈ $y $RR)
    "使得序对" (tu0 $x $y) "满足该等式. "
    "因此, 存在一个函数" $f ", 现代意义上的函数, "
    "使得其图" (&= $y (app $f $x))
    "是(1.4)的轨迹.")
   
   (H3. "隐函数定理的一个非正式版本")
   (P "启发式地进行思考, 人们通常期望一个变元的一个方程"
      (MB (&= (app $F $x) $c))
      "足够用来确定" $x "的值, 其中" $c "是一个常数 "
      "(尽管如此, 存在多于一个但有限多个解也是令人毫不意外的). "
      "当存在两个变元时, 人们期望需要通过两个同时的 "
      "(或者说联立的) 方程"
      (eqn*
       ((appl $F $x $y) $= $c)
       ((appl $G $x $y) $= $d))
      "来确定" $x "和" $y "的值, 其中" $c "和" $d "是常数. "
      "一般情况下, 人们期望着具有" $m "个变元的由" $m
      "个方程构成的一个方程组"
      (MBL
       "(1.6)"
       (&Table
        ((appl $F_1 $x_1 $x_2 $..h $x_m) $= $c_1)
        ((appl $F_2 $x_1 $x_2 $..h $x_m) $= $c_2)
        ($ $..v $)
        ((appl $F_m $x_1 $x_2 $..h $x_m) $= $c_m)))
      "恰好就具有用于确定这些变元的值的合适方程数目, "
      "其中" (&cm $c_1 $c_2 $..h $c_m) "是常数. "
      "但是当然了, 我们必须意识到这些方程之间可能存在着冗余. "
      "也就是说, 我们必须验证这个方程组 (或者说系统) "
      "是非退化的&mdash;&mdash;"
      "意即一个特定的行列式非零 (does not vanish).")
   (P "在(1.6)里的方程均为线性方程的情况下, "
      "我们可以诉诸于线性代数来使得我们的启发式思考精确化 "
      "(见任意一本线性代数教科书): "
      "保证(1.6)对于所有常量" $c_i "的值都存在唯一解的"
      "充分必要条件是线性方程组的系数矩阵秩为" $m
      ". {译注: 说白了就是可逆.}")
   (P "我们继续启发式地思考: 如果变量的数目比方程的数目多, 即"
      (MBL
       "(1.7)"
       (&Table
        ((appl $F_1 $x_1 $x_2 $..h $x_n) $= $c_1)
        ((appl $F_2 $x_1 $x_2 $..h $x_n) $= $c_2)
        ($ $..v $)
        ((appl $F_m $x_1 $x_2 $..h $x_n) $= $c_m)))
      "其中" $c_i "仍然是常量而" (&> $n $m)
      ", 那么我们希望将" (&- $n $m) "个额外变量视为参数. "
      "在线性方程组的情况下, 这又是被理解透彻的: "
      "如果系数矩阵的秩为" $m ", 那么就可以将某"
      $m "个变量表达为其余" (&- $n $m)
      "个变量的函数. {译注: 从某种意义上说, "
      "这是通过将自由变元移至等式右侧完成的, "
      "那么此时又变回了之前的良好情形.} "
      "而且, 对于系数矩阵的任意" $m
      "个独立 (线性无关) 列, 相对应的" $m
      "个变元可以被表达为其余变元的函数. "
      "{译注: 仍然可以这么思考, 将其余变量移至右侧后, "
      "又变回了之前的良好情形. "
      "当然了, 这些线性无关列的选择一般的确是不唯一的.}")
   (P "在一般情形下, 与线性情形相对的是, "
      "(1.7)这个方程组定义了一个全然任意的" $RR^n
      "子集 (如果这些函数都是连续函数, 那么这是一个任意的闭子集). "
      "只有在特殊条件下(1.7)才会定义一个隐函数, 其中"
      $m "个变量由其余" (&- $n $m) "个变量确定. "
      "隐函数定理的意图在于为我们提供一个强大的方法, "
      "或者一组强大的方法, 用以确保我们之前的启发式想法能够成立.")
   (P "隐函数定理根植于微分演算, 而微分演算的基岩是线性近似. "
      "据此, 我们在一个点" (tu0 $p_1 $p_2 $..h $p_n)
      "的一个邻域内工作, 其中(1.7)在点" (tu0 $p_1 $p_2 $..h $p_n)
      "处得到满足, 而(1.7)中的诸函数可以由它们的微分 (differential) "
      "进行线性近似. 我们现在就要以非形式化的语言陈述应函数定理 "
      "(之后也会给出更为形式化的说明):"
      (Blockquote
       (B "(Informal) Implicit Function Theorem") (Br)
       "令(1.7)中的诸函数都是连续可微的. 如果(1.7)在"
       (tu0 $p_1 $p_2 $..h $p_n) "处成立, "
       "并且如果当(1.7)的诸函数被替换为其线性近似时特定的"
       $m "个变量可以表达为其余" (&- $n $m)
       "个变量的函数, 那么对于(1.7)本身而言, 在"
       (tu0 $p_1 $p_2 $..h $p_n)
       "的一个邻域内, 相同的这" $m "个变量可以定义为其余"
       (&- $n $m) "个变量的隐函数. 并且, 作为结果的隐函数"
       "是连续可微的, 且其导数可以藉由隐式微分进行计算."))
   (P "让我们来看一个非常简单的例子, "
      "其只有两个变元和一个方程.")
   ((Example)
    "考虑"
    (MBL
     "(1.8)"
     (&= (&+ $x^2 $y^2) $1))
    "由(1.8)所定义的轨迹是以原点为中心的半径为" $1
    "的圆. 对于任意满足(1.8)且" (&!= $q $0)
    "的点" (&= $P (tu0 $p $q))
    ", 在其一个合适的邻域内, 我们都可以解这个方程以将"
    $y "显式表达为"
    (MB (&= $y (&+- (Msqrt (&- $1 $x^2)))))
    "其中正负号的选取依赖于" $q "是为正还是为负. "
    "(类似地, 在" (&!= $p $0) "的情况下, "
    "我们可以解这个方程以将" $x "表达为关于" $y
    "的显式函数.)" (Br)
    "隐函数定理的有用性在于我们可以避免显式解出方程. "
    "为采取隐函数定理的观念, 我们对于(1.8)的左侧进行线性近似. "
    "在一个点" (tu0 $p $q) "的一个邻域内, "
    "一个连续线性可微函数" (appl $F $x $y) "由"
    (MB (&+ (&i* $a (Δ $x))
            (&i* $b (Δ $y))
            $c))
    "所线性近似, 其中" $a "是" (∂/∂ $F $x) "在" $P
    "处求得的值, " (Δ $x) "是" $x "从"
    (&= $P (tu0 $p $q)) "到点" (tu0 $x $y)
    "的过程中所发生的改变, " $b "是"
    (∂/∂ $F $y) "在" $P "处求得的值, "
    (Δ $y) "是" $y "从"
    (&= $P (tu0 $p $q)) "到点" (tu0 $x $y)
    "的过程中所发生的改变, " $c "是" $F
    "在" $P "处的值. 在这个例子里, "
    (&= (appl $F $x $y) (&+ $x^2 $y^2))
    ", 即(1.8)的左侧." (Br)
    "我们计算"
    (MB (&= (evaluate (∂//∂ (@+ $x^2 $y^2) $x)
                      (&= (tu0 $x $y) (tu0 $p $q)))
            (&i* $2 $p)))
    "以及"
    (MB (&= (evaluate (∂//∂ (@+ $x^2 $y^2) $y)
                      (&= (tu0 $x $y) (tu0 $p $q)))
            (&i* $2 $q)))
    "因此, 在满足(1.8)的点" (&= $P (tu0 $x $y))
    "的一个邻域内, (1.8)的左侧由"
    (MB (&= (&+ (&i* (@i* $2 $p) (@- $x $p))
                (&i* (@i* $2 $q) (@- $y $q))
                $1)
            (&- (&+ (&i* $2 $p $x) (&i* $2 $q $y)) $1)))
    "所线性近似. "
    "当我们将(1.8)的左侧替换以其线性近似并进行化简时, "
    "我们就得到了"
    (MBL "(1.9)" (&= (&+ (&i* $p $x) (&i* $q $y)) $1))
    "其当然是圆在点" $P "处的切线方程." (Br)
    "隐函数定理告诉我们, 每当我们可以对于近似线性方程(1.9)将"
    $y "解为" $x "的一个函数时, 那么原本的方程(1.8)就定义了"
    $y "为" $x "的一个隐函数. 显然, 恰当" (&!= $q $0)
    "时我们可以将" $y "作为" $x "的函数解出. "
    )
   (H3. "隐函数定理范式")
   (H2. "历史")
   (H3. "历史性引论")
   (H3. "Newton")
   
   (H2. "基本想法")
   (H2. "应用")
   (H2. "变种和推广")
   ))