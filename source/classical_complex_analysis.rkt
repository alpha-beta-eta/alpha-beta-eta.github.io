#lang racket
(provide classical_complex_analysis.html)
(require SMathML)
(define (Holomorphic U)
  (app $H U))
(define (openBall x r)
  (appl $B x r))
(define (trineq x y z)
  (&<= (&d x z) (&+ (&d x y) (&d y z))))
(define (&d x y) (appl $d x y))
(define (symmetry & x y)
  (&= (& x y) (& y x)))
(define @/ (@lize &/))
(define $⊥ (Mo "&perp;"))
(define (&⊥ u) (^ u $⊥))
(define (\[\) a b)
  (Mrow $lb a $cm b $rp))
(define LC
  (case-lambda
    ((u) u)
    ((k u) (&i* k u))
    ((k u . arg*) (&+ (&i* k u) (apply LC arg*)))))
(define (@LC . arg*)
  (@ (apply LC arg*)))
(define ϵ_0^* (_^ $epsilon $0 $*))
(define ϵ_1^* (_^ $epsilon $1 $*))
(define ϵ_2^* (_^ $epsilon $2 $*))
(define (format-num section index)
  (and index
       (format "~a.~a" (cadr (reverse section)) index)))
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
(define classical_complex_analysis.html
  (TnTmPrelude
   #:title "复平面中的古典分析笔记"
   #:css "styles.css"
   (H1 "复平面中的古典分析笔记")
   (H2 "第0章 预备")
   (H3 "第0.1节 集合论")
   (H3 "第0.2节 代数")
   (H3 "第0.3节 战场")
   (P $CC "是在" $RR "向量空间" (&:= $RR^2 (&c* $RR $RR))
      "之中引入一个乘法得到的. 一个自然的要求是这个乘法使得" $RR^2
      "成为一个" $RR "代数而" (&:= $epsilon_1 (tu0 $1 $0))
      "充当了幺元. 设" (&:= $epsilon_2 (tu0 $0 $1))
      ", 这意味着对于所有的" (∈ $a $b $c $d $RR) "有"
      (MB (deriv (&i* (tu0 $a $b) (tu0 $c $d))
                 (&i* (@LC $a $epsilon_1 $b $epsilon_2)
                      (@LC $c $epsilon_1 $d $epsilon_2))
                 (&+ (&i* $a $c $epsilon_1)
                     (&i* $b $d $epsilon_2 $epsilon_2)
                     (&i* (@LC $a $d $b $c) $epsilon_2))))
      "这个乘法显然是交换的, 而且只要知道" (&i* $epsilon_2 $epsilon_2)
      ", 整个积就是完全确定的. 更进一步的自然要求是向量之积的长度等于其"
      "长度之积, 而这个要求就足以将该乘法确定下来. 设"
      (&:= (tu0 $x $y) (&i* $epsilon_2 $epsilon_2))
      ". 首先, 这个要求可以推出" (&= (&+ $x^2 $y^2) $1)
      ". 其次, 考虑" (&+ $epsilon_1 $epsilon_2) "和"
      (&- $epsilon_1 $epsilon_2) "相乘. 它们的长度都是" (Msqrt $2)
      ", 所以乘积的长度应该是" $2 ", 于是"
      (MB (deriv (&i* (@+ $epsilon_1 $epsilon_2)
                      (@- $epsilon_1 $epsilon_2))
                 (&- $epsilon_1 (&i* $epsilon_2 $epsilon_2))
                 (&- (&i* (@- $1 $x) $epsilon_1)
                     (&i* $y $epsilon_2))))
      "的长度也应该是" $2 ", 那么"
      (MB (deriv (Msqrt (&+ (^ (@- $1 $x) $2) $y^2))
                 (Msqrt (&+ (&- $1 (&i* $2 $x))
                            $x^2 $y^2))
                 (Msqrt (&- $2 (&i* $2 $x)))
                 $2))
      "这可以推出"
      (&cm (&= $x $-1) (&= $y $0)
           (&= (&i* $epsilon_2 $epsilon_2) (&- $epsilon_1)))
      ". " $CC "中的乘法因此必然是"
      (MB (&= (&i* (tu0 $a $b) (tu0 $c $d))
              (tu0 (&- (&i* $a $c) (&i* $b $d))
                   (&+ (&i* $a $d) (&i* $b $c)))) ".")
      "容易验证代数恒等式"
      (MB (&= (&+ (^ (@- (&i* $a $c) (&i* $b $d)) $2)
                  (^ (@+ (&i* $a $d) (&i* $b $c)) $2))
              (&i* (@+ $a^2 $b^2) (@+ $c^2 $d^2))))
      "这个积的确保持向量长度, 或者说对于长度具有乘性.")
   (P "一个有趣的事实是, François Viète所谓的三角演算"
      "可以称得上是对于复数的早期几何预示. 他考虑了将一个直角边为"
      $a "和" $b "而斜边为" $c "的直角三角形&quot;乘上&quot;一个直角边为"
      $alpha "和" $beta "而斜边为" $gamma "的直角三角形以得到一个斜边为"
      (&i* $c $gamma) "的直角三角形. 作为结果的直角三角形的直角边分别为"
      (&- (&i* $a $alpha) (&i* $b $beta)) "和"
      (&+ (&i* $a $beta) (&i* $b $alpha)) ". 实际上, Viète的著作里没有写"
      "绝对值, 因为他总考虑正值. (而且, 更准确地说, Viète考虑的其实是"
      (&- (&i* $b $beta) (&i* $a $alpha))
      ".) 对于Viète而言, 他已经知道了代数恒等式"
      (MB (&= (&i* (@+ $a^2 $b^2) (@+ $alpha^2 $beta^2))
              (&+ (^ (@- (&i* $a $alpha) (&i* $b $beta)) $2)
                  (^ (@+ (&i* $a $beta) (&i* $b $alpha)) $2))))
      "用以将平方和之积表达为平方和.")
   (P "复数系统构造中所牵涉的长度的几何概念实际上是可以避免的. 我们可以转而要求使得"
      $RR^2 "成为一个" $RR "代数的乘法也使其成为一个" (Em "整环")
      " (也就是非平凡的无零因子的含幺交换环) 并具有一个乘法恒元" ϵ_1^*
      ", 无零因子是一个弱于保持向量长度的条件. (这个想法可以追溯至1867年"
      "Hermann Hankel的所作所为. Hermann Hankel是一个数学家和数学史家, 其最早意识到"
      "Grassman代数的重要性.) 将" (setE ϵ_1^*) "扩展为一个基"
      (setE ϵ_0^* ϵ_1^*) ", 那么存在" (∈ $c $d $RR) "满足"
      (&= (&i* ϵ_0^* ϵ_0^*) (LC $c ϵ_1^* (&i* $2 $d) ϵ_0^*))
      ". 也就是说, " (&= (^ (@- ϵ_0^* (&i* $d ϵ_1^*)) $2) (&i* (@+ $c $d^2) ϵ_1^*))
      ". 如果" (&:= $k (&+ $c $d^2)) "是非负的, 那么"
      (: ϵ_0^* $- (&i* $d ϵ_1^*) $+- (&i* (Msqrt $k) ϵ_1^*))
      "均不可能为" $0 ", 但是它们的积却是" $0 ", 因此只能有" (&< $k $0) ". 令"
      (&:= ϵ_2^* (&i* (@/ $1 (Msqrt (&- $k))) (@- ϵ_0^* (&i* $d ϵ_1^*))))
      ", 其线性无关于" ϵ_1^* ", 满足" (&= (&i* ϵ_2^* ϵ_2^*) (&- ϵ_1^*))
      ". 在基" (setE ϵ_1^* ϵ_2^*) "下乘法具有之前的形式:"
      (MB (&= (&i* (@LC $a ϵ_1^* $b ϵ_2^*)
                   (@LC $c ϵ_1^* $d ϵ_2^*))
              (LC (@- (&i* $a $c) (&i* $b $d)) ϵ_1^*
                  (@+ (&i* $a $d) (&i* $b $c)) ϵ_2^*)) "."))
   (P "乘法的另一个几何性质在于其赋予乘积向量的方向. 如果" (&:= $u (tu0 $a $b))
      "是具有单位长度的向量, 而" (&:= $v (tu0 $c $d)) "是任意的向量, 那么平凡的计算"
      "告诉我们" (&i* $u $v) "是向量" (LC $c $u $d (&⊥ $u)) ", 其中"
      (&:= (&⊥ $u) (tu0 (&- $b) $a)) ". 换言之, " (&i* $u $v) "相对于基"
      (setE $u (&⊥ $u)) "的坐标是" $v "相对于基" (setE $epsilon_1 $epsilon_2)
      "的坐标. 也就是说, " (&i* $u $v) "是将" $v "旋转" $u "的结果. 反过来, 对于"
      (&= $u $v $epsilon_2) ", 这可以推出" (&= (&i* $epsilon_2 $epsilon_2) (&- $epsilon_1))
      ". 那么, 这个性质也要求乘法具有之前我们所推出的令人熟悉的形式. 而且, 每个旋转变换都是"
      "乘上某个具有单位长度的向量" $u ", 因为每个" $RR^2 "的自映射若固定" (tu0 $0 $0)
      "且保持距离则必然具有" (&\|-> (tu0 $x $y) (&i* (tu0 $a $b) (tu0 $x $y))) "或者"
      (&\|-> (tu0 $x $y) (&i* (tu0 $a $b) (tu0 $x (&- $y))))
      "的形式, 其中" (tu0 $a $b) "是一个具有单位长度的向量. 既然"
      (&\|-> (tu0 $x $y) (tu0 $x (&- $y))) "是一个" $RR "线性变换, 其非恒等算子且固定了非零向量"
      $epsilon_1 ", 那么它肯定不是旋转. 于是, 此变换再复合一个旋转得到的"
      (&\|-> (tu0 $x $y) (&i* (tu0 $a $b) (tu0 $x (&- $y)))) "当然也不可能是旋转变换.")
   (P "因为旋转性质和对于长度具有乘性是相当有用的, 所以该" $RR^2 "中的乘法被用作定义" $CC
      "的运算. 其不仅是一个" $RR "代数, 还是一个域. 非零元素" (tu0 $a $b) "的乘法逆元为"
      (tu0 (&i* (inv (@+ $a^2 $b^2)) $a) (&- (&i* (inv (@+ $a^2 $b^2)) $b)))
      ". 对于长度具有乘性其实是代数乘法上的一个相当强的假设, 如著名的Gelfand-Mazur定理"
      "所刻画的那样: 任意的装备了一个乘性范数的" $CC "代数都必然拥有"
      "一个乘法恒元, 且其为该乘法恒元的所有" $CC "倍数构成的集合. (?不是很懂)")
   (P "当" $RR^2 "的元素写成列向量的形式时, 乘上" (tu0 $a $b) "这个" $RR
      "线性变换由矩阵" (Mat ($a (&- $b)) ($b $a)) "表示. 所有这样的" (&c* $2 $2)
      "实矩阵构成的集合在通常的矩阵乘法下是一个" $RR "代数, 其同构于装备了复数乘法的"
      $RR^2 ". " $CC "实际上可以被定义为这个矩阵的子代数.")
   (P "如果将" $RR^2 "考虑成所有从" (setE $1 $2) "到" $RR "的函数构成的集合, 那么在"
      "通常的函数运算下其成为一个交换含幺" $RR "代数. 但是, 这个逐坐标乘法"
      "没什么好的性质, 所以这个代数尚未发现什么重要用途.")
   (P "除了恒等映射之外的唯一的" $CC "的unital " $RR "代数自同态是"
      (&\|-> (tu0 $x $y) (tu0 $x (&- $y))) ". 这是一个对合的例子, 其被称为(复)共轭. "
      $z "的像记作" (OverBar $z) ", 即" $z "的共轭. 数字" (tu0 $0 $1)
      "被称为虚数单位, 一般(遵循Euler)记成" $i ", 其实倍数被称为纯虚数. " $RR
      "向量空间的等式" (&= $z (tu0 $x $y) (&+ (&d* $x (tu0 $1 $0)) (&d* $y (tu0 $0 $1))))
      "通常被记为" (&= (tu0 $x $y) (&+ $x (&i* $y $i))) ", 即" $RR
      "被等同为" $RR^2 "中与之同构的复制" (&d* $RR (tu0 $1 $0)) ". 我们将"
      (Msqrt (&i* $z (OverBar $z))) "记成" (&abs $z) ", 即"
      (&= (&i* $z (OverBar $z)) (&+ $x^2 $y^2)) "的非负平方根. 这个数字被称为向量" $z
      "的范数, 长度, 模 (modulus), 或者绝对值. " $x "被称为" $z "的实部或者横坐标, "
      $y "被称为" $z "的虚部或者纵坐标, 记" (&:= (Re $z) $x) "和" (&:= (Im $z) $y) ". "
      (&= (^ (&abs (&i* $z $w)) $2) (&i* $z $w (OverBar (&i* $z $w)))
          (&i* $z $w (OverBar $z) (OverBar $w))
          (&i* (^ (&abs $z) $2) (^ (&abs $w) $2)))
      ", 以及从" (&= (&i* $z (OverBar $z)) (^ (&abs $z) $2)) "中可以看出"
      (&= (inv $z) (&i* (^ (&abs $z) $-2) (OverBar $z))) ".")
   
   (H3 "第0.4节 度量空间")
   (P "回忆一下一个度量空间是一个(非空)集合, 附带有一个距离函数或者说度量"
      (func $d (&c* $X $X) (\[\) $0 $inf)) ", 其对于所有" (∈ $x_1 $x_2 $x_3 $X) "满足"
      (Ol (Li (symmetry &d $x_1 $x_2) " (对称性);")
          (Li (&<=> (&= (&d $x_1 $x_2) $0) (&= $x_1 $x_2)) " (自反性);")
          (Li (trineq $x_1 $x_2 $x_3) " (三角不等式)."))
      "从形式上说, 一个度量空间是一个序对" (tu0 $X $d) ", 但是一般我们都说度量空间"
      $X ". 如果" (∈ $x_0 $X) "且" (∈ $r (\[\) $0 $inf)) ", 那么集合"
      (setI (∈ $x $X) (&< (&d $x $x_0) $r)) "被称为以" $x_0 "为中心而"
      $r "为半径的开球. (这个定义和很多书不一样, 那里排除了半径为零的情况.) 其被记为"
      (openBall $x_0 $r) ". 度量空间中的一个集合被称为有界的, 如果其完全落于某个"
      "开球之中. 一个集合被称为是开的, 如果其是开球之并. 一个集合其被称为是闭集, "
      "如果其补是开集. "
      )
   (H2. "曲线, 连通性和凸性")
   (H3. "连通性的初等事实")
   (P "本节的" $M "代表一个任意的度量空间.")
   
   (H2. "(复)导数和(曲线)积分")
   (P "这简短的一章有大约一半是由非常基本又非常简单的材料构成的, "
      
      )
   (H3. "全纯函数和调和函数")
   (P "古典复分析, 即本书的主题, 主要是对于全纯函数的研究:")
   ((Definition)
    "如果" $f "是定义在点" $z_0 "的一个邻域内的一个复值函数, "
    "我们称" $f "在" $z_0 "处" (Em "(复)可微") ", 如果复数之商"
    (&/ (@- (app $f $z) (app $f $z_0)) (@- $z $z_0))
    "在" $z "接近" $z_0 "时在" $CC "中有一个极限. "
    "当其存在时, 这个极限被称为" (Em $f "在" $z_0 "处的导数")
    ", 记作" (app $f^ $z_0) ". 如果" $f "在" $CC
    "的一个开子集" $U "的每一个点处都(复)可微, 那么我们就称"
    (Em $f "在" $U "中是全纯的") ", 并以"
    (Holomorphic $U) "代表由所有这样的函数构成的集合. "
    "这个替代性术语是为了将复可微与实可微 (对于"
    $RR^2 "中的" $RR^2 "值函数而言) 区分开来. "
    
    )
   (H3. "沿着曲线的积分")
   
   ))