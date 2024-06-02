#lang racket
(provide calculus.html)
(require SMathML)
(define (Item label . x*)
  (apply Li (B label) " " x*))
(define (&cup A B)
  (Mrow A $union B))
(define (integral0 fx)
  (integral $a $b fx $x))
(define LC
  (case-lambda
    ((u) u)
    ((k u) (&i* k u))
    ((k u . arg*) (&+ (&i* k u) (apply LC arg*)))))
(define $cos (Mi "cos"))
(define (&cos x) (ap $cos x))
(define $sin (Mi "sin"))
(define (&sin x) (ap $sin x))
(define calculus.html
  (TnTmPrelude
   #:title "微积分"
   #:css "styles.css"
   (P "注记: 本书是经典的分析学/微积分教程, 其循循善诱之风格, "
      "大概只有菲赫金哥尔茨的微积分学教程能与之媲美.")
   (H1 "微积分第一卷")
   (H2 "第1章 积分演算的概念")
   (H3 "第1.6节 作为集合函数的面积概念")
   ((definition #:n ". 面积的公理")
    "我们假定存在一个平面上的可测集合的类" $M:script
    "和一个函数" $a ", 其定义域为" $M:script
    ", 并带有如下性质:"
    (Ol (Li (B "非负性质.") " 对于每个" $M:script
            "中的集合" $S ", 我们有" (&>= (app $a $S) $0) ".")
        (Item "可加性质." "如果" $S "和" $T "在" $M:script
              "之中, 那么" (&cup $S $T) "和" (&cap $S $T)
              "也在" $M:script "之中, 并且"
              (MB (&= (app $a (&cup $S $T))
                      (&- (&+ (app $a $S) (app $a $T))
                          (app $a (&cap $S $T)))) "."))
        (Item "差集性质." "如果" $S "和" $T "在" $M:script
              "之中, 并且" (&sube $S $T) ", 那么"
              (&- $T $S) "也在" $M:script "之中, 并有"
              (&= (app $a (&- $T $S))
                  (&- (app $a $T) (app $a $S))) ".")
        (Item "相合关系之下的不变性."
              "如果" $S "在" $M:script "中且" $T
              "合于" $S ", 那么" $T "也在" $M:script
              "中, 并有" (&= (app $a $S) (app $a $T)) ".")
        (Item "缩放之选择."
              "每个矩形" $R "都在" $M:script "之中, 并且若"
              $R "的两边长为" $h "和" $k "那么"
              (&= (app $a $R) (&i* $h $k)) ".")
        (Item "穷竭性质."
              "令" $Q "是一个集合, 其夹在两个step region "
              $S "和" $T "之间, 于是"
              (MB (&sube $S $Q $T) ".")
              "如果有且仅有一个数字" $c "对于所有这样的"
              $S "和" $T "都满足不等式"
              (MB (&<= (app $a $S) $c (app $a $T)) ",")
              "那么" $Q "是可测的, 并且"
              (&= (app $a $Q) $c) ".")))
   
   (H3 "第1.9节 划分与阶跃函数")
   ((definition #:n ". 阶跃函数")
    "一个以闭区间" (li0 $a $b) "为定义域的函数" $s
    "被称为一个阶跃函数, 如果存在一个" (li0 $a $b)
    "的划分" (&= $P (setE $x_0 $x_1 $..h $x_n))
    "满足" $s "在" $P "的每个开子区间上都是常函数. "
    "也就是说, 对于每个" (&= $k (&cm $1 $2 $..h $n))
    ", 存在实数" $s_k "满足"
    (MB (&= (app $s $x) $s_k) ", 如果"
        (&< (_ $x (&- $k $1)) $x $x_k) ".")
    "阶跃函数也被称为分段常函数.")
   
   (H3 "第1.10节 阶跃函数之和与积")
   (P "阶跃函数之和与积仍然是阶跃函数.")
   (H3 "第1.12节 阶跃函数积分之定义")
   ((definition #:n ". 阶跃函数的积分")
    $s "从" $a "到" $b "的积分, 以符号"
    (integral $a $b (app $s $x) $x)
    "表示, 由以下公式定义:"
    (MB (&= (integral $a $b (app $s $x) $x)
            (sum (&= $k $1) $n
                 (&d* $s_k
                      (@- $x_k (_ $x (&- $k $1)))))) "."))
   (H3 "第1.13节 阶跃函数积分之性质")
   ((theorem #:n "1.2. 可加性质")
    (MB (&= (integral0 (bra0 (&+ (app $s $x) (app $t $x))))
            (&+ (integral0 (app $s $x))
                (integral0 (app $t $x)))) "."))
   ((theorem #:n "1.3. 齐次性质")
    (MB (&= (integral0 (&d* $c (app $s $x)))
            (&i* $c (integral0 (app $s $x)))) "."))
   ((theorem #:n "1.4. 线性性质")
    (MB (&= (integral0
             (bra0
              (LC $c_1 (app $s $x)
                  $c_2 (app $t $x))))
            (LC $c_1 (integral0 (app $s $x))
                $c_2 (integral0 (app $t $x)))) "."))
   ((theorem #:n "1.5. 比较定理")
    "若对于每个区间" (li0 $a $b) "之中的" $x
    "有" (&< (app $s $x) (app $t $x)) ", 那么"
    (MB (&< (integral0 (app $s $x))
            (integral0 (app $t $x))) "."))
   
   (H2 "第2章 积分的一些应用")
   (H3 "第2.5节 三角函数")
   ((definition #:n ". 正弦和余弦的性质")
    (Ol (Item "定义域." "正弦和余弦在实轴上处处有定义.")
        (Item "特殊值." "我们有" (&= (&cos $0) (&sin (~ $pi $2)) $1)
              ", " (&= (&cos $pi) $-1) ".")
        (Item "差的余弦." "对于所有" $x "和" $y ", 我们有"
              (MB (&= (&cos (@- $y $x))
                      (&+ (&i* (&cos $y) (&cos $x))
                          (&i* (&sin $y) (&sin $x)))) "."))
        (Item "基本不等式." "对于" (&< $0 $x (~ $pi $2)) ", 我们有"
              (MB (&< $0 (&cos $x) (~ (&sin $x) $x) (~ $1 (&cos $x))) "."))))
   
   (H2 "第3章 连续函数")

   (H2 "第4章 微分演算")

   (H2 "第5章 积分和微分之间的关系")

   (H2 "第6章 对数函数, 指数函数和反三角函数")

   (H2 "第7章 函数的多项式近似")

   (H2 "第8章 微分方程导引")

   (H2 "第9章 复数")

   (H2 "第10章 序列, 无穷级数, 反常积分")

   (H2 "第11章 函数序列和级数")

   (H2 "第12章 向量代数")

   (H2 "第13章 向量代数于解析几何的应用")

   (H2 "第14章 向量值函数的演算")

   (H2 "第15章 线性空间")

   (H2 "第16章 线性变换和矩阵")
   
   (H1 "微积分第二卷")
   (H2 "第1章 线性空间")
   
   (H2 "第2章 线性变换和矩阵")
   
   (H2 "第3章 行列式")
   
   (H2 "第4章 特征值和特征向量")
   
   (H2 "第5章 作用于Euclid空间的算子的特征值")
   
   (H2 "第6章 线性微分方程")
   
   (H2 "第7章 微分方程组")
   
   (H2 "第8章 标量和向量域的微分演算")
   
   (H2 "第9章 微分演算的应用")
   
   (H2 "第10章 线积分")
   
   (H2 "第11章 多重积分")
   
   (H2 "第12章 面积分")
   
   (H2 "第13章 集合函数与初等概率论")
   
   (H2 "第14章 概率的演算")
   
   (H2 "第15章 数值分析导引")
   
   ))