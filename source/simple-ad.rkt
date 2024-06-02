#lang racket
(provide simple-ad.html)
(require SMathML)
(define (_cm A . x*)
  (_ A (apply &cm x*)))
(define $:: (Mo "::"))
(define $partial (Mi "&part;"))
(define (&partial x)
  (ap $partial x))
(define-infix*
  (&:: $::))
(define-@lized-op*
  (@d* &d*))
(define (Arrow f A B)
  (&:: f (&-> A B)))
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign "left center right")
                (width "100%"))
              (Mtr (Mtd (Mphantom label))
                   `(mtd ((displaystyle "true")) . ,exp*)
                   (Mtd label)))))
(define simple-ad.html
  (TnTmPrelude
   #:title "自动微分的简单本质"
   #:css "styles.css"
   (H1. "自动微分的简单本质")
   (H2. "引论")
   (H2. "什么是导数?")
   (P "既然自动微分 (AD) 和计算导数有关, "
      "让我们以考虑什么是导数开始. "
      "如果你所接受的入门性微积分课程和我差不多的话, "
      "那么你会学到一个函数" (Arrow $f $RR $RR)
      "在一个点" $x "处 (要求其在" $f
      "的定义域之中) 的导数" (app $f^ $x)
      "是一个" (Em "数字") ", 定义如下:"
      (MBL "(1)"
           (&= (app $f^ $x)
               (lim $epsilon $0
                    (~ (&- (app $f (&+ $x $epsilon))
                           (app $f $x))
                       $epsilon))))
      "也就是说, " (app $f^ $x) "告诉了我们" $f
      "在" $x "处对于输入变化的缩放有多快.")
   (P "这个定义对于类型" (&-> $RR $RR)
      "之外的函数有多适用呢? 复数情形 ("
      (&-> $CC $CC) ") 表现良好, 其中除法也有定义. "
      "扩展至" (&-> $RR $RR^n) "的情形也能成立, "
      "如果我们以通常的方式解释一个(" $RR^n
      "中的)向量除以一个标量. 然而, 如果我们扩展至"
      (&-> $RR^m $RR^n) "的情形, 或者甚至只是"
      (&-> $RR^m $RR) ", 这个定义就不再适用了, "
      "因为其依赖于除以一个向量" (&:: $epsilon $RR^m) ".")
   (P "这种非标量定义域上的微分的困难通常以相对于"
      $RR^m "的" $m "个标量分量的"
      (Q "偏导数") "的概念解决, 经常记作"
      (&/ (&partial $f) (&partial $x_j))
      ", 其中" (∈ $j (setE $1 $..h $m))
      ". 当" $RR^n "也是一个非标量时, 即"
      (&> $n $1) ", 那么我们就有了一个矩阵"
      $J:bold " (Jacobi矩阵), 其中"
      (&= (_cm $J:bold $i $j)
          (&/ (&partial $f_i) (&partial $x_j)))
      "而每个" $f_i "是函数" $f
      "的第" $i "投影, 其由取" $f
      "的结果的第" $i "个分量得到. "
      "{译注: 然而, 即便" (&= $n $1)
      ", 我们得到的也是一个矩阵, 只不过是"
      (&c* $1 $m) "的矩阵而已.}")
   (P "到目前为止, 我们已经看到了一个函数的导数可以是一个数字 ("
      (&-> $RR $RR) "), 一个向量 (" (&-> $RR $RR^n)
      "), 一个矩阵(" (&-> $RR^m $RR^n)
      "). 而且, 每种情形都有与之相伴的链式规则, "
      "其说明了该如何对于函数的复合进行微分. "
      "标量链式规则牵涉将两个标量导数相乘, "
      "而向量链式规则牵涉将两个矩阵"
      $A:bold "和" $B:bold " (Jacobi矩阵) "
      (Q "相乘") ", 其定义如下:"
      (MB (&= (_cm (@d* $A:bold $B:bold) $i $j)
              (sum (&= $k $1) $m
                   (&d* (_cm $A:bold $i $k)
                        (_cm $B:bold $k $j)))))
      "既然我们可以将标量视为向量的特殊情形, "
      "那么标量乘法也可以是为矩阵乘法的特殊情形, "
      "或许我们已经抵达了所需的一般性. "
      "然而, 当我们将注意力转向高阶导数的时候, "
      "即导数的导数, 情况就变得复杂起来了, "
      "我们需要更高维度的表示, "
      "以及相应的更加复杂的链式规则.")
   (P "幸运的是, "
      )
   (H2. "微分的规则")
   (H3. "顺序复合")
   (H3. "并行复合")
   (H3. "线性函数")
   (H2. "将碎片拼在一起")
   (H3. "范畴")
   (H3. "幺半范畴")
   (H3. "笛卡尔范畴")
   (H3. "余笛卡尔范畴")
   ))