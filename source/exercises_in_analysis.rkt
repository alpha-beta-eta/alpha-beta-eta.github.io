#lang racket
(provide exercises_in_analysis.html)
(require SMathML)
(define tuple tu0)
(define $Hat (Mo "&Hat;"))
(define (Hat x) (^^ x $Hat))
(define (distance x y) (&abs (&- x y)))
(define $int (Mi "int"))
(define (&int X) (ap $int X))
(define (&closure X) (OverBar X))
(define seq:x_n
  (_ (setE $x_n) (&>= $n $1)))
(define seq:x_k_n
  (_ (setE (_ $x $k_n)) (&>= $n $1)))
(define exercises_in_analysis.html
  (TmPrelude
   #:title "分析学练习"
   #:css "styles.css"
   (H1 "分析学练习")
   (P "这本书看上去内容符合我的期望, 我想我或许可以通过这本书学会分析.")
   (P "这里不是对于本书的翻译, 只是一些笔记, 但是不排除我会翻译其中一些内容.")
   (H2 "第1章 度量空间")
   (H3 "第1.1节 介绍")
   (H4 "第1.1.1小节 基本定义和记号")
   (P "唉, 又是标号狂魔.")
   ((definition #:n "1.1")
    "度量空间的定义而已, 没必要写了.")
   ((remark #:n "1.2")
    "若是修改度量空间的定义, 允许互异的元素之间的距离为" $0
    ", 那么我们就得到了伪度量空间, 或者说半度量空间, 此时的距离函数被称为"
    "伪度量, 半度量, 或者ecart (这应该是法语).")
   ((example #:n "1.3")
    "举了不少不那么平凡的例子, 之后再抄吧.")
   ((proposition #:n "1.4")
    "如果" (tuple $X $d_X) "是一个度量空间, 并且"
    (MB (&= (appl (_ (Hat $d) $X) $x $y)
            (~ (appl $d_X $x $y)
               (&+ $1 (appl $d_X $x $y)))))
    "那么" (tuple $X (_ (Hat $d) $X)) "也是一个度量空间.")
   ((remark #:n "1.5")
    "注意到"
    (MB (&< (appl (_ (Hat $d) $X) $x $y) $1))
    "而且, 使用以上命题, 我们可以发现实序列的空间" $X
    "在装备了距离"
    (MB (&= (appl $d_X $x $y)
            (sum (&= $k $1) $inf
                 (&i* (~ $1 $2^k)
                      (~ (distance $x_k $y_k)
                         (&+ $1 (distance $x_k $y_k)))))))
    "之后就成为一个度量空间 (见例子1.3的b).")
   ((definition #:n "1.6")
    "唉, 又是一些基本概念的定义."
    )
   (H4 "第1.1.2小节 序列和完备度量空间")
   ((definition #:n "1.7")
    "令" (tuple $X $d_X) "是一个度量空间, "
    (&sube (_ (setE $x_n) (&>= $n $1)) $X)
    "是一个序列."
    (Ol #:attr* '((type "a"))
        (Li "我们称序列" (&sube (_ (setE $x_n) (&>= $n $1)) $X)
            "收敛至" (∈ $x $X) "当且仅当对于任意的" (&> $r $0)
            ", 我们可以找到一个整数"
            (&>= (&= $n_0 (app $n_0 $r)) $1) "使得"
            
            )
        )
    )
   (H4 "第1.1.3小节 度量空间的拓扑")
   (H4 "第1.1.4小节 Baire定理")
   (P "度量空间最重要的性质之一是完备性, 而许多分析学的基础结果"
      "都严重依赖于该性质. 完备性是藉由所谓的" (B "Baire纲定理")
      "而成为强大的工具的.")
   ((definition #:n "1.25")
    "令" (tuple $X $d_X) "是一个度量空间. 集合" (&sube $E $X)
    "被称为是" (B "无处稠密") "的, 如果"
    (&= (&int (&closure $E)) $empty)
    ". 集合" (&sube $E $X) "被称为是" (B "meager")
    "的, 或者" (B "第一纲") "的, 如果其可以被写成可数个"
    "无处稠密集合之并. 如果" (&sube $E $X)
    "不是第一纲集, 那么它就被称为是" (B "第二纲") "的.")
   ((theorem #:n "1.26")
    
    )
   (H3 "第1.2节 问题")
   ((exercise #:n "1.1")
    "设" (tuple $X $d_X) "是一个度量空间, 证明一个Cauchy序列在"
    $X "中收敛当且仅当其具有一个收敛的子序列.")
   ((proof)
    "给定Cauchy序列" (&sube seq:x_n $X)
    ". 鉴于收敛序列的每个子序列都收敛, 所以我们只需要证明当" seq:x_n
    "具有一个收敛的子序列时, 其在" $X "中收敛即可. 设这个收敛的子序列为"
    seq:x_k_n ", 其中" $k "应该理解为一个从正整数集到正整数集的严格单调映射, "
    "并且我们设其极限为" (∈ $x $X) ". 对于" (&> $epsilon $0)
    ", 存在正整数" $n_1 "使得对于每个" (&>= $m $n_1) ", 都有"
    (&< (appl $d_X (_ $x $k_m) $x) (&/ $epsilon $2))
    ". 另外, 根据Cauchy序列的定义, 对于相同的" $epsilon
    ", 存在正整数" $n_2 "使得对于每个" (&>= (&cm $l $m) $n_2)
    ", 都有" (&< (appl $d_X $x_l $x_m) (&/ $epsilon $2))
    ". 置" (&= $n_3 (&max $n_1 $n_2)) ". 因为" (&>= $n_3 $n_1) ", 所以"
    (&< (appl $d_X (_ $x (_ $k $n_3)) $x) (&/ $epsilon $2))
    ". 并且, 我们还知道" (&>= (_ $k $n_3) (_ $k $n_2) $n_2)
    ". 因此, 对于每个" (&>= $m $n_2) ", 我们有"
    (MB (deriv0 (appl $d_X $x_m $x)
                $<=
                (&+ (appl $d_X $x_m (_ $x (_ $k $n_3)))
                    (appl $d_X (_ $x (_ $k $n_3)) $x))
                $<
                (&+ (~ $epsilon $2) (~ $epsilon $2))
                $=
                $epsilon))
    "即" (&< (appl $d_X $x_m $x) $epsilon)
    ". 根据定义, 我们知道" seq:x_n "收敛并且极限为" $x
    ", 这就完成了证明.")
   (H2 "第2章 拓扑空间")
   (H3 "第2.1节 介绍")
   (H4 "第2.1.1小节 基本定义和记号")
   ((definition #:n "2.1")
    "一个" (B "拓扑空间") "是一个序对" (tuple $X $tau)
    ", 其中" $X "是一个集合, 而" $tau "是一个" $X
    "的子集的族, 其元素被称为" (B "开集")
    ", 它们需要满足以下三个要求:"
    (Ol #:attr* '((type "a"))
        (Li (∈ $empty $X $tau) ";")
        (Li $tau "在任意的并下封闭, 即"
            (MB "如果" (&sube (_ (setE $U_i) (∈ $i $I)) $tau)
                ", 那么" (∈ (Cup (∈ $i $I) $U_i) $tau) ";"))
        (Li $tau "在有限的交下封闭, 即"
            
            )
        )
    )
   (H4 "第2.1.2小节 拓扑基和子基")
   ((definition #:n "2.19")
    "令" (tuple $X $tau) "是一个拓扑空间."
    (Ol #:attr* '((type "a"))
        (Li "一个族" (&sube $B:script $tau)
            "是" $tau "的一个" (B "基")
            ", 如果每个" (∈ $U $tau)
            "都是" $B:script "中的某些元素之并.")
        (Li "一个族" (&sube $Y:script $tau)
            "是" $tau "的一个" (B "子基")
            ", 如果" $Y:script
            "中的集合的所有有限的交之集构成了"
            $tau "的一个基."))
    "[注记: 子基的一个等价定义是, " $Y:script
    "生成了" $tau ", 即" $tau "是包含"
    $Y:script "的最小的拓扑.]")
   ((proposition #:n "2.20")
    "如果" (tuple $X $tau) "是一个拓扑空间, 并且"
    (&sube $B:script $tau) ", 那么" $B:script
    "是一个基当且仅当对于每个" (∈ $x $X)
    "和每个" (∈ $U (app $N:script $x))
    ", 我们可以找到一个" (∈ $V $B:script)
    "使得" (∈ $x (&sube $V $U)) ".")
   ((proof)
    "如果" $B:script "是一个基, 那么对于"
    (∈ $x $X) "和" (∈ $U (app $N:script $x))
    ", 存在" (∈ $W $tau) "满足"
    (∈ $x $W) "且" (&sube $W $U)
    ". 既然" $W "是一个开集, 那么其可以表示为"
    $B:script "的一些元素之并. 因此, 我们可以断言存在"
    (∈ $V $B:script) "满足" (∈ $x $V)
    ". 反过来, 如果我们对于每个" (∈ $x $X)
    "和每个" (∈ $U (app $N:script $x))
    "都可以找到一个" (∈ $V $B:script)
    "使得" (∈ $x (&sube $V $U))
    ", 那么对于任意的" (∈ $W $tau)
    ", 对于" $W "的每个点" $x
    ", 我们都可以找到一个" (∈ (app $V $x) $B:script)
    "满足" (∈ $x (app $V $x)) "且"
    (&sube (app $V $x) $W)
    ", 既然" (∈ $W (app $N:script $x))
    ". 我们考虑" (Union (∈ $x $W) (app $V $x))
    ", 这个集合显然是" $W "的子集, 并且既然"
    (∈ $x (app $V $x)) ", 所以" $W
    "也是该集合的子集. 换言之, "
    (&= $W (Union (∈ $x $W) (app $V $x)))
    ". 因为" $W "是任意的开集, 所以这告诉我们"
    $B:script "是" $tau "的一个基.")
   ((proposition #:n "2.21")
    "如果" (tuple $X $tau) "是一个拓扑空间, 并且"
    $B:script "是" $tau "的一个基, 那么"
    $U "为开集 (即" (∈ $U $tau) ") 当且仅当对于每个"
    (∈ $x $U) ", 我们可以找到" (∈ $V $B:script)
    "使得" (∈ $x (&sube $V $U)) ".")
   ((proof)
    "前者推出后者是2.20的一个推论. 对于后者推出前者, "
    "基本上也可以按照2.20的证明方式推进, 即"
    (&= $U (Union (∈ $x $U) (app $V $x)))
    ", 其中" (app $V $x) "是一个满足"
    (∈ (app $V $x) $B:script) "且"
    (∈ $x (&sube (app $V $x) $U))
    "的集合. 鉴于" (&sube $B:script $tau)
    ", 故" $U "是开集无疑.")
   (P "注记: 取" (&= $B:script $tau)
      ", 那么命题2.21即" $U "是开集当且仅当"
      $U "的每个点都是内部点 (interior point).")
   ((definition #:n "2.22")
    "令" $A:script "是" $X "的一个子集族, 那么由"
    $A:script "生成的拓扑 (记作" (app $tau $A:script)
    ") 是"
    )
   (H3 "第2.2节 问题")
   
   (H2 "第3章 测度, 积分和鞅")
   
   (H2 "第4章 测度和拓扑")
   
   (H2 "第5章 泛函分析")
   
   ))