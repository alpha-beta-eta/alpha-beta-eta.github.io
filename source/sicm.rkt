#lang racket
(provide sicm.html)
(require SMathML "sicm_utils.rkt")
(define sicm.html
  (Tm
   (Prelude
    #:title "SICM笔记"
    #:css "styles.css"
    (H1 "SICM笔记")
    (H2 "前言")
    (P "函数式风格的Lagrange方程 (下标自" $0 "开始记):"
       (MB (&= (&- (&D (&compose (@ap $partial_2 $L) (@Gmap $q)))
                   (&compose (@ap $partial_1 $L) (@Gmap $q)))
               $0)))
    (H2 "第1章 Lagrange力学")
    (H3 "第1.1节 配置空间")
    (P "配置描述了系统的状态, 所有可能的配置构成了配置空间. "
       "配置空间的维数也被称为自由度. 实际上这是对于具有完整性约束的系统而言的, "
       "否则的话配置空间的维数可能大于自由度. "
       "路径是将时间映射至配置的函数.")
    (H3 "第1.2节 广义坐标")
    (P "用于刻画配置的参数被称为广义坐标. 之后, 我们总是用" $gamma
       "表示路径, " $chi "表示将配置转换为广义坐标的映射, 而"
       (&= $q (&compose $chi $gamma)) "表示坐标路径. 如果配置空间的维数为" $n ", 那么广义坐标就是"
       $n "元实数组. 我们用" (&cm $chi^i (&= $i (&cm $0 $..h (&- $n $1)))) "表示" $chi
       "的各个分量, 而" (&= $q^i (&compose $chi^i $gamma)) ". 我们用" (ap $D $q) "表示" $q
       "的导数, 也就是广义速度, 那么"
       (&= (app (@D $q) $t)
           (tu0 (app (@D $q^0) $t) $..h (app (@D (^ $q (&- $n $1))) $t))) ".")
    (H3 "第1.3节 稳定作用量原理")
    
    (H3 "第1.4节 计算作用量")
    (H3 "第1.5节 Euler-Lagrange方程")
    (H3 "第1.6节 如何寻找Lagrange量")
    
    (H2 "第2章 刚体")
    
    (H2 "第3章 Hamilton力学")
    (H3 "第3.1节 Hamilton方程")
    (H3 "第3.2节 Poisson括号")
    
    (H2 "第4章 相空间结构")
    (H2 "第5章 正则变换")
    
    )))