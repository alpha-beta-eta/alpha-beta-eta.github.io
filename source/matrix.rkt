#lang racket
(provide matrix.html)
(require SMathML)
(define $+= (Mo "+="))
(define-infix*
  (&+= $+=))
(define matrix.html
  (TnTmPrelude
   #:title "矩阵乘法"
   #:css "styles.css"
   (H1. "矩阵乘法")
   (P "这里是对于Oleg Kiselyov关于矩阵乘法的博客的笔记.")
   (H2. "基本想法")
   (P "矩阵乘法通常被形式化地定义为"
      (MB (&= (mref $C $i $j)
              (sum $k (&i* (mref $A $i $k)
                           (mref $B $k $j)))))
      "其中的" $i "遍历矩阵" $A "的行的索引, "
      $j "遍历矩阵" $B "的列的索引, 而"
      $k "遍历矩阵" $A "的列或者矩阵" $B
      "的行的索引, 这两者应该是相同的. "
      "如果加上这些附注, 我大概会写成"
      (MB (&cm (&= (mref $C $i $j)
                   (sum $k (&i* (mref $A $i $k)
                                (mref $B $k $j))))
               (∈ $i $I)
               (∈ $j $J)
               (∈ $k $K)))
      "不过, 从命令式的角度来看, "
      "如果我们引入形式如下的语句 (statement):"
      (MB (&+= (mref $C $i $j)
               (&i* (mref $A $i $k)
                    (mref $B $k $j))))
      "那么矩阵乘法可以写成"
      (MB (&cm (&+= (mref $C $i $j)
                    (&i* (mref $A $i $k)
                         (mref $B $k $j)))
               (∈ $i $I)
               (∈ $j $J)
               (∈ $k $K)))
      "我们需要不重复不遗漏地遍历这些索引集, "
      "但是这种命令式描述的优雅之处在于"
      "我们可以任意地排列语句的执行顺序.")
   
   ))