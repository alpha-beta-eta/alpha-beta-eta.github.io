#lang racket
(provide modal_analysis.html)
(require SMathML)
(define $□ (Mo "&EmptySmallSquare;"))
(define $Mini-ML (Mi "Mini-ML"))
(define $Mini-ML^□ (^ $Mini-ML $□))
(define modal_analysis.html
  (TnTmPrelude
   #:title "分阶段计算的模态分析"
   #:css "styles.css"
   (H1. "分阶段计算的模态分析")
   (H2 "摘要")
   (P "我们表明了在函数式语言的上下文里一个基于直觉主义模态逻辑S4的类型系统"
      "为描述和分析计算阶段提供了一个富有表达力的框架. "
      "我们的主要技术性结果是一个从Nielson &amp; Nielson的两层次函数式语言到"
      "我们的语言" $Mini-ML^□ "的一个保守嵌入, "
      "由此证明了在这个片段上绑定时间正确性等价于模态正确性. "
      "除此之外, " $Mini-ML^□ "也可以表达直接求值和跨越多个阶段的代码分享, "
      "因而既支持运行时代码生成也支持部分求值.")
   (H2. "引论")
   (P "将计算划分为不同的阶段是推导算法时常用的一种非正式技术. "
      "例如, 与其直接将字符串与正则表达式进行匹配, "
      "我们可以先将正则表达式编译为有限自动机, "
      "然后再在给定字符串上执行该自动机. "
      "部分求值基于某些函数参数的及早可用性 (early availability), "
      "将计算划分为两个阶段. "
      "绑定时间分析静态地确定哪些计算可以在第一阶段执行, "
      "哪些计算需要留到第二阶段完成.")
   
   (H2. "模态Mini-ML: 显式表述")
   (H3. "句法")
   
   (H3. "定型规则")
   
   ))