#lang racket
(provide sml-history.html)
(require SMathML)
(define sml-history.html
  (TnTmPrelude
   #:title "Standard ML的历史"
   #:css "styles.css"
   (H1 "Standard ML的历史")
   (H2 "第1章 引入: Standard ML何以重要?")
   (H2 "第2章 背景")
   (H3 "第2.1节 LCF/ML&mdash;&mdash;嵌入LCF定理证明器的原始ML")
   (P "我们从看看一些影响了LCF/ML设计的语言开始.")
   (P "Lisp是Stanford LCF项目所使用的语言, 该项目是Milner来"
      "Edinburgh之前和Richard Weyhrauch以及Malcolm Newey一道"
      "建立的. Lisp对于所有ML设计的参与者而言都是很熟悉的, 在"
      "当时它是符号计算事实上的标准语言. Lisp是一个函数式语言, "
      "其可以将函数视为数据操作, 尽管其作为函数式语言的行为"
      "因动态绑定的使用而存在缺陷. "
      )
   (H4 "第2.1.1小节 控制结构")
   (P "类似于ISWIM, LCF/ML的核心是一个按值调用的应用性语言, 其"
      "基本表达式通过函数应用进行复合. "
      )
   (H3 "第2.2节 HOPE")
   (H3 "第2.3节 Cardelli ML")
   (H2 "第3章 Standard ML历史的概览")
   (H2 "第4章 SML的类型系统")
   (H2 "第5章 模块")
   (H2 "第6章 Standard ML的定义")
   (H2 "第7章 类型论和Standard ML的新定义")
   (H2 "第8章 SML基础库")
   (H2 "第9章 总结")
   ))