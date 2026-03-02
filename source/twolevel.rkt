#lang racket
(provide twolevel.html)
(require SMathML)
(define (make-identifier str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define-syntax-rule (define-identifier* (id str) ...)
  (begin
    (define id (make-identifier str))
    ...))
(define-identifier*
  ($list "list")
  ($Int "Int"))
(define $space:2ex (&space 2))
(define (&list a)
  (: a $space:2ex $list))
(define-@lized-op*
  (@-> &->))
(define twolevel.html
  (TnTmPrelude
   #:title "两层函数式语言"
   #:css "styles.css"
   (H1. "两层函数式语言")
   (H2. "引论")
   (P (Em "函数式编程风格") "与高阶函数的使用密切相关. "
      "具体而言, 其认为许多函数定义是相同的一般计算模型的实例, "
      "而这种模式可由高阶函数定义. "
      "模式的各种实例可由给高阶函数提供其一些参数得到.")
   
   (H2. "类型显式化")
   (P "Miranda和Standard ML都享有这样的性质, "
      "程序员不必描述程序中所有被定义的实体的类型. "
      "这是因为实现能够在程序良构的假定下推导出剩下的类型. "
      "函数式编程风格的有点在于高阶函数可以相当直接的方式定义.")
   (P "作为一个例子, Miranda和Standard ML将会推导第1章所考虑的"
      (Code "reduce") "函数的类型为"
      (MB (&-> (@-> $alpha $beta $beta)
               $beta (&list $alpha) $beta))
      "这里的" $alpha "和" $beta "是所谓的类型变量, "
      "其可以实例化为任意的类型. 第1章里的"
      (Code "sum") "的定义中所出现的" (Code "reduce")
      "具有类型"
      (MB (&-> (@-> $Int $Int $Int)
               $Int (&list $Int) $Int))
      
      )
   (H3. "类型化" $lambda "演算")
   (H3. "类型分析")
   (H2. "绑定时间显式化")
   (H3. $2 "层" $lambda "演算")
   (H3. "绑定时间分析")
   (H3. "改进绑定时间分析")
   (H2. "组合子显式化")
   (H2. "参数化语义")
   (H2. "代码生成")
   (H2. "抽象解释")
   (H2. "结论")
   ))