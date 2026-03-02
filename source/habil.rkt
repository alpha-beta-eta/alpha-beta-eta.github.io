#lang racket
(provide habil.html)
(require SMathML)
(define habil.html
  (TnTmPrelude
   #:title "通过求值进行规范化: 依赖类型和非直谓性"
   #:css "styles.css"
   (H1. "通过求值进行规范化: 依赖类型和非直谓性")
   (H2 "摘要")
   (H2. "引论")
   (H2. "简单类型: 从求值到规范化")
   (H3. "求值")
   (P (Em "求值") "是运行一个程序并最终获得一个结果的过程, "
      "这个结果也就是" (Em "值")
      ". 这一般牵涉将程序转换为某种机器语言的编译以及"
      "在机器上对于机器代码进行执行. "
      
      )
   (H2. "无类型NbE和类型赋予")
   (H2. "依赖类型")
   (H2. "非直谓性")
   
   ))