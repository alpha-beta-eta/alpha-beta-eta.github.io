#lang racket
(provide msp.html)
(require SMathML)
(define msp.html
  (TnTmPrelude
   #:title "多阶段编程的温和介绍"
   #:css "styles.css"
   (H1. "多阶段编程的温和介绍")
   (H2. "引论")
   (H3. "构建程序生成器的问题")
   (H3. "三种基本的MSP构造")
   (P (B "括号")
      " (记作" (Code ".&lt;...>.")
      ") 可以被插入到任意表达式的周围来推迟其执行. "
      
      )
   (H3. "基本的等价概念")
   ))