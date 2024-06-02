#lang racket
(provide macro.html)
(require SMathML)
(define macro.html
  (TnTmPrelude
   #:title "卫生宏技术"
   #:css "styles.css"
   (H1 "卫生宏技术")
   (H2 "第1章 捕获问题")
   (P "宏的本质是在一段代码中将一个表达式替换为另一个表达式.")
   (P "正如人们在逻辑学或lambda演算中所熟知的, naive的替换是unsound的. "
      "例如, "
      )
   (H2 "第2章 卫生之前的Lisp宏")
   
   (H2 "第3章 卫生之前的Scheme宏")
   
   (H2 "第4章 Kohlbecker的算法")
   
   (H2 "第5章 国家和国际标准")
   
   (H2 "第6章 句法闭包的兴起与衰落")
   
   ))