#lang racket
(provide mop.html)
(require SMathML)
(define mop.html
  (TnTmPrelude
   #:title "元对象协议艺术"
   #:css "styles.css"
   (H1 "元对象协议艺术")
   (H2 "第I部分 元对象协议的设计和实现")
   (H3 "第1章 CLOS是如何实现的")
   (P "我们将分阶段呈现元对象协议, "
      )
   (H3 "第2章 自省与分析")
   (H3 "第3章 扩展语言")
   (H3 "第4章 协议设计")
   
   (H2 "第II部分 CLOS的一个元对象协议")
   (H3 "第5章 概念")
   (H3 "第6章 Generic Functions and Methods")
   
   ))