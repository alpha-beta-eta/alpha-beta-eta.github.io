#lang racket
(provide macro_notes.html)
(require SMathML)
(define macro_notes.html
  (TmPrelude
   #:title "关于宏的笔记"
   #:css "styles.css"
   (H1 "关于宏的笔记")
   (H2 (A "卫生宏技术" #:attr* '((href "macro.html"))))
   (H2 (A "句法闭包" #:attr* '((href "synclo.html"))))
   (H2 (A "从宏到DSL: Racket的演化" #:attr* '((href "macro_dsl.html"))))
   
   ))