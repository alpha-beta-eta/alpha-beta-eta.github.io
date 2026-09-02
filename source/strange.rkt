#lang racket
(provide strange.html)
(require SMathML)
(define strange.html
  (TnTmPrelude
   #:title "古怪的Lisp"
   #:css "styles.css"
   (H1. "古怪的Lisp")
   (P "这里我收录了一些历史上出现的古怪Lisp方言或者想法.")
   (H2. "Linear Lisp")
   
   ))