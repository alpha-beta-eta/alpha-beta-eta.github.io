#lang racket
(provide smathml_comments.html)
(require SMathML)
(define smathml_comments.html
  (TmPrelude
   #:title "SMathML注记"
   #:css "styles.css"
   (H1 "SMathML注记")
   (P "这里主要是为了记录MathML和SMathML微妙的地方, "
      "以及当前的SMathML有哪些地方需要修改或补充. "
      "这里只是草稿而已, 或许会有些混乱, 而且也不一定正确.")
   
   ))