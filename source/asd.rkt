#lang racket
(provide asd.html)
(require SMathML)
(define asd.html
  (TmPrelude
   #:title "抽象Stone对偶笔记"
   #:css "styles.css"
   (H1 "抽象Stone对偶笔记")
   (P "先翻译Paul Taylor的主页吧, 似乎我也在翻译另一本Paul Taylor翻译了的书, "
      "即Proofs and Types."
      )
   ))