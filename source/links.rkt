#lang racket
(provide links.html)
(require SMathML)
(define links.html
  (TnTmPrelude
   #:title "友情链接"
   #:css "styles.css"
   (H1 "友情链接")
   (columnize
    (H2 (A "https://blog.wxiwnd.net/"
           #:attr* '((href "https://blog.wxiwnd.net/"))))
    (H2 (A "https://kokic.github.io/"
           #:attr* '((href "https://kokic.github.io/"))))
    (H2 (A "https://alias.qliphoth.tech/"
           #:attr* '((href "https://alias.qliphoth.tech/"))))
    (H2 (A "https://sora.ink/"
           #:attr* '((href "https://sora.ink/"))))
    
    )
   ))