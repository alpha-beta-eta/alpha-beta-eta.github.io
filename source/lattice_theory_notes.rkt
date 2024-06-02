#lang racket
(provide lattice_theory_notes.html)
(require SMathML)
(define lattice_theory_notes.html
  (TmPrelude
   #:title "格论笔记"
   #:css "styles.css"
   (H1 "格论笔记")
   (H2 (A "格与序导论" #:attr* '((href "order.html"))))
   (H2 (A "格论 (Birkhoff)" #:attr* '((href "lattices.html"))))
   (H2 (A "瞎写的格论笔记" #:attr* '((href "lattice_notes.html"))))
   
   ))