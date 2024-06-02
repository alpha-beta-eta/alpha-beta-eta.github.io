#lang racket
(provide smathml_everything.html)
(require SMathML)
(define smathml_everything.html
  (TmPrelude
   #:title "SMathML"
   #:css "styles.css"
   (H1 "SMathML")
   (columnize
    (H2 (A "SMathML论文 (draft)" #:attr* '((href "notes/SMathML.pdf"))))
    (H2 (A "SMathML参考" #:attr* '((href "smathml.html"))))
    (H2 (A "SMathML Reference" #:attr* '((href "smathml_ref.html"))))
    (H2 (A "SMathML注记" #:attr* '((href "smathml_comments.html"))))
    
    )
   ))