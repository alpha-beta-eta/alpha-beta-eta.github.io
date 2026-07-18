#lang racket
(provide smathml_everything.html)
(require SMathML "utils.rkt")
(define smathml_everything.html
  (TmPrelude
   #:title "SMathML"
   #:css "styles.css"
   (H1 "SMathML")
   (columnize
    (TITLE "SMathML参考 (新)" "smathml0.html")
    (TITLE "SMathML Reference (New)" "smathml1.html")
    (TITLE "SMathML指南" "smathml_guide0.html")
    ;(TITLE "SMathML论文 (draft)" "notes/SMathML.pdf")
    ;(TITLE "SMathML参考" "smathml.html")
    ;(TITLE "SMathML Reference" "smathml_ref.html")
    (TITLE "证明树排版测试" "proof_tree_test.html")
    (TITLE "Napier筹 (排版测试)" "napier.html")
    (TITLE "SMathML注记" "smathml_comments.html"))))
