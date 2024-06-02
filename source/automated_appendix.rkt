#lang racket
(provide automated_appendix)
(require (except-in
          SMathML
          H3. H4. $!= &~ Lemma Corollary Definition
          Remark Theorem Example Proposition Warning)
         "automated_utils.rkt")
(define automated_appendix
  (Tm*
   (H2 "附录1: 数学背景")
   (H2 "附录2: OCaml轻松入门")
   (H2 "附录3: 公式的句法分析和打印")
   ))
