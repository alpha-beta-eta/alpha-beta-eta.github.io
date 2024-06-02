#lang racket
(provide set_theory.html)
(require SMathML)
(define $aleph (Mi "&aleph;"))
(define $aleph_0 (_ $aleph $0))
(define $aleph_1 (_ $aleph $1))
(define $aleph_omega
  (_ $aleph $omega))
(define set_theory.html
  (TnTmPrelude
   #:title "集合论"
   #:css "styles.css"
   (H1. "集合论")
   (P "本书是久负盛名的Kunen的集合论教材的修订版本. "
      "一个有趣的事实是, Kunen其实是Church的学术后代, "
      "而且我也发现Kunen其实也在某种证明助手中写过不少程序.")
   (H2. "引论")
   (H3. "为何阅读本书?")
   (P "常规数学是基于" (I "ZFC")
      "的, 即包含选择公理的Zermelo-Fraenkel公理. 在"
      (I "ZFC") "内部工作, 我们可以建立:"
      (Ol (Li "序数" (&cm $0 $1 $..h $omega $..h)
              "和无限基数"
              (&cm $aleph_0 $aleph_1 $..h
                   $aleph_omega $..h)
              "的抽象性质.")
          (Li "在分析, 拓扑, 代数等领域的基本材料里可以找到的一切数学."))
      
      )
   ))