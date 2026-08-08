#lang racket
(provide lax.html)
(require SMathML)
(define $○ (Mo "○"))
(define lax.html
  (TnTmPrelude
   #:title "命题松弛逻辑"
   #:css "styles.css"
   (H1. "命题松弛逻辑")
   (P "我们研究一种颇为奇特的直觉主义模态逻辑, "
      "称为命题松弛逻辑 (Propositional Lax Logic, PLL), "
      "它在计算机硬件的形式验证方面很有应用前景. "
      "该逻辑源于这样一种尝试: 把"
      (Q "在行为约束意义下的正确性") --
      "硬件验证中的一个核心概念" --
      "表达为一个逻辑模态. 作为一种模态逻辑, "
      "它的特别之处在于只配备了单一的模态算子" $○
      ", 而该算子同时带有可能性与必然性两种意味. "
      "本文给出PLL的研究动机, 并给出若干技术性结果. "
      "我们考察了它的一些证明论性质, "
      "针对该逻辑的标准Gentzen式相继式系统给出了一个切消定理. "
      "随后, 我们为PLL定义了一类新的"
      "可谬 (fallible) 双框架Kripke模型. "
      
      )
   ))