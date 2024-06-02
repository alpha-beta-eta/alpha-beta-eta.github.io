#lang racket
(provide proof_theory.html)
(require SMathML)
(define proof_theory.html
  (TmPrelude
   #:title "证明论和逻辑复杂度"
   #:css "styles.css"
   (H1 "证明论和逻辑复杂度")
   (H2 "前言")
   (P "本书意在作为证明论的一本导引. 从Hilbert开始, "
      "其第二问题 (即著名的1900年Hilbert提出的23个数学问题) "
      "是数论的一致性, 我们描述了Hilbert纲领, "
      "以及其经G&ouml;del之手的覆灭. 接着, "
      "我们讨论了Gentzen的结果 (即" (Em "Hauptsatz")
      ", 这是第I部分的主要结果, 第I部分即第2, 3, 4章) "
      "以及其于" $omega "逻辑 (第5, 6, 7章) 和更加"
      (Q "高级") "的逻辑 (第8-12章) 的改进和泛化. "
      "我已经尽力涵盖了证明论的主要想法和技术. "
      "我们从G&ouml;del的结果作为我们的出发点 "
      "(其已在绝大多数优秀材料中得到证明, "
      "想必最好不要这里再次呈现了), "
      )
   
   (P "本书以英语写成, 出于全然相同的原因, "
      "若是数百年前编写此书, 则会用拉丁语, "
      "不过还是有一点不同之处: "
      "那时已无拉丁母语人士, 情况反倒更简单一些. "
      "因此, 我向那些不把英语仅仅视为"
      "一种灵活的科学语言 "
      "(a flexible scientific tongue) "
      "的人道歉.")
   ))