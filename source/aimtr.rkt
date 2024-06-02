#lang racket
(provide aimtr.html)
(require SMathML)
(define aimtr.html
  (TnTmPrelude
   #:title "AIM和AITR考古"
   #:css "styles.css"
   (H1. "AIM和AITR考古")
   (P "AIM即AI Memo, 即人工智能备忘录; AITR即AI Technical Report, "
      "即人工智能技术报告. 作为一个Lisper, 我总是接触到这些论文.")
   (H2. "AITR-474 (RABBIT: A Compiler for SCHEME)")
   (P "这是历史上第一个Scheme语言的编译器, 也是历史上第一个以CPS为IR的编译器. "
      "这个编译器的目标语言并非什么native assembly, 而是受限形式的MacLisp. "
      )
   (H2. "AITR-794 (Presentation Based User Interface)")
   
   (H2. "AIM-168 (PLANNER: A Language for Manipulating Models and Proving Theorems in a Robot)")

   (H2. "AIM-349 (SCHEME: An Interpreter for Extended Lambda Calculus)")
   (P "这是历史上关于Scheme编程语言的第一篇论文. "
      "这篇论文没有什么理论性质的内容, 几乎全是在编写程序. "
      )
   ))