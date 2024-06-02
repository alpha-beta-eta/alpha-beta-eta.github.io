#lang racket
(provide extensible_spec.html)
(require SMathML)
(define $o* (Mo "&otimes;"))
(define $o+ (Mo "&oplus;"))
(define $d*_bottom (_ $d* $bottom))
(define $--> (Mo "&xrarr;"))
(define-infix*
  (&o* $o*)
  (&o+ $o+)
  (&--> $-->))
(define extensible_spec.html
  (TnTmPrelude
   #:title "可扩展指称性语言描述"
   #:css "styles.css"
   (H1. "可扩展指称性语言描述")
   (H2. "复杂语言的指称性描述")
   
   (H2. "可扩展的操作语义")

   (H2. "可扩展的指称性描述")
   (H3. "语义框架: 扩展了的直接语义")
   (H3. "纯Scheme")
   (H3. "状态Scheme")
   (H3. "控制Scheme")
   (H3. "稳定指称")
   (H2. "将解释器复合")
   (H2. "相关工作")
   (H2. "结论")
   (H2 "附录A. domain描述的记号")
   (P "本篇论文所呈现的语义定义依赖于domain构造子"
      $o* " (smash product), " $o+
      " (coalesced sum), " $d*_bottom
      " (lifting), " $--> " (连续函数), "
      
      )
   ))