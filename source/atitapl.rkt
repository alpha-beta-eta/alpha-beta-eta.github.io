#lang racket
(provide atitapl.html)
(require SMathML)
(define atitapl.html
  (TnTmPrelude
   #:title "类型和编程语言高级主题"
   #:css "styles.css"
   (H1. "类型和编程语言高级主题")
   (P "这本书可以视为Types and Programming Languages的续篇. "
      "虽然是由多人所写, 但是写作风格还是参考了TaPL的.")
   (H2. "亚结构类型系统")
   (H3. "结构性质")
   (H3. "一个线性类型系统")
   (H3. "扩展和变体")
   (H2. "依赖类型")
   (H2. "")
   (H2. "类型化汇编语言")
   (H2. "携带证明的代码")
   (H2. "逻辑关系和一个关于等价检查的案例研究")
   (H2. "")
   (H2. "ML风格模块系统的设计考量")
   (H2. "类型定义")
   (H2. "ML类型推导的本质")
   ))