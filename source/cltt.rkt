#lang racket
(provide cltt.html)
(require SMathML)
(define cltt.html
  (TnTmPrelude
   #:title "范畴逻辑和类型论"
   #:css "styles.css"
   (H1. "范畴逻辑和类型论")
   (H2. "纤维范畴论导引")
   (H3. "纤维")
   (P "这真正的第一章从纤维范畴论的基本开始; 其为本书的剩余部分提供了基础. "
      "一个纤维, 或者说纤维范畴, 意在捕获范畴" $CC_I "的合集"
      (_ (@ $CC_I) (∈ $I $BB)) "的概念, 其于一个基范畴" $BB
      "上变动, 这推广了诸如集合族" (_ (@ $X_i) (∈ $i $I))
      "的概念, 其于一个基集或者说指标集" $I "上变动. 主要的范畴论例子是"
      )
   ))