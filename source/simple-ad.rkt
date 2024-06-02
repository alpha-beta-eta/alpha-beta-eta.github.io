#lang racket
(provide simple-ad.html)
(require SMathML)
(define simple-ad.html
  (TnTmPrelude
   #:title "自动微分的简单本质"
   #:css "styles.css"
   (H1. "自动微分的简单本质")
   (H2. "引论")
   (H2. "什么是导数?")
   (H2. "微分的规则")
   (H3. "顺序复合")
   (H3. "并行复合")
   (H3. "线性函数")
   (H2. "将碎片拼在一起")
   (H3. "范畴")
   (H3. "幺半范畴")
   (H3. "笛卡尔范畴")
   (H3. "余笛卡尔范畴")
   ))