#lang racket
(provide tropical.html)
(require SMathML)
(define tropical.html
  (TnTmPrelude
   #:title "热带几何引论"
   #:css "styles.css"
   (H1. "热带几何引论")
   (H2. "热带岛屿")
   (P "在热带代数中, 两数之和为其最小元, 两数之积为其之和. "
      "这种代数结构被人称为" (Em "热带半环")
      "或者min-plus代数. 将最小替换为最大, "
      "我们就得到了同构的max-plus代数. "
      "形容词" (Q "热带")
      
      )
   (H3. "算术")
   
   (H2. "构筑块")
   (H2. "热带簇")
   (H2. "热带雨林")
   (H2. "热带花园")
   
   ))