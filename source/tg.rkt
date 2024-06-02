#lang racket
(provide tg.html)
(require SMathML)
(define tg.html
  (TnTmPrelude
   #:title "乌龟几何"
   #:css "styles.css"
   (H1 "乌龟几何")
   (H2 "第1章 乌龟几何引论")
   
   (H2 "第2章 反馈, 成长和模式")
   (H2 "第3章 乌龟几何中的向量方法")
   (H2 "第4章 乌龟路径的拓扑")
   (H2 "第5章 逃出平面的乌龟")
   (H2 "第6章 探索立方体")
   (H2 "第7章 再看球面")
   (H2 "第8章 分段平面")
   (H2 "第9章 弯曲空间和广义相对论")
   ))