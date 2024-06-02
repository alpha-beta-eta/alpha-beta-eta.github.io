#lang racket
(provide elements_knopp.html)
(require SMathML)
(define elements_knopp.html
  (TmPrelude
   #:title "函数论基础"
   #:css "styles.css"
   (H1 "函数论基础")
   (P "此书是Knopp所写的简明扼要的复变函数引论.")
   (H2 "第1部分 复数及其几何表示")
   (H3 "第1章 基础")
   (H3 "第2章 复数系和高斯平面")
   (H3 "第3章 黎曼球面")
   (H2 "第2部分 线性函数和循环变换")
   (H2 "第3部分 集合和序列; 幂级数")
   (H2 "第4部分 解析函数和共形映射")
   (H2 "第5部分 初等函数")
   ))