#lang racket
(provide linear_algebra_contents.html)
(require SMathML)
(define linear_algebra_contents.html
  (Prelude
   #:title "线性代数目录"
   #:css "styles.css"
   (H1 "线性代数目录")
   (columnize
    (TITLE "前言" "hoffman_preface.html")
    (TITLE "第1章 线性方程" "hoffman_ch1.html")
    (TITLE "第2章 向量空间" "hoffman_ch2.html")
    (TITLE "第3章 线性变换" "hoffman_ch3.html")
    (TITLE "第4章 多项式" "hoffman_ch4.html")
    (TITLE "第5章 行列式" "hoffman_ch5.html")
    (TITLE "第6章 初等标准形式" "hoffman_ch6.html")
    (TITLE "第7章 有理形式和Jordan形式" "hoffman_ch7.html")
    (TITLE "第8章 内积空间" "hoffman_ch8.html")
    (TITLE "第9章 内积空间上的算子" "hoffman_ch9.html")
    (TITLE "第10章 双线性形式" "hoffman_ch10.html")
    (TITLE "附录" "hoffman_appendix.html"))))