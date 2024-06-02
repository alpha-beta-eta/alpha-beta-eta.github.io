#lang racket
(provide linear_algebra_contents.html)
(require SMathML)
(define linear_algebra_contents.html
  (Prelude
   #:title "线性代数目录"
   #:css "styles.css"
   (H1 "线性代数目录")
   (columnize
    (H2 (A "前言" #:attr* '((href "hoffman_preface.html"))))
    (H2 (A "第1章 线性方程" #:attr* '((href "hoffman_ch1.html"))))
    (H2 (A "第2章 向量空间" #:attr* '((href "hoffman_ch2.html"))))
    (H2 (A "第3章 线性变换" #:attr* '((href "hoffman_ch3.html"))))
    (H2 (A "第4章 多项式" #:attr* '((href "hoffman_ch4.html"))))
    (H2 (A "第5章 行列式" #:attr* '((href "hoffman_ch5.html"))))
    (H2 (A "第6章 初等标准形式" #:attr* '((href "hoffman_ch6.html"))))
    (H2 (A "第7章 有理形式和Jordan形式"
           #:attr* '((href "hoffman_ch7.html"))))
    (H2 (A "第8章 内积空间" #:attr* '((href "hoffman_ch8.html"))))
    (H2 (A "第9章 内积空间上的算子"
           #:attr* '((href "hoffman_ch9.html"))))
    (H2 (A "第10章 双线性形式"
           #:attr* '((href "hoffman_ch10.html"))))
    (H2 (A "附录" #:attr* '((href "hoffman_appendix.html"))))
    )
   ))