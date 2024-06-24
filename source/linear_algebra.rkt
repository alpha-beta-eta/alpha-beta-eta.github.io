#lang racket
(provide linear_algebra.html
         hoffman_preface.html
         hoffman_ch1.html
         hoffman_ch2.html
         hoffman_ch3.html
         hoffman_ch4.html
         hoffman_ch5.html
         hoffman_ch6.html
         hoffman_ch7.html
         hoffman_ch8.html
         hoffman_ch9.html
         hoffman_ch10.html
         hoffman_appendix.html)
(require SMathML
         "hoffman_preface.rkt"
         "hoffman_ch1.rkt"
         "hoffman_ch2.rkt"
         "hoffman_ch3.rkt"
         "hoffman_ch4.rkt"
         "hoffman_ch5.rkt"
         "hoffman_ch6.rkt"
         "hoffman_ch6.rkt"
         "hoffman_ch7.rkt"
         "hoffman_ch8.rkt"
         "hoffman_ch9.rkt"
         "hoffman_ch10.rkt"
         "hoffman_appendix.rkt")
(define linear_algebra.html
  (Prelude
   #:title "线性代数"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_preface
   div:hoffman_ch1
   div:hoffman_ch2
   div:hoffman_ch3
   div:hoffman_ch4
   div:hoffman_ch5
   div:hoffman_ch6
   div:hoffman_ch7
   div:hoffman_ch8
   div:hoffman_ch9
   div:hoffman_ch10
   div:hoffman_appendix))
(define hoffman_preface.html
  (Prelude
   #:title "线性代数 前言"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_preface))
(define hoffman_ch1.html
  (Prelude
   #:title "线性代数 第1章 线性方程"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch1))
(define hoffman_ch2.html
  (Prelude
   #:title "线性代数 第2章 向量空间"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch2))
(define hoffman_ch3.html
  (Prelude
   #:title "线性代数 第3章 线性变换"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch3))
(define hoffman_ch4.html
  (Prelude
   #:title "线性代数 第4章 多项式"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch4))
(define hoffman_ch5.html
  (Prelude
   #:title "线性代数 第5章 行列式"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch5))
(define hoffman_ch6.html
  (Prelude
   #:title "线性代数 第6章 初等标准形式"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch6))
(define hoffman_ch7.html
  (Prelude
   #:title "线性代数 第7章 有理形式和Jordan形式"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch7))
(define hoffman_ch8.html
  (Prelude
   #:title "线性代数 第8章 内积空间"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch8))
(define hoffman_ch9.html
  (Prelude
   #:title "线性代数 第9章 内积空间上的算子"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch9))
(define hoffman_ch10.html
  (Prelude
   #:title "线性代数 第10章 双线性形式"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_ch10))
(define hoffman_appendix.html
  (Prelude
   #:title "线性代数 附录"
   #:css "styles.css"
   (H1 "线性代数")
   div:hoffman_appendix))