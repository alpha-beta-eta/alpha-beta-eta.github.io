#lang racket
(provide automated.html)
(require SMathML
         "automated_preface.rkt"
         "automated_ch1.rkt"
         "automated_ch2.rkt"
         "automated_ch3_part1.rkt"
         "automated_ch3_part2.rkt"
         "automated_ch4.rkt"
         "automated_ch5.rkt"
         "automated_ch6.rkt"
         "automated_ch7.rkt"
         "automated_appendix.rkt")
(define automated.html
  (Tn (keyword-apply
       Prelude
       '(#:css #:title)
       '("styles.css"
         "实用逻辑和自动推理手册")
       (cons
        (H1. "实用逻辑和自动推理手册")
        (append
         automated_preface
         automated_ch1
         automated_ch2
         automated_ch3_part1
         automated_ch3_part2
         automated_ch4
         automated_ch5
         automated_ch6
         automated_ch7
         automated_appendix)))))
