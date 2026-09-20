#lang racket
(provide hot.html)
(require SMathML)
(define lecture*
  '((1 "Sax: 一种命令式中间语言" "Sax: An Imperative Intermediate Language" "hot_01_sax.html")
    (2 "线性类型检查" "Linear Typechecking" "hot_02_typechecking.html")
    (3 "线性自然演绎" "Linear Natural Deduction" #f)
    (4 "编译" "Compilation" #f)
    (5 "求值" "Evaluation" #f)
    (6 "优化" "Optimizations" #f)
    (7 "负类型" "Negative Types" #f)
    (8 "闭包" "Closures" #f)
    (9 "闭包变换" "Closure Conversion" #f)
    (10 "伴随类型" "Adjoint Types" #f)
    (11 "法与序" "Law and Order" #f)
    (13 "有序类型检查" "Ordered Type Checking" #f)
    (14 "伴随Sax" "Adjoint Sax" #f)
    (15 "数据布局" "Data Layout" #f)
    (16 "调用约定" "Calling Conventions" #f)
    (17 "相继式演算" "Sequent Calculus" #f)
    (18 "伴随有序类型" "Adjoint Ordered Types" #f)
    (19 "垃圾回收" "Garbage Collection" #f)
    (20 "续延传递风格" "Continuation-Passing Style" #f)
    (21 "无堆函数式程序" "Heap-Free Functional Programs" #f)
    (22 "极化类型" "Polarized Types" #f)
    (23 "丰富效应演算" "The Enriched Effect Calculus" #f)))
(define (Lecture n title en link)
  (if link
      (Li (format "第~a讲 " n) (LINK title link) " (" (I en) ")")
      (Li (format "第~a讲 " n) title " (" (I en) ") " -- "未翻译")))
(define hot.html
  (Prelude
   #:title "HOT编译"
   #:css "styles.css"
   (H1 "HOT编译")
   (P "这是CMU 15-417/817: HOT Compilation (2025年春季) 课程讲义的中文翻译, "
      "讲义主要由Frank Pfenning撰写. 这门课以Sax这种基于半公理相继式演算的"
      "中间语言为核心, 逐步为愈来愈复杂的高阶有类型语言编写编译器. "
      "每一讲对应一个页面, 各讲共用同一份"
      (LINK "术语翻译对照" "hot_glossary.html") ".")
   (H2 "讲座")
   (apply Ul (map (lambda (l) (apply Lecture l)) lecture*))
   ))
