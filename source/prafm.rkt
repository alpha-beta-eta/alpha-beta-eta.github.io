#lang racket
(provide prafm.html)
(require SMathML)
(define prafm.html
  (TnTmPrelude
   #:title "数学的实际基础"
   #:css "styles.css"
   (H1. "数学的实际基础")
   (H2. "一阶推理")
   (H3. "替换")
   (H3. "指称和描述")
   (H3. "函数和关系")
   (H3. "直接推理")
   
   (H2. "类型和归纳")
   (H2. "偏序集和格")
   (H2. "笛卡尔闭范畴")
   (H2. "极限和余极限")
   (H2. "结构递归")
   (H2. "伴随")
   (H2. "带有依赖类型的代数")
   (H2. "量化子")
   ))