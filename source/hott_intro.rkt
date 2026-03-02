#lang racket
(provide hott_intro.html)
(require SMathML)
(define (Miv str variant)
  (Mi str #:attr* `((mathvariant ,variant))))
(define (make-id str)
  (Miv str "sans-serif"))
(define-syntax-rule (define-id* (id str) ...)
  (begin
    (define id (make-id str))
    ...))
(define-id*
  ($Id "Id")
  ($refl "refl"))
(define Id
  (case-lambda
    ((A a b) (appl (_ $Id A) a b))
    ((a b) (appl $Id a b))))
(define $=_A (_ $= $A))
(define-infix*
  (&=_A $=_A))
(define hott_intro.html
  (TnTmPrelude
   #:title "同伦类型论引论"
   #:css "styles.css"
   (H1. "同伦类型论引论")
   (H2 "引论")
   (P "这是一本关于单价数学和同伦类型论的导引性质教科书. "
      "同伦类型论是一种利用了数学定义和构造的结构性质的数学基础. "
      "数学实践普遍将等价对象视为相同的, 例如, 将同构的群视作等同. "
      "在集合论中并不可能形式化这一通常惯例. 例如, "
      "集合论背景下存在多少不同的单元素集合就有多少不同的平凡群. "
      "另一方面, 类型论采用了数学基础的一种更为结构化的方式, "
      "其与单价公理兼容. 然而, 这要求我们重新思考两个对象相等的含义.")
   (H3 "同伦类型论的起源")
   (P "同伦类型论大约在十年前浮出水面, "
      "其沿着Awodey和Warren [3]对于Martin-Löf的依赖类型论的同伦解释 ("
      "而这也由Voevosky [25]独立发现), 以及Voevosky的单价公理 [26]继续前进. "
      "Martin-Löf的依赖类型论 [19]是数学的一种基础性语言, "
      "用在了如今许多计算机证明助手之中.")
   (P "在依赖类型论中存在着称为类型的原始对象和称为元素的原始对象. "
      "Martin-Löf的依赖类型论包含了诸多我们所熟悉的"
      "来源于传统数学的运算的类型形成规则, "
      "例如积, 和, 以及归纳类型, 比如说自然数的类型. "
      "其之所以被称为" (Em "依赖") "类型论, "
      "是因为类型和元素都可以由其他类型的元素参数化. "
      "{译注: 换言之, 可以依赖于类型的值.}")
   (P "Martin-Löf的依赖类型论的突出特征之一是"
      (Em "相等类型(identity type)") ". 相等类型"
      (MB (Id $A $a $b))
      "是依赖类型的一个例子, 因为其由两个元素" (&: (&cm $a $b) $A)
      "参数化了. 相等类型的元素被称为identification, "
      "而断言" $a "和" $b "是类型" $A "的相等元素的类型论方式是"
      "断言相等类型" (Id $A $a $b) "中存在一个元素. "
      "换言之, 为了在类型论中证明两个元素" $a "和" $b
      "在类型" $A "中相等, 我们不得不定义一个identification "
      (&: $p (Id $A $a $b)) ". 因此, 通常将相等类型"
      (Id $A $a $b) "记作" (&= $a $b)
      ", 或者为了使得作为背景的类型显式化, 可以写成"
      (&=_A $a $b) ".")
   
   (H2. "依赖类型论")
   (H2. "依赖函数类型")
   (H2. "自然数")
   (H2. "更多的归纳类型")
   (H2. "恒等类型")
   (H2. "宇宙")
   (H2. "藉由Curry-Howard解释的模算术")
   (H2. "初等数论的可判定性")
   (H2. "等价")
   (H2. "可缩类型和可缩映射")
   (H2. "恒等类型的基本定理")
   (H2. "命题, 集合, 以及高阶截断层次")
   (H2. "函数外延性")
   (H2. "命题性截断")
   
   ))