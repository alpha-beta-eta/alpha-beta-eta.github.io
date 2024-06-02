#lang racket
(provide macro.html)
(require SMathML)
(define-syntax SimpleTable
  (syntax-rules ()
    ((_ (x ...) ...)
     (Table
      (Tr (Td x) ...)
      ...))))
(define $. (Mo "."))
(define (∀ x P)
  (: $forall x $. P))
(define $forall:id (Mi "&forall;"))
(define ∀E (: $forall:id $E))
(define (subst X Y x)
  (: X (bra0 (&/ Y x))))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define macro.html
  (TnTmPrelude
   #:title "卫生宏技术"
   #:css "styles.css"
   (H1. "卫生宏技术")
   (H2 "翻译术语对照")
   (set-attr*
    (SimpleTable
     ("英文" "中文")
     ("macro" "宏")
     ("naive" "朴素")
     ("sound" "可靠")
     ("unsound" "不可靠")
     ("elimination rule" "消去规则")
     ("hygiene" "卫生")
     ("hygienic macro" "卫生宏"))
    'align "center")
   (H2. "捕获问题")
   (P "宏的本质是在一段代码里将一个表达式替换为另一个表达式.")
   (P "正如逻辑学和lambda演算中为人所熟知的, 朴素替换是不可靠的. "
      "例如, 考虑一阶逻辑的全称消去规则. 这条规则经常被陈述为"
      (MB (&rull
           ∀E
           (∀ $x $phi)
           (subst $phi $t $x)))
      "其是说"
      )
   (H2. "卫生之前的Lisp宏")
   (H2. "卫生之前的Scheme宏")
   (P "Scheme编程语言由Guy L Steele Jr和Gerald J Sussman于1975年设计和实现. "
      "鉴于Scheme起初被构想为Carl Hewitt的Actor模型的一个顺序实现 "
      "[Steele and Gabriel 1993a; Clinger 2008], "
      "其与(当时)大多数其他的Lisp方言在以下方面有所不同:"
      (Ul (Li "正确尾递归 (于是过程调用不会施行没有必要的压栈操作)")
          (Li "第一级延续 (所有那时已知的顺序控制结构均可由此合成)")
          (Li "词法作用域 (如lambda演算, Algol, 以及其他许多块结构语言)")
          (Li "第一级过程 (如lambda演算)")
          (Li "一致的求值 (如lambda演算)")
          (Li "统一的环境 (如lambda演算)"))
      "根据上述的最后四点, Scheme在lambda演算的意义下是一个"
      (Em "高阶语言") ": "
      )
   (H2. "Kohlbecker的算法")
   (P "现在我们考虑" (Em "Kohlbecker算法")
      ", 其似乎是第一个全然可靠的技术, "
      "(1) 赋予了程序员以避免意外捕获的力量, "
      "(2) 被实际实现.")
   
   (H3. "Kohlbecker的术语")
   
   (H3. "目标和设计决策")
   
   (H3. "其是如何工作的")
   
   (H2. "国家和国际标准")
   (H2. "句法闭包的兴起与衰落")
   (H2. "行之有效的宏")
   (H2. "强卫生性被发现压根不那么强")
   (H2. (Code "syntax-case"))
   (H2. "从R4RS到R5RS")
   (H2. "从R5RS到R6RS")
   (H2. "从R6RS到R7RS")
   (H2. "绑定作为作用域集合")
   
   ))