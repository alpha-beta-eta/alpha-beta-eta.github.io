#lang racket
(provide godel.html)
(require SMathML)
(define (format-num section index)
  (and index
       (format "~a" index)))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
(define (Entry name class)
  (define (present %entry attr* . html*)
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define head (format-head name section index))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define id (%entry-id %entry))
    (define href (string-append "#" id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define num (format-num section index))
    (Cite `(a ((href ,href)) ,name ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry
             #:id id #:auto? auto? #:present present
             #:cite cite #:local? #f)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Remark "评注" "remark")
  (Theorem "定理" "theorem")
  (Warning "警告" "warning")
  (Example "例子" "example")
  (Proposition "命题" "proposition"))
(define godel.html
  (TnTmPrelude
   #:title "没有(太多)眼泪的Gödel"
   #:css "styles.css"
   (H1. "没有(太多)眼泪的Gödel")
   (P "这本书也是Peter Smith所写, 我想其风格也相当适合编程语言人阅读.")
   (H2. "关于Kurt Gödel的简要注记")
   (H2. "不完备性的大意")
   (P "Gödel的1931年的伟大论文的标题被翻译为"
      (I "'On formally undecidable propositions of "
         "Principia Mathematica and related systems I'")
      ". {译注: 当然这是英文翻译的标题, 中文或许可以翻译为"
      (Em "关于数学原理以及相关系统的形式不可判定命题(第一部分)")
      "}.")
   (P "这里的'I'指出这意图成为一个具有两个部分的论文的第一部分, "
      "而第二部分详细展开了在第一部分中只是简要提示的第二定理的证明细节. "
      "但是, 第二部分从没有被写下. "
      "我们将会在适当的时候明白为什么不写下来的原因.")
   (P "这个标题已经向我们提出了许多需要解释的东西. "
      "什么是一个'形式不可判定命题'? 什么是"
      (I "Principia Mathematica")
      "? 嗯, 你可能已经听说过由A. N. Whitehead和Bertrand Russell"
      "所写的三卷本书籍, 这是一个多世纪之前面世的了, "
      "除了逻辑史学家之外很少有人阅读: "
      "但是这本书的计划是什么? "
      "并且什么可以算作'相关系统'&mdash;"
      "难道是一个和" (I "Principia")
      "中的系统有着适切联系的系统? "
      "实际上, 我们直接就想问这里的'系统'"
      "所表达的意思是什么呢?")
   (H3. "有效可判定性, 有效可计算性")
   (P "我们先来看最后一个问题, 一个'系统'(在与之相近的意义下)"
      "是一个有效可公理化理论. {原注: 实际上Gödel"
      "最初心中所想的只是这个宽泛意义下的系统的一个中心子类; "
      "但是现在让我们不要把故事变得更复杂.} "
      "但是这是什么意思? 为了开始, 我们需要解释'有效'.")
   (P "以下是一些最初我们要与之打交道的定义:")
   ((Definition)
    "一个性质" $P " (定义于某个对象域" $D "之上) "
    "是有效可判定的当且仅当存在一个算法 "
    "(一个确定性计算的一有限指令集合) 可以用来在有限步骤内, "
    "对于任意的对象" (∈ $o $D) ", 定下是否" $o "具有性质" $P ".")
   (P "换言之, 一个性质是有效可判定的恰当存在一个逐步的"
      "机械化例程可以用来定下是否" $o "具有性质" $P
      ", 使得一个编程得当的确定性计算机器可以在原则上实现这个例程 "
      "(理想化了实际的时间限制, 诸如此类).")
   (P "以下是两个既初等又令人熟悉的来源于命题逻辑的例子. "
      "为 (being) 一个重言式的性质是有效可判定的 (通过真值表测试). "
      "为一个句子的主要联结词的性质也是一样 (通过括号计数).")
   (P "与之前的定义有关, 但是更加基本地, 我们说:")
   ((Definition)
    "一个函数" $f " (一个完全函数, 定义于某个对象域" $D
    "之上) 是有效可计算的当且仅当存在一个算法可以用来在有限步骤内, "
    "对于任意的对象" (∈ $o $D) ", 计算值" (app $f $o) ".")
   (P "又一次, 一个函数是有效可计算的, 如果一个"
      )
   ))