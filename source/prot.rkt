#lang racket
(provide prot.html)
(require SMathML)
(define (Cite0 #:attr* [attr* '()] . id*)
  `(cite ,attr* . ,(fenced (map Ref id*))))
(define (default-entry:bib-present %entry:bib attr* . xml*)
  (define index (%entry-index %entry:bib))
  (define numbering (format "[~s] " index))
  (define id (%entry-id %entry:bib))
  (keyword-apply
   Div '(#:attr*)
   (list (attr*-set attr* 'class "bibliography" 'id id))
   numbering xml*))
(define (default-entry:bib-cite %entry:bib)
  (define id (%entry-id %entry:bib))
  (define index (%entry-index %entry:bib))
  (define numbering (format "~s" index))
  (define href (string-append "#" id))
  `(a ((href ,href)) ,numbering))
(define (build-%entry:bib id)
  (build-%entry #:local? #f #:class "bibliography" #:id id
                #:present default-entry:bib-present
                #:cite default-entry:bib-cite))
(define (Bib #:id [id #f] #:attr* [attr* '()] . xml*)
  `(,(build-%entry:bib id) ,attr* . ,xml*))
(define prot.html
  (TnTmPrelude
   #:title "证明与类型"
   #:css "styles.css"
   (H1 "证明与类型")
   (H2 "前言")
   (P "这本小书来源于1986至1987年秋季学期于Universit&eacute; Paris VII"
      "开设的一门关于类型" $lambda "演算的短期研究生课程. 它并不意图是"
      "百科全书式的, 例如Church-Rosser定理就没有证明, 而且主题的选取是"
      "相当随意的.")
   (P "关于逻辑的基本常识是必要的, 然而我们也并不会陷入乏味的细节之中. "
      
      )
   (H2 "第1章 涵义, 指称和语义")
   (P "理论计算尚非科学. 许多基本概念亟待澄清, "
      "并且当前该领域的研究遵循一种&quot;婚礼蛋糕&quot;"
      "范式: 例如, 语言设计让人想到Ptolemy天文学&mdash;&mdash;"
      "不断需要更加深入的修正. 然而, 也存在一些有限的主题, "
      "例如复杂度理论和指称语义学, 它们相当远离这种批判.")
   (P "在这样的情况下, 方法论式的评论极其重要, 因为我们不得不"
      "将方法论视为" (Em "战略") "而将具体的结果视为具有"
      (Em "战术") "性质.")
   (P "我们尤其感兴趣的东西可在1900年代的逻辑漩涡的源头找到, "
      "由Frege, Löwenheim, Gödel等名字刻画. 不熟悉逻辑学史"
      "的读者应该参考" (Cite0 "vanHeijenoort") ".")
   (H3 "第1.1节 逻辑中的涵义和指称")
   (P "让我们从一个例子开始. 存在一个乘法的标准过程, 它由输入"
      (Mn "27") "和" (Mn "37") "产生结果" (Mn "999")
      ". 对于这个事实我们可以言称什么?")
   (P "最初的尝试是言称我们拥有了一个" (Em "等式")
      (MB (&= (&c* (Mn "27") (Mn "37")) (Mn "999")))
      "这个等式在数学主流中以言称两边指称相同的整数且" $c*
      "是Cantor的图的意义下的一个" (Em "函数")
      "而获得了含义. (译注: 这里&quot;整数&quot;的原文是"
      "&quot;integer&quot;, 又有原注, 全文的" (Em "integer")
      "将表示" (Em "natural number") ": "
      (&cm $0 $1 $2 $..h) ")")
   (P "这是指称性的方面, 无疑是正确的, 然而它忽略了基本的点.")
   (P "存在一个有限的" (Em "计算") "过程表明这两个指称是相等的. 言称"
      (&c* (Mn "27") (Mn "37")) "等于" (Mn "999")
      "是一种滥用 (这并非什么廉价的哲学&mdash;&mdash;而是一个具体的问题), "
      "因为如果我们所拥有的这两个东西真是" (Em "相同") "的, 那么我们就不会"
      "感到陈述它们的相等性的需要了. 具体地说, 我们在问一个" (Em "问题")
      ", " (&c* 27 37) ", 然后得到了一个答案, " 999 ". 这两个表达式"
      "具有不同的" (Em "涵义") ", 而我们必须" (Em "做") "些什么 (编制"
      "证明或者进行计算, 或是至少查询百科全书) 来表明这两个" (Em "涵义")
      "具有相同的" (Em "指称") ".")
   (P "关于" $c* ", 将其称为一个(作为图的)函数是不正确的, 因为加载了乘法程序"
      "的机器无法容纳下一个无限的图. (译注: 这句话是说" (Em "实无限")
      "是不可能容纳于一个经典计算机器之中的, 当然" (Em "潜无限")
      "的确是可以的.) 因此, 我们不得不总结道, 我们面对的是与这个涵义之问"
      "相关的一种" (Em "有限") "的动力学.")
   (P "尽管指称在很早的阶段就被建模, 涵义则被推向了" (Em "主观主义")
      ", 导致当前的数学对于涵义的处理或多或少沦为了" (Em "句法")
      "操作. 这在我们所要讨论的主题的本质之下并非" (Em "先验")
      ", 而我们可以期待在接下来的几十年里找到一种对于计算的处理, 它结合了"
      "指称语义学 (数学的清晰性) 和句法 (有限的动力学) 的优点. "
      )
   (H2 "第2章 自然演绎")
   
   (H2 "第3章 Curry-Howard同构")
   
   (H2 "参考文献")
   (Bib #:id "vanHeijenoort"
        "J. van Heijenoort, From Frege to G&ouml;del, "
        "a source book in mathematical logic, 1879–1931, "
        "Harvard University Press (1967)")
   
   ))