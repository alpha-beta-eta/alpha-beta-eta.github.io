#lang racket
(provide proof_theory_and_algebra_in_logic.html)
(require SMathML)
(define $impl $->)
(define-infix*
  (&conj $conj)
  (&disj $disj)
  (&impl $impl))
(define &impl*
  (case-lambda
    ((x y) (&impl x y))
    ((x y . z*) (&impl x (@ (apply &impl* y z*))))))
(define-@lized-op*
  (@conj &conj)
  (@disj &disj)
  (@impl &impl)
  (@impl* &impl*)
  (@neg &neg))
(define proof_theory_and_algebra_in_logic.html
  (TnTmPrelude
   #:title "证明论和逻辑代数"
   #:css "styles.css"
   (H1. "证明论和逻辑代数")
   (H2. "相继式系统")
   (P "在第1.1节给出预备评注和对于本书范围的简要解释之后, "
      "我们将分别为古典逻辑和直觉主义逻辑引入两个相继式系统"
      (B "LK") "和" (B "LJ")
      ". 这两个相继式系统对于第I部分的证明论研究都是基础性的, "
      "其最初由G. Gentzen在他的论文 (Gentzen 1935) 里引入. "
      "基本的证明和可证明性的概念首先被引入. "
      "然后对于" (B "LK") "的完备性和切削的初等证明被给出, "
      "通过使用古典逻辑的一个可逆相继式系统" (B "LK*")
      ", 其是" (B "LK") "的一种替代. "
      "这将会是展现证明论式论证如何进行的一个简明例子.")
   (H3. "开篇")
   (H4 "基本概念和记号")
   (P "我们将会使用" $conj ", " $disj ", " $impl ", " $neg
      "分别表达基本的逻辑联结词" (Em "合取") ", " (Em "析取")
      ", " (Em "implication") ", " (Em "否定")
      ". {译注: 为了避免各种可能的混淆, implication就不翻译了.} "
      "若有必要, 例如当我们讨论模态逻辑和亚结构逻辑的时候, "
      "我们将会引入额外的逻辑联结词和逻辑常量. "
      "我们从外部取一个" (Em "固定的命题变量的可数集")
      ". 公式和通常情况一样通过归纳定义, "
      "从命题变量和逻辑常量开始, 然后反复应用逻辑联结词. "
      "括号会按照通常方式进行省略, 只要不至于引起误解. "
      "为了简化表达式, 我们采取约定, " $neg
      "比其他逻辑联结词都结合得更紧密. 因此, 例如, 公式"
      (&disj $p (&neg $q)) "表示" (&disj $p (@neg $q))
      ". 我们将会使用小写希腊字母表达公式. 当一个公式"
      $beta "出现在一个公式" $alpha "的归纳定义之中, "
      "公式" $beta "被称为" $alpha "的一个" (Em "子公式")
      ". 除开" $alpha "之外的" $alpha "的每个子公式被称为"
      $alpha "的一个" (Em "真子公式") ". 例如, "
      (&cm $p $q (&neg $q) (&disj $p (&neg $q)))
      "都是" (&disj $p (&neg $q)) "的子公式, 而前三个则是"
      (&disj $p (&neg $q)) "的真子公式.")
   (P "有时我们按照以下方式对于公式的构造施行归纳论证. "
      "也就是说, 为了确保每个公式都具有某个给定的性质" $P
      ", 我们要表明 (1) 每个命题变量 (以及每个常量) 都具有"
      $P " (2) 对于每个" $beta ", 若" $beta
      "的每个真子公式" $gamma "都具有" $P ", 那么"
      $beta "也具有" $P ". 这种形式的归纳也可以表达为"
      "对于公式的" (Em "长度") "进行归纳. "
      "这里某个给定公式" $alpha "的长度的含义是"
      $alpha "中的逻辑符号的总数目. 可能公式" $beta
      "在某个给定的" $alpha "中作为子公式出现不止一次, "
      "但是在不同的地方出现. 例如, 公式" $p
      "在公式" (&disj $p (&neg $p)) "里出现了两次. "
      "如果我们需要进行区分, 那么我们会使用术语"
      (Em "公式出现") ". 然后, 我们可以说"
      (&disj $p (&neg $p)) "拥有" $p
      "的两次公式出现. 一般而言, "
      "当我们不只是关心表达式本身也关心其出现的位置时, "
      "我们就会使用词汇" (Em "出现")
      ". 在本书中, 我们有时使用" (Em "iff")
      "作为" (Em "if and only if")
      "的缩写. {译注: 当然了, 我们都会将其翻译为"
      (Em "当且仅当") ", 这是Paul Halmos所创制的表达.}")
   (H4 "逻辑的句法方法和语义方法之比较")
   (P "在以下内容里, 我们将简要介绍主题的背景, "
      "特别是本教科书的第I部分和第II部分的区别和联系. "
      "熟悉这部分内容的读者可以跳过.")
   (P "引入一个逻辑的标准方式时将其描述为一个形式系统. "
      "一个形式系统通常由" (Em "公理") "和"
      (Em "推理规则") "构成, 其确定了系统中的公式的"
      (Em "可证明性") ". 一个给定公式" $alpha
      "在系统中是" (Em "可证的") ", 如果" $alpha
      "可以在系统中被推导出来, 即通过从系统的公理开始, "
      "反复应用系统的规则得到. 一个对于" $alpha
      "的" (Em "证明") " (或者" (Em "推导")
      ") 是一个有限的figure, 其表明了" $alpha
      "在系统中是如何推导出来的. 证明和可证明性的概念由此是"
      (Em "句法性") "的概念, 其由符号和机械过程构成.")
   (P "作为形式系统的一个例子, 以下我们将给出古典逻辑的"
      "一个标准Hilbert风格系统" (B "HK")
      ". 其由作为单一规则的" (Em "modus ponens")
      "和以下给出的" (Em "公理模式")
      "构成. 这里的modus ponens是说一个公式"
      $beta "可以由" $alpha "和" (&-> $alpha $beta)
      "共同推导出来. 另外, 每个公理模式不是单一的公式, "
      "而是具有模式所表示的相同句法形式的所有公式. "
      "因此, 一个公理模式的每个实例都可以视为一个公理. "
      "(出于简单性的考量, " (&neg $alpha)
      "在本节的剩余部分里将被视为" (&impl $alpha $0)
      "的缩略, 其中" $0 "是一个逻辑常量, "
      "其表达了一个矛盾.) "
      "{译注: 为了避免符号太过复杂, "
      "我们使用相同的符号表达公式和公式模式.}")
   (P "现在" (B "HK") "的公理模式列出如下, "
      "其看上去和标准的有些许不同. "
      "不过, 我们的选择是为了容易看出其与"
      "下一节要引入的古典逻辑的相继式系统"
      (B "LK") "之间的联系."
      (Ol (Li (&impl* $alpha $beta $alpha)
              " (左弱化公理)")
          (Li (&impl
               (@impl* $alpha $alpha $gamma)
               (@impl $alpha $gamma))
              " (收缩公理)")
          (Li (&impl
               (@impl* $alpha $beta $gamma)
               (@impl* $beta $alpha $gamma))
              " (交换公理)")
          (Li (&impl $0 $alpha))
          (Li (&impl*
               (@impl $alpha $beta)
               (@impl $gamma $alpha)
               (@impl $gamma $beta)))
          (Li (&impl*
               (@impl $alpha $gamma)
               (@impl $beta $gamma)
               (@disj $alpha $beta)
               $gamma))
          (Li (&impl $alpha (@disj $alpha $beta)))
          (Li (&impl $beta (@disj $alpha $beta)))
          (Li (&impl*
               (@impl $gamma $alpha)
               (@impl $gamma $beta)
               $gamma
               (@conj $alpha $beta)))
          (Li (&impl (@conj $alpha $beta) $alpha))
          (Li (&impl (@conj $alpha $beta) $beta))
          (Li (&impl (&neg (&neg $alpha)) $alpha)
              " (双重否定律)")))
   (P "呈现Hilbert系统的另一种方式是取modus ponens和"
      (Em "替换规则") "作为规则, 然后取" (Em "公理")
      "而非公理模式. "
      )
   (H2. "相继式系统的切消")
   (H2. "逻辑性质的证明论式分析")
   (H2. "模态逻辑和亚结构逻辑")
   (H2. "")
   (H2. "从代数到逻辑")
   (H2. "代数逻辑的基本")
   
   ))