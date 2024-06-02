#lang racket
(provide proof_theory_and_algebra_in_logic.html)
(require SMathML)
(define $sharp (Mi "&sharp;"))
(define &split16 (&split 16))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define $impl $->)
(define-infix*
  (&=> $=>)
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
(define $=>:id (Mi "&rArr;"))
(define @disj=>
  (@ $disj $=>:id))
(define @=>disj1
  (@ $=>:id $disj $1))
(define @=>disj2
  (@ $=>:id $disj $2))
(define @conj1=>
  (@ $conj $1 $=>:id))
(define @conj2=>
  (@ $conj $2 $=>:id))
(define @=>conj
  (@ $=>:id $conj))
(define @impl=>
  (@ $impl $=>:id))
(define @=>impl
  (@ $=>:id $impl))
(define @neg=>
  (@ $neg $=>:id))
(define @=>neg
  (@ $=>:id $neg))
(define $cut (Mi "cut"))
(define @cut (@ $cut))
(define @e=> (@ $e $=>:id))
(define @=>e (@ $=>:id $e))
(define @c=> (@ $c $=>:id))
(define @=>c (@ $=>:id $c))
(define @w=> (@ $w $=>:id))
(define @=>w (@ $=>:id $w))
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
      "然后对于" (B "LK") "的完备性和切消的初等证明被给出, "
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
      "{原注: 为了避免符号太过复杂, "
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
   (P "呈现Hilbert风格系统的另一种方式是取modus ponens和"
      (Em "替换规则") "作为规则, 然后取" (Em "公理")
      "而非公理模式. 通过这里的替换规则我们可以根据"
      $alpha "对于任意的一致替换" $sigma "推出"
      (app $sigma $alpha)
      ". {原注: 这里的" $sigma
      "是一个从命题变量的有限集合"
      (setE $p_1 $..h $p_m)
      "到某个公式集合的映射, 而"
      (app $sigma $alpha) "代表由"
      $alpha "通过将" $alpha
      "中的每个" $p_i "同时替换为"
      (app $sigma $p_i)
      "得到的公式, 其中"
      (&= $i (&cm $1 $..h $m))
      ". 这样一个公式" (app $sigma $alpha)
      "被称为" $alpha "的一个" (Em "替换实例")
      " (或者更简单地说, 实例).} "
      "在这种情况下, 例如弱化公理就是通过一个单独的公理"
      (&impl* $p $q $p) "给出的, 其中" $p
      "和" $q "是不同的命题变量. "
      "其他的弱化公理则是通过这条公理应用替换规则得到的.")
   
   (H3. "古典逻辑的相继式系统LK")
   (P "我们将要引入古典逻辑的一个相继式系统" (B "LK")
      ". 与Hilbert风格系统不同的是, 相继式系统中的基本表达式是"
      (Em "相继式") ". " (B "LK")
      "中的每个相继式都是具有以下形式的表达式, 其中每个"
      $alpha_i " (" (&<= $i $m) ") 和每个" $beta_j
      " (" (&<= $j $n) ") 都是公式, 而"
      (&>= (&cm $m $n) $0) ". 这里的逗号" (Q $cm)
      "和箭头" (Q $=>) "都是元逻辑符号."
      (MB (&=> (&cm $alpha_1 $..h $alpha_m)
               (&cm $beta_1 $..h $beta_n)))
      "因此, 每个相继式都是由逗号分隔的公式的有限序列, "
      "而相继式又由" $=> "划分为"
      (Em "前件(antecedents)")
      (&cm $alpha_1 $..h $alpha_m)
      "和" (Em "后件(succedents)")
      (&cm $beta_1 $..h $beta_n) ".")
   (P "大致来说, 前件和后件可以分别理解为" (Em "假设")
      "和" (Em "结论") ". 但是, 这里我们必须要小心, 因为前件是"
      (Em "合取式(conjunctive-like)") "理解而后件是"
      (Em "析取式(disjunctive-like)")
      "理解. 也就是说, 上述的相继式从直觉上来理解即"
      (@disj $beta_1 $..h $beta_n) "可由假设"
      (&cm $alpha_1 $..h $alpha_m)
      "推出, 或者等价地说, 可由假设"
      (@conj $alpha_1 $..h $alpha_m)
      "推出. 当后件为空时, 相继式"
      (&=> (&cm $alpha_1 $..h $alpha_m) $)
      "的含义是"
      (Q "根据假设" (&cm $alpha_1 $..h $alpha_m)
         "可以推出一个矛盾")
      ". 从另一方面来说, 如果前件为空, 相继式"
      (&=> $ (&cm $beta_1 $..h $beta_n))
      "应该理解为(在没有任何假设的情况下)可以推出"
      (&disj $beta_1 $..h $beta_n) ".")
   (P "在相继式系统里, 我们讨论" (Em "相继式")
      "的可证明性和有效性, 而非公式的, "
      "它们分别代表了句法和语义的视角. "
      "每个相继式系统都由" (Em "初始相继式") "和" (Em "规则")
      "构成. 前者对应于Hilbert风格系统中的公理模式. "
      "一个给定的相继式系统的每条规则确定了"
      "一个相继式何时以及如何由其他已经推得的相继式推出 "
      "(通常来说, 是从有限数目的相继式推出). "
      "在我们的系统" (B "LK")
      "中, 如下所示, 每条规则由一或两个"
      (Em "上相继式(upper sequent)") "和一个"
      (Em "下相继式(lower sequent)")
      "构成, 意即下相继式可由这些上相继式推出. "
      "以下的大写希腊字母代表(可能为空的)公式的有限序列. "
      (B "LK") "具有以下三个种类的规则"
      "{原注: 切规则经常被视为一种结构规则, "
      "但是本书将其从结构规则中分离出来, "
      "因为接下来介绍亚结构逻辑的时候会更加方便.}:"
      (Ol (Li "逻辑联结词"
              (&cm $disj $conj $impl $neg)
              "的(左和右)规则,")
          (Li "切规则,")
          (Li "(左和右)结构规则.")))
   (P (B "LK") "具有以下初始相继式和规则.")
   (P "0. 初始相继式: " (B "LK")
      "的每个初始相继式都是一个具有形式"
      (&=> $alpha $alpha) "的相继式.")
   (P "1. 逻辑联结词的规则:"
      (MB (&rull
           @disj=>
           (&=> (&cm $alpha $Gamma) $Pi)
           (&=> (&cm $beta $Gamma) $Pi)
           (&=> (&cm (&disj $alpha $beta) $Gamma) $Pi)))
      (MB (&split16
           (&rull
            @=>disj1
            (&=> $Gamma (&cm $Lambda $alpha))
            (&=> $Gamma (&cm $Lambda (&disj $alpha $beta))))
           (&rull
            @=>disj2
            (&=> $Gamma (&cm $Lambda $beta))
            (&=> $Gamma (&cm $Lambda (&disj $alpha $beta))))))
      (MB (&split16
           (&rull
            @conj1=>
            (&=> (&cm $alpha $Gamma) $Pi)
            (&=> (&cm (&conj $alpha $beta) $Gamma) $Pi))
           (&rull
            @conj2=>
            (&=> (&cm $beta $Gamma) $Pi)
            (&=> (&cm (&conj $alpha $beta) $Gamma) $Pi))))
      (MB (&rull
           @=>conj
           (&=> $Gamma (&cm $Lambda $alpha))
           (&=> $Gamma (&cm $Lambda $beta))
           (&=> $Gamma (&cm $Lambda (&conj $alpha $beta)))))
      (MB (&split16
           (&rull
            @impl=>
            (&=> $Gamma (&cm $Lambda $alpha))
            (&=> (&cm $beta $Delta) $Pi)
            (&=> (&cm (&impl $alpha $beta)
                      $Gamma $Delta)
                 (&cm $Lambda $Pi)))
           (&rull
            @=>impl
            (&=> (&cm $alpha $Gamma)
                 (&cm $Lambda $beta))
            (&=> $Gamma
                 (&cm $Lambda
                      (&impl $alpha $beta))))))
      (MB (&split16
           (&rull
            @neg=>
            (&=> $Gamma (&cm $Lambda $alpha))
            (&=> (&cm (&neg $alpha) $Gamma) $Lambda))
           (&rull
            @=>neg
            (&=> (&cm $alpha $Gamma) $Lambda)
            (&=> $Gamma (&cm $Lambda (&neg $alpha)))))))
   (P "2. 切规则"
      (MB (&rull
           @cut
           (&=> $Gamma (&cm $Lambda $alpha))
           (&=> (&cm $alpha $Delta) $Pi)
           (&=> (&cm $Gamma $Delta)
                (&cm $Lambda $Pi)))))
   (P "3. 结构规则"
      (Ul (Li "交换规则:"
              (MB (&split16
                   (&rull
                    @e=>
                    (&=> (&cm $Gamma $alpha $beta $Delta) $Pi)
                    (&=> (&cm $Gamma $beta $alpha $Delta) $Pi))
                   (&rull
                    @=>e
                    (&=> $Gamma (&cm $Pi $alpha $beta $Lambda))
                    (&=> $Gamma (&cm $Pi $beta $alpha $Lambda))))))
          (Li "收缩规则:"
              (MB (&split16
                   (&rull
                    @c=>
                    (&=> (&cm $alpha $alpha $Gamma) $Pi)
                    (&=> (&cm $alpha $Gamma) $Pi))
                   (&rull
                    @=>c
                    (&=> $Gamma (&cm $Pi $alpha $alpha))
                    (&=> $Gamma (&cm $Pi $alpha))))))
          (Li "弱化规则:"
              (MB (&split16
                   (&rull
                    @w=>
                    (&=> $Gamma $Pi)
                    (&=> (&cm $alpha $Gamma) $Pi))
                   (&rull
                    @=>w
                    (&=> $Gamma $Pi)
                    (&=> $Gamma (&cm $Pi $alpha))))))))
   (P "出于方便的考量, 我们为每条规则附加了一个诸如" @disj=>
      "这样的名字. 在每条规则里, 使用小写希腊字母" $alpha "和"
      $beta "的公式被称为" (Em "活跃(active)")
      "公式. 另外, 每条规则里的下相继式里显式呈现的"
      "使用小写希腊字母的公式被称为规则的" (Em "主(principal)")
      "公式. 例如, " (&disj $alpha $beta) "是规则" @disj=>
      "的主公式, 而" $alpha "是规则" @w=> "的主公式. "
      "其他的公式都被称为" (Em "副(side)")
      "公式. 切规则的活跃公式被称为" (Em "切公式")
      ". 对于逻辑联结词的规则而言, 主公式出现在前件里的是"
      (Em "左规则") ". 对于结构规则, 活跃公式都出现在前件里的是"
      (Em "左规则") ". 将前件替换为后件, 可以得到"
      (Em "右规则") "的定义. 左规则的名字具有形式"
      (@ $sharp $=>:id) ", 右规则的名字具有形式"
      (@ $=>:id $sharp) ".")
   (P "每条规则都是说"
      (Em "当上相继式可证明时, 其下相继式也是可证明的")
      ". 例如, 规则" @conj1=> "是说每当相继式"
      (&=> (&cm $alpha $Gamma) $Pi)
      "可证明时, 相继式"
      (&=> (&cm (&conj $alpha $beta) $Gamma) $Pi)
      "也是可证明的. 类似地, 规则" @=>conj
      "是说 (为了简洁, 假定" $Lambda "为空) 每当"
      (&=> $Gamma $alpha) "和" (&=> $Gamma $beta)
      "都是可证的, 那么" (&=> $Gamma (&conj $alpha $beta))
      "也是可证的. 以这种方式, 每个逻辑联结词的公式"
      "精确地描述了这个联结词的功能.")
   (P "切规则表达了演绎推理的一种基础原则. "
      "实际上, 其是在说 (又一次为了简洁, 假定"
      $Lambda "为空) 如果" (&=> $Gamma $alpha)
      "是可证明的, 并且"
      (&=> (&cm $alpha $Delta) $Pi)
      "是可证明的, 那么"
      (&=> (&cm $Gamma $Delta) $Pi)
      "也是可证明的. "
      "所有的结构规则作为一个整体"
      "控制了给定相继式里的前件和后件里的公式的"
      "顺序, 重复和省略. 特别地, "
      "每条左结构规则从直觉上来说含义如下:"
      (Ul (Li "交换规则允许我们使用前件里的公式, "
              "不论它们以何种顺序排列.")
          (Li "收缩规则表达了前件里的"
              "公式出现在推理结论时"
              "可以使用不止一次.")
          (Li "左弱化规则允许我们将任意的公式放在前件里, "
              "即便其在推理结论时没有用到.")))
   ((definition #:n "1.1")
    (Em "(证明和可证明性) ")
    (B "LK") "中的某个相继式" (&=> $Gamma $Delta)
    "的一个" (Em "证明") $P:sans-serif
    "是一个有限树状图形, 以如下方式定义, "
    "其展现了该相继式在" (B "LK")
    "中是如何从初始相继式推导出来的."
    (Ul (Li "证明" $P:sans-serif
            "中的每个最上层相继式都是初始相继式.")
        (Li "证明" $P:sans-serif
            "中除了最上层相继式之外的所有相继式"
            "都是通过应用任意一条规则得到的.")
        (Li (&=> $Gamma $Delta)
            "是唯一的最下层相继式, 其被称为证明"
            $P:sans-serif "的"
            (Em "终相继式(end sequent)") "."))
    "一个相继式" (&=> $Gamma $Delta) "在"
    (B "LK") "中是" (Em "可证明的")
    "当且仅当存在一个对于" (&=> $Gamma $Delta)
    "的证明. 另外, 当相继式" (&=> $ $alpha)
    "在" (B "LK") "中可证明时, 我们也称公式"
    $alpha "在" (B "LK") "是可证明的.")
   (P "我们注意到一个相继式在可证明时或许拥有数个不同的证明. "
      "对于某个相继式" (&=> $Gamma $Delta)
      "的一个给定证明" $P:sans-serif
      ", 设" (&=> $Sigma $Theta) "是出现在"
      $P:sans-serif "中的一个相继式. 那么, 证明"
      $P:sans-serif "中相继式"
      (&=> $Sigma $Theta)
      "之上 (也包括自身) 的子图形" $Q:sans-serif
      "构成了一个终相继式为" (&=> $Sigma $Theta)
      "的证明. 作为" $Q:sans-serif
      "这样的一个证明被称为" $P:sans-serif
      "的一个" (Em "子证明") ".")
   (P "在给出证明的具体例子之前, "
      "我们给出为了简化对于证明的描述的约定. "
      "我们注意到在我们所列出的" (B "LK")
      "的规则里, 上相继式里的活跃公式总是置于前件的最左边或者后件的最右边, "
      "除了交换规则. 理由只是为了简化规则的呈现, "
      "因而这些限制不是本质性的. 实际上, "
      (Em "只要活跃公式出现在相应的前件或者后件的任何位置")
      ", 规则就可以应用. 这是因为, "
      "我们可以首先对于上相继式应用交换规则, "
      "将活跃公式移至前件的最左边或者后件的最右边, "
      "然后应用所牵涉的规则, "
      "最后再用交换规则将活跃公式移回去. "
      "为了解释这种调整, "
      "让我们考虑以下形式的规则" @conj1=> ":"
      (MB (&rule
           (&=> (&cm $Sigma $alpha $Gamma) $Delta)
           (&=> (&cm $Sigma
                     (&conj $alpha $beta)
                     $Gamma)
                $Delta)))
      "以下是从这上相继式得到下相继式的方式."
      (MB (&rule
           (&rule
            (&rull
             @conj1=>
             (&rule
              (&rule
               (&=> (&cm $Sigma $alpha $Gamma) $Delta)
               $..c)
              (&=> (&cm $alpha $Sigma $Gamma) $Delta))
             (&=> (&cm (&conj $alpha $beta)
                       $Sigma $Gamma)
                  $Delta))
            $..c)
           (&=> (&cm $Sigma
                     (&conj $alpha $beta)
                     $Gamma)
                $Delta)))
      "这里的点" $..h "代表对于交换规则的数次应用. "
      "以这种方法, 我们将允许应用每条规则"
      "而不管其活跃公式出现在哪里.")
   ((example #:n "1.2")
    
    )
   ((exercise #:n "1.1")
    "在" (B "LK") "中给出对于以下相继式的证明."
    (Ol (Li (&=> $ (&impl* $alpha $beta $alpha)))
        (Li (&=> (&impl* $alpha $beta $gamma)
                 (&impl (@impl $alpha $beta)
                        (@impl $alpha $gamma))))))
   ((exercise #:n "1.2")
    
    )
   ((remark #:n "1.3")
    
    )
   (H4 "使用公式的多重集的一个对于LK的改进呈现")
   (H4 "在给定证明的长度上进行归纳")
   (H4 "可逆系统LK*")
   (H2. "相继式系统的切消")
   (H2. "逻辑性质的证明论式分析")
   (H2. "模态逻辑和亚结构逻辑")
   (H2. "")
   (H2. "从代数到逻辑")
   (H2. "代数逻辑的基本")
   
   ))