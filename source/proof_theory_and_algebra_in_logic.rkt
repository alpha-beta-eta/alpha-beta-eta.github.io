#lang racket
(provide proof_theory_and_algebra_in_logic.html)
(require SMathML)
(define (powerset S)
  (app $P:script S))
(define (&neg2 A) (&neg (&neg A)))
(define (!0 f) (curry app f))
(define (!1 f) (curry apply f))
(define (!commute f g . x*)
  (&= (f (apply g x*))
      ((!1 g) (map f x*))))
(define ((tcomment #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "tcomment")))
   (B (format "译者注记~a." n)) " " x*))
(define (distributeR a * b + c)
  (&= (* a (@ (+ b c)))
      (+ (@ (* a b)) (@ (* a c)))))
(define BA2 (Mi "2" #:attr* '((mathvariant "bold"))))
(define $->:id (Mi "&rarr;"))
(define $Join (Mo "&Vee;"))
(define Join (make-bigop $Join))
(define $Meet (Mo "&Wedge;"))
(define Meet (make-bigop $Meet))
(define (&max S)
  (ap $max S))
(define $min (Mi "min"))
(define (&min S)
  (ap $min S))
(define ((answer #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "answer")))
   (B (format "解答~a." n)) " " x*))
(define $sharp (Mi "&sharp;"))
(define &split16 (&split 16))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define $impl $->)
(define $join $disj)
(define $join^ (&prime $join))
(define $meet $conj)
(define $meet^ (&prime $meet))
(define $\\ (Mo "\\"))
(define $*^A (^ $* $A:bold))
(define $*^B (^ $* $B:bold))
(define $*:id (Mi "&#8270;"))
(define $<=^A (^ $<= $A:bold))
(define $<=^B (^ $<= $B:bold))
(define $meet^A (^ $meet $A:bold))
(define $meet^B (^ $meet $B:bold))
(define $join_A (_ $join $A))
(define $meet_A (_ $meet $A))
(define $->_A (_ $-> $A))
(define-infix*
  (&join_A $join_A)
  (&meet_A $meet_A)
  (&->_A $->_A)
  (&meet^A $meet^A)
  (&meet^B $meet^B)
  (&<=^A $<=^A)
  (&<=^B $<=^B)
  (&*^A $*^A)
  (&*^B $*^B)
  (&join^ $join^)
  (&meet^ $meet^)
  (&\\ $\\)
  (&join $join)
  (&meet $meet)
  (&=> $=>)
  (&conj $conj)
  (&disj $disj)
  (&impl $impl))
(define &impl*
  (case-lambda
    ((x y) (&impl x y))
    ((x y . z*) (&impl x (@ (apply &impl* y z*))))))
(define-@lized-op*
  (@-> &->)
  (@join &join)
  (@meet &meet)
  (@conj &conj)
  (@disj &disj)
  (@impl &impl)
  (@impl* &impl*)
  (@neg &neg))
(define $=>:id (Mi "&rArr;"))
(define $disj:id (Mi "&or;"))
(define @disj=>
  (@ $disj:id $=>:id))
(define @=>disj1
  (@ $=>:id $disj:id $1))
(define @=>disj2
  (@ $=>:id $disj:id $2))
(define $conj:id (Mi "&and;"))
(define @conj1=>
  (@ $conj:id $1 $=>:id))
(define @conj2=>
  (@ $conj:id $2 $=>:id))
(define @=>conj
  (@ $=>:id $conj:id))
(define $impl:id (Mi "&rarr;"))
(define @impl=>
  (@ $impl:id $=>:id))
(define @=>impl
  (@ $=>:id $impl:id))
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
(define $join:id $disj:id)
(define $meet:id $conj:id)
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
   ((answer)
    (Ol (Li (MB (&rull
                 @=>impl
                 (&rull
                  @=>impl
                  (&rull
                   @w=>
                   (&=> $alpha $alpha)
                   (&=> (&cm $beta $alpha) $alpha))
                  (&=> $alpha (&impl $beta $alpha)))
                 (&=> $ (&impl* $alpha $beta $alpha)))))
        (Li (let* ((deriv0
                    (&rull
                     @impl=>
                     (&=> $beta $beta)
                     (&=> $gamma $gamma)
                     (&=> (&cm (&impl $beta $gamma) $beta)
                          $gamma)))
                   (deriv1
                    (&rull
                     @impl=>
                     (&=> $alpha $alpha)
                     deriv0
                     (&=> (&cm (&impl* $alpha $beta $gamma)
                               $alpha $beta)
                          $gamma)))
                   (deriv2
                    (&rull
                     @e=>
                     deriv1
                     (&=> (&cm $beta $alpha
                               (&impl* $alpha $beta $gamma))
                          $gamma)))
                   (deriv3
                    (&rull
                     @impl=>
                     (&=> $alpha $alpha)
                     deriv2
                     (&=> (&cm (&impl $alpha $beta)
                               $alpha $alpha
                               (&impl* $alpha $beta $gamma))
                          $gamma)))
                   (deriv4
                    (&rull
                     @c=>
                     deriv3
                     (&=> (&cm (&impl $alpha $beta)
                               $alpha
                               (&impl* $alpha $beta $gamma))
                          $gamma)))
                   (deriv5
                    (&rull
                     @e=>
                     deriv4
                     (&=> (&cm $alpha
                               (&impl $alpha $beta)
                               (&impl* $alpha $beta $gamma))
                          $gamma)))
                   (deriv6
                    (&rull
                     @=>impl
                     deriv5
                     (&=> (&cm (&impl $alpha $beta)
                               (&impl* $alpha $beta $gamma))
                          (&impl $alpha $gamma))))
                   (deriv7
                    (&rull
                     @=>impl
                     deriv6
                     (&=> (&impl* $alpha $beta $gamma)
                          (&impl (@impl $alpha $beta)
                                 (@impl $alpha $gamma))))))
              (MB deriv7)))))
   ((exercise #:n "1.2")
    "在" (B "LK") "中给出对于以下相继式的证明. "
    "(检查你的证明中是否存在某个相继式在后件里至少拥有两个公式.)"
    (Ol (Li (&=> $ (&disj $alpha (&neg $alpha))))
        (Li (&=> (&neg (@conj $alpha $beta))
                 (&disj (&neg $alpha) (&neg $beta))))
        (Li (&=> (&impl (@impl $alpha $beta) $alpha)
                 $alpha))))
   ((answer)
    (Ol (Li (let* ((deriv0
                    (&rull
                     @=>neg
                     (&=> $alpha $alpha)
                     (&=> $ (&cm $alpha (&neg $alpha)))))
                   (deriv1
                    (&rull
                     @=>disj1
                     deriv0
                     (&=> $ (&cm (&disj $alpha (&neg $alpha))
                                 (&neg $alpha)))))
                   (deriv2
                    (&rull
                     @=>disj2
                     deriv1
                     (&=> $ (&cm (&disj $alpha (&neg $alpha))
                                 (&disj $alpha (&neg $alpha))))))
                   (deriv3
                    (&rull
                     @=>c
                     deriv2
                     (&=> $ (&disj $alpha (&neg $alpha))))))
              (MB deriv3))
            "是的, 的确存在相继式在后件中拥有两个及以上的公式.")
        (Li ""
            )
        )
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
   (P "逻辑的句法或者说符号方法起源于19世纪中叶. "
      "G. Boole在他的书 (Boole 1854) 中"
      "试图将逻辑推理表达为代数计算. "
      "又过了几十年我们才有了Hilbert风格的系统. "
      "尽管并非以其完备形式, "
      "Boole通过引入一些代数等式 (作为逻辑公理) "
      "和一些用于从代数等式推导其他代数等式的规则 (作为推理规则) "
      "建立了代数逻辑的公理基础. "
      "我们应该注意到大约在那个时期, "
      "诸如群, 环, 域这样的抽象代数结构得到了引入, "
      "而对于这些结构的研究开始了. "
      "尽管如此, 那时数学家主要关注具体的代数结构, "
      "例如整数集, 有理数集, 实数集, 等等. "
      "本章所介绍的布尔代数是一种抽象代数结构, "
      "其基本上是由Boole引入的. "
      "显然, 布尔代数的定义来源于对于古典逻辑的行为的代数描述.")
   (P "在给出包括格与布尔代数等的一些基本代数结构之后, "
      "我们将会在第6.2节引入三个代数概念, "
      "其基本上贯穿了我们整个第二部分的讨论. "
      "本章的最后两节我们给出了逻辑与代数的联系的一些例子, "
      "这是第二部分的主题.")
   (H3. "格与布尔代数")
   (P "以下我们 先引入一些代数结构的基本概念, 然后呈现其性质.")
   ((definition #:n "6.1")
    "(偏序) 一个集合" $A "上的一个" (Em "偏序") $<=
    "是" $A "上的一个二元关系, 其满足以下性质: 对于所有的"
    (∈ $x $y $z $A) ", 我们有"
    (Ol (Li (&<= $x $x) " (自反性);")
        (Li "如果" (&<= $x $y) "且" (&<= $y $z)
            ", 那么" (&<= $y $z) " (传递性);")
        (Li "如果" (&<= $x $y) "且" (&<= $y $x)
            ", 那么" (&= $x $y) " (反对称性).")))
   (P "一个" (Em "偏序集") (tupa0 $A $<=)
      "是由一个集合" $A "和一个其上的偏序" $<=
      "构成的序对. 一个集合上的一个偏序" $<=
      "是一个" (Em "全序") " (或者说" (Em "线序")
      "), 如果对于所有的" (∈ $x $y $A)
      ", " (&<= $x $y) "或者" (&<= $y $x)
      "总是成立. 在这种情况下, " (tupa0 $A $<=)
      "被称为是一个" (Em "全序集") ", 或者说"
      (Em "链") ". 关系" (&< $x $y) "由条件"
      (&<= $x $y) "但不" (&<= $y $x)
      "定义, 其被称为(由偏序" $<= "导出的)"
      (Em "严格序") ". 显然, " (&<= $x $y)
      "当且仅当" (&< $x $y) "或" (&= $x $y)
      "成立. 以下每张图 (Hasse图) 都表示了一个偏序集, "
      "只有最左边的是一个链.")
   ((example #:n "6.1")
    "令" $NN "是自然数集, 即正整数集, 而" $<=
    "是自然数上的通常序. 显然, "
    )
   (H4 "格")
   ((definition #:n "6.2")
    "(格) 一个偏序集" (tupa0 $A $<=) "是一个" (Em "格")
    ", 如果对于所有的" (∈ $x $y $A) ", 存在"
    (&join $x $y) " (join) 和" (&meet $x $y)
    " (meet) 满足:"
    (Ol (Li (&<= $x (&join $x $y)) "且"
            (&<= $y (&join $x $y)) ";")
        (Li "对于" (∈ $z $A) ", 如果"
            (&<= $x $z) "且" (&<= $y $z)
            ", 那么" (&<= (&join $x $y) $z) ";")
        (Li (&<= (&meet $x $y) $x) "且"
            (&<= (&meet $x $y) $y) ";")
        (Li "对于" (∈ $z $A) ", 如果"
            (&<= $z $x) "且" (&<= $z $y)
            ", 那么" (&<= $z (&meet $x $y)) "."))
    "以上定义中的第一个条件是说" (&join $x $y)
    "是" $x "和" $y "的一个" (Em "上界")
    ", 即一个元素其既大于等于" $x "又大于等于" $y
    ". 第二个条件是说" (&join $x $y) "小于等于"
    $x "和" $y "的任何上界. 因此, " $x "和" $y
    "的join " (&join $x $y) "是" $x "和" $y
    "的" (Em "最小上界") ". 类似地, 第三个条件是说"
    $x "和" $y "的meet " (&meet $x $y) "是"
    $x "和" $y "的一个" (Em "下界")
    ", 而第四个条件和第三个条件连在一起是说"
    $x "和" $y "的meet " (&meet $x $y) "是"
    $x "和" $y "的" (Em "最大下界")
    ". 显然, 以下等价在每个格中都成立: "
    (&= (&join $x $y) $y) "当且仅当"
    (&<= $x $y) "当且仅当"
    (&= (&meet $x $y) $x)
    ". 由此我们可以推出, 一个给定格的偏序"
    $<= "可以完全由" $join "或者" $meet
    "确定. 因此, 我们可以安全地说"
    (tupa0 $A $join:id $meet:id) "而非"
    (tupa0 $A $<=) "是一个格. 一个给定的格是"
    (Em "有界的") ", 如果其存在最大元素和最小元素, "
    "往往我们分别将其记作" $top "和" $bottom
    ". 在以下三种偏序集里, 左边的和中间的是格, "
    "右边的不是.")
   ((example #:n "6.2")
    "1. 每个全序集都构成了一个格, 只要定义"
    (&= (&join $x $y) (&max (setE $x $y))) "而"
    (&= (&meet $x $y) (&min (setE $x $y)))
    ". 全序集" (tupa0 $NN $<=) "具有最小元素"
    $1 ", 但是没有最大元素, 因而其不是有界的." (Br)
    "2. 例子6.1里的第二个例子, 即偏序集"
    )
   ((exercise #:n "6.1")
    "证明在任意的格中, " (&<= $x $y)
    "可以推出对于每个" $z "有"
    (&<= (&join $x $z) (&join $y $z)) "且"
    (&<= (&meet $x $z) (&meet $y $z)) ".")
   ((answer)
    "根据" (&join $y $z) "的定义, 我们知道"
    (&<= $y (&join $y $z)) "且"
    (&<= $z (&join $y $z)) ". 另外, 我们还知道"
    (&<= $x $y) ", 于是根据传递性可知"
    (&<= $x (&join $y $z)) ". 那么, "
    (&join $y $z) "是" $x "和" $z
    "的一个上界, 所以"
    (&<= (&join $x $z) (&join $y $z))
    ". 根据" (&meet $x $z) "的定义, "
    (&<= (&meet $x $z) $x) "且"
    (&<= (&meet $x $z) $z) ". 另外根据"
    (&<= $x $y) "和传递性可知"
    (&<= (&meet $x $z) $y)
    ". 于是, " (&meet $x $z)
    "是" $y "和" $z "的一个下界. 那么, 我们有"
    (&<= (&meet $x $z) (&meet $y $z)) ".")
   (P "我们将给出格的基本等式.")
   ((lemma #:n "6.1")
    "以下等式在任意的格中成立. 对于所有的"
    (&cm $x $y $z) ", 我们有"
    (Ul (Li "(1a) " (&= (&join $x $x) $x))
        (Li "(1b) " (&= (&meet $x $x) $x))
        (Li "(2a) " (commute &join $x $y))
        (Li "(2b) " (commute &meet $x $y))
        (Li "(3a) " (associate &join $x $y $z))
        (Li "(3b) " (associate &meet $x $y $z))
        (Li "(4a) " (&= (&join $x (@meet $x $y)) $x))
        (Li "(4b) " (&= (&meet $x (@join $x $y)) $x))))
   ((exercise #:n "6.2")
    "给出引理6.1的(3a)和(4a)的证明.")
   ((answer)
    
    )
   ((remark #:n "6.3")
    "(格的另一种定义) 根据定义6.2, "
    "一个格是一个偏序集且对于由其成员构成的序对, "
    "join和meet总是存在. 然后, 引理6.1说的是对于每个格而言, "
    "这八个等式总是成立. 实际上, 定义格的另一种方式是取一个具有形式"
    (tupa0 $A $join:id $meet:id) "的代数, 其两个运算"
    $join "和" $meet "总是满足这八个等式." (Br)
    "为了表明这种等价性, 有必要引入一个偏序使得在这样的代数之中运算"
    $join "和" $meet "分别表达了(相对于这个偏序而言的)join和meet. "
    "事实上, 对于八个等式成立的代数" (tupa0 $A $join:id $meet:id)
    "我们可以证明以下三条陈述总是成立:"
    (Ol (Li "对于所有的" $x "和" $y ", "
            (&= (&join $x $y) $y) "当且仅当"
            (&= (&meet $x $y) $x) ".")
        (Li "根据" (&= (&meet $x $y) $x)
            "定义" $A "上的一个二元关系"
            (&<= $x $y) ", 那么" $<=
            "是一个偏序. (我们将其称为由格运算"
            $join "和" $meet (Em "导出") "的偏序.)")
        (Li (&join $x $y) "和" (&meet $x $y)
            "分别是" $x "和" $y "相对于这个偏序"
            $<= "的join和meet. {译注: 当然了, join和meet"
            "的另外说法分别是最小上界和最大下界.}")))
   ((exercise #:n "6.3")
    "证明以上三条陈述成立.")
   ((answer)
    (Ol (Li "由" (&= (&join $x $y) $y) "推出"
            (&= (&meet $x $y) $x) ": 将"
            (&= (&join $x $y) $y) "带入(4b)可得"
            (&= (&meet $x $y) $x) ";" (Br)
            "由" (&= (&meet $x $y) $x) "推出"
            (&= (&join $x $y) $y)
            ": 根据(4a)和替换可得"
            (&= (&join $y (@meet $y $x)) $y)
            ", 根据(2b)可知"
            (&= (&join $y (@meet $x $y)) $y)
            ", 带入" (&= (&meet $x $y) $x)
            "得到" (&= (&join $y $x) $y)
            ", 然后再应用(2a)就有"
            (&= (&join $x $y) $y) ".")
        (Li ""
            )
        )
    )
   ((definition #:n "6.3")
    "(分配格) 一个格" (tupa0 $A $join:id $meet:id) "是"
    (Em "分配的") ", 如果等式"
    (distributeR $x &meet $y &join $z)
    " (即分配律) 对于每个" (∈ $x $y $z $A) "成立.")
   ((remark #:n "6.4")
    "以下三条陈述成立."
    (Ol (Li "每个全序集都是分配格.")
        (Li "在每个格中对于任意的" (&cm $x $y $z) "不等式"
            (&>= (&meet $x (@join $y $z))
                 (&join (@meet $x $y) (@meet $x $z)))
            "都成立.")
        (Li "定义6.3中的分配律可以替换为其" (Em "对偶")
            "形式: "
            )
        )
    )
   ((definition #:n "6.4")
    "(完备格) 一个格" (tupa0 $A $join:id $meet:id)
    "是" (Em "完备的") ", 如果对于" $A "的任意(可能为空的)子集"
    $S ", 其最小上界 (记作" (Join $S) ") 和最大下界 (记作"
    (Meet $S) ") 均存在. 这里的" $A "的一个元素" $a
    "是一个集合" $S "的" (Em "最小上界") ", 如果"
    (Ul (Li "对于每个" (∈ $x $S) ", " (&<= $x $a)
            ", 即" $a "是" $S "的一个上界;")
        (Li "对于每个" (∈ $c $A) ", 如果对于每个" (∈ $x $S)
            "有" (&<= $x $c) ", 那么" (&<= $a $c)
            ", 即" (&<= $a $c) "对于" $S "的任意上界"
            $c "都成立."))
    (Em "最大下界") "可以对偶地定义. 因此, " $S
    "的最大下界是一个大于等于" $S "的任意下界的"
    $S "的下界.")
   (P "根据定义, " (Join $empty) "和" (Meet $empty)
      "分别是" $A "的最小元素" $bottom "和最大元素"
      $top ". 因此, 完备格必然是有界的.")
   ((example #:n "6.5")
    
    )
   ((exercise #:n "6.6")
    
    )
   ((example #:n "6.6")
    
    )
   (H4 "布尔代数")
   (P "让我们回忆一下古典逻辑的逻辑联结词" $disj
      ", " $conj ", " $impl "的真值表, 其中"
      $0 "和" $1 "分别代表" (Em "falsehood")
      "和" (Em "truth") "."
      (MB (&split16
           (set-attr*
            (&Table
             ((&\\ $a $b) $1 $0)
             ($1          $1 $1)
             ($0          $1 $0))
            'rowlines "solid" 'columnlines "solid")
           (set-attr*
            (&Table
             ((&\\ $a $b) $1 $0)
             ($1          $1 $0)
             ($0          $0 $0))
            'rowlines "solid" 'columnlines "solid")
           (set-attr*
            (&Table
             ((&\\ $a $b) $1 $0)
             ($1          $1 $0)
             ($0          $1 $1))
            'rowlines "solid" 'columnlines "solid")))
      "现在我们在集合" (setE $0 $1) "上定义一个自然的偏序, 即"
      (&< $0 $1) ". 那么, 左边和中间的真值表是说析取"
      (&disj $a $b) "与合取" (&conj $a $b)
      "分别是相对于该偏序的join和meet. "
      "当然, 我们注意到这种情况下join和meet也可以表达为"
      (&= (&join $a $b) (&max (setE $a $b))) "和"
      (&= (&meet $a $b) (&min (setE $a $b))) ".")
   (P "运用代数的术语, 我们可以说"
      (tupa0 (setE $0 $1) $join:id $meet:id $->:id $0)
      "是一个代数, 其中" (tupa0 (setE $0 $1) $join:id $meet:id $0)
      "是一个带有最小元素" $0 "的格, 并且这个代数还有一个额外的运算"
      $-> ", 其满足如果" (&<= $a $b) "则" (&= (&-> $a $b) $1)
      ", 不然的话" (&= (&-> $a $b) $0) ". 藉由最小元素" $0
      ", 我们定义" (&= (&neg $a) (&-> $a $0))
      ". 显然, " (&= (&neg $a) $1) "当且仅当" (&= $a $0)
      ". 并且, 很容易看出来对于" (∈ $a (setE $0 $1))
      "我们有" (&= (&neg (&neg $a)) $a)
      ". 通过泛化, 我们按照以下方式引入了古典逻辑的代数.")
   ((definition #:n "6.5")
    "(布尔代数) 一个代数"
    (&= $A:bold (tupa0 $A $join:id $meet:id $->:id $0))
    "是一个" (Em "布尔代数") "当且仅当"
    (Ol (Li (tupa0 $A $join:id $meet:id)
            "是一个格且其最小元素是" $0 ";")
        (Li "剩余律成立, 即对于所有的" (∈ $a $b $c $A)
            ", " (&<= (&meet $a $b) $c) "当且仅当"
            (&<= $a (&-> $b $c)) ";")
        (Li "双重否定律成立, 即对于每个" (∈ $a $A)
            ", " (&= (&neg (&neg $a)) $a)
            ", 其中" (&neg $a) "被定义为" (&-> $a $0) ".")))
   (P "由以上真值表所确定的代数"
      (tupa0 (setE $0 $1) $join:id $meet:id $->:id $0)
      "是一个布尔代数. 为了确认这点, 只需表明剩余律成立. "
      "实际上, 我们可以看到" (&<= $1 (&-> $b $c))
      "当且仅当" (&<= $b $c) "当且仅当" (&<= (&meet $1 $b) $c)
      ", 也就是当" (&= $a $1) "时的剩余律成立, 另外"
      (&= $a $0) "时" (&<= $0 (&-> $b $c)) "和"
      (&<= (&meet $0 $b) $c) "总是都成立的. 这个由"
      $0 "和" $1 "构成的布尔代数被称为" (Em "二值的")
      ", 记作" BA2 ".")
   (P "剩余律是说对于每个" (&cm $b $c) "而言集合"
      (&= $U (setI $x (&<= (&meet $x $b) $c)))
      "中的最大元素总是存在且就等于" (&-> $b $c)
      ". 实际上, 根据剩余律, 对于每个" $x "而言, 如果"
      (&<= (&meet $x $b) $c) ", 那么"
      (&<= $x (&-> $b $c)) ". 这意味着" (&-> $b $c)
      "是" $U "的一个上界. 另一方面, 鉴于"
      (&<= (&-> $b $c) (&-> $b $c))
      ", 我们有" (&<= (&meet (@-> $b $c) $b) $c)
      ", 通过使用剩余律的另一方向. 这意味着"
      (∈ (&-> $b $c) $U) ", 故"
      (&= (&-> $b $c)
          (&max (setI (∈ $x $A)
                      (&<= (&meet $x $b) $c))))
      ". 现在考虑" (&= $b $c) "这一特定情形. 鉴于"
      (&<= (&meet $x $b) $b) "总是成立, 具有形式"
      (&-> $b $b) "必然都是" $A "的最大元素, 记作"
      $1 ". 这就推出了每个布尔代数既有最小元素" $0
      "也有最大元素" $1 ". 布尔代数的一个病态例子是满足"
      (&= $0 $1) "的布尔代数. 这种布尔代数由单独一个元素"
      $0 "构成, 其被称为" (Em "退化") "布尔代数. "
      "之后的文本里我们提及布尔代数时, 除非另有说明, "
      "不然我们总指的是一个" (Em "非退化")
      "布尔代数. 以下的图片展示一个" $8 "-值布尔代数.")
   (P "我们可以证明以下的引理6.2对于布尔代数成立, "
      "但不需要使用双重否定律. 因此, 实际上这些结果对于"
      (Em "Heyting代数") "也成立, 而Heyting代数的定义"
      "不过就是从布尔代数的定义里去掉双重否定律. "
      "Heyting代数将会在第7章详细讨论.")
   ((lemma #:n "6.2")
    "下列陈述对于任何布尔代数之中的所有" (&cm $x $y $z) "成立."
    (Ol (Li (&<= (&meet $x (@-> $x $y)) $y)
            "总是成立, 因而" (&= (&meet $x (&neg $x)) $0)
            "作为其一特殊情形而成立.")
        (Li (&<= $x $y) "可以推出" (&<= (&-> $z $x) (&-> $z $y))
            "和" (&<= (&-> $y $z) (&-> $x $z)) ". 因此, "
            (&<= $x $y) "可以推出" (&<= (&neg $y) (&neg $x)) ".")
        (Li "分配律" (distributeR $x &meet $y &join $z) "成立.")))
   ((proof)
    "这里我们只会给出对于第三条陈述的证明. 根据评注6.4, 在任意的格中"
    (&meet $x (@join $y $z)) "都是" (&meet $x $y) "和" (&meet $x $z)
    "的一个上界. 因此, 只需说明" (&meet $x (@join $y $z))
    "是这些上界之中最小的那个. 设" $u "是" (&meet $x $y) "和"
    (&meet $x $z) "的任意一个上界, 那么" (&<= (&meet $x $y) $u) "且"
    (&<= (&meet $x $z) $u) ". 根据剩余律, " (&<= $y (&-> $x $u))
    "和" (&<= $z (&-> $x $u)) "成立. 因此, "
    (&<= (&join $y $z) (&-> $x $u))
    ". 再次以相反方向使用剩余律, 我们有"
    (&<= (&meet $x (@join $y $z)) $u)
    ". 于是, 我们就得到了想要的结果.")
   ((tcomment)
    "补充一下对于前两条陈述的证明."
    (Ol (Li (&<= (&meet $x (@-> $x $y)) $y)
            "根据剩余律等价于"
            (&<= (&-> $x $y) (&-> $x $y))
            ", 而这不过就是自反性.")
        (Li "根据1, "
            (&<= (&meet $z (@-> $z $x)) $x)
            ". 又因为" (&<= $x $y) "和传递性, "
            (&<= (&meet $z (@-> $z $x)) $y)
            ", 于是根据剩余律可知"
            (&<= (&-> $z $x) (&-> $z $y))
            ". 根据1, "
            (&<= (&meet $y (@-> $y $z)) $z)
            ", 又因为" (&<= $x $y) "和练习6.1, "
            (&<= (&meet $x (@-> $y $z))
                 (&meet $y (@-> $y $z)))
            ", 于是"
            (&<= (&meet $x (@-> $y $z)) $z)
            ". 应用剩余律, 就得到"
            (&<= (&-> $y $z) (&-> $x $z)) ".")))
   ((lemma #:n "6.3")
    "下列陈述在每个布尔代数之中都成立."
    (Ol (Li "对于所有的" (&cm $x $y) ", "
            (&= (&join $x $y)
                (&neg (@meet (&neg $x) (&neg $y))))
            "成立.")
        (Li "对于所有的" (&cm $x $y) ", "
            (&= (&-> $x $y) (&join (&neg $x) $y))
            "成立. 特别地, 对于每个" $x ", "
            (&= (&join $x (&neg $x)) $1) ".")))
   ((proof)
    "我们来证明第二条陈述. 很容易看出来" (&-> $x $y)
    "是" (&neg $x) "和" $y "的一个上界. 设" $w
    "是" (&neg $x) "和" $y "的任何一个上界. 根据"
    (&<= (&neg $x) $w) ", 可以推出"
    (&<= (&neg $w) (&= (&neg (&neg $x)) $x))
    ", 因而不等式"
    (&<= (&-> $x $y)
         (&-> (&neg $w) $y)
         (&-> (&neg $w) $w))
    "成立, 这些是根据引理6.2的第二条得到的. "
    "另一方面, 鉴于"
    (&<= (&meet (&neg $w) (@-> (&neg $w) $w))
         (&= (&meet (&neg $w) $w) $0))
    ", 我们有"
    (&<= (&-> (&neg $w) $w)
         (&= (&-> (&neg $w) $0)
             (&neg (&neg $w))
             $w))
    ". 将两个不等式结合在一起, 我们就得到了"
    (&<= (&-> $x $y) $w)
    ". 换言之, " (&-> $x $y)
    "是" (&neg $x) "和" $y
    "的最小上界, 故"
    (&= (&-> $x $y) (&join (&neg $x) $y)) ".")
   ((tcomment)
    "这个证明中的"
    (&<= (&meet (&neg $w) (@-> (&neg $w) $w))
         (&= (&meet (&neg $w) $w) $0))
    "卡住了译者好一会儿. 不过, 实际上没那么困难. "
    "根据引理6.1的第一条, "
    (&<= (&meet (&neg $w) (@-> (&neg $w) $w)) $w)
    ". 另外, " (&meet (&neg $w) (@-> (&neg $w) $w))
    "根据" $meet "的定义本来就有"
    (&<= (&meet (&neg $w) (@-> (&neg $w) $w)) (&neg $w))
    ". 于是, " (&meet (&neg $w) (@-> (&neg $w) $w))
    "是" (&neg $w) "和" $w "的一个下界, 那么"
    (&<= (&meet (&neg $w) (@-> (&neg $w) $w))
         (&meet (&neg $w) $w)) ".")
   ((tcomment)
    "让我们来证明引理6.3的第一条陈述. "
    "首先我们想要证明" (&neg (@meet (&neg $x) (&neg $y)))
    "是" $x "和" $y "的一个上界. 如何证明"
    (&<= $x (&neg (@meet (&neg $x) (&neg $y))))
    "呢? 根据" $neg "的定义我们将其改写为等价的"
    (&<= $x (&-> (@meet (&neg $x) (&neg $y)) $0))
    ". 根据剩余律, 其等价于"
    (&<= (&meet $x (@meet (&neg $x) (&neg $y))) $0)
    ". 根据引理6.2的第一条陈述里的" (&= (&meet $x (&neg $x)) $0)
    ", 这个不等式的左边就等于" $0 ", 因而总是成立. "
    (&<= $y (&neg (@meet (&neg $x) (&neg $y))))
    "可以按照类似的方式进行证明." (Br)
    "现在设" $u "是" $x "和" $y "的任意一个上界, 我们想要证明"
    (&<= (&neg (@meet (&neg $x) (&neg $y))) $u)
    ". 根据引理6.2的第二条陈述和双重否定律, 这等价于证明"
    (&<= (&neg $u) (&meet (&neg $x) (&neg $y)))
    ". 这个实际上可由" $u "为" $x "和" $y
    "的上界这一事实推得. 因为" (&<= $x $u)
    ", 所以" (&<= (&neg $u) (&neg $x))
    ", 同理" (&<= (&neg $u) (&neg $y))
    ". 换言之, " (&neg $u) "是" (&neg $x)
    "和" (&neg $y) "的一个下界, 故"
    (&<= (&neg $u) (&meet (&neg $x) (&neg $y)))
    ". Q.E.D.")
   (P "我们注意到引理6.3中出现的等式"
      (&= (&join $x (&neg $x)) $1)
      "表达了" (Em "排中律")
      " (见第1.1节) 的某种代数类比. "
      "另外, 以上的引理6.3是说" $join "和" $->
      "可以基于" $meet "和" $neg
      "定义, 也就是说" $join "和" $->
      "在任何布尔代数之中都是冗余的. "
      "另一方面, 为了使得布尔代数和"
      "之后章节将要引入的其他代数之间的比较更加清晰, "
      "我们保留了将布尔代数定义为具有形式"
      (tupa0 $A $join:id $meet:id $->:id $0)
      "的五元组.")
   ((exercise #:n "6.7")
    "表明以下陈述在每个布尔代数之中都成立."
    (Ol (Li "对于任意的" (&cm $x $y) ", "
            (&= (&join (@-> $x $y) (@-> $y $x)) $1) ".")
        (Li "对于任意的" (&cm $x $y) ", "
            (&<= (&-> (@-> $x $y) $x) $x) ".")))
   ((answer)
    (Ol (Li (MB (deriv
                 (&join (@-> $x $y) (@-> $y $x))
                 (&join (@join (&neg $x) $y)
                        (@join (&neg $y) $x))
                 (&join (@join $x (&neg $x))
                        (@join $y (&neg $y)))
                 (&join $1 $1)
                 $1)))
        (Li (MB (deriv
                 (&-> (@-> $x $y) $x)
                 (&-> (@join (&neg $x) $y) $x)
                 (&join (&neg (@join (&neg $x) $y)) $x)
                 (&join (@meet $x (&neg $y)) $x)
                 $x))
            "换言之, 在布尔代数之中实际上可以将" $<=
            "加强为" $= ".")))
   (H3. "子代数, 同态和直积")
   (P "为了进一步发展我们的代数研究, "
      "我们要引入三个基本的代数概念. "
      "鉴于这些概念可以对于多种代数进行定义, "
      "我们要在一般的上下文中进行定义. "
      "代数的一个" (Em "语言") $L:script
      "是一个" (Em "运算符号") "的集合, "
      "每个运算符号都有一个固定的" (Em "元数")
      ", 元数是一个非负整数. 具有元数" $0
      "的运算符号被称为" (Em "常量符号")
      ". 以下我们假定" $L:script "是一个有限有序集"
      (tupa0 $f_1 $f_2 $..h $f_m) ", 遵循约定"
      (&>= $n_1 $n_2 $..c $n_m) ", 其中" $n_i
      "是" $f_i "的元数. 一个类型为" $L:script
      "的代数" $A:bold "是一个具有形式"
      (tu0 $A (^ $f_1 $A:bold)
           (^ $f_2 $A:bold) $..h
           (^ $f_m $A:bold))
      "的结构, 其中" $A "是一个非空集合, 被称为"
      $A:bold "的" (Em "宇宙") "或者" (Em "潜在集合")
      ", 而" (^ $f_i $A:bold) "是" $A
      "上的一个" $n_i "元运算, 其中" (&<= $1 $i $m)
      ". 每个" (^ $f_i $A:bold) "都应该理解为"
      $L:script "的运算符号" $f_i "在" $A:bold
      "中的解释. 出于简洁性的考量, 有时我们会省略"
      (^ $f_i $A:bold) "的角标" (^ $ $A:bold)
      ", 如果不至于引起误解.")
   (P "举个例子, 前一节我们取了一个语言"
      (tupa0 $join:id $meet:id $->:id $0)
      "来描述所有布尔代数的类, 其元数分别为"
      (&cm $2 $2 $2 $0) ". 以下的三个定义里, "
      "我们将假定代数" $A:bold "和" $B:bold
      "具有相同的类型.")
   ((definition #:n "6.6")
    "(子代数) 一个代数" $B:bold "是" $A:bold
    "的一个" (Em "子代数") ", 如果" $B
    "是" $A "的一个子集, 而对于每个" $i
    "而言" (^ $f_i $B:bold) "是"
    (^ $f_i $A:bold) "在" $B
    "上的限制. 也就是说, 对于所有的" $i "和"
    (∈ $b_1 $..h (_ $b $n_i) $B) "而言, "
    (let ((f (λ (sig)
               (appl (^ $f_i sig)
                     $b_1 $..h (_ $b $n_i)))))
      (&= (f $B:bold) (f $A:bold))) ".")
   ((example #:n "6.7")
    "设" (&= $A:bold (tupa0 $A $join:id $meet:id))
    "为一个格, 而" $B "是" $A "的一个非空子集. 那么, "
    (&= $B:bold (tupa0 $B (&prime $join:id) (&prime $meet:id)))
    "是" $A:bold "的一个子代数 (一般称为格" $A:bold
    "的一个" (Em "子格") ") 当且仅当对于任意的"
    (∈ $b_1 $b_2 $B) ", " (&= (&join^ $b_1 $b_2) (&join $b_1 $b_2))
    "且" (&= (&meet^ $b_1 $b_2) (&meet $b_1 $b_2))
    ", 其等价于对于任意的" (∈ $b_1 $b_2 $B) ", "
    (&join $b_1 $b_2) "和" (&meet $b_1 $b_2)
    "都属于" $B ". 让我们考虑练习6.4"
    )
   ((definition #:n "6.7")
    "(同态和同态像) 一个映射" (func $h $A $B) "是一个从"
    $A:bold "到" $B:bold "的" (Em "同态") ", 如果"
    (MB (&= (app $h (appl (^ $f_i $A:bold)
                          $a_1 $..h (_ $a $n_i)))
            (appl (^ $f_i $B:bold)
                  (app $h $a_1) $..h (app $h (_ $a $n_i)))))
    "对于所有的" $i "和" (∈ $a_1 $..h (_ $a $n_i) $A)
    "成立. 当" $h "是一个单射时, " $h "被称为是一个(从"
    $A:bold "到" $B:bold "的)" (Em "嵌入(embedding)")
    ". 在这种情形下, " $A:bold "被称为是由" $h
    (Em "嵌入") "到" $B:bold "之中. 如果" $h
    "是满射的, 即每个元素" (∈ $b $B) "都可以表达为对于某个"
    (∈ $a $A) "的" (app $h $a) ", 那么" $B:bold
    "被称为是" $A:bold "的一个" (Em "同态像")
    " (由同态" $h "). 当" $h "是双射的, " $h
    "被称为是一个" (Em "同构") "而" $A:bold
    "被称为是" (Em "同构") "于" $B:bold
    " (由同构" $h ").")
   ((remark #:n "6.8")
    "设" $h "是从一个格" $A:bold "到一个格" $B:bold
    "的一个同态, 那么" $h "是" (Em "保序的") ", 即对于所有的"
    (∈ $a $a^ $A) ", " (&<=^A $a $a^) "可以推出"
    (&<=^B (app $h $a) (app $h $a^)) ", 其中" $<=^A "和"
    $<=^B "分别是" $A:bold "和" $B:bold
    "由其格运算导出的序关系. 这是因为如果" (&<=^A $a $a^)
    "成立, 那么" (&= (&meet^A $a $a^) $a) ", 因而有"
    (&= (&meet^B (app $h $a) (app $h $a^))
        (app $h (&meet^A $a $a^))
        (app $h $a))
    ". 这意味着"
    (&<=^B (app $h $a) (app $h $a^)) ".")
   ((exercise #:n "6.8")
    
    )
   ((definition #:n "6.8")
    "(直积) 对于给定的代数" $A:bold "和" $B:bold ", 定义" (Em "直积")
    (&c* $A:bold $B:bold) "为一个代数"
    (tupa0 (&c* $A $B) (^ $f_1 (&c* $A:bold $B:bold))
           $..h (^ $f_m (&c* $A:bold $B:bold)))
    ", 其中"
    (&= (&c* $A $B)
        (setI (tu0 $a $b)
              (: (∈ $a $A) "且" (∈ $b $B))))
    ", 并且对于每个" $i ", 令"
    (MB (&= (appl (^ $f_i (&c* $A:bold $B:bold))
                  (tu0 $a_1 $b_1) $..h
                  (tu0 (_ $a $n_i) (_ $b $n_i)))
            (tu0 (appl (^ $f_i $A:bold)
                       $a_1 $..h (_ $a $n_i))
                 (appl (^ $f_i $B:bold)
                       $b_1 $..h (_ $b $n_i)))))
    "也就是说, " (^ $f_i (&c* $A:bold $B:bold))
    "是逐分量定义的. 更一般地, 代数" (_ $A:bold $j)
    " (" (∈ $j $J) ") (" $J
    "可能无限) 的直积被定义为这样一个代数" $A:bold
    " (记作" (prod (∈ $j $J) (_ $A:bold $j))
    "), 其宇宙" $A "是集合" $A_j " (" (∈ $j $J)
    ") 的直积" (prod (∈ $j $J) $A_j) ", 而"
    (appl (^ $f_i $A:bold) $a_1 $..h (_ $a $n_i))
    "的第" $j "分量"
    (app (appl (^ $f_i $A:bold) $a_1 $..h (_ $a $n_i)) $j)
    "是由"
    (appl (^ $f_i (_ $A:bold $j))
          (app $a_1 $j) $..h
          (app (_ $a $n_i) $j))
    "给出的, 其中" (∈ $j $J) "而" (∈ $a_k $A)
    ". 这里的" (app $a_k $j) "是" $A_j
    "的一个元素, 其为" $a_k "的第" $j "分量.")
   ((example #:n "6.9")
    
    )
   ((exercise #:n "6.9")
    "令" $f "是从" $A:bold "到" $C:bold "的格同态, "
    $g "是从" $B:bold "到" $D:bold "的格同态. 定义从"
    (&c* $A:bold $B:bold) "到" (&c* $C:bold $D:bold)
    "的映射" $h "为"
    (&= (appl $h $a $b)
        (tu0 (app $f $a) (app $g $b)))
    ". 证明" $h "是一个格同态.")
   ((answer)
    
    )
   (P "在由所有布尔代数构成的类这一情形下, " $B:bold
      "是一个布尔代数" $A:bold "的一个子代数当且仅当"
      $B "是" $A "的一个子集, 其包含" (^ $0 $A:bold)
      "且在" (&cm $join:id $meet:id $->:id)
      "下封闭. 显然每个非退化的布尔代数都有一个"
      "同构于二值布尔代数" BA2 "的子代数. 当"
      $A:bold "和" $B:bold "都是布尔代数时, 一个映射"
      (func $h $A $B) "是一个从" $A:bold "到" $B:bold
      "的同态当且仅当对于每个"
      (∈ $*:id (setE $join:id $meet:id $->:id))
      "有"
      (&= (app $h (&*^A $a $a^))
          (&*^B (app $h $a) (app $h $a^)))
      ", 而且"
      (&= (app $h (^ $0 $A:bold)) (^ $0 $B:bold)) ".")
   (P "对于任意的布尔代数" $A:bold ", " $A:bold
      "的每个子代数也是一个布尔代数. 另外, 如果"
      $B:bold "是" $A:bold "藉由某个同态" $h
      "的一个同态像, 那么" $B:bold
      "也是一个布尔代数. 为了证明后者, 我们验证"
      $B:bold "满足布尔代数的三个条件. "
      "对于第三个条件, 取任意元素" (∈ $a^ $B)
      ". 然后, 对于某个元素" (∈ $a $A) "而言"
      $a^ "具有形式" (app $h $a) ". 那么, "
      (&= (&neg2 $a^) (&neg2 (app $h $a))
          (app $h (&neg2 $a))
          (app $h $a) $a^)
      ", 鉴于" $A:bold "是一个布尔代数. "
      "至于剩余律, 对于给定的" (∈ $a^ $b^ $c^ $B)
      ", 取" (∈ $a $b $c $A) "使得"
      (&= $a^ (app $h $a)) ", "
      (&= $b^ (app $h $b)) ", "
      (&= $c^ (app $h $c)) ", 因为"
      $B:bold "是" $A:bold "的一个同态像. 首先设"
      (&<= (&meet $a^ $b^) $c^) ", 这等价于条件"
      (&<= (app $h (&meet $a $b)) (app $h $c))
      ". 既然" (&<= $b (&-> $a (@meet $a $b)))
      "在" $A:bold "之中成立, 不等式"
      (&<= (app $h $b)
           (&-> (app $h $a) (app $h (&meet $a $b)))
           (&-> (app $h $a) (app $h $c)))
      "成立, 鉴于" $h "是保序的. 因此, "
      (&<= $b^ (&-> $a^ $c^))
      ". 反过来, 设" (&<= $b^ (&-> $a^ $c^))
      ". 然后, " (&<= (app $h $b) (app $h (&-> $a $c)))
      ". 那么, 我们有"
      (&<= (&meet (app $h $a) (app $h $b))
           (&meet (app $h $a) (app $h (&-> $a $c)))
           (app $h $c))
      ", 鉴于" (&<= (&meet $a (@-> $a $c)) $c)
      "在" $A:bold "中成立. 因此, "
      (&<= (&meet $a^ $b^) $c^) "成立.")
   ((tcomment)
    "这个证明里有一个gap, 也就是没有说明布尔代数的第一个条件成立. "
    "换言之, 我们要证明带最小元" $0 "的格结构"
    (tupa0 $A $join:id $meet:id $0)
    "保持同态像. 之所以这个是重要的, "
    "是因为证明中的序关系及其性质依赖于这个事实. "
    "然而, 从泛代数的角度来看, 这是平凡的, "
    "因为带最小元的格结构只需要通过等式进行定义, "
    "而不是像布尔代数的第二个条件那样还依赖于等式之间的关系. "
    "(所以说, 对于布尔代数的第三个条件的验证也是平凡的.) "
    "其中比较有趣的是最小元" $0 "也有一个等式刻画, 即其为运算"
    $join "的单位元. 另外说一句, 之所以格同态" $h
    "保持序关系, 是因为这个序关系也是用等式定义的, "
    "而泛代数的同态皆保持等式.")
   (P "如果" $B:bold "是布尔代数" (_ $A:bold $j) " ("
      (∈ $j $J) ") 的一个直积" (prod (∈ $j $J) (_ $A:bold $j))
      ", 那么" $B:bold "也是一个布尔代数, 这是因为"
      $B:bold "的每个运算都是逐分量定义的. "
      "{译注: 并且, 布尔代数的定义也只是用到等式和等式之间的关系.} "
      "为了总结, 我们按照以下方式陈述这些结果. "
      "这个主题将会在第8.2节里以更泛化的方式进行讨论.")
   ((theorem #:n "6.4")
    "由所有布尔代数构成的类在子代数, 同态像, 直积下封闭.")
   (H3. "布尔代数的表示")
   (P "在例子6.5和练习6.6中, 我们表明了任意一个集合" $C
      "的幂集" (powerset $C) "相对于并" $union "和交"
      $cap "构成了一个分配格"
      (tupa0 (powerset $C) $union $cap)
      ", 其中" $empty "是最小元. 对于" $C
      "的每个子集" $X ", 令" (&- $X) "是" $X
      "的补. 对于" (&sube (&cm $X $Y) $C) ", 定义"
      (&= (&-> $X $Y) (&union (&- $X) $Y)) ". 那么, "
      (&= (powerset $C:bold)
          (tupa0 (powerset $C) $union $cap $->:id $empty))
      "形成了一个布尔代数. 具有这种形式的布尔代数被称为"
      (Em "幂集布尔代数") ".")
   ((exercise #:n "6.10")
    )
   ((exercise #:n "6.11")
    )
   (H4 "有限布尔代数")
   (P "令" $C "是任意一个具有" $m "个元素的有限集合, 例如"
      (setI $a_k (&<= $1 $k $m))
      ". {译注: 这默认了当" (&!= $i $j) "时, "
      (&!= $a_i $a_j) ".} 那么, 幂集布尔代数"
      (powerset $C:bold) "是一个" $2^m
      "-值布尔代数. 如果" $D "是另一个具有" $m
      "个元素的集合, 那么显然" (powerset $D:script)
      "同构于" (powerset $C:bold)
      ". 我们注意到"
      )
   (H4 "无限布尔代数")
   (H3. "古典逻辑的代数完备性")
   (P "第1.1节所讨论的二值语义的每个赋值都确定了一种对于公式的解释. "
      "赋值和有效性的概念可以自然地扩展至任意的布尔代数. 令"
      $A:bold "是一个布尔代数. 一个" $A:bold "上的"
      (Em "赋值") $h "是从所有命题变量的集合到" $A "的任意映射. 赋值"
      $h "可以自然地延拓成一个从所有公式的集合到集合"
      $A "的映射, 通过定义"
      (&= (app $h (&disj $alpha $beta))
          (&join_A (app $h $alpha) (app $h $beta)))
      ", "
      (&= (app $h (&conj $alpha $beta))
          (&meet_A (app $h $alpha) (app $h $beta)))
      ", "
      (&= (app $h (&impl $alpha $beta))
          (&->_A (app $h $alpha) (app $h $beta)))
      ", 以及" (&= (app $h $0) $0_A)
      ". (藉由符号滥用, 对于这个延拓的映射我们也使用符号"
      $h ". 以上的符号" $disj ", " $conj ", " $impl
      ", " $0 "代表逻辑联结词和常量, 而" $join_A ", "
      $meet_A ", " $->_A ", " $0_A "则代表相应的"
      $A:bold "的代数运算以及" $A "的最小元素, 以避免歧义.) "
      "一个公式" $phi "在某个布尔代数" $A:bold "中是"
      (Em "有效的") ", 如果其总是取值" $1_A ", 即对于每个"
      $A:bold "上的赋值" $h "总有" (&= (app $h $phi) $1_A)
      ". 公式有效性的概念也可以自然地扩展至相继式的有效性. "
      "显然, 第1章所讨论的重言式不过就是在二值布尔代数"
      BA2 "中有效的公式和相继式.")
   ((theorem #:n "6.6")
    "(代数完备性) 对于任意给定的非退化布尔代数" $B:bold
    ", 以下三个条件互相等价. 对于每个公式" $phi ","
    (Ol (Li $phi "在古典逻辑中是可证明的;")
        (Li $phi "在所有布尔代数之中都是有效的;")
        (Li $phi "在布尔代数" $B:bold "中是有效的.")))
   ((proof)
    
    )
   (H3. "多值链和剩余律")
   (P "从代数角度而言, 询问如何将我们的二值语义 "
      "(即基于二值布尔代数的代数语义) 推广为多值语义是自然的. "
      "本节我们将会考虑将二值语义扩展至多值语义的两种可能方式, "
      "其将会取决于我们维持何种形式的剩余律. "
      "我们将会引入两种类型的" (Em "多值链")
      ", 即定义在全序集上的带有剩余律的代数. "
      "它们分别是Gödel链和Łukasiewicz链. "
      "之前章节引入的代数方法也可应用于这两种代数, "
      "而两类不同的多值逻辑的基本结果也将呈现. "
      "之后的章节我们将会进一步将这种想法推广至定义在格上的剩余代数, "
      "并最终将我们引向Heyting代数和剩余格的概念.")
   (P "作为对于二值语义的一种推广, "
      "我们考虑如何为和古典逻辑相同的语言引入多值语义. "
      "我们将我们这里的注意力限制于真值集合" $A
      "为全序集的情况 (但可能无限), 即一个链, "
      "并且带有最小元素" $0 "和最大元素" $1
      ". 例如, 如果是三值语义, 我们可以取" $A "为"
      (setE $0 (&/ $1 $2) $1)
      ". 在这种情形下, " (&/ $1 $2)
      "会被视为" (Em "halfway truth")
      ", 如果我们将真值理解为"
      (Em "degree of truth")
      ". 因为我们假定了" $A "为全序集, 那么"
      (&join $a $b) "和" (&meet $a $b) "分别可以表达为"
      (&max (setE $a $b)) "和" (&min (setE $a $b))
      ". 因此, 主要的问题在于如何在这个集合上定义implication "
      $-> ". 一种想法不过就是"
      (Em "保持合取与implication之间的剩余律")
      ". {译注: 虽然这里的用辞是conjunction和implication, "
      "但是实际上它们指的是代数运算而不是逻辑联结词, "
      "只不过一般其会使用相同的符号罢了.}")
   (P "首先设" $-> "满足剩余律, 那么" $->
      "的定义必然要满足对于给定的" (∈ $a $b $A)
      ", 对于所有的" (∈ $d $A) "都有"
      (&<= (&meet $d $a) $b) "当且仅当"
      (&<= $d (&-> $a $b)) ". 这等价于说对于所有的"
      (∈ $d $A) "都有" (&<= (&min (setE $d $a)) $b)
      "当且仅当" (&<= $d (&-> $a $b))
      ". "
      )
   (H2. "代数逻辑的基本")
   (H3. "Heyting代数")
   ((definition #:n "7.1")
    "(Heyting代数) 一个代数"
    (&= $A:bold (tupa0 $A $join:id $meet:id $->:id $0))
    "是一个" (Em "Heyting代数") "当且仅当"
    (Ol (Li (tupa0 $A $join:id $meet:id)
            "是一个具有最小元" $0 "的格.")
        (Li "剩余律成立, 即对于所有的" (∈ $a $b $c $A)
            ", " (&<= (&meet $a $b) $c) "当且仅当"
            (&<= $a (&-> $b $c)) ".")))
   (P "每个Heyting代数也(一定)有最大元素" $1
      ", 并且对于每个元素" $a "都有" $1 "等于"
      (&-> $a $a) ". 另外, " (&neg $a)
      "被定义为" (&-> $a $0)
      ". 显然, 每个布尔代数都是一个Heyting代数. "
      
      )
   (H3. "Lindenbaum-Tarski代数")
   (H3. "局部有限代数")
   (H3. "有限可嵌入性质和有限模型性质")
   (H3. "Heyting代数的canonical扩张")
   (H2. "Logics and Varieties")
   (H3. "超直觉主义逻辑的格结构")
   (H3. "由所有Heyting代数构成的variety HA")
   (H2. "剩余结构 (Residuated Structures)")
   (P "本章我们将会给出对于" (Em "剩余结构")
      "的简短引论, 其是亚结构逻辑的代数结构. "
      "布尔代数和Heyting代数被定义为是带有二元运算"
      $-> "的格结构, 其满足" $meet "和" $->
      "之间的剩余律, 即" (&<= (&meet $a $b) $c)
      "当且仅当" (&<= $a (&-> $b $c))
      "对于任意的" (&cm $a $b $c)
      "成立. 从另一方面来说, "
      "第6章引入的Łukasiewicz推出并不总是满足此律. "
      "但是, 其仍然满足融合运算" $d* "和" $->
      "之间的剩余律.")
   (P "本章我们将会讨论一般形式的剩余律成立的代数结构, "
      "并且将注意力主要集中于" (Em "剩余格")
      ". 这个概念是从代数角度理解亚结构逻辑的关键所在. "
      
      )
   (H3. "剩余格和FL-代数")
   (H2. "模态代数")
   (P "对于模态逻辑的语义研究已经藉由使用Kripke语义而发展得相当成功. "
      
      )
   (H3. "模态代数")
   ))