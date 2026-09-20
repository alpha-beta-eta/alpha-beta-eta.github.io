#lang racket
(provide type_theory_and_category_theory.html)
(require SMathML "utils.rkt")
(define (Href url . html*)
  (keyword-apply A '(#:attr*) `(((href ,url))) html*))
(define (nLab page . html*)
  (apply Href (string-append "https://ncatlab.org/nlab/show/" page) html*))
(define (Bib id . html*)
  (apply Href (string-append "#" id) html*))
(define (Item id . html*)
  (keyword-apply Li '(#:attr*) `(((id ,id))) html*))
(define-syntax-rule (HTable (h ...) (x ...) ...)
  (Table
   #:attr* '((width "100%")
             (border "1")
             (style "border-collapse: collapse;"))
   (Thead (Tr (Th h) ...))
   (Tbody (Tr (Td x) ...) ...)))
(define-simple*
  (&Cont $Cont "Cont")
  (&Lang $Lang "Lang")
  (&Con $Con "Con"))
(define $FirstOrderTheories (Mi "FirstOrderTheories"))
(define $Hyperdoctrines (Mi "Hyperdoctrines"))
(define $MLDependentTypeTheories (Mi "MLDependentTypeTheories"))
(define $LocallyCartesianClosedCategories (Mi "LocallyCartesianClosedCategories"))
(define $type (Mi "type"))
(define $ast (Mo "&lowast;"))
(define $simeq (Mo "&simeq;"))
(define-infix*
  (&simeq $simeq))
(define $inf1 (tu0 $inf $1))
(define (stack2 top bottom)
  (Mfrac #:attr* '((linethickness "0")) top bottom))
(define (&equivalence A L R B)
  (: A (stack2 (^^ $<- L) (__ $-> R)) B))
(define (&biequivalence A L R B)
  (: A (Munderover $simeq (__ $--> R) (^^ $<-- L)) B))
(define (dep B x) (: B (bra0 x)))
(define (ihom A B) (bra0 (&cm A B)))
(define (slice C A) (_ C (: $/ A)))
(define (fiber f a) (app (^ f $-1) a))
(define (Pullback P A B C f g)
  (&Table
   (P $-> A)
   ($darr $ (^ $darr f))
   (B (^^ $-> g) C)))
(define (Equalizer P A f g B)
  (: P $-> A (stack2 (^^ $-> f) (__ $-> g)) B))
(define (Tower . X*)
  (apply Mtable
         (map (lambda (X) (Mtr (Mtd X)))
              (add-between X* $darr))))
(define type_theory_and_category_theory.html
  (TnTmPrelude
   #:title "类型论和范畴论之间的关系"
   #:css "styles.css"
   (H1. "类型论和范畴论之间的关系")
   (P "nLab词条"
      (Q (nLab "relationship+between+type+theory+and+category+theory"
               "relationship between type theory and category theory"))
      "的翻译, 原页面最后修订于2025年10月4日. "
      "{译注: 原页面侧栏的导航目录没有翻译.}")
   (H2. "想法")
   (P (nLab "type+theory" "类型论") "和某些种类的" (nLab "category+theory" "范畴论")
      "是紧密相关的. 通过一种" (nLab "syntax-semantics+duality" "句法-语义对偶")
      ", 我们可以将类型论视为范畴论的一种形式" (nLab "syntax" "句法")
      "语言, 或者说" (Em "演算") "; 反过来, 我们可以认为范畴论为类型论提供了"
      (nLab "semantics" "语义") ". 所使用的范畴论的种类依赖于类型论的种类. 例如, "
      (nLab "homotopy+type+theory" "同伦类型论") "和某些种类的"
      (nLab "%28%E2%88%9E%2C1%29-category+theory" $inf1 "-范畴论") "就具有这种关系.")
   (H2. "概览")
   (HTable
    ("内逻辑/类型论 (句法)" "等价于" "语境范畴 (语义)" "")
    ((nLab "propositional+logic" "命题逻辑") ""
     (nLab "Lindenbaum-Tarski+algebra" "Lindenbaum-Tarski代数") "")
    ((Span (nLab "intuitionistic+logic" "直觉主义") (nLab "propositional+logic" "命题逻辑")) ""
     (nLab "Heyting+algebra" "Heyting代数") "")
    ((Span (nLab "classical+logic" "经典") (nLab "propositional+logic" "命题逻辑")) ""
     (nLab "Boolean+algebra" "Boole代数") "")
    ((Span (nLab "dual+intuitionistic+logic" "对偶直觉主义") (nLab "propositional+logic" "命题逻辑")) ""
     (nLab "co-Heyting+algebra" "余Heyting代数") "")
    ((nLab "first-order+logic" "一阶逻辑") ""
     (nLab "hyperdoctrine" "hyperdoctrine") (Bib "SeelyA" "Seely 1984a"))
    ((nLab "regular+logic" "正则逻辑") ""
     (Span (nLab "regular+hyperdoctrine" "正则hyperdoctrine") "/"
           (nLab "regular+category" "正则范畴")) "")
    ((nLab "coherent+logic" "连贯逻辑") ""
     (Span (nLab "coherent+hyperdoctrine" "连贯hyperdoctrine") "/"
           (nLab "coherent+category" "连贯范畴")) "")
    ((Span (nLab "intuitionistic+logic" "直觉主义") (nLab "predicate+logic" "谓词逻辑")) ""
     (Span (nLab "first-order+hyperdoctrine" "一阶hyperdoctrine") "/"
           (nLab "Heyting+category" "Heyting范畴")) "")
    ((Span (nLab "classical+logic" "经典") (nLab "predicate+logic" "谓词逻辑")) ""
     (Span (nLab "Boolean+hyperdoctrine" "Boole hyperdoctrine") "/"
           (nLab "Boolean+category" "Boole范畴")) "")
    ((nLab "modal+logic" "模态逻辑") "" (nLab "modal+hyperdoctrine" "模态hyperdoctrine") "")
    ((nLab "linear+logic" "线性逻辑") "" (nLab "linear+hyperdoctrine" "线性hyperdoctrine") "")
    ((nLab "simply-typed+lambda+calculus" "简单类型lambda演算") ""
     (nLab "cartesian+closed+category" "笛卡尔闭范畴")
     (Bib "LambekScott86" "Lambek &amp; Scott 1986, 第I部分"))
    ((Span "外延" (nLab "dependent+type+theory" "依赖类型论")) ""
     (nLab "locally+cartesian+closed+category" "局部笛卡尔闭范畴")
     (Bib "Seely84b" "Seely 1984b"))
    ((nLab "bunched+logic" "束逻辑 (bunched logic)") ""
     (nLab "doubly+closed+monoidal+category" "双重闭幺半范畴")
     (nLab "bunched+logic#O'HearnPym99" "O&rsquo;Hearn &amp; Pym 1999"))
    ((Span "没有" (nLab "univalence" "泛等") "的" (nLab "homotopy+type+theory" "同伦类型论")
           " (内涵M-L依赖类型论)") ""
     (nLab "locally+cartesian+closed+%28%E2%88%9E%2C1%29-category" "局部笛卡尔闭" $inf1 "-范畴")
     (Span (nLab "locally+cartesian+closed+%28infinity%2C1%29-category#Presentations"
                 "Cisinski 2012")
           " &amp; " (Bib "Shulman12" "Shulman 2012")))
    ((Span "带有" (nLab "higher+inductive+types" "高阶归纳类型") "和" (nLab "univalence" "泛等")
           "的" (nLab "homotopy+type+theory" "同伦类型论")) ""
     (nLab "elementary+%28%E2%88%9E%2C1%29-topos" "初等" $inf1 "-topos")
     (Span "见"
           (Href "https://ncatlab.org/homotopytypetheory/show/model+of+type+theory+in+an+%28infinity%2C1%29-topos"
                 "这里")))
    ((nLab "multiplicative+intuitionistic+linear+logic" "乘性直觉主义线性逻辑") ""
     (Span (nLab "symmetric+monoidal+category" "对称")
           (nLab "closed+monoidal+category" "闭幺半范畴"))
     (nLab "linear%20type%20theory#HistoryCategoricalSemantics" "自约68年以来的诸多作者"))
    ((nLab "classical+linear+logic" "经典线性逻辑") ""
     (nLab "star-autonomous+category" "星自治范畴 (star-autonomous category)")
     (Bib "Seely89" "Seely 1989"))
    ((nLab "dependent+linear+type+theory" "依赖线性类型论") ""
     (Span (nLab "indexed+monoidal+category" "索引幺半范畴") " (带有概括 (comprehension))")
     (Bib "Riley22Thesis" "Riley 2022")))
   (P (B (nLab "computational+trinitarianism" "计算三位一体")) " = " (Br)
      (B (nLab "propositions+as+types" "命题即类型")) " + "
      (B (nLab "programs+as+proofs" "程序即证明")) " + "
      (B (nLab "relation+between+type+theory+and+category+theory" "类型论/范畴论关系")))
   (HTable
    ((nLab "logic" "逻辑")
     (Span (nLab "set+theory" "集合论") "(的" (nLab "internal+logic+of+set+theory" "内逻辑") ")")
     (nLab "category+theory" "范畴论")
     (nLab "type+theory" "类型论"))
    ((nLab "proposition" "命题") (nLab "set" "集合") (nLab "object" "对象") (nLab "type" "类型"))
    ((nLab "predicate" "谓词") (nLab "family+of+sets" "集合族")
     (nLab "display+morphism" "显示态射") (nLab "dependent+type" "依赖类型"))
    ((nLab "proof" "证明") (nLab "element" "元素") (nLab "generalized+element" "广义元素")
     (Span (nLab "term" "项") "/" (nLab "program" "程序")))
    ((nLab "cut+rule" "切规则") ""
     (Span (nLab "classifying+morphisms" "分类态射") "的" (nLab "composition" "复合") "/"
           (nLab "display+maps" "显示映射") "的" (nLab "pullback" "拉回"))
     (nLab "substitution" "代换"))
    ((Span (nLab "implication" "推出") "的" (nLab "introduction+rule" "引入规则")) ""
     (Span "hom-张量伴随的" (nLab "counit" "余单位")) "lambda")
    ((Span (nLab "implication" "推出") "的" (nLab "elimination+rule" "消去规则")) ""
     (Span "hom-张量伴随的" (nLab "unit" "单位")) "应用")
    ((Span (nLab "implication" "推出") "的" (nLab "cut+elimination" "切消去")) ""
     (Span "hom-张量伴随的" (nLab "zigzag+identities" "之字形恒等式") "之一")
     (nLab "beta+reduction" "&beta;归约"))
    ((Span (nLab "implication" "推出") "的恒等消去") ""
     (Span "hom-张量伴随的另一个" (nLab "zigzag+identity" "之字形恒等式"))
     (nLab "eta+conversion" "&eta;转换"))
    ((nLab "true" "真") (nLab "singleton" "单元集")
     (Span (nLab "terminal+object" "终对象") "/"
           (nLab "%28-2%29-truncated+object" "(-2)-截断对象"))
     (Span (nLab "h-level+0" "h-层次0") "-" (nLab "type" "类型") "/"
           (nLab "unit+type" "单位类型")))
    ((nLab "false" "假") (nLab "empty+set" "空集") (nLab "initial+object" "始对象")
     (nLab "empty+type" "空类型"))
    ((Span (nLab "proposition" "命题") ", " (nLab "truth+value" "真值"))
     (nLab "subsingleton" "子单元集")
     (Span (nLab "subterminal+object" "子终对象") "/"
           (nLab "%28-1%29-truncated+object" "(-1)-截断对象"))
     (Span (nLab "h-proposition" "h-命题") ", " (nLab "mere+proposition" "纯命题")))
    ((nLab "logical+conjunction" "逻辑合取") (nLab "cartesian+product" "笛卡尔积")
     (nLab "product" "积") (nLab "product+type" "积类型"))
    ((nLab "disjunction" "析取")
     (Span (nLab "disjoint+union" "不交并") "(的" (nLab "support" "支撑") ")")
     (Span (nLab "coproduct" "余积") "(的" (nLab "%28-1%29-truncation" "(-1)-截断") ")")
     (Span (nLab "sum+type" "和类型") "(的" (nLab "bracket+type" "括号类型") ")"))
    ((nLab "implication" "推出")
     (Span "(到" (nLab "subsingleton" "子单元集") "中的)" (nLab "function+set" "函数集"))
     (Span "(到" (nLab "subterminal+object" "子终对象") "中的)" (nLab "internal+hom" "内hom"))
     (Span "(到" (nLab "h-proposition" "h-命题") "中的)" (nLab "function+type" "函数类型")))
    ((nLab "negation" "否定")
     (Span "到" (nLab "empty+set" "空集") "中的" (nLab "function+set" "函数集"))
     (Span "到" (nLab "initial+object" "始对象") "中的" (nLab "internal+hom" "内hom"))
     (Span "到" (nLab "empty+type" "空类型") "中的" (nLab "function+type" "函数类型")))
    ((nLab "universal+quantification" "全称量化")
     (Span "(" (nLab "subsingletons" "子单元集") "族的)索引" (nLab "cartesian+product" "笛卡尔积"))
     (Span "(" (nLab "subterminal+objects" "子终对象") "族的)" (nLab "dependent+product" "依赖积"))
     (Span "(" (nLab "h-propositions" "h-命题") "族的)" (nLab "dependent+product+type" "依赖积类型")))
    ((nLab "existential+quantification" "存在量化")
     (Span "索引" (nLab "disjoint+union" "不交并") "(的" (nLab "support" "支撑") ")")
     (Span (nLab "dependent+sum" "依赖和") "(的" (nLab "%28-1%29-truncation" "(-1)-截断") ")")
     (Span (nLab "dependent+sum+type" "依赖和类型") "(的" (nLab "bracket+type" "括号类型") ")"))
    ((nLab "logical+equivalence" "逻辑等价") (nLab "bijection+set" "双射集")
     (nLab "object+of+isomorphisms" "同构对象") (nLab "equivalence+type" "等价类型"))
    ("" (nLab "support+set" "支撑集")
     (Span (nLab "support+object" "支撑对象") "/" (nLab "%28-1%29-truncation" "(-1)-截断"))
     (Span (nLab "propositional+truncation" "命题截断") "/" (nLab "bracket+type" "括号类型")))
    ("" ""
     (Span "到" (nLab "terminal+object" "终对象") "中的" (nLab "morphism" "态射") "的"
           (nLab "n-image" "n-像") "/" (nLab "n-truncation" "n-截断"))
     (nLab "n-truncation+modality" "n-截断模态"))
    ((nLab "propositional+equality" "命题等式")
     (Span (nLab "diagonal+function" "对角函数") "/" (nLab "diagonal+subset" "对角子集") "/"
           (nLab "diagonal+relation" "对角关系"))
     (nLab "path+space+object" "道路空间对象")
     (Span (nLab "identity+type" "恒等类型") "/" (nLab "path+type" "道路类型")))
    ((nLab "completely+presented+set" "完全表示的集合") (nLab "set" "集合")
     (Span (nLab "discrete+object" "离散对象") "/" (nLab "0-truncated+object" "0-截断对象"))
     (Span (nLab "h-level+2" "h-层次2") "-" (nLab "type" "类型") "/" (nLab "set" "集合") "/"
           (nLab "h-set" "h-集合")))
    ((nLab "set" "集合")
     (Span "带有" (nLab "equivalence+relation" "等价关系") "的" (nLab "set" "集合"))
     (nLab "groupoid+object+in+an+%28infinity%2C1%29-category" "内0-广群")
     (Span "其" (nLab "pseudo-equivalence+relation" "伪等价关系") "是真正的"
           (nLab "equivalence+relation" "等价关系") "的" (nLab "Bishop+set" "Bishop集合") "/"
           (nLab "setoid" "setoid")))
    (""
     (Span (nLab "equivalence+class" "等价类") "/" (nLab "quotient+set" "商集"))
     (nLab "quotient" "商") (nLab "quotient+type" "商类型"))
    ((nLab "induction" "归纳") "" (nLab "colimit" "余极限")
     (Span (nLab "inductive+type" "归纳类型") ", " (nLab "W-type" "W-类型") ", "
           (nLab "M-type" "M-类型")))
    ((Span "高阶" (nLab "induction" "归纳")) ""
     (nLab "%28infinity%2C1%29-colimit" "高阶余极限")
     (nLab "higher+inductive+type" "高阶归纳类型"))
    ("-" ""
     (Span (nLab "0-truncated" "0-截断") (nLab "%28infinity%2C1%29-colimit" "高阶余极限"))
     (nLab "quotient+inductive+type" "商归纳类型"))
    ((nLab "coinduction" "余归纳") "" (nLab "limit" "极限") (nLab "coinductive+type" "余归纳类型"))
    ("" (nLab "preset" "预集合") ""
     (Span "没有" (nLab "identity+types" "恒等类型") "的" (nLab "type" "类型")))
    ("" (Span (nLab "truth+values" "真值") "的" (nLab "set" "集合"))
     (nLab "subobject+classifier" "子对象分类子") (nLab "type+of+propositions" "命题的类型"))
    ((nLab "domain+of+discourse" "论域") (nLab "universe" "宇宙")
     (nLab "object+classifier" "对象分类子") (nLab "type+universe" "类型宇宙"))
    ((nLab "modality" "模态") ""
     (Span (nLab "closure+operator" "闭包算子") ", (" (nLab "idempotent+monad" "幂等") ")"
           (nLab "monad" "单子"))
     (Span (nLab "modal+type+theory" "模态类型论") ", "
           (nLab "monad+%28in+computer+science%29" "单子 (计算机科学中的)")))
    ((nLab "linear+logic" "线性逻辑") ""
     (Span "(" (nLab "symmetric+monoidal+category" "对称") ", "
           (nLab "closed+monoidal+category" "闭") ")" (nLab "monoidal+category" "幺半范畴"))
     (Span (nLab "linear+type+theory" "线性类型论") "/" (nLab "quantum+computation" "量子计算")))
    ((nLab "proof+net" "证明网") "" (nLab "string+diagram" "弦图")
     (nLab "quantum+circuit" "量子线路"))
    ((Span "(缺少)" (nLab "contraction+rule" "收缩规则")) ""
     (Span "(缺少)" (nLab "diagonal" "对角")) (nLab "no-cloning+theorem" "不可克隆定理"))
    ("" "" (nLab "synthetic+mathematics" "综合数学")
     (nLab "domain+specific+embedded+programming+language" "领域特定的嵌入式编程语言")))
   (H2. "定理")
   (P "我们在此讨论各种风味的类型论和相应风味的范畴之间的关系/等价的形式化和证明."
      (Ul (Li (Href "#FirstOrderLogic" "一阶逻辑和hyperdoctrine"))
          (Li (Href "#DependentTypeTheory" "依赖类型论和局部笛卡尔闭范畴"))
          (Li (Href "#HomotopyTypeTheory" "同伦类型论和局部笛卡尔闭" $inf1 "-范畴"))
          (Li (Href "#HomotopyWithUnivalence" "泛等同伦类型论和初等" $inf1 "-topos"))))
   (H3. #:id "FirstOrderLogic" "一阶逻辑和hyperdoctrine")
   ((Theorem)
    (nLab "functors" "函子")
    (Ul (Li $Cont ", 其构成一个" (nLab "first-order+logic" "一阶理论") "的"
            (nLab "category+of+contexts" "语境范畴") ";")
        (Li $Lang ", 其构成一个" (nLab "hyperdoctrine" "hyperdoctrine") "的"
            (nLab "internal+language" "内语言")))
    "构成一个" (nLab "equivalence+of+categories" "范畴等价")
    (MB (&equivalence $FirstOrderTheories $Lang $Cont $Hyperdoctrines) "."))
   (P "(" (Bib "SeelyA" "Seely, 1984a") ")")
   (H3. #:id "DependentTypeTheory" "依赖类型论和局部笛卡尔闭范畴")
   (P "我们在此讨论" (nLab "dependent+type+theory" "依赖类型论") "如何是"
      (nLab "locally+cartesian+closed+categories" "局部笛卡尔闭范畴") "为其提供"
      (nLab "semantics" "语义") "的句法. 关于这一点 (以及所涉及的微妙的"
      (nLab "coherence" "融贯性") " (coherence) 问题) 的专门讨论, 另见"
      (Em (nLab "categorical+model+of+dependent+types"
                "依赖类型的范畴模型 (categorical model of dependent types)")) ".")
   ((Theorem #:id "SeelyEquivalence")
    "存在" (nLab "2-functors" "2-函子")
    (Ul (Li $Cont ", 其构成一个"
            (nLab "Martin-L%C3%B6f+dependent+type+theory" "Martin-Löf依赖类型论") "的"
            (nLab "category+of+contexts" "语境范畴") ";")
        (Li $Lang ", 其构成一个"
            (nLab "locally+cartesian+closed+category" "局部笛卡尔闭范畴") "的"
            (nLab "internal+language" "内语言")))
    "其构成一个" (nLab "equivalence+of+2-categories" "2-范畴等价")
    (MB (&biequivalence $MLDependentTypeTheories $Lang $Cont
                        $LocallyCartesianClosedCategories) "."))
   (P "这原本是作为一个" (nLab "equivalence+of+categories" "范畴等价") "而被声称的 "
      (Bib "Seely84b" "Seely 1974, 定理6.3")
      ". {译注: 应为Seely 1984.} 然而, 该论证没有恰当地处理一个对于整个主题而言是核心的微妙之处: "
      (nLab "terms" "项") "对于" (nLab "variables" "变量") "的" (nLab "substitution" "代换")
      "是严格地复合的, 而其通过" (nLab "pullback" "拉回") "给出的"
      (nLab "categorical+semantics" "范畴语义") ", 按照拉回的"
      (nLab "universal+construction" "本性") ", 只是在" (nLab "isomorphism" "同构")
      "的意义下定义的. 这个问题由" (Bib "Curien" "Curien") "和"
      (Bib "Hofmann" "Hofmann") "指出并给出了修复的方法; 关于后者见"
      (Em (nLab "categorical+model+of+dependent+types" "依赖类型的范畴模型"))
      ". 然而, 完整的范畴等价直到"
      (Bib "ClairambaultDybjer" "Clairambault &amp; Dybjer")
      "通过将陈述提升为一个" (nLab "equivalence+of+2-categories" "2-范畴等价")
      "解决了这两个问题才得以恢复, 另见"
      (Bib "CurienGarnerHofmann" "Curien, Garner &amp; Hofmann")
      ". 另一种也适用于" (nLab "intensional+identity+types" "内涵恒等类型")
      "并因而适用于" (nLab "homotopy+type+theory" "同伦类型论") "的处理方式见于"
      (Bib "LumsdaineWarren13" "Lumsdaine &amp; Warren 2013") ".")
   (P "我们现在指出一些细节.")
   (H4. #:id "TypeTheories" "类型论")
   (P "为了明确性, 自足性以及为了下面的引用, 我们按照 ("
      (Bib "Seely84b" "Seely, 定义1.1") ") 说明一个"
      (nLab "dependent+type+theory" "依赖类型论") "是什么.")
   ((Definition)
    "一个" (B "Martin-Löf" (nLab "dependent+type+theory" "依赖类型论")) $T
    "是一个带有某个由取值于类型和项中的依赖函数符号构成的"
    (nLab "signature+%28in+logic%29" "签名") " (...) 的"
    (Em (nLab "theory" "理论")) ", 服从以下规则"
    (Ol (Li (B "类型形成规则")
            (Ol (Li $1 "是一个类型 (" (nLab "unit+type" "单位类型") ");")
                (Li "如果" (&cm $a $b) "是类型" $A "的项, 那么"
                    (@= $a $b) "是一个类型 (" (nLab "equality+type" "等式类型") ");")
                (Li "如果" $A "和" (dep $B $x) "是类型, " $B
                    "依赖于一个类型为" $A "的" (nLab "free+variable" "自由变量")
                    ", 那么以下符号是类型"
                    (Ol (Li (prod (&: $a $A) (dep $B $a))
                            " (" (nLab "dependent+product" "依赖积") "), 如果"
                            (dep $B $x) "实际上不依赖于" $x ", 也写成" (@-> $A $B) ";")
                        (Li (sum (&: $a $A) (dep $B $a))
                            " (" (nLab "dependent+sum" "依赖和") "), 如果"
                            (dep $B $x) "实际上不依赖于" $x ", 也写成" (&c* $A $B) ";")))))
        (Li (B "项形成规则")
            (Ol (Li (&in $ast $1) "是" (nLab "unit+type" "单位类型") "的一个项;")
                (Li "(...)")))
        (Li (B "等式规则")
            (Ol (Li "(...)")))))
   (H4. #:id "category_of_contexts" "语境范畴")
   ((Definition)
    "给定一个" (nLab "dependent+type+theory" "依赖类型论") $T ", 其"
    (B (nLab "category+of+contexts" "语境范畴")) (&Con $T) "是这样的范畴, 其"
    (Ul (Li (nLab "objects" "对象") "是" $T "的" (nLab "types" "类型") ";")
        (Li (nLab "morphisms" "态射") (Arrow $f $A $B) "是"
            (nLab "function+type" "函数类型") (&-> $A $B) "的" (nLab "terms" "项") $f "."))
    "复合以显然的方式给出.")
   ((Proposition)
    (&Con $T) "具有" (nLab "finite+limits" "有限极限") "并且是一个"
    (nLab "cartesian+closed+category" "笛卡尔闭范畴") ".")
   (P "(" (Bib "Seely84b" "Seely, 命题3.1") ")")
   ((proof)
    "构造是直接的. 我们指出了其中一些."
    (P "注意到一旦存在所有" (nLab "pullbacks" "拉回") "和" (nLab "equalizers" "等化子")
       ", 所有" (nLab "finite+limits" "有限极限") " (如那里所讨论的) 就都被诱导出来了. "
       (&Con $T) "中的一个" (nLab "pullback" "拉回")
       (MB (Pullback $P $A $B $C $f $g))
       "由"
       (MB (&simeq $P (sum (&: $a $A)
                           (sum (&in $b $B)
                                (@= (app $f $a) (app $g $b))))) ".")
       "给出. " (nLab "equalizer" "等化子")
       (MB (Equalizer $P $A $f $g $B))
       "由"
       (MB (&= $P (sum (&: $a $A) (@= (app $f $a) (app $g $a)))) ".")
       "给出. 接着, " (nLab "internal+hom" "内hom") "/"
       (nLab "exponential+object" "指数对象") "由"
       (nLab "function+type" "函数类型")
       (MB (&simeq (ihom $A $B) (@-> $A $B)) ".")
       "给出."))
   ((Proposition)
    (&Con $T) "是一个" (nLab "locally+cartesian+closed+category" "局部笛卡尔闭范畴") ".")
   (P "(" (Bib "Seely84b" "Seely, 定理3.2") ")")
   ((proof)
    "定义" (&Con $T) "-" (nLab "indexed+category" "索引的")
    (nLab "hyperdoctrine" "hyperdoctrine") (app $P $T) ", 其方式是对于"
    (&in $A (&Con $T)) ", 取范畴" (app (app $P $T) $A) "以" $A "-"
    (nLab "dependent+types" "依赖类型") "为对象, 并以依赖函数类型的项"
    (par0 (!- (&: $a $A) (&: $t (@-> (app $X $a) (app $Y $a)))))
    "为态射"
    (&-> (par0 (!- (&: $a $A) (&: (app $X $a) $type)))
         (par0 (!- (&: $a $A) (&: (app $Y $a) $type)))) "."
    (P "这是笛卡尔闭的, 论证与前一个证明中的同类. 现在只需展示一个与"
       (nLab "slice+category" "切片范畴") (slice (&Con $T) $A) "之间的相容的"
       (nLab "equivalence+of+categories" "范畴等价") "."
       (MB (&simeq (slice (&Con $T) $A) (app (app $P $T) $A)) ".")
       "在一个方向上, 将一个态射" (Arrow $f $X $A) "送至依赖类型"
       (MB (!- (&: $a $A)
               (&:= (fiber $f $a) (sum (&: $x $X) (@= $a (app $f $x)))))
           ".")
       "反过来, 对于一个依赖类型" (!- (&: $a $A) (app $X $a)) ", 将其送至投影"
       (&-> (sum (&: $a $A) (app $X $a)) $A) ".")
    (P "可以表明这确实给出了一个与基变换相容的范畴等价 ("
       (Bib "Seely84b" "Seely, 命题3.2.4") ")."))
   ((Definition)
    "对于依赖类型论" $T "和局部笛卡尔闭范畴" $C ", " $T "在" $C "中的一个"
    (B (nLab "categorical+semantics" "解释")) "是局部笛卡尔闭范畴之间的一个态射"
    (MB (&-> (&Con $T) $C) ".")
    $T "在另一个依赖类型论" $T^ "中的一个解释是局部笛卡尔闭范畴之间的一个态射"
    (MB (&-> (&Con $T) (&Con $T^)) "."))
   (H4. #:id "internal_language" "内语言")
   ((Proposition)
    "给定一个" (nLab "locally+cartesian+closed+category" "局部笛卡尔闭范畴") $C
    ", 如下定义相应的" (nLab "dependent+type+theory" "依赖类型论") (&Lang $C)
    (Ul (Li (&Lang $C) "的非依赖类型是" $C "的" (nLab "objects" "对象") ";")
        (Li $A "-依赖类型是态射" (&-> $B $A) ";")
        (Li "一个语境" (&cm (&: $x_1 $X_1) (&: $x_2 $X_2) $..c (&: $x_n $X_n))
            "是一个态射的塔"
            (MB (Tower $X_n $..c $X_2 $X_1)))
        (Li "项" (&: (dep $t (_ $x $A)) (dep $B (_ $x $A)))
            "是" (slice $C $A) "中的" (nLab "sections" "截面") (&-> $A $B))
        (Li (nLab "equality+type" "等式类型") (@= (_ $x $A) (_ $y $A)) "是"
            (nLab "diagonal" "对角") (&-> $A (&c* $A $A)))
        (Li "...")))
   (H3. #:id "HomotopyTypeTheory" "同伦类型论和局部笛卡尔闭" $inf1 "-范畴")
   (P "以上所有内容在" (nLab "%28%E2%88%9E%2C1%29-category+theory" $inf1 "-范畴论") "和"
      (nLab "homotopy+type+theory" "同伦类型论") "中都有一个类比.")
   ((Proposition)
    "每个" (nLab "presentable+%28%E2%88%9E%2C1%29-category" "可表现")
    "且" (nLab "locally+cartesian+closed+%28%E2%88%9E%2C1%29-category" "局部笛卡尔闭的" $inf1 "-范畴")
    "都有一个由" (nLab "type-theoretic+model+category" "类型论模型范畴")
    "给出的表现. 这为" (nLab "homotopy+type+theory" "同伦类型论") "提供了"
    (nLab "categorical+semantics" "范畴语义") " (可能不带"
    (nLab "univalence" "泛等") (nLab "axiom" "公理") ")."
    (P "这特别地包括所有(" (nLab "%E2%88%9E-stack" $inf "-栈") ")"
       (nLab "%28%E2%88%9E%2C1%29-toposes" $inf1 "-topos") " (其还应该满足"
       (nLab "univalence" "泛等") "). 另见"
       (Em (nLab "internal+logic+of+an+%28%E2%88%9E%2C1%29-topos" $inf1 "-topos的内逻辑")) "."))
   (P "这一陈述的某种形式最初是在 (" (Bib "Joyal11" "Joyal 11")
      ") 中被形式化地猜想的, 继 (" (Bib "Awodey10" "Awodey 10")
      ") 之后. 更多细节见"
      (Em (nLab "locally+cartesian+closed+%28infinity%2C1%29-category#InternalLogic"
                "局部笛卡尔闭" $inf1 "-范畴")) ".")
   (H3. #:id "HomotopyWithUnivalence" "泛等同伦类型论和初等" $inf1 "-topos")
   (Blockquote
    (P "更多细节见"
       (Em (nLab "model+of+type+theory+in+an+%28infinity%2C1%29-topos"
                 $inf1 "-topos中的类型论模型")) "."))
   (P "一个额外具有一个" (nLab "object+classifiers" "对象分类子") "系统的("
      (nLab "locally+presentable+%28%E2%88%9E%2C1%29-category" "局部可表现的") ")"
      (nLab "locally+Cartesian+closed+%28%E2%88%9E%2C1%29-category" "局部笛卡尔闭" $inf1 "-范畴")
      " (如" (Bib "HomotopyTypeTheory" "上") ") 是一个("
      (nLab "%28%E2%88%9E%2C1%29-category+of+%28%E2%88%9E%2C1%29-sheaves" $inf1 "-层") ")"
      (nLab "%28%E2%88%9E%2C1%29-topos" $inf1 "-topos") ".")
   (P "(" (Bib "Awodey10" "Awodey 10") ") 中猜想, 这个"
      (nLab "object+classifier" "对象分类子") "是一个" (nLab "univalence" "泛等")
      (nLab "type+universe" "类型宇宙") " (" (nLab "type+of+types" "类型的类型")
      ") 的范畴语义, 因而带有" (nLab "univalence" "泛等") "的"
      (nLab "homotopy+type+theory" "同伦类型论") "在"
      (nLab "%28%E2%88%9E%2C1%29-toposes" $inf1 "-topos")
      "中具有范畴语义. 这一陈述对于典范的" $inf1 "-topos"
      (nLab "%E2%88%9EGrpd" $inf "Grpd") "在 ("
      (Bib "KapulkinLumsdaineVoevodsky12" "Kapulkin-Lumsdaine-Voevodsky 12")
      ") 中得到了证明, 更一般地, 对于"
      (nLab "elegant+Reedy+categories" "优雅Reedy范畴") "上的"
      (nLab "%28%E2%88%9E%2C1%29-presheaf" $inf1 "-预层") $inf1
      "-topos在 (" (Bib "Shulman13" "Shulman 13") ") 中得到了证明. 一般情形在"
      (Bib "Shulman19" "Shulman 19") "中得到证明.")
   (P "在这些证明中, 解释同伦类型论句法的"
      (nLab "type-theoretic+model+categories" "类型论模型范畴")
      "被要求提供在拉回下严格地表现的类型宇宙. "
      "这与类型论中通常的句法上方便的宇宙 (要么是Russell式的, 要么是Tarski式的) 相匹配, "
      "但在范畴语义中更难实现. 更灵活地, 我们可以考虑句法上的"
      (nLab "type+of+types#TarskiStyle" "弱Tarski式类型宇宙") " ("
      (Bib "Luo12" "Luo 12") ", " (Bib "Gallozzi14" "Gallozzi 14")
      "). 这些在句法上用起来更复杂, 但应该在任何"
      (nLab "%28%E2%88%9E%2C1%29-topos" $inf1 "-topos") " (的表现它的"
      (nLab "type-theoretic+model+categories" "类型论模型范畴")
      ") 中都有解释. 在这一一般的灵活意义下对" (nLab "univalence" "泛等") "的讨论见于 ("
      (Bib "GepnerKock12" "Gepner-Kock 12") "). 关于一般的句法问题见"
      (Ul (Li (Href "https://ncatlab.org/homotopytypetheory/show/model+of+type+theory+in+an+%28infinity%2C1%29-topos"
                    "model of type theory in an (infinity,1)-topos"))))
   (P "虽然" (nLab "%28%E2%88%9E%2C1%29-sheaf" $inf1 "-层")
      (nLab "%28%E2%88%9E%2C1%29-toposes" $inf1 "-topos") "是目前被理解的那些, "
      "带有泛等宇宙的基本类型论并不看到或者在意它们本身的"
      (nLab "locally+presentable+%28%E2%88%9E%2C1%29-category" "局部可表现性")
      " (尽管它在其他地方被用到, 例如" (nLab "higher+inductive+types" "高阶归纳类型")
      "的构造). 可以预期存在一个像样的"
      (nLab "elementary+%28%E2%88%9E%2C1%29-topos" "初等" $inf1 "-topos") "概念, 使得带有"
      (nLab "univalence" "泛等") (nLab "type+universes" "类型宇宙") "和一定供给的"
      (nLab "higher+inductive+types" "高阶归纳类型") "的"
      (nLab "homotopy+type+theory" "同伦类型论") "恰好在"
      (nLab "elementary+%28%E2%88%9E%2C1%29-toposes" "初等" $inf1 "-topos")
      "中具有范畴语义 (如" (Bib "Awodey10" "Awodey 10") "中所猜想的). "
      "但是这一陈述的精细调整目前仍在研究之中.")
   (P "注意到这一陈述, 一旦实现, 就使得 (或者将会使得) 泛等HoTT+HIT成为"
      (nLab "William+Lawvere" "William Lawvere") "所提出的"
      (nLab "topos+theory" "topos理论") "中的"
      (nLab "foundations+of+mathematics" "数学基础") "的一种"
      (nLab "homotopy+theory" "同伦论") "精化. 它可以与他的"
      (nLab "elementary+theory+of+the+category+of+sets" "集合范畴的初等理论")
      "相比较, 尽管由于是一个类型论而非一阶逻辑中的理论, "
      "它更类似于一个初等topos的内类型论.")
   (H2. "相关概念")
   (Ul (Li (nLab "initiality+conjecture" "初始性猜想 (initiality conjecture)"))
       (Li (nLab "categorical+model+of+dependent+types"
                 "依赖类型的范畴模型 (categorical model of dependent types)"))
       (Li (nLab "syntax-semantics+duality" "句法-语义对偶 (syntax-semantics duality)"))
       (Li (nLab "computational+trinitarianism" "计算三位一体 (computational trinitarianism)"))
       (Li (nLab "Awodey%27s+conjecture" "Awodey猜想 (Awodey's conjecture)")))
   (H2. "参考文献")
   (P "以" (nLab "Haskell" "Haskell") (nLab "programming+language" "编程语言")
      "给出的一个初等阐述见于"
      (Ul (Li "WikiBooks, "
              (I (Href "https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism"
                       "Haskell/The Curry&ndash;Howard isomorphism"))))
      (nLab "first+order+logic" "一阶理论") "和" (nLab "hyperdoctrines" "hyperdoctrine")
      "之间的" (nLab "equivalence+of+categories" "范畴等价") "讨论于"
      (Ul (Item "SeelyA"
                (nLab "R.+A.+G.+Seely" "R. A. G. Seely") ", "
                (I "Hyperdoctrines, natural deduction, and the Beck condition")
                ", Zeitschrift für Math. Logik und Grundlagen der Math. (1984) ("
                (Href "http://www.math.mcgill.ca/rags/ZML/ZML.PDF" "pdf") ")"))
      (nLab "categorical+model+of+dependent+types" "依赖类型的范畴模型") "和初始性讨论于"
      (Ul (Li "Simon Castellan, " (I "Dependent type theory as the initial category with families")
              ", 2014 ("
              (Href "http://iso.mor.phis.me/archives/2011-2012/stage-2012-goteburg/report.pdf" "pdf")
              ")"))
      "其在带有" (nLab "higher+inductive+types" "高阶归纳类型")
      "的集合商的类型论内部被形式化于:"
      (Ul (Item "AltKap2015"
                (nLab "Thorsten+Altenkirch" "Thorsten Altenkirch") ", Ambrus Kaposi, "
                (I "Type Theory in Type Theory using Quotient Inductive Types") ", (2015) ("
                (Href "http://www.cs.nott.ac.uk/~txa/publ/tt-in-tt.pdf" "pdf") "), ("
                (Href "https://bitbucket.org/akaposi/tt-in-tt" "formalisation in Agda") ")."))
      "综述包括"
      (Ul (Li (nLab "Tom+Hirschowitz" "Tom Hirschowitz") ", "
              (I "Introduction to categorical logic") " (2010) ("
              (Href "http://www.lama.univ-savoie.fr/~hirschowitz/talks/cours.pdf" "pdf") ")"
              (P "(见导向"
                 (Href "http://www.lama.univ-savoie.fr/~hirschowitz/talks/cours.pdf#page=96"
                       "第96张幻灯片")
                 "上的定理的讨论)"))
          (Li "Roy Crole, " (I "Deriving category theory from type theory")
              ", Theory and Formal Methods 1993 Workshops in Computing 1993, pp 15-26")
          (Item "Maietti05"
                (nLab "Maria+Maietti" "Maria Maietti") ", "
                (I "Modular correspondence between dependent type theories and categories including pretopoi and topoi")
                ", Mathematical Structures in Computer Science archive Volume 15 Issue 6, "
                "December 2005 Pages 1089 - 1149 ("
                (Href "https://www.math.unipd.it/~maietti/papers/tumscs.pdf" "pdf") ")"))
      (nLab "linear+logic" "线性逻辑") "在"
      (nLab "star-autonomous+categories" "星自治范畴") "中的"
      (nLab "categorical+semantics" "范畴语义") "归功于"
      (Ul (Item "Seely89"
                (nLab "R.+A.+G.+Seely" "R. A. G. Seely") ", "
                (I "Linear logic, " $ast "-autonomous categories and cofree coalgebras")
                ", in " (I "Categories in Computer Science and Logic") ", Contemporary Mathematics "
                (B "92") " (1989) ["
                (Href "https://ncatlab.org/nlab/files/SeelyLinearLogic.pdf" "pdf") ", "
                (Href "http://www.math.mcgill.ca/rags/nets/llsac.ps.gz" "ps.gz") ", "
                (Href "https://bookstore.ams.org/conm-92" "ISBN:978-0-8218-5100-5") "]"))
      "综述/进一步的发展见于"
      (Ul (Li "G. M. Bierman, " (I "What is a Categorical Model of Intuitionistic Linear Logic?") " ("
              (Href "http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.8687" "web") ")")
          (Li "Andrew Graham Barber, " (I "Linear Type Theories, Semantics and Action Calculi") ", 1997 ("
              (Href "http://www.lfcs.inf.ed.ac.uk/reports/97/ECS-LFCS-97-371/" "web") ", "
              (Href "http://www.lfcs.inf.ed.ac.uk/reports/97/ECS-LFCS-97-371/ECS-LFCS-97-371.pdf" "pdf") ")")
          (Li (nLab "Paul-Andr%C3%A9+Melli%C3%A8s" "Paul-André Melliès") ", "
              (I "Categorial Semantics of Linear Logic") ", in "
              (I "Interactive models of computation and program behaviour")
              ", Panoramas et synthèses 27, 2009 ("
              (Href "http://www.pps.univ-paris-diderot.fr/~mellies/papers/panorama.pdf" "pdf") ")"))
      "关于" (nLab "dependent+linear+type+theory" "依赖线性类型论") ", 见:"
      (Ul (Item "Vakar14"
                (nLab "Matthijs+V%C3%A1k%C3%A1r" "Matthijs Vákár") ", "
                (I "Syntax and Semantics of Linear Dependent Types") " ["
                (Href "http://arxiv.org/abs/1405.0033" "arXiv:1405.0033") "]")
          (Item "RileyFinsterLicata21"
                (nLab "Mitchell+Riley" "Mitchell Riley") ", "
                (nLab "Eric+Finster" "Eric Finster") ", "
                (nLab "Daniel+Licata" "Daniel Licata") ", "
                (I "Synthetic Spectra via a Monadic and Comonadic Modality") " ["
                (Href "https://arxiv.org/abs/2102.04099" "arXiv:2102.04099") "]")
          (Li (nLab "Mitchell+Riley" "Mitchell Riley") ", "
              (I "Extending Homotopy Type Theory with Linear Type Formers")
              ", talk at " (I "The ForML Lab") " (2021) ["
              (Href "https://the-au-forml-lab.github.io/colloquium_talks/Riley.html" "web") ", "
              (Href "https://www.youtube.com/watch?v=sPxtdCtjSDc" "video") "]")
          (Item "Riley22Thesis"
                (nLab "Mitchell+Riley" "Mitchell Riley") ", "
                (I "A Bunched Homotopy Type Theory for Synthetic Stable Homotopy Theory")
                ", PhD Thesis (2022) ["
                (Href "https://doi.org/10.14418/wes01.3.139" "doi:10.14418/wes01.3.139") ", "
                (Href "https://digitalcollections.wesleyan.edu/object/ir%3A3269" "ir:3269") ", "
                (Href "https://mvr.hosting.nyu.edu/pubs/thesis.pdf" "pdf") "]"
                (Blockquote (P "(使用了来自" (nLab "bunched+logic" "束逻辑") "的想法)")))
          (Item "Riley22"
                (nLab "Mitchell+Riley" "Mitchell Riley") ", "
                (I "Linear Homotopy Type Theory") ", talk at: "
                (Href "https://www.uwo.ca/math/faculty/kapulkin/seminars/hottest_junior_2022.html"
                      "HoTTEST Event for Junior Researchers 2022")
                " (Jan 2022) [slides: "
                (Href "https://www.uwo.ca/math/faculty/kapulkin/seminars/hottestfiles/Riley-2022-01-20-HoTTEST.pdf" "pdf")
                ", video: " (Href "https://www.youtube.com/watch?v=o2oWhHabjdM" "YT") "]"))
      (nLab "typed+lambda-calculus" "类型化lambda演算") "和"
      (nLab "cartesian+closed+categories" "笛卡尔闭范畴") "之间的关系讨论于第I部分, 而带有"
      (nLab "product+types" "积类型") "的" (nLab "type+theory" "类型论") "的范畴和"
      (nLab "toposes" "topos") "之间的" (nLab "adjunction" "伴随") "讨论于第II章:"
      (Ul (Item "LambekScott86"
                (nLab "Joachim+Lambek" "Joachim Lambek") ", "
                (nLab "Philip+J.+Scott" "Philip J. Scott") ", "
                (I "Introduction to higher order categorical logic")
                ", Cambridge Studies in Advanced Mathematics " (B "7") " (1986) ["
                (Href "https://www.cambridge.org/ae/academic/subjects/mathematics/logic-categories-and-sets/introduction-higher-order-categorical-logic?format=PB&amp;isbn=9780521356534"
                      "ISBN: 0-521-24665-2")
                ", "
                (Href "https://raw.githubusercontent.com/Mzk-Levi/texts/master/Lambek%20J.%2C%20Scott%20P.J.%20Introduction%20to%20Higher%20Order%20Categorical%20Logic.pdf"
                      "pdf")
                "]"))
      (nLab "locally+cartesian+closed+categories" "局部笛卡尔闭范畴") "和"
      (nLab "dependent+type+theories" "依赖类型论") "之间的"
      (nLab "equivalence+of+categories" "范畴等价") "最初声称于"
      (Ul (Item "Seely84b"
                (nLab "R.+A.+G.+Seely" "R. A. G. Seely") ", "
                (I "Locally cartesian closed categories and type theory")
                ", Math. Proc. Camb. Phil. Soc. " (B "95") " (1984) 33-48 ["
                (Href "https://doi.org/10.1017/S0305004100061284" "doi:10.1017/S0305004100061284") ", "
                (Href "http://www.math.mcgill.ca/rags/LCCC/LCCC.pdf" "pdf") "]"))
      "继先前猜想于以下文献中的一个陈述之后"
      (Ul (Li (nLab "Per+Martin-L%C3%B6f" "Per Martin-Löf") ", "
              (I "An intuitionistic theory of types: predicative part")
              ", In Logic Colloquium (1973), ed. H. E. Rose and J. C. Shepherdson "
              "(North-Holland, 1974), 73-118. ("
              (Href "http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.131.926" "web") ")"))
      "这一论证中严格的代换相较于弱的拉回的问题讨论并修复于"
      (Ul (Item "Curien"
                (nLab "Pierre-Louis+Curien" "Pierre-Louis Curien") ", "
                (I "Substitution up to isomorphism") ", Fundamenta Informaticae, "
                (B "19") " 1-2 (1993) 51-86 ["
                (Href "https://dl.acm.org/doi/10.5555/175469.175471" "doi:10.5555/175469.175471")
                "] and: Diagrammes " (B "23") " (1990) 43-66 ["
                (Href "http://www.numdam.org/item/DIA_1990__23__43_0" "numdam:DIA_1990__23__43_0") "]")
          (Item "Hofmann"
                (nLab "Martin+Hofmann" "Martin Hofmann") ", "
                (I "On the interpretation of type theory in locally cartesian closed categories")
                ", in " (I "Computer Science Logic. CSL 1994") ", Lecture Notes in Computer Science "
                (B "933") " (1994) 427&ndash;441 ["
                (Href "https://doi.org/10.1007/BFb0022273" "doi:10.1007/BFb0022273") "]"))
      "但是在此过程中范畴等价丢失了. 这最终在以下文献中被全部纠正"
      (Ul (Item "ClairambaultDybjer"
                (nLab "Pierre+Clairambault" "Pierre Clairambault") ", "
                (nLab "Peter+Dybjer" "Peter Dybjer") ", "
                (I "The Biequivalence of Locally Cartesian Closed Categories and Martin-Löf Type Theories")
                ", in " (I "Typed lambda calculi and applications") ", Lecture Notes in Comput. Sci. "
                (B "6690") " Springer (2011) ["
                (Href "http://arxiv.org/abs/1112.3456" "arXiv:1112.3456") ", "
                (Href "https://doi.org/10.1007/978-3-642-21691-6_10" "doi:10.1007/978-3-642-21691-6_10") "]"))
      "以及"
      (Ul (Item "CurienGarnerHofmann"
                (nLab "Pierre-Louis+Curien" "Pierre-Louis Curien") ", "
                (nLab "Richard+Garner" "Richard Garner") ", "
                (nLab "Martin+Hofmann" "Martin Hofmann") ", "
                (I "Revisiting the categorical interpretation of dependent type theory")
                ", Theoretical Computer Science " (B "546") " 21 (2014) 99-119 ["
                (Href "https://doi.org/10.1016/j.tcs.2014.03.003" "doi:10.1016/j.tcs.2014.03.003") ", "
                (Href "https://ncatlab.org/nlab/files/CurienGarnerHofmann.pdf" "pdf") "]"))
      "另一个也适用于" (nLab "intensional+identity+types" "内涵恒等类型") "并因而适用于"
      (nLab "homotopy+type+theory" "同伦类型论") "的版本见于"
      (Ul (Item "LumsdaineWarren13"
                (nLab "Peter+LeFanu+Lumsdaine" "Peter LeFanu Lumsdaine") ", "
                (nLab "Michael+Warren" "Michael Warren") ", "
                (I "An overlooked coherence construction for dependent type theory") ", CT2013 ("
                (Href "https://ncatlab.org/nlab/files/LumsdaineWarren2013.pdf" "pdf") ")")
          (Li (nLab "Peter+LeFanu+Lumsdaine" "Peter LeFanu Lumsdaine") ", "
              (nLab "Michael+Warren" "Michael Warren") ", "
              (I "The local universes model: an overlooked coherence construction for dependent type theories")
              " (" (Href "http://arxiv.org/abs/1411.1736" "arXiv:1411.1736") ")"))
      "联系" (nLab "homotopy+type+theory" "同伦类型论") "和"
      (nLab "locally+cartesian+closed+%28infinity%2C1%29-categories" "局部笛卡尔闭" $inf1 "-范畴")
      "的类似陈述大约在以下文献中被形式化地猜想"
      (Ul (Item "Joyal"
                (nLab "Andr%C3%A9+Joyal" "André Joyal") ", "
                (I "Remarks on homotopical logic") ", Oberwolfach (2011) ("
                (Href "http://hottheory.files.wordpress.com/2011/06/report-11_2011.pdf#page=19" "pdf") ")"))
      "继" (nLab "Steve+Awodey" "Steve Awodey") "先前的提议之后. 明确地, 添加"
      (nLab "univalence" "泛等") "公理后这被精化为"
      (nLab "%28%E2%88%9E%2C1%29-topos+theory" $inf1 "-topos理论") "的提议大约出现于"
      (Ul (Item "Awodey"
                (nLab "Steve+Awodey" "Steve Awodey") ", " (I "Type theory and homotopy") " ("
                (Href "http://www.andrew.cmu.edu/user/awodey/preprints/TTH.pdf" "pdf") ")"))
      (nLab "homotopy+type+theory" "同伦类型论") "的这一高阶范畴语义的细节见于"
      (Ul (Item "Shulman12"
                (nLab "Michael+Shulman" "Michael Shulman") ", "
                (I "Univalence for inverse diagrams and homotopy canonicity")
                " (" (I "From type theory and homotopy theory to Univalent Foundations of Mathematics")
                "), Mathematical Structures in Computer Science " (B "25") " 5 (2015) ["
                (Href "https://arxiv.org/abs/1203.3253" "arXiv:1203.3253") ", "
                (Href "https://doi.org/10.1017/S0960129514000565" "doi:/10.1017/S0960129514000565") "]"))
      "讲义见于"
      (Ul (Li (nLab "Mike+Shulman" "Mike Shulman") ", "
              (I "Categorical models of homotopy type theory") ", April 13, 2012 ("
              (Href "https://home.sandiego.edu/~shulman/hottminicourse2012/03models.pdf" "pdf") ")")
          (Item "Joyal11"
                (nLab "Andr%C3%A9+Joyal" "André Joyal") ", "
                (I "Remarks on homotopical logic") ", Oberwolfach (2011) ("
                (Href "http://hottheory.files.wordpress.com/2011/06/report-11_2011.pdf#page=19" "pdf") ")")
          (Item "Joyal14"
                (nLab "Andr%C3%A9+Joyal" "André Joyal") ", "
                (I "Categorical homotopy type theory") ", March 17, 2014 ("
                (Href "http://ncatlab.org/homotopytypetheory/files/Joyal.pdf" "pdf") ")"))
      "另见"
      (Ul (Item "Kapulkin14"
                (nLab "Chris+Kapulkin" "Chris Kapulkin") ", "
                (I "Type theory and locally cartesian closed quasicategories")
                ", Oxford 2014 (" (Href "https://www.youtube.com/watch?v=g87bZJ2bvYk" "video") ")")
          (Li (nLab "Chris+Kapulkin" "Chris Kapulkin") ", "
              (nLab "Peter+LeFanu+Lumsdaine" "Peter LeFanu Lumsdaine") ", "
              (I "The homotopy theory of type theories")
              ", Advances in Mathematics " (B "337") " (2018) 1-38 ("
              (Href "https://arxiv.org/abs/1610.00037" "arXiv:1610.00037") ", "
              (Href "https://doi.org/10.1016/j.aim.2018.08.003" "doi:10.1016/j.aim.2018.08.003") ")")
          (Item "KapulkinSzumilo17"
                (nLab "Chris+Kapulkin" "Chris Kapulkin") ", "
                (nLab "Karol+Szumilo" "Karol Szumilo") ", "
                (I "Internal Language of Finitely Complete " $inf1 "-categories") " ("
                (Href "https://arxiv.org/abs/1709.09519" "arXiv:1709.09519") ")")
          (Item "Isaev16"
                (nLab "Valery+Isaev" "Valery Isaev") ", "
                (I "Algebraic Presentations of Dependent Type Theories") " ("
                (Href "https://arxiv.org/abs/1602.08504" "arXiv:1602.08504") ")")
          (Item "Isaev18"
                (nLab "Valery+Isaev" "Valery Isaev") ", "
                (I "Morita equivalences between algebraic dependent type theories") " ("
                (Href "https://arxiv.org/abs/1804.05045" "arXiv:1804.05045") ")"))
      "特别是在(" (nLab "constructive+set+theory" "构造性的") ")"
      (nLab "cubical+sets" "立方集合") "中的模型讨论于"
      (Ul (Item "BezemCoquandHuber13"
                "Marc Bezem, " (nLab "Thierry+Coquand" "Thierry Coquand") ", Simon Huber, "
                (I "A model of type theory in cubical sets")
                ", 2013 (" (Href "http://drops.dagstuhl.de/opus/volltexte/2014/4628/" "web") ", "
                (Href "http://drops.dagstuhl.de/opus/volltexte/2014/4628/pdf/7.pdf" "pdf") ")")
          (Item "KaposiAltenkirch14"
                "Ambrus Kaposi, " (nLab "Thorsten+Altenkirch" "Thorsten Altenkirch") ", "
                (I "A syntax for cubical type theory") " ("
                (Href "http://mazzo.li/dump/aim-kaposi-pres.pdf" "pdf") ")")
          (Item "Docherty14"
                "Simon Docherty, " (I "A model of type theory in cubical sets with connection") ", 2014 ("
                (Href "http://www.illc.uva.nl/Research/Publications/Reports/MoL-2014-12.text.pdf" "pdf") ")"))
      "受给予" (nLab "homotopy+type+theory" "同伦类型论") "和" (nLab "univalence" "泛等")
      "一个自然的等价所启发的"
      (nLab "elementary+%28infinity%2C1%29-topos" "初等" $inf1 "-topos")
      "的一个精确定义随后提出于"
      (Ul (Li (nLab "Mike+Shulman" "Mike Shulman") ", "
              (I "Inductive and higher inductive types") " (2012) ("
              (Href "http://www.sandiego.edu/~shulman/hottminicourse2012/04induction.pdf" "pdf") ")"))
      (nLab "univalence" "泛等") (nLab "type+universes" "类型宇宙") "的范畴语义讨论于"
      (Ul (Item "Awodey10"
                (nLab "Steve+Awodey" "Steve Awodey") ", " (I "Type theory and homotopy") " (2010) ("
                (Href "http://www.andrew.cmu.edu/user/awodey/preprints/TTH.pdf" "pdf") ")")
          (Item "KapulkinLumsdaineVoevodsky12"
                (nLab "Chris+Kapulkin" "Chris Kapulkin") ", "
                (nLab "Peter+LeFanu+Lumsdaine" "Peter LeFanu Lumsdaine") ", "
                (nLab "Vladimir+Voevodsky" "Vladimir Voevodsky") ", "
                (I "The Simplicial Model of Univalent Foundations") " ("
                (Href "http://arxiv.org/abs/1211.2851" "arXiv:1211.2851") ")")
          (Item "Shulman13"
                (nLab "Michael+Shulman" "Michael Shulman") ", "
                (I "The univalence axiom for elegant Reedy presheaves") " ("
                (Href "http://arxiv.org/abs/1307.6248" "arXiv:1307.6248") ")")
          (Item "GepnerKock12"
                (nLab "David+Gepner" "David Gepner") ", " (nLab "Joachim+Kock" "Joachim Kock") ", "
                (I "Univalence in locally cartesian closed " $inf "-categories") " ("
                (Href "http://arxiv.org/abs/1208.1749" "arXiv:1208.1749") ")")
          (Item "Cisinski14"
                (nLab "Denis-Charles+Cisinski" "Denis-Charles Cisinski") ", "
                (I "Univalent universes for elegant models of homotopy types") " ("
                (Href "http://arxiv.org/abs/1406.0058" "arXiv:1406.0058") ")"))
      (nLab "Awodey%27s+conjecture" "Awodey猜想") "的证明, 即所有"
      (nLab "%E2%88%9E-stack" $inf "-栈") (nLab "%28%E2%88%9E%2C1%29-topos" $inf1 "-topos")
      "都具有由解释带有" (nLab "univalence" "泛等") (nLab "type+universes" "类型宇宙") "的"
      (nLab "homotopy+type+theory" "同伦类型论") " (为其提供"
      (nLab "categorical+semantics" "范畴语义") ") 的" (nLab "model+categories" "模型范畴")
      "给出的" (nLab "presentable+%28%E2%88%9E%2C1%29-category" "表现") ":"
      (Ul (Item "Shulman19"
                (nLab "Michael+Shulman" "Michael Shulman") ", "
                (I "All " $inf1 "-toposes have strict univalent universes") " ("
                (Href "https://arxiv.org/abs/1904.07004" "arXiv:1904.07004") ")."))
      "介绍和综述见于:"
      (Ul (Item "Riehl22"
                (nLab "Emily+Riehl" "Emily Riehl") ", "
                (I "On the " $inf "-topos semantics of homotopy type theory")
                ", lecture at " (I (Href "https://conferences.cirm-math.fr/2689.html" "Logic and higher structures"))
                " CIRM (Feb. 2022) ["
                (Href "https://emilyriehl.github.io/files/semantics.pdf" "pdf") ", "
                (Href "https://ncatlab.org/nlab/files/Riehl-InfinityToposSemantics.pdf" "pdf") "]"))
      "弱Tarski式同伦类型宇宙的讨论见于"
      (Ul (Item "Luo12"
                (nLab "Zhaohui+Luo" "Zhaohui Luo") ", "
                (I "Notes on Universes in Type Theory") ", 2012 ("
                (Href "http://www.cs.rhul.ac.uk/home/zhaohui/universes.pdf" "pdf") ")")
          (Item "Gallozzi14"
                (nLab "Cesare+Gallozzi" "Cesare Gallozzi") ", "
                (I "Constructive Set Theory from a Weak Tarski Universe")
                ", MSc thesis (2014) ("
                (Href "https://ncatlab.org/nlab/files/GalloziCSTTarski.pdf" "pdf") ")"))
      "类型论和从lex范畴到topos的各类范畴之间的对应的一个讨论见于"
      (Ul (Li "Maria Emilia Maietti, "
              (I "Modular correspondence between dependent type theories and categories including pretopoi and topoi")
              ", Math. Struct. in Comp. Science (2005), vol. 15, pp. 1089&ndash;1149 ("
              (Href "http://www.math.unipd.it/~maietti/papers/tumscs.ps.gz" "gzipped ps") ") ("
              (Href "http://dx.doi.org/10.1017/S0960129505004962" "doi") ")"))
      "关于"
      (nLab "locally+cartesian+closed+%28infinity%2C1%29-categories"
            "局部笛卡尔闭" $inf1 "-范畴")
      "的" (nLab "internal+language" "内语言") "是带有"
      (nLab "dependent+sum+types" "依赖和类型") ", " (nLab "identity+types" "恒等类型") ", "
      (nLab "dependent+product+types" "依赖积类型") "以及"
      (nLab "function+extensionality" "函数外延性") "的"
      (nLab "Martin-L%C3%B6f+dependent+type+theory" "Martin-Löf依赖类型论") ":"
      (Ul (Li (nLab "El+Mehdi+Cherradi" "El Mehdi Cherradi") ", "
              (I "Internal languages of locally cartesian closed " $inf1 "-categories")
              " [" (Href "https://arxiv.org/abs/2509.03371" "arXiv:2509.03371") "]")))
   ))
