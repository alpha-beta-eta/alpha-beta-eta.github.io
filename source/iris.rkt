#lang racket
(provide iris.html)
(require SMathML)
(define (&Valid x)
  (app $V:script x))
(define (deno x c)
  (_ (&db0 x) c))
(define $!- (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (Discrete X)
  (ap Δ X))
(define $c_inf (_ $c $inf))
(define $ne (Mi "ne"))
(define (&eq n) (^^ $= n))
(define $eq0 (&eq $0))
(define $eqn (&eq $n))
(define $eqm (&eq $m))
(define $eqn+1 (&eq (&+ $n $1)))
(define $SProp (Mi "SProp"))
(define $dom (Mi "dom"))
(define (&dom f) (app $dom f))
(define (Extend f i a)
  (: f (bra0 (&<- i a))))
(define (Func . x*)
  (apply : (map-toggle
            #f (λ (f) (^^ $-> f)) x*)))
(define $Res (Mi "Res"))
(define $mon (Mi "mon"))
(define $later (Mo "▷"))
(define $later0 (Mo "▶"))
(define (Later P)
  (ap $later P))
(define (Later0 X)
  (ap $later0 X))
(define $wand
  (Mo "&minus;&#8270;"))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define $prcue (Mo "&prcue;"))
(define $⊎ (Mo "⊎"))
(define $? (Mi "?"))
(define (&? x) (^ x $?))
(define $dummy (Mi "&minus;"))
(define (Core a) (&abs a))
(define (RLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define (Box #:attr* [attr* '()] . xml*)
  `(menclose ,(attr*-set attr* 'notation "box")
             . ,xml*))
(define (DBox . xml*)
  `(mrow ((style "border: 0.8px dashed black; padding: 0px 4px;"))
         . ,xml*))
(define (Inv N P)
  (^ (Box P) N))
(define (Own γ a)
  (^ (DBox a) γ))
(define $ViewShift (Mo "⇛"))
(define ViewShift
  (case-lambda
    ((P Q) (ViewShift P $E:script Q))
    ((P E Q) (: P (_ $ViewShift E) Q))))
(define $Valid (OverBar $V:script))
(define (Valid a) (app $Valid a))
(define $Valid_1 (_ $Valid $1))
(define (Valid_1 a) (app $Valid_1 a))
(define $Valid_2 (_ $Valid $2))
(define (Valid_2 a) (app $Valid_2 a))
(define $Persistently (Mo "&square;"))
(define (Persistently P) (ap $Persistently P))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define (appc f . x*)
  (ap f (cur0 (apply : x*))))
(define $Prop (Mi "Prop"))
(define $iProp (Mi "iProp"))
(define $Val (Mi "Val"))
(define $Expr (Mi "Expr"))
(define $Ctx (Mi "Ctx"))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define $Inl (Pid "inl"))
(define $Inr (Pid "inr"))
(define (Inl t) (app $Inl t))
(define (Inr t) (app $Inr t))
(define $true (Pid "true"))
(define $false (Pid "false"))
(define $fork (Pid "fork"))
(define $assert (Pid "assert"))
(define $ref (Pid "ref"))
(define $CAS (Pid "CAS"))
(define $unit (tu0))
(define $ell (Mi "&ell;"))
(define $hole $bull)
(define (Lam x e)
  (: $lambda x $. e))
(define (Fork e) (appc $fork e))
(define (Assert e) (app $assert e))
(define (Ref e) (app $ref e))
(define (Deref e) (ap $! e))
(define (CAS e e1 e2)
  (appl $CAS e e1 e2))
(define (Meta str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $inl (Meta "inl"))
(define $inr (Meta "inr"))
(define (&inl x)
  (app $inl x))
(define (&inr x)
  (app $inr x))
(define $atomic (Meta "atomic"))
(define (Atomic e)
  (app $atomic e))
(define $True (Meta "True"))
(define $False (Meta "False"))
(define $Emp (Meta "Emp"))
(define $persistent (Meta "persistent"))
(define (Persistent P)
  (app $persistent P))
(define $pending (Meta "pending"))
(define $shot (Meta "shot"))
(define (&shot n)
  (app $shot n))
(define $lightning (Mi "↯"))
(define $impl $=>)
(define PointsTo &\|->)
(define Assign &<-)
(define split2 (&split 2))
(define App (&split 2))
(define split16 (&split 16))
(define Hoare
  (case-lambda
    ((P e v Q) (Hoare P e v Q $E:script))
    ((P e v Q E) (_ (split2 (cur0 P) e (cur0 (Bind v Q))) E))))
(define Hoare^
  (case-lambda
    ((P e Q) (split2 (cur0 P) e (cur0 Q)))
    ((P e Q E) (_ (Hoare^ P e Q) E))))
(define (subst e v x)
  (: e (bra0 (&/ v x))))
(define $~~> (Mo "⇝"))
(define $\\ (Mo "\\"))
(define $+_l (_ $+ $lightning))
(define $Ex (Mi "Ex"))
(define (Ex X)
  (app $Ex X))
(define $ex (Meta "ex"))
(define (&ex x)
  (app $ex x))
(define $AG (Mi "AG"))
(define $AG_0 (_ $AG $0))
(define (AG_0 X) (app $AG_0 X))
(define $ag (Meta "ag"))
(define $ag_0 (_ $ag $0))
(define (&ag_0 x)
  (app $ag_0 x))
(define $OneShot (Mi "OneShot"))
(define (OneShot X)
  (app $OneShot X))
(define $Own (Meta "Own"))
(define (&Own a)
  (app $Own a))
(define $rharu (Mo "&rharu;"))
(define $fin (Mi "fin"))
(define $rharu^^fin
  (^^ $rharu $fin))
(define-infix*
  (&eq0 $eq0)
  (&eqn $eqn)
  (&eqm $eqm)
  (&eqn+1 $eqn+1)
  (FinPartial $rharu^^fin)
  (&rharu $rharu)
  (&+_l $+_l)
  (&wand $wand)
  (&\\ $\\)
  (&~~> $~~>)
  (&prcue $prcue)
  (&⊎ $⊎)
  (Bind $.)
  (&impl $impl))
(define-@lized-op*
  (@wand &wand)
  (@∀ ∀)
  (@∃ ∃)
  (@Lam Lam)
  (@∈ ∈))
(define iris.html
  (TnTmPrelude
   #:title "Iris from the ground up"
   #:css "styles.css"
   (H1. "Iris from the ground up")
   (H2 "摘要")
   (P "Iris是一个用于高阶并发分离逻辑的框架, 它已在Coq证明助手中实现, "
      "并在众多验证项目中得到了非常有效的应用. "
      "Iris的设计初衷是简化并统一现代分离逻辑的基础, 但它随着时间不断演进, "
      "而Iris本身的设计与语义基础至今仍未被完整地整理成文, 也未在同一处得到系统的阐释. "
      "在本文中, 我们尝试填补这一空白: "
      "从第一性原理出发, 以一条连贯的叙述主线, "
      "较为完整地呈现Iris最新版本 (3.1版) 的全貌.")
   (H2. "引论")
   (P "Iris是一个用于高阶并发分离逻辑的框架, 在Coq证明助手中实现. "
      "自2014年以来, 我们和一个不断壮大的合作者网络一直在积极开发它. "
      "它是迄今为止唯一一个同时支持以下特性的验证工具:"
      (Ul (Li "基础性的机器检验证明, 用于")
          (Li "证明深层正确性性质, 对象是")
          (Li "细粒度并发程序, 这些程序使用")
          (Li "高阶命令式语言编写."))
      "所谓基础性的机器检验证明, 是指直接在证明助手中, "
      "针对所研究编程语言的操作语义进行的证明, "
      "并且只假设数理逻辑的底层公理 (以Coq的类型论来编码). "
      "所谓深层正确性性质, 是指超越单纯安全性的性质, "
      "例如上下文精化 (contextual refinement) 或"
      "完全函数正确性 (full functional correctness). "
      "所谓细粒度并发程序, 是指使用比较并设置 (compare-and-set, CAS) "
      "等底层原子同步指令来尽可能提高并行性的程序. "
      "而所谓高阶命令式语言, 是指像ML和Rust这样的语言, "
      "它们同时具备一等函数, 抽象类型和高阶可变引用.")
   (P "此外, Iris是通用的: 它不绑定于某种特定的语言语义, "
      "可以用来推导和部署一系列不同的形式系统, 包括但不限于: "
      "用于细粒度并发数据结构原子性精化的逻辑 (Jung et al., 2015), "
      "用于类ML语言关系推理的Kripke逻辑关系模型 "
      "(Krebbers et al., 2017b; Krogh-Jespersen et al., 2017; "
      "Timany et al., 2018; Frumin et al., 2018), "
      "用于弱内存模型 (relaxed memory models) "
      "的程序逻辑 (Kaiser et al., 2017), "
      "用于类JavaScript语言中对象能力模式 (object capability patterns) "
      "的程序逻辑 (Swasey et al., 2017), "
      "以及针对Rust编程语言一个实际子集的安全性证明 (Jung et al., 2018).")
   (P "在本文中, 我们介绍Iris的语义基础与逻辑基础, 重点是其最新版本 ("
      (Q "Iris 3.1") "). 在讨论这些基础及其有趣之处之前, "
      "我们先简要回顾一下并发分离逻辑的历史背景.")
   (H3. "并发分离逻辑简史")
   (P "大约在千禧年前后, Peter O'Hearn, John Reynolds, "
      "Hongseok Yang及其合作者提出了分离逻辑 (separation logic). "
      "它是Hoare逻辑的一个衍生, 旨在以更模块化, "
      "更可扩展的方式对操作指针的程序进行推理 "
      "(O'Hearn et al., 2001; Reynolds, 2002). "
      "分离逻辑继承了早期关于BI" -- "即" (Q "成束蕴涵")
      " (bunched implications) 逻辑 "
      "(O'Hearn & Pym, 1999; Ishtiaq & O'Hearn, 2001)"
      -- "的工作中的思想, 是一种" (Q "资源")
      "逻辑: 其中的命题不仅表示关于程序状态的事实, "
      "还表示对资源的所有权. 在最初的分离逻辑中, "
      (Q "资源") "的概念被固定为" (Q "堆") --
      "即全局内存的一部分, "
      "表示为从内存位置到其中所存储值的有限部分映射"
      -- "而关于堆的关键基本命题是" (Q "指向")
      " (points-to) 联结词" (PointsTo $ell $v)
      ", 它断言对将" $ell "映射到" $v
      "的单元素堆的所有权. 如果我们要验证的表达式"
      $e "在其前置条件中包含" (PointsTo $ell $v)
      ", 那么我们不仅可以假设位置" $ell "当前指向" $v
      ", 还可以假设更新" $ell "的"
      (Q "权利") "归" $e (Q "所有")
      ". 因此, 在验证" $e "时, 我们无需考虑程序中另一段代码可能在"
      $e "执行期间通过更新" $ell "来" (Q "干扰") $e
      "的可能性. 这种对干扰的抵抗能力反过来使我们能够模块化地验证"
      $e ", 也就是说, 无需关心" $e "所处的环境.")
   (P "尽管分离逻辑最初是作为一种针对顺序程序的逻辑而提出的, "
      "但没过多久O'Hearn就做出了一个关键的观察: "
      "分离逻辑内建的对干扰抵抗的支持, 对于并发程序的推理同样有用"
      -- "甚至可能更加有用. "
      "在并发分离逻辑 (concurrent separation logic, CSL) "
      "(O'Hearn, 2007; Brookes, 2007) 中, "
      "命题的含义与传统分离逻辑中大致相同, "
      "只是现在它们表示的是由正在运行相应代码的"
      "那个线程所拥有的所有权. 具体而言, "
      "这意味着如果线程" $t "能够断言"
      (PointsTo $ell $v) ", 那么" $t
      "就知道没有其他线程能够并发地读写" $ell
      ", 因此它可以完全忽略其他线程, "
      "就像在顺序环境中运行一样对" $ell
      "进行推理. 在一块状态同一时刻"
      "只被一个线程操作的常见情形下, "
      "这对简化验证来说是一个巨大的优势!")
   (P "当然, 这里有一个问题: "
      "线程通常总会在某个时刻需要通过某种共享状态 "
      "(无论是可变的堆还是消息传递通道) 彼此通信, "
      "而这种通信构成了一种无法避免的干扰. "
      "为了模块化地推理这种干扰, "
      "最初的CSL使用了一种简单形式的"
      "资源不变式 (resource invariants), "
      "它与用于同步的" (Q "条件临界区")
      " (conditional critical region) 构造相绑定. "
      "O'Hearn表明, 仅仅使用分离逻辑的标准规则"
      "加上一条关于资源不变式的简单规则, "
      "就可以优雅地验证相当" (Q "大胆")
      "的同步模式的安全性" --
      "在这些模式中, 共享资源的所有权以"
      "一种无法从程序文本中从语法上"
      "看出的方式在线程之间微妙地转移.")
   (P "在O'Hearn关于CSL的开创性论文 "
      "(以及Brookes为其给出的开创性可靠性证明) 之后, "
      "涌现出了大量令人振奋的后续工作. "
      "这些工作为CSL扩展了更复杂的机制, "
      "以模块化地控制干扰, 并在更细的粒度上刻画所有权转移 "
      "(例如通过原子的比较并交换 (compare-and-swap) 指令, 而非临界区), "
      "从而支持验证更加" (Q "大胆")
      "的并发程序 (Vafeiadis & Parkinson, 2007; "
      "Feng et al., 2007; Feng, 2009; Dodds et al., 2009; "
      "Dinsdale-Young et al., 2010b; Fu et al., 2010; "
      "Turon et al., 2013; Svendsen & Birkedal, 2014; "
      "Nanevski et al., 2014; da Rocha Pinto et al., 2014; "
      "Jung et al., 2015). 这一系列工作中一个重要的概念性进展是"
      (Q "虚构分离") " (fictional separation) 的概念 "
      "(Dinsdale-Young et al., 2010a,b): "
      "即使多个线程在并发地操作同一块共享的物理状态, "
      "我们也可以把它们看作是在更抽象的层面上操作状态中逻辑上互不相交的部分, "
      "然后用分离逻辑对这些抽象部分进行模块化推理. "
      "近年来的若干逻辑还加入了对高阶量化和非直谓不变式 "
      "(impredicative invariants) 的支持 "
      "(Svendsen & Birkedal, 2014; "
      "Jung et al., 2015, 2016; Appel, 2014). "
      "如果要验证具有语义循环特性的语言 "
      "(例如ML或Rust, 它们允许指向任意类型值的可变引用) "
      "中的代码, 这些特性是必需的.")
   (P "然而, 这一大批关于并发分离逻辑的工作也带来了一个弊端. "
      "Matthew Parkinson在其立场论文"
      "The Next 700 Separation Logics (Parkinson, 2010) "
      "中极具先见地指出了这一点: "
      (Q "近年来, 分离逻辑为验证领域带来了巨大的进步. "
         "然而, 出现了一个令人不安的趋势: "
         "每一个新的库或并发原语都需要一种新的分离逻辑.")
      " 此外, 随着CSL的表达能力越来越强, "
      "每一种逻辑都积累了越来越繁复, 越来越定制化的证明规则. "
      "这些规则是原始的 (primitive), 也就是说, "
      "它们的可靠性是直接诉诸同样繁复而定制化的逻辑模型来建立的. "
      "其结果是, 人们很难理解这些逻辑中的程序规范究竟意味着什么, "
      "它们彼此之间有何关系, 或者它们能否被可靠地组合到同一个推理框架中.")
   (P "Parkinson认为, 我们需要的是一种用于并发推理的通用逻辑, "
      "各种有用的规范都可以借助该逻辑的抽象机制编码到其中. 他写道: "
      (Q "通过找到正确的核心逻辑, 我们就能把精力集中在真正困难的问题上.")
      " 我们认为, 现在正是重新踏上Parkinson所倡导的寻找并发"
      (Q "正确的核心逻辑") "之旅的时候了.")
   (H3. "Iris")
   (P "为此, 我们开发了Iris (Jung et al., 2015), "
      "这是一种高阶并发分离逻辑, 其明确目标是简化与统一. "
      "Iris的核心思想是: 即使是近年来并发逻辑中最精巧的干扰控制机制, "
      "也可以由两个正交 (且早已为人熟知) 的要素组合来表达: "
      "部分交换幺半群 (partial commutative monoids, PCMs) "
      "和不变式 (invariants). "
      "PCM使逻辑的使用者能够自行定义所需类型的虚构 (或称"
      (Q "逻辑") "或" (Q "幽灵") " (ghost)) 状态, "
      "这对于编码高级CSL中出现的各种推理机制" --
      "例如许可 (permissions)、令牌 (tokens)、"
      "能力 (capabilities)、历史 (histories) 和协议 (protocols)"
      -- "至关重要. 不变式则用于将这种虚构状态与程序底层的物理状态联系起来. "
      "仅使用这两种机制, Jung et al. (2015) "
      "就展示了如何将先前逻辑中复杂的原始证明规则在Iris内部推导出来, "
      "由此产生了一句口号: " (Q "幺半群和不变式就是你所需要的一切."))
   (P "遗憾的是, 在Iris的最初形态 (后来被称为"
      (Q "Iris 1.0") ") 中, 这句口号在两个方面被证明是有误导性的:"
      (Ul (Li "幺半群并不够用. 存在某些有用的幽灵状态, 例如"
              (Q "命名命题") " (named propositions) (见§3.3), "
              "其中幽灵状态的结构必须与分离逻辑命题的语言相互递归地定义. "
              "我们将这类幽灵状态称为高阶幽灵状态 (higher-order ghost state). "
              "为了编码高阶幽灵状态, 我们似乎需要比幺半群更复杂的结构.")
          (Li "Iris不只是幺半群 + 不变式. 尽管幺半群和不变式"
              "确实构成了Iris 1.0的两个主要概念要素" --
              "而且就其简洁性和普适性而言, 它们可以说是"
              (Q "规范的") " (canonical)" --
              "但这些概念在逻辑中的实现涉及许多相互作用的逻辑机制, "
              "其中一些简单而规范, 另一些则不然. 例如, "
              "若干用于控制幽灵状态和不变式命名空间的机制"
              "被作为原始机制内建于逻辑中, "
              "此外还有掩码变换的视图转换 "
              "(mask-changing view shift) 的概念 "
              "(用于对资源执行逻辑更新, 这些更新可能涉及"
              (Q "打开") "或" (Q "关闭")
              "不变式) 以及最弱前置条件 "
              "(weakest preconditions) "
              "(用于编码Hoare三元组). 而且, "
              "这些机制的原始证明规则是非标准的, "
              "其语义模型也相当复杂, 这使得原始规则的正当性论证"
              -- "更不用说Iris的Hoare风格程序规范本身的含义" --
              "非常难以理解或解释. 事实上, "
              "Iris 1.0的论文 (Jung et al., 2015) "
              "甚至完全没有尝试详细给出程序规范的形式化模型.")))
   (P "我们随后在Iris 2.0 (Jung et al., 2016) "
      "和Iris 3.0 (Krebbers et al., 2017a) "
      "上的工作分别解决了这两个问题:"
      (Ul (Li "在Iris 2.0中, 为了支持高阶幽灵状态, "
              "我们提出了PCM的一种推广, 称为相机 (cameras). "
              "粗略地说, 相机可以被看作一种" (Q "步进索引的PCM")
              " (step-indexed PCM), 也就是说, "
              "一个配备了步进索引相等概念 "
              "(Appel & McAllester, 2001; "
              "Birkedal et al., 2011) 的PCM, "
              "并且PCM的组合运算与步进索引相等适当地"
              (Q "相容") ". 出于§4中讨论的原因, "
              "步进索引一直是Iris高阶分离逻辑命题模型中不可或缺的部分; "
              "通过将步进索引相等融入PCM, "
              "相机使我们能够对可以嵌入命题的幽灵状态进行建模.")
          (Li "在Iris 3.0中, 我们的目标是通过将Iris的思路贯彻到其 "
              "(可以说是) 逻辑上的终点, 来简化Iris中剩余的复杂性来源: "
              "将Iris的还原论方法论应用于Iris自身! 具体而言, "
              "Iris 3.0的核心是一个小巧且富有资源特性的基础逻辑 (base logic), "
              "它将Iris的本质提炼到我们认为的最低限度: "
              "它是一种高阶逻辑, 扩展了BI的基本联结词 "
              "(分离合取 (separating conjunction) 和魔杖 (magic wand))、"
              "一个表示资源所有权的谓词以及少量简单的模态 (modalities), "
              "但它并不将任何关于程序的命题作为原始概念内建其中. "
              "只有这个基础逻辑的可靠性需要直接针对"
              "底层语义模型 (使用前述的相机) 来证明. "
              "此外, 利用基础逻辑提供的少量机制, "
              "Iris 1.0中更精巧的掩码变换视图转换和最弱前置条件机制"
              -- "以及与之相关的证明规则" --
              "都可以在逻辑内部推导出来. "
              "而通过将这些更精巧的机制表达为派生形式, "
              "我们现在能够在比以往高得多的抽象层次上"
              "解释Iris程序规范的含义.")))
   (H3. "论文概览")
   (P "Iris在多篇会议论文中逐步发展, 这带来了一个不幸的后果: "
      "(1) 这些论文彼此之间并不完全一致 (因为逻辑随着时间发生了变化), "
      "并且 (2) Iris的设计与语义基础至今仍未被完整地整理成文, "
      "也未在同一处得到系统的阐释.")
   (P "在本文中, 我们尝试填补这一空白, "
      "以尽可能统一且自成体系的方式介绍Iris的最新版本 (即Iris 3.1"
      -- "我们将在§8.1中讨论自Iris 3.0以来的细微变化). "
      "我们的目标并不是让读者相信Iris是有用的, 尽管它确实有用"
      -- "许多论文 (本引言开头已提及) 已经证明了这一点, "
      "而对这些论文中所获经验的更系统的总结值得单独写一篇期刊文章. "
      "相反, 我们在这里的目标是从第一性原理出发, "
      "以一条连贯的叙述主线, 较为完整地呈现Iris的全貌.")
   (P "为此, 我们在§2和§3中首先通过一个简单的示例来介绍Iris的关键特性. "
      "在§4中, 我们接着介绍构建Iris语义模型所需的关键代数构造, "
      "包括我们新提出的相机 (cameras) 概念. "
      "在§5中, 我们描述Iris 3.1的基础逻辑, "
      "并展示如何利用上一节中的构造给出该基础逻辑的模型. "
      "然后, 在§6和§7中, 我们展示如何通过在Iris基础逻辑之上"
      "进行编码来恢复Iris完整的程序逻辑. 最后, "
      "在§8和§9中, 我们讨论若干技术要点, 并与相关工作进行详细比较.")
   (P "本文中的所有结果均已在Coq中形式化. "
      "我们的Coq源代码以及更多Iris 3.1的文档可在以下URL免费获取:"
      )
   (H2. "Iris之旅")
   (P "Iris是一种通用的高阶并发分离逻辑. 这里的" (Q "通用")
      "指的是该逻辑以人们希望推理的程序表达式所属的语言为参数, "
      "因此同一种逻辑可以用于多种多样的语言. "
      "为了使本节的讨论更加具体, "
      "我们用一种类ML的语言来实例化Iris, "
      "该语言支持高阶存储, fork以及"
      "比较并交换 (compare-and-set, CAS), 如下所示:"
      (eqn*
       ((∈ $v $Val)
        $::=
        (&\| $unit $z $true $false $ell (Lam $x $e) $..h)
        (@∈ $z $ZZ))
       ((∈ $e $Expr)
        $::=
        (&\| $v $x (app $e_1 $e_2) (Fork $e) (Assert $e)))
       ($ $\| (&\| (Ref $e) (Deref $e) (Assign $e_1 $e_2)
                   (CAS $e $e_1 $e_2) $..h))
       ((∈ $K $Ctx)
        $::=
        (&\| $hole (app $K $e) (app $v $K) (Assert $K)
             (Ref $K) (Deref $K) (Assign $K $e) (Assign $v $K)))
       ($ $\| (&\| (CAS $K $e_1 $e_2) (CAS $v $K $e_2)
                   (CAS $v $v_1 $K) $..h)))
      "(我们省略了序对与和上的常规操作.)")
   (P "该逻辑包含高阶分离逻辑中常见的联结词和规则, "
      "其中一部分已在下面的语法中列出. "
      "(实际上, 该语法中给出的许多联结词在Iris"
      "中都是作为导出形式 (derived forms) 定义的, "
      "这种灵活性是该逻辑的一个重要方面, "
      "不过我们将这一点的进一步讨论留到第5-7章.)"
      (eqn*
       ((&cm $P $Q $R)
        $::=
        (&\| $True $False (&conj $P $Q)
             (&disj $P $Q) (&impl $P $Q)))
       ($ $\| (&\| (∀ $x $P) (∃ $x $P) (&* $P $Q)
                   (PointsTo $ell $v)
                   (Persistently $P) (&= $t $u)))
       ($ $\| (&\| (Inv $N:script $P) (Own $gamma $a)
                   (Valid $a) (Hoare $P $e $v $Q)
                   (ViewShift $P $Q) $..h))))
   (P "Iris的证明规则包含并发分离逻辑中关于Hoare三元组的常见规则, "
      "如图1所示. 注意, " (RLabel "HOARE-BIND")
      "是将常见的顺序规则推广到任意求值上下文的结果.")
   (P "图1. Hoare三元组的基本规则."
      (MB (&rull (RLabel "HOARE-FRAME")
                 (Hoare $P $e $w $Q)
                 (Hoare (&* $P $R) $e $w (&* $Q $R))))
      (MB (&rull (RLabel "HOARE-VAL")
                 (Hoare $True $v $w (&= $w $v))))
      (MB (&rull (RLabel "HOARE-BIND")
                 (Hoare $P $e $v $Q)
                 (∀ $v (Hoare $Q (app $K $v) $w $R))
                 (Hoare $P (app $K $e) $w $R)))
      (MB (&rull (RLabel "HOARE-&lambda;")
                 (Hoare $P (subst $e $v $x) $w $Q)
                 (Hoare $P (App (@Lam $x $e) $v) $w $Q)))
      (MB (&rull (RLabel "HOARE-FORK")
                 (Hoare^ $P $e $True)
                 (Hoare^ $P (Fork $e) $True $E:script)))
      (MB (&rull (RLabel "HOARE-ASSERT")
                 (Hoare^ $True (Assert $true) $True $E:script)))
      (MB (&rull (RLabel "HOARE-ALLOC")
                 (Hoare $True (Ref $v) $ell
                        (PointsTo $ell $v))))
      (MB (&rull (RLabel "HOARE-LOAD")
                 (Hoare (PointsTo $ell $v)
                        (Deref $ell)
                        $w (&* (&= $w $v)
                               (PointsTo $ell $v)))))
      (MB (&rull (RLabel "HOARE-STORE")
                 (Hoare^ (PointsTo $ell $v)
                         (Assign $ell $w)
                         (PointsTo $ell $w)
                         $E:script)))
      (MB (&rull (RLabel "HOARE-CAS-SUC")
                 (Hoare (PointsTo $ell $v)
                        (CAS $ell $v $w)
                        $b (&* (&= $b $true)
                               (PointsTo $ell $w)))))
      (MB (&rull (RLabel "HOARE-CAS-FAIL")
                 (&!= $v $v^)
                 (Hoare (PointsTo $ell $v)
                        (CAS $ell $v^ $w)
                        $b (&* (&= $b $false)
                               (PointsTo $ell $v))))))
   (P "使Iris成为高阶分离逻辑的原因在于, "
      "全称量词和存在量词可以遍历任意类型, "
      "包括命题和(高阶)谓词. "
      "而且, 注意到Hoare三元组" (Hoare $P $e $v $Q)
      "是命题逻辑 (经常也被称为断言逻辑) "
      "的一部分而非独立的实体. "
      "因此, 三元组可以像任何逻辑命题一样使用. "
      "特别地, 它们可以嵌套使用, 以给出高阶函数的规约. "
      "此外, Hoare三元组" (Hoare $P $e $v $Q)
      "还标注了一个掩码" $E:script
      ", 用于记录当前哪些不变式正在生效. "
      "我们将在第2.2节再回到不变式和掩码的话题, "
      "目前暂且将其省略.")
   (P "图2. 示例代码和规约." (Br)
      "代码:"
      (CodeB "mk_oneshot := λ_.
  let x = ref (inl 0) in
  {
    tryset = λn. CAS(x, inl 0, inr n),

    check  = λ_.
      let y = !x in
      λ_.
        match (y, !x) with
        | (inl _, _)     => ()
        | (inr n, inl _) => assert(false)
        | (inr n, inr m) => assert(n = m)
        end
  }")
      "规约:"
      (CodeB "{ True }
  mk_oneshot ()
{ c. ∀v.
       { True } c.tryset(v) { w. w ∈ {true, false} }
     *
       { True } c.check()   { f. { True } f() { True } }
}"))
   (P (B "一个启发性的例子. ")
      "我们将通过验证图2中给出的一个简单高阶程序的安全性, "
      "来展示Iris的高阶特性以及它的其他一些核心特性. "
      "这个程序当然颇为刻意, 但它足以展示Iris的核心特性.")
   (P "函数" (Code "mk_oneshot()") "在" (Code "x")
      "处分配一个oneshot位置, "
      "并返回一个包含两个闭包的记录" (Code "c")
      ". (形式上, 记录是二元组的语法糖.) 函数"
      (Code "c.tryset(n)") "尝试将位置"
      (Code "x") "设置为" (Code "n")
      ", 如果该位置已经被设置过, 则会失败. "
      "我们使用" (Code "CAS")
      "来保证即使两个线程并发地尝试设置该位置, "
      "检查也依然正确. 函数" (Code "c.check()")
      "记录位置" (Code "x")
      "的当前状态, 然后返回一个闭包; 如果位置"
      (Code "x") "已经被初始化, "
      "该闭包会检查它的值没有发生改变.")
   (P "这个规范看起来有点奇怪, "
      "因为大多数前置条件和后置条件都是" (Code "True")
      ". 原因在于, 我们在这里想要证明的仅仅是代码是安全的, "
      "也就是说, 断言会" (Q "成功")
      ": 带有" (Code "assert(false)")
      "的分支永远不会被执行, 并且在最后一个分支中, "
      (Code "n") "总是等于" (Code "m")
      ". 在Iris中, Hoare三元组可以推出安全性, "
      "因此我们不需要再施加任何额外的条件.")
   (P "与函数式程序的Hoare三元组的常见做法一样, "
      "每个Hoare三元组的后条件都带有一个绑定子, 用于引用返回值. "
      "如果结果总是unit, 我们将省略该绑定子.")
   (P "我们使用嵌套的Hoare三元组来表达"
      (Code "mk_oneshot()")
      "返回闭包这一事实: "
      "由于Hoare三元组本身就是命题, "
      "我们可以将它们放入"
      (Code "mk_oneshot()")
      "的后条件中, 以描述客户端可以对" (Code "c")
      "做出哪些假设. 此外, 由于Iris是一种并发程序逻辑, "
      (Code "mk_oneshot()")
      "的规约实际上允许客户端从多个线程以任意组合并发地调用"
      (Code "c.tryset(n)") "和" (Code "c.check()")
      ", 以及由" (Code "c.check()")
      "返回的闭包" (Code "f") ".")
   (P "值得指出的是, Iris是一种仿射分离逻辑, "
      "这意味着它满足弱化规则"
      (&impl (&* $P $Q) $P)
      ". 直观地说, 这条规则允许人们丢弃资源; "
      "例如, 在Hoare三元组的后置条件中, "
      "人们可以丢弃对未使用内存位置的所有权. "
      "由于Iris是仿射的, 它没有" $Emp
      "联结词 (该联结词断言不拥有任何资源 "
      "(O'Hearn et al., 2001)); "
      "相反, Iris的" $True
      "联结词 (它描述对任意资源的所有权) "
      "是分离合取的单位元 (即"
      (&<=> (&* $P $True) $P)
      "). 我们将在第9.5节中进一步讨论这一设计选择.")
   (P (B "高层证明结构. ")
      "为了完成这个证明, "
      "我们需要以某种方式编码这样一个事实: 我们对"
      (Code "x") "只进行一次性 (oneshot) 更新. "
      "为此, 我们将分配一个名为"
      $gamma ", 值为" $a
      "的幽灵位置 (ghost location) "
      (Own $gamma $a) ", 它镜像" (Code "x")
      "的当前状态. 乍一听这似乎毫无意义: "
      "为什么要在幽灵状态中记录一个"
      "与某个物理位置中的值完全相同的值呢?")
   (P "关键在于, 使用幽灵状态让我们能够选择该位置上可以进行何种共享. "
      "对于物理位置" $ell ", 命题"
      (PointsTo $ell $v) "表示对" $ell
      "的完全所有权 (因此也意味着它不存在任何共享). "
      "与之相对, Iris允许我们为幽灵位置" $gamma
      "选择任意我们想要的结构和所有权形式. "
      "特别地, 我们可以这样定义它: 虽然"
      $gamma "的内容镜像" (Code "x")
      "的内容, 但一旦" $gamma
      "被初始化 (通过调用" (Code "tryset")
      "), 我们就可以自由地共享" $gamma
      "的所有权. 这又使得" (Code "check")
      "返回的闭包能够拥有" $gamma
      "的一部分, 用以见证 (witness) 其初始化之后的值. "
      "然后我们会有一个不变式 (见第2.2节), 将"
      $gamma "的值与" (Code "x")
      "的值联系起来. 这样我们就知道该闭包从"
      (Code "x") "读取时将会看到哪个值, "
      "并且知道该值将与" (Code "y") "相匹配.")
   (P "描述这一过程的另一种方式是: "
      "我们正在应用虚构分离 (fictional separation) "
      "的思想 (Dodds et al., 2009): "
      $gamma "上的分离是" (Q "虚构的")
      ", 其含义是多个线程可以拥有" $gamma
      "的各个部分, 从而操作同一个共享变量" (Code "x")
      ", 二者通过一个不变式联系在一起.")
   (P "在理解了这一高层证明结构之后, "
      "我们现在来解释幽灵状态的所有权与共享究竟是如何被控制的.")
   (H3. "Iris中的幽灵状态: 资源代数")
   (P "图3. 资源代数." (Br)
      "一个资源代数 (RA) 是一个元组"
      (tu0 $M (&: $Valid (&-> $M $Prop))
           (&: (Core $dummy) (&-> $M (&? $M)))
           (&: (@ $d*) (&-> (&c* $M $M) $M)))
      ", 其满足:"
      (MBL (RLabel "(RA-ASSOC)")
           (∀ $a $b $c
              (associate &d* $a $b $c)))
      (MBL (RLabel "(RA-COMM)")
           (∀ $a $b (commute &d* $a $b)))
      (MBL (RLabel "(RA-CORE-ID)")
           (∀ $a (&impl (∈ (Core $a) $M)
                        (&= (&d* (Core $a) $a) $a))))
      (MBL (RLabel "(RA-CORE-IDEM)")
           (∀ $a (&impl (∈ (Core $a) $M)
                        (&= (Core (Core $a))
                            (Core $a)))))
      (MBL (RLabel "(RA-CORE-MONO)")
           (∀ $a $b
              (&impl (&conj (∈ (Core $a) $M)
                            (&prcue $a $b))
                     (&conj (∈ (Core $b) $M)
                            (&prcue (Core $a) (Core $b))))))
      (MBL (RLabel "(RA-VALID-OP)")
           (∀ $a $b
              (&impl (Valid (&d* $a $b))
                     (Valid $a))))
      "其中" (&def= (&? $M) (&⊎ $M (setE $bottom))) "而"
      (&def= (&d* (&? $a) $bottom)
             (&d* $bottom (&? $a))
             (&? $a))
      (MBL (RLabel "(RA-INCL)")
           (&def= (&prcue $a $b)
                  (∃ (∈ $c $M)
                     (&= $b (&d* $a $c)))))
      (MB (&def= (&~~> $a $B)
                 (∀ (∈ (&? $c) (&? $M))
                    (&impl (Valid (&d* $a (&? $c)))
                           (∃ (∈ $b $B)
                              (Valid (&d* $b (&? $c))))))))
      (MB (&def= (&~~> $a $b)
                 (&~~> $a (setE $b))))
      )
   (P "一个单位资源代数 (uRA) 是一个资源代数" $M
      "并带有一个元素" $epsilon "满足:"
      (MB (split16 (Valid $epsilon)
                   (∀ (∈ $a $M)
                      (&= (&d* $epsilon $a) $a))
                   (&= (Core $epsilon)
                       $epsilon)))
      )
   (P "Iris允许人们通过命题" (Own $gamma $a)
      "使用幽灵状态, 其断言了幽灵位置" $gamma
      "的一个部分" $a "的所有权. "
      "Iris的灵活性是从以下事实中生发的: "
      "对于每个幽灵位置" $gamma
      ", 逻辑的使用者可以挑选其值" $a "的类型" $M
      ", 而非这个类型提前由逻辑固定. "
      "然而, 为了能够以有趣的方式使用"
      (Own $gamma $a) ", " $M
      "不能只是任意的类型, "
      "而应该具有某种额外的结构, 即"
      (Ul (Li "其应该能够对于不同线程的所有权进行复合. "
              "为了使得这成为可能, 类型" $M
              "应该具有一个运算" (@ $d*)
              "用于复合. 逻辑中这种运算的重要规则是"
              (&<=> (Own $gamma (&d* $a $b))
                    (&* (Own $gamma $a)
                        (Own $gamma $b)))
              " (见图4的" (RLabel "GHOST-OP") ").")
          (Li "无意义的复合"
              (&* (Own $gamma $a)
                  (Own $gamma $b))
              "应当被逻辑排除 (即它们应该蕴涵" $False
              "). 例如, 这发生于多个线程"
              "声称具有一个互斥资源的所有权. "
              "为了使得这成为可能, 运算" (@ $d*)
              "应当是部分的 (partial).")))
   (P "此外, 所有权的组合应当满足结合律和交换律, "
      "以反映分离合取的结合性与交换性. "
      "正因如此, 部分交换幺半群 "
      "(partial commutative monoids, PCMs) "
      "已成为分离逻辑中表示幽灵状态的标准结构. "
      "在Iris中, 我们对此略有偏离, "
      "使用了我们自己的资源代数 (resource algebra, RA) 概念, "
      "其定义见图3. 每个PCM都是一个RA, 但反之则不然" --
      "正如我们将在例子中看到的, "
      "RA所提供的额外灵活性带来了额外的逻辑表达能力.")
   (P "图4. 一些Iris证明规则."
      (MB (&rull (RLabel "GHOST-ALLOC")
                 (Valid $a)
                 (ViewShift $True (∃ $gamma (Own $gamma $a)))))
      (MB (&rull (RLabel "GHOST-OP")
                 (&<=> (Own $gamma (&d* $a $b))
                       (&* (Own $gamma $a)
                           (Own $gamma $b)))))
      (MB (&rull (RLabel "GHOST-VALID")
                 (&impl (Own $gamma $a)
                        (Valid $a))))
      
      (MB (&rull (RLabel "HOARE-VS")
                 (ViewShift $P $P^)
                 (Hoare $P^ $e $v $Q^)
                 (∀ $v (ViewShift $Q^ $Q))
                 (Hoare $P $e $v $Q)))
      (MB (&rull (RLabel "VS-TRANS")
                 (ViewShift $P $Q)
                 (ViewShift $Q $R)
                 (ViewShift $P $R)))
      (MB (&rull (RLabel "VS-FRAME")
                 (ViewShift $P $Q)
                 (ViewShift (&* $P $R) (&* $Q $R))))
      (MB (&rull (RLabel "INV-ALLOC")
                 (ViewShift $P (Inv $N:script $P))))
      (MB (&rull (RLabel "HOARE-INV")
                 (Hoare (&* (Later $P) $Q_1)
                        $e $v
                        (&* (Later $P) $Q_2)
                        (&\\ $E:script $N:script))
                 (Atomic $e)
                 (&sube $N:script $E:script)
                 (Hoare (&* (Inv $N:script $P)
                            $Q_1)
                        $e $v
                        (&* (Inv $N:script $P)
                            $Q_2)
                        $E:script)))
      (MB (&rull (RLabel "HOARE-CTX")
                 (Hoare (&* $P $Q) $e $v $R)
                 (Persistent $Q)
                 (&wand $Q (Hoare $P $e $v $R))))
      (MB (&rull (RLabel "PERSISTENT-DUP")
                 (Persistent $P)
                 (&<=> $P (&* $P $P))))
      (MB (&rull (RLabel "PERSISTENT-SEP")
                 (Persistent $P)
                 (Persistent $Q)
                 (Persistent (&* $P $Q))))
      (MB (&rull (RLabel "PERSISTENT-GHOST")
                 (&= (Core $a) $a)
                 (Persistent (Own $gamma $a))))
      )
   (P "RA与PCM之间有两个关键区别:"
      (Ol (Li "没有使用部分性, "
              "RA使用合法性来排除非法的所有权组合. "
              "具体而言, 存在一个谓词"
              (func $Valid $M $Prop)
              "可以识别出合法的元素. "
              "合法性与复合运算是兼容的 ("
              (RLabel "RA-VALID-OP")
              "). 我们在第4.3节将会看到合法性取代部分性对于定义"
              (Em "高阶幽灵状态") "的结构而言是必要的, "
              "即结构依赖于" $iProp "的幽灵状态, "
              "这是Iris逻辑的命题的类型.")
          (Li "RA并不具有一个对每个元素都是恒元的单一单位元"
              $epsilon " (即对于任意的" $a "都有"
              (&= (&d* $epsilon $a) $a)
              "), 而是具有一个部分函数" (Core $dummy)
              "为元素" $a "指派其可复制的核"
              (Core $a) ", 如" (RLabel "RA-CORE-ID")
              "所要求的那样. 我们进一步要求"
              (Core $dummy) "是幂等的 ("
              (RLabel "RA-CORE-IDEM")
              ") 和单调的 ("
              (RLabel "RA-CORE-MONO")
              "), 相对于" (Em "扩展序")
              "而言, 其定义类似于PCM的 ("
              (RLabel "RA-INCL") ")." (Br)
              "一个元素可以没有核, 这是由"
              (&= (Core $a) $bottom)
              "指示的. 为了方便地处理部分核, "
              "我们使用元变量" (&? $a) "遍历"
              (&def= (&? $M) (&⊎ $M (setE $bottom)))
              "的元素, 并将复合" (@ $d*) "提升至" (&? $M)
              ". 我们将会在第3.1节看到, "
              "部分核有助于我们从更小的原语构建有趣的复合RA." (Br)
              "在RA的确有一个单位元" $epsilon
              "的特殊情形下, 我们将其称为单位RA (uRA). 根据"
              (RLabel "RA-CORE-MONO")
              ", 可以推出uRA的核是一个完全函数, 即"
              (&!= (Core $a) $bottom) "." (Br)
              "可复制核的想法并不新颖, "
              "我们将在第9.3节讨论相关工作.")))
   ((tcomment)
    "不仅需要使用" (RLabel "RA-CORE-MONO")
    ", 还需要使用uRA的公理"
    (&= (Core $epsilon) $epsilon) ".")
   (P (B "我们例子的一个资源代数. ")
      "现在我们来定义可用于验证我们示例的RA, "
      "我们称之为oneshot RA. "
      "这个RA的目标是恰当地反映物理位置" $x
      ". carrier的定义使用了如下的类数据类型记号."
      (MB (&def= $M (&\| $pending (&shot (&: $n $ZZ))
                         $lightning))))
   (P "幽灵位置的两种重要状态为: " $pending
      ", 代表单次更新尚未发生, 以及" (&shot $n)
      ", 其是说位置已经被置为" $n
      ". 我们需要额外的元素" $lightning
      "来表示部分性; 其是唯一的非法元素:"
      (MB (&def= (Valid $a)
                 (&!= $a $lightning))))
   (P "当然了, 一个RA最有趣的地方在于其复合: "
      "两个线程的所有权合并时会发生什么? "
      "(以下等式没有定义的复合自动映射至"
      $lightning ".)"
      (MB (&def= (&d* (&shot $n)
                      (&shot $m))
                 (Choice0
                  ((&shot $n) ", 如果" (&= $n $m))
                  ($lightning ", 否则的话")))))
   (P "这个定义具有三种重要性质:"
      (MBL (RLabel "(PENDING-EXCL)")
           (&impl (Valid (&d* $pending $a))
                  $False))
      (MBL (RLabel "(SHOT-AGREE)")
           (&impl (Valid (&d* (&shot $n) (&shot $m)))
                  (&= $n $m)))
      (MBL (RLabel "(SHOT-IDEM)")
           (&= (&d* (&shot $n) (&shot $n))
               (&shot $n))))
   (P "性质" (RLabel "(PENDING-EXCL)")
      "表明, " $pending "与任何其他元素的复合都是无效的. "
      "因此, 如果我们拥有" $pending
      ", 就可以知道没有其他线程能够拥有该位置的其他部分. 此外, "
      (RLabel "(SHOT-AGREE)") "表明, 两个" (&shot $dummy)
      "元素的复合仅在参数 (即为oneshot选定的值) 相同时才有效. "
      "这体现了这样一种思想: 一旦某个值被选定, "
      "它就成为该位置唯一可能的值; "
      "所有线程对这个值是什么都达成一致.")
   (P "最后, " (RLabel "(SHOT-IDEM)")
      "表明, 一旦该位置被设置为某个" $n
      ", 我们就可以任意复制对它的所有权. "
      "这使得我们可以在任意数量的线程之间共享"
      "这一幽灵状态的所有权.")
   (P "请注意, 我们在这里使用的" (Q "所有权")
      "一词含义相当宽泛: RA 中的任何元素都可以被"
      (Q "拥有") ". 对于像" (&shot $n)
      "这样满足性质" (&= $a (&d* $a $a))
      "的元素, 拥有" $a "等价于拥有" $a
      "的多个副本. 在这种特殊情况下, "
      "所有权不再是排他的, "
      "称之为知识或许更为恰当. 因此, "
      "我们可以把" (&shot $n) "理解为表示"
      (Q "该位置的值已被设置为" $n)
      "这一知识. 由此可见, "
      "资源代数用同一种机制实现了双重目的: "
      "既能建模 (1) 资源的所有权, "
      "也能建模 (2) 知识的共享.")
   (P "回到我们的oneshot RA, "
      "我们还需要定义核" (Core $dummy) ":"
      (MB (split16
           (&def= (Core $pending) $bottom)
           (&def= (Core (&shot $n))
                  (&shot $n))
           (&def= (Core $lightning)
                  $lightning)))
      "注意到既然" $pending
      "的所有权是排他的, "
      "其没有合适的单位元素, "
      "故没有给它分配一个核.")
   (P "现在我们完成了oneshot RA的定义, "
      "验证该RA满足RA的公理是直接的.")
   (P (B "保持框架的更新. ")
      "到目前为止, 我们已经定义了幽灵位置可以处于哪些状态, "
      "以及该位置的状态如何分布在多个线程之间. "
      "然而, 还缺少一种改变幽灵位置状态的方法. "
      "当幽灵状态发生变化时, 重要的是它必须保持有效: "
      "Iris始终维护这样一个不变式, "
      "即把所有线程各自的贡献组合起来"
      "得到的状态是一个合法的RA元素. "
      "我们把维持这一不变式的状态变化"
      "称为保持框架的更新.")
   (P "最简单的保持框架的更新是确定性的. "
      "当满足以下条件时, 我们可以进行从"
      $a "到" $b "的保持框架的更新 (记作"
      (&~~> $a $b) "):"
      (MB (∀ (∈ (&? $c) (&? $M))
             (&impl (Valid (&d* $a (&? $c)))
                    (Valid (&d* $b (&? $c))))))
      "换言之, 对于任意的资源 (称为框架) "
      (∈ (&? $c) (&? $M)) "满足" $a
      "与" (&? $c) "兼容 (即"
      (Valid (&d* $a (&? $c)))
      "), 必然也有" $b "与"
      (&? $c) "兼容.")
   (P "例如, 对于我们的oneshot RA而言, "
      "如果它仍在pending, 取一个值是可能的:"
      (MBL (RLabel "(ONESHOT-SHOOT)")
           (&~~> $pending (&shot $n)))
      "原因在于" (RLabel "(PENDING-EXCL)")
      ": " $pending "实际上不与任何元素兼容; "
      "复合总是产生" $lightning
      ". 因此由" (Valid (&d* $pending (&? $c)))
      ", 我们可以知道" (&= (&? $c) $bottom)
      ". 这使得证明的剩余内容变得平凡.")
   (P "如果把框架" (&? $c)
      "看作所有其他线程所拥有资源的组合, "
      "那么保持框架的更新就能保证不会使并发运行的线程的资源失效. "
      "如果没有其他线程对该幽灵位置拥有任何所有权, "
      "框架可以是" $bottom
      ". 只要只进行保持框架的更新, "
      "我们就知道自己永远不会"
      (Q "踩到别人的脚趾")
      " (即不会干扰其他线程).")
   (P "一般而言, 我们也允许非确定性的保持框架的更新 (记作"
      (&~~> $a $B) "). 此时目标元素" $b
      "并非事先固定, 而是固定一个集合" $B
      ", 再根据当前框架从中选取某个元素" (∈ $b $B)
      ". 其形式化定义见图3. "
      "当我们在第3.2节中遇到第一个此类更新的例子时, "
      "会进一步讨论非确定性的保持框架的更新.")
   (P (B "幽灵状态的证明规则. ")
      "资源代数通过命题" (Own $gamma $a)
      "嵌入到逻辑中, 该命题断言对幽灵位置"
      $gamma "中的一个片段" $a "拥有所有权. "
      "操作这些幽灵断言的主要联结词称为视图转换 (或幽灵移动): "
      (ViewShift $P $Q) "表示, 给定满足" $P
      "的资源, 我们可以改变幽灵状态, 最终得到满足"
      $Q "的资源. (我们将在第2.2节中再讨论掩码标注"
      $E:script ".) 直观上, 视图转换就像Hoare三元组, "
      "只是没有任何代码: 只有前置条件和后置条件. "
      "它们不需要代码, 因为它们只涉及幽灵状态, "
      "而幽灵状态不对应实际程序中的任何操作.")
   (P "图4中的证明规则" (RLabel "GHOST-ALLOC")
      "可用于分配一个新的幽灵位置, 其初始状态"
      $a "可以任意选取, 只要" $a
      "在所选的RA中是有效的. 规则"
      (RLabel "GHOST-UPDATE")
      "表示, 我们可以按照上文所述, "
      "对幽灵位置进行保持框架的更新.")
   (P "Hoare三元组的所有常用结构规则对视图转换同样成立, "
      "例如框架规则 (" (RLabel "VS-FRAME")
      "). 规则" (RLabel "HOARE-VS")
      "说明了视图转换在程序验证中的用法: "
      "我们可以在Hoare三元组的前置条件和后置条件中应用视图转换. "
      "这相当于把" (Q "幽灵移动的三元组")
      " (即视图转换) 与关于" $e
      "的Hoare三元组组合起来. "
      "这样做不会改变三元组中的表达式, "
      "因为视图转换执行的幽灵状态操作不涉及任何实际代码.")
   (P "规则" (RLabel "GHOST-OP")
      "表示, 幽灵状态可以按照该RA所定义的组合运算"
      (@ $d*) "进行分离 (在分离逻辑的意义上); 而"
      (RLabel "GHOST-VALID")
      "刻画了这样一个事实: "
      "只有合法的RA元素才可能被拥有.")
   (P "请注意, 视图转换" (@ $ViewShift)
      "与推出 " (@ $impl) "和魔杖" (@ $wand)
      "非常不同: 推出" (&impl $P $Q)
      "是说每当" $P "成立则" $Q "必然也成立. "
      "与之相对比的是, 视图转换"
      (ViewShift $P $Q)
      "是说每当" $P "成立, " $Q
      "在我们改变幽灵状态的proviso下成立. "
      "因此, 不像推出和魔杖, "
      "唯一能够消去视图转换的方式是藉由"
      (RLabel "HOARE-VS") ".")
   (H3. "不变量")
   (P "既然我们已经搭建好了幽灵位置" $gamma
      "的结构, 接下来就需要把"
      $gamma "的状态与" $x
      "的实际物理值联系起来. "
      "这一步是通过不变式来完成的.")
   (P "不变式 (Ashcroft, 1975) 是一种在任何时刻都成立的性质: "
      "每个访问该状态的线程在执行每一步计算之前都可以假定不变式成立, "
      "但它也必须保证在每一步执行之后不变式仍然成立. "
      "由于我们使用的是分离逻辑, 不变式不仅仅是"
      (Q "成立") "而已; 它表达的是对某些资源的所有权, "
      "访问该不变式的线程就能获得这些资源的访问权. "
      "规则HOARE-INV按如下方式实现了这一思想:"
      (MB (&rule (Hoare (&* (Later $P) $Q_1)
                        $e $v
                        (&* (Later $P) $Q_2)
                        (&\\ $E:script $N:script))
                 (Atomic $e)
                 (&sube $N:script $E:script)
                 (Hoare (&* (Inv $N:script $P)
                            $Q_1)
                        $e $v
                        (&* (Inv $N:script $P)
                            $Q_2)
                        $E:script))))
   (P "这条规则内容相当多, 所以我们会仔细地逐一讲解. "
      "首先是命题" (Inv $N:script $P)
      ", 它表示" $P " (一个任意命题) 被维持为一个不变式. "
      "该规则表明, 在上下文中拥有这个命题就允许我们访问该不变式, "
      "具体包括: 在验证" $e "之前获得" $P
      "的所有权, 并在验证完成之后交还" $P
      "的所有权. 关键在于, 我们要求" $e
      "是原子的, 即计算保证在单步之内完成. "
      "这对可靠性至关重要: "
      "该规则允许我们暂时使用甚至破坏不变式, "
      "但在一个原子步骤之后 "
      "(也就是在任何其他线程有机会执行之前), "
      "我们必须重新建立它.")
   (P "注意, " (Inv $N:script $P)
      "只是另一种命题, "
      "它可以用在任何普通命题可以使用的地方, "
      "包括Hoare三元组的前置条件和后置条件, "
      "以及不变式本身, 从而产生嵌套不变式. "
      "后一种性质有时被称为非直谓性. "
      "正是由于非直谓性, 不变式" $P
      "在前置条件和后置条件中是以带有"
      (Q "later") "模态的形式" (Later $P)
      "出现的; 如果没有later模态, "
      "打开不变式的规则就是不可靠的 (见第8.2节). "
      "不过, 如果不在不变式中存放Hoare三元组或不变式, "
      "通常就可以忽略later模态, "
      "本节中我们也将一直这样做. "
      "我们会在第5.5节进一步讨论later模态.")
   (P "最后, 我们来看掩码" $E:script
      "和命名空间" $N:script
      ": 它们用来避免重入问题. "
      "我们必须确保同一个不变式不会同时被访问两次, "
      "否则会错误地复制其底层资源. 为此, "
      "每个不变式都有一个用于标识它的命名空间" $N:script
      ". 此外, 每个Hoare三元组都标注了一个掩码, "
      "用来记录哪些不变式仍处于启用状态. "
      "访问一个不变式会将它的命名空间从掩码中移除, "
      "从而确保它不能以嵌套的方式被再次访问.")
   (P "不变式是通过INV-ALLOC规则 (图 4) 创建的: "
      "只要一个命题" $P "已被建立, "
      "就可以把它转化为一个不变式. "
      "这可以看作是一种资源转移: 支撑"
      $P "的资源从由某个线程局部拥有, "
      "转变为通过不变式由所有线程共享. "
      "创建不变式是一个视图转换; "
      "其原因我们将在第7章看到, "
      "届时我们会了解Iris中的不变式在" (Q "底层")
      "究竟是如何运作的. 在那之前, 我们将忽略不变式"
      (Inv $N:script $P) "的命名空间" $N:script
      ", 以及Hoare三元组 ("
      (Hoare $P $e $v $Q)
      ") 和视图转换 ("
      (ViewShift $P $Q)
      ") 的掩码" $E:script ".")
   (H3. "持久命题")
   (P "我们已经看到, Iris既能表达对独占资源的所有权 (例如"
      (PointsTo $ell $v) "或者" (Own $gamma $pending)
      "), 也能表达关于某些性质的知识, 如"
      (Inv $N:script $P) "或者" (Own $gamma (&shot $n))
      ", 其一旦成立则永远成立. "
      "我们把后一类命题称为持久的. "
      "持久命题的其他例子还有合法性"
      (Valid $a) ", 相等性" (&= $t $u)
      ", Hoare三元组" (Hoare^ $P $e (Bind $v $Q))
      ", 以及视图转换" (ViewShift $P $ $Q)
      ". 持久命题可以被自由复制 (PERSISTENT-DUP); "
      "资源只能使用一次这一通常的限制对它们并不适用.")
   (P "幽灵所有权的多用途性也体现在它与持久性的关系上: "
      "对某些元素 (如" $pending ") 的幽灵所有权是短暂的, "
      "而对核的所有权则是持久的 (PERSISTENT-GHOST). "
      "这体现了第2.1节中提到的思想, "
      "即RA能够在一个统一的框架中同时表达所有权和知识, "
      "而知识仅仅是指对持久资源的所有权. "
      "从这个角度看, 核" (Core $a)
      "是一个从RA元素" $a
      "中提取出知识的函数. 在" (Code "check")
      "的证明中, 持久幽灵所有权将起到至关重要的作用.")
   (P "持久命题的一个重要作用与嵌套Hoare三元组有关: "
      "正如规则HOARE-CTX所表达的, "
      "嵌套的Hoare三元组只能使用"
      (Q "外部") "上下文中持久的命题" $Q
      ". 持久性保证了当该Hoare三元组被"
      (Q "调用") "时 (即它所描述的代码被执行时), "
      $Q "仍然成立, 即使调用发生多次也是如此.")
   (P "一个与之密切相关的概念是可复制命题, 即满足"
      (&<=> $P (&* $P $P)) "的命题" $P
      ". 然而, 这是一个严格更弱的概念: "
      "并非所有可复制命题都是持久的. "
      "例如, 考虑带有分数权限" $q
      "的指向联结词" (: $ell (^^ $\|-> $q) $v)
      " (Boyland, 2003; Bornat et al., 2005), 命题"
      (∃ $q (: $ell (^^ $\|-> $q) $v))
      "是可复制的 (这可以通过将分数权限"
      $q "减半得到), 但它不是持久的.")
   (H3. "例子的证明")
   (P "现在我们已经了解了Iris足够多的特性, "
      "可以着手处理图2中概述的实际验证问题了. "
      "我们在图5中给出了该证明的Hoare大纲. "
      "注意, 我们把" (Code "match")
      "中对" (Code "x") "的读取用"
      (Code "let") "展开了. "
      "这样做仅仅是为了便于书写证明大纲; "
      "Iris完全有能力验证原始形式代码的正确性.")
   (P (B "mk_oneshot的证明. ")
      "首先, 通过" (Code "ref")
      "构造所执行的分配, 我们得到"
      (PointsTo $x (Inl $0))
      ". 接着, 我们分配一个新的幽灵位置"
      $gamma ", 其结构为上文定义的oneshot RA, "
      "并选取初始状态" $pending
      ". (这里隐式地使用了HOARE-VS, "
      "以证明在验证Hoare三元组的过程中"
      "应用视图转换是合理的.) "
      "最后, 我们建立并创建如下不变式:"
      (MB (&def= $I (&disj (@* (PointsTo $x (Inl $0))
                               (Own $gamma $pending))
                           (@∃ $n (&* (PointsTo $x (Inr $n))
                                      (Own $gamma (&shot $n)))))))
      "由于" $x "被初始化为" (Inl $0)
      ", 不变式" $I "在初始时成立. "
      "剩下要做的就是建立我们的后置条件, "
      "它由两个Hoare三元组组成. "
      "借助HOARE-CTX, 我们可以在证明"
      "这两个三元组时使用刚刚分配的不变式. "
      "(在证明大纲中, HOARE-CTX允许我们在"
      (Q "跨过一个" $lambda)
      "时保留资源, 但前提是这些资源是持久的. "
      "这对应于该函数可能被调用多次这一事实, "
      "因此资源不应被某一次调用耗尽.)")
   
   (H2. "高级幽灵状态构造")
   (P "在上一节中我们已经看到, "
      "用户自定义的幽灵状态在Iris中起着重要作用. "
      "在本节中, 我们将更深入地考察幽灵状态. "
      "首先, 在第3.1节中, 我们表明许多常用的资源代数"
      "都可以通过组合更小的, 可复用的组件来构造. "
      "接着在第3.2节中, 我们表明所有权联结词"
      (Own $gamma $a) "实际上可以用一个更为基础的"
      (Q "全局") "幽灵所有权概念来定义. "
      "在第3.3节中, 我们引入更高级的高阶幽灵状态概念, "
      "并表明它的朴素形式是不一致的.")
   (H3. "RA的构造")
   (P "Iris的关键特性之一是"
      "它把幽灵状态的结构完全交给逻辑的使用者来决定. "
      "如果需要某种专用的资源代数, 使用者可以直接使用它. "
      "然而事实证明, 许多常用的资源代数都可以通过"
      "组合更小的, 可复用的组件来构造. 因此, "
      "虽然在需要时我们可以使用整个资源代数空间, "
      "但我们不必为每一个新证明都构造定制的资源代数.")
   (P "例如, 回顾§2.1中的一次性 (oneshot) 资源代数, "
      "它实际上做了三件事:"
      (Ol (Li "它把资源代数中元素的分配与"
              "决定在那里存储什么值分离开来 (ONESHOT-SHOOT).")
          (Li "当一次性位置尚未初始化时, 所有权是排他的, "
              "即至多只有一个线程可以拥有该位置.")
          (Li "一旦值被决定, 它确保所有人都认同这个值."))
      "因此, 我们可以把一次性资源代数分解为和资源代数, "
      "排他资源代数与一致资源代数, 如下所述. "
      "(在所有资源代数的定义中, 省略的复合与核的情形都为"
      $lightning ".)")
   (P (B "和. ")
      "对任意资源代数" $M_1 "和" $M_2
      ", 和资源代数" (&+_l $M_1 $M_2)
      "定义为:"
      (eqn*
       ((&+_l $M_1 $M_2)
        $def=
        (&\| (&inl (&: $a_1 $M_1))
             (&inr (&: $a_2 $M_2))
             $lightning))
       ((Valid $a)
        $def=
        (&disj (@∃ (∈ $a_1 $M_1)
                   (&conj (&= $a (&inl $a_1))
                          (Valid_1 $a_1)))
               (@∃ (∈ $a_2 $M_2)
                   (&conj (&= $a (&inr $a_2))
                          (Valid_2 $a_2)))))
       ((&d* (&inl $a_1) (&inl $a_2))
        $def=
        (&inl (&d* $a_1 $a_2)))
       ((&d* (&inr $a_1) (&inr $a_2))
        $def=
        (&inr (&d* $a_1 $a_2)))
       ((Core (&inl $a_1))
        $def=
        (Choice0
         ($bottom ", 如果" (&= (Core $a_1) $bottom))
         ((&inl (Core $a_1)) ", 否则的话")))
       ((Core (&inr $a_2))
        $def=
        (Choice0
         ($bottom ", 如果" (&= (Core $a_2) $bottom))
         ((&inr (Core $a_2)) ", 否则的话")))))
   (P (B "排他. ")
      "给定集合" $X ", 排他资源代数" (Ex $X)
      "的作用是确保某一方排他地拥有一个值"
      (∈ $x $X) ". 我们按照以下方式定义"
      $Ex ":"
      (eqn*
       ((Ex $X) $def= (&\| (&ex (&: $x $X))
                           $lightning))
       ((Valid $a) $def= (&!= $a $lightning))
       ((Core (&ex $x)) $def= $bottom))
      "复合的结果总是" $lightning
      ", 以确保所有权是排他的. 这就像"
      $pending "与任何其他元素的复合都是"
      $lightning "一样 (PENDING-EXCL).")
   (P (B "一致. ")
      "给定集合" $X ", 一致资源代数"
      (AG_0 $X) "是为了保证多方可以对于已取得的值"
      (∈ $x $X) "达成一致意见. "
      "(之所以我们将其称为" $AG_0
      ", 是因为我们将在第4.3节对于该定义进行改进"
      "以得到最终版本" $AG
      ".) 我们定义" $AG_0 "如下:"
      (eqn*
       ((AG_0 $X)
        $def=
        (&\| (&ag_0 (&: $x $X))
             $lightning))
       ((Valid $a)
        $def=
        (∃ (∈ $x $X) (&= $a (&ag_0 $x))))
       ((&d* (&ag_0 $x) (&ag_0 $y))
        $def=
        (Choice0
         ((&ag_0 $x) ", 如果" (&= $x $y))
         ($lightning ", 否则的话")))
       ((Core (&ag_0 $x))
        $def=
        (&ag_0 $x)))
      "特别地, 一致资源代数满足以下性质, "
      "其对应于SHOT-AGREE:"
      (MBL "(AG0-AGREE)"
           (&impl (Valid (&d* (&ag_0 $x)
                              (&ag_0 $y)))
                  (&= $x $y))))
   (P (B "oneshot. ")
      "现在我们可以将oneshot RA的一般想法定义为"
      (&def= (OneShot $X)
             (&+_l (Ex $1)
                   (AG_0 $X)))
      ", 并将我们例子的RA恢复为"
      (OneShot $ZZ) ".")
   (P (B "保持框架的更新. ")
      "将RA分解为单独部分的另一个有点在于"
      "对于这些部分我们可以证明一般的保持框架更新. "
      "对于和与排他, 我们有以下一般的保持框架更新:"
      (MB (&rull
           "INL-UPDATE"
           (&~~> $a $B)
           (&~~> (&inl $a)
                 (setI (&inl $b)
                       (∈ $b $B)))))
      (MB (&rull
           "INR-UPDATE"
           (&~~> $a $B)
           (&~~> (&inr $a)
                 (setI (&inr $b)
                       (∈ $b $B)))))
      (MB (&rull
           "EX-UPDATE"
           (&~~> (&ex $x) (&ex $y))))
      "一致RA不允许非平凡的保持框架更新.")
   (P "考虑oneshot RA, 以上规则尚不足够. "
      
      )
   (P "图6. 幽灵状态的原始规则."
      (MB (&rull
           "OWN-OP"
           (&<=> (&Own (&d* $a $b))
                 (&* (&Own $a)
                     (&Own $b)))))
      (MB (&rull
           "OWN-UNIT"
           (&impl $True (&Own $epsilon))))
      (MB (&rull
           "OWN-CORE"
           (&impl (&Own (Core $a))
                  (Persistently
                   (&Own (Core $a))))))
      (MB (&rull
           "OWN-VALID"
           (&impl (&Own $a)
                  (Valid $a))))
      (MB (&rull
           "OWN-UPDATE"
           (&~~> $a $B)
           (ViewShift
            (&Own $a) $
            (∃ (∈ $b $B) (&Own $b)))))
      )
   (H3. "导出形式和全局幽灵状态")
   (P "Iris非常强调只提供一个最小的核心逻辑, "
      "并尽可能在逻辑内部推导出其余的构造, "
      "而不是把它们作为原语内置. "
      "例如, Hoare三元组和形如"
      (PointsTo $ell $v)
      "的命题实际上都是导出形式. "
      "我们将在§4和§5中看到, "
      "这样做的好处是模型可以保持得更简单, "
      "因为它只需要证明一个最小核心逻辑的可靠性.")
   (P "在本节中, 我们讨论幽灵所有权命题"
      (Own $gamma $a)
      "的编码, 它同样不是内置概念. "
      "如前所述, 这个命题允许我们拥有多个幽灵位置"
      $gamma ", 并且每个位置都可以取值于不同的资源代数. "
      "作为原语, Iris只提供单个全局幽灵位置, "
      "其结构由使用者选定的单个全局资源代数描述. "
      "然而, 通过选择合适的资源代数, "
      "我们可以在Iris中定义命题" (Own $gamma $a)
      ", 并在逻辑内部推导出图4中给出的它的规则.")
   (P "Iris对于幽灵所有权的原始构造是" (&Own $a)
      ", 其规则给定于图6. 值得注意的是, "
      "全局RA必须要是单位的, "
      "这意味着其应该有一个单位元素" $epsilon
      " (图3). 原因有两方面. "
      "首先, 其允许我们有规则OWN-UNIT, "
      "这可以用来证明GHOST-ALLOC. "
      "其次, 单位RA享有这样的性质: "
      "扩展序是自反的, " (Core $dummy)
      "函数是完全的. 这简化了第4章和第5章的模型构造. "
      "注意到我们总是可以将一个RA转换为一个uRA, "
      "通过以一个单位元素扩展它, "
      "保持所有的保持框架更新.")
   (P "为了定义联结词" (Own $gamma $a)
      ", 我们需要初始化单个全局幽灵状态RA以"
      "一个幽灵cell的堆. 为此, "
      "假定对于某个指标集" $I:script
      "给定了一族RA "
      (_ (@ $M_i) (∈ $i $I:script))
      ", 然后我们定义全局幽灵状态的RA "
      $M "为" (Q $M_i "之堆")
      "上的指标(依赖)积如下:"
      (MB (&def= $M (prod (∈ $i $I:script)
                          (FinPartial $NN $M_i)))))
   ((tcomment)
    "直觉性的理解是, 每种资源占了依赖积的一个分量, "
    "而每个分量里面在各处的值则是一个个同种但独立的资源. "
    "幽灵堆只纪录了每种每个资源的总和, "
    "线程拥有什么部分则不在此编码. "
    $gamma "标签是这里的" $NN ".")
   (P "在这种构造中, 我们把各个" $M_i
      "上的RA运算经由有限映射和积, "
      "按自然的逐点方式一路提升到" $M
      ", 所以" $M "本身也是一个RA. "
      "事实上它是一个uRA (带单位元的RA), "
      "单位元是所有空映射构成的积:"
      (MB (&def= $epsilon (Lam $j $empty))))
   (P "不难证明, 保帧更新可以在积中逐点进行, "
      "并且对有限映射上的保帧更新有以下规则成立:"
      (MB (&rull
           "FMAP-UPDATE"
           (&~~> $a $B)
           (&~~> (Extend $f $i $a)
                 (setI (Extend $f $i $b)
                       (∈ $b $B)))))
      (MB (&rull
           "FMAP-ALLOC"
           (Valid $a)
           (&~~> $f (setI (Extend $f $i $a)
                          (&!in $i (&dom $f))))))
      "注意FMAP-ALLOC是一个非确定性的保帧更新: 新元素的索引" $i
      "依赖于帧 (frame), 所以无法事先选定. "
      "我们能保证的是会得到一个新的单元素映射, "
      "其中某个新鲜索引" $i "被映射到" $a
      ". 规则FMAP-UPDATE表示把更新逐点提升到有限映射上.")
   (P "使用作为uRA的" $M "实例化Iris允许我们 "
      "(a) 证明中可以使用所有的" $M_i
      "; (b) 幽灵状态可以当作堆来处理, "
      "随时都能分配任意" $M_i "的新实例. "
      "我们把单个位置上的幽灵所有权联结词定义为:"
      (MB (&def= (Own $gamma (&: $a $M_i))
                 (&Own (Lam $j (Choice0
                                ((bra0 (&\|-> $gamma $a))
                                 ", 如果" (&= $i $j))
                                ($empty ", 否则的话")))))))
   (P "换言之, " (Own $gamma (&: $a $M_i))
      "断言了对于积里位置" $i "处的singleton堆"
      (bra0 (&\|-> $gamma $a))
      "的所有权. 我们通常隐式化具体的" $M_i
      ", 就记成" (Own $gamma $a)
      ". 图4中给出的" (Own $dummy $dummy)
      "的规则可以由图6中的" (&Own $dummy)
      "的规则导出.")
   (P (B "获得模块化证明. ")
      "即使有了多个RA可用, 似乎仍然存在模块化问题: "
      "每个证明都是在用某个特定RA族实例化的Iris中完成的. "
      "因此, 如果两个证明对RA的选择不同, "
      "它们就是在完全不同的逻辑中进行的, 因而无法组合.")
   (P "为了解决这个问题, 我们让证明对Iris所实例化的RA族进行泛化. "
      "所有证明都在用某个未知的" (_ (@ $M_i) (∈ $i $I:script))
      "实例化的Iris中进行. 如果某个证明需要一个特定的RA, "
      "它会进一步假设存在某个" $j ", 使得" $M_j
      "恰好是所需的RA. 这样一来, 组合两个证明就很直接了: "
      "得到的证明在任何包含了两个证明各自所需的全部特定RA的RA族中都成立. "
      "最后, 如果我们想在某个具体的Iris实例中得到某个证明的"
      (Q "封闭形式") ", 只需构造一个仅包含该证明所需RA的RA族即可.")
   (P "注意, 这里对资源代数的泛化发生在元层面 (meta-level), "
      "因此可用的资源代数集合必须在证明开始之前就固定下来. "
      "例如, 如果幽灵状态的类型需要依赖某个只有在运行时才能确定的程序值, "
      "那就会涉及某种形式的依赖类型. 我们的方法在多大程度上能与这种情况兼容, "
      "我们尚未探究, 因为这种情况在实践中似乎并不会出现.")
   (H3. "朴素高阶幽灵状态的悖论")
   (P "正如我们所看到的, Iris的设置是由用户提供一个资源代数" $M
      ", 逻辑以它为参数. 这带来了很大的灵活性, "
      "因为用户可以自由决定使用哪种幽灵状态. "
      "现在我们讨论能否把这种灵活性再推进一步: "
      "允许资源代数" $M "的构造依赖于Iris命题的类型" $iProp
      ". 在Jung et al. (2016) 中, "
      "我们把这种现象称为高阶幽灵状态, "
      "并说明了它在程序验证中有实际用途. "
      "随后在Krebbers et al. (2017a) 中, "
      "我们又说明了高阶幽灵状态的用途不止于程序验证: "
      "它还可以用普通的幽灵状态来编码不变式.")
   (P "我们对高阶幽灵状态的建模方式, "
      "限制了在构造用户提供的资源代数时" $iProp
      "的使用方式. 在Jung et al. (2016) 中, "
      "这些限制看起来只是为逻辑建模而产生的语义副产物. "
      "然而在本节中, 我们将说明某种形式的限制其实是必要的: "
      "如果以朴素的或不加限制的方式允许高阶幽灵状态, "
      "就会导致悖论 (即逻辑不可靠). 为了展示这个悖论, "
      "我们使用高阶幽灵状态最简单的实例, "
      "即命名命题 (named propositions, 也称为"
      (Q "存储的") "或" (Q "保存的")
      "命题) (Dodds et al., 2016).")
   ((theorem #:n "1. 高阶幽灵状态悖论")
    
    )
   (H2. "一个Iris的模型")
   (P "在前面的几节中, 我们对Iris做了一个高层次的概览. "
      "在本文的其余部分, 我们将更精确地给出Iris的定义, "
      "并证明它是可靠的 (sound).")
   (P "与许多先前关于分离逻辑的工作一样, "
      "我们通过给出一个语义模型来证明Iris的可靠性. "
      "从根本上说, 这样的证明分为三个步骤:"
      (Ol (Li "定义一个命题的语义domain, "
              "并配以适当的蕴涵 (entailment) 概念.")
          (Li "定义一个解释函数, "
              "将逻辑中的所有陈述映射到语义domain中的元素.")
          (Li "对于逻辑的每一条证明规则, "
              "证明前提的语义解释蕴涵结论的语义解释."))
      "在本节中, 我们处理第 (1) 步: "
      "定义Iris命题的语义domain. "
      "由于我们还没有确切地确定Iris的命题到底是什么, "
      "这看起来可能有些困难. 不过, 在§2中, "
      "我们已经直观地介绍了我们希望在Iris中表达的那类陈述. "
      "这已足以让我们讨论Iris命题语义domain的构造. "
      "接下来我们将自底向上地推进: 首先在§5中, "
      "在这个语义domain之上构建Iris基础逻辑 (base logic); "
      "然后在§6和§7中, 在基础逻辑之上构建我们在§2中"
      "见过的那些更高层的程序逻辑联结词.")
   (P "如 §2 所示, Iris的主要特性之一是"
      "资源所有权 (resource ownership), "
      "对于一个分离逻辑来说, 这并不令人意外. "
      "建模分离逻辑命题的常用方法 "
      "(O'Hearn et al., 2001) "
      "是将其视为资源上的谓词. 传统上"
      (Q "资源") "指的是堆的一个片段, "
      "而在我们的情形中, " (Q "资源")
      "是用户可以通过自己选择的RA来定义的东西. "
      "此外, 由于Iris是一个仿射 (affine) "
      "分离逻辑 (另见§9.5), "
      "这些谓词必须是单调的: "
      "如果一个谓词对某个资源成立, "
      "那么它对任何更大的资源也应当成立.")
   (P "由此, 我们得到了对Iris命题语义domain的如下初步尝试:"
      (MB (&def= $iProp (Func $Res $mon $Prop)))
      "这里, " $Res "是由用户选择的全局资源代数 "
      "(global resource algebra), " $Prop
      "是外围元逻辑 (ambient meta-logic) 中的命题domain.")
   (P "然而, 回想一下我们在§3.3中提到过, "
      "为了编码Iris的一些高级特性 (如不变式), "
      "我们需要高阶幽灵状态 (higher-order ghost state). "
      "这意味着我们希望资源" $Res "依赖于" $iProp
      ", 而与此同时" $iProp "又依赖于" $Res "!")
   (P "为了形式化地刻画这种依赖关系, 我们不再固定选择"
      $Res ", 而是用一个可以依赖于" $iProp
      "的函数" $F "来代替它. 这样, 用户选择的是"
      $F " (而不是" $Res "), 并且" $Res" 被定义为"
      (app $F $iProp) ". 于是, 定义变为如下形式:"
      (MBL "(PRE-IRIS)"
           (&= $iProp (Func $Res $mon $Prop))
           ", 其中"
           (&def= $Res (app $F $iProp)))
      "遗憾的是, 这已经不再只是一个定义了: "
      "该方程是否存在解" $iProp
      "并不显然. 事实上, 对于某些"
      $F "的选择, 例如取"
      (&def= (app $F $X) (AG_0 $X))
      ", 我们最终会得到"
      (&= $iProp (Func (AG_0 $iProp) $mon $Prop))
      ", 其使用基数论证可以表明并无解. "
      "这并不奇怪: 毕竟在§3.3中我们已经表明, 以"
      (AG_0 $iProp) "为资源类型会导致逻辑中出现矛盾, "
      "这意味着不可能存在以" (AG_0 $iProp)
      "作为资源类型的模型!")
   (P "为了规避这个问题, 我们使用步进索引 "
      "(Appel & McAllester, 2001). "
      "粗略地说, 步进索引的思想是让" $iProp
      "的元素成为一个命题序列: 序列中的第" $n
      "个命题只在接下来" $n "步计算的范围内成立. "
      "这个步进索引可以用来对PRE-IRIS中的递归进行分层. "
      "接下来, 我们将在§4.1中对模型以及步进索引的使用"
      "给出概念性的概述. 随后, 在§4.2-§4.4中, "
      "我们会引入以模块化方式处理步进索引的具体基础设施. "
      "最后, 在§4.5-§4.7中, 我们利用这一基础设施"
      "得到Iris命题的语义domain.")
   (H3. "对于模型构造的非形式化和概念性的概览")
   (P "下面几节中的精确定义比较技术化, "
      "可能会在一定程度上掩盖该模型核心处的概念简洁性. "
      "因此, 我们先非形式化地概述应如何理解这个模型, "
      "并说明后面几节中使用的技术性定义其实是对"
      "我们已经见过的概念的系统而自然的推广.")
   (P "自 (Birkedal et al., 2011) 以来, "
      "人们已经知道步进索引可以用一种抽象的, 范畴论的方式来处理. "
      "粗略地说, 这意味着我们可以按如下方式替换术语, "
      "从通常的集合论设定过渡到步进索引设定:"
      (Table
       #:attr* '((align "center"))
       (Tr (Td "集合论设定") (Td "步进索引设定"))
       (Tr (Td "集合") (Td "OFE (有序等价族)"))
       (Tr (Td $Prop " (命题)")
           (Td $SProp " (步进索引的下闭集合)"))
       (Tr (Td "函数") (Td "非扩张性函数"))))
   (P "表格右侧的概念将在§4.2中解释. 目前需要知道的是: "
      $Prop "上的常用运算 (例如相等, 合取, 推出和量词) 在"
      $SProp "上同样可用, 集合上的构造 (例如积与和类型) "
      "在OFE上同样可用. 此外, "
      "这些运算和构造满足许多我们在集合论中熟知的定律. "
      "这意味着在大多数情况下, "
      "我们完全可以不去关心自己究竟处在这两种设定中的哪一种.")
   (P "注意, 发生变化的不只是命题和集合的概念, 函数空间也变了: "
      "在步进索引设定中, 我们只能使用非扩张函数. "
      "这类函数与OFE在其定义域和值域上施加的结构是"
      (Q "相容") "的. 通过限制函数空间, 我们得到: 命题"
      (∀ (&cm $x $y)
         (&=> (&= $x $y)
              (&= (app $f $x)
                  (app $f $y))))
      "在" $SProp "中 (即翻译到步进索引设定之后) 总是成立. "
      "事实上, 这正是" $f "的非扩张性的定义.")
   (P "既然集合论和步进索引设定的行为基本相同, "
      "那么在步进索引设定中工作有什么好处呢? "
      "事实证明, 与集合论设定不同, 在步进索引设定中, "
      "我们可以为前面见过的方程PRE-IRIS"
      "求得一个 (同构意义下的) 解:"
      (MB (&cong $iProp (Func $Res $mon $SProp))
          ", 其中" (&def= $Res (app $F $iProp)))
      "在步进索引设定中求解这个方程的前提是, " $F
      "必须在某种意义上是" (Q "受保护的")
      " (guarded). 本节后面会看到, 这种保护是通过"
      (Q "later") " ▶来实现的.")
   (P "本节给出的Iris模型看上去可能有些随意, "
      "读者也许会觉得一切只是" (Q "碰巧")
      "在最后行得通了, 但事实并非如此. "
      "这些定义来自于把集束蕴涵逻辑 "
      "(logic of bunched implications) "
      "(O’Hearn & Pym, 1999) "
      "的一个标准集合论模型翻译到步进索引世界. "
      "由于集合论设定和步进索引设定之间对应得很紧密, "
      "这种翻译能按预期的方式进行. 不过, "
      "要证明我们可以在步进索引设定中工作而甚至察觉不到这一点, "
      "需要用到相当多的范畴论. 本文不会深入范畴论, "
      "因此我们用初等的方式而非范畴论的方式来介绍这个模型. "
      "本质上, 我们把步进索引的定义" (Q "展开")
      ", 这样就可以一直以集合论作为背景框架.")
   (P "用初等的方式介绍模型不仅让构造更容易理解, "
      "也简化了在Coq中的形式化. 因此, "
      "Iris模型在Coq中的形式化与本节的介绍非常接近.")
   (H3. "有序等价族")
   (P (MBL "(OFE-EQUIV)"
           (∀ $n (: (@ $eqn) "是一个等价关系")))
      (MBL "(OFE-MONO)"
           (∀ (&cm $n $m)
              (&=> (&>= $n $m)
                   (&sube (@ $eqn) (@ $eqm)))))
      (MBL "(OFE-LIMIT)"
           (∀ (&cm $x $y)
              (&<=> (&= $x $y)
                    (@∀ $n (&eqn $x $y)))))
      (MBL "(OFE-COMPL)"
           (∀ (_ (@ $c_n) (∈ $n $NN))
              (&=> (@∀ (&cm $n $m)
                       (&=> (&<= $n $m)
                            (&eqn $c_m $c_n)))
                   (@∃ $c_inf
                       (∀ $n (&eqn $c_n $c_inf))))))
      
      )
   (P "有序等价族 (Ordered families of equivalences, OFE) "
      "由Di Gianantonio & Miculan (2002) 提出, "
      "定义见图7. 它们是带有一些基本代数结构的集合, "
      "这些结构给集合赋予了某种形式的步进索引. "
      "OFE背后的核心直觉是: 如果元素" $x "和" $y
      "在" $n "步计算内是等价的, 也就是说, "
      "任何运行不超过" $n "步的程序都无法区分它们, "
      "那么就称它们是" $n "-等价的, 记作"
      (&eqn $x $y) ". 由此可知, 随着" $n
      "增大, " $eqn "会变得越来越精细 (OFE-MONO). "
      "在极限情况下, 它与普通的相等一致 (OFE-LIMIT).")
   (P "两个OFE之间的函数" (&: $f (Func $T $ne $U))
      "如果保持由" (@ $eqn) "定义的结构, "
      "那么就称其为非扩张性的 (OFE-NONEXP). "
      "换句话说, 把这样的函数作用于某些数据, "
      "不会让看起来相等的数据突然出现差异. 在"
      $n "步内无法被程序区分的元素, 在应用"
      $f "之后仍然无法区分.")
   (P "在步进索引世界中, 我们总是使用"
      (Q "直到某个" $n "为止")
      "的相等. 为了让这些相等真正有用, "
      "所有函数都必须尊重等价关系" $eqn
      ", 这一点至关重要. "
      "这就是我们要求所有函数都是非扩张的原因.")
   (P "如果" $f "不仅保持" $n "-相等, 还会让事物变得"
      (Q "更加相等") ", 那么我们就称" $f
      "是压缩的 (contractive) (OFE-CONTR). "
      "注意, 图7中给出的定义等价于要求"
      (&eq0 (app $f $x) (app $f $y)) "和"
      (&=> (&eqn $x $y)
           (&eqn+1 (app $f $x) (app $f $y)))
      ".")
   (P "OFE对于图8所定义的iris模型是根本性的. "
      "离散OFE " (Discrete $X) "将任意的集合" $X
      "转化为一个OFE, 通过赋予退化的步进索引等价. "
      "later OFE " (Later0 $T)
      "将一个OFE " $T "的步进索引等价向上移动一格, "
      "因而使得一切比" $T "都"
      (Q "更加相等一层") ".")
   (P "步进索引命题的OFE " $SProp
      "表示了对于某些步骤为真或者永远为真的命题 ("
      (Q "真值") "). (" $SProp
      "对应于up to和包括" $omega
      "的序数集合.) 最重要的是, " $SProp
      "的元素是下闭的, 这意味着如果一个陈述对于"
      $n "步是有效的, 那么其对于" (&<= $m $n)
      "步都是有效的. " $SProp
      "处理的不是事情为真为假的绝对概念, "
      "而是提供了一种更加近似的方法. "
      "完全有效的陈述以" $NN "表示, 假陈述以"
      $empty "表示; 但在这两个极端之间还有无穷多种"
      (Q "真性的深浅") ". 两个步骤索引命题在步骤索引"
      $n "处是相等的, 如果它们对于所有直至且包括"
      $n "的步进索引都是一致的. 类似地, "
      "我们定义步骤索引包含关系为忽略大于" $n
      "的元素的包含关系.")
   (P "一类重要的步骤索引命题是某个OFE的元素之间的相等. "
      "的确, " $x "和" $y "相等的步骤索引的集合"
      (setI $n (&eqn $x $y)) "是一个" $SProp
      ": 根据OFE-MONO, " $SProp
      "所要求的封闭性质是满足的. 显然, 如果"
      $f "是一个非扩张性函数, 那么"
      (&sube (setI $n (&eqn $x $y))
             (setI $n (&eqn (app $f $x) (app $f $y))))
      ", 这意味着" $f "的非扩张性允许我们在"
      $f "下执行步进索引相等的重写.")
   (P (B "链和完备OFE. ")
      "OFE的步骤索引相等给出了链的概念, 其是一个序列"
      (_ (@ $c_n) (∈ $n $NN)) ", 变得"
      (Q "越来越相等") ":"
      (MB (∀ (&cm $n $m)
             (&=> (&<= $n $m)
                  (&eqn $c_m $c_n))))
      "一个链有一个极限, 如果存在一个元素" $c_inf
      ", 其随着" $n "的增长越来越等于" $c_n ":"
      (MB (∀ $n (&eqn $c_n $c_inf)))
      "一个OFE " $T "被称为是完备的 (COFE), 如果每个"
      $T "中的链都有一个极限 (OFE-COMPL).")
   (P "链是有趣的, 因为其可以用来将对象定义为逼近序列的极限, "
      "而非直接定义对象. 例如, "
      "第5.5节我们将会看到其可以用来取函数的不动点. "
      "目前所有讨论的OFE都是完备的.")
   (H3. "高阶一致构造")
   
   (H2. "Iris基逻辑")
   (H3. "分离逻辑")
   (P "连接词" $* "和" $wand
      "来自bunched implications (O'Hearn & Pym, 1999), "
      "它们使我们的基础逻辑成为一个分离逻辑: "
      "它们让我们能够对资源的所有权进行推理. "
      "关键点在于, " (&* $P $Q)
      "描述的是对一个可以被分离为两个不相交部分的资源的所有权, "
      "其中一部分满足" $P ", 另一部分满足" $Q
      ". 这与" (&conj $P $Q)
      "形成对比, 后者描述的是对一个同时满足"
      $P "和" $Q "的资源的所有权.")
   (P "例如, 考虑并发程序中不同线程所拥有的资源. "
      "由于这些线程并发运行, 它们的所有权必须是不相交的, "
      "这一点至关重要. 因此, "
      "分离合取是组合并发线程所有权的自然运算符.")
   (P "然而请注意, 基础逻辑并没有"
      (Q "线程") "或" (Q "程序")
      "的概念, 而只有分离的概念. "
      "归根结底, 该逻辑并不关心"
      (Q "谁") "拥有所考虑的资源; "
      "它只关心对所有权的推论进行推理, "
      "以及所有权如何被不相交地合并与拆分. "
      "与线程的联系将在我们于基础逻辑中"
      "编码Hoare三元组时建立 (见§6.3和§7.3).")
   (P "与分离合取一起, 我们有第二种形式的推出: 魔杖"
      (&wand $P $Q) ". 它描述的是对" (Q $Q "减去" $P)
      "的所有权, 即它描述的是这样的资源: "
      "如果你 (不相交地) 添加满足" $P
      "的资源, 你就得到满足" $Q "的资源.")
   (P "这与推出" (&=> $P $Q)
      "形成对比, 在后者中, 不是添加一个满足"
      $P "的不相交资源, 而是要求同一个资源额外地 (在"
      (Q "普通") "合取的意义上) 满足"
      $P ". 换言之, 要消去一个推出, 我们使用规则"
      (!- (&conj $P (@=> $P $Q)) $Q)
      ", 但要消去一个魔杖, 我们需要要求分离合取, 如"
      (!- (&* $P (@wand $P $Q)) $Q) ".")
   (P "分离合取的模型是直觉主义分离逻辑通常的模型 (Reynolds, 2000), "
      "只是稍作调整以适应步骤索引 (step-indexed) 的设定. "
      "与Kripke语义 (Kripke, 1965) 中通常的情形一样, "
      "推出和魔杖都必须在更小的步骤索引和更大的资源下封闭.")
   (H3. "资源所有权")
   
   (H3. "持久模态和持久资源")
   (P "在§2.3中, 我们解释过某些命题是持久的, "
      "意思是它们永远成立, 并且它们可以被任意复制. "
      "例如, 有效性, 相等, 以及对一个core的所有权都是持久命题. "
      "这种持久的概念在Iris中借助持久性模态"
      $Persistently "来表达.")
   (P "持久性模态" (Persistently $P)
      "的目的是说" $P "成立而不断言任何排他的所有权. "
      "其建模方式是: 定义" (Persistently $P)
      "对某个资源" $a "成立, 如果"
      $P "对" (Core $a)
      ", 即" $a "的core, 成立:"
      (MB (&def= (deno (&: (Persistently $P) $iProp)
                       $rho)
                 (Lam $a (app (deno (&: $P $iProp)
                                    $rho)
                              (Core $a)))))
      "因此, 假设" (Persistently $P)
      "可以被任意多次使用, 即不能被" (Q "用完")
      -- "它是持久的. "
      "我们现在可以定义一个命题是持久的意味着什么:"
      (MB (&def= (Persistent $P)
                 (!- $P (Persistently $P)))))
   (P "仅使用图11中的规则, 很容易验证有效性"
      (&Valid $a) ", 相等" (&= $t $u)
      ", 对一个core的所有权" (&Own (Core $a))
      ", 以及持久性模态" (Persistently $P)
      "本身都是持久的. 此外, "
      "很容易验证任何持久命题" $P "都享有: "
      (&<=> (&* $P $P) $P (Persistently $P))
      "以及" (&<=> (&* $P $Q) (&conj $P $Q)) ".")
   (P "持久性模态提供了比仅仅拥有持久命题的概念"
      "更多的灵活性: 我们利用这种灵活性来定义视图转换 "
      "(view shift) (§7.2) 和Hoare三元组 (§6), "
      "使得它们是持久的. 现在, 我们将给出一个简单的预告.")
   (P "考虑命题" (&wand $P $Q)
      ". 这个命题一般只能被应用 (以产生结果"
      $Q ") 一次, 因为它的证明可能依赖于"
      "某个被排他地拥有的资源. 然而, "
      "我们可以使用持久性模态来" (Q "标记")
      (&wand $P $Q)
      "的证明不依赖于任何排他的所有权. 这产生了"
      (Persistently (@wand $P $Q))
      ", 它是一个可以被复制并重复应用的持久"
      (Q "事实") ".")
   (P "显然, 这是有代价的: 在证明"
      (Persistently (@wand $P $Q))
      "时, 我们不能利用周围上下文中的任何非持久假设. "
      "这一点由以下" $Persistently
      "引入的派生规则所见证, "
      "它只允许在上下文是持久的时候引入持久性模态"
      $Persistently ":"
      (MB (&rull (: $Persistently "-INTRO")
                 (!- (Persistently $P) $Q)
                 (!- (Persistently $P)
                     (Persistently $Q))))
      "事实上, 这恰好就是线性逻辑中" $!
      "的通常引入规则. (对于Iris, "
      "我们转而选择了使用□-MONO和□-IDEMP的等价表述, "
      "但两者是等价的.) 换言之, 持久性模态很像"
      $! ", 只是它支持更多的一些操作 "
      "(□-CONJ以及与量词交换).")
   (H3. "基本更新模态")
   (P "到目前为止, 资源都是静态的: "
      "基础逻辑提供了命题来对你所拥有的资源, "
      "该所有权的推论, 以及所有权如何能被不相交地分离进行推理. "
      "一直缺少的是一种改变当前资源的方法. "
      "如§2.1中所讨论的, 资源更新的自然概念是帧保持更新 "
      "(frame-preserving update). "
      
      )
   (H3. "later模态")
   (H3. "守卫不动点")
   (H3. "timeless命题")
   (H3. "一致性")
   
   (H2. "最弱前条件")
   (H3. "操作语义")
   
   (H3. "证明规则")
   (H3. "最弱前条件的定义")
   (H2. "恢复iris程序逻辑")
   (H2. "讨论")
   (H2. "相关工作")
   (P "在§9.1中, 我们将讨论与Iris 1.0的两大支柱相关的工作: "
      "用户定义的ghost状态和不变式. "
      "§9.2和§9.3涵盖与Iris 2.0的新特性相关的工作: "
      "高阶ghost状态, 以及从PCM到RA/camera的推广. "
      "在§9.4中, 我们将与那些和我们的Iris 3.x"
      "基础逻辑目标相近的工作进行比较. "
      "最后, 在§9.5中, 我们讨论为什么Iris"
      "是一个仿射/直觉主义的 (而非线性/经典的) 分离逻辑.")
   (H3. "用户定义幽灵状态和不变量")
   (P "Iris提供用户定义的ghost状态的主要手段, "
      "是将逻辑以一个用户选定的部分交换幺半群 "
      "(partial commutative monoid, PCM) 为参数 "
      "(在Iris 2.0中我们将其推广为用户选定的camera). "
      "将PCM用于这一目的并不完全出人意料, 毕竟: "
      "自分离逻辑最早的模型起, 它们就已被使用. "
      "因此, 早在Iris 1.0之前, "
      "就已有许多不同的模型和逻辑开始采用PCM, "
      "作为刻画更细粒度的干涉概念的一种方式 "
      "(Krishnaswami et al., 2012; "
      "Dinsdale-Young et al., 2013; "
      "Ley-Wild & Nanevski, 2013). "
      "特别是Views框架 "
      "(Dinsdale-Young et al., 2013), "
      "它使用户能够将ghost资源" --
      "由用户定义的PCM表示" --
      "与物理资源绑定起来.")
   (P "然而, Views框架的局限在于, "
      "它实际上要求用户预先固定一个"
      "将ghost资源与物理资源绑定的不变式, "
      "而在逻辑上不支持在这些ghost资源之上叠加更多的不变式. "
      "与之相对, Iris为用户定义的不变式提供了内建的逻辑支持, "
      "这使得人们可以将 (ghost的和物理的) 资源彼此关联起来. "
      "与此相关, 在Verified Software Toolchain (VST) (Appel, 2014) 中, "
      "人们可以为ghost状态选择一个固定的PCM (实际上是一个"
      (Q "分离代数") " (separation algebra)), "
      "由此构造出一个高阶分离逻辑. 然而, "
      "所选ghost状态的原始规则必须在逻辑的模型中证明, "
      "而在Iris中, 这些规则可以从少数几条基本证明规则推导出来.")
   (P "通过不变式关联 (ghost的和物理的) 资源的想法, "
      "在Iris 1.0之前发表的工作中就已出现" --
      "例如, Pilkiewicz和Pottier关于"
      (Q "单调状态") " (monotonic state) "
      "的工作 (Pilkiewicz & Pottier, 2011), "
      "Jensen和Birkedal关于" (Q "虚构分离逻辑")
      " (fictional separation logic) 的工作 "
      "(Jensen & Birkedal, 2012), "
      "以及Krishnaswami等人关于"
      (Q "表面子结构类型")
      " (superficially substructural types) "
      "的工作 (Krishnaswami et al., 2012). "
      "然而, 所有这些早期工作都局限于顺序的设定.")
   (P "在并发的设定中, Iris 1.0之前的工作, "
      "如CaReSL (Turon et al., 2013), "
      "iCAP (Svendsen & Birkedal, 2014) "
      "和TaDA (da Rocha Pinto et al., 2014), "
      "通常使用一个特定的PCM构造来固定其ghost状态. "
      "Iris的用户定义ghost状态与不变式的组合"
      "可以用来编码这些逻辑中的推理模式, "
      "但所用的原始机制和证明规则更为简单.")
   (P "大约在Iris 1.0开发的同一时期, "
      "其他几个逻辑也集成了对用户定义的ghost状态的支持. "
      "值得注意的是, GPS (Turon et al., 2014) 同样使用了PCM, "
      "而FCSL使用了一种称为" (Q "concurroid")
      "的抽象 (Nanevski et al., 2014).")
   (P "Iris与FCSL之间一个有趣的表达能力差异在于对不变式"
      (Q "隐藏") " (hiding) 的支持. "
      "FCSL支持某种隐藏, 即能够将一些局部状态转移到一个不变式 "
      "(实际上是一个" (Q "concurroid")
      ") 中, 该不变式在单个表达式" $e
      "的执行期间被强制维持, 而在此之后, "
      "由该不变式管辖的状态被交还给局部控制. "
      "Iris也可以支持这种隐藏, 通过可取消的不变式 "
      "(cancellable invariant) (§7.1.3). "
      "此外, Iris允许另一种隐藏, 即能够隐藏被 (嵌套的) "
      "Hoare三元组规约所使用的不变式. 例如, "
      "§2中的高阶函数mk_oneshot返回两个函数 (tryset和check), "
      "它们的Hoare三元组规约只在不变式" $I
      " (该不变式在mk_oneshot的执行过程中建立) 之下才是正确的. "
      "由于Iris中的不变式是持久命题 (persistent proposition), "
      $I "可以被隐藏, 即它无需污染mk_oneshot, tryset或check的规约. "
      "据我们所知, FCSL不支持这种形式的隐藏.")
   (P "与Iris一样, Cohen et al. (2009) "
      "致力于为并发推理提供一个极简的基础, "
      "但他们的基础建立在ghost堆和双状态不变式之上, "
      "而我们的则建立在任意的ghost PCM和单状态不变式之上. "
      "这两种方法针对不同的目标进行优化. "
      "我们的逻辑是子结构的, 并支持更具表达力的" --
      "例如高阶的" -- "规约. 他们的则不是, "
      "这使其更适合与SMT求解器等自动验证工具配合使用.")
   (H3. "高阶ghost状态")
   (P "正如我们刚才所见, 一些早于Iris 2.0的逻辑, "
      "如GPS (Turon et al., 2014) "
      "和FCSL (Nanevski et al., 2014), "
      "像Iris 1.0一样支持用户定义的ghost状态. "
      "然而, 这些逻辑都局限于一阶ghost状态, "
      "因为ghost状态的结构在逻辑被实例化之前就已定义并固定.")
   (P "其他先前的工作采用了一种可以被视为涉及二阶ghost状态的模型, "
      "例如, 用以证成它们对动态分配的锁 "
      "(Gotsman et al., 2007; "
      "Hobor et al., 2008; Buisse et al., 2011), "
      "区域 (Svendsen & Birkedal, 2014) "
      "或不变式 (Jung et al., 2015) 的使用. "
      "然而, 这种ghost状态具有固定的结构, "
      "而不是像camera那样作为某个通用代数结构的实例.")
   (P "在一些早于Iris 2.0的逻辑中, "
      "尽管ghost状态的结构是固定的, "
      "它有时也能被巧妙地改造以支持一系列证明模式. "
      "例如, Dodds et al. (2016) 展示了iCAP逻辑 "
      "(Svendsen & Birkedal, 2014) 中内建的"
      (Q "协议") " (protocol) 机制可以"
      "被重新利用来编码命名命题 "
      "(named proposition) (§3.3), "
      "而这一功能并非它原本的设计意图. "
      "然而, 这种编码有些人为, "
      "必须通过直接诉诸iCAP的模型来验证, "
      "并且无法扩展到支持Iris 2.0中引入的"
      "一般高阶ghost状态 (Jung et al., 2016).")
   (P "Verified Software Toolchain "
      "(VST) (Appel, 2014) "
      "是另一个在资源种类上通用的逻辑. 它应用"
      (Q "间接理论") " (indirection theory) "
      "(Hobor et al., 2010) 来求解递归域方程, "
      "这些方程与§4中的方程IRIS非常相似. "
      "尽管VST已被证明支持特定形式的二阶ghost状态, "
      "但目前尚不清楚间接理论与基于COFE的范畴论方法究竟如何比较. "
      "我们相信VST有可能以Iris 2.0的方式被推广, "
      "以支持更一般的高阶ghost状态. 值得注意的是, "
      "VST有一种与老化 (aging) (步索引 (step-indexing)) "
      "相容的类PCM结构的概念, 它们似乎大致对应于camera.")
   (H3. "PCM的推广")
   (P "在先前的工作中, 已有若干种对PCM的推广的表述, 它们具有"
      (Q "多个单位元 (multiple units)") ", 或包含"
      (Q "可复制核 (duplicable core)") "的概念.")
   (P "Dockins et al. (2009) 引入了"
      (Q "多单位元分离代数 "
         "(multi-unit separation algebras)")
      ", 与PCM不同, 它只要求对任意元素" $a
      ", 存在一个可能各不相同的单位元" $u_a
      ", 满足" (&= (&d* $u_a $a) $a)
      ". 多单位元分离代数与RA之间一个显著的差异在于, "
      "我们将幺半群运算" $d* "和核" (Core $dummy)
      "表述为函数, 而他们将这些运算表示为关系. "
      "像我们这样将这些运算视为函数, "
      "使人们能够进行等式推理, "
      "这使得进行证明更为方便.")
   (P "(可复制) 核这一术语改编自Pottier, 他在"
      (Q "单调分离代数 (monotonic separation algebras)")
      "的语境中引入了它 (Pottier, 2013). "
      "然而, Pottier的核的公理, 以及其他工作 "
      "(Turon et al., 2014; Appel, 2014) 中相关概念的公理, "
      "与我们的RA概念的公理有所不同. "
      "一些共同的性质出现在这些工作中的大多数里 "
      "(或者作为公理, 或者作为可容许规则): "
      "核必须产生一个单位元 (RA-CORE-ID), "
      "必须是幂等的 (RA-CORE-IDEM), "
      "并且必须是一个同态, 即与复合相容: "
      (&= (Core (&d* $a $b))
          (&d* (Core $a) (Core $b)))
      ". 最后这条性质强于我们的单调性公理 (RA-CORE-MONO), "
      "因此, RA的公理弱于先前工作所要求的公理. "
      "事实上, RA是严格更弱的: 我们最重要的RA构造之一, "
      "状态转移系统 (state-transition system, STS), "
      "具有一个不是同态的核. 这表明, "
      "像先前工作那样要求核是一个同态, "
      "会排除掉有用的实例.")
   (P "另一个差异在于, 我们的核可以是部分的, "
      "而在先前的工作中它总是一个全函数. "
      "正如§3.1中所讨论的, 部分核使得从和 (sum) "
      "等更简单的构造出发复合出RA更加容易.")
   (P "最后, Bizjak and Birkedal (2017) 已经证明, "
      "我们的核的概念恰恰是获得一个性质良好的持久命题 "
      "(persistent proposition) 概念所需要的.")
   (H3. "极简基础逻辑")
   (P "缓解最先进的分离逻辑在模型构造上的困难的最引人注目的尝试, "
      "是Svendsen和Birkedal关于iCAP逻辑的工作 (2014). "
      "他们在树的拓扑斯 (topos of trees) "
      "的内部逻辑中定义了iCAP逻辑的模型, "
      "该内部逻辑包含一个later连接词, 用以抽象地推理步索引, "
      "即步索引不会显式地出现在逻辑的连接词在模型中的解释里. "
      "然而, 他们的Hoare三元组的模型仍然涉及显式的资源管理, "
      "而我们的则不涉及.")
   (P "在光谱的另一端, 已有一些在并发分离逻辑中编码二元逻辑关系 "
      "(binary logical relation) 的工作 "
      "(Dreyer et al., 2010; Turon et al., 2013; "
      "Krogh-Jespersen et al., 2017; Krebbers et al., 2017b). "
      "这些二元逻辑关系的编码依赖于一个外围的分离逻辑, "
      "而该逻辑已经包含了大量的高层概念, "
      "例如最弱前置条件 (weakest precondition) "
      "和视图转换 (view shift). 与之相对, "
      "我们的目标恰恰是用更简单的术语来定义这些概念.")
   (P "FCSL (Sergey et al., 2015) "
      "采取了与我们的基础逻辑相反的方法. "
      "为了简化在证明助手中对程序的推理, "
      "他们尽可能地避免在分离逻辑中推理, "
      "而主要在外围逻辑 (在他们的情形中, "
      "即Coq的逻辑) 中推理. "
      "这要求模型保持尽可能简单; "
      "特别地, FCSL不使用步索引. "
      "其结果是, 他们不支持非直谓不变式 "
      "(impredicative invariant), "
      "而我们认为这是Iris的一个重要特性. "
      "例如, 它们被用来建模非直谓类型系统 "
      "(Krebbers et al., 2017b), "
      "或建模一个可重入的事件循环库 "
      "(Svendsen & Birkedal, 2014). "
      "此外, 正如我们在近期的工作 "
      "(Krebbers et al., 2017b) 中所展示的, "
      "人们实际上可以在Coq中"
      "便捷地在分离逻辑里进行推理, "
      "因此我们的模型的额外复杂性"
      "对我们逻辑的用户而言几乎是不可见的.")
   (P "Verified Software Toolchain "
      "(VST) (Appel, 2014) 是一个框架, "
      "它提供了在Coq中构造支持非直谓不变式"
      "的复杂高阶分离逻辑的机制. 然而, "
      "VST不是一个逻辑, 因此, "
      "它并不对步索引和资源进行抽象. "
      "因而, 在VST中定义一个程序逻辑仍然"
      "需要对这些细节进行大量的手工管理, "
      "而在Iris基础逻辑中定义程序逻辑时, "
      "这些细节被抽象掉了. "
      "使用VST构建的逻辑通常为其目的"
      "固定一个特定的ghost状态, "
      "并为这个特定的ghost状态提供原始规则, "
      "而Iris的设计是让这类证明规则"
      "在单个逻辑内部从少数几条基本证明规则推导出来. "
      "其结果是, 使用不同ghost状态的Iris证明"
      "可以被安全地组合 (§3.2), "
      "而在不同的VST逻辑中完成的证明不一定能够互操作. "
      "此外, VST迄今为止只在顺序推理和粗粒度 "
      "(基于锁的) 并发的语境中得到展示 "
      "(Beringer et al., 2014), "
      "而Iris的重点在于细粒度并发.")
   (H3. "仿射与线性分离逻辑")
   (P "正如我们在§2中所指出的, "
      "Iris是一个仿射分离逻辑, "
      "意思是我们总是可以丢弃资源" --
      "即, 所有资源最多只能被使用一次. "
      "形式化地说, 这意味着对每个命题"
      $P "和" $Q ", 我们有:"
      (MBL "(SEP-AFFINE)"
           (!- (&* $P $Q) $P))
      "满足上述规则的分离逻辑常被称为"
      (Q "直觉主义的 (intuitionistic)")
      ", 因为它们不容许排中律 ("
      (!- $True (&disj $P (&neg $P)))
      "): 由排中律和SEP-AFFINE"
      "可以证明分离合取是退化的, 即对任意"
      $P "和" $Q ", 有"
      (&<=> (&conj $P $Q) (&* $P $Q))
      ". 反之, 不满足规则SEP-AFFINE"
      "的分离逻辑常被称为"
      (Q "经典的 (classical)") "或"
      (Q "线性的 (linear)")
      ", 因为资源应当恰好被使用一次.")
   (P "读者当然可能会好奇, "
      "为什么Iris是仿射/直觉主义的, "
      "而不是线性/经典的. 这有多方面的原因:"
      (Ul (Li "包含later模态的逻辑尽管是线性的, "
              "也不容许排中律: 由排中律和LÖB归纳可以证明"
              $later "是退化的, 即对任意" $P ", 有"
              (&<=> (Later $P) $True) ".")
          (Li "线性分离逻辑的传统优势在于对资源更精确的追踪: "
              "不能丢弃资源使人们能够推理资源泄漏的不存在. "
              "例如, 在传统的经典分离逻辑中, "
              "可以保证已分配的内存都已被释放, "
              "因为人们不能丢弃points-to谓词"
              (PointsTo $ell $v)
              ". 然而, 据我们所知, "
              "对于任何现有的细粒度并发逻辑 "
              "(Nanevski et al., 2014; Svendsen & Birkedal, 2014; "
              "da Rocha Pinto et al., 2014), "
              "这一性质都从未被证明过. 此外, "
              "对于具有非直谓不变式的逻辑 "
              "(Hobor et al., 2008; Svendsen & Birkedal, 2014), "
              "尽管是线性的, 也可以将一个不变式存储在"
              (Q "它自身内部") ", 这又使得泄漏不变式"
              "以及它们所包含的所有资源成为可能. "
              "因此, 在上述逻辑中, 就追踪资源泄漏而言, "
              "人们在形式上并不能从线性中获得任何东西.")))
   (P "尽管如此, 即使是在非直谓并发分离逻辑中进行推理时, "
      "线性也可以是有用的, 例如, "
      "用来建立精化 (refinement) 性质 "
      "(Tassarotti et al., 2017), "
      "或用资源来管理义务 (obligation) "
      "(Bizjak et al., 2018).")
   (H2. "Iris正在进行和未来的发展")
   (P "在本文中, 我们试图从基础出发, "
      "描绘一幅当今Iris的全面图景, "
      "但这当然不是关于这一主题的定论. "
      "Iris框架在持续开发中, 尽管我们"
      "并不预期我们所呈现的Iris基础会发生重大变化, "
      "我们一直在积极探索若干扩展, "
      "它们将进一步拓宽Iris的一般性和表达能力.")
   (P "首先, 我们正在探索将线性资源 (§9.5) 纳入Iris的方式. "
      "我们希望找到Iris的一个合适的推广, "
      "使得Iris的用户能够控制哪些资源是仿射的或线性的, "
      "沿着Cao et al. (2017) 近期工作的思路. "
      "此外, 我们希望确定需要对非直谓不变式施加哪些限制, "
      "才能使Iris被用来推理资源泄漏的不存在.")
   (P "在线性之外, 我们尝试过为不依赖于任何资源 "
      "(即使是可复制的资源) 的断言添加一个新的"
      (Q "平凡模态 (plainness modality)")
      ". 这样的" (Q "平凡 (plain)")
      "断言在与更新模态 (update modality) "
      "结合时满足更强的证明规则 (Timany et al., 2018). "
      "我们一直致力于推广我们的Coq基础设施, "
      "即交互式证明模式 (interactive proof mode) "
      "(Krebbers et al., 2017b), "
      "以处理更多种类的逻辑 (Krebbers et al., 2018), "
      "这带来了Iris模型的一个更模块化的定义. "
      "最后, 我们在两个方面扩展了最弱前置条件: "
      "支持对可能卡住 (get stuck) 的程序进行推理, "
      "这在与不可信代码交互的应用中很有用 "
      "(Swasey et al., 2017); "
      "以及支持对程序完全正确性 "
      "(total correctness) 进行推理.")
   (H2 "致谢")
   (P "本研究部分得到了以下资助: 欧洲研究理事会 "
      "(European Research Council, ERC) 为项目"
      (Q "RustBelt") "提供的Consolidator Grant, "
      "该资助来自欧盟Horizon 2020框架计划 (资助协议编号683289); "
      "以及丹麦独立研究理事会自然科学分会 "
      "(The Danish Council for Independent Research "
      "for the Natural Sciences, FNU) "
      "的ModuRes Sapere Aude Advanced Grant.")
   (P "我们感谢Philippa Gardner提供的有益反馈, "
      "以及Rose Hoberman提供的编辑建议. "
      "我们还要向所有为Iris框架的开发, "
      "实现和应用做出贡献的人致以诚挚的谢意.")
   
   ))