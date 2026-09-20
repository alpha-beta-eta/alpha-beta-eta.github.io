#lang racket
(provide types.html)
(require SMathML)
(define okmij "https://okmij.org/ftp/")
(define (okmij/ path) (string-append okmij path))
(define (Ref-link title path)
  (LINK title (okmij/ path)))
(define-syntax-rule (define-code* (id str) ...)
  (begin
    (define id (Code str))
    ...))
(define-code*
  (:int "int")
  (:lfix-calc.elf "lfix-calc.elf")
  (:fork "fork(2)")
  (:Memory "Memory")
  (:Ref-a "Ref a")
  (:new "new")
  (:get "get")
  (:set "set")
  (:Data.STRef "Data.STRef")
  (:newSTRef "newSTRef")
  (:readSTRef "readSTRef")
  (:writeSTRef "writeSTRef")
  (:Data.IntMap "Data.IntMap")
  (:=== "===")
  (:Dynamic "Dynamic")
  (:Int "Int")
  (:fromDynamic-type "fromDynamic :: Dynamic -&gt; Maybe a")
  (:fromDynamic "fromDynamic")
  (:a "a")
  (:Nothing "Nothing")
  (:unsafeCoerce "unsafeCoerce")
  (:r "r")
  (:m "m")
  (:get-r "get r")
  (:set-r "set r")
  (:new-x-m "new x m")
  (:get-r-m^ "get r m'")
  (:set-r-y-m^ "set r y m'")
  (:f-1 "f 1")
  (:f "f")
  (:z "z")
  (:y "y")
  (:exp "exp")
  (:1 "1")
  (:empty-string "\"\"")
  (:empty-list "[]")
  (:True "True")
  (:False "False")
  (:callcc "call/cc")
  (:shift "shift")
  (:reset "reset")
  (:let "let")
  (:gensym "gensym")
  (:NJ "&lt;NJ&gt;"))
(define types.html
  (TnTmPrelude
   #:title "类型"
   #:css "styles.css"
   (H1. "类型")
   (P "本文译自Oleg Kiselyov的"
      (Ref-link "Types" "Computation/types.html")
      ".")
   (Ul (Li (LINK "OCaml类型检查器如何工作" "generalization.html")
           -- "或者说, 多态和垃圾回收有何共同之处")
       (Li (Ref-link "实现并理解类型类" "Computation/typeclass.html"))
       (Li (Ref-link "将类型解释为抽象值:" "Haskell/AlgorithmsH.html#teval")
           "Hindley-Milner类型推断教程")
       (Li (Ref-link "Leibniz相等终究可以被做成单射的"
                     "Computation/extra-polymorphism.html#injectivity"))
       (Li (Ref-link "更简单类型的语言中的高级多态"
                     "Computation/extra-polymorphism.html")))
   (Ul (Li (Ref-link "let多态, 值限制及其放宽的动机和发展"
                     "meta-programming/calculi.html#staged-poly-gen"))
       (Li (LINK "没有诡辩的会话类型" "#sessions"))
       (Li (LINK "类型推断和不死代码" "#zombie"))
       (Li (LINK "作为小步抽象求值的类型检查" "#small-step-typechecking"))
       (Li (LINK "第一级内存的挑战" "#first-class-memory")))
   (Ul (Li (Ref-link "轻量级静态保证" "Computation/lightweight-static-guarantees.html"))
       (Li (Ref-link "诱人的Haskell类型:" "Haskell/types.html")
           "Haskell中的依赖, 高秩, 存在等类型以及逻辑类型编程")
       (Li (Ref-link "规范结构:" "ML/canonical.html") "可编程的重载解析"))
   (Ul (Li (Ref-link "可扩展效应和可扩展类型状态" "Haskell/extensible/"))
       (Li (Ref-link "多态的静动态平衡AVL树" "Haskell/types.html#stanamic-AVL"))
       (Li (Ref-link "真正的依赖类型以及在类型安全的printf的例子上伪造它们"
                     "typed-formatting/index.html#dep-type"))
       (Li (Ref-link "抽象与多态:" "Computation/lambda-calc.html#haskell-type-level")
           "三行Haskell写成的类型层面lambda计算器"))
   (Ul (Li (LINK "逆类型检查以及直觉主义逻辑和经典逻辑中的定理证明" "#typechecker-CH"))
       (Li (Ref-link "以数字为参数的类型和十进制类型算术"
                     "Haskell/number-parameterized-types.html"))
       (Li (Ref-link "类型可靠性的句法方法, Twelf中的证明"
                     "formalizations/index.html#type-soundness-small-step"))
       (Li (Ref-link "第一级模块: Leibniz相等, GADT和泛型编程"
                     "ML/first-class-modules/")))
   (Ul (Li (Ref-link "用同构的简单类型表示存在数据类型"
                     "Computation/Existentials.html"))
       (Li (LINK "如何具体化新鲜类型变量?" "#type-gensym"))
       (Li (Ref-link "ANSI C中不使用强制转换的全称多态列表和多态FOLD"
                     "cpp-digest/poly-list.html"))
       (Li (Ref-link "有类型无标签解释和有类型编译" "tagless-final/")
           "类型检查, 分阶段和依赖类型"))
   (H2. "作为小步抽象求值的类型检查" #:id "small-step-typechecking")
   (P "我们阐述一种将类型检查视为以" (Q "抽象值") "进行求值的观点. "
      "动态语义, 即求值, 处理的是像0, 1等等这样的(动态)值, "
      "而静态语义, 即类型检查, 处理的是像" :int "这样的近似. "
      "一个类型系统是可靠的, 如果它正确地近似了动态行为并预测其结果: "
      "如果静态语义预测一个项具有类型" :int ", 那么该项的动态求值, 如果终止的话, 将产生一个整数.")
   (P "传统的类型检查是抽象层面上的大步求值: 为了找到一个表达式的类型, "
      "我们将其直接子项完全" (Q "求值") "为它们的类型. "
      "我们提出一种不同的类型检查, 它是抽象层面上的小步求值: "
      "它将一个表达式拆开 (unzip, 双关是有意的: 参见Huet的zipper) 为一个上下文和一个可约式. "
      "论文中的类型检查算法是用Twelf实现的; 完整的代码可以获得. "
      "特别地, 注释详尽的文件" :lfix-calc.elf
      ", 作为引入和比较, 实现了简单类型lambda演算的小步类型检查.")
   (P "小步类型检查的好处在于, 通过" (Q "跟随") "控制流, 它更适合于有效应的计算. "
      "一个更有趣的结果来自于这样一个事实: 静态求值与动态求值不同, 它会进入"
      (Q "lambda之下") ". 因此小步静态求值上下文是一个绑定上下文.")
   (P "冒号成为了新的十字转门! 由于求值上下文既不是交换的也不是结合的, "
      "而是服从于聚焦规则, 我们得到了表达各种子结构逻辑和模态逻辑的框架.")
   (P "与Chung-chieh Shan的合作工作.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为1.1, 2007年6月.")
    (Dt (B "参考文献"))
    (Dd
     (LINK "定界延续的一个子结构类型系统" "delimited_control.html#delimcc-type")
     (Br) "这篇论文在为动态定界延续设计第一个可靠的类型系统时引入了小步类型检查."
     (P (Ref-link "small-step-typechecking.tar.gz"
                  "Computation/small-step-typechecking.tar.gz")
        " [10K]"
        (Br) "带注释的Twelf代码, 包含小巧的小步类型检查算法和许多样例推导.")
     (P "指称语境下的抽象解释:"
        (Br) "Patrick Cousot: Types as abstract interpretations. POPL 1997, 316-331.")
     (P "作为逻辑程序的程序开发过程中的辅助的抽象解释:"
        (Br) "Manuel Hermenegildo, Germ&aacute;n Puebla, and Francisco Bueno: "
        "Using global analysis, partial specifications, and an extensible "
        "assertion language for program validation and debugging."
        (Br) "In: The logic programming paradigm: A 25-year perspective, "
        "ed. Krzysztof R. Apt, Victor W. Marek, Mirek Truszczynski, "
        "and David S. Warren, 161-192, 1999. Berlin: Springer-Verlag.")
     (P (Ref-link "将类型解释为抽象值:" "Haskell/AlgorithmsH.html#teval")
        " Hindley-Milner类型推断教程")))
   (H2. "没有诡辩的会话类型" #:id "sessions")
   (P "普通的类型近似的是结果, 而会话类型近似的是计算之间的通信. "
      "会话类型之所以吸引人, 是因为它们可以被静态地推断/检查, "
      "也因为良会话类型的程序" (Q "不会出错")
      ": 计算不会发送其通信方没有准备好处理的数据或命令, "
      "不会试图使用已关闭的或已委托出去的信道.")
   (P "会话类型是类型状态的一种形式: 它们不仅描述现在通信的是什么, "
      "也描述接下来要通信的是什么. 在像OCaml这样的普通编程语言中编写会话类型的程序, "
      "需要不普通的巧妙来模拟类型层面的计算和线性定型" --
      "这意味着实现和错误信息都非常难以理解. 人们会不断地想起C++中的模板元编程.")
   (P "我们呈现一个系统, 它探索了一种非常不同的会话定型方法: "
      "将类型层面的诡辩降低为普通的编程, 同时保持静态保证. "
      "错误信息详细而可定制, 并且可以使用普通的调试器来调查会话类型问题. "
      "我们的系统是一个用于OCaml中面向服务编程的二元会话类型的DSL, "
      "支持多个通信信道, 内部选择和外部选择, 递归, 以及信道委托.")
   (P "关键想法是分阶段: 从被生成程序的角度来看, 生成器中普通的运行时检查扮演了"
      (Q "类型检查") "的角色. 对于后者而言是花哨类型的东西, 对于生成器而言是普通的数据.")
   (P "与今井敬吾 (Keigo Imai, 岐阜大学) 的合作工作.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为2020年2月.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "description.pdf" "meta-programming/sessions/description.pdf") " [332K]"
     (Br) "发表于Lecture Notes in Computer Science, vol 12073, pp. 66-87, 2020的论文 "
     (LINK "doi:10.1007/978-3-030-59025-3_5" "http://doi.org/10.1007/978-3-030-59025-3_5")
     (P (Ref-link "flops2020-slides.pdf" "meta-programming/sessions/flops2020-slides.pdf")
        " [790K]"
        (Br) "今井敬吾. 在Functional and Logic Programming (FLOPS) 2020上所作报告的幻灯片. "
        "2020年9月15日")
     (P (Ref-link "0README.dr" "meta-programming/sessions/0README.dr") " [2K]"
        (Br) "当前代码的索引: 用于通信进程的嵌入式DSL; 包括信道委托在内的若干例子; "
        "带有会话类型推断和进程代码生成的DSL实现.")
     (P (Ref-link "ppl-poster.pdf" "meta-programming/sessions/ppl-poster.pdf") " [90K]"
        (Br) "在Programming and Programming Languages研讨会PPL 2019上的海报. "
        "2019年3月6-8日, 志戸平温泉 (日本岩手县花巻市).")))
   (H2. "第一级内存的挑战" #:id "first-class-memory")
   (P (Em "第一级内存") " (或者说, 第一级存储) 是一个表示一个可变内存区域的程序值. "
      "换言之, 它是一个可变单元的集合" -- "带有分配新单元, 获取和更新单元内容的接口. "
      "第一级内存有助于实现带版本的数组, 重放调试和嵌套事务 (Morrisett, 1993) 等等. "
      "它在非确定性编程和概率编程中特别有用. 非确定性选择在许多方面类似于Unix的" :fork
      ", 将当前的计算进程分裂为若干个, 每个选择的备选项一个. 每个进程都有它自己的内存, "
      "其内容从父进程继承, 但可以在不影响其他进程的情况下被修改"
      -- "从而确保独立的非确定性选择确实被独立地求值. 正如在Unix中一样, "
      "复制进程内存最好惰性地进行, 即" (Q "写时复制")
      ". 为了管理进程内存和写时复制, Unix内核维护一个特殊的数据结构. "
      "第一级内存就像这样的数据结构, 让普通程序员能够做类似内核的内存管理.")
   (P "第一级内存至少有以下接口 (下面以Haskell描述):"
      (CodeB "type Memory       -- the abstract type of first-class memory
empty :: Memory        -- allocate new, initially empty memory
size  :: Memory -> Int -- get the current memory size (the number of allocated cells)

type Ref a        -- the abstract reference to a memory cell holding a value of type a
new  :: a -> Memory -> (Ref a, Memory)
get  :: Ref a -> Memory -> a
set  :: Ref a -> a -> Memory -> Memory")
      "该接口类似于熟悉的" :Data.STRef "接口, 带有" :newSTRef ", " :readSTRef ", "
      :writeSTRef "运算. 然而, 我们的" :new ", " :get "和" :set
      "显式地使用了单元所在(或将要所在)的" :Memory ".")
   (P "该接口附带有性能要求: " :get "和" :set
      "的执行时间 (或者更精确地说, 对真实计算机内存的访问次数) 不应超过一个小常数. "
      "换言之, " :Memory "的实现应该至少与" :Data.IntMap "的trie一样高效.")
   (P "挑战在于高效地实现该接口" -- "并且同时静态地保证某种程度的正确性. "
      "正确的实现应该满足熟悉的定律:"
      (CodeB "let (r,m') = new x m in get r m'  === x           for all x.
let m' = set r' x m in get r m'   === get r m     if r /= r', r' exists
let m' = set r x m  in get r m'   === x           if r exists
let m' = set r x m in set r y m'  === set r y m")
      "即: 我们得到我们所存储的; 在一个内存单元中存储会覆盖其先前的值, 而不影响其他单元. "
      "在类型中确保这些定律似乎需要过于复杂而无法实用的类型系统. "
      "尽管如此, 我们希望类型至少能够保证这些定律的一个近似, 其中" :===
      "被理解为将同一类型的所有值等同起来. 换言之, 我们想要确保对已存在的引用进行"
      :get "和" :set "是" (Em "完全的") "并且是保持类型的. "
      "我们想要使用类型, 使得正确性保证即使对于编译器也是清楚的, 编译器继而会将冗余的检查优化掉. "
      "实现带有这种定型保证的实用的第一级内存仍然是一个开放的挑战.")
   (P "让我们考虑一个直截了当的非解答. 回忆一下, " :Memory
      "应该存储任意类型的值. 第一个冲动是将内存单元表示为" :Dynamic ", 即"
      (Q "动态类型的") "值. 于是实现为"
      (CodeB "type Memory = IntMap Int Dynamic
type Ref a  = Int")
      "一个从位置 (" :Int ") 到" (Em "某个") "类型的值的有限映射. "
      "唉, 用于从这个" :Dynamic "引用单元读取的运算" :fromDynamic-type "是部分的. "
      :Dynamic "存储一个值及其类型的表示; " :fromDynamic "检查所请求的结果类型" :a
      "(的表示)是否与所存储的值的类型(表示)相匹配. 试图从" :Dynamic
      "中提取一个与所存储的类型不同的类型的值会返回" :Nothing ". 在正确实现的"
      :Memory "中, 对于已存在的引用的" :get "是完全的且保持类型的. 因此, "
      :fromDynamic "的类型匹配检查应该总是成功, 因而是冗余的. " :Dynamic
      "表示是不令人满意的, 因为它强加了一个永远不应失败的运行时检查. "
      "再说一遍, 挑战在于静态地确保正确性并避免运行时断言.")
   (P "除了性能之外, " :Dynamic "在理论上也不令人满意. 一个" :Dynamic
      "存储一个值及其类型的一个表示. 另一方面, 唯一地标识一个单元的内存地址"
      -- "内存单元的位置" -- "也唯一地描述了所存储的值的类型. 也就是说, 在给定的"
      :Memory "之内, 位置本身就是一个类型表示: 位置相等推出类型相等. 因此, "
      :Dynamic "中使用的类型表示是冗余的.")
   (P "避免冗余检查的实用主义的方式是使用" :unsafeCoerce
      ". 尽管它有助于性能, 但它迫使我们依赖纸笔的正确性证明. "
      "我们最好在证明中不犯任何错误, 否则就要冒段错误的风险.")
   (P "让我们回忆一下纸笔正确性论证的梗概, 来看看为什么它如此难以用类型表达. 首先, 引用单元的类型"
      :Ref-a "包含了所存储的值的类型. " :new ", " :get "和" :set
      "的类型签名表明, 一旦一个引用" :r "被创建来保存某个类型" :a "的值, "
      :get-r "和" :set-r "就只能读写该类型" :a "的值. 为了完成证明, 我们必须检查由"
      :new-x-m "创建的引用" :r "是" (Q "新鲜的") -- "它命名了一个与父内存" :m
      "中的任何单元都不同的单元" -- "并且" :get-r-m^ "和" :set-r-y-m^
      "确实访问的是那个单元" :r ". 因此" :get "和" :set "的正确性与第三个函数"
      :new "联系在一起: " :new "确实创建新鲜的引用. "
      "在类型中表达并使用这一新鲜性性质才是真正的挑战.")
   (Dl
    (Dt (B "参考文献"))
    (Dd
     "J. Gregory Morrisett: Refining First-Class Stores"
     (Br) "ACM SIGPLAN Workshop on State in Programming Languages, 1993"
     (P (Ref-link "Memory.hs" "Computation/Memory.hs") " [4K]"
        (Br) "概率编程语言Hakaru10中使用的第一级内存模块"
        (Br) "在这个实现中, 引用Ref充当所存储的值的缓存.")
     (P (Ref-link "使用第一级存储和第一级延续的概率编程"
                  "kakuritu/Hansei.html#ML2010"))))
   (H2. "类型推断和不死代码" #:id "zombie")
   (P "类型推断一般而言是非组合的: 它是依赖于上下文的. 类Hindley-Milner的类型推断, "
      "尤其是在其" (Q "收集约束并求解它们")
      "的表述中, 被明确地设计为让上下文影响/决定一个表达式 (例如一个裸变量) 的类型. "
      "尽管如此, 类型推断的非组合性有时确实会让我们吃惊. 例如, 当"
      (Q "显然已死的") "良类型代码影响一个表达式的可定型性" -- "甚至影响其结果的时候. "
      "本文来自于与Andreas Abel的一次讨论, 该讨论于2012年7月在Haskell-Cafe邮件列表上开始. "
      "他提到希望保存讨论中提出的例子. 本文就是为了实现这个愿望"
      -- "通过收集, 扩展和润色这些例子并建立联系.")
   (P (Q "死代码") "一般指这样一个子表达式, 它没有信息流" -- "既没有数据流也没有控制流"
      -- "流入表达式的结果. 在本文中, 我们专门用这个术语指无关定义: "
      "良类型的, 无副作用的, 没有信息流流入所关心的表达式的定义; "
      "被定义的标识符不会甚至不能出现在所讨论的表达式中.")
   (P "我们从一个无关定义使得表达式不可定型的最简单的, 众所周知的例子开始. "
      "在任何带有类Hindley-Milner推断的语言中, 人们肯定都见过这样的例子. "
      "具体起见, 我们使用下面这个OCaml的简单子集:"
      (CodeB "let exp =
    let id = fun u -> u in
    let f = id id in
    let z = fun () -> f true in
    f 1")
      "这段代码不能通过类型检查, 类型错误报告在" :f-1 "处. 然而, 如果我们移除" :z
      "的定义" -- "它显然没有出现在" :f-1 "之中" -- "后一个表达式就变成良类型的了.")
   (P "练习: 还有什么其他的 (从动态语义的角度来看) 有效的对" :exp
      "的变换会使它变成良类型的?")
   (P "在Haskell98及以上版本中, 类似的问题通常与臭名昭著的MonomorphismRestriction联系在一起:"
      (CodeB "f = print
exp = f 1
z = f True")
      "上面的代码不能通过类型检查, 编译器归咎于" :f-1 "中的" :1 ". " :z
      "的定义显然与" :exp "无关 (在现实的程序中, 它可能出现在程序往下许多行的地方). "
      "然而, 如果我们移除那个定义, 程序就能通过类型检查并运行. "
      "尽管MonomorphismRestriction常常是罪魁祸首, 但并不总是要怪它. 例如:"
      (CodeB "exp = do
  f &lt;- return id
  r &lt;- return (f 1)
  z &lt;- return (f True)
  print r")
      "尽管" :z "是在" :r "之后被绑定的, 它仍然设法让类型检查器抱怨" :r
      "的定义, 具体来说, 是抱怨字面量" :1
      ". 这一次, 问题与MonomorphismRestriction无关, 也与显式类型签名的有无无关.")
   (P "在所有这些例子中, " :f "的类型都不能完全从其定义确定; 我们需要知道它被使用的上下文. 死代码"
      -- :z "的定义" -- "尽管对" :exp "的结果毫无贡献, 却在一个从活代码的角度来看"
      (Q "误导性的") "上下文中使用了" :f ", 从而导致了类型错误. 值得思考的是, 为什么类型错误报告在"
      (Q "活的") "表达式中, 而不是在死代码中 (即使死代码出现在活代码之后).")
   (P "可能不那么为人所知的是, 死代码可以有相反的效果: 移除它会使程序不可定型. "
      "就计算及其结果而言, 这样的代码什么也不做. 然而, 从类型检查器的角度来看, 它做了一些有用的事情. "
      "例如, 下面的程序能成功编译."
      (CodeB "let id = fun u -> u
let f  = id id
let exp x = f x

let z () = f True")
      "如果我们移除" :z "的定义, 它就不能了: 编译器会归咎于" :f "的定义, 说它的类型"
      (Q "contains type variables that cannot be generalized")
      ". 这是Haskell98中一个类似的例子:"
      (CodeB "main = do
  x &lt;- return []
  print (x == x)
  let z = (x == \"\")
  return ()")
      "照原样写的话, 它能通过类型检查并运行. 如果我们删除看似无关的 (并且也是更晚绑定的) "
      :z "的定义, 类型检查器就会抱怨"
      (Q "Ambiguous type variable `t0' arising from a use of `==' in x==x")
      ". 有时死者可以通过约束生者来帮助生者.")
   (P "最令人惊讶的是, 死代码可能影响程序的结果" -- "即使它甚至没有被执行. "
      "这是一个Haskell98的例子:"
      (CodeB "instance Num Char
main = do
  x &lt;- return []
  print x
  let y = abs (head x)
  let z = (x == \"\")
  return ()")
      "照原样写的话, 程序打印" :empty-string ". 如果我们移除无关的" :z
      "定义, 打印的结果变为" :empty-list ". 顺便一提, 该程序还有另一个无关定义, 即"
      :y "的定义. 如果把它移除会发生什么?")
   (P "这是一个更简单的Haskell98例子, 提炼自Andreas Abel的学生在某个图形相关的代码中"
      "做RGB转换时遇到的问题. 它不仅仅是一个学术问题."
      (CodeB "main = do
  x &lt;- return (2^65)
  print (x == 0)
  let z = x > length []
  return ()")
      "程序打印" :True "; 如果我们移除" :z "的定义, 结果变为" :False ".")
   (P "类型推断的上下文敏感性本身不应该令人惊讶. 类型推断本质上是猜测每个子表达式的类型, "
      "以使整个表达式可定型. 使该算法成为确定性的是" (Q "惰性猜测")
      ": 用一个新鲜的类型变量来表示猜测. 随着推断的进行和看到更多的上下文, "
      "有望有某个东西告知要替换该类型变量的具体类型.")
   (P "我们所有例子背后的问题在于所引入的类型变量的作用域. 一个" (Q "自然的")
      "作用域是当前定义的作用域: let绑定. 如果在对定义的类型检查结束时有些类型变量仍未确定, "
      "它们就会被泛化. 如果泛化是不可能/不适用的呢? 一种设计选择是将该定义作为非良类型的而拒绝. "
      "正如Andreas Abel所指出的, 这实际上就是Agda所做的. MetaOCaml也是如此, "
      "不过是针对生成代码的变量; 一旦一个变量被发现离开了其预定的绑定者的作用域, "
      "代码生成过程就被终止.")
   (P "我猜想, 将类型变量限制在程序定义的作用域内被语言设计者认为是过于严苛的. "
      "他们允许类型变量漂移到更大的上下文中, 希望最终能为它们找到具体类型. "
      "因此, 一个被定义的标识符的类型不仅由其定义决定, 也由其使用决定. "
      "这就是一些人, 特别是Andreas Abel, 认为令人反感的非组合性. "
      "尽管如此, Haskell和OCaml确实对类型变量施加了一个严格的限制: 编译单元. "
      "如果在整个单元被类型检查完毕时有些变量仍未被绑定且未被泛化, 就必须做些什么. "
      "OCaml编译器拒绝该单元. Haskell尝试应用类型默认规则; "
      "如果它们没有帮助, 程序就被宣布为有歧义的.")
   (P "大多数时候, 非组合性是" (Q "良性的")
      -- "一个标识符的各个使用上下文如果相互冲突, 可能会使程序不可定型, 但不会改变其结果. "
      "然而, 当类型默认, 反射, 重叠实例等登场时, 我们就会遇到真正的惊讶.")
   (Dl
    (Dt (B "参考文献"))
    (Dd
     "Haskell Wiki上关于MonomorphismRestriction的页面收集了由Haskell的这一特性所导致的更多的惊讶"
     (Br) (LINK "&lt; https://wiki.haskell.org/Monomorphism_restriction &gt;"
                "https://wiki.haskell.org/Monomorphism_restriction")
     (P (Ref-link "精化的环境分类器: 带有可变单元的类型安全和作用域安全的代码生成"
                  "meta-programming/calculi.html#refined-classif")
        (Br) "类型变量的全局的, 不受限制的作用域与生成代码中的变量名有许多共同之处. "
        "所引用的论文中发展的演算" :NJ "使用一个特殊的" (Q "名字堆")
        "来追踪这样的未来阶段的名字.")))
   (H2. "逆类型检查以及直觉主义逻辑和经典逻辑中的定理证明" #:id "typechecker-CH")
   (P "一个带有第一级关系和完备的判定过程的逻辑编程系统 (例如Kanren) "
      "可以为一个带有多态let, 和类型与积类型的语言定义纯Hindley-Milner类型检查"
      (Em "关系") ". 类型检查关系将一个项与其类型联系起来: 给定一个项, 我们得到它的类型. "
      "它也可以反向工作: 给定一个类型, 我们可以得到具有这个类型的项. "
      "或者, 我们可以给出一个带有空白的项和一个带有空白的类型, 让该关系填充这些空白.")
   (P "下面的代码实现了这种方法. 我们对源语言使用Scheme记号 "
      "(我们同样可以支持类ML或类Haskell的记号). 类型项的记号是中缀的, 箭头是右结合的. "
      "作为一个例子, 文件type-inference.scm的末尾展示了从" :callcc ", " :shift "和" :reset
      "在延续单子中的类型对它们的推导. 给定类型:"
      (CodeB "(define (cont a r) `((,a -> . ,r) -> . ,r))
(((a -> . ,(cont 'b 'r)) -> . ,(cont 'b 'b)) -> . ,(cont 'a 'b))")
      "在2毫秒之内, 我们得到了shift的项:"
      (CodeB "(lambda (_.0) (lambda (_.1)
               ((_.0 (lambda (_.2) (lambda (_.3) (_.3 (_.1 _.2)))))
                 (lambda (_.4) _.4))))"))
   (P "根据Curry-Howard对应, 为一个类型确定一个项等同于证明一个定理"
      -- "就我们的语言而言, 是在直觉主义逻辑中. 我们用类型来表述命题, 例如:"
      (CodeB "(define (neg x) `(,x -> . F))
(,(neg '(a * b)) -> . ,(neg (neg `(,(neg 'a) + ,(neg 'b)))))")
      "这是de Morgan律的一个方向. 在直觉主义逻辑中, de Morgan律更为复杂:"
      (CodeB "NOT (A &amp; B) == NOTNOT (NOT A | NOT B)")
      "系统给出了相应的项, 即证明:"
      (CodeB "(lambda (_.0)
   (lambda (_.1)
     (_.1 (inl (lambda (_.2)
                 (_.1 (inr (lambda (_.3) (_.0 (cons _.2 _.3))))))))))"))
   (P "反类型检查器也可以通过双重否定 (又称CPS) 翻译来证明经典逻辑中的定理. 我们表述命题:"
      (CodeB "(neg (neg `(,(neg 'a) + ,(neg (neg 'a)))))")
      "并在403毫秒之内得到其证明:"
      (CodeB "(lambda (_.0) (_.0 (inr (lambda (_.1) (_.0 (inl _.1))))))")
      "该命题是排中律在双重否定翻译下的陈述.")
   (P "编程语言确实可能有助于研究逻辑.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为1.2, 2006年1月26日.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "type-inference.scm" "Scheme/type-inference.scm") " [19K]"
     (Br) "用于纯Hindley-Milner类型推断" (Em "关系")
     "的Kanren代码, 该关系将带有不动点, 多态" :let
     ", 和类型与积类型的lambda演算中的一个项与其类型联系起来. "
     "代码包含许多注释, 解释了记号, 类型检查关系的增量组合, "
     "以及利用Kanren关系的第一级地位对多态" :let "进行类型检查."
     (P (Ref-link "logic.scm" "Scheme/logic.scm") " [9K]"
        (Br) "Kanren代码, 展示了将逆类型检查器应用于证明直觉主义逻辑和经典逻辑中的定理.")
     (P (Ref-link "反转Haskell类型检查器" "Haskell/types.html#de-typechecker"))
     (P "从一个程序的不完全规格 (例如类型) 出发, 使用约束, 背景知识, "
        "启发式或训练集推导出该程序, 是归纳编程的主题"
        (Br) (LINK "&lt; http://www.inductive-programming.org &gt;"
                   "http://www.inductive-programming.org/"))))
   (H2. "如何具体化新鲜类型变量?" #:id "type-gensym")
   (P "为了对秩2多态的一次使用进行类型检查, 类型检查器按照全称引入的证明规则, 生成 ("
      :gensym ") 一个新鲜的类型变量并留意其作用域. 由于这种实现, "
      "秩2多态被用来 (有人会说被滥用来) 表达静态能力, 并静态地确保各种各样的安全性质, 包括:"
      (Ul (Li "可变状态线程的隔离,")
          (Li "数组下标的边界,")
          (Li "对文件句柄等资源的有序访问和及时释放,")
          (Li "在操纵开放代码的同时只求值封闭代码,")
          (Li "生成代码中变量的词法作用域, 以及")
          (Li "自动微分中扰动变量的区分.")))
   (P "在这些应用中的每一个里, 类型检查器生成的新鲜类型似乎都反映了程序在运行时生成的新鲜值. "
      "例如, 当将代码作为一阶数据结构生成时, 我们不仅需要在类型层面上的gensym以确保词法作用域, "
      "也需要在值层面上的gensym以产生唯一的具体变量名. "
      "依赖类型能否揭示这种反映不过是多余的编码, 并将其消除? "
      "换言之, 我们用于生成新鲜值的代码能否在类型检查期间被重用, 从而更直接地确保它们的安全性质? "
      "特别地, 生成的类型只是隐式地新鲜的, 并且不配备任何运算, "
      "而生成的值是显式地新鲜的, 并且配备了比较等运算. 我们能否使用生成的变量名来自动化弱化?")
   (P "与Chung-chieh Shan的合作工作.")
   (Dl
    (Dt (B "参考文献"))
    (Dd
     "Chung-chieh Shan: 在关于依赖类型编程的Shonan Seminar 007上的报告. "
     "湘南国际村中心, 日本. 2011年9月15日."
     (P (Ref-link "轻量级静态保证" "Computation/lightweight-static-guarantees.html")
        (Br) (Ref-link "轻量级单子区域" "Haskell/regions.html#light-weight")
        (Br) "新鲜类型变量的若干应用, ST单子不计在内")))
   (P "Oleg Kiselyov (okmij.org). 页面最后更新于2021年1月1日.")
   ))
