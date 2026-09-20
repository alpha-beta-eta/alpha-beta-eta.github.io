#lang racket
(provide map_monoid_reduce.html)
(require (except-in SMathML $oplus &oplus))
(define-syntax-rule (define-code* (id str) ...)
  (begin
    (define id (Code str))
    ...))
(define-code*
  (:fold_left "fold_left")
  (:fold_right "fold_right")
  (:reduce "reduce")
  (:reduce^ "reduce'")
  (:map "map")
  (:map_reduce "map_reduce")
  (:map_reduce_arr "map_reduce_arr")
  (:foldMap "foldMap")
  (:Foldable "Foldable")
  (:Domain "Domain")
  (:Sum "Sum")
  (:Prod "Prod")
  (:Min "Min")
  (:Max "Max")
  (:All "All")
  (:Some "Some")
  (:EndoF "EndoF")
  (:EndoF^ "EndoF'")
  (:endof_monoid "endof_monoid")
  (:Av "Av")
  (:Horner "Horner")
  (:Horner.op "Horner.op")
  (:digits_to_num "digits_to_num")
  (:monoid "'a monoid")
  (:op "op")
  (:zero "zero")
  (:gen "gen")
  (:opgen "opgen")
  (:obs "obs")
  (:inv "inv")
  (:opinv "opinv")
  (:min "min")
  (:max "max")
  (:argmax "argmax")
  (:bool "bool")
  (:unit "unit")
  (:true "true")
  (:false "false")
  (:ix "ix")
  (:ncores "ncores")
  (:chunk "chunk")
  (:bm_monoid "bm_monoid")
  (:bm_monoid_ext "bm_monoid_ext")
  (:ebm "'a ebm")
  (:fst "fst")
  (:snd "snd")
  (:list "list")
  (:mmap "mmap")
  (:of_chunk "of_chunk")
  (:obs_del_monoid "obs_del_monoid")
  (:ChunkMonoid "ChunkMonoid")
  (:del_ign "del_ign")
  (:M1 "M1")
  (:M2 "M2")
  (:upd_monoid "upd_monoid")
  (:newupd "newupd")
  (:updgen "updgen")
  (:u "u")
  (:unt "unt")
  (:us "us")
  (:par_nested_agg.ml "par_nested_agg.ml")
  (:A "A")
  (:B "B")
  (:X "X")
  (:c "c")
  (:d "d")
  (:f "f")
  (:h "h")
  (:i "i")
  (:l "l")
  (:m "m")
  (:n "n")
  (:p "p")
  (:t "t")
  (:x "x")
  (:y "y")
  (:z "z"))
(define-syntax SimpleTable
  (syntax-rules ()
    ((_ (x ...) ...)
     (Table
      (Tr (Td x) ...)
      ...))))
(define (Href url . html*)
  (keyword-apply A '(#:attr*) `(((href ,url))) html*))
(define $oplus (Mo "&oplus;" #:attr* '((largeop "false"))))
(define $otimes (Mo "&otimes;" #:attr* '((largeop "false"))))
(define $oslash (Mo "&osol;" #:attr* '((largeop "false"))))
(define $ast (Mo "*"))
(define $_ (Mi "_"))
(define $10 (Mn "10"))
(define-infix*
  (&oplus $oplus)
  (&otimes $otimes)
  (&oslash $oslash))
(define &matmul &i*)
(define &cat &i*)
(define $i-1 (&- $i $1))
(define $i-2 (&- $i $2))
(define $m-1 (&- $m $1))
(define $n-1 (&- $n $1))
(define $x_i-1 (_ $x $i-1))
(define $x_i-2 (_ $x $i-2))
(define $a_n-1 (_ $a $n-1))
(define (K* x) (^ x $ast))
(define (K+ x) (^ x $+))
(define (Seq a d) (K* (pare (&+ a d))))
(define $alpha^* (K* $alpha))
(define $alpha^*uarr (^ $alpha (: $ast $uarr)))
(define $alpha+delta^* (Seq $alpha $delta))
(define (Delims x)
  (&cat $alpha^* $delta (K* (pare (&cat x $delta))) $alpha^*))
(define $chunks (&+ $alpha^* (Delims $alpha^*uarr)))
(define map_monoid_reduce.html
  (TnTmPrelude
   #:title "幺半群的并行乐趣"
   #:css "styles.css"
   (H1. "幺半群的并行乐趣")
   (P "Oleg Kiselyov (okmij.org). 原文标题为" (Q "Folding in Parallel")
      ", 页面最后更新于2026年10月7日.")
   (H2 "翻译术语对照")
   (set-attr*
    (SimpleTable
     ("英文" "中文")
     ("embarrassingly parallel" "易并行")
     ("memory bank conflict" "内存体冲突")
     ("worker" "工作者")
     ("fold" "折叠")
     ("reduce, reduction" "归约")
     ("wedge in" "楔入")
     ("big operator" "大运算符")
     ("Eindhoven quantifier" "Eindhoven量词")
     ("carrier set" "载体集")
     ("generator" "生成元")
     ("generated monoid" "生成的幺半群")
     ("out-of-band" "带外")
     ("collection monoid" "容器幺半群")
     ("observable monoid" "可观察幺半群")
     ("conjugate transform" "共轭变换")
     ("monoid action" "幺半群作用")
     ("iterative action" "迭代作用")
     ("monoid-cached tree" "幺半群缓存树")
     ("near-semiring" "近半环")
     ("semidirect product" "半直积")
     ("majority element" "多数元素")
     ("ghost list" "幽灵列表")
     ("grouping-aggregation" "分组聚合")
     ("monoid comprehension" "幺半群推导式")
     ("vertical monoid composition" "垂直幺半群复合")
     ("chunk" "块")
     ("staged" "分阶段的")
     ("offshore" "离岸"))
    'align "center")
   (H2 "序言")
   (P "玩转幺半群, 亲自发现它们以各种伪装出现得多么频繁, 以及它们能表达多少东西, 是一件乐事. "
      "它不仅给重要但坦白说乏味的数据分析带来了亮色. 它不仅挑战我们去发现高效的 "
      "(其运算具有常数时间和空间的) 幺半群, 这需要独创性. 用Guy Steele的话来说, "
      "发明高效的结合性组合运算符是" (Em "一件非常重大的事") ". 一个高效的幺半群开启了并行实现"
      -- "不只是并行, 而是易并行: 输入序列被任意地划分给各个工作者, "
      "它们全都并行运行, 没有竞争, 没有依赖, 甚至没有内存体冲突. "
      "这是多核, GPU或分布式处理的理想情形.")
   (P "我们展示为什么幺半群是一件大事, 也是一件大乐事" -- "甚至比预想的还要有趣. "
      "Horner法则和Boyer-Moore多数投票" -- "通常被视为本质上顺序的算法的典型" --
      "原来是幺半群, 因而是易并行的. 当我们将幺半群推广为可观察幺半群时, "
      "我们发现了一种新的, 垂直的组合它们的方式, 使得能够直接在原始的反序列化的 (大) 数据上, "
      "在多核或多处理器上, 高效地进行迭代的分组和聚合. 多核OCaml和OpenMP的例子证实了这一点.")
   (H2. "为什么是幺半群?")
   (P "当被要求对一个数组求和时, 首先想到的是什么?")
   (P "对于一些读者而言, 可能是"
      (CodeB "int sum = 0;
for(int i=0; i&lt;N; i++) sum += arr[i];")
      "对于另一些读者而言, 是类似于"
      (CodeB "fold_left (+) 0 arr")
      "这样的东西 (尽管记号不同, 但在好的编译器下, 最终会成为相同的机器码.)")
   (P "Guy Steele在他的ICFP 2009主题演讲中所倡导的理想是"
      (MB (sum $a_i))
      "这个记号说明了" (Em "做什么") ", 而不去事无巨细地管理" (Em "怎么做")
      ". 此外, 有许多种" (Em "怎么做") ". 例如,"
      (MB (&= (sum $0 $n-1 $a_i)
              (@ (&+ (@ (&+ (@ (&+ $0 $a_0)) $a_1)) $..h $a_n-1))))
      "这就是上面的左累积. 也可以从右边累积: OCaml中的" :fold_right
      ", 其他语言中也类似. 另一种选择是"
      (MB (&= (sum $0 $n-1 $a_i)
              (&+ (pare (sum $0 $m-1 $a_i)) (pare (sum $m $n-1 $a_i))))
          (&space 4) (% "其中") (&space 1) (&< $0 $m $n))
      "换言之, 我们将数组分成两个 (或更多个, 并且不一定相等的) 部分, 分别对它们求和, 然后汇总. "
      "部分和可以通过左或右累积来计算, 或者再次分割. 此外, 它们可以并行地计算: 不只是并行, 而且是"
      (Em "易并行地") ". 如果部分和的计算被分配给不同的核, 它们将在没有任何同步或加锁的情况下完成其工作, "
      "不窥探其邻居, 甚至没有" (Em "读") "内存冲突, 充分利用内存带宽. 易并行尤其适合GPU.")
   (P "如何计算" $sum "取决于你的情况 (数组有多大, 它在内存或磁盘中或跨节点是如何布局的) "
      "以及有多少核/节点可用. 所有这些决策都可以推迟到调优时再做" --
      "或者留给运行时和/或编译器.")
   (P "求和并不是唯一容许如此多种实现 (包括易并行实现) 的运算. 同样的道理适用于任何带有一个结合运算"
      "以及一个用于初始化累加器的类零值的累积" -- "或者, 如我们所说, 任何" (Em "幺半群归约") ".")
   (P "归约相当古老 (与左折叠和右折叠同时代), 在1960年代早期就已出现在APL中. 在APL中, 数组"
      :X "的求和以特有的简洁性写作" (Code "+/X") ": 应该将其视为在数组的相邻元素之间"
      (Q "楔入") "加法运算. 数组求和" -- "对比当时命令式语言中的累积与归约" --
      "据我所见, 至少从1970年起就一直是APL教程中的常见例子. 稍微提前一点说, 我们将在OCaml中把求和写作"
      (Code "reduce (monoid Sum)") -- "不如APL中那样简洁, 但更容易念出来, 而且可以扩展到"
      (Q "更丰富的") "幺半群. 像Futhark或SaC这样的现代数组语言确实让程序员以类似方式编写归约, "
      "将其编译为高效的GPU, 多核等代码, 并根据问题和硬件选择实现策略. "
      "幺半群归约在2000年代中期得到了相当大的推动, 这要归功于Google的MapReduce. "
      "(我们很快会看到为什么reduce经常伴随着map.)")
   (P "实现/编译幺半群归约已经得到了充分的研究和支持, 尤其是在现代数组语言或MapReduce框架中. "
      "剩下的是设计/寻找" (Em "高效的") "幺半群" --
      "正如Guy Steele所说, 这并非平凡, 需要创造力. 真正的乐趣就从这里开始.")
   (P (B "参考文献")
      (Ul (Li "Guy Steele: Organizing Functional Code for Parallel Execution; "
              "or, foldl and foldr considered slightly harmful. 2009年8月 (ICFP 2009, 主题演讲).")
          (Li "Olivier Danvy: "
              (Q "Folding left and right matters: direct style, accumulators, and continuations")
              ". Journal of Functional Programming, vol 33, e2. Functional Pearl, 2023年2月. "
              "附录A: A brief history of folding left and right over lists.")))
   (P "根据Danvy, " :fold_left "和" :fold_right "的最初实例是由Christopher Strachey在1961年 (!) 研究的. "
      "Reduce是APL (Iverson, 1962) 的一部分, 在那里它被称为" (Q "/") ": 因此"
      (Code "+/x") "对数组" :x "求和. System T中的Gödel递归子R是折叠的一个更一般的版本 "
      "(现在称为para-fold). Church数码是折叠.")
   (P "幺半群归约有时被称为" (Q "大运算符") " (类比于分别用于求和与乘积的大写Σ和Π). "
      "它们的另一个名字是" (Q "Eindhoven量词") ".")
   (P "有人可能认为Haskell的" :Foldable "类中的" :foldMap "也与之相关: 它也谈论幺半群归约. 然而, "
      :foldMap "被规定为一个显式右结合的运算符, 因而缺乏Guy Steele所强调的map-reduce的主要性质: "
      "规格与实现的解耦. 也可以说该归咎于" :Foldable "类型类本身: 根据它, 一个容器只有一个"
      :foldMap ", 而事实上同一个容器可能有许多归约实现.")
   (H2. "折叠和归约")
   (P "在讲到幺半群归约之前, 让我们回顾一下折叠" --
      "便于参考, 也因为它们会频繁出现, 与幺半群形成对比. 序列上的左折叠作为状态机的记号尤其重要: "
      "一种顺序计算, 或者说流式计算的一般模式.")
   (P "折叠, 或者说在一个序列 (具体起见, 一个列表) 上的顺序累积, 被定义为"
      (CodeB "fold_left  : ('z -> 'a -> 'z) -> 'z -> 'a list -> 'z
fold_right : ('a -> 'z -> 'z) -> 'a list -> 'z -> 'z")
      "实际上, 这里有两个运算: 左折叠和右折叠. 它们的含义从以下例子中应该很清楚:"
      (CodeB "fold_left (+) 0 [1;2;3;4]  ≡ (((0 + 1) + 2) + 3) + 4

fold_right (+) [1;2;3;4] 0 ≡ 1 + (2 + (3 + (4 + 0)))")
      "在这种情况下, 即折叠函数为加法时, 结果是相同的: 两个表达式都对列表求和. "
      "一般而言, 左折叠和右折叠产生不同的结果: 例如, 试试将减法作为折叠函数. 列表元素的类型"
      (Code "'a") "和累加器的类型" (Code "'z") "不必相同: 例如,"
      (CodeB "fold_left (fun z _ -> z + 1) 0 l")
      "计算任何列表的长度,"
      (CodeB "fold_left (fun z x -> x :: z) [] l")
      "反转列表" :l ", 而"
      (CodeB "fold_right (fun x z -> if p x then x::z else z) l []")
      "过滤列表: 省略谓词" :p "对其返回" :false "的元素. 列表上的许多其他运算 (事实上, 全部运算) "
      "都可以表达为折叠. 折叠确实是对序列进行顺序有状态处理的一般模式.")
   (P "折叠还有其他的定义: 有时右折叠的最后两个参数是交换的. 如果右折叠函数的参数同样交换, "
      "那么左折叠和右折叠就有相同的签名. 它们的行为, 即结合模式, 仍然是不同的. "
      "Olivier Danvy (见上文) 追溯了列表折叠及其参数顺序的历史.")
   (P "具体起见我们展示了列表上的折叠, 但类似的运算也存在于数组, 流, 文件, 树, 字典以及任何其他容器上.")
   (P "在本文中, 我们所说的reduce总是指幺半群上的归约. 幺半群是一个集合 (称为" (Q "载体集")
      ") 配上一个结合的二元运算, 该运算具有一个中性元 (也称为单位元, 或零元). 具体而言, 在OCaml中"
      (CodeB "module type monoid = sig
  type t                                (* carrier set *)
  val zero : t
  val op : t -> t -> t
end
type 'a monoid = (module monoid with type t = 'a)")
      "运算" :op "必须是结合的 (并且是完全的: 对所有" :t "值都有定义), 并且"
      (CodeB "op zero x = op x zero = x")
      "必须对幺半群的每个元素" :x "成立. 在Google MapReduce中, 运算" :op
      "还被认为是交换的. 我们不施加这样的要求. 然而, 我们将强调效率: "
      "我们追求的幺半群应该具有常数时间和空间的运算" :op ". 显然的例子是"
      (CodeB "module Sum = struct
  type t = int
  let zero = 0
  let op = (+)
end"))
   (P "序列 (这里是列表) 上的归约是运算"
      (CodeB "reduce : 'a monoid -> 'a list -> 'a")
      "其行为可以说明为"
      (CodeB "reduce monoid []  ≡ monoid.zero
reduce monoid [x] ≡ x             (* for any x and monoid *)

reduce (module Sum) [1;2;3;4] ≡ 1 + 2 + 3 + 4")
      "可以说reduce在序列的相邻元素之间" (Q "楔入") "幺半群运算. 由于" :op
      "是结合的, 括号不是必需的. 因此, 与左折叠和右折叠不同, 只有一种reduce.")
   (P "在一个序列上折叠就是执行一个以该序列为输入的有限状态机: 即, " (Code "fold_left f z l")
      "在输入" :l "上运行初始状态为" :z "且转移函数为" :f "的有限状态机, 并返回最终状态. "
      "这是一个本质上顺序的计算. 如前所述, 左折叠只是累积式for循环的另一种记号. "
      "(右折叠类似, 但从右到左访问序列.)")
   (P "然而, reduce可以用各种方式实现: 例如,"
      (CodeB "let reduce (module M) l = List.fold_left M.op M.zero l
let reduce (module M) l = List.fold_right M.op l M.zero")
      "或"
      (CodeB "let reduce m l = split l |> map (reduce' m) |> reduce'' m")
      "{译注: 原文此处写作" (Code "map reduce' m") ", 应为笔误.} "
      "也就是说, 将序列分为两个或更多部分 (使用一个适当定义的" (Code "split : 'a list -> 'a list list")
      "), 对每个部分进行归约 (使用reduce的某个实现, 这里记作" :reduce^
      "), 然后汇总结果, 也许使用reduce的另一种实现. (使用OCaml的普通列表原样运行这段代码, "
      "不太可能带来任何好处; 然而, 有更好的序列, 例如分块列表, 数组或可寻址的文件句柄; "
      "中间列表可以通过融合消除. 我们稍后会看到例子.) 这个实现 (模式) 的吸引力在于, "
      "部分归约可以全部并行完成" -- "易并行地. 例如, 上面的" :reduce^
      "可以运行在它自己的线程中 (多核OCaml的" :Domain "或者OpenMP任务), 我们稍后会看到. "
      "再次强调, 不需要加锁或同步.")
   (P "因此reduce提供了相当大的灵活性: 是顺序还是并行地归约, 将序列分成多少部分, "
      "部分归约和最终汇总使用什么实现. 这些决策取决于序列的 (估计) 大小和可用的硬件 (核), "
      "并且可以留给之后的调优或运行时. 另一方面, 左折叠或右折叠除了严格顺序的策略之外别无选择. "
      "因此, Guy Steele称折叠" (Q "略有害处") ", 并敦促尽可能使用reduce.")
   (P "因此主要的问题是: 我们能否将fold转换为reduce? 本文的其余部分回答这个问题.")
   (P (B "参考文献")
      (Ul (Li "Guy Steele: Organizing Functional Code for Parallel Execution; "
              "or, foldl and foldr considered slightly harmful. 2009年8月 (ICFP 2009, 主题演讲).")
          (Li (Href "https://okmij.org/ftp/Algorithms/monoid_reduce.ml" "monoid_reduce.ml")
              -- "本文的完整代码.")
          (Li (Href "https://okmij.org/ftp/Streams.html#zip-folds" "How to zip folds")
              -- "一个完整的以折叠表示的列表的库, 展示了所有列表处理运算都可以表达为折叠.")
          (Li (Href "https://okmij.org/ftp/Scheme/xml.html#Papers"
                    "Accumulating tree traversals, a better tree fold")
              -- "及其在XML解析中的应用.")))
   (H2. "幺半群" #:id "monoids")
   (P "回忆一下, 幺半群是一个带有两个运算 (满足特定性质) 的集合" -- "换言之, 是一个具有如下签名的"
      (Href "https://okmij.org/ftp/tagless-final/Algebra.html" "代数") "."
      (CodeB "module type monoid = sig
  type t
  val zero : t
  val op : t -> t -> t
end")
      "不同的幺半群" -- "幺半群代数" -- "是该签名的实现. 运算" :op "必须是结合的, 并以"
      :zero "为其左单位元和右单位元. 此外, 我们特别强调高效的, 即常数空间和时间的运算" :op ".")
   (P "显然的例子是"
      (CodeB "module Sum = struct
  type t = int
  let zero = 0
  let op = (+)
end")
      "和" :Prod -- "还有" :Min " (以" :min "为运算) 和" :Max "; " :All " (载体为" :bool
      ", 零元是" :true ", " :op "是合取) 和" :Some " (载体为" :bool ", 零元是" :false ", "
      :op "是析取). 列表 (或者一般而言, 序列) 是原型性的幺半群, 以空列表为" :zero
      ", 以连接 (或者反向连接) 为运算. 然而, 这个幺半群并不高效: 连接两个非空序列会产生一个更大的序列; "
      "此外, 对于列表而言, 连接不是常数时间的.")
   (P "还必须提到" :EndoF "幺半群: 它的载体是某个类型" :t "上的" (Code "t->t")
      "函数 (一般而言, 集合论意义上的映射) 的集合; " :zero "是恒等函数, 而" :op "是复合 "
      "(对于相关的" :EndoF^ ", " :op "是从左到右的复合). " :EndoF "具有重大的理论意义, 由它产生了"
      (Q "幺半群作用") ". 然而在实践中 (如果我们谈论的是编程语言的函数), 它是浪费的; 我们稍后会看到一个例子. "
      "尽管函数复合通常是常数时间的, 但它会创建一个新的闭包 (以引用的方式包含其参数函数), "
      "因此不是常数空间的.")
   (H3. "同态" #:id "hom")
   (P "幺半群在我们的列表里成对出现, 例如" :All "和" :Some ", 这暗示了它们之间的联系. "
      "最重要的联系被称为同态. 从幺半群" :A "到幺半群" :B "的同态" :h "是一个从" :A "的载体到" :B
      "的载体的映射 (用编程语言的术语来说, 是一个" (Code "A.t -> B.t") "函数), 其满足"
      (CodeB "h A.zero = B.zero         h (A.op x y) = B.op (h x) (h y)")
      "{译注: 原文此处及下文写作" (Code "(b y)") ", 应为笔误.} "
      "对于幺半群" :A "的任意元素" :x "和" :y "成立. 我们说, " :h (Q "与运算交换")
      ". 例如, 布尔否定是从" :Some "到" :All "的同态, 反之亦然, 这由de Morgan律保证. 指数运算是从"
      :Sum "到" :Prod "的同态. 试着找一些有趣的同态, 例如从" :Sum "到" :Some "的同态. 从幺半群"
      :A "到" :EndoF "的同态有一个特别的名字: 幺半群作用.")
   (P "还有另一个重要的同态. 考虑整数列表, 如我们已经看到的, 它们是幺半群. 从整数列表到" :Sum
      "的同态" :h "应该满足"
      (CodeB "h [] = 0     h (x @ y) = h x + h y")
      "列表求和恰好满足这些条件: " (Em "reduce是一个同态.") " " (Code "List.length")
      "也满足: 因此同态条件并不能唯一地定义归约. 我们需要扩展幺半群的概念, "
      "以便将reduce精确地规定为一个序列同态.")
   (H3. "Map-reduce和容器幺半群" #:id "map-reduce")
   (P "有许多迹象表明幺半群接口过于极简了. 首先, 比较列表折叠的类型"
      (CodeB "fold_left  : ('z -> 'a -> 'z) -> 'z -> 'a list -> 'z")
      "和reduce的类型"
      (CodeB "reduce : 'a monoid -> 'a list -> 'a")
      "将" :monoid "内联之后, 后者变为"
      (CodeB "reduce : ('a -> 'a -> 'a) -> 'a -> 'a list -> 'a")
      :fold_left "的类型更一般. 这种一般性让" :fold_left "能够表达例如任意类型的列表的长度:"
      (CodeB "let length : 'a list -> int = List.fold_left (fun z _ -> z + 1) 0")
      "尽管列表长度也可以作为一个归约来计算, 但它不仅仅是一个归约:"
      (CodeB "let length : 'a list -> int = map (Fun.const 1) >> reduce (module Sum)")
      "(其中" (Code ">>") "是从左到右的函数复合). 也就是说, 我们需要" :map
      ", 以便首先将元素的类型 (和值) 转换为要累积的幺半群. 这就是为什么" :reduce "之前经常有一个"
      :map ", 用来做这种调整.")
   (P "如果幺半群签名中的类型" :t "被视为抽象的, 那么我们能够得到的幺半群元素只有"
      :zero ", " (Code "op zero zero") ", 等等" -- "全都是零元. 我们需要一种获得其他元素的方式: "
      "我们需要" (Em "生成元") ".")
   (P "在抽象代数中, 生成元通常作为零目运算, 或者说常量" -- "也许是无穷多个常量" --
      "引入. 在编程语言的语境下, 最好也许是将生成元视为由某个集合索引, 或者说, "
      "视为一个从索引集到幺半群元素的映射 (函数):"
      (CodeB "module type coll_monoid = sig
  include monoid
  type ix               (* index set *)
  val gen : ix -> t
end
type ('a,'e) coll_monoid = (module coll_monoid with type t='a and type ix='e)")
      "索引集可以是空的, 单元素的 (例如" :unit "), 有限的 (例如" :bool
      ") 或者无限的. 尽管幺半群元素的类型" :t "可以是抽象的, 但索引类型" :ix
      "应该是具体的. Fegaras和Maier将这个接口 (的一个更简单的版本) 称为容器幺半群.")
   (P "早先的" :Sum "等幺半群可以平凡地扩展为容器幺半群:"
      (CodeB "module Sum = struct
  type t = int
  let zero = 0
  let op = (+)
  type ix = int
  let gen = Fun.id
end")
      "换言之, 如果幺半群的类型是具体的, 那么我们总是可以" (Q "在带外")
      "找到/构造其值, 可以这么说, 而不使用幺半群接口. 容器幺半群的一个非平凡的例子是"
      (CodeB "let list_monoid : type a e. (a list, a) coll_monoid =
    (module struct
      type ix = a
      type t = a list
      let zero = []
      let op = (@)
      let gen x = [x]
    end)")
      "对于这个幺半群, 它的所有元素都可以仅通过幺半群运算获得: 作为" :zero ", 对于某个" :i "的"
      (Code "gen i") ", 或者应用" :op ". 换言之, 它是一个" (Em "生成的幺半群") ".")
   (P "容器幺半群上的同态还必须与运算" :gen "交换: 换言之, 从容器幺半群" :A "到容器幺半群" :B
      "的同态是满足以下条件的函数" (Code "h : A.t -> B.t") ":"
      (CodeB "h A.zero = B.zero     h (A.op x y) = B.op (h x) (h y)
h (A.gen i) = B.gen i")
      "特别地, 从" (Code "int list") "容器幺半群到" :Sum "的同态" :h "必须满足"
      (CodeB "h [] = 0     h (x @ y) = (h x) + (h y)      h [i] = i")
      "这恰好就是列表求和. 这些条件现在唯一地规定了同态. 列表长度也是一个同态, "
      "但是是到另一个类似于" :Sum "的幺半群的同态"
      (CodeB "struct include Sum let gen = Fun.const 1 end")
      "有了容器幺半群, 列表长度就直接表达为归约 (同态), 而不需要" :map
      ". 可以说, 有了容器幺半群, " (Em "从序列出发的同态就是map-reduce."))
   (P "因此, 一个容器幺半群唯一地规定了从序列出发的同态. 换言之, 我们可以对于任何容器幺半群, 泛型地定义"
      (Code "map_reduce : ('a,'e) coll_monoid -> 'e list -> 'a")
      ". 然而, 在这样做之前, 让我们考虑生成元的另一种运算:"
      (CodeB "val opgen : t -> ix -> t")
      "它与" :gen "是等价的, 或者说可以相互转换:"
      (CodeB "opgen t x = op t (gen x)
gen x = opgen zero x")
      "然而, " :opgen "常常可以更高效地实现. 还有另一个原因.")
   (P "如果我们要将容器幺半群归约实现为左累积, 它看起来应该像这样"
      (CodeB "let map_reduce : type t e. (t,e) coll_monoid -> e list -> t =
  fun (module M) -> List.fold_left M.opgen M.zero")
      "换言之, " :opgen "恰好就是折叠函数. 这与群有一个紧密的类比, 群是带有一个额外的一元运算"
      :inv " (表示逆) 的幺半群. 然而, 有时人们不定义" :inv ", 而是定义二元运算"
      (Code "opinv x y = op x (inv y)") ". 对于" :Sum "群, 其" :inv "是取负, 而" :opinv "是减法.")
   (H3. "数组上的顺序和并行map-reduce" #:id "seqpar")
   (P "如前所强调的, 幺半群归约容许许多其他实现 (全都给出相同的结果). 这里我们只展示两种, "
      "使用数组切片作为容器. (OCaml的标准库并不提供数组切片; 因此, 我们必须自己定义它们.)"
      (CodeB "type 'a array_slice = {arr:'a array; from:int; upe:int}
let whole_slice (arr:'a array) : 'a array_slice =
  {arr; from=0; upe=Array.length arr}")
      "顺序的, 左结合的实现:"
      (CodeB "let map_reduce_arr : type t e. (t,e) coll_monoid -> e array_slice -> t =
  fun (module M) {arr;from;upe} ->
  let acc = ref M.zero in
  for i=from to upe-1 do
    acc := M.opgen !acc arr.(i)
  done;
  !acc")
      "以及在" :ncores "个核上的并行实现."
      (CodeB "let map_reduce_par_n :
 type t e. int -> (t,e) coll_monoid -> e array_slice -> t =
  fun ncores (module M) {arr;from;upe} ->
  let len = upe - from in
  let chunk = len / ncores in
  List.init ncores Fun.id |>
  List.map (fun i ->
    Domain.spawn (fun () ->
        map_reduce_arr (module M)
         {arr; from = i*chunk; upe = min len ((i+1)*chunk)}))
    |>
    map_reduce (module struct include M
        type ix = M.t Domain.t
        let gen = Domain.join
        let opgen x y = op x (Domain.join y) end)")
      "它有点幼稚: 我们应该首先检查长度是否足够大: 如果不够大, 就只做顺序的" :map_reduce_arr
      ", 因为否则的话, 并行求值始终存在的开销将占主导地位. 我们还应该使" :chunk
      "成为页面大小的倍数. 此外, 我们也应该让Domain 0做一些工作. 真实的例子见"
      (Ref "nested-agg") "的结尾.")
   (H3. "积" #:id "products")
   (P "到目前为止, 我们都是从零开始构建幺半群. 还有另一种方式: 组合, 或者说复合, 已有的幺半群. "
      "最显然直接的复合是配对, 或者说 (容器) 幺半群的积:"
      (CodeB "module Product(M1:coll_monoid)(M2:coll_monoid with type ix = M1.ix) :
    (coll_monoid with type t = M1.t * M2.t and type ix = M1.ix)")
      "它实际上就是一对幺半群, 逐元素地进行运算. 两个幺半群应该具有相同的索引集, "
      "以便生成两个幺半群的元素. 一个例子是计算序列的平均值:"
      (CodeB "module Av =
  Product(Sum)(struct include Sum let opgen x _ = x+1 end)")
      "在" :Av "上的map-reduce一遍就得到序列的和与长度. (你会如何修改" :Av
      "以避免求和计算中的溢出? 毕竟, 对于任何非空序列, 无论多长, 无论其元素多大, 平均值都是良定义的.)")
   (H3. "可观察幺半群和共轭变换" #:id "conj")
   (P "如果 (容器) 幺半群接口中的类型" :t "确实是抽象的, 那么幺半群归约的结果就是抽象类型" :t
      "的一个值, 我们无法在幺半群之外使用它. 我们还需要一个运算: 观察幺半群, 将其作为某个具体类型"
      (Code "obs") "的值."
      (CodeB "module type obs_monoid = sig
  include coll_monoid
  type obs
  val obs : t -> obs
end")
      "另一个动机来自计算序列的平均值: 用上面的" :Av "幺半群进行幺半群归约会给出一对数字: "
      "和与长度. 为了得到所需的平均值, 我们必须" (Q "观察") "这个对: 做最后的除法. 运算" :obs
      "看起来像是" :gen "的补, 这也令人满意.")
   (P "任何容器幺半群都可以原样观察:"
      (CodeB "module AsIs(M:coll_monoid) :
       (obs_monoid with type ix = M.ix and type obs = M.t) = struct
  include M
  type obs = M.t
  let obs = Fun.id
end")
      "一个非平凡的可观察幺半群是"
      (CodeB "module Average = struct
  include Av
  type obs = float
  let obs (s,l) = float_of_int s /. float_of_int l
end")
      "使用可观察幺半群的map-reduce具有如下形式"
      (CodeB "let map_reduce_conj (type i) (type o)
    (module M:obs_monoid with type ix = i and type obs = o) (l:i list) : o =
  map_reduce (module M) l |> M.obs")
      "执行map-reduce, 然后观察结果. 整个过程" --
      "使用" :gen "将序列元素注入幺半群, 归约, 并观察结果" -- "被称为"
      (Em "共轭变换") ". 它是一般的幺半群归约.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/Algorithms/monoids.ml" "monoids.ml")
              -- "各种幺半群和幺半群复合的OCaml代码.")
          (Li "Leonidas Fegaras, David Maier: Towards an Effective Calculus for Object Query Languages. "
              "SIGMOD 1995, pp. 47-58.")
          (Li (Href "https://okmij.org/ftp/tagless-final/Algebra.html" "Algebra")
              -- "为计算机科学家, 特别是为tagless-final程序员准备的数学上的代数概念导论.")))
   (H2. "平凡地将折叠作为map-reduce")
   (P "回到Guy Steele关于优先使用" :reduce "而非折叠的劝告, 有人可能会想, 是否总是可以这样做. "
      "在本章中我们看到, 折叠确实" (Em "总是") "可以用reduce来表达" --
      "但其方式既无用也无趣. 事实上,"
      (CodeB "fold_right f [1;2;3;4] z
= f 1 (f 2 (f 3 (f 4 z)))
= (f 1 · f 2 · f 3 · f 4) z
= map_reduce (endof_monoid f) [1;2;3;4] |> (fun g -> g z)")
      "{译注: 原文此处漏掉了" :map_reduce "的列表参数" (Code "[1;2;3;4]") ".} "
      "对于任何适当类型的" :f ", " :z "和序列成立. 对于左折叠可以写出非常类似的表达式 "
      "(留给读者作为练习). 关键的想法在于函数复合是结合的. 这里:"
      (CodeB "let endof_monoid : type a e. (e -> a -> a) -> (a->a,e) coll_monoid =
 fun f ->
    (module struct
      type t = a -> a
      let zero = Fun.id
      let op g h = fun x -> g (h x)     (* may also consider >> *)
      type ix = e
      let gen = f
      let opgen m i = fun x -> m (f i x)
    end)")
      "由于到" :endof_monoid "的同态被称为幺半群作用, 折叠因此被称为" (Q "迭代作用") --
      "这是数学中相当知名的构造.")
   (P "不幸的是, 这个平凡的回答几乎没有实际用处. 从这个例子中我们看到, map-reduce创建了一个闭包"
      (Code "(f 1 · f 2 · f 3 · f 4)") ", 它最终被应用于" :z ". 复合的闭包"
      (Code "(f 1 · f 2 · f 3 · f 4)") "具有与原列表相同的结构" --
      "因而具有相同的大小 (实际上, 要大好几倍: 我们不仅必须存储列表元素本身, 还必须存储函数指针"
      :f ", 再加上开销). 实际上我们构建了一个无界大小的中间数据结构. 尽管闭包的复合可以并行化, "
      "但有用的工作只有在大的闭包复合最终被应用于初始累加器" :z "时才会完成" --
      "在那时折叠函数" :f "被一步一步地, 顺序地应用, 就像在原来的" (Code "fold_right f l z")
      "中一样. 这种对折叠的平凡归约只会浪费时间和空间.")
   (P "因此问题不仅仅是将折叠表达为reduce, 而是要" (Em "高效地")
      "这样做, 没有不当的和无界的开销. 只有这样我们才能有利地并行折叠.")
   (P (B "参考文献")
      (Ul (Li "Jeremy Gibbons: Origami Programming for Fun and Profit. "
              (Href "https://www.cs.ox.ac.uk/publications/publication16637-abstract.html"
                    "https://www.cs.ox.ac.uk/publications/publication16637-abstract.html")
              -- "将折叠阐述为" :endof_monoid ".")))
   (H2. "更有趣地将折叠作为map-reduce")
   (P "将一个顺序算法 (折叠) 表示为幺半群归约, 从而开启易并行化, 只有在我们能够" (Em "高效地")
      "做到这一点时才有意义, 即没有无界大小的中间数据, 也不花费不当的时间. 换言之, "
      "如果折叠对每个序列元素以常数时间并在常数 (工作) 空间中工作, 那么reduce也应如此.")
   (P "一般的方法, 即上面介绍的共轭变换, 是一个原则, 或者说一个模式. 它并没有告诉我们如何实际找到高效的幺半群, "
      "甚至没有告诉我们它是否存在. 事实上, 这常常是非平凡的, 需要独创性. 为了体会这一点, "
      "这里有几个给读者的练习. 要强调的是, 在所有情况下我们感兴趣的都是高效的幺半群."
      (Ul (Li (B "迭代减法") ": 考虑前面顺带提到的减法折叠. 减法不是结合的, 因此"
              (Code "fold_left (-)") "本身并不是reduce的实例" -- "而" (Code "fold_right (-)")
              "也不是 (并且它们显然给出不同的结果). 然而, 两者都可以表达为在高效幺半群上的map-reduce. "
              "试着找到它们. 提示: 两种情况中有一种要容易得多.")
          (Li (B "ArgMax") ": 寻找序列的最大元素是一个在" :Max "幺半群上的归约. "
              "如果我们不仅想找到一个最大元素, 同时还想找到它在序列中的位置" --
              "如果有多次出现, 则为最左边的位置, 那会怎样? 最右边的位置又如何? 换言之, 将" :max "和"
              :argmax "作为一个高效的幺半群归约, 一遍计算出来.")
          (Li (B "括号匹配") ": 判断在一个由开圆括号和闭圆括号组成的字符串中, 是否所有括号都匹配. "
              "试着找到将这种匹配作为map-reduce来做的幺半群. 下一个问题是, 假设字符串可能包含其他非括号字符. "
              "你能否修改这个幺半群, 使得归约不仅能告诉我们括号是否匹配, 而且在不匹配的情况下, "
              "还能告诉我们最左边的没有对应开括号的闭括号的位置和最右边的未闭合的开括号的位置? "
              "那么最右边的没有对应开括号的闭括号和最左边的未闭合的开括号呢?")))
   (H2. "广义Horner法则" #:id "Horner")
   (P "Horner法则, 或者说Horner模式, 是一个广泛使用的高效求多项式值的顺序算法. "
      "类似的累积模式也经常出现在解析中. 尽管从表面上看这个算法是本质上顺序的, "
      "但它原来可以表示为幺半群归约. 我们的归约比其他已知的并行化Horner法则的方式更一般, 更灵活.")
   (P "作为一个说明性的例子, 我们以将一个数位序列 (最高位在前) 转换为对应的数字为例" --
      "一个简单的解析运算. 它是一个累积式的顺序运算, 可以写成折叠:"
      (CodeB "let digits_to_num =
  List.fold_left (fun z x -> 10*z + (Char.code x - Char.code '0')) 0")
      "例如, " (Code "digits_to_num ['1'; '7'; '5']") "给出" (Code "175")
      ". 可以很容易地将其分解为一个" :map "和"
      (CodeB "List.fold_left (fun z x -> 10*z + x) 0")
      "的复合, 后者本质上就是Horner法则: 在" (&= $b $10) "处对多项式"
      (sum (&i* $d_i (^ $b $i))) "求值. 然而, 折叠函数" (Code "fun z x -> 10*z + x") "不是结合的.")
   (P "尽管如此, 还是有一种方式可以构建一个幺半群:"
      (CodeB "module Horner : (obs_monoid with type ix = int and type obs = int) =
struct
  type t = int * int
  let zero = (0,1)
  let op (x,b1) (y,b2) = (x*b2+y, b1*b2)
  type ix = int
  let gen x = (x,10)
  type obs = int
  let obs = fst
  end")
      "而" :digits_to_num "是一个归约"
      (CodeB "let digits_to_num =
  map_reduce_conj (module struct
      include Horner
      type ix = char
      let opgen m x = opgen m (Char.code x - Char.code '0') end)")
      "因此, Horner法则可以表达为map-reduce, 因而可以易并行地求值.")
   (P :Horner "确实是一个幺半群, 这需要检查, 特别是要检查" :Horner.op
      "是结合的. 这是一个相当简单但乏味的算术. 幸运的是, 正如我们将在下一章中看到的, 这是不必要的: "
      "通过采取一个不同的视角, 结合性变得一目了然.")
   (P "与Guy Steele的ICFP09主题演讲中的幺半群缓存树不同, 我们不依赖任何特殊的数据结构: "
      "我们可以使用普通的数组, 将它们划分给可用的核.")
   (P "由于Horner方法在多项式求值中无处不在, 已经有一些将其并行化的方法. 最常见的是多项式系数的奇偶拆分. "
      "尽管它可能很适合SIMD处理, 但奇偶拆分对于多核是不利的, 因为它会造成读争用 (内存体冲突) 并浪费缓存空间. "
      "我们的幺半群归约让我们可以将不同的内存体分配给不同的核, 供它们独占地, 无冲突地访问.")
   (P "另一方面, Estrin方案可以看作是我们的幺半群构造的一个非常特殊的实例: 二项分组. "
      "我们的幺半群归约允许任意的分组, 如果需要的话可以分层, 并且各组的大小不一定相同.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/Algorithms/monoid_reduce.ml" "monoid_reduce.ml")
              -- "本文的完整代码.")))
   (H2. "线性递推" #:id "lin-rec")
   (P "Horner法则是所谓的 (一阶) 线性递推的一个特例"
      (eqn* ($x_i $= (: (&+ (@ (&d* $a_i $x_i-1)) $b_i) $cm) (&> $i $0))
            ($x_0 $= (: $b_0 $cm) (&= $i $0)))
      "其作用于两个输入序列" $a_i "和" $b_i ". (在Horner法则中, " $a_i "是一个常数序列.) "
      "线性递推 (以及相关的线性差分方程) 无处不在: 例如, 积分微分方程的Euler方法, "
      "信号处理中的无限冲激响应滤波器, 时间序列, 等等. 这个递推显然是一个顺序计算: 事实上, "
      "它是在拉链 (zip) 起来的" $a_i "和" $b_i "序列上的左折叠. 求解递推就是在不需要计算之前的"
      (: $x_i $cm (&< $i $n)) "的情况下得到" $x_n " (也就是说, 以" (Q "闭形式")
      "). 然而, 我们寻找的是一种将" $x_n "作为幺半群归约来计算的解法" -- "甚至是并行地计算.")
   (P "令人惊讶的是, 这很容易做到. 让我们将线性递推重写为一个矩阵乘法"
      (MB (&= (Mat ($x_i) ($1))
              (&matmul (Mat ($a_i $b_i) ($0 $1)) (Mat ($x_i-1) ($1)))))
      "(这应该会让人想起齐次坐标.) 就是这样: 于是" $x_n "就是"
      (MB (&= (Mat ($x_n) ($1))
              (&matmul (pare (prod (Mat ($a_i $b_i) ($0 $1)))) (Mat ($b_0) ($1)))))
      "因此, 计算" $x_n "就归约 (一语双关) 为矩阵的乘积" --
      "换言之, 归约所在的幺半群是最后一行为" (Mat ($0 $1)) "的" (&c* $2 $2)
      "矩阵, 以矩阵乘法为幺半群运算. 单位元是单位矩阵. 众所周知矩阵乘法是结合的, "
      "所以没有更多需要证明的了. (这应该会让人想起快速Fibonacci计算.) 因此, 计算" $x_n
      "类似于我们最初的对一个序列求和的运行例子, 其中矩阵乘法扮演了加法的角色.")
   (P "由于最后一行为" (Mat ($0 $1)) "的矩阵的乘积仍然给出相同形式的矩阵, "
      "在实践中我们不需要费心计算最后一行. 对于常数" $a_i ", 我们就得到了上一章中的Horner幺半群.")
   (P "扩展到更高阶的递推是直接的. 例如, 二阶线性递推"
      (MB (&= $x_i (&+ (@ (&d* $a_i $x_i-1)) (@ (&d* $b_i $x_i-2)) $c_i)))
      "可以重写为矩阵形式"
      (MB (&= (Mat ($x_i) ($x_i-1) ($1))
              (&matmul (Mat ($a_i $b_i $c_i) ($1 $0 $0) ($0 $0 $1))
                       (Mat ($x_i-1) ($x_i-2) ($1))))))
   (H3. "(近) 半环")
   (P "我们可以进一步推广到数值乘法和加法以外的运算, 将其写作" $otimes "和" $oplus
      ". 容易验证, 为了使矩阵乘法 (以" $otimes "为其元素乘法, 以" $oplus
      "为加法) 是良定义的且结合的, 运算" $otimes "和" $oplus "都应该是结合的, " $oplus
      "应该是交换的, 并且" $otimes "应该对" $oplus "左分配和右分配:"
      (eqn* ((&otimes $x (@ (&oplus $y $z))) $=
             (&oplus (@ (&otimes $x $y)) (@ (&otimes $x $z))))
            ((&otimes (@ (&oplus $y $z)) $x) $=
             (&oplus (@ (&otimes $y $x)) (@ (&otimes $z $x)))))
      "由于我们在矩阵中还使用了" $0 "和" $1 ", 一般的" $oplus "和" $otimes
      "也必须具有中性元 (我们仍然分别将其写作" $0 "和" $1 "). 此外, 以下吸收律必须成立"
      (MB (&= (&otimes $x $0) (&otimes $0 $x) $0))
      "换言之, " $otimes "和" $oplus "应该构成一个" (Em "半环")
      ". 因此, 任何半环上的线性递推都可以作为幺半群归约来求解.")
   (P "事实上, 对于一阶线性递推, 由于其矩阵的特殊形式, 即最后一行为" (Mat ($0 $1)) "的"
      (&c* $2 $2) "矩阵, 我们可以只用一个更弱的条件. 我们不需要" $oplus "的交换性, 只需要"
      $otimes "的左分配性.")
   (P "进一步的推广也是可能的: 两个结合的运算" $oplus "和" $otimes ", 其中后者对前者左分配. "
      "两者都不必是交换的. (这似乎被称为近半环.) 事实上, 我们总是可以向载体集中添加两个新元素, 称之为"
      $0 "和" $1 ", 并扩展" $oplus "和" $otimes "使得"
      (MB (&Table
           ((&oplus $0 $x) $= (&oplus $x $0) $= $x)
           ((&otimes $1 $x) $= (&otimes $x $1) $= $x)
           ((&otimes $0 $x) $= (&otimes $x $0) $= $0)))
      "我们还必须定义" (&oplus $1 $x) "和" (&oplus $x $1)
      ". 然而, 我们在矩阵计算中从来不会实际需要这个结果; 因此, 我们可以任意地定义它们.")
   (H3. "半直积")
   (P "最后, 我们从一个不同的视角来看一阶递推"
      (MB (: (&= $x_i (&+ (@ (&d* $a_i $x_i-1)) $b_i)) $cm (&space 1) (&> $i $0)))
      "这个视角揭示了更多的联系. 这个递推是如下形式的一个特例"
      (MB (: (&= $x_i (&oplus (@ (ap $h_i $x_i-1)) $b_i)) $cm (&space 1) (&> $i $0)))
      "其中" $oplus "是一个结合运算, 而" $h_i "是某些函数. 原来的递推是" $oplus "为加法且"
      $h_i "为乘以" $a_i "的情形.")
   (P "值得将一般的递推以一种更具提示性, 也更一般的形式呈现出来"
      (MB (: (&= $x_i (&oplus (@ (&oslash $c_i $x_i-1)) $b_i)) $cm (&space 1) (&> $i $0)))
      "它既涵盖了原来的递推 (" $oslash "为乘法且" $c_i "为" $a_i
      "), 也涵盖了其函数推广 (" $c_i "为" $h_i "且" $oslash
      "为函数应用). 我们应该强调, 与原来的递推不同, " $c_i "和" $b_i "不必具有相同的类型. 如果"
      $b_i " (因而还有" $x_i ") 具有类型" $S ", 那么" $c_i "可以具有一个不同的类型, 称之为" $P
      ". 因此, 运算" $oslash "具有一般的类型" (&-> $P $S $S) ".")
   (P "对于一个类型为" (&-> $P $S $S) "的运算, 谈论它的结合性是没有意义的. 尽管如此, 如果"
      $oslash "具有某种在某种程度上推广了原来递推中乘法的结合性的性质, 会有所帮助 (对于找到最终的幺半群归约而言). "
      "显然的性质是存在结合运算" (&: $otimes (&-> $P $P $P)) "以及" $P "的一个元素" $1 ", 使得"
      (eqn* ((&oslash $1 $x) $= $x)
            ((&oslash (@ (&otimes $c $d)) $x) $= (&oslash $c (@ (&oslash $d $x)))))
      "也就是说, 如果我们将" (&oslash $c $_) "视为一个" (&-> $S $S)
      "函数, 那么应该有某个" $c "给出恒等函数, 并且应该有一种复合这些函数的方式. 对于原来的递推关系, 这样的"
      $otimes "存在 (即乘法), 对于其函数推广也存在 (即函数复合).")
   (P "带有以" $1 "为中性元的结合运算" $otimes "的集合" $P "是一个幺半群. 于是, 满足上述两个条件的运算"
      $oslash "被称为幺半群" $P "在" $S "上的" (Em "幺半群作用") ".")
   (P "原来的递推关系中的乘法不仅是以" $1 "为中性元的结合运算; 它还吸收零元并对加法分配. 对于我们的推广, 假设"
      $S "是一个幺半群 (以" $oplus "为运算, 以" $0 "为中性元), 这两个条件 (吸收性和分配性) 具有如下形式"
      (eqn* ((&oslash $c $0) $= $0)
            ((&oslash $c (@ (&oplus $x $y))) $=
             (&oplus (@ (&oslash $c $x)) (@ (&oslash $c $y)))))
      "如果将" (&oslash $c $_) "视为一个" (&-> $S $S) "映射 (函数), 那么这些条件说的是该映射保持" $0
      "并与" $oplus "交换. 换言之, " (&oslash $c $_) "是一个从" $S "到其自身的同态 (或者说, 自同态).")
   (P "所有这些条件" -- $P "在" $S "上的幺半群作用是" $S "上的自同态" --
      "让我们能够将一般的递推关系写成已经熟悉的形式 (以" $oslash "为" (Q "乘法") ", 以" $oplus "为加法)"
      (MB (&= (Mat ($x_i) ($1))
              (&matmul (Mat ($c_i $b_i) ($0 $1)) (Mat ($x_i-1) ($1)))))
      "它具有矩阵乘积形式的解, 通过幺半群归约来计算. 诚然, 这些是相当不寻常的矩阵: "
      "第一列和第二列具有不同的类型. 尽管如此, 在给定这些条件的情况下, " (Q "矩阵")
      "乘法是良定义的且结合的 (容易验证). 这样的矩阵幺半群 (以如此定义的不寻常的矩阵乘积) 被称为"
      $P "和" $S "的 (外) " (Em "半直积") ".")
   (P "我感谢James McKinna让我注意到了幺半群积.")
   (P "作为给读者的练习, 考虑线性递推"
      (eqn* ($x_i $= (: (&oplus (@ (&otimes $x_i-1 $a_i)) $b_i) $cm) (&> $i $0))
            ($x_0 $= (: $b_0 $cm) (&= $i $0)))
      "其中" $otimes "和" $oplus "是" (Em "非") "交换的结合运算. 它并不完全是本章所处理的形式. "
      "我们仍然能将" $x_n "作为幺半群归约来计算吗? 在" $otimes "和" $oplus "满足什么条件的情况下?")
   (P "线性递推有着悠久的历史, 可以追溯到1960年代中期. 使用幺半群归约 (实际上是幺半群前缀扫描) 求解它们, "
      "在Blelloch 1993年的论文第1.4.1节中有所总结 (奇怪的是, 其中并没有使用幺半群这个词). "
      "第1.4.2节提到了矩阵, 但与第1.4.1节一样, 错过了矩阵乘积的解法. 第1.4.2节还错误地陈述了对运算的要求: "
      "更高阶的递推需要半环. 第1.4.1节中讨论的半结合运算就是幺半群作用" -- "同样, 未被认出.")
   (P (B "参考文献")
      (Ul (Li "Guy E. Blelloch: Prefix Sums and Their Applications. "
              "Synthesis of Parallel Algorithms, John H. Reif ed., Morgan Kaufmann, 1993. "
              (Href "https://www.cs.cmu.edu/~blelloch/papers/Ble93.pdf"
                    "https://www.cs.cmu.edu/~blelloch/papers/Ble93.pdf"))))
   (H2. "Boyer-Moore多数投票" #:id "BM")
   (P "Boyer-Moore多数投票是一个寻找序列中的多数元素" -- "即出现次数超过一半的元素" --
      "的算法. 该算法需要对序列进行一遍扫描, 并使用常数量的工作空间. 重要的是要记住, 返回值是多数元素, "
      (Em "如果它存在的话") ". 该算法" (Em "一般而言") "无法判断序列是否有多数元素: "
      "如果多数元素不存在, 返回值是一个任意的元素, 不一定是出现得最频繁的. 因此, 一般而言, "
      "需要对序列进行第二遍扫描, 来统计返回元素的出现次数, 并验证它确实是多数元素. "
      "在某些情况下, 第二遍扫描可以避免或者是不必要的.")
   (P "Boyer-Moore算法被称为原型性的流式算法, 并且是本质上顺序的. 因此, 可能令人惊讶的是, "
      "它可以被呈现为幺半群归约, 因而可以同样高效地并行执行" -- "事实上, 是易并行地. "
      "幺半群归约的实现实际上很简单. 复杂的是让我们自己相信它确实在所有情况下都能工作并做正确的事.")
   (P "Boyer-Moore算法是一个状态机处理, 逐个元素地摄入输入. 因此它可以实现为折叠 "
      "(假设输入以非空列表的形式给出):"
      (CodeB "let bm : 'a list -> 'a = function h::t ->
  let fsm (c,m) x =
      if c = 0 then (1,x)
      else if m = x then (c+1,m)
      else (c-1,m)
  in List.fold_left fsm (1,h) t |> snd")
      "{译注: 原文此处写作" (Code ">> snd") ", 应为笔误.} "
      "状态是计数器" :c "和候选多数元素" :m ". 在算法结束时, 即整个序列被扫描完毕时, " :m
      "就是多数元素 (如果序列有多数元素的话). 如果最后计数器" :c
      "大于序列长度的一半, 那么序列有多数元素, 并且" :m "就是多数元素. 否则, 我们必须再扫描一遍序列, 以验证"
      :m "确实是多数元素. 如果我们事先知道多数元素存在" --
      "或者如果在多数元素不存在时我们满足于任何元素" -- "那么验证这一遍可能是不必要的.")
   (P "尽管该算法看起来是本质上顺序的, 但它事实上可以表示为幺半群归约. 这个幺半群简单而高效:"
      (CodeB "type 'a bm = int * 'a
type t = a bm
let zero = (0,z)
let op (c1,m1) (c2,m2) =
 if c1 = 0 then (c2,m2) else
 if c2 = 0 then (c1,m1) else
 if m1 = m2 then (c1+c2,m1) else
 if c1 > c2 then (c1-c2, m1) else
 (c2-c1, m2)
type ix = a
let gen x = (1,x)
let opgen m x = op m (gen x)")
      "幺半群的元素 (载体集) 是对: 由一个自然数" :c "和一个序列元素" :m "组成. 当" :c "为零时, "
      :m "可以是任意的; 在上面的代码中, 我们用" :z "表示这样一个任意的元素. "
      "我们本可以通过为幺半群载体定义一个更复杂的数据类型, 或者使用" (Code "'a option")
      ", 来避免指定它. 下面的推理会变得更加混乱; 因此, 为了清晰起见, 我们保持上面的定义.")
   (P "可以看出" :op "是交换的, 并且" :zero "确实是" :op "的零元. 然而, " :op "是结合的吗? 也就是说, "
      :bm_monoid "真的是一个幺半群吗? 这很容易检查:"
      (CodeB "let m1 = (2,10) and m2 = (3,9) and m3 = (2,8)
op (op m1 m2) m3  ⇝ (1, 8)
op m1 (op m2 m3)  ⇝ (1, 10)")
      "不幸的是, " :bm_monoid "不是一个幺半群.")
   (P "为了不卖关子, 事实证明" :bm_monoid "在" (Q "道德上")
      "是一个幺半群, 并且用它进行的归约在所有情况下都确实给出正确的结果. 然而, 证明这一点并不是那么直接.")
   (P "为了证明, 我们将" :bm_monoid "扩展为:"
      (CodeB "type 'a ebm = int * 'a * ('a*'a) list
type t = a ebm
let zero = (0,z,[])
let op (c1,m1,l1) (c2,m2,l2) =
 if c1 = 0 then (c2,m2,l1@l2) else
 if c2 = 0 then (c1,m1,l1@l2) else
 if m1 = m2 then (c1+c2,m1,l1@l2) else
 if c1 > c2 then (c1-c2, m1, repeat c2 (m1,m2) @ l1 @ l2) else
 (c2-c1, m2, repeat c1 (m1,m2) @ l1 @ l2)
type ix = a
let gen x = (1,x,[])
let opgen m x = op m (gen x)")
      "这里, 中缀运算" (Code "@") "是列表连接, 而" (Code "repeat (n:int) (x:'a) : 'a list")
      "产生长度为" :n "且元素全都等于" :x "的列表.")
   (P :bm_monoid_ext "的载体集是" :ebm ": 由一个自然数" :c ", 一个序列元素" :m
      ", 以及一个分量互异的对的列表 (即任何对的" :fst "都不等于其" :snd ") 组成的三元组. 我们注意到, "
      :op "保持了" :l "中的所有对都具有互异分量这一不变量.")
   (P "如此扩展的" :bm_monoid_ext "的运算" :op "不是结合的. " :bm_monoid_ext
      "不仅不是一个幺半群, 而且也不高效: " :op "中的列表连接运算既不是常数空间的, 也不是常数时间的.")
   (P "让我们在该集合上引入关系" (Code "≈") ", 定义如下: " (Code "x ≈ y") "当且仅当" (Code "flatten x")
      "等于" (Code "flatten y") ", 其中"
      (CodeB "let flatten : 'a ebm -> 'a multiset = fun (c,m,l) ->
  repeat c m @ List.map fst l @ List.map snd l")
      "也就是说, " (Code "flatten x") "将" :x "中提到的所有元素收集到一个" (Em "多重集")
      "中. 因此, " (Code "x ≈ y") "当且仅当" :x "的元素的多重集等于" :y
      "的元素的多重集. 容易看出, 关系" (Code "≈") "是自反的, 对称的和传递的. "
      "{译注: 原文此处写作" (Q "reflexive, commutative and associative") ", 应为笔误.} 也就是说, "
      "它是一个等价关系. 它还具有对我们有用的性质, 如下所述.")
   ((Proposition)
    :bm_monoid_ext "是一个模" (Code "≈") "的幺半群. 也就是说,"
    (CodeB "op (0,z,[]) (c,m,l) ≈ op (c,m,l) (0,z,[]) ≈ (c,m,l)
op (c1,m1,l1) (op (c2,m2,l2) (c3,m3,l3)) ≈ op (op (c1,m1,l1) (c2,m2,l2)) (c3,m3,l3)"))
   (P "结合性质源于这样一个事实: " :op "保持了在" (Code "(c,m,l)")
      "中出现的元素, 包括它们的重数 (并且多重集的相等是结合的).")
   (P "此外, " (Code "≈") "对于寻找多数元素的任务而言是" (Em "充分的") ".")
   ((Proposition)
    (B "[充分性]") " 如果" (Code "(c1,m1,l1) ≈ (c2,m2,l2)") "并且多重集"
    (Code "flatten (c1,m1,l1)") "有多数元素, 那么" (Code "c1>0") ", " (Code "c2>0")
    ", 并且" (Code "m1=m2") ". 非形式地说, " (Code "≈")
    "保持了找到的多数元素 (如果多数元素存在的话).")
   (P "该命题是多数元素对于序列置换不变这一事实以及以下表示引理的推论.")
   ((Lemma)
    (B "[表示]") " 如果一个序列 (多重集) " (Code "flatten (c,m,l)") "有多数元素, 那么"
    (Code "c>0") "并且" :m "是多数元素.")
   (P "关键的想法是, " (Code "(c,m,l)") "是多重集" (Code "flatten (c,m,l)") "的一个" (Em "划分")
      "的表示: 具体来说, 是将多重集划分为互异元素的对以及剩余部分 (如果存在的话, 由某个特定元素"
      :m "的一个或多个副本组成). {译注: 原文此处写作" (Code "flatten m") ", 应为笔误.} "
      "这样的划分一般不是唯一的: 考虑序列" (Code "[1; 2; 3; 3; 3]")
      "及其两个划分" (Code "(1,3,[(1,3);(2,3)])") "和" (Code "(3,3,[(1,2)])")
      ". {译注: 原文将前者写作" (Code "(3,1,[(1,3);(2,3)])") ", 应为笔误.} "
      "然而, 它具有引理中所规定的性质.")
   (P "为了证明, 我们注意到, 如果互异对的列表" :l "有" :n "个元素, 那么它有" (Code "2*n")
      "个值. 并非所有这些值都是互异的. 然而, 没有值出现超过" :n "次; 否则, 根据鸽巢原理, " :l
      "中的某个对就会有相等的分量. 因此, " (Code "(c,m,l)") "中出现的任何不同于" :m " (如果"
      (Code "c>0") ") 的值都不可能是多数元素. 如果多数元素存在, 那么它必须是" :m ", 并且"
      (Code "c>0") ".")
   (P "我们的证明受到了Boyer和Moore的原始证明的启发. 然而, 他们使用的是归纳法 (这对于顺序算法是合适的). "
      "与之相对, 我们使用的是等式推理 (以及鸽巢原理)" -- "而没有使用归纳法.")
   (P "顺带一提, 似乎归纳法是计算机科学中使用的唯一的证明方法" --
      "许多广泛使用的教科书除了归纳法什么也没介绍. 在我看来, 归纳法被过度使用了. 数学还有许多其他的证明方法.")
   (P "最后一步是观察到, " :bm_monoid_ext "中我们实际关心的部分" -- "元素" :m "和计数" :c --
      "的计算完全不考虑列表" :l ". 可以说, 这个列表是一个" (Q "幽灵")
      "列表, 只是证明正确性所需要的, 而不是计算多数元素所需要的. 因此, 我们可以省略它, 从而重新得到"
      :bm_monoid ".")
   (P "总而言之, 多数投票因此可以作为在幺半群" :bm_monoid "上的map-reduce来执行. "
      "它与原来的Boyer-Moore算法同样高效: 对序列进行一遍扫描, 使用常数且很小的工作空间. 然而, "
      "输入可以被任意地划分给可用的工作者, 它们并行地进行搜索, 没有任何干扰或依赖. 多数投票可以易并行地完成.")
   (P (B "参考文献")
      (Ul (Li (Href "https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm"
                    "https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm"))
          (Li (Href "https://okmij.org/ftp/Algorithms/monoid_reduce.ml" "monoid_reduce.ml")
              -- "本文的完整代码.")
          (Li "Lambert Meertens: Reducing hopeful majority. The Squiggolist, 1(1):5, 1989. "
              (Href "https://www.kestrel.edu/people/meertens/publications/papers/Reducing_hopeful_majority.pdf"
                    "https://www.kestrel.edu/people/meertens/publications/papers/Reducing_hopeful_majority.pdf")
              -- "该论文也注意到了结合性的缺失, 并非形式地论证了 (援引非确定性) 一致性并不需要它. "
              "我们展示了如何在不援引非确定性的情况下, 使用互异对的" (Q "幽灵")
              "列表, 形式化地重新获得结合性. 这个" (Q "幽灵") "列表是能够定义等价关系"
              (Q "≈") "的关键要素.")))
   (H2. "嵌套分组聚合: 垂直幺半群复合" #:id "nested-agg")
   (P "幺半群归约的另一个有启发性的例子是嵌套分组和聚合" -- "数据分析中的一个典型问题. "
      "分组和聚合本身可以表达为幺半群推导式" -- "map-reduce" --
      "这一点由Fegaras和Maier在他们关于幺半群推导式的开创性论文中深刻地阐明. 然而, 在我们的情形中, "
      "分组聚合的结果会被再次分组聚合" -- "一次又一次. 一篇较早的文章描述了这个问题的一个顺序的最优解法. "
      "奇妙的是, 它也用到了幺半群. 然而, 它们的复合并没有被阐明. 这里我们展示, " (Q "堆叠")
      "特定的幺半群仍然给出一个幺半群" -- "因而给出易并行的处理.")
   (P "与较早的那篇文章一样, 运行例子是Advent of Code 2022的一个问题, 概括如下:"
      (Blockquote
       (P "输入是一个数字序列, 由竖线分隔成块, 其中每个块包含多个以逗号分隔的数字. "
          "任务是计算每个块的和, 并找出所有块中的最大值.")
       (P "一个示例输入如下:" (Code "100,200,300|400|500,600|700,800,900|1000"))))
   (P "较早的那篇文章展示了一个高效的" -- "单遍且没有缓冲的" --
      "顺序解法. 我们现在给出一个易并行的解法. 需要强调的是, 输入可以被任意地分割, 例如"
      (CodeB "100,20  0,300|40    0|500,600   |700,800  ,900|1000")
      "随意地切穿数字和分隔符. 我们可以" :mmap
      "输入文件, 并将一段页面范围分配给一个工作者 (一个CPU核), 而不必担心数据如何或是否与页面边界对齐. "
      "然后所有工作者都可以并行运行, 使用常数内存, 没有依赖或竞争" --
      "获得线性的加速. 这样的处理也非常适合GPU.")
   (P "这个问题的一个更现实的变体是, 比如说, 在一个UTF-8编码的文本文件中找出10个最频繁的单词.")
   (H3. "热身" #:id "nga.warm-up")
   (P "为了建立直觉, 让我们首先考虑这个问题的一个简单版本. 输入是一个由分隔符分隔的类型为" $alpha
      "的项的序列. 例如, 在" $alpha "是" (Code "char") ", 或者准确地说是数字字符的情形下:"
      (CodeB "let adv22_input =
['1'; '0'; '0'; ','; '2'; '0'; '0'; ','; '3'; '0'; '0'; '|'; '4'; '0'; '0';...]")
      "为了简单起见, 我们这里将输入视为一个列表; 在完整版本中, 输入是一个字节数组或一个文件. "
      "任务是将分隔符 (逗号和竖线) 之间的项分组.")
   (P "形式化地说, 任务是将" $alpha+delta^* "转换为" (K* (pare $alpha^*uarr)) ", 其中" $alpha^*uarr
      "是不包含分隔符的极大子序列. 这里, " $delta "是分隔符的类型 (集合). 我们用" (Q "+")
      "表示 (两个类型或序列的) 并, 用乘法表示连接; 上标的星号是Kleene星. 观察:"
      (MB (deriv $alpha+delta^*
                 (K* (pare (&cat $alpha^* (K* $delta))))
                 (&+ $alpha^* (Delims $alpha^*))
                 $chunks))
      "这里使用的是几乎标准的代数 (只有乘法, 即序列连接, 不是交换的). 用文字来说: 一个序列"
      $alpha+delta^* "要么:"
      (Ul (Li "没有分隔符: " $alpha^*)
          (Li "有一个分隔符: " (&cat $alpha^* $delta $alpha^*))
          (Li "有多于一个分隔符: " (&cat $alpha^* $delta (K+ (pare (&cat $alpha^*uarr $delta))) $alpha^*)))
      "在最后一种情形中, 分隔符之间的" $alpha^* "子序列是极大的" -- "根据定义. "
      "这种表示在连接 (" (Q "乘法") ") 下是封闭的:"
      (MB (deriv $alpha+delta^*
                 (&cat $alpha+delta^* $alpha+delta^*)
                 (&cat (pare $chunks) (pare $chunks))
                 (&+ $alpha^* (Delims $alpha^*uarr)
                     (&cat (Delims $alpha^*uarr) (Delims $alpha^*uarr)))
                 $chunks)))
   (P "原来的" $alpha+delta^* "是一个幺半群: 运算是连接, 零元是空序列. 正如我们刚刚展示的, 表示"
      $chunks "也是一个幺半群; 幺半群运算可以直接从上面的等式中读出.")
   (P "具体而言, 在OCaml中, " $chunks "是和数据类型:"
      (CodeB "type delim = char
type 'a chunk = A of 'a list
           | C of 'a list * delim * (('a list) * delim) list * 'a list")
      "于是幺半群是"
      (CodeB "let wap_monoid =
{zero = A [];
 op = fun x y -> match (x,y) with
 | (A lx,A ly) -> A (lx @ ly)
 | (A lx,C (l,d,c,r)) -> C (lx @ l, d,c,r)
 | (C (l,d,c,r),A ly) -> C (l,d,c, r @ ly)
 | (C (lx,dx,cx,rx),C (ly,dy,cy,ry)) ->
              C (lx,dx, (cx @ [(rx@ly),dy] @ cy), ry)}")
      "它是序列连接的一个复杂版本, 考虑到了两侧为分隔符的" $alpha^* "子序列必须是极大的.")
   (P "这个幺半群表示一个" $alpha+delta^* "子序列: 空子序列由" (Code "A []")
      "表示, 单元素子序列由" (Code "A [c]") "或" (Code "C ([],d,[],[])")
      "表示 (取决于其元素是一个数字字符" :c "还是一个分隔符" :d
      "). 表示整个输入的幺半群通过map-reduce找到:"
      (CodeB "adv22_input |>
map_reduce (function '0'..'9' as c -> A [c] | d -> C ([],d,[],[]))
  wap_monoid")
      "结果为"
      (CodeB "C (['1'; '0'; '0'], ',',
   [(['2'; '0'; '0'], ','); (['3'; '0'; '0'], '|'); ...],
   ['1'; '0'; '0'; '0'])")
      "然后我们可以提取出极大的无分隔符子序列:"
      (CodeB "let of_chunk : 'a chunk -> ('a list) list = function
    | A x -> [x]
    | C (l,d,c,r) -> [l] @ (List.map fst c) @ [r]"))
   (P "因此, 热身任务通过map-reduce, 再接着用" :of_chunk "提取结果来解决.")
   (P "热身例子将输入的数字字符和分隔符的列表转换为" (Code "((char list)) list") ", 其中" :list
      "出现了两次. 我们现在将它们替换为任意的幺半群.")
   (H3. "垂直幺半群复合" #:id "vertical-composition")
   (P "刚刚介绍的代数背景让我们能够容易地推广热身例子, 以解决任意的嵌套分组聚合问题. "
      "广义地看, 热身例子是将一个序列" $alpha+delta^* -- "由分隔符" $delta "穿插的" $alpha "项" --
      "转换为一个序列" (Seq $alpha^*uarr $delta^) ", 即一个同样由一般而言更少的分隔符" $delta^
      "穿插的极大无分隔符子序列" $alpha^*uarr "的序列. (组的分隔符被保留, 但仅用于在组内分隔项的分隔符在聚合之后消失.)")
   (P "我们现在从序列推广到任意的幺半群. 首先, 假设有一个可观察幺半群" :M1 ", 它将一个序列"
      $alpha^*uarr "归约为某个值" $rho " (例如, 将一个数字字符序列归约为一个整数). 第一个推广是从"
      $alpha+delta^* "到" (Seq $rho $delta^) "的归约. 现在, 假设有一个幺半群" :M2 "来归约"
      (Seq $rho $delta^) ". 这个幺半群不仅应该接受" $rho
      "项, 还应该接受分隔符. 于是值得引入接受分隔符的幺半群"
      (CodeB "module type obs_del_monoid = sig
  include obs_monoid
  type del                           (* type of delimiters *)
  val del : del -> t
end")
      "给定这样的" :M1 "和" :M2 ", 我们可以构建一个" :obs_del_monoid ", 它一遍就能归约" $alpha+delta^* ":"
      (CodeB "module ChunkMonoid(S:sig
   module M1: obs_monoid
   module M2: obs_del_monoid with type ix = M1.obs
   val del_ign : M2.del -> bool
  end) :
    (obs_del_monoid with
     type del = S.M2.del and type ix = S.M1.ix and type obs = S.M2.obs) = struct
  open S
  type t = A of M1.t | C of M1.t * M2.t * M1.t    (* chunk *)
  let zero = A M1.zero
  let m1_to_m2 : M1.t -> M2.t = fun x -> M2.gen (M1.obs x)
  let op x y = match (x,y) with
  | (A lx,A ly) -> A (M1.op lx ly)
  | (A lx,C (l,c,r)) -> C (M1.op lx l, c, r)
  | (C (l,c,r),A ly) -> C (l,c, M1.op r ly)
  | (C (lx,cx,rx),C (ly,cy,ry)) ->
      C (lx, M2.op cx (M2.op (m1_to_m2 (M1.op rx ly)) cy), ry)
  type ix = M1.ix
  let gen x = A (M1.gen x)
  type del = M2.del
  let del c =
    if del_ign c then A M1.zero
    else C (M1.zero, M2.del c, M1.zero)
  type obs = M2.obs
  let obs = function
    | A x -> m1_to_m2 x |> M2.obs
    | C (l,c,r) -> M2.(op (m1_to_m2 l) (op c (m1_to_m2 r)) |> obs)
end")
      "这里, " :del_ign "是被忽略的分隔符的谓词. 这就是垂直幺半群复合. 当幺半群" :M1 "和" :M2
      "是高效的时候" -- "它们的运算花费常数时间和空间" -- :ChunkMonoid "也是高效的.")
   (P :ChunkMonoid "产生一个" :obs_del_monoid ", 它可以被再次复合, 一次又一次. "
      "完整的Advent of Code 2022问题是一个使用如下幺半群的归约"
      (CodeB "struct
 include
  ChunkMonoid (struct
      module M1 = struct include Horner
                  type ix = char
                  let gen x = (Char.code x - Char.code '0') |> gen end
      let del_ign = Fun.const false
      module M2 = ChunkMonoid (struct
          module M1 = AsIs(Sum)
          let del_ign = ((=) ',')
          module M2 = struct
           include AsIs(Max)
           type del = char
           let del c = assert (c='|'); zero
           end
         end)
    end)
 let gen = function '0'..'9' as c -> gen c | c -> del c
end"))
   (P "我们因此展示了将嵌套分组聚合作为使用一个高效幺半群的幺半群归约. "
      "它不仅可以一遍完成, 使用常数空间和 (关于输入大小的) 线性时间. 它还可以易并行地完成, 获得线性的加速.")
   (P "这里给出的问题让人想起Steele在他的ICFP演讲中所描述的问题. 他也使用了" (Q "块")
      "和段的想法. 然而, 我们这里的问题涉及分块处理的三重嵌套" -- "复杂三倍" --
      "并且我们推导出了一般的垂直幺半群复合.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/Algorithms/nested-aggregation.html" "Nested Grouping-Aggregation")
              -- "介绍了这个问题及其顺序解法. 不可思议的是, 它也使用了幺半群和幺半群复合, 尽管没有明确意识到.")
          (Li (Href "https://okmij.org/ftp/Algorithms/talk_FP_Madrid.ml" "talk_FP_Madrid.ml")
              -- "包含各种幺半群和幺半群复合 (包括嵌套聚合复合) 的完整OCaml代码.")
          (Li (Href "https://okmij.org/ftp/Algorithms/par_nested_agg.ml" "par_nested_agg.ml")
              -- "用于并行嵌套聚合的完整的多核OCaml代码, 使用OCaml的domain. "
              "作为基准测试, 我们使用一个4.6GB的文件 (与Advent of Code的示例文件具有相同的结构). "
              "每个核得到文件的一个切片来归约; 然后主线程收集结果. 并行代码不需要加锁, 并且确保没有竞争.")))
   (H2. "为嵌套分组聚合生成并行map-reduce" #:id "nga.gen")
   (P "本章将幺半群和幺半群归约 (具体而言, 是早先的" (Ref "vertical-composition")
      ") 转化为另一种命令式的形式" --
      "适合为共享内存多处理生成高性能的, 低层次的一阶命令式代码. (早先的函数式接口更适合消息传递, 例如MPI.)")
   (P "为了说明, 我们使用MetaOCaml生成OCaml代码. 然而, 生成的OCaml代码是刻意低层次的: "
      "没有代数数据类型, 没有模式匹配, 当然也没有第一级函数. 于是它可以被离岸到C.")
   (P "如反复强调的那样, 幺半群归约可以用多种方式完成. 一种是顺序的: 左折叠, 或者说累积式for循环. "
      "它只需要可观察幺半群接口的一个子集:"
      (CodeB "module type state_machine = sig
  type t
  val zero : t
  type ix
  val opgen : t -> ix -> t
  type obs
  val obs : t -> obs
end")
      "这是一个状态机的接口: 状态为" :t "; 初始状态为" :zero "; 输入字母表为" :ix "; 状态转移" :opgen
      "根据当前状态和当前输入计算新的状态; 以及观察" :obs ". 这个接口可以转化为命令式的形式:"
      (CodeB "module type upd_state_machine = sig
  type u
  type unt
  val newupd : (u -> unt) -> unt
  type ix
  val updgen  : u -> ix -> unt
  type obs
  val obs : u -> (obs -> unt) -> unt
end")
      "类型" :u "是可更新状态 (例如引用单元) 的类型; " :newupd "分配/初始化状态, " :updgen
      "根据当前输入更新它, 而" :obs "观察它. 类型" :unt "可以说是" (Q "动作")
      "的类型; 它也是一个幺半群, 让我们能够复合动作. 例如, " :unt "可以是" :unit
      " (命令式语句的类型) 或" (Code "unit code") " (命令式语句的代码).")
   (P "幺半群不仅仅是一个状态机: 可以说, 幺半群归约还可以按切片完成. 因此, 幺半群不仅提供"
      (Code "opgen : t -> ix -> t") "以根据当前输入" :ix "更新状态" :t ", 还提供"
      (Code "op : t -> t -> t") "以连接, 或者说合并 (部分累积的) 状态. 因此, "
      "可观察幺半群的命令式版本提供了从可更新状态" :u "中提取当前状态, 以及用导入的状态" :t "更新"
      :u "的运算:"
      (CodeB "module type upd_monoid = sig
  type u
  type unt
  val newupd : (u -> unt) -> unt
  type ix
  val updgen  : u -> ix -> unt
  type obs
  val obs : u -> (obs -> unt) -> unt

  type t                                (* exportable state *)
  val from_upd : u -> t                 (* export u         *)
  val upd   : u -> t -> unt             (* update u         *)
  val set   : u -> t -> unt
  val reset : u  -> unt

  type us                               (* plural of u *)
  type usix
  val newupds : usix -> (us -> unt) -> unt
  val us_set : us -> usix -> t -> unt
  val us_get : us -> usix -> t
end")
      "按切片的处理可以独立地完成, 如果可能的话并行地完成. 因此, 该接口提供了" :us
      ", 用于为每个工作者 (核) 创建一个独立的可更新状态. 因此, 类型" :us
      "及其运算支持并行for (我们在多核OCaml中使用domain来模拟它).")
   (P "实现了若干分阶段的" :upd_monoid " (包括垂直幺半群复合) 的源代码见下文. "
      "生成的OCaml代码也见下文" -- "它确实是一阶的, 低层次的和命令式的" --
      "本质上是以OCaml记号写成的C.")
   (P "总体而言, 代码生成和更新语义一致地带来了4倍的单核加速, 并且可以扩展到多个核.")
   (P (B "版本"))
   (P "当前版本为2025年12月.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/Algorithms/spar_nested_agg.ml" "spar_nested_agg.ml")
              -- :par_nested_agg.ml "的分阶段版本, 也引入了面向更新的幺半群.")
          (Li (Href "https://okmij.org/ftp/Algorithms/pa_na_red.ml" "pa_na_red.ml")
              -- "为并行嵌套聚合生成的代码. 它接受文件描述符和要使用的核数.")
          (Li (Href "https://okmij.org/ftp/Algorithms/par_na_main.ml" "par_na_main.ml")
              -- "运行生成代码的main函数, 也展示了基准测试结果.")))
   (H2. "结论")
   (P "在他的ICFP'09主题演讲中, Guy Steele劝告我们尽可能使用reduce (或者说" :map_reduce
      ") 而不是折叠. 与折叠不同, reduce并不使我们受限于某个特定的求值策略. 它可以顺序地执行, "
      "易并行地执行, 或者以树状的方式执行. 就像数学中的" $sum "记号一样, 它规定了应该对什么求和, "
      "而不规定如何求和或以何种顺序求和.")
   (P "Guy Steele的结论也适用于本文. 尤其是他最后的想法:"
      (Ul (Li "结合性的组合运算符是一件非常重大的事")
          (Li "发明 (并证明) 新的组合运算符是一件非常, 非常重大的事")))
   ))
