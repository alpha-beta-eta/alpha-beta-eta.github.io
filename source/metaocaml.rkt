#lang racket
(provide metaocaml.html)
(require SMathML)
(define $UnderBrace (Mo "&UnderBrace;"))
(define (UnderBrace x y)
  (__ x (__ $UnderBrace y)))
(define (Cells . x*)
  (Mtable
   #:attr*
   '((frame "solid")
     (rowlines "solid")
     (columnlines "solid"))
   (apply Mtr (map Mtd x*))))
(define $.
  (Mo "." #:attr* '((lspace "0") (rspace "0"))))
(define (make-blank n)
  (Mspace #:attr* `((width ,(format "~spx" n)))))
(define $blank:8px (make-blank 8))
(define-infix*
  (&. $.)
  (&blank:8px $blank:8px))
(define (_prime x n)
  (_^ x n $prime))
(define metaocaml.html
  (TnTmPrelude
   #:title "协调抽象和高性能: MetaOCaml方法"
   #:css "styles.css"
   (H1. "协调抽象和高性能: MetaOCaml方法")
   (H2 "摘要")
   (P "生成式编程的一个常见应用是生成高度特化于手头问题的高性能计算内核. "
      "一个典型的线性代数内核会针对数值域 (有理数, 浮点数, 双精度浮点数, 等等), "
      "循环展开因子, 数组布局和先验知识 (例如, 矩阵是正定的) 进行特化. "
      "手动特化 (编写相同算法的诸多变种) 是乏味无聊且容易出错的.")
   (P "广泛使用的生成器, 例如ATLAS和SPIRAL, 能够可靠地产生高度特化的代码, "
      "但是很难扩展. 对于ATLAS而言, 其使用printf生成代码, "
      "甚至连括号匹配都成为了挑战. 根据ATLAS的作者所言, debug如同梦魇.")
   
   (H2. "引入")
   (H3. "为什么元编程?")
   (H3. "为什么这个教程?")
   (H3. "为什么MetaOCaml")
   (H3. "概览")
   (H3. "获得MetaOCaml")
   (H2. "第一步")
   (H3. "Now or later")
   (H3. "幂")
   (P "尽管已经成为陈词滥调, " (Code "power") "这个例子仍然是介绍元编程的最快方式之一. "
      "以下的" (Code "power") "函数可以计算" $x^n ":"
      (CodeB "let square x = x * x
let rec power : int -> int -> int = fun n x ->
  if n = 0 then 1
  else if n mod 2 = 0 then square(power (n/2) x)
  else x * (power (n-1) x)")
      "我们已经显式地附加了类型签名, 尽管并无必要: 类型可以推断出来. "
      "不过, 给top-level的函数添加签名一般而言是个好的想法: "
      "它可以使得代码更容易编写, 之后更容易理解, 并且能够使得错误信息更加清晰. "
      
      )
   (P "设我们的程序需要计算" $x^7 "很多次, 那么我们可以定义"
      (CodeB "let power7 x = power 7 x")
      "来命名和分享这部分求值.")
   (P "在MetaOCaml里, 我们也可以" (Em "特化") "幂函数于一个特定的值"
      $n ", 得到之后可以接受" $x "并计算" $x^n "的代码. "
      )
   (H2. "滤波")
   (P "滤波在无线电以及各种音声应用之中普遍存在. "
      "当我们想要收听电台90.5 MHz时, "
      )
   (P "和之前一样, 我们将会在与MetaOCaml的交互过程中编写代码, "
      
      )
   (P "数字滤波对于表示为采样或者说一致采样信号值的序列的数字化了的信号进行变换: "
      $x_i "代表在第" $i "次采样的时刻的信号幅度. "
      "这个教程处理的是所谓有限冲激响应 (FIR) 滤波, "
      "具有以下的一般形式:"
      (MB (&= $y_i (sum (&= $k $0) (&- $m $1)
                        (&i* $b_k (_ $x (&- $i $k))))))
      "输出信号的采样" $y_i "是" $m "个最近的输入信号采样"
      (&cm $x_i (_ $x (&- $i $1)) $..h (_ $x (&+ (&- $i $m) $1)))
      "的加权和. 权重" $b_k "是滤波器的系数. 数字" (&- $m $1)
      "被称为滤波器的阶. 既然系数的数目 (一般还有系数的值) 往往是已知的, "
      "那么特化滤波器的计算是很合理的. "
      "滤波器往往需要处理很长的信号, 或者需要在线处理信号.")
   (P "这里我们遇到了我们之前已经说过太多了的一般性和性能之间的取舍, "
      "而且我们还要再遇到无数次. 我们想要以全然的一般性一劳永逸地"
      "写完滤波器的代码, 并且希望其具有对于领域专家而言熟悉的形式. "
      "那么, 我们想要借助于特定的情形 (例如系数已知) 来特化或者优化代码. "
      "我们不会在本章立刻达到理想的境地. "
      
      )
   (P "上述的滤波器等式可以按照以下习用方式在OCaml之中进行编码:"
      (CodeB "let filter : float array -> float array -> float array = fun b x ->
  let m = Array.length b in
  let y i =
    if i &lt; m-1 then x.(i) else
    let sum = ref 0.0 in
    for k = 0 to m-1 do
      sum := !sum +. b.(k) *. x.(i-k)
    done;
    !sum
  in
  Array.init (Array.length x) y        (* essentially, for-loop *)")
      "函数" (Code "filter") "接受系数的数组" (Code "b") "和输入采样的数组"
      (Code "x") ", 返回经过滤波处理的(新鲜分配的)信号采样数组. "
      "在OCaml之中, 取一个数组" (Code "x") "的第" (Code "i")
      "个元素记作" (Code "x.(i)") ". 为了简单起见, 我们没有考虑原地滤波或者"
      "类似于流的滤波. 然而, 本章我们所学到的"
      )
   (H2. "线性代数DSL: 复向量运算和数据布局")
   (P "现在我们转向生成优化代码的系统性方法. "
      "我们的目的在于以清晰和显然正确的方式编写代码, "
      "并且记号近于领域专家所使用的&mdash;&mdash;"
      "然后将这种记号解释为一个代码生成器, "
      "然后逐步添加各种优化. "
      "每个优化都体现了某种领域知识, "
      "由领域专家独立开发. "
      "和前一章作为对比的是, "
      "我们特化和优化代码而不需要不断重写和注解.")
   (P "使得优化模块化和可复合的是一个共同的领域特定语言 (DSL), "
      "或者更准确地说, 是一种特定的DSL实现技巧: "
      "所谓的tagless-final风格. "
      "当在类似于OCaml这样的宿主语言之中嵌入一个DSL时, "
      "一般会将DSL表达式表示为某个代数数据类型的值: "
      "每个DSL语言形式, 诸如数值字面量或者加法, "
      "都对应于这个数据类型的一个数据构造子. "
      "这是所谓的" (Q "深嵌入")
      ". tagless-final方法则是一种替代. "
      "其被定义为从DSL表达式到宿主语言 (OCaml) 的映射, "
      "因而可以视为表达式的意义, 或者说指称. "
      "这个映射是复合性的: 每种DSL表达式形式都对应于一个宿主函数, "
      "其根据其直接组件的意义来计算形式的意义. "
      "因此, tagless-final方法将一个DSL表示为一个代数, "
      "也称为一个" (Q "代数结构")
      ". 我们可以选取各种OCaml的值集合作为DSL表达式的意义. "
      "换言之, 我们可以以不同的方式解释相同的表达式, "
      "这取决于手头上的任务. 一个代码生成器只是其中一种解释. "
      "tagless-final风格的DSL是可扩展的: "
      "特性可以依照愿望增加, 而不需要破坏之前所写过的任何代码. "
      "解释也是可扩展的, 可以根据需要添加(领域特定的)优化. "
      "这种DSL是有类型的. "
      "解释 (代码生成器) 也是 (和MetaOCaml本身一样) 有类型的. "
      "因此, 生成的代码总是良形式, 卫生, 且良类型的. "
      "类型不仅阻止了显然错误的生成器, 而且还能够帮助我们编写生成器. "
      "因此, 在接下来的章节里, 我们将会开发以tagless-final风格"
      "嵌入(Meta)OCaml的DSL.")
   (P "我们的主题是线性代数. 本章处理的向量-向量运算 (也就是所谓的BLAS Level 1) "
      "和数据布局. " (Ref "BLAS2") "继续处理矩阵-向量运算 (BLAS Level 2), "
      "代数化简, 以及领域特定的稀疏矩阵优化. 我们的DSL代码看起来会像是"
      (CodeB "let vmult vout v1 v2 = vout := v1 *. v2
let mvmult vout a v = vout := a * v")
      (S "{译注: 内心小小的疑问, 这里的" (Code "a * v")
         "真不是" (Code "a *. v") "吗? 抑或是甚至"
         (Code "v1 *. v2") "其实是" (Code "v1 * v2")
         "? 总之感觉不是很协调.}")
      " 这里的记号类似于Matlab, 还能生成特化于特定数据布局的命令式循环. "
      "顺便一说, 这里也没有见到bracket和escape {译注: 用于元编程的}. "
      "的确如此, 以精心择取的抽象, 面向用户的代码 (以及许多优化层) "
      "并不会出现台面上的staging注解. 它们只会出现在非常低层次的生成器里.")
   (H3. "数据布局问题")
   (P "这个问题是对于一种特定的数据表示进行实际的特化. "
      "某些计算机架构偏爱所谓的结构的数组, 而GPU和向量处理器则偏爱数组的结构. "
      "将一个程序从GPU移植到超级计算机上或者反过来一般需要改变数据表示. "
      "我们的目的在于自动化这样的改变, 仅仅实现算法一次, "
      "然后机械化地使其适应于不同的数据布局.")
   (P "具体来说, 当前的问题是实现两个复向量的逐点乘法, "
      "以允许轻易特化于不同复向量表示的方式:"
      (Ul (Li (Code "float cmplx array") (Br)
              "一个数组, 其元素都是复数: 浮点数序对, 分别代表"
              $R:fraktur "部和" $I:fraktur
              "部. 这种所谓的结构的数组表示流行于基于缓存的机器之中.")
          (Li "单独的" (Code "float array") " (" $R:fraktur
              "部) 和" (Code "float array") " (" $I:fraktur
              "部)" (Br)
              "向量处理器和GPU更青睐于这种数组的结构表示.")
          (Li "一个单独的" (Code "float array") (Br)
              "前半部分" $R:fraktur "部, 后半部分"
              $I:fraktur "部, 前一种表示的变种.")))
   (P (Table
       #:attr* '((align "center"))
       (Tr (Td (Code "v: float cmplx array"))
           (Td (&blank:8px
                (UnderBrace
                 (Cells (&. $v_0 $R:fraktur)
                        (&. $v_0 $I:fraktur))
                 $v_0)
                (UnderBrace
                 (Cells (&. $v_1 $R:fraktur)
                        (&. $v_1 $I:fraktur))
                 $v_1)
                $..c)))
       (Tr (Td (Code "v': float array cmplx"))
           (Td (&blank:8px
                (UnderBrace
                 (Cells
                  (&. (_prime $v $0) $R:fraktur)
                  (&. (_prime $v $1) $R:fraktur)
                  $..c)
                 (&. $v^ $R:fraktur))
                (UnderBrace
                 (Cells
                  (&. (_prime $v $0) $I:fraktur)
                  (&. (_prime $v $1) $I:fraktur)
                  $..c)
                 (&. $v^ $I:fraktur))))))
      (B "图4.1: ")
      "数据布局: 结构的数组" (Code "v")
      "和数组的结构" (Code "v'"))
   (P "不同布局在图4.1之中进行了系统地比较. "
      "为了使得区别更加明显, "
      )
   (H3. "抽象算术")
   (P "我们开始教授系统性的方法, 通过为能够相加和相乘的东西设计一个DSL. "
      "这看起来像是经典开局, 特别是对于HPC实践者而言: "
      "算术在HPC之中是如此基础, 以至于任何的运算开销都是不可忍受的. "
      "犬儒主义者或许会将我们的DSL当作沉溺于抽象废话 (abstract nonsense) 的借口. "
      "很快我们将会看到, 我们的抽象不仅是实际的, 而且最终还是无开销的.")
   (P "又一次, "
      )
   (P "我们从最小的DSL开始, 只有目前所需的运算. "
      "更多的东西总是可以之后再加: "
      "可扩展性是tagless-final方法的优点. "
      "暂时我们需要的是一集我们可以做加法, 减法, 乘法的对象, 带有两个突出元素"
      (Code "zero") "和" (Code "one") ". 这集操作可以组织为所谓的(模块)"
      (Em "签名") ":"
      (CodeB "module type RING = sig
  type t
  val zero : t
  val one  : t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
end")
      "声明" (Code "type t") "给我们的对象的类型赋予了一个(非常没有想象力的)名字. "
      "这个类型是抽象的: 我们对其一无所知, 除了它的名字" (Code "t")
      ". 具体的实现可以实现" (Code "t") "为浮点数或者复数之类的东西. "
      "然而, DSL代码并不能知晓或者依赖于" (Code "t")
      "的具体实现 (这是OCaml的类型系统所控制的), 因而可以应用于任何可能的实现. "
      "我们也可以选取更加fancy的名字, 比如说" (Code "+%")
      "甚至是" (Code "+") "而非" (Code "add") ".")
   (P "暂时我们仅仅是定义了" (Code "t") "上的运算的名字和它们的元数. "
      "这些运算被隐式地假定满足我们所熟悉的性质: "
      (Code "add") "是交换且结合的, 并且具有单位元" (Code "zero")
      "; " (Code "mul") "是结合的, 且具有单位元" (Code "one")
      "和零元" (Code "zero") " {译注: " (Code "zero")
      "是所谓的零元的事实可由环的公理导出}; " (Code "mul")
      "对于" (Code "add") "分配. 换言之, 我们是要定义一个数学上的"
      (Em "环") ". 尽管如此, 我们还没有在地方陈述过这些代数律. "
      (Ref "BLAS2") "里我们将会接触它们.")
   (P "我们可以将签名" (Code "RING") "视为定义了一个迷你DSL, "
      "其具有两个常量和三个运算. (实际上, 这就是tagless-final的观点.) "
      "这个DSL可以立刻使用:"
      (CodeB "module ExArith(R:RING) = struct
  open R
  let xsq1 x = add (mul x x) one
end
(* module ExArith : functor (R : RING) -> sig val xsq1 : R.t -> R.t end *)")
      
      )
   (P "为了尝试刚才定义的DSL函数, 我们需要" (Code "RING")
      "的一个实现. "
      )
   (H3. "对于向量进行抽象")
   (P "特化于各种各样的向量内存布局呼吁着找到一个合适的对于向量的抽象表示. "
      "一个常规的浮点数数组或许可以想成是一个函数" (Code "int -> float")
      ", 当然取值是一些特定的整数. 如果推广这个想法, "
      "我们可以定义(输入)向量为一个函数, 其取一个索引, 返回这个索引位置的值; "
      "向量也应该携带关于向量定义域范围的最小和最大索引的信息."
      (CodeB "type ('i,'a) vec = Vec of 'i * ('i -> 'a)")
      "索引" $iota "被假定是一个可枚举的类型. "
      "为了简单起见, 我们默认索引范围的下界是零值, "
      "而其只需携带上界的信息: 索引范围的长度. 这种" (Code "vec")
      "有时也被称为" (Q "pull vector") ". 数组是可变的数据结构. "
      "为了表示数组赋值, 我们引入输出向量:"
      (CodeB "type ('i,'a,'w) ovec = OVec of 'i * ('i -> 'a -> 'w)")
      "其是向量长度和变动函数 (mutation function) 的序对. "
      "后者取一个索引和一个存储于该索引位置的值, "
      "然后返回更新过了的向量的某种表示. "
      "对于破坏性的复制, 结果" $omega "可以是" (Code "unit") ".")
   (P "为了获得对于抽象向量更为真切的感受, "
      )
   (H3. "向量算术DSL")
   (H4. "湘南挑战1")
   
   (H4. "BLAS 2 DSL")
   
   (H2. "线性代数DSL: 矩阵-向量运算和模块化优化" #:id "BLAS2")
   
   (H2. "从解释器到编译器: 图像操作DSL")
   
   ))