#lang racket
(provide having_an_effect.html)
(require SMathML)
(define :Expr (Code "Expr"))
(define :Dom (Code "Dom"))
(define :DError (Code "DError"))
(define :DInt (Code "DInt"))
(define :eval (Code "eval"))
(define :EInt (Code "EInt"))
(define :EInc (Code "EInc"))
(define :tinc (Code "tinc"))
(define :tif (Code "tif"))
(define :EBasic (Code "EBasic"))
(define :ECond (Code "ECond"))
(define :ttinc (Code "ttinc"))
(define :ttif (Code "ttif"))
(define :ttS (Code "ttS"))
(define :app (Code "app"))
(define :inc (Code "inc"))
(define :get (Code "get"))
(define :put (Code "put"))
(define :if_ (Code "if_"))
(define :d (Code "d"))
(define :DomIntState (Code "DomIntState"))
(define :DomFCF (Code "DomFCF"))
(define :var (Code "var"))
(define :lam (Code "lam"))
(define :Comp (Code "Comp"))
(define :DomC (Code "DomC"))
(define :ReqT (Code "ReqT"))
(define :ReqError (Code "ReqError"))
(define :err (Code "err"))
(define :bind (Code "bind"))
(define :ReqState (Code "ReqState"))
(define :Get (Code "Get"))
(define :handleState (Code "handleState"))
(define :ReqHO (Code "ReqHO"))
(define :ReqVar (Code "ReqVar"))
(define :ReqClosure (Code "ReqClosure"))
(define :handleVarD (Code "handleVarD"))
(define :handleVar (Code "handleVar"))
(define :th0 (Code "th0"))
(define :v (Code "v"))
(define :z (Code "z"))
(define :body (Code "body"))
(define :new (Code "new"))
(define $ask (Mi "ask"))
(define $int (Mi "int"))
(define $handle (Mi "handle"))
(define $with (Mi "with"))
(define $10 (Mn "10"))
(define split2 (&split 2))
(define (Ty e a)
  (ap (ap $T e) a))
(define (Judg M t)
  (: $vdash M $: t))
(define (Lam x e)
  (: $lambda x $. e))
(define (Handle e op v)
  (split2 $handle e $with (: op $=> v)))
(define (Eff . x*)
  (apply setE x*))
(define (Ann op t)
  (: op $: t))
(define having_an_effect.html
  (TnTmPrelude
   #:title "Having an Effect"
   #:css "styles.css"
   (H1. "Having an Effect")
   (P "Oleg Kiselyov (okmij.org). 当前版本: 2017年9月; 页面最后更新于2025年9月5日.")
   (H2 "引论")
   (P "这项研究是一段追随一条诱人线索的旅程: 关于单子 (monad), 单子变换器 (monad transformer), "
      "自由单子 (free monad) 和可扩展作用 (extensible effect) 的奠基性论文全都是关于可扩展解释器的. "
      "一路上我们已经意识到, 表达式问题 (expression problem), 稳定指称 (stable denotation), "
      "以及可扩展解释器全都是同一事物的不同名字. 我们最终找到了组织原则: 交互" --
      "在客户端和服务器之间, 解释器和被解释的代码之间, 表达式和其上下文之间.")
   (P "很可能交互最好是在进程演算 (process calculus) 中表达. 在顺序演算中, Cartwright和Felleisen的"
      (Q "Extensible Denotational Language Specifications")
      "是将作用作为交互的最早的完全成熟的严格陈述.")
   (P "本报告给出了Cartwright和Felleisen的方法的现代重构. 我们的重构更简单, "
      "得益于tagless-final陈述, 并且可以立即执行.")
   (P "进一步发展, 我们使得作用处理器 (effect handler) 本身是模块化的和可扩展的" --
      "成为程序的一部分, 而不是永久地驻扎在外面. 这使得以下这一步成为可能:")
   (P "Cartwright和Felleisen不得不将变量环境烤进基本的语义框架之中, 即便整数, "
      "或者一般而言的一阶子语言, 并不使用它. 我们避免了假定变量环境以及对于高阶的任何特殊处理. "
      "毕竟, 我们将Lambda与State平等对待, 在同一个统一的形式体系之中. "
      "动态或者词法绑定或者各种调用约定归结为处理解引用一个绑定变量的作用的不同方式.")
   (P "重访这一领域的起源并找回其洞见和被遗忘的替代方案有助于使得我们的程序具有预期的作用.")
   (P "本报告首先于2016年8月25日在Indiana大学信息学与计算学院 (SoIC) "
      "计算机科学讨论会上呈现. 我感谢Martin Berger的许多有益的讨论, 特别是解释了进程演算的起源. "
      "感谢Shin-Cheng Mu和Matthias Felleisen的有益评论.")
   (P (B "参考文献")
      (Ul (Li (Code "EDSLNG.hs") -- "本报告背后完整的, 基本上是Haskell98的代码.")
          (Li (Code "edlsng.ml") -- "OCaml版本: Cartwright-Felleisen框架的现代重构. "
              "状态, 动态绑定, 传值调用和传名调用的词法绑定是示例作用.")
          (Li "于2016年12月2日在台湾台北中央研究院资讯科学研究所讨论会上的报告录像: "
              (A "https://www.youtube.com/watch?v=GhERMBT7u4w"
                 #:attr* '((href "https://www.youtube.com/watch?v=GhERMBT7u4w"))))))
   (H2. "不稳定的指称, 脆弱的解释器, 难以应用的理论" #:id "unstable-denotations")
   (H3. "从" (Q "实用") "角度来看指称语义")
   (P "既然 (副) 作用无处不在又富有争议, 它们就应该被研究. 而它们也已经被研究过了, 尤其是, "
      "被1970年代的指称语义维也纳学派" -- "其目标是理解PL/I. 我们也将使用指称语义" --
      "但是带有实用的倾向, 朝向 (定义性) 解释器. 它最终将我们引向" (Q "可扩展作用")
      -- "一种在现实程序中管理作用的实用方式.")
   (P "在这一节中, 我们演示众所周知的不稳定指称的问题. 它是使用指称语义研究和定义现实语言及其作用的绊脚石, "
      "正如John Reynolds早在1974年首先指出的那样. " (Ref "stable-denotations")
      "带来了来自进程演算的洞见以克服这一问题.")
   (P "为了具体起见, 我们将使用Haskell来编写我们的定义性解释器, 尽管OCaml, F#或者Scala, "
      "以及其他语言, 也同样可以. 即便是不熟悉Haskell的读者, 希望也能够通过观察代码并与标准数学记号进行类比, "
      "而领会主要思想. 毕竟, 当拜访异国他乡时, 尽管有语言和文化的隔阂, 我们也能设法生存下来并享受旅程. "
      "交流的意愿大有帮助.")
   (H3. "定义性解释器")
   (P "我们在一个简单到令人尴尬的例子上解释指称语义的定义性解释器方法. 尽管它很平凡, "
      "这个例子已经能够说明需要可扩展解释器的理由.")
   (P "我们从最简单的算术表达式语言开始: 整数数码, 增量函数, 以及让我们能够应用增量的应用运算符. "
      "在Haskell中, 我们将这个语言定义为数据类型" :Expr ":"
      (CodeB "data Expr = EInt Int | EInc | EApp Expr Expr")
      "于是一个示例表达式" -- "两次增量的数码2" -- "看起来就像" :tinc ":"
      (CodeB "tinc = EApp EInc (EApp EInc (EInt 2)) -- inc (inc 2)"))
   (P "现在我们引入指称语义以赋予" :Expr "意义. 回忆一下, 指称语义是一个从" :Expr
      "到其他某种东西的复合性的映射. 我们取下面这个在某种程度上通用的domain " :Dom
      "作为我们的语言中的表达式的意义, 或者说指称. 它包含整数, 布尔值, 函数, "
      "以及一个代表无意义的表达式的意义的特殊元素."
      (CodeB "data Dom = DError | DInt Int | DBool Bool | DFun (Dom -> Dom)")
      "于是语义, 即从表达式" :Expr "到其指称" :Dom "的映射, 被构造性地定义为Haskell函数"
      :eval ". 它是我们的简单语言的一个解释器."
      (CodeB "eval :: Expr -> Dom
eval (EInt x) = DInt x
eval EInc     = DFun $ \\x -> case x of
                          DInt n -> DInt (succ n)
                          _      -> DError
eval (EApp e1 e2) = case eval e1 of
  DFun f -> f (eval e2)
  _      -> DError"))
   (P (Code "EApp EInc (EInt 8)") "表示我们的语言中的句法成分 (一个纯粹的字符串) "
      (Code "inc 8") ", 而" (Code "DInt 9") "是其意义, 一个Haskell整数. " :eval
      "的最后一个子句展示了复合性: " (Code "EApp e1 e2") "的意义仅依赖于" (Code "eval e1")
      "和" (Code "eval e2") ": 也就是说, 依赖于参数" (Code "e1") "和" (Code "e2")
      "的意义, 而不是依赖于参数本身, 即不依赖于它们的结构. 因此, "
      "指称语义是由一个结构归纳的解释器定义的, 即一个fold.")
   (P "这个语言迫切需要新的特性; 例如, 布尔值, 整数上的相等检查, 以及条件表达式. 唉, "
      "Haskell中的数据类型是不可扩展的. 因此, 我们不得不重新定义" :Expr "以容纳这些扩展:"
      (CodeB "data Expr = EInt Int | EInc | EApp Expr Expr
          | EEq | EIf Expr Expr Expr")
      "一个示例条件表达式"
      (CodeB "if 3 == inc (inc 2) then 10 else inc (inc (inc 2))")
      "看起来就像下面的" :tif ". 我们想要重用之前为" (Code "inc (inc 2)") "定义的"
      :tinc "但是不能: 类型" :Expr "已经改变了. 我们不得不使用新的" :Expr "重新定义"
      :tinc ", 尽管它看起来与之前完全一样."
      (CodeB "tinc1 = EApp EInc (EApp EInc (EInt 2))
tif   = EIf (EApp (EApp EEq (EInt 3)) tinc1) (EInt 10) (EApp EInc tinc1)"))
   (P "解释器" :eval "赋予扩展后的语言以意义. 前三个子句看起来与旧的解释器完全一样. 唉, "
      "我们不能引用它而不得不重新写: Haskell中的函数也是不可扩展的."
      (CodeB "eval :: Expr -> Dom
eval (EInt x)      = DInt x
eval EInc          = -- ... as before ...
eval (EApp e1 e2)  = -- ... as before ...
eval EEq           = -- similar to EInc
eval (EIf e et ef) = case eval e of
  DBool True  -> eval et
  DBool False -> eval ef
  _           -> DError"))
   (P "因此我们遇到了不稳定性, 或者说表达式问题: 我们的指称语义是脆弱的而且不可扩展. "
      "我们不能重用之前写过的示例项和之前写过的解释器; 我们不得不重写它们. 可以说, "
      "这是Haskell形式化的问题而不是指称语义的问题. 但它仍然很能说明问题. "
      "通常指称语义是以数学的方式陈述的, 而数学通常是非形式的, 尽管仍然是严格的. "
      "许多问题在形式化中才浮现出来. 幸运的是, 有一种方法可以使得我们的解释器可扩展.")
   (H3. "可扩展解释器")
   (P "让我们回到最简单的整数和增量的语言, 并以不同的方式定义它. 不使用随后不得不被解释为"
      :Dom "的数据类型, 让我们通过直接告知每个语言句法成分的意义, "
      "以及如何从其组成部分的意义理解一个复合句法成分, 来定义一个语言. "
      "因此我们一开始就引入指称语义, 而没有中间的" :eval "步骤. 毕竟, "
      "定义语义包括定义语言的结构 (句法). 为了一般性, 这在后面会派上用场, 我们并不将指称的domain固定为"
      :Dom ". 而是, 我们使它成为变量" :d ". 总而言之, 我们通过一组定义来定义一个语言, "
      "每个句法形式一个定义, 每个定义都为该形式定义了domain映射. domain保持抽象. "
      "因此我们将一个语言定义为Haskell中的一个类型类 (或者OCaml中的一个模块类型, 等等):"
      (CodeB "class EBasic d where
  int :: Int -> d
  inc :: d
  app :: d -> d -> d

infixl 1 `app`   -- make the infix `app` left-associative")
      "这个类型类说的是我们的语言有两个原语, 整数数码和增量, 以及一个复合表达式, 带有两个子表达式的"
      :app ". 复合表达式的意义是由子表达式的意义" :d "决定的. 语义确实是复合性的, "
      "这是由其设计本身决定的. 示例项" (Code "inc (inc 2)") "现在采取以下形式:"
      (CodeB "ttinc = inc `app` (inc `app` int 2)")
      "其推导出的类型为" (Code "ttinc :: EBasic d => d") ". 可以将其读作: " :ttinc
      "是" :EBasic "语言中的项. 或者: 给定一个合适的domain" -- "对于我们的语言而言足够有表达力的" --
      :ttinc "给出了句法成分" (Code "inc (inc 2)") "在该domain中的意义. 剩下的就是告知"
      :Dom "是" (Q "合适的") "以赋予基本语言意义:"
      (CodeB "instance EBasic Dom where
  int = DInt
  inc = injI succ
  app (DFun f) e2 = f e2
  app _ _         = DError")
      "其中我们写" (Code "injI succ") "表示被提升至" :Dom "的Haskell后继函数, 如果要增量的值不是一个"
      :DInt "就返回" :DError ". 因此示例表达式的意义为" (Code "ttinc :: Dom") ", 也就是说, "
      :ttinc "特化至domain类型" :Dom ".")
   (P "和之前一样, 我们将一个表达式映射到了其意义, 即" :Dom "的元素. 早先, "
      "这个映射是由Haskell函数" :eval "实现的. 现在, 它被内置于" :ttinc "本身, "
      "而原语的映射被收集在" :EBasic "中. 因此我们已经认识到了数学映射并不一定要对应于一个Haskell函数. "
      "还有其他方式可以编写它.")
   (P "数学是抽象的, 这是一种福气" -- "它适用于如此之多的情形" -- "也是一种诅咒: 人们必须知道如何应用它.")
   (P "在新的方法中扩展语言要容易得多. 为了添加相等和条件形式, 我们只需写下相应的定义, "
      "将它们收集到一个新的类型类之中:"
      (CodeB "class ECond d where
  eq  :: d
  if_ :: d -> d -> d -> d")
      "新的特性立即就可以使用:"
      (CodeB "ttif = if_ (eq `app` (int 3) `app` ttinc) (int 10) (inc `app` ttinc)")
      "现在, 我们重用了之前写的项" :ttinc "而无需重写它. 类似地, 我们重用, 而不是重写解释器. "
      :ttif "的推导出的类型, 即" (Code "(EBasic d, ECond d) => d") ", "
      "告诉我们为了使其有意义, 它需要基本原语和条件原语的domain映射. " (Code "EBasic Dom")
      "映射早先已经定义过了并且可以原样使用. 剩下的就是告知新特性的意义:"
      (CodeB "instance ECond Dom where
  eq = -- like inc
  if_ (DBool True)  et ef = et
  if_ (DBool False) et ef = ef
  if_ _             _  _  = DError"))
   (P "总结一下, 定义语言的新方式, 通过指称语义, 让我们能够轻易地扩展语言" -- "重用而不是重写, "
      "之前写过的定义, 解释器和表达式. 因此我们得到了用于语义分析的方便的框架. "
      "现在我们将把它应用于真正的作用, 状态.")
   (P (B "参考文献")
      (Ul (Li "Tagless-final风格")))
   (H3. "定义状态")
   (P "这一小节为增量和条件的语言添加第一个真正的作用: 全局可变整数状态. "
      "它也让我们直面指称语义的真正问题.")
   (P "起初, 添加带有其两个操作的状态作用进展顺利. 我们定义原语以访问当前状态以及改变它, 返回新的值:"
      (CodeB "class GlobalIntState d where
  get :: d
  put :: d -> d")
      "示例项"
      (CodeB "ttS :: (EBasic d, ECond d, GlobalIntState d) => d
ttS = if_ (eq `app` (int 3) `app` (put ttinc))
          (put (int 10)) (put (inc `app` get))")
      "展示了新的操作与旧的操作并用: 早先的项" :ttinc "中的增量. 赋予" :ttS
      "意义要比写出它困难得多. 我们究竟可以将" :get "映射到哪个" :Dom
      "元素上呢? 当前状态是可变的, 没有固定的值. 我们需要一个新的语义domain, 即状态变换器 (state transformer), "
      "其将当前状态映射到新的状态."
      (CodeB "type DomIntState = Int -> (Dom,Int)

instance GlobalIntState DomIntState where
  get   = \\s -> (DInt s,s)
  put e = \\s -> case e s of
      (DInt s',_) -> (DInt s',s')
      (_,s)       -> (DError,s)")
      :get "和" :put "的意义变得容易表达: " :get "代表当前状态, 其保持不变.")
   (P "唉, 为了赋予" :ttS "意义, 我们也需要" (Code "EBasic DomIntState") "和"
      (Code "ECond DomIntState") "实例: 旧特性在新domain中的解释. 诚然, 这是直截了当的, "
      "仅仅是修饰和重用早先的定义, 例如" :app ":"
      (CodeB "instance EBasic DomIntState where
  int x = \\s -> (DInt x,s)
  inc   = \\s -> (injI succ,s)
  app e1 e2 = \\s0 -> let (f,s1) = e1 s0
                         (x,s2) = e2 s1
                     in (app f x,s2)

instance ECond DomIntState where
  eq = \\s -> (eq,s)
  if_ e  et ef = \\s -> case e s of
    (DBool True,s)  -> et s
    (DBool False,s) -> ef s"))
   (P "尽管" :app "的意义是合理的并且对于许多例子而言足够了, 但它并不如应有的那般一般. "
      "请读者思考为什么. 第二个小测验是为什么我们无法重用" :if_ "的" :Dom "解释.")
   (P "问题不在于为新的domain编写" :EBasic ", " :ECond "等实例的复杂性, "
      "而在于我们不得不这样做这一事实. 如果我们想要添加带有若干分量 (可变变量) 的全局状态呢? "
      "我们不得不引入另一个domain, 并且再一次编写" :EBasic ", " :ECond
      "实例. 对于具有许多特性的语言而言, 这是一个重大的实际问题. 这也是令人深感不满的: "
      "诸如" (Code "EInt 1") "这样的整数数码或者增量" :EInc "的" (Q "真正") "意义并没有改变. "
      "增量的意义是整数上的后继" -- "无论语言是否具有状态. 整数永远是整数, "
      "而不应该在为了越来越复杂的特性而扩展指称的domain时不得不被重新构想.")
   (H3. "第一级函数?")
   (P "第一级函数和状态一样容易添加, 至少在一开始是这样. 我们只需写下基本操作的签名: "
      "访问绑定于一个 (具名) 变量的值, 以及创建一个函数."
      (CodeB "type VarName = String

class Lam d where
  var :: VarName -> d
  lam :: VarName -> d -> d")
      "麻烦在于赋予这些操作意义. 早先的domain没有一个能够胜任: 一个变量没有任何与之关联的固定值; "
      "绑定的值只有在函数被应用时才被确定, 而这可以发生许多次, 并且应用于不同的参数. "
      "我们需要一个新的domain, 反映一个表达式的意义依赖于其上下文: 为绑定变量赋予指称的环境:"
      (CodeB "type DomFCF = Env -> Dom
type Env    = VarName -> Dom")
      "尽管" :DomFCF "看起来可能对于" :var "和" :lam "有效, 实际上并不是. 请读者思考为什么.")
   (P "如果我们想要传名调用的函数呢? 协程呢? 非确定性呢? 我们需要越来越多的domain. "
      "每个新的domain都需要重写" :EBasic ", " :ECond
      "实例, 以给出整数, 增量在这些更为花哨的domain中的意义. 这就是问题所在: "
      "表达式不具有稳定的指称. 当语言被扩展时, 旧的表达式突然改变了意义.")
   (H2. "作用和交互")
   (P "不稳定的指称反映了一个表达式与其上下文的交互. 编程语言语义的传统方法, 从Landin和Strachey以降, "
      "对于交互几乎没有什么可说的. Robin Milner在他的访谈中说得很好:"
      (Blockquote
       (Q "但是与此同时 [大约1971年] 我不知怎么地, 我也不知道是如何, 对并发产生了兴趣. "
          "我记得, 并没有特别将其与验证联系起来, 我对交互自动机感到好奇. "
          "我有一个想法, 即自动机理论是不充分的, 因为它没有说两个自动机彼此交互是什么意思. "
          "除了Krohn-Rhodes表示定理, 其说了一些关于将一个自动机的输出馈入另一个的东西. "
          "但是自动机之间并没有交互."))
      "交互这一主题可以追溯至:"
      (Ul (Li "Petri, 1939年 (13岁时) 和1962年 (博士论文)")
          (Li "Hewitt, 1976年")
          (Li "第五代计算机计划, 1982-1992年")
          (Li "Robin Milner, 1980年代")
          (Li "本田耕平 (Kohei Honda), 1980年代末")
          (Li "Cartwright和Felleisen, 1994年")))
   (P "特别值得注意的是Hewitt的有影响力的笔记"
      (Q "Viewing Control Structures as Patterns of Passing Messages")
      ": 它展示了递归, 迭代和" (Q "复杂的") "控制结构如何都可以用参与者 (actor) 之间的交互来表示. "
      "听说进程演算的所有进一步发展都是在使得Hewitt的想法更加精确和形式化. "
      "就作用而言, 其逻辑上的顶点是Cartwright和Felleisen的工作, 我们在下面以现代语言和实用的术语重构它.")
   (P "同样值得注意的是Abramsky和McCusker的Reynolds理想化Algol的博弈语义" --
      "其本质上是一个纯函数式语言, 除了" :new ". Abramsky和McCusker的论文明确地将副作用称为一种交互:"
      (Blockquote
       (Q "我们想要将" (Code "new x in C") "理解为将类型为" (Code "var[X]") "的自由标识符"
          (Code "x") "绑定到一个给出存储单元的行为的" (Q "对象") "或者" (Q "进程") ". 于是"
          (Code "new x in C") "的行为来源于" (Code "C") "与这个单元之间的交互...")))
   (P (B "参考文献")
      (Ul (Li "Robin Milner访谈. "
              (A "http://users.sussex.ac.uk/~mfb21/interviews/milner/"
                 #:attr* '((href "http://users.sussex.ac.uk/~mfb21/interviews/milner/"))))
          (Li "Carl Hewitt: Viewing Control Structures as Patterns of Passing Messages. "
              "MIT A.I. Memo 410, 1976.")
          (Li "Robert Cartwright, Matthias Felleisen: Extensible Denotational Language Specifications. "
              "Symposium on Theoretical Aspects of Computer Software, 1994. LNCS 789, pp. 244-272.")
          (Li "Samson Abramsky and Guy McCusker: Linearity, Sharing and State: "
              "a fully abstract game semantics for Idealized Algol with active expressions. "
              "Electr. Notes Theor. Comput. Sci., 1996, v3, pp. 2-14.")))
   (H2. "稳定指称, 可扩展作用" #:id "stable-denotations")
   (H3. "迈向稳定指称")
   (P "现在我们以一种显式考虑表达式与上下文的交互的方式来定义表达式的意义. "
      "我们追随Cartwright和Felleisen的引领:"
      (Blockquote
       (Q "一个完整的程序被认为是一个与外部世界, 例如文件系统, 交互, 并且影响全局资源, 例如存储, 的代理. "
          "一个中央权威管理这些资源. 一个程序句法成分的意义是一个计算, 其可以是一个值或者一个作用. "
          "如果一个程序句法成分的意义是一个作用, 它就被传播至中央权威. "
          "传播过程为作用包添加一个函数, 使得中央权威可以恢复被挂起的计算."))
      "Cartwright和Felleisen的想法可以立即编写出来:"
      (CodeB "data Comp = Done DomC | Req ReqT (DomC -> Comp)

data DomC = DCInt Int | DCBool Bool | DCFun (DomC -> Comp)")
      "从现在开始, 一个表达式将意味着" :Comp ", 其为" :DomC " (类似于旧的" :Dom
      ") 和请求的不交并. 一个请求总是包括" (Q "返回") "地址" (Code "(DomC -> Comp)")
      ": 从一个回复到接收者的意义的映射. 一个请求还包括" (Q "载荷") " " :ReqT
      ", 告知究竟请求的是什么. 目前, 载荷是" :ReqError ", 代表一个错误消息 (我们稍后揭示更多的选项)."
      (CodeB "data ReqT = ReqError ...

err :: Comp
err = Req ReqError (\\_ -> err)")
      "函数" :err "发送这样的请求, 其指称一个错误的计算. 与" (Ref "unstable-denotations") "中的" :Dom "不同, " :DomC
      "不再具有" :DError "元素. 错误现在被视为一个作用: 一个终止计算的请求.")
   (P "以" :Comp "来给出基本语言的意义是相当直截了当的. 我们再写一个" :EBasic
      "实例; 然而, 这是我们最后一次不得不这样做."
      (CodeB "instance EBasic Comp where
  -- old
  int = Done . DCInt
  inc = Done . DCFun $ \\x -> case x of
    DCInt x -> int (succ x)
    _       -> err
  app (Done (DCFun f)) (Done e2) = f e2
  app (Done _)         (Done e2) = err
  -- new
  app (Req r k) e2 = Req r (\\x -> app (k x) e2)
  app e1 (Req r k) = Req r (\\x -> app e1 (k x))")
      "大部分代码都类似于" (Code "EBasic Dom") "实例, 这并不令人惊讶. 值得注意的是, "
      "整数数码被映射到" :DomC "的整数, 无论语言中可能有什么作用 (请求): 整数永远是整数. 类似地, "
      :inc "总是意味着整数上的后继. " (Code "EBasic Comp") "的新东西是最后两个" :app
      "子句中的作用传播: 如果一个应用的参数是一个请求, 那么整个应用就意味着同一个请求, "
      "但是带有修改后的返回地址: 在回复被递送之后, 应用被重新尝试. 的确, 作用从子表达式向上传播.")
   (P "示例项" :ttinc "作为" :Comp "具有以下意义. "
      "(早先写的项在domain上是多态的, 因此其定义可以原样使用. 为了便于参考, 我们在下面重复它.)"
      (CodeB "ttinc :: EBasic d => d
ttinc = inc `app` (inc `app` int 2)

*EDSLNG> ttinc :: Comp
Done (DCInt 4)")
      "条件表达式以" :Comp "表示的意义同样直截了当:"
      (CodeB "instance ECond Comp where
  eq = ... like inc

  if_ (Done (DCBool True))  et ef = et
  if_ (Done (DCBool False)) et ef = ef
  if_ (Done _)              _  _  = err
  -- new
  if_ (Req r k) et ef = Req r (\\x -> if_ (k x) et ef)")
      "同样, 其大部分都类似于旧的" (Code "ECond Dom") "实例, 除了最后一行, 其描述了作用传播. "
      "这开始看起来有点无聊了: 这一行与我们刚刚为" :app
      "写的作用传播的那几行没有多大区别. 让我们将这个共同的模式提取出来并写一个更清晰的"
      (Code "ECond Comp") "实例:"
      (CodeB "instance ECond Comp where
  eq = ...
  if_ e et ef = bind e $ \\x -> case x of
    DCBool True  -> et
    DCBool False -> ef
    _            -> err

bind :: Comp -> (DomC -> Comp) -> Comp
bind (Done x) k    = k x
bind (Req r k1) k2 = Req r (\\x -> bind (k1 x) k2)")
      "这里" (Code "bind e k") "表达了消耗" (Code "e") "的值的共同模式. "
      "如果那个表达式不是一个值而是发送一个请求, 那么该请求就被传播, 带有更新后的返回地址. "
      "看起来很眼熟? 的确, 单子在交互的视角中自然地出现. 事实上, Mike Spivey, "
      "在追随与我们刚刚展示的并无二致的错误传播线索时, "
      "大致与Moggi同时提出了将范畴论的单子应用于编程语言.")
   (P (B "参考文献")
      (Ul (Li "Eugenio Moggi: Computational lambda-calculus and monads. LICS 1989.")
          (Li "Mike Spivey: A functional theory of exceptions. Science of Computer Programming, v14, 1990.")))
   (H3. "状态, 无痛地")
   (P "到目前为止, 唯一的作用一直是错误传播. 当我们添加一个真正的作用: 全局可变整数状态, "
      "新方法的关键时刻就到来了. 我们需要再写一个" :EBasic "实例吗? 带有其旧的指称的旧示例项"
      (Code "ttinc :: Comp") "可以与新的带有作用的操作并用吗? 我们将会找到答案.")
   (P "用于检索和更新当前状态的操作" :get "和" :put "已经在" (Ref "unstable-denotations") "中定义过了. 为了便于参考, "
      "我们在下面重复这些定义, 以及示例项" :ttS "."
      (CodeB "class GlobalIntState d where
  get :: d
  put :: d -> d

ttS :: (EBasic d, ECond d, GlobalIntState d) => d
ttS = if_ (eq `app` (int 3) `app` (put ttinc))
          (put (int 10)) (put (inc `app` get))")
      "为了赋予" :get ", " :put "以及最终" :ttS "意义, " (Ref "unstable-denotations") "不得不引入特别的指称domain "
      :DomIntState ", 以及" :EBasic "和其他类型类的相应实例. 在新的方法中, 早先定义的" :Comp
      "就足够了. 我们只需揭示" :ReqT "的更多部分: 访问和更新状态的请求."
      (CodeB "data ReqT = ReqError | ReqState StateReq ...
data StateReq = Get | Put Int

instance GlobalIntState Comp where
  get   = Req (ReqState Get) Done
  put e = bind e $ \\x -> case x of
    DCInt x -> Req (ReqState (Put x)) Done
    _       -> err")
      "因此" :get "和" :put "的意义本质上就是" :ReqState "请求: 例如, " :get
      "发送请求并立即返回回复. 既然" :Comp "没有改变, 而实例" (Code "EBasic Comp") "和"
      (Code "ECond Comp") "早先已经写好了, 我们立即就可以看到" :ttS "的意义:"
      (CodeB "*EDSLNG> ttS :: Comp
ReqState (Put 4)")
      "项" :ttS "意味着将状态更新为4的请求; 意义的其余部分 (GHCi没有打印) 依赖于对该请求的回复. "
      "这是能做到的最好的了, 因为还没有任何请求处理器. " :ReqError "并不是用来被回复的: "
      "一个未处理的请求总是表示一个错误. 另一方面, 人们应该回复" :ReqState
      ", 并且按照请求访问和更新全局状态."
      (CodeB "handleState :: Int -> Comp -> Comp
handleState _ (Done x)    = Done x
handleState s (Req (ReqState r) k) = case r of
  Get   -> handleState s $ k (DCInt s)
  Put s -> handleState s $ k (DCInt s)
-- everything else
handleState s (Req r k) = Req r (handleState s . k)")
      "处理器" :handleState "就是Cartwright和Felleisen所说的权威, 负责资源" -- "在我们的情形中是状态" --
      "并且回复请求. 当要处理的计算是" :Get "请求时, 处理器以当前状态回复, "
      "并继续处理被回复了的计算, 以防它有其他请求. " :handleState "中的最后一行处理"
      :ReqState "以外的请求; 它们被向上传播, 或者说, " (Q "发送给更高的权威")
      ". 这一行就是我们的方法与Cartwright和Felleisen的方法的不同之处: 对于我们而言, "
      "并没有驻扎在程序之外的中央权威. 我们的处理器是局部化的: 是程序的一部分, 形成一个官僚体系. "
      "一个人处理不了的东西就被发送给上级.")
   (P "示例表达式" :ttS "在初始状态0下使用状态处理器, 即" (Code "handleState 0 ttS")
      ", 具有" (Code "DCInt 5") "的意义. 我们已经指出过处理器是递归的: 在接收到一个回复之后, "
      "一个表达式可能发送进一步的请求, 依次被处理. 因此严格来说, "
      "处理器所建立的数学映射是一个不动点" -- "而我们不得不为一个不终止的处理计算隐式地引入"
      (Code "Done bottom") ". 我们之前没有看到底元素 (bottom), 因为我们没有发散的计算. "
      "顺便一提, 一个发送无尽的" :Get "请求流的表达式并不是发散的: 其意义是富有成效的请求流.")
   (P "因此Cartwright和Felleisen的想法确实有效! 我们添加了一个真正的作用, "
      "而无需重写任何早先的domain和解释器. 人们可以同样无痛地引入其他种类的可变状态, "
      "或者一般而言的其他作用. 整数数码总是意味着整数, 并且可以在任意带有作用的表达式中被如此使用. "
      "Cartwright和Felleisen的想法甚至比作者们所描述的效果更好: 我们使得处理器本身是模块化的, "
      "每个负责其自己的请求.")
   (H3. "第一级函数, 第一次尝试")
   (P "早先在" (Ref "unstable-denotations") "中, 我们试图用第一级函数扩展语言, 结果发现这需要如此深入和众多的修改, "
      "以至于我们放弃了. 让我们在稳定指称的框架中再试一次. "
      "我们将要做的事情在作用即交互的框架中尚未有人做过.")
   (P (Ref "unstable-denotations") "以如下操作引入了第一级函数, 通过示例项" :th0 "演示:"
      (CodeB "type VarName = String

class Lam d where
  var :: VarName -> d
  lam :: VarName -> d -> d

th0 = lam &quot;x&quot; (inc `app` var &quot;x&quot;) -- increment (eta-expanded)")
      "现在我们以" :Comp "给出" :var "和" :lam "的意义. 用于访问绑定值的操作" :var
      "感觉起来很像早先用于访问当前状态的" :get "操作. 既然绑定不会被修改, "
      "最好将其视为环境而不是状态. 与早先的全局状态不同, 绑定的环境局部于一个应用. "
      "这里在我们的框架中 (与Cartwright和Felleisen的不同) 处理器可以出现在表达式中确实很有帮助. "
      "这些提示已经足以写下语义了: 将" :var "解释为" :ReqHO "请求的发送者 (类似于" :Get
      ", 但以变量名为参数); 将" :lam "解释为这样的函数, 其接受一个参数并通过" :handleVarD
      "处理在其体中引发的针对其绑定变量的" :ReqHO "请求."
      (CodeB "data ReqT = ReqError | ReqState StateReq | ReqHO HOReq
data HOReq = ReqVar VarName

instance Lam Comp where
  var v      = Req (ReqHO (ReqVar v)) Done
  lam v body = Done . DCFun $ \\x -> handleVarD [(v,x)] body

type Env = [(VarName,DomC)]

handleVarD :: Env -> Comp -> Comp
handleVarD _ (Done x) = Done x
handleVarD env (Req (ReqHO (ReqVar v)) k) | Just x &lt;- lookup v env =
  handleVarD env $ k x
-- everything else
handleVarD env (Req r k) = Req r (handleVarD env . k)")
      "它看起来确实有效, 例如, " (Code "th0 `app` ttinc :: Comp") "具有预期的意义, 即整数5. 然而,"
      (CodeB "*EDSLNG> (lam &quot;z&quot; (lam &quot;x&quot; (var &quot;z&quot; `app` var &quot;x&quot;))) `app` inc `app` int 1 :: Comp
ReqHO (ReqVar &quot;z&quot;)")
      " (这是计算" (Code "inc 1") "的一种花哨方式) 出乎意料地以对绑定于变量" :z
      "的值的请求作为其意义" -- "也就是说, 未绑定变量错误. 读者能猜出哪里出错了吗?")
   (H3. "第一级函数, 带有词法作用域")
   (P "在稳定指称框架中赋予第一级函数意义的第一次尝试遇到了可预见的问题: "
      "我们不经意间为绑定变量实现了动态作用域而不是词法作用域. 动态绑定是多么" (Q "自然")
      ", 这真是不可思议: 它是在实现lambda演算时首先想到的. 难怪John McCarthy和Alan Turing都犯了这个错误. "
      "词法绑定需要多得多的努力才能认识到和实现. 毕竟, 闭包 (closure) 这个术语直到1964年才由Landin引入, "
      "此时lambda演算发表已经20多年了.")
   (P "让我们再看一看刚才写的动态绑定代码的关键部分:"
      (CodeB "lam v body = Done . DCFun $ \\x -> handleVarD [(v,x)] body

handleVarD env (Req (ReqHO (ReqVar v)) k) | Just x &lt;- lookup v env =
  handleVarD env $ k x
handleVarD env (Req r k) = Req r (handleVarD env . k)")
      "创建一个第一级函数是直截了当的: 仅仅是将函数的体包裹在回应对lambda绑定的变量" :v
      "的值的请求的处理器之中. 如果" :body "除了" :v "之外还有其他变量, 那么这些请求将传播出我们的"
      (Code "handleVarD [(v,x)]") "并且应该由其他处于 (动态) 作用域中的处理器处理. "
      "因此, 创建一个第一级函数不需要上下文信息也不需要上下文交互. 我们可以称这样的操作是纯的. "
      "另一方面, 将第一级函数应用于一个值可能引起 (针对" :v "以外的变量的) " :ReqVar
      "请求. 因此, 函数应用不是上下文无关的: 它是带有作用的.")
   (P "对于词法作用域而言, 情况恰恰相反. 回忆一下, 一个词法作用域的函数在其创建之处捕获变量环境 "
      "(绑定上下文): 如Landin所言, 闭合 (close over) 上下文. 因此, 构建一个闭包确实依赖于上下文, "
      "并且根据我们的定义, 是带有作用的. 当闭包被应用时, 这个被捕获的环境被用来满足所有变量解引用请求:"
      (CodeB "data ReqT = ReqError | ReqState StateReq | ReqHO HOReq

data HOReq = ReqVar VarName | ReqClosure VarName Comp
type Env = [(VarName,DomC)]

instance Lam Comp where
  var v      = Req (ReqHO (ReqVar v)) Done
  lam v body = Req (ReqHO (ReqClosure v body)) Done

handleVar :: Env -> Comp -> Comp
handleVar _ (Done x) = Done x
handleVar env (Req (ReqHO r) k) = case r of
  ReqVar v | Just x &lt;- lookup v env -> handleVar env $ k x
  ReqVar _  -> err          -- unbound variable
  ReqClosure v body ->
    handleVar env $ k (DCFun $ \\x -> handleVar ((v,x):env) body)
-- everything else
handleVar env (Req r k) = Req r (handleVar env . k)")
      "现在函数的体被包围在" (Code "handleVar ((v,x):env)") "之中, 其不仅处理针对" :v
      " (由创建闭包的lambda所绑定的变量) 的" :ReqVar "请求, 而且处理针对" :body
      "中所有其他变量的请求. 因此, 当闭包被应用时, 没有变量请求传播到闭包之外而需要由上下文处理. "
      "因此闭包应用是上下文无关的: 闭包是纯的.")
   (P "另一方面, 为了创建一个闭包, 我们必须向上下文询问其中绑定的所有变量. 因此, 捕获环境是一个作用. "
      :lam "形式发起捕获, 发送" (Code "ReqClosure v") "请求. 当它到达一个" :handleVar
      "解释器" -- "回忆一下, 其包含关于在其作用域内绑定的变量的完整信息" -- "时, "
      "解释器通过将其变量环境向下传递给函数体的处理器来创建闭包. 顶层的" (Code "handleVar []")
      "传递空环境, 因为在顶层不应该有变量被绑定. 现在,"
      (CodeB "handleVar [] $ (lam &quot;z&quot; (lam &quot;x&quot; (var &quot;z&quot; `app` var &quot;x&quot;)))
                 `app` inc `app` int 1 :: Comp")
      "具有所欲的意义: 整数2.")
   (P "我们刚刚演示了即便是添加词法作用域的第一级函数也保留了早先定义的特性的指称. 增量" :inc
      "仍然意味着后继, 而整数数码仍然意味着整数, 无论它们是否出现在函数体中. "
      "我们的指称让人想起lambda演算在pi演算中的嵌入. 我们的语义的一个或许出人意料的特点在于"
      :lam "被视为一个作用. 人们不应该太过惊讶: 毕竟, 闭包创建确实不得不与上下文交互, "
      "并且确实不得不为环境分配空间. 语言FX和ATS同样将闭包创建视为一个作用, 这并非偶然.")
   (P (B "参考文献")
      (Ul (Li "作为变量绑定环境的栈: 通过动态绑定实现的词法绑定, 严格的和惰性的")
          (Li "动态绑定和词法绑定的概览和比较讨论")))
   (H3. "可扩展作用")
   (P "细心的读者一定已经注意到了, 我们隐式地将请求的数据类型" :ReqT
      "视为好像它是可扩展的. Haskell数据类型是不可扩展的; 伴随本文的源代码不得不预先定义请求类型. "
      "这在实践中是不可行的.")
   (P "幸运的是, 通过嵌套余积 (co-product) (" (Code "Either") "类型), "
      "引入一个可扩展的请求类型并不需要太多痛苦. Haskell和其他语言中的可扩展作用库正是这样做的. "
      "最终结果是真正可扩展的" -- "而且是有类型的" -- "框架, 其让我们的程序具有所欲的作用.")
   (P (B "参考文献")
      (Ul (Li "Extensible Effects: an alternative to Monad Transformers")
          (Li "特别参见论文" (Q "Freer Monads, More Extensible Effects") "的第2.5节")))
   (H2. "高阶编程是一个作用")
   (P "我们主张, 高阶编程与各种作用的交互这一核心问题可以通过消除区分来解决: "
      "高阶设施本身就是一个作用, 与状态作用并没有太大不同.")
   (P "我们演示第一级抽象可以像任何其他作用一样被统一对待, 从而完成Cartwright和Felleisen的"
      (Q "Extensible Denotational Language Specifications") "计划:"
      (Ul (Li "变量代换与解引用一个类C的变量无法区分;")
          (Li (Q "lambda") ", 或者说创建一个闭包, 也是一个作用.")
          (Li " (词法) 闭包充当在其体的执行过程中所产生的所有变量解引用作用的处理器.")
          (Li "我们的方法统一地处理动态和词法绑定以及各种调用约定.")))
   (P "与call-by-push-value以及语言ATS或者FX一样, 我们将lambda抽象视为一个带有作用的表达式. "
      "与这些演算和语言不同, 我们也将变量视为一个带有作用的表达式. "
      "绑定变量本质上是在第一级存储中分配的单次赋值引用单元. 这正是它们在机器码中通常被编译的方式. "
      "因此我们的方法有望对分布式和异构系统给出忠实的描述, 其中变量解引用是一个非平凡的作用.")
   (P "通常将一个带有作用的语言设计为lambda演算的扩展, 带有以下熟悉的定型判断:"
      (MB (: $Gamma (Judg $M (Ty $e $a))))
      "即表达式" $M "具有类型" (Ty $e $a) ". 这里, " $a "是成功求值" $M "时所产生的值的类型, 而"
      $e "描述了" $M "在此过程中可能执行的作用. (" $T "是二元类型构造子: "
      "可以将其设想为一个 (以作用为索引的) 单子.) " $Gamma "是熟悉的定型环境, 告知"
      $M "中自由变量的类型. 我们谦逊地提议将类型判断简化为仅仅是"
      (MB (Judg $M (Ty $e $a)))
      "也就是说, 移除定型环境" -- "以及作为起点的lambda演算. 那么如何判断, 比如说, "
      (&+ $x $1) "的类型呢, 其中" $x "是一个自由变量? " $x "的类型从何而来? 让我们考虑"
      (&+ $ask $1) ". 为了给这样的带有作用的表达式定型, 我们需要假定" $ask
      "是一个产生整数的带有作用的操作. 因此我们写:"
      (MB (Judg (pare (&+ $ask $1)) (Ty (Eff (Ann $ask $int)) $int)))
      "将作用注解" (Eff (Ann $ask $int)) "视为表达式内作用操作" $ask
      "的绑定: 即作为作用的定型环境. 解引用一个变量也是一个作用! 因此, 我们完全可以写"
      (MB (&Table
           ((Judg (pare (&+ $x $1)) (Ty (Eff (Ann $x $int)) $int)))
           ((Judg (pare (&+ $ask $x)) (Ty (Eff (Ann $x $int) (Ann $ask $int)) $int)))
           ((Judg (Lam $x (pare (&+ $ask $x)))
                  (Ty (Eff (Ann $ask $int)) (pare (&-> $int $int)))))
           ((Judg (Handle (pare (&+ $ask $x)) $ask $10)
                  (Ty (Eff (Ann $x $int)) $int)))))
      "既然作用注解充当尚待处理的作用的绑定环境, 它也完全可以描述尚待绑定的变量的类型. "
      $Gamma "确实变得多余了.")
   (P "整个程序" $P "应该是闭的:"
      (MB (Judg $P (Ty (Eff) $a)))
      "它不应该有未绑定的变量" -- "也不应该有未处理的作用.")
   (P "总而言之, 一个像可扩展作用这样支持多种作用的框架因此在抽象和代换作用上不应该有任何问题" --
      "这给出了HOPE.")
   (P (B "参考文献")
      (Ul (Li (Code "HOPE-talk.pdf") -- "在与ICFP 2017联合举办的HOPE研讨会上所作报告的幻灯片. "
              "2017年9月3日. 英国牛津.")
          (Li (Code "edlsng.ml") -- "OCaml版本: Cartwright-Felleisen框架的现代重构. "
              "状态, 动态绑定, 传值调用和传名调用的词法绑定是示例作用.")
          (Li (Code "EDSLNG.hs") -- "代码的基本上是Haskell98的版本.")))
   (H2. "结论")
   (P "那么什么是作用呢? 它是与上下文的交互. 如果一个表达式向上下文发送消息, 那么它就是带有作用的. "
      "一个词法作用域的lambda不得不向上下文询问绑定环境, 因而是带有作用的. "
      "一个闭包的体, 在被求值时, 发送对其自由变量的值的请求. 然而, 整个闭包满足所有这样的变量请求, "
      "并且 (在没有状态, IO等的情况下) 不需要进一步的上下文交互. 因此闭包是不带作用的, 纯的.")
   (P "因此, 某样东西是否是副作用, 是一个视角的问题. 当聚焦于计算的一小部分时, "
      "所有与上下文的交互" -- "无论是访问外部存储器还是访问一个绑定变量" -- "都是副作用. "
      "甚至lambda也可以被视为一个作用, 即闭包创建的作用.")
   (P "我们是否应该将Lambda或者State视为一个作用取决于手头的任务: 我们是在对一个表达式求值还是在编译它, "
      "我们是只对表达式的结果感兴趣还是也对计算它的时间感兴趣. 因此, 作用作为一种便利和一种抽象而浮现, "
      "让我们能够聚焦于整个系统的一部分, 将其余部分抽象为要通过固定协议与之交互的" (Q "上下文")
      ". 不同的协议容许不同程度的等式推理.")
   (P "这就是为什么, 当涉及作用时, 明确和有意识是如此重要.")
   ))
