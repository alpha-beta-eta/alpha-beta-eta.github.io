#lang racket
(provide having_an_effect.html)
(require SMathML)
(define ---- "&mdash;&mdash;")
(define having_an_effect.html
  (TnTmPrelude
   #:title "Having an Effect"
   #:css "styles.css"
   (H1. "Having an Effect")
   (P "这是对于Oleg Kiselyov博客的翻译.")
   (H2 "引论")
   (P "这个研究是追寻某条旖旎线索的旅程: "
      "关于monad, monad变换器, 自由monad和可扩展作用"
      "的奠基性论文都是关于可扩展解释器的. "
      "沿着这条道路我们意识到" (Em "表达式问题")
      ", " (Em "稳定指称") ", 以及" (Em "可扩展解释器")
      "都是相同东西的不同名字. "
      "最终我们发现了组织性的原则: " (Em "交互")
      ---- "一个客户端和一个服务器之间, "
      "一个解释器和被解释的代码之间, "
      "一个表达式和其上下文之间.")
   (P "或许交互最好使用某种进程演算进行表达. "
      "若以顺序性演算的语言, Cartwright和Felleisen的"
      (Q "Extensible Denotational Language Specifications")
      "是最早的将作用当作交互进行全然处理的呈现."
      (Ul (Li "这个演讲给出了Cartwright和Felleisen"
              "的方法的现代重构. "
              "我们的重构更加简单, "
              "受到了tagless-final呈现的辅助, "
              "并且立即可以执行.")
          (Li "更进一步, 我们使得作用处理器 (effect handler) "
              "本身变得模块化和可扩展" ----
              "成为程序的一部分而非永久地伫立于外. "
              "这使得接下来的一步成为可能:")
          (Li "Cartwright和Felleisen不得不将变量环境烘焙进基本的语义框架之中, "
              "尽管整数, 或者更一般地说是一阶子语言, 并不会用到环境. "
              "我们避免了变量环境的假定以及任何对于高阶的特殊处理. "
              "毕竟, 我们以相同的一致形式化将lambda和状态一并进行了处理. "
              "动态或词法绑定, 或者各种各样的调用约定, "
              "可以归结为对于绑定变量解引用的作用的不同处理方式."))
      "重温该领域的起源并恢复洞察以及被遗忘的替代方案"
      "有助于使得我们的程序具有预期的作用 (effect).")
   (H2. "不稳定的指称, 脆弱的解释器, 难以应用的理论")
   (H3. "从" (Q "实用") "角度来看指称语义")
   (P "既然(副)作用无处不在又充满争议, 那么其理应得到研究. "
      "并且, 显然其已被指称语义的Vienna学派于1970年代进行研究"
      ---- "目的在于澄清PL/1(的语义). 我们也将会使用指称语义"
      ---- "但是带有实用的倾向, 面向(定义性)解释器. "
      "它最终将我们导向" (Q "可扩展作用") ----
      "一种管理真实程序之中的作用的实用方法.")
   (P "本章我们刻画了人尽皆知的不稳定指称问题. "
      "它是使用指称语义研究和定义真实语言及其作用的绊脚石, "
      "这首先是由John Reynolds于1974年注意到的. "
      (Ref "stable-denotations")
      "使用从进程演算那里得到的洞察解决了这个问题.")
   (P "为了具体, 我们将会使用Haskell来编写我们的定义性解释器, "
      "尽管使用OCaml, F#, Scala等语言也没有问题. "
      "我们希望即便是不熟悉Haskell的读者也应该能够"
      "通过阅读代码并将其与标准数学记号进行类比以理解主要的想法. "
      "毕竟当我们来到未知之地时, 即便语言和文化不通, "
      "我们也总是会设法生存并且享受旅行. 交流的意愿才是最重要的.")
   (H3. "定义性解释器")
   (P "我们将通过极其简单的例子来解释指称语义的定义性解释器方法. "
      "尽管其相当平凡, 这个例子已经澄清了可扩展解释器的价值.")
   (P "我们从最简单的算术表达式语言开始: 整数字面量, "
      "增量函数, 以及让我们应用增量的应用运算符. "
      "在Haskell之中, 我们将这个语言定义为一个数据类型"
      (Code "Expr") ":"
      (CodeB "data Expr = EInt Int | EInc | EApp Expr Expr")
      "一个示例表达式" ---- "对于字面量" (Code "2")
      "施行增量两次" ---- "looks then as " (Code "tinc")
      " {译注: 这里所说的示例表达式, 指的是具体句法}:"
      (CodeB "tinc = EApp EInc (EApp EInc (EInt 2)) -- inc (inc 2)")
      "现在我们引入指称语义来为" (Code "Expr")
      "赋予意义. 回忆一下, 指称语义应该是一个从" (Code "Expr")
      "到别的什么东西的复合性映射. 我们取以下有些宽泛的论域"
      (Code "Dom") "作为我们语言的表达式的意义, 或者说指称. "
      "其包含整数, 布尔, 函数, "
      "以及一个代表无意义表达式的意义的特殊元素. "
      (CodeB "data Dom = DError | DInt Int | DBool Bool | DFun (Dom -> Dom)")
      "然后语义, 即从表达式" (Code "Expr") "到其指称" (Code "Dom")
      "的映射, 被构造性地定义, 作为Haskell函数" (Code "eval")
      ". 其是我们的简单语言的一个解释器."
      (CodeB "eval :: Expr -> Dom
eval (EInt x) = DInt x
eval EInc     = DFun $ \\x -> case x of
                          DInt n -> DInt (succ n)
                          _      -> DError
eval (EApp e1 e2) = case eval e1 of
  DFun f -> f (eval e2)
  _      -> DError")
      "尽管" (Code "EApp EInc (EInt 8)")
      " signifies the phrase (a mere string) "
      (Code "inc 8") " in our language, "
      (Code "DInt 9") "是其意义, 一个Haskell整数. "
      (Code "eval") "的最后一个子句展示了复合性: "
      (Code "EApp e1 e2") "的意义仅仅依赖于"
      (Code "eval e1") "和" (Code "eval e2")
      ": 即依赖于参数" (Code "e1") "和"
      (Code "e2") "的意义, 但是并不依赖于参数本身, "
      "也就是并不依赖于其结构. "
      "因此, 指称语义藉由一个结构归纳性的解释器定义, "
      "即一个折叠 (fold).")
   (P "这语言急需新的特性; 例如, 布尔, 整数上的相等检测, "
      "以及条件表达式. Alas, Haskell中的数据类型是不可扩展的. "
      "因此, 我们不得不重新定义" (Code "Expr") "以容纳扩展:"
      (CodeB "data Expr = EInt Int | EInc | EApp Expr Expr
          | EEq | EIf Expr Expr Expr")
      "一个示例条件表达式"
      (CodeB "if 3 == inc (inc 2) then 10 else inc (inc (inc 2))")
      "looks as " (Code "tif") " below. 我们想要重用之前为"
      (Code "inc (inc 2)") "定义的" (Code "tinc")
      ", 但是并不可行: 类型" (Code "Expr") "已然发生了改变. "
      "我们不得不使用新的" (Code "Expr") "来重新定义"
      (Code "tinc") ", 尽管其看起来和之前毫无二致."
      (CodeB "tinc1 = EApp EInc (EApp EInc (EInt 2))
tif   = EIf (EApp (EApp EEq (EInt 3)) tinc1) (EInt 10) (EApp EInc tinc1)")
      "以下的解释器" (Code "eval") "给出了经过扩展的语言的意义. "
      "前三个子句和旧有的解释器一模一样. "
      "Alas, 我们不能引用旧的解释器, 而不得不写一个新的出来: "
      "Haskell之中的函数也是不可扩展的."
      (CodeB "eval :: Expr -> Dom
eval (EInt x)      = DInt x
eval EInc          = -- ... as before ...
eval (EApp e1 e2)  = -- ... as before ...
eval EEq           = -- similar to EInc
eval (EIf e et ef) = case eval e of
  DBool True  -> eval et
  DBool False -> eval ef
  _           -> DError")
      "因此, 我们遇到了不稳定性, 或者说表达式问题: "
      "我们的指称语义是脆弱的且不可扩展. "
      "我们不能重用之前写下的示例项和之前写下的解释器; "
      "我们需要重写它们. "
      "或许可以争辩说这是Haskell形式化的问题而非指称语义的问题. "
      "尽管如此, 这仍然反映了一种现实. "
      "一般指称语义是被数学地呈现, 而数学通常是非形式化的, "
      "尽管仍然往往是严格的. 诸多问题出现于形式化的过程之中. "
      "幸运的是, 存在使我们的解释器变得可扩展的方法.")
   (H3. "可扩展解释器")
   (P "让我们回到只有整数和增量的最简单的语言上来, "
      "然后以不同的方式定义它. "
      "我们不再使用某个数据类型, "
      "因为若是这样的话其又不得不被解释为" (Code "Dom")
      "了. 让我们通过直接告知 (tell) 每个语言phrase的意义, "
      "以及如何根据复合phrase的分量的意义来make sense of这个复合phrase, "
      "来定义语言. 因此, 我们即刻引入了指称语义而不需要中间的步骤" (Code "eval")
      ". {译注: 这两句话需要结合后文 (特别是程序) 进行理解. "
      "当前的意义其实并非具体的语义, 而只是语义的大致框架/形状.} "
      "为了一般性 (这在之后会派上用场), "
      "我们并没有固定指称的论域为" (Code "Dom")
      ". 转而, 我们将其作成了变量" (Code "d")
      ". 总而言之, 我们藉由一集定义来定义了一个语言, "
      "每个句法形式都对应于一个定义, "
      "而这每个定义都定义了相应形式的论域映射. "
      "{译注: 虽然这里有一些绕, 但其实就是之前说的, "
      "根据分量的意义来得到复合的意义.} "
      "论域是保持抽象的. 因此, 我们将语言定义为Haskell中的类型类 "
      "(或者OCaml中的模块类型, 诸如此类):"
      (CodeB "class EBasic d where
  int :: Int -> d
  inc :: d
  app :: d -> d -> d
    
infixl 1 `app`   -- make the infix `app` left-associative")
      "这个模块类型言称我们的语言具有两个原语, 即整数字面量和增量, "
      "以及一个复合表达式, 即带有两个子表达式的" (Code "app")
      ". 复合之意义是由子表达式之意义" (Code "d")
      "所确定的. 根据这个设计, 语义的确是复合性的. "
      "示例项" (Code "inc (inc 2)") "现在具有以下形式:"
      (CodeB "ttinc = inc `app` (inc `app` int 2)")
      "其被推导出的类型为" (Code "ttinc :: EBasic d => d")
      ". 我们或许可以将其读作: " (Code "ttinc")
      "是" (Code "EBasic") "语言之中的项, 或者说给定一个合适的论域"
      ---- "对于我们的语言足够有表达力" ----
      (Code "ttinc") "给出了phrase " (Code "inc (inc 2)")
      "在这个论域之中的意义. {译注: 仅仅有论域并不足够, 实际上还需要解释.} "
      "剩下来我们所要做的事情是表明" (Code "Dom")
      "的确" (Q "适合于") "给出这个基本语言的意义:"
      (CodeB "instance EBasic Dom where
  int = DInt
  inc = injI succ
  app (DFun f) e2 = f e2
  app _ _         = DError")
      "其中我们写下" (Code "injI succ") "以将Haskell的后继函数提升至"
      (Code "Dom") ", 若增量的值并非一个" (Code "DInt")
      "则其会返回" (Code "DError") ". 然后, 示例表达式的意义就是"
      (Code "ttinc :: Dom") "了, 也就是说, " (Code "ttinc")
      "被特化于论域类型" (Code "Dom") "之上.")
   (P "和之前一样的是, 我们将一个表达式映射为了其意义, 即"
      (Code "Dom") "的元素. "
      )
   (H3. "定义状态")
   (P "这一节给增量和条件式的语言添加了第一个真实的作用: "
      "全局的可变的整数状态. "
      )
   (H3. "第一级函数?")
   (H2. "作用和交互")
   (H2. "稳定指称, 可扩展作用" #:id "stable-denotations")
   (H3. "迈向稳定指称")
   (H3. "状态, 无痛地")
   (H3. "第一级函数, 第一次尝试")
   (H3. "第一级函数, 带有词法作用域")
   (H3. "可扩展作用")
   (H2. "高阶编程是一个作用")
   (H2. "结论")
   ))