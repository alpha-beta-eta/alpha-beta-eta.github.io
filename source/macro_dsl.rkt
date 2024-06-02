#lang racket
(provide macro_dsl.html)
(require SMathML)
(define macro_dsl.html
  (TmPrelude
   #:title "从宏到DSL: Racket的演化"
   #:css "styles.css"
   (H1 "从宏到DSL: Racket的演化")
   (H2 "第1章 宏和DSL")
   (P "Racket宣言倡导一种软件开发的" (Em "面向语言编程")
      "方法. 想法在于严肃对待Hudak的口号" (Q "语言是终极的抽象")
      ", 并编程以DSL, 就好像它们是所选语言所内蕴的适切抽象. "
      "就和其他各种抽象一样, 程序员希望能够创建DSL, "
      "以DSL编写程序, 将这些程序嵌入宿主语言的代码中, "
      "并且DSL之间可以相互交流.")
   (P "根据Lisp的世界观, 带有宏的语言能够很好地支持这种看法. "
      "使用宏, 程序员可以定制语言, 使其适合某个特定领域. "
      "因为定制语言的程序仍然位于宿主程序之中, "
      "它们可以轻易地与宿主程序交流, 并且之间也可以交流. "
      "简而言之, 创建, 使用, 组合DSL似乎看上去很容易.")
   
   (H2 "第2章 Lisp和Scheme的史前史")
   (P "一般而言, 一个Lisp或Scheme实现需要使用一个reader"
      "将程序从字符序列的形式转换为一种具体的树表示, "
      "也就是所谓的S-expression. 接着, 实现会通过一个"
      "expander来获得抽象句法树. 这个expander会遍历"
      "S-expression, 寻找并消除对于宏的使用. 更技术性地说, "
      "一个宏是一个函数, 其类型为"
      (CodeB "S-expression -> S-expression")
      (Code "define-macro") "形式可以用来定义宏, 这些宏操作S-expression. "
      "每个宏定义都给宏的表添加了一个宏函数, expander使用这个宏的表"
      "将S-expression映射为抽象句法表示, 故expander的类型应为"
      (CodeB "S-expression * TableOf[MacroId, (S-expression -> S-expression)] -> AST")
      (Code "AST") "是对于S-expression的一种内部表示, 当然其消除了宏的使用.")
   (P "让我们看一个简单的例子, 即" (Code "let") "宏."
      (CodeB "(define-macro (let e)
  (define decl (cadr e))
  (define lhs (map car decl))
  (define rhs (map cadr decl))
  (define body (cddr e))
  `((lambda ,lhs ,@body) ,@rhs))")
      "这个宏假定提供的S-expression应该具有以下形式"
      (CodeB "(let ((lhs rhs) ...) body ...)")
      "每当expander遇到一个首符号为" (Code "'let")
      "的S-expression时, 它就会调用相应的宏转换过程于该S-expression上. 从"
      (Code "let") "宏的定义可知, 其将被转换为一个" (Code "lambda")
      "抽象的直接应用.")
   (P "显然宏增强了Lisp的表达力, 但是我们也看到这种刻画方式不仅容易出错, 也相当不便. "
      "例如, 刚才的宏转换过程的创建者假定了相应的S-expression应该具有某种特殊的形状, "
      "但是这并不能由宏展开过程保证. 而且, 即便不考虑这类问题, 从S-expression"
      "中提取我们所需要的部分也是相当令人难受的.")
   (P "就正确性而言, 若在刚才的" (Code "let") "宏的上下文中考虑, 可能会出现以下问题:"
      (Ol (Li "S-expression可能是一个improper list. 这显然违反了我们对于"
              (Code "let") "的句法的期望, 但是并不一定会产生异常.")
          (Li "S-expression有可能太短了. 比如说, 声明部分的元素可能没有" (Code "cadr")
              "域, 那么宏转换过程应该会抛出异常, 编译过程就此终止.")
          (Li "S-expression有可能太长了. 比如说, 声明部分的元素可能不止两个元素. "
              "在这种情况下, 宏转换过程直接就忽略了这个问题.")
          (Li "S-expression的长度可能都符号要求, 但是仍然不符合句法. 例如, "
              (Code "lhs") "的元素不都是标识符. 即便都是标识符, "
              "我们还要求这些标识符是相异的. 在这种情况下, 宏转换过程仍然会生成代码, "
              "而问题的发现依赖于编译过程的剩余部分. 而当发现问题时, 就会出现"
              (Ol #:attr* '((type "a"))
                  (Li "无法报告因为源代码而产生的错误, 因为很可能句法错误无处可寻.")
                  (Li "无法报告因为语言扩展而产生的错误, 因为很可能无法理解错误信息."))
              "[注记: 我没有完全理解以上两句话的细节, 但作为笔记者个人的理解是, "
              "经过宏扩展得到的代码中的错误无法定位到原始的源代码里, 因为"
              "其有可能已变得面目全非, 另外就是定义句法扩展的代码很难排查错误, "
              "因为往往产生的错误信息和代码没有很强的逻辑关联性.]")
          (Li "宏作者可能忘记加上" (Code "unquote") "什么的. 例如, 没有给"
              (Code "lhs") "前加上" (Code ",") ". 这其实对于许多Lisp实现而言"
              "仍然可以生成句法合理的代码, 因为它会把所有表达式的值作为列表绑定至"
              (Code "lhs") ", 但是这却与原意相去甚远. "
              "[注记: 这个问题实际上就个人而言感觉也和卫生问题有着密切联系.]"))
      
      )
   (H2 "第3章 DSL不仅是一堆宏")
   (P "Scheme风格的宏就" (Em "扩展") "既有语言来看对比Lisp风格的宏而言是极大的改进. "
      "开发者可以添加精确并且词法正确 [注记: 意即尊重词法作用域] 的宏并立即使用, "
      "例如用于编写通常的运行时代码或者额外的什么宏. 这种立即性是强大且诱人的, "
      "因为程序员无需离开熟悉的编程环境 "
      "[注记: 可能指的是那些通用宏处理工具吧, 不过它们一般是操作在字符串层次上的].")
   (P "宏的想法在抽象层面上也是容易理解的. 从概念上说, 一个宏定义给Racket的"
      "语法产生式添加了一个新的分支: 定义或者表达式. 声明性的方法使得描述简单的"
      "S-expression句法转换过程相当直接, 卫生宏展开则保证了程序词法作用域的完整性.")
   (P "从传统上说, 创建DSL需要一下编译器步骤 (pass, 我也不知道怎么翻译) 管线:"
      (Ol (Li "一个" (Em "parser") ", "
              )
          )
      )
   (H2 "第4章 雄心壮志的开端")
   (P "当Racket的设计者们发现传统Scheme宏系统的短板时, 他们决定以三项创新解决这些短板. "
      "首先, 他们决定从更传统的对于句法的S-expression表示转向更丰富的结构. "
      "其次, 他们意识到宏需要能够一起协作以实现上下文敏感的检查. 为此, "
      "他们支持声明性宏以过程性" (Em "micro") ", micro可以处理展开上下文的属性. "
      "最后, 他们决定使用模块不仅作为基于宏的DSL实现的容器, 也作为使用DSL的单元.")
   (H3 "第4.1节 从S-expression到句法对象")
   (P "为了在宏展开之间追踪源位置, Racket和Dybvig的Chez Scheme一样, "
      "都引入了表面代码的" (Em "句法对象") "表示, 而放弃了传统的S-expression表示. "
      "大致说来, 句法对象其实和S-expression类似, 只是每个结点都包裹了一层结构, "
      "这个结构记录了一些信息. 至少, 这个结构包含句法中的各种token的源位置. "
      "使用此信息, 我们便能定位句法错误的源位置, 这部分地解决了之前的问题4a.")
   (H3 "第4.2节 micro的语汇")
   (P "回忆一下, 宏是句法表示上的函数. 一旦这个表示使用的是更丰富的结构而非S-expression, "
      "那么宏的签名也应该随之修改:"
      (CodeB "Syntax-Object -> Syntax-Object")
      "当然, 这种签名正说明宏不能自然地表达牵涉展开上下文的属性的交流通道. "
      "[原注: 宏作者可以通过某种协议将属性编码为句法对象, 但是我们将这种手段视为不自然的.]")
   (P "Krishnamurthi等人的工作支持宏以micro来解决这个问题. 类似于" (Code "define-macro")
      ", " (Code "define-micro") "也描述了一个函数, 其消费句法的表示. "
      "并且, 其可以吸收任意数量的" (Code "Attribute") "值, 这使得micro"
      "之间可以显式地交流上下文信息:"
      (CodeB "Syntax-Object -> (Attribute ... -> Output)")
      "正如这个签名所显示的, micro和宏的另一个不同之处在于其结果是某种任意的类型, 我们称之为"
      (Code "Output") ". 这个类型对于相互协作的一群micro而言必须是相同的, "
      "但是不同群的micro之间当然可以是不同的. 对于类似于宏的micro而言, " (Code "Output")
      "即" (Code "Syntax-Object") ". 与之对比的是, 对于一个嵌入式编译器而言, "
      (Code "Output") "会是" (Code "AST") ", 这指的是代表目标语言的抽象句法树的类型. "
      "目标语言可以是Racket, 但是也可以是全然不同的某种东西, 例如GPU汇编代码.")
   (P "正如这种解释所指出的, DSL的micro应该被视为某个合集的成员. 为了使得这种想法具体化, "
      "Krishnamurthi等人引入了" (Em "语汇") "的概念. 既然宏和micro的合集确定了DSL的"
      (Q "词汇") "和" (Q "句子结构") ", 语汇代表了一种词典和语法规则的形式等价物. "
      "micro自身将被嵌入语言的" (Q "句子") "转换为有意义的 (即可执行的) 程序.")
   (P "在Krishnamurthi等人的情境下, 语汇以" (Code "(make-vocabulary)")
      "创建, 并随之带来了两种操作: " (Code "define-macro")
      "添加micro函数于给定的语汇, 而" (Code "dispatch")
      "在特定语汇的上下文中应用micro于表达式.")
   (CodeB ";; type Output = RacketAST
(define compiler (make-vocabulary))
_ _ _ elided _ _ _
(define-micro compiler
  (if cond then else)
  ==>
  (lambda ()
    (define (expd t)
      ((dispatch t compiler)))
    (define cond-ir (expd cond))
    (define then-ir (expd then))
    (define else-ir (expd else))
    (make-AST-if
     cond-ir then-ir else-ir)))
_ _ _ elided _ _ _
(define compiler-language
  (extend-vocabulary
   base-language
   compiler))")
   (CodeB ";; type Output = RacketType
(define type-check (make-vocabulary))
_ _ _ elided _ _ _
(define-micro type-check
  (if cond then else)
  ==>
  (lambda (Γ)
    ;; first block
    (define (tc t)
      ((dispatch t type-check) Γ))
    (define cond-type (tc cond))
    (unless (type-== cond-type Boolean)
      (error _ _ _ elided _ _ _))
    (define then-type (tc then))
    (define else-type (tc else))
    (unless (type-== then-type else-type)
      (error _ _ _ elided _ _ _))
    then-type))
_ _ _ elided _ _ _")
   
   ))