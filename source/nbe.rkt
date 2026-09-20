#lang racket
(provide nbe.html)
(require SMathML)
(define-syntax-rule (define-code* (id str) ...)
  (begin
    (define id (Code str))
    ...))
(define-code*
  (:val "val")
  (:do-ap "do-ap")
  (:CLOS "CLOS")
  (:N-var "N-var")
  (:N-ap "N-ap")
  (:NEU "NEU")
  (:THE "THE")
  (:assv "assv")
  (:go "go")
  (:stop "stop")
  (:go-on "go-on")
  (:synth "synth")
  (:check "check")
  (:the "the")
  (:λ "λ")
  (:Π "Π")
  (:Σ "Σ")
  (:Nat "Nat")
  (:zero "zero")
  (:add1 "add1")
  (:rec "rec")
  (:→ "→")
  (:U "U")
  (:Absurd "Absurd")
  (:ind-Nat "ind-Nat")
  (:ind-Absurd "ind-Absurd")
  (:replace "replace")
  (:same "same")
  (:type=? "type=?")
  (:α-equiv? "α-equiv?")
  (:gensym "gensym")
  (:cons "cons")
  (:car "car")
  (:cdr "cdr")
  (:Γ "Γ"))
(define-syntax SimpleTable
  (syntax-rules ()
    ((_ (x ...) ...)
     (Table
      (Tr (Td x) ...)
      ...))))
(define (Href url . html*)
  (keyword-apply A '(#:attr*) `(((href ,url))) html*))
(define (Bcite key)
  (Span "[" (Href (string-append "#bib-" key) key) "]"))
(define (Bib key . html*)
  (keyword-apply Li '(#:attr*) `(((id ,(string-append "bib-" key))))
                 "[" key "] " html*))
(define (sf str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $zero (sf "zero"))
(define $add1 (sf "add1"))
(define $rec (sf "rec"))
(define $Nat (sf "Nat"))
(define $FV (Mi "FV"))
(define split2 (&split 2))
(define split16 (&split 16))
(define (Ap f . x*) (apply split2 f x*))
(define (Axiom j) (&rule (Mrow) j))
(define (Lam x e) (: $lambda x $. e))
(define (Has G e t) (: G $vdash (&: e t)))
(define (Syn G e t) (: G $vdash e $=> t))
(define (Chk G e t) (: G $vdash e $lArr t))
(define (Eqv G a b t) (: G $vdash (&equiv a b) $: t))
(define (Rec A) (: $rec (brac A)))
(define Γ_1 (_ Γ $1))
(define Γ_2 (_ Γ $2))
(define $t_b (_ $t $b))
(define nbe.html
  (TnTmPrelude
   #:title "用基于求值的规范化检查依赖类型: 教程"
   #:css "styles.css"
   (H1. "用基于求值的规范化检查依赖类型: 教程")
   (P "David Thrane Christiansen (davidchristiansen.dk). 原文标题为"
      (Q "Checking Dependent Types with Normalization by Evaluation: A Tutorial") ".")
   (H2 "翻译术语对照")
   (set-attr*
    (SimpleTable
     ("英文" "中文")
     ("normalization by evaluation (NbE)" "基于求值的规范化 (NbE)")
     ("dependent type" "依赖类型")
     ("bidirectional type checking" "双向类型检查")
     ("checking / synthesis" "检查 / 综合")
     ("syntax-directed" "句法导向的")
     ("literate program" "文学化程序")
     ("evaluator" "求值器")
     ("closure" "闭包")
     ("runtime environment" "运行时环境")
     ("association list" "关联列表")
     ("fresh name" "新鲜名字")
     ("normal form" "规范形式")
     ("redex" "可约式")
     ("neutral expression" "中性表达式")
     ("read back / quote" "读回 / 引用")
     ("α-equivalence" (Span $alpha "等价"))
     ("substitution" "代换")
     ("Church numeral" "Church数码")
     ("introduction form / elimination form" "引入形式 / 消去形式")
     ("constructor / eliminator" "构造子 / 消去子")
     ("target (of an eliminator)" "(消去子的)目标")
     ("type annotation" "类型标注")
     ("subsumption" "归入")
     ("judgment / premise / conclusion" "判断 / 前提 / 结论")
     ("metavariable" "元变量")
     ("reflection / reification" "反射 / 具化")
     ("η-expansion" (Span $eta "展开"))
     ("type-directed" "类型导向的")
     ("binding form / binder" "约束形式 / 约束子")
     ("elaborate, elaboration" "精化")
     ("core language" "核心语言")
     ("universe / cumulativity" "宇宙 / 累积性")
     ("hereditary substitution" "遗传代换")
     ("conversion checking" "转换检查"))
    'align "center")
   (H2 "序言")
   (P "为了实现依赖类型, 我们需要能够判定两个类型何时相同. "
      "在简单的类型系统中, 这一过程是相当直接的结构相等性检查, "
      "但是随着类型系统表达能力的增强, 这一相等性检查变得更加困难. "
      "特别地, 当类型可以包含程序时, 我们需要能够运行这些程序, 并检查它们的输出是否相同. "
      "基于求值的规范化是进行这种相同性检查的一种方式, 而双向类型检查则指导何时调用这些检查.")
   (P "这些笔记是一组文学化程序, 展示了如何从一个求值器或解释器导出一个规范化过程, "
      "然后如何使用这个规范化过程为一个小的依赖类型语言编写类型检查器, "
      "该语言基于" (Href "http://thelittletyper.com" (I "The Little Typer"))
      " " (Bcite "Friedman18") "中的语言Pie.")
   (P "这些笔记假定你了解以下内容:"
      (Ul (Li "Racket的基础, 包括模式匹配以及如何定义结构体和函数")
          (Li "依赖类型的基础, 相当于学完" (Href "http://thelittletyper.com" (I "The Little Typer"))
              "的前一半, 或者"
              (Href "http://docs.idris-lang.org/en/latest/tutorial/" "Idris教程")
              "的第一部分"))
      "理解某些章节还需要熟悉推理规则, 但这些章节可以放心跳过. "
      (I "The Little Typer") "的附录描述了如何阅读推理规则.")
   (H2. "对无类型" $lambda "演算求值" #:id "untyped-eval")
   (P "让我们从无类型" $lambda "演算的一个求值器开始. 编写一个求值器需要以下步骤:"
      (Ul (Li "确定作为求值结果的" (Em "值"))
          (Li "弄清楚哪些表达式立即成为值, 哪些需要计算")
          (Li "为值实现" (Code "struct") ", 并用过程来进行计算")))
   (P "在这里, 对于无类型" $lambda "演算而言, 唯一可用的值是闭包, "
      "而计算发生于一个闭包被应用于另一个值之时.")
   (H3. "值与运行时环境" #:id "values-and-runtime-environments")
   (P (CodeB "(struct CLOS (env var body)
    #:transparent)
  env : Env
  var : symbol?
  body : any/c")
      "一个" (Em "闭包") "将一个尚未求值的表达式与创建该表达式时的运行时环境打包在一起. "
      "这里, 闭包总是表示带有一个变量的表达式, 该变量将被实例化为一个值, "
      "所以这些闭包还额外带有" (Code "var") "字段."
      (CodeB "(struct CLOS (env var body) #:transparent)"))
   (P (Code "#:transparent") "修饰符使得所产生的结构体使用结构相等性来比较, 并且是可打印的. "
      "前者对这些笔记的测试套件有用, 而后者便于调试. 因为" :CLOS
      "这样的结构体的文档包含了声明的完整源代码, 所以在这些笔记的后面只展示文档.")
   (P "运行时环境为每个变量提供值. 按照惯例, 运行时环境用希腊字母" (Q $rho) "来指称. "
      "{原注: " $rho "可以写作" (Q "rho") ", 读音像" (Q "row") ".} "
      "在这个实现中, 环境是关联列表, 包含由变量名和值构成的序对. "
      "列表中较早的值覆盖较晚的值.")
   (P "Racket函数" :assv "可以用来在环境中查找一个名字, "
      "返回第一个匹配的序对, 或者返回" (Code "#f") ". 例如:"
      (CodeB "> (assv 'x (list (cons 'y \"peaches\") (cons 'x \"apples\")))
'(x . \"apples\")
> (assv 'x (list (cons 'y \"peaches\") (cons 'z \"apples\")))
#f")
      (CodeB "(extend ρ x v) → environment?
  ρ : environment?
  x : symbol?
  v : value?")
      "因为" :assv "返回的是" (Em "第一个") "匹配的条目, "
      "用一个新变量扩展环境只需要加在前面."
      (CodeB "(define (extend ρ x v)
  (cons (cons x v) ρ))"))
   (H3. "求值器" #:id "untyped-eval-val")
   (P "求值器由两个过程构成: " :val "在一个为表达式的自由变量提供值的运行时环境中对该表达式求值, 而"
      :do-ap "负责将一个函数的值应用于其参数的值. "
      "{原注: Racket的" (Code "apply") "稍有不同, 它接受一个参数的" (Em "列表")
      ". 然而, 这里总是恰好有一个参数, 所以这种区别无关紧要.} "
      "过程" :val "和" :do-ap "通常称为" (Code "eval") "和" (Code "apply")
      "; 这里我们使用不同的名字, 以免与Racket的内置运算符冲突.")
   (P (CodeB "(val ρ e) → value?
  ρ : environment?
  e : expression?")
      "求出表达式" (Code "e") "的值, 如果这样的值存在的话."
      (CodeB "(define (val ρ e)
  (match e
    [`(λ (,x) ,b)
     (CLOS ρ x b)]
    [x #:when (symbol? x)
     (let ((xv (assv x ρ)))
       (if xv
           (cdr xv)
           (error 'val \"Unknown variable ~a\" x)))]
    [`(,rator ,rand)
     (do-ap (val ρ rator) (val ρ rand))]))"))
   (P (CodeB "(do-ap clos arg) → value?
  clos : value?
  arg : value?")
      "将一个函数值应用于一个参数."
      (CodeB "(define (do-ap clos arg)
  (match clos
    [(CLOS ρ x b)
     (val (extend ρ x arg) b)]))"))
   (P "名字" (Em "rator") "和" (Em "rand") "是" (Q "operator") " (运算符) 和"
      (Q "operand") " (运算数) 的缩写. 这些名字可以追溯到Peter Landin "
      (Bcite "Landin64") ".")
   (P "例子:" (CodeB "> (val '() '(λ (x) (λ (y) y)))
(CLOS  '()     'x '(λ (y) y))
> (val '() '((λ (x) x) (λ (x) x)))
(CLOS  '()     'x 'x)
> (val '() 'x)
val: Unknown variable x"))
   (H3. "添加定义" #:id "adding-definitions")
   (P "真正的" (Em "程序") "不是单个的表达式; 通常, 程序员会把代码写成一些有意义的部分. "
      "为了支持这种工作方式, 我们可以为这个小小的无类型编程语言添加定义. "
      "定义提供了" $rho "的初始内容, 这意味着添加一个定义可以这样完成: "
      "先对定义体求值, 然后将结果添加到一个定义的列表中."
      (CodeB "(run-program exprs) → void?
  exprs : (listof expression?)")
      (CodeB "(define (run-program ρ exprs)
  (match exprs
    ['() (void)]
    [(cons `(define ,x ,e) rest)
     (let ([v (val ρ e)])
       (run-program (extend ρ x v) rest))]
    [(cons e rest)
     (displayln (val ρ e))
     (run-program ρ rest)]))")
      (CodeB "> (run-program '() '((define id (λ (x) x))
                     (id (λ (y) (λ (z) (z y))))))
#(struct:CLOS ((id . #(struct:CLOS () x x))) y (λ (z) (z y)))
> (run-program '() '((define z
                       (λ (f)
                         (λ (x)
                           x)))
                     (define s
                       (λ (n)
                         (λ (f)
                           (λ (x)
                             (f ((n f) x))))))
                     (s (s z))))
#(struct:CLOS
  ((n
    .
    #(struct:CLOS
      ((n . #(struct:CLOS () f (λ (x) x)))
       (z . #(struct:CLOS () f (λ (x) x))))
      f
      (λ (x) (f ((n f) x)))))
   (z . #(struct:CLOS () f (λ (x) x))))
  f
  (λ (x) (f ((n f) x))))")
      "{译注: 为了适应页面 (特别是打印时) 的宽度, 译文对少数过长的代码行重新换行, "
      "并用Racket的" (Code "pretty-write") "重新排版了过长的REPL输出, 内容没有改动.}")
   (P "这种处理定义的方法意味着没有相互递归, 因为新的定义只在程序的剩余部分中有效. "
      "此外, 由于唯一的值是闭包, 程序的返回值看起来并没有什么意思. "
      (Ref "untyped-norm") "展示了如何从这些闭包值中恢复出可读的代码.")
   (H2. "生成新鲜名字" #:id "generating-fresh-names")
   (P "规范化需要生成新鲜的名字, 以避免变量名冲突. 生成新鲜名字的策略有很多. "
      "这里, 总体的方法意在保持简单, 并且生成对人类来说有意义的名字. "
      "技巧是追踪已经使用过的, 因而不可用的名字. "
      "当需要一个新鲜名字时, 取某个起始名字, 并在其后添加星号, 直到它不在已用名字之中.")
   (P (CodeB "(add-* x) → symbol?
  x : symbol?")
      "为一个符号添加一个星号."
      (CodeB "(define (add-* x)
  (string->symbol
   (string-append (symbol->string x)
                  \"*\")))"))
   (P (CodeB "(freshen used x) → symbol?
  used : (listof symbol?)
  x : symbol?")
      "构造一个不出现在" (Code "used") "中的名字."
      (CodeB "(define (freshen used x)
  (if (memv x used)
      (freshen used (add-* x))
      x))"))
   (P "例子:" (CodeB "> (freshen '() 'x)
'x
> (freshen '(x x*) 'x)
'x**
> (freshen '(x y z) 'y)
'y*"))
   (H2. "规范化无类型" $lambda "演算" #:id "untyped-norm")
   (H3. "规范形式" #:id "normal-forms")
   (CodeB "‹expr› ::= ‹id›
        |  (λ (‹id›) ‹expr›)
        |  (‹expr› ‹expr›)")
   (P $lambda "演算中的表达式并不仅仅由表达式的文法所定义. "
      "还有一个等式理论, 告诉我们两个表达式何时意味着同样的东西. "
      "第一条规则是, 一致地重命名约束变量不会改变表达式的含义, 这一性质称为"
      (Em $alpha "等价") ". 第二条规则是, 将一个" $lambda "表达式应用于一个参数等于该应用的结果, 这条规则称为"
      (Em $beta) ". 由零个或多个" $alpha "和" $beta "步骤等同起来的表达式称为" (Em (: $alpha $beta) "等价的")
      ". 重要的是要记住, 这两条规则都是" (Em "等式")
      ", 这意味着它们可以应用在表达式中的任何地方, 而且既可以从左往右读, 也可以从右往左读.")
   (P "用数学记号来表达的话, " $beta "规则依赖于" (Em "代换")
      ", 即一致地将一个变量的约束出现替换为某个其他表达式, 并且不捕获任何变量. "
      "在" $e_1 "中将" $x "替换为" $e_2 "的操作写作" (: $e_1 (brac (&/ $e_2 $x)))
      ". 可以把它想成除以" $x ", 从而移除每个出现, 然后在每个空位放入一个" $e_2 "."
      (MB (&equiv (Ap (pare (Lam $x $e_1)) $e_2) (: $e_1 (brac (&/ $e_2 $x))))))
   (P "当我们有一组关于句法的等式时, 句法可以被看作是划分成了各个" (Q "桶")
      ", 桶中的每个表达式都与同一个桶中的所有其他表达式" (: $alpha $beta) "等价. "
      "检查两个表达式是否在同一个桶中的一种方式是, 为每个桶指定一个代表表达式, "
      "并提供一种方法, 为任意给定的表达式找到其所在桶的代表. "
      "那么, 如果两个表达式在同一个桶中, 它们就会有相同的代表. "
      "这个典范的代表称为彼此相等的那一组表达式" -- "也就是同一个" (Q "桶")
      "中的表达式" -- "的" (Em "规范形式") ".")
   (P "这里, 我们采用这样的约定: 规范形式是那些不包含可归约表达式, 即" (Em "可约式")
      "的表达式, 也就是说, 其中没有直接应用于参数的" $lambda "表达式. "
      "因为" $alpha "等价比" $beta "等价更容易检查, 大多数人只考虑相对于" $beta "规则的规范形式, "
      "然后在比较" $beta "规范形式时使用" $alpha "等价.")
   (P "从左往右读" $beta "规则, 我们看到总是可以把一个可约式替换为代换的结果. "
      "看待" $beta "规范形式的另一种方式是, 把它们看作所有可约式都已经被替换掉了的表达式.")
   (H3. "寻找规范形式" #:id "finding-normal-forms")
   (P "寻找一个表达式的规范形式的一种方法是反复地遍历它, 进行所有可能的" $beta "归约. "
      "然而, 这是极其低效的" -- "首先, 必须定位一个可约式, 然后应用" $beta "规则构造一个新的表达式. "
      "接着, 必须重建原先那个可约式周围的上下文, 使之指向新构造的表达式. "
      "对每一个可约式都这样做并不是特别高效, 而且所得的代码通常读起来也不令人愉快.")
   (P "另一种选择是, 修改" (Ref "untyped-eval")
      "中基于环境的求值器, 添加第二个步骤, 将值读回成它们的句法, 从而进行规范化. "
      "重用我们的求值器, 添加处理" :λ "表达式体内归约的情形, 要高效得多, "
      "因为不需要多次遍历表达式. 此外, 实现一个带环境的求值器, "
      "比正确地实现代换要容易得多, 后者微妙得出人意料. "
      "通过仔细选择值的集合, 使其" (Em "只") "表示不包含可约式的表达式, "
      "我们可以非常确信我们的规范化过程确实产生了相对于" $beta "转换的规范形式.")
   (P "在" :λ "之下归约时, 还会有在环境中没有值的变量. 为了处理这些情形, 我们需要表示"
      (Em "中性") "表达式的值. 中性表达式是我们尚不能将其归约为值的表达式, "
      "因为诸如函数参数的值这样的信息还不知道. "
      "在这个语言中, 有两种中性表达式: 尚没有值的变量, 以及函数位置是中性的应用.")
   (P "换言之, 值所要捕获的规范形式的文法如下:"
      (CodeB "‹norm› ::= ‹neu›
        |  (λ (‹id›) ‹norm›)
‹neu›  ::= ‹id›
        |  (‹neu› ‹norm›)"))
   (P "除了表示" $lambda "表达式的" :CLOS "以外, 我们还需要表示中性表达式的值."
      (CodeB "(struct N-var (name))
  name : symbol?")
      :N-var "表示一个中性变量, 其名字被保存下来."
      (CodeB "(struct N-ap (rator rand))
  rator : neutral?
  rand : value?")
      :N-ap "表示一个中性应用, 其中rator尚不能被求值.")
   (P "求值器本身应该扩展以处理中性表达式. 也许令人惊讶的是, " :val "保持不变, 而"
      :do-ap "需要为中性应用添加一个额外的情形. 这里再次定义" :val
      ", 以便它能引用新的" :do-ap ".")
   (P (CodeB "(val ρ e) → value?
  ρ : environment?
  e : expression?")
      "对一个表达式求值."
      (CodeB "(define (val ρ e)
  (match e
    [`(λ (,x) ,b)
     (CLOS ρ x b)]
    [x #:when (symbol? x)
     (let ((xv (assv x ρ)))
       (if xv
           (cdr xv)
           (error 'val \"Unknown variable ~v\" x)))]
    [`(,rator ,rand)
     (do-ap (val ρ rator) (val ρ rand))]))"))
   (P (CodeB "(do-ap fun arg) → value?
  fun : value?
  arg : value?")
      "应用一个函数, 可能会给一个中性表达式附加更多的参数."
      (CodeB "(define (do-ap fun arg)
  (match fun
    [(CLOS ρ x b)
     (val (extend ρ x arg) b)]
    ; If the argument is neutral, construct a bigger neutral expression.
    [neutral-fun
     (N-ap fun arg)]))"))
   (P "现在我们可以处理中性的值了, 但是每个中性值都以一个中性变量开头, 而中性变量没有在任何地方引入. "
      "此外, 我们的值显然不是表达式的规范形式. 实现规范化器的第二步是编写一个过程, "
      "将值转换回其作为句法的表示" -- "这一过程称为将值" (Em "读回") "或"
      (Em "引用") "为句法."
      (CodeB "(read-back used-names v) → expression?
  used-names : (listof symbol?)
  v : value?")
      (CodeB "(define (read-back used-names v)
  (match v
    [(CLOS ρ x body)
     (let* ((y (freshen used-names x))
            (neutral-y (N-var y)))
       `(λ (,y)
          ,(read-back (cons y used-names)
                      (val (extend ρ x neutral-y) body))))]
    [(N-var x) x]
    [(N-ap rator rand)
     `(,(read-back used-names rator) ,(read-back used-names rand))]))"))
   (P "例子:" (CodeB "> (read-back '() (val '() '((λ (x) (λ (y) (x y))) (λ (x) x))))
'(λ (y) y)"))
   (P "求值与读回相结合, 就得到了规范化."
      (CodeB "(norm e) → expression?
  e : expression?")
      "规范化一个表达式."
      (CodeB "(define (norm ρ e)
  (read-back '() (val ρ e)))"))
   (P "有了规范化, 就可以以一种更友好的方式运行程序. 可以显示每个表达式的规范形式, 而不是它的值."
      (CodeB "(run-program ρ exprs) → void?
  ρ : (listof (pair symbol? value?))
  exprs : (listof expression?)")
      (CodeB "(define (run-program ρ exprs)
  (match exprs
    [(list) (void)]
    [(list `(define ,x ,e) rest ...)
     (let ([v (val ρ e)])
       (run-program (extend ρ x v) rest))]
    [(list e rest ...)
     (displayln (norm ρ e))
     (run-program ρ rest)]))"))
   (H3. "例子: Church数码" #:id "church-numerals")
   (P (Em "Church数码") "是将自然数编码为无类型" $lambda "演算中的程序的一种方式. "
      "基本的想法是, 一个数" $n "被表示为一个函数, 这个函数接受另一个函数和某个起始值, "
      "并将那个函数对起始值应用" $n "次."
      (CodeB "(with-numerals e) → program?
  e : expression?")
      "将表达式" (Code "e") "放进一个定义了基本Church数码的上下文中."
      (CodeB "(define (with-numerals e)
  `((define church-zero
      (λ (f)
        (λ (x)
          x)))
    (define church-add1
      (λ (n-1)
        (λ (f)
          (λ (x)
            (f ((n-1 f) x))))))
    ,e))"))
   (P (CodeB "(to-church n) → expression?
  n : exact-nonnegative-integer?")
      "将一个自然数转换为它的Church编码."
      (CodeB "(define (to-church n)
  (cond [(zero? n) 'church-zero]
        [(positive? n)
         (let ([church-of-n-1 (to-church (sub1 n))])
           `(church-add1 ,church-of-n-1))]))"))
   (P "例子:" (CodeB "> (run-program '() (with-numerals (to-church 0)))
(λ (f) (λ (x) x))
> (run-program '() (with-numerals (to-church 1)))
(λ (f) (λ (x) (f x)))
> (run-program '() (with-numerals (to-church 4)))
(λ (f) (λ (x) (f (f (f (f x))))))"))
   (P (CodeB "church-add : expression?")
      "Church数码的加法."
      (CodeB "(define church-add
  `(λ (j)
     (λ (k)
       (λ (f)
         (λ (x)
           ((j f) ((k f) x)))))))"))
   (P "例子:" (CodeB "> (run-program
   '()
   (with-numerals `((,church-add ,(to-church 2)) ,(to-church 2))))
(λ (f) (λ (x) (f (f (f (f x))))))"))
   (H2. "错误处理" #:id "error-handling")
   (P "之前, 包含诸如未约束变量或畸形句法之类错误的程序会直接让求值器崩溃. "
      "然而, 类型不匹配并不是" (Em "错误") -- "相反, 它们是在构造程序时可以利用的有用反馈. "
      "这就要求以一种更方便一些的方式来表示类型检查可能失败这一事实."
      (CodeB "(struct go (result)
    #:transparent)
  result : any/c
(struct stop (expr message)
    #:transparent)
  expr : expression?
  message : string?"))
   (P :go "是对一个成功结果的包装, 表示其内容通过了某种测试. " :stop
      "表示某样东西没有通过, 它包含出错的那个表达式, 以及对出错原因的解释. "
      "为了让实现对用户更友好, 可以考虑把" :stop
      "的表达式参数换成诸如源代码位置之类的东西, 把字符串换成某种类型不匹配的结构化表示.")
   (P "下一个定义" :go-on "需要额外的导入. 在模块中添加"
      (CodeB "(require (for-syntax syntax/parse))")
      "以导入定义了" (Code "syntax-parse") "的库. 如果你使用的是"
      (Href "http://docs.racket-lang.org/reference/index.html" (Code "racket"))
      "以外的语言, 例如"
      (Href "http://docs.racket-lang.org/reference/index.html" (Code "racket/base"))
      ", 那么改为添加"
      (CodeB "(require (for-syntax racket/base syntax/parse))")
      "."
      (CodeB "(go-on ([pattern expr] ...) result)"))
   (P "显式地对" :go "的内容做模式匹配读写起来可能很繁琐, 所以" :go-on "宏允许一种类似于"
      (Code "let*") "的语法, 只不过只要有任何子计算产生" :stop ", 整个表达式就是" :stop
      ". 这类似于Haskell中用于" (Code "Either") "单子的" (Code "do") "记法."
      (CodeB "(define-syntax (go-on stx)
  (syntax-parse stx
    [(go-on () result)
     (syntax/loc stx
       result)]
    [(go-on ([pat0 e0] [pat e] ...) result)
     (syntax/loc stx
       (match e0
         [(go pat0) (go-on ([pat e] ...) result)]
         [(go v) (error 'go-on \"Pattern did not match value ~v\" v)]
         [(stop expr msg) (stop expr msg)]))]))"))
   (P "例子:" (CodeB "> (define (bigger-than-two n)
    (if (> n 2)
        (go n)
        (stop n \"Not greater than two\")))
> (go-on ([x (bigger-than-two 4)]
          [y (bigger-than-two 5)])
    (go (+ x y)))
(go  9)
> (go-on ([x (bigger-than-two 1)]
          [y (bigger-than-two 5)])
    (go (+ x y)))
(stop  1 \"Not greater than two\")
> (go-on ([x (bigger-than-two 4)]
          [y (bigger-than-two -3)])
    (go (+ x y)))
(stop  -3 \"Not greater than two\")"))
   (H2. "双向类型检查" #:id "bidirectional-type-checking")
   (P "类型系统规定了特定的表达式在何种条件下可以具有特定的类型, "
      "但并不保证我们有一个高效地检查这一点的算法. 然而, 有时可以直接从定义类型系统的规则中读出一个算法. "
      "这通常发生在任何给定情形下至多只有一条规则适用的时候. "
      "因为是程序的句法和类型决定了要作出哪个选择, 这一性质称为" (Em "句法导向的") ".")
   (P "类型系统可能在许多方面不是句法导向的. 一种情况是, 程序中可能没有足够的信息来确定类型. "
      "例如, 像ML这样所有类型都可以推断的语言, 通常需要一个看起来与规则很不相像的类型检查算法. "
      "另一种选择是要求程序在足够多的位置标注类型, 使得类型规则可以跟随这些标注. "
      "类型系统常常不是句法导向的另一种情况是, 具有复杂的类型相等或归入的概念, "
      "这使得类型检查器在每一步都既可以变换一个类型, 也可以跟随程序的句法. "
      "这些并不是类型可能不是句法导向的仅有的方式, 但它们经常出现.")
   (P "一个不是句法导向的类型系统的例子是函数上没有类型标注的简单类型" $lambda "演算:"
      (MB (split16
           (Axiom (Has (&cm Γ_1 (&: $x $t) Γ_2) $x $t))
           (&rule (Has (&cm Γ (&: $x $t_1)) $e $t_2)
                  (Has Γ (Lam $x $e) (&-> $t_1 $t_2)))
           (&rule (Has Γ $e_1 (&-> $A $B))
                  (Has Γ $e_2 $A)
                  (Has Γ (Ap $e_1 $e_2) $B))))
      (MB (split16
           (Axiom (Has Γ $zero $Nat))
           (&rule (Has Γ $n $Nat)
                  (Has Γ (Ap $add1 $n) $Nat))))
      (MB (&rule (Has Γ $n $Nat)
                 (Has Γ $b $t_b)
                 (Has Γ $s (&-> $Nat $t_b $t_b))
                 (Has Γ (Ap $rec $n $b $s) $t_b))))
   (P "如果把这些规则当作一个根据类型检查程序的程序的源代码来读, 那么" (Lam $x $e)
      "的规则不能直接翻译, 因为没有地方可以得到类型" $t_1 ".")
   (P (Em "双向类型检查") "是一种使类型系统成为句法导向的技术, 它只增加最少的标注负担. "
      "通常, 只有表达式的顶层或任何显式的可约式需要标注. "
      "此外, 双向类型检查还为插入类型相等或归入检查的恰当位置提供了指导.")
   (H3. "类型" #:id "types")
   (P "Racket中类型的表示把运算符放在前缀位置, 所以函数类型的箭头位于开头."
      (CodeB "‹type› ::= Nat
        |  (→ ‹type› ‹type›)")
      (CodeB "(type=? t1 t2) → boolean?
  t1 : any/c
  t2 : any/c")
      "两个类型相等, 当它们都是有效的类型并且在结构上匹配时."
      (CodeB "(define (type=? t1 t2)
  (match* (t1 t2)
    [('Nat 'Nat) #t]
    [(`(→ ,A1 ,B1) `(→ ,A2 ,B2))
     (and (type=? A1 A2) (type=? B1 B2))]
    [(_ _) #f]))"))
   (P (CodeB "(type? t) → boolean?
  t : any/c")
      "如果某样东西作为类型与自身相等, 那么它就是一个类型. "
      "因此, 不需要再编写第二个递归函数来识别有效的类型."
      (CodeB "(define (type? t)
  (type=? t t))"))
   (P "例子:" (CodeB "> (type? 'Nat)
#t
> (type? '(Nat))
#f
> (type? '(→ Nat Nat))
#t
> (type=? 'Nat 'Nat)
#t
> (type=? '(→ Nat (→ Nat Nat))
          '(→ Nat (→ Nat Nat)))
#t
> (type=? '(→ (→ Nat Nat) Nat)
          '(→ Nat (→ Nat Nat)))
#f"))
   (H3. "检查类型" #:id "checking-types")
   (P "编写双向类型检查器时, 第一步是把编程语言中的表达式分为" (Em "引入") "形式和"
      (Em "消去") "形式. 引入形式, 也称为" (Em "构造子")
      ", 用来创建一个类型的成员, 而消去子则将构造子内部的信息暴露给计算. "
      "在本章中, " :→ "类型的构造子是" :λ ", " :Nat "的构造子是" :zero "和" :add1
      ". 消去子是函数应用和" :rec ".")
   (P "在双向类型检查之下, 类型系统被分成两种模式: 在" (Em "检查")
      "模式中, 一个表达式相对于一个已知的类型进行分析, 看它是否符合; 而在" (Em "综合")
      "模式中, 一个类型直接从一个表达式导出. 每个可以综合出类型的表达式, "
      "都可以相对于一个给定的类型进行检查, 方法是进行综合, 然后将综合出的类型与期望的类型进行比较. "
      "这正是可以插入归入或其他某种非平凡的类型相等检查的地方. 此外, 类型标注 (这里写作"
      (&in $e $A) ") 使得一个可以检查的表达式能够用在需要综合的地方. "
      "通常, 引入形式有检查规则, 而消去形式允许综合.")
   (P "当消去子遇到构造子时, 就出现了计算步骤 (即可约式). "
      "通常, 大多数程序不包含显式的可约式, 这是双向类型检查能有效减轻标注负担的一个关键原因"
      -- "只有当一个可检查的表达式 (引入形式) 被用在综合的位置 (通常是消去的目标) 时, 才需要标注. "
      "因为这种情况很少见, 通常只需要在包围表达式的最外层进行标注.")
   (P "例如, 一个返回其第二个参数的函数通常会写成"
      (MB (&in (pare (Lam $x (Lam $y $y))) (&-> $Nat $Nat $Nat)))
      "而不是"
      (MB (&in (pare (Lam $x (Lam $y (Ap (pare (&in (pare (Lam $z $z)) (&-> $Nat $Nat))) $y))))
               (&-> $Nat $Nat $Nat)))
      "因为没有理由在程序中包含这个可约式.")
   (P "在数学记号中, 类型系统的双向版本将判断形式" (Has Γ $e $t) "拆分为两种判断形式: "
      (Syn Γ $e $t) ", 意为类型" $t "可以从" $e "综合出来; 以及" (Chk Γ $e $t)
      ", 意为可以检查" $e "具有类型" $t ". 箭头的方向表示类型信息的流向. "
      "检查时, 类型和表达式都被视为算法的输入; 而在综合中, 只有表达式是输入, 类型则是输出. "
      "记住哪种判断形式是哪种的一个办法是, 注意到" $lArr "的尖端看起来有点像字母" (Q "C") ".")
   (P "阅读双向类型系统时, 从横线下方开始. 如果结论是综合, 那么当表达式匹配时规则适用. "
      "如果结论是检查, 那么当表达式和类型都与规则匹配时规则适用. "
      "换言之, 在横线下方, 输入被匹配, 而输出被构造. 输入约束元变量, 而输出引用已经约束的元变量. "
      "发现一条规则匹配之后, 接下来按照阅读英语的顺序, 从左到右, 从上到下检查横线上方的前提. "
      "在前提中, 输入构造数据, 所以它们的元变量必须已经约束; 而输出与数据进行匹配, 并且可以约束元变量.")
   (P "前两条规则规定了系统如何在检查与综合之间转换. 如果有类型标注, "
      "那么当表达式可以按相应的类型检查时, 综合就会成功. "
      "如果我们能综合出一个类型, 那么我们就可以检查它, 方法是先综合出一个类型, "
      "然后检查综合出的类型是否等于期望的类型."
      (MB (split16
           (&rule (Chk Γ $e $A)
                  (Syn Γ (&in $e $A) $A))
           (&rule (Syn Γ $e $B) (&= $B $A)
                  (Chk Γ $e $A)))))
   (P "对于变量, 总是可以通过在" Γ "中查找来综合出一个类型. 对于函数, "
      "我们通过相对于任意的箭头类型进行检查, 来避免必须为约束变量发明一个类型. "
      "类似地, 我们要求能为被应用的函数综合出一个类型, 然后相对于该函数的参数类型检查参数, "
      "从而避免在应用中必须发明参数类型."
      (MB (split16
           (Axiom (Syn (&cm Γ_1 (&: $x $A) Γ_2) $x $A))
           (&rule (Chk (&cm Γ (&: $x $A)) $e $B)
                  (Chk Γ (Lam $x $e) (&-> $A $B)))
           (&rule (Syn Γ $e_1 (&-> $A $B))
                  (Chk Γ $e_2 $A)
                  (Syn Γ (Ap $e_1 $e_2) $B)))))
   (P "一般而言, 引入形式需要类型检查, 而不是综合, 尽管用综合来实现" :Nat
      "也是可能的, 因为它的构造子非常简单. 但是, 例如列表, 并没有显式地指明列表中条目的类型, "
      "所以相对于一个已知的列表类型来检查它们, 可以使这一信息被提取出来并传播下去. "
      "对构造子参数的递归类型检查应该在检查模式下进行, "
      "因为构造子的类型会包含足够的信息来发现参数的类型. 在本教程中, 所有构造子的类型都是被检查的."
      (MB (split16
           (Axiom (Chk Γ $zero $Nat))
           (&rule (Chk Γ $n $Nat)
                  (Chk Γ (Ap $add1 $n) $Nat)))))
   (P "消去形式的类型一般是综合的, 而不是检查的. 目标的类型应当被综合出来, "
      "而这应该提供足够的类型信息, 使得其余参数的类型可以被检查. "
      "这确实要求直接的可约式在目标周围有一个类型标注, 以便在检查与综合之间进行调解. "
      "然而, 因为大多数实际的程序不包含显式的可约式, 而且消去和变量都允许综合, "
      "所以实践中出现的目标不需要标注."
      (MB (&rule (Chk Γ $n $Nat)
                 (Chk Γ $b $A)
                 (Chk Γ $s (&-> $Nat $A $A))
                 (Syn Γ (Ap (Rec $A) $n $b $s) $A))))
   (P "表达为程序, 这些规则就成为过程" :synth "和" :check "."
      (CodeB "(synth Γ e) → (perhaps/c type?)
  Γ : context?
  e : expression?")
      (CodeB "(define (synth Γ e)
  (match e
    ; Type annotations
    [`(the ,t ,e2)
     (if (not (type? t))
         (stop e (format \"Invalid type ~a\" t))
         (go-on ([_ (check Γ e2 t)])
           (go t)))]
    ; Recursion on Nat
    [`(rec ,type ,target ,base ,step)
     (go-on ([target-t (synth Γ target)]
             [_ (if (type=? target-t 'Nat)
                    (go 'ok)
                    (stop target (format \"Expected Nat, got ~v\"
                                         target-t)))]
             [_ (check Γ base type)]
             [_ (check Γ step `(→ Nat (→ ,type ,type)))])
       (go type))]
    [x #:when (and (symbol? x)
                   (not (memv x '(the rec λ zero add1))))
     (match (assv x Γ)
       [#f (stop x \"Variable not found\")]
       [(cons _ t) (go t)])]
    [`(,rator ,rand)
     (go-on ([rator-t (synth Γ rator)])
       (match rator-t
         [`(→ ,A ,B)
          (go-on ([_ (check Γ rand A)])
            (go B))]
         [else (stop rator (format \"Not a function type: ~v\"
                                   rator-t))]))]))")
      (CodeB "(check Γ e t) → (perhaps/c 'ok)
  Γ : context?
  e : expression?
  t : type?")
      (CodeB "(define (check Γ e t)
  (match e
    ['zero
     (if (type=? t 'Nat)
         (go 'ok)
         (stop e (format \"Tried to use ~v for zero\" t)))]
    [`(add1 ,n)
     (if (type=? t 'Nat)
         (go-on ([_ (check Γ n 'Nat)])
           (go 'ok))
         (stop e (format \"Tried to use ~v for add1\" t)))]
    [`(λ (,x) ,b)
     (match t
       [`(→ ,A ,B)
        (go-on ([_ (check (extend Γ x A) b B)])
          (go 'ok))]
       [non-arrow
        (stop e (format \"Instead of → type, got ~a\" non-arrow))])]
    [other
     (go-on ([t2 (synth Γ e)])
       (if (type=? t t2)
           (go 'ok)
           (stop e
                 (format \"Synthesized type ~v where type ~v was expected\"
                         t2
                         t))))]))"))
   (P "例子:" (CodeB "> (synth (list (cons 'x 'Nat)) 'x)
(go  'Nat)
> (check '() 'zero 'Nat)
(go  'ok)
> (check '() '(add1 zero) 'Nat)
(go  'ok)
> (check '() '(λ (x) x) '(→ Nat Nat))
(go  'ok)
> (check '()
         '(λ (j)
            (λ (k)
              (rec Nat j k (λ (n-1)
                             (λ (sum)
                               (add1 sum))))))
         '(→ Nat (→ Nat Nat)))
(go  'ok)"))
   (H3. "定义" #:id "definitions")
   (P "当运行程序求其值, 或者寻找程序中一个表达式的规范形式时, 定义扩展了运行时环境" $rho
      ". 类似地, 对于类型检查, 定义扩展了上下文" Γ ".")
   (P "为了让实现尽可能简单, 定义上的类型标注没有单独的句法. 相反, 每个定义的定义体都应当支持类型综合, 而"
      :the "可以用来标注诸如函数这样的检查形式."
      (CodeB "(check-program Γ prog) → (perhaps/c context?)
  Γ : context?
  prog : (listof (or/c expression? (list/c 'define symbol? expression?)))")
      "对一系列定义和/或表达式进行类型检查."
      (CodeB "(define (check-program Γ prog)
  (match prog
    ['()
     (go Γ)]
    [(cons `(define ,x ,e) rest)
     (go-on ([t (synth Γ e)])
       (check-program (extend Γ x t) rest))]
    [(cons e rest)
     (go-on ([t (synth Γ e)])
       (begin
         (printf \"~a has type ~a\\n\" e t)
         (check-program Γ rest)))]))"))
   (P "例子:" (CodeB "> (check-program '()
                 '((define three
                     (the Nat
                          (add1 (add1 (add1 zero)))))
                   (define +
                     (the (→ Nat (→ Nat Nat))
                          (λ (n)
                            (λ (k)
                              (rec Nat n
                                k
                                (λ (pred)
                                  (λ (almost-sum)
                                    (add1 almost-sum))))))))
                   (+ three)
                   ((+ three) three)))
(+ three) has type (→ Nat Nat)
((+ three) three) has type Nat
(go  '((+ → Nat (→ Nat Nat)) (three . Nat)))"))
   (P "输出的最后一行把" (Code "+") "映射到类型" (Code "(→ Nat (→ Nat Nat))")
      ", 这是因为在Racket中, 序对和列表都是用" :cons
      "构成的, 而打印器没有办法分辨一个特定的序对是打算作为列表的一部分, 还是作为一个"
      (Code "cdr") "恰好是列表的序对.")
   (P "例子:" (CodeB "> (cons '+ '(→ Nat (→ Nat Nat)))
'(+ → Nat (→ Nat Nat))"))
   (H2. "有类型的基于求值的规范化" #:id "typed-nbe")
   (P "类型不仅仅是程序的集合. 类型还可以规定这些程序中哪些是彼此等价的. 写下"
      (: Γ $vdash (&= $e_1 $e_2) $: $A) "意味着类型" $A "认为表达式" $e_1 "和" $e_2
      "是等价的, 并且预设" $e_1 "和" $e_2 "都具有类型" $A ". 在本章中, 我们使用以下规则:"
      (MB (split16
           (: (&rule (Has Γ $f (&-> $A $B))
                     (&!in $x (app $FV $f))
                     (Eqv Γ (Lam $x (Ap $f $x)) $f (&-> $A $B)))
              (@ $eta))
           (&rule (Eqv (&cm Γ (&: $x $A)) $e_1 $e_2 $B)
                  (Eqv Γ (Lam $x $e_1) (Lam $x $e_2) (&-> $A $B)))))
      "{译注: 原文" $eta "规则的结论漏掉了" (: Γ $vdash) ", 这里补上.}"
      (MB (&rule (Has (&cm Γ (&: $x $A)) $e_2 $B)
                 (Has Γ $e_1 $A)
                 (Eqv Γ (Ap (pare (Lam $x $e_2)) $e_1) (: (brac (&/ $e_1 $x)) $e_2) $B)))
      (MB (split16
           (Axiom (Eqv Γ $zero $zero $Nat))
           (&rule (Eqv Γ $e_1 $e_2 $Nat)
                  (Eqv Γ (Ap $add1 $e_1) (Ap $add1 $e_2) $Nat))))
      (MB (&rule (Has Γ $b $A)
                 (Has Γ $s (&-> $Nat $A $A))
                 (Eqv Γ (Ap (Rec $A) $zero $b $s) $b $A)))
      (MB (&rule (Has Γ $n $Nat)
                 (Has Γ $b $A)
                 (Has Γ $s (&-> $Nat $A $A))
                 (Eqv Γ (Ap (Rec $A) (pare (Ap $add1 $n)) $b $s)
                      (Ap $s $n (pare (Ap (Rec $A) $n $b $s)))
                      $A))))
   (P "因为类型是相等性的内在组成部分, 我们可以自由地在一个类型上采用这样的规则, "
      "它们不一定是由作用于构造子的消去子的归约所给出的" -- "有些规则在消去子的周围引入构造子, "
      "例如上面函数的" $eta "规则.")
   (P "之后, 在检查依赖类型时, 相等性判断将由计算机来检查. 计算机能够等同的表达式越多, "
      "使用所得的语言就越不繁琐. 然而, 规范形式不只是不包含可约式的表达式. "
      "成为规范形式, 就是成为由相等性判断所诱导的等价类的" (Em "那个")
      "典范代表, 使得比较规范形式的" $alpha "等价性就足以判定两个表达式是否相等.")
   (P "解决这一问题的一种技术是扩展基于求值的规范化, 使其将类型考虑在内. "
      "归约仍然像以前一样在求值器中发生, 但是读回过程的有类型版本会考虑类型, 以进行" $eta "展开.")
   (H3. "有类型NbE的值" #:id "values-for-typed-nbe")
   (P (CodeB "(struct ZERO ()
    #:transparent)
(struct ADD1 (pred)
    #:transparent)
  pred : value?
(struct NEU (type neu)
    #:transparent)
  type : type?
  neu : neutral?")
      "与无类型的基于求值的规范化不同, 有类型的版本需要追踪中性表达式的类型, "
      "以便在必要时确保它们被" $eta "展开. 构造子" :NEU "用来追踪这些类型. "
      "这种将表达式嵌入到值之中的做法常常称为" (Em "反射") ", 并用向上的箭头" $uarr "来表示."
      (CodeB "(struct N-var (name)
    #:transparent)
  name : symbol?
(struct N-ap (rator rand))
  rator : neutral?
  rand : norm?
(struct N-rec (type target base step)
    #:transparent)
  type : type?
  target : neutral?
  base : norm?
  step : norm?")
      "中性表达式的构造方式与无类型的基于求值的规范化中的类似. 然而, 嵌入的值现在包含了由"
      :THE "提供的类型标注."
      (CodeB "(struct THE (type value)
    #:transparent)
  type : type?
  value : value?
(norm? v) → boolean?
  v : any/c")
      "等待被读回的规范化表达式与它们的类型保存在一起. 这通常称为" (Em "具化")
      ", 即把一个值变成一个表达式, 并且常常用向下的箭头" $darr "来表示."
      (CodeB "(define (norm? v)
  (THE? v))"))
   (H3. "求值器" #:id "typed-eval")
   (P "就像在" (Ref "untyped-norm") "中一样, 规范化由求值以及随后的读回构成. "
      "这里, 包含了自然数的引入规则和消去规则."
      (CodeB "(val ρ e) → value?
  ρ : environment?
  e : expression?"))
   (P "这个求值器的工作方式与无类型规范化的求值器本质上相同. 构造子表达式成为值, "
      "而消去子则委托给辅助过程, 这些辅助过程要么在目标是值时计算出正确的答案, "
      "要么在目标是中性时构造更大的中性项."
      (CodeB "(define (val ρ e)
  (match e
    [`(the ,type ,expr)
     (val ρ expr)]
    ['zero (ZERO)]
    [`(add1 ,n) (ADD1 (val ρ n))]
    [x #:when (and (symbol? x)
                   (not (memv x '(the zero add1 λ rec))))
     (cdr (assv x ρ))]
    [`(λ (,x) ,b)
     (CLOS ρ x b)]
    [`(rec ,type ,target ,base ,step)
     (do-rec type (val ρ target) (val ρ base) (val ρ step))]
    [`(,rator ,rand)
     (do-ap (val ρ rator) (val ρ rand))]))")
      (CodeB "(do-ap fun arg) → value?
  fun : value?
  arg : value?
(do-rec type target base step) → value?
  type : type?
  target : value?
  base : value?
  step : value?"))
   (P "消去子的实现与无类型规范化非常相似. 因为相等性是基于" (Em "类型")
      "的, 所以嵌入的规范表达式 (例如应用中的运算数) 包含类型标注. "
      "这是因为读回中性表达式时必须进行" $eta "展开. 同样, 包围中性表达式的类型标注被用来计算每个嵌入的规范形式的恰当类型."
      (CodeB "(define (do-ap fun arg)
  (match fun
    [(CLOS ρ x e)
     (val (extend ρ x arg) e)]
    [(NEU `(→ ,A ,B) ne)
     (NEU B (N-ap ne (THE A arg)))]))")
      (CodeB "(define (do-rec type target base step)
  (match target
    [(ZERO) base]
    [(ADD1 n)
     (do-ap (do-ap step n)
            (do-rec type n base step))]
    [(NEU 'Nat ne)
     (NEU type
          (N-rec type
                 ne
                 (THE type base)
                 (THE `(→ Nat (→ ,type ,type)) step)))]))"))
   (H3. "有类型的读回" #:id "typed-read-back")
   (P "在无类型的基于求值的规范化中, 是通过考察值来决定如何读回它们的. "
      "然而, 在有类型NbE中, 每个类型都可以规定它自己的相等性概念, 从而规定它的规范形式的句法. "
      "因此, 读回现在是对" (Em "类型") "的结构而不是值的结构进行递归."
      (CodeB "(read-back used-names type value) → expression?
  used-names : (listof symbol?)
  type : type?
  value : value?
(read-back-neutral used-names ne) → expression?
  used-names : (listof symbol?)
  ne : neutral?")
      (CodeB "(define (read-back used-names type value)
  (match type
    ['Nat
     (match value
       [(ZERO) 'zero]
       [(ADD1 n) `(add1 ,(read-back used-names 'Nat n))]
       [(NEU _ ne)
        (read-back-neutral used-names ne)])]
    [`(→ ,A ,B)
     (let ([x (freshen used-names 'x)])
       `(λ (,x)
          ,(read-back (cons x used-names)
                      B
                      (do-ap value (NEU A (N-var x))))))]))
(define (read-back-neutral used-names ne)
  (match ne
    [(N-var x) x]
    [(N-ap fun (THE arg-type arg))
     `(,(read-back-neutral used-names fun)
       ,(read-back used-names arg-type arg))]
    [(N-rec type target (THE base-type base) (THE step-type step))
     `(rec ,type
        ,(read-back-neutral used-names target)
        ,(read-back used-names base-type base)
        ,(read-back used-names step-type step))]))"))
   (H3. "带定义的程序" #:id "programs-with-definitions")
   (P "就像在无类型语言中一样, 使用带定义的系统更为方便. 此外, "
      "双向类型检查的某些好处要等到语言有了顶层定义才会显现, 因为标注的减少没有那么明显."
      (CodeB "(struct def (type value)
    #:transparent)
  type : type?
  value : value?"))
   (P "对于定义的名字, 类型和值都必须保存下来, 以便对其余的表达式既进行类型检查又进行求值. 一个"
      (Code "def") "由一个类型和该类型的一个值组成.")
   (P "{原注: 希腊字母" Δ "读作" (Q "delta") ". 在这里它代表定义 (definitions).} "
      "与" Γ "和" $rho "类似, 定义保存在一个关联列表中, 我们将其称为" Δ
      ". 定义既可以转换为类型检查上下文, 也可以转换为运行时环境, 因为它同时包含了类型和定义."
      (CodeB "(defs->ctx Δ) → context?
  Δ : definitions?
(defs->env Δ) → environment?
  Δ : definitions?")
      (CodeB "(define (defs->ctx Δ)
  (match Δ
    ['() '()]
    [(cons (cons x (def type _)) rest)
     (extend (defs->ctx rest) x type)]))
(define (defs->env Δ)
  (match Δ
    ['() '()]
    [(cons (cons x (def _ value)) rest)
     (extend (defs->env rest) x value)]))")
      (CodeB "(run-program Δ prog) → (perhaps/c definitions?)
  Δ : definitions?
  prog : (listof (or/c (list 'define symbol? expression?)
              expression?))"))
   (P "运行带定义的有类型程序与其无类型的对应物类似. 定义被类型检查, 求值, 然后保存; "
      "表达式被类型检查和求值, 并打印出它们的规范形式."
      (CodeB "(define (run-program Δ prog)
  (match prog
    ['() (go Δ)]
    [(cons `(define ,x ,e) rest)
     (go-on ([type (synth (defs->ctx Δ) e)])
       (run-program (extend Δ x (def type (val (defs->env Δ) e)))
                    rest))]
    [(cons e rest)
     (let ([Γ (defs->ctx Δ)]
           [ρ (defs->env Δ)])
       (go-on ([type (synth Γ e)])
         (let ([v (val ρ e)])
           (begin
             (printf \"(the ~a\\n  ~a)\\n\"
                     type
                     (read-back (map car Γ) type v))
             (run-program Δ rest)))))]))"))
   (P "例子:" (CodeB "> (run-program '() '((define +
                       (the (→ Nat
                                (→ Nat
                                    Nat))
                            (λ (x)
                              (λ (y)
                                (rec Nat x
                                  y
                                  (λ (_)
                                    (λ (sum)
                                      (add1 sum))))))))
                     +
                     (+ (add1 (add1 zero)))
                     ((+ (add1 (add1 zero))) (add1 zero))))
(the (→ Nat (→ Nat Nat))
  (λ (x) (λ (x*) (rec Nat x x* (λ (x**) (λ (x***) (add1 x***)))))))
(the (→ Nat Nat)
  (λ (x) (add1 (add1 x))))
(the Nat
  (add1 (add1 (add1 zero))))
(go
 (list
  (cons
   '+
   (def
    '(→ Nat (→ Nat Nat))
    (CLOS '() 'x '(λ (y) (rec Nat x y (λ (_) (λ (sum) (add1 sum))))))))))"))
   (H2. "一小块Pie" #:id "tiny-pie")
   (P "现在是时候把所有部分组合起来, 为一个小小的依赖类型语言编写类型检查器了, 这个语言称为"
      (Em "Tartlet") ". Tartlet与" (I "The Little Typer")
      "中的语言Pie非常相像, 只是它的特性更少, 规则更简单. "
      "Tartlet包含函数, 序对, 自然数, 原子, 以及单位类型和空类型. "
      "{译注: 这里没有列出相等类型 (" (Code "=") ", " (Code "same") ", " (Code "replace") "), 但下面的文法中包含它.} "
      "此外, Tartlet的类型的类型"
      :U "是一个" :U ". 这使得它作为逻辑是不一致的, 但作为编程语言它仍然是安全的. "
      "{译注: 所有类型都以" :U "为类型, 而" :U "自身的类型也是" :U ", 这通常称为type-in-type. "
      "按照Curry-Howard对应, 类型即命题, 类型的居民即证明. Girard悖论表明, 在这样的系统中可以构造出空类型"
      :Absurd "的一个居民, 于是任何命题都可以证明, 所以作为逻辑它是不一致的. "
      "但是这个居民是一个永不终止的项: 它破坏的是终止性, 而不是类型安全. "
      "良类型的程序仍然不会卡住 (例如把" :zero "当作函数来应用), 只是可能陷入无穷的计算, "
      "就像其他语言中一个声称返回整数的死循环. 由于类型检查依赖于规范化, "
      "这也意味着刻意构造的程序可以使类型检查器不终止. 标准的解决办法是宇宙的层级, 见"
      (Ref "projects") "的最后一条.}")
   (H3. "语言" #:id "the-language")
   (P "Tartlet的句法由包含一组固定关键字的S表达式构成. 任何不是关键字的Racket符号都是有效的变量名. "
      "有三种约束形式: " :λ ", " :Π "和" :Σ "."
      (CodeB "‹expr› ::= ‹id›
        |  (Π ((‹id› ‹expr›)) ‹expr›)
        |  (λ (‹id›) ‹expr›)
        |  (‹expr› ‹expr›)
        |  (Σ ((‹id› ‹expr›)) ‹expr›)
        |  (cons ‹expr› ‹expr›)
        |  (car ‹expr›)
        |  (cdr ‹expr›)
        |  Nat
        |  zero
        |  (add1 ‹expr›)
        |  (ind-Nat ‹expr› ‹expr› ‹expr› ‹expr›)
        |  (= ‹expr› ‹expr› ‹expr›)
        |  same
        |  (replace ‹expr› ‹expr› ‹expr›)
        |  Trivial
        |  sole
        |  Absurd
        |  (ind-Absurd ‹expr› ‹expr›)
        |  Atom
        |  '‹id›
        |  U
        |  (the ‹expr› ‹expr›)"))
   (H4. "标识符" #:id "identifiers")
   (P "标识符是那些尚不是关键字的符号. 符号" (Code "quote") "是保留的, 因为在内部, "
      (Code "'a") "被表示为" (Code "(list 'quote a)") "."
      (CodeB "keywords : (listof symbol?)")
      "保留关键字的列表"
      (CodeB "(define keywords
  (list 'define
        'U
        'Nat 'zero 'add1 'ind-Nat
        'Σ 'Sigma 'cons 'car 'cdr
        'Π 'Pi 'λ 'lambda
        '= 'same 'replace
        'Trivial 'sole
        'Absurd 'ind-Absurd
        'Atom 'quote
        'the))"))
   (P (CodeB "(keyword? x) → boolean?
  x : any/c")
      "检查" (Code "x") "是否是一个有效的关键字."
      (CodeB "(define (keyword? x)
  (if (memv x keywords)
      #t
      #f))"))
   (P (CodeB "(var? x) → boolean?
  x : any/c")
      "检查" (Code "x") "是否是一个有效的变量名."
      (CodeB "(define (var? x)
  (and (symbol? x)
       (not (keyword? x))))"))
   (H4. "程序的" $alpha "等价" #:id "alpha-equivalence")
   (P "检查简单类型时可以用" :type=? "来比较两个类型, "
      "但在依赖类型语言中需要" $alpha "等价, 因为类型是可以约束变量的程序. "
      "为了让" $alpha "等价过程保持简短, 它并不拒绝无效的程序, 所以它不能像" :type=?
      "的自反情形可以用来检验句法上有效的简单类型那样, 用来检查句法上有效的程序."
      (CodeB "(α-equiv? e1 e2) → boolean?
  e1 : expression?
  e2 : expression?")
      "判定两个表达式是否" $alpha "等价."
      (CodeB "(define (α-equiv? e1 e2)
  (α-equiv-aux e1 e2 '() '()))")
      (CodeB "(α-equiv-aux e1 e2 xs1 xs2) → boolean?
  e1 : expression?
  e2 : expression?
  xs1 : (listof (pair symbol symbol))
  xs2 : (listof (pair symbol symbol))"))
   (P "检查" $alpha "等价的真正工作使用了更多的状态来追踪约束. 遇到一个约束形式时, 生成一个新鲜的符号 (用"
      :gensym ") 并将其保存为两边的目标. 然后, 在比较名字时, 查阅这张新鲜名字映射的表, "
      "以确定变量是否是约束的, 如果是, 再确定它们是否被同一个约束子所约束.")
   (P "在第二个模式情形中, 有三种有趣的可能性. 如果两个变量都是约束的, "
      "那么比较表示它们约束位置的" :gensym ". 如果它们都是自由的, 那么直接比较它们. "
      "如果一个是约束的而另一个是自由的, 那么它们永远不会匹配, 因为" :gensym
      "永远不会等于用户提供的符号."
      (CodeB "(define (α-equiv-aux e1 e2 xs1 xs2)
  (match* (e1 e2)
    [(kw kw)
     #:when (keyword? kw)
     #t]
    [(x y)
     #:when (and (var? x) (var? y))
     (match* ((assv x xs1) (assv y xs2))
       [(#f #f) (eqv? x y)]
       [((cons _ b1) (cons _ b2)) (eqv? b1 b2)]
       [(_ _) #f])]
    [(`(λ (,x) ,b1) `(λ (,y) ,b2))
     (let ([fresh (gensym)])
       (let ([bigger1 (cons (cons x fresh) xs1)]
             [bigger2 (cons (cons y fresh) xs2)])
         (α-equiv-aux b1 b2 bigger1 bigger2)))]
    [(`(Π ((,x ,A1)) ,B1) `(Π ((,y ,A2)) ,B2))
     (and (α-equiv-aux A1 A2 xs1 xs2)
          (let ([fresh (gensym)])
            (let ([bigger1 (cons (cons x fresh) xs1)]
                  [bigger2 (cons (cons y fresh) xs2)])
              (α-equiv-aux B1 B2 bigger1 bigger2))))]
    [(`(Σ ((,x ,A1)) ,B1) `(Σ ((,y ,A2)) ,B2))
     (and (α-equiv-aux A1 A2 xs1 xs2)
          (let ([fresh (gensym)])
            (let ([bigger1 (cons (cons x fresh) xs1)]
                  [bigger2 (cons (cons y fresh) xs2)])
              (α-equiv-aux B1 B2 bigger1 bigger2))))]
    [(`',x  `',y)
     (eqv? x y)]
    ; This, together with read-back-norm, implements the η law for Absurd.
    [(`(the Absurd ,e1) `(the Absurd ,e2))
     #t]
    [((cons op args1) (cons op args2))
     #:when (keyword? op)
     (and (= (length args1) (length args2))
          (for/and ([arg1 (in-list args1)]
                    [arg2 (in-list args2)])
            (α-equiv-aux arg1 arg2 xs1 xs2)))]
    [((list rator1 rand1) (list rator2 rand2))
     (and (α-equiv-aux rator1 rator2 xs1 xs2)
          (α-equiv-aux rand1 rand2 xs1 xs2))]
    [(_ _) #f]))"))
   (H3. "值与规范化" #:id "values-and-normalization")
   (H4. "值" #:id "the-values")
   (P "按照基于求值的规范化的配方, 我们需要为语言中的每个构造子和类型构造子定义值的表示."
      (CodeB "(struct PI (domain range)
    #:transparent)
  domain : value?
  range : closure?
(struct LAM (body)
    #:transparent)
  body : closure?
(struct SIGMA (car-type cdr-type)
    #:transparent)
  car-type : value?
  cdr-type : closure?
(struct PAIR (car cdr)
    #:transparent)
  car : value?
  cdr : value?
(struct NAT ()
    #:transparent)
(struct ZERO ()
    #:transparent)
(struct ADD1 (pred)
    #:transparent)
  pred : value?
(struct EQ (type from to)
    #:transparent)
  type : value?
  from : value?
  to : value?
(struct SAME ()
    #:transparent)
(struct TRIVIAL ()
    #:transparent)
(struct SOLE ()
    #:transparent)
(struct ABSURD ()
    #:transparent)
(struct ATOM ()
    #:transparent)
(struct QUOTE (symbol)
    #:transparent)
  symbol : symbol?
(struct UNI ()
    #:transparent)"))
   (P "这些值表示引入形式和类型构造子. 因为除了中性表达式之外, "
      "它们不包含任何潜在的计算, 所以它们只表示Tartlet的值.")
   (P "就像在" (Ref "typed-nbe") "中一样, 当中性表达式作为值被包含进来时, 它们带有一个类型标注. "
      "这是因为读回的过程是" (Em "类型导向的") ", 这意味着重建一个值或中性表达式的句法的方式, "
      "取决于它是在哪个类型上构造的. 然而, 因为Tartlet的类型与其他程序一样都是程序, 所以类型标注是一个值."
      (CodeB "(struct NEU (type neutral)
    #:transparent)
  type : value?
  neutral : neutral?"))
   (P "最后, 我们时不时需要直接构造类型值, 而由于诸如变量捕获之类的问题, "
      "构建句法远不如直接构建值方便. 所以有一种新的闭包, 它使用Racket的闭包."
      (CodeB "(struct H-O-CLOS (x fun)
    #:transparent)
  x : symbol?
  fun : (-> value? value?)")
      (Code "H-O-CLOS") "是闭包的另一种表示, 它重用Racket函数及其内置的闭包. "
      "这是为了在类型检查器需要构造一个包含约束变量的类型时, "
      "避免不必要且不方便地经由句法绕一个来回.")
   (P (CodeB "(closure? c) → boolean?
  c : any/c")
      "检查某样东西是否是两种闭包形式之一."
      (CodeB "(define (closure? c)
  (or (CLOS? c) (H-O-CLOS? c)))"))
   (P (CodeB "(closure-name c) → symbol?
  c : closure?")
      "提取闭包中约束变量的名字."
      (CodeB "(define (closure-name c)
  (match c
    [(CLOS _ x _) x]
    [(H-O-CLOS x _) x]))"))
   (H4. "中性表达式" #:id "neutral-expressions")
   (P (CodeB "(struct N-var (name)
    #:transparent)
  name : symbol?")
      "每个中性表达式的基础都是一个中性变量."
      (CodeB "(struct N-ap (fun arg)
    #:transparent)
  fun : neutral?
  arg : normal?
(struct N-car (pair)
    #:transparent)
  pair : neutral?
(struct N-cdr (pair)
    #:transparent)
  pair : neutral?
(struct N-ind-Nat (target motive base step)
    #:transparent)
  target : neutral?
  motive : normal?
  base : normal?
  step : normal?
(struct N-replace (target motive base)
    #:transparent)
  target : neutral?
  motive : normal?
  base : normal?
(struct N-ind-Absurd (target motive)
    #:transparent)
  target : neutral?
  motive : normal?")
      "语言中的每个消去子, 包括函数应用, 都必须能够识别中性的目标, 并将其自身的表示构造为一个中性表达式.")
   (H4. "规范形式" #:id "pie-norm")
   (P (CodeB "(struct THE (type val)
    #:transparent)
  type : value?
  val : value?")
      "规范形式的内部表示用" :THE "构造, 它将一个类型值与一个由该类型所分类的值配成对.")
   (H3. "定义与依赖类型" #:id "definitions-and-dependent-types")
   (P (Ref "typed-nbe") "中的简单类型语言可以把定义与上下文和环境分开存储, "
      "在需要进行类型检查或求值时再分别构造出来. 然而, 在依赖类型语言中, 类型检查可能会调用求值. "
      "这意味着上下文需要区分两种自由变量: 一种来自诸如" :λ ", " :Π "和" :Σ
      "这样的约束形式, 对于它们, 在类型检查期间没有可用的值; 另一种来自定义, 它们在类型检查期间确实有值."
      (CodeB "(struct def (type value)
    #:transparent)
  type : value?
  value : value?
(struct bind (type)
    #:transparent)
  type : value?"))
   (P "上下文包含" (Em "定义") ", 用" (Code "def") "表示, 以及" (Em "自由变量约束")
      ", 用" (Code "bind") "表示."
      (CodeB "(context? Γ) → boolean?
  Γ : any/c")
      "判定" :Γ "是否是一个上下文."
      (CodeB "(define (context? Γ)
  (match Γ
    ['() #t]
    [(cons (cons x b) rest)
     (and (symbol? x) (or (def? b) (bind? b)) (context? rest))]
    [_ #f]))"))
   (P (CodeB "(lookup-type x Γ) → (perhaps/c value?)
  x : symbol?
  Γ : context?")
      "在上下文中查找一个变量, 找到它的类型."
      (CodeB "(define (lookup-type x Γ)
  (match (assv x Γ)
    [#f (stop x \"Unknown variable\")]
    [(cons _ (bind type)) (go type)]
    [(cons _ (def type _)) (go type)]))"))
   (P (CodeB "(ctx->env Γ) → environment?
  Γ : context?")
      "求值器一次调用的初始环境基于当前的类型上下文, "
      "因为上下文中的每个条目都表示一个可能出现在被求值表达式中的自由变量. "
      :Γ "中的每个条目都在初始环境中被转换为一个中性变量值 (" :N-var ")."
      (CodeB "(define (ctx->env Γ)
  (map (lambda (binder)
         (match binder
           [(cons name (bind type))
            (cons name
                  (NEU type
                       (N-var name)))]
           [(cons name (def _ value))
            (cons name value)]))
       Γ))"))
   (P (CodeB "(extend-ctx Γ x type) → context?
  Γ : context?
  x : symbol?
  type : value?")
      "用一个自由变量的类型扩展上下文."
      (CodeB "(define (extend-ctx Γ x t)
  (cons (cons x (bind t)) Γ))"))
   (H4. "求值器" #:id "the-evaluator")
   (P (CodeB "(val-of-closure c v) → value?
  c : closure?
  v : value?")
      "对闭包" (Code "c") "求值, 将其约束变量实例化为" (Code "v") "."
      (CodeB "(define (val-of-closure c v)
  (match c
    [(CLOS ρ x e) (val (extend ρ x v) e)]
    [(H-O-CLOS x f) (f v)]))"))
   (P (CodeB "(val ρ e) → value?
  ρ : environment?
  e : expression?")
      (CodeB "(define (val ρ e)
  (match e
    [`(the ,type ,expr)
     (val ρ expr)]
    ['U (UNI)]
    [`(Π ((,x ,A)) ,B)
     (PI (val ρ A) (CLOS ρ x B))]
    [`(λ (,x) ,b)
     (LAM (CLOS ρ x b))]
    [`(Σ ((,x ,A)) ,D)
     (SIGMA (val ρ A) (CLOS ρ x D))]
    [`(cons ,a ,d)
     (PAIR (val ρ a) (val ρ d))]
    [`(car ,pr)
     (do-car (val ρ pr))]
    [`(cdr ,pr)
     (do-cdr (val ρ pr))]
    ['Nat (NAT)]
    ['zero (ZERO)]
    [`(add1 ,n) (ADD1 (val ρ n))]
    [`(ind-Nat ,target ,motive ,base ,step)
     (do-ind-Nat (val ρ target) (val ρ motive) (val ρ base) (val ρ step))]
    [`(= ,A ,from ,to)
     (EQ (val ρ A) (val ρ from) (val ρ to))]
    ['same
     (SAME)]
    [`(replace ,target ,motive ,base)
     (do-replace (val ρ target) (val ρ motive) (val ρ base))]
    ['Trivial (TRIVIAL)]
    ['sole (SOLE)]
    ['Absurd (ABSURD)]
    [`(ind-Absurd ,target ,motive)
     (do-ind-Absurd (val ρ target) (val ρ motive))]
    ['Atom (ATOM)]
    [`',a  (QUOTE a)]
    [`(,rator ,rand)
     (do-ap (val ρ rator) (val ρ rand))]
    [x #:when (var? x)
     (cdr (assv x ρ))]))"))
   (H4. "消去子" #:id "eliminators")
   (P "每个消去子都由一个Racket过程实现. 这个过程检查消去的目标是否是中性的, "
      "如果是, 它就产生一个新的中性表达式. 否则, 它求出结果值."
      (CodeB "(do-car v) → value?
  v : value?
(do-cdr v) → value?
  v : value?")
      "注意, 一个中性表达式的" :cdr "在其类型标注中包含了该表达式的" :car "."
      (CodeB "(define (do-car v)
  (match v
    [(PAIR a d) a]
    [(NEU (SIGMA A _) ne)
     (NEU A (N-car ne))]))
(define (do-cdr v)
  (match v
    [(PAIR a d) d]
    [(NEU (SIGMA _ D) ne)
     (NEU (val-of-closure D (do-car v))
          (N-cdr ne))]))"))
   (P (CodeB "(do-ap fun arg) → value?
  fun : value?
  arg : value?")
      "实现函数应用 (" $beta "归约), 并将中性函数考虑在内."
      (CodeB "(define (do-ap fun arg)
  (match fun
    [(LAM c)
     (val-of-closure c arg)]
    [(NEU (PI A B) ne)
     (NEU (val-of-closure B arg)
          (N-ap ne (THE A arg)))]))"))
   (P (CodeB "(do-ind-Absurd target motive) → value?
  target : value?
  motive : value?")
      "因为每个" :Absurd "都是中性的, 所以" (Code "do-ind-Absurd") "只有针对中性目标的情形."
      (CodeB "(define (do-ind-Absurd target motive)
  (match target
    [(NEU (ABSURD) ne)
     (NEU motive (N-ind-Absurd ne (THE (UNI) motive)))]))"))
   (P (CodeB "(do-replace target motive base) → value?
  target : value?
  motive : value?
  base : value?")
      "实现" :replace ". 当相等性的证明是" :same "时, 相等的两边是相同的, 所以可以原样返回基础情形."
      (CodeB "(define (do-replace target motive base)
  (match target
    [(SAME) base]
    [(NEU (EQ A from to) ne)
     (NEU (do-ap motive to)
          (N-replace ne
                     (THE (PI A (H-O-CLOS 'x (lambda (_) (UNI))))
                          motive)
                     (THE (do-ap motive from)
                          base)))]))"))
   (P (CodeB "(do-ind-Nat target motive base step) → value?
  target : value?
  motive : value?
  base : value?
  step : value?
(ind-Nat-step-type motive) → value?
  motive : value?")
      :ind-Nat "的运行时实现使用辅助过程" (Code "ind-Nat-step-type") "来构造"
      :ind-Nat "的步骤的类型."
      (CodeB "(define (do-ind-Nat target motive base step)
  (match target
    [(ZERO) base]
    [(ADD1 n) (do-ap (do-ap step n) (do-ind-Nat n motive base step))]
    [(NEU (NAT) ne)
     (NEU (do-ap motive target)
          (N-ind-Nat
           ne
           (THE (PI (NAT)
                    (H-O-CLOS 'k (lambda (k) (UNI))))
                motive)
           (THE (do-ap motive (ZERO)) base)
           (THE (ind-Nat-step-type motive)
                step)))]))
(define (ind-Nat-step-type motive)
  (PI (NAT)
      (H-O-CLOS 'n-1
                (lambda (n-1)
                  (PI (do-ap motive n-1)
                      (H-O-CLOS 'ih
                                (lambda (ih)
                                  (do-ap motive (ADD1 n-1)))))))))"))
   (H4. "读回" #:id "reading-back")
   (P "就像在" (Ref "typed-nbe") "中一样, 从值读回到句法是通过两个相互递归的过程完成的: "
      (Code "read-back-norm") "和" (Code "read-back-neutral") "."
      (CodeB "(read-back-norm Γ norm) → expression?
  Γ : context?
  norm : norm?")
      "将一个规范值转换为表示它的句法."
      (CodeB "(define (read-back-norm Γ norm)
  (match norm
    [(THE (NAT) (ZERO)) 'zero]
    [(THE (NAT) (ADD1 n))
     `(add1 ,(read-back-norm Γ (THE (NAT) n)))]
    [(THE (PI A B) f)
     (define x (closure-name B))
     (define y (freshen (map car Γ) x))
     (define y-val (NEU A (N-var y)))
     `(λ (,y)
        ,(read-back-norm (extend-ctx Γ y A)
                         (THE (val-of-closure B y-val)
                              (do-ap f y-val))))]
    [(THE (SIGMA A D) p)
     (define the-car (THE A (do-car p)))
     (define the-cdr (THE (val-of-closure D the-car) (do-cdr p)))
     `(cons ,(read-back-norm Γ the-car) ,(read-back-norm Γ the-cdr))]
    [(THE (TRIVIAL) _) 'sole]
    [(THE (ABSURD) (NEU (ABSURD) ne))
     `(the Absurd
           ,(read-back-neutral Γ ne))]
    [(THE (EQ A from to) (SAME)) 'same]
    [(THE (ATOM) (QUOTE x)) `',x]
    [(THE (UNI) (NAT)) 'Nat]
    [(THE (UNI) (ATOM)) 'Atom]
    [(THE (UNI) (TRIVIAL)) 'Trivial]
    [(THE (UNI) (ABSURD)) 'Absurd]
    [(THE (UNI) (EQ A from to))
     `(= ,(read-back-norm Γ (THE (UNI) A))
         ,(read-back-norm Γ (THE A from))
         ,(read-back-norm Γ (THE A to)))]
    [(THE (UNI) (SIGMA A D))
     (define x (closure-name D))
     (define y (freshen (map car Γ) x))
     `(Σ ((,y ,(read-back-norm Γ (THE (UNI) A))))
         ,(read-back-norm (extend-ctx Γ y A)
                          (THE (UNI) (val-of-closure D (NEU A (N-var y))))))]
    [(THE (UNI) (PI A B))
     (define x (closure-name B))
     (define y (freshen (map car Γ) x))
     `(Π ((,y ,(read-back-norm Γ (THE (UNI) A))))
         ,(read-back-norm (extend-ctx Γ y A)
                          (THE (UNI) (val-of-closure B (NEU A (N-var y))))))]
    [(THE (UNI) (UNI)) 'U]
    [(THE t1 (NEU t2 ne))
     (read-back-neutral Γ ne)]))"))
   (P (CodeB "(read-back-neutral Γ neu) → expression?
  Γ : context?
  neu : neutral?")
      "将一个中性表达式转换为它作为句法的表示. 唯一一个不能立即看出与其他情形相同的情形是"
      :ind-Absurd "的情形; 它为它的目标添加一个类型标注, 这与" :α-equiv?
      "中的一个特殊情形一起, 使得" :Absurd "的所有中性居民都彼此等同."
      (CodeB "(define (read-back-neutral Γ neu)
  (match neu
    [(N-var x) x]
    [(N-ap ne rand)
     `(,(read-back-neutral Γ ne)
       ,(read-back-norm Γ rand))]
    [(N-car ne) `(car ,(read-back-neutral Γ ne))]
    [(N-cdr ne) `(cdr ,(read-back-neutral Γ ne))]
    [(N-ind-Nat ne motive base step)
     `(ind-Nat ,(read-back-neutral Γ ne)
               ,(read-back-norm Γ motive)
               ,(read-back-norm Γ base)
               ,(read-back-norm Γ step))]
    [(N-replace ne motive base)
     `(replace ,(read-back-neutral Γ ne)
               ,(read-back-norm Γ motive)
               ,(read-back-norm Γ base))]
    [(N-ind-Absurd ne motive)
     `(ind-Absurd (the Absurd ,(read-back-neutral Γ ne))
                  ,(read-back-norm Γ motive))]))"))
   (H3. "类型检查" #:id "type-checking")
   (H4. "类型检查器" #:id "the-type-checker")
   (P "像Pie以及类型论的许多其他实现一样, Tartlet的类型检查器是一个" (Em "精化")
      "类型检查器. 这意味着, 它不只是指出一个表达式是良类型的, 而是返回该表达式的一个插入了更多细节的版本. "
      "换言之, 类型检查器所接受的语言包含规范化器所不理解的结构, 而类型检查器输出的是那个更简单的语言中的表达式.")
   (P "在一个精化的双向类型检查器中, 检查输出输入表达式的一个精化后的等价物, "
      "而综合则同时输出一个精化后的表达式及其类型, 其类型属于规范化器的核心语言. "
      "许多精化类型检查器还会在更简单的语言中重新检查表达式; 为了让这些笔记短一些, Tartlet省去了这一步.")
   (P "在考察类型, 寻找特定的类型构造子时, 类型检查器与它们的" (Em "值")
      "进行匹配. 这保证了类型检查器永远不会忘记在检查之前进行规范化, "
      "否则可能导致包含未实现的计算的类型无法被正确匹配. 例如, " :ind-Nat
      "的定型规则可能会为基础情形给出类型" (Code "((λ (k) Atom) zero)")
      ". 不先归约它就试图将该表达式用作" (Code "'sandwich")
      "的类型是不正确的. 使用值, 它甚至无法表示可约式, 就不必再担心在检查之前进行规范化了."
      (CodeB "(synth Γ e) → (list/c 'the expr? expr?)
  Γ : context?
  e : expr?")
      "类型综合通过考察一个表达式来构造一个类型, 并将两者一起返回."
      (CodeB "(define (synth Γ e)
  (match e
    [`(the ,type ,expr)
     (go-on ([t-out (check Γ type (UNI))]
             [e-out (check Γ expr (val (ctx->env Γ) t-out))])
       (go `(the ,t-out ,e-out)))]
    ['U
     (go '(the U U))]
    [`(,(or 'Σ 'Sigma) ((,x ,A)) ,D)
     (go-on ([A-out (check Γ A (UNI))]
             [D-out (check (extend-ctx Γ x (val (ctx->env Γ) A-out)) D (UNI))])
       (go `(the U (Σ ((,x ,A-out)) ,D-out))))]
    [`(car ,pr)
     (go-on ([`(the ,pr-ty ,pr-out) (synth Γ pr)])
       (match (val (ctx->env Γ) pr-ty)
         [(SIGMA A D)
          (go `(the ,(read-back-norm Γ (THE (UNI) A)) (car ,pr-out)))]
         [non-SIGMA
          (stop e (format \"Expected Σ, got ~v\"
                          (read-back-norm Γ (THE (UNI) non-SIGMA))))]))]
    [`(cdr ,pr)
     (go-on ([`(the ,pr-ty ,pr-out) (synth Γ pr)])
       (match (val (ctx->env Γ) pr-ty)
         [(SIGMA A D)
          (define the-car (do-car (val (ctx->env Γ) pr-out)))
          (go `(the ,(read-back-norm Γ (THE (UNI) (val-of-closure D the-car)))
                    (cdr ,pr-out)))]
         [non-SIGMA
          (stop e (format \"Expected Σ, got ~v\"
                          (read-back-norm Γ (THE (UNI) non-SIGMA))))]))]
    ['Nat (go '(the U Nat))]
    [`(ind-Nat ,target ,motive ,base ,step)
     (go-on ([target-out (check Γ target (NAT))]
             [motive-out
              (check Γ motive (PI (NAT) (H-O-CLOS 'n (lambda (_) (UNI)))))]
             [motive-val (go (val (ctx->env Γ) motive-out))]
             [base-out (check Γ base (do-ap motive-val (ZERO)))]
             [step-out (check Γ
                              step
                              (ind-Nat-step-type motive-val))])
       (go `(the ,(read-back-norm
                   Γ
                   (THE (UNI)
                        (do-ap motive-val (val (ctx->env Γ) target-out))))
                 (ind-Nat ,target-out ,motive-out ,base-out ,step-out))))]
    [`(= ,A ,from ,to)
     (go-on ([A-out (check Γ A (UNI))]
             [A-val (go (val (ctx->env Γ) A-out))]
             [from-out (check Γ from A-val)]
             [to-out (check Γ to A-val)])
       (go `(the U (= ,A-out ,from-out ,to-out))))]
    [`(replace ,target ,motive ,base)
     (go-on ([`(the ,target-t ,target-out) (synth Γ target)])
       (match (val (ctx->env Γ) target-t)
         [(EQ A from to)
          (go-on ([motive-out
                   (check Γ
                          motive
                          (PI A (H-O-CLOS 'x (lambda (x) (UNI)))))]
                  [motive-v (go (val (ctx->env Γ) motive-out))]
                  [base-out (check Γ base (do-ap motive-v from))])
            (go `(the ,(read-back-norm Γ (THE (UNI) (do-ap motive-v to)))
                      (replace ,target-out ,motive-out ,base-out))))]
         [non-EQ
          (stop target (format \"Expected =, but type is ~a\" non-EQ))]))]
    [`(,(or 'Π 'Pi) ((,x ,A)) ,B)
     (go-on ([A-out (check Γ A (UNI))]
             [B-out (check (extend-ctx Γ x (val (ctx->env Γ) A-out)) B (UNI))])
       (go `(the U (Π ((,x ,A-out)) ,B-out))))]
    ['Trivial (go '(the U Trivial))]
    ['Absurd (go '(the U Absurd))]
    [`(ind-Absurd ,target ,motive)
     (go-on ([target-out (check Γ target (ABSURD))]
             [motive-out (check Γ motive (UNI))])
       (go `(the ,motive-out (ind-Absurd ,target-out ,motive-out))))]
    ['Atom (go '(the U Atom))]
    [`(,rator ,rand)
     (go-on ([`(the ,rator-t ,rator-out) (synth Γ rator)])
       (match (val (ctx->env Γ) rator-t)
         [(PI A B)
          (go-on ([rand-out (check Γ rand A)])
            (go `(the ,(read-back-norm Γ
                                       (THE (UNI)
                                            (val-of-closure B
                                                            (val (ctx->env Γ)
                                                                 rand-out))))
                      (,rator-out ,rand-out))))]
         [non-PI (stop rator
                       (format \"Expected a Π type, but this is a ~a\"
                               (read-back-norm Γ (THE (UNI) non-PI))))]))]
    [x #:when (var? x)
     (go-on ([t (lookup-type x Γ)])
       (go `(the ,(read-back-norm Γ (THE (UNI) t)) ,x)))]
    [none-of-the-above (stop e \"Can't synthesize a type\")]))"))
   (P (CodeB "(check Γ e t) → (perhaps/c expr?)
  Γ : context?
  e : expr?
  t : value?")
      "判定" (Code "e") "是否具有类型" (Code "t") ", 成功时返回精化后的表达式."
      (CodeB "(define (check Γ e t)
  (match e
    [`(cons ,a ,d)
     (match t
       [(SIGMA A D)
        (go-on ([a-out (check Γ a A)]
                [d-out (check Γ d (val-of-closure D (val (ctx->env Γ) a-out)))])
          (go `(cons ,a-out ,d-out)))]
       [non-SIGMA (stop e (format \"Expected Σ, got ~v\"
                                  (read-back-norm Γ (THE (UNI) non-SIGMA))))])]
    ['zero
     (match t
       [(NAT) (go 'zero)]
       [non-NAT (stop e (format \"Expected Nat, got ~v\"
                                (read-back-norm Γ (THE (UNI) non-NAT))))])]
    [`(add1 ,n)
     (match t
       [(NAT)
        (go-on ([n-out (check Γ n (NAT))])
          (go `(add1 ,n-out)))]
       [non-NAT (stop e (format \"Expected Nat, got ~v\"
                                (read-back-norm Γ (THE (UNI) non-NAT))))])]
    ['same
     (match t
       [(EQ A from to)
        (go-on ([_ (convert Γ A from to)])
          (go 'same))]
       [non-= (stop e (format \"Expected =, got ~v\"
                              (read-back-norm Γ (THE (UNI) non-=))))])]
    ['sole
     (match t
       [(TRIVIAL)
        (go 'sole)]
       [non-Trivial
        (stop e (format \"Expected Trivial, got ~v\"
                        (read-back-norm Γ (THE (UNI) non-Trivial))))])]
    [`(,(or 'λ 'lambda) (,x) ,b)
     (match t
       [(PI A B)
        (define x-val (NEU A (N-var x)))
        (go-on ([b-out (check (extend-ctx Γ x A) b (val-of-closure B x-val))])
          (go `(λ (,x) ,b-out)))]
       [non-PI (stop e (format \"Expected Π, got ~v\"
                               (read-back-norm Γ (THE (UNI) non-PI))))])]
    [`',a
     (match t
       [(ATOM)
        (go `',a)]
       [non-ATOM (stop e (format \"Expected Atom, got ~v\"
                                 (read-back-norm Γ (THE (UNI) non-ATOM))))])]
    [none-of-the-above
     (go-on ([`(the ,t-out ,e-out) (synth Γ e)]
             [_ (convert Γ (UNI) t (val (ctx->env Γ) t-out))])
       (go e-out))]))"))
   (P (CodeB "(convert Γ t v1 v2) → (perhaps/c 'ok)
  Γ : context?
  t : value?
  v1 : value?
  v2 : value?")
      "检查" (Code "v1") "和" (Code "v2") "是否表示同一个" (Code "t") -- "换言之, "
      "它们作为句法的表示是否" $alpha "等价."
      (CodeB "(define (convert Γ t v1 v2)
  (define e1 (read-back-norm Γ (THE t v1)))
  (define e2 (read-back-norm Γ (THE t v2)))
  (if (α-equiv? e1 e2)
      (go 'ok)
      (stop e1 (format \"Expected to be the same ~v as ~v\"
                       (read-back-norm Γ (THE (UNI) t))
                       e2))))"))
   (H4. "带定义的类型检查" #:id "type-checking-with-definitions")
   (P (CodeB "(interact Γ input) → (perhaps/c context?)
  Γ : context?
  input : (or/c (list/c 'define symbol? expression?)
      expression?)")
      "如果" (Code "input") "是一个表达式, 检查并规范化它. 如果" (Code "input")
      "是一个定义, 检查它并将其添加到上下文中."
      (CodeB "(define (interact Γ input)
  (match input
    [`(define ,x ,e)
     (if (assv x Γ)
         (stop x \"Already defined\")
         (go-on ([`(the ,ty ,expr) (synth Γ e)])
           (let ([ρ (ctx->env Γ)])
             (go (cons (cons x (def (val ρ ty) (val ρ expr)))
                       Γ)))))]
    [e
     (go-on ([`(the ,ty ,expr) (synth Γ e)])
       (let ([ρ (ctx->env Γ)])
         (begin
           (printf \"Type: ~v\\nNormal form:~v\\n\"
                   ty
                   (read-back-norm Γ
                                   (THE (val ρ ty)
                                        (val ρ expr))))
           (go Γ))))]))"))
   (P (CodeB "(run-program Γ input) → (perhaps/c context?)
  Γ : context?
  input : (listof (or/c (list/c 'define symbol? expression?)
              expression?))")
      "检查一系列定义和表达式."
      (CodeB "(define (run-program Γ inputs)
  (match inputs
    ['() (go Γ)]
    [(cons d rest)
     (go-on ([new-Γ (interact Γ d)])
       (run-program new-Γ rest))]))"))
   (P "例子:" (CodeB "> (void
   (run-program '()
                '(; What are the consequences of Nat equality?
                  (define nat=consequence
                    (the (Pi ((j Nat))
                           (Pi ((k Nat))
                             U))
                         (lambda (j)
                           (lambda (k)
                             (ind-Nat j
                                      (lambda (_) U)
                                      (ind-Nat k
                                               (lambda (_) U)
                                               Trivial
                                               (lambda (_)
                                                 (lambda (_)
                                                   Absurd)))
                                      (lambda (j-1)
                                        (lambda (_)
                                          (ind-Nat k
                                                   (lambda (_) U)
                                                   Absurd
                                                   (lambda (k-1)
                                                     (lambda (_)
                                                       (= Nat j-1 k-1)))))))))))
                  ; The consequences hold for Nats that are the same
                  (define nat=consequence-refl
                    (the (Pi ((n Nat))
                           ((nat=consequence n) n))
                         (lambda (n)
                           (ind-Nat n
                                    (lambda (k)
                                      ((nat=consequence k) k))
                                    sole
                                    (lambda (n-1)
                                      (lambda (_)
                                        same))))))
                  (nat=consequence-refl zero)
                  (nat=consequence-refl (add1 (add1 zero)))
                  ; The consequences hold for all equal Nats
                  (define there-are-consequences
                    (the (Pi ((j Nat))
                           (Pi ((k Nat))
                             (Pi ((j=k (= Nat j k)))
                               ((nat=consequence j) k))))
                         (lambda (j)
                           (lambda (k)
                             (lambda (j=k)
                               (replace j=k
                                        (lambda (n)
                                          ((nat=consequence j) n))
                                        (nat=consequence-refl j)))))))
                  ((there-are-consequences zero) zero)
                  (((there-are-consequences zero) zero) same)
                  ((there-are-consequences (add1 zero)) (add1 zero))
                  (((there-are-consequences (add1 zero)) (add1 zero)) same)
                  ((there-are-consequences zero) (add1 zero))
                  ((there-are-consequences (add1 zero)) zero))))
Type: 'Trivial
Normal form:'sole
Type: '(= Nat (add1 zero) (add1 zero))
Normal form:'same
Type: '(Π ((j=k (= Nat zero zero))) Trivial)
Normal form:'(λ (j=k) sole)
Type: 'Trivial
Normal form:'sole
Type: '(Π ((j=k (= Nat (add1 zero) (add1 zero)))) (= Nat zero zero))
Normal form:'(λ (j=k)
               (replace
                j=k
                (λ (x)
                  (ind-Nat
                   x
                   (λ (k) U)
                   Absurd
                   (λ (n-1) (λ (ih) (= Nat zero n-1)))))
                same))
Type: '(= Nat zero zero)
Normal form:'same
Type: '(Π ((j=k (= Nat zero (add1 zero)))) Absurd)
Normal form:'(λ (j=k)
               (the
                Absurd
                (replace
                 j=k
                 (λ (x)
                   (ind-Nat x (λ (k) U) Trivial (λ (n-1) (λ (ih) Absurd))))
                 sole)))
Type: '(Π ((j=k (= Nat (add1 zero) zero))) Absurd)
Normal form:'(λ (j=k)
               (the
                Absurd
                (replace
                 j=k
                 (λ (x)
                   (ind-Nat
                    x
                    (λ (k) U)
                    Absurd
                    (λ (n-1) (λ (ih) (= Nat zero n-1)))))
                 same)))"))
   (H3. "项目" #:id "projects")
   (P "这个Pie的小子集可以扩展出许多特性. 这里有几个想法, 帮你开个头:"
      (Ul (Li "添加一个和类型" (Code "Either") ", 其构造子为" (Code "left") "和" (Code "right") ".")
          (Li "添加列表和向量 (以长度为索引的列表).")
          (Li "为类型检查器添加非依赖的函数类型和非依赖的序对类型. 这应该不需要修改求值器.")
          (Li "添加接受多个参数的函数, 但将它们精化为单参数的Curry化函数.")
          (Li "添加洞和/或具名的元变量, 以允许增量地构造程序/证明.")
          (Li "使求值策略成为惰性的, 可以是按名调用, 更好的是按需调用.")
          (Li "用无穷多个宇宙和一个累积性关系替换" :U
              ". 为此, 类型相等检查应当被替换为归入检查, 其中每个类型构造子都有类似于其他带子类型的系统的变型规则.")))
   (H3. "融会贯通" #:id "putting-it-together")
   (P "检查依赖类型需要回答两个问题:"
      (Ul (Li "如何检查表达式的相等性?")
          (Li "何时检查表达式的相等性?"))
      "在本教程中, 第一个问题是用基于求值的规范化来回答的, 第二个问题则用双向类型检查来回答. "
      "在类型检查期间使用精化, 以输出一个适合求值的简化的核心语言. 这些并不是仅有的可能的答案.")
   (P "相等性可以增量地检查, 这样在两个表达式不相等的情况下不需要规范化整个表达式; 或者可以使用诸如"
      (Em "遗传代换") " " (Bcite "Watkins02") "这样的技术, 使得" (Em "只会")
      "出现表达式的规范形式. 对检查相等性判断可以作的另一个改变是, 成功时不返回一个平凡的值, "
      "而是返回使该判断成立的条件. 例如, 如果表达式可以包含元变量, "
      "这些元变量代表程序中被省略的, 程序员期望能自动推断出来的部分, "
      "那么相等性检查可以输出一组关于这些元变量的约束, 交由一个单独的遍来求解. 这是Agda所采用的方法 "
      (Bcite "Norell07") ".")
   (P "除了双向的方法之外, 也可以采用只有综合的方法, 其中每个表达式都包含足够的信息来重建其类型, "
      "所以检查只有最后那个兜底的情形. 这种方法常常作为精化类型检查器之后的第二步, 以确保精化没有出错. "
      "当精化器包含许多步骤, 产生与高层语言相当不同的核心语言输出时, 这特别有用, 例如在Idris中 "
      (Bcite "Brady13") ".")
   (H2. "延伸阅读" #:id "further-reading")
   (H3. "实现类型论的教程" #:id "tutorials")
   (P "Andres Löh, Conor McBride和Wouter Swierstra " (Bcite "Löh10")
      "写了一个在Haskell中使用双向类型检查实现依赖类型的教程, 而"
      (Href "https://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html"
            "Lennart Augustsson")
      "写了一篇回应, 使用了更简单的语言特性.")
   (P "Stephanie Weirich在"
      (Href "https://www.cs.uoregon.edu/research/summerschool/summer13/curriculum.html"
            "Oregon编程语言暑期学校")
      "讲授了一门关于实现依赖类型的课程, 其中包括一个双向类型检查器. "
      "如果你善于从视频讲座中学习, 那么这门课值得一看. 她在"
      (Href "https://github.com/sweirich/pi-forall" "GitHub上")
      "维护着这个实现.")
   (H3. "双向类型检查" #:id "bidi-bib")
   (P "Pierce和Turner " (Bcite "Pierce98")
      "向世界介绍了双向类型检查; 然而, 他们指出这一想法是编译器作者之间早已存在的未发表的民间知识. "
      "{译注: " (Ref "other-approaches") "提到, Coquand " (Bcite "Coquand96") "在此两年前就描述了一个双向类型检查器.} "
      "在这篇论文之后, 双向定型被应用于许多问题; 截至2013年的一个很好的综述, 参见Dunfield和Krishnaswami "
      (Bcite "Dunfield13") "的引言.")
   (P "此外, " (Href "https://people.mpi-sws.org/~joshua/bitype.pdf" "Joshua Dunfield") "和"
      (Href "https://www.cs.cmu.edu/~fp/courses/15312-f04/handouts/15-bidirectional.pdf" "Frank Pfenning")
      "都写过很好的双向类型检查导引; 几年前"
      (Href "http://www.davidchristiansen.dk/tutorials/bidirectional.pdf" "我也写过一篇")
      ". 前面提到的Stephanie Weirich在Oregon编程语言暑期学校的课程是另一个很好的导引.")
   (H3. "基于求值的规范化" #:id "nbe-bib")
   (P "基于求值的规范化是由Berger和Schwichtenberg " (Bcite "Berger91")
      "发明的, 用来高效地实现简单类型的规范化, 其方法是重用Scheme函数作为简单类型" $lambda "演算中函数的语义. "
      "Olivier Danvy " (Bcite "Danvy96") "将他们的方案扩展到了诸如递归, 和, 以及效应这样的特性, "
      "并将所得的推广后的系统称为" (Q "类型导向的部分求值") ".")
   (P "James Chapman, Thorsten Altenkirch和Conor McBride写了一个类型论的实现, "
      "使用了类型导向的 " (Bcite "Chapman06") "从值到句法的读回, 与这里类似. "
      "他们的系统在编写时考虑了效率, 小心地利用了Haskell的惰性来把句法和语义维持在一起, "
      "只计算绝对必要的那么多.")
   (P "Andreas Abel的特许任教资格 (habilitation) 论文 " (Bcite "Abel13")
      "是对依赖类型论的各种变体上的基于求值的规范化的一长串研究的极好综述, "
      "使用了一致的记号和解释. 这篇论文详尽的参考文献引用了Abel及其合作者的许多其他论文, 我在这里就不再重复了.")
   (H3. "其他方法" #:id "other-approaches")
   (P "有类型的基于求值的规范化远不是为依赖类型实现转换检查的唯一方式. "
      "事实上, 基于求值的规范化有许多特点, 使得它只适用于某些理论: 它尽可能多地对表达式进行" $eta "展开, "
      "但是" $eta "展开对于某些理论 (包括Coq的构造演算) 并不有效, 而且如果保留下来还可能消耗大量内存"
      "{译注: Coq自8.4版 (2012年) 起已在转换检查中支持函数类型的" $eta "规则, 但诸如单位类型这样的归纳类型仍然没有" $eta "规则.}"
      "; "
      "此外, 它会完全规范化项, 而本来可能通过一次直接的" $alpha "等价检查就能判定它们是相同的. "
      "因为使用高阶闭包的NbE重用了实现语言的函数, 所以在一个严格的实现语言中实现惰性需要额外的工作.")
   (P "Thierry Coquand " (Bcite "Coquand96") "描述了一个双向类型检查器 (比Pierce和Turner "
      (Bcite "Pierce98") "早两年), 它使用一种抽象机来实现转换检查. "
      "这个机器维护一个环境和一个指向表达式的游标, 并增量地归约游标下的表达式, 直到失败或者检查完整个表达式. "
      "这种增量的方法支持利用部分的" $alpha "等价, 但是如果想要" $eta "等价, 就需要另一种解决方案.")
   (P "Benjamin Grégoire和Xavier Leroy " (Bcite "Grégoire02")
      "实现了一个从Coq到OCaml的ZAM机器的一个版本的编译器, 极大地提高了Coq的效率. "
      "Coq之所以能在很大程度上适用于大型问题, 正是他们工作的直接结果.")
   (P "Dirk Kleeblatt的博士论文 " (Bcite "Kleeblatt11")
      "描述了类型论的一个实现, 其中表达式被直接编译为机器码, 使用的是GHC中所用的STG机器的一个强规范化变体. "
      "因为类型论通常以惰性的运行时语义来呈现, 所以这是一种高效的实现.")
   (H3. "参考文献" #:auto? #f)
   (Ul (Bib "Abel13" "Andreas Abel, "
            (Q "Normalization by Evaluation: Dependent Types and Impredicativity")
            ", Habilitation Thesis, Institut für Informatik, Ludwig-Maximilians-Universität München, 2013. "
            "{译注: 原文此处作者名写作Andres Abel, 应为笔误.}")
       (Bib "Berger91" "Ulrich Berger and Helmut Schwichtenberg, "
            (Q "An inverse to the evaluation functional for typed λ-calculus")
            ", Logic in Computer Science, 1991.")
       (Bib "Brady13" "Edwin Brady, "
            (Q "Idris, a General Purpose Dependently Typed Programming Language: Design and Implementation")
            ", Journal of Functional Programming 23 (5), pp. 552–593, 2013.")
       (Bib "Chapman06" "James Chapman, Thorsten Altenkirch, and Conor McBride, "
            (Q "Epigram reloaded: a standalone typechecker for ETT")
            ", Post-Proceedings of Trends in Functional Programming, 2005. "
            "{译注: 文献键名为Chapman06而年份为2005, 因为该文收录于TFP 2005的会后论文集, 后者于2006年出版.}")
       (Bib "Coquand96" "Thierry Coquand, "
            (Q "An algorithm for type-checking dependent types")
            ", Science of Computer Programming 26, pp. 167–177, 1996.")
       (Bib "Danvy96" "Olivier Danvy, "
            (Q "Type-Directed Partial Evaluation")
            ", Symposium on Principles of Programming Languages, 1996.")
       (Bib "Dunfield13" "Joshua Dunfield and Neelakantan Krishnaswami, "
            (Q "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism")
            ", International Conference on Functional Programming, 2013.")
       (Bib "Friedman18" "Daniel P. Friedman and David Thrane Christiansen, "
            (I "The Little Typer") ", MIT Press, 2018. "
            (Href "http://thelittletyper.com" "http://thelittletyper.com"))
       (Bib "Grégoire02" "Benjamin Grégoire and Xavier Leroy, "
            (Q "A compiled implementation of strong reduction")
            ", International Conference on Functional Programming, 2002.")
       (Bib "Kleeblatt11" "Dirk Kleeblatt, "
            (Q "On a Strongly Normalizing STG Machine With an Application to Dependent Type Checking")
            ", PhD Thesis, Technical University of Berlin, 2011.")
       (Bib "Landin64" "Peter Landin, "
            (Q "The mechanical evaluation of expressions")
            ", Computer Journal 6, pp. 308–320, 1964.")
       (Bib "Löh10" "Andres Löh, Conor McBride, Wouter Swierstra, "
            (Q "A tutorial implementation of a dependently typed lambda calculus")
            ", Fundamenta Informaticae (102) 2, pp. 177-207, 2010.")
       (Bib "Norell07" "Ulf Norell, "
            (Q "Towards a practical programming language based on dependent type theory")
            ", PhD thesis, Chalmers University of Technology and Göteborg University, 2007.")
       (Bib "Pierce98" "Benjamin C. Pierce and David N. Turner, "
            (Q "Local Type Inference")
            ", Symposium on Principles of Programming Languages, 1998.")
       (Bib "Watkins02" "Kevin Watkins, Iliano Cervesato, Frank Pfenning, and David Walker, "
            (Q "A concurrent logical framework I: Judgments and properties")
            ", Technical Report CMU-CS-02-101, Carnegie Mellon University, "
            "School of Computer Science, 2002, revised May 2003."))
   (H2. "致谢" #:id "acknowledgments")
   (P "我要感谢Dan Friedman对本教程草稿提出的建设性意见, 感谢Sam Tobin-Hochstadt"
      "在开发文学化编程库" (Code "brush") "时提供的关键技术咨询, 感谢Rutvik Patel, Siyuan Chen和Xie Yuheng"
      "发现了许多bug, 尤其要感谢Andreas Abel对基于求值的规范化的清晰描述. "
      "当然, bug, 糟糕的解释和错误都是我自己的.")
   ))
