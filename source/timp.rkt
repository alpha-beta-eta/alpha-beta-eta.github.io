#lang racket
(provide timp.html)
(require SMathML)
(define (&label x . t*)
  (Table #:attr* '((align "center"))
         (Tr (Td #:attr* '((align "center")) x))
         (Tr (keyword-apply
              Td '(#:attr*) '(((align "center")))
              t*))))
(define body-id
  (Mi "body" #:attr* '((mathvariant "italic"))))
(define (idlize x)
  (Mi x #:attr* '((mathvariant "italic"))))
(define var-id (idlize "var"))
(define val-id (idlize "val"))
(define exp-id (idlize "exp"))
(define init-id (idlize "init"))
(define x-id (idlize "x"))
(define obj-id (idlize "obj"))
(define vars-id (idlize "vars"))
(define then-id (idlize "then"))
(define else-id (idlize "else"))
(define s-id (idlize "s"))
(define a-id (idlize "a"))
(define e-id (idlize "e"))
(define r-id (idlize "r"))
(define ret-id (idlize "ret"))
(define v-id (idlize "v"))
(define next-id (idlize "next"))
(define fcn-id (idlize "fcn"))
(define arg-id (idlize "arg"))
(define (:Exp e)
  (cond ((symbol? e) (list (idlize (symbol->string e))))
        ((pair? e) (apply append
                          (add-between
                           (map :Exp (cdr e)) '((" "))
                           #:before-first
                           `(("(" ,(symbol->string (car e)) " "))
                           #:after-last '((")"))
                           #:splice? #t)))
        (else (list e))))
(define (:Code e)
  (apply Code (:Exp e)))
(define (:CodeB e)
  (apply CodeB (:Exp e)))
(define timp.html
  (TnTmPrelude
   #:title "Scheme的三种实现模型"
   #:css "styles.css"
   (H1 "Scheme的三种实现模型")
   (P "注记: 本文是1987年发表的, 所以其中的Scheme语言与现在相比或多或少有些区别. "
      "例如, 空表也被视为假, 而不是R" (Sup "5") "RS及之后的真.")
   (H2 "第1章 引论")
   (H3 "第1.1节 函数式编程语言")
   (H3 "第1.2节 函数式编程语言实现")
   (H3 "第1.3节 多处理器系统和实现")
   (H2 "第2章 Scheme语言")
   (H3 "第2.1节 句法形式和原始函数")
   (H4 "第2.1.1小节 核心句法形式")
   (P (CodeB "&lt;core> -> &lt;object>
&lt;core> -> &lt;variable>
&lt;core> -> (quote &lt;object>)
&lt;core> -> (lambda (&lt;variable> ...) &lt;core>)
&lt;core> -> (if &lt;core> &lt;core> &lt;core>)
&lt;core> -> (set! &lt;variable> &lt;core>)
&lt;core> -> (call/cc &lt;core>)
&lt;core> -> (&lt;core> &lt;core> ...)")
      "图2.1 Scheme核心语言的句法")
   (H4 "第2.1.2小节 原始函数")
   
   (H4 "第2.1.3小节 句法扩展")
   (CodeB "(let (["var-id" "val-id"] ...) "exp-id " ...) "$->"
((lambda ("var-id " ...) "exp-id" ...) "val-id" ...)")
   (CodeB "(rec "var-id" "exp-id") "$->"
(let (["var-id" '()])
  (set! "var-id" "exp-id"))")
   (P "注记: 从时代背景来说, 当时已有R"(Sup "2")"RS, 其中" (Code "rec")
      "作为特殊形式出现. 另外, " (Code "(set! "var-id" "exp-id")")
      "的返回值在所有Scheme标准中都是unspecified, 但是这里的实现的决定是"
      "返回RHS的值, 目前Kent Dybvig的Chez Scheme的决定是返回" (Code "(void)")
      "的值.")
   (CodeB "(recur f (["var-id" "init-id" ...) "exp-id" ...) "$->"
((rec f (lambda ("var-id" ...) "exp-id" ...)) "init-id" ...)")
   (P "注记: " (Code "recur") "其实就相当于named let.")
   (CodeB "(and "(_ exp-id $1)") "$->" "(_ exp-id $1)"
(and "(_ exp-id $1)" "(_ exp-id $2)" ...) "$->"
(if "(_ exp-id $1)" (and "(_ exp-id $2)" ...) '())")
   (CodeB "(or "(_ exp-id $1)") "$->" "(_ exp-id $1)"
(or "(_ exp-id $1)" "(_ exp-id $2)" ...) "$->"
(if "(_ exp-id $1)" 't (or "(_ exp-id $2)" ...))")
   (P "注记: 实际上这里不论" (Code "and") "还是" (Code "or")
      "都与现代Scheme稍有不同. 一个是现在" (Code "(and)")
      "和" (Code "(or)") "也有定义, 另外就是对于"
      (Code "(or "(_ exp-id $1)" "(_ exp-id $2)" ...)")
      "而言, 如果" (Code (_ exp-id $1)) "为真, 那么应该返回"
      (Code (_ exp-id $1)) "的值. 当然, 读者应该还看出了一点不同, "
      "就是当时的Scheme用" (Code "'t") "来表示真, 而不是特殊的布尔类型, "
      "这与其他许多Lisp方言是类似的.")
   (CodeB "(record ("var-id" ...) "val-id" "exp-id" ...) "$->"
(apply (lambda ("var-id" ...) "exp-id" ...) "val-id")")
   (H3 "第2.2节 闭包")
   (H3 "第2.3节 赋值")
   (H3 "第2.4节 延续")
   (H3 "第2.5节 一个元循环解释器")
   (P "本节呈现了一个Scheme的元循环解释器. 一个Scheme元循环解释器是用Scheme写成的"
      "Scheme解释器. 运行解释器的Scheme系统相对于由解释器实现的Scheme系统而言位于"
      "元层次. 显然, 元循环解释器中可以再运行一个解释器, 那么原本的元层次就变成了"
      "元元层次. 从理论上来说, 这个过程可以无限地 (indefinitely) 被执行下去, "
      "提供一个解释器的无穷之塔 [Smi82].")
   (P ""
      )
   (CodeB "(define meta
  (lambda (exp)
    (exec exp '())))")
   (CodeB "(define exec
  (lambda (exp env)
    (cond
      [(symbol? exp) (car (lookup exp env))]
      [(pair? exp)
       (record-case exp
         [quote (obj) obj]
         [lambda (vars body)
          (lambda (vals)
            (exec body (extend env vars vals)))]
         [if (test then else)
          (if (exec test env)
              (exec then env)
              (exec else env))]
         [set! (var val)
          (set-car! (lookup var env) (exec val env))]
         [call/cc (exp)
          (call/cc
            (lambda (k)
              ((exec exp env)
               (list (lambda (args) (k (car args)))))))]
         [else
          ((exec (car exp) env)
           (map (lambda (x) (exec x env)) (cdr exp)))])]
      [else exp])))")
   (P "注记: 以上的解释器主体原本多出一行"
      (Code "[call/cc (exp) (call/cc (exec exp env))]")
      ", 实际上并不发挥任何作用, 而且是错误的. 愚猜测是Kent Dybvig"
      "最初采取了错误的写法, 之后又纠正过来, 但是忘了把原本的错误删掉 "
      "(抑或是有意保留错误给读者看).")
   (CodeB "(define lookup
  (lambda (var e)
    (let nxtrib ([e e])
      (let nxtelt ([vars (caar e)] [vals (cdar e)])
        (cond
          [(null? vars) (nxtrib (cdr e))]
          [(eq? (car vars) var) vals]
          [else (nxtelt (cdr vars) (cdr vals))])))))")
   (P "注记: 愚将以上的两个" (Code "recur") "改成了" (Code "let")
      ", 因为虽然它们是等价的, 但几乎没有Scheme实现提供"
      (Code "recur") ", 即便是Kent Dybvig本人的Chez Scheme.")
   (CodeB "(define extend
  (lambda (env vars vals)
    (cons (cons vars vals) env)))")
   (H2 "第3章 基于堆的模型")
   (H3 "第3.1节 动机和问题")
   (H3 "第3.2节 数据结构的表示")
   (H4 "第3.2.1小节 环境")
   (P "环境是由序对构成的, 其结构就像肋骨笼. 一个环境是一个列表, 其每个元素都是"
      "列表的序对. 这个序对由变量的肋骨和相应的值的肋骨构成. 让我们考虑一个简单的例子:"
      (CodeB "((lambda (a b)
   ((lambda (c)
      ((lambda (d e f) " body-id ") 3 4 5))
    2))
 0 1)")
      "假设环境为空, 对于" (Code body-id) "求值时, 环境的结构为:"
      (CodeB "(((d e f) . (3 4 5))
 ((c) . (2))
 ((a b) . (0 1)))"))
   (P "环境的实现见第2.5节.")
   (H4 "第3.2.2小节 帧与控制栈")
   (P ""
      )
   (H3 "第3.3节 实现策略")
   (P "本节的实现策略使用五个寄存器 [注记: 这里所谓的寄存器, 是抽象的寄存器]:"
      (Ul (Li (Code a-id) ": 累积器")
          (Li (Code x-id) ": 下一个表达式")
          (Li (Code e-id) ": 当前环境")
          (Li (Code r-id) ": 当前值肋骨")
          (Li (Code s-id) ": 当前栈")))
   (P "以下是这些寄存器用途的详细描述. [注记: 虽然作者已经尽量小心, 但是以下的描述不可避免地"
      "杂糅了两个层次的表达式, 即Scheme源层次和第3.4.1小节的汇编代码.]")
   (P (B "累积器") "存放了值返回 (value-returning) 操作 (例如加载一个常量或者引用一个变量) "
      "的结果. 在函数应用期间, 其依次存放了每个参数的值, 在这些值被保存到值肋骨上之前. "
      "当然, 函数值在应用之前也会存放在累积器里. 若对于" (Code "if")
      "表达式求值, 那么寄存器存放了要被测试真假的表达式的值, 以此来决定对于哪一个子表达式"
      "进行求值. 当整个计算结束时, 也就是遇到" (Code "(halt)") " (见3.4.1小节) "
      "的时候, 累积器存放的是整个计算的值.")
   (P (B "下一个表达式") "刻画了要被求值的下一个表达式, 例如加载一个常量, 创建一个闭包, "
      "应用一个闭包, 等等. 实际上, 这里的表达式几乎与Scheme源表达式是一样的, "
      "只是其被编译以使得求值更加高效而已.")
   (P "原注: 实际上, 避免编译步骤而直接使用一个源层次的解释器 (寄存器与这里给定的相同) "
      "也是很合理的. 基于堆的解释器的解释开销与分配帧和环境以及变量引用的开销相比并不那么高. "
      "这里使用编译器出于两个原因, 一个是第3.5节的修改不论如何都需要一个预处理的步骤, 另外一个"
      "原因在于下一章所呈现的基于栈的模型中, 编译是更重要的, 所以说为了一致性, 我们将"
      "使用编译策略贯穿始终. [注记: 尽管解释器所使用的寄存器相同, 寄存器的意图也大致相同, "
      "但具体形式会有所不同. 在某种意义上, 这是解释开销的一部分.]")
   (P (B "当前环境") "存放着活跃的词法绑定. 在应用闭包时, 新的环境从闭包保存的环境和实际参数"
      "中建立起来. 变量引用, 变量赋值, 以及" (Code "lambda") "表达式 (即创建闭包) 都需要"
      "用到当前环境. 因为环境会被函数应用破坏, 所以在应用之前当前环境会被保存在一个调用帧里, "
      "而从应用返回时则会恢复.")
   (P "在对于应用求值期间, " (B "当前值肋骨") "是一个列表, 存放着已经求值的参数的值. "
      "对于参数表达式而言, 当其计算结束时, 其值被存放在累积器里, 随即通过" (Code "cons")
      "添加到当前值肋骨上. 一旦所有的参数值和闭包值都已经计算完毕, 那么当前值肋骨会和闭包的"
      "环境联合起来以产生新的当前环境. 因为当前值肋骨也会被应用的求值所破坏, 所以在"
      "应用发生之前它也和环境一起被保存到调用帧里.")
   (P "最后, " (B "当前栈") "存放着最顶部的调用帧. 在应用开始之前, 调用帧被添加到栈上, "
      "而从闭包返回时, 则被立即移除. 一个调用帧由一个保存的环境, 一个保存的值肋骨, "
      "一个与返回地址相关的保存的表达式以及一个指向先前调用帧的链接构成. 当一个调用帧从"
      "当前栈上被移除时, 这些被保存下来的值将用来恢复当前环境, 当前值肋骨, 以及下一个表达式"
      "这些寄存器. 当前栈本身可以在任意时刻通过对于" (Code "call/cc") "表达式求值被保存到"
      "一个延续对象里. [注记: 一个延续对象被实现为一个闭包.]")
   (P "在读完以上描述后, 如何对于Scheme表达式进行求值在大体上已经是很显然的事情了. "
      "当然, 一些细节, 例如该如何对于应用进行求值, 尚不明确. 下面我们将给出求值策略的细节, "
      "特别是其将如何影响这些寄存器.")
   (P "一个变量引用将累积器变为在当前环境中找到的变量的值. (当然, 下一个表达式"
      (Code x-id) "也需要改变. 除非另有情况, 否则省略这项说明.)")
   (P "常量和" (Code "quote") "表达式的处理是一样的, 都是将某个特定的对象加载到累积器里.")
   (P (Code "lambda") "表达式将导致闭包的创建, 这个闭包将被放入累积器中.")
   (P "对于" (Code "if") "表达式的求值在逻辑上分为两个步骤. 首先, 编译器生成的合适代码会将"
      "测试表达式的结果置于累积器中, 然后" (Code "if") "操作将根据累积器的值选择接下来"
      "对于哪一个分支进行求值, 即将下一个表达式置为哪一个分支.")
   (P "一个" (Code "set!") "破坏性地改变了当前环境的结构, 这当然是为了改变被赋值的变量的值. 和"
      (Code "if") "一样, 编译器都需要设法将所需要的值在操作进行之前安排到累积器里.")
   (P "对于" (Code "call/cc") "表达式的求值将会导致用于保存当前环境, 当前值肋骨和要返回至的"
      "表达式的新调用帧的创建. 接着这个新栈将被捕获进一个延续对象里, 然后这个延续对象将被"
      "添加到当前值肋骨上 (如果一切正常, 此时当前值肋骨应该是一个空表). 下一个表达式将被更新为"
      "先对于一个函数表达式求值, 然后将得到的闭包应用于当前值肋骨的表达式 (此时值肋骨里仅有一个"
      "延续对象而已). 当这个延续在之后被调用时, 保存的栈得以恢复, 顶部的帧被移除, 而这个延续的"
      "实际参数的值被放在累积器中.")
   (P "对于一个应用进行求值将分为多个步骤进行. 第一步在于创建一个新的栈帧用于保存当前环境, "
      "当前值肋骨, 以及应用所要返回至的表达式. 在此步骤之中, 当前值肋骨也被重新初始化为空表. "
      "接着, 每个参数依次被求值, 并且它们的值将被添加到当前值肋骨上. 函数表达式也将被求值, 而其值"
      "将留在累积器里. 最终, 累积器里的闭包将被应用于当前值肋骨里的各参数值. 闭包保存的环境, "
      "闭包的形式参数和当前值肋骨将联合起来构成一个新的环境, 置于当前环境寄存器中. 闭包的体将置于"
      "下一个表达式寄存器里. 当闭包返回时, 顶部的栈帧将被移除, 保存的值将被恢复. 当然, 返回值将"
      "置于累积器里, 这是不动的. (不然的话, 不就白算了吗.)")
   (P "在对于" (Code "call/cc") "和应用表达式进行求值时, 有一点值得特别注意. 为了优化尾调用, "
      "即为了使得尾调用不去build up控制栈, 新的调用帧是不会添加到栈上的. 添加调用帧的目的在于"
      "保存环境, 值肋骨以及调用之后所需要返回至的表达式, 但是在尾调用的情况下, 调用之后所需要"
      "做的事情不过就是立即返回, 恢复接下来的那些值, 所以添加调用帧是多余的.")
   (H3 "第3.4节 实现基于堆的模型")
   (P "本章和之后的两章, 我们将刻画每个模型以一个完整的编译器和一个执行被编译的代码或者(第5章里)"
      "由编译器产生的低层次语言的语义描述的虚拟机器 (virtual machine, VM). 本章和第4章中, "
      "每个编译器都将输入的Scheme表达式转换为相应虚拟机器的&quot;汇编代码&quot;. 这个汇编代码"
      "并不具有读者一般所期望的线性形式, 而是有向无环图, 不需要标签和跳转亦可处理. 将这种形式"
      "转换为更加传统的汇编语言是简单的事情. 或者, 虚拟机器的汇编代码可以采用字节码的形式, "
      "用于更加紧凑和快速的虚拟机器, 甚至是用于虚拟机器的硬件或者微码实现.")
   (P "本节的编译器执行相当简单的变换, 而虚拟机器本身也是相当简单的. 之后的编译器和虚拟机将会"
      "变得更加复杂.")
   (H4 "第3.4.1小节 汇编代码")
   (P (Code "(halt)") "停下虚拟机器, 而累积器之中的值就是计算的结果.")
   (P (Code "(refer "var-id" "x-id")") "找出变量" (Code var-id)
      "在当前环境中的值, 将其置于累积器之中, 将下一个表达式置为" (Code x-id) ".")
   (P (Code "(constant "obj-id" "x-id")") "将" (Code obj-id)
      "置于累积器之中, 将下一个表达式置为" (Code x-id) ".")
   (P (Code "(close "vars-id" "body-id" "x-id")") "根据" (Code body-id)
      ", " (Code vars-id) "和当前环境创建闭包, 将其置于累积器之中, 并"
      "将下一个表达式置为" (Code x-id) ".")
   (P (Code "(test " then-id " " else-id ")") "测试累积器的值是否是为真, 是则"
      "将下一个表达式置为" (Code then-id) ", 否则将下一个表达式置为"
      (Code else-id) ". 注意一下, 当时空表被视为假, 不是空表的值均被视为真.")
   (P (Code "(assign " var-id " " x-id ")") "将当前环境中变量" (Code var-id)
      "绑定至的值变为累积器中的值, 并将下一个表达式置为" (Code x-id) ".")
   (P (Code "(conti " x-id ")") "根据当前的栈创建一个延续, 将其置于累积器中, "
      "并将下一个表达式置为" (Code x-id) ".")
   (P (Code "(nuate " s-id " " var-id ")") "将" (Code s-id)
      "恢复为当前栈, 将累积器置为" (Code var-id) "在当前环境中的值, 并将下一个表达式置为"
      (Code "(return)") ".")
   (P (Code "(frame " ret-id " " x-id ")") "根据当前环境, 当前值肋骨和" (Code ret-id)
      "创建一个新帧, 将其加入到当前的栈上, 置当前的值肋骨为空表, 置下一个表达式为"
      (Code x-id) ".")
   (P "注记: 原文是" (Code "(frame " x-id " " ret-id ")")
      ", 但是结合后文来看, 可能原本Kent Dybvig采用的是" (:Code '(frame x ret))
      "的形式, 后来改成了" (:Code '(frame ret x)) ", 但是有些地方又忘了修改, "
      "导致描述和实际代码不太一致.")
   (P (Code "(argument " x-id ")") "将累积器中的值加入到当前值肋骨中, 接着置下一个表达式为"
      (Code x-id) ".")
   (P (Code "(apply)") "应用累积器中的闭包于当前的值肋骨. 精确地说, 这个指令扩展闭包的环境以"
      "闭包的变量列表和当前值肋骨, 将当前环境设置为这个新的环境, 将当前值肋骨设置为空表, "
      "将下一个表达式设置为闭包的体.")
   (P (Code "(return)") "从栈中去除第一个帧, 然后重新设置当前环境, 当前值肋骨, "
      "下一个表达式, 以及当前栈.")
   (H4 "第3.4.2小节 转换")
   (P "这个编译器将Scheme表达式转换为上列汇编语言指令. 一些Scheme表达式, 例如变量和常量, "
      "被转换为单条指令, 而其他一些表达式, 例如应用, 将被转换为多条指令.")
   (P "这个编译器检视表达式的类型, 然后将其转换为相应的指令. 编译器的输入是要编译的表达式以及"
      "表达式完成之后所要执行的指令. 这个" (Code "next") "指令或可以想成是表达式的延续 "
      "(不要将其与" (Code "call/cc") "返回的延续对象混淆).")
   (P "以下是编译器的代码. 所用Scheme的句法形式, 参见第2章.")
   (CodeB "(define compile
  (lambda (x next)
    (cond
      [(symbol? x)
       (list 'refer x next)]
      [(pair? x)
       (record-case x
         [quote (obj)
          (list 'constant obj next)]
         [lambda (vars body)
          (list 'close vars (compile body '(return)) next)]
         [if (test then else)
          (let ([thenc (compile then next)]
                [elsec (compile else next)])
            (compile test (list 'test thenc elsec)))]
         [set! (var x)
          (compile x (list 'assign var next))]
         [call/cc (x)
          (let ([c (list 'conti
                         (list 'argument
                               (compile x '(apply))))])
            (if (tail? next)
                c
                (list 'frame next c)))]
         [else
          (recur loop ([args (cdr x)]
                       [c (compile (car x) '(apply))])
            (if (null? args)
                (if (tail? next)
                    c
                    (list 'frame next c))
                (loop (cdr args)
                      (compile (car args)
                               (list 'argument c)))))])]
      [else
       (list 'constant x next)])))")
   (P "这个编译器没有错误检查, 全文的编译器和虚拟机皆是如此. 这是为了缩短代码, 简化呈现.")
   (P "注记: 以下内容与前文在本质上是相当重复的.")
   (P "对于变量 (句法上即符号), " (Code "quote") "表达式, 常量表达式 (即" (Code "cond")
      "表达式的" (Code "else") "子句所刻画的) 的变换都是直接的. 一个变量" (Code v-id)
      "以" (Code next-id) "为接下来的指令的话, 将被转换为" (:Code '(refer v next)) ". 类似地, "
      (:Code '(quote obj)) "和简单的" (Code obj-id) "都将被转换为" (:Code '(constant obj next)) ".")
   (P "对于" (Code "lambda") "表达式的变换也是直接的. 具有形式" (:Code '(lambda vars body))
      "的表达式以" (Code next-id) "为接下来的指令的话, 将被转换为"
      (:Code '(close vars cbody next)) ", 其中" (:Code 'cbody) "是编译" (Code body-id)
      "的结果. 在编译" (Code body-id) "时所使用的" (Code next-id) "参数是一条"
      (Code "(return)") "指令.")
   (P (Code "if") "和" (Code "set!") "表达式都需要先对于其某个子表达式进行求值, 之后才能执行表达式真正的"
      "工作. 这就是编译器的" (Code next-id) "参数有用的地方. 对于具有形式" (:Code '(if test then else))
      "的" (Code "if") "表达式, 编译" (:Code 'test) "子表达式的" (Code next-id) "参数是由编译过了的"
      (:Code 'then) "和" (:Code 'else) "的子表达式构造的" (:Code '(test cthen celse))
      ". 此即编译" (Code "if") "表达式的结果. 读者应该注意到这样一个事实, 将编译" (Code "if")
      "表达式的" (Code next-id) "参数作为编译" (:Code 'then) "和" (:Code 'else)
      "子表达式的" (Code next-id) "参数, 编译器创建了一个图结构, 这是如何避免使用标签和跳转的方法. "
      "[注记: 但是, 如果直接输出的话, 编译的结果可能会有相当的重复或者说膨胀, 在可读性上比较差.]")
   (P "对于" (Code "set!") "表达式的处理当然与" (Code "if") "类似. " (:Code '(set! var x))
      "被转换至以" (:Code '(assign var next)) "为接下来的指令来编译" (Code x-id)
      "的结果, 其中" (Code next-id) "即编译" (Code "set!") "表达式时的接下来的指令.")
   (P "剩下来的" (Code "call/cc") "和应用表达式, 处理方式在某种意义上是类似的. 具有形式"
      (Code "(" fcn-id " " (_ arg-id $1) " " $..h " " (_ arg-id $n) ")")
      "的应用表达式被转换为具有以下形式的指令&quot;序列&quot;:"
      (CodeB "frame
  " next-id "
  " (_ arg-id $n) "
    argument
      " $..v "
        " (_ arg-id $1) "
          argument
            " fcn-id "
              apply")
      "首先要执行的是具有形式" (:Code '(frame next c)) "的" (Code "frame")
      "指令 [注记: 这里原文的" (Code "frame") "指令的参数顺序仍然是颠倒的, "
      "原因前已分析, 不再赘述], 其中"
      (:Code 'c) "指的是(被编译至的)执行应用的指令代码, 而"
      (Code next-id) "则是编译器的接下来的指令参数 (即应用的返回地址). 真正的下一条指令"
      (:Code 'c) "是应用的最后一个参数所被编译至的代码, 紧接着就是" (Code "argument")
      "指令. [注记: " (:Code 'c) "从概念上讲是许多指令, 但是这里指的是其第一条指令.] "
      "然后是倒数第二个参数被编译至的代码, 接着又是一个" (Code "argument")
      "指令. 如此反复, 直至第一个参数以及其相应的" (Code "argument") "指令. 最终, "
      "对于函数表达式求值, 然后执行" (Code "apply") "指令.")
   (P "应用的参数从右往左求值是为了使得其值" (Code "cons") "到值肋骨后顺序正确. "
      "[注记: 编译的时候反过来一次, 求值的时候再反过来一次, 最终得到正确的顺序.]")
   (P (Code "call/cc") "表达式可以想成是只有一个参数的特殊应用, 这个想象中的参数返回"
      "当前的延续. 具有形式" (:Code '(call/cc exp)) "的表达式将产生以下形式的指令序列:"
      (CodeB "frame
  conti
    argument
      " exp-id "
        apply")
      "这将导致帧被压栈, 接着就是延续的创建, 将这个延续对象加入到当前值肋骨中, 对于"
      (Code exp-id) "求值, 最后将" (Code exp-id) "的值应用于仅含有一个延续对象"
      "的参数列表.")
   (P "若应用和" (Code "call/cc") "表达式位于尾位置, 那么对于它们的处理将会有"
      "些许不同, 而判断是否位于尾位置的方法不过就是看看下一条指令是否是返回指令"
      (Code "(return)") "而已:"
      (CodeB "(define tail?
  (lambda (next)
    (eq? (car next) 'return)))")
      "尾位置的应用和" (Code "call/cc") "表达式不需要压入调用帧, 于是"
      (Code "frame") "指令就直接被忽略了.")
   (P "注记: 以下是笔者在Racket中重写的版本. 因为使用了另外的模式匹配宏, "
      "所以相比原始版本更加紧凑."
      (CodeB "(define (tail? next)
  (eq? (car next) 'return))
;not a good name
(define (make-tail c ret)
  (if (tail? ret)
      c
      `(frame ,ret ,c)))
(define (compile x next)
  (match x
    [,var (guard (symbol? var)) `(refer ,var ,next)]
    [,obj (guard (not (pair? obj))) `(constant ,obj ,next)]
    [(quote ,obj) `(constant ,obj ,next)]
    [(lambda ,var* ,body)
     `(close ,var* ,(compile body '(return)) ,next)]
    [(if ,test ,then ,else)
     (let ([thenc (compile then next)]
           [elsec (compile else next)])
       (compile test `(test ,thenc ,elsec)))]
    [(set! ,var ,exp)
     (compile exp `(assign ,var ,next))]
    [(call/cc ,exp)
     (let ([c `(conti (argument ,(compile exp '(apply))))])
       (make-tail c next))]
    [(,rator . ,rand*)
     (let loop ([rand* rand*]
                [c (compile rator '(apply))])
       (if (null? rand*)
           (make-tail c next)
           (loop (cdr rand*)
                 (compile (car rand*)
                          `(argument ,c)))))]))"))
   (H4 "第3.4.3小节 求值")
   (P "虚拟机器" (Code "VM") "使用之前所描述的数据结构和寄存器解释编译器所产生的指令. "
      "其结构类似于SECD机器 [Lan64, Lan65]. 寄存器的状态改变由尾递归函数模拟. "
      "这个函数的参数即那些寄存器本身. " (Code "VM") "的每次递归调用都象征着"
      "新的机器循环的开始. " (Code "VM") "的寄存器的新值是由其参数刻画的. "
      "这样的结构避免了赋值的使用, 使得对于虚拟机器及其状态改变的描述更加干净和简短. "
      "以下是" (Code "VM") "的代码:")
   (CodeB "(define VM
  (lambda (a x e r s)
    (record-case x
      [halt () a]
      [refer (var x)
       (VM (car (lookup var e)) x e r s)]
      [constant (obj x)
       (VM obj x e r s)]
      [close (vars body x)
       (VM (closure body e vars) x e r s)]
      [test (then else)
       (VM a (if a then else) e r s)]
      [assign (var x)
       (set-car! (lookup var e) a)
       (VM a x e r s)]
      [conti (x)
       (VM (continuation s) x e r s)]
      [nuate (s var)
       (VM (car (lookup var e)) '(return) e r s)]
      [frame (ret x)
       (VM a x e '() (call-frame ret e r s))]
      [argument (x)
       (VM a x e (cons a r) s)]
      [apply ()
       (record a (body e vars)
         (VM a body (extend e vars r) '() s))]
      [return ()
       (record s (x e r s)
         (VM a x e r s))])))")
   (P (Code "VM") "的操作遵循之前对于指令的描述. 注意到大多数指令只改变一两个寄存器. "
      "只有一条指令具有副作用, 即" (Code "assign") ", 其破坏性地改变当前环境. 辅助函数为"
      (Code "lookup") ", " (Code "closure") ", " (Code "continuation")
      ", " (Code "call-frame") "和" (Code "extend") ".")
   (P "对于环境的实现, 即" (Code "lookup") "和" (Code "extend") ", 和之前一样.")
   (CodeB "(define closure
  (lambda (body e vars)
    (list body e vars)))")
   (CodeB "(define continuation
  (lambda (s)
    (closure (list 'nuate s 'v) '() '(v))))")
   (CodeB "(define call-frame
  (lambda (x e r s)
    (list x e r s)))")
   (P "最终, 我们需要一个函数将编译器和虚拟机连接起来形成一个可以运行的Scheme求值器:"
      (CodeB "(define evaluate
  (lambda (x)
    (VM '() (compile x '(halt)) '() '() '())))")
      "累积器的初值是不重要的, 但是其他寄存器的初值是重要的. 下一个表达式的值应该是"
      "编译输入的Scheme表达式的结果, 而编译器的下一条指令参数是" (Code "(halt)")
      ". 当前环境从空环境开始 (即肋骨序对的空表). 当前值肋骨和栈当然也应该从空表开始. "
      "[注记: 对于实现正确的编译器和虚拟机以及合法的Scheme表达式, 其实"
      "栈的初值是无所谓的, 而且从理论上讲, 当遇到" (Code "(halt)")
      "指令时, 栈应该会回到初值.]")
   (P "注记: 以下是笔者在Racket中重新实现的版本."
      (CodeB "(define (extend env vars vals)
  (cons (cons vars vals) env))
(define lookup
  (lambda (var e)
    (let nxtrib ([e e])
      (let nxtelt ([vars (caar e)] [vals (cdar e)])
        (cond
          [(null? vars) (nxtrib (cdr e))]
          [(eq? (car vars) var) (car vals)]
          [else (nxtelt (cdr vars) (cdr vals))])))))
(define (apply-env env var)
  (unbox (lookup var env)))
(define (set-val! env var val)
  (set-box! (lookup var env) val))
(define (addv a r)
  (cons (box a) r))
(define (closure body env vars)
  `(closure ,body ,env ,vars))
(define (continuation s)
  `(closure (nuate ,s v) () (v)))
(define (call-frame x e r s)
  `(call-frame ,x ,e ,r ,s))
(define (VM a x e r s)
  (match x
    [(halt) a]
    [(refer ,var ,x) (VM (apply-env e var) x e r s)]
    [(constant ,obj ,x) (VM obj x e r s)]
    [(close ,var* ,body ,x) (VM (closure body e var*) x e r s)]
    [(test ,then ,else) (VM a (if a then else) e r s)]
    [(assign ,var ,x) (set-val! e var a) (VM a x e r s)]
    [(conti ,x) (VM (continuation s) x e r s)]
    [(nuate ,s ,var) (VM (apply-env e var) '(return) e r s)]
    [(frame ,ret ,x) (VM a x e '() (call-frame ret e r s))]
    [(argument ,x) (VM a x e (addv a r) s)]
    [(apply) (match a
               [(closure ,body ,e ,var*)
                (VM a body (extend e var* r) '() s)])]
    [(return) (match s
                [(call-frame ,x ,e ,r ,s) (VM a x e r s)])]))")
      "鉴于Racket中的序对是不可变的, 我对于整个环境的实现做出了一些调整, "
      "即用盒子包裹上了环境中的每个值. 这连带着给当前值肋骨添加新值也需要修改一下. "
      "另外, 现在闭包和栈帧都打上了标签, 这样观察运行时的情况更加清晰. "
      "读者应该注意到" (Code "if") "表达式的行为取决于宿主语言本身的行为, 所以"
      "这里的情况和当时不太一样. 不过, 这里的" (Code "set!") "表达式选择返回RHS的值, "
      "我并没有进一步改动, 而是选择与原始版本保持一致.")
   (H3 "第3.5节 改进变量访问")
   (P "注记: 完全类似的东西, 你也可以在SICP和EoPL中看到. 当然, 既然这东西和de Bruijn索引是一种想法, "
      "大部分编程语言教科书中你都能看到差不多的存在.")
   (P "Scheme的变量是静态作用域的, 也就是说任何变量的绑定都直接反映于程序文本的静态结构之中. "
      "因此, 从源程序确定变量在运行时环境中的位置是可能的. 一种方法是在编译器里维护一个类似的环境结构.")
   (H4 "第3.5.1小节 转换")
   (CodeB "(define compile
  (lambda (x e next)
    (cond
      [(symbol? x)
       (list 'refer (compile-lookup x e) next)]
      [(pair? x)
       (record-case x
         [quote (obj)
          (list 'constant obj next)]
         [lambda (vars body)
          (list 'close
                (compile body (extend e vars) '(return))
                next)]
         [if (test then else)
          (let ([thenc (compile then e next)]
                [elsec (compile else e next)])
            (compile test e (list 'test thenc elsec)))]
         [set! (var x)
          (let ([access (compile-lookup var e)])
            (compile x e (list 'assign access next)))]
         [call/cc
          (let ([c (list 'conti
                         (list 'argument
                               (compile x e '(apply))))])
            (if (tail? next)
                c
                (list 'frame next c)))]
         [else
          (recur loop ([args (cdr x)]
                       [c (compile (car x) e '(apply))])
            (if (null? args)
                (if (tail? next)
                    c
                    (list 'frame next c))
                (loop (cdr args)
                      (compile (car args)
                               e
                               (list 'argument c)))))])]
      [else
       (list 'constant x next)])))")
   
   (CodeB "(define compile-lookup
  (lambda (var e)
    (let nxtrib ([e e] [rib 0])
      (let nxtelt ([vars (car e)] [elt 0])
        (cond
          [(null? vars) (nxtrib (cdr e) (+ rib 1))]
          [(eq? (car vars) var) (cons rib elt)]
          [else (nxtelt (cdr vars) (+ elt 1))])))))")
   
   (CodeB "(define extend
  (lambda (e r)
    (cons r e)))")
   (H4 "第3.5.2小节 求值")
   (P "主体过程" (Code "VM") "其实几乎没有变动, 除了对于" (Code "close")
      "指令的解释需要反映其结构的变化 (也就是不需要形式变量了):"
      (CodeB "      [close (body x)
       (VM (closure body e) x e r s)]")
      "当然, 相应的辅助过程也需要改变. 从本质上说, 这最终反映的是环境结构的变化. "
      (Code "call-frame") "没有任何变化, 因为调用帧的结构不需要改变."
      (CodeB "(define closure
  (lambda (body e)
    (list body e)))")
      (CodeB "(define continuation
  (lambda (s)
    (closure (list 'nuate s '(0 . 0)) '())))")
      (CodeB "(define lookup
  (lambda (access e)
    (let nxtrib ([e e] [rib (car access)])
      (if (= rib 0)
          (let nxtelt ([r (car e)] [elt (cdr access)])
            (if (= elt 0)
                r
                (nxtelt (cdr r) (- elt 1))))
          (nxtrib (cdr e) (- rib 1))))))")
      "注记: 原文的这个新" (Code "lookup") "笔者看着有些膈应, 其实它就相当于"
      (CodeB "(define (lookup access e)
  (list-tail (list-ref e (car access))
             (cdr access)))")
      )
   
   (H2 "第4章 基于栈的模型")
   (P "对于基于堆的Scheme实现的profiling表明测试程序超过一半的运行时间都花在了变量查找和函数调用上. "
      "与之相对的是, 创建闭包, 创建延续以及调用延续所用的时间不那么多. 这当然主要是因为绝大多数程序"
      "中的变量引用和函数调用都很多, 而且还要考虑执行这些操作的开销. 为了提升一个Scheme系统的效率, "
      "自然需要改进这些基本操作. 在基于堆的系统中, 寻找一个变量潜在需要跨越数个链接, 而执行函数调用"
      "需要在堆上分配环境肋骨和调用帧.")
   (P "本章的第一节描述了一个块结构语言的一个典型的基于栈的实现, 包含对于所牵涉的数据结构和概念的"
      "讨论. 这个标准实现的编译器和虚拟机, 奠定了本章剩余内容的基础, 为支持完整Scheme语言的基于栈"
      "的模型提供了托底的样板.")
   (P ""
      )
   (H3 "第4.1节 块结构语言的基于栈的实现")
   (P "本节描述对于Algol 60, C或者Pascal而言典型的实现. 为了简化呈现, 这里考虑的源语言是Scheme的一个"
      "没有第一级函数, 延续以及尾调用优化的方言, 使其类似于更加标准的块结构语言. 参数是按值调用的, "
      "与Algol 60 (其允许参数按名调用) 和Pascal (其允许参数按引用调用) 相对, 尽管这里参数传递风格的"
      "区别并不重要. 函数可以作为参数 (functional), 但是它们不是第一级对象, 因为其在定义作用域"
      "不复存在之后则不能可靠地被保留和调用.")
   (H4 "第4.1.1小节 调用帧")
   (P "调用栈在每次函数应用于一集参数时创建. 它必须要存放被调用函数的参数, 以及任何需要保存的"
      "指针和寄存器 (程序计数器, 临时表达式, 等等) 以在返回后恢复局部临时变量占用的空间, "
      "动态链接和静态链接.")
   (P "原注: 在某些例子中, 调用帧也在每当进入新块时建立, 比如某些Algol 60的实现, 其将块当作是"
      "无参函数. 类似地, 块的局部声明可以被视为函数调用, 其参数是那些局部声明的变量. 这种观点对于"
      "诸如Scheme的" (Code "let") "表达式而言是合适的, 其中初始化总是伴随着声明一起出现. "
      "然而, 最经常的情况是实现将嵌套块中声明的变量当作局部于当前调用帧的临时存储位置处理, "
      "这可以避免创建单独的调用帧的开销.")
   (P "本节描述的基于栈的模型的调用帧包含"
      (Ol (Li "存放着被推迟调用的调用帧地址的帧指针 (即动态链接) "
              "[注记: 在前一章里, 动态链接其实就是将控制栈串起来的那些指针];")
          (Li "被调用函数的参数;")
          (Li "返回地址 (即调用之后的下一个表达式);")
          (Li "存放着被调用函数的次一级外部作用域的调用帧地址的帧指针 (即静态链接) "
              "[注记: 静态链接实际上是用来寻找变量绑定在什么地方的, 在前一章里, "
              "这差不多相当于在环境里指向下一个肋骨的指针].")))
   (P "这些信息在调用帧里的布局如下:"
      (Pre
       (Blockquote
        "静态链接 (最后压栈)\n第一个参数\n" $..v
        "\n最后一个参数\n下一个表达式\n动态链接 (最先压栈)"))
      "调用帧的布局顺序在某种程度上是任意的. 但是为了支持尾调用优化, 最好还是把"
      "下一个表达式 (返回地址) 和动态链接置于参数和静态链接之下 (见第4.6节).")
   (H4 "第4.1.2小节 动态和静态链接")
   (P "动态链接总是指向调用者的帧. 它是在从一个函数返回时用来决定下一帧在"
      "栈上的哪一个位置的. 动态链接有时是不必要的, 因为下一帧在栈上总是恰好就在"
      "当前帧的下面. 如果编译器知道当前帧和前一帧的大小, 那么其总是能够"
      "生成在返回时恢复前一帧的高效代码. 然而, 动态链接几乎总是被使用, 以简化"
      "返回序列, 以支持调试, 或者为了利用微编码的调用和返回指令 (例如VAX的"
      (Code "calls/callg") "和" (Code "ret") "指令 [Dig81]).")
   (P "另一方面, 静态链接总是指向包裹着被调用函数的最内层的函数定义的帧, 即"
      "包含对于被调用函数可见的次外部 (closest outer) 变量绑定的帧. "
      "这里语言不支持闭包, 尾调用优化, 以及延续的假设是重要的, 因为它们必须保证"
      "这个帧仍然在栈上. 例如, 如果一个闭包可以被返回并在其作用域之外被使用, "
      "那么在调用闭包的时候, 对于闭包静态可见的帧可能已经没了. 类似地, "
      "优化尾调用可能导致当前的栈帧被删除, "
      )
   (P "静态链接和动态链接不一定指向同一个帧. 动态链接在栈上串起了一条"
      "单独的帧的链条 (动态链), 而静态链接潜在可能创造了数个静态链, 而"
      "每个环境都代表一个静态链. 静态链接和动态链接一起支撑起了一个结构. "
      "一个动态链接指向控制栈上的下一帧, 而一个静态链接指向一个环境的"
      "下一个肋骨.")
   (P ""
      )
   (H4 "第4.1.3小节 Functionals")
   (P "在这个简单的栈模型的框架下将函数作为参数传递是可能的, 只要这个函数被调用时"
      "其作用域的调用帧仍然活跃即可. "
      )
   (P "在这个系统中创建functional的代码似乎与在基于堆的模型中"
      "创建闭包的代码是一样的:"
      (CodeB "(define functional
  (lambda (body e)
    (list body e)))")
      "然而, 这里有一点重要的不同. 传递给" (Code "functional")
      "的环境" (Code "e") "是一个指向栈上的某个帧的帧指针, 而不是一个"
      "在堆上分配的环境.")
   (H4 "第4.1.4小节 栈操作")
   (P "栈被实现为Scheme的向量."
      (CodeB "(define stack (make-vector 1000))")
      "当然, 这里选择的长度" (Code "1000") "是任意的.")
   (P "函数" (Code "push") "接受一个栈指针 (从虚拟机器的概念来说应该是"
      "当前栈顶) 和一个对象, 然后将这个对象加入到栈顶, 并返回更新过了的"
      " (增量了的) 栈指针:"
      (CodeB "(define push
  (lambda (x s)
    (vector-set! stack s x)
    (+ s 1)))")
      (Code "push") "函数与许多现代机器架构提供的自动增量或者自动减量"
      "寻址模式相对应.")
   
   (CodeB "(define index
  (lambda (s i)
    (vector-ref stack (- (- s i) 1))))
(define index-set!
  (lambda (s i v)
    (vector-set! stack (- (- s i) 1) v)))")
   (H4 "第4.1.5小节 转换")
   (P "以下编译器和第3.5节的有三处小的不同. 首先, 既然参数值直接置于函数的"
      "调用帧之中, 那么调用帧的大小现在是一个关于参数数目的变量. 虚拟机器的"
      (Code "return") "指令必须拥有这项信息才能正确移除调用帧, 因此"
      (Code "return") "指令现在多了一个参数" (Code "n") ". 这个参数告诉"
      "机器要从栈上移除多少个元素. 不计返回地址和动态链接, 因为它们被显式"
      "移除然后恢复, 但是静态链接和参数值一起就顺便被移除了. 第二点不同在于"
      "既然尾调用优化没有得到支持, 相关代码可以移除了. 第三点不同在于"
      "对于延续的支持也被整个移除了.")
   (CodeB "(define compile
  (lambda (x e next)
    (cond
      [(symbol? x)
       (compile-lookup x e
         (lambda (n m)
           (list 'refer n m next)))]
      [(pair? x)
       (record-case x
         [quote (obj)
          (list 'constant obj next)]
         [lambda (vars body)
          (list 'close
                (compile body
                         (extend e vars)
                         (list 'return (+ (length vars) 1)))
                next)]
         [if (test then else)
          (let ([thenc (compile then e next)]
                [elsec (compile else e next)])
            (compile test e (list 'test thenc elsec)))]
         [set! (var x)
          (compile-lookup var e
            (lambda (n m)
              (compile x e (list 'assign n m next))))]
         [else
          (recur loop ([args (cdr x)]
                       [c (compile (car x) e '(apply))])
            (if (null? args)
                (list 'frame next c)
                (loop (cdr args)
                      (compile (car args)
                               e
                               (list 'argument c)))))])]
      [else
       (list 'constant x next)])))")
   (P "注记: 这里又有一点很小的不一致, 可以算是第四点不同. "
      "我相当怀疑这点不同是因为Kent Dybvig后期修改了原本的代码, "
      "但是忘了维护陈述的一致性. 其实" (Code "refer") "和"
      (Code "assign") "的句法也被修改了, 之前变量是一个序对, "
      "现在变成了两个分量. " (Code "compile-lookup")
      "自然也和之前有些许不同, 多了一个&quot;延续&quot;"
      "参数" (Code "return") "用以返回多值, 但是" (Code "extend")
      "仍然和之前第3.5节保持一致.")
   (CodeB "(define compile-lookup
  (lambda (var e return)
    (let nxtrib ([e e] [rib 0])
      (let nxtelt ([vars (car e)] [elt 0])
        (cond
          [(null? vars) (nxtrib (cdr e) (+ rib 1))]
          [(eq? (car vars) var) (return rib elt)]
          [else (nxtelt (cdr vars) (+ elt 1))])))))")
   (H4 "第4.1.6小节 求值")
   (P "本节的虚拟机器与第3.5节的拥有类似的结构. 原本的寄存器中, 累积器"
      (Code "a") "和下一个表达式" (Code "x") "的用途不变, 而环境"
      (Code "e") "和栈" (Code "s") "则稍有变化. 寄存器" (Code "e")
      "仍然存放着某种意义上的环境, 即静态链接. 然而, 它现在是一个栈指针, "
      "指向次外部作用域的调用帧. 寄存器" (Code "s") "现在是一个指向"
      "当前栈顶的栈指针. 原本的寄存器, 即当前值肋骨" (Code "r")
      ", 被整个移除了. 现在参数值被直接置于栈上, 而不是放在一个堆分配的"
      "值肋骨里.")
   (P "虚拟机器的指令和前一章相比当然也发生了一些变化, 以下是指令总结:"
      (Ul (Li (Code "(halt)") "仍表现得一致.")
          (Li (:Code '(refer var x)) "沿着栈上的静态链接而不是"
              "堆分配的环境中的链接. [注记: 这里的句法其实稍有改变, "
              "笔者怀疑是Kent Dybvig后来修改过了, 但忘了维护前后的一致性. "
              "原本变量是一个序对, 现在是句法里的两个分量. 或许更好的记号是"
              (:Code '(refer n m x)) ".]")
          (Li (:Code '(constant obj x)) "仍表现得一致.")
          (Li (:Code '(close vars body x))
              "现在创建一个functional而不是一个闭包. "
              "[注记: Kent Dybvig误将" (Code "close")
              "写成了" (Code "closure") ".]")
          (Li (:Code '(test then else)) "仍表现得一致.")
          (Li (:Code '(assign var x))
              "沿着栈上的链接而不是堆分配的环境中的链接. [注记: 同"
              (Code "refer") "指令.]")
          (Li (:Code '(conti x)) "现在没有支持.")
          (Li (:Code '(nuate s var)) "现在没有支持.")
          (Li (:Code '(frame ret x))
              " [注记: 顺序怎么还是反的] 依次将动态链接 (当前帧指针) "
              "和下一个表达式" (Code ret-id) "压入栈中以开始一个新帧. "
              "前一章的虚拟机器是在堆上建立的调用帧.")
          (Li (:Code '(argument x))
              "将参数压入栈中而不是把它添加到当前值肋骨上.")
          (Li (Code "(apply)")
              "表现得类似, 只是其将functional里的静态链接压入栈中, "
              "而不是通过添加值肋骨来构造一个新的堆上分配的环境.")
          (Li (:Code '(return n))
              "现在接受额外的参数" (:Code 'n) ", 其决定了"
              "该从栈上移除多少个元素.")))
   (P "以下是虚拟机器的代码:")
   (CodeB "(define VM
  (lambda (a x e s)
    (record-case x
      [halt () a]
      [refer (n m x)
       (VM (index (find-link n e) m) x e s)]
      [constant (obj x)
       (VM obj x e s)]
      [close (body x)
       (VM (functional body e) x e s)]
      [test (then else)
       (VM a (if a then else) e s)]
      [assign (n m x)
       (index-set! (find-link n e) m a)
       (VM a x e s)]
      [frame (ret x)
       (VM a x e (push ret (push e s)))]
      [argument (x)
       (VM a x e (push a s))]
      [apply ()
       (record a (body link)
         (VM a body s (push link s)))]
      [return (n)
       (let ([s (- s n)])
         (VM a (index s 0) (index s 1) (- s 2)))])))")
   (P "尚未描述的辅助函数仅有" (Code "find-link")
      ", 它是" (Code "lookup") "的外部循环的类似物.")
   (CodeB "(define find-link
  (lambda (n e)
    (if (= n 0)
        e
        (find-link (- n 1) (index e -1)))))")
   (P "注记: 在对于" (:Code '(return n)) "指令求值前, 栈帧中元素的布局大概是"
      (MB (set-attr*
           (&Table
            ("索引"
             (@- $s $1) (: (@- $s $2) "到" (@- $s $n))
             (&- (@- $s $n) $1) (&- (@- $s $n) $2))
            ("意义"
             "静态链接/下一个值肋骨"
             (: (@- $n $1) "个参数")
             "返回地址/表达式"
             "动态链接/保存的环境"))
           'frame "solid" 'rowlines "solid" 'columnlines "solid"))
      "读者还应该注意到当前栈顶指针总是指向空白的那个位置, 所以"
      (Code "index") "操作总是减去一.")
   (H2 "第5章 基于字符串的模型")
   (H2 "第6章 结论")
   ))