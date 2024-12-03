#lang racket
(provide programming.html)
(require SMathML)
(define $fib (Mi "fib"))
(define (&fib n)
  (app $fib n))
(define programming.html
  (TmPrelude
   #:title "程序设计的邀请"
   #:css "styles.css"
   (H1 "程序设计的邀请")
   (H2 "序言")
   (P "程序设计是计算机科学的基础. 这句话或许有人同意或许有人反对, 但是并不会影响到"
      "程序设计教育的糟糕现实. 不论如何, 程序设计的重要性都被严重低估了, 尤其是当其"
      "被人拿来与算法和数据结构进行比较的时候. 程序设计教育的核心应该是让人理解到"
      "可复合性, 或者说抽象和组合, 在这个世界中的地位, 而不是教授具体编程语言的句法. ")
   (P "请允许我在这里重复SICP (一本光辉而伟大的计算机科学教科书) 里的文字和想法. "
      "编程语言不仅是指挥计算机器执行命令的手段, 还是组织关于过程的想法的框架. "
      "任何编程语言都必须提供原语, 组合的手段, 和抽象的手段. 原语就是最基本的表达形式, "
      "组合是由较为简单的东西构造更加复杂的东西, 而抽象将不论复杂还是简单的东西当作一个整体看待和操纵, "
      "忽略其细节. 任何复杂的系统, 不论看起来有多么不可思议, 也都是由组合和抽象的基本原则构筑起来的. "
      "换句话说, 就是没有真正的魔法."
      )
   (H2 "第1章 编程基础")
   (H3 "第1.1节 读取求值输出循环")
   (P "读取求值输出循环是一种模式, 其中用户输入一个表达式, 按下回车, "
      "系统就会对于该表达式求值, 并将结果输出于下方, 接着等待用户的下一次输入, "
      "因而是一个循环. "
      "学习程序设计的最好方法或许就是观察读取求值输出循环的交互结果, 并思考缘由.")
   (P "让我们观察以下示例交互.")
   (CodeB "> 42
42")
   (P "这段交互应该读作&quot;表达式" (Code "42") "的值是" (Code "42")
      "&quot;.")
   (H3 "第1.2节 字面量")
   (P "字面量是对其求值得到本身的表达式.")
   (CodeB "> -123
-123
> 22/7
22/7
> 3.14
3.14
> #t
#t
> #f
#f
> &quot;hello, world&quot;
&quot;hello, world&quot;")
   (P "数字, 布尔值, 字符串等都是字面量. Scheme用" (Code "#t")
      "表示真, 用" (Code "#f") "表示假.")
   (H3 "第1.3节 变量")
   (P "变量在字面上是符号, 然而它不代表字面, 而是引用一个值, 值也被称为对象.")
   (CodeB "> (define three 3)
> three
3")
   (P "我们可以使用" (Code "define") "形式将符号和值关联起来, 之后就可以"
      "用符号来引用这个值.")
   (H3 "第1.4节 过程应用")
   (P "过程是函数的近义词, 过程应用即函数应用, 即应用函数于具体的参数上. "
      "一般说来我们并不区分过程和函数这两种术语, 但是现在我们希望讨论一下"
      "它们之间微妙的区别.")
   (P "现代数学意义上的函数是函数的外延, 因为它只是输入和输出的对应, 但并不直接诉诸于"
      "算术, 几何与算法. 过程, 在某种意义上来说, 是函数的古老概念, 它蕴涵着如何从输入"
      "通过计算得到输出的规则. 也就是说, 过程是内涵性的实体.")
   (P "现在让我们回到正题. 数学中函数应用的句法是多种多样的, 例如中缀式" (&+ $3 $4) ", 前缀式"
      (appl $f $x $y $z) ", 抑或是采用角标记号, 例如用于引用集族"
      (_ (@ $A_lambda) (&in $lambda $I)) "里的一个集合" $A_mu ". 实际上, 为了各种各样的目的, "
      "数学家经常创制出许多ad hoc的记号, Halmos就喜欢这么做. 但是, Scheme中过程应用的句法"
      "是异常简单的, 或可以被称为全加括号的前缀记号."
      (CodeB "> (+ 1 (* 2 3))
7
> (quotient 11 3)
3
> (remainder 11 3)
2
> (* 1/2 1/3 1/4)
1/24")
      "尽管这种记号看上去非常古怪, 且违背读者自小学以来被灌输的数学实践, 然而它却是更加"
      "简单和一致的. 在真正复杂的情况下, 这种记号将展示出它的威力." (Br)
      "至于Scheme实现所提供的内置过程, 请读者参考Scheme标准和实现手册."
      )
   (H3 "第1.5节 " (Code "lambda") "表达式")
   (P "显然我们需要一种方式来表达过程, 在Scheme语言中, 那就是" (Code "lambda") "表达式. "
      "这个名字来源于" (Math $lambda) "演算, 一种由Alonzo Church设计的内涵性函数理论.")
   (CodeB "> ((lambda (x) (* x x)) 1/2)
1/4")
   (P "在以上交互中, " (Code "(lambda (x) (* x x))") "表示一个函数, 其中" (Code "(x)")
      "是参数的列表, " (Code "(* x x)") "过程的体. 在某种意义上, "
      "它就类似于数学记号" (&\|-> $x (&d* $x $x)) ". 或者, 对于大多数人更熟悉的表达是"
      "&quot;令函数" (&= (app $f $x) (&d* $x $x)) "&quot;, 但是读者应该注意到我们没有"
      "必要为每个函数取一个名字.")
   (P "另外, 有时" (Code "(x)") "中的" (Code "x") "也被称为形式参数, 这是因为它本身"
      "不具备意义, 仅仅是一个占位的符号而已. 如果将其一致地替换为其他符号, 也不会改变"
      "函数本身的意义, 例如" (Code "(lambda (y) (* y y))") "也表达了和"
      (Code "(lambda (x) (* x x))") "一样的含义.")
   (P "这种记号是相当灵活的, 请读者观察以下例子.")
   (CodeB "> (((lambda (f g)
      (lambda (x)
        (f (g x))))
    (lambda (x) (* x x))
    (lambda (x) (+ x 3)))
   13)
256")
   (P "这似乎看上去有些复杂, 然而读者只要明白"
      (Code "(lambda (f g) (lambda (x) (f (g x))))")
      "表达了函数复合之意, 那就不难理解.")
   (H3 "第1.6节 定义")
   (P "定义即命名, 是最基本的抽象方式, 请读者阅读以下交互.")
   (CodeB "> (define pi 3.14)
> (define square
    (lambda (x) (* x x)))
> (define area
    (lambda (radius)
      (* pi (square radius))))
> (area 5)
78.5")
   "鉴于函数的定义频繁出现, Scheme具有一个定义函数的句法糖. 所谓句法糖, "
   "就是等价的但更为简化的句法. 以下是用句法糖改写上述交互的结果."
   (CodeB "> (define pi 3.14)
> (define (square x) (* x x))
> (define (area radius)
    (* pi (square radius)))
> (area 5)
78.5")
   (H3 "第1.7节 条件分支")
   (P "条件分支表达了某种选择. 在Scheme中, 我们有两种这样的构造: "
      (Code "if") "和" (Code "cond") ", 其中" (Code "cond")
      "可以被视为" (Code "if") "的句法糖. 首先让我们来看" (Code "if")
      ", 这是更简单的构造.")
   (CodeB "(define (abs x)
  (if (&lt; x 0)
      (- x)
      x))")
   (P "以上的绝对值函数" (Code "abs") "在某种意义上可能是自明的, "
      "但我们仍然最好对其作出一些解释. 这里的" (Code "if") "表达式会先对于"
      (Code "(&lt; x 0)") "求值, 然后根据其结果来确定对于" (Code "(- x)")
      "求值还是对于" (Code "x") "求值, 并将这个结果作为整个" (Code "if")
      "表达式的值. 机敏的读者应该注意到, " (Code "if")
      "并非函数, 因为它不会对于它的每个参数都进行求值, 因而若当我们试图按照"
      (CodeB "(define (if^ Q A E)
  (if Q A E))")
      "来定义我们自己的" (Code "if") "类似物" (Code "if^")
      ", 则会造成问题.")
   ((exercise)
    "举一个例子说明为什么在行为上" (Code "if^")
    "并不等价于" (Code "if") "? (读者不必立即回答此问题, 之后的练习也是类似.)")
   (P "接下来让我们看" (Code "cond") "表达式. "
      )
   (H3 "第1.8节 " (Code "and") ", " (Code "or") ", " (Code "not"))
   (P "与或非是三种常见的逻辑联结词. 在Scheme中, 和绝大多数编程语言一样, "
      (Code "and") "和" (Code "or") "具有短路性质. 也正因如此, " (Code "and")
      "和" (Code "or") "显然不是函数, 鉴于它并不遵循通常函数应用的求值规则. "
      "请看以下交互.")
   (CodeB "> (and)
#t
> (and 1)
1
> (and 1 2)
2
> (and 1 2 3)
3
> (or)
#f
> (or 1)
1
> (or 1 2)
1
> (or 1 2 3)
1")
   (H3 "第1.9节 递归定义")
   (P "所谓递归定义, 指的是在定义中(直接或间接地)引用自身的定义. "
      "尽管这听上去有些奇怪, 但是在阅读了以下的几个例子之后, "
      "读者应该意识到递归是一种相当自然的表达方式.")
   (P "请读者观察第一个例子, 即Fibonacci数列, 它在数学上是由递归定义的.")
   (MB (&= (&fib $n) (Choice0 ($n "," (&< $n $2))
                              ((&+ (&fib (&- $n $1))
                                   (&fib (&- $n $2)))
                               ",否则的话"))))
   (P "我们可以将" $fib "较为直接地翻译为Scheme代码.")
   (CodeB "(define (fib n)
  (if (&lt; n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))")
   (P "阶乘是另一个数学上常见的函数, 其也可以用递归进行定义.")
   (MB (&= (&fact $n)
           (Choice0
            ($1 "," (&= $n $0))
            ((&d* $n (&fact (@- $n $1))) ",否则的话"))))
   (CodeB "(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))")
   (P "递归的要义在于: 对于某些简单的基本情形, 我们可以直接给出结果; "
      "对于复杂的情形, 我们可以将其转化为更简单的情形的结果的组合. "
      "之后我们将看到, 递归不仅可以在自然数上进行, 还可以在列表和"
      "树等结构上进行. 实际上, 最一般地, 递归可以定义于良基集合上. "
      "良基的条件将允许我们用归纳法来证明关于程序的性质.")
   (H3 "第1.10节 局部定义")
   (P "以下过程" (Code "sqrt") "可以计算正实数的平方根.")
   (CodeB "(define (sqrt x)
  (define init 1.0)
  (define tol 0.0001)
  (define (average a b)
    (/ (+ a b) 2))
  (define (good-enough? guess)
    (&lt; (abs (- x (sqr guess))) tol))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter init))")
   (P "当然, " (Code "sqrt") "的正确性是一个(数学)分析的问题, 但我们的重点不在于此. "
      
      )
   (H3 "第1.11节 高阶过程")
   (P "高阶过程指的是以过程作为参数或者返回值的过程. "
      "或许有人会问为什么需要高阶过程, 那是因为若是没有高阶过程, 许多概念就无法表达. "
      "实际上, 我们已经在第1.5节见过一个高阶过程的例子了, "
      "即函数复合, 现在我们为其取一个名字" (Code "compose") ".")
   (CodeB "(define (compose f g)
  (lambda (x)
    (f (g x))))")
   (P "实际上数学中高阶函数的例子是很多的, 就是往往它们被称为算子, 泛函之类的, 但就是不叫函数. "
      "例如, 求和记号" (Mi "&sum;") "在某种意义上就是一个高阶过程.")
   (CodeB "(define (Sigma f a b)
  (if (> a b)
      0
      (+ (f a)
         (Sigma f (+ a 1) b))))")
   
   (H3 "第1.12节 计算行为")
   (P "过程" (Code "gcd") "和" (Code "fact") "呈现的计算行为有何不同? "
      "实际上, 这只需略微动笔计算即可得知."
      (CodeB "  (gcd 21 13)
= (gcd 13 8)
= (gcd 8 5)
= (gcd 5 3)
= (gcd 3 2)
= (gcd 2 1)
= (gcd 1 0)
= 1")
      (CodeB "  (fact 3)
= (* 3 (fact 2))
= (* 3 (* 2 (fact 1)))
= (* 3 (* 2 (* 1 (fact 0))))
= (* 3 (* 2 (* 1 1)))
= (* 3 (* 2 1))
= (* 3 2)
= 6")
      "它们最大的区别在于, " (Code "fact") "需要在计算过程之中不断记住要乘上的数字, "
      "以至于&quot;记忆&quot;不断膨胀, 反观" (Code "gcd") ", 它不需要在计算过程之中"
      "记住任何额外的信息, 参数本身就已经构成了计算的全部状态. 如果一个过程在计算之中"
      "所需要的记忆是没有上界可言的, 那么我们就称其为呈现了递归计算行为. 反之, 我们"
      "就称该过程呈现了迭代计算行为. 所谓递归计算行为和迭代计算行为和过程本身的形式"
      "无关, 比如这里的" (Code "gcd") "在形式上就是递归的. 有的时候, 我们将呈现了"
      "迭代计算行为但是形式上是递归的过程称为尾递归的. 其实是这样的, 出现在尾位置的"
      "过程调用并不需要记住什么新的信息, 这是最关键的地方. 但是, 尾递归过程是否在"
      "实际执行中真的仅使用有界空间的记忆依赖于语言本身的实现方式.")
   (P "实际上, 存在一种编程的技巧被称为延续传递风格 (Continuation-Passing Style), "
      "其可以用尾递归的形式表达任意的递归过程. 然而, 实际的空间消耗并不可能减少, "
      "因为它把记忆转移到了所谓的延续参数上. 延续是这样一种函数, 其接受值作为参数, "
      "然后完成&quot;剩余的计算&quot;. 对于延续进行&quot;具体化&quot;, 也就是说"
      "用某种非常具体的数据来表示延续将导致所谓的&quot;累积器传递风格&quot;. "
      "似乎以上的讨论非常抽象, 然而现在我们举一个简单的例子来说明这些想法."
      (CodeB "(define (fact-cps n k)
  (if (= n 0)
      (k 1)
      (fact-cps (- n 1)
                (lambda (v)
                  (k (* n v))))))")
      (CodeB "(define (fact-acc n a)
  (if (= n 0)
      a
      (fact-acc (- n 1) (* n a))))")
      )
   (H3 "第1.13节 复杂度")
   (P "复杂度是对于程序运行的代价的一种度量. ")
   (H2 "第2章 列表处理")
   (P "Scheme是Lisp的方言, Lisp旧称LISP, 而LISP代表LISt Processor, 列表处理器之意.")
   (H3 "第2.1节 序对和列表")
   (P "序对是数据的黏合剂, 一个序对恰将两个对象组合成一个对象. 在Scheme中, "
      (Code "cons") "用来构造序对, " (Code "car") "是第一投影, " (Code "cdr")
      "是第二投影.")
   (CodeB "> (define p (cons 1 2))
> (car p)
1
> (cdr p)
2")
   (P (Code "cons") ", " (Code "car") ", " (Code "cdr") "并非魔法. 实际上, 仅使用第1章"
      "读者所学到的东西, 也可以实现它们.")
   (CodeB "(define (cons x y)
  (lambda (m)
    (m x y)))
(define (car p)
  (p (lambda (x y) x)))
(define (cdr p)
  (p (lambda (x y) y)))")
   (H3 "第2.2节 列表上的函数")
   (P (Code "length") "计算一个列表的长度.")
   (CodeB "(define (length lst)
  (if (null? lst)
      0
      (+ (length (cdr lst)) 1)))")
   (H3 "第2.3节 列表处理工具箱")
   (P "列表可以用来表示有限的序列. 围绕列表, 我们可以设计一组过程, 它们可以对于列表进行各种各样的操作. "
      "这些列表处理的过程可以灵活地组合在一起, 以表达各种各样的概念.")
   (P "或许各种列表处理过程之中最为典型的就是" (Code "map") ", 它对于列表的每个元素施行某种变换, "
      "而结果仍然是一个列表.")
   (CodeB "(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (map proc (cdr lst)))))")
   (P (Code "filter") "从列表中过滤出满足谓词的元素.")
   (CodeB "(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst)
               (filter pred (cdr lst))))
        (else
         (filter pred (cdr lst)))))")
   (P (Code "append") "合并两个列表.")
   (CodeB "(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1)
            (append (cdr lst1) lst2))))")
   (P (Code "reverse") "反转一个列表.")
   (CodeB "(define (reverse lst)
  (let iter ((rest lst) (result '()))
    (if (null? rest)
        result
        (iter (cdr rest)
              (cons (car rest) result)))))")
   (P (Code "fold-left") "和" (Code "fold-right")
      "是对于列表处理的抽象.")
   (CodeB "(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc init (car lst)) (cdr lst))))
(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst)
            (fold-right proc init (cdr lst)))))")
   (P "我们应该注意到, 这里的" (Code "fold-left")
      "是尾递归的, 但是" (Code "fold-right")
      "不是尾递归的. 不过, 实际上的确可以将" (Code "fold-right")
      "定义成尾递归的, 这留给读者思考.")
   ((exercise)
    "定义尾递归的" (Code "fold-right") ".")
   (P "我们可以使用" (Code "fold-left") "和" (Code "fold-right")
      "来表达之前定义的几个列表处理过程.")
   (CodeB "(define (map proc lst)
  (fold-right
   (lambda (a d)
     (cons (proc a) d))
   '() lst))
(define (filter pred lst)
  (fold-right
   (lambda (a d)
     (if (pred a) (cons a d) d))
   '() lst))
(define (append lst1 lst2)
  (fold-right cons lst2 lst1))
(define (reverse lst)
  (fold-left
   (lambda (a d) (cons d a))
   '() lst))")
   (H3 "第2.4节 列表处理: 更进一步")
   (P "上一节我们已经讨论了列表处理的最基本的一些模式, "
      "现在我们想要处理一些更加复杂的问题.")
   (P "许多时候, 虽然我们希望使用" (Code "map")
      "来对于列表的每个元素施行某种变换, 但是这种变换返回的并不是一个结果, "
      "而是一列结果. 从某种角度来说, 此时列表处理作为有限序列带来的可复合性"
      "被稍稍破坏了. 然而, 我们很容易修复这个问题, 通过一个与" (Code "map")
      "类似的过程" (Code "append-map") ", 有时也被称为" (Code "flatmap") ".")
   (CodeB "(define (append-map proc lst)
  (if (null? lst)
      '()
      (append (proc (car lst))
              (append-map proc (cdr lst)))))")
   (P "和" (Code "map") "不同的是, " (Code "append-map")
      "使用" (Code "append") "将变换的结果连接在一起, 起到了展平的效果, 因而"
      (Code "flatmap") "的确也是一个合适的名字, 这被其他许多编程语言所采用.")
   (P "为了刻画" (Code "append-map") "的用途, 接下来我们举一个稍微复杂一点的例子, "
      "即所谓的" $n "皇后. 这个问题要求我们枚举出在一个" (&c* $n $n)
      "的棋盘上放置" $n "枚皇后棋子并使得这些棋子相互之间无法攻击的所有可能方案.")
   (P "在编写程序之前, 我们最好先观察一下问题的结构, 有时简单的观察可以"
      "极大地简化程序或者提高效率. 当然, 读者也可以观察良久, 不过那更多是数学家的习惯. "
      "首先, 我们注意到既然皇后可以自由地横行或直行, 那么对于可能的方案而言, "
      "每一列和每一行必恰有一个皇后. 显然, 读者看出来我们可以将方案表示为一个置换. "
      
      )
   ))