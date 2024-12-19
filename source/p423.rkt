#lang racket
(provide p423.html)
(require SMathML)
(define p423.html
  (TmPrelude
   #:title "P423笔记"
   #:css "styles.css"
   (H1 "P423笔记")
   (P "P423是IUB一门关于编译器构造的课程. 和C311一样, 历史都比较悠久了. "
      "当然, 课程的内容是不断更新的, 比如说现在的P423课程会使用一本叫做"
      "Essentials of Compilation的书. 但是, 我这里的笔记是针对曾经的"
      "Kent Dybvig还在IUB时上的P423课程的作业材料的探究.")
   (P "我最初是在2020年初的时候开始阅读这些作业材料, "
      "大概花了一段时间我才搞明白整个作业的结构. "
      "最终, 我决定倒着完成这些作业. 不过, 实际上我只完成了作业的后半部分. "
      "前半部分里我遇到了一些困难, 最终导致我没能完成所有的作业. "
      "当初写的代码已经全被我删除了, 这是我的坏习惯. "
      "不过, 至少我还记得一些东西. 现在, 我决定复习一下之前完成的作业, "
      "并试着真正写完一个从Scheme到x86的优化编译器.")
   (P "nanopass编译器框架实际上不是必要的, 只需要一个最简单的"
      "模式匹配宏也足够完成P423作业了. 当然了, nanopass提供了"
      "比较fancy的机制, 例如catamorphism, 这真的很方便, "
      "但是没有的话也不是不能写编译器就是了.")
   (P "我决定和上次一样, 仍然使用C311课程里的那个模式匹配宏, "
      "据说这是Oleg Kiselyov写的, 我做了一点修改."
      (CodeB "(define-syntax match
  (syntax-rules (guard)
    ((_ v) (error 'match &quot;~s&quot; v))
    ((_ v (pat (guard g ...) e ...) cs ...)
     (let ((fk (lambda () (match v cs ...))))
       (ppat v pat (if (and g ...) (let () e ...) (fk)) (fk))))
    ((_ v (pat e ...) cs ...)
     (let ((fk (lambda () (match v cs ...))))
       (ppat v pat (let () e ...) (fk))))))
(define-syntax ppat
  (syntax-rules (unquote)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (eqv? v (quote lit)) kt kf))))"))
   (P "我印象中, 后半部分最令我捉摸不透的地方实际上是" (Code "purify-letrec")
      ", 我到现在还是没能理解" (Code "letrec") "和" (Code "letrec*")
      "的语义, 而且似乎顶层程序的语义也值得好好琢磨. 闭包变换"
      "也多花了我一段时间, 因为实际上网上没法找到那次作业的pdf文件. "
      "但是, 我的确可以猜出来大致上要做什么, 而且在某个上过P423课程"
      "的人写的作业里, 我找到了比较详尽的描述, 所以没有耗费我太多精力. "
      "实际上, 闭包变换在概念上是比较简单的. 至于是不是有人故意删除了"
      "这次作业的文件, 那就不得而知了.")
   (P "为了完成这个作业, 除了作业要求材料之外, Andy Keep为了展示nanopass"
      "框架而写的两个编译器是值得阅读的, Andy Keep的PhD论文也是值得阅读的. "
      "Kent Dybvig的PhD论文, 以及其他一些关于Chez Scheme优化的论文, "
      "同样地令人感兴趣. 有些材料没有那么适切, 但也仍然值得阅读, "
      "比如Appel的Compiling with Continuations, 其刻画了SML/NJ的后端.")
   (H2 "作业15")
   (P "这次作业仅需要完成" (Code "parse-scheme")
      ", 目的主要是将最初的源语言规约为更加紧凑的形式, "
      "并对于输入进行一些简单的验证, 排除一些基本的错误. "
      "当然, 顺便进行一下" $alpha
      "变换, 使得变量名唯一, 这是之后诸多分析和变换的基础.")
   (P "虽然概念上没有什么复杂的地方, 但是写起来有许多细节问题, "
      "导致我当初写得非常难受, 似乎也犯了不少错误. "
      "不过, 最终我的代码和主流也稍微有所不同, "
      "因为我试图解决一类常见但也经常不被注意到的问题, "
      "就是绑定可以引入名字和" (Q "关键词")
      "相同的变量, 这时的处理我选择和一般的Scheme实现一致. "
      "但是, 似乎网上别人上传的作业解答都没有注意到这一点. "
      "当然, 这也不能算很重要就是了.")
   (P (Code "parse-scheme") "这个pass的输入语言的句法如下:"
      (CodeB "&lt;exp> ::= &lt;fixnum>
       |  &lt;boolean>
       |  (quote &lt;datum>)
       |  &lt;var>
       |  (if &lt;exp> &lt;exp>)
       |  (if &lt;exp> &lt;exp> &lt;exp>)
       |  (set! &lt;var> &lt;exp>)
       |  (begin &lt;exp>+)
       |  (lambda (&lt;var>*) &lt;exp>+)
       |  (let ((&lt;var> &lt;exp>)*) &lt;exp>+)
       |  (letrec ((&lt;var> &lt;exp>)*) &lt;exp>+)
       |  (and &lt;exp>*)
       |  (or &lt;exp>*)
       |  (&lt;prim> &lt;exp>*)
       |  (&lt;exp> &lt;exp>*)
&lt;datum> ::= ()
         |  &lt;boolean>
         |  &lt;fixnum>
         |  (&lt;datum> . &lt;datum>)
         |  #(&lt;datum>*)
&lt;boolean> ::= #t
           |  #f")
      "这里当然有一些需要解释的地方, 而且还有一些额外的限制, "
      "但是这些限制没有体现在句法里. 首先, " (Code "&lt;fixnum>")
      "是一个" 61 "位的带符号整数. 至于为什么是" 61
      "位, 这是因为我们拿了" 3 "位用作类型标记 (type tag). "
      "Scheme是一种动态类型的语言, 所以需要运行时的类型标记. "
      "另外, 可以提前剧透一下, " (Code "&lt;fixnum>")
      "的类型标记是" (Code "000") ", 这不是随意选择的, 而是有原因的. "
      "当然, 原因之后再说. " (Code "&lt;var>")
      "实际上就是一个Scheme符号而已, 不是其他东西. 单分支的"
      (Code "if") ", 理论上而言是利用其副作用, 若是不能成立, 我们选择返回"
      (Code "(void)") ", 这是许多Scheme实现的选择, 用其表达利用副作用的意图. "
      "当然, 单分支的" (Code "if") "是以宏扩展一般的方式转化为正常的"
      (Code "if") "表达式的. 非终结符后面跟着的星号叫做Kleene star, "
      "这是相当标准的东西, 表示零个或任意有限多个. 非终结符后面跟着的加号叫做"
      "Kleene plus, 也基本上是相当标准的东西, 表示一个或(更多的)任意有限多个. "
      (Code "begin") "表达式可以有多个句法参数, 但是只有最后一个才是作为值意图的, "
      "其余的那些表达式我们则是意图利用其副作用. "
      (Code "lambda") ", " (Code "let") ", " (Code "letrec")
      "这三种构造里出现的诸变量应该是互异的, 这没有什么可说的. "
      (Code "let") "和" (Code "letrec") "的绑定规则, 其实大家也是知道的, "
      "当然的确是有所不同的. " (Code "and") "和" (Code "or")
      "也相当于是利用宏扩展实现的, 会被转换为嵌套的" (Code "if")
      "表达式. 应用一个原始过程和应用非原始过程的句法其实很像, "
      "不过其实在这种情况下的确是必要的. 读者可能会发现, "
      (Code "&lt;prim>") "竟然不能单独出现, 这其实和正常的Scheme实现还不太一样. "
      "不过, 也不是什么大的限制, 毕竟可以使用" $eta "扩展. "
      "当然, 编译器可以考虑纳入这种单独出现的原始过程引用的句法, 但是这里无所谓啦. "
      "之前只出现了" (Code "and") "和" (Code "or")
      ", " (Code "not") "去哪里了呢, " (Code "not")
      "是一个过程, 而不是特殊句法. 不过, 老实说, " (Code "not")
      "也将以宏展开的形式实现就是了. 我们选取的Scheme子集的每个过程都有固定的元数 (arity), "
      "原始过程当然不例外, 我们会检查应用原始过程时元数是否正确. "
      "至于在最一般的情况下检查元数是否合规, 其实不是简单的问题. "
      "的确可以做一部分简单的情况, 但是我不想增加复杂性了. "
      "老实说, 在句法上区分原始过程应用和非原始过程应用对于之后的pass是有益的, "
      "但是P423里没有这么做. 我怀疑出发点还是在于这使得中间的pass"
      "看起来会更像是普通的Scheme表达式, 因而检查的时候可以直接在Scheme的REPL里"
      "尝试, 不知道Kent Dybvig是不是这样想的. 反正不管怎么说, 我们并没有进行区分. "
      "最后我们来说说" (Code "&lt;datum>") ", 但其实也没有什么可说的了. "
      "序对和向量的记号都是按照Lisp语言的记法来的, 所以Lisper应该都能理解. "
      "注意, 这里没有符号字面量, 因为实现符号需要另外的机制, 其实不算复杂, "
      "但是估计Kent Dybvig认为没有必要教这种东西, 之后学生可以自己学会. "
      "大概要说的就是这么多了.")
   
   ))