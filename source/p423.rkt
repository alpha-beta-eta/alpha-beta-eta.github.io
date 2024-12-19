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
   (P "我现在意识到我几年前写的每个pass都犯了一个共同的错误, "
      "就是没有区分原始过程应用和非原始过程应用. "
      "以我的直觉来看, 这应该会导致一些混乱而不清晰的代码, "
      "比如说很多时候原始应用不必再次进入递归过程之中. "
      "这会导致一个问题, 处理变量的逻辑有时还需要剔除意外而来的原始过程, "
      "这并不符合follow the syntax的精神. "
      "至于是否一定会造成什么问题, 我其实回忆不起来很细节的事情了. "
      "但是印象中, 可能因此出现的问题我应该是以丑陋的方式解决了.")
   (P "现在其实可以动笔开始写了, 不过在这之前, 其实我们还需要写一些辅助过程, "
      "辅助过程被放在" (Code "utils.rkt") "里. 正好, 我们也可以说明一下"
      "编译器整体的结构, 实际上非常简单. 每个编译器pass都是独立(成一个文件)的, "
      "只依赖于" (Code "utils.rkt") ", 然后都会provide一个过程. "
      "最后, 有一个文件将这些pass组合在一起, 形成一个完整的编译器.")
   (P "首先, 我们需要" (Code "unique-symbol")
      "来生成唯一的标识符, 这是通过一个全局的计数器实现的."
      (CodeB "(define (make-counter x)
  (lambda ()
    (set! x (+ x 1))
    x))
(define unique-symbol
  (let ((counter (make-counter -1)))
    (lambda (x)
      (string->symbol
       (format &quot;~s.~s&quot; x (counter))))))")
      "这个标识符实际上很有特点, 它将计数器信息保存在了名字里.")
   (P "其次, 我们需要实现集合. 其实, Racket的确有一个集合数据结构, "
      "而且效率肯定比我们随便用顺序可达的列表实现的集合要高. "
      "但是呢, 现在我们的确不在乎编译的效率, 也没想着测试复杂的输入. "
      "不论如何, 之后都可以再改."
      (CodeB "(define (set? x)
  (cond ((null? x) #t)
        ((memq (car x) (cdr x)) #f)
        (else (set? (cdr x)))))
(define (set-cons x s)
  (if (memq x s)
      s
      (cons x s)))
(define (U s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (U (cdr s1) s2))
        (else (cons (car s1) (U (cdr s1) s2)))))
(define (I s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (cons (car s1) (I (cdr s1) s2)))
        (else (I (cdr s1) s2))))
(define (D s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (D (cdr s1) s2))
        (else (cons (car s1) (D (cdr s1) s2)))))"))
   (P "接着, 让我们想一想我们对于" (Code "&lt;fixnum>")
      "的要求, 写下谓词" (Code "target-fixnum?") "."
      (CodeB "(define (target-fixnum? x)
  (and (integer? x)
       (exact? x)
       (<= (- (expt 2 60)) x (- (expt 2 60) 1))))")
      "当然, 谓词" (Code "datum?") "也不要忘了."
      (CodeB "(define (datum? x)
  (or (null? x) (boolean? x) (target-fixnum? x)
      (and (pair? x) (datum? (car x)) (datum? (cdr x)))
      (and (vector? x)
           (let ((len (vector-length x)))
             (let loop ((i 0))
               (cond ((= len i) #t)
                     ((datum? (vector-ref x i)) (loop (+ i 1)))
                     (else #f)))))))"))
   (P "最后, 让我们写下用来解构" (Code "let") "或"
      (Code "letrec") "绑定的过程, 以及构造" (Code "let")
      "或" (Code "letrec") "构造的过程. 老实说, 没有必要将"
      "中间表示设计得看上去和Scheme长得一样, 但是可能"
      "Kent Dybvig乐意这么做吧. 另外, nanopass本身"
      "提供了方便解构和构造这类东西的机制, 但是我们没有就是了, "
      "所以不得不采取这种看起来比较迂回的方法."
      (CodeB "(define (: bds k)
  (k (map car bds) (map cadr bds)))
(define (Let x* e* body)
  (list 'let (map list x* e*) body))
(define (Letrec x* e* body)
  (list 'letrec (map list x* e*) body))"))
   (P "好了, 差不多就是这样. 如果之后用到什么其他的辅助过程, 那就之后再说. "
      "接下来, 我们终于开始着手写" (Code "parse-scheme") ".")
   (P "在" (Code "parse-scheme") "的内部, 我们会有两个互递归的过程"
      (Code "parse") "和" (Code "parse-form")
      ". 这两者的功能很容易从名字中看出来, " (Code "parse-form")
      "用来parse特殊形式相关的构造, 其余则由" (Code "parse")
      "处理, " (Code "parse") "可以接受一般的" (Code "&lt;exp>")
      "作为参数.")
   (CodeB "(define (parse-scheme exp)
  (define prim-env
    '((+ . 2)
      (- . 2)
      (* . 2)
      (= . 2)
      (&lt; . 2)
      (> . 2)
      (&lt;= . 2)
      (>= . 2)
      (null? . 1)
      (boolean? . 1)
      (fixnum? . 1)
      (pair? . 1)
      (vector? . 1)
      (box? . 1)
      (procedure? . 1)
      (eq? . 2)
      (not . 1)
      (cons . 2)
      (car . 1)
      (cdr . 1)
      (set-car! . 2)
      (set-cdr! . 2)
      (make-vector . 1)
      (vector-length . 1)
      (vector-ref . 2)
      (vector-set! . 3)
      (box . 1)
      (unbox . 1)
      (set-box! . 2)
      (void . 0)))
  (define (make-body exps env)
    (cond ((null? exps) (error 'parse-scheme &quot;empty begin body&quot;))
          ((null? (cdr exps)) ((parse env) (car exps)))
          (else (cons 'begin (map (parse env) exps)))))
  (define (parse env)
    (lambda (exp)
      (match exp
        (,n (guard (number? n)) (if (target-fixnum? n)
                                    `(quote ,n)
                                    (error 'parse-scheme &quot;invalid number ~s&quot; n)))
        (,b (guard (boolean? b)) `(quote ,b))
        (,x (guard (symbol? x))
            (cond ((assq x env) => cdr)
                  ((assq x prim-env)
                   (error 'parse-scheme
                          &quot;primitive ~s should not appear independently&quot; x))
                  (else (error 'parse-scheme &quot;unbound variable ~s&quot; x))))")
   (P (Code "parse-scheme") "起手是这样的, 实际上我们还未触及什么实质性的逻辑. "
      "即便如此, 还是有一些应该解释的东西. " (Code "prim-env")
      "是一个固定的上下文, 在递归过程中不会发生改变. 或许有的人将其当作基础的环境, "
      "在递归中会得到增长, 但从概念上来说这么做是有点混乱的, 不过能达成目的就行. "
      (Code "prim-env") "里记录了原始过程的名字和其对应的元数, 元数是用来检查原始应用表达式的. "
      (Code "env") "和" (Code "prim-env") "不同, 其记录的是旧名字和新名字之间的绑定. "
      "另外, 读者看到, 我们的确不允许原始过程单独以变量的形式的出现. 对于常量, "
      "我们会将其用" (Code "quote") "包裹, 这约简了句法的复杂性 (也就是减少了一些产生式). "
      "至于" (Code "make-body") "的作用, 之后我们才会看到, 它主要是为了将"
      (Code "&lt;exp>+") "都转换为恰好一个" (Code "&lt;exp>")
      ". 多于一个" (Code "&lt;exp>") "的情况下, 我们会将其转换成"
      (Code "begin") "形式, 这是我们所期望的语义.")
   (CodeB "        ((,rator . ,rands)
         (if (symbol? rator)
             (let ((a (assq rator env)))
               (if a
                   (let ((rator (cdr a))
                         (rands (map (parse env) rands)))
                     (cons rator rands))
                   (let ((a (assq rator prim-env)))
                     (if a
                         (let ((arity (cdr a)))
                           (unless (= (length rands) arity)
                             (error 'parse-scheme
                                    &quot;arity mismatch expected ~s given ~s&quot;
                                    arity (length rands)))
                           (let ((rands (map (parse env) rands)))
                             (if (eq? rator 'not)
                                 `(if ,(car rands) '#f '#t)
                                 (cons rator rands))))
                         ((parse-form env) exp)))))
             (map (parse env) exp))))))")
   (P (Code "parse") "的最后一部分还是比较复杂的, 我们需要分类拆解来看. "
      "首先, 在" (Code "rator") "是一个符号的情况下, 实际上有两种可能. "
      "一种可能是原始过程应用, 还有一种可能是非原始过程应用. "
      "我们需要先在环境中去检索这个符号, 如果存在的话, 就说明其应该是一个"
      "非原始过程应用. 这里有一个很自然的地方, 就是如果绑定变量的名字和"
      "原始过程一致, 那么这个绑定就会遮盖原始过程. 因此, 我们不能先在"
      "原始环境中检索符号, 那不符合输入语言的语义. "
      "若是环境中没有这个符号, 那么就在原始环境中寻找. "
      "若是找到了, 就说明这是一个原始过程应用. 并且, 此时我们可以检查"
      "元数的一致性. 这里我们对于" (Code "not") "进行了特殊处理, "
      "对其进行了扩展. 如果环境和原始环境中都找不到这个符号, "
      "说明这个表达式可能是一个特殊形式, 所以接下来的任务就交给" (Code "parse-form")
      "了. 当然, 我们不能排除这个符号是在引用一个未绑定的变量, "
      "不过这件事情当然也交由" (Code "parse-form") "处理. "
      "以上我们只考虑了" (Code "rator") "为一个符号的情况, 实际上"
      (Code "rator") "还有可能是一个表达式, 那么此时肯定是一个非原始的过程应用, "
      "我们以直接的结构递归对待.")
   (CodeB "  (define (parse-form env)
    (lambda (exp)
      (match exp
        ((quote ,d) (if (datum? d)
                        exp
                        (error 'parse-scheme &quot;invalid datum ~s&quot; d)))
        ((if ,q ,a) `(if ,((parse env) q) ,((parse env) a) (void)))
        ((if ,q ,a ,e) `(if ,((parse env) q) ,((parse env) a) ,((parse env) e)))
        ((set! ,x ,e)
         (let ((x (cond ((assq x env) => cdr)
                        ((assq x prim-env)
                         (error 'parse-scheme &quot;can't assign to prim ~s&quot; x))
                        (else (error 'parse-scheme &quot;unbound variable ~s&quot; x))))
               (e ((parse env) e)))
           `(set! ,x ,e)))
        ((begin . ,exps) (make-body exps env))
        ((and . ,exps)
         (if (null? exps)
             ''#t
             (let ((exps (map (parse env) exps)))
               (let loop ((exp (car exps)) (exps (cdr exps)))
                 (if (null? exps)
                     exp
                     `(if ,exp ,(loop (car exps) (cdr exps)) '#f))))))
        ((or . ,exps)
         (if (null? exps)
             ''#f
             (let ((exps (map (parse env) exps)))
               (let loop ((exp (car exps)) (exps (cdr exps)))
                 (if (null? exps)
                     exp
                     (let ((t (unique-symbol 't)))
                       `(let ((,t ,exp))
                          (if ,t ,t ,(loop (car exps) (cdr exps))))))))))")
   (P "我们继续来看" (Code "parse-form") ". "
      )
   ))