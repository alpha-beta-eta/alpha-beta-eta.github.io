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
   (H2 "作业15: " (Code "parse-scheme"))
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
   (P "我们继续来看" (Code "parse-form")
      ". 这一部分所做的事情基本都是自明的. "
      "对于" (Code "and") "和" (Code "or")
      "形式的扩展方式, 每一个Schemer应该都不陌生. "
      "这本质上和利用宏来实现" (Code "and") "和"
      (Code "or") "如出一辙. 在扩展" (Code "or")
      "的时候, 不要忘了使用" (Code "let")
      "绑定引入临时变量以避免重复求值.")
   (CodeB "        ((lambda ,x* . ,exps)
         (unless (set? x*)
           (error 'parse-scheme &quot;invalid formals ~s&quot; x*))
         (let* ((x*^ (map unique-symbol x*))
                (env (append (map cons x* x*^) env))
                (body (make-body exps env)))
           `(lambda ,x*^ ,body)))
        ((let ,bds . ,exps)
         (: bds
            (lambda (x* e*)
              (unless (set? x*)
                (error 'parse-scheme &quot;invalid let LHS vars ~s&quot; x*))
              (let* ((x*^ (map unique-symbol x*))
                     (env^ (append (map cons x* x*^) env))
                     (e* (map (parse env) e*))
                     (body (make-body exps env^)))
                (Let x*^ e* body)))))
        ((letrec ,bds . ,exps)
         (: bds
            (lambda (x* e*)
              (unless (set? x*)
                (error 'parse-scheme &quot;invalid letrec LHS vars ~s&quot; x*))
              (let* ((x*^ (map unique-symbol x*))
                     (env^ (append (map cons x* x*^) env))
                     (e* (map (parse env^) e*))
                     (body (make-body exps env^)))
                (Letrec x*^ e* body)))))
        (,else (error 'parse-scheme &quot;unbound variable ~s&quot; (car exp))))))
  ((parse '()) exp))")
   (P "以上就是" (Code "parse-scheme") "的全部代码了. 这段程序主要是处理绑定结构. "
      "对于这三种绑定结构, 我们都需要使用" (Code "unique-symbol")
      "来引入新的名字, 然后将这些旧名字和新名字之间的绑定加入到环境中. "
      "我们对于" (Code "let") "和" (Code "letrec")
      "的处理几乎是完全一致的, 只是在处理" (Code "e*")
      "那里, " (Code "let") "的环境是" (Code "env")
      ", 而" (Code "letrec") "的环境是" (Code "env^")
      ", 这反映了两者在语义上的不同.")
   (P "让我们来看几个例子. 注意, 变量的编号并不重要, 应该" $alpha "等价地阅读."
      (CodeB "> (parse-scheme
   '(letrec ((even? (lambda (n)
                      (if (= n 0)
                          #t
                          (odd? (- n 1)))))
             (odd? (lambda (n)
                     (if (= n 0)
                         #f
                         (even? (- n 1))))))
      (even? 88)))
'(letrec ((even?.0 (lambda (n.2) (if (= n.2 '0) '#t (odd?.1 (- n.2 '1)))))
          (odd?.1 (lambda (n.3) (if (= n.3 '0) '#f (even?.0 (- n.3 '1))))))
   (even?.0 '88))")
      (CodeB "> (parse-scheme '(and 1 2 3 4 5))
'(if '1 (if '2 (if '3 (if '4 '5 '#f) '#f) '#f) '#f)")
      (CodeB "> (parse-scheme '(or 1 2 3 4 5))
'(let ((t.0 '1))
   (if t.0
     t.0
     (let ((t.1 '2))
       (if t.1 t.1 (let ((t.2 '3)) (if t.2 t.2 (let ((t.3 '4)) (if t.3 t.3 '5))))))))")
      (CodeB "> (parse-scheme
   '(let ((x 0))
      (let ((x (+ x 1)))
        (let ((x (+ x 2)))
          (+ x 3)))))
'(let ((x.0 '0)) (let ((x.1 (+ x.0 '1))) (let ((x.2 (+ x.1 '2))) (+ x.2 '3))))")
      )
   (P "本部分的最后, 我们应该给出" (Code "parse-scheme") "的输出语言的句法."
      (CodeB "&lt;exp> ::= (quote &lt;datum>)
       |  &lt;uvar>
       |  (if &lt;exp> &lt;exp> &lt;exp>)
       |  (set! &lt;uvar> &lt;exp>)
       |  (begin &lt;exp>+)
       |  (lambda (&lt;uvar>*) &lt;exp>)
       |  (let ((&lt;uvar> &lt;exp>)*) &lt;exp>)
       |  (letrec ((&lt;uvar> &lt;exp>)*) &lt;exp>)
       |  (&lt;prim> &lt;exp>*)
       |  (&lt;exp> &lt;exp>*)")
      "我们看到, " (Code "&lt;var>") "变成了"
      (Code "&lt;uvar>") ". 并且, 产生式的种类更少, 也更加规整了.")
   (H2 "作业14: " (Code "convert-complex-datum"))
   (P "这次作业需要编写多个pass, 不过先让我们来完成" (Code "convert-complex-datum")
      ". 这个pass的目的在于将复杂的" (Code "&lt;datum>")
      "转换为简单的" (Code "&lt;immediate>")
      ", 其中" (Code "&lt;immediate>") "的句法为"
      (CodeB "&lt;immediate> ::= ()
             |  &lt;boolean>
             |  &lt;fixnum>")
      "那么其他的" (Code "&lt;datum>") "该怎么办呢? "
      "答曰, 通过某些原始过程的组合得到, 并且在一开始就进行分配. "
      "何谓原始过程的组合, 我们来枚举一些例子."
      (CodeB "'(#t . #f)")
      "将被转化为"
      (CodeB "(cons '#t '#f)")
      (CodeB "'#(#t #f)")
      "将被转化为"
      (CodeB "(let ((vec.0 (make-vector '2)))
  (begin
    (vector-set! vec.0 '0 '#t)
    (vector-set! vec.0 '1 '#f)
    vec.0))")
      "当然, " (Code "vec.0") "这个变量的编号只是意思一下, 不代表实际一定会是这样.")
   (P "既然在一开始就分配这些常量, 我们需要在执行的过程中记录一些绑定. "
      "这些绑定可以随着执行过程一起传递, 也可以定义一个全局变量, "
      "通过赋值来积累这些绑定. 在这里更多是一种口味问题, "
      "我认为没有明显的优劣, 两种风格我都尝试过.")
   (CodeB "(define (convert-datum x)
  (cond ((pair? x)
         (list 'cons (convert-datum (car x)) (convert-datum (cdr x))))
        ((vector? x)
         (let ((l (vector-length x)))
           (if (= l 0)
               '(make-vector '0)
               (let ((v (unique-symbol 'vec)))
                 `(let ((,v (make-vector (quote ,l))))
                    ,(let loop ((i l) (e* (cons v '())))
                       (if (= i 0)
                           (cons 'begin e*)
                           (let ((i (- i 1)))
                             (loop i
                                   (cons
                                    `(vector-set!
                                      ,v (quote ,i)
                                      ,(convert-datum (vector-ref x i)))
                                    e*))))))))))
        (else `(quote ,x))))")
   (P "当然, 不论采用何种风格, 首先我们都需要编写转换过程" (Code "convert-datum")
      ". 这个过程没有什么难的, 就是很直接的结构递归.")
   (P "以下我将展示两种风格, 首先是利用副作用来积累绑定的版本.")
   (CodeB "(define (convert-complex-datum exp)
  (define bindings '())
  (define (add-binding! datum)
    (define t (unique-symbol 't))
    (define d (convert-datum datum))
    (push! (list t d) bindings)
    t)
  (define (convert exp)
    (match exp
      ((quote ,d) (if (or (pair? d) (vector? d))
                      (add-binding! d)
                      exp))")
   (P "这是" (Code "convert-complex-datum")
      "的开头, 我们定义了一个全局的变量" (Code "bindings")
      ". 当我们遇到一个复杂的" (Code "&lt;datum>")
      "时, 就会调用" (Code "add-binding!") ", 作为命名约定的"
      (Code "!") "是在提示我们该过程含有副作用. 它会生成一个新的变量, "
      "然后构造这个变量和转换之后的" (Code "&lt;datum>")
      "之间的绑定, 将其添加到" (Code "bindings")
      "里, 并且最终返回生成的这个变量, 其中" (Code "push!")
      "是一个特殊形式, 可以按照以下方式定义."
      (CodeB "(define-syntax push!
  (syntax-rules ()
    ((_ x l) (set! l (cons x l)))))"))
   (P "实际上主要的逻辑就集中于开头, 剩余的代码只是一些定式而已.")
   (CodeB "      (,uvar (guard (symbol? uvar)) uvar)
      ((if ,q ,a ,e) `(if ,(convert q)
                          ,(convert a)
                          ,(convert e)))
      ((set! ,uvar ,exp) `(set! ,uvar ,(convert exp)))
      ((begin . ,exps) (cons 'begin (map convert exps)))
      ((lambda ,x* ,body) `(lambda ,x* ,(convert body)))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let ((e* (map convert e*))
                  (body (convert body)))
              (Let x* e* body)))))
      ((letrec ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let ((e* (map convert e*))
                  (body (convert body)))
              (Letrec x* e* body)))))
      ((,prim . ,rands)
       (guard (prim? prim))
       `(,prim . ,(map convert rands)))
      ((,rator . ,rands)
       (map convert exp))))
  (let ((exp (convert exp)))
    (if (null? bindings)
        exp
        `(let ,bindings ,exp))))")
   (P "最后, 如果没有绑定, 也没有必要添加空的" (Code "let")
      "就是了. 当然, 这是一个可有可无的优化, "
      "主要目的只是让我看得更顺眼一点. 另外, 还有一点要说, 通过"
      (Code "prim?") "来区分是否是原始应用看起来有点尴尬, "
      "但是既然我们没有设计直接分辨两者的句法, 那么这的确是必要的. "
      "至于如何实现, 没有什么秘密, 读到这里的人应该都明白.")
   (P "接着, 让我们展示不基于副作用来积累绑定的版本, "
      "或者说更函数式的版本. 有的人可能更喜欢这个版本, "
      "这是因为在刚才的版本里, 有些求值顺序是由实现"
      "隐式决定的, 不那么确定或者说具有可预测性. "
      "更函数式的版本允许我们细致地控制求值顺序, "
      "这样不同的Scheme实现也可以显而易见地产生相同的结果.")
   (CodeB "(define (convert-complex-datum exp)
  (define (convert* exp* k)
    (if (null? exp*)
        (k '() '())
        (convert
         (car exp*)
         (lambda (exp bindings0)
           (convert*
            (cdr exp*)
            (lambda (exp* bindings1)
              (k (cons exp exp*)
                 (append bindings0 bindings1))))))))
  (define (convert exp k)
    (match exp
      ((quote ,d) (if (or (pair? d) (vector? d))
                      (let ((t (unique-symbol 't)))
                        (k t `((,t ,(convert-datum d)))))
                      (k exp '())))")
   (P "还是先来看开头, 这奠定了代码的基本结构. "
      "我们使用了所谓的延续传递风格来返回多值, "
      "这只是一种可能的手段, 当然还有其他各种写法. "
      (Code "convert*") "基于" (Code "convert")
      ", 它意图转换一列表达式, 并将得到的绑定合并起来. "
      "当然, 更准确地说, " (Code "convert*") "和"
      (Code "convert") "是一种互递归的关系. "
      "对于" (Code "(quote ,d)")
      "的处理和之前并没有本质区别, 只是我们现在将结果传递给"
      (Q "延续参数") (Code "k") "而已.")
   (CodeB "      (,uvar (guard (symbol? uvar)) (k uvar '()))
      ((if ,q ,a ,e)
       (convert*
        (cdr exp)
        (lambda (qae bindings)
          (k (cons 'if qae) bindings))))
      ((set! ,uvar ,exp)
       (convert exp (lambda (exp bindings)
                      (k `(set! ,uvar ,exp) bindings))))
      ((begin . ,exp*)
       (convert* exp* (lambda (exp* bindings)
                        (k (cons 'begin exp*) bindings))))
      ((lambda ,x* ,body)
       (convert body (lambda (body bindings)
                       (k `(lambda ,x* ,body) bindings))))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (convert*
             e* (lambda (e* bindings0)
                  (convert
                   body (lambda (body bindings1)
                          (k (Let x* e* body)
                             (append bindings0 bindings1)))))))))
      ((letrec ,bds ,body)
       (: bds
          (lambda (x* e*)
            (convert*
             e* (lambda (e* bindings0)
                  (convert
                   body (lambda (body bindings1)
                          (k (Letrec x* e* body)
                             (append bindings0 bindings1)))))))))
      ((,prim . ,rands)
       (guard (prim? prim))
       (convert* rands (lambda (rands bindings)
                         (k (cons prim rands) bindings))))
      ((,rator . ,rands) (convert* exp k))))
  (convert exp (lambda (exp bindings)
                 (if (null? bindings)
                     exp
                     `(let ,bindings ,exp)))))")
   (P "以上是剩下来的代码, 没有什么好说的. "
      "我们通常遵循这样一种模式, 通过"
      (Code "convert") "或者" (Code "convert*")
      "进行转换, 然后对于返回的表达式和绑定加工一下再返回.")
   (P "现在让我们来看一些例子. 当然, 在这之前, 我们需要将"
      (Code "convert-complex-datum") "和"
      (Code "parse-scheme") "连接起来, 通过函数复合"
      (Code "compose") ".")
   (CodeB "(define compil
  (compose convert-complex-datum
           parse-scheme
           ))")
   (P "接着才是例子.")
   (CodeB "> (compil '(letrec ((append (lambda (l1 l2)
                              (if (null? l1)
                                  l2
                                  (cons (car l1)
                                        (append (cdr l1) l2))))))
             (append '(0 1 2) '(3 4 5))))
'(let ((t.3 (cons '0 (cons '1 (cons '2 '())))) (t.4 (cons '3 (cons '4 (cons '5 '())))))
   (letrec ((append.0
             (lambda (l1.1 l2.2)
               (if (null? l1.1) l2.2 (cons (car l1.1) (append.0 (cdr l1.1) l2.2))))))
     (append.0 t.3 t.4)))")
   (CodeB "> (compil '(+ 1 (* 2 3)))
'(+ '1 (* '2 '3))")
   (CodeB "> (compil '(vector-length '#(0 1 2)))
'(let ((t.0
        (let ((vec.1 (make-vector '3)))
          (begin
            (vector-set! vec.1 '0 '0)
            (vector-set! vec.1 '1 '1)
            (vector-set! vec.1 '2 '2)
            vec.1))))
   (vector-length t.0))")
   (CodeB "> (compil ''#(#(0) (1) (#t #f)))
'(let ((t.0
        (let ((vec.1 (make-vector '3)))
          (begin
            (vector-set!
             vec.1
             '0
             (let ((vec.2 (make-vector '1))) (begin (vector-set! vec.2 '0 '0) vec.2)))
            (vector-set! vec.1 '1 (cons '1 '()))
            (vector-set! vec.1 '2 (cons '#t (cons '#f '())))
            vec.1))))
   t.0)")
   (P "本部分的最后, 让我们给出" (Code "convert-complex-datum")
      "的输出语言的句法."
      (CodeB "&lt;exp> ::= (quote &lt;immediate>)
       |  &lt;uvar>
       |  (if &lt;exp> &lt;exp> &lt;exp>)
       |  (set! &lt;uvar> &lt;exp>)
       |  (begin &lt;exp>+)
       |  (lambda (&lt;uvar>*) &lt;exp>)
       |  (let ((&lt;uvar> &lt;exp>)*) &lt;exp>)
       |  (letrec ((&lt;uvar> &lt;exp>)*) &lt;exp>)
       |  (&lt;prim> &lt;exp>*)
       |  (&lt;exp> &lt;exp>*)
&lt;immediate> ::= ()
             |  &lt;boolean>
             |  &lt;fixnum>")
      "实际上句法没有发生很大的变化, 只有" (Code "&lt;datum>")
      "被替换为了" (Code "&lt;immediate>") ".")
   (H2 "作业14: " (Code "uncover-assigned"))
   (P "现在让我们接着完成" (Code "uncover-assigned")
      ", 这个pass实际上是为了" (Code "convert-assignments")
      "作准备, 当然" (Code "purify-letrec")
      "也会用到这里得到的信息. " (Code "uncover-assigned")
      "是为了分析被赋值了的变量, 然后在这些变量被绑定引入的地方"
      "标示出来. 正如刚才所说, " (Code "uncover-assigned")
      "是为了" (Code "convert-assignments")
      "作准备, " (Code "convert-assignments")
      "的目的在于消除赋值. 消除赋值的方法是将" (Code "set!")
      "形式转换为等效的利用" (Code "box")
      "相关函数的表达式. 为什么要消除赋值呢? "
      "这是因为在赋值存在的情况下之后的闭包变换难以进行.")
   (P (Code "uncover-assigned")
      "写起来当然也非常容易, 不过这次我们最好要先明确其输出语言的句法."
      (CodeB "&lt;exp> ::= (quote &lt;immediate>)
       |  &lt;uvar>
       |  (if &lt;exp> &lt;exp> &lt;exp>)
       |  (set! &lt;uvar> &lt;exp>)
       |  (begin &lt;exp>+)
       |  (lambda (&lt;uvar>*) (assigned (&lt;uvar>*) &lt;exp>))
       |  (let ((&lt;uvar> &lt;exp>)*) (assigned (&lt;uvar>*) &lt;exp>))
       |  (letrec ((&lt;uvar> &lt;exp>)*) (assigned (&lt;uvar>*) &lt;exp>))
       |  (&lt;prim> &lt;exp>*)
       |  (&lt;exp> &lt;exp>*)")
      "我们看到, 只有三种绑定结构" (Code "lambda") ", "
      (Code "let") ", " (Code "letrec")
      "的句法发生了变化, 也就是记录了被赋值的变量. 为了明显起见, 这里还使用了"
      (Code "assigned") "进行提示, 但是从功能上来说这可有可无, 只是便于阅读而已.")
   (P "编写" (Code "uncover-assigned") "需要我们分析和积累被赋值的变量, "
      "并自下而上地传递. 当我们碰到绑定结构时, 它会" (Q "截胡")
      "由它引入且被赋值的变量, 这其实有点让人联想到" (Code "lambda")
      "绑定自由变量的过程. 诚然如此, 之后当我们进行闭包变换时, 首先要进行"
      (Code "uncover-free") "来分析自由变量, 而其写法与"
      (Code "uncover-assigned") "如出一辙. 多说一句, 自下而上地分析"
      "也保证了我们分析的被赋值变量和自由变量与绑定结构(的遮盖行为)相适配.")
   (CodeB "(define (uncover-assigned exp)
  (define (uncover* exp*)
    (if (null? exp*)
        (values '() '())
        (let-values (((exp u*) (uncover (car exp*)))
                     ((exp* v*) (uncover* (cdr exp*))))
          (values (cons exp exp*) (U u* v*)))))
  (define (uncover exp)
    (match exp
      ((quote ,i) (values exp '()))
      (,uvar (guard (symbol? uvar)) (values exp '()))
      ((if ,q ,a ,e)
       (let-values (((qae u*) (uncover* (cdr exp))))
         (values (cons 'if qae) u*)))
      ((set! ,uvar ,exp)
       (let-values (((exp u*) (uncover exp)))
         (values `(set! ,uvar ,exp)
                 (set-cons uvar u*))))")
   (P "这是" (Code "uncover-assigned")
      "的开头, 我展示了一种和" (Code "convert-complex-datum")
      "不同的返回多值的方法, 也就是使用" (Code "values")
      ". 这里最值得关注的其实是处理" (Code "(set! ,uvar ,exp)")
      "的部分, " (Code "uvar") "是被赋值的变量, 当然也不要忘记"
      (Code "exp") "的被赋值变量. " (Code "set-cons")
      "和" (Code "U") "类似, 但只是添加一个元素到某个集合中.")
   (CodeB "      ((begin . ,exp*)
       (let-values (((exp* u*) (uncover* exp*)))
         (values (cons 'begin exp*) u*)))
      ((lambda ,x* ,body)
       (let-values (((body u*) (uncover body)))
         (values `(lambda ,x*
                    (assigned ,(I x* u*) ,body))
                 (D u* x*))))
      ((let ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let-values (((e* u*) (uncover* e*))
                         ((body v*) (uncover body)))
              (values (Let x* e* `(assigned ,(I x* v*) ,body))
                      (U u* (D v* x*)))))))
      ((letrec ,bds ,body)
       (: bds
          (lambda (x* e*)
            (let-values (((e* u*) (uncover* e*))
                         ((body v*) (uncover body)))
              (define w* (U u* v*))
              (values (Letrec x* e* `(assigned ,(I x* w*) ,body))
                      (D w* x*))))))")
   (P "接着, 我们应该将注意力集中到三种绑定结构上来: "
      (Code "lambda") ", " (Code "let") ", " (Code "letrec")
      ". 处理这三种构造的代码都是清晰的, 只是我想提请读者注意一下"
      (Code "letrec") "和" (Code "let")
      "的处理方式的确不同, " (Code "letrec")
      "的绑定可以管辖到它的右支的那些表达式, 因而需要将"
      (Code "e*") "的被赋值变量" (Code "u*") "和"
      (Code "body") "的被赋值变量" (Code "v*")
      "并为" (Code "w*") ".")
   (CodeB "      ((,prim . ,rands)
       (guard (prim? prim))
       (let-values (((rands u*) (uncover* rands)))
         (values `(,prim . ,rands) u*)))
      ((,rator . ,rands) (uncover* exp))))
  (let-values (((exp u*) (uncover exp)))
    (if (null? u*)
        exp
        (error 'uncover-assigned &quot;unbound assigned variables ~s&quot; u*))))")
   (P "收尾的部分并不那么有趣, 只是我们需要注意, 我们并不期望存在未被绑定的被赋值变量. "
      "当然了, 一般来说这不太可能, 因为未被绑定的变量在" (Code "parse-scheme")
      "那里就被拦截下来了. 不过, 层层设防还是有好处的, 因为笔误是永远也无法排除的.")
   (P "说点无关紧要的话, 就是我们的转换是" (Q "保守") "的. 何谓保守, "
      "就是我们没有试图分析什么样的赋值是在" (Em "运行时")
      "真正可达的, 而是将所有具有嫌疑的变量都一网打尽. "
      "当然, 根据可计算理论的经典结果, 想要完美地进行这种分析在一般情况下是不可能的. "
      "因此, 其实绝大多数编译器中的pass都是保守的. "
      "不过, 的确我们在之后是可以有机会去优化一下的, 不过现在不是好时机.")
   (P "以下是一些例子, 当然记得更新" (Code "compil") "的定义.")
   (CodeB "> (compil '(let ((counter (let ((x 0))
                            (lambda ()
                              (set! x (+ x 1))
                              x))))
             (counter)
             (counter)
             (counter)))
'(let ((counter.0
        (let ((x.1 '0))
          (assigned
           (x.1)
           (lambda () (assigned () (begin (set! x.1 (+ x.1 '1)) x.1)))))))
   (assigned () (begin (counter.0) (counter.0) (counter.0))))")
   (CodeB "> (compil '(letrec ((append (lambda (l1 l2)
                              (if (null? l1)
                                  l2
                                  (cons (car l1)
                                        (append (cdr l1) l2))))))
             (append '(0 1 2) '(3 4 5))))
'(let ((t.3 (cons '0 (cons '1 (cons '2 '()))))
       (t.4 (cons '3 (cons '4 (cons '5 '())))))
   (assigned
    ()
    (letrec ((append.0
              (lambda (l1.1 l2.2)
                (assigned
                 ()
                 (if (null? l1.1)
                   l2.2
                   (cons (car l1.1) (append.0 (cdr l1.1) l2.2)))))))
      (assigned () (append.0 t.3 t.4)))))")
   (H2 "作业14: " (Code "purify-letrec"))
   (P "老实说, 我并不真正理解" (Code "purify-letrec")
      ". 但是, 既然它将要做的事情描述得很明确, 只是写一写没有多大问题.")
   (P (Code "letrec") "的问题在于它太灵活了, 其实对它稍作限制, "
      "也不会影响任何常见程序的表达. 不过, 既然Scheme的原则就是消除限制, "
      "那么我就不得不严肃一点思考怎么处理" (Code "letrec") ".")
   (P "如果一个Scheme实现不提供" (Code "letrec")
      ", 那么我们可以通过宏来实现" (Code "letrec")
      ", 其中最简单但也最低效的转换方式是将"
      (CodeB "(letrec ((x e) ...) body)")
      "变换为"
      (CodeB "(let ((x (void)) ...)
  (let ((t e) ...)
    (set! x t)
    ...
    body))")
      "其中诸" (Code "t") "是新的变量. 为什么说这种方式低效呢? "
      "因为它会阻碍之后的优化.")
   (P (Code "letrec") "并非洪水猛兽, 对于最简单纯粹的" (Code "letrec")
      ", 我们想直接保留其形式. 何谓纯粹呢? 也就是说, "
      (Code "letrec") "的右支诸表达式均为" (Code "lambda")
      "表达式, 并且由该" (Code "letrec") "引入的诸变量均未得到赋值, 例如"
      (CodeB "(letrec ((even? (lambda (n)
                  (if (= n 0)
                      #t
                      (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0)
                     #f
                     (even? (- n 1))))))
  (even? 88))"))
   (P "现在让我们来详细描述作业14里的" (Code "purify-letrec")
      "的所作所为. 首先, " (Code "letrec") "的右支诸表达式和体"
      "都会递归地应用这个转换. 接着, 我们将" (Code "letrec")
      "的绑定分为三种情况, 分别是简单, lambda, 复杂. "
      "lambda我们已经说过了, 也就是右支是一个" (Code "lambda")
      "且其绑定至的变量未被赋值. 至于简单和复杂之分, 还是让我引用作业的原文吧."
      (Blockquote
       "A simple expression contains no occurrences of the variables bound by the "
       (Code "letrec") " expression and no applications unless nested within " (Code "lambda")
       " expressions. The latter constraint prevents simple expressions from reaching a call to "
       (Code "call/cc") " if we ever add " (Code "call/cc")
       " to our language. Of course, at that time we would also "
       "rule out primitive calls to " (Code "call/cc")
       " itself. It would also make sense to disallow " (Code "letrec")
       " expressions, to prevent this pass from becoming nonlinear, and to disallow "
       "other expressions, such as " (Code "lambda")
       " expressions, to reduce the cost of the " (Q "simple")
       " check. Use your own judgement on this as long as you do "
       "treat as simple constants, references to variables not bound by "
       "the letrec, and primitive calls with simple operands.")
      "可以看到, 对于简单表达式的判定存在一定的灵活空间. 不过, 简单绑定还不完全等同于"
      "简单表达式, 它还需要被绑定至的变量没有被赋值. 若是一个绑定不满足这两个条件中的任何一个, "
      "那么它就是一个复杂绑定.")
   
   ))