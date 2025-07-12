#lang racket
(provide fear-of-macros.html)
(require SMathML)
(define fear-of-macros.html
  (TnTmPrelude
   #:title "对于宏的恐惧"
   #:css "styles.css"
   (H1. "对于宏的恐惧")
   (P "这只是我阅读"
      (A "Fear of Macros"
         #:attr*
         '((href "https://www.greghendershott.com/fear-of-macros/index.html")))
      "记下的笔记.")
   (H2. "前言")
   (H2. "我们的作战计划")
   (H2. "变换!")
   (H3. "什么是一个句法转换器?")
   (P "简而言之, 一个句法转换器接受句法而返回句法. "
      "换言之, 其对于句法进行变换.")
   (P "以下是一个转换器的例子, 其直接忽略了输入的句法."
      (CodeB "> (define-syntax foo
    (lambda (stx)
      (syntax &quot;I am foo&quot;)))
> (foo)
&quot;I am foo&quot;")
      "使用" (Code "define-syntax")
      "创建了转换器的" (Em "绑定") ".")
   (P "恰如" (Code "define") ", "
      (Code "define-syntax") "也有类似的句法糖."
      (CodeB "> (define-syntax (also-foo stx)
    (syntax &quot;I am also foo&quot;))
> (also-foo)
&quot;I am also foo&quot;"))
   (P "恰如" (Code "'") "是" (Code "quote")
      "的简记法, " (Code "#'") "是" (Code "syntax")
      "的简记法."
      (CodeB "> (define-syntax (quoted-foo stx)
    #'&quot;I am also foo, using #' instead of syntax&quot;)
> (quoted-foo)
&quot;I am also foo, using #' instead of syntax&quot;"))
   (P "当然, 返回的句法不仅限于字符串字面量."
      (CodeB "> (define-syntax (say-hi stx)
    #'(displayln &quot;hi&quot;))
> (say-hi)
hi"))
   (H3. "输入的是什么?")
   (P "之前的例子只是直接忽略了输入的句法, "
      "但一般情况下我们总是想要输入的句法转换为别的什么东西. "
      "首先, 让我们来仔细观察一下输入的究竟是什么."
      (CodeB "> (define-syntax (show-me stx)
    (print stx)
    #'(void))
> (show-me '(+ 1 2))
#&lt;syntax:eval:10:0 (show-me (quote (+ 1 2)))>")
      "从中可以看出, 转换器接受的是一个句法对象 (syntax object).")
   (P "一个句法对象除了字面, 还包含了诸多有趣的信息, "
      "例如其位置还有关于词法作用域的东西. "
      "{译注: 因此, 读者会发现这里的(交互所呈现的)句法对象的信息, "
      "大概和自己试验时不太一样.}")
   (P "存在着各种各样可以访问句法对象的函数. "
      "首先, 让我们定义一个句法."
      (CodeB "> (define stx #'(if x (list &quot;true&quot;) #f))
> stx
#&lt;syntax:eval:11:0 (if x (list &quot;true&quot;) #f)>")
      "然后, 以下是一些用于获取源信息的函数."
      (CodeB "> (syntax-source stx)
'eval
> (syntax-line stx)
11
> (syntax-column stx)
0")
      "更有趣的是句法字面本身, 我们可以用" (Code "syntax->datum")
      "将其转换为一个S-expression."
      (CodeB "> (syntax->datum stx)
'(if x (list &quot;true&quot;) #f)")
      "与之相对的是, " (Code "syntax-e") "只往下走一层."
      (CodeB "> (syntax-e stx)
'(#&lt;syntax:eval:11:0 if> #&lt;syntax:eval:11:0 x> "
             "#&lt;syntax:eval:11:0 (list &quot;true&quot;)> #&lt;syntax:eval:11:0 #f>)")
      "还有一个函数叫做" (Code "syntax->list")
      ", 某些时候和" (Code "syntax-e")
      "表现类似, 但其实相当不同."
      (CodeB "> (syntax->list stx)
'(#&lt;syntax:eval:11:0 if> #&lt;syntax:eval:11:0 x> "
             "#&lt;syntax:eval:11:0 (list &quot;true&quot;)> #&lt;syntax:eval:11:0 #f>)"))
   (H3. "实际地对于输入进行变换")
   (P "现在让我们写一个转换器函数, 其将输入的句法颠倒."
      (CodeB "> (define-syntax (reverse-me stx)
    (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
> (reverse-me &quot;backwards&quot; &quot;am&quot; &quot;i&quot; values)
&quot;i&quot;
&quot;am&quot;
&quot;backwards&quot;")
      (Code "datum->syntax") "的第一个参数包含了我们想要与输出的句法对象关联的词法上下文信息. "
      "如果其被设置为" (Code "#f") ", 那么就是没有信息.")
   (H3. "编译时和运行时的对比")
   (P "通常的Racket代码运行在运行时, 这是显而易见的. "
      "但是, 转换器被Racket调用是parse, expand, 编译程序这一过程的组成部分. "
      "换言之, 句法转换器函数在编译时被求值. 当然, 也有人会说"
      (Q "句法阶段") "和" (Q "运行时阶段") ", 只不过是相同概念的不同说法而已.")
   (P "先让我们回顾一下为什么要有宏的存在. "
      "一个经典的例子可能是Racket的" (Code "if") "形式."
      (CodeB "(if &lt;condition> &lt;true-expression> &lt;false-expression>)")
      "如果我们将" (Code "if") "实现为一个函数, "
      "那么所有的参数都会在提供给函数之前被求值."
      (CodeB "> (define (our-if condition true-expr false-expr)
    (cond [condition true-expr]
          [else false-expr]))
> (our-if #t
          &quot;true&quot;
          &quot;false&quot;)
&quot;true&quot;")
      "似乎这能够成立, 然而请看以下交互."
      (CodeB "> (define (display-and-return x)
    (displayln x)
    x)
> (our-if #t
          (display-and-return &quot;true&quot;)
          (display-and-return &quot;false&quot;))
true
false
&quot;true&quot;")
      "这暗示我们" (Code "if") "并不可能是一个平然的函数. "
      "然而, 句法转换器可以帮助我们完成, "
      "因为其会在编译时对于句法进行重写, "
      "但直到运行时并不会实际进行求值."
      (CodeB "> (define-syntax (our-if-v2 stx)
    (define xs (syntax->list stx))
    (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                              [else ,(cadddr xs)])))
> (our-if-v2 #t
             (display-and-return &quot;true&quot;)
             (display-and-return &quot;false&quot;))
true
&quot;true&quot;
> (our-if-v2 #f
             (display-and-return &quot;true&quot;)
             (display-and-return &quot;false&quot;))
false
&quot;false&quot;"))
   (P "这给出了正确的答案, 但它是如何运作的呢? "
      "让我们抽出转换器本身, 看看它到底做了什么. "
      "首先, 让我们从一个输入句法作为例子开始."
      (CodeB "> (define stx #'(our-if-v2 #t &quot;true&quot; &quot;false&quot;))
> (displayln stx)
#&lt;syntax:eval:32:0 (our-if-v2 #t &quot;true&quot; &quot;false&quot;)>")
      "1. 我们取原始的句法, 使用" (Code "syntax->list")
      "以将其转换为一个句法对象的列表."
      (CodeB "> (define xs (syntax->list stx))
> (displayln xs)
(#&lt;syntax:eval:32:0 our-if-v2> #&lt;syntax:eval:32:0 #t> "
             "#&lt;syntax:eval:32:0 &quot;true&quot;> #&lt;syntax:eval:32:0 &quot;false&quot;>)")
      "2. 为了将其转换为" (Code "cond")
      "形式, 我们需要从列表中取出我们所感兴趣的三个部分, "
      "通过使用" (Code "cadr") ", " (Code "caddr") ", " (Code "cadddr")
      ", 之后的安排则是顺理成章的."
      (CodeB "`(cond [,(cadr xs) ,(caddr xs)]
       [else ,(cadddr xs)])")
      "3. 最后, 我们使用" (Code "datum->syntax")
      "以将其转换回为一个句法对象."
      (CodeB "> (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)]))
#&lt;syntax (cond (#t &quot;true&quot;) (else &quot;false&quot;))>")
      "我们大概已经明白了其工作的流程, 然而使用这些函数来解构列表"
      "并不是很清晰自然的事情, 而且也很容易出错. "
      "因此, 我们想到可以使用Racket提供的模式匹配机制"
      (Code "match") ".")
   (P "我们想着与其写成"
      (CodeB "> (define-syntax (our-if-v2 stx)
    (define xs (syntax->list stx))
    (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                              [else ,(cadddr xs)])))")
      "不如写成"
      (CodeB "> (define-syntax (our-if-using-match stx)
    (match (syntax->list stx)
      [(list name condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]))")
      "然后, 我们会遇到"
      (CodeB "> (our-if-using-match #t &quot;true&quot; &quot;false&quot;)
match: undefined;
 cannot reference an identifier before its definition
  in module: 'program")
      "其在抱怨" (Code "match") "并没有被定义.")
   (P "我们的转换器函数在编译时工作, 然而编译时能用的只是"
      (Code "racket/base") ", 并非完整的Racket语言. "
      "为了使用超出" (Code "racket/base")
      "的东西, 我们需要" (Code "require") "的"
      (Code "for-syntax") "形式."
      (CodeB "> (require (for-syntax racket/match))
> (define-syntax (our-if-using-match-v2 stx)
    (match (syntax->list stx)
      [(list _ condition true-expr false-expr)
       (datum->syntax stx `(cond [,condition ,true-expr]
                                 [else ,false-expr]))]))
> (our-if-using-match-v2 #t &quot;true&quot; &quot;false&quot;)
&quot;true&quot;"))
   (H3. (Code "begin-for-syntax"))
   (P "之前我们已经用了" (Code "for-syntax") "来" (Code "require")
      "需要在编译时用到的" (Code "racket/match")
      "模块. 不过, 显然有时我们也需要自己定义宏所需的辅助函数. "
      "通常的" (Code "define") "无法解决问题, "
      "因为正如你所知的, 其出现于运行时而非编译时. "
      "然而, 我们可以使用" (Code "begin-for-syntax") "."
      (CodeB "(begin-for-syntax
 (define (my-helper-function ....)
   ....))
(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)")
      "其实也可以使用" (Code "define-for-syntax") "."
      (CodeB "(define-for-syntax (my-helper-function ....)
  ....)
(define-syntax (macro-using-my-helper-function stx)
  (my-helper-function ....)
  ....)"))
   (H2. "模式匹配: " (Code "syntax-case") "和" (Code "syntax-rules"))
   (P "绝大部分有用的句法转换器都是对于输入句法的组成部分的重新排列, "
      "因此我们应该使用模式匹配. 实际上, "
      "Racket本来就有基于模式匹配的宏系统, "
      "例如" (Code "syntax-rules") "和" (Code "syntax-case")
      ". {译注: Schemer都知道, 不只是Racket提供这两个, "
      "其他许多Scheme实现也会提供这两个. 另外, "
      "原文说Racket的" (Code "define-syntax-rule")
      "是基于" (Code "syntax-case") "的, 这种说法有点问题, "
      "因为实际上它是基于" (Code "syntax-rules") "的.}")
   (P "基于" (Code "syntax-case") ", 我们之前的例子"
      (CodeB "(require (for-syntax racket/match))
(define-syntax (our-if-using-match-v2 stx)
  (match (syntax->list stx)
    [(list _ condition true-expr false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))")
      "可以写成"
      (CodeB "> (define-syntax (our-if-using-syntax-case stx)
    (syntax-case stx ()
      [(_ condition true-expr false-expr)
       #'(cond [condition true-expr]
               [else false-expr])]))
> (our-if-using-syntax-case #t &quot;true&quot; &quot;false&quot;)
&quot;true&quot;")
      "这看起来其实和之前的也相当类似, "
      "但是免去了使用" (Code "datum->syntax")
      "还有准引用. 当然, 这种情况下使用"
      (Code "define-syntax-rule") "也差不多, "
      "甚至还更简单一点."
      (CodeB "> (define-syntax-rule (our-if-using-syntax-rule condition true-expr false-expr)
    (cond [condition true-expr]
          [else false-expr]))
> (our-if-using-syntax-rule #t &quot;true&quot; &quot;false&quot;)
&quot;true&quot;"))
   (H3. "模式变量vs.模板&mdash;&mdash;fight!")
   (P "Racket的" (Code "struct") "可以完成一种有趣的事情, 即例如"
      (CodeB "(struct foo (field1 field2))")
      "那么其会生成一些新的名字, 包括"
      (Code "foo-field1") ", " (Code "foo-field2")
      ", " (Code "foo?") ".")
   (P "让我们也用宏做点类似的事情. 比如说, 我们想要将句法"
      (Code "(hyphen-define a b (args) body)") "转换为"
      (Code "(define (a-b args) body)") ".")
   (P "以下是一次错误的尝试."
      (CodeB "> (define-syntax (hyphen-define/wrong1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (let ([name (string->symbol (format &quot;~a-~a&quot; a b))])
         #'(define (name args ...)
             body0 body ...))]))
eval:47:0: a: pattern variable cannot be used outside of a
template
  in: a")
      "当然, 现在读者还无法理解错误信息的含义. "
      "不过, 我要说明所谓的模板 (template) 指的是诸如"
      (Code "#'(define (name args ...) body0 body ...)")
      "这样的部分. 那么, 看起来像是我们在这" (Code "let")
      "绑定里不能使用" (Code "a") " (或者" (Code "b") ").")
   (P "实际上, " (Code "syntax-case")
      "里你想用多少模板就可以有多少. "
      "虽然我们不能直接使用模式变量, "
      "但是我们可以将其置于" (Code "#'")
      "之中. 也就是说, 我们或许可以尝试一下如下定义."
      (CodeB "> (define-syntax (hyphen-define/wrong1.1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (let ([name (string->symbol (format &quot;~a-~a&quot; #'a #'b))])
         #'(define (name args ...)
             body0 body ...))]))
> (hyphen-define/wrong1.1 foo bar () #t)
> (foo-bar)
foo-bar: undefined;
 cannot reference an identifier before its definition
  in module: 'program")
      "如你所见, 虽然定义时没有出错, 但是仍然并不符合我们的预期.")
   (P "实际上, 如果你使用宏步进器, 就会发现"
      (CodeB "(hyphen-define/wrong1.1 foo bar () #t)")
      "会被扩展为"
      (CodeB "(define (name) #t)")
      "而非我们预期的"
      (CodeB "(define (foo-bar) #t)")
      "我们的模板使用了符号" (Code "name")
      ", 但是我们期望其能够使用" (Code "name") "的值.")
   (P "回想一下, 有什么出现在模板里的变量是使用其值的呢? "
      "显然, 模式变量是我们已知的答案. "
      "或许我们可以想象一种变通的解决方案, "
      "也就是使用两次" (Code "syntax-case")
      ", 其中一次用于创建模式变量" (Code "name") "."
      (CodeB "> (define-syntax (hyphen-define/wrong1.2 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (syntax-case (datum->syntax #'a
                                   (string->symbol (format &quot;~a-~a&quot; #'a #'b)))
                    ()
         [name #'(define (name args ...)
                   body0 body ...)])]))
> (hyphen-define/wrong1.2 foo bar () #t)
> (foo-bar)
foo-bar: undefined;
 cannot reference an identifier before its definition
  in module: 'program")
      "这个定义看起来有点奇怪, 虽然看起来也很合理. "
      "但是, 仍然并不正确.")
   (P "或许我们应该继续使用宏步进器看看我们的问题究竟出在了哪里. "
      "实际上, 我们的"
      (CodeB "(hyphen-define/wrong1.2 foo bar () #t)")
      "被转换为了"
      (CodeB "(define (|#&lt;syntax:11:24foo>-#&lt;syntax:11:28 bar>|) #t)")
      "这下真相大白了, 问题在于通过" (Code "#'a") "和"
      (Code "#'b") ", 我们得到的其实是句法对象. "
      "因此, 为了达成我们的预期, 我们应该使用"
      (Code "syntax->datum") "进行转换."
      (CodeB "> (define-syntax (hyphen-define/ok1 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (syntax-case (datum->syntax #'a
                                   (string->symbol (format &quot;~a-~a&quot;
                                                           (syntax->datum #'a)
                                                           (syntax->datum #'b))))
                    ()
         [name #'(define (name args ...)
                   body0 body ...)])]))
> (hyphen-define/ok1 foo bar () #t)
> (foo-bar)
#t")
      "终于结束了! 接下来我们要引入一些帮助我们编写宏的简便方法.")
   (H4. (Code "with-syntax"))
   (P "与其使用嵌套的" (Code "syntax-case")
      ", 其实我们可以使用" (Code "with-syntax")
      ". 从某种意义上说, " (Code "with-syntax")
      "长得有点像" (Code "let") "."
      (CodeB "> (define-syntax (hyphen-define/ok2 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ([name (datum->syntax #'a
                                          (string->symbol (format &quot;~a-~a&quot;
                                                                  (syntax->datum #'a)
                                                                  (syntax->datum #'b))))])
         #'(define (name args ...)
             body0 body ...))]))
> (hyphen-define/ok2 foo bar () #t)
> (foo-bar)
#t")
      "实际上, " (Code "with-syntax") "可以看成是一种"
      (Code "syntax-case") "的句法糖."
      (CodeB "(with-syntax ([&lt;pattern> &lt;syntax>]) &lt;body>)")
      "差不多等价于"
      (CodeB "(syntax-case &lt;syntax> () [&lt;pattern> &lt;body>])")
      "所以这不是什么魔法!")
   (H4. (Code "with-syntax*"))
   (P (Code "with-syntax*") "之于" (Code "with-syntax")
      ", 就像" (Code "let*") "之于" (Code "let") "."
      (CodeB "> (require (for-syntax racket/syntax))
> (define-syntax (foo stx)
    (syntax-case stx ()
      [(_ a)
       (with-syntax* ([b #'a]
                      [c #'b])
         #'c)]))")
      "读者需要注意的是, " (Code "with-syntax*")
      "并没有由" (Code "racket/base") "提供, 所以你需要"
      (Code "(require (for-syntax racket/syntax))")
      ". {译注: 原文所说的令人困惑的错误消息, 现在已经变了, "
      "变得相当正常:"
      (CodeB "> (define-syntax (foo stx)
    (syntax-case stx ()
      [(_ a)
       (with-syntax* ([b #'a]
                      [c #'b])
         #'c)]))
> (foo 0)
with-syntax*: undefined;
 cannot reference an identifier before its definition"))
   (H4. (Code "format-id"))
   (P (Code "racket/syntax") "里面存在着一个叫做" (Code "format-id")
      "的辅助函数, 其可以帮助我们更优雅地生成想要的标识符."
      (CodeB "> (require (for-syntax racket/syntax))
> (define-syntax (hyphen-define/ok3 stx)
    (syntax-case stx ()
      [(_ a b (args ...) body0 body ...)
       (with-syntax ([name (format-id #'a &quot;~a-~a&quot; #'a #'b)])
         #'(define (name args ...)
             body0 body ...))]))
> (hyphen-define/ok3 bar baz () #t)
> (bar-baz)
#t")
      "这免去了之前我们所体验到的诸多繁琐.")
   (P (Code "format-id") "的一个参数是词法上下文, "
      "一般来说读者应该不会想在这里填上" (Code "stx")
      ", 而会是更加特化的上下文信息, 例如这里是"
      (Code "#'a") ".")
   (H4. "另一个例子")
   (P "以下是一个变种, 其可以接受多个名称部分, "
      "而将它们以连字号连接."
      (CodeB "> (require (for-syntax racket/string racket/syntax))
> (define-syntax (hyphen-define* stx)
    (syntax-case stx ()
      [(_ (names ...) (args ...) body0 body ...)
       (let ([name-stxs (syntax->list #'(names ...))])
         (with-syntax ([name (datum->syntax (car name-stxs)
                                            (string->symbol
                                             (string-join (for/list ([name-stx name-stxs])
                                                            (symbol->string
                                                             (syntax-e name-stx)))
                                                          &quot;-&quot;)))])
           #'(define (name args ...)
               body0 body ...)))]))
> (hyphen-define* (foo bar baz) (v) (* 2 v))
> (foo-bar-baz 50)
100")
      "或许这里最值得注意的地方是我们提供给"
      (Code "datum->syntax") "的词法上下文参数是什么.")
   (H3. "制作我们自己的" (Code "struct"))
   (P "现在让我们应用我们所学到的东西于一个更为实际的例子. "
      "我们将会实现一个类似于" (Code "struct") "的机制, 但远为简化.")
   (P "对于以下的结构声明:"
      (CodeB "(our-struct name (field1 field2 ...))")
      "我们需要定义以下的一些过程:"
      (Ul (Li "一个构造子, 其名字和结构的名字相同. "
              "我们将会表示结构以" (Code "vector")
              ", 而结构名出现在第零位置.")
          (Li "一个谓词, 其名字是结构名附加以" (Code "?") ".")
          (Li "对于每个field, 都有一个访问子以获得其值. "
              "其命名和Racket的" (Code "struct")
              "保持一致, 也就是结构名和field名之间以连字号连接."))
      (CodeB "> (require (for-syntax racket/syntax))
> (define-syntax (our-struct stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       (with-syntax ([pred-id (format-id #'id &quot;~a?&quot; #'id)])
         #`(begin
             ; Define a constructor.
             (define (id fields ...)
               (apply vector (cons 'id  (list fields ...))))
             ; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))
             ; Define an accessor for each field.
             #,@(for/list ([x (syntax->list #'(fields ...))]
                           [n (in-naturals 1)])
                  (with-syntax ([acc-id (format-id #'id &quot;~a-~a&quot; #'id x)]
                                [ix n])
                    #`(define (acc-id v)
                        (unless (pred-id v)
                          (error 'acc-id &quot;~a is not a ~a struct&quot; v 'id))
                        (vector-ref v ix))))))]))
; Test it out
> (require rackunit)
> (our-struct foo (a b))
> (define s (foo 1 2))
> (check-true (foo? s))
> (check-false (foo? 1))
> (check-equal? (foo-a s) 1)
> (check-equal? (foo-b s) 2)
> (check-exn exn:fail?
             (lambda () (foo-a &quot;furble&quot;)))
; The tests passed.
; Next, what if someone tries to declare:
> (our-struct &quot;blah&quot; (&quot;blah&quot; &quot;blah&quot;))
format-id: contract violation
  expected: (or/c string? symbol? identifier? keyword? char?
number?)
  given: #&lt;syntax:eval:83:0 &quot;blah&quot;>")
      "这里的错误信息不是很有用, 因为其是来源于"
      (Code "format-id") "的, 算是一种实现细节.")
   (P "你可能听说过" (Code "syntax-case")
      "的语句可以包含一个可选的" (Q "guard")
      "或者说" (Q "fender")
      "表达式. 一个语句不仅可能是"
      (CodeB "[&lt;pattern> &lt;template>]")
      "还可能是"
      (CodeB "[&lt;pattern> &lt;guard> &lt;template>]")
      "让我们为" (Code "our-struct") "添加guard表达式."
      (CodeB "> (require (for-syntax racket/syntax))
> (define-syntax (our-struct stx)
    (syntax-case stx ()
      [(_ id (fields ...))
       ; Guard or &quot;fender&quot; expression:
       (for-each (lambda (x)
                   (unless (identifier? x)
                     (raise-syntax-error #f &quot;not an identifier&quot; stx x)))
                 (cons #'id (syntax->list #'(fields ...))))
       (with-syntax ([pred-id (format-id #'id &quot;~a?&quot; #'id)])
         #`(begin
             ; Define a constructor.
             (define (id fields ...)
               (apply vector (cons 'id  (list fields ...))))
             ; Define a predicate.
             (define (pred-id v)
               (and (vector? v)
                    (eq? (vector-ref v 0) 'id)))
             ; Define an accessor for each field.
             #,@(for/list ([x (syntax->list #'(fields ...))]
                           [n (in-naturals 1)])
                  (with-syntax ([acc-id (format-id #'id &quot;~a-~a&quot; #'id x)]
                                [ix n])
                    #`(define (acc-id v)
                        (unless (pred-id v)
                          (error 'acc-id &quot;~a is not a ~a struct&quot; v 'id))
                        (vector-ref v ix))))))]))
; Now the same misuse gives a better error message:
> (our-struct &quot;blah&quot; (&quot;blah&quot; &quot;blah&quot;))
eval:86:0: our-struct: not an identifier
  at: &quot;blah&quot;
  in: (our-struct &quot;blah&quot; (&quot;blah&quot; &quot;blah&quot;))")
      "之后, 我们将会看到" (Code "syntax-parse")
      "做类似的事情将会更加容易.")
   (H3. "为嵌套哈希查找使用点记号")
   (P "之前的例子将一些标识符连接起来形成新的标识符, "
      "而这里我们要做相反的事情: 将标识符拆成数个部分.")
   (P "其他的语言中你经常可以看到点记法, 例如在JavaScript中使用JSON. "
      "迭代使用点记法在Racket中的等价物往往是繁琐的, 例如"
      (CodeB "foo = js.a.b.c;")
      "可能要写成"
      (CodeB "(hash-ref (hash-ref (hash-ref js 'a) 'b) 'c)")
      "或许我们可以编写一个辅助函数, 使得类似的事情变得更为容易和清晰."
      (CodeB "; This helper function:
> (define/contract (hash-refs h ks [def #f])
    ((hash? (listof any/c)) (any/c) . ->* . any)
    (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                            [else def]))])
      (for/fold ([h h])
        ([k (in-list ks)])
        (hash-ref h k))))
; Lets us say:
> (hash-refs js '(a b c))
&quot;value&quot;")
      "这已经是不错了, 但是或许我们还可以用宏做得更好."
      (CodeB "; This macro:
> (require (for-syntax racket/syntax))
> (define-syntax (hash.refs stx)
    (syntax-case stx ()
      ; If the optional ‘default' is missing, use #f.
      [(_ chain)
       #'(hash.refs chain #f)]
      [(_ chain default)
       (let* ([chain-str (symbol->string (syntax->datum #'chain))]
              [ids (for/list ([str (in-list (regexp-split #rx&quot;\\\\.&quot; chain-str))])
                     (format-id #'chain &quot;~a&quot; str))])
         (with-syntax ([hash-table (car ids)]
                       [keys       (cdr ids)])
           #'(hash-refs hash-table 'keys default)))]))
; Gives us &quot;sugar&quot; to say this:
> (hash.refs js.a.b.c)
&quot;value&quot;
; Try finding a key that doesn't exist:
> (hash.refs js.blah)
#f
; Try finding a key that doesn't exist, specifying the default:
> (hash.refs js.blah 'did-not-exist)
'did-not-exist")
      "的确可行.")
   (P "现在和之前一样, 我们希望能够使得错误信息提供有用的提示."
      (CodeB "> (require (for-syntax racket/syntax))
> (define-syntax (hash.refs stx)
    (syntax-case stx ()
      ; Check for no args at all
      [(_)
       (raise-syntax-error #f &quot;Expected hash.key0[.key1 ...] [default]&quot; stx)]
      ; If the optional ‘default' is missing, use #f.
      [(_ chain)
       #'(hash.refs chain #f)]
      [(_ chain default)
       (unless (identifier? #'chain)
         (raise-syntax-error #f &quot;Expected hash.key0[.key1 ...] [default]&quot; stx #'chain))
       (let* ([chain-str (symbol->string (syntax->datum #'chain))]
              [ids (for/list ([str (in-list (regexp-split #rx&quot;\\\\.&quot; chain-str))])
                     (format-id #'chain &quot;~a&quot; str))])
         ; Check that we have at least hash.key
         (unless (and (>= (length ids) 2)
                      (not (eq? (syntax-e (cadr ids)) '||)))
           (raise-syntax-error #f &quot;Expected hash.key&quot; stx #'chain))
         (with-syntax ([hash-table (car ids)]
                       [keys       (cdr ids)])
           #'(hash-refs hash-table 'keys default)))]))
; See if we catch each of the misuses
> (hash.refs)
eval:97:0: hash.refs: Expected hash.key0[.key1 ...]
[default]
  in: (hash.refs)
> (hash.refs 0)
eval:98:0: hash.refs: Expected hash.key0[.key1 ...]
[default]
  at: 0
  in: (hash.refs 0 #f)
> (hash.refs js)
eval:99:0: hash.refs: Expected hash.key
  at: js
  in: (hash.refs js #f)
> (hash.refs js.)
eval:100:0: hash.refs: Expected hash.key
  at: js.
  in: (hash.refs js. #f)")
      "还算可以, 但是这种错误处理在某种意义上有点恼人, "
      "使得逻辑的主干部分不那么明晰.")
   (P "说到底, 很难说使用" (Code "hash-refs")
      "还是" (Code "hash.refs") "更加清晰, "
      "但是至少Racket提供了这样的选择.")
   (H2. "句法参数")
   (P (Q "照应if") "是一个流行的宏的例子. 例如, 我们可以将"
      (CodeB "(let ([tmp (big-long-calculation)])
  (if tmp
      (foo tmp)
      #f))")
      "写成"
      (CodeB "(aif (big-long-calculation)
     (foo it)
     #f)")
      "换言之, 当condition为真时, 标识符" (Code "it")
      "会被自动创建并置为condition之值. "
      "似乎定义这个宏可能很简单:"
      (CodeB "> (define-syntax-rule (aif condition true-expr false-expr)
    (let ([it condition])
      (if it
          true-expr
          false-expr)))
> (aif #t (displayln it) (void))
it: undefined;
 cannot reference an identifier before its definition
  in module: 'program")
      "当然了, 这么简单不太可能.")
   
   (H2. (Code "racket/splicing") "的要义为何?")
   
   (H2. "健壮的宏: " (Code "syntax-parse"))
   (H3. "函数的错误处理策略")
   (Ol (Li "完全不处理:"
           (CodeB "> (define (misuse s)
    (string-append s &quot; snazzy suffix&quot;))
; User of the function:
> (misuse 0)
string-append: contract violation
  expected: string?
  given: 0
  argument position: 1st
  other arguments...:
   &quot; snazzy suffix&quot;
; I guess I goofed, but – what is this &quot;string-append&quot; of which you
; speak??"))
       (Li "手工编写错误处理代码:"
           (CodeB "> (define (misuse s)
    (unless (string? s)
      (error 'misuse &quot;expected a string, but got ~a&quot; s))
    (string-append s &quot; snazzy suffix&quot;))
; User of the function:
> (misuse 0)
misuse: expected a string, but got 0
; I goofed, and understand why! It's a shame the writer of the
; function had to work so hard to tell me."))
       (Li "使用合同 (contract):"
           (CodeB "> (define/contract (misuse s)
    (string? . -> . string?)
    (string-append s &quot; snazzy suffix&quot;))
; User of the function:
> (misuse 0)
misuse: contract violation
  expected: string?
  given: 0
  in: the 1st argument of
      (-> string? string?)
  contract from: (function misuse)
  blaming: program
   (assuming the contract is correct)
  at: eval:131.0
; I goofed, and understand why! I'm happier, and I hear the writer of
; the function is happier, too."))
       (Li "使用Typed Racket:"
           (CodeB "#lang typed/racket")
           (CodeB "> (: misuse (String -> String))
> (define (misuse s)
    (string-append s &quot; snazzy suffix&quot;))
> (misuse 0)
eval:3:0: Type Checker: type mismatch
  expected: String
  given: Zero
  in: 0")))
   (H3. "宏的错误处理策略")
   (P "对于宏而言, 我们也有着类似的选择."
      (Ol (Li "完全不处理, 不过在宏的情况下错误信息可能更加糟糕.")
          (Li "手工编写错误处理代码, 但是既给编写者添加了负担, "
              "也给阅读者增添了麻烦.")
          (Li "使用" (Code "syntax-parse")
              ". 对于宏而言, 这相当于使用合同或者类型.")))
   (H3. "使用" (Code "syntax-parse"))
   (P "原作者其实还没有写这个部分就是了, 而是建议阅读"
      (A #:attr* '((href "https://docs.racket-lang.org/syntax/stxparse-intro.html"))
         "Introduction")
      ". "
      )
   (H2. "参考和致谢")
   ))