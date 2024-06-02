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
   
   (H3. (Code "begin-for-syntax"))
   
   (H2. "模式匹配: " (Code "syntax-case") "和" (Code "syntax-rules"))
   (H2. "句法参数")
   (H2. (Code "racket/splicing") "的要义为何?")
   (H2. "健壮的宏: " (Code "syntax-parse"))
   (H2. "参考和致谢")
   ))