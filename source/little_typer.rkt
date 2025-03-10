#lang racket
(provide little_typer.html)
(require SMathML)
(define (Small-caps . x*)
  (keyword-apply
   Span '(#:attr*) '(((style "font-variant: small-caps;")))
   x*))
(define $FROM (Small-caps "from"))
(define $TO (Small-caps "to"))
(define l-1
  (Mi "l-1" #:attr* '((mathvariant "script"))))
(define (Mid str) (Mi #:attr* '((mathvariant "italic")) str))
(define $arg (Mid "arg"))
(define $name (Mid "name"))
(define $expr (Mid "expr"))
(define $target (Mid "target"))
(define $base (Mid "base"))
(define $step (Mid "step"))
(define $es (Mid "es"))
(define $mot (Mid "mot"))
(define $from (Mid "from"))
(define $to (Mid "to"))
;very tricky, difficult to use
(define (C exp #:constant* [constant* '(zero nil vecnil Nat Atom)]
           #:special* [special* '(U l l-1)])
  (define (T exp)
    (match exp
      (,x (guard (symbol? x))
          (define s (symbol->string x))
          (list
           (cond ((memq x constant*) s)
                 ((memq x special*)
                  (Mi s #:attr* '((mathvariant "script"))))
                 (else
                  (if (= (string-length s) 1)
                      (Mi s)
                      (Mid s))))))
      (,n (guard (number? n)) (list (number->string n)))
      ((,op . ,arg*)
       `("("
         ,@(if (symbol? op)
               (list (symbol->string op))
               (T op))
         ,@(if (null? arg*) '()
               (apply
                append
                (cons
                 '(" ")
                 (add-between
                  (map T arg*) '(" ")))))
         ")"))))
  (define (optimize lst)
    (if (null? lst)
        '()
        (let loop ((head (car lst))
                   (tail (cdr lst)))
          (cond ((null? tail) (list head))
                ((string? head)
                 (cond ((string? (car tail))
                        (loop (string-append head (car tail))
                              (cdr tail)))
                       (else
                        (cons head (optimize tail)))))
                (else
                 (cons head (optimize tail)))))))
  (apply Code (optimize (T exp))))
(define CB (compose Pre C))
(define (Dim . html*)
  `(span ((style "color: grey;")) . ,html*))
(define (Frame . html*)
  `(span ((style
           "border: 2px solid white; display: inline-block;"))
         . ,html*))
(define (WB . html*)
  `(span ((style
           "background-color: white; color: black;"))
         . ,html*))
(define (make-entry name class)
  (lambda (#:n [n ""])
    (lambda x*
      (keyword-apply
       Div '(#:attr*) `(((class ,class)))
       (B (format "~a~a." name n)) " " x*))))
(define tcomment (make-entry "译注" "tcomment"))
(define (CodeD . html*)
  (Pre #:attr* '((class "dashed"))
       (apply Code html*)))
(define (heading-present %heading attr* . html*)
  (define level (%heading-level %heading))
  (define auto? (%heading-auto? %heading))
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  (cond ((= level 2) `(h2 ,(attr*-set attr* 'id id)
                          ,(format "第~a章 " (format-section section))
                          . ,html*))
        ((= level 1) `(h1 ,(attr*-set attr* 'id id) . ,html*))
        (else (error 'heading-present "invalid level ~s" level))))
(define (format-section section)
  (define sec (cdr (reverse section)))
  (apply string-append
         (add-between (map number->string sec) ".")))
(define (heading-cite %heading)
  (define id (%heading-id %heading))
  (define href (string-append "#" id))
  (define section (%heading-section %heading))
  `(a ((href ,href)) ,(format-section section)))
(define (H1 #:attr* [attr* '()] . html*)
  `(,(build-%heading #:present heading-present #:level 1)
    ,attr* . ,html*))
(define (H2 #:attr* [attr* '()] #:id [id #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite #:level 2 #:id id)
    ,attr* . ,html*))
(define (H2: #:attr* [attr* '()] . html*)
  `(,(build-%heading #:level 2 #:auto? #f)
    ,attr* . ,html*))
(define ((law) . html*)
  (Table #:attr* '((class "law"))
         (Tr (Td (apply Blockquote html*)))))
(define ((dialogue #:attr* [attr* '()] #:id [id #f]) left right)
  (list (build-%entry #:class "dialogue" #:id id
                      #:present dialogue-present
                      #:cite dialogue-cite)
        attr* left right))
(define (dialogue-present %entry attr* left right)
  (define index (%entry-index %entry))
  (define id (%entry-id %entry))
  (define numbering
    (Td #:attr* '((class "middled"))
        (Sup (number->string index))))
  (Table #:attr* (attr*-set attr* 'class "dialogue" 'id id)
         (Tr left numbering right)))
(define (dialogue-cite %entry)
  (define id (%entry-id %entry))
  (define href (string-append "#" id))
  (define index (%entry-index %entry))
  (define chapter (car (%entry-section %entry)))
  `(a ((href ,href)) ,(format "第~s章第~s框" chapter index)))
(define (Ld . html*)
  (keyword-apply
   Td '(#:attr*) '(((class "leftd")))
   html*))
(define (Rd . html*)
  (keyword-apply
   Td '(#:attr*) '(((class "rightd")))
   html*))
(define (Center . html*)
  `(div ((style "text-align: center;")) . ,html*))
(define (CodeI . html*)
  (set-attr*
   (apply CodeB html*)
   'style "display: inline-block"))
(define (same-as #:attr* [attr* '()] . x*)
  (keyword-apply
   Ol '(#:attr*) (list attr*)
   (map (lambda (x) (Li "| " x)) x*)))
(define little_typer.html
  (TnTmPrelude
   #:title "The Little Typer"
   #:css "styles.css"
   (H1 "The Little Typer")
   (H2 "愈是变化, 愈是不变" #:id "ch1")
   ((dialogue)
    (Ld "欢迎回来!")
    (Rd "很高兴回到这里!"))
   ((dialogue)
    (Ld "让我们为一门叫做Pie的新语言擦拭并更新一些我们的旧玩具." (Br)
        "以下东西是一个" (Code "Atom") ", 这显然吗?"
        (CodeB "'atom"))
    (Rd "一点也不, " (Code "Atom") "是什么意思?"))
   ((dialogue)
    (Ld "要成为一个" (Code "Atom") ", 就是要成为一个"
        (Em "原子(atom)") "."
        ((comment)
         "在Lisp中, 原子可以是符号, 数字, 以及其他许多东西. "
         "但在这里, 原子只是符号."))
    (Rd "那么, " (Code "'atom") "是一个" (Code "Atom")
        "是因为" (Code "'atom") "是一个原子."))
   ((dialogue)
    (Ld "以下东西是一个" (Code "Atom") ", 这显然吗?"
        (CodeB "'ratatouille")
        ((tcomment)
         "ratatouille指的是普罗旺斯杂烩."))
    (Rd "是的, 因为" (Code "'ratatouille") "也是一个原子."
        (P "但是, 精确地说, 什么是一个原子呢?")))
   ((dialogue #:id "atom-definition")
    (Ld "原子是由一个单引号 (tick mark) 后跟着一个或更多的字母或者连字符而来的."
        ((comment)
         "在Pie中, 只有原子使用单引号."))
    (Rd "因此, 那么"
        (CodeB "'is-it-obvious-that-this-is-an-atom")
        "显然是一个" (Code "Atom") "吗?"))
   ((dialogue)
    (Ld "当然了, 因为原子可以包含连字符." (Br)
        "那么"
        (CodeB "'---")
        "和"
        (CodeB "---")
        "和"
        (CodeB "'")
        "如何, 它们是原子吗?")
    (Rd (CodeB "'---")
        "是一个原子, 因为连字符可以出现在一个原子里的任何位置;"
        (CodeB "---")
        "不是一个原子, 因为它缺少了单引号;"
        (CodeB "'")
        "不是一个原子, 因为它的后面既没有跟着一个字母, 也没有跟着一个连字符."))
   ((dialogue)
    (Ld (Code "'Atom") "是一个" (Code "Atom") "吗?")
    (Rd "是的, 即便" (Code "'Atom") "也是一个" (Code "Atom")
        ", 因为它是一个单引号后面跟着一个或更多的字母或者连字符."))
   ((dialogue)
    (Ld (Code "'at0m") "是一个" (Code "Atom") "吗?")
    (Rd "不是, 因为根据" (Ref "atom-definition")
        ", 原子只能包含字母或者连字符, 而字符"
        (Code "0") "并不是一个字母, 它是数位零."))
   ((dialogue)
    (Ld (Code "'cœurs-d-artichauts") "是一个" (Code "Atom") "吗?"
        ((tcomment)
         "cœurs d'artichauts是一种食物, 即洋蓟心."))
    (Rd "是的, 因为" (Code "œ") "是一个字母."))
   ((dialogue)
    (Ld (Code "'ἄτομον") "是一个" (Code "Atom") "吗?"
        ((tcomment)
         "ἄτομον是一个古希腊语词, 意思是不可分割的东西, 即原子."))
    (Rd "That's Greek to me!"
        ((tcomment)
         "这算是一语双关.")
        "但是希腊字母也是字母, 所以它必然是一个" (Code "Atom") "."))
   ((law)
    (Center "单引号之律")
    (P "一个单引号后直接跟着一个或更多的字母或者连字符则为一个"
       (Code "Atom") "."))
   ((dialogue)
    (Ld "诸如"
        (P (Code "'ratatouille") "是一个" (Code "Atom"))
        "和"
        (P (Code "'cœurs-d-artichauts") "是一个" (Code "Atom"))
        "这样的句子被称为" (Em "判断(judgment)") "."
        ((comment)
         "感谢Per Martin-Löf (1942–)."))
    (Rd "判断的要义在于什么?"))
   ((dialogue #:id "judgment")
    (Ld "一个判断是一个人对于一个表达所采取的一个态度. "
        "当我们得以获知什么东西时, 我们就在作出一个判断." (Br)
        "关于" (Code "Atom") "和" (Code "'courgette")
        ", 可以判断什么?")
    (Rd (Code "'courgette") "是一个" (Code "Atom") "."
        ((tcomment)
         "courgette指的是西葫芦.")))
   ((dialogue)
    (Ld "一个" (Em "判断形式(form of judgment)")
        "是一个带有空白的观察, 例如"
        (P "____是一个____."))
    (Rd "还有其他的判断形式吗?"))
   ((dialogue)
    (Ld (Q "判断 (judgment)") "的另一种形式是"
        (Q "判断 (judgement)") ".")
    (Rd "十分有趣."))
   ((dialogue)
    (Ld (CodeB "'ratatouille")
        "和"
        (CodeB "'ratatouille")
        "是相同的"
        (CodeB "Atom")
        "吗?")
    (Rd "是的."
        (P "它们之所以是相同的" (Code "Atom")
           ", 是因为单引号之后有着相同的字母.")))
   ((dialogue)
    (Ld (CodeB "'ratatouille")
        "和"
        (CodeB "'courgette")
        "是相同的"
        (CodeB "Atom")
        "吗?")
    (Rd "不是."
        (P "单引号之后, 它们有着不同的字母.")))
   ((law)
    (Center "单引号之诫")
    (P "两个表达式为相同的" (Code "Atom")
       ", 如果它们的值是单引号后跟着完全等同的字母和连字符."))
   ((dialogue)
    (Ld "第二种判断形式为"
        (P "____和____是相同的____."))
    (Rd "因而"
        (P (Code (U "'citron")) "和" (Code (U "'citron"))
           "是相同的" (Code (U "Atom")))
        "是一个判断."
        ((tcomment)
         "citron指的是香橼.")))
   ((dialogue)
    (Ld "这的确是一个判断, 而且我们有理由去相信."
        (P (P (Code "'pomme") "和" (Code "'orange")
              "是相同的" (Code "Atom"))
           "是判断吗?"))
    (Rd "诚然它是判断, 但是我们没有理由去相信它. "
        "毕竟, 我们不该比较苹果 (apple) 和橙子."
        ((tcomment)
         "pomme是法语的苹果.")))
   ((dialogue)
    (Ld (CodeB "(cons 'ratatouille 'baguette)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "这显然吗?"
        ((comment)
         "当准备好的时候, 读[page 62]看"
         (Q "定型 (typing)") "指令."))
    (Rd "不, 完全不是."
        (P "成为一个"
           (CodeB "(Pair Atom Atom)")
           "是什么意思呢?")
        ((tcomment)
         "baguette指的是法棍面包.")))
   ((dialogue)
    (Ld "成为一个"
        (CodeB "(Pair Atom Atom)")
        "就是要成为一个序对, 其"
        (Code "car") "是一个"
        (Code "Atom") ", 例如"
        (Code "'ratatouille") ", 其"
        (Code "cdr") "也是一个"
        (Code "Atom") ", 例如"
        (Code "'baguette") ".")
    (Rd (Code "cons") ", " (Code "car")
        ", " (Code "cdr")
        "看上去很眼熟. 不谈以前, "
        "这里它们是什么意思呢? "
        "它们和序对 (pair) 又有什么关系呢?"))
   ((dialogue)
    (Ld "一个序对以" (Code "cons")
        "起手, 而以另外两个部分作结, "
        "我们称其为它的" (Code "car")
        "和" (Code "cdr") "."
        ((comment)
         "在Lisp中, " (Code "cons")
         "可以使得列表更长. 但在这里, "
         (Code "cons") "只是用来构造序对."))
    (Rd "好吧, 这意味着之所以"
        (CodeB "(cons 'ratatouille 'baguette)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "是因为"
        (Code "(cons 'ratatouille 'baguette)")
        "是一个序对, 其" (Code "car")
        "是一个" (Code "Atom") ", 其"
        (Code "cdr") "也是一个" (Code "Atom") "."
        (P "那么, " (Code "cons") "是一个"
           (Code "Pair") "吗?")))
   ((dialogue #:id "alone")
    (Ld "甚至" (Code "cons") "和" (Code "Pair")
        "在单独情况下都不是表达式, 它们都需要两个参数."
        (P (CodeB "(cons 'ratatouille 'baguette)")
           "和"
           (CodeB "(cons 'ratatouille 'baguette)")
           "是相同的"
           (CodeB "(Pair Atom Atom)")
           "吗?")
        ((comment)
         "在Lisp中, " (Code "cons")
         "是一个过程, 有着其自身的含义, 但是诸如"
         (Code "cond") "或" (Code "lambda")
         "这样的形式如果单独出现则是没有意义的."))
    (Rd "两个表达式为相同的"
        (CodeB "(Pair Atom Atom)")
        "是什么意思呢?"))
   ((dialogue #:id "same-pair")
    (Ld "这意味着它们的" (Code "car")
        "都是相同的" (Code "Atom")
        ", 它们的" (Code "cdr")
        "也都是相同的" (Code "Atom") ".")
    (Rd "那么的确"
        (CodeB "(cons 'ratatouille 'baguette)")
        "和"
        (CodeB "(cons 'ratatouille 'baguette)")
        "是相同的"
        (CodeB "(Pair Atom Atom)")))
   ((dialogue)
    (Ld (CodeB "(cons 'ratatouille 'baguette)")
        "和"
        (CodeB "(cons 'baguette 'ratatouille)")
        "是相同的"
        (CodeB "(Pair Atom Atom)")
        "吗?")
    (Rd (CodeB "(cons 'ratatouille 'baguette)")
        "的" (Code "car") "是" (Code "'ratatouille")
        "而"
        (CodeB "(cons 'baguette 'ratatouille)")
        "的" (Code "car") "是" (Code "'baguette") "."
        (P "因此, 我们没有理由去相信它们是相同的"
           (Code "(Pair Atom Atom)") ".")))
   ((dialogue)
    (Ld (CodeB "(cdr
 (cons 'ratatouille 'baguette))")
        "可以怎样描述呢?")
    (Rd "它是一个"
        (CodeB "Atom")))
   ((dialogue)
    (Ld "描述其他表达式的表达式, 例如" (Code "Atom")
        ", 被称为" (Em "类型(type)") "."
        (P (Code "(Pair Atom Atom)") "是一个类型吗?")
        ((comment)
         "当一个名字 (例如" (Code "Pair") "或者"
         (Code "Atom") ") 牵涉类型时, 首字母会大写."))
    (Rd "是的, 因为它描述了" (Code "car") "和"
        (Code "cdr") "均为" (Code "Atom") "的序对."))
   ((dialogue)
    (Ld "第三种判断形式为"
        (P "____是一个类型."))
    (Rd "这意味着"
        (P (Code "Atom") "是一个类型")
        "和"
        (P (Code "(Pair Atom Atom)") "是一个类型")
        "都是判断."))
   ((law)
    (Center (Code "Atom") "之律")
    (P (Code "Atom") "是一个类型."))
   ((dialogue)
    (Ld (P (Code "'courgette") "是一个类型.")
        "是一个判断吗?")
    (Rd "它的确是一个判断, 但是我们没有任何理由去相信它, 因为"
        (Code "'courgette") "并不描述其他表达式."))
   ((dialogue)
    (Ld (Code "Atom") "和" (Code "Atom")
        "是相同的类型吗?")
    (Rd "想必如此, 它们看上去就应该是相同的类型."))
   ((dialogue)
    (Ld "第四种判断形式, 也是最后一种, 如下"
        (P "____和____是相同的类型."))
    (Rd "那么, 所以说"
        (P (Code (U "Atom")) "和" (Code (U "Atom"))
           "是相同的类型")
        "是一个判断, 并且我们有理由去相信它."))
   ((law)
    (Center "判断的四种形式")
    (Ol (Li "____是一个____.")
        (Li "____和____是相同的____.")
        (Li "____是一个类型.")
        (Li "____和____是相同的类型.")))
   ((dialogue)
    (Ld "以下是一个判断吗?"
        (P (Code "Atom") "和" (Code "(Pair Atom Atom)")
           "是相同的类型."))
    (Rd "它是一个判断, 但是没有理由去相信它."))
   ((dialogue)
    (Ld (CodeB "(Pair Atom Atom)")
        "和"
        (CodeB "(Pair Atom Atom)")
        "是相同的类型吗?")
    (Rd "看起来相当可信啊."))
   ((dialogue)
    (Ld "判断是获知的行为, 而相信是获知的一部分.")
    (Rd "判断难道不是句子吗?"))
   ((dialogue)
    (Ld "句子从理解它们的人那里获得意义. "
        "句子捕获了我们所拥有的思想, "
        "而思想比我们用来表达思想的词语要重要得多.")
    (Rd "啊, 所以说得以获知"
        (CodeB "(Pair Atom Atom)")
        "和"
        (CodeB "(Pair Atom Atom)")
        "是相同的类型这一行为是一个判断."))
   ((dialogue)
    (Ld "的确如此."
        (P (Code "'pêche") "和" (Code "'pêche")
           "是相同的" (Code "'fruit") "吗?")
        ((tcomment)
         "pêche指的是桃子."))
    (Rd "好问题."
        (P (Code "'pêche") "是一个"
           (Code "'fruit") "吗?")))
   ((dialogue)
    (Ld "不, 当然不是, 因为"
        (P (Code "'fruit") "是一个类型")
        "并不可信."
        (P "某些形式的判断只有在早前的判断的基础之上才具备意义.")
        ((comment)
         "这种早前的判断有时被称为"
         (Em "前置假定(presupposition)") "."))
    (Rd "有哪些呢?"))
   ((dialogue)
    (Ld "为了提问一个表达式是否由一个类型描述, "
        "我们必须已经判断过给定的类型的确是一个类型. "
        "为了提问两个表达式是否在一个类型下是相同的, "
        "我们必须首先判断出这两个表达式都由该类型所描述."
        ((comment)
         "当然, 为了描述那两个表达式, "
         "那个给定的类型也得是类型.")
        "在提问两个表达式是否是相同的类型之前, "
        "有什么判断是必要的吗?")
    (Rd "为了提问两个表达式是否是相同的类型, "
        "我们必须先要判断这两个表达式的确都是类型."))
   ((dialogue)
    (Ld (CodeB "(car
 (cons 'ratatouille 'baguette))")
        "和"
        (CodeB "'ratatouille")
        "是相同的"
        (CodeB "Atom")
        "吗?")
    (Rd "看起来十分熟悉啊. 想必是这样, 因为"
        (Code "car") "可以找到一个序对的"
        (Code "car") ", 所以它们" (Em "是")
        "相同的."))
   ((dialogue #:id "ch1-39")
    (Ld (CodeB "(cdr
 (cons 'ratatouille 'baguette))")
        "和"
        (CodeB "'baguette")
        "是相同的"
        (CodeB "Atom")
        "吗?")
    (Rd "诚然如此, 因为这个序对的" (Code "cdr")
        "是" (Code "'baguette") "."))
   ((dialogue)
    (Ld "于是"
        (CodeB "(car
 (cons
  (cons 'aubergine 'courgette)
  'tomato))")
        "是一个..."
        ((tcomment)
         "aubergine指的是茄子."))
    (Rd "... " (Code "(Pair Atom Atom)") ", 因为"
        (CodeB "(cons 'aubergine 'courgette)")
        "是一个序对, 其" (Code "car") "是"
        (Code "Atom") " " (Code "'aubergine")
        ", 其" (Code "cdr") "是"
        (Code "Atom") " " (Code "'courgette") "."))
   ((dialogue #:id "ch1-41")
    (Ld (CodeB "(car
 (cdr
  (cons
   'ratatouille
   (cons 'baguette 'olive-oil))))")
        "和"
        (CodeB "'baguette")
        "是相同的"
        (CodeB "Atom")
        "吗?")
    (Rd "是的, 的确如此."))
   ((dialogue)
    (Ld "正如从" (Ref "ch1-39") "到"
        (Ref "ch1-41") "所展现的那样, "
        "写法不同的表达式或许可能是相同的. "
        "其中一种写法要比其他写法都更加直接.")
    (Rd (Code "'baguette") "的确看上去比"
        (CodeB "(car
 (cdr
  (cons
   'ratatouille
   (cons 'baguette 'olive-oil))))")
        "更加直接."))
   ((dialogue)
    (Ld "一个表达式的" (Em "规范形式(normal form)")
        "是书写该表达式最直接的方式. "
        "任何两个相同的表达式都有着等同的规范形式, "
        "并且任何两个有着等同规范形式的表达式都是相同的.")
    (Rd (Code "'olive-oil") "是"
        (CodeB "(cdr
 (cdr
  (cons
   'ratatouille
   (cons 'baguette 'olive-oil))))")
        "的规范形式吗?"))
   ((dialogue)
    (Ld "这个问题是不完整的."
        (P "相同总是相对于某个类型而言的, 因此"
           "规范形式也由类型决定."))
    (Rd (Code "'olive-oil") "是" (Code "Atom")
        (CodeB "(cdr
 (cdr
  (cons
   'ratatouille
   (cons 'baguette 'olive-oil))))")
        "的规范形式吗?"))
   ((dialogue)
    (Ld "是的, 的确如此."
        (CodeB "(cons 'ratatouille 'baguette)")
        "是一个规范的"
        (CodeB "(Pair Atom Atom)")
        "吗?"
        ((comment)
         (Em "规范的(normal)") "是"
         (Em "具有规范形式(in normal form)")
         "的简略说法."))
    (Rd "是的, "
        (Code "(cons 'ratatouille 'baguette)")
        "的确是规范的."
        (P "每个表达式都具有一个规范形式吗?")))
   ((dialogue)
    (Ld "如果不刻画一个表达式的类型, "
        "那么提问其是否具有规范形式也是没有意义的."
        (P "然而, 给定一个类型, 每个由该类型描述的"
           "表达式的确都有一个由该类型确定的规范形式."))
    (Rd "如果了两个表达式根据它们的类型是相同的, "
        "那么它们就具有等同的规范形式. 因此, 这必然意味着"
        "我们可以通过比较两个表达式的规范形式来"
        "判断 (check) 它们是否相同."))
   ((law)
    (Center "规范形式")
    (P "给定一个类型, 每个由该类型所描述的表达式都具有一个"
       (Em "规范形式") ", 这是书写该表达式最直接的方式. "
       "如果两个表达式是相同的, 那么它们有着等同的规范形式. "
       "如果两个表达式有着等同的规范形式, 那么它们是相同的."))
   ((dialogue)
    (Ld (CodeB "(car
 (cons
  (cons 'aubergine 'courgette)
  'tomato))")
        "的规范形式是什么?")
    (Rd "类型是什么呢?"
        (P "如果类型是"
           (CodeB "(Pair Atom Atom)")
           "那么规范形式为"
           (CodeB "(cons 'aubergine 'courgette)"))))
   ((dialogue)
    (Ld "干得好!"
        (P "之前我们对于什么是一个"
           (CodeB "(Pair Atom Atom)")
           "的描述其实是不完整的, 而完整的描述应该是..."))
    (Rd "...成为一个序对, 其" (Code "car") "是一个"
        (Code "Atom") ", 其" (Code "cdr") "也是一个"
        (Code "Atom") ", "
        (Em "或者是与这样一个序对相同的一个表达式.")))
   ((law)
    (Center "规范形式和类型")
    (P "相同总是根据一个类型来的, 因而规范形式也由类型决定."))
   ((dialogue)
    (Ld (CodeB "(car
 (cons
  (cons 'aubergine 'courgette)
  'tomato))")
        "和"
        (CodeB "(cons 'aubergine 'courgette)")
        "是相同的"
        (CodeB "(Pair Atom Atom)")
        "吗?")
    (Rd "是的, 之所以这两个表达式是相同的"
        (Code "(Pair Atom Atom)") ", 是因为"
        (CodeB "(car
 (cons
  (cons 'aubergine 'courgette)
  'tomato))")
        "的规范形式是"
        (CodeB "(cons 'aubergine 'courgette)")))
   ((dialogue)
    (Ld "为什么"
        (CodeB "(cons 'aubergine 'courgette)")
        "和"
        (CodeB "(cons 'aubergine 'courgette)")
        "是相同的" (Code "(Pair Atom Atom)") "呢?")
    (Rd "这看起来非常显然?"))
   ((dialogue)
    (Ld "是的, 但是不是每个" (Em "看上去")
        "显然的东西" (Em "实际上") "都是显然的."
        (P (Ref "same-pair")
           "描述了何谓一个表达式和另一个表达式是相同的"
           (CodeB "(Pair Atom Atom)"))
        ((comment)
         "在Lisp中, 相同的原子使用两次" (Code "cons")
         "产生的序对并不" (Code "eq")
         ". 但在这里, 它们无法以任何方式进行区分."))
    (Rd (CodeB "(cons 'aubergine 'courgette)")
        "和"
        (CodeB "(cons 'aubergine 'courgette)")
        "的顶层都是" (Code "cons") ", "
        (Code "'aubergine") "和"
        (Code "'aubergine") "是相同的"
        (Code "Atom") ", " (Code "'courgette")
        "和" (Code "'courgette") "是相同的"
        (Code "Atom") "."
        (P "这两个表达式具有相同的" (Code "car")
           "和相同的" (Code "cdr") ", 因而它们是相同的"
           (CodeB "(Pair Atom Atom)"))))
   ((law)
    (Center (Code "cons") "之第一诫")
    (P "两个" (Code "cons") "表达式是相同的"
       (Code "(Pair " $A " " $D ")")
       ", 如果它们的" (Code "car")
       "是相同的" $A "而它们的" (Code "cdr")
       "是相同的" $D ", 其中" $A "和" $D
       "代表任意的类型."))
   ((dialogue #:id "not-a-type")
    (Ld "很好."
        (CodeB "(Pair
 (cdr
  (cons Atom 'olive))
 (car
  (cons 'oil Atom)))")
        "的规范形式是什么呢?")
    (Rd "我猜是" (Code "(Pair 'olive 'oil)")
        ", 是这样吗?"))
   ((dialogue)
    (Ld "实际上, 表达式"
        (CodeB "(Pair
 (cdr
  (cons Atom 'olive))
 (car
  (cons 'oil Atom)))")
        "既不由某个类型刻画, 本身也不是一个类型, "
        "因此提问其规范形式是毫无意义的."
        ((comment)
         "不被类型刻画且自身不是类型的表达式也被称为是"
         (Em "病态类型(ill-typed)") "的."))
    (Rd "为什么呢?"))
   ((dialogue #:id "Pair-type")
    (Ld "因为" (Code "Pair") "在其参数为实际原子时并非类型."
        (P "只有在其参数均为类型 (例如" (Code "Atom")
           ") 时, 它才是一个表达式."))
    (Rd "这是不是意味着" (Code "Pair")
        "不能和" (Code "car") "与" (Code "cdr")
        "一起使用呢?"))
   ((dialogue #:id "type-eval")
    (Ld "不, 完全不是."
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "的规范形式是什么?")
    (Rd "它的类型是什么呢? 规范形式总是相对于某个类型而言的."))
   ((dialogue)
    (Ld "类型本身也有规范形式. 如果两个类型有着等同的规范形式, "
        "那么它们就是相同的类型. 如果两个类型是相同的类型, "
        "那么它们就具有等同的规范形式.")
    (Rd "类型"
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "的规范形式必然是"
        (Code "(Pair Atom Atom)")
        ", 因为"
        (CodeB "(car
 (cons Atom 'olive))")
        "的规范形式为" (Code "Atom")
        ", 并且"
        (CodeB "(cdr
 (cons 'oil Atom))")
        "的规范形式也是" (Code "Atom") "."))
   ((law)
    (Center "类型的规范形式")
    (P "每个为类型的表达式都具有一个规范形式, "
       "其是书写该类型最直接的方式. "
       "如果两个表达式是相同的类型, 那么"
       "它们有着等同的规范形式. "
       "如果两个类型有着等同的规范形式, 那么"
       "它们是相同的类型."))
   ((dialogue)
    (Ld "就是这样. 现在我们知道"
        (CodeB "(cons 'ratatouille 'baguette)")
        "也是一个"
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "因为...")
    (Rd "..."
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "的规范形式为"
        (CodeB "(Pair Atom Atom)")
        "而"
        (CodeB "(cons 'ratatouille 'baguette)")
        "是一个"
        (CodeB "(Pair Atom Atom)")))
   ((dialogue)
    (Ld "另一种说法是"
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "和"
        (CodeB "(Pair Atom Atom)")
        "是相同的类型.")
    (Rd "如果一个表达式是一个"
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "那么它也是一个"
        (CodeB "(Pair Atom Atom)")
        "因为这两个类型是相同的类型."))
   ((dialogue)
    (Ld "类似地, 如果一个表达式是一个"
        (CodeB "(Pair Atom Atom)")
        "那么它也是一个"
        (CodeB "(Pair
 (car
  (cons Atom 'olive))
 (cdr
  (cons 'oil Atom)))")
        "因为这两个类型是相同的类型.")
    (Rd "对于"
        (CodeB "(Pair
 Atom
 (cdr
  (cons 'oil Atom)))")
        "也是类似的, 因为它和前面两个也是相同的类型."))
   ((dialogue)
    (Ld (Code "'6") "是一个" (Code "Atom") "吗?")
    (Rd "不是, 我们没有理由去相信"
        (P (Code "'6") "是一个" (Code "Atom"))
        "因为数位" (Code "6")
        "既非字母也非连字符, 不是吗?"))
   ((dialogue)
    (Ld "的确如此."
        (CodeB "(cons '17 'pepper)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "吗?")
    (Rd "不, 因为" (Code "(cons '17 'pepper)")
        "的" (Code "car") "是" (Code "'17")
        ", 它" (Em "并非") "一个" (Code "Atom") "."
        (P "尽管如此, 拥有数字应该也是很自然的事情才对.")))
   ((dialogue)
    (Ld "数字当然是很方便的. 除了" (Code "Atom")
        "和" (Code "Pair") ", 我们还可以判断某个东西是否是一个"
        (Code "Nat") ".")
    (Rd "让我们来试一下."))
   ((dialogue)
    (Ld (Code "1") "是一个" (Code "Nat") "吗?"
        ((comment)
         (Code "Nat") "是"
         (Em "自然数(natural number)") "的缩写."))
    (Rd "的确, " (Code "1") "是一个" (Code "Nat") "."))
   ((dialogue)
    (Ld (Code "1729") "是一个" (Code "Nat") "吗?")
    (Rd "是的, " (Code "1729") "是一个" (Code "Nat")
        ". 它不仅是一个" (Code "Nat") ", 还很著名!"
        ((comment)
         "谢谢Srinivasa Ramanujan (1887–1920) 和"
         "Godfrey Harold Hardy (1877–1947).")))
   ((dialogue)
    (Ld (Code "-1") "是一个" (Code "Nat") "吗?")
    (Rd "嗯, 你确定?"))
   ((dialogue)
    (Ld "不, 当然不是. " (Code "-23") "呢?")
    (Rd "不是很清楚啊."))
   ((dialogue)
    (Ld "正数是" (Code "Nat") ".")
    (Rd "啊, 那么" (Code "-23") "不是一个"
        (Code "Nat") "?"))
   ((dialogue)
    (Ld "我们更喜欢采取积极 (positive) 的态度."
        ((tcomment)
         "这大概是一语双关吧.")
        "最小的" (Code "Nat") "是什么?")
    (Rd (Code "0") "不是一个自然数吗?"))
   ((dialogue)
    (Ld "啊, 所以我们也不能总是很积极. "
        "我们该如何获得剩下来的" (Code "Nat") "呢?"
        ((comment)
         "然而, 数字" (Code "1") "总" (Em "是") "正的."))
    (Rd "我们可以使用我们的老朋友" (Code "add1")
        ". 如果" $n "是一个" (Code "Nat")
        ", 那么" (Code "(add1 " $n ")")
        "也是一个" (Code "Nat") ", 并且这个新的"
        (Code "Nat") "永远是正数, 即便" $n
        "是" (Code "0") "."
        (P "有多少个" (Code "Nat") "呢?")))
   ((dialogue)
    (Ld "很多很多!")
    (Rd "存在最大的" (Code "Nat") "吗?"))
   ((dialogue)
    (Ld "并不, 因为我们总是可以...")
    (Rd "...使用" (Code "add1") "来加上一?"))
   ((dialogue)
    (Ld "的确是这样!"
        (P (Code "0") "和" (Code "26") "是相同的"
           (Code "Nat") "吗?")
        ((comment)
         "谢谢Giuseppe Peano (1838-1932)."))
    (Rd "显然不是."))
   ((dialogue)
    (Ld (Code "(+ 0 26)") "和" (Code "26") "是相同的吗?"
        ((comment)
         "即便我们还没有解释" (Code "+")
         ", 暂时请用你自己对于加法的理解."))
    (Rd "这个问题没有意义. 但是, 我们可以问"
        "它们是否是相同的" (Code "Nat") "吗?"))
   ((dialogue)
    (Ld "当然可以."
        (P (Code "(+ 0 26)") "和" (Code "26")
           "是相同的" (Code "Nat") "吗?"))
    (Rd "是的, 这是因为" (Code "(+ 0 26)")
        "的规范形式为" (Code "26") ", 而"
        (Code "26") "和" (Code "26")
        "当然是相同的."))
   ((dialogue)
    (Ld (Code "zero") "的意思是什么呢?")
    (Rd (Code "zero") "和" (Code "0")
        "是相同的吗?"))
   ((dialogue)
    (Ld "在Pie里, " (Code "zero") "和" (Code "0")
        "不过是书写同一个" (Code "Nat") "的不同方式而已."
        (P (Code "one") "和" (Code "1") "是相同的"
           (Code "Nat") "吗?"))
    (Rd "嗯, 如果" (Code "zero") "和" (Code "0")
        "是相同的" (Code "Nat") "的话, 那么这似乎很合理."))
   ((dialogue #:id "definition")
    (Ld "实际上, " (Code "one") "没有意义. 但是, "
        (Code "(add1 zero)") "是书写数字" (Code "1")
        "的另一种方式."
        (P "通过" (Em "定义") "使得" (Code "one")
           "为" (Code "(add1 zero)") "的确是可行的."
           (CodeD "(define one
  (add1 zero))")))
    (Rd "为什么这个定义周围用虚线框住了呢?"))
   ((dialogue)
    (Ld "虚线框意味着这个定义有点问题, "
        "以至于它不能在之后使用.")
    (Rd "这个定义有什么问题呢?"
        (P "看上去很正常啊.")))
   ((dialogue)
    (Ld "当定义一个名字时, 有必要先"
        (Code "claim") "这个名字具有一个类型, 而"
        (Code "one") "是一个" (Code "Nat") "."
        (CodeB "(claim one Nat)
(define one
  (add1 zero))"))
    (Rd "因此, " (Code "two") "可以被定义成"
        (CodeB "(claim two Nat)
(define two
  (add1 one))")))
   ((law)
    (Center "定义前先声明")
    (P "使用" (Code "define")
       "将一个名字和一个表达式联系起来之前需要使用"
       (Code "claim")
       "将名字和表达式的类型联系起来."))
   ((dialogue)
    (Ld "如果" (Code "1") "是书写" (Code "(add1 zero)")
        "的另一种方式, 那么书写" (Code "4")
        "的另一种方式是什么呢?")
    (Rd "难道不应该是"
        (CodeB "(add1
 (add1
  (add1
   (add1 zero))))")
        "吗? 我们不能定义" (Code "four")
        "来指代这个表达式吗?"))
   ((dialogue #:id "top-add1")
    (Ld "当然可以了."
        (CodeB "(claim four Nat)
(define four
  (add1
   (add1
    (add1
     (add1 zero)))))")
        "那么, 再问一下书写" (Code "8")
        "有另外的方式吗?")
    (Rd "那必然是"
        (CodeB "(" (U "add1") "
 (add1
  (add1
   (add1
    (add1
     (add1
      (add1
       (add1 zero))))))))")))
   ((dialogue)
    (Ld (Code "8") "是规范的吗?")
    (Rd "似乎如此, 但是为什么" (Code "8")
        "是规范的呢?"))
   ((dialogue)
    (Ld "之所以" (Code "8") "是" (Em "规范")
        "的, 是因为其顶" (Code (U "add1"))
        "是一个" (Em "构造子(constructor)")
        ", 并且塞在顶" (Code (U "add1"))
        "下面的参数, 即" (Code "7")
        ", 也是规范的."
        ((comment)
         (Ref "top-add1") "中的顶" (Code (U "add1"))
         "只此一次用下划线标注出来以示强调."))
    (Rd "为什么" (Code "7") ", 亦写作"
        (CodeB "(add1
 (add1
  (add1
   (add1
    (add1
     (add1
      (add1 zero)))))))")
        "是规范的呢?"))
   ((dialogue)
    (Ld (Code "7") "是规范的完全是同理可得.")
    (Rd "这意味着" (Code "zero")
        "必然是规范的, 不然的话" (Code "(add1 zero)")
        "就不是规范的了."))
   ((dialogue)
    (Ld (Code "zero") "的顶是什么呢?")
    (Rd "必须是" (Code "zero") "."))
   ((dialogue #:id "zero-constructor")
    (Ld "之所以" (Code "zero") "是规范的, 是因为顶"
        (Code "zero") "是一个构造子, 并且其没有参数."
        (CodeB "(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))")
        "是规范的吗?")
    (Rd "不是, 因为" (Code "+") "不是构造子."))
   ((dialogue)
    (Ld "一个以构造子为顶的表达式被称为一个"
        (Em "值(value)") "."
        (P "即便"
           (CodeB "(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))")
           "不是规范的, 它的确是一个值.")
        ((comment)
         "值也被称为" (Em "典则(canonical)")
         "表达式."))
    (Rd "这个表达式不是规范的, 是因为"
        (CodeB "(+ (add1 zero)
   (add1
    (add1 zero)))")
        "并非书写" (Code "3") "的最直接方式."))
   ((law)
    (Center "值")
    (P "以构造子为顶的表达式被称为" (Em "值") "."))
   ((dialogue)
    (Ld "现在我们给出另一个并非规范的表达式."
        (CodeB "(+ (add1
    (add1 zero))
   (add1 zero))")
        "这是书写" (Code "3") "的最直接方式吗?")
    (Rd "肯定不是."
        (P "准确来说, 构造子究竟是什么呢?")))
   ((dialogue)
    (Ld "某些表达式是类型, 例如" (Code "Nat")
        "和" (Code "(Pair Nat Atom)") "."
        (P "对于新类型的解释的一部分是要说明其"
           "构造子为何. 构造子表达式是构造"
           "具有该类型的表达式的直接方式."))
    (Rd "构造子的例子有哪些呢?"))
   ((dialogue)
    (Ld (Code "Nat") "的构造子是" (Code "zero")
        "和" (Code "add1") ", 而" (Code "Pair")
        "的构造子是" (Code "cons") ".")
    (Rd "值和规范形式之间有何关系?"))
   ((dialogue)
    (Ld "在一个值里, 顶层的构造子的参数不必是规范的. "
        "但如果这些参数的确是规范的, "
        "那么整个构造子表达式就具有规范形式."
        (P "所有的值都是规范的吗?"))
    (Rd "显然不是."
        (CodeB "(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))")
        "和"
        (CodeB "(add1
 (+ (add1 zero) (add1 one)))")
        "都是值, 但是它们都不是规范的."))
   ((law)
    (Center "值和规范形式")
    (P "不是所有的值都具有规范形式. "
       "这是因为构造子的参数不必是规范的. "
       "每个表达式只有一个规范形式, "
       "但是有时可能将其写成值的方式不止一种."))
   ((dialogue)
    (Ld "以下空白方框里填什么会使得整个表达式"
        (Em "不") "是一个" (Code "Nat") "值呢?"
        (CodeB "(add1 "
               (Frame "          ")
               ")"))
    (Rd (Code "'aubergine") "怎么样?"))
   ((dialogue)
    (Ld "诚然如此."
        (CodeB "(add1 'aubergine)")
        "并非一个" (Code "Nat")
        "值, 因为" (Code "'aubergine")
        "是一个" (Code "Atom")
        "而不是一个" (Code "Nat") "."
        (P "当填充这样的方框时, 我们的期望是"
           "作为结果的表达式由某个类型刻画."))
    (Rd "然而, 若置于该方框里的是任何一个"
        (Code "Nat") "表达式, 那么整个表达式就"
        (Em "是") "一个值. 这一整个表达式以"
        (Code "add1") "为顶, 而" (Code "add1")
        "是一个" (Code "Nat") "构造子."))
   ((dialogue)
    (Ld "找出一个与某起始表达式相同的值被称为"
        (Em "求值(evaluation)") ".")
    (Rd "类型呢? 毕竟, 相同需要类型."))
   ((dialogue)
    (Ld "有时当我们提及相同时, 我们并不显式提及类型. "
        "然而, 总是存在一个意图的类型, "
        "并且这个类型可以通过仔细阅读找到.")
    (Rd "难道求值指的不是找到一个表达式的"
        (Em "意义(meaning)")
        "吗? 这不只是某个更简单的表达式."))
   ((dialogue)
    (Ld "我们这里的含义不一样. "
        "表达式并不指向某种外部的意义概念&mdash;&mdash;"
        "在Pie中, 除了表达式和我们对于表达式的判断之外, 别无其他."
        ((comment)
         "在Lisp中, 值和表达式是不同的, 而求值的结果是一个值."))
    (Rd "这是一种看待求值的新方式."
        (P "为什么规范形式和值之间要做区分呢?")))
   ((law)
    (Center "每个东西都是一个表达式")
    (P "在Pie中, 值也是表达式. Pie中的求值是寻找一个表达式, "
       "而不是其他别的什么东西."))
   ((dialogue)
    (Ld "规范表达式没有可供求值的剩余机会了. "
        "通常而言, 规范的表达式更容易理解. "
        "然而, 往往找到一个值就足够了, "
        "因为顶层的构造子可以用来判断"
        "接下来必然发生的事情.")
    (Rd "如果找到一个值经常就足够了的话, "
        "难道说这意味着我们可以自由地去"
        "寻找值, 并且然后可以想停就停呢?"))
   ((dialogue)
    (Ld "是这样的, 只要关于构造子的参数的信息"
        "从没有用到即可."
        (CodeB "(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))")
        "和" (Code "four") "是相同的"
        (Code "Nat") "吗?")
    (Rd "这里给出一个可能的回答."
        (P "它们不是相同的" (Code "Nat") ", 因为"
           (CodeB "(add1
 (+ (add1 zero)
    (add1
     (add1 zero))))")
           "是一个值, 而且它当然长得不像变量"
           (Code "four") ". 找到"
           (Code "four") "的值也无济于事, 因为"
           (Code "four") "的值看起来非常不同.")))
   ((dialogue)
    (Ld "算是好的尝试."
        (P "但是, 实际上它们是相同的"
           (Code "Nat") "."))
    (Rd "怎么能这样呢?"))
   ((dialogue #:id "Nat-same")
    (Ld "两个不是值的" (Code "Nat")
        "表达式相同, 如果它们的值相同. "
        "恰存在两种" (Code "Nat")
        "值可以相同的方式: 每种构造子一个."
        (P "如果两个都是" (Code "zero")
           ", 那么它们就是相同的"
           (Code "Nat") "."))
    (Rd "那么两个值都以" (Code "add1")
        "为顶的情况呢?"))
   ((law)
    (Center (Code "zero") "之诫")
    (P (Code "zero") "和" (Code "zero")
       "是相同的" (Code "Nat") "."))
   ((dialogue #:id "add1-zero")
    (Ld "如果每个" (Code "add1")
        "的参数是相同的" (Code "Nat")
        ", 那么两个" (Code "add1")
        "表达式是相同的" (Code "Nat") "值."
        (P "为什么"
           (CodeB "(add1 zero)")
           "和"
           (CodeB "(add1 zero)")
           "是相同的"
           (CodeB "Nat")))
    (Rd "这两个表达式都是值. 这两个值都以" (Code "add1")
        "为顶, 因此它们的参数应该是相同的"
        (Code "Nat") "."
        (P "这两个参数都是" (Code "zero")
           ", " (Code "zero") "是一个值, 而且"
           (Code "zero") "和" (Code "zero")
           "是相同的" (Code "Nat") "值.")))
   ((law)
    (Center (Code "add1") "之诫")
    (P "如果" $n "和" $k "是相同的" (Code "Nat")
       ", 那么" (Code "(add1 " $n ")") "和"
       (Code "(add1 " $k ")") "是相同的"
       (Code "Nat") "."))
   ((dialogue)
    (Ld "为什么"
        (CodeB "(add1 (+ 0 1))")
        "和"
        (CodeB "(add1 (+ 1 0))")
        "是相同的"
        (CodeB "Nat"))
    (Rd "这两个" (Code "Nat") "都以" (Code "add1")
        "为顶, 于是它们都是值."
        (P "它们之所以相同, 是因为"
           (CodeB "(+ 0 1)")
           "和"
           (CodeB "(+ 1 0)")
           "是相同的"
           (CodeB "Nat"))))
   ((dialogue)
    (Ld "为什么" (Code "(+ 0 1)")
        "和" (Code "(+ 1 0)")
        "是相同的" (Code "Nat") "?")
    (Rd "这些" (Code "Nat") "并非值, 因而为了判断它们是否相同, "
        "第一步应该是找出它们的值."
        (P "这两个表达式都以" (Code "(add1 zero)")
           "为其一个值, 而" (Ref "add1-zero")
           "解释了为什么"
           (CodeB "(add1 zero)")
           "和"
           (CodeB "(add1 zero)")
           "是相同的"
           (CodeB "Nat"))))
   ((dialogue)
    (Ld "很对.")
    (Rd "是否这意味着" (Code "four")
        "本可以按照以下方式定义?"
        (CodeD "(define four
  (add1
   (+ (add1 zero)
      (add1
       (add1 zero)))))")))
   ((dialogue)
    (Ld "为什么有虚线框呢?")
    (Rd (Code "four")
        "已经被定义了, 因而不能被再次定义."))
   ((law)
    (Center "定义是永恒的")
    (P "一旦一个名字被" (Code "claim")
       "了, 那么它就不能被重新" (Code "claim")
       ". 一旦一个名字被" (Code "define")
       "了, 那么它就不能被重新" (Code "define") "."))
   ((dialogue)
    (Ld "不过当然了, 一开始" (Code "four")
        "的确可以像那样定义."
        (P "实际上, 其他表达式都不能分辨出"
           (Code "four") "的两种定义之间的不同, "
           "因为这两个定义了" (Code "four")
           "为相同的" (Code "Nat") "."))
    (Rd (Code "cons") "是一个构造子吗?"))
   ((dialogue)
    (Ld "是的, " (Code "cons")
        "构造" (Code "Pair") ".")
    (Rd "为了对于" (Code "car") "表达式求值, 是否有必要对于"
        (Code "car") "的参数求值?"))
   ((dialogue)
    (Ld "的确. 为了找出一个" (Code "car")
        "表达式的值, 我们从找出其参数的值开始."
        (P "关于这参数的值, 我们能说什么呢?"))
    (Rd "这参数的值以" (Code "cons") "为顶."))
   ((dialogue)
    (Ld "在找出参数的值之后, 接下来应该做什么呢?")
    (Rd "整个表达式的值是" (Code "cons")
        "的第一个参数."))
   ((dialogue)
    (Ld (CodeB "(car
 (cons (+ 3 5) 'baguette))")
        "的值是什么?")
    (Rd (Code "cons") "的第一个参数是"
        (CodeB "(+ 3 5)")
        "这并非一个值."))
   ((dialogue)
    (Ld "为了找出一个" (Code "car")
        "表达式的值, 首先找出其参数的值, 这应该是"
        (Code "(cons " $a " " $d ")") ", 而"
        (CodeB "(car
 (cons " $a " " $d "))")
        "的值然后就是" $a "的" (Em "值") "."
        (P "如何找出一个" (Code "cdr")
           "表达式的值呢?")
        ((comment)
         "这里的" $a "代表" (Code "car")
         "而" $d "代表" (Code "cdr") "."))
    (Rd "就和" (Code "car") "一样, 我们从对于"
        (Code "cdr") "的参数求值开始, 直至其变为"
        (Code "(cons " $a " " $d ")") ", 然后"
        (CodeB "(cdr
 (cons " $a " " $d "))")
        "的值就是" $d "的值."
        (P "所有的构造子都有参数吗?")))
   ((dialogue)
    (Ld "当然不是, 回忆一下, "
        (Ref "zero-constructor")
        "里的" (Code "zero")
        "是一个构造子."
        (P "两个表达式为相同的"
           (Code "(Pair Atom Nat)")
           "是什么意思?"))
    (Rd "这必然意味着每个表达式的值都以"
        (Code "cons") "为顶, 并且它们的"
        (Code "car") "是相同的" (Code "Atom") "而"
        (Code "cdr") "是相同的" (Code "Nat") "."))
   ((dialogue)
    (Ld "非常好.")
    (Rd "原子是构造子吗?"))
   ((dialogue)
    (Ld "原子" (Code "'bay") "是一个构造子, 因而原子"
        (Code "'leaf") "也是一个构造子.")
    (Rd (Em "所有") "原子都是构造子吗?"))
   ((dialogue)
    (Ld "是的, 每个原子都构造其自身."
        (P "这是不是意味着每个原子都是值?"))
    (Rd "的确, 因为解释为什么"
        (P (Code "Atom") "是一个类型")
        "就是在说原子是" (Code "Atom") "值."))
   ((dialogue)
    (Ld "嗯."
        (P "在表达式" (Code "zero")
           "中, 顶层构造子是什么?"))
    (Rd "那必然是" (Code "zero")
        ", 因为" (Code "zero")
        "是没有参数的构造子."))
   ((dialogue)
    (Ld "对于表达式" (Code "'garlic")
        "而言, 什么是顶层的构造子?")
    (Rd "原子" (Code "'garlic")
        "是仅有的构造子, 所以它必然就是顶层的构造子."
        (P "那么, " (Code "Nat") "是一个构造子吗?")))
   ((dialogue)
    (Ld "不是, " (Code "Nat") "并非一个构造子. "
        (Code "zero") "和" (Code "add1")
        "是创造" (Em "数据") "的构造子, 而"
        (Code "Nat") (Em "描述")
        "了特定的数据, 其要么就是" (Code "zero")
        ", 要么以" (Code "add1")
        "为顶, 且以另一个" (Code "Nat")
        "为其参数."
        (P (Code "Pair") "是一个构造子吗?"))
    (Rd "不是, 因为" (Code "Pair") "表达式是在描述以"
        (Code "cons") "为顶的表达式. 构造子创建"
        (Em "数据") ", 而不是类型."
        (P "那么, " (Code "Pair") "应该叫做什么呢?")))
   ((dialogue)
    (Ld (Code "Pair") "是一个"
        (Em "类型构造子(type constructor)")
        ", 因其构造了一个类型. 类似地, "
        (Code "Nat") "和" (Code "Atom")
        "也是类型构造子."
        (CodeB "(cons zero 'onion)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "吗?")
    (Rd "不是."
        (P "难道它不应该是一个"
           (CodeB "(Pair Nat Atom)")
           "吗?")))
   ((dialogue)
    (Ld "的确如此! 但是"
        (CodeB "(cons 'zero 'onion)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "请问"
        (CodeB "(cons 'basil
 (cons 'thyme 'oregano))")
        "的类型是什么?"
        ((comment)
         "谢谢Julia Child (1912-2004)."))
    (Rd "基于我们的所见所闻, 它必然是一个"
        (CodeB "(Pair Atom
 (Pair Atom Atom))")
        ((tcomment)
         "basil, thyme, oregano分别是罗勒, "
         "百里香, 牛至, 这是三种典型的香料. "
         "另外, Julia Child是一位厨师和电视名人.")))
   ((dialogue)
    (Ld "诚然如此.")
    (Rd "好吧, 暂时就那么多了, 我的脑袋要炸了!"))
   ((dialogue)
    (Ld "或许你应该再一次阅读这一章. "
        "判断, 表达式, 类型是本书最重要的概念.")
    (Rd "在完全读完这一章之后, 或许应该来点新鲜蔬菜."))
   (H2 "从心所欲, 道法自然")
   ((dialogue)
    (Ld "ratatouille如何?")
    (Rd (Em "很好(très bien)") ", 谢谢提问."))
   ((dialogue)
    (Ld "第" (Ref "ch1") "章里有构造子和类型构造子, "
        "分别构造值和类型."
        (P "然而, " (Code "car")
           "既不是构造子也不是类型构造子."))
    (Rd "那么, " (Code "car") "是什么呢?"))
   ((dialogue)
    (Ld (Code "car") "是一个" (Em "消去子(eliminator)")
        ". 消去子将构造子构造的值拆开来."
        (P "另一个消去子是什么呢?"))
    (Rd "如果" (Code "car") "是一个消去子, 那么显然"
        (Code "cdr") "也是一个消去子."))
   ((law)
    (Center "构造子和消去子")
    (P "构造子构造值, 而消去子将构造子构造的值拆开来."))
   ((dialogue)
    (Ld "另一种看待(消去子和构造子的)不同的方式在于"
        "值包含信息, 而消去子允许我们去利用这样的信息.")
    (Rd "存在兼作构造子和消去子的东西吗?"))
   ((dialogue)
    (Ld "不, 并不存在."
        (P "可以定义一个" (Em "函数(function)")
           ", 其表达力相当于" (Code "car")
           "和" (Code "cdr") "的联合."))
    (Rd "怎么做呢?"))
   ((dialogue)
    (Ld "这需要请出我们的老朋友" (Code "λ") ".")
    (Rd "这是什么? 我有点陌生."))
   ((dialogue)
    (Ld "Oops! 它也被称为" (Code "lambda") "."
        ((comment)
         (Code "λ") "可以选择写成"
         (Code "lambda") "."))
    (Rd "好吧, " (Code "λ") "可以构造函数."
        (P "这是不是意味着" (Code "λ")
           "是一个构造子呢?")))
   ((dialogue)
    (Ld "是的, 的确如此, 因为每个长得像"
        (Code "(λ (" $x_0 " " $x " " $..h
              ") " (Mid "body") ")")
        "的表达式都是一个值."
        (P "这种值的消去子是什么呢?")
        ((comment)
         "记号" (M $x "&nbsp;" $..h)
         "的意思是零个或更多的" $x ", 因此"
         (M $x_0 "&nbsp;" $x "&nbsp;" $..h)
         "的意思是一个或更多的" $x "."))
    (Rd "我们唯一能对函数做的事情就是将其应用于参数上."
        (P "这样的话函数怎么拥有消去子呢?")))
   ((dialogue)
    (Ld "应用函数于参数" (Em "是")
        "函数的消去子.")
    (Rd "好吧."))
   ((law)
    (Center "消去函数")
    (P "应用函数于参数" (Em "是")
       "函数的消去子."))
   ((dialogue)
    (Ld (CodeB "(λ (flavor)
  (cons flavor 'lentils))")
        "的值是什么?"
        ((tcomment)
         "lentils指的是小扁豆."))
    (Rd "它自" (Code "λ")
        "起, 因而它已经是一个值了."))
   ((dialogue)
    (Ld "是的."
        (CodeB "((λ (flavor)
   (cons flavor 'lentils))
 'garlic)")
        "的值是什么呢?")
    (Rd "那必然是" (Code "(cons 'garlic 'lentils)")
        ", 如果" (Code "λ") "和"
        (Code "lambda") "的行为一致且" (Code "cons")
        "是一个构造子的话."
        (P "但是这难道不意味着即便" (Code "cons")
           "表达式已经是一个值了, " (Code "cons")
           "的第一个参数仍然会被求值吗?")))
   ((dialogue #:id "lam-app")
    (Ld "不, 并非如此, 但是这是个非常好的问题. "
        "替换这个" (Code "λ") "表达式的"
        (Code "flavor") "行为之发生是因为该"
        (Code "λ") "表达式被应用于了一个参数, "
        "而不是因为" (Code "cons") "."
        ((comment)
         "一致地替换一个变量以一个表达式有时被称为"
         (Em "替换(substitution)") ".")
        "这个" (Code "λ") "表达式的体的每个"
        (Code "flavor") "都会被替换为"
        (Code "'garlic") ", 不论环绕"
        (Code "flavor") "的表达式是什么.")
    (Rd "因此, 也就是说"
        (CodeB "((λ (root)
   (cons root
         (cons (+ 1 2) root)))
 'potato)")
        "的值应该是"
        (CodeB "(cons 'potato
      (cons (+ 1 2) 'potato))")
        "对吗?"))
   ((dialogue)
    (Ld "为什么前一个框里的"
        (CodeB "(+ 1 2)")
        "不需要计算呢?")
    (Rd "整个表达式以" (Code "cons")
        "为顶, 故其已是值."))
   ((dialogue)
    (Ld (Ref "lam-app") "里有点夸大其词. "
        "如果以下" (Code "λ")
        "表达式的体中的" (Code (U "root"))
        "出现在另一个同名的" (Code "λ")
        "之下的话, 那么它就不会被替换了."
        (CodeB "((λ (root)
   (cons " (U "root") "
         (λ (root) root)))
 'carrot)")
        "的值是什么?"
        ((tcomment)
         "这里的例子有两个同名但不同的变量, 以示对比."))
    (Rd "那必然是"
        (CodeB "(cons 'carrot
      (λ (root) root))")
        "因为内层的" (Code "root")
        "出现在一个同名的" (Code "λ")
        "表达式之下."))
   ((dialogue)
    (Ld (Code "λ") "的确和" (Code "lambda")
        "行为一致, 因而这的确是正确的答案."
        (P "成为一个"
           (CodeB "(→ Atom (Pair Atom Atom))")
           "就是要成为一个" (Code "λ")
           "表达式, 当其应用于一个" (Code "Atom")
           "作为参数时, 则求值至一个"
           (CodeB "(Pair Atom Atom)"))
        ((comment)
         (Code "(→ Atom (Pair Atom Atom))") "读作"
         (Q "箭头 原子 " (Em "暂停") " 序对 原子 原子")
         ", 并且" (Code "→") "可以写成是两个字符的版本: "
         (Code "->") "."))
    (Rd "那以这样的" (Code "λ") "表达式为值的表达式呢?"))
   ((dialogue)
    (Ld "是的, 那些表达式也是"
        (CodeB "(→ Atom (Pair Atom Atom))")
        "因为当给出一个" (Code "Atom")
        "作为其参数时, 它们也会变成"
        (CodeB "(Pair Atom Atom)"))
    (Rd "它们也是"
        (CodeB "(→ (car (cons Atom 'pepper))
   (Pair (cdr (cons 'salt Atom))
         Atom))")
        "吗?"))
   ((dialogue)
    (Ld "的确如此, 因为"
        (CodeB "(car (cons Atom 'pepper))")
        "是" (Code "Atom") "而"
        (CodeB "(cdr (cons 'salt Atom))")
        "也是" (Code "Atom") ".")
    (Rd "提问何谓两个表达式为相同的"
        (Code "Nat") ", " (Code "Atom")
        ", " (Code "(Pair Nat Atom)")
        "都是有意义的."
        (P "提问何谓两个表达式为相同的"
           (CodeB "(→ Nat Atom)")
           "或者相同的"
           (CodeB "(→ (Pair Atom Nat) Nat)")
           "也是有意义的吗?")))
   ((dialogue)
    (Ld "是的, 的确如此. 两个表达式为相同的"
        (CodeB "(→ Nat Atom)")
        "当它们的值是相同的"
        (CodeB "(→ Nat Atom)"))
    (Rd "它们的值是" (Code "λ")
        "表达式. 两个" (Code "λ")
        "为相同的"
        (CodeB "(→ Nat Atom)")
        "是什么意思呢?"))
   ((dialogue #:id "twin")
    (Ld "两个接受 (expect) 相同数目的参数的"
        (Code "λ") "是相同的, 如果它们的体是相同的. "
        "例如, 两个" (Code "λ") "表达式是相同的"
        (CodeB "(→ Nat (Pair Nat Nat))")
        "如果它们的体是相同的"
        (CodeB "(Pair Nat Nat)"))
    (Rd "这意味着"
        (CodeB "(λ (x) (cons x x))")
        "和"
        (CodeB "(λ (y) (cons y y))")
        "不是相同的"
        (CodeB "(→ Nat (Pair Nat Nat))")
        "吗?"))
   ((dialogue)
    (Ld "这两个表达式有什么不同之处呢?")
    (Rd "参数的名字是不同的. "
        "尽管如此, 这通常无关紧要. "
        "现在紧不紧要呢?"))
   ((dialogue #:id "rename")
    (Ld "两个" (Code "λ") "表达式也是相同的, "
        "如果可以通过一致地对于参数换名使得"
        "它们的体变得相同."
        ((comment)
         "一致地对于变量换名常被称为"
         (Em $alpha "变换(alpha-conversion)")
         ". 感谢Alonzo Church (1903-1995).")
        "一致地对于变量换名不会改变任何东西的意义.")
    (Rd (CodeB "(λ (a d) (cons a d))")
        "和"
        (CodeB "(λ (d a) (cons a d))")
        "是相同的"
        (CodeB "(→ Atom Atom (Pair Atom Atom))")
        "吗?"))
   ((law)
    (Center "应用之始律")
    (P "如果" $f "是一个"
       (CodeB "(→ " $Y " " $X ")")
       "而" $arg "是一个" $Y ", 那么"
       (CodeB "(" $f " " $arg ")")
       "是一个" $X "."))
   ((law)
    (Center (Code "λ") "始第一诫")
    (P "两个接受 (expect) 相同数目的参数的"
       (Code "λ") "是相同的, "
       "如果在一致地对于它们的变量进行换名之后, "
       "它们的体是相同的."))
   ((law)
    (Center (Code "λ") "始第二诫")
    (P "如果" $f "是一个"
       (CodeB "(→ " $Y " " $X ")")
       "那么" $f "和"
       (CodeB "(λ (" $y ") (" $f " " $y "))")
       "是相同的"
       (CodeB "(→ " $Y " " $X ")")
       "只要" $y "不出现在" $f "中.")
    ((tcomment)
     "这里的" (Q "出现") "应该指的是"
     (Q "自由出现") "."))
   ((dialogue)
    (Ld "不, 并非如此, 因为对于第二个" (Code "λ")
        "表达式中的变量进行一致换名以匹配第一个"
        (Code "λ") "表达式的参数将会产生"
        (CodeB "(λ (a d) (cons d a))")
        "而" (Code "(cons d a)") "和"
        (Code "(cons a d)") "并非相同的"
        (Code "(Pair Atom Atom)") ".")
    (Rd (CodeB "(λ (y) (car (cons y y)))")
        "呢? 它和"
        (CodeB "(λ (x) x)")
        "是相同的"
        (CodeB "(→ Nat Nat)")
        "吗?"))
   ((law)
    (Center "对于变量换名之律")
    (P "一致地对于变量换名不会改变任何东西的意义."))
   ((dialogue)
    (Ld "首先, 一致地将" $y "换名为" $x
        ". 现在, 问题就变为"
        (CodeB "(car (cons x x))")
        "和" (Code "x") "是否是相同的"
        (Code "Nat") ".")
    (Rd "恰有两种方式可以使得两个表达式成为相同的"
        (Code "Nat") ". 一种是它们的值都是"
        (Code "zero") ". 另一种是它们的值都以"
        (Code "add1") "为顶, 而这两个"
        (Code "add1") "的参数是相同的"
        (Code "Nat") "."
        (P "这些表达式并非" (Code "Nat")
           "值, 因为它们既不以" (Code "add1")
           "为顶, 也不是" (Code "zero") ".")))
   ((dialogue)
    (Ld (Code "x") "的值尚不可知, 因为这个"
        (Code "λ") "表达式还没有被应用于一个参数. "
        "但是, 当这个" (Code "λ")
        "表达式已被应用于一个参数时, "
        (Code "x") "的值" (Em "仍然是")
        "一个" (Code "Nat") ", 因为...")
    (Rd "...因为这个" (Code "λ")
        "表达式是一个"
        (CodeB "(→ Nat Nat)")
        "故参数" (Code "x")
        "不可能是任何其他什么东西."))
   ((dialogue)
    (Ld "并非值且因为变量的缘故"
        (Em "还") "不能被求值的表达式被称为"
        (Em "中立(neutral)") "的.")
    (Rd "这意味着"
        (CodeB "(cons y 'rutabaga)")
        "是中立的咯?"
        ((tcomment)
         "rutabaga是芜菁甘蓝的意思.")))
   ((dialogue)
    (Ld "不, 它并非中立, 因为"
        (CodeB "(cons y 'rutabaga)")
        "是一个值."
        (P "如果" (Code "x") "是一个"
           (Code "(Pair Nat Atom)")
           ", 那么"
           (CodeB "(cdr x)")
           "是一个值吗?"))
    (Rd "不是, 因为" (Code "cdr")
        "是一个消去子, 而消去子把值拆散."
        (P "在不知道" (Code "x")
           "的值的情况下, 没有办法找出"
           (Code "(cdr x)")
           "的值, 故" (Code "(cdr x)")
           "是中立的.")))
   ((dialogue)
    (Ld "中立表达式使得我们有必要扩展对于何谓相同的看法. "
        "每个变量与其自身都是相同的, 不管其类型如何. "
        "这是因为变量只能被" (Em "一致")
        "地替换, 所以说一个变量的两次出现"
        "不可能被代之以不同的值.")
    (Rd "因此, 如果我们假定" (Code "y")
        "是一个" (Code "Nat") ", 那么"
        (CodeB "(car (cons y 'rutabaga))")
        "和" (Code "y") "是相同的" (Code "Nat")
        ", 这是因为该" (Code "car")
        "表达式的规范形式是" (Code "y")
        ", 而" (Code "y") "和" (Code "y")
        "是相同的" (Code "Nat") "."))
   ((dialogue)
    (Ld "的确如此, 并且类似地, 我们有"
        (CodeB "(λ (x) (car (cons x x)))")
        "和"
        (CodeB "(λ (x) x)")
        "是相同的"
        (CodeB "(→ Nat Nat)"))
    (Rd "是的, 因为中立表达式" (Code "x")
        "和" (Code "x") "是相同的"
        (Code "Nat") "."))
   ((dialogue)
    (Ld (CodeB "(λ (x) (car x))")
        "和"
        (CodeB "(λ (y) (car y))")
        "是相同的"
        (CodeB "(→ (Pair Nat Nat) Nat)")
        "吗?")
    (Rd "我们应该会这样认为, 但是理由是什么呢?"))
   ((dialogue)
    (Ld "第一步应该是一致地将" (Code "y")
        "换名为" (Code "x") "."
        (P (CodeB "(λ (x) (car x))")
           "和"
           (CodeB "(λ (x) (car x))")
           "是相同的"
           (CodeB "(→ (Pair Nat Nat) Nat)")
           "吗?"))
    (Rd "是的, 如果假定"
        (CodeB "(car x)")
        "和"
        (CodeB "(car x)")
        "是相同的" (Code "Nat") "."
        (P "但是" (Code "(car x)")
           "并不是一个变量, 在知道"
           (Code "x") "的值之前我们都不可能找出"
           (Code "(car x)") "的值.")))
   ((dialogue)
    (Ld "如果两个表达式以等同的消去子为顶, "
        "并且两个消去子的参数是相同的, "
        "那么这两个表达式也是相同的. "
        "字面上等同的中立表达式是相同的, "
        (Em "不管它们的类型如何."))
    (Rd "因而"
        (CodeB "(car x)")
        "和"
        (CodeB "(car x)")
        "是相同的" (Code "Nat")
        ", 若假定" (Code "x")
        "是一个" (Code "(Pair Nat Nat)") "."))
   ((law)
    (Center "中立表达式之诫")
    (P "字面上等同的中立表达式是相同的, "
       (Em "不管它们的类型如何.")))
   ((dialogue)
    (Ld (CodeB "(λ (a d) (cons a d))")
        "是一个"
        (CodeB "(→ Atom Atom (Pair Atom Atom))")
        "吗?")
    (Rd (Code "→") "后面跟着更多的表达式意味着什么呢?"))
   ((dialogue)
    (Ld (Code "→") "后面跟着的表达式, 除了最后一个, "
        "都是参数的类型. 最后一个则是值的类型."
        ((comment)
         "最后一个类型读的时候前面会有"
         (Em "停顿") "."))
    (Rd "好吧, 那么"
        (CodeB "(λ (a d) (cons a d))")
        "的确是一个"
        (CodeB "(→ Atom Atom (Pair Atom Atom))")
        "这些表达式不可避免地正在逐渐变长."))
   ((dialogue)
    (Ld "一种缩短它们的方式是小心地使用"
        (Code "define") ", 如" (Ref "definition")
        "那样, 其总是允许我们用简短的名字"
        "来代替冗长的表达式.")
    (Rd "好想法."))
   ((dialogue)
    (Ld "设构造子" (Code "cons")
        "被应用于" (Code "'celery") "和"
        (Code "'carrot")
        ", 我们可以称这个值为"
        (Code "vegetables") "."
        (CodeB "(claim vegetables
  (Pair Atom Atom))
(define vegetables
  (cons 'celery 'carrot))")
        "从现在开始, 每当名字"
        (Code "vegetables") "被使用, 它就和"
        (CodeB "(cons 'celery 'carrot)")
        "是相同的"
        (CodeB "(Pair Atom Atom)")
        "因为这就是" (Code "vegetables")
        "如何被" (Code "define") "的."
        ((tcomment)
         "celery指的是西芹."))
    (Rd "为什么" (Code "claim")
        "后面有写着"
        (CodeB "(Pair Atom Atom)")
        "呢?"))
   ((law)
    (Center (Code "define") "之律和诫")
    (P "遵照"
       (P (Code "(claim " $name " " $X ")")
          "和"
          (Code "(define " $name " " $expr ")"))
       "如果"
       (P $expr "是一个" $X)
       "那么"
       (P $name "是一个" $X)
       "并且"
       (P $name "和" $expr "是相同的" $X)))
   ((dialogue #:id "nested-Pair")
    (Ld (Code "(Pair Atom Atom)")
        "描述了我们可以怎样使用"
        (Code "vegetables")
        ". 例如, 我们知道"
        (Code "(car vegetables)")
        "是一个" (Code "Atom")
        ", 而" (Code "(cons 'onion vegetables)")
        "是一个"
        (CodeB "(Pair Atom (Pair Atom Atom))")
        ((comment)
         "这对于小扁豆汤 (lentil soup) 来说也是一个好的开始."))
    (Rd "啊, 懂了."))
   ((dialogue)
    (Ld (CodeB "vegetables")
        "和"
        (CodeB "(cons (car vegetables)
      (cdr vegetables))")
        "是相同的"
        (CodeB "(Pair Atom Atom)")
        "吗?")
    (Rd "的确如此, 因为每个表达式的值都是一个序对, 并且其"
        (Code "car") "是" (Code "'celery") "而"
        (Code "cdr") "是" (Code "'carrot") "."))
   ((dialogue)
    (Ld "实际上, 每当" $p "是一个"
        (Code "(Pair Atom Atom)")
        "时, 那么" $p "和"
        (CodeB "(cons (car " $p ") (cdr " $p "))")
        "是相同的"
        (CodeB "(Pair Atom Atom)")
        "找出" (Code "(car " $p ")") "和"
        (Code "(cdr " $p ")")
        "的值是没有必要的.")
    (Rd "这看起来很合理."))
   ((law)
    (Center (Code "cons") "之第二诫")
    (P "如果" $p "是一个"
       (Code "(Pair " $A " " $D ")")
       ", 那么它和"
       (Code "(cons (car " $p ") (cdr " $p "))")
       "是相同的"
       (Code "(Pair " $A " " $D ")") "."))
   ((dialogue #:id "wrong-five")
    (Ld "以下定义可以允许吗?"
        (CodeD "(claim five Nat)
(define five (+ 7 2))"))
    (Rd "什么鬼?"))
   ((dialogue)
    (Ld "尽管可能是个愚蠢的想法, 但它是可以被允许的."
        (CodeB "(+ five 5)")
        "的规范形式是什么?")
    (Rd "那必然是" (Code "10")
        ", 因为五 (five) 加" $5 "等于十."))
   ((dialogue)
    (Ld "再想想. 请记得" (Code "five")
        "的奇怪定义...")
    (Rd "...哦, 好吧, 那应该是" (Code "14")
        ", 因为" (Code "five")
        "被定义成了" (Code "9") "."))
   ((dialogue)
    (Ld "的确如此.")
    (Rd (Em "这个") "定义可以允许吗? "
        "似乎它看上去不那么蠢."
        (CodeD "(claim zero Nat)
(define zero 0)")))
   ((dialogue)
    (Ld "虽然和将" (Code "five")
        "定义为" (Code "9")
        "相比没那么愚蠢, 但是这个定义也是"
        "不被允许的."
        (P "已经被使用了的名字, "
           "不论是用于构造子, 消去子, "
           "还是之前的定义, "
           "都不适合再与" (Code "claim")
           "或者" (Code "define")
           "一起使用了."))
    (Rd "好."))
   ((law)
    (Center "定义中的名字")
    (P "在Pie语言中, 只有没有用过的名字, "
       "才能和" (Code "claim")
       "或者" (Code "define")
       "一起使用, 不论是用作构造子, 消去子, "
       "还是之前的定义."))
   ((dialogue)
    (Ld (Code "Nat") "有一个消去子可以区分值为"
        (Code "zero") "的" (Code "Nat")
        "和值以" (Code "add1") "为顶的"
        (Code "Nat") ". 这个消去子被称为"
        (Code "which-Nat") ".")
    (Rd (Code "which-Nat")
        "到底是如何辨别应该是哪一种"
        (Code "Nat") "的呢?"))
   ((dialogue #:id "which-Nat")
    (Ld "一个" (Code "which-Nat")
        "表达式具有三个参数: "
        $target ", " $base
        ", " $step ":"
        (CodeB "(which-Nat " $target "
  " $base "
  " $step ")")
        (Code "which-Nat")
        "判断" $target "是否是" (Code "zero")
        ", 如果是, 那么整个" (Code "which-Nat")
        "表达式的值即" $base "的值. 否则的话, 如果"
        $target "是" (Code "(add1 " $n ")")
        ", 那么整个" (Code "which-Nat")
        "表达式的值即" (Code "(" $step " " $n ")")
        "的值.")
    (Rd "因此, " (Code "which-Nat")
        "既要判断一个数字是否是" (Code "zero")
        ", 又要在数字并非" (Code "zero")
        "时去除其顶的" (Code "add1") "."))
   ((dialogue)
    (Ld "的确如此."
        (CodeB "(which-Nat zero
  'naught
  (λ (" (Dim "n") ")
    'more))")
        "的规范形式是什么呢?")
    (Rd "必然是" (Code "'naught")
        ", 因为其target " (Code "zero")
        "是" (Code "zero") ", 故整个"
        (Code "which-Nat")
        "表达式的值是" $base
        ", 即" (Code "'naught") "."
        (P "为什么" (Code (Dim "n"))
           "是黯淡的?")))
   ((dialogue #:id "dim")
    (Ld "黯淡是用来指出" (Code (Dim "n"))
        "在那个" (Code "λ")
        "表达式的体里没有被使用. "
        "没有被使用的名字都将以黯淡的形式出现.")
    (Rd "为什么没有被使用呢?"))
   ((dialogue)
    (Ld (Code "which-Nat")
        "提供了使用更小的" (Code "Nat")
        "的可能性, 但是它并不要求一定使用. "
        "当然, 为了提供这种可能性, "
        (Code "which-Nat")
        "的最后一个参数必须要接受一个"
        (Code "Nat") ".")
    (Rd "好."))
   ((law)
    (Center "黯淡的名字")
    (P "没有被使用的名字是黯淡的, "
       "但是它们的确有必要呆在那里."))
   ((dialogue)
    (Ld (CodeB "(which-Nat 4
  'naught
  (λ (" (Dim "n") ")
    'more))")
        "的值是什么?")
    (Rd "必然是" (Code "'more")
        ", 因为" (Code "4")
        "是另一种书写" (Code "(add1 3)")
        "的方式, 而其以" (Code "add1")
        "为顶."
        (CodeB "((λ (" (Dim "n") ") 'more) 3)")
        "的规范形式为"
        (CodeB "'more")))
   ((law)
    (Center (Code "which-Nat") "之律")
    (P "如果" $target "是一个" (Code "Nat")
       ", " $base "是一个" $X
       ", 并且" $step "是一个"
       (CodeB "(→ Nat " $X ")")
       "那么"
       (CodeB "(which-Nat " $target "
  " $base "
  " $step ")")
       "是一个" $X "."))
   ((law)
    (Center (Code "which-Nat") "之第一诫")
    (P "如果"
       (CodeB "(which-Nat zero
  " $base "
  " $step ")")
       "是一个" $X ", 那么其和" $base
       "是相同的" $X "."))
   ((law)
    (Center (Code "which-Nat") "之第二诫")
    (P "如果"
       (CodeB "(which-Nat (add1 " $n ")
  " $base "
  " $step ")")
       "是一个" $X ", 那么其和"
       (Code "(" $step " " $n ")")
       "是相同的" $X "."))
   ((dialogue)
    (Ld (CodeB "(which-Nat 5
  0
  (λ (n)
    (+ 6 n)))")
        "的规范形式是什么?")
    (Rd "难道说是" (Code "11")
        ", 因为"
        (CodeB "((λ (n) (+ 6 n)) 5)")
        "是" (Code "11") "?"))
   ((dialogue)
    (Ld "这个的规范形式应该是"
        (Code "10") ", 因为一个"
        (Code "which-Nat")
        "表达式的值是将藏在其target底下的"
        (Code "Nat") "作为参数传给其step得到的."
        ((tcomment)
         "也就是说, 要扒开一层"
         (Code "add1") "."))
    (Rd "啊, 所以说规范形式为" (Code "10")
        "是因为"
        (CodeB "((λ (n) (+ 6 n)) 4)")
        "是" (Code "10") "."))
   ((dialogue);errata?
    (Ld "请定义一个叫做" (Code "gauss")
        "的函数, 使得" (Code "(gauss " $n ")")
        "是从" (Code "zero") "到" $n
        "的所有" (Code "Nat") "之和."
        (P (Code "gauss") "的类型应该是什么?")
        ((comment)
         "根据民间传说, Carl Friedrich Gauss "
         "(1777-1855) 在他上小学被要求"
         "加起一个很长的数列时发现了"
         (&= (&+ $0 $..c $n)
             (~ (&i* $n (@+ $n $1)) $2)) "."))
    (Rd "将多个" (Code "Nat") "加起来当然应该是一个"
        (Code "Nat") "."
        (CodeB "(claim gauss
  (→ Nat Nat))")))
   ((dialogue)
    (Ld "是的. 现在请定义它.")
    (Rd "怎么做呢?"))
   ((dialogue)
    (Ld "第一步是挑选一个样例参数. "
        "好的选择大概应该在" (Code "5")
        "至" (Code "10")
        "之间, 这样的话既足够大而有趣, "
        "也足够小而可控.")
    (Rd (Code "5")
        "怎么样, 然后呢?"))
   ((dialogue)
    (Ld "听上去不错."
        (CodeB "(gauss 5)")
        "的规范形式应该是什么?")
    (Rd "应该是" (&+ $0 $1 $2 $3 $4 $5)
        ", 即" 15 "."
        ((tcomment)
         "当然, 更严格地说, 是" (Code "15") ".")))
   ((dialogue #:id "almost-answer")
    (Ld "接下来的一步应该是收缩参数."
        (P (Code "(gauss 4)") "和"
           (Code "(gauss 5)")
           "差不多, 前者是" (Code "10")
           ", 后者是" (Code "15") ".")
        (P "白框框住的是未知的代码, "
           "而白纸黑字的部分则是其中的已知部分. "
           "在这被框住的内容之中, 我们该如何从"
           (CodeB "(gauss 4)")
           "得到"
           (CodeB "(gauss 5)")
           "呢?")
        (CodeB
         "    "
         (Frame "    "
                (WB "(gauss 4)")
                "    ")))
    (Rd "我们必须给" (Code "(gauss 4)")
        "加上" (Code "5") ", 而和为"
        (Code "15") "."
        (CodeB
         "    "
         (Frame "(+ 5 "
                (WB "(gauss 4)")
                ")"))))
   ((dialogue)
    (Ld "接下来, 我们要使得这种手段对于任意的以"
        (Code "add1") "为顶的" (Code "Nat")
        "成立."
        (P "如果" $n "是一个" (Code "Nat")
           ", 那么该如何从"
           (CodeB "(gauss " $n ")")
           "得到"
           (CodeB "(gauss (add1 " $n "))")
           "呢?")
        (CodeB
         "    "
         (Frame "    "
                (WB "(gauss " $n ")")
                "    "))
        (P "请记得" (Code "5") "是书写"
           (Code "(add1 4)")
           "的另一种方式."))
    (Rd "找出" (Code "(gauss (add1 " $n "))")
        "的方法在于将前一个框中的答案里的"
        (Code "4") "替换成" $n "."
        (CodeB
         "    "
         (Frame "(+ (add1 " $n ") "
                (WB "(gauss " $n ")")
                ")"))
        (Code "zero") "怎么样呢?"))
   ((dialogue)
    (Ld (Code "(gauss zero)")
        "是什么?")
    (Rd "显然是" (Code "0") "."))
   ((dialogue #:id "pesudo-gauss")
    (Ld "现在让我们来定义" (Code "gauss") "."
        (P "请记得白色的框和白纸黑字的部分."))
    (Rd "小菜一碟! 名字" (Code "n-1")
        "暗示了其代表着一个脱去了" (Code "n")
        "的一层" (Code "add1") "的" (Code "Nat")
        ", 或者说比" (Code "n") "小一的"
        (Code "Nat") "."
        (CodeD
         "(define gauss
  (λ (n)
    (which-Nat n
      0
      (λ (n-1)
        " (Frame "(+ (add1 n-1) "
                 (WB "(gauss n-1)")
                 ")")
          "))))")))
   ((dialogue)
    (Ld "很好的尝试. 如果递归可以作为一个选项, "
        "那么这就不需要用虚线框住了. 可惜, 递归"
        (Em "不是") "一个选项.")
    (Rd "为什么不让其成为一个选项呢?"))
   ((dialogue)
    (Ld "因为递归不是一个选项.")
    (Rd "为什么不让其成为一个选项呢?"))
   ((dialogue)
    (Ld "因为递归不是一个选项.")
    (Rd "好吧, 请解释为什么递归不是一个选项."))
   ((dialogue)
    (Ld "递归之所以不是一个选项, "
        "是因为每个表达式都必须具有一个值. "
        "一些递归定义使得我们可以写下"
        "没有值的表达式.")
    (Rd "举个例子呢? 一个递归定义和一个没有值的表达式."))
   ((dialogue)
    (Ld (Code "forever") "就是一个这样的例子."
        (CodeD "(claim forever (→ Nat Atom))
(define forever
  (λ (and-ever)
    (forever and-ever)))")
        (Code "(forever 71)")
        "的值是什么?")
    (Rd "好问题."
        (P "为什么它被虚线框住了?")))
   ((dialogue)
    (Ld "递归不是一个选项, 故像"
        (Code "forever")
        "这样的递归定义将永远被"
        "虚线框住.")
    (Rd "但是, 对于像" (Code "gauss")
        "这种需要递归的定义该怎么办呢?"))
   ((dialogue)
    (Ld "递归定义存在着一种安全的替代品, "
        "其允许我们写下" (Code "gauss")
        "而不需要包括" (Code "gauss")
        "这个名字, 对于其他诸多类似的定义"
        "也是如此.")
    (Rd "这里是" (Code "gauss")
        "的安全替代版本定义的起点."
        (CodeD
         "(define gauss
  (λ (n)
    "
         (Frame "gauss不是一个选项!")
         "))")))
   ((dialogue)
    (Ld "就目前的走向来看, 还是正确的. "
        "要义在于" (Code "gauss")
        "不能出现在其自身的定义里.")
    (Rd "现在" (Q "递归不是一个选项")
        "这句话的含义是清晰的了."
        (P "难道说这意味着Pie中不可能写下"
           (Code "gauss") "吗?")))
   ((dialogue)
    (Ld "Pie中写下" (Code "gauss")
        (Em "是") "可能的, 但是"
        (Code "which-Nat") "和"
        (Code "define")
        "还不足以让我们能够应对这个任务. "
        "我们需要不同的消去子, "
        "但是时机还远未成熟.")
    (Rd "耐心是一种美德."))
   ((dialogue)
    (Ld "给诸如" (Code "(Pair Nat Nat)")
        "这样的表达式定义更加简短的名字也是可能的.")
    (Rd "这种情况下的" (Code "claim")
        "是什么样的呢?"))
   ((dialogue)
    (Ld "另一个好问题!"
        (P "诸如" (Code "Atom")
           ", " (Code "Nat")
           ", " (Code "(Pair Atom Nat)")
           "这样的表达式是类型, "
           "而每一个这样的类型都是一个"
           $U:script ".")
        ((comment)
         $U:script "读作" (Q "you")
         ", 是" (Em "universe(宇宙)")
         "的缩写, 以其描述了"
         (Em "所有") "的类型 (除了自身)."))
    (Rd "类型是值吗?"))
   ((dialogue)
    (Ld "有的类型是值."
        (P "一个为类型的表达式是一个值, "
           "当其以某个类型构造子为顶时. "
           "到目前为止, 我们见过的类型构造子有"
           (Code "Nat") ", "
           (Code "Atom") ", "
           (Code "→") ", 和"
           $U:script "."))
    (Rd "所有的类型都是值吗?"))
   ((law)
    (Center "类型值")
    (P "一个由某个类型描述的表达式是一个值, "
       "当其以某个构造子为顶. 类似地, "
       "一个为类型的表达式是一个(类型)值, "
       "当其以某个类型构造子为顶."))
   ((dialogue)
    (Ld "并非如此."
        (CodeB "(car (cons Atom 'prune))")
        "是一个类型, 但不是一个值, 因为"
        (Code "car") "既不是一个构造子, "
        "也不是一个类型构造子.")
    (Rd "什么样的表达式由"
        (CodeB "(car (cons Atom 'prune))")
        "描述呢?"
        ((tcomment)
         "prune, 西梅干.")))
   ((dialogue)
    (Ld "因为"
        (CodeB "(car (cons Atom 'prune))")
        "和"
        (CodeB "Atom")
        "是相同的类型, 所以"
        (CodeB "(car (cons Atom 'prune))")
        "所描述的表达式和" (Code "Atom")
        "是同样的.")
    (Rd "类型构造子和构造子有什么区别?"))
   ((dialogue)
    (Ld "类型构造子构造类型, 而构造子 (或者说"
        (Em "数据") "构造子) 构造由那些类型描述的值."
        (P "作出一个表达式是一个类型的判断"
           "需要知道其构造子. 但是, " $U:script
           "的意义并不由知道所有的类型构造子给出, "
           "因为可以引入新的类型."))
    (Rd (Code "(cons Atom Atom)")
        "是一个" $U:script "吗?"))
   ((dialogue)
    (Ld "不是, 但"
        (CodeB "(cons Atom Atom)")
        "是一个"
        (CodeB "(Pair " $U:script
               " " $U:script ")")
        "一个原子, 例如" (Code "'plum")
        ", 是一个" (Code "Atom")
        ". 从另一方面来说, " (Code "Atom")
        "不是一个" (Code "Atom")
        ", 而是一个由" $U:script
        "刻画的类型.")
    (Rd "让我们来思考"
        (Code "(Pair Atom Atom)")
        "的事情."
        (CodeB "(cons Atom Atom)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "吗?"))
   ((dialogue)
    (Ld "不是, 这是因为" (Code "Atom")
        "是一个类型, 而不是一个"
        (Code "Atom") ".")
    (Rd $U:script "是一个"
        $U:script "吗?"))
   ((dialogue #:id "paradox")
    (Ld "不是, 但" $U:script
        "的确是一个类型. 没有表达式"
        "可以成为其自身的类型."
        ((comment)
         $U:script "可以是一个"
         (_ $U:script $1)
         ", 而" (_ $U:script $1)
         "可以是一个"
         (_ $U:script $2)
         ", 如此类推. 感谢"
         "Bertrand Russell (1872-1970). "
         "感谢Jean-Yves Girard (1947-). "
         "在这本书里, 单一的"
         $U:script "就够用了, "
         "因为我们不需要" $U:script
         "被某个类型刻画."))
    (Rd "每个为" $U:script
        "的表达式是否也都是一个类型呢?"))
   ((dialogue)
    (Ld "是的, 如果" $X "是一个" $U:script
        ", 那么" $X "是一个类型.")
    (Rd "是否每个类型都由" $U:script "刻画呢?"))
   ((law)
    (Center "每个" $U:script "都是一个类型")
    (P "每个由" $U:script "刻画的表达式都是一个类型, "
       "但是不是每个类型都由" $U:script "刻画."))
   ((dialogue)
    (Ld "每个由" $U:script "刻画的表达式都是一个类型, "
        "但是不是每个为类型的表达式都由"
        $U:script "刻画.")
    (Rd (CodeB "(cons Atom Nat)")
        "是一个"
        (CodeB "(Pair " $U:script " " $U:script ")")
        "吗?"))
   ((dialogue #:id "Pear-definition")
    (Ld "的确如此."
        (P "定义" (Code "Pear")
           ", 其意指" (Code "Nat")
           "的序对的类型.")
        ((tcomment)
         "pear和pair同音."))
    (Rd "那必然是"
        (CodeB "(claim Pear " $U:script ")
(define Pear
  (Pair Nat Nat))")
        "从现在开始, " (Code "Pear") "的意思是"
        (CodeB "(Pair Nat Nat)")
        "这个名字只有四个字符, 而其代表的类型"
        "有十四个字符."))
   ((dialogue)
    (Ld "每当" (Code "Pear")
        "出现时, 是不是它就和"
        (Code "(Pair Nat Nat)")
        "是相同的类型?")
    (Rd "的确, 根据" (Code "define") "之诫."))
   ((dialogue)
    (Ld (Code "(cons 3 5)")
        "是一个" (Code "Pear") "吗?")
    (Rd "是的, 因为"
        (CodeB "(cons 3 5)")
        "是一个"
        (CodeB "(Pair Nat Nat)")
        "而"
        (CodeB "Pear")
        "恰被" (Em "定义") "为这个类型."))
   ((dialogue)
    (Ld "这是很好的论证.")
    (Rd (Code "Pear") "是一个值吗?"))
   ((dialogue)
    (Ld "不是. 由" (Code "define")
        "定义的名字既不是类型构造子也不是构造子, "
        "故并非值."
        (P (Code "Pear") "有消去子吗?"))
    (Rd "你说的消去子是不是指的是将类型"
        (Code "Pear") "的值拆开的那种东西?"))
   ((dialogue)
    (Ld "是的."
        (P (Code "Pear")
           "的消去子必然要使得具有类型"
           (Code "Pear") "的值中的信息能够被使用."))
    (Rd "什么叫做使得信息能够被使用?"))
   ((dialogue)
    (Ld "使得任意" (Code "Pear")
        "中的信息能够被使用的" (Code "Pear")
        "的消去子是这样的一种东西, 其可以应用函数于"
        (Code "Pear") "的两个" (Code "Nat") "参数.")
    (Rd "好吧."))
   ((dialogue)
    (Ld "什么样的函数可以应用到两个作为参数的"
        (Code "Nat") "上呢?")
    (Rd "一个例子: " (Code "+") "."))
   ((dialogue)
    (Ld "交换" (Code "Nat") "的表达式是什么样的呢?")
    (Rd (CodeB "(λ (a d)
  (cons d a))")
        "如何?"))
   ((dialogue)
    (Ld "非常好. 从一个" (Code "Pear")
        "里提取出第一个" (Code "Nat")
        "的表达式是什么样的呢?")
    (Rd "那必然是"
        (CodeB "(λ (a d) a)")))
   ((dialogue)
    (Ld "非常接近了. 实际上, 应该是"
        (CodeB "(λ (a " (Dim "d") ") a)"))
    (Rd "你真无聊. 但是, 没有这个黯淡的效果, "
        "表达式本身也应该是正确的吧?"))
   ((dialogue)
    (Ld "是这样的. 为了从一个" (Code "Pear")
        "中得到一个类型为" $X
        "的值, 我们必然需要有一个类型为"
        (CodeB "(→ Nat Nat " $X ")")
        "的表达式. " (Code "+")
        "的类型是什么?"
        ((comment)
         "毕竟" $X "可以是任意的类型."))
    (Rd "其取两个" (Code "Nat") "而产生一个"
        (Code "Nat") ", 故类型必然是"
        (CodeB "(→ Nat Nat Nat)")))
   ((dialogue)
    (Ld "很好."
        (P "当" (Code "a") "和" (Code "d")
           "都为" (Code "Nat") "时, "
           (CodeB "(λ (a d)
  (cons d a))")
           "的类型是什么?"))
    (Rd "显然是"
        (CodeB "(→ Nat Nat Pear)")
        "其和"
        (CodeB "(→ Nat Nat (Pair Nat Nat))")
        "相同."
        (P "一个" (Code "λ") "表达式该怎么和一个"
           (Code "Pear") "一起使用呢?")))
   ((law)
    (Center "定义是没有必要的")
    (P "没有定义也可以完成一切事情, "
       "但是定义的确有助于理解."))
   ((dialogue)
    (Ld "试试这个:"
        (CodeB "(claim Pear-maker " $U:script ")
(define Pear-maker
  (→ Nat Nat Pear))

(claim elim-Pear
  (→ Pear Pear-maker Pear))
(define elim-Pear
  (λ (pear maker)
    (maker (car pear)
           (cdr pear))))")
        "是否可以写下" (Code "elim-Pear")
        "的" (Code "claim") "而不使用"
        (Code "Pear") "或" (Code "Pear-maker")
        "呢?")
    (Rd "可以, 只需要将" (Code "Pear-maker")
        "和" (Code "Pear")
        "替换为相应的定义即可."
        (CodeD "(claim elim-Pear
  (→ (Pair Nat Nat)
     (→ Nat Nat
        (Pair Nat Nat))
     (Pair Nat Nat)))")
        "名字" (Code "Pear-maker")
        "和" (Code "Pear")
        "从不是必要的. 名字"
        (Code "elim-Pear")
        "是必要的吗?"))
   ((dialogue)
    (Ld "什么时候定义是必要的呢?")
    (Rd "什么时候都不是必要的!"))
   ((dialogue)
    (Ld "是这样的. " (Code "elim-Pear")
        "和它定义里的那个" (Code "λ")
        "表达式是相同的."
        (CodeB "(elim-Pear
 (cons 3 17)
 (λ (a d)
   (cons d a)))")
        "的值是什么?")
    (Rd (CodeB "((λ (pear maker)
   (maker (car pear) (cdr pear)))
 (cons 3 17)
 (λ (a d)
   (cons d a)))")
        "怎么样?"))
   ((dialogue)
    (Ld "一个好的开始, 但这还不是值.")
    (Rd "值是" (Code "(cons 17 3)")
        ", 这是因为" (Code "elim-Pear")
        "和其定义中的那个" (Code "λ")
        "表达式相同, 并且"
        (CodeB "(car (cons 3 17))")
        "和" (Code "3") "是相同的"
        (Code "Nat") ", 而"
        (CodeB "(cdr (cons 3 17))")
        "和" (Code "17") "是相同的"
        (Code "Nat") ", 最后"
        (CodeB "((λ (a d)
   (cons d a))
 3 17)")
        "和"
        (CodeB "(cons 17 3)")
        "是相同的" (Code "Pear") "."))
   ((dialogue)
    (Ld "将两个" (Code "Pear")
        "加起来会是什么意思呢?")
    (Rd "难道说是将每个" (Code "Pear")
        "的第一个" (Code "Nat")
        "和第二个" (Code "Nat")
        "分别加起来吗?"))
   ((dialogue)
    (Ld "很好的猜测."
        (P "这种" (Em "pearwise")
           "加法的类型是什么?")
        ((tcomment)
         "译者认为pearwise并非贴切, "
         "因为是逐" (Code "Pear")
         "的部分相加, 而不是逐"
         (Code "Pear") "相加, "
         "可能partwise是更好的选择."))
    (Rd (CodeB "(→ Pear Pear Pear)")
        "对吗?"))
   ((dialogue)
    (Ld "怎么用" (Code "elim-Pear")
        "定义pearwise加法?")
    (Rd "好难啊, 是不是两个"
        (Code "Pear")
        "都要消去, 因为这些"
        (Code "Nat")
        "都是结果的一部分?"))
   ((dialogue)
    (Ld "当然了."
        (P "请定义" (Code "pearwise+")
           ", 以使得"
           (CodeB "(pearwise+
 (cons 3 8)
 (cons 7 6))")
           "和"
           (CodeB "(cons 10 14)")
           "是相同的"
           (Code "Pear") "."))
    (Rd "首先, 将" (Code "anjou")
        "和" (Code "bosc")
        "各自拆成其相应的部分, "
        "然后再将它们的第一部分和第二部分"
        "分别加起来."
        (CodeB "(claim pearwise+
  (→ Pear Pear Pear))
(define pearwise+
  (λ (anjou bosc)
    (elim-Pear
     anjou
     (λ (a" (Sub "1") " d" (Sub "1") ")
       (elim-Pear
        bosc
        (λ (a" (Sub "2") " d" (Sub "2") ")
          (cons
           (+ a" (Sub "1") " a" (Sub "2") ")
           (+ d" (Sub "1") " d" (Sub "2") "))))))))")
        ((tcomment)
         "anjou和bosc都是梨的品种.")))
   ((dialogue)
    (Ld "或许现在应该好好休息一下, "
        "然后回来重读本章.")
    (Rd "是的, 的确是个好想法."
        (P "但是, 我们该如何到达第" (Ref "ch3") "章呢?")))
   ((dialogue)
    (Ld "通过到达第" (Ref "ch3") "章.")
    (Rd "幸亏递归不是一个选项."))
   (H2: "课间: 一叉子Pie")
   ((dialogue)
    (Ld "是时候玩Pie了呢.")
    (Rd "玩弄食物是不是有点不礼貌?"))
   ((dialogue)
    (Ld "尽管派 (pie) 的确是一种美味的食物, "
        "但是Pie是一种语言, 玩玩而已无伤大雅.")
    (Rd "让我们开始吧."))
   ((dialogue)
    (Ld "使用Pie很像是对话: "
        "其接受声明, 定义和表达式, "
        "并回之以反馈.")
    (Rd "什么样的反馈呢?"))
   ((dialogue)
    (Ld "对于声明和定义而言, "
        "反馈是判断它们是否具备意义. "
        "对于表达式而言, "
        "反馈是表达式的类型和规范形式.")
    (Rd "不具备意义会怎么样呢?"))
   ((dialogue)
    (Ld "Pie会解释它们出了什么问题, "
        "有时添加有用的提示.")
    (Rd "表达式可能会出什么问题呢?"))
   ((dialogue)
    (Ld "在吃Pie前请吃蔬菜."
        (P "尝试输入"
           (CodeB "'spinach")
           "看看会发生什么.")
        ((tcomment)
         "spinach, 菠菜."))
    (Rd "Pie回之以"
        (CodeB "(the Atom 'spinach)")
        "这里的" (Code "the")
        "是什么意思?"))
   ((dialogue)
    (Ld "其意即" (Code "'spinach")
        "是一个" (Code "Atom") "."
        (P "在Pie里, 一个表达式要么是一个类型, "
           "要么由一个类型描述. "
           "Pie可以自己找出诸多表达式的类型, "
           "包括原子."))
    (Rd (CodeB "(car 'spinach)")
        "怎么样呢?"))
   ((dialogue)
    (Ld "这个表达式并不由某个类型刻画, 因为"
        (Code "'spinach") "不是一个序对.")
    (Rd "Pie总能判断描述了表达式的类型吗?"))
   ((dialogue)
    (Ld "不能, 有时Pie需要帮助."
        (P "此时, 使用" (Code "the")
           "表达式来告诉Pie意图的类型.")
        ((comment)
         (Code "the") "表达式也被称为"
         (Em "类型注解") "."))
    (Rd "例如?"))
   ((dialogue)
    (Ld "Pie不能确定单独的" (Code "cons")
        "表达式的类型.")
    (Rd "为什么呢?"
        (CodeB "(cons 'spinach 'cauliflower)")
        "是一个"
        (CodeB "(Pair Atom Atom)")
        "这难道不是显然的吗?"
        ((tcomment)
         "cauliflower, 花椰菜.")))
   ((dialogue)
    (Ld "这对于我们来说是显然的, "
        "但是之后" (Code "cons")
        "将变得更加壮丽 (magnificent), "
        "提升了的表达力 (power) "
        "使得其类型不再能被自动确定.")
    (Rd "那么, 怎样让Pie确定"
        (CodeB "(cons 'spinach 'cauliflower)")
        "是一个序对呢?"))
   ((dialogue)
    (Ld "试试这个:"
        (CodeB "(the (Pair Atom Atom)
  (cons 'spinach 'cauliflower))"))
    (Rd "因此, 一个" (Code "the")
        "表达式将一个表达式与其类型联系起来, "
        "不仅是在Pie的反馈里, "
        "也可以出现在我们写下来的表达式里."))
   ((law)
    (Center (Code "the") "之律")
    (P "如果" $X "是一个类型而" $e "是一个" $X ", 那么"
       (CodeB "(the " $X " " $e ")")
       "是一个" $X "."))
   ((dialogue)
    (Ld "Pie中存在两种表达式: "
        "一种Pie可以自行判断其类型, "
        "另一种对于Pie而言则需要我们的帮助.")
    (Rd "还有其他帮助Pie判断类型的方式吗?"))
   ((dialogue)
    (Ld "的确是有的. 在第" (Ref "ch1")
        "章里, " (Code "claim")
        "在其相应的" (Code "define")
        "之前出现是必要的, 而这告诉了Pie"
        "对于定义的意义而言使用何种类型.")
    (Rd "为什么我们不在每次Pie不能确定"
        "一个表达式的类型时就使用"
        (Code "claim") "和"
        (Code "define") "呢?"))
   ((dialogue)
    (Ld "原则上是可以的, "
        "但总是把名字都写出来令人厌倦.")
    (Rd "还有什么其他方法能够帮助"
        "Pie找到类型吗?"))
   ((dialogue)
    (Ld "的确还有一种方法. "
        "如果某处使用了一个表达式而"
        "此处只有一个类型能够成立 (make sense), "
        "那么Pie就会使用这个类型.")
    (Rd "举个例子呢?"))
   ((dialogue)
    (Ld "当检查"
        (CodeB "(the (Pair Atom
       (Pair Atom Atom))
  (cons 'spinach
    (cons 'kale 'cauliflower)))")
        "(是否)由一个类型描述时, Pie使用"
        (CodeB "(Pair Atom Atom)")
        "作为"
        (CodeB "(cons 'kale 'cauliflower)")
        "的类型.")
    (Rd "这里, 内层的" (Code "cons")
        "不需要一个" (Code "the")
        "是因为其类型是来源于外层的"
        (Code "cons") "的类型的."
        (P "以" (Code "the")
           "为顶的表达式都是值吗?")
        ((tcomment)
         "kale, 羽衣甘蓝.")))
   ((dialogue)
    (Ld "并非如此."
        (CodeB "(the " $X " " $e ")")
        "的值是" $e "的值.")
    (Rd "那么"
        (CodeB "(car
  (the (Pair Atom Nat)
    (cons 'brussels-sprout 4)))")
        "的值是什么?"
        ((tcomment)
         "brussels sprout, 抱子甘蓝.")))
   ((law)
    (Center (Code "the") "之诫")
    (P "如果" $X "是一个类型而"
       $e "是一个" $X ", 那么"
       (CodeB "(the " $X " " $e ")")
       "和" $e "是相同的" $X "."))
   ((dialogue)
    (Ld "值是很小一点的" (Code "'brussels-sprout") "."
        (P "现在试试这个:"
           (CodeB "U")))
    (Rd "Pie说:"
        (CodeB "U")
        "为什么不是"
        (CodeB "(the U U)")
        "呢?"))
   ((dialogue)
    (Ld $U:script "是一个类型, 但是其不" (Em "具备")
        "类型. 这是因为没有表达式可以是其自身的类型, "
        "正如" (Ref "paradox") "所写的那样."
        (P "当一个表达式是一个类型但不具备类型时, "
           "Pie只回以其规范形式."))
    (Rd "还有其他别的什么类型不具备类型"
        $U:script "的吗?"))
   ((dialogue)
    (Ld "当然有了, 例如"
        (CodeB "(Pair " $U:script " " $U:script ")")
        (CodeB "(Pair Atom " $U:script ")")
        (CodeB "(→ " $U:script " " $U:script ")")
        "都是类型, 但是它们不以" $U:script
        "作为它们的类型.")
    (Rd "还有其他什么关于Pie的方面是"
        "我们应该知道的呢?"))
   ((dialogue)
    (Ld "暂时就这么多了. 之后还有机会.")
    (Rd "下一步是什么?"))
   ((dialogue)
    (Ld "尽情玩乐.")
    (Rd "好主意. (Sounds like a plan!)"))
   (H2 "消去所有的自然数!" #:id "ch3")
   ((dialogue)
    (Ld "以下是来源于" (Ref "pesudo-gauss")
        "的" (Code "gauss") "的虚框定义."
        (CodeD "(define gauss
  (λ (n)
    (which-Nat n
      0
      (λ (n-1)
        (+ (add1 n-1) (gauss n-1))))))")
        "现在是时候以不使用显式递归的方式"
        "来正确地定义" (Code "gauss") "了.")
    (Rd "难道说我们将要像下面这样来定义"
        (Code "gauss") "吗?"
        (CodeD
         "(define gauss
  (λ (n)
    "
         (Frame "...这里没有gauss?")
         "))")))
   ((dialogue)
    (Ld "为什么递归定义不是一个选项?")
    (Rd "因为" (B "递归定义不是一个选项") "."))
   ((dialogue)
    (Ld "确实."
        (P "但是, 某些递归定义总是能产生一个值."))
    (Rd "比如说" (Code "gauss") ", 不是吗?"))
   ((dialogue)
    (Ld "的确如此."
        (P (Code "(gauss 0)")
           "的规范形式是什么?"))
    (Rd "是" (Code "zero") "."))
   ((dialogue)
    (Ld (Code "(gauss 1)") "的值是什么?")
    (Rd "是" (Code "1") ", 因为"
        (Ol (Li "| " (Code "(gauss (add1 zero))")
                "相同于")
            (Li "| " (Code "(+ 1 (gauss zero))")
                "相同于")
            (Li "| " (Code "(add1 (gauss zero))")))
        ((comment)
         "当表达式垂直对齐而左边又有竖杠时, "
         "我们将假定除了最后一行之外的每一行后面都跟着"
         (Q "相同于 (is the same as)")
         ". 这种图表被称为" (Q "相同于 (same as)")
         "图表.")
        ((tcomment)
         "原书的竖杠是连贯的, 虽然也可以实现这样的效果, "
         "但是译者太懒了.")))
   ((dialogue)
    (Ld "这就是值吗?")
    (Rd "不是还有更多要做的吗?"
        (Ol #:attr* '((start "3"))
            (Li "| " (Code "(add1 (gauss zero))"))
            (Li "| " (Code "(add1 zero)")))))
   ((law)
    (Center "相同性")
    (P "如果一个" (Q "相同于") "图表可以用来表明"
       "两个表达式是相同的, 那么这个事实可以在任何地方使用"
       "而不需要进一步的澄清. " (Q "相同于")
       "图表只是为了辅助构建理解的."))
   ((dialogue)
    (Ld "实际上"
        (CodeB "(add1 (gauss zero))")
        (Em "已经") "是一个值了. 为什么?")
    (Rd "好吧, 那是因为构造子" (Code "add1")
        "就在顶上."))
   ((dialogue)
    (Ld "诚然如此."
        (P (Code "(gauss 1)")
           "的规范形式是什么?"))
    (Rd "是" (Code "(add1 zero)") "."))
   ((dialogue)
    (Ld "为什么" (Code "(gauss 2)")
        "具有规范形式?")
    (Rd "因为" (Code "(gauss 2)")
        "的规范形式只依赖于"
        (Code "(gauss 1)")
        "的规范形式 (其的确" (Em "有")
        "一个规范形式) 和" (Code "+")
        "的规范形式."
        (P (Code "+") "有规范形式吗?")))
   ((dialogue)
    (Ld "一旦定义了" (Code "+")
        ", 它的确具有规范形式. "
        "暂时, 假定" (Code "+")
        "具有规范形式.")
    (Rd "好吧."))
   ((dialogue)
    (Ld "为什么" (Code "(gauss 3)")
        "具有规范形式?")
    (Rd "因为" (Code "(gauss 3)")
        "的规范形式只依赖于"
        (Code "(gauss 2)")
        "的规范形式 (其的确有一个规范形式) 和"
        (Code "+") "的规范形式. 暂时, 我们假定"
        (Code "+") "具有规范形式."))
   ((dialogue)
    (Ld "为什么对于任意的"
        (Code "Nat") " " $k ", "
        (Code "(gauss (add1 " $k "))")
        "都有规范形式?")
    (Rd "因为" (Code "(gauss (add1 " $k "))")
        "的规范形式仅依赖于"
        (Code "(gauss " $k ")")
        "的规范形式, " $k "的值, 以及"
        (Code "+") "的规范形式."
        (P $k "的值要么是" (Code "zero")
           ", 要么以" (Code "add1")
           "为顶. 我们已经知道了"
           (Code "(gauss 0)")
           "具有规范形式, 并且我们刚刚验证了, "
           "对于任意的" (Code "Nat") " " $k ", "
           (Code "(gauss (add1 " $k "))")
           "都有规范形式.")
        ((tcomment)
         "恕译者愚钝, 此处无法理解.")))
   ((dialogue)
    (Ld "为" (Em "每个") "可能的参数都赋有一个值的函数被称为"
        (Em "完全函数(total function)") "."
        (P (Code "+") "和" (Code "gauss")
           "都是完全的."))
    (Rd "存在并非完全的函数吗?"))
   ((law)
    (Center "完全函数")
    (P "为" (Em "每个") "可能的参数都赋有一个值的函数被称为"
       (Em "完全函数(total function)") "."))
   ((dialogue)
    (Ld "至少这里没有. 在Pie里, "
        (Em "所有的函数都是完全的") "."
        (P "消去子是什么?")
        ((comment)
         "因为所有函数都是完全的, 所以子表达式的求值顺序并不重要. "
         "如果某些函数并非完全, 那么求值顺序将会变得重要, "
         "因为其将决定函数是否会应用到并非拥有值的参数上."))
    (Rd "消去子拆开由构造子构造的值."))
   ((dialogue)
    (Ld "将一个" (Code "Nat") "拆开是什么意思?")
    (Rd "难道" (Code "which-Nat")
        "不会拆开一个" (Code "Nat") "吗?"))
   ((dialogue)
    (Ld "这意味着" (Code "which-Nat")
        "是" (Code "Nat") "的一个消去子. "
        "但是, 以" (Code "add1") "为顶的"
        (Code "Nat") "底下还藏着一个更小的"
        (Code "Nat") ", 而" (Code "which-Nat")
        "并不会消去这更小的" (Code "Nat") ".")
    (Rd "存在消去更小" (Code "Nat") "的方法吗?"))
   ((dialogue)
    (Ld "一种消去更小" (Code "Nat")
        "的方法是使用" (Code "iter-Nat") ".")
    (Rd "什么是" (Code "iter-Nat") "?"))
   ((dialogue)
    (Ld "一个" (Code "iter-Nat")
        "表达式长得像这样:"
        (CodeB "(iter-Nat " $target "
  " $base "
  " $step ")")
        "和" (Code "which-Nat")
        "一样, 当" $target "是" (Code "zero")
        "时, 整个" (Code "iter-Nat")
        "表达式的值即" $base "的值.")
    (Rd (Code "iter-Nat") "和"
        (Code "which-Nat")
        "不一样的地方在哪里?"))
   ((dialogue)
    (Ld "和" (Code "which-Nat")
        "不一样的是, 当" $target
        "是" (Code "(add1 " $n ")")
        "时, 整个" (Code "iter-Nat")
        "表达式的值是"
        (CodeB "(" $step "
 (iter-Nat " $n "
   " $base "
   " $step "))")
        "的值.")
    (Rd "所以说" $target "的值中的每一个"
        (Code "add1") "都将被代之以一个"
        $step ", 而" (Code "zero")
        "会被代之以" $base "."))
   ((law)
    (Center (Code "iter-Nat") "之律")
    (P "如果" $target "是一个" (Code "Nat")
       ", " $base "是一个" $X ", 而"
       $step "是一个"
       (CodeB "(→ " $X " " $X ")")
       "那么"
       (CodeB "(iter-Nat " $target "
  " $base "
  " $step ")")
       "是一个" $X "."))
   ((law)
    (Center (Code "iter-Nat") "之第一诫")
    (P "如果"
       (CodeB "(iter-Nat zero
  " $base "
  " $step ")")
       "是一个" $X ", 那么它和" $base
       "是相同的" $X "."))
   ((law)
    (Center (Code "iter-Nat") "之第二诫")
    (P "如果"
       (CodeB "(iter-Nat (add1 " $n ")
  " $base "
  " $step ")")
       "是一个" $X ", 那么它和"
       (CodeB "(" $step "
 (iter-Nat " $n "
   " $base "
   " $step "))")
       "是相同的" $X "."))
   ((dialogue)
    (Ld "是的."
        (CodeB "(iter-Nat 5
  3
  (λ (smaller)
    (add1 smaller)))")
        "的规范形式是什么?")
    (Rd "是" (Code "8") ", 因为"
        (Code "add1") "相继应用于"
        (Code "3") "五次是" (Code "8") ":"
        (CodeB "(add1
  (add1
    (add1
      (add1
        (add1 3)))))")))
   ((dialogue)
    (Ld "整个" (Code "iter-Nat")
        "表达式的类型和" $base
        "的类型相同吗?")
    (Rd "必然如此, 因为当" $target
        "是" (Code "zero") "时, 整个"
        (Code "iter-Nat")
        "表达式的值是" $base "的值."))
   ((dialogue)
    (Ld "让我们使用" $X "作为" $base
        "的类型的一个名字."
        (P $step "的类型如何?")
        ((tcomment)
         "也就是说, " $X "是一个元变量."))
    (Rd $step "被应用于" $base
        ", 其也被应用于由" $step
        "产生的几乎是答案的东西 (almost-answer), 故"
        $step "必然是一个"
        (CodeB "(→ " $X " " $X ")")))
   ((dialogue)
    (Ld "就和" (Ref "which-Nat")
        "中的" (Code "which-Nat")
        "一样, 名字" $target ", "
        $base ", " $step
        "是引用" (Code "iter-Nat")
        "的参数的便利方式."
        (P "以下的" (Code "iter-Nat")
           "表达式里, target, base, step"
           "分别是什么?"
           (CodeB "(iter-Nat 5
  3
  (λ (k)
    (add1 k)))")))
    (Rd "target是"
        (CodeB "5")
        "base是"
        (CodeB "3")
        "step是"
        (CodeB "(λ (k)
  (add1 k))")))
   ((dialogue)
    (Ld "到目前为止, 我们引用" (Code "+")
        "就好像其已被完全理解了一样, "
        "并且我们还假定其具有规范形式, "
        "然而我们还没有定义" (Code "+") "."
        (P (Code "+") "的类型应该是什么?"))
    (Rd (Code "+") "接受两个" (Code "Nat")
        "而返回一个" (Code "Nat") "."
        (CodeB "(claim +
  (→ Nat Nat Nat))")))
   ((dialogue)
    (Ld "的确如此."
        (P "若是递归可以成为一个选项, "
           "那么以下将会是一个正确的定义."
           (CodeD "(define +
  (λ (n j)
    (which-Nat n
      j
      (λ (n-1)
        " (Frame "(add1 "
                 (WB "(+ n-1 j)")
                 ")")
          "))))")
           "那么, 该如何利用"
           (Code "iter-Nat") "来定义"
           (Code "+") "呢?"))
    (Rd "使用" (Code "iter-Nat")
        "定义" (Code "+")
        "需要一个base和一个step. base应该是"
        (Code "j") ", 鉴于以下"
        (Q "相同于") "图表:"
        (Ol (Li "| " (Code "(+ zero j)"))
            (Li "| " (Code "j")))
        "有什么好的办法找到step是什么吗?"))
   ((dialogue)
    (Ld "step基于" (Code "+")
        "的递归版本的白框里的内容, "
        "其描述了如何将一个几乎是答案的东西"
        (Code "+" (Sub "n-1"))
        "变为一个真正的答案."
        (P "将白纸黑字的部分 (其包含有递归) "
           "替换以献给step的几乎是答案的参数, "
           "记得保留白框里其他内容."))
    (Rd "以下就是我们想要的step了."
        (CodeB "(claim step-+
  (→ Nat Nat))
(define step-+
  (λ (+" (Sub "n-1") ")
    "
         (Frame "(add1 " (WB "+" (Sub "n-1")) ")")
         "))")))
   ((dialogue)
    (Ld "除非类型和定义里的所有名字都已定义, "
        "不然我们无法定义一个新的名字."
        ((comment)
         "如果定义可以相互引用, 那么我们就无法保证"
         "每个被定义的函数都是完全函数了."))
    (Rd "而" (Code "+") "需要引用" (Code "step-+")
        ", 当然其既已定义."
        (CodeB "(define +
  (λ (n j)
    (iter-Nat n
      j
      step-+)))")))
   ((dialogue)
    (Ld "是的, 现在" (Code "+") "就定义好了."
        (P (Code "(+ (add1 zero) 7)")
           "是什么?")
        ((tcomment)
         "右侧排版没有按照原文, 见谅."))
    (Rd "是" (Code "8") ", 因为"
        (Ol (Li "| " (Code "(+ (add1 zero) 7)"))
            (Li "| " (set-attr* (CodeB "(iter-Nat (add1 zero)
  7
  step-+)") 'style "display: inline-block"))
            (Li "| " (set-attr* (CodeB "(step-+
  (iter-Nat zero
    7
    step-+))") 'style "display: inline-block"))
            (Li "| " (set-attr* (CodeB "(add1
  (iter-Nat zero
    7
    step-+))") 'style "display: inline-block"))
            (Li "| " (Code "(add1 7)")))
        "即" (Code "8") "."))
   ((dialogue)
    (Ld (Code "iter-Nat") "可以用来定义"
        (Code "gauss") "吗?")
    (Rd (Code "iter-Nat")
        "展现了一种反复消去" (Code "add1")
        "下面藏着的更小的" (Code "Nat")
        "的方式."
        (P "消去更小的" (Code "Nat")
           "... 这听起来的确像是"
           (Code "gauss")
           "所遵照的方法.")))
   ((dialogue)
    (Ld "是挺接近的, 但是step没有足够的信息. "
        (Code "gauss") "需要一个结合了"
        (Code "which-Nat") "和"
        (Code "iter-Nat")
        "的表达力的消去子. 这个消去子叫做"
        (Code "rec-Nat") ".")
    (Rd (Code "rec-Nat") "是什么?"
        ((tcomment)
         "根据The Little Typer官网的勘误表, "
         "其实" (Code "rec-Nat") "是可以用"
         (Code "iter-Nat") "表达的.")))
   ((dialogue)
    (Ld (Code "rec-Nat") "的step会被应用到两个参数上: "
        "一个是" (Code "add1") "下面藏着的那个更小的"
        (Code "Nat") ", 另一个是这更小的" (Code "Nat")
        "上的递归答案. 这是" (Ref "pesudo-gauss")
        "里的" (Code "gauss") "定义所使用的方法."
        (P "此即所谓" (Code "rec-Nat") "模式.")
        ((comment)
         (Code "rec-Nat") "模式也被称为"
         (Em "原始递归(primitive recursion)")
         ". 感谢R&oacute;zsa P&eacute;ter (1905-1977), "
         "Wilhelm Ackermann (1896-1962), "
         "Gabriel Sudan (1899-1977), 以及"
         "David Hilbert (1862-1943)."))
    (Rd "如何使用" (Code "rec-Nat")
        "定义" (Code "gauss") "呢?"))
   ((dialogue)
    (Ld "这个框里有两种" (Code "gauss")
        "的定义: 一个是来源于" (Ref "pesudo-gauss")
        "虚框版本, 另一个则是使用"
        (Code "rec-Nat") "的版本."
        (CodeD
         "(define gauss
  (λ (n)
    (which-Nat n
      0
      (λ (n-1)
        " (Frame "(+ (add1 n-1) "
                 (WB "(gauss n-1)")
                 ")")
          "))))")
        (CodeD
         "(define gauss
  (λ (n)
    (rec-Nat n
      0
      (λ (n-1 gauss" (Sub "n-1") ")
        " (Frame "(+ (add1 n-1) "
                 (WB "gauss" (Sub "n-1"))
                 ")")
          "))))")
        "有什么区别呢?")
    (Rd "三个区别:"
        (Ol (Li (Code "which-Nat")
                "代之以"
                (Code "rec-Nat") ",")
            (Li "内层的" (Code "λ")
                "表达式多了一个变量"
                (Code "gauss" (Sub "n-1"))
                ", 以及")
            (Li "递归" (Code "(gauss n-1)")
                "代之以几乎是答案的"
                (Code "gauss" (Sub "n-1"))
                "."))
        ((tcomment)
         "原文中" (Code "rec-Nat")
         "版本的" (Code "gauss")
         "也被加上了虚框, 并不是因为这个定义是错的, "
         "而是因为之后出现的利用辅助过程"
         (Code "step-gauss") "的定义才是正式的.")))
   ((dialogue)
    (Ld "名字" (Code "n-1") "和"
        (Code "gauss" (Sub "n-1"))
        "暗示了其意义, 尽管它们只是变量名而已."
        (P (Code "rec-Nat")
           "的参数和" (Code "iter-Nat")
           "有着同样的特殊名字: 其总被称为"
           $target ", " $base ", " $step "."))
    (Rd "我们该如何确定一个" (Code "rec-Nat")
        "表达式的值呢?"))
   ((dialogue)
    (Ld "和" (Code "iter-Nat") "一样, 如果target是"
        (Code "zero") ", 那么整个"
        (Code "rec-Nat") "表达式的值即"
        $base "的值.")
    (Rd "如果target以" (Code "add1") "为顶呢?"))
   ((dialogue)
    (Ld (Code "which-Nat") "应用其step于藏在"
        (Code "add1") "下的这更小的"
        (Code "Nat") "."
        (P (Code "iter-Nat") "应用其step于一个新的"
           (Code "iter-Nat") "表达式, "
           "其和原本的表达式带有相同的base和step, "
           "但是以藏在" (Code "add1") "下的这更小的"
           (Code "Nat") "为新的target.")
        (P "这两者该如何结合起来呢?"))
    (Rd "我猜一下."
        (P "step将被应用到这更小的" (Code "Nat")
           "上. 而且, step也会被应用于一个新的"
           (Code "rec-Nat") "表达式, 其和原本的"
           "表达式带有相同的base和step, "
           "但是以恰好相同的这更小的"
           (Code "Nat") "为target.")))
   ((dialogue)
    (Ld "很好的想法. 当" (Code "rec-Nat")
        "和作为target的非" (Code "zero")
        "的" (Code "Nat") "一起使用时, "
        "target将藉由每次移除一个" (Code "add1")
        "收缩. 而且和" (Code "iter-Nat")
        "一样, base和step是不会改变的."
        (CodeB "(rec-Nat (add1 zero)
  0
  (λ (" (Dim "n-1") " almost)
    (add1
      (add1 almost))))")
        "的值是什么?")
    (Rd "其为step应用于" (Code "zero")
        "和新的" (Code "rec-Nat")
        "表达式, 即"
        (CodeB "((λ (" (Dim "n-1") " almost)
   (add1
     (add1 almost)))
  zero
  (rec-Nat zero
    0
    (λ (" (Dim "n-1") " almost)
      (add1
        (add1 almost)))))")))
   ((dialogue)
    (Ld "前一个框中作为结果的表达式并不是一个值, "
        "但是其和原本的表达式是相同的."
        (P "那么值是什么呢?"))
    (Rd (CodeB "(add1
  (add1
    (rec-Nat zero
      0
      (λ (" (Dim "n-1") " almost)
        (add1
          (add1 almost))))))")
        "其是一个值, 因为其以"
        (Code "add1") "为顶."))
   ((dialogue)
    (Ld "其规范形式如何?")
    (Rd (CodeB "(add1
  (add1 0))")
        "target是" (Code "zero")
        ", 而base是" (Code "0") "."))
   ((dialogue)
    (Ld "一个" (Code "rec-Nat")
        "表达式只在target为"
        (Code "Nat") "时是一个表达式.")
    (Rd "base和step的类型应该是什么呢?"))
   ((dialogue)
    (Ld "base必然具有某个类型, 让我们再次称其为"
        $X ". " $X "可以是任意的类型, 但是整个"
        (Code "rec-Nat") "表达式必然和base"
        "有着相同的类型, 即" $X ".")
    (Rd "这就是所有要说的了吗?"))
   ((dialogue)
    (Ld "还没有说完."
        (P "如果base是一个" $X
           ", 那么step必然是一个"
           (CodeB "(→ Nat " $X " " $X ")")
           "为什么这是step的合适类型呢?"))
    (Rd "step会被应用于两个参数: 第一个参数是一个"
        (Code "Nat") ", 因为其为target里藏在"
        (Code "add1") "下的那个东西; 第二个参数是"
        (Code "almost") ", 其为" $X
        "是因为其也由" (Code "rec-Nat") "产生."))
   ((dialogue)
    (Ld "那么, 这是如何与" (Code "which-Nat")
        "和" (Code "iter-Nat")
        "里的step的类型联系起来的呢?")
    (Rd "就像" (Code "which-Nat") ", "
        (Code "rec-Nat") "的step接受藏在target的"
        (Code "add1") "下的那个更小的" (Code "Nat")
        "作为参数. 就像" (Code "iter-Nat")
        ", 其也接受递归的几乎是答案的结果."))
   ((dialogue #:id "dim-almost")
    (Ld "以下是一个函数, 其判断一个" (Code "Nat")
        "是否是" (Code "zero") "."
        (CodeB "(claim step-zerop
  (→ Nat Atom Atom))
(define step-zerop
  (λ (" (Dim "n-1") " " (Dim "zerop" (Sub "n-1")) ")
    'nil))
(claim zerop
  (→ Nat Atom))
(define zerop
  (λ (n)
    (rec-Nat n
      't
      step-zerop)))")
        ((comment)
         "我们将" (Code "'t") "和" (Code "'nil")
         "当成两个任意的值使用. 这或许对于Lisper而言"
         "是熟悉的 (感谢John McCarthy (1927-2011)), 但是"
         (Code "zerop") "在Scheme中被称为" (Code "zero?")
         " (感谢Gerald J. Sussman (1947-) 和"
         "Guy L. Steele Jr. (1954-))."))
    (Rd "为什么要使用递归性的" (Code "rec-Nat")
        "来定义实际上只需要判断顶层构造子是"
        (Code "zero") "还是" (Code "add1")
        "的过程呢? 毕竟, " (Code "which-Nat")
        "就已经够用了."))
   ((dialogue #:id "step-naming")
    (Ld (Code "which-Nat")
        "很容易解释, 但是" (Code "rec-Nat")
        "可以做到任何" (Code "which-Nat")
        " (和" (Code "iter-Nat")
        ") 可以完成的事情."
        (P "为什么" (Code "step-zerop")
           "中的" (Code "λ") "变量被称为"
           (Code (Dim "n-1")) "和"
           (Code (Dim "zerop" (Sub "n-1")))
           "呢?"))
    (Rd "之所以选取" (Code (Dim "n-1"))
        "这个暗示比" (Code "n")
        "小一的名字, 还是因为其比target "
        (Code "Nat") "小一, target "
        (Code "Nat") "即正被消去的那个"
        (Code "Nat") "表达式. 名字"
        (Code (Dim "zerop" (Sub "n-1")))
        "暗示着" (Code "(zerop n-1)") "."))
   ((dialogue)
    (Ld "step仅仅是一个" (Code "λ")
        "表达式, 故其他任意未被使用的变量名"
        "亦可运用, 但是我们往往在step中使用"
        "这种命名变量的风格."
        (P (Code "step-zerop")
           "的两个参数皆未被使用, 故其为黯淡的. "
           "因此, 这个定义只是看上去像递归的, "
           "实则并非如此."))
    (Rd "不使用其参数的" (Code "λ")
        "表达式意义何在?"))
   ((dialogue #:id "step-zerop")
    (Ld (Code "rec-Nat")
        "的step总是接受两个参数, "
        "尽管其并不总是使用它们."
        (P (Code "(zerop 37)")
           "的值是什么?"))
    (Rd "让我们看看."
        (Ol (Li "| " (Code "(zerop (add1 36))"))
            (Li "| " (set-attr* (CodeB "(rec-Nat (add1 36)
  't
  step-zerop)") 'style "display: inline-block"))
            (Li "| " (set-attr* (CodeB "(step-zerop 36
  (rec-Nat 36
    't
    step-zerop))") 'style "display: inline-block"))
            (Li "| " (Code "'nil")))
        "值是立刻确定的. 对于" (Code "36")
        " (也就是" (Code "(add1 35)")
        ") 而言的" (Code "zerop") "值并不需要, "
        "故我们没有找到它的理由."))
   ((dialogue)
    (Ld "在表达式的值实际变得必要之前, "
        "我们都无需对于表达式求值. "
        "否则的话, 对于" (Code "step-zerop")
        "的参数"
        (CodeB "(rec-Nat 36
  't
  step-zerop)")
        "求值就太花工夫了, 以至于若记录在"
        (Q "相同于") "图表中则需另费至少105行.")
    (Rd "有时懒惰 (laziness) 是一种美德."))
   ((dialogue)
    (Ld (Code "(zerop 37)") "和"
        (Code "(zerop 23)") "相同吗?")
    (Rd "是的, 的确."
        (Ol (Li "| " (Code "'nil"))
            (Li "| " (set-attr* (CodeB "(step-zerop 22
  (rec-Nat 22
    't
    step-zerop))") 'style "display: inline-block"))
            (Li "| " (set-attr* (CodeB "(rec-Nat (add1 22)
  't
  step-zerop)") 'style "display: inline-block"))
            (Li "| " (Code "(zerop (add1 22))")))))
   ((dialogue)
    (Ld "以下是" (Code "gauss") "的step."
        (CodeB "(claim step-gauss
  (→ Nat Nat
    Nat))
(define step-gauss
  (λ (n-1 gauss" (Sub "n-1") ")
    (+ (add1 n-1) gauss" (Sub "n-1") ")))"))
    (Rd "这个定理使用了来源于"
        (Ref "step-naming") "的命名约定."))
   ((dialogue)
    (Ld "是的, 的确如此."
        (P "定义step的另一个好处在于我们需要显式地写下其类型, "
           "而不是任其由其于" (Code "rec-Nat")
           "中的使用方式被推断出来."))
    (Rd "显式类型使得阅读和理解定义更为容易."))
   ((dialogue)
    (Ld "诸如" (Code (Dim "zerop" (Sub "n-1")))
        "和" (Code "gauss" (Sub "n-1"))
        "这样的step中的" (Code "λ")
        "变量" (Em "几乎(almost)")
        "就是答案了, 其意见于"
        (Ref "almost-answer") ".")
    (Rd "好."
        (P (Code "gauss") "的正式定义是什么?")))
   ((dialogue)
    (Ld "以下就是了."
        (CodeB "(define gauss
  (λ (n)
    (rec-Nat n
      0
      step-gauss)))")
        "base是什么?")
    (Rd "base即" (Code "rec-Nat")
        "的第二个参数. 这个情况下, 是"
        (Code "0") ", 一个" (Code "Nat") "."))
   ((dialogue)
    (Ld "step是什么?")
    (Rd "即" (Code "step-gauss") "."))
   ((dialogue)
    (Ld "的确如此.")
    (Rd "根据这个定义, " (Code "(gauss zero)")
        "是什么?"))
   ((dialogue)
    (Ld "是" (Code "0") ", 因为"
        (CodeB "(rec-Nat zero
  0
  step-gauss)")
        "和" (Code "rec-Nat")
        "的第二个参数相同, 即" (Code "0") "."
        (P "以下是我们找出"
           (Code "(gauss (add1 zero))")
           "的值的开始."
           (Ol (Li "| " (Code "(gauss (add1 zero))"))
               (Li "| " (set-attr* (CodeB "(step-gauss zero
  (rec-Nat zero
    0
    step-gauss))") 'style "display: inline-block"))
               (Li "| " (set-attr* (CodeB "(+ (add1 zero)
  (rec-Nat zero
    0
    step-gauss))") 'style "display: inline-block")))
           "现在请继续下去以找到值."))
    (Rd "出发."
        (Ol #:attr* '((start "4"))
            (Li "| " (set-attr* (CodeB "(iter-Nat (add1 zero)
  (rec-Nat zero
    0
    step-gauss)
  step-+)") 'style "display: inline-block"))
            (Li "| " (set-attr* (CodeB "(step-+
  (iter-Nat zero
    (rec-Nat zero
      0
      step-gauss)
    step-+))") 'style "display: inline-block"))
            (Li "| " (set-attr* (CodeB "(add1
  (iter-Nat zero
    (rec-Nat zero
      0
      step-gauss)
    step-+))") 'style "display: inline-block")))
        "其已是值, 鉴于其以" (Code "add1") "为顶."))
   ((dialogue)
    (Ld "这个值是规范的吗?")
    (Rd "不是, 但是以下图表找出了其规范形式."
        (Ol #:attr* '((start "7"))
            (Li "| " (set-attr* (CodeB "(add1
  (rec-Nat zero
    0
    step-gauss))") 'style "display: inline-block"))
            (Li "| " (Code "(add1 0)")))
        "这的确是规范的了."))
   ((dialogue)
    (Ld "为什么" (Code "rec-Nat")
        "使用起来总是安全的?")
    (Rd "这是个好问题."
        (P "当target以" (Code "add1")
           "为顶时, " (Code "rec-Nat")
           "是递归性的. 既然递归不是一个选项, "
           "为什么这是可以接受的?")))
   ((dialogue)
    (Ld "如果step和" (Ref "dim-almost")
        "的情况一样不依赖于几乎是答案的参数, "
        "那么我们就已经抵达了一个值. "
        "如果step的确依赖于几乎是答案的参数, "
        "那么我们可以保证递归抵达base, "
        "其总是一个值或者是一个可以成为值的"
        "表达式.")
    (Rd "我们是怎么知道的呢?"))
   ((dialogue)
    (Ld "因为每个target " (Code "Nat")
        "要么和" (Code "zero")
        "相同, 要么和" (Code "(add1 " $n ")")
        "相同, 其中" $n "是一个更小的"
        (Code "Nat") ".")
    (Rd "我们怎么知道" $n "更小的?"))
   ((dialogue)
    (Ld "使得" $n "相同或者更大的方式是"
        "假设target " (Code "Nat")
        "由无限多的" (Code "add1")
        "构造而成. 然而, 因为每个函数都是完全的, "
        "我们没有办法做到这点. "
        "类似地, 没有步骤 (step) 可以不是完全的, "
        "因为这里" (Em "所有") "的函数都是完全的, "
        "而每一步骤不过就是应用一个函数."
        ((tcomment)
         "这段话中的step有点一语双关的意味."))
    (Rd "所以说, 为什么我们不能将这种推理风格"
        "应用于任意的递归函数呢?"))
   ((dialogue)
    (Ld "这种推理风格无法以我们的工具表达. "
        "但是, 一旦我们相信step完全的" (Code "rec-Nat")
        "是一种消去任意作为target的" (Code "Nat")
        "的方法, 那么每个新定义都是完全的了."
        ((comment)
         "大致上说, 我们没法做, 但是即便我们能做, "
         "那也会很累人的."))
    (Rd "还有什么使用" (Code "rec-Nat")
        "的更有趣的例子吗?"))
   ((dialogue)
    (Ld "其可以用来定义" (Code "*")
        ", 意即乘法."
        (P "换言之, 如果" (Code "n") "和" (Code "j")
           "是" (Code "Nat") ", 那么"
           (CodeB "(* n j)")
           "应该是" (Code "n") "和" (Code "j") "之积.")
        ((comment)
         (Code "*") "读作" (Q "乘 (times)") "."))
    (Rd (Code "*") "接受两个" (Code "Nat")
        "并返回一个" (Code "Nat")
        ", 故以下是" (Code "*") "的类型."
        (CodeB "(claim *
  (→ Nat Nat
    Nat))")))
   ((dialogue)
    (Ld "对于每一个步骤 (step), " (Code "+")
        "给当时的答案 (the answer so far) "
        "加上一. " (Code "*") "的每一步会做什么呢?")
    (Rd (Code "*") "会将" (Code "j")
        "加到第二个参数上去, "
        "也就是几乎是答案的那个参数."))
   ((dialogue)
    (Ld "以下是" (Code "make-step-*")
        ", 其对于每个给定的" (Code "j")
        "产生一个step函数."
        (CodeD "(claim make-step-*
  (→ Nat
    (→ Nat Nat
      Nat)))
(define make-step-*
  (λ (j)
    (λ (n-1 *" (Sub "n-1") ")
      (+ j *" (Sub "n-1") "))))"))
    (Rd "这看起来和之前的step不太一样."))
   ((dialogue)
    (Ld "不论" (Code "j")
        "为何, " (Code "make-step-*")
        "都构造了一个合适的" (Code "step")
        ". 这个step接受两个参数, 因为和"
        (Code "rec-Nat") "一起使用的step"
        "都要接受两个参数, 就和" (Ref "step-zerop")
        "中的" (Code "step-zerop") "一样."
        (P "现在定义" (Code "*") "."))
    (Rd "好."
        (P (Code "make-step-*")
           "的参数是" (Code "j")
           ", 其在每一步里被加到积上去. base为"
           (Code "0") "是因为乘" (Code "zero")
           "等于" (Code "0") "."
           (CodeD "(define *
  (λ (n j)
    (rec-Nat n
      0
      (make-step-* j))))"))))
   ((dialogue #:id "step-*")
    (Ld "看起来好像" (Code "make-step-*")
        "在干什么新鲜的事情. 它是一个"
        (Code "λ") "表达式, 并且产生一个"
        (Code "λ") "表达式. 其实无需两步过程, "
        "我们可以将嵌套的" (Code "λ")
        "合并为一个."
        (CodeB "(claim step-*
  (→ Nat Nat Nat
    Nat))
(define step-*
  (λ (j n-1 *" (Sub "n-1") ")
    (+ j *" (Sub "n-1") ")))")
        (Code "make-step-*")
        "对于任意给定的" (Code "j")
        "产生一个step. 而且, 尽管看起来不同, "
        (Code "make-step-*") "和"
        (Code "step-*") "实际上有着"
        (Em "相同的定义") ".")
    (Rd "不可能是相同的定义呀, "
        (Code "step-*")
        "明明是一个具有三个参数的"
        (Code "λ") "表达式."))
   ((dialogue)
    (Ld "实际上, 所有的" (Code "λ")
        "表达式都恰接受一个参数."
        (CodeB "(λ (x y z)
  (+ x (+ y z)))")
        "不过是"
        (CodeB "(λ (x)
  (λ (y)
    (λ (z)
      (+ x (+ y z)))))")
        "的缩写而已.")
    (Rd "是不是"
        (CodeB "(→ Nat Nat Nat
  Nat)")
        "也是某种东西的缩写呢?"))
   ((dialogue)
    (Ld "其是"
        (CodeB "(→ Nat
  (→ Nat
    (→ Nat
      Nat)))")
        "的缩写.")
    (Rd "如果一个函数接受三个参数, "
        "那么可以只应用函数于其中一个."
        (P "可以仅仅应用这个函数于两个参数吗?")))
   ((dialogue)
    (Ld "如果" $f "是一个"
        (CodeB "(→ Nat Nat Nat
  Nat)")
        "那么"
        (CodeB "(" $f " " $x " " $y " " $z ")")
        "不过是"
        (CodeB "((" $f " " $x " " $y ") " $z ")")
        "的缩写, 而这又是"
        (CodeB "(((" $f " " $x ") " $y ") " $z ")")
        "的缩写.")
    (Rd "是不是每个函数都恰接受一个参数呢?"))
   ((dialogue)
    (Ld "诚然如此, 每个函数恰接受一个参数."
        (P "定义多参数函数为嵌套的单参数函数的过程被称为"
           (Em "Currying") ".")
        ((comment)
         "感谢Haskell B. Curry (1900-1982) 和"
         "Moses Ilyich Schönfinkel (1889–1942)."))
    (Rd "以下是" (Code "*") "的正式定义."
        (CodeB "(define *
  (λ (n j)
    (rec-Nat n
      0
      (step-* j))))")
        "尽管" (Code "step-*")
        "看上去像一个三参数的" (Code "λ")
        "表达式, 它可以只接受一个参数. "
        (Code "rec-Nat") "期望其" (Code "step")
        "是一个恰接受 (get) 两个参数的函数."))
   ((dialogue)
    (Ld "以下是计算" (Code "(* 2 29)")
        "的规范形式的图表的前五行."
        (Ol (Li "| " (Code "(* 2 29)"))
            (Li "| " (CodeI "((λ (n j)
   (rec-Nat n
     0
     (step-* j)))
  2 29)"))
            (Li "| " (CodeI "(rec-Nat (add1
           (add1 zero))
  0
  (step-* 29))"))
            (Li "| " (CodeI "((step-* 29)
 (add1 zero)
 (rec-Nat (add1 zero)
   0
   (step-* 29)))"))
            (Li "| " (CodeI "((λ (n-1 *" (Sub "n-1") ")
   (+ 29 *" (Sub "n-1") "))
 (add1 zero)
 (rec-Nat (add1 zero)
   0
   (step-* 29)))")))
        "现在, 继续找出规范形式.")
    (Rd "啊, Currying也有参与."
        (Ol #:attr* '((start "6"))
            (Li "| " (CodeI "(+ 29
   (rec-Nat (add1 zero)
     0
     (step-* 29)))"))
            (Li "| " (CodeI "(+ 29
   ((step-* 29)
    zero
    (rec-Nat zero
      0
      (step-* 29))))"))
            (Li "| " (CodeI "(+ 29
   (+ 29
      (rec-Nat zero
        0
        (step-* 29))))"))
            (Li "| " (CodeI "(+ 29
   (+ 29 0))"))
            (Li "| " (Code "58")))
        "这个图表有遗漏什么步骤吗?"))
   ((law)
    (Center (Code "rec-Nat") "之律")
    (P "如果" $target "是一个" (Code "Nat")
       ", " $base "是一个" $X ", " $step "是一个"
       (CodeB "(→ Nat " $X " " $X ")")
       "那么"
       (CodeB "(rec-Nat " $target "
  " $base "
  " $step ")")
       "是一个" $X "."))
   ((law)
    (Center (Code "rec-Nat") "之第一诫")
    (P "如果"
       (CodeB "(rec-Nat zero
  " $base "
  " $step ")")
       "是一个" $X ", 那么其和" $base
       "是相同的" $X "."))
   ((law)
    (Center (Code "rec-Nat") "之第二诫")
    (P "如果"
       (CodeB "(rec-Nat (add1 " $n ")
  " $base "
  " $step ")")
       "是一个" $X ", 那么其和"
       (CodeB "(" $step " " $n "
  (rec-Nat " $n "
    " $base "
    " $step "))")
       "是相同的" $X "."))
   ((dialogue)
    (Ld "当然有了, 比如说使得"
        (Code "(+ 29 0)")
        "以及作为结果的"
        (Code "(+ 29 29)")
        "规范的过程."
        ((comment)
         "这个图表节约纸张, 能量, 时间."))
    (Rd "谢谢."
        (P "从一开始, 这种图表似乎就会变得很乏味.")))
   ((dialogue #:id "wrong-fact")
    (Ld "就是这样."
        (CodeD "(claim step-" (Frame "    ") "
  (→ Nat Nat
    Nat))
(define step-" (Frame "    ") "
  (λ (n-1 almost)
    (* (add1 n-1) almost)))
(claim " (Frame "    ") "
  (→ Nat
    Nat))
(define " (Frame "    ") "
  (λ (n)
    (rec-Nat n
      0
      step-" (Frame "    ") ")))")
        "这个定义取什么名字比较合适呢?")
    (Rd "这个函数总是返回" (Code "0") "."))
   ((dialogue)
    (Ld "非常善于观察啊."
        (P "像" (Code "Nat")
           "这样的类型的一个缺陷在于其不能说明"
           (Em "哪个(which)") " " (Code "Nat")
           "是我们所意图的. 之后, 我们将遇到更为强大的类型, "
           "其允许我们讨论" (Em "特定") "的"
           (Code "Nat") ".")
        ((comment)
         "实际上, " (Ref "wrong-fact")
         "中的定义本意在是" (Code "factorial")
         ". 然而, 疏忽导致了错误一直存在于诸多草稿版本"
         "之中却未被发现 (至于有多少这样的版本, 作者不想说了). "
         "我们将纠错的任务留给读者."))
    (Rd "那么, 更为强大的类型可以阻止我们像"
        (Ref "wrong-five") "里那样将" (Code "five")
        "定义成是" (Code "9") "吗?"
        ((tcomment)
         "原文为第2章第36框, 应该是笔误.")))
   ((dialogue)
    (Ld "当然不行了."
        (P "类型并不能阻止将" (Code "five")
           "定义成是" (Code "9")
           "的愚蠢行径. 但是, 我们可以将我们的"
           (Em "一些") "想法写成类型."))
    (Rd "非常有趣."))
   (H2 "小菜一碟, 简单如Pie")
   ((dialogue)
    (Ld "在" (Ref "Pear-definition")
        "里, 我们定义" (Code "Pear") "为"
        (CodeD "(claim Pear " $U:script ")
(define Pear
  (Pair Nat Nat))")
        (Code "Pear") "的消去子是由" (Code "car")
        "和" (Code "cdr") "定义的.")
    (Rd "并且..."))
   ((dialogue)
    (Ld (Code "Pear") "的一个消去子必须要做什么呢?")
    (Rd "这个消去子必须要暴露 (或者说解包) 一个"
        (Code "Pear") "中的信息."))
   ((dialogue)
    (Ld (Code "Pair") "的消去子呢? 它必须要做什么?")
    (Rd (Code "Pair") "的一个消去子必然要暴露一个"
        (Code "Pair") "中的信息."))
   ((dialogue)
    (Ld "那很接近了." (Br)
        "正如" (Ref "alone") "所言, " (Code "Pair")
        "单独不是一个表达式, 然而"
        (CodeB "(Pair Nat Nat)")
        "是一个表达式, 并且它有消去子."
        (CodeB "(Pair Nat Atom)")
        "也有消去子.")
    (Rd "再次尝试:"
        (CodeB "(Pair Nat Nat)")
        "的一个消去子必然要暴露一个特定"
        (CodeB "(Pair Nat Nat)")
        "中的信息, 一个"
        (CodeB "(Pair Nat Atom)")
        "的消去子必然要暴露一个特定"
        (CodeB "(Pair Nat Atom)")
        "中的信息."))
   ((dialogue)
    (Ld "但是, 这意味着" (Code "Pair")
        "需要许多消去子, 因为就像"
        (Ref "nested-Pair")
        "一样, 更深的嵌套总是可能的.")
    (Rd "听起来要记得好多名字的样子."))
   ((dialogue)
    (Ld "本会如此!"
        (P "实际上, 还有更好的方式. "
           "我们可以提供一个"
           (Code "(Pair " $A " " $D ")")
           "的消去子, "
           (Em "不论" $A "和" $D "为何.")))
    (Rd "不论为何? 即便" $A "是"
        (Code "'apple-pie") "吗?"))
   ((dialogue)
    (Ld "好吧, 当然不是绝对任意的."
        (P "根据" (Ref "Pair-type")
           ", 除非" $A "和" $D
           "是" (Em "类型")
           ". 也就是说, " $A "必须是一个类型且"
           $D "必须是一个类型."))
    (Rd "哇! 那这消去子看起来长什么样呢?"))
   ((dialogue)
    (Ld "给个例子."
        (CodeB "(claim kar
  (→ (Pair Nat Nat)
    Nat))")
        (CodeD "(define kar
  (λ (p)
    (elim-Pair
      Nat Nat
      Nat
      p
      (λ (a " (Dim "d") ")
        a))))")
        "因为" (Code "elim-Pair")
        "还未被定义, 所以"
        (Code "kar") "的定义才会用虚线框起来, "
        "不是别的什么原因.")
    (Rd "为什么" (Code "elim-Pair")
        "有这么多参数啊?"))
   ((dialogue)
    (Ld "在这个定义里, " (Code "elim-Pair")
        "的前三个参数都是类型" (Code "Nat")
        ". 前两个参数刻画了要被消去的"
        (Code "Pair") "的" (Code "car")
        "部分和" (Code "cdr") "部分的类型. "
        "这第三个参数" (Code "Nat")
        "描述了内层的" (Code "λ")
        "表达式的结果类型."
        ((comment)
         "因此, 内层的" (Code "λ")
         "表达式的参数" (Code "a")
         "和" (Code (Dim "d"))
         "的类型也都是" (Code "Nat") "."))
    (Rd "内层的" (Code "λ") "表达式的意图是什么?"))
   ((dialogue)
    (Ld "内层的" (Code "λ") "表达式描述了如何使用"
        (Code "p") "的值中的信息. 所谓的信息即"
        (Code "p") "的" (Code "car") "和"
        (Code "cdr") "部分.")
    (Rd "为什么" (Code (Dim "d")) "是黯淡的?"))
   ((dialogue)
    (Ld "参数名" (Code (Dim "d"))
        "之所以是黯淡的, 是因为其出现在内层的"
        (Code "λ") "表达式里, 却未被使用, 就和"
        (Ref "dim") "一样."
        (P "现在请定义一个类似的函数" (Code "kdr")
           ", 其找出一个" (Code "Nat")
           "序对的" (Code "cdr") "."))
    (Rd "几乎和" (Code "kar") "一模一样."
        (CodeB "(claim kdr
  (→ (Pair Nat Nat)
    Nat))")
        (CodeD "(define kdr
  (λ (p)
    (elim-Pair
      Nat Nat
      Nat
      p
      (λ (" (Dim "a") " d)
        d))))")
        "这次之所以" (Code (Dim "a"))
        "是黯淡的, 是因为其在内层的"
        (Code "λ") "表达式中未被使用, 而"
        (Code "d") "是正常的. 鉴于"
        (Code "elim-Pair") "还没有定义, "
        (Code "kdr") "也被虚框包裹, 就和"
        (Code "kar") "一样."))
   ((dialogue)
    (Ld "的确如此."
        (P "请编写一个叫做" (Code "swap")
           "的函数, 其交换一个"
           (Code "(Pair Nat Atom)")
           "的" (Code "car") "和"
           (Code "cdr") "."))
    (Rd "以下是" (Code "swap") "的类型."
        (CodeB "(claim swap
  (→ (Pair Nat Atom)
    (Pair Atom Nat)))")))
   ((dialogue)
    (Ld "现在定义" (Code "swap") ".")
    (Rd "以下是" (Code "swap")
        "的定义. 就和" (Code "kar")
        "和" (Code "kdr")
        "一样, 其也被包裹在虚框里."
        (CodeD "(define swap
  (λ (p)
    (elim-Pair
      Nat Atom
      (Pair Atom Nat)
      p
      (λ (a d)
        (cons d a)))))")))
   ((dialogue #:id "invalid-type")
    (Ld "一般说来, " (Code "elim-Pair") "的用法如下:"
        (CodeB "(elim-Pair
  " $A " " $D "
  " $X "
  " $p "
  " $f ")")
        "其中" $p "是一个" (Code "(Pair " $A " " $D ")")
        ", 而" $f "根据" $p "的" (Code "car")
        "和" (Code "cdr") "来确定整个表达式的值. "
        "这个值必然具有类型" $X "."
        (P (Code "elim-Pair") "的类型是什么?"))
    (Rd "以下是一个猜测, 可能是"
        (CodeB "(→ A D
   X
   (Pair A D)
   (→ A D
     X)
  X)")
        "因为" (Code "A") ", " (Code "D")
        ", " (Code "X") "是前三个参数, 第四个参数是一个"
        (Code "(Pair A D)") ", 而第五个应该基于一个"
        (Code "A") "和一个" (Code "D")
        "作出一个" (Code "X") "来."))
   ((dialogue)
    (Ld "但是, 这个表达式里的" (Code "A") ", " (Code "D")
        ", " (Code "X") "是什么呢?")
    (Rd (Code "A") ", " (Code "D") ", " (Code "X")
        "是" (Code "elim-Pair") "的前三个参数?"))
   ((dialogue)
    (Ld "它们会引用之前已经定义过的类型吗?")
    (Rd "不会, 因为它们指的是" (Em "不论什么样的参数") "."))
   ((dialogue)
    (Ld "一个表达式中的名字要么引用一个定义, 要么指的是由一个"
        (Code "λ") "所命名的一个参数. 显然这个表达式里没有"
        (Code "λ") ", 并且" (Code "A") ", " (Code "D")
        ", " (Code "X") "也都没有被定义.")
    (Rd "这必然意味着" (Ref "invalid-type")
        "中的那个表达式实际上并非一个类型."))
   ((dialogue)
    (Ld "的确如此."
        (P "然而, 这种思维过程的确言之成理. "
           "回忆一下, 成为一个"
           (CodeB "(→ " $Y " " $X ")")
           "是什么意思."))
    (Rd "一个"
        (CodeB "(→ " $Y " " $X ")")
        "是一个" (Code "λ") "表达式, 当接受一个"
        $Y "时, 产生一个" $X ". 它也可以是一个"
        "值为这样的" (Code "λ") "表达式的表达式. "
        "我说的对不对呢?"))
   ((dialogue)
    (Ld $Y "和" $X "都是类型吗?")
    (Rd "必然如此, 否则"
        (CodeB "(→ " $Y " " $X ")")
        "就不是一个类型了."))
   ((dialogue)
    (Ld "在之前我们提出的" (Code "elim-Pair")
        "的类型里, " (Code "A") ", " (Code "D")
        ", " (Code "X") "是类型构造子吗?")
    (Rd "并非如此, 它们和" (Code "Nat")
        "或者" (Code "Atom") "不是一种表达式, 因为每次"
        (Code "elim-Pair") "应用时它们都可能改变, 而"
        (Code "Nat") "永远是" (Code "Nat") "."))
   ((dialogue)
    (Ld "在之前我们提出的" (Code "elim-Pair")
        "的类型里, " (Code "A") ", " (Code "D")
        ", " (Code "X") "是被定义来指代类型的名字吗?")
    (Rd "当然不是, 出于相同的原因, 即每次"
        (Code "elim-Pair") "应用时它们都可能改变. "
        "但是, 一旦一个名字被" (Code "define")
        "了, 它就永远指的是相同的东西了."))
   ((dialogue)
    (Ld "这个消去子必然要能够讨论" (Em "任意")
        "的类型" (Code "A") ", " (Code "D")
        ", " (Code "X") ".")
    (Rd "听上去" (Code "→") "没法完成任务."))
   ((dialogue)
    (Ld "的确不行, 但是" (Code "Π") "行."
        ((comment)
         (Code "Π") "读作" (Q "pie")
         ", 并且也可以写成" (Code "Pi") "."))
    (Rd (Code "Π") "是什么意思呢?"))
   ((dialogue #:id "flip-def")
    (Ld "以下是一个例子."
        (CodeB "(claim flip
  (Π ((A " $U:script ")
      (D " $U:script "))
    (→ (Pair A D)
      (Pair D A))))
(define flip
  (λ (" (Dim "A") " " (Dim "D") ")
    (λ (p)
      (cons (cdr p) (car p)))))"))
    (Rd "是不是这意味着一个" (Code "λ")
        "表达式的类型可以是一个"
        (Code "Π") "表达式?"))
   ((dialogue)
    (Ld "好问题."
        (P "的确可以."))
    (Rd "如果说" (Code "Π") "和"
        (Code "→") "都可以用来描述" (Code "λ")
        "表达式, 那么它们有什么区别呢?"))
   ((dialogue)
    (Ld (CodeB "(flip Nat Atom)")
        "的值是什么?")
    (Rd "那必然是" (Code "λ") "表达式"
        (CodeB "(λ (p)
  (cons (cdr p) (car p)))")
        "这是因为" (Code "flip")
        "被定义为是一个" (Code "λ")
        "表达式, 而其被应用于了两个参数, "
        (Code "Nat") "和" (Code "Atom") "."))
   ((dialogue #:id "flip-use")
    (Ld (CodeB "((flip Nat Atom)
 (cons 17 'apple))")
        "的值是什么?")
    (Rd "其是"
        (CodeB "(cons 'apple 17)")
        "一个"
        (CodeB "(Pair Atom Nat)")))
   ((dialogue)
    (Ld (Code "Π") "和" (Code "→")
        "的不同在于一个函数应用于其参数"
        "这样的表达式的类型."
        (CodeB "(flip Nat Atom)")
        "的类型为"
        (CodeB "(→ (Pair Nat Atom)
  (Pair Atom Nat))")
        "这是因为当一个由" (Code "Π")
        "表达式所描述的表达式被应用时, "
        "参数表达式将代替" (Code "Π")
        "表达式的" (Em "体") "里的"
        (Em "参数名") ".")
    (Rd (Code "Π") "表达式的体和"
        (Code "λ") "表达式的体有着怎样的联系呢?"))
   ((dialogue)
    (Ld (Code "Π") "表达式和" (Code "λ")
        "表达式都会引入参数名, 而体是这些名字"
        "可以使用的地方.")
    (Rd "什么是" (Em "参数名") "?"))
   ((dialogue)
    (Ld (CodeB "(Π ((A " $U:script ")
    (D " $U:script "))
  (→ (Pair A D)
    (Pair D A)))")
        "在这个" (Code "Π") "表达式里, 参数名是"
        (Code "A") "和" (Code "D") ". " (Code "Π")
        "表达式可以有一个或更多的参数名, "
        "而这些参数名可以出现在" (Code "Π")
        "表达式的体中.")
    (Rd (Code "Π") "表达式的" (Em "体")
        "是什么?"))
   ((dialogue)
    (Ld (CodeB "(Π ((A " $U:script ")
    (D " $U:script "))
  (→ (Pair A D)
    (Pair D A)))")
        "在这个" (Code "Π") "表达式里, 体是"
        (CodeB "(→ (Pair A D)
  (Pair D A))")
        "这个是(" (Code "flip")
        "所代表的)" (Code "λ")
        "表达式的体的类型, 这个体由"
        (Code "Π") "表达式的体所描述."
        ((tcomment)
         "这句话读起来稍显冗余."))
    (Rd (Code "Π") "表达式的体里的"
        (Code "A") "和" (Code "D")
        "指的是什么?"))
   ((law)
    (Center "应用之中律")
    (P "如果" $f "是一个"
       (CodeB "(Π ((" $Y " " $U:script ")) " $X ")")
       "而" $Z "是一个" $U:script ", 那么"
       (CodeB "(" $f " " $Z ")")
       "是一个" $X ", 其中每个" $Y
       "都已被一致地替换为了" $Z ".")
    ((tcomment)
     "原文的" $Y "实际上使用的是无衬线字体, 即"
     (Code "Y") ". 但是, 译者认为" $Y
     "应该是一个代表句法上的变量的元变量."))
   ((dialogue)
    (Ld "体里的" (Code "A") "和" (Code "D")
        "指的是尚不可知的特定类型. "
        "不论哪两个类型" $A "和" $D
        "作为由" (Code "Π") "表达式所描述的"
        (Code "λ") "表达式的参数, 应用这"
        (Code "λ") "表达式的结果总是一个"
        (CodeB "(→ (Pair " $A " " $D ")
  (Pair " $D " " $A "))")
        ((tcomment)
         "请读者注意字体, " (Code "A")
         "和" $A "相当不同."))
    (Rd "是不是这意味着"
        (CodeB "(flip Atom (Pair Nat Nat))")
        "的类型为"
        (CodeB "(→ (Pair Atom
     (Pair Nat Nat))
  (Pair (Pair Nat Nat)
    Atom))")
        "呢?"))
   ((dialogue)
    (Ld "对的."
        (P "但为什么会是如此呢?"))
    (Rd "变量" (Code "A") "和" (Code "D")
        "被替换以其相应的参数: "
        (Code "Atom") "和"
        (Code "(Pair Nat Nat)") "."))
   ((dialogue)
    (Ld (CodeB "(Π ((A " $U:script ")
    (D " $U:script "))
  (→ (Pair A D)
    (Pair D A)))")
        "和"
        (CodeB "(Π ((Lemon " $U:script ")
    (Meringue " $U:script "))
  (→ (Pair Lemon Meringue)
    (Pair Meringue Lemon)))")
        "是相同的类型吗?")
    (Rd "的确如此, 因为正如" (Ref "rename")
        "所言, 一致地对于变量换名不会改变任何东西的意义."
        ((tcomment)
         "meringue, 蛋白酥.")))
   ((dialogue)
    (Ld (CodeB "(Π ((A " $U:script ")
    (D " $U:script "))
  (→ (Pair A D)
    (Pair D A)))")
        "和"
        (CodeB "(Π ((A " $U:script ")
    (D " $U:script "))
  (→ (Pair
       (car
         (cons A D))
       (cdr
         (cons A D)))
    (Pair D A)))")
        "是相同的类型吗?")
    (Rd "是的, 因为"
        (CodeB "(car
  (cons A D))")
        "和" (Code "A") "是相同的类型, 而"
        (CodeB "(cdr
  (cons A D))")
        "和" (Code "D") "是相同的类型."))
   ((dialogue #:id "flip-def2")
    (Ld "我们可以这样定义" (Code "flip") "吗?"
        (CodeD "(claim flip
  (Π ((A " $U:script ")
      (D " $U:script "))
    (→ (Pair A D)
      (Pair D A))))
(define flip
  (λ (" (Dim "C") " " (Dim "A") ")
    (λ (p)
      (cons (cdr p) (car p)))))"))
    (Rd "以下是我的猜测."
        (P "在这个定义里, 外层的" (Code "λ")
           "表达式中的(参数)名字和" (Code "Π")
           "表达式中的名字不同. 似乎这个定义"
           "不应该能够成立. " (Code (Dim "A"))
           "出现在错误的位置, 而" (Code (Dim "C"))
           "既不是" (Code "A") "也不是"
           (Code "D") ".")
        ((tcomment)
         "原文的后两个" (Code "A") "和"
         (Code "D") "本是黯淡的, "
         "但是译者认为既然它们指的是"
         (Code "Π") "表达式中的相应变量, "
         "所以说它们应该是正常颜色更好.")))
   ((dialogue)
    (Ld (Ref "flip-def2") "中提出的"
        (Code "flip") "定义是可以允许的. "
        "然而, 就像定义" (Code "five")
        "为意指" (Code "9") "一样, "
        "这是愚蠢的.")
    (Rd "为什么可以允许这样的定义呢?"))
   ((dialogue)
    (Ld "外层的" (Code "λ") "中的名字不需要匹配"
        (Code "Π") "表达式中的名字. 外层的"
        (Code "λ") "表达式中的" (Code (Dim "C"))
        "与" (Code "Π") "表达式中的" (Code "A")
        "相对应, 因为它们都是第一个名字. 外层的"
        (Code "λ") "表达式中的" (Code (Dim "A"))
        "与" (Code "Π") "表达式中的" (Code "D")
        "相对应, 因为它们都是第二个名字. "
        "重要的是参数命名的" (Em "顺序") "."
        ((comment)
         "尽管使用不相匹配的名字并非错误, "
         "但这的确相当令人困惑. "
         "我们总是使用匹配的名字.")
        "内层的" (Code "λ") "表达式中的"
        (Code "p") "与什么相对应?")
    (Rd "与" (Code "p") "相对应的是"
        (Code "→") "后跟着的"
        (Code "(Pair A D)")
        ", 其给出了内层" (Code "λ")
        "表达式的参数类型."))
   ((dialogue)
    (Ld "如何对于" (Ref "flip-def2")
        "中的定义里的" (Code (Dim "C"))
        "和" (Code (Dim "A"))
        "进行一致换名以改善这个定义?")
    (Rd "首先, " (Code (Dim "A"))
        "应该被重命名为" (Code (Dim "D"))
        ". 接着, " (Code (Dim "C"))
        "应该被重命名为" (Code (Dim "A"))
        ". 这不就是" (Ref "flip-def")
        "中的定义吗?"))
   ((dialogue)
    (Ld "现在可以定义" (Code "Pair")
        "的那个消去子了吗?")
    (Rd "是的, 其类型应该是"
        (CodeB "(Π ((A " $U:script ")
    (D " $U:script ")
    (X " $U:script "))
  (→ (Pair A D)
     (→ A D
       X)
    X))")
        "这看起来很像" (Ref "invalid-type")
        "里的那个类型."))
   ((dialogue)
    (Ld "的确如此."
        (P (Code "elim-Pair")
           "的定义是什么?"))
    (Rd (CodeB "(claim elim-Pair
  (Π ((A " $U:script ")
      (D " $U:script ")
      (X " $U:script "))
    (→ (Pair A D)
       (→ A D
         X)
      X)))
(define elim-Pair
  (λ (" (Dim "A") " " (Dim "D") " " (Dim "X") ")
    (λ (p f)
      (f (car p) (cdr p)))))")))
   ((dialogue)
    (Ld "现在" (Code "kar") "不需要虚框了."
        (CodeB "(define kar
  (λ (p)
    (elim-Pair
      Nat Nat
      Nat
      p
      (λ (a " (Dim "d") ")
        a))))"))
    (Rd (Code "kdr") "也是."
        (CodeB "(define kdr
  (λ (p)
    (elim-Pair
      Nat Nat
      Nat
      p
      (λ (" (Dim "a") " d)
        d))))")))
   ((dialogue)
    (Ld (Code "swap") "也是.")
    (Rd "是的."
        (CodeB "(define swap
  (λ (p)
    (elim-Pair
      Nat Atom
      (Pair Atom Nat)
      p
      (λ (a d)
        (cons d a)))))")))
   ((dialogue)
    (Ld "尽管一个" (Code "Π") "表达式可以拥有任意数目的参数名, "
        "还是首先描述单参数" (Code "Π") "表达式为类型的情形最为简单."
        (P "成为一个"
           (CodeB "(Π ((" $Y " " $U:script ")) " $X ")")
           "即是成为一个" (Code "λ") "表达式, 当且被应用于一个类型"
           $T "时, 将会产生一个表达式, 其类型是将" $X
           "中的每个" $Y "一致地替换以" $T "的结果."))
    (Rd "是不是忘了什么?"))
   ((dialogue)
    (Ld "也可以是一个表达式, " (Em "其值是")
        "这样的" (Code "λ") "表达式.")
    (Rd "不要忘记求值是重要的."
        (P "这是对于" (Code "Π")
           "表达式的完整描述了吗?")))
   ((dialogue)
    (Ld "不, 还不完全."
        (P "基于单参数的" (Code "Π") "表达式"
           (CodeB "(Π ((" $Y " " $U:script ") ("
                  $Z " " $U:script ")) " $X ")")
           "该怎样理解呢?"))
    (Rd "它应该意味着一个" (Code "λ")
        "表达式 (或者能求值至这样的" (Code "λ")
        "表达式), 当其被应用于两个类型" $T "和" $S
        "时, 将会产生一个表达式, 其类型可由一致地将"
        $X "中的每个" $Y "替换以" $T "然后再将这新的"
        $X "中的每个" $Z "替换以" $S "获得."
        ((tcomment)
         "这里假定" $Y "和" $Z "相异, "
         "至于不相异的情况该如何解释, "
         "实际上多个参数的" (Code "Π")
         "表达式可以理解为单参数" (Code "Π")
         "表达式的嵌套.")))
   ((dialogue)
    (Ld (Code "Π") "表达式可以拥有任意数目的参数, "
        "而其所描述的" (Code "λ")
        "表达式有着相同数目的参数."
        (P "什么表达式具有类型"
           (CodeB "(Π ((A " $U:script "))
  (→ A (Pair A A)))")
           "呢?"))
    (Rd "比如说以下这个?"
        (CodeB "(λ (" (Dim "A") ")
  (λ (a)
    (cons a a)))")))
   ((dialogue)
    (Ld "以下我们为熟悉的表达式取了个名字."
        (CodeD "(claim twin-Nat
  (→ Nat
    (Pair Nat Nat)))
(define twin-Nat
  (λ (x)
    (cons x x)))")
        (CodeB "(twin-Nat 5)")
        "的值是什么?"
        ((comment)
         "自" (Ref "twin")
         "起就熟悉了."))
    (Rd "是"
        (CodeB "(cons 5 5)")))
   ((dialogue)
    (Ld "以下是一个非常类似的定义."
        (CodeD "(claim twin-Atom
  (→ Atom
    (Pair Atom Atom)))
(define twin-Atom
  (λ (x)
    (cons x x)))")
        (CodeB "(twin-Atom 'cherry-pie)")
        "的值是什么?")
    (Rd "是"
        (CodeB "(cons 'cherry-pie 'cherry-pie)")
        "这些定义有什么问题? "
        "为什么它们被虚线框起来了?"
        ((tcomment)
         "之所以用虚线框起来, 是因为这些定义不够一般, "
         "于是不值得定义, 而应该代之以一个更一般的版本.")))
   ((dialogue)
    (Ld "之于"
        (CodeB "(λ (a)
  (cons a a))")
        (Code "Nat") "和" (Code "Atom")
        "没什么特别之处. 因此, 与其对于每个类型写下一个新的定义, "
        "我们不如使用" (Code "Π") "构建一个一般目的性的"
        (Code "twin") ", 其可以对于" (Em "任意")
        "的类型成立.")
    (Rd "以下即是一般目的性的" (Code "twin") "."
        (CodeB "(claim twin
  (Π ((Y U))
    (→ Y
      (Pair Y Y))))
(define twin
  (λ (Y)
    (λ (x)
      (cons x x))))")))
   ((dialogue)
    (Ld (Code "(twin Atom)") "的值是什么?")
    (Rd (Code "(twin Atom)") "是"
        (CodeB "(λ (x)
  (cons x x))")))
   ((dialogue)
    (Ld (Code "(twin Atom)") "的类型是什么?")
    (Rd "一致地将"
        (CodeB "(→ Y
  (Pair Y Y))")
        "中的每个" (Code "Y")
        "替换以" (Code "Atom") "就得到了"
        (CodeB "(→ Atom
  (Pair Atom Atom))")))
   ((dialogue)
    (Ld (Code "twin-Atom") "的类型和"
        (Code "(twin Atom)")
        "的类型之间有什么联系?")
    (Rd (Code "twin-Atom") "的类型和"
        (Code "(twin Atom)")
        "的类型是相同的类型."))
   ((dialogue #:id "twin-Atom")
    (Ld "接着, 使用一般性的" (Code "twin")
        "来定义" (Code "twin-Atom") "."
        (CodeB "(claim twin-Atom
  (→ Atom
    (Pair Atom Atom)))"))
    (Rd "可以使用来自于" (Ref "flip-use") "的技巧."
        (CodeB "(define twin-Atom
  (twin Atom))")))
   ((dialogue)
    (Ld (CodeB "(twin-Atom 'cherry-pie)")
        "和"
        (CodeB "((twin Atom) 'cherry-pie)")
        "是相同的"
        (CodeB "(Pair Atom Atom)")
        "吗?")
    (Rd "是的, 并且其值 (也是规范形式) 为"
        (CodeB "(cons 'cherry-pie 'cherry-pie)")
        "对于甜点而言, 这就是双倍了!"))
   (H2 "表, 表, 更多的表")
   ((dialogue)
    (Ld (Code "Π") "怎么样?")
    (Rd "美味至极. 尽管如此, 使用餐巾可以使得用餐不那么狼狈."))
   ((dialogue)
    (Ld "在我们开始之前, 你有没有"
        (Ul (Li "烹饪普罗旺斯杂烩,")
            (Li "吃完两个樱桃派,")
            (Li "尝试使用带图画的餐巾清理一下,")
            (Li "理解" (Code "rec-Nat") ", 以及")
            (Li "休息安睡好"))
        "呢?")
    (Rd "这俨然是期望清单."))
   ((dialogue #:id "nil")
    (Ld "是的, 不过这些是很好的期望."
        (CodeB "(claim expectations
  (List Atom))
(define expectations
  (:: 'cooked
    (:: 'eaten
      (:: 'tried-cleaning
        (:: 'understood
          (:: 'slept nil))))))"))
    (Rd "这段代码有些令我困惑, 在于以下几个方面:"
        (Ul (Li (Code "::") "还没有描述,")
            (Li "类型构造子" (Code "List")
                "还没有描述, 以及")
            (Li "原子" (Code "'nil") "已经作为"
                (Code "step-zerop")
                "的一部分被使用."))))
   ((dialogue)
    (Ld (Code "'nil") "和"
        (Ref "nil") "里的"
        (Code "nil") "是相同的吗?")
    (Rd "不是, 因为" (Ref "nil")
        "里的" (Code "nil")
        "并非一个" (Code "Atom")
        ", 其不以单引号开头."
        (P (Code "nil") "是一个表达式吗?")))
   ((dialogue)
    (Ld (Code "List") "是一个类型构造子. 如果"
        $E "是一个类型, 那么"
        (Code "(List " $E ")")
        "是一个类型."
        ((comment)
         "读作" (Q "类型为" $E "的元素的列表")
         "或者更简单的" (Q $E "的列表") "."))
    (Rd "那么, 成为一个" (Code "(List " $E ")")
        "的意思是什么呢?"))
   ((law)
    (Center (Code "List") "之律")
    (P "如果" $E "是一个类型, 那么"
       (Code "(List " $E ")")
       "是一个类型."))
   ((dialogue)
    (Ld (Code "nil") "是一个"
        (Code "(List Atom)") "吗?")
    (Rd "在" (Ref "nil") "里" (Code "nil")
        "看起来扮演着空列表的角色."))
   ((dialogue)
    (Ld "是的, " (Code "nil") "的确是一个"
        (Code "(List Atom)") "."
        (P (Code "nil") "是一个"
           (Code "(List Nat)") "吗?"))
    (Rd "似乎不是这样, 因为" (Code "nil")
        "已经是一个" (Code "(List Atom)") "了."))
   ((dialogue)
    (Ld "实际上, " (Code "nil") "也是一个"
        (Code "(List Atom)") "."
        (P (Code "nil") "是一个"
           (Code "(List (List Atom))") "吗?"))
    (Rd "是的, 因为" (Code "(List Atom)")
        "是一个类型, 所以" (Code "(List (List Atom))")
        "也是一个类型. " (Code "(List (Pair Nat Atom))")
        "怎么样呢?"
        (P "是不是" (Code "nil")
           "可以具有任意以上类型之一呢?")))
   ((dialogue)
    (Ld "的确如此.")
    (Rd "这意味着" (Code "nil") "也是一个"
        (CodeB "(List 'potato)")
        "咯?"))
   ((dialogue)
    (Ld "不, 并非如此, 因为" (Code "'potato")
        "不是一个类型.")
    (Rd "是不是这和" (Ref "not-a-type") "中"
        (CodeB "(Pair 'olive 'oil)")
        "并非类型出于相同的原因?"))
   ((dialogue)
    (Ld "是的."
        (P "之所以" (Code "(List 'potato)")
           "不是一个类型, 是因为"
           (Code "'potato")
           "是一个" (Code "Atom")
           ", 而不是一个类型."))
    (Rd "好吧. 这意味着如果" $E "是一个类型, 那么"
        (Code "(List " $E ")") "是一个类型, 对吗?"))
   ((dialogue)
    (Ld "并且, 如果" (Code "(List " $E ")")
        "是一个类型, 那么" (Code "nil")
        "是一个" (Code "(List " $E ")") ".")
    (Rd "好吧."
        (P (Code "nil") "是一个构造子吗?")))
   ((dialogue)
    (Ld "是的, " (Code "nil") "的确是一个构造子."
        (P "猜猜" (Code "(List " $E ")")
           "还有什么其他的构造子."))
    (Rd "根据" (Code "expectations")
        ", " (Code "::") "是另一个构造子."))
   ((dialogue)
    (Ld (Code "::") "和" (Code "cons") "有什么区别?"
        (P "构造子" (Code "::") "构建一个" (Code "List")
           "...")
        ((comment)
         "出于历史原因, " (Code "::")
         "亦读作" (Q "cons") "或者"
         (Q "list-cons") "."))
    (Rd "..., 但是构造子" (Code "cons")
        "构建一个" (Code "Pair") "."))
   ((dialogue)
    (Ld "可以有序对的列表, 也可以有列表的序对."
        (P "何时" (Code "(:: " $e " " $es ")")
           "是一个" (Code "(List " $E ")") "?")
        ((comment)
         $e "的复数形式是" $es ", 读作"
         (Q "ease") ". 使用" $es
         "是因为一个列表的剩余部分可以"
         "具有任意数目的元素."))
    (Rd "嗯, " $es "必须要是一个" (Code "(List " $E ")")
        ". " $es "可以是" (Code "nil") ", 而"
        (Code "nil") "是一个" (Code "(List " $E ")") "."))
   ((dialogue)
    (Ld $e "可以是任意的东西吗?")
    (Rd "当然可以!"))
   ((dialogue)
    (Ld "当然不行! 再试试.")
    (Rd "我猜一下: " $e "必须要是一个" $E
        ", 不然的话" $E "还没有任何用处."))
   ((dialogue)
    (Ld "正确的答案, 错误的原因."
        (P "之所以" $e "必须要是一个" $E
           ", 是因为为了能够使用" (Code "(List " $E ")")
           "的一个消去子, 我们需要保证这样的列表的每个元素"
           "都是一个" $E ".")
        (P "请将" (Code "rugbrød") "定义为丹麦黑麦面包的成分.")
        ((comment)
         "读作[ˈʁuˌb̥ʁœðˀ], 如果还是不懂, 就去问问丹麦人.")
        ((tcomment)
         "rugbrød, 丹麦语, 意思是黑麦面包."))
    (Rd "那么成分有什么呢?"))
   ((dialogue)
    (Ld (Em "rugbrød") "里的成分有:"
        (Ul (Li "全黑麦面粉,")
            (Li "黑麦籽粒, 浸泡直至变软,")
            (Li "纯净水,")
            (Li "活跃的酵种, 以及")
            (Li "盐.")))
    (Rd (Code "rugbrød") "应该具有什么类型?"))
   ((dialogue)
    (Ld (Code "(List Atom)") ", 因为每个成分都是一个"
        (Code "Atom") ".")
    (Rd "好的, 以下就是了."
        (CodeB "(claim rugbrød
  (List Atom))
(define rugbrød
  (:: 'rye-flour
    (:: 'rye-kernels
      (:: 'water
        (:: 'sourdough
          (:: 'salt nil))))))")))
   ((dialogue)
    (Ld "非常好.")
    (Rd "是的, " (Code "rugbrød")
        "非常美味! 尽管如此, 顶上还需要加点什么才好."))
   ((dialogue)
    (Ld "让我们回到正题."
        (P (Code "rugbrød") "和" (Code "5")
           "有什么不同之处?"))
    (Rd "似乎看起来它们就没有相同的地方. "
        (Code "5") "由" (Code "add1") "和"
        (Code "zero") "构成. 而且, "
        (Code "5") "也不好吃."))
   ((dialogue)
    (Ld (Code "rugbrød") "里包含多少种成分呢?")
    (Rd "五种."))
   ((dialogue)
    (Ld "不仅只需要五种成分, " (Code "rugbrød")
        "甚至不需要揉面.")
    (Rd "那么, " (Code "::") "是不是和"
        (Code "add1") "有什么关系?"))
   ((dialogue)
    (Ld (Code "::") "使得一个列表更大, 而"
        (Code "add1") "使得一个" (Code "Nat")
        "更大."
        (P (Code "nil") "是不是和"
           (Code "zero") "有什么关系?"))
    (Rd (Code "nil") "是最小的列表, 而"
        (Code "zero") "是最小的自然数."
        (P "列表的消去子和" (Code "Nat")
           "的消去子是不是看起来差不多?")))
   ((law)
    (Center (Code "nil") "之律")
    (P (Code "nil") "是一个" (Code "(List " $E ")")
       ", 不论类型" $E "为何."))
   ((law)
    (Center (Code "::") "之律")
    (P "如果" $e "是一个" $E "而" $es "是一个"
       (Code "(List " $E ")") ", 那么"
       (Code "(:: " $e " " $es ")") "是一个"
       (Code "(List " $E ")") "."))
   ((dialogue)
    (Ld "是的."
        (CodeB "(rec-Nat " $target "
  " $base "
  " $step ")")
        "的类型是什么?")
    (Rd "当"
        (Ul (Li $target "是一个" (Code "Nat") ",")
            (Li $base "是一个" $X ", 且")
            (Li $step "是一个"
                (Code "(→ Nat " $X " " $X ")")))
        "时, " (Code "rec-Nat")
        "表达式是一个" $X "."))
   ((dialogue)
    (Ld (Code "(List " $E ")") "的消去子写作"
        (CodeB "(rec-List " $target "
  " $base "
  " $step ")")
        "并且当"
        (Ul (Li $target "是一个" (Code "(List " $E ")") ",")
            (Li $base "是一个" $X ", 且")
            (Li $step "是一个"
                (Code "(→ " $E " (List " $E ") " $X " "
                      $X ")")))
        "时, 其是一个" $X "."
        (P "这和" (Code "rec-Nat") "有什么不同之处?"))
    (Rd (Code "rec-List") "的" $step "比"
        (Code "rec-Nat") "的" $step "多一个参数&mdash;&mdash;"
        "其接受" $e ", 列表里的一个元素."))
   ((dialogue)
    (Ld "说的很好!"
        (P "在这两种情况下, step都接受相应构造子的每个参数, "
           "并且也接受对于更小的值的递归性消去."))
    (Rd "消去子暴露了值中的信息."))
   ((dialogue)
    (Ld "base也暴露了不少关于一个" (Code "rec-List")
        "的结果的信息. 能给出两个以" (Code "0")
        "为base的" (Code "rec-List") "用例吗?")
    (Rd "一个是找出列表的长度, 另一个是找出一个"
        (Code "(List Nat)") "中的所有"
        (Code "Nat") "之和."))
   ((dialogue)
    (Ld "的确是两个不错的例子."
        (CodeD "(claim step-" (Frame "     ") "
  (→ Atom (List Atom) Nat
    Nat))
(define step-" (Frame "     ") "
  (λ (" (Dim "e") " " (Dim "es") " n)
    (add1 n)))")
        "以此定义,"
        (CodeB "(rec-List nil
  0
  step-" (Frame "     ") ")")
        "之值为何?")
    (Rd "必然是" (Code "0")
        ", 因为" (Code "0")
        "是base, 而base之值必然是"
        (Code "rec-List")
        "对于" (Code "nil")
        "之值."))
   ((dialogue)
    (Ld "是的."
        (P "一个" (Code "kartoffelmad")
           "是带有" (Code "toppings")
           "和" (Code "condiments") "的"
           (Code "rugbrød") "."
           (CodeB "(claim toppings
  (List Atom))
(define toppings
  (:: 'potato
    (:: 'butter nil)))
(claim condiments
  (List Atom))
(define condiments
  (:: 'chives
    (:: 'mayonnaise nil)))"))
        ((comment)
         "butter和mayonnaise"
         "也可以换成你喜欢的非乳制品替代物."))
    (Rd "听起来非常地" (Em "美味(lækkert)") "!"))
   ((law)
    (Center (Code "rec-List") "之律")
    (P "如果" $target "是一个" (Code "(List " $E ")")
       ", " $base "是一个" $X ", 而" $step "是一个"
       (CodeB "(→ " $E " (List " $E ") " $X " " $X ")")
       "那么"
       (CodeB "(rec-List " $target "
  " $base "
  " $step ")")
       "是一个" $X "."))
   ((law)
    (Center (Code "rec-List") "之第一诫")
    (P "如果"
       (CodeB "(rec-List nil
  " $base "
  " $step ")")
       "是一个" $X ", 那么它和" $base
       "是相同的" $X "."))
   ((law)
    (Center (Code "rec-List") "之第二诫")
    (P "如果"
       (CodeB "(rec-List (:: " $e " " $es ")
  " $base "
  " $step ")")
       "是一个" $X ", 那么它和"
       (CodeB "(" $step " " $e " " $es "
  (rec-List " $es "
    " $base "
    " $step "))")
       "是相同的" $X "."))
   ((dialogue)
    (Ld "的确美味!"
        (CodeB "(rec-List condiments
  0
  step-" (Frame "     ") ")")
        "的值是什么?")
    (Rd "让我们看看."
        (same-as
         (CodeI "(rec-List (:: 'chives
            (:: 'mayonnaise
              nil))
  0
  step-" (Frame "     ") ")")
         (CodeI "(step-" (Frame "     ") "
  'chives
  (:: 'mayonnaise nil)
  (rec-List (:: 'mayonnaise
              nil)
    0
    step-" (Frame "     ") "))")
         (CodeI "(add1
  (rec-List (:: 'mayonnaise
              nil)
    0
    step-" (Frame "     ") "))"))))
   ((dialogue)
    (Ld "规范形式是什么? 不要对于省略中间的表达式保有负担.")
    (Rd "规范形式为"
        (CodeB "(add1
  (add1 zero))")
        "或者更为人知的版本是" (Code "2") "."))
   ((dialogue #:id "step-length-def")
    (Ld "这个" (Code "rec-List") "表达式将"
        (Code "condiments") "中的每个" (Code "::")
        "都替换为了" (Code "add1") ", 而" (Code "nil")
        "则被替换为了" (Code "0") "."
        (P "方框里填什么名字好呢?"))
    (Rd (Code "length") "似乎比较恰当."
        (CodeD "(claim step-length
  (→ Atom (List Atom) Nat
    Nat))
(define step-length
  (λ (" (Dim "e") " " (Dim "es") " length" (Sub "es") ")
    (add1 length" (Sub "es") ")))")))
   ((dialogue)
    (Ld "那么以下必然是" (Code "length") "的定义了."
        (CodeD "(claim length
  (→ (List Atom)
    Nat))
(define length
  (λ (es)
    (rec-List es
      0
      step-length)))"))
    (Rd "但是"
        (CodeB "(:: 17
  (:: 24
    (:: 13 nil)))")
        "的长度是多少?"))
   ((dialogue)
    (Ld "那简单, 只需将" (Code "Atom")
        "替换以" (Code "Nat") "."
        (CodeD "(claim step-length
  (→ Nat (List Nat) Nat
    Nat))
(define step-length
  (λ (" (Dim "e") " " (Dim "es") " length" (Sub "es") ")
    (add1 length" (Sub "es") ")))"))
    (Rd "而以下是对于" (Code "Nat") "列表而言的"
        (Code "length") "."
        (CodeD "(claim length
  (→ (List Nat)
    Nat))
(define length
  (λ (es)
    (rec-List es
      0
      step-length)))")))
   ((dialogue)
    (Ld "列表可以包含任意类型的元素, 不仅是"
        (Code "Atom") "和" (Code "Nat") "."
        (P "什么可以用来作成一个对于所有类型成立的"
           (Code "step-length") "版本呢?"))
    (Rd "It's as easy as " (Code "Π") "."
        (CodeB "(claim length
  (Π ((E " $U:script "))
    (→ (List E)
      Nat)))")
        ((tcomment)
         "这里是显然的一语双关.")))
   ((dialogue)
    (Ld "这个" (Code "claim") "需要一个step."
        (CodeB "(claim step-length
  (Π ((E " $U:script "))
    (→ E (List E) Nat
      Nat)))"))
    (Rd "每个步骤 (step) 时, 长度通过" (Code "add1") "增长."
        (CodeB "(define step-length
  (λ (" (Dim "E") ")
    (λ (" (Dim "e") " " (Dim "es") " length" (Sub "es") ")
      (add1 length" (Sub "es") "))))")))
   ((dialogue)
    (Ld "这个定义使用了和" (Ref "step-*") "中的"
        (Code "step-*") "相同的技巧, 以将"
        (Code "step-length") "应用于" (Code (Dim "E")) "."
        (P "现在定义" (Code "length") "."))
    (Rd "将" (Code (Dim "E")) "传递给" (Code "step-length")
        "将导致其(结果)取三个参数."
        (CodeB "(define length
  (λ (E)
    (λ (es)
      (rec-List es
        0
        (step-length E)))))")))
   ((dialogue)
    (Ld "为什么" (Code "step-length") "中的"
        (Code (Dim "e")) "是黯淡的?")
    (Rd "因为列表中的特定元素在计算列表长度时无需使用."))
   ((dialogue)
    (Ld (Code "(length Atom)") "的值是什么?")
    (Rd "其为"
        (CodeB "(λ (es)
  (rec-List es
    0
    (step-length Atom)))")
        "这是将内层" (Code "λ") "表达式的体中的每个"
        (Code "E") "替换以" (Code "Atom") "得到的."))
   ((dialogue)
    (Ld "请定义" (Code "length") "的一个特化版本, "
        "其找出一个" (Code "(List Atom)")
        "的元素数目.")
    (Rd "这会使用和" (Ref "twin-Atom")
        "中的" (Code "twin-Atom") "定义相同的技巧."
        (CodeB "(claim length-Atom
  (→ (List Atom)
    Nat))
(define length-Atom
  (length Atom))")))
   ((dialogue)
    (Ld "这是一个很实用的技巧."
        (P "现在是时候将一片面包, "
           (Code "toppings") "以及"
           (Code "condiments")
           "组装为一个美味的"
           (Em "kartoffelmad") ".")
        (P "请定义一个函数, 其合并两个列表."))
    (Rd "这个定义的类型应该是什么呢?"))
   ((dialogue)
    (Ld "我们可以将一个" (Code "(List Nat)")
        "和一个" (Code "(List (Pair Nat Nat))")
        "合并吗?")
    (Rd "不能."
        (P "一个列表中的所有元素都必须具有相同的类型.")))
   ((law)
    (Center "列表的元素之类型")
    (P "一个列表中的所有元素都必须具有相同的类型."))
   ((dialogue)
    (Ld "只要两个列表包含相同的元素类型, 它们就可以被合并, "
        "不论这个类型是什么."
        (P "这是否暗示了" (Code "append")
           "定义中的类型呢?"))
    (Rd "其类型必然是一个" (Code "Π") "表达式."
        (CodeD "(claim append
  (Π ((E " $U:script "))
    " (Frame "          ") "))")))
   ((dialogue)
    (Ld "的确如此."
        (P "剩下来的参数应该是什么呢?"))
    (Rd "这里必须有两个" (Code "(List E)")
        "参数. 而且, 结果应该也是一个"
        (Code "(List E)") ". 据此, "
        (Code "append") "必然是一个"
        (Code "λ") "表达式."))
   ((dialogue)
    (Ld "以下是claim, 现在请开始定义."
        (CodeB "(claim append
  (Π ((E " $U:script "))
    (→ (List E) (List E)
      (List E))))"))
    (Rd "它应该是一个" (Code "λ") "表达式, 但是其体仍然成谜."
        (CodeD "(define append
  (λ (E)
    (λ (start end)
      " (Frame "               ") ")))")))
   ((dialogue)
    (Ld "方框里应该是什么呢?")
    (Rd "应该是某种" (Code "rec-List") "."))
   ((dialogue)
    (Ld (CodeB "(append Atom
  nil
  (:: 'salt
    (:: 'pepper nil)))")
        "的值是什么?")
    (Rd "显然应该是"
        (CodeB "(:: 'salt
  (:: 'pepper nil))")))
   ((dialogue)
    (Ld (CodeB "(append Atom
  (:: 'cucumber
    (:: 'tomato nil))
  (:: 'rye-bread nil))")
        "的规范形式又应该是什么呢?")
    (Rd "那必然是"
        (CodeB "(:: 'cucumber
  (:: 'tomato
    (:: 'rye-bread nil)))")))
   ((dialogue)
    (Ld (Code "(append E nil end)")
        "的值应该是" (Code "end")
        "的值. 因此, " (Code "append")
        "的最后一个参数" (Code "end")
        "应该是base."
        ((tcomment)
         (Code "(append E nil end)") "中的"
         (Code "E") "和" (Code "end")
         "是元变量."))
    (Rd "step呢?"))
   ((dialogue)
    (Ld "step的类型由" (Code "rec-List")
        "之律确定. 其应该能够对于任意的元素类型成立.")
    (Rd "比如说这个?"
        (CodeB "(claim step-append
  (Π ((E " $U:script "))
    (→ E (List E) (List E)
      (List E))))")))
   ((dialogue)
    (Ld "以之前的框为例, 填充" (Code "step-append")
        "的剩余部分."
        (CodeD "(define step-append
  (λ (E)
    (λ (e es append" (Sub "es") ")
      " (Frame "               ") ")))
(define append
  (λ (E)
    (λ (start end)
      (rec-List start
        end
        (step-append E)))))")
        ((comment)
         "当列表包含的元素类型为" (Code "E")
         "时, 表达式" (Code "(step-append E)")
         "应该是" (Code "append") "的一个step. "
         "请注意Currying."))
    (Rd "如果" (Code "append" (Sub "es")) "是"
        (CodeB "nil")
        "那么" (Code "step-append") "应该产生"
        (CodeB "(:: 'rye-bread nil)")
        "如果" (Code "append" (Sub "es")) "是"
        (CodeB "(:: 'rye-bread nil)")
        "那么" (Code "step-append") "应该产生"
        (CodeB "(:: 'tomato
  (:: 'rye-bread nil))")
        "最后, 如果" (Code "append" (Sub "es")) "是"
        (CodeB "(:: 'tomato
  (:: 'rye-bread nil))")
        "那么" (Code "step-append") "应该产生"
        (CodeB "(:: 'cucumber
  (:: 'tomato
    (:: 'rye-bread nil)))")
        ((tcomment)
         "这里的第一段我不是很理解, 因为你并不会在运行"
         (Code "append") "过程中把参数" (Code "end")
         "拆开.")))
   ((dialogue)
    (Ld "这是很好的推理."
        (P "所以正确的定义是什么?"))
    (Rd "现在" (Code "append") "就不应该用虚线框住了."
        (CodeB "(define step-append
  (λ (E)
    (λ (e " (Dim "es") " append" (Sub "es") ")
      (:: e append" (Sub "es") "))))
(define append
  (λ (E)
    (λ (start end)
      (rec-List start
        end
        (step-append E)))))")))
   ((dialogue)
    (Ld (Code "append") "的定义很像" (Code "+") ".")
    (Rd "是不是有一个" (Code "iter-List")
        ", 就像是" (Code "iter-Nat")
        ", 而我们可以用它来定义" (Code "append") "?"
        ((tcomment)
         "我想是不太行的, 因为按照类比, " (Code "iter-List")
         "的step不能知道参数" (Code "e") "会是什么.")))
   ((dialogue)
    (Ld "没有什么能够阻止我们定义" (Code "iter-List")
        ", 但是又没有必要, 因为" (Code "iter-List")
        "能做的, " (Code "rec-List") "也能做, 就像"
        (Code "iter-Nat") "和" (Code "which-Nat")
        "能做的, " (Code "rec-Nat") "也能做.")
    (Rd "好的, 这里我们就用更富表达力的消去子吧."))
   ((dialogue)
    (Ld "实际上还可以用另外的方式定义" (Code "append")
        ", 其将" (Code "::") "替换成别的什么东西.")
    (Rd "真的能行吗?"))
   ((dialogue)
    (Ld "当然可以. 除了使用" (Code "::")
        "以将第一个列表的元素" (Q "cons")
        "到结果的开头, 也可以使用" (Code "snoc")
        "将第二个列表的元素加到结果的末尾."
        (P "例如,"
           (CodeB "(snoc Atom toppings 'rye-bread)")
           "的值为"
           (CodeB "(:: 'potato
  (:: 'butter
    (:: 'rye-bread nil)))"))
        (P (Code "snoc") "的类型是什么?")
        ((comment)
         "感谢David C. Dickson (1947-)."))
    (Rd (Code "snoc") "的类型是"
        (CodeB "(claim snoc
  (Π ((E " $U:script "))
    (→ (List E) E
      (List E))))")
        "step必须要做什么呢?"))
   ((dialogue)
    (Ld "这个step必须要将列表的当前元素"
        (Q "cons") "到结果上.")
    (Rd "啊, 所以说这就像" (Code "step-append") "."))
   ((dialogue)
    (Ld "现在请定义" (Code "snoc") ".")
    (Rd "以下是" (Code "snoc") "."
        (CodeB "(define snoc
  (λ (E)
    (λ (start e)
      (rec-List start
        (:: e nil)
        (step-append E)))))")
        ((tcomment)
         "说白了, 其实就是复用" (Code "append") ".")))
   ((dialogue)
    (Ld "干得好."
        (P "现在请定义" (Code "concat")
           ", 其行为与" (Code "append")
           "类似, 但是在其step中使用了" (Code "snoc") "."
           (CodeB "(claim concat
  (Π ((E " $U:script "))
    (→ (List E) (List E)
      (List E))))")
           (Code "concat") "的类型和" (Code "append")
           "的类型相同, 因为它们做相同的事情."))
    (Rd "除了使用" (Code "snoc") "而不是" (Code "List")
        "的" (Q "cons") (Code "::") ", " (Code "concat")
        "必须要消去的是其第二个(作为参数的)列表."
        (CodeB "(claim step-concat
  (Π ((E " $U:script "))
    (→ E (List E) (List E)
      (List E))))
(define step-concat
  (λ (E)
    (λ (e " (Dim "es") " concat" (Sub "es") ")
      (snoc E concat" (Sub "es") " e))))
(define concat
  (λ (E)
    (λ (start end)
      (rec-List end
        start
        (step-concat E)))))")
        ((tcomment)
         "不幸的是, " (Code "concat")
         "的定义是错误的, 它实际上相当于第一个列表"
         "和反转了的第二个列表合并. "
         "没有什么特别好的补救措施, 因为"
         (Code "snoc") "和" (Code "concat")
         "本身对于" (Code "List") "就不那么自然. "
         "尽管如此, 你的确可以使用之后的"
         (Code "reverse") "来反转" (Code "end")
         "以得到一个更加别扭但是还算正确的定义.")))
   ((dialogue)
    (Ld "一个列表也可以通过使用" (Code "snoc")
        "得以反转."
        (P (Code "reverse") "的类型应该是什么?"))
    (Rd (Code "reverse") "接受单独一个列表作为参数."
        (CodeB "(claim reverse
  (Π ((E " $U:script "))
    (→ (List E)
      (List E))))")))
   ((dialogue)
    (Ld "每一步骤 (step) 该做什么呢?")
    (Rd "对于每个步骤, " (Code "e")
        "应该通过" (Code "snoc")
        "添加到反转了的" (Code "es")
        "的末尾."
        (CodeB "(claim step-reverse
  (Π ((E " $U:script "))
    (→ E (List E) (List E)
      (List E))))")))
   ((dialogue)
    (Ld "现在请定义" (Code "step-reverse")
        "和" (Code "reverse") ".")
    (Rd "以下就是了."
        (CodeB "(define step-reverse
  (λ (E)
    (λ (e " (Dim "es") " reverse" (Sub "es") ")
      (snoc E reverse" (Sub "es") " e))))
(define reverse
  (λ (E)
    (λ (es)
      (rec-List es
        nil
        (step-reverse E)))))")
        ((comment)
         "在使用Pie语言时, 必须要将这里的"
         (Code "nil") "写成"
         (Code "(the (List E) nil)")
         "才行.")))
   ((dialogue)
    (Ld "现在是时候来点" (Em "lækkert")
        "的东西了."
        (CodeB "(claim kartoffelmad
  (List Atom))
(define kartoffelmad
  (append Atom
    (concat Atom
      condiments toppings)
    (reverse Atom
      (:: 'plate
        (:: 'rye-bread nil)))))")
        (Code "kartoffelmad")
        "的规范形式是什么?")
    (Rd "即"
        (CodeB "(:: 'chives
  (:: 'mayonnaise
    (:: 'potato
      (:: 'butter
        (:: 'rye-bread
          (:: 'plate nil))))))")
        ((tcomment)
         "原文将" (Code "condiments")
         "和" (Code "toppings")
         "的顺序弄反了, 翻译是按照经过勘误的版本来的.")))
   ((dialogue)
    (Ld "我们问的是规范形式, 而非值, 这是很好的. "
        "否则的话, 你原本可能就要在吃的时候自己组转除了"
        (Code "'chives") "的一切了.")
    (Rd "反转列表是令人发饿的工作."))
   (H2 "究竟到底是多少?")
   ((dialogue)
    (Ld "...")
    (Rd "在吃了那么多三明治之后, 吃点"
        (Code "Π") "也是好的."))
   ((dialogue)
    (Ld "我们很高兴你问了...")
    (Rd "我很擅长预测你想要我提出的问题."))
   ((dialogue)
    (Ld "当然了, 让我们开始吧."
        (P "让我们定义一个函数" (Code "first")
           ", 其找出" (Em "任意") (Code "List")
           "的第一个元素."))
    (Rd "那不是很简单吗?"))
   ((dialogue)
    (Ld "实际上, 这是不可能哒!")
    (Rd "为什么不可能?"))
   ((dialogue)
    (Ld "之所以不可能, 是因为" (Code "nil")
        "没有第一个元素...")
    (Rd "...因而" (Code "first") "不是完全的."))
   ((dialogue)
    (Ld "写一个" (Code "last")
        "函数怎么样, 它不是找出第一个元素, 而是找出一个"
        (Code "List") "的最后一个元素?")
    (Rd "函数" (Code "last") "也不是完全的, 因为"
        (Code "nil") "也没有最后一个元素."))
   ((dialogue)
    (Ld "为了能够写下一个完全函数" (Code "first")
        ", 我们必须使用一个比" (Code "List")
        "更加特化 (specific) 的类型. "
        "这样一个更加特化的类型被称为"
        (Code "Vec") ", 其是" (Q "向量 (vector)")
        "的缩写, 但是它真的只是带有长度的列表而已."
        (P "当" $E "是一个类型而" $k
           "是一个" (Code "Nat") "时, 表达式"
           (Code "(Vec " $E " " $k ")")
           "是一个类型. 这个" (Code "Nat")
           "给出了列表的长度.")
        (P (Code "(Vec Atom 3)")
           "是一个类型吗?")
        ((comment)
         (Code "(Vec " $E " " $k ")") "读作"
         (Q "长度为" $k "的" $E "的列表")
         "或者更简单的"
         (Q $E "的列表, 长度为" $k) "."))
    (Rd "类型可以包含不是类型的表达式吗?"))
   ((dialogue)
    (Ld "正如类型可以是对于某个表达式求值的结果一样 (见"
        (Ref "type-eval")
        "), 某些类型可以包含其他并非类型的表达式."
        ((tcomment)
         "意味不明的类比."))
    (Rd "那么, 之所以" (Code "(Vec Atom 3)")
        "是一个类型, 是因为" (Code "Atom")
        "是一个类型而" (Code "3")
        "显然是一个" (Code "Nat") "."))
   ((dialogue)
    (Ld (CodeB "(Vec
  (cdr
    (cons 'pie
      (List (cdr (cons Atom Nat)))))
  (+ 2 1))")
        "是一个类型吗?")
    (Rd "必然是的, 因为"
        (CodeB "(cdr
  (cons 'pie
    (List (cdr (cons Atom Nat)))))")
        "和"
        (CodeB "(List Nat)")
        "是相同的类型, 且"
        (CodeB "(+ 2 1)")
        "和"
        (CodeB "3")
        "是相同的" (Code "Nat")
        ". 这意味着该表达式和"
        (CodeB "(Vec (List Nat) 3)")
        "是相同的, 而其 (指后者) 显然是一个类型."))
   ((dialogue)
    (Ld (Code "(Vec " $E " zero)")
        "唯一的构造子是" (Code "vecnil") ".")
    (Rd "这是因为" (Code "vecnil")
        "的长度为" (Code "zero") "吗?"))
   ((dialogue)
    (Ld "恰是如此."
        (P (Code "vec::") "是"
           (CB '(Vec E (add1 k)))
           "唯一的构造子."))
    (Rd "这里的" $k "是什么?"))
   ((dialogue #:id "vec::")
    (Ld "这里, " $k "可以是任意的" (Code "Nat") "."
        (P "当" $e "是一个" $E "而" $es "是一个"
           (Code "(Vec " $E " " $k ")") "时, "
           (Code "(vec:: " $e " " $es ")") "是一个"
           (C '(Vec E (add1 k))) "."))
    (Rd "如果一个表达式是一个"
        (C '(Vec E (add1 k)))
        "那么其值至少拥有一个元素, 因而有可能定义"
        (Code "first") "和" (Code "last")
        ", 是不是这样呢?"))
   ((dialogue)
    (Ld "对的."
        (CodeB "(vec:: 'oyster vecnil)")
        "是一个"
        (CodeB "(Vec Atom 1)")
        "吗?")
    (Rd "是的, 因为"
        (CodeB "'oyster")
        "是一个"
        (CodeB "Atom")
        "而"
        (CodeB "vecnil")
        "是一个"
        (CodeB "(Vec Atom zero)")))
   ((law)
    (Center (Code "Vec") "之律")
    (P "如果" $E "是一个类型而" $k
       "是一个" (Code "Nat") ", 那么"
       (Code "(Vec " $E " " $k ")")
       "是一个类型."))
   ((law)
    (Center (Code "vecnil") "之律")
    (P (Code "vecnil") "是一个"
       (Code "(Vec " $E " zero)") "."))
   ((law)
    (Center (Code "vec::") "之律")
    (P "如果" $e "是一个" $E "而" $es "是一个"
       (Code "(Vec " $E " " $k ")") ", 那么"
       (Code "(vec:: " $e " " $es ")") "是一个"
       (C '(Vec E (add1 k))) "."))
   ((dialogue)
    (Ld (CodeB "(vec:: 'crimini
  (vec:: 'shiitake vecnil))")
        "是一个"
        (CodeB "(Vec Atom 3)")
        "吗?")
    (Rd "不是, 因为其并非恰有三个原子的列表."))
   ((dialogue)
    (Ld "这和" (Ref "vec::")
        "是怎样联系起来的呢?"
        ((tcomment)
         "原文是第11框, 但是译者认为第12框更合理一点."))
    (Rd "之所以其并非"
        (CodeB "(Vec Atom 3)")
        "是因为"
        (CodeB "(vec:: 'shiitake vecnil)")
        "不是一个"
        (CodeB "(Vec Atom 2)")))
   ((dialogue)
    (Ld "为什么"
        (CodeB "(vec:: 'shiitake vecnil)")
        "不是一个"
        (CodeB "(Vec Atom 2)"))
    (Rd "如果它真的是一个" (Code "(Vec Atom 2)")
        "的话, 那么基于" (Ref "vec::") "中的描述"
        (CodeB "vecnil")
        "就会是一个"
        (CodeB "(Vec Atom 1)")))
   ((dialogue)
    (Ld "为什么不能是这样呢?")
    (Rd "因为"
        (CodeB "vecnil")
        "是一个"
        (CodeB "(Vec Atom zero)")
        "而" (Code "1") "和" (Code "zero")
        "不是相同的" (Code "Nat") "."))
   ((dialogue)
    (Ld "为什么" (Code "1") "和" (Code "zero")
        "不是相同的" (Code "Nat") "呢?")
    (Rd "根据" (Ref "Nat-same")
        "的解释, 两个" (Code "Nat")
        "相同, 当其值相同; 而其值相同, "
        "当其均为" (Code "zero")
        "或者均以" (Code "add1")
        "为顶(且" (Code "add1")
        "的参数是相同的" (Code "Nat") ")."
        ((tcomment)
         "括号里的内容是译者添加的. 另外, "
         "不仅是第100框, 也有第101框的内容.")))
   ((dialogue)
    (Ld "现在可以定义" (Code "first-of-one")
        "了, 其获取一个" (Code "(Vec " $E " 1)")
        "的第一个元素.")
    (Rd "但是这可能吗? 到目前为止, 我们还没有"
        (Code "Vec") "的消去子."))
   ((dialogue)
    (Ld "很好的论点. " (Code "Vec")
        "的两个消去子是" (Code "head")
        "和" (Code "tail") ".")
    (Rd (Code "head") "和" (Code "tail")
        "是什么意思呢?"))
   ((dialogue)
    (Ld "当"
        (CodeB $es)
        "是一个"
        (CB '(Vec E (add1 k)))
        "时,"
        (CodeB "(head " $es ")")
        "是一个"
        (CodeB $E)
        $es "的值可以具有怎样的形式?")
    (Rd "它不可能是" (Code "vecnil")
        ", 因为" (Code "vecnil")
        "只有" (Code "zero") "个元素. 因此, "
        $es "以" (Code "vec::") "为顶."))
   ((dialogue)
    (Ld "表达式"
        (CodeB "(head
  (vec:: " $a " " $d "))")
        "和" $a "是相同的" $E ".")
    (Rd (Code "tail") "怎么样呢?"))
   ((dialogue)
    (Ld "当"
        (CodeB $es)
        "是一个"
        (CB '(Vec E (add1 k)))
        "时,"
        (CodeB "(tail " $es ")")
        "是一个"
        (CodeB "(Vec " $E " " $k ")"))
    (Rd $es "以" (Code "vec::") "为顶."
        (CodeB "(tail
  (vec:: " $a " " $d "))")
        "和"
        (CodeB $d)
        "是相同的"
        (CodeB $E)
        "吗?"))
   ((dialogue)
    (Ld "不是, 但"
        (CodeB "(tail
  (vec:: " $a " " $d "))")
        "和"
        (CodeB $d)
        "是相同的"
        (CodeB "(Vec " $E " " $k ")")
        "现在定义" (Code "first-of-one") ".")
    (Rd (Code "first-of-one") "使用"
        (Code "head") "来找出这仅有的元素."
        (CodeB "(claim first-of-one
  (Π ((E " $U:script "))
    (→ (Vec E 1)
      E)))
(define first-of-one
  (λ (" (Dim "E") ")
    (λ (es)
      (head es))))")))
   ((dialogue)
    (Ld (CodeB "(first-of-one Atom
  (vec:: 'shiitake vecnil))")
        "的值是什么?")
    (Rd "是" (Code "'shiitake") "."))
   ((dialogue)
    (Ld (CodeB "(first-of-one Atom vecnil)")
        "的值是什么?")
    (Rd "这个问题是没有意义的, 因为"
        (CodeB "(first-of-one Atom vecnil)")
        "并不由某个类型刻画, 而这又是因为"
        (CodeB "vecnil")
        "并非一个"
        (CodeB "(Vec Atom 1)")))
   ((dialogue)
    (Ld "完全正确, 这的确是一个毫无意义的问题."
        (P "现在请定义" (Code "first-of-two") "."))
    (Rd "那必然非常类似于" (Code "first-of-one") "."
        (CodeB "(claim first-of-two
  (Π ((E " $U:script "))
    (→ (Vec E 2)
      E)))
(define first-of-two
  (λ (" (Dim "E") ")
    (λ (es)
      (head es))))")))
   ((dialogue)
    (Ld (CodeB "(first-of-two Atom
  (vec:: 'matsutake
    (vec:: 'morel
      (vec:: 'truffle vecnil))))")
        "的值是什么?")
    (Rd "这个列表上的都是蘑菇珍品."
        (P "然而, 问题本身并不意义, "
           "因为这个蘑菇珍品的列表放了三个蘑菇, "
           "而不是恰有两个蘑菇.")))
   ((dialogue)
    (Ld "很好的论点."
        (P "是时候定义" (Code "first-of-three") "了."))
    (Rd "存在" (Code "first")
        "对于任意长度成立吗?"))
   ((dialogue)
    (Ld "不行, 因为长度为" (Code "zero")
        "时并不存在" (Code "first")
        "的元素. 但是, 可以定义" (Code "first")
        ", 使其找出任意" (Em "至少拥有一个元素")
        "的列表的第一个元素.")
    (Rd "听起来有点困难."))
   ((dialogue)
    (Ld "实际上, 并不那么困难."
        (P "事实上, 简单如..."))
    (Rd "..." (Code "Π") "?"))
   ((dialogue)
    (Ld (Code "Π") "比我们所见更加灵活.")
    (Rd "什么是更加灵活的" (Code "Π") "呢?"))
   ((dialogue)
    (Ld "一人食的蘑菇派. "
        "(A mushroom pot pie, for one.)")
    (Rd "什么是更加灵活的" (Code "Π") "表达式呢?"))
   ((dialogue)
    (Ld "以下是" (Code "first") "的声明."
        (CodeB "(claim first
  (Π ((E " $U:script ")
      (" $l:script " Nat))
    (→ (Vec E (add1 " $l:script "))
      E)))")
        "这里有什么新奇之处吗?")
    (Rd "参数名" (Code $l:script)
        "后面跟着的, 是" (Code "Nat")
        ". 而之前, " (Code "Π")
        "表达式里参数名后面跟着的总是"
        $U:script "."
        (P (CodeB "(→ (Vec E (add1 " $l:script "))
  E)")
           "中的" (Code "E")
           "指的是不论什么作为" (Code "first")
           "的第一个参数的" $U:script ". 是不是这意味着"
           (Code "(add1 " $l:script ")")
           "中的" (Code $l:script)
           "指的是不论什么作为" (Code "first")
           "的第二个参数的" (Code "Nat") "呢?")))
   ((law)
    (Center (Code "Π") "之律")
    (P "表达式"
       (CodeB "(Π ((" $y " " $Y ")) " $X ")")
       "是一个类型, 当" $Y "是一个类型, 且若"
       $y "是一个" $Y ", " $X "是一个类型.")
    ((tcomment)
     "虽然没有明说, 但是" $X "中的" $y "是被绑定的."))
   ((dialogue)
    (Ld "完全正确. " (Code "(add1 " $l:script ")")
        "保证了作为" (Code "first")
        "的第三个参数的列表至少拥有一个元素."
        (P "现在请定义" (Code "first") "."))
    (Rd "以下就是了."
        (CodeB "(define first
  (λ (" (Dim "E") " " (Dim $l:script) ")
    (λ (es)
      (head es))))")))
   ((dialogue)
    (Ld (CodeB "(first Atom 3
  (vec:: 'chicken-of-the-woods
    (vec:: 'chantrelle
      (vec:: 'lions-mane
        (vec:: 'puffball vecnil)))))")
        "的值是什么?")
    (Rd "是" (Code "'chicken-of-the-woods") "."
        (P "但是, 为什么元素的数目是"
           (CodeB "(add1 " $l:script ")")
           "而非仅仅是"
           (CodeB $l:script)
           "呢?")))
   ((dialogue)
    (Ld (Code "vecnil") "中不能找到第一个元素, "
        "因其只有" (Code "zero") "个元素."
        (P "不论" (C 'l) "如何, "
           (C '(add1 l)) "和" (Code "zero")
           "永远不可能是相同的" (Code "Nat")
           ", 于是" (Code "vecnil") "不是一个"
           (C #:constant* '(E)
              '(Vec E (add1 l))) "."))
    (Rd "我们通过使用更加特化的类型以排除不想要的(实际)参数, "
        "这避免了试图定义一个并非完全的函数."))
   ((law)
    (Center "使用更加特化的类型")
    (P "通过使用更加特化的类型以排除不想要的参数"
       "来使得函数变成完全的."))
   ((dialogue)
    (Ld "相同的定义本可写成两个嵌套的" (Code "Π")
        "表达式的形式."
        (CodeD "(claim first
  (Π ((E " $U:script "))
    (Π ((" $l:script " Nat))
      (→ (Vec E (add1 " $l:script "))
        E))))
(define first
  (λ (" (Dim "E") ")
    (λ (" (Dim $l:script) ")
      (λ (es)
        (head es)))))")
        "为什么这会是相同的定义?")
    (Rd "答案是, 因为具有多个参数名的" (Code "Π")
        "表达式不过就是嵌套的单参数名" (Code "Π")
        "表达式的简略写法而已."))
   ((dialogue)
    (Ld "实际上, 这个定义本也可以写成三个嵌套的"
        (Code "Π") "表达式的形式."
        (CodeD "(claim first
  (Π ((E " $U:script "))
    (Π ((" $l:script " Nat))
      (Π ((" (Dim "es") " (Vec E (add1 " $l:script "))))
        E))))
(define first
  (λ (" (Dim "E") ")
    (λ (" (Dim $l:script) ")
      (λ (es)
        (head es)))))")
        "为什么" (Em "这") "也是相同的定义?")
    (Rd "真的是相同的定义吗?"
        (P "前一个定义有" (Code "→")
           ", 但这个定义没有.")))
   ((dialogue)
    (Ld "实际上, " (Code "→")
        "是当参数名不在" (Code "Π")
        "表达式的体中出现时的对于"
        (Code "Π") "表达式的一种简略写法.")
    (Rd "啊, 好吧."))
   ((law)
    (Center (Code "→") "和" (Code "Π"))
    (P "类型"
       (CB '(→ Y X))
       "是对于"
       (CodeB "(Π ((" (Dim $y) " " $Y ")) "
              $X ")")
       "的简略写法, 当"
       (Dim $y) "在" $X "中没有被用到时."))
   ((law)
    (Center (Code "λ") "之终律")
    (P "若当" $y "是一个" $Y "时" $x
       "是一个" $X ", 那么"
       (CodeB "(λ (" $y ") " $x ")")
       "是一个"
       (CodeB "(Π ((" $y " " $Y ")) "
              $X ")"))
    ((tcomment)
     "虽然没有显式写出, 但是这里的" $x "依赖于" $y
     ", " $X "也依赖于" $y ". 当然, 实际上这两个"
     $y "不需要使用相同的名字 (但指的是同一个东西). "
     "多说一句, 虽然这里是" (Code "λ")
     "之终律, 但是本书之中并无" (Code "λ")
     "之始律, 这可能是作者的失误. 不过, "
     "既然律陈述的是定型规则, " (Code "λ")
     "之始律应该说的是还没有" (Code "Π")
     "的情况下" (Code "λ") "表达式的类型."))
   ((law)
    (Center "应用之终律")
    (P "如果" $f "是一个"
       (CodeB "(Π ((" $y " " $Y ")) "
              $X ")")
       "而" $z "是一个" $Y ", 那么"
       (CodeB "(" $f " " $z ")")
       "是一个" $X ", 其中每个" $y
       "都已被一致地替换为了" $z ".")
    ((tcomment)
     "应用之始律里没有" (Code "Π")
     ", 应用之中律里" (Code "Π")
     "的参数的类型都是" $U:script
     ", 终律放宽了这个限制."))
   ((law)
    (Center (Code "λ") "终第一诫")
    (P "如果两个" (Code "λ")
       "表达式可以通过一致换名使其成为相同的"
       (CodeB "(Π ((" $y " " $Y ")) "
              $X ")")
       "那么它们就是相同的."))
   ((law)
    (Center (Code "λ") "终第二诫")
    (P "如果" $f "是一个"
       (CodeB "(Π ((" $y " " $Y ")) "
              $X ")")
       "而" $y "不出现在" $f "中, 那么" $f "和"
       (CodeB "(λ (" $y ") (" $f " " $y "))")
       "是相同的.")
    ((tcomment)
     "这里的" (Q "出现") "应该指的是"
     (Q "自由出现") "."))
   ((dialogue)
    (Ld "类型"
        (CodeB "(Π ((" (Dim "es") " (Vec E (add1 " $l:script "))))
  E)")
        "本可以写成"
        (CB #:constant* '(E)
            '(→ (Vec E (add1 l)) E))
        "因为" (Code (Dim "es")) "在" (Code "E")
        "中没有被用到."
        (P "实际上, 我们本也可以将" (Code "first")
           "的声明写成一个单独的" (Code "Π")
           "表达式而不使用" (Code "→") "."))
    (Rd "之前最后一个版本的" (Code "first")
        "也可以写成这样:"
        (CodeD "(claim first
  (Π ((E " $U:script ")
      (" $l:script " Nat)
      (" (Dim "es") " (Vec E (add1 " $l:script "))))
    E))
(define first
  (λ (" (Dim "E") " " (Dim "l") " es)
    (head es)))")
        "这是因为嵌套的" (Code "Π")
        "表达式本就可以写成单独一个"
        (Code "Π") "表达式的形式."))
   ((dialogue)
    (Ld "更加特化的类型使得我们能够定义"
        (Code "first")
        ", 这是我们自己的类型化版本的"
        (Code "head") "."
        (P "若是要定义" (Code "rest")
           ", 这是我们自己的版本的"
           (Code "tail")
           ", 是不是也需要更加特化的类型呢?"))
    (Rd "当然如此, 因为" (Code "(tail vecnil)")
        "和" (Code "(head vecnil)")
        "同样地毫无意义."))
   ((dialogue)
    (Ld "这更特化的类型是什么呢?")
    (Rd "参数必然以" (Code "vec::") "为顶."
        (P "因为" (Code "head")
           "不是tail的一部分, 因此作为结果的"
           (Code "Vec") "变得更短了."
           (CodeB "(claim rest
  (Π ((E " $U:script ")
      (" $l:script " Nat))
    (→ (Vec E (add1 " $l:script "))
      (Vec E " $l:script "))))"))))
   ((dialogue)
    (Ld (Code "head") "和" (Code "tail")
        "都是函数, 而所有函数都是完全的. "
        "这意味着它们不可能与" (Code "List")
        "一起使用, 因为" (Code "List")
        "无法排除" (Code "nil") "."
        (P "现在请定义" (Code "rest") "."))
    (Rd "以下就是了."
        (CodeB "(define rest
  (λ (" (Dim "E") " " (Dim "l") ")
    (λ (es)
      (tail es))))")))
   (H2 "完全取决于动机" #:id "ch7")
   ((dialogue #:id "peas-type0")
    (Ld "我们的蘑菇派需要少许豌豆搭配. "
        "是时候定义" (Code "peas")
        "了, 其产生所需数目的豌豆."
        (P "什么样的类型表达了这种行为呢?"))
    (Rd "类型是"
        (CB '(→ Nat (List Atom)))
        "因为" (Code "peas")
        "能够产生任意数目的豌豆."))
   ((dialogue)
    (Ld (Code "peas")
        "到底应该产生多少豌豆呢?")
    (Rd "看情况咯. (It depends.)"))
   ((dialogue)
    (Ld "依赖于什么呢? (What does it depend on?)")
    (Rd "其依赖于豌豆所需要的数目, 即参数."))
   ((dialogue)
    (Ld (Ref "peas-type0") "中的类型"
        (CB '(→ Nat (List Atom)))
        "不够特化. 它没有表达出"
        (Code "peas") (Em "精确地")
        "产生了其被索取的豌豆数目.")
    (Rd "豌豆的数目是" (Code "Nat")
        "参数. 以下的类型有用吗?"
        (CodeB "(claim peas
  (Π ((how-many-peas Nat))
    (Vec Atom how-many-peas)))")))
   ((dialogue)
    (Ld "是的, 这个类型表达了作为" (Code "peas")
        "的参数的豌豆数目依赖于其被索取的数目. "
        "这样的类型被称为" (Em "依赖类型(dependent type)") "."
        (P (Code "peas") "可以用"
           (Code "rec-Nat") "写出来吗?"))
    (Rd "当然了."
        (CodeD "(define peas
  (λ (how-many-peas)
    (rec-Nat how-many-peas
      vecnil
      (λ (" (Dim l-1) " peas" (Sub l-1) ")
        (vec:: 'pea peas" (Sub l-1) ")))))")))
   ((law)
    (Center "依赖类型")
    (P "由某个不是类型的东西所确定的类型被称为"
       (Em "依赖类型(dependent type)") "."))
   ((dialogue)
    (Ld "这" (Code "peas") "的定义并非表达式. 为了能够使用"
        (Code "rec-Nat") ", base和step的参数"
        (Code "peas" (Sub l-1)) "必须具有相同的类型. "
        "然而, 这里的" (Code "peas" (Sub l-1))
        "可以是一个" (Code "(Vec Atom 29)")
        ", 但是" (Code "vecnil") "是一个"
        (Code "(Vec Atom 0)") "."
        (P "换言之, 当类型依赖于作为target的" (Code "Nat")
           "时, " (Code "rec-Nat") "就不能使用了."))
    (Rd (Code "iter-Nat") "如何呢?"))
   ((dialogue)
    (Ld (Code "rec-Nat") "可以做任何" (Code "iter-Nat")
        "可以做的事情.")
    (Rd "有什么更强大的东西可用吗?"))
   ((dialogue)
    (Ld "那被称为" (Code "ind-Nat") ", 这是"
        (Q "induction on "(Code "Nat")) "的缩写.")
    (Rd "什么是" (Code "ind-Nat") "?"))
   ((dialogue)
    (Ld (Code "ind-Nat") "和" (Code "rec-Nat")
        "很像, 除了其允许base和step中几乎是答案的参数 "
        "(这里是" (Code "peas" (Sub l-1))
        ") 的类型包括作为target的" (Code "Nat") "."
        (P "换言之, " (Code "ind-Nat") "用于依赖类型."))
    (Rd "这里有一个被称为" (Code "how-many-peas")
        "的" (Code "Nat") "包含在类型"
        (CodeB "(Vec Atom how-many-peas)")
        "里, 它是一个依赖类型吗?"))
   ((dialogue)
    (Ld "是的, 它依赖于" (Code "Nat") " "
        (Code "how-many-peas") "."
        (P "为了与依赖类型打交道, "
           (Code "ind-Nat") "需要额外的参数: "
           "为了使用" (Code "ind-Nat")
           ", 有必要陈述base和step几乎是答案的参数的类型是"
           (Em "如何") "依赖于作为target的"
           (Code "Nat") "的."))
    (Rd "这个额外的参数长什么样呢?"))
   ((dialogue)
    (Ld "这个额外的参数, 被称为" (Em "动机(motive)")
        ", 可以是任意的"
        (CB '(→ Nat U))
        "一个" (Code "ind-Nat") "表达式的类型是动机"
        "应用于作为target的" (Code "Nat") "的结果."
        ((comment)
         "感谢Conor McBride (1973-)."))
    (Rd "所以说动机是一个函数, 其体是一个" $U:script "."))
   ((dialogue)
    (Ld "的确如此. 动机解释了" (Em "为什么")
        "target要被消去."
        (P (Code "peas") "的动机是什么?"))
    (Rd "这是个好问题."
        (P "不过, 至少其类型是清晰的."
           (CodeB "(claim mot-peas
  (→ Nat " $U:script "))"))
        ((comment)
         (Q "mot") "读作" (Q "moat") ".")))
   ((law)
    (Center "对于依赖类型应使用" (Code "ind-Nat"))
    (P "当" (Code "rec-Nat") "或者" (Code "ind-Nat")
       "表达式的类型依赖于作为target的" (Code "Nat")
       "时, 应使用" (Code "ind-Nat") "而非"
       (Code "rec-Nat") ". " (Code "ind-Nat")
       "表达式的类型是动机 (motive) 应用于target的结果."))
   ((dialogue)
    (Ld "以下就是" (Code "mot-peas") "了."
        (CodeB "(define mot-peas
  (λ (k)
    (Vec Atom k)))")
        (C '(mot-peas zero)) "的值是什么?")
    (Rd "它应该是一个" $U:script
        ", 因而也是类型, 即"
        (CodeB "(Vec Atom zero)")))
   ((dialogue)
    (Ld (Code "peas") "的base必然具有什么类型呢?")
    (Rd "当然其类型必然为"
        (CodeB "(Vec Atom zero)")
        "因为base的值是当" (Code "zero")
        "为target时的值."))
   ((dialogue)
    (Ld (Code "peas") "的base应该是什么呢?")
    (Rd "其必然是" (Code "vecnil")
        ", 因为" (Code "vecnil") "是仅有的"
        (CodeB "(Vec Atom zero)")))
   ((dialogue)
    (Ld "这个(类型)也是" (Code "(mot-peas zero)") "."
        (P (Code "rec-Nat") "中的step的目的是什么?"))
    (Rd "在" (Code "rec-Nat") "里, step的参数是"
        (Code "n-1") "和几乎是答案的东西, "
        "其为消去" (Code "n-1") "得到的值."
        (P "给定" (Code "n-1") "和几乎是答案的参数, "
           "step确定了" (Code "(add1 n-1)") "时的值.")))
   ((dialogue)
    (Ld (Code "ind-Nat") "里的step的参数也是"
        (Code "n-1") "和几乎是答案的东西."
        (P "那么, 几乎是答案的东西的类型是什么?"))
    (Rd "几乎是答案的东西的类型是动机应用于"
        (Code "n-1") "的结果, 因为几乎是答案的东西是"
        "target为" (Code "n-1") "时的值."))
   ((dialogue)
    (Ld "对于target " (Code "(add1 n-1)")
        "而言, 值的类型是什么?")
    (Rd "一个" (Code "ind-Nat") "表达式的类型"
        "动机应用于target的结果."))
   ((dialogue)
    (Ld "如果动机是" $mot ", 那么step的类型为"
        (CodeB "(Π ((n-1 Nat))
  (→ (" $mot " n-1)
    (" $mot " (add1 n-1))))"))
    (Rd "举一个" (Code "ind-Nat")
        "的step的例子呢?"))
   ((dialogue)
    (Ld "以下是" (Code "peas") "的step."
        (CodeB "(claim step-peas
  (Π ((" l-1 " Nat))
    (→ (mot-peas " l-1 ")
      (mot-peas (add1 " l-1 ")))))
(define step-peas
  (λ (" (Dim l-1) ")
    (λ (peas" (Sub l-1) ")
      (vec:: 'pea peas" (Sub l-1) "))))"))
    (Rd "为什么" (Code "mot-peas")
        "在" (Code "step-peas")
        "的类型里出现了两次?"))
   ((dialogue)
    (Ld "好问题."
        (P (Code "(mot-peas " l-1 ")")
           "的值是什么?"))
    (Rd "是" (C '(Vec Atom l-1)) "."))
   ((law)
    (Center (Code "ind-Nat") "之律")
    (P "如果" $target "是一个" (Code "Nat")
       ", " $mot "是一个"
       (CB '(→ Nat U))
       $base "是一个" (Code "(" $mot " zero)")
       ", 而" $step "是一个"
       (CodeB "(Π ((n-1 Nat))
  (→ (" $mot " n-1)
    (" $mot " (add1 n-1))))")
       "那么"
       (CodeB "(ind-Nat " $target "
  " $mot "
  " $base "
  " $step ")")
       "是一个" (Code "(" $mot " " $target ")") ".")
    ((tcomment)
     "这里的变量" (Code "n-1")
     "其实可以是任意的, 并且从理论上来说" $mot
     "里不应该存在" (Code "n-1") "的自由出现."))
   ((law)
    (Center (Code "ind-Nat") "之第一诫")
    (P (Code "ind-Nat") "表达式"
       (CodeB "(ind-Nat zero
  " $mot "
  " $base "
  " $step ")")
       "和" $base "是相同的"
       (Code "(" $mot " zero)") "."))
   ((law)
    (Center (Code "ind-Nat") "之第二诫")
    (P (Code "ind-Nat") "表达式"
       (CodeB "(ind-Nat (add1 " $n ")
  " $mot "
  " $base "
  " $step ")")
       "和"
       (CodeB "(" $step " " $n "
  (ind-Nat " $n "
    " $mot "
    " $base "
    " $step "))")
       "是相同的" (Code "(" $mot " (add1 " $n "))") "."))
   ((dialogue)
    (Ld "这是" (Code "peas" (Sub l-1))
        "的类型, 其描述了包含" l-1 "个豌豆的列表."
        (P (CB '(mot-peas (add1 l-1)))
           "的值如何, 其又意味着什么?"))
    (Rd "其是"
        (CB '(Vec Atom (add1 l-1)))
        "其描述了包含"
        (CB '(add1 l-1))
        "个豌豆的列表."))
   ((law)
    (Center "自然数上的归纳")
    (P "通过给出零时的值以及将" $n
       "时的值转换为" (&+ $n $1)
       "时的值的方法来构造对于任意自然数的值被称为"
       (Em "自然数上的归纳") "."))
   ((dialogue)
    (Ld "step必须要能够根据对于" (Code l-1)
        "的值构造出对于" (C '(add1 l-1)) "的值."
        (P "再次观察" (Code "step-peas")
           "的类型, 其在文中到底为何意?"))
    (Rd "不论" (Code l-1) "是什么" (Code "Nat")
        ", " (Code "step-peas") "总是接受一个"
        (CB '(Vec Atom l-1))
        "然后产生一个"
        (CB '(Vec Atom (add1 l-1)))
        "这是通过" (Q "cons") "一个"
        (Code "'pea") "到前端完成的."))
   ((dialogue)
    (Ld "base将"
        (CodeB "zero")
        "替换以"
        (CodeB "vecnil")
        "因为"
        (CodeB "vecnil")
        "是仅有的"
        (CodeB "(Vec Atom zero)")
        (Code "step-peas") "将一个"
        (Code "add1") "替换成什么呢?")
    (Rd (Code "step-peas") "将每个"
        (Code "add1") "替换以一个"
        (Code "vec::") ", 就像"
        (Ref "step-length-def")
        "中的" (Code "length")
        "将一个列表中的每个" (Code "::")
        "替换以" (Code "add1") "."))
   ((dialogue)
    (Ld "现在定义" (Code "peas")
        "是可能的了, 只需使用"
        (Code "mot-peas") "和"
        (Code "step-peas") ".")
    (Rd "以下是我们的定义."
        (CodeB "(define peas
  (λ (how-many-peas)
    (ind-Nat how-many-peas
      mot-peas
      vecnil
      step-peas)))")))
   ((dialogue)
    (Ld (Code "(peas 2)") "的值是什么?"
        (P "以下是最初的三个计算步骤."
           (same-as
            (CodeI "(peas
  (add1
    (add1 zero)))")
            (CodeI "(ind-Nat (add1
           (add1 zero))
  mot-peas
  vecnil
  step-peas)")
            (CodeI "(step-peas (add1 zero)
  (ind-Nat (add1 zero)
    mot-peas
    vecnil
    step-peas))"))
           "现在, 请找出其值. "
           "记得参数(有时)无需被求值.")
        ((tcomment)
         "不知道为什么, 原文说是两个步骤."))
    (Rd "以下就是了."
        (same-as
         #:attr* '((start "4"))
         (CodeI "(vec:: 'pea
  (ind-Nat (add1 zero)
    mot-peas
    vecnil
    step-peas))"))
        "而且, 我们最终可以找出其规范形式."
        (same-as
         #:attr* '((start "5"))
         (CodeI "(vec:: 'pea
  (step-peas zero
    (ind-Nat zero
      mot-peas
      vecnil
      step-peas)))")
         (CodeI "(vec:: 'pea
  (vec:: 'pea
    (ind-Nat zero
      mot-peas
      vecnil
      step-peas)))")
         (CodeI "(vec:: 'pea
  (vec:: 'pea vecnil))"))
        "这就是规范形式了."))
   ((dialogue)
    (Ld "如果动机的参数是黯淡的, 那么说明"
        (Code "ind-Nat") "表现得就像是"
        (Code "rec-Nat")
        ". 现在请定义一个函数"
        (Code "also-rec-Nat")
        ", 其使用" (Code "ind-Nat")
        ", 而行为正如" (Code "rec-Nat") "."
        (CodeB "(claim also-rec-Nat
  (Π ((X U))
    (→ Nat
       X
       (→ Nat X X)
      X)))"))
    (Rd "因为类型并不依赖于target, 所以"
        (Code (Dim "k")) "是黯淡的."
        (CodeB "(define also-rec-Nat
  (λ (X)
    (λ (target base step)
      (ind-Nat target
        (λ (" (Dim "k") ") X)
        base
        step))))")))
   ((dialogue #:id "last-claim")
    (Ld "就像" (Code "first") "可以找出一个列表的第一个元素, "
        (Code "last") "可以找出最后一个元素."
        (P (Code "last") "的类型应该是什么?"))
    (Rd "此列表必然是非空的, 这意味着我们可以应用和"
        (Code "first") "的类型相同的想法."
        (CodeB "(claim last
  (Π ((E " $U:script ")
      (" $l:script " Nat))
    (→ (Vec E (add1 " $l:script "))
      E)))")))
   ((dialogue)
    (Ld "如果一个列表只包含一个" (Code "Atom")
        ", 那么哪个" (Code "Atom") "是最后一个呢?")
    (Rd "显然只有一种可能."))
   ((dialogue)
    (Ld (CodeB "(last Atom zero
  (vec:: 'flour vecnil))")
        "的规范形式是什么?")
    (Rd "以下是我的猜测. 这个问题没有意义, "
        "因为列表包含的是一个元素而不是零个元素."))
   ((dialogue)
    (Ld (Code "(last Atom zero)")
        "的类型是什么?"
        (P "请记得Currying."))
    (Rd (Code "(last Atom zero)") "的类型为"
        (CodeB "(→ (Vec Atom (add1 zero))
  Atom)")
        "因此, 前一个框中的问题, 实际上是有意义的!"))
   ((dialogue)
    (Ld (CodeB "(last Atom zero
  (vec:: 'flour vecnil))")
        "的规范形式是什么?")
    (Rd "那必然是" (Code "'flour") "."))
   ((dialogue)
    (Ld "的确如此."
        (P "使用这个洞察, " (Code "base-last")
           "的类型是什么?"))
    (Rd "base在(作为target的)" (Code "Nat") "为"
        (Code "zero") "时使用."
        (CodeB "(claim base-last
  (Π ((E " $U:script "))
    (→ (Vec E (add1 zero))
      E)))")))
   ((dialogue)
    (Ld (Code "base-last") "的定义是什么?")
    (Rd "其使用" (Code "head") "以获得一个"
        (Code "(Vec E (add1 zero))")
        "中的唯一元素."
        (CodeB "(define base-last
  (λ (" (Dim "E") ")
    (λ (es)
      (head es))))")
        ((tcomment)
         "原文是" (Code "(Vec Atom (add1 zero))")
         ", 这可能是一个笔误.")))
   ((dialogue)
    (Ld "这是我们第一次遇到base是一个函数的情况. "
        "根据动机, base和step的几乎是答案的参数都是函数."
        (P "当base是一个函数而step将一个几乎是答案的函数"
           "转换为另一个函数时, 整个" (Code "ind-Nat")
           "表达式当然也是在构造一个函数.")
        ((tcomment)
         "虽然实际的动机还没有给出, 但是读者应该料想得到."))
    (Rd (Code "λ") "表达式是值吗?"))
   ((dialogue)
    (Ld "是的, 因为" (Code "λ") "是一个构造子.")
    (Rd "函数的确是值."))
   ((dialogue)
    (Ld (Code "ind-Nat") "表达式的类型是"
        "动机应用于target的结果, 这个target是要被消去的"
        (Code "Nat") "."
        (P "当抵达base时, 作为target的" (Code "Nat")
           "是什么?"))
    (Rd "应该是" (Code "zero") ", 这就是base的意义所在."))
   ((dialogue)
    (Ld "动机应用于" (Code "zero") "的结果是base的类型."
        (P "请找出一个可以用作动机的表达式."))
    (Rd (CodeB "(Π ((" $E " " $U:script ")
    (" $k " Nat))
  (→ (Vec " $E " (add1 " $k "))
    " $E "))")
        "怎么样呢? 将" $E "填上列表元素的类型而"
        $k "填上" (Code "zero") "就得到了base的类型."
        ((tcomment)
         "这里使用的元变量" $E "和" $k "实际上是不必要的, "
         "或者说可以算是一种误用.")))
   ((law)
    (Center (Code "ind-Nat") "的base的类型")
    (P "在" (Code "ind-Nat") "之中, base的类型是"
       "动机应用于作为target的" (Code "zero") "的结果."))
   ((dialogue)
    (Ld "很接近了, 但并不那么正确."
        (P (Code "ind-Nat") "的动机会被应用于" (Code "zero")
           ", 但是应用一个" (Code "Π") "表达式并无意义. "
           (Code "ind-Nat") "的动机应该是一个函数, "
           "而非函数的类型."))
    (Rd "啊, 所以说那必然是"
        (CodeB "(λ (E k)
  (→ (Vec E (add1 k))
    E))")
        "其可以被应用于列表元素的类型和" (Code "zero")
        "以得到base的类型."))
   ((dialogue)
    (Ld "现在定义" (Code "last") "的动机."
        (CodeB "(claim mot-last
  (→ " $U:script " Nat
    " $U:script "))"))
    (Rd "以下就是了."
        (CodeB "(define mot-last
  (λ (E k)
    (→ (Vec E (add1 k))
      E)))")))
   ((dialogue)
    (Ld (CodeB "(mot-last Atom)")
        "的类型和值分别是什么?")
    (Rd "类型是"
        (CB '(→ Nat U))
        "而值为"
        (CodeB "(λ (k)
  (→ (Vec Atom (add1 k))
    Atom))")))
   ((dialogue)
    (Ld "这就像什么?")
    (Rd (Ref "twin-Atom") "里的" (Code "twin-Atom")
        ". 应用" (Code "mot-last") "于一个" $U:script
        "将产生一个适合用于" (Code "ind-Nat") "的动机."))
   ((dialogue)
    (Ld "此时base的类型的值是什么? 这个类型即"
        (Code "(mot-last Atom zero)"))
    (Rd "应该是类型"
        (CodeB "(→ (Vec Atom (add1 zero))
  Atom)")))
   ((dialogue)
    (Ld (CB '(mot-last Atom (add1 l-1)))
        "的值是什么?")
    (Rd "应该是"
        (CodeB "(→ (Vec Atom (add1
               (add1 " l-1 ")))
  Atom)")))
   ((dialogue)
    (Ld (Code "last") "的step的目的何在?")
    (Rd (Code "last") "的step将" (Code l-1)
        "时的几乎答案转换为对于" (C '(add1 l-1))
        "的答案."
        (P "换言之, " (Code "last")
           "的step将一个获取一个"
           (CB '(Vec E (add1 l-1)) #:constant* '(E))
           "中的最后一个元素的函数变为一个获取一个"
           (CB '(Vec E (add1 (add1 l-1)))
               #:constant* '(E))
           "中的最后一个元素的函数. "
           "为什么这里有两个" (Code "add1") "?")))
   ((dialogue)
    (Ld "外层的" (Code "add1")
        "作为类型的一部分是为了保证送给" (Code "last")
        "的列表至少拥有一个元素. 内层的" (Code "add1")
        "来源于将" (C '(add1 l-1)) "传递给"
        (Code "mot-last") ".")
    (Rd "外层的" (Code "add1") "使得函数完全, 而内层的"
        (Code "add1") "是出于" (Code "ind-Nat") "之律."))
   ((dialogue)
    (Ld "step的类型是什么?")
    (Rd "step的类型必然是"
        (CodeB "(→ (→ (Vec E (add1 " l-1 "))
     E)
  (→ (Vec E (add1
              (add1 " l-1 ")))
    E))")
        "因为step必须要根据一个"
        (CB '(mot-last E l-1)
            #:constant* '(E))
        "构造出一个"
        (CB '(mot-last E (add1 l-1))
            #:constant* '(E))))
   ((dialogue)
    (Ld "这个类型如何以文字解释?")
    (Rd "step将一个对于"
        (CB '(add1 l-1))
        "而言的"
        (CodeB "last")
        "函数转换为一个对于"
        (CB '(add1 (add1 l-1)))
        "而言的"
        (CodeB "last")
        "函数."
        ((tcomment)
         "原文分别是" (Code $l:script)
         "和" (Code "(add1 " $l:script ")")
         ", 但是译者自作主张改成了以上的形式.")))
   ((law)
    (Center (Code "ind-Nat") "的step的类型")
    (P "在" (Code "ind-Nat") "之中, step必须要接受两个参数: 某个类型为"
       (Code "Nat") "的" (Code "n") "和一个几乎是答案的东西, "
       "其类型是动机" $mot "应用于" (Code "n")
       "的结果. step返回的答案的类型是动机应用于"
       (Code "(add1 n)") "的结果. step的类型是:"
       (CodeB "(Π ((n Nat))
  (→ (" $mot " n)
    (" $mot " (add1 n))))")))
   ((dialogue #:id "step-last")
    (Ld "以下是" (Code "step-last") "的声明."
        (CodeB "(claim step-last
  (Π ((E U)
      (" l-1 " Nat))
    (→ (mot-last E " l-1 ")
      (mot-last E (add1 " l-1 ")))))")
        "现在请定义" (Code "step-last") ".")
    (Rd (Code "last" (Sub l-1))
        "是几乎正确的函数, 但是只是对于拥有"
        (C '(add1 l-1)) "个元素的列表而言的, "
        "因而其接受拥有" (C '(add1 (add1 l-1)))
        "个元素的列表的" (Code "tail")
        "作为参数."
        (CodeB "(define step-last
  (λ (" (Dim "E") " " (Dim l-1) ")
    (λ (last" (Sub l-1) ")
      (λ (es)
        (last" (Sub l-1) " (tail es))))))")
        ((tcomment)
         "原文有误, 已修正.")))
   ((dialogue)
    (Ld "内层的" (Code "λ") "表达式的参数"
        (Code "es") "的类型是什么?")
    (Rd (Code "es") "是一个"
        (CB '(Vec E (add1 (add1 l-1)))
            #:constant* '(E))))
   ((dialogue)
    (Ld "为什么这是" (Code "es") "的类型呢?")
    (Rd "整个这内层的" (Code "λ")
        "表达式的类型为"
        (CB '(mot-last E (add1 l-1))
            #:constant* '(E))
        "而这个类型和"
        (CodeB "(→ (Vec E (add1
            (add1 " l-1 ")))
  E)")
        "是相同的类型. 因此, 该" (Code "λ")
        "表达式的参数, 即" (Code "es")
        ", 应该是一个"
        (CB '(Vec E (add1 (add1 l-1)))
            #:constant* '(E))))
   ((dialogue)
    (Ld "聪明."
        (P (Code "(tail es)")
           "的类型是什么?"))
    (Rd (Code "(tail es)") "的类型为"
        (CB '(Vec E (add1 l-1))
            #:constant* '(E))
        "其是几乎准备好的函数的适切参数的类型."
        ((tcomment)
         "这个函数即" (Code "last" (Sub l-1)) ".")))
   ((dialogue)
    (Ld (Ref "step-last") "中的较外层" (Code "λ")
        "表达式里的" (Code "last" (Sub l-1))
        "的类型是什么?")
    (Rd (Code "last" (Sub l-1)) "的类型为"
        (CodeB "(→ (Vec E (add1 " l-1 "))
  E)")
        "即" (C '(mot-last E l-1) #:constant* '(E))
        "之值."))
   ((dialogue)
    (Ld "现在是时候定义" (Code "last") "了, 其"
        (Code "claim") "出现在"
        (Ref "last-claim") "之中.")
    (Rd "以下就是了."
        (CodeB "(define last
  (λ (E " $l:script ")
    (ind-Nat " $l:script "
      (mot-last E)
      (base-last E)
      (step-last E))))")))
   ((dialogue)
    (Ld (CodeB "(last Atom 1
  (vec:: 'carrot
    (vec:: 'celery vecnil)))")
        "的规范形式是什么?"
        (P "以下是我们计算的开始."
           (same-as
            (CodeI "(last Atom (add1 zero)
  (vec:: 'carrot
    (vec:: 'celery vecnil)))")
            (CodeI "((ind-Nat (add1 zero)
   (mot-last Atom)
   (base-last Atom)
   (step-last Atom))
  (vec:: 'carrot
    (vec:: 'celery vecnil)))")
            (CodeI "((step-last Atom zero
   (ind-Nat zero
     (mot-last Atom)
     (base-last Atom)
     (step-last Atom)))
  (vec:: 'carrot
    (vec:: 'celery vecnil)))"))))
    (Rd "感谢帮助. 以下是更多的计算."
        (same-as
         #:attr* '((start "4"))
         (CodeI "((λ (es)
   ((ind-Nat zero
      (mot-last Atom)
      (base-last Atom)
      (step-last Atom))
     (tail es)))
  (vec:: 'carrot
    (vec:: 'celery vecnil)))")
         (CodeI "((ind-Nat zero
   (mot-last Atom)
   (base-last Atom)
   (step-last Atom))
  (tail
    (vec:: 'carrot
      (vec:: 'celery
        vecnil))))")
         (CodeI "(base-last Atom
  (tail
    (vec:: 'carrot
      (vec:: 'celery
        vecnil))))"))
        ((tcomment)
         "原文漏了一个编号.")))
   ((dialogue)
    (Ld "这是规范形式吗?")
    (Rd "并非如此, 还需要更多的步骤."
        (same-as
         #:attr* '((start "7"))
         (CodeI "((λ (es)
   (head es))
  (tail
    (vec:: 'carrot
      (vec:: 'celery
        vecnil))))")
         (CodeI "(head
  (tail
    (vec:: 'carrot
      (vec:: 'celery
        vecnil))))")
         (CodeI "(head
  (vec:: 'celery vecnil))")
         (CodeI "'celery"))))
   ((dialogue)
    (Ld "漂亮."
        (P "现在休息一下, 或许可以来点养生的蘑菇派."))
    (Rd "听起来很不错的样子."))
   ((dialogue)
    (Ld "猜猜" (Code "drop-last") "的意思是什么.")
    (Rd "想必是丢掉一个" (Code "Vec")
        "里的最后一个元素."))
   ((dialogue)
    (Ld "猜得不错!"
        (P (Code "(drop-last Atom 3 vecnil)")
           "是什么?"))
    (Rd "它不由某个类型描述, 正如"
        (CodeB "(first Atom 3 vecnil)")
        (CodeB "(last Atom 3 vecnil)")
        (CodeB "(rest Atom 3 vecnil)")
        "也不由类型描述一样."
        (P "这个类型必然包含一个其中带有"
           (Code "add1") "的" (Code "Vec") ".")
        ((tcomment)
         (Q "这个") "意即" (Q (Code "drop-last") "的") ".")))
   ((dialogue #:id "drop-last-claim")
    (Ld "这是坚实的思考方式."
        (P (Code "drop-last") "的类型是什么?"))
    (Rd (Code "drop-last") "使得列表的长度收缩了一."
        (CodeB "(claim drop-last
  (Π ((E " $U:script ")
      (" $l:script " Nat))
    (→ (Vec E (add1 " $l:script "))
      (Vec E " $l:script "))))")))
   ((dialogue #:id "base-drop-last")
    (Ld (Code "base-drop-last") "是什么?")
    (Rd "base应该找出拥有一个元素的列表的"
        (CodeB "drop-last")
        "也就是"
        (CodeB "vecnil")
        "因为最后一个元素就是那唯一的元素."
        (CodeB "(claim base-drop-last
  (Π ((E " $U:script "))
    (→ (Vec E (add1 zero))
      (Vec E zero))))
(define base-drop-last
  (λ (" (Dim "E") ")
    (λ (" (Dim "es") ")
      vecnil)))")))
   ((dialogue)
    (Ld "以下对于" (Code "base-drop-last")
        "的定义也能成立吗?"
        (CodeD "(define base-drop-last
  (λ (" (Dim "E") ")
    (λ (es)
      (tail es))))"))
    (Rd "这个定义总是能产生相同的值, "
        "但是没能同样清晰地传达想法."
        (P "我们的意图在于" (Code "base-drop-last")
           "总是忽略列表的最后一个元素.")))
   ((dialogue)
    (Ld "听起来很对."
        (P "不过为什么上面的定义要用虚线框住呢?"))
    (Rd "只是得到正确的答案是没有价值的, 如果我们不"
        (Em "知道") "为什么正确的话. "
        "理解答案至少和拥有正确答案同等重要."))
   ((law)
    (Center "可读的表达式")
    (P "只是得到正确的答案是没有价值的, 如果我们不"
       (Em "知道") "为什么正确的话. "
       "理解答案至少和拥有正确答案同等重要."))
   ((dialogue)
    (Ld "看来某人一直在认真听讲!"
        (P (Code "mot-drop-last") "是什么?"))
    (Rd (Code "mot-drop-last")
        "需要表达" (Code "drop-last")
        "总是在构造长度小一的" (Code "Vec") "."
        (CodeB "(claim mot-drop-last
  (→ " $U:script " Nat
    " $U:script "))
(define mot-drop-last
  (λ (E k)
    (→ (Vec E (add1 k))
      (Vec E k))))")))
   ((dialogue)
    (Ld "太快了, 请解释一下.")
    (Rd "在" (Code "ind-Nat") "之中, 应用动机于"
        (Code "zero") "得到的就是base的类型. "
        "这意味着我们可以反过来将"
        (Ref "base-drop-last")
        "中base的类型里的"
        (Code "zero") "替换成参数" (Code "k")
        "以" (Code "mot-drop-last") "."
        ((tcomment)
         (Code "mot-drop-last")
         "这里是一个动词.")))
   ((dialogue)
    (Ld "这是一个敏锐的观察. "
        "这种方法并不总是成立, "
        "但的确是一个好的起点."
        (P "将特定的常量替换以变量并用该变量的"
           (Code "λ") "包裹的行为被称为"
           (Em "抽象出常量(abstracting over constants)")
           ", 并且我们经常使用这种方法. "
           "这里, 动机对于" (Code "base-drop-last")
           "里的" (Code "zero") "进行抽象."))
    (Rd (Code "step-drop-last")
        "的类型遵循" (Code "ind-Nat") "之律."
        (CodeB "(claim step-drop-last
  (Π ((E " $U:script ")
      (" l-1 " Nat))
    (→ (mot-drop-last E " l-1 ")
      (mot-drop-last E (add1 " l-1 ")))))")))
   ((dialogue #:id "step-drop-last")
    (Ld "那么" (Code "step-drop-last")
        "该如何定义?")
    (Rd "想出" (Code "step-drop-last")
        "需要动动脑子."
        (CodeB "(define step-drop-last
  (λ (" (Dim "E") " " (Dim l-1) ")
    (λ (drop-last" (Sub l-1) ")
      (λ (es)
        (vec:: (head es)
          (drop-last" (Sub l-1) "
            (tail es)))))))")))
   ((dialogue)
    (Ld "这是令人熟悉的归纳模式:"
        (CodeB "step-drop-last")
        "将一个对于"
        (CodeB "(Vec E (add1 " l-1 "))")
        "成立的"
        (CodeB "drop-last")
        "转换为一个对于"
        (CodeB "(Vec E (add1 (add1 " l-1 ")))")
        "成立的"
        (CodeB "drop-last")
        "这个转换是如何工作的呢?")
    (Rd "正如"
        (CodeB "step-last")
        "使用其几乎是答案的参数, 即"
        (Code "last" (Sub l-1))
        ", 以找出它自己的"
        (Code "(tail es)") "的"
        (Code "last") ","
        (CodeB "step-drop-last")
        "也使用其几乎是答案的参数, 即"
        (Code "drop-last" (Sub l-1))
        ", 以找出它自己的"
        (Code "(tail es)") "的"
        (Code "drop-last") "."
        (P "根据" (Code "mot-drop-last")
           ", 由" (Code "step-drop-last")
           "产生的函数必须还要给那个列表添加一个元素. "
           "因此, " (Ref "step-drop-last")
           "里的最内层的" (Code "λ")
           "表达式使用" (Code "vec::")
           "将" (Code "es") "的" (Code "head")
           (Q "cons") "到"
           (CodeB "(drop-last" (Sub l-1)
                  " (tail es))")
           "上去.")
        ((tcomment)
         "原文有误, 已修正.")))
   ((dialogue)
    (Ld (Code "drop-last") "的" (Code "claim")
        "出现在" (Ref "drop-last-claim") "里."
        (P "现在请定义" (Code "drop-last") "."))
    (Rd "这个事情只是把我们已经得到的碎片拼接起来."
        (CodeB "(define drop-last
  (λ (E " $l:script ")
    (ind-Nat " $l:script "
      (mot-drop-last E)
      (base-drop-last E)
      (step-drop-last E))))")))
   ((dialogue)
    (Ld "是的, " (Code "drop-last") "现在已得到定义."
        (P "有时找出之后会被用到的函数是方便的. 例如, "
           (Code "(drop-last Atom 2)")
           "可以找出任意由三个" (Code "Atom")
           "构成的列表的前两个元素.")
        (P "请找出"
           (CodeB "(drop-last Atom
  (add1
    (add1 zero)))")
           "之值以阐明这是如何成立的."))
    (Rd "以下是找出其值的图表."
        (same-as
         (CodeI "(drop-last Atom
  (add1
    (add1 zero)))")
         (CodeI "(ind-Nat (add1
           (add1 zero))
  (mot-drop-last Atom)
  (base-drop-last Atom)
  (step-drop-last Atom))")
         (CodeI "(step-drop-last
  Atom (add1 zero)
  (ind-Nat (add1 zero)
    (mot-drop-last Atom)
    (base-drop-last Atom)
    (step-drop-last Atom)))")
         (CodeI "(λ (es)
  (vec:: (head es)
    ((ind-Nat (add1 zero)
       (mot-drop-last Atom)
       (base-drop-last Atom)
       (step-drop-last Atom))
      (tail es))))"))))
   ((dialogue)
    (Ld "很好&mdash;&mdash;" (Code "λ")
        "表达式的确也是值. "
        "为了找出规范形式, 则需要更多的步骤. "
        "以下则是第一步."
        (same-as
         #:attr* '((start "5"))
         (CodeI "(λ (es)
  (vec:: (head es)
    ((step-drop-last Atom zero
       (ind-Nat zero
         (mot-drop-last Atom)
         (base-drop-last Atom)
         (step-drop-last Atom)))
      (tail es))))"))
        "现在轮到你来找出规范形式了.")
    (Rd "在第6步里, " (Code "es")
        "被一致地换名为了" (Code "ys")
        ", 这是为了明确这内层的"
        (Code "λ") "表达式有着自己的变量."
        (same-as
         #:attr* '((start "6"))
         (CodeI "(λ (es)
  (vec:: (head es)
    ((λ (ys)
       (vec:: (head ys)
         ((ind-Nat zero
            (mot-drop-last Atom)
            (base-drop-last Atom)
            (step-drop-last Atom))
           (tail ys))))
      (tail es))))"))))
   ((dialogue)
    (Ld "将" (Code "es") "换名为"
        (Code "ys") "其实当然是没有必要的, "
        "因为变量名总是由包裹它的最内层" (Code "λ")
        "所绑定, 不过使得表达式更容易理解总归是一个想法."
        (P "以下是新的两步."
           (same-as
            #:attr* '((start "7"))
            (CodeI "(λ (es)
  (vec:: (head es)
    (vec:: (head (tail es))
      ((ind-Nat zero
         (mot-drop-last Atom)
         (base-drop-last Atom)
         (step-drop-last Atom))
        (tail (tail es))))))")
            (CodeI "(λ (es)
  (vec:: (head es)
    (vec:: (head (tail es))
      (base-drop-last Atom
        (tail (tail es))))))"))))
    (Rd "几乎就要到终点了."
        (same-as
         #:attr* '((start "9"))
         (CodeI "(λ (es)
  (vec:: (head es)
    (vec:: (head (tail es))
      ((λ (" (Dim "ys") ") vecnil)
        (tail (tail es))))))")
         (CodeI "(λ (es)
  (vec:: (head es)
    (vec:: (head (tail es))
      vecnil)))"))
        "规范形式显然要比最初的表达式更容易理解!"))
   ((dialogue)
    (Ld (Em "C'est magnifique!")
        " 想必你已经累了.")
    (Rd "的确如此, 而且也很饿."))
   (H2: "课间: 一次吃一块")
   ((dialogue)
    (Ld "有时该怎么写下Pie表达式并不是显而易见的.")
    (Rd "那就是空白方框的作用, 是吧?"))
   ((dialogue)
    (Ld "的确如此. 然而, 绝大多数键盘都不容易输入空白方框."
        (P "与其输入空白方框, 不如使用" (Code "TODO")
           "形式来保留表达式里之后要完成的部分."))
    (Rd "什么是" (Code "TODO") "?"))
   ((dialogue)
    (Ld (Code "TODO") "是一个表达式, "
        "它是代表某个其他表达式的占位符. 一个"
        (Code "TODO") "可以具有任意的类型, "
        "而Pie会追踪哪个" (Code "TODO")
        "应该具有哪个类型.")
    (Rd (Code "TODO") "可以怎么用呢?"))
   ((dialogue)
    (Ld "每个" (Code "TODO") "都来自于某个特定的位置. "
        "这里, 我们以框号引用它们. 在本书之外使用Pie时, "
        "则有其他合适的手段."
        (P "试着输入"
           (CodeB "(claim peas
  TODO)")
           "看看会发生什么."))
    (Rd "Pie会回之以"
        (CodeB "Frame 4:2.3: TODO: U")
        "而其所提及的" (Code "TODO")
        "的确是出现在第4框的表达式的第2行第3列位置上的一个"
        $U:script "."))
   ((dialogue)
    (Ld "现在试试"
        (CodeB "(claim peas
  (Pi ((n Nat))
    TODO))")
        "其更接近于第" (Ref "ch7") "章里的"
        (Code "peas") "的类型.")
    (Rd "Pie会回之以"
        (CodeB "Frame 5:3.5: TODO:
 n : Nat
--------------
 U")
        "但是这个水平线是什么意思呢?"))
   ((dialogue)
    (Ld "当Pie回之以" (Code "TODO")
        "所被期望具有的类型时, "
        "它也会包括可以在" (Code "TODO")
        "的位置上使用的变量的类型.")
    (Rd "水平线上面的" (Code "n : Nat")
        "的意思是变量" (Code "n")
        "是一个" (Code "Nat") "."))
   ((dialogue)
    (Ld "是这样的."
        (P "现在试试"
           (CodeB "(claim peas
  (Pi ((n Nat))
    (Vec Atom n)))
(define peas
  TODO)")
           "其中的" (Code "TODO")
           "出现在定义的位置."))
    (Rd "Pie会回之以"
        (CodeB "Frame 7:5.3: TODO:
 (Π ((n Nat))
  (Vec Atom n))")
        "这也就是被" (Code "claim")
        "的那个类型."))
   ((dialogue)
    (Ld "当" (Code "TODO") "被" (Code "λ")
        "包裹时, Pie会如何回应呢?"
        (CodeB "(claim peas
  (Pi ((n Nat))
    (Vec Atom n)))
(define peas
  (λ (n)
    TODO))"))
    (Rd "水平线上会有和" (Code "n") "相关的一行."))
   ((dialogue)
    (Ld "试试看看.")
    (Rd "以下是发生的事情."
        (CodeB "Frame 8:6.5: TODO:
 n : Nat
--------------
 (Vec Atom n)")))
   ((dialogue)
    (Ld "然后该怎么写呢?")
    (Rd "因为" (Code "'pea")
        "的数量依赖于" (Code "n") ", 所以需要"
        (Code "ind-Nat") "."))
   ((dialogue)
    (Ld "Pie对于以下版本的" (Code "peas")
        "会作何回应呢?"
        (CodeB "(claim peas
  (Pi ((n Nat))
    (Vec Atom n)))
(define peas
  (λ (n)
    (ind-Nat n
      (λ (k)
        (Vec Atom k))
      TODO
      TODO)))"))
    (Rd "每一个" (Code "TODO")
        "都具有" (Code "ind-Nat")
        "所期望的类型."
        (CodeB "Frame 11:9.7: TODO:
 n : Nat
--------------
 (Vec Atom 0)

Frame 11:10.7: TODO:
 n : Nat
--------------------
 (Π ((n-1 Nat))
  (→ (Vec Atom n-1)
    (Vec Atom
      (add1 n-1))))")))
   ((dialogue)
    (Ld "那么该将这里的两个" (Code "TODO")
        "替换成什么呢?")
    (Rd "第一个" (Code "TODO") "是一个"
        (Code "(Vec Atom 0)") ", 故"
        (Code "vecnil") "是合适的. 第二个"
        (Code "TODO") "应该是一个二参数的函数, 以"
        (Code "λ") "构造, 应该使用" (Code "vec::")
        "来给" (Code "n-1") "个豌豆添加一个"
        (Code "'pea") "."))
   ((dialogue)
    (Ld "很好的选择, 那么Pie对于以下版本该作何回应?"
        (CodeB "(claim peas
  (Pi ((n Nat))
    (Vec Atom n)))
(define peas
  (λ (n)
    (ind-Nat n
      (λ (k)
        (Vec Atom k))
      vecnil
      (λ (n-1 peas-of-n-1)
        (vec:: TODO TODO)))))"))
    (Rd (Code "vec::") "之律确定了每个" (Code "TODO")
        "的类型."
        (CodeB "Frame 13:11.16: TODO:
           n : Nat
         n-1 : Nat
 peas-of-n-1 : (Vec Atom n-1)
------------------------------
 Atom


Frame 13:11.21: TODO:
           n : Nat
         n-1 : Nat
 peas-of-n-1 : (Vec Atom n-1)
------------------------------
 (Vec Atom n-1)")
        ((tcomment)
         "这里是反用其律.")))
   ((dialogue)
    (Ld "现在请将最后的两个" (Code "TODO") "替换.")
    (Rd "以下即是最终的定义."
        (CodeB "(claim peas
  (Pi ((n Nat))
    (Vec Atom n)))
(define peas
  (λ (n)
    (ind-Nat n
      (λ (k)
        (Vec Atom k))
      vecnil
      (λ (n-1 peas-of-n-1)
        (vec:: 'pea
          peas-of-n-1)))))")))
   (H2 "选一个数字, 任意的数字")
   ((dialogue)
    (Ld "蘑菇派怎么样?")
    (Rd "很好吃, 就是有点胀人. "
        "这里有什么不那么胀人的吃的吗?"))
   ((dialogue)
    (Ld (CodeB "(sandwich 'hoagie)")
        "怎么样呢?")
    (Rd "那应该还是可控的."))
   ((dialogue)
    (Ld (Code "(+ 1)") "的规范形式是什么?")
    (Rd "让我们来找出它吧."))
   ((dialogue)
    (Ld "现在开始."
        (same-as
         (CodeI "(+ (add1 zero))")
         (CodeI "(λ (j)
  (iter-Nat (add1 zero)
    j
    step-+))"))
        "这已经是一个值了.")
    (Rd "规范形式需要一点更多的工作."
        (same-as
         #:attr* '((start "3"))
         (CodeI "(λ (j)
  (step-+
    (iter-Nat zero
      j
      step-+)))")
         (CodeI "(λ (j)
  (add1
    (iter-Nat zero
      j
      step-+)))")
         (CodeI "(λ (j)
  (add1 j))"))))
   ((dialogue)
    (Ld "以下是一个定义."
        (CodeB "(claim incr
  (→ Nat Nat))
(define incr
  (λ (n)
    (iter-Nat n
      1
      (+ 1))))")
        (Code "(incr 0)")
        "的规范形式是什么?")
    (Rd "只需三个步骤."
        (same-as
         (CodeI "(incr zero)")
         (CodeI "(iter zero
  1
  (+ 1))")
         (CodeI "1"))
        "规范形式为" (Code "1")
        ", 即" (Code "(add1 zero)") "."))
   ((dialogue)
    (Ld (Code "(incr 3)")
        "的规范形式是什么?")
    (Rd "计算该规范形式需要更多的步骤. "
        "以下是为了找出其值的最初几步."
        (same-as
         (CodeI "(iter-Nat 3
  1
  (+ 1))")
         (CodeI "(+ 1
  (iter-Nat 2
    1
    (+ 1)))")
         (CodeI "(add1
  (iter-Nat 2
    1
    (+ 1)))"))))
   ((dialogue)
    (Ld "这的确是值. 但是规范形式又是什么呢? 再走几步."
        (same-as
         #:attr* '((start "4"))
         (CodeI "(add1
  (+ 1
    (iter-Nat (add1 zero)
      1
      (+ 1))))")
         (CodeI "(add1
  (add1
    (iter-Nat (add1 zero)
      1
      (+ 1))))")))
    (Rd "规范形式是" (Code "4") "."
        (same-as
         #:attr* '((start "6"))
         (CodeI "(add1
  (add1
    (+ 1
      (iter-Nat zero
        1
        (+ 1)))))")
         (CodeI "(add1
  (add1
    (add1
      (iter-Nat zero
        1
        (+ 1)))))")
         (CodeI "(add1
  (add1
    (add1
      1)))"))))
   ((dialogue)
    (Ld (CodeB "(+ 1)")
        "和"
        (CodeB "incr")
        "之间有什么关系呢?")
    (Rd "它们总能得到相同的答案, 不论参数为何."))
   ((dialogue)
    (Ld "这意味着" (Code "(+ 1)") "和" (Code "incr")
        "是相同的"
        (CodeB "(→ Nat Nat)")
        "咯?")
    (Rd "它们是相同的, 如果它们有着相同的规范形式."
        (P (Code "(+ 1)") "的规范形式为"
           (CodeB "(λ (n)
  (add1 n))")
           "而" (Code "incr") "的规范形式为"
           (CodeB "(λ (n)
  (iter-Nat n
    1
    (λ (j)
      (add1 j))))")
           "因此它们并非相同.")))
   ((dialogue)
    (Ld "是这样的."
        (P "即便它们并非相同, "
           "它们总是能够得到相同答案的事实"
           "其实可以写成一个类型."))
    (Rd "但是相同性难道不是断言一种嘛? "
        "这也不是类型啊."))
   ((dialogue)
    (Ld "相同性的确是一种断言. "
        "但是, 通过一个新的类型构造子, "
        "类型可以表达被称为" (Em "相等性(equality)")
        "的新想法."
        (P "将"
           (Blockquote
            (Q (Code "incr") "和" (Code "(+ 1)")
               "总是能够得到相同的答案."))
           "写成一个类型是很消耗能量的. 你最好先吃这个"
           (CodeB "(sandwich 'grinder)")
           "使你能量满满."))
    (Rd "另一个三明治?"
        (P "好吧.")))
   ((dialogue #:id "equality-type")
    (Ld "一个表达式"
        (CB '(= X from to))
        "是一个类型, 如果"
        (CodeB $X)
        "是一个类型, 且"
        (CB 'from)
        "是一个" $X ", 且"
        (CB 'to)
        "是一个" $X ".")
    (Rd "这是另一种构造依赖类型的方式吗?"))
   ((law)
    (Center (Code "=") "之律")
    (P "一个表达式"
       (CB '(= X from to))
       "是一个类型, 如果" $X
       "是一个类型, " $from "是一个" $X
       ", 而" $to "也是一个" $X "."))
   ((dialogue)
    (Ld "是的, " (Code "=") "是另一种构造依赖类型的方式, 因为"
        $from "和" $to "无需是类型."
        (P "因为" $from "和" $to "是方便的名字, 一个"
           (Code "=") "表达式的相应部分被称为"
           $FROM "和" $TO "."))
    (Rd "好的."))
   ((law)
    (Center $FROM "和" $TO "应理解为名词")
    (P "因为" $from "和" $to "是方便的名字, 一个"
       (Code "=") "表达式的相应部分被称为"
       $FROM "和" $TO "."))
   ((dialogue)
    (Ld (CodeB "(= Atom 'kale 'blackberries)")
        "是一个类型吗?")
    (Rd "是的, 因为" (Code "Atom") "是一个类型, 而"
        (Code "'kale") "和" (Code "'blackberries")
        "都是" (Code "Atom") "."))
   ((dialogue)
    (Ld (CodeB "(= Nat (+ 1 1) 2)")
        "是一个类型吗?"
        ((comment)
         "感谢Alfred North Whitehead (1861-1947) 并再次感谢"
         "Bertrand Russell. 他们的三卷本"
         (Em "Principia Mathematica")
         " (分别出版于1910, 1912, 1913年) 的第379页写道, "
         (Q "根据这个命题可以推出, 当算术加法被定义时, "
            (&= (&+ $1 $1) $2)) "."))
    (Rd "是的, 因为" (Code "Nat") "是一个类型而"
        (Code "(+ 1 1)") "和" (Code "2") "都是"
        (Code "Nat") "."))
   ((dialogue)
    (Ld (CodeB "(= (car (cons Nat 'kale))
  17
  (+ 14 3))")
        "是一个类型吗?")
    (Rd "是的, 的确如此, 因为"
        (CodeB "(car (cons Nat 'kale))")
        "和" (Code "Nat") "是相同的类型, 而"
        $FROM "和" $TO "都是" (Code "Nat") "."))
   ((dialogue)
    (Ld (CodeB "(= (car (cons Nat 'kale))
  15
  (+ 14 3))")
        "是一个类型吗?")
    (Rd "是的, 的确如此. " (Ref "equality-type")
        "只是要求" $FROM "和" $TO
        "都是" (Code "Nat") ", 但无需是相同的"
        (Code "Nat") "."
        (P "但是" (Code "=") "的目的何在?")))
   ((dialogue)
    (Ld "为了理解" (Code "=")
        ", 首先需要理解看待类型的另一种视角."
        (P "类型也可以读作"
           (Em "陈述(statement)") ".")
        ((comment)
         "感谢Robert Feys (1889-1961) 和"
         "Nicolaas Govert de Bruijn (1918-2012), "
         "并再次感谢Haskell B. Curry. "
         "感谢William Alvin Howard (1926-). "
         "陈述有时也被称为"
         (Em "命题(proposition)") "."))
    (Rd (CodeB "(= Atom 'apple 'apple)")
        "怎么读作陈述呢?"))
   ((dialogue)
    (Ld "类型"
        (CodeB "(= Atom 'apple 'apple)")
        "可以这么读:"
        (Blockquote
         (Q "表达式" (Code "'apple") "和" (Code "'apple")
            "是相等的" (Code "Atom")) ".")
        (CodeB "(= Nat (+ 2 2) 4)")
        "怎么读作陈述呢?")
    (Rd (Blockquote
         (Q "二加二等于四"))
        "怎么样?"))
   ((dialogue)
    (Ld "是的, 很好.")
    (Rd (Blockquote
         (Q "三加四等于七"))
        "和"
        (Blockquote
         (Code "(+ 3 4)") "和" (Code "7")
         "是相同的" (Code "Nat"))
        "有什么区别?"))
   ((dialogue)
    (Ld "陈述"
        (Blockquote
         (Q "三加四等于七"))
        "是另一种写下类型"
        (CodeB "(= Nat (+ 3 4) 7)")
        "的方法, 其" (Em "是") "一个表达式, 但是"
        (Blockquote
         (Code "(+ 3 4)") "和" (Code "7")
         "是相同的" (Code "Nat"))
        "是一个" (Em "关于") "表达式的判断."
        (P (Ref "judgment") "描述了判断. "
           "一个判断不是一个表达式, "
           "而是一个人在思考表达式时所采取的态度."))
    (Rd "以下是一个判断:"
        (Blockquote
         (Q "三加四等于七") "是一个类型.")))
   ((dialogue)
    (Ld "你的观察十分敏锐."
        (P (Code "=") "表达式不仅是类型, 还可以读作陈述."))
    (Rd "还有其他的什么类似的吗?"))
   ((dialogue)
    (Ld "一个" (Code "Π") "表达式可以读作"
        (Q "对于每个 (for every)")
        ". 考虑以下例子:"
        (CodeB "(Π ((n Nat))
  (= Nat (+ 1 n) (add1 n)))")
        "可以读作"
        (Blockquote
         (Q "对于每个" (Code "Nat") " " $n
            ", " (C '(+ 1 n)) "等于"
            (C '(add1 n))) "."))
    (Rd "好的, 但是将类型读作陈述的目的何在?"))
   ((dialogue)
    (Ld ""
        )
    (Rd ""
        ))
   ((dialogue)
    (Ld ""
        )
    (Rd ""
        ))
   ((dialogue)
    (Ld ""
        )
    (Rd ""
        ))
   (H2 "钱数翻倍, 得到两倍")
   ((dialogue)
    (Ld ""
        )
    (Rd ""
        ))
   ((dialogue)
    (Ld ""
        )
    (Rd ""
        ))
   ))