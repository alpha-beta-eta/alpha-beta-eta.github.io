#lang racket
(provide little_typer.html)
(require SMathML)
(define (make-entry name class)
  (lambda (#:n [n ""])
    (lambda x*
      (keyword-apply
       Div '(#:attr*) `(((class ,class)))
       (B (format "~a~a." name n)) " " x*))))
(define tcomment (make-entry "译者注记" "tcomment"))
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
        ((= level 1) `(h1 ,attr* . ,html*))
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
(define (Cite #:attr* [attr* '()] id)
  `(cite ,attr* ,(Ref id)))
(define (Center . html*)
  `(div ((style "text-align: center;")) . ,html*))
(define little_typer.html
  (TnTmPrelude
   #:title "The Little Typer"
   #:css "styles.css"
   (H1 "The Little Typer")
   (P "目前的排版不论从效果还是从实现来看都相当丑陋, 但也只能将就一下了. "
      "或许只有等到博客进行大型实质性重构时, 我才会考虑这些问题.")
   (P "和其他小人书一样, 本书也包含大量文字游戏, 这有点难以传达? "
      "若有必要, 我会添加一些译注.")
   (H2: "他序")
   (H2: "自序")
   (H2 "愈是变化, 愈是不变")
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
    (Rd "不是, 因为根据" (Cite "atom-definition")
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
   ((dialogue)
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
   ((dialogue)
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
   ((dialogue)
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
   ((dialogue)
    (Ld "因为" (Code "Pair") "在其参数为实际原子时并非类型."
        (P "只有在其参数均为类型 (例如" (Code "Atom")
           ") 时, 它才是一个表达式."))
    (Rd "这是不是意味着" (Code "Pair")
        "不能和" (Code "car") "与" (Code "cdr")
        "一起使用呢?"))
   ((dialogue)
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
   ((dialogue)
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
        (CodeB "(define four
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
   ((dialogue)
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
               (Span #:attr*
                     '((style "padding: 6px 25px; border: 1.2px solid white; display: inline-block;"))
                     "");"" reflects the tricky part of html.
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
   (H2 "从心所欲, 道法自然")
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
   (H2: "课间: 一叉子Pie")
   (H2 "消去所有的自然数!")
   (H2 "小菜一碟, 简单如Pie")
   ((dialogue)
    (Ld "在[frame 2:70]里, 我们定义" (Code "Pear") "为"
        (CodeD "(claim Pear U)
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
        "正如[frame 1:22]所言, " (Code "Pair")
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
   (H2 "表, 表, 更多的表")
   
   ))