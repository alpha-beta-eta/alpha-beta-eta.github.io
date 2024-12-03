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
(define ((dialogue #:attr* [attr* '()] #:id [id #f]) left right)
  (list (build-%entry #:class "dialogue" #:id id
                      #:present dialogue-present)
        attr* left right))
(define (dialogue-present %entry attr* left right)
  (define index (%entry-index %entry))
  (define id (%entry-id %entry))
  (define numbering
    (Td #:attr* '((class "middled"))
        (Sup (number->string index))))
  (Table #:attr* (attr*-set attr* 'class "dialogue" 'id id)
         (Tr left numbering right)))
(define (Ld . html*)
  (keyword-apply
   Td '(#:attr*) '(((class "leftd")))
   html*))
(define (Rd . html*)
  (keyword-apply
   Td '(#:attr*) '(((class "rightd")))
   html*))
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
    (Rd "是的, 因为" (Code "'ratatouille")
        "也是一个原子." (Br)
        "但是, 精确地说, 什么是一个原子呢?"))
   
   (H2 "从心所欲, 道法自然")
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