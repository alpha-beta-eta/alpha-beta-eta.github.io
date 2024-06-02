#lang racket
(provide smathml0.html)
(require SMathML)
(define (!commute f g . x*)
  (&= (f (apply g x*))
      (apply g (map f x*))))
(define (char->mn c)
  (Mn (string c)))
(define (add0 l)
  (if (null? (cdr l))
      (cons #\0 l)
      l))
(define (cell ab)
  (define-values (c d)
    (apply values
           (map char->mn
                (add0 (string->list
                       (number->string (apply * ab)))))))
  (Menclose
   #:attr* '((notation "updiagonalstrike"))
   (Mtable
    #:attr* '((frame "solid"))
    (Mtr (Mtd c) (Mtd $))
    (Mtr (Mtd $) (Mtd d)))))
(define 1--9 (range 1 10))
(define (product l1 l2)
  (map (lambda (x)
         (map (lambda (y) (list x y))
              l2))
       l1))
(define (tmap f t)
  (map (lambda (l) (map f l)) t))
(define (list->table lst)
  (keyword-apply
   Mtable
   '(#:attr*)
   '(((rowspacing "0")
      (columnspacing "0")))
   (map (lambda (row)
          (apply Mtr (map Mtd row)))
        lst)))
(define (napier m n)
  (list->table
   (tmap cell (product m n))))
(define $!- (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define $.
  (Mo "." #:attr* '((lspace "0"))))
(define (Lam x t)
  (: $lambda x $. t))
(define App (&split 2))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (: (apply &rule x*) label))
(define smathml0.html
  (TnTmPrelude
   #:title "SMathML参考"
   #:css "styles.css"
   (H1. "SMathML参考")
   (P "当前SMathML是一个仍然在快速推进的项目, "
      "因而这里的参考可能是过时的, 必须以实现为准.")
   (H2. "引论")
   (P "SMathML不是一个排版引擎, "
      "目前它将所有的排版工作都委托给了浏览器. "
      "从本质上来说, 排版工作不仅可以委托给浏览器, "
      "还可以通过转换过程或者输出过程 "
      "(那么此时输出也是某种变换) "
      "委托给LaTeX等其他后端. "
      "但是, 它的模型是基于浏览器的. "
      "也就是说, 它提供的是表达HTML和MathML的手段. "
      "(另外, 它还考虑了CSS和SVG, 但是远非完善, 几乎很难使用.) "
      "即便真有人设计实现了其他后端, "
      "那也是非直接的, 并且也并非作者本人的意图.")
   (P "SMathML的目的是提供在网页上书写数学内容的成熟方式. "
      "尽管如此, 当前MathJax等其他软件/库的流行"
      "使得SMathML看起来似乎没有必要. "
      "问题在于, 当前这些流行的选择几乎"
      "都没能提供必要的抽象和组合手段. "
      "这个问题并没有引起太多的重视, "
      "因为数学排版的事实标准LaTeX"
      "在抽象和组合方面也已经是一言难尽了. "
      "(并且, 大多数软件/库提供的都是对于LaTeX的" (Em "受限")
      "模拟, 使得抽象和组合的能力被进一步削弱.) "
      "然而, 这个问题终究是真实存在的, "
      "所以才有了作者设计和实现的SMathML.")
   (P "当前SMathML是一个嵌入Racket的DSL. "
      "换言之, 使用者可以不加限制地享受Racket的完整生态系统, "
      "这与某些其他排版系统形成了鲜明的对比, 例如Typst. "
      "嵌入其他的Scheme实现或许是可能的, "
      "但是因为当前SMathML不加限制地使用"
      "Racket特有的关键词机制, "
      "所以说兼容层可能并不容易实现. "
      "不过用户也可以选择另起炉灶, "
      "修改SMathML本身的一些表达机制.")
   (P "SMathML基于SXML, SXML可以理解为在"
      "Scheme/Lisp语言中对于XML的直接列表表示. "
      "另外, 或许SXML也可以理解为一种句法, "
      "但是这种理解既没有必要, 也太过受限, "
      "所以我们并不会使用. "
      "这是因为SMathML的表达力来源于"
      "它能够对于SXML进行直接的操纵和变换. "
      "我们的SXML和Oleg Kiselyov的SXML并不完全一样, "
      "而Racket提供的SXML库和Oleg Kiselyov的设计是相同的. "
      "不同之处在于当前SXML的属性是强制存在的, "
      "如果没有就使用空表来表示, 这使其更为一致. "
      "不过, 这也使得裸表达SXML更为繁琐. "
      "不过, 通过提供众多的基本函数, "
      "这一点在实际使用SMathML中完全不是问题. "
      "SMathML也提供了类似于Oleg Kiselyov的SXSLT的变换机制, "
      "但是只提供了SXSLT的一小部分机能. "
      "至少对于当前而言, 作者认为这已经是足够了的.")
   (H2. "基本函数")
   (P "这一章我们提供了用于表达MathML和HTML的"
      "SXML形式的最为基本函数的参考. "
      "这一章没有什么令人意外的地方, "
      "读者仅需快速浏览即可完全掌握.")
   (H3. "MathML部分")
   (P (CodeB "(define (Math #:attr* [attr* '()] . mathml*)
  `(math ,attr* . ,mathml*))")
      "让我们先来看这一个过程, "
      "本章剩余的过程均和此过程的模式完全一致. "
      "它有一个关键词参数, 这个关键词参数是属性, "
      "默认为空. 另外, 我们的命名约定是, "
      "所有的MathML和HTML在SMathML中对应过程的名字"
      "是原本的元素名的首字母大写版本. "
      "这就是需要说明的全部内容了.")
   (P "以下是所有的MathML部分基本函数."
      (CodeB "(define (Math #:attr* [attr* '()] . mathml*)
  `(math ,attr* . ,mathml*))
(define (Merror #:attr* [attr* '()] . mathml*)
  `(merror ,attr* . ,mathml*))
(define (Mfrac #:attr* [attr* '()] . mathml*)
  `(mfrac ,attr* . ,mathml*))
(define (Mi #:attr* [attr* '()] . mathml*)
  `(mi ,attr* . ,mathml*))
(define (Mmultiscripts #:attr* [attr* '()] . mathml*)
  `(mmultiscripts ,attr* . ,mathml*))
(define (Mprescripts #:attr* [attr* '()] . mathml*)
  `(mprescripts ,attr* . ,mathml*))
(define (Mn #:attr* [attr* '()] . mathml*)
  `(mn ,attr* . ,mathml*))
(define (Mo #:attr* [attr* '()] . mathml*)
  `(mo ,attr* . ,mathml*))
(define (Mover #:attr* [attr* '()] . mathml*)
  `(mover ,attr* . ,mathml*))
(define (Mpadded #:attr* [attr* '()] . mathml*)
  `(mpadded ,attr* . ,mathml*))
(define (Mphantom #:attr* [attr* '()] . mathml*)
  `(mphantom ,attr* . ,mathml*))
(define (Mroot #:attr* [attr* '()] . mathml*)
  `(mroot ,attr* . ,mathml*))
(define (Mrow #:attr* [attr* '()] . mathml*)
  `(mrow ,attr* . ,mathml*))
(define (Ms #:attr* [attr* '()] . mathml*)
  `(ms ,attr* . ,mathml*))
(define (Mspace #:attr* [attr* '()] . mathml*)
  `(mspace ,attr* . ,mathml*))
(define (Msqrt #:attr* [attr* '()] . mathml*)
  `(msqrt ,attr* . ,mathml*))
(define (Mstyle #:attr* [attr* '()] . mathml*)
  `(mstyle ,attr* . ,mathml*))
(define (Msub #:attr* [attr* '()] . mathml*)
  `(msub ,attr* . ,mathml*))
(define (Msubsup #:attr* [attr* '()] . mathml*)
  `(msubsup ,attr* . ,mathml*))
(define (Msup #:attr* [attr* '()] . mathml*)
  `(msup ,attr* . ,mathml*))
(define (Mtable #:attr* [attr* '()] . mathml*)
  `(mtable ,attr* . ,mathml*))
(define (Mtd #:attr* [attr* '()] . mathml*)
  `(mtd ,attr* . ,mathml*))
(define (Mtext #:attr* [attr* '()] . mathml*)
  `(mtext ,attr* . ,mathml*))
(define (Mtr #:attr* [attr* '()] . mathml*)
  `(mtr ,attr* . ,mathml*))
(define (Munder #:attr* [attr* '()] . mathml*)
  `(munder ,attr* . ,mathml*))
(define (Munderover #:attr* [attr* '()] . mathml*)
  `(munderover ,attr* . ,mathml*))
(define (Maction #:attr* [attr* '()] . mathml*)
  `(maction ,attr* . ,mathml*))
(define (Menclose #:attr* [attr* '()] . mathml*)
  `(menclose ,attr* . ,mathml*))
(define (Mfenced #:attr* [attr* '()] . mathml*)
  `(mfenced ,attr* . ,mathml*))")
      "实际上它们也不完全是手写的, 而是生成的. "
      "然而, 从软件工程角度来看, "
      "我应该使用宏进行抽象, "
      "目前的实现并不那么令人满意. "
      "不过, 至少当前的定义足够清晰, 也易于修改.")
   (H4. "一些实用补充定义")
   (P "还有一些实用的补充定义, 值得放在这一章."
      (CodeB "(define (MathB #:attr* [attr* '()] . mathml*)
  `(math ((display &quot;block&quot;) . ,attr*) . ,mathml*))
(define M Math)
(define MB MathB)
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign &quot;left center right&quot;)
                (width &quot;100%&quot;))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))")
      "其中" (Code "MathB") "设定" (Code "display")
      "属性为" (Code "block")
      ", 而" (Code "M") "和" (Code "MB")
      "分别是" (Code "Math") "和" (Code "MathB")
      "的缩写. 注意" (Code "Math")
      "元素分别有两种渲染方式, 一种是"
      (Code "inline") ", 另一种是" (Code "block")
      ", 默认情况是" (Code "inline")
      ", 使用过LaTeX的人应该不难理解. 至于"
      (Code "MBL") ", 它在" (Code "MB")
      "的基础上为公式添加了标签.")
   (P (CodeB "(define $ (Mrow))
(define ^ Msup)
(define _ Msub)
(define _^ Msubsup)
(define __ Munder)
(define ^^ Mover)
(define __^^ Munderover)
(define ~ Mfrac)
(define % Mtext)")
      "除了" (Code "$")
      ", 其余都是一些基本元素的简短别名. "
      "至于" (Code "$")
      ", 它的功能就是占位, 别无其他.")
   (H3. "HTML部分")
   (P "HTML部分和MathML部分如出一辙."
      (CodeB "(define (A #:attr* [attr* '()] . html*)
  `(a ,attr* . ,html*))
(define (Abbr #:attr* [attr* '()] . html*)
  `(abbr ,attr* . ,html*))
(define (Address #:attr* [attr* '()] . html*)
  `(address ,attr* . ,html*))
(define (Area #:attr* [attr* '()] . html*)
  `(area ,attr* . ,html*))
(define (Article #:attr* [attr* '()] . html*)
  `(article ,attr* . ,html*))
(define (Aside #:attr* [attr* '()] . html*)
  `(aside ,attr* . ,html*))
(define (Audio #:attr* [attr* '()] . html*)
  `(audio ,attr* . ,html*))
(define (B #:attr* [attr* '()] . html*)
  `(b ,attr* . ,html*))
(define (Base #:attr* [attr* '()] . html*)
  `(base ,attr* . ,html*))
(define (Bdi #:attr* [attr* '()] . html*)
  `(bdi ,attr* . ,html*))
(define (Bdo #:attr* [attr* '()] . html*)
  `(bdo ,attr* . ,html*))
(define (Blockquote #:attr* [attr* '()] . html*)
  `(blockquote ,attr* . ,html*))
(define (Body #:attr* [attr* '()] . html*)
  `(body ,attr* . ,html*))
(define (Br #:attr* [attr* '()] . html*)
  `(br ,attr* . ,html*))
(define (Button #:attr* [attr* '()] . html*)
  `(button ,attr* . ,html*))
(define (Canvas #:attr* [attr* '()] . html*)
  `(canvas ,attr* . ,html*))
(define (Caption #:attr* [attr* '()] . html*)
  `(caption ,attr* . ,html*))
(define (Cite #:attr* [attr* '()] . html*)
  `(cite ,attr* . ,html*))
(define (Code #:attr* [attr* '()] . html*)
  `(code ,attr* . ,html*))
(define (Col #:attr* [attr* '()] . html*)
  `(col ,attr* . ,html*))
(define (Colgroup #:attr* [attr* '()] . html*)
  `(colgroup ,attr* . ,html*))
(define (Data #:attr* [attr* '()] . html*)
  `(data ,attr* . ,html*))
(define (Datalist #:attr* [attr* '()] . html*)
  `(datalist ,attr* . ,html*))
(define (Dd #:attr* [attr* '()] . html*)
  `(dd ,attr* . ,html*))
(define (Del #:attr* [attr* '()] . html*)
  `(del ,attr* . ,html*))
(define (Details #:attr* [attr* '()] . html*)
  `(details ,attr* . ,html*))
(define (Dfn #:attr* [attr* '()] . html*)
  `(dfn ,attr* . ,html*))
(define (Dialog #:attr* [attr* '()] . html*)
  `(dialog ,attr* . ,html*))
(define (Div #:attr* [attr* '()] . html*)
  `(div ,attr* . ,html*))
(define (Dl #:attr* [attr* '()] . html*)
  `(dl ,attr* . ,html*))
(define (Dt #:attr* [attr* '()] . html*)
  `(dt ,attr* . ,html*))
(define (Em #:attr* [attr* '()] . html*)
  `(em ,attr* . ,html*))
(define (Embed #:attr* [attr* '()] . html*)
  `(embed ,attr* . ,html*))
(define (Fieldset #:attr* [attr* '()] . html*)
  `(fieldset ,attr* . ,html*))
(define (Figcaption #:attr* [attr* '()] . html*)
  `(figcaption ,attr* . ,html*))
(define (Figure #:attr* [attr* '()] . html*)
  `(figure ,attr* . ,html*))
(define (Footer #:attr* [attr* '()] . html*)
  `(footer ,attr* . ,html*))
(define (Form #:attr* [attr* '()] . html*)
  `(form ,attr* . ,html*))
(define (H1 #:attr* [attr* '()] . html*)
  `(h1 ,attr* . ,html*))
(define (H2 #:attr* [attr* '()] . html*)
  `(h2 ,attr* . ,html*))
(define (H3 #:attr* [attr* '()] . html*)
  `(h3 ,attr* . ,html*))
(define (H4 #:attr* [attr* '()] . html*)
  `(h4 ,attr* . ,html*))
(define (H5 #:attr* [attr* '()] . html*)
  `(h5 ,attr* . ,html*))
(define (H6 #:attr* [attr* '()] . html*)
  `(h6 ,attr* . ,html*))
(define (Head #:attr* [attr* '()] . html*)
  `(head ,attr* . ,html*))
(define (Header #:attr* [attr* '()] . html*)
  `(header ,attr* . ,html*))
(define (Hgroup #:attr* [attr* '()] . html*)
  `(hgroup ,attr* . ,html*))
(define (Hr #:attr* [attr* '()] . html*)
  `(hr ,attr* . ,html*))
(define (Html #:attr* [attr* '()] . html*)
  `(html ,attr* . ,html*))
(define (I #:attr* [attr* '()] . html*)
  `(i ,attr* . ,html*))
(define (Iframe #:attr* [attr* '()] . html*)
  `(iframe ,attr* . ,html*))
(define (Img #:attr* [attr* '()] . html*)
  `(img ,attr* . ,html*))
(define (Input #:attr* [attr* '()] . html*)
  `(input ,attr* . ,html*))
(define (Ins #:attr* [attr* '()] . html*)
  `(ins ,attr* . ,html*))
(define (Kbd #:attr* [attr* '()] . html*)
  `(kbd ,attr* . ,html*))
(define (Label #:attr* [attr* '()] . html*)
  `(label ,attr* . ,html*))
(define (Legend #:attr* [attr* '()] . html*)
  `(legend ,attr* . ,html*))
(define (Li #:attr* [attr* '()] . html*)
  `(li ,attr* . ,html*))
(define (Link #:attr* [attr* '()] . html*)
  `(link ,attr* . ,html*))
(define (Main #:attr* [attr* '()] . html*)
  `(main ,attr* . ,html*))
(define (Map #:attr* [attr* '()] . html*)
  `(map ,attr* . ,html*))
(define (Mark #:attr* [attr* '()] . html*)
  `(mark ,attr* . ,html*))
(define (Menu #:attr* [attr* '()] . html*)
  `(menu ,attr* . ,html*))
(define (Meta #:attr* [attr* '()] . html*)
  `(meta ,attr* . ,html*))
(define (Meter #:attr* [attr* '()] . html*)
  `(meter ,attr* . ,html*))
(define (Nav #:attr* [attr* '()] . html*)
  `(nav ,attr* . ,html*))
(define (Noscript #:attr* [attr* '()] . html*)
  `(noscript ,attr* . ,html*))
(define (Object #:attr* [attr* '()] . html*)
  `(object ,attr* . ,html*))
(define (Ol #:attr* [attr* '()] . html*)
  `(ol ,attr* . ,html*))
(define (Optgroup #:attr* [attr* '()] . html*)
  `(optgroup ,attr* . ,html*))
(define (Option #:attr* [attr* '()] . html*)
  `(option ,attr* . ,html*))
(define (Output #:attr* [attr* '()] . html*)
  `(output ,attr* . ,html*))
(define (P #:attr* [attr* '()] . html*)
  `(p ,attr* . ,html*))
(define (Picture #:attr* [attr* '()] . html*)
  `(picture ,attr* . ,html*))
(define (Pre #:attr* [attr* '()] . html*)
  `(pre ,attr* . ,html*))
(define (Progress #:attr* [attr* '()] . html*)
  `(progress ,attr* . ,html*))
(define (Q #:attr* [attr* '()] . html*)
  `(q ,attr* . ,html*))
(define (Rp #:attr* [attr* '()] . html*)
  `(rp ,attr* . ,html*))
(define (Rt #:attr* [attr* '()] . html*)
  `(rt ,attr* . ,html*))
(define (Ruby #:attr* [attr* '()] . html*)
  `(ruby ,attr* . ,html*))
(define (S #:attr* [attr* '()] . html*)
  `(s ,attr* . ,html*))
(define (Samp #:attr* [attr* '()] . html*)
  `(samp ,attr* . ,html*))
(define (Script #:attr* [attr* '()] . html*)
  `(script ,attr* . ,html*))
(define (Search #:attr* [attr* '()] . html*)
  `(search ,attr* . ,html*))
(define (Section #:attr* [attr* '()] . html*)
  `(section ,attr* . ,html*))
(define (Select #:attr* [attr* '()] . html*)
  `(select ,attr* . ,html*))
(define (Slot #:attr* [attr* '()] . html*)
  `(slot ,attr* . ,html*))
(define (Small #:attr* [attr* '()] . html*)
  `(small ,attr* . ,html*))
(define (Source #:attr* [attr* '()] . html*)
  `(source ,attr* . ,html*))
(define (Span #:attr* [attr* '()] . html*)
  `(span ,attr* . ,html*))
(define (Strong #:attr* [attr* '()] . html*)
  `(strong ,attr* . ,html*))
(define (Style #:attr* [attr* '()] . html*)
  `(style ,attr* . ,html*))
(define (Sub #:attr* [attr* '()] . html*)
  `(sub ,attr* . ,html*))
(define (Summary #:attr* [attr* '()] . html*)
  `(summary ,attr* . ,html*))
(define (Sup #:attr* [attr* '()] . html*)
  `(sup ,attr* . ,html*))
(define (Table #:attr* [attr* '()] . html*)
  `(table ,attr* . ,html*))
(define (Tbody #:attr* [attr* '()] . html*)
  `(tbody ,attr* . ,html*))
(define (Td #:attr* [attr* '()] . html*)
  `(td ,attr* . ,html*))
(define (Template #:attr* [attr* '()] . html*)
  `(template ,attr* . ,html*))
(define (Textarea #:attr* [attr* '()] . html*)
  `(textarea ,attr* . ,html*))
(define (Tfoot #:attr* [attr* '()] . html*)
  `(tfoot ,attr* . ,html*))
(define (Th #:attr* [attr* '()] . html*)
  `(th ,attr* . ,html*))
(define (Thead #:attr* [attr* '()] . html*)
  `(thead ,attr* . ,html*))
(define (Time #:attr* [attr* '()] . html*)
  `(time ,attr* . ,html*))
(define (Title #:attr* [attr* '()] . html*)
  `(title ,attr* . ,html*))
(define (Tr #:attr* [attr* '()] . html*)
  `(tr ,attr* . ,html*))
(define (Track #:attr* [attr* '()] . html*)
  `(track ,attr* . ,html*))
(define (U #:attr* [attr* '()] . html*)
  `(u ,attr* . ,html*))
(define (Ul #:attr* [attr* '()] . html*)
  `(ul ,attr* . ,html*))
(define (Var #:attr* [attr* '()] . html*)
  `(var ,attr* . ,html*))
(define (Video #:attr* [attr* '()] . html*)
  `(video ,attr* . ,html*))
(define (Wbr #:attr* [attr* '()] . html*)
  `(wbr ,attr* . ,html*))"))
   (H4. "一些实用补充定义")
   (P "同样对于HTML部分, 我们也有一些实用补充定义."
      (CodeB "(define (Prelude #:title [title &quot;index&quot;] #:css [css '()] . body*)
  (Html
   (apply
    Head
    (Meta #:attr* '((charset &quot;utf-8&quot;)))
    (Title title)
    (map (lambda (stylesheet)
           (Link #:attr* `((href ,stylesheet) (rel &quot;stylesheet&quot;))))
         (if (string? css) (list css) css)))
   (apply Body body*)))")
      (Code "Prelude") "允许用户更简单地构造一个网页, "
      "它为基本情形作了简单抽象.")
   (P (CodeB "(define (CodeB . str*)
  (Pre (apply Code str*)))")
      (Code "CodeB") "用于定义代码块. "
      "或许自定义属性对其也有价值, "
      "不过我从来没有用到过.")
   (H2. "扩展基本定义")
   (P "这一章提供了许多尽管不是最为基本, "
      "但仍然相当重要的定义. "
      "所以说, 我们才将其称为"
      (Q "扩展基本定义")
      ". 若不使用这些定义, "
      "许多简单的情况会变得颇为复杂.")
   (H3. "MathML部分")
   (P "首先我们定义基本字母所对应的数学标识符."
      (CodeB "(define $a (Mi &quot;a&quot;))
(define $b (Mi &quot;b&quot;))
(define $c (Mi &quot;c&quot;))
(define $d (Mi &quot;d&quot;))
(define $e (Mi &quot;e&quot;))
(define $f (Mi &quot;f&quot;))
(define $g (Mi &quot;g&quot;))
(define $h (Mi &quot;h&quot;))
(define $i (Mi &quot;i&quot;))
(define $j (Mi &quot;j&quot;))
(define $k (Mi &quot;k&quot;))
(define $l (Mi &quot;l&quot;))
(define $m (Mi &quot;m&quot;))
(define $n (Mi &quot;n&quot;))
(define $o (Mi &quot;o&quot;))
(define $p (Mi &quot;p&quot;))
(define $q (Mi &quot;q&quot;))
(define $r (Mi &quot;r&quot;))
(define $s (Mi &quot;s&quot;))
(define $t (Mi &quot;t&quot;))
(define $u (Mi &quot;u&quot;))
(define $v (Mi &quot;v&quot;))
(define $w (Mi &quot;w&quot;))
(define $x (Mi &quot;x&quot;))
(define $y (Mi &quot;y&quot;))
(define $z (Mi &quot;z&quot;))
(define $A (Mi &quot;A&quot;))
(define $B (Mi &quot;B&quot;))
(define $C (Mi &quot;C&quot;))
(define $D (Mi &quot;D&quot;))
(define $E (Mi &quot;E&quot;))
(define $F (Mi &quot;F&quot;))
(define $G (Mi &quot;G&quot;))
(define $H (Mi &quot;H&quot;))
(define $I (Mi &quot;I&quot;))
(define $J (Mi &quot;J&quot;))
(define $K (Mi &quot;K&quot;))
(define $L (Mi &quot;L&quot;))
(define $M (Mi &quot;M&quot;))
(define $N (Mi &quot;N&quot;))
(define $O (Mi &quot;O&quot;))
(define $P (Mi &quot;P&quot;))
(define $Q (Mi &quot;Q&quot;))
(define $R (Mi &quot;R&quot;))
(define $S (Mi &quot;S&quot;))
(define $T (Mi &quot;T&quot;))
(define $U (Mi &quot;U&quot;))
(define $V (Mi &quot;V&quot;))
(define $W (Mi &quot;W&quot;))
(define $X (Mi &quot;X&quot;))
(define $Y (Mi &quot;Y&quot;))
(define $Z (Mi &quot;Z&quot;))
(define $aa (Mi &quot;&amp;aopf;&quot;))
(define $bb (Mi &quot;&amp;bopf;&quot;))
(define $cc (Mi &quot;&amp;copf;&quot;))
(define $dd (Mi &quot;&amp;dopf;&quot;))
(define $ee (Mi &quot;&amp;eopf;&quot;))
(define $ff (Mi &quot;&amp;fopf;&quot;))
(define $gg (Mi &quot;&amp;gopf;&quot;))
(define $hh (Mi &quot;&amp;hopf;&quot;))
(define $ii (Mi &quot;&amp;iopf;&quot;))
(define $jj (Mi &quot;&amp;jopf;&quot;))
(define $kk (Mi &quot;&amp;kopf;&quot;))
(define $ll (Mi &quot;&amp;lopf;&quot;))
(define $mm (Mi &quot;&amp;mopf;&quot;))
(define $nn (Mi &quot;&amp;nopf;&quot;))
(define $oo (Mi &quot;&amp;oopf;&quot;))
(define $pp (Mi &quot;&amp;popf;&quot;))
(define $qq (Mi &quot;&amp;qopf;&quot;))
(define $rr (Mi &quot;&amp;ropf;&quot;))
(define $ss (Mi &quot;&amp;sopf;&quot;))
(define $tt (Mi &quot;&amp;topf;&quot;))
(define $uu (Mi &quot;&amp;uopf;&quot;))
(define $vv (Mi &quot;&amp;vopf;&quot;))
(define $ww (Mi &quot;&amp;wopf;&quot;))
(define $xx (Mi &quot;&amp;xopf;&quot;))
(define $yy (Mi &quot;&amp;yopf;&quot;))
(define $zz (Mi &quot;&amp;zopf;&quot;))
(define $AA (Mi &quot;&amp;Aopf;&quot;))
(define $BB (Mi &quot;&amp;Bopf;&quot;))
(define $CC (Mi &quot;&amp;Copf;&quot;))
(define $DD (Mi &quot;&amp;Dopf;&quot;))
(define $EE (Mi &quot;&amp;Eopf;&quot;))
(define $FF (Mi &quot;&amp;Fopf;&quot;))
(define $GG (Mi &quot;&amp;Gopf;&quot;))
(define $HH (Mi &quot;&amp;Hopf;&quot;))
(define $II (Mi &quot;&amp;Iopf;&quot;))
(define $JJ (Mi &quot;&amp;Jopf;&quot;))
(define $KK (Mi &quot;&amp;Kopf;&quot;))
(define $LL (Mi &quot;&amp;Lopf;&quot;))
(define $MM (Mi &quot;&amp;Mopf;&quot;))
(define $NN (Mi &quot;&amp;Nopf;&quot;))
(define $OO (Mi &quot;&amp;Oopf;&quot;))
(define $PP (Mi &quot;&amp;Popf;&quot;))
(define $QQ (Mi &quot;&amp;Qopf;&quot;))
(define $RR (Mi &quot;&amp;Ropf;&quot;))
(define $SS (Mi &quot;&amp;Sopf;&quot;))
(define $TT (Mi &quot;&amp;Topf;&quot;))
(define $UU (Mi &quot;&amp;Uopf;&quot;))
(define $VV (Mi &quot;&amp;Vopf;&quot;))
(define $WW (Mi &quot;&amp;Wopf;&quot;))
(define $XX (Mi &quot;&amp;Xopf;&quot;))
(define $YY (Mi &quot;&amp;Yopf;&quot;))
(define $ZZ (Mi &quot;&amp;Zopf;&quot;))")
      "其中字母重复两次的标识符对应于其"
      (Q "黑板粗体 (blackboard bold)")
      ", 例如" (&cm $QQ $RR $CC)
      "分别是有理数集, 实数集, 复数集的常规记号.")
   (P "对于希腊字母, 我们也有完整的定义."
      (CodeB "(define $alpha (Mi &quot;&amp;alpha;&quot;))
(define $beta (Mi &quot;&amp;beta;&quot;))
(define $gamma (Mi &quot;&amp;gamma;&quot;))
(define $delta (Mi &quot;&amp;delta;&quot;))
(define $epsilon (Mi &quot;&amp;epsilon;&quot;))
(define $zeta (Mi &quot;&amp;zeta;&quot;))
(define $eta (Mi &quot;&amp;eta;&quot;))
(define $theta (Mi &quot;&amp;theta;&quot;))
(define $iota (Mi &quot;&amp;iota;&quot;))
(define $kappa (Mi &quot;&amp;kappa;&quot;))
(define $lambda (Mi &quot;&amp;lambda;&quot;))
(define $mu (Mi &quot;&amp;mu;&quot;))
(define $nu (Mi &quot;&amp;nu;&quot;))
(define $xi (Mi &quot;&amp;xi;&quot;))
(define $omicron (Mi &quot;&amp;omicron;&quot;))
(define $pi (Mi &quot;&amp;pi;&quot;))
(define $rho (Mi &quot;&amp;rho;&quot;))
(define $sigma (Mi &quot;&amp;sigma;&quot;))
(define $tau (Mi &quot;&amp;tau;&quot;))
(define $upsilon (Mi &quot;&amp;upsilon;&quot;))
(define $phi (Mi &quot;&amp;phi;&quot;))
(define $chi (Mi &quot;&amp;chi;&quot;))
(define $psi (Mi &quot;&amp;psi;&quot;))
(define $omega (Mi &quot;&amp;omega;&quot;))
(define $Alpha (Mi &quot;&amp;Alpha;&quot;))
(define $Beta (Mi &quot;&amp;Beta;&quot;))
(define $Gamma (Mi &quot;&amp;Gamma;&quot;))
(define $Delta (Mi &quot;&amp;Delta;&quot;))
(define $Epsilon (Mi &quot;&amp;Epsilon;&quot;))
(define $Zeta (Mi &quot;&amp;Zeta;&quot;))
(define $Eta (Mi &quot;&amp;Eta;&quot;))
(define $Theta (Mi &quot;&amp;Theta;&quot;))
(define $Iota (Mi &quot;&amp;Iota;&quot;))
(define $Kappa (Mi &quot;&amp;Kappa;&quot;))
(define $Lambda (Mi &quot;&amp;Lambda;&quot;))
(define $Mu (Mi &quot;&amp;Mu;&quot;))
(define $Nu (Mi &quot;&amp;Nu;&quot;))
(define $Xi (Mi &quot;&amp;Xi;&quot;))
(define $Omicron (Mi &quot;&amp;Omicron;&quot;))
(define $Pi (Mi &quot;&amp;Pi;&quot;))
(define $Rho (Mi &quot;&amp;Rho;&quot;))
(define $Sigma (Mi &quot;&amp;Sigma;&quot;))
(define $Tau (Mi &quot;&amp;Tau;&quot;))
(define $Upsilon (Mi &quot;&amp;Upsilon;&quot;))
(define $Phi (Mi &quot;&amp;Phi;&quot;))
(define $Chi (Mi &quot;&amp;Chi;&quot;))
(define $Psi (Mi &quot;&amp;Psi;&quot;))
(define $Omega (Mi &quot;&amp;Omega;&quot;))")
      "它们在数学中的使用颇为频繁.")
   (P "对于数字, 我们仅定义了" $0 "到" $9 "."
      (CodeB "(define $0 (Mn &quot;0&quot;))
(define $1 (Mn &quot;1&quot;))
(define $2 (Mn &quot;2&quot;))
(define $3 (Mn &quot;3&quot;))
(define $4 (Mn &quot;4&quot;))
(define $5 (Mn &quot;5&quot;))
(define $6 (Mn &quot;6&quot;))
(define $7 (Mn &quot;7&quot;))
(define $8 (Mn &quot;8&quot;))
(define $9 (Mn &quot;9&quot;))"))
   (H3. "HTML部分")
   (P "HTML部分也有一些扩展基本定义, "
      "但是数量并不是很多. "
      "而且, 或许很多都不应该放在该部分.")
   (P (CodeB "(define (make-table #:attr* [attr* '()] lst)
  (keyword-apply
   Table '(#:attr*) (list attr*)
   (map (lambda (row)
          (apply Tr (map Td row)))
        lst)))")
      (Code "make-table")
      "是用于定义表格的简便过程, "
      "它自动添加了" (Code "Tr")
      "和" (Code "Td")
      ", 但保留了表格的属性自定义.")
   (P (CodeB "(define (map2 proc lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (proc (car lst))))
        (else (cons (proc (car lst) (cadr lst))
                    (map2 proc (cddr lst))))))")
      (Code "map2") "类似于" (Code "map")
      ", 但每次作为参数的过程应用于列表的连续两个参数. "
      "这个定义颇为实用, 在很多场合都能用到.")
   (P (CodeB "(define (columnize . xml*)
  (make-table
   #:attr* '((width &quot;100%&quot;))
   (map2 list xml*)))")
      "这个定义实际上不那么基本, "
      "但在我的博客中经常用到, "
      "用于构造整齐排列的两列元素.")
   (P (CodeB "(define -- &quot;&amp;mdash;&amp;mdash;&quot;)")
      "这个常量定义同样也只是在我的博客中经常用到, "
      "不过我想或许读者也能经常用到.")
   (H2. "实用定义")
   (P "这一章开始, 我们将诸多实用定义. "
      "其中有些定义充当了基本的工具, "
      "用户同样也可以使用这些工具. "
      "另外, 还有一些实用定义是基于这些工具而来的.")
   (H3. "运算符和中缀运算符")
   (P "运算符是数学记号的基本成分之一, "
      "所以我们提供了诸多方便的工具, "
      "以及许多常见的运算符定义.")
   (P (CodeB "(define ((make-op op n u) . x*)
  (cond
    ((null? x*) (n))
    ((null? (cdr x*)) (u (car x*)))
    (else
     (let iter ((x (car x*)) (x* (cdr x*)) (r '()))
       (if (null? x*)
           (apply Mrow (reverse (cons x r)))
           (iter (car x*)
                 (cdr x*)
                 (cons op (cons x r))))))))
(define (err0 id)
  (lambda ()
    (error id &quot;&quot;)))
(define (err1 id)
  (lambda (x)
    (error id &quot;~s&quot; x)))")
      (Code "make-op")
      "定义一个运算符所对应的过程, "
      "它可以接受任意数目的参数. "
      "对于参数数目大于等于二的情况, "
      "它的表现就是中缀运算符. "
      "至于参数数目为零或一的情况, "
      "它允许用户单独定义. 另外, "
      "如果用户想要报错, 可以使用"
      (Code "err0") "和" (Code "err1") ".")
   (P (CodeB "(define (make-infix op id)
  (make-op op
    (err0 id)
    (err1 id)))
(define-syntax define-infix*
  (syntax-rules ()
    ((_ (id op) ...)
     (begin
       (define id (make-infix op 'id))
       ...))))")
      "另外, 我们还有" (Code "make-infix")
      "过程和" (Code "define-infix*")
      "宏, 它们可以方便地定义只适用于"
      "参数数目大于等于二情形的中缀运算符过程.")
   (P "我们通过一些例子来理解上述工具的用法."
      (CodeB "(define &amp;cm
  (make-op $cm
    (lambda () $)
    (lambda (x) x)))")
      (Code "&amp;cm")
      "的定义依赖于" (Code "$cm")
      ", 而" (Code "$cm") "的定义为"
      (CodeB "(define $cm (Mo &quot;,&quot;))")
      "也就是说, " (Code "&amp;cm")
      "是一个用逗号隔开其参数的过程. "
      "如果只有一个参数, 就返回该参数本身. "
      "如果没有参数, 就返回占位符" (Code "$")
      ". 请读者注意, 我们在一般情况下总是使用"
      (Code "$") "前缀来表示非函数, "
      "而使用" (Code "&amp;")
      "前缀来表示该非函数实体所对应的表达过程.")
   (P "再看一个例子."
      (CodeB "(define &amp;-
  (make-op $-
    (err0 '&amp;-)
    (lambda (x) (Mrow $- x))))")
      (Code "&amp;-")
      "是代表减法的过程, "
      "它在只有一个参数时是negation. "
      "由此SMathML提供了一些负数常量."
      (CodeB "(define $-1 (&amp;- $1))
(define $-2 (&amp;- $2))
(define $-3 (&amp;- $3))
(define $-4 (&amp;- $4))
(define $-5 (&amp;- $5))
(define $-6 (&amp;- $6))
(define $-7 (&amp;- $7))
(define $-8 (&amp;- $8))
(define $-9 (&amp;- $9))"))
   (H3. "大运算符")
   (P "还有一类特殊的运算符我们经常用到, "
      "或许可以称其为" (Q "大运算符") ".")
   (P (CodeB "(define (make-bigop op)
  (case-lambda
    ((u o e) (Mrow (Munderover op u o) e))
    ((u e) (Mrow (Munder op u) e))
    ((e) (Mrow op e))))
(define-syntax define-bigop*
  (syntax-rules ()
    ((_ (id op) ...)
     (begin
       (define id (make-bigop op))
       ...))))")
      (Code "make-bigop")
      "用于定义大运算符对应的过程, 而"
      (Code "define-bigop*")
      "是用于一次性定义许多大运算符过程的宏.")
   (P "大运算符过程总是可以接受一个, 两个或三个参数. "
      "如果只有一个参数, 那么大运算符就没有上下标. "
      "两个参数的话, 就只有下标. "
      "三个参数的话, 就同时拥有上下标. "
      "这与大运算符的使用习惯完全一致. "
      "不过, 读者需要注意的是大运算符的上下标"
      "渲染方式在内联模式和块模式下并不相同, "
      "这实际上也和LaTeX差不多就是了. "
      "内联模式下, 上下标会变成角标. "
      "而在块模式下, 上下标才真的出现在大运算符的上方和下方. "
      "如果想要在内联模式下使用块模式的风格, "
      "可以修改" (Code "displaystyle") "属性为"
      (Code "true") ".")
   (P (CodeB "(define-bigop*
  (sum $sum)
  (prod $prod)
  (Union $Union)
  (Cup $Union)
  (Intersect $Cap)
  (Cap $Cap)
  (Disj $Disj)
  (Conj $Conj))")
      "目前SMathML所提供的有这些大运算符过程的定义. "
      "或许其中的" (Code "sum") "和" (Code "prod")
      "应该首字母大写, 但目前已经积重难返.")
   (H3. "各种括号过程和列表枚举过程")
   (P "括号和列表式枚举是最常用到的数学表达, "
      "值得我们提供所有常见的可能性.")
   (P "不过, 先让我们看几个常量定义."
      (CodeB "(define $lp (Mo &quot;(&quot;))
(define $rp (Mo &quot;)&quot;))
(define $lp0
  (Mo &quot;(&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rp0
  (Mo &quot;)&quot; #:attr* '((stretchy &quot;false&quot;))))")
      "首先我必须要承认的是, " (Code "$lp")
      "和" (Code "$rp") "这样的命名并不直观, "
      "也不容易记忆, 不如改成" (Code "$\\(")
      "和" (Code "$\\)") ", 可是目前也已经是积重难返. "
      "不过, 还有一点值得读者注意, 那就是" (Code "stretchy")
      "属性. 这个其实是为了解决Chrome浏览器上的一个问题的, "
      "而这个问题在Firefox浏览器中并不存在. "
      "如果不设定" (Code "stretchy") "为" (Code "false")
      ", 那么常规括号总是会在Chrome的渲染中被拉伸, "
      "这并不符合用户的期望. 不过, " (Code "stretchy")
      "这个属性本身是控制运算符是否会被拉伸的, "
      "这在数学表达中用到不多, 但一旦碰到也是相当有用的. "
      "所以说, 即便我们主要只考虑Firefox上的渲染效果, "
      "提供两类括号过程仍然具有价值."
      (CodeB "(define (pare x)
  (Mrow $lp x $rp))
(define (par0 x)
  (Mrow $lp0 x $rp0))")
      "当然, 现在我对于" (Code "pare")
      "和" (Code "par0") "同样不是很满意.")
   (P "有了括号过程之后, 我们可以定义与之相伴的列表枚举过程."
      (CodeB "(define (tup . x*)
  (pare (apply &amp;cm x*)))
(define (tu0 . x*)
  (par0 (apply &amp;cm x*)))"))
   (P "以上仅用到了小括号, 对于中括号, 花括号, 角括号, "
      "竖线以及双竖线, 我们都有一些类似的定义."
      (CodeB "(define $lb (Mo &quot;[&quot;))
(define $rb (Mo &quot;]&quot;))
(define $lb0
  (Mo &quot;[&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rb0
  (Mo &quot;]&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $lc (Mo &quot;{&quot;))
(define $rc (Mo &quot;}&quot;))
(define $lc0
  (Mo &quot;{&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rc0
  (Mo &quot;}&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $lv (Mo &quot;|&quot;))
(define $rv (Mo &quot;|&quot;))
(define $lv0
  (Mo &quot;|&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rv0
  (Mo &quot;|&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $la (Mo &quot;&amp;lang;&quot;))
(define $ra (Mo &quot;&amp;rang;&quot;))
(define $la0
  (Mo &quot;&amp;lang;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $ra0
  (Mo &quot;&amp;rang;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $ld (Mo &quot;&amp;Vert;&quot;))
(define $rd (Mo &quot;&amp;Vert;&quot;))
(define $ld0
  (Mo &quot;&amp;Vert;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rd0
  (Mo &quot;&amp;Vert;&quot; #:attr* '((stretchy &quot;false&quot;))))")
      (CodeB "(define (brac x)
  (Mrow $lb x $rb))
(define (bra0 x)
  (Mrow $lb0 x $rb0))
(define (curb x)
  (Mrow $lc x $rc))
(define (cur0 x)
  (Mrow $lc0 x $rc0))
(define (vert x)
  (Mrow $lv x $rv))
(define (ver0 x)
  (Mrow $lv0 x $rv0))
(define (angb x)
  (Mrow $la x $ra))
(define (ang0 x)
  (Mrow $la0 x $ra0))
(define (dver x)
  (Mrow $ld x $rd))
(define (dve0 x)
  (Mrow $ld0 x $rd0))")
      (CodeB "(define (lis . x*)
  (brac (apply &amp;cm x*)))
(define (li0 . x*)
  (bra0 (apply &amp;cm x*)))
(define (enu . x*)
  (curb (apply &amp;cm x*)))
(define (en0 . x*)
  (cur0 (apply &amp;cm x*)))
(define (tupan . x*)
  (angb (apply &amp;cm x*)))
(define (tupa0 . x*)
  (ang0 (apply &amp;cm x*)))")
      "注意竖线和双竖线倒是没有定义对应的列表枚举过程, "
      "但是我也没有看过或者用到过这种记号.")
   (P "后来我在书写有关指称语义的内容时频繁用到双中括号, "
      "于是有了以下定义."
      (CodeB "(define $ldb (Mo &quot;&amp;LeftDoubleBracket;&quot;))
(define $rdb (Mo &quot;&amp;RightDoubleBracket;&quot;))
(define (&amp;db x)
  (Mrow $ldb x $rdb))
(define $ldb0 (Mo &quot;&amp;LeftDoubleBracket;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rdb0 (Mo &quot;&amp;RightDoubleBracket;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define (&amp;db0 x)
  (Mrow $ldb0 x $rdb0))"))
   (P "对于set-builder记号, 我们有一些定义, 放在这一节似乎比较合适."
      (CodeB "(define setE en0)
(define (setI a b)
  (cur0 (Mrow a $lv0 b)))
(define (setEnum a b)
  (setE a $..h b))")
      "其中" (Code "E") "代表外延, 而"
      (Code "I") "代表内涵. 或许"
      (Code "setI") "应该接受不定数目的参数, "
      "但暂时作者还未进行修改.")
   (H3. "括号化")
   (P "经常我们需要给表达式加上括号, "
      "所以SMathML提供了一个括号化过程, "
      "其将一个普通的数学表达过程变成加上括号的版本."
      (CodeB "(define (@ . x*)
  (par0 (apply : x*)))
(define (@lize &amp;) (compose @ &amp;))
(define-syntax define-@lized-op*
  (syntax-rules ()
    ((_ (id op) ...)
     (begin
       (define id (@lize op))
       ...))))")
      "其中用到的" (Code ":") "类似于"
      (Code "Mrow") ", 定义如下:"
      (CodeB "(define (: . x*)
  (cond
    ((null? x*) $)
    ((null? (cdr x*)) (car x*))
    (else (apply Mrow x*))))")
      (Code "@lize") "用于定义一个这样的过程, "
      (Code "define-@lized-op*")
      "用于一次性定义许多这样的过程.")
   (P (CodeB "(define-@lized-op*
  (@+ &amp;+)
  (@- &amp;-)
  (@i* &amp;i*)
  (@c* &amp;c*)
  (@t* &amp;t*)
  (@w* &amp;w*)
  (@compose &amp;compose)
  (@conj &amp;conj)
  (@disj &amp;disj)
  (@sqcup &amp;sqcup)
  (@sqcap &amp;sqcap)
  (@: &amp;:)
  (@neg &amp;neg)
  (@=> &amp;=>)
  (@= &amp;=)
  (@ap ap))")
      "暂时我们只定义了这些括号化版本过程, "
      "或许还有很多应该定义. 不过, "
      "读者应该注意到这里的命名约定, "
      "也就是使用" (Code "@")
      "前缀代表括号化版本.")
   (H3. "函数应用")
   (P "标准的前缀式函数应用是数学中最频繁使用的记号之一. "
      "所以说, 我们提供了以下过程供用户使用."
      (CodeB "(define $af (Mo &quot;&amp;af;&quot;))
(define (ap f x)
  (Mrow f $af x))
(define (app f . x*)
  (ap f (apply @ x*)))
(define (appl f . x*)
  (ap f (apply tu0 x*)))")
      "添加" (Code "$af") "是官方文档的做法, "
      "我没有选择违背这一标准. "
      (Code "ap") "用于直接的应用, "
      "数学中运用不多但偶尔出现. "
      (Code "app") "用于给参数加括号的应用, "
      "这在数学中最为常见. "
      (Code "appl") "用于多参数函数应用, "
      "这用到了" (Code "tu0") ".")
   (P "实际上我们还提供了" (Code "@ap")
      ", 它是" (Code "ap") "的括号化版本.")
   (H3. "表格相关过程")
   (P "MathML的表格在某些地方类似于HTML表格, "
      "但是保留了许多传统的表格属性. "
      "这些属性对于HTML表格而言已经是不建议使用的了, "
      "而对于Chrome而言其对于这些MathML表格属性也没有任何支持. "
      "不过, 至少Firefox还是有相应支持的. "
      "当前我只考虑了Firefox的渲染效果, "
      "或许为了兼容性应该修改以适应其他浏览器平台. "
      "Chrome建议MathML用户使用CSS来控制MathML表格.")
   (P "我认为目前这一部分的细节我写得非常之烂, "
      "达到了令人发指的地步. "
      "然而, 近来仍然没有更新的精力. "
      "这些过程和宏仍然是值得使用的. "
      "表格的用法非常灵活, "
      "普通的排列, 分支选择, 矩阵, 行列式都可以用表格来表达."
      (CodeB "(define (&amp;table l)
  (apply Mtable
         (map (lambda (r)
                (apply Mtr (map Mtd r)))
              l)))
(define-syntax &amp;Table
  (syntax-rules ()
    ((_ (x ...) ...)
     (&amp;table (list (list x ...) ...)))))
(define (choice l)
  (Mrow $lc (&amp;table l)))
(define-syntax Choice
  (syntax-rules ()
    ((_ (x ...) ...)
     (choice (list (list x ...) ...)))))
(define (choice0 l)
  (Mrow
   $lc
   (keyword-apply
    Mtable
    '(#:attr*) '(((displaystyle &quot;true&quot;)))
    (map (lambda (r)
           (Mtr (Mtd (car r))
                (keyword-apply
                 Mtd '(#:attr*) '(((columnalign &quot;left&quot;)))
                 (cdr r))))
         l))))
(define-syntax Choice0
  (syntax-rules ()
    ((_ (x ...) ...)
     (choice0 (list (list x ...) ...)))))
(define (mat l)
  (brac (&amp;table l)))
(define-syntax Mat
  (syntax-rules ()
    ((_ (x ...) ...)
     (mat (list (list x ...) ...)))))
(define (ma0 l)
  (pare (&amp;table l)))
(define-syntax Ma0
  (syntax-rules ()
    ((_ (x ...) ...)
     (ma0 (list (list x ...) ...)))))
(define (det l)
  (vert (&amp;table l)))
(define-syntax Det
  (syntax-rules ()
    ((_ (x ...) ...)
     (det (list (list x ...) ...)))))")
      "目前的一个难题是没法灵活直接调整属性, "
      "但至少读者还有之后的"
      (Code "set-attr*")
      "这一方便过程可以使用, "
      "所以在表达上也不是问题.")
   (H3. "一撇和两撇")
   (P "SMathML提供了一撇和两撇所对应的过程, "
      "它们经常用来表示导数, 或者不同的名字."
      (CodeB "(define $prime (Mo &quot;&amp;prime;&quot;))
(define $Prime (Mo &quot;&amp;Prime;&quot;))
(define (&amp;prime x)
  (Msup x $prime))
(define (&amp;Prime x)
  (Msup x $Prime))
(define (_prime x i)
  (_^ x i $prime))
(define (_Prime x i)
  (_^ x i $Prime))")
      "其中" (Code "_prime") "和" (Code "_Prime")
      "是为了方便既有撇号又有下标的情况, 时有用到.")
   (H3. "各种常量的排列组合")
   (P "各种常量通过" (Code "^") "和" (Code "_")
      "的组合在数学中频繁出现, "
      "所以为了用户的方便起见, "
      "我们有大量的预定义. "
      "不过, 这些预定义也是自动生成的.")
   (P "各种常量的一撇版本也经常用到, 也就是应用"
      (Code "&amp;prime")
      ". 我们的命名约定是在其后加上" (Code "^")
      ". 实际上, 两撇版本也应该添加, "
      "但是我们遗漏了, 不过命名约定是其后加上"
      (Code "^^") ".")
   (P "各种字母的所有" (Code "mathvariant")
      "变体我们都有提供, 命名约定是在其后添加"
      (Code ":") "和变体名称, 例如:"
      (MB (set-attr*
           (&Table
            ($P:normal (Ms "$P:normal"))
            ($P:bold (Ms "$P:bold"))
            ($P:italic (Ms "$P:italic"))
            ($P:bold-italic (Ms "$P:bold-italic"))
            ($P:double-struck (Ms "$P:double-struck"))
            ($P:bold-fraktur (Ms "$P:bold-fraktur"))
            ($P:script (Ms "$P:script"))
            ($P:bold-script (Ms "$P:bold-script"))
            ($P:fraktur (Ms "$P:fraktur"))
            ($P:sans-serif (Ms "$P:sans-serif"))
            ($P:bold-sans-serif (Ms "$P:bold-sans-serif"))
            ($P:sans-serif-italic (Ms "$P:sans-serif-italic"))
            ($P:sans-serif-bold-italic (Ms "$P:sans-serif-bold-italic"))
            ($P:monospace (Ms "$P:monospace")))
           'columnalign "center left"
           'frame "solid"
           'rowlines "solid"
           'columnlines "solid")))
   
   (H3. "各种条目")
   (P "数学文献中经常有各种条目: "
      "公理, 定义, 命题, 定理, 引理, 推论, 诸如此类. "
      "所以说, 有必要提供一个工具, 以及提供一些预定义的条目过程."
      (CodeB "(define (entry-make class template)
  (lambda (#:n [n &quot;&quot;])
    (lambda x*
      (keyword-apply
       Div '(#:attr*) `(((class ,class)))
       (B (format template n)) &quot; &quot; x*))))
(define-syntax-rule
  (define-entry* (id class template) ...)
  (begin
    (define id (entry-make class template))
    ...))"))
   (P "暂时我们提供了以下这些预定义条目."
      (CodeB "(define-entry*
  (definition &quot;definition&quot; &quot;定义~a.&quot;)
  (lemma &quot;lemma&quot; &quot;引理~a.&quot;)
  (theorem &quot;theorem&quot; &quot;定理~a.&quot;)
  (corollary &quot;corollary&quot; &quot;推论~a.&quot;)
  (example &quot;example&quot; &quot;例子~a.&quot;)
  (notation &quot;notation&quot; &quot;记号~a.&quot;)
  (comment &quot;comment&quot; &quot;注记~a.&quot;)
  (program &quot;program&quot; &quot;程序~a.&quot;)
  (exercise &quot;exercise&quot; &quot;练习~a.&quot;)
  (proposition &quot;proposition&quot; &quot;命题~a.&quot;)
  (remark &quot;remark&quot; &quot;评注~a.&quot;)
  (convention &quot;convention&quot; &quot;约定~a.&quot;)
  (tcomment &quot;tcomment&quot; &quot;译者注记~a.&quot;)
  (answer &quot;answer&quot; &quot;解答~a.&quot;))"))
   
   (H3. "对于一些律/公理的抽象")
   (P "SMathML的灵活性之一在于它可以"
      "表达许多高阶 (higher-order) 抽象, "
      "其中一个直接的应用就是表达各种律/公理, "
      "例如交换律和结合律. "
      "当前的这些定义打磨不够充分, "
      "往往建议读者按照需求自行定义, "
      "这里只是抛砖引玉."
      (CodeB "(define (commute* a b)
  (&amp;= (&amp;i* a b) (&amp;i* b a)))
(define (commute+ a b)
  (&amp;= (&amp;+ a b) (&amp;+ b a)))
(define (commute &amp; a b)
  (&amp;= (&amp; a b) (&amp; b a)))")
      (CodeB "(define (assoc* a b c)
  (&amp;= (&amp;i* a (@ (&amp;i* b c)))
      (&amp;i* (@ (&amp;i* a b)) c)))
(define (assoc+ a b c)
  (&amp;= (&amp;+ a (@ (&amp;+ b c)))
      (&amp;+ (@ (&amp;+ a b)) c)))
(define (associate &amp; a b c)
  (&amp;= (&amp; (@ (&amp; a b)) c) (&amp; a (@ (&amp; b c)))))")
      (CodeB "(define (disR a b c)
  (&amp;= (&amp;i* a (@ (&amp;+ b c)))
      (&amp;+ (&amp;i* a b)
          (&amp;i* a c))))
(define (disL a b c)
  (&amp;= (&amp;i* (@ (&amp;+ a b)) c)
      (&amp;+ (&amp;i* a c)
          (&amp;i* b c))))")
      "读者可能会发现有时他需要将等式的左右反过来, "
      "这也是本来这些过程的疏忽之一.")
   (P "还有一个过程, 尽管尚未加入SMathML, 或许值得一看:"
      (CodeB "(define (!commute f g . x*)
  (&amp;= (f (apply g x*))
      (apply g (map f x*))))")
      "这也是一种数学中常说的" (Q "交换")
      ", 并且" (Code "!commute")
      "也相当灵活, 可以适应许多不同的情况. "
      "以下是一个简单的例子."
      (CodeB "(MB (!commute (curry app $F)
              &amp;compose
              $f $g))")
      (MB (!commute (curry app $F)
                    &compose
                    $f $g)))
   (H3. "推导式")
   (P "推导式实际上也是基于表格实现的, "
      "不过作者更想单开一节呈现."
      (CodeB "(define (make-row . x*)
  (apply Mtr (map Mtd x*)))
(define (deriv x y . z*)
  (keyword-apply
   Mtable
   '(#:attr*) '(((displaystyle &quot;true&quot;) (columnalign &quot;right center left&quot;)))
   (cons (make-row x $= y)
         (map (lambda (z) (make-row $ $= z)) z*))))
(define (deriv0 x -> y . z*)
  (keyword-apply
   Mtable
   '(#:attr*) '(((displaystyle &quot;true&quot;) (columnalign &quot;right center left&quot;)))
   (cons (make-row x -> y)
         (map2 (lambda (-> w) (make-row $ -> w)) z*))))")
      (Code "deriv") "用于都是等号连接的推导, "
      (Code "deriv0") "则允许用户自定义. "
      "最开始它们的定义都颇为复杂, "
      "这尤其是因为原本的" (Code "columnalign")
      "是逐个单元格进行设定的, "
      "后来作者才发现没有必要.")
   (P "基于之后引入的" (Code "set-attr*")
      ", SMathML还提供了一些衍生过程."
      (CodeB "(define-syntax-rule (eqn* (x ...) ...)
  (MB (set-attr*
       (&amp;Table (x ...) ...)
       'columnalign &quot;right center left&quot;
       'displaystyle &quot;true&quot;)))")
      (Code "eqn*") "和" (Code "deriv")
      "的对齐方式类似, 但是允许除了第一行之外的左侧也有元素."
      (CodeB "(define (deriv^ x . y*)
  (set-attr*
   (apply
    Mtable
    (Mtr (Mtd $) (Mtd x))
    (map (lambda (y)
           (Mtr (Mtd $=) (Mtd y)))
         y*))
   'displaystyle &quot;true&quot;
   'columnalign &quot;center left&quot;))
(define (deriv^0 x . y*)
  (set-attr*
   (apply
    Mtable
    (Mtr (Mtd $) (Mtd x))
    (map2 (lambda (= y)
            (Mtr (Mtd =) (Mtd y)))
          y*))
   'displaystyle &quot;true&quot;
   'columnalign &quot;center left&quot;))")
      (Code "deriv^") "和" (Code "deriv^0")
      "分别类似于" (Code "deriv") "和"
      (Code "deriv0") ", 其不同之处在于"
      "原本第一行左侧的元素被折到了右侧. "
      "对于数学公式极长的情况, "
      "这两个过程是完全必要的.")
   (H3. "分隔")
   (P "我们经常需要以一定的间隔枚举一些数学表达式, "
      "因而SMathML也提供了相关的过程."
      (CodeB "(define (&amp;space n)
  (Mspace #:attr* `((width ,(format &quot;~sex&quot; n)))))
(define (&amp;split n)
  (make-op (&amp;space n)
    (lambda () $)
    (lambda (x) x)))")
      (Code "&amp;space")
      "可以创造具有一定宽度的常量, 而"
      (Code "&amp;split")
      "则可以构造一个过程, 其为参数插入指定的间隔.")
   (H3. "推理规则")
   (P "逻辑学和编程语言领域经常需要排版推理规则和证明树. "
      "SMathML提供的相关功能完全是基于" (Code "Mfrac")
      "的, 其效果并不特别符合一般审美, "
      "但是至少能够让人理解.")
   (P (CodeB "(define (&amp;rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (if (null? j*)
        (car j1)
        (~ #:attr* '((displaystyle &quot;true&quot;))
           (apply (&amp;split n) j*) (car j1)))))")
      (Code "&amp;rule")
      "构造一条规则, 不过在其只有一个参数时会省略横线. "
      "许多书籍实际上也偏爱不省略的风格, "
      "那么或许读者需要自行定义. "
      "当然, 或许我们应该定义一些parameter. "
      "但是, 那时作者还未考虑到这一点.")
   (P (CodeB "(define (&amp;rule* j . j*)
  (if (null? j*)
      j
      (apply &amp;rule*
             (~ #:attr* '((displaystyle &quot;true&quot;))
                j (car j*))
             (cdr j*))))")
      (Code "&amp;rule*")
      "有点类似于" (Code "&amp;rule")
      ", 但是表达的是连续的规则应用.")
   
   (H2. "变换")
   (P "变换是SMathML的核心, 没有变换就很难表达许多抽象.")
   (H3. "一般变换" (Code "T"))
   (P "让我们先来呈现" (Code "T") "的代码."
      (CodeB "(define (T style*)
  (define default-handler
    (let ((default-binding (assoc '(default) style*)))
      (match default-binding
        (`((default) *preorder* ,handler) handler)
        (`((default) ,handler)
         (lambda (tag attr* . xml*)
           (apply handler tag attr* (map transform xml*))))
        (else
         (lambda (tag attr* . xml*)
           `(,tag ,attr* . ,(map transform xml*)))))))
  (define text-handler
    (let ((text-binding (assoc '(text) style*)))
      (if text-binding
          (cadr text-binding)
          identity)))
  (define Style*
    (map
     (lambda (binding)
       (match binding
         (`(,tag *preorder* ,handler)
          (cons tag handler))
         (`(,tag ,handler)
          (cons tag
                (lambda (tag attr* . xml*)
                  (apply handler tag attr*
                         (map transform xml*)))))))
     (filter (lambda (style) (symbol? (car style)))
             style*)))
  (define (transform xml)
    (match xml
      (`(,tag ,attr* . ,xml*)
       (let ((binding (assq tag Style*)))
         (if binding
             (apply (cdr binding) tag attr* xml*)
             (apply default-handler tag attr* xml*))))
      (str (text-handler str))))
  transform)")
      "而为了理解这些代码, 首先我们应该来看其注释."
      (CodeB ";&lt;xml> ::= &lt;string> | &lt;symbol> | &lt;number> | &lt;boolean> | &lt;vector>
;       |  (&lt;tag> (&lt;attr>*) &lt;xml>*)
;&lt;tag> ::= &lt;symbol> | &lt;a scheme value other than symbol>
;&lt;attr> ::= (&lt;symbol> &lt;string>)
;&lt;binding> ::= (&lt;symbol> &lt;handler>)
;           |  (&lt;symbol> *preorder* &lt;handler>)
;           |  ((default) &lt;handler>)
;           |  ((default) *preorder* &lt;handler>)
;           |  ((text) &lt;text-handler>)
;&lt;handler> : &lt;symbol> * &lt;attr>* * &lt;xml>* -> &lt;xml>
;&lt;text-handler> : &lt;string> -> &lt;string>"))
   (P "当前" (Code "T") "的设计有一尴尬之处, "
      "那就是没有提供按照相同方式处理多个可能符号的句法. "
      "不过, 存在各种变通途径, 以至于不是不能表达. "
      "未来这一点应该修改.")
   
   (H3. "属性修饰")
   (P "SMathML提供了两个过程用于修饰属性, 一个是"
      (Code "attr*-set") ", 另外一个是"
      (Code "set-attr*") ". 实际上, "
      (Code "set-attr*") "基于"
      (Code "attr*-set") "."
      (CodeB "(define (attr*-set attr* . xv*)
  (define (s a* x v)
    (if (eq? v #f)
        a*
        (let s ((a* a*) (x x) (v v))
          (cond ((null? a*)
                 (list (list x v)))
                ((eq? (caar a*) x)
                 (cons (list x v) (cdr a*)))
                (else
                 (cons (car a*)
                       (s (cdr a*) x v)))))))
  (if (null? xv*)
      attr*
      (let iter ((x (car xv*))
                 (v (cadr xv*))
                 (xv* (cddr xv*))
                 (a* attr*))
        (if (null? xv*)
            (s a* x v)
            (iter (car xv*) (cadr xv*) (cddr xv*)
                  (s a* x v))))))")
      (Code "attr*-set")
      "用于修改表示属性的关联列表, "
      "其可以接受多个需要修改的参数. "
      "目前这个过程效率并不高, "
      "实际上或许简单" (Code "append")
      "是更好的实际选择. 不过, "
      "这样语义会发生微妙的改变, "
      "因为原本后出现的要修饰属性可以覆盖之前的. "
      "不过, 在我使用SMathML时, "
      "似乎从来没有这么做过. "
      "当然, 目前的实现也有优点, "
      "那就是可以产生最为紧凑的属性."
      (CodeB "(define (set-attr* xml . xv*)
  (match xml
    (`(,tag ,attr* . ,xml*)
     `(,tag ,(apply attr*-set attr* xv*) . ,xml*))
    ((? string? str)
     `(div ,(apply attr*-set '() xv*) ,str))))")
      (Code "set-attr*")
      "用于修改SXML元素, "
      "如果不是元素则会自动添加" (Code "div")
      ". 实际上, 就作者而言, 似乎使用"
      (Code "span") "更为合理, "
      "不过目前已经积重难返了. "
      "(实际上, 作者已经记不得博客里是否用到过这种特殊情况.)")
   (H3. (Code "Tm") ": 数学内容辅助变换")
   (P (Code "Tm") "是一个核心的变换过程, "
      "作者的博客几乎每一页面都会用到" (Code "Tm")
      ". 实际上, 只要页面出现数学内容, "
      "就应该使用" (Code "Tm") ", 这是推荐做法.")
   (P (Code "Tm") "是使用SMathML的部分尴尬体验的修复. "
      "例如, 若不使用" (Code "Tm")
      ", 那么即便是意图内联的MathML元素, "
      "也必须要在外面添加" (Code "Math")
      "或者" (Code "M") ", 这明显不太合理. "
      "不过, 单纯添加" (Code "Math")
      "可以简单自动化, 只需检测是否有MathML元素裸露在外即可. "
      "但对于已经被MathML根元素包裹的表达式, "
      "我们必须要予以保留, 并且属性也必须要保留. "
      "不然的话, 比如说内联和块模式改变就糟糕了. "
      (Code "Tm") "还改进了其他一些尴尬之处. "
      "例如, 原本要在MathML内部使用文本, "
      "需要手动添加" (Code "Mtext")
      ", 这一点也被" (Code "Tm") "完全自动化了. "
      (Code "Tm") "还对于其他一些原本异常的常量进行了变换, "
      "例如将整数变为由" (Code "Mn")
      "包裹. 而且, 对于裸的整数, " (Code "Tm")
      "还会不忘添加" (Code "Math")
      ". 对于符号和分数的处理, " (Code "Tm")
      "也尽可能保持直觉化了.")
   (P "以下是我们对于" (Code "Tm") "的实现."
      (CodeB "(define (Mid id) (Mi (symbol->string id)))
(define (Mnum n) (Mn (number->string n)))
(define (Mint n)
  (if (&lt; n 0)
      (&amp;- (Mnum (- n)))
      (Mnum n)))
(define (Mrat n)
  (let* ((a (numerator n))
         (b (denominator n)))
    (if (&lt; a 0)
        (&amp;- (~ (Mnum (- a)) (Mnum b)))
        (~ (Mnum a) (Mnum b)))))
(define mathml-tag*
  (list->seteq
   '(merror
     mfrac mi mmultiscripts mn mo mover
     mpadded mphantom mroot mrow
     ms mspace msqrt mstyle msub msubsup
     msup mtable mtd mtext mtr munder
     munderover maction menclose mfenced
     #;mprescripts)))
(define math-style*
  `(((default)
     *preorder*
     ,(lambda (tag attr* . xml*)
        (cond ((eq? tag 'math) `(,tag ,attr* . ,(map Tx xml*)))
              ((set-member? mathml-tag* tag)
               (Math (Tx `(,tag ,attr* . ,xml*))))
              (else `(,tag ,attr* . ,(map Tm xml*))))))
    ((text) ,(lambda (text)
               (cond ((string? text) text)
                     ((symbol? text) (Math (Mid text)))
                     ((integer? text) (Math (Mint text)))
                     ((rational? text) (Math (Mrat text)))
                     (else (error 'Tm &quot;unknown element ~s&quot; text)))))
    ))
;token elements: mtext mi mn mo mspace ms
(define token-tag*
  (seteq 'mtext 'mi 'mn 'mo 'mspace 'ms))
(define mtext-style*
  `(((default)
     *preorder*
     ,(lambda (tag attr* . xml*)
        (cond ((memq tag '(mi mn mo ms mspace mtext))
               `(,tag ,attr* . ,xml*))
              (else `(,tag ,attr* . ,(map Tx xml*))))))
    ((text) ,(lambda (text)
               (cond ((string? text) (% text))
                     ((symbol? text) (Mid text))
                     ((integer? text) (Mint text))
                     ((rational? text) (Mrat text))
                     (else (error 'Tm &quot;unknown element ~s&quot; text)))))))
(define Tm (T math-style*))
(define Tx (T mtext-style*))")
      "这里的命名比较随意, 但作者也想不出更好的方案了. "
      "或许细心的读者会发现作者没有使用" (Code "token-tag*")
      ", 这主要是因为作者发现在元素较少时, "
      "使用直接的列表似乎效率更高.")
   (P "基于" (Code "Tm") ", 我们定义了"
      (Code "TmPrelude") ", 其是直接的复合."
      (CodeB "(define TmPrelude
  (compose Tm Prelude))")
      "Racket本身的" (Code "compose")
      "有一个很大的优点, 就是它能够保留关键词参数.")
   (H3. "表格变换")
   (P "在使用SMathML的初期, "
      "作者感觉有必要提供一个位置敏感的表格变换. "
      "不过, 现在作者认为不如在构造表格时就进行变换."
      (CodeB "(define (mapi f l)
  (let m ((i 0) (l l))
    (if (null? l)
        '()
        (cons (f (car l) i)
              (m (+ i 1) (cdr l))))))
(define (Ttable Td)
  (define (Tt tag attr* . r*)
    `(mtable ,attr* . ,(mapi Tr r*)))
  (define (Tr r i)
    (apply
     (lambda (tag attr* . d*)
       `(mtr ,attr* .
             ,(mapi (lambda (d j) (Td d i j)) d*)))
     r))
  (T `((mtable *preorder* ,Tt))))")
      "也就是说, 作者近来都没有使用过"
      (Code "Ttable") ".")
   (H2. "自动编号和引用")
   (P "自动编号和引用机制对于实际使用是非常必要的, "
      "这一点目前SMathML有了初步支持, "
      "但是使用起来颇为繁琐.")
   (P "大致的思路如下: 我们使用特殊的向量tag, "
      "其中的各个field自然是可变的, "
      "首先我们通过线性遍历积累信息, "
      "并同时构造出相应的标题和条目, 以及引用格式. "
      "当然, 我们也会构造一个字符串和引用格式之间的关联列表. "
      "然后, 我们进行第二次遍历, "
      "其将通过字符串的引用转换为对应的引用格式.")
   (P "这套机制并不优雅, 但是至少堪用, "
      "未来很有可能会进行大幅度的重写.")
   (H3. "实现")
   (P (CodeB "(define (pass0 exp)
  (define citation-table '())
  (define (extend-table! id citation)
    (if (assoc id citation-table)
        (error 'automatic-numbering-pass0 &quot;id conflict!&quot;)
        (set! citation-table
              (cons (cons id citation) citation-table))))
  (let iterate ((henv '(0)) (section #f) (genv '()) (lenv '()) (rest exp) (result '()))
    (if (null? rest)
        (cons citation-table (reverse result))
        (let ((current (car rest)) (rest (cdr rest)))
          (match current
            ((,tag ,attr* . ,xml*)
             (cond
               ((symbol? tag)
                (iterate henv section genv lenv rest (cons current result)))
               ((%heading? tag)
                (define level (%heading-level tag))
                (define id (%heading-id tag))
                (define auto? (%heading-auto? tag))
                (define present (%heading-present tag))
                (define cite (%heading-cite tag))
                (define switch? (%heading-switch? tag))
                (cond (auto? (define section (henv-next henv level))
                             (set-%heading-section! tag section)
                             (when id (extend-table! id (cite tag)))
                             (iterate section section genv
                                      (if switch? '() lenv) rest
                                      (cons (apply present tag attr* xml*) result)))
                      (else (define section (henv-* henv level))
                            (set-%heading-section! tag section)
                            (when id (extend-table! id (cite tag)))
                            (iterate henv section genv
                                     (if switch? '() lenv) rest
                                     (cons (apply present tag attr* xml*) result)))))
               ((%entry? tag)
                (define local? (%entry-local? tag))
                (define class (%entry-class tag))
                (define id (%entry-id tag))
                (define auto? (%entry-auto? tag))
                (define present (%entry-present tag))
                (define cite (%entry-cite tag))
                (set-%entry-section! tag section)
                (cond (auto? (let-values (((g/lenv nval)
                                           (g/lenv-next (if local? lenv genv) class)))
                               (set-%entry-index! tag nval)
                               (when id (extend-table! id (cite tag)))
                               (if local?
                                   (iterate henv section genv g/lenv rest
                                            (cons (apply present tag attr* xml*) result))
                                   (iterate henv section g/lenv lenv rest
                                            (cons (apply present tag attr* xml*)
                                                  result)))))
                      (else (when id (extend-table! id (cite tag)))
                            (iterate henv section genv lenv rest
                                     (cons (apply present tag attr* xml*) result)))))
               (else
                (iterate henv section genv lenv rest (cons current result)))))
            (,else (iterate henv section genv lenv rest (cons current result))))))))")
      (Code "pass0") "颇为复杂, 读者需要注意的一点是, "
      "它只会对于顶层进行线性遍历, 但不会深入.")
   (P (CodeB ";pass1
(define (Ref id) `(ref () ,id))
(define (pass1 exp)
  (define citation-table (car exp))
  (define (reify id)
    (cond ((assoc id citation-table) => cdr)
          (else (error 'pass1 &quot;unknown id ~s&quot; id))))
  (define Tr
    (T `((ref *preorder* ,(lambda (ref empty id) (reify id))))))
  (define xml* (cdr exp))
  (map Tr xml*))")
      (Code "pass1") "要简单很多, "
      "因为它做的事情从本质上来说只是简单替换. "
      (Code "Ref") "是SMathML提供的一个简单API, "
      "用于引用标题或条目.")
   (P (CodeB ";automatic numbering
(define numbering-style*
  `((body
     *preorder*
     ,(lambda (tag attr* . xml*)
        `(,tag ,attr* . ,(pass1 (pass0 xml*)))))))
(define Tn
  (T numbering-style*))")
      "最后我们将" (Code "pass0") "和" (Code "pass1")
      "组合到一起, 就得到了" (Code "Tn") ".")
   (P "和" (Code "TmPrelude") "类似, SMathML也提供了"
      (Code "TnTmPrelude") "."
      (CodeB "(define TnTmPrelude
  (compose Tn Tm Prelude))"))
   (H3. "默认辅助过程")
   (P (Code "Tn") "只是一个整体性的框架, "
      "但是对于标题和条目而言, "
      "present和cite函数都需要编写之后才能使用. "
      "因此, SMathML也提供了一些默认的辅助过程.")
   (P (CodeB "(define (format-section section)
  (define sec (cdr (reverse section)))
  (apply string-append
         (add-between (map number->string sec) &quot;.&quot;)))
(define (heading-present %heading attr* . html*)
  (define level (%heading-level %heading))
  (define auto? (%heading-auto? %heading))
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  (cond ((= level 4) `(h4 ,(attr*-set attr* 'id id)
                          ,(format &quot;第~a小节 &quot; (format-section section))
                          . ,html*))
        ((= level 3) `(h3 ,(attr*-set attr* 'id id)
                          ,(format &quot;第~a节 &quot; (format-section section))
                          . ,html*))
        ((= level 2) `(h2 ,(attr*-set attr* 'id id)
                          ,(format &quot;第~a章 &quot; (format-section section))
                          . ,html*))
        ((= level 1) `(h1 ,(attr*-set attr* 'id id) . ,html*))
        (else (error 'heading-present &quot;invalid level ~s&quot; level))))
(define (h4-present %heading attr* . html*)
  (define auto? (%heading-auto? %heading))
  (define id (%heading-id %heading))
  (if auto?
      (let ((section (%heading-section %heading)))
        `(h4 ,(attr*-set attr* 'id id)
             ,(format &quot;第~a小节 &quot; (format-section section))
             . ,html*))
      `(h4 ,(attr*-set attr* 'id id) . ,html*)))
(define (h3-present %heading attr* . html*)
  (define auto? (%heading-auto? %heading))
  (define id (%heading-id %heading))
  (if auto?
      (let ((section (%heading-section %heading)))
        `(h3 ,(attr*-set attr* 'id id)
             ,(format &quot;第~a节 &quot; (format-section section))
             . ,html*))
      `(h3 ,(attr*-set attr* 'id id) . ,html*)))
(define (h2-present %heading attr* . html*)
  (define auto? (%heading-auto? %heading))
  (define id (%heading-id %heading))
  (if auto?
      (let ((section (%heading-section %heading)))
        `(h2 ,(attr*-set attr* 'id id)
             ,(format &quot;第~a章 &quot; (format-section section))
             . ,html*))
      `(h2 ,(attr*-set attr* 'id id) . ,html*)))
(define (h1-present %heading attr* . html*)
  (define id (%heading-id %heading))
  `(h1 ,(attr*-set attr* 'id id) . ,html*))
(define (heading-cite %heading)
  (define id (%heading-id %heading))
  (define href (string-append &quot;#&quot; id))
  (define section (%heading-section %heading))
  (define level (%heading-level %heading))
  (define ref
    `(a ((href ,href)) ,(format-section section)))
  (cond ((= level 4) (Cite &quot;第&quot; ref &quot;小节&quot;))
        ((= level 3) (Cite &quot;第&quot; ref &quot;节&quot;))
        ((= level 2) (Cite &quot;第&quot; ref &quot;章&quot;))
        (else
         (error 'heading-cite &quot;invalid level ~s&quot; level))))
(define (H1. #:attr* [attr* '()] #:id [id #f] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h1-present #:level 1
                     #:id id #:auto? auto?)
    ,attr* . ,html*))
(define (H2. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #t] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h2-present #:cite heading-cite
                     #:level 2 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (H3. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #t] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h3-present #:cite heading-cite
                     #:level 3 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (H4. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #t] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h4-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))")
      "这里也存在一些设计失误, "
      "例如很多东西都应该参数化, "
      "但是目前读者只能通过同名的新定义来覆盖这些定义. "
      "另外, 函数" (Code "heading-present")
      "是一个历史遗留产物, 目前尚未删除的原因是担心兼容性, "
      "但是未来大概率会删除.")
   (P "至于条目, 目前SMathML还没有提供相关过程. "
      "但是, 在作者书写博客时, 的确产生了一套既定的程序. "
      "问题在于, 作者总是要为每一个页面对其进行小小的修改, "
      "所以这些代码目前还没有合并到SMathML之中.")
   
   (H2. "输出")
   (P "为了使得SMathML实际有用, 我们必须要提供一系列的输出过程.")
   (H3. "输出HTML/XML")
   (P "为了将SXML转换为真正的HTML/XML格式, 我们提供了一个过程"
      (Code "Xml") ", 其实现如下:"
      (CodeB ";&lt;xml> ::= &lt;string>
;       |  (&lt;tag> (&lt;attr>*) &lt;xml>*)
;&lt;tag> ::= &lt;symbol>
;&lt;attr> ::= (&lt;symbol> &lt;string>)
(define (Attr* attr*)
  (for-each
   (lambda (attr)
     (printf &quot; ~s=~s&quot; (car attr) (cadr attr)))
   attr*))
(define (Xml xml)
  (match xml
    (`(,tag ,attr* . ,xml*)
     #:when (symbol? tag)
     (if (null? xml*)
         (begin
           (printf &quot;&lt;~s&quot; tag)
           (Attr* attr*)
           (printf &quot;/>&quot;))
         (begin
           (printf &quot;&lt;~s&quot; tag)
           (Attr* attr*)
           (printf &quot;>&quot;)
           (for-each Xml xml*)
           (printf &quot;&lt;/~s>&quot; tag))))
    ((? string? str)
     (printf &quot;~a&quot; str))))")
      "注意到虽然在中间变换的过程中我们允许非符号的tag, "
      "以及非字符串的叶子, 但是到了使用" (Code "Xml")
      "输出时, 我们必须回到最原始的定义.")
   (H3. "输出CSS")
   (P (CodeB ";&lt;attr> ::= (&lt;symbol> &lt;string>)
;&lt;style> ::= (&lt;string> &lt;attr>*)
;&lt;css> ::= (&lt;style>*)
(define (Attr* attr*)
  (for-each
   (lambda (attr)
     (printf &quot;~s: ~a;&quot; (car attr) (cadr attr)))
   attr*))
(define (Style style)
  (printf &quot;~a {&quot; (car style))
  (Attr* (cdr style))
  (printf &quot;}\\n&quot;))
(define (Css css)
  (for-each Style css))")
      "SMathML并没有提供什么对于CSS进行抽象和组合的手段, "
      "而是只有输出的过程.")
   (H3. "输出到文件")
   (P "当然, 仅是输出还不够, 我们也提供了输出到文件的一些辅助过程."
      (CodeB "(define (size x)
  (cond ((pair? x) (+ (size (car x)) (size (cdr x))))
        ((null? x) 0)
        ((string? x) (string-length x))
        (else 1)))
(define replace? (make-parameter #f))
(define compute-size? (make-parameter #f))
(define ((emit proc) exp path)
  (define (emit-file)
    (when (compute-size?)
      (printf &quot;size of [~a]: ~a\\n&quot; path (size exp)))
    (with-output-to-file path
      (lambda () (proc exp))
      #:exists 'replace))
  (if (file-exists? path)
      (if (replace?)
          (emit-file)
          (printf &quot;~s exists, emit cancelled\\n&quot; path))
      (emit-file)))
(define ((emit-thunk proc) thunk path)
  (define (emit-file)
    (define exp (thunk))
    (when (compute-size?)
      (printf &quot;size of [~a]: ~a\\n&quot; path (size exp)))
    (with-output-to-file path
      (lambda () (proc exp))
      #:exists 'replace))
  (if (file-exists? path)
      (if (replace?)
          (emit-file)
          (printf &quot;~s exists, emit cancelled\\n&quot; path))
      (emit-file)))
(define ((emit-promise proc) promise path)
  (define (emit-file)
    (define exp (force promise))
    (when (compute-size?)
      (printf &quot;size of [~a]: ~a\\n&quot; path (size exp)))
    (with-output-to-file path
      (lambda () (proc exp))
      #:exists 'replace))
  (if (file-exists? path)
      (if (replace?)
          (emit-file)
          (printf &quot;~s exists, emit cancelled\\n&quot; path))
      (emit-file)))")
      "其实我们也可以将" (Code "emit") ", " (Code "emit-thunk")
      ", " (Code "emit-promise")
      "合为一个过程, 但是当前设计或许能够减少错误的可能. "
      "另外, 这些过程的功能正如名字所暗示的那样: "
      (Code "emit") "就是输出到文件; " (Code "emit-thunk")
      "是要输出一个thunk (无参函数), 故需要先求值; "
      (Code "emit-promise")
      "是要输出一个promise, 所以需要先force.")
   (P "之所以我们提供了" (Code "emit-thunk") "和"
      (Code "emit-promise")
      ", 是考虑到promise会缓存计算的结果. "
      "然而, 有时我们可能需要做benchmark, "
      "那么这时使用简单thunk更符合我们的需求. "
      "不过, 这两个过程都可以在需要输出许多文件时避免一些工作量. "
      "(当然, 这完全没有自动化, 需要用户手工判断.)")
   (P "基于" (Code "emit") "和" (Code "emit-thunk")
      ", 我们提供了以下过程:"
      (CodeB "(define emitXml (emit Xml))
(define emitCss (emit Css))
(define emitXml-thunk (emit-thunk Xml))
(define emitCss-thunk (emit-thunk Css))")
      "实际上, 我们还应该提供"
      (Code "emitXml-promise") "和"
      (Code "emitCss-promise")
      ", 但是作者忘了, 这是一个失误. "
      "未来大概会添加.")
   
   (H2. "杂项")
   (P "本章记录一些SMathML提供的不成体系的杂七杂八的功能.")
   (H3. "转义")
   (P "SMathML提供了一个叫做" (Code "escape*")
      "的无参数过程. 一旦调用, 会进入交互. "
      "用户将需要转义的文本复制粘贴并按下回车后, "
      "就会得到转义后的文本."
      (CodeB "(define (escape s)
  (define l (string-length s))
  (let iter ((i 0))
    (unless (= i l)
      (let ((c (string-ref s i)))
        (case c
          ((#\\&quot;) (printf &quot;&amp;quot;&quot;))
          ((#\\&lt;) (printf &quot;&amp;lt;&quot;))
          ((#\\&amp;) (printf &quot;&amp;amp;&quot;))
          ((#\\\\) (printf &quot;\\\\\\\\&quot;))
          (else (printf &quot;~a&quot; c))))
      (iter (+ i 1)))))
(define (escape*)
  (escape (read-line))
  (newline)
  (escape*))"))
   (H3. "日期")
   (P (CodeB "(define now (current-date))
(define (current-date-string #:format [format 'rfc2822])
  (parameterize ((date-display-format format))
    (date->string now #t)))")
      "这个就不建议使用了. 不过, "
      "它实际上也就是提供一个当前日期字符串.")
   (H3. "正体大写希腊字母")
   (P "鉴于正体大写希腊字母时有用到, "
      "SMathML提供了以下定义:"
      (CodeB "(define Γ $Gamma:normal)
(define Δ $Delta:normal)
(define Θ $Theta:normal)
(define Λ $Lambda:normal)
(define Ξ $Xi:normal)
(define Π $Pi:normal)
(define Σ $Sigma:normal)
(define Φ $Phi:normal)
(define Ψ $Psi:normal)
(define Ω $Omega:normal)"))
   (H3. "RSS")
   (P "SMathML提供了一些过程用于编写RSS, "
      "其需要通过" (Code "(require SMathML/rss)")
      "引入."
      (CodeB "(define (Rss #:version [version &quot;2.0&quot;] channel)
  `(rss ((version ,version)) ,channel))
(define (Title str) `(title () ,str))
(define (Link str) `(link () ,str))
(define (Description str) `(description () ,str))
(define (Channel #:title title #:link link #:description description . xml*)
  `(channel () ,(Title title) ,(Link link) ,(Description description) . ,xml*))
(define (RC #:version [version &quot;2.0&quot;]
            #:title title #:link link #:description description
            . xml*)
  (Rss #:version version
       (keyword-apply
        Channel '(#:description #:link #:title) (list description link title)
        xml*)))
(define (Item . xml*) `(item () . ,xml*))
(define (PubDate str) `(pubDate () ,str))"))
   
   (H2. "SVG绘图")
   (P "SMathML所提供的SVG绘图机制还非常原始, 几乎不可使用. "
      "不过, 有总是比没有更好就是了.")
   (H3. "基本函数")
   (P (CodeB "(define (A #:attr* [attr* '()] . svg*)
  `(a ,attr* . ,svg*))
(define (Animate #:attr* [attr* '()] . svg*)
  `(animate ,attr* . ,svg*))
(define (AnimateMotion #:attr* [attr* '()] . svg*)
  `(animateMotion ,attr* . ,svg*))
(define (AnimateTransform #:attr* [attr* '()] . svg*)
  `(animateTransform ,attr* . ,svg*))
(define (Circle #:attr* [attr* '()] . svg*)
  `(circle ,attr* . ,svg*))
(define (ClipPath #:attr* [attr* '()] . svg*)
  `(clipPath ,attr* . ,svg*))
(define (Defs #:attr* [attr* '()] . svg*)
  `(defs ,attr* . ,svg*))
(define (Desc #:attr* [attr* '()] . svg*)
  `(desc ,attr* . ,svg*))
(define (Ellipse #:attr* [attr* '()] . svg*)
  `(ellipse ,attr* . ,svg*))
(define (FeBlend #:attr* [attr* '()] . svg*)
  `(feBlend ,attr* . ,svg*))
(define (FeColorMatrix #:attr* [attr* '()] . svg*)
  `(feColorMatrix ,attr* . ,svg*))
(define (FeComponentTransfer #:attr* [attr* '()] . svg*)
  `(feComponentTransfer ,attr* . ,svg*))
(define (FeComposite #:attr* [attr* '()] . svg*)
  `(feComposite ,attr* . ,svg*))
(define (FeConvolveMatrix #:attr* [attr* '()] . svg*)
  `(feConvolveMatrix ,attr* . ,svg*))
(define (FeDiffuseLighting #:attr* [attr* '()] . svg*)
  `(feDiffuseLighting ,attr* . ,svg*))
(define (FeDisplacementMap #:attr* [attr* '()] . svg*)
  `(feDisplacementMap ,attr* . ,svg*))
(define (FeDistantLight #:attr* [attr* '()] . svg*)
  `(feDistantLight ,attr* . ,svg*))
(define (FeDropShadow #:attr* [attr* '()] . svg*)
  `(feDropShadow ,attr* . ,svg*))
(define (FeFlood #:attr* [attr* '()] . svg*)
  `(feFlood ,attr* . ,svg*))
(define (FeFuncA #:attr* [attr* '()] . svg*)
  `(feFuncA ,attr* . ,svg*))
(define (FeFuncB #:attr* [attr* '()] . svg*)
  `(feFuncB ,attr* . ,svg*))
(define (FeFuncG #:attr* [attr* '()] . svg*)
  `(feFuncG ,attr* . ,svg*))
(define (FeFuncR #:attr* [attr* '()] . svg*)
  `(feFuncR ,attr* . ,svg*))
(define (FeGaussianBlur #:attr* [attr* '()] . svg*)
  `(feGaussianBlur ,attr* . ,svg*))
(define (FeImage #:attr* [attr* '()] . svg*)
  `(feImage ,attr* . ,svg*))
(define (FeMerge #:attr* [attr* '()] . svg*)
  `(feMerge ,attr* . ,svg*))
(define (FeMergeNode #:attr* [attr* '()] . svg*)
  `(feMergeNode ,attr* . ,svg*))
(define (FeMorphology #:attr* [attr* '()] . svg*)
  `(feMorphology ,attr* . ,svg*))
(define (FeOffset #:attr* [attr* '()] . svg*)
  `(feOffset ,attr* . ,svg*))
(define (FePointLight #:attr* [attr* '()] . svg*)
  `(fePointLight ,attr* . ,svg*))
(define (FeSpecularLighting #:attr* [attr* '()] . svg*)
  `(feSpecularLighting ,attr* . ,svg*))
(define (FeSpotLight #:attr* [attr* '()] . svg*)
  `(feSpotLight ,attr* . ,svg*))
(define (FeTile #:attr* [attr* '()] . svg*)
  `(feTile ,attr* . ,svg*))
(define (FeTurbulence #:attr* [attr* '()] . svg*)
  `(feTurbulence ,attr* . ,svg*))
(define (Filter #:attr* [attr* '()] . svg*)
  `(filter ,attr* . ,svg*))
(define (ForeignObject #:attr* [attr* '()] . svg*)
  `(foreignObject ,attr* . ,svg*))
(define (G #:attr* [attr* '()] . svg*)
  `(g ,attr* . ,svg*))
(define (Image #:attr* [attr* '()] . svg*)
  `(image ,attr* . ,svg*))
(define (Line #:attr* [attr* '()] . svg*)
  `(line ,attr* . ,svg*))
(define (LinearGradient #:attr* [attr* '()] . svg*)
  `(linearGradient ,attr* . ,svg*))
(define (Marker #:attr* [attr* '()] . svg*)
  `(marker ,attr* . ,svg*))
(define (Mask #:attr* [attr* '()] . svg*)
  `(mask ,attr* . ,svg*))
(define (Metadata #:attr* [attr* '()] . svg*)
  `(metadata ,attr* . ,svg*))
(define (Mpath #:attr* [attr* '()] . svg*)
  `(mpath ,attr* . ,svg*))
(define (Path #:attr* [attr* '()] . svg*)
  `(path ,attr* . ,svg*))
(define (Pattern #:attr* [attr* '()] . svg*)
  `(pattern ,attr* . ,svg*))
(define (Polygon #:attr* [attr* '()] . svg*)
  `(polygon ,attr* . ,svg*))
(define (Polyline #:attr* [attr* '()] . svg*)
  `(polyline ,attr* . ,svg*))
(define (RadialGradient #:attr* [attr* '()] . svg*)
  `(radialGradient ,attr* . ,svg*))
(define (Rect #:attr* [attr* '()] . svg*)
  `(rect ,attr* . ,svg*))
(define (Script #:attr* [attr* '()] . svg*)
  `(script ,attr* . ,svg*))
(define (Set #:attr* [attr* '()] . svg*)
  `(set ,attr* . ,svg*))
(define (Stop #:attr* [attr* '()] . svg*)
  `(stop ,attr* . ,svg*))
(define (Style #:attr* [attr* '()] . svg*)
  `(style ,attr* . ,svg*))
(define (Svg #:attr* [attr* '()] . svg*)
  `(svg ,attr* . ,svg*))
(define (Switch #:attr* [attr* '()] . svg*)
  `(switch ,attr* . ,svg*))
(define (Symbol #:attr* [attr* '()] . svg*)
  `(symbol ,attr* . ,svg*))
(define (Text #:attr* [attr* '()] . svg*)
  `(text ,attr* . ,svg*))
(define (TextPath #:attr* [attr* '()] . svg*)
  `(textPath ,attr* . ,svg*))
(define (Title #:attr* [attr* '()] . svg*)
  `(title ,attr* . ,svg*))
(define (Tspan #:attr* [attr* '()] . svg*)
  `(tspan ,attr* . ,svg*))
(define (Use #:attr* [attr* '()] . svg*)
  `(use ,attr* . ,svg*))
(define (View #:attr* [attr* '()] . svg*)
  `(view ,attr* . ,svg*))")
      "注意到其中一些函数和HTML部分的基本函数有冲突, "
      "所以说当用户使用SMathML时, "
      (Code "A") ", " (Code "Script") ", "
      (Code "Style") ", " (Code "Title")
      "都没有被导出. 然而, 其和HTML版本是等效的.")
   (H3. "图形库")
   (P "鉴于当前尚未有切实可行的优秀设计思路, "
      "所以图形库只提供了最为基础的不会出错的函数. "
      "对于作者而言, Penrose库似乎是理想的图形排版方式."
      (CodeB "(struct pt (x y) #:transparent)
(struct vec (x y) #:transparent)
(define (vec+ u v)
  (vec (+ (vec-x u) (vec-x v))
       (+ (vec-y u) (vec-y v))))
(define (vec- u v)
  (vec (- (vec-x u) (vec-x v))
       (- (vec-y u) (vec-y v))))
(define (vec* s v)
  (vec (* s (vec-x v))
       (* s (vec-y v))))
(define (pt+ p v)
  (pt (+ (pt-x p) (vec-x v))
      (+ (pt-y p) (vec-y v))))
(define (pt- p q)
  (vec (- (pt-x p) (pt-x q))
       (- (pt-y p) (pt-y q))))
(define (dot u v)
  (+ (* (vec-x u) (vec-x v))
     (* (vec-y u) (vec-y v))))
(define (vec-len v)
  (sqrt (dot v v)))
(define (vec-normalize v)
  (vec* (/ 1 (vec-len v)) v))
(define (pt-lerp p q t)
  (pt+ p (vec* t (pt- q p))))"))
   
   (H2. "画廊")
   (P "这一章的目的是为了提供SMathML的实际例子, "
      "以激发读者的想象力, 帮助他们更好地使用SMathML.")
   (H3. "Napier筹")
   (P "Napier筹是对数的发明者Napier设计的一种计算工具, "
      "最基本的应用是计算乘法. 以下我们将要编写代码, "
      "用于排版Napier筹计算乘法的过程."
      (CodeB "(define (char->mn c)
  (Mn (string c)))
(define (add0 l)
  (if (null? (cdr l))
      (cons #\\0 l)
      l))
(define (cell ab)
  (define-values (c d)
    (apply values
           (map char->mn
                (add0 (string->list
                       (number->string (apply * ab)))))))
  (Menclose
   #:attr* '((notation &quot;updiagonalstrike&quot;))
   (Mtable
    #:attr* '((frame &quot;solid&quot;))
    (Mtr (Mtd c) (Mtd $))
    (Mtr (Mtd $) (Mtd d)))))
(define 1--9 (range 1 10))
(define (product l1 l2)
  (map (lambda (x)
         (map (lambda (y) (list x y))
              l2))
       l1))
(define (tmap f t)
  (map (lambda (l) (map f l)) t))
(define (list->table lst)
  (keyword-apply
   Mtable
   '(#:attr*)
   '(((rowspacing &quot;0&quot;)
      (columnspacing &quot;0&quot;)))
   (map (lambda (row)
          (apply Mtr (map Mtd row)))
        lst)))
(define (napier m n)
  (list->table
   (tmap cell (product m n))))"))
   (P "以下我们来看几个例子.")
   (CodeB "(P (MB (napier 1--9 1--9))
   &quot;这是一个完整的Napier筹表.&quot;)")
   (P (MB (napier 1--9 1--9))
      "这是一个完整的Napier筹表.")
   (CodeB "(P (MB (napier '(4 6 7 8 5 3 9 9)
               '(9 6 4 3 1)))
   (MB (&amp;= (&amp;c* 46785399 96431)
           (* 46785399 96431))))")
   (P (MB (napier '(4 6 7 8 5 3 9 9)
                  '(9 6 4 3 1)))
      (MB (&= (&c* 46785399 96431)
              (* 46785399 96431))))
   (H3. "简单类型" $lambda "演算")
   (P $lambda "演算是一种内涵性函数理论, "
      "而(Church风格的)简单类型" $lambda
      "演算的定型规则如下:"
      (CodeB "(MB (&amp;rull &quot;(Var)&quot;
           (&amp;= (app Γ $x) $tau)
           (G!- (&amp;: $x $tau))))")
      (MB (&rull "(Var)"
                 (&= (app Γ $x) $tau)
                 (G!- (&: $x $tau))))
      (CodeB "(MB (&amp;rull &quot;(Abs)&quot;
           (G!- (&amp;: $x $tau_1)
                (&amp;: $e $tau_2))
           (G!- (&amp;: (Lam (&amp;: $x $tau_1) $e)
                    (&amp;-> $tau_1 $tau_2)))))")
      (MB (&rull "(Abs)"
                 (G!- (&: $x $tau_1)
                      (&: $e $tau_2))
                 (G!- (&: (Lam (&: $x $tau_1) $e)
                          (&-> $tau_1 $tau_2)))))
      (CodeB "(MB (&amp;rull &quot;(App)&quot;
           (G!- (&amp;: $e_1 (&amp;-> $tau_1 $tau_2)))
           (G!- (&amp;: $e_2 $tau_1))
           (G!- (&amp;: (App $e_1 $e_2) $tau_2))))")
      (MB (&rull "(App)"
                 (G!- (&: $e_1 (&-> $tau_1 $tau_2)))
                 (G!- (&: $e_2 $tau_1))
                 (G!- (&: (App $e_1 $e_2) $tau_2))))
      "其中"
      (CodeB "(define $!- (Mo &quot;&amp;vdash;&quot;))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &amp;cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define $.
  (Mo &quot;.&quot; #:attr* '((lspace &quot;0&quot;))))
(define (Lam x t)
  (: $lambda x $. t))
(define App (&amp;split 2))
(define (&amp;rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle &quot;true&quot;))
       (apply (&amp;split n) j*) (car j1))))
(define (&amp;rull label . x*)
  (: (apply &amp;rule x*) label))")
      "这些代码有的地方稍显尴尬, "
      "说明SMathML存在一些值得改进的地方.")
   
   ))