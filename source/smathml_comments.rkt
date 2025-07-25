#lang racket
(provide smathml_comments.html)
(require SMathML)
(define smathml_comments.html
  (TmPrelude
   #:title "SMathML注记"
   #:css "styles.css"
   (H1 "SMathML注记")
   (P "这里主要是为了记录MathML和SMathML微妙的地方, "
      "以及当前的SMathML有哪些地方需要修改或补充. "
      "这里只是草稿而已, 或许会有些混乱, 而且也不一定正确.")
   (H2 "关于基于数据类型的可扩展渲染")
   (P "在某种意义上来说, 提供给用户自行扩展的功能总是好的, 因此"
      "SMathML设计了一种相当一般的变换机制, 即过程" (Code "T")
      ". 并且, 基于过程" (Code "T") ", 还提供了一个用于数学内容渲染的过程"
      (Code "Tm") ". " (Code "Tm") "会对于整数和分数以及符号进行额外的"
      "变换, 但这是硬编码的, 或许" (Code "Tm") "乃至其他什么部分应该给用户"
      "自行设计渲染方式的空间.")
   (P "问题主要在于自行设计的这些部分该如何与SMathML的其他部分进行交互. "
      "最妥当的方式当然是强制要求不能产生任何交互, 必须从头开始编写, "
      "但这的确不方便, 且没有必要. 那么, 或许这种机制需要像" (Code "T")
      "一样让用户有选择是否交互的余地. 但是, 似乎把事情做得漂亮相当困难, "
      "所以我卡在这里了. 当我想明白的时候, 我会将其加入SMathML之中.")
   (H2 "实验性特性")
   (P "这里记录一些编写博客的时候创造的抽象.")
   (CodeB "(define (_cm A . x*)
  (_ A (apply &cm x*)))")
   (CodeB "(define (|[]| a b)
  (: $lb a $cm b $rb))
(define (|()| a b)
  (: $lp a $cm b $rp))
(define (|[)| a b)
  (: $lb a $cm b $rp))
(define (|(]| a b)
  (: $lp a $cm b $rb))")
   (CodeB "(define-syntax-rule (define-simple* (&id $id str) ...)
  (begin
    (define $id (Mi str))
    ...
    (define (&id x) (app $id x))
    ...))")
   (CodeB "(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign &quot;left center right&quot;)
                (width &quot;100%&quot;))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))")
   (CodeB "(define app*
  (case-lambda
    ((f x) (app f x))
    ((f g . arg*) (app f (apply app* g arg*)))))")
   (CodeB "(define (map-toggle flag proc lst)
  (cond ((null? lst) '())
        (flag (cons (proc (car lst))
                    (map-toggle (not flag) proc (cdr lst))))
        (else (cons (car lst)
                    (map-toggle (not flag) proc (cdr lst))))))")
   (H2 "关于引用")
   (P "之前我的设计太过局限, 让引用在一开始就生成, "
      "然而更好的方式是将其设计成一个接受上下文信息的函数. "
      "因为设计的问题, 似乎这不能立即实现.")
   (H2 "关于标号")
   (P "之前我的一个设计失误在于没有考虑到跨section的连续标号. "
      "当然, 全局的连续标号是存在的, 但是和跨section标号不是一类东西. "
      "之前的诸多页面总是滥用全局标号的一些特性实现的. "
      "似乎存在两种容易想到的解决之道, 一种是允许heading"
      "不刷新局部环境, 另一种是通过某种新的构造来擦除全局环境里的信息, "
      "后者仍然显得相当tricky, 因为还是需要用到全局的标号. "
      "或许允许擦除特定局部环境里的信息也是有价值的, 甚至可以不局限于擦除, "
      "而设计某种更加通用的变换. 新特性施工中. (目前已完成)")
   (H2 "关于公式标号")
   (P "这个似乎也是一种设计局限, 因为我没有允许heading和entry"
      "不出现在top level, 不然的话判断顺序对于我而言有点难办. "
      "也有可能存在一些扭曲的解决之道, 比如说遇到公式就另起一段, "
      "或者也可以设计一些特殊的entry, 通过特殊的规则转换它们. "
      "然而, 总归不是什么well-designed的东西.")
   (H2 "关于代码呈现")
   (P "我几乎没有设计任何关于代码呈现的功能 (除了一个用于转义的ad hoc过程), "
      "这可以包括着色, 格式化, 等等. 我在翻译The Little Typer时遇到了这样的需求, "
      "即我需要频繁地在代码中插入以不同字体显示的" (Q "元变量")
      ". 我发现基于字符串的" (Q "可复合性")
      "并不好, 主要是试图将几段(用于呈现的)代码合并的时候. "
      "我不能直接将这些元素转为字符串然后合并, 因为这将丢失给转换过程使用的结构信息. "
      "但我相信某种程度上的自动化仍然是可能的, 这之后再思考, 因为我希望尽可能一般.")
   (H2 "关于证明树的排版")
   (P "基于分数线的ad hoc方案在呈现上问题较多, 但这还不是主要问题, "
      "现在最大的问题在于没有一种排版证明树的可复合方式. "
      "现在我大致想明白了, 对于一条规则的实例而言, "
      "我们需要知道它的前提 (可能多个) 和结论. "
      "当然, 这仍然相当有问题, 例如结论不止一个, "
      "以及前提的证明不按顺序排布的话 (我觉得暂时有必要加上这条限制), ... "
      "或许等待很久以后施工了.")
   (H2 "关于SVG")
   (P "虽然整个博客的每个图片都是基于SVG的, "
      "但是目前SMathML提供的对于SVG的包装相当有限, "
      "这是不可避免需要改进的地方. 首先, 或许我应该改进"
      (Code "&lt;path>") "标签的属性" (Code "d")
      "的表达, 因为它可以视为SVG的一个子语言. "
      )
   (H2 "关于某种形式的自动输出")
   (P "我的个人博客将所有的源文件都放在了一个路径下. "
      "因此, 其实从原则上说, 可以无需手工" (Code "require")
      "那么多文件, 其实可以达成某种形式的自动化. "
      "不过, 这是尚未完成的事情.")
   (P "除了自动" (Code "require")
      ", 生成网页也不应该逐个调用" (Code "emitXml")
      ", 也应该能够自动化. "
      "并且, 目录性质的页面也应该可以自动构建. "
      "总之, 有很多可做的用于简化生活的事情.")
   
   ))