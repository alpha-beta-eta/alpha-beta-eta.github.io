#lang racket
(provide intro.html)
(require SMathML)
(define intro.html
  (TmPrelude
   #:title "网站介绍"
   #:css "styles.css"
   (H1 "网站介绍")
   (let ((SMathML:link
          (A "SMathML" #:attr* '((href "https://github.com/alpha-beta-eta/SMathML"))))
         (source:link
          (A "源代码" #:attr* '((href "https://github.com/alpha-beta-eta/alpha-beta-eta.github.io")))))
     (P "我的兴趣在于可复合性所适用的领域, 例如逻辑学和编程语言. 我也对于Lisp语族和古典AI抱有特别的兴趣, "
        "特别是其作为表达想法的媒介的方面. 此博客使用" SMathML:link "生成, 一个嵌入Racket的用于表达"
        "MathML和HTML的DSL, 博客的" source:link "可供感兴趣的读者借鉴. 另外值得一说的是, "
        "目前对于MathML支持最好的是Gecko内核, 因而最好使用Firefox浏览器阅读此博客."))
   (P "这个网站上的绝大部分内容都不是我的原创, 即便可以算是原创的内容几乎也完全不是新的, "
      "你总是可以在其他书籍或者材料中找到. 我写下的这些东西主要是为了备忘, "
      "以免我迷失在先验之海当中. 这个网站之中显然包含一些错误, 不论是出于理解问题还是不小心的失误, "
      "也包含一些并不能算得上错但是并无任何价值的一般论, 有的时候我会打磨一些内容, "
      "但是更多时候我就选择撂在那里. 即便如此, 我也已经感到我花了严重过多的时间在无关紧要的事情上. "
      "正如Boltzmann所言, 优雅应该留给裁缝.")
   ))