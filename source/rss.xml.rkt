#lang racket
(provide rss.xml)
(require SMathML/rss SMathML/xml racket/date)
(define (current-date-string #:format [format 'rfc2822])
  (date-display-format format)
  (date->string (current-date) #t))
(define rss.xml
  (RC #:title "愚者"
      #:link "https://alpha-beta-eta.github.io/"
      #:description "愚者的主页, 关于逻辑, 语言, 计算."
      (Item (Title "Linear Algebra (Hoffman &amp; Kunze) 翻译")
            (Link "linear_algebra.html")
            (Description "对于线性代数书籍Hoffman &amp; Kunze的翻译, 目前已完成了前8章.")
            (PubDate "Wed, 18 Sep 2024 19:41:36 -0400"))
      (Item (Title "Lectures on the Curry-Howard Isomorphism翻译")
            (Description "这是一本关于Curry-Howard同构的逻辑学书籍, 深入浅出. 目前我还没有翻译多少.")
            (Link "curry-howard.html")
            (PubDate "Sun, 21 Jan 2024 21:16:21 +0800"))
      (Item (Title "程序设计的邀请")
            (Description "定位于初学者的程序设计教程, 尚未完工.")
            (Link "programming.html"))
      (Item (Title "The Little Typer翻译")
            (Description "The Little Typer是一本关于依赖类型的书籍.")
            (Link "little_typer.html"))
      (Item (Title "P423笔记")
            (Description "IUB P423是一门关于编译器的课程.")
            (Link "p423.html"))
      
      ))