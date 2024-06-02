#lang racket
(provide index.html)
(require SMathML racket/date "colorfulize.rkt")
(define index.html
  (Prelude
   #:title "愚者"
   #:css "styles.css"
   (H1 "愚者 " (A "RSS" #:attr* '((href "rss.xml"))) " "
       (A "网站介绍" #:attr* '((href "intro.html"))))
   (P "近来我删去了一些内容, 主要是为了重写. "
      "以下这些框的色彩是随机生成的, 仅是因为看厌了白色而临时为之.")
   ((compose colorfulize columnize)
    (H2 (A "编程语言的邀请" #:attr* '((href "notes/pl.pdf"))) ": "
        (A "程序集" #:attr* '((href "notes/pl.zip"))))
    (H2 (A "SMathML" #:attr* '((href "smathml_everything.html"))))
    (H2 (A "Brzozowski导数" #:attr* '((href "brzozowski.html"))))
    (H2 (A "Hoffman &amp; Kunze" #:attr* '((href "hoffman.html"))))
    (H2 (A "程序设计的邀请" #:attr* '((href "programming.html"))))
    (H2 (A "指称语义学讲义翻译" #:attr*
           '((href "denotational_semantics_notes.html"))))
    (H2 (A "Curry-Howard同构讲义" #:attr* '((href "curry-howard.html"))))
    (H2 (A "The Little Typer翻译" #:attr* '((href "little_typer.html"))))
    (H2 (A "P423笔记" #:attr* '((href "p423.html"))))
    (H2 (A "EoPL3笔记" #:attr* '((href "eopl.html"))))
    (H2 (A "一些没用的笔记" #:attr* '((href "useless.html"))))
    (H2 (A "书籍推荐" #:attr* '((href "books.html"))))
    (H2 (A "有趣的链接" #:attr* '((href "ilinks.html"))))
    (H2 (A "胡言乱语" #:attr* '((href "huyan.html"))))
    (H2 (A "友情链接" #:attr* '((href "links.html"))))
    
    )
   ))