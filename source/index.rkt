#lang racket
(provide index.html)
(require SMathML racket/date "colorfulize.rkt")
(define index.html
  (Prelude
   #:title "愚者"
   #:css "styles.css"
   (H1 "愚者 " (A "RSS" #:attr* '((href "rss.xml"))) " "
       (A "网站介绍" #:attr* '((href "intro.html"))))
   (P "我写下这些笔记的目的, 主要是为了遗忘.")
   (P "编程语言和证明论是一体两面.")
   (columnize
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
    (H2 (A "The Zipper翻译" #:attr* '((href "zipper.html"))))
    (H2 (A "一些程序" #:attr* '((href "some_programs.html"))))
    (H2 (A "Proofs and Types第一章翻译"
           #:attr* '((href "notes/prot-ch1.pdf"))))
    (H2 (A "EoPL3笔记" #:attr* '((href "eopl.html"))))
    (H2 (A "一些没用的笔记" #:attr* '((href "useless.html"))))
    (H2 (A "书籍推荐" #:attr* '((href "books.html"))))
    (H2 (A "有趣的链接" #:attr* '((href "ilinks.html"))))
    (H2 (A "胡言乱语" #:attr* '((href "huyan.html"))))
    (H2 (A "友情链接" #:attr* '((href "links.html"))))
    #;
    (H2 (A "书评" #:attr* '((href "reviews.html"))))
    (H2 (A "范畴论笔记" #:attr* '((href "cat_awodey.html"))))
    #;
    (H2 (A "(瞎写的)游戏编程" #:attr* '((href "game_programming.html"))))
    (H2 (A "关于数组语言的随想" #:attr* '((href "array.html"))))
    (H2 (A "摆弄SICP图形语言" #:attr* '((href "picture.html"))))
    (H2 (A "The Ship of Theseus" #:attr* '((href "Ship-of-Theseus.html"))))
    (H2 (A "monad的Schemer之见" #:attr* '((href "monad.html"))))
    
    )
   ))