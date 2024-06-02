#lang racket
(provide index.html)
(require SMathML "utils.rkt")
(define index.html
  (Prelude
   #:title "愚者"
   #:css "styles.css"
   (H1 "愚者 " (A "RSS" #:attr* '((href "rss.xml"))) " "
       (A "网站介绍" #:attr* '((href "intro.html"))))
   (columnize
    (H2 (LINK "编程语言的邀请" "notes/pl.pdf") ": "
        (LINK "程序集" "notes/pl.zip"))
    (TITLE "SMathML" "smathml_everything.html")
    (TITLE "Brzozowski导数" "brzozowski.html")
    (TITLE "Hoffman &amp; Kunze" "hoffman.html")
    (TITLE "程序设计的邀请" "programming.html")
    (TITLE "指称语义学讲义翻译" "denotational_semantics_notes.html")
    (TITLE "Curry-Howard同构讲义" "curry-howard.html")
    (TITLE "The Little Typer翻译" "little_typer.html")
    (TITLE "P423笔记" "p423.html")
    (TITLE "The Zipper翻译" "zipper.html")
    (TITLE "一些程序" "some_programs.html")
    (TITLE "Proofs and Types第一章翻译" "notes/prot-ch1.pdf")
    (TITLE "EoPL3笔记" "eopl.html")
    (TITLE "一些没用的笔记" "useless.html")
    (TITLE "书籍推荐" "books.html")
    (TITLE "有趣的链接" "ilinks.html")
    (TITLE "胡言乱语" "huyan.html")
    (TITLE "友情链接" "links.html")
    (TITLE "范畴论笔记" "cat_awodey.html")
    (TITLE "关于数组语言的随想" "array.html")
    (TITLE "摆弄SICP图形语言" "picture.html")
    (TITLE "The Ship of Theseus" "Ship-of-Theseus.html")
    (TITLE "monad的Schemer之见" "monad.html")
    (TITLE "证明论和逻辑代数笔记" "proof_theory_and_algebra_in_logic.html")
    (TITLE "语义学讲义笔记" "aczel1997.html")
    (TITLE "实用逻辑和自动推理手册" "automated.html")
    
    )))