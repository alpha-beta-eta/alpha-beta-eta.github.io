#lang racket
(provide index.html)
(require SMathML)
(define index.html
  (Prelude
   #:title "愚者"
   #:css "styles.css"
   (H1 "愚者 " (A "RSS" #:attr* '((href "rss.xml"))))
   (P "我的兴趣是可复合性 (抽象和组合) 所适用的领域, 例如编程语言和逻辑学. "
      "我也对于古典人工智能和Lisp编程语言抱有特别的兴趣, 特别是其作为表达思想的媒介的方面.")
   (P "重要提示: 你应该使用Firefox浏览器阅读此网站, 因为Chrome等其他浏览器对于我所使用的MathML支持很差. "
      "若你是Android用户, 那么请使用移动版Firefox浏览器, 并打开&quot;桌面版网站&quot;.")
   (P "修改日期 (北京时间): " (current-date-string #:format 'chinese))
   (columnize
    (H2 (A "编程语言的邀请" #:attr* '((href "notes/pl.pdf"))) ": "
        (A "程序集" #:attr* '((href "notes/pl.zip"))))
    (H2 (A "Proofs and Types第一章翻译"
           #:attr* '((href "notes/prot-ch1.pdf"))))
    (H2 (A "一些程序" #:attr* '((href "some_programs.html"))))
    (H2 (A "SMathML论文 (draft)"
           #:attr* '((href "notes/SMathML.pdf"))))
    (H2 (A "SMathML参考" #:attr* '((href "smathml.html"))))
    (H2 (A "Brzozowski导数" #:attr* '((href "brzozowski.html"))))
    (H2 (A "SICM笔记" #:attr* '((href "sicm.html"))))
    (H2 (A "程序设计的邀请" #:attr* '((href "programming.html"))))
    (H2 (A "Linear Algebra (Hoffman &amp; Kunze) 翻译"
           #:attr* '((href "linear_algebra.html"))))
    (H2 (A "Linear Algebra (Hoffman &amp; Kunze) 习题作答"
           #:attr* '((href "linear_algebra_exercises.html"))))
    (H2 (A "EoPL3笔记" #:attr* '((href "eopl.html"))))
    (H2 (A "Geometric Algebra (E. Artin) 翻译"
           #:attr* '((href "geometric_algebra.html"))))
    (H2 (A "Lem编辑器使用笔记"
           #:attr* '((href "lem_editor.html"))))
    (H2 (A "LearningZIL笔记"
           #:attr* '((href "zil.html"))))
    (H2 (A "Lectures on the Curry-Howard Isomorphism翻译"
           #:attr* '((href "curry-howard.html"))))
    (H2 (A "The Ship of Theseus"
           #:attr* '((href "Ship-of-Theseus.html"))))
    (H2 (A "Miscellaneous Notes" #:attr* '((href "misc.html"))))
    (H2 (A "Computation Structures笔记"
           #:attr* '((href "computation_structures.html"))))
    (H2 (A "书籍推荐" #:attr* '((href "books.html"))))
    (H2 (A "混沌空间" #:attr* '((href "https://bianshuyan.github.io/"))))
    (H2 (A "友情链接" #:attr* '((href "links.html"))))
    (H2 (A "SMathML Reference" #:attr* '((href "smathml_ref.html"))))
    
    )
   ))