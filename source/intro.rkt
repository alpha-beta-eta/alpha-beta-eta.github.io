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
        "目前对于MathML支持最好的是Gecko内核, 因而最好使用Firefox浏览器阅读此博客."
        ))
   ))