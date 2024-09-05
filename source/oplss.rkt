#lang racket
(provide oplss.html)
(require SMathML)
(define oplss.html
  (TmPrelude
   #:title "OPLSS"
   #:css "styles.css"
   (H1 "OPLSS 俄勒冈编程语言暑期学校")
   (P "老实说, 我觉得我压根不懂编程语言. 当然, 这不妨碍我从"
      "OPLSS中汲取智慧. 这里是我的一些翻译, 一些笔记?")
   (H2 "Probabilistic Programming from the Ground Up (2024)")
   (H3 "Lecture 1")
   (P "我们呈现了Tiny PPL, 其句法:"
      (CodeB "pure terms
&lt;p> ::= true
     |  false
     |  &lt;var>
     |  (if &lt;p> &lt;p> &lt;p>)
     |  (disj &lt;p> &lt;p>)
     |  (conj &lt;p> &lt;p>)

effectful or probabilistic terms
&lt;e> ::= (let &lt;var> &lt;e> &lt;e>)
     |  (return &lt;p>)
     |  (flip &lt;theta>)")
      "其语义:" (Br)
      "首先我们考虑pure term的语义, 当然其是相对于环境而言的."
      (MB (func (&db0 $p) 'env 'bool))
      
      )
   ))