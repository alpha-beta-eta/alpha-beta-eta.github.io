#lang racket
(provide cat_awodey.html)
(require SMathML)
(define $range (Mi "range"))
(define (&range f) (app $range f))
(define cat_awodey.html
  (TmPrelude
   #:title "范畴论笔记"
   #:css "styles.css"
   (H1 "范畴论笔记")
   (H2 "第1章 范畴")
   (H3 "第1.1节 引入")
   (P "范畴论在某种意义上可以被视为" (Em "函数的代数学") ".")
   (H3 "第1.2节 集合的函数")
   (P "令" $f "是从集合" $A "到集合" $B "的函数, 记为" (func $f $A $B)
      ". 若用" (&range $f) "表示" $f "的值域, 那么" (&sube (&range $f) $B)
      ". 现在设我们还有一个函数" (func $g $B $C) ", 那么我们可以构造复合"
      (func (&compose $g $f) $A $C) ". "
      )
   (H2 "第2章 抽象结构")
   (H2 "第3章 对偶")
   (H2 "第4章 群和范畴")
   (H2 "第5章 极限和余极限")
   
   ))