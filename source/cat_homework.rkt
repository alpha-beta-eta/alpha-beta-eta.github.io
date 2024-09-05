#lang racket
(provide cat_homework.html)
(require SMathML)
(define ((answer #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "answer")))
   (B (format "解答~a." n)) " " `(,@x* ,Q.E.D.)))
(define Cat:Rel (Mi "Rel" #:attr* '((mathvariant "bold"))))
(define Cat:Sets (Mi "Sets" #:attr* '((mathvariant "bold"))))
(define Cat:Monoids (Mi "Monoids" #:attr* '((mathvariant "bold"))))
(define Cat:Posets (Mi "Posets" #:attr* '((mathvariant "bold"))))
(define $exist (Mo "&exist;"))
(define (&exist q p)
  (: $exist q p))
(define $op (Mi "op"))
(define (&op C) (^ C $op))
(define $cong (Mo "&cong;"))
(define $and (Mo "&amp;"))
(define (powerset X) (app $P X))
(define-infix*
  (&and $and)
  (&cong $cong)
  
  )
(define-@lized-op*
  (@and &and)
  )
(define cat_homework.html
  (TmPrelude
   #:css "styles.css"
   #:title "范畴论作业"
   (H1 "范畴论作业")
   (H2 "作业1")
   ((exercise #:n "1")
    Cat:Rel "的对象是集合, 而其箭头" (func $f $A $B)
    "是从" $A "到" $B "的关系, 即子集" (&sube $f (&c* $A $B))
    ". 等于关系" (setI (∈ (tupa0 $a $a) (&c* $A $A)) (∈ $a $A))
    "是集合" $A "上的恒等箭头. " Cat:Rel "中的复合由"
    (MB (&= (&compose $g $f)
            (setI (∈ (tupa0 $a $c) (&c* $A $C))
                  (&exist $b (@and (∈ (tupa0 $a $b) $f)
                                   (∈ (tupa0 $b $c) $g))))))
    "定义, 其中" (&sube $f (&c* $A $B))
    "而" (&sube $g (&c* $B $C)) "." (Br)
    "证明" Cat:Rel "是一个范畴, 并证明存在函子"
    (func $G Cat:Sets Cat:Rel) ", 其将对象送至自身, 而将每个函数"
    (func $f $A $B) "送至其图, 即"
    (MB (&= (app $G $f)
            (setI (∈ (tupa0 $a (app $f $a)) (&c* $A $B))
                  (∈ $a $A))) "."))
   ((answer)
    
    )
   ((exercise #:n "2")
    "考虑以下范畴的同构, 判断其中哪些成立."
    (Ol #:attr* '((type "a"))
        (Li (&cong Cat:Rel (&op Cat:Rel)))
        (Li (&cong Cat:Sets (&op Cat:Sets)))
        (Li "对于固定的集合" $X ", 其幂集为" (powerset $X) ", 作为偏序集范畴"
            (&cong (powerset $X) (&op (powerset $X))) ", " (powerset $X)
            "中的箭头是子集包含关系.")))
   ((answer)
    
    )
   ((exercise #:n "3")
    (Ol #:attr* '((type "a"))
        (Li "证明" Cat:Sets "中的同构恰是双射.")
        (Li "证明" Cat:Monoids "中的同构恰是双射的同态.")
        (Li "证明" Cat:Posets "中的同构和双射的同态并非等价的概念.")))
   ((answer)
    
    )
   ((exercise #:n "4")
    
    )
   ((exercise #:n "5")
    
    )
   ))