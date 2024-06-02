#lang racket
(provide differential.html)
(require SMathML)
(define $RR^+ (^ $RR $+))
(define (∀ P . x*)
  (&cm P (: $forall (apply &cm x*))))
(define norm dve0)
(define (distance x y)
  (appl $d x y))
(define normd (compose norm &-))
(define (triangle d x y z)
  (&<= (d x z) (&+ (d x y) (d y z))))
(define absd (compose &abs &-))
(define (cBall a r)
  (appl $B^ a r))
(define differential.html
  (TnTmPrelude
   #:title "微分学"
   #:css "styles.css"
   (H1. "微分学")
   (P "微分学深妙精微.")
   (H2. "Banach空间中的微分学")
   (H3. "关于Banach空间和连续线性映射之概念的回顾")
   (P "下面的基域" $KK "是" $RR "或" $CC
      ". 如果" $E "是复向量空间, 那么" $E
      "具有隐式的实向量空间结构, 限于考虑向量"
      (∈ $x $E) "和标量" (∈ $lambda $RR)
      "之数乘" (&i* $lambda $x) "时.")
   (H4. "向量空间" $E "上的范数")
   (P "范数是满足如下条件的映射"
      (func $rho $E $RR^+) ":"
      (let ((norm (lambda (x) (app $rho x))))
        (Ol #:attr* '((type "i"))
            (Li (&= (norm $0) $0) "; "
                (&=> (@= (norm $x) $0)
                     (@= $x $0)) ";")
            (Li (∀ (&<= (norm (&+ $x $y))
                        (&+ (norm $x) (norm $y)))
                   (∈ $x $y $E)) ";")
            (Li (∀ (&= (norm (&i* $lambda $x))
                       (&d* (&abs $lambda) (norm $x)))
                   (∈ $x $E) (∈ $lambda $KK)) ".")))
      "带有给定范数的向量空间称为赋范向量空间. "
      "当范数" $rho "给定时, 往往将向量" $x
      "的范数" (app $rho $x) "记为" (norm $x)
      ". 采用这种记号, 上述条件可以写成:"
      (Ol #:attr* '((type "i"))
          (Li (&= (norm $0) $0) "; "
              (&=> (@= (norm $x) $0)
                   (@= $x $0)) ";")
          (Li (∀ (&<= (norm (&+ $x $y))
                      (&+ (norm $x) (norm $y)))
                 (∈ $x $y $E)) ";")
          (Li (∀ (&= (norm (&i* $lambda $x))
                     (&d* (&abs $lambda) (norm $x)))
                 (∈ $x $E) (∈ $lambda $KK)) ".")))
   ((tcomment)
    "i可以合并为" (&<=> (@= (norm $x) $0) (@= $x $0)) ".")
   (P "设" $E "是一个赋范向量空间, 定义" $E
      "中两点" (&cm $x $y) "之距离如下:"
      (MB (&= (distance $x $y) (normd $x $y)) ".")
      "由iii可得" (commute normd $x $y)
      ", 于是我们有" (commute distance $x $y)
      ". 又由ii立即可以推出"
      (MB (triangle distance $x $y $z) ".")
      "最后, " (&= (distance $x $y) $0) "当且仅当"
      (&= $x $y) ". 因此, " $E "是一个度量空间. "
      "并且和其他度量空间一样, " $E
      "有拓扑结构. 对于这种拓扑, 范数" (&\|-> $u (norm $u))
      "是连续映射" (&-> $E $RR) ", 因为"
      (&<= (absd (norm $u) (norm $v))
           (normd $u $v)) ".")
   ((tcomment)
    (&\|-> $u (norm $u)) "更是一致连续的.")
   (P "对于" (∈ $a $E) "和" (&> $r $0)
      ", " (cBall $a $r) "是心为" $a
      ", 半径为" $r "的球, 其定义如下:"
      (MB (setI (∈ $x $E)
                (&<= (distance $x $a) $r)))
      "或者说"
      (MB (setI (∈ $x $E)
                (&<= (normd $x $a) $r)))
      "考虑一个子集" (&sub $U $E)
      ", "
      )
   ))