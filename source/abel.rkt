#lang racket
(provide abel.html)
(require SMathML)
(define (∈ . arg*)
  (let-values (((x* A*) (split-at-right arg* 1)))
    (let ((A (car A*)))
      (&in (apply &cm x*) A))))
(define ((answer #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "answer")))
   (B (format "解答~a." n)) " " x*))
(define app*
  (case-lambda
    ((f x) (app f x))
    ((f . arg*) (app f (apply app* arg*)))))
(define $: (Mo ":"))
(define-infix*
  (&: $:)
  
  )
(define (&map f A B x fx)
  (&: f (&cm (&-> A B) (&\|-> x fx))))
(define abel.html
  (TnTmPrelude
   #:title "Abel定理"
   #:css "styles.css"
   (H1 "Abel定理")
   (P "注记: 本书是Arnold为高中生写的讲义.")
   (H2 "第1章 群")
   (H3 "第1.1节 例子")

   (H3 "第1.2节 变换群")

   (H3 "第1.3节 群")
   ((exercise #:n "19")
    "证明对于一个群的任意元素" $a ", " $a
    "的逆元是唯一的.")
   ((proof)
    "设" $b "和" $c "都是" $a "的逆元, 那么"
    (MB (&= $b (&d* $b $e) (&d* $b (@ (&d* $a $c)))
            (&d* (@ (&d* $b $a)) $c)
            (&d* $e $c) $c) "."))
   ((exercise #:n "20")
    "证明"
    (Ol (Li (&= (inv $e) $e) ";")
        (Li "对于一个群的任意元素" $a ", "
            (&= (inv (@ (inv $a))) $a) ".")))
   ((proof)
    (Ol (Li "因为" (&= (&d* $e $e) $e) ", 所以"
            (&= (inv $e) $e) ".")
        (Li "因为" (&= (&d* $a (inv $a)) (&d* (inv $a) $a) $e)
            ", 所以" $a "是" (inv $a) "的(唯一的)逆元, 即"
            (&= (inv (@ (inv $a))) $a) ".")))
   ((exercise #:n "24")
    "令" $a "和" $b "是群" $G "的元素, 证明方程"
    (&= (&i* $a $x) $b) "和" (&= (&i* $y $a) $b)
    "在" $G "中均有唯一解.")
   ((proof)
    "显然, " (&i* (inv $a) $b) "是" (&= (&i* $a $x) $b)
    "的一个解, " (&i* $b (inv $a)) "是" (&= (&i* $y $a) $b)
    "的一个解. 如果" $x_1 "和" $x_2 "都是" (&= (&i* $a $x) $b)
    "的解, 那么"
    (MB (&= $x_1 (&i* (inv $a) (@i* $a $x_1))
            (&i* (inv $a) (@i* $a $x_2))
            $x_2) ".")
    "如果" $y_1 "和" $y_2 "都是" (&= (&i* $y $a) $b) "的解, 那么"
    (MB (&= $y_1 (&i* (@i* $y_1 $a) (inv $a))
            (&i* (@i* $y_2 $a) (inv $a))
            $y_2) "."))
   ((exercise #:n "25")
    "如果对于每个群" $G "的元素" $a "有" (&= (&d* $a $a) $e)
    ", 证明" $G "是交换的.")
   ((proof)
    "对于" $G "的任意元素" $a "和" $b ", 有"
    (MB (&= $e
            (&d* (@ (&d* $a $b)) (@ (&d* $a $b)))
            (&d* $a (@ (&d* $b $a)) $b)
            (&d* $e $e)
            (&d* (@ (&d* $a $a)) (@ (&d* $b $b)))
            (&d* $a (@ (&d* $a $b)) $b)) ",")
    "即"
    (MB (&= (&d* $a (@ (&d* $b $a)) $b)
            (&d* $a (@ (&d* $a $b)) $b)) ",")
    "故"
    (MB (&= (&d* $a $b) (&d* $b $a)) "."))
   
   (H3 "第1.4节 循环群")

   (H3 "第1.5节 同构")
   ((exercise #:n "44")
    "令" (func $phi_1 $G_1 $G_2) "和" (func $phi_2 $G_2 $G_3)
    "是同构, 证明" (func (&i* $phi_2 $phi_1) $G_1 $G_3)
    "也是同构.")
   ((proof)
    "对于" (&in (&cm $a $b) $G_1) ", "
    (MB (deriv (app (@i* $phi_2 $phi_1) (&d* $a $b))
               (app* $phi_2 $phi_1 (&d* $a $b))
               (app $phi_2 (&d* (app $phi_1 $a) (app $phi_1 $b)))
               (&d* (app* $phi_2 $phi_1 $a)
                    (app* $phi_2 $phi_1 $b))
               (&d* (app (@i* $phi_2 $phi_1) $a)
                    (app (@i* $phi_2 $phi_1) $b))))
    (&i* $phi_2 $phi_1) "为双射是显然的.")
   ((exercise #:n "47")
    "令" (func $phi $G $F) "是一个同构, 证明" (&= (app $phi $e_G) $e_F)
    ", 其中" $e_G "和" $e_F "分别是" $G "和" $F "的幺元.")
   ((proof)
    "对于任意的" (&in $a_F $F) ", 存在" (&in $a_G $G) "满足"
    (&= (app $phi $a_G) $a_F) ", 那么"
    (MB (deriv (&d* (app $phi $e_G) $a_F)
               (&d* (app $phi $e_G) (app $phi $a_G))
               (app $phi (&d* $e_G $a_G))
               (app $phi $a_G)
               $a_F))
    (MB (deriv (&d* $a_F (app $phi $e_G))
               (&d* (app $phi $a_G) (app $phi $e_G))
               (app $phi (&d* $a_G $e_G))
               (app $phi $a_G)
               $a_F))
    "于是" (app $phi $e_G) "的确是" $F "的幺元, 即"
    (&= (app $phi $e_G) $e_F) ".")
   
   (H3 "第1.6节 子群")

   (H3 "第1.7节 直积")
   ((exercise #:n "70")
    "设群" $G "有" $n "个元素而群" $H "有" $k "个元素, 那么群" (&c* $G $H)
    "有多少元素?")
   ((answer) "显然是" (&i* $n $k) "个.")
   ((exercise #:n "71")
    "证明群" (&c* $G $H) "和" (&c* $H $G) "是同构的.")
   ((proof)
    "考虑映射" (&map $phi (&c* $G $H) (&c* $H $G) (tu0 $g $h) (tu0 $h $g))
    ", 这是显然的同构.")
   ((exercise #:n "73")
    "设" $G "和" $H "是交换群, 那么证明" (&c* $G $H) "也是交换的.")
   ((proof)
    "对于" (&in (&cm (tu0 $g_1 $h_1) (tu0 $g_2 $h_2)) (&c* $G $H)) ","
    (MB (deriv (&d* (tu0 $g_1 $h_1) (tu0 $g_2 $h_2))
               (tu0 (&i* $g_1 $g_2) (&i* $h_1 $h_2))
               (tu0 (&i* $g_2 $g_1) (&i* $h_2 $h_1))
               (&d* (tu0 $g_2 $h_2) (tu0 $g_1 $h_1)))))
   ((exercise #:n "74")
    "令" $G_1 "是" $G "的子群, " $H_1 "是" $H "的子群, 证明"
    (&c* $G_1 $H_1) "也是" (&c* $G $H) "的子群.")
   ((proof)
    "显然" (&c* $G_1 $H_1) "非空. 对于" (∈ (tu0 $g $h) (tu0 $g^ $h^) (&c* $G_1 $H_1)) ","
    (MB (deriv0 (&d* (tu0 $g $h) (tu0 $g^ $h^)) $=
                (tu0 (&i* $g $g^) (&i* $h $h^)) $in
                (&c* $G_1 $H_1)))
    "对于" (∈ (tu0 $g $h) (&c* $G_1 $H_1)) ","
    (MB (deriv0 (inv (tu0 $g $h)) $=
                (tu0 (inv $g) (inv $h)) $in
                (&c* $G_1 $H_1))))
   
   (H3 "第1.8节 陪集和Lagrange定理")
   ((exercise #:n "81")
    "设元素" $y "属于由元素" $x "生成的" $H "的左陪集, 证明由元素" $x "和" $y
    "生成的" $H "的左陪集恰好是相等的.")
   ((proof)
    "因为" (&in $y (&i* $x $H)) ", 所以存在" (&in $h_0 $H) "满足"
    (&= $y (&i* $x $h_0)) ", 于是" (&= $x (&i* $y (_^ $h $0 $-1)))
    ". 对于" (&in $g (&i* $x $H)) ", 存在" (&in $h_1 $H) "满足"
    (&= $g (&i* $x $h_1)) ", 那么"
    (MB (deriv0 $g $= (&i* $x $h_1)
                $= (&i* (@i* $y (_^ $h $0 $-1)) $h_1)
                $= (&i* $y (@i* (_^ $h $0 $-1) $h_1))
                $in (&i* $y $H)))
    "对于" (&in $g (&i* $y $H)) ", 存在" (&in $h_2 $H) "满足"
    (&= $g (&i* $y $h_2)) ", 那么"
    (MB (deriv0 $g $= (&i* $y $h_2)
                $= (&i* (@i* $x $h_0) $h_2)
                $= (&i* $x (@i* $h_0 $h_2))
                $in (&i* $x $H)))
    "综上所述, " (&= (&i* $x $H) (&i* $y $H)) ".")
   ((exercise #:n "82")
    "设由元素" $x "和" $y "生成的两个" $H "的左陪集拥有相同的元素, 那么证明它们恰好相等.")
   ((proof)
    "设" $z "是" (&i* $x $H) "和" (&i* $y $H) "的共同元素, 那么根据练习81我们知道"
    (&= (&i* $x $H) (&i* $z $H)) "和" (&= (&i* $y $H) (&i* $z $H)) ", 即"
    (&= (&i* $x $H) (&i* $y $H)) ".")
   ((exercise #:n "83")
    "证明任意元素的阶都整除群的阶.")
   ((proof)
    "因为任意元素的阶就是其生成的循环子群的阶, 所以显然.")
   ((exercise #:n "84")
    "证明任意素数阶的群都是循环群, 并且除了" $e "都是其生成元.")
   ((proof)
    
    )
   ((exercise #:n "85")
    "如果一个群" $G "有" (Mn "31") "个元素, 那么" $G "有多少个子群?")
   ((answer)
    "鉴于" (Mn "31") "是素数, 所以" $G "只有平凡子群, 也就是" $2 "个子群.")
   
   (H3 "第1.9节 内自同构")
   ((exercise #:n "94")
    "证明内自同构是一个群到自身的同构.")
   ((proof)
    "对于群" $G "和" (∈ $g $G) ", 考虑内自同构" (&map $phi_g $G $G $h (&i* $g $h (inv $g)))
    ". 如果" (&= (app $phi_g $h_1) (app $phi_g $h_2)) ", 即"
    (&= (&i* $g $h_1 (inv $g)) (&i* $g $h_2 (inv $g))) ", 那么显然有"
    (&= $h_1 $h_2) ", 于是" $phi_g "是单射. 对于" (∈ $h $G) ", 我们知道"
    (&= (app $phi_g (&i* (inv $g) $h $g)) $h) ", 于是" $phi_g "是满射. 对于"
    (∈ $h_1 $h_2 $G) ","
    (let ((phi_g (lambda (h) (&i* $g h (inv $g))))
          (&phi_g (lambda (h) (app $phi_g h))))
      (MB (deriv (&d* (&phi_g $h_1) (&phi_g $h_2))
                 (&i* (@ (phi_g $h_1)) (@ (phi_g $h_2)))
                 (phi_g (&i* $h_1 (@i* (inv $g) $g) $h_2))
                 (phi_g (@i* $h_1 $h_2))
                 (&phi_g (&i* $h_1 $h_2)))))
    "也就是说, " $phi_g "的确是一个群同态. 综上所述, " $phi_g "是一个自同构.")
   
   (H3 "第1.10节 正规子群")
   ((exercise #:n "99")
    "证明交换群的每个子群都是正规子群.")
   ((proof) "显然, 因为对于" (∈ $h $G) ", " (&= (app $phi_g $h) $h) ".")
   ((exercise #:n "103")
    "证明正规子群族之交仍然是正规子群.")
   ((proof)
    "我们已经知道这仍然是一个子群, 正规性也是显然的.")
   ((exercise #:n "104")
    "群" $G "中所有与其他每个元素交换的元素构成的集合被称为群" $G
    "的中心, 证明它是一个正规子群.")
   ((proof)
    "设" $G "的中心为" $Z ". 我们首先证明" $Z "是一个子群. 我们注意到" (∈ $e $Z)
    ", 这是显然的. 其次, 对于" (∈ $z_1 $z_2 $Z) ", " (∈ (&i* $z_1 $z_2) $Z)
    "也是显然的, 因为对于每个" (∈ $g $G) ", 我们有"
    (MB (deriv (&i* $g (@i* $z_1 $z_2))
               (&i* (@i* $g $z_1) $z_2)
               (&i* (@i* $z_1 $g) $z_2)
               (&i* $z_1 (@i* $g $z_2))
               (&i* $z_1 (@i* $z_2 $g))
               (&i* (@i* $z_1 $z_2) $g)))
    "最后, 对于" (∈ $z $Z) ", 有对于每个" (∈ $g $G) ", "
    (&= (&i* $g $z) (&i* $z $g)) ", 于是" (&= (&i* (inv $z) $g) (&i* $g (inv $z)))
    ", 即" (∈ (inv $z) $Z) ". 综上所述, " $Z "是一个子群. 接下来证明" $Z
    "是正规子群, 然而这是自明的.")
   
   (H2 "第2章 复数")
   (H3 "第2.1节 域和多项式")
   (H3 "第2.2节 复数域")
   (H3 "第2.3节 复数域的唯一性")
   (H3 "第2.4节 复数的几何表示")
   (H3 "第2.5节 复数的三角表示")
   (H3 "第2.6节 连续性")
   (H3 "第2.7节 连续曲线")
   
   ))