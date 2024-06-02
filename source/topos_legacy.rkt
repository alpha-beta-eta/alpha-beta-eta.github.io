#lang racket
(provide topos.html)
(require SMathML)
(define Cat:Set
  (Mi "Set" #:attr* '((mathvariant "bold"))))
(define Cat:C $C:script)
(define app*
  (case-lambda
    ((x) x)
    ((f . arg*) (app f (apply app* arg*)))))
(define (Entry label . x*)
  (apply P (B label) " " x*))
(define $dom (Mi "dom"))
(define (&dom f) (ap $dom f))
(define $cod (Mi "cod"))
(define (&cod f) (ap $cod f))
(define (@compose f g)
  (@ (&compose f g)))
(define $rrarr (Mo "&rrarr;"))
(define $: (Mo ":"))
(define-infix*
  (&rrarr $rrarr)
  (&: $:)
  )
(define arrow
  (case-lambda
    ((obj) obj)
    ((obj arr . arg*)
     (Mrow obj (^^ $-> arr) (apply arrow arg*)))))
(define (&assoc & a b c)
  (&= (& a (@ (& b c)))
      (& (@ (& a b)) c)))
(define marker0
  (Marker
   #:attr*
   '((id "arrow")
     (viewbox "0 0 10 10")
     (refX "5")
     (refY "5")
     (markerWidth "6")
     (markerHeight "6")
     (orient "auto-start-reverse"))
   (Path #:attr* '((d "M 0 3 L 5 5 L 0 7 z")))))
(define (make-pt x y)
  (vector 'pt x y))
(define (pt-x pt)
  (vector-ref pt 1))
(define (pt-y pt)
  (vector-ref pt 2))
(define (make-vec x y)
  (vector 'vec x y))
(define (vec-x vec)
  (vector-ref vec 1))
(define (vec-y vec)
  (vector-ref vec 2))
(define (make-arr p v s) (vector 'arr p v s))
(define (arr-p arr) (vector-ref arr 1))
(define (arr-v arr) (vector-ref arr 2))
(define (arr-s arr) (vector-ref arr 3))
(define vec:N (make-vec 0 -1))
(define vec:S (make-vec 0 1))
(define vec:W (make-vec -1 0))
(define vec:E (make-vec 1 0))
(define vec:SE (make-vec 1 1))
(define vec:NE (make-vec 1 -1))

(define topos.html
  (TnTmPrelude
   #:title "Topos: 逻辑的范畴分析"
   #:css "styles.css"
   (H1 "Topos: 逻辑的范畴分析")
   (H2 "第1章 数学=集合论?")
   (Blockquote
    "无人可将我们从Cantor创造的天堂之中驱离. &mdash;&mdash; David Hilbert")
   (H3 "第1.1节 集合论")
   (P "被称为集合论的这个领域所坐落于的基本概念是集合成员所属. 一个集合最初"
      "可简单地被认为是一个对象的合集, 而这些对象被称为是这个合集的元素. "
      
      )
   (H4 "子集")
   
   (H4 "Russell悖论")
   
   (H4 "NBG")
   
   (H4 "ZF")
   (Entry "分离原理." "给定一个集合" $A "和一个条件" (app $phi $x)
          ", 存在一个集合其元素恰是" $A "中那些满足" (app $phi $x)
          "的成员.")
   (P "该集合被记作"
      
      )
   (H4 "一致性")
   
   (H3 "第1.2节 数学基础")
   (H3 "第1.3节 作为集合论的数学")
   (H2 "第2章 范畴是什么")
   (H3 "第2.1节 函数作为集合?")
   
   (H3 "第2.2节 函数的复合")
   
   (H3 "第2.3节 范畴: 最初的例子")
   ((definition)
    "一个范畴" $C:script "由以下资料构成."
    (Ol (Li "一个被称为" $C:script "对象的东西的合集;")
        (Li "一个被称为" $C:script "箭头的东西的合集;")
        (Li "赋予每个" $C:script "箭头" $f "一个" $C:script "对象" (&dom $f)
            " (" $f "的&quot;domain&quot;) 和一个" $C:script "对象" (&cod $f) " (" $f
            "的&quot;codomain&quot;) 的运算. 如果" (&= $a (&dom $f)) "而" (&= $b (&cod $f))
            ", 我们将其呈现为"
            (MB (func $f $a $b) "或者" (arrow $a $f $b) ";"))
        (Li "一个运算, 其赋予每对" $C:script "箭头" (tupa0 $g $f) ", 若满足"
            (&= (&dom $g) (&cod $f)) ", 以一个" $C:script "箭头" (&compose $g $f)
            ", 即" $f "和" $g "的复合, 并且有" (&= (&dom (@compose $g $f)) (&dom $f))
            "和" (&= (&cod (@compose $g $f)) (&cod $g)) ", 换句话说就是"
            (func (&compose $g $f) (&dom $f) (&cod $g)) ". 而且, 如此可得以下条件:"
            (Blockquote
             "结合律: 给定" $C:script "对象和" $C:script "箭头的配置"
             (MB (arrow $a $f $b $g $c $h $d))
             "那么" (&assoc &compose $h $g $f) ".")
            "结合律断言了具有以下形式的图"
            (Div
             (Svg
              #:attr* '((width "240") (height "135") (stroke "black"))
              (Defs marker0)
              
              ))
            "交换."
            )
        )
    )
   (H2 "第3章 箭头而不是Epsilon")
   (Blockquote
    "思想的世界并不会一下子就向我们敞开. 我们必须持续不断地在我们的意识之中重新创造它."
    " &mdash;&mdash; Ren&eacute; Thom")
   (H3 "第3.1节 单态的箭头")
   (P "一个集合函数" (func $f $A $B) "被称为是" (Em "单射的") ", 或者" (Em "一一的")
      ", 如果没有两个不同的输入会给出相同的输出, 即对于输入" (&in (&cm $x $y) $A) ","
      (Blockquote
       "如果" (&= (app $f $x) (app $f $y)) ", 那么" (&= $x $y) ".")
      "现在让我们取一个单射" (func $f $A $B) "和两个&quot;平行&quot;的函数"
      (&: (&cm $g $h) (&rrarr $C $A)) "使得"
      (Div "(还没画的交换图)")
      "交换, 即" (&= (&compose $f $g) (&compose $f $h)) ".")
   (P "那么, 对于" (&in $x $C) ", 我们有" (&= (app (&compose $f $g) $x) (app (&compose $f $h) $x))
      ", 即" (&= (app* $f $g $x) (app* $f $h $x)) ". 但是因为" $f "是单射的, 这意味着"
      (&= (app $g $x) (app $h $x)) ". 因此" $g "和" $h ", 对于每个输入给出了相同的输出, 是相同的函数. "
      "我们证明了单射的" $f "是&quot;左可消去的&quot;, 即"
      (Blockquote
       "每当" (&= (&compose $f $g) (&compose $f $h)) ", 那么" (&= $g $h) ".")
      "从另一方面来说, 如果" $f "具有左可消去性质, 那么它必然是单射的. 为了看出这点, 我们取" $A
      "中" $x "和" $y "满足" (&= (app $f $x) (app $f $y)) "."
      (Div "(图还没画)")
      "指令&quot;" (&= (app $g $0) $x) "&quot;和&quot;" (&= (app $h $0) $y)
      "&quot;建立了一对从" (setE $0) " (即序数" $1 ") 到" $A "的函数" $g "和" $h
      "满足" (&= (&compose $f $g) (&compose $f $h)) ". 根据左可消去律, " (&= $g $h)
      ", 于是" (&= (app $g $0) (app $h $0)) ", 即" (&= $x $y) ".")
   (P "因此我们看到" Cat:Set "中的单射箭头恰是那些左可消去的箭头. 要义在于后一种性质在陈述时"
      "仅需引用箭头, 而这导向了以下的抽象定义:")
   (P "一个范畴" Cat:C "中的箭头" (func $f $a $b) "在" Cat:C "中是单态的, 如果对于任意一对平行的"
      Cat:C "箭头" (&: (&cm $g $h) (&rrarr $c $a)) ", "
      )
   (H3 "第3.2节 满态的箭头")
   
   (H2 "第4章 介绍Topos")
   
   ))