#lang racket
(provide cat_awodey.html)
(require SMathML)
(define $range (Mi "range"))
(define (&range f) (app $range f))
(define @compose (@lize &compose))
(define $dom (Mi "dom"))
(define $cod (Mi "cod"))
(define (&dom f) (app $dom f))
(define (&cod f) (app $cod f))
(define $iso (Mo "&cong;"))
(define-infix*
  (&iso $iso)
  
  )
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
      (func (&compose $g $f) $A $C) ", 其由"
      (MB (&cm (&= (app (@compose $g $f) $a)
                   (app $g (app $f $a))) (∈ $a $A)))
      "给定. 函数复合" $compose "是结合性的, 即若再有一个函数"
      (func $h $C $D) ", 那么"
      (MB (&assoc &compose $h $g $f) ".")
      "这里的函数相等显然是外延相等 (体现为逐点的函数值相等), 即对于每个" (∈ $a $A) "有"
      (MB (&= (app (@compose (@compose $h $g) $f) $a)
              (app (@compose $h (@compose $g $f)) $a)
              (app $h (app $g (app $f $a)))) ".")
      "对于任意的集合" $A ", 存在一个恒等函数" (func $1_A $A $A) ", 其由"
      (&= (app $1_A $a) $a) "定义. 恒等函数在某种意义上是函数复合" $compose "的单位元, 即"
      (MB (&= (&compose $f $1_A) (&compose $1_B $f) $f) ".")
      "对于函数的概念进行抽象或许提供了定义范畴的动机.")
   (H3 "第1.3节 范畴的定义")
   ((definition #:n "1.1")
    "一个" (Em "范畴") "由以下资料构成:"
    (Ul (Li "对象: " (&cm $A $B $C $..h))
        (Li "箭头: " (&cm $f $g $h $..h))
        (Li "对于每个箭头" $f ", 存在两个(箭头所内蕴的)对象"
            (MB (&dom $f) "和" (&cod $f))
            "其被称为" $f "的定义域 (domain) 和陪域 (codomain). 我们记"
            (MB (func $f $A $B))
            "以指明" (&= $A (&dom $f)) "和" (&= $B (&cod $f)) ".")
        (Li "给定箭头" (func $f $A $B) "和" (func $g $B $C) ", 即"
            (MB (&= (&cod $f) (&dom $g)))
            "存在与之对应的箭头"
            (MB (func (&compose $g $f) $A $C))
            "其被称为" $f "和" $g "的复合.")
        (Li "对于每个对象" $A ", 存在与之对应的箭头"
            (MB (func $1_A $A $A))
            "其被称为" $A "的恒等箭头."))
    "以上这些资料需要满足以下法则."
    (Ul (Li "结合律: 对于所有的"
            (&cm (func $f $A $B) (func $g $B $C) (func $h $C $D)) "有"
            (MB (&assoc &compose $h $g $f) "."))
        (Li "单位元: 对于所有的" (func $f $A $B) "有"
            (MB (&= (&compose $f $1_A) (&compose $1_B $f) $f) "."))))
   (P "范畴的定义是全然抽象的, 对象不必是集合, 箭头不必是函数.")
   (H3 "第1.4节 范畴的例子")
   ((definition #:n "1.2")
    "一个范畴" $C:bold "和" $D:bold "之间的" (Em "函子")
    (MB (func $F $C:bold $D:bold))
    "是从对象到对象和从箭头到箭头的映射, 其满足"
    (Ol #:attr* '((type "a"))
        (Li (&= (app $F (func $f $A $B))
                (func (app $F $f) (app $F $A) (app $F $B))) ";")
        (Li (&= (app $F $1_A) (_ $1 (app $F $A))) ";")
        (Li (&= (app $F (&compose $g $f))
                (&compose (app $F $g) (app $F $f))) ".")))
   ((example)
    "一个" (Em "预序") "是一个装备有满足自反性和传递性的二元关系的集合. 若将"
    "集合的元素视为对象, 而两个对象之间存在唯一的箭头当且仅当其满足二元关系, "
    "则预序可以被视为范畴.")
   
   (H3 "第1.5节 同构")
   ((definition #:n "1.3")
    "在任意的集合" $C:bold "中, 称箭头" (func $f $A $B) "为" (Em "同构")
    ", 如果存在箭头" (func $g $B $A) "满足"
    (MB (&= (&compose $g $f) $1_A) "且" (&= (&compose $f $g) $1_B) ".")
    "这样的逆显然是唯一的, 我们记" (&= $g (inv $f)) ". 我们称" $A
    "同构于" $B ", 如果其间存在同构, 此时记" (&iso $A $B) ".")
   ((theorem #:n ". Cayley") "每个群都同构于一个置换群.")
   
   (H3 "第1.6节 范畴上的构造")
   (Ol (Li ""
           )
       )
   (H3 "第1.7节 自由范畴")
   (P (B "自由幺半群. ") "我们从字母表" $A "开始. 一个" $A
      "上的词是来自于" $A "的字母构成的有限序列. " $A
      "的Kleene闭包" (&* $A) "被定义为所有" $A "上的词构成的集合. "
      (&* $A) "上我们可以定义所谓的连接运算" $*
      ". 这个运算显然是结合的, 并且长度为零的空序列是其单位元. 因此, "
      (&* $A) "形成了一个幺半群, 其被称为集合" $A
      "上的" (Em "自由幺半群") ". 元素" (∈ $a $A)
      "可以被视为长度为一的词, 即我们有一个函数"
      (MB (&: $i (&cm (&-> $A (&* $A)) (&\|-> $a $a))))
      "虽然说以上的定义算是一种符号滥用 (abuse of notation) 吧. "
      $A "的元素在某种意义上&quot;生成&quot;了这个自由幺半群, 即每个"
      (∈ $w (&* $A)) "都是" $A "的元素的" $* "积. 也就是说, 对于某些"
      (∈ $a_1 $a_2 $..h $a_n $A) "有"
      (&= $w (&* $a_1 $a_2 $..c $a_n)) ".")
   (P "到底何谓&quot;自由&quot;呢? "
      )
   (H3 "第1.8节 基础问题: 大, 小, 局部小")
   (H2 "第2章 抽象结构")
   (H2 "第3章 对偶")
   (H2 "第4章 群和范畴")
   (H2 "第5章 极限和余极限")
   
   ))