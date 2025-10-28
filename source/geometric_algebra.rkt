#lang racket
(provide geometric_algebra.html)
(require SMathML)
;; (define (func0 f A B)
;;   (: A (^^ $-> f) B))
(define (format-num section index)
  (and index
       (format "~a.~a" (cadr (reverse section)) index)))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
(define (Entry name class)
  (define (present %entry attr* . html*)
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define head (format-head name section index))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define id (%entry-id %entry))
    (define href (string-append "#" id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define num (format-num section index))
    (if num
        (Cite `(a ((href ,href)) ,name ,num))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite
                          #:class class)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Theorem "定理" "theorem"))
(define (GlobalEntry name class)
  (define (present %entry attr* . html*)
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define index (%entry-index %entry))
    (define head
      (if index
          (B name (format "~s. " index))
          (B name ". ")))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define id (%entry-id %entry))
    (define href (string-append "#" id))
    (define index (%entry-index %entry))
    (if index
        (Cite `(a ((href ,href)) ,name ,(format "~s" index)))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite
                          #:local? #f #:class class)
            (cons attr* html*)))))
(define-syntax-rule (define-GlobalEntry* (id name class) ...)
  (begin (define id (GlobalEntry name class))
         ...))
(define-GlobalEntry*
  (Axiom "公理" "axiom"))
(define $parallel (Mo "&parallel;"))
(define $nparallel (Mo "&nparallel;"))
(define-infix*
  (&parallel $parallel)
  (&nparallel $nparallel))
(define geometric_algebra.html
  (TnTmPrelude
   #:title "几何代数"
   #:css "styles.css"
   (H1. "几何代数")
   (P "这是一本相当经典的数学著作, "
      "从中可以看出E. Artin深厚的写作功力.")
   (H2. "预备概念")
   (H2. "仿射几何和射影几何")
   (H3. "引论和最初三条公理")
   ((Definition)
    "如果" $l "和" $m "是两条直线满足要么" (&= $l $m)
    ", 要么没有点" $P "同时落于" $l "和" $m
    "之上, 那么我们称" $l "和" $m "平行, 记作"
    (&parallel $l $m) ". 如果" $l "和" $m
    "不平行, 我们记作" (&nparallel $l $m) ".")
   (P "如果" (&nparallel $l $m)
      ", 那么至少存在一个点" $P
      "同时落于" $l "和" $m "之上.")
   ((Axiom #:id "axiom1")
    "给定两个" (Em "相异") "的点" $P "和" $Q
    ", 存在一条" (Em "唯一") "的直线" $l "使得"
    $P "落于" $l "之上且" $Q "落于" $l
    "之上. 我们将其记作" (&= $l (&+ $P $Q)) ".")
   (P "如果" (&nparallel $l $m) ", 那么恰存在一个点"
      $P "同时落于" $l "和" $m "之上. 诚然如此, "
      "假使存在两个这样的点, 那么根据" (Ref "axiom1")
      ", " (&= $l $m) ", 因而" (&parallel $l $m) ".")
   ((Axiom)
    "给定一个点" $P "和一条直线" $l
    ", 存在唯一的直线" $m "使得" $P "落于" $m
    "之上且" (&parallel $m $l) ".")
   ((Theorem)
    (Q "平行") "是一个等价关系.")
   ((proof)
    
    )
   ((Definition)
    "一个由平行直线构成的等价类被称为一个平行直线的束 (pencil).")
   ((Theorem)
    "设存在三个不同的平行直线束" (&cm $pi_1 $pi_2 $pi_3)
    ", 那么"
    )
   (H2. "辛几何和正交几何")
   (H2. "一般线性群")
   (H2. "辛群和正交群的结构")
   ))
#;
(define geometric_algebra.html
  (TmPrelude
   #:title "几何代数"
   #:css "styles.css"
   (H1 "几何代数")
   (H2 "第1章 预备概念")
   (H3 "第1.1节 集合论的概念")
   (P "我们从一列习用符号开始:"
      (MB
       ((Ttable (lambda (d i j)
                  (set-attr* d 'columnalign "left")))
        (&Table
         ((&in $a $S) (: (% "意味着") $a (% "是集合") $S (% "的一个元素.")))
         ((&sub $S $T)
          (: (% "意味着") $S (% "是") $T (% "的一个子集.")))
         ((&cap $S $T)
          (: (% "的意思是集合") $S (% "和") $T
             (% "的交集; 如果其为空, 我们则称它们不相交.")))
         ((&union $S $T)
          (: (% "代表") $S (% "和") $T (% "的并集.")))))))
   (P (: (__ $Cap $i) $S_i) "和" (: (__ $Union $i) $S_i)
      "代表一族被索引集合的交集和并集. 若" (&!= $i $j) "时有"
      (&!= $S_i $S_j) ", 则称" (: (__ $Union $i) $S_i)
      "为无交并. 集合有时用符号" (cur0 $..c) "定义, 其中元素在括号间被枚举出来. "
      "或者, 通过符号" (setI $x $A) ", 其中" $A "是关于" $x "的性质. 这个符号应该读作"
      "&quot;所有具有性质" $A "的" $x "的集合&quot;. 因此, 例如"
      (MB (&= (&cap $S $T) (setI $x (&cm (&in $x $S) (&in $x $T))))))
   (P "若" $f "是从一非空集合" $S "到集合" $T "的映射, 即一对于所有元素"
      (&in $s $S) "有定义的函数" (app $f $s) ", 其值在" $T "之中, 那么我们记成"
      (MB (func $f $S $T) (% "或") (func0 $f $S $T)))
   (P "若有" (func0 $f $S $T) "和" (func0 $g $T $U) ", 那么我们也写成"
      (: $S (^^ $-> $f) $T (^^ $-> $g) $U) ". 如果" (&in $s $S) ", 那么我们可以构造"
      (&in (app $g (app $f $s)) $U) ", 因此得到了一个从" $S "到" $U "的映射, 用"
      (func0 (&i* $g $f) $S $U) "表示. 注意到对于这些映射的&quot;积&quot;, "
      "结合律平凡地成立. 两个因子" (&i* $g $f) "的顺序来源于元素的像的记号"
      (app $f $s) ". 若我们写作" (ap (@ $s) $f) "而不是" (app $f $s) ", 那么记成"
      (&i* $f $g) "而不是" (&i* $g $f) "可能更加自然. 尽管我们仍然将坚持使用记号"
      (app $f $s) " (很少有例外), 读者也应该能够以相反的记号处理一切. "
      "有时写成" $s^f "而不是" (app $f $s) "甚至更加方便, 我们应该注意到在此记号之下有"
      (&= (^ (@ $s^f) $g) (^ $s (&i* $g $f))) ".")
   (P "如果有" (func0 $f $S $T) "和" (&sub $S_0 $S) ", 那么" $S_0
      "所有元素的像的集合记作" (app $f $S_0) ", 其被称为" $S_0 "的像. "
      "当然可以对于" $S "这样做, 那么有" (&sub (app $f $S) $T) ". 若"
      (&= (app $f $S) $T) ", 则称该映射为映上的 (onto), 并称" $f
      "将" $S "映上至" $T ". [译注: 这是英文的传统表达, 现多使用Bourbaki的术语, 称"
      $f "为满射的 (surjective).]")
   (P "令" $T_0 "是" $T "的一个子集. 所有满足" (&in (app $f $s) $T_0) "的"
      (&in $s $S) "构成的集合被称为" $T_0 "的逆像, 用" (app (inv $f) $T_0)
      "表示. 注意到即便在" $T_0 "不为空的情况下" (app (inv $f) $T_0)
      "当然也有可能是空集, 也要记住" (inv $f) "不是一个映射. 对于一个特定的" (&in $t $T)
      ", 记号" (app (inv $f) $t) "的意思是仅包含一个元素" $t "的集合" (cur0 $t) "的逆像. "
      (app (inv $f) $t) "从不包含多于一个元素是有可能发生的, 那么我们称" $f
      "是一一的 (one-to-one) 映射. [译注: 更现代的说法是单射的 (injective).] 如果" $f
      "是一个映上且一一的映射, 那么我们称" $f "是一个一一映上, 或者&quot;一一对应&quot;. "
      "[译注: 更现代的说法是双射 (bijection).] 仅在这种情况下" (inv $f)
      "可以被解释为一个映射" (func0 (inv $f) $T $S) ", 其也是一个一一映上. 注意到"
      (func (&i* (inv $f) $f) $S $S) "和" (func (&i* $f (inv $f)) $T $T)
      "均为恒等映射, 分别在" $S "上和" $T "上.")
   (P "如果" (&!= $t_1 $t_2) "是" $T "的元素, 那么集合" (app (inv $f) $t_1) "和"
      (app (inv $f) $t_2) "是不相交的. 如果" $s "是集合" $S "的一个给定元素并且"
      (&= (app $f $s) $t) ", 那么" $s "就在" (app (inv $f) $t) "中, 这表明" $S
      "是所有集合" (app (inv $f) $t) "的无交并:"
      (MB (&= $S (: (__ $Union (&in $t $T)) (app (inv $f) $t))))
      "集合" (app (inv $f) $t) "之中可能有某些是空集. 我们只保留非空的集合并称"
      "以这些非空的" (app (inv $f) $t) "为元素的集合为" $S_f ". 注意" $S_f
      "的元素是集合, 而不是" $S "的元素. " $S_f "被称为商集, 其元素也被称为等价类. "
      "因此, " $s_1 "和" $s_2 "在相同的等价类之中当且仅当" (&= (app $f $s_1) (app $f $s_2))
      ". 任何给定的" $s "都恰处于一个等价类中. 如果" (&= (app $f $s) $t) ", 那么" $s
      "的等价类是" (app (inv $f) $t) ".")
   (P "现在我们构造一个映射" (func $f_1 $S $S_f) ", 将每个" (&in $s $S) "映射至其等价类. "
      "因此, 如果" (&= (app $f $s) $t) ", 那么" (&= (app $f_1 $s) (app (inv $f) $t))
      ". 该映射为满射. [译注: 自此之后, 我们将用单射, 满射, 双射代替一一, 映上, 一一映上.]")
   (P "接下来, 我们构造一个映射" (func $f_2 $S_f (app $f $S)) ", 将非空的等价类"
      (app (inv $f) $t) "映射至" (&in $t (app $f $S)) ". 如果" (&in $t (app $f $S))
      ", 因而" (&= $t (app $f $s)) ", 那么" $t "是等价类" (app (inv $f) $t)
      "的像, 别无其他. 因此, " $f_2 "是单射也是满射. 若" (&in $s $S) "且" (&= (app $f $s) $t)
      ", 那么" (&= (app $f_1 $s) (app (inv $f) $t)) "并有" (app (inv $f) $t) "在" $f_2
      "下的像为" $t ". 于是, " (&= (app (&i* $f_2 $f_1) $s) $t) ".")
   (P "最终, 我们构造一个非常平凡的映射" (func $f_3 (app $f $S) $T) ", 对于每个" (&in $t (app $f $S))
      "设置" (&= (app $f_3 $t) $t) ". 这个映射不应该被称为恒等映射, 因为它是将一个子集映射至"
      "一个可能更大的集合" $T ". 这样的映射被称为嵌入, 当然是单射的. 对于" (&= (app $f $s) $t)
      ", 我们有" (&= (app (&i* $f_2 $f_1) $s) $t) ", 故" (&= (app (&i* $f_3 $f_2 $f_1) $s) $t)
      ". 我们有" (: $S (^^ $-> $f_1) $S_f (^^ $-> $f_2) (app $f $S) (^^ $-> $f_3) $T)
      ", 于是" (func (&i* $f_3 $f_2 $f_1) $S $T) ". 我们看到我们原本的映射" $f
      "被分解为了三个映射"
      (MB (&= $f (&i* $f_3 $f_2 $f_1))))
   (P "重复一下: " $f_1 "是满射, " $f_2 "是一一对应, " $f_3 "是单射. 我们将称其为映射" $f
      "的典范分解. 术语&quot;典范&quot;或者&quot;自然&quot;可在相当宽泛的意义下应用于这样的"
      "数学构造, 其中对象的选择没有什么自由.")
   (P "作为一个例子, 令" $G "和" $H "是群, " (func $f $G $H) "是一个从" $G "到" $H "的同态, 即"
      "对于所有" (&in (&cm $x $y) $G) "满足" (&= (app $f (&i* $x $y)) (&i* (app $f $x) (app $f $y)))
      "的映射. 设" (&= $x $y $1) " (" $G "的单位元), 我们得到了" (&= (app $f $1) $1)
      " (" $H "的单位元). 置" (&= $y (inv $x)) ", 接着我们得到了"
      (&= (app $f (inv $x)) (^ (bra0 (app $f $x)) $-1)) ". 现在我们将描述" $f
      "的典范分解, 等于说必须首先找到商集" $G_f ". 元素" $x "和" $y "在相同的等价类之中当且仅当"
      (&= (app $f $x) (app $f $y)) ", 或者" (&= (app $f (&i* $x (inv $y))) $1) ", 又或是"
      (&= (app $f (&i* (inv $y) $x)) $1) ". 用" $K "代表" $1 "的逆像, 这意味着"
      (&in (&i* $x (inv $y)) $K) "和" (&in (&i* (inv $y) $x) $K) "都成立 (或者说"
      (&in $x (&i* $K $y)) "和" (&in $x (&i* $y $K)) "都成立). 两个陪集" (&i* $y $K) "和"
      (&i* $K $y) "因此是相同的, 而等价于" $y "的元素" $x "构成了陪集" (&i* $y $K) ". 若" $y
      "已经在" $K "之中, 因而" $y "在" $1 "的等价类之中, 我们就得到了" (&= (&i* $y $K) $K)
      ", 故" $K "是一个群. 左陪集和右陪集的相等意味着" $K "是一个不变子群, 因而我们的商集"
      "不过就是商群 (factor group) " (&/ $G $K) ". 映射" $f_1 "对于每个" (&in $x $G) "联系以陪集"
      (&i* $x $K) "作为像: " (&= (app $f_1 $x) (&i* $x $K)) ". 现在的要点在于" $f_1
      "是一个同态 (也是满射), 的确"
      (&= (app $f_1 (&i* $x $y)) (&i* $x $y $K) (&d* (&i* $x $y $K) $K)
          (&d* $x (&i* $K $y) $K) (&d* (&i* $x $K) (&i* $y $K))
          (&i* (app $f_1 $x) (app $f_1 $y))) ".")
   (P "这个映射被称为从一个群到 (onto) 其商群的典范同态.")
   (P "映射" $f_2 "将" (&i* $x $K) "映射至 (onto) " (app $f $x) ": "
      (&= (app $f_2 (&i* $x $K)) (app $f $x)) ". 因为"
      (&= (app $f_2 (&d* (&i* $x $K) (&i* $y $K)))
          (app $f_2 (&d* (&i* $x $y) $K))
          (app $f (&i* $x $y))
          (&i* (app $f $x) (app $f $y))
          (&i* (app $f_2 (&i* $x $K)) (app $f_2 (&i* $y $K))))
      ", 所以它是一个同态. 既然它是一个一一对应, 那么它也是一个同构, 于是可知商群"
      (&/ $G $K) "同构于像群" (app $f $G) ". " $G "的不变子群" $K
      "被称为映射" $f "的核.")
   (P "映射" $f_3 "不过就是一个嵌入, 因此它是一个到" $H "的同态.")
   (H3 "第1.2节 向量空间上的定理")
   (P "我们将假定读者熟悉向量空间的概念和最基本的性质, 但是将重复其定义, "
      "并讨论一些读者可能不熟悉的方面.")
   ((definition #:n "1.1")
    "一个域" $k " (" $k "不一定是交换的) 上的右向量空间是一个加性群, 并带有一个运算, "
    "联系" (&in $A $V) "和" (&in $a $k) "以" (&in (&i* $A $a) $V) ", 其满足以下法则:"
    (Ol (Li (disL $A $B $a) ";")
        (Li (disR $A $a $b) ";")
        (Li (assoc* $A $a $b) ";")
        (Li (&= (&d* $A $1) $A) "."))
    "其中" (&in (&cm $A $B) $V) ", " (&in (&cm $a $b) $k) ", 并且" $1
    "是" $k "的幺元.")
   (P "对于左向量空间, 这个运算被写成" (&i* $a $A) ", 也有类似的法则成立.")
   (P "令" $V "是一个" $k "上的右向量空间而" $S "是" $V "的任意一个子集. "
      
      )
   (H3 "第1.3节 同态更细致的结构")
   (H3 "第1.4节 对偶与配对")
   (H3 "第1.5节 线性方程")
   (H3 "第1.6节 ")
   (H3 "第1.7节 群论的概念")
   (H3 "第1.8节 域论的概念")
   (H3 "第1.9节 有序域")
   (H3 "第1.10节 赋值")
   
   (H2 "第2章 仿射几何与投影几何")
   (H3 "第2.1节 引论和最初三条公理")
   (P "我们都熟悉解析几何, 其中平面上的点由实数序对" (tu0 $x $y) "描述, 直线由线性方程"
      "描述, 圆锥曲线由二次方程描述. 解析几何使得我们能够将任何初等几何问题规约为仅仅"
      "一个代数问题. 然而, 直线与圆相交的问题提示我们扩大这个系统, 引入一个新的平面, "
      "其中的点是复数的序对. 这种过程显见的泛化如下. "
      )
   (H2 "第3章 辛几何与正交几何")
   (H3 "第3.1节 向量空间上的度量结构")
   (H3 "第3.2节 辛几何与正交几何的定义")
   
   (H2 "第4章 一般线性群")
   (H3 "第4.1节 非交换行列式")
   (P "J. Dieudonn&eacute;将行列式的理论扩展到了非交换的域上. "
      )
   (H3 "第4.2节 " (app (_ (Mi "GL") $n) $k) "的结构")
   (H3 "第4.3节 有限域上的向量空间")
   (H2 "第5章 辛群和正交群的结构")
   
   ))
