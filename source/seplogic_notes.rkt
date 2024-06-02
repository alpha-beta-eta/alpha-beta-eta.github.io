#lang racket
(provide seplogic_notes.html)
(require SMathML)
(define $emp (Mi "emp"))
(define (∃ x p)
  (: $exists x $. p))
(define (∀ x p)
  (: $forall x $. p))
(define $-* (Mo "&minus;&#8270;"))
(define $\|= (Mo "&vDash;"))
(define $disjoint (Mo "&bottom;"))
(define (fetch x) (bra0 x))
(define $cons (Mi "cons"))
(define (&cons . x*)
  (apply appl $cons x*))
(define $dispose (Mi "dispose"))
(define (&dispose e)
  (app $dispose e))
(define-infix*
  (&-* $-*)
  (&\|= $\|=)
  (&disjoint $disjoint))
(define-@lized-op*
  (@\|-> &\|->)
  (@\|= &\|=)
  (@disjoint &disjoint))
(define (sh p)
  (&\|= (&cm $s $h) p))
(define (deno e)
  (bra0 (&abs e)))
(define $_ (Mi "_"))
(define (Triple a b c)
  (: (cur0 a) b (cur0 c)))
(define seplogic_notes.html
  (TnTmPrelude
   #:title "分离逻辑笔记"
   #:css "styles.css"
   (H1. "分离逻辑笔记")
   (P "这是关于分离逻辑 (separation logic) 的笔记.")
   (H2. "引论")
   (P "分离逻辑相较于Hoare逻辑, 允许局部推理.")
   (P "分离逻辑的精髓是" (Q "分离合取/空间合取")
      ", 它允许我们在描述规约时避免提及框架, "
      "而在需要时重新引入框架. "
      "框架指的是与当前操作无关的内存区域.")
   (H2. "状态的模型")
   (P "这里的选择不是唯一的, 而且是ad hoc的.")
   (P "我们将存储 (store) " $s
      "定义为从变量到整数的部分函数, "
      "将堆 (heap) " $h
      "定义为从地址到整数的部分函数. "
      "地址集被定义为自然数集的一个子集. "
      "存储和堆共同构成了状态的模型.")
   (H2. "断言")
   (H3. "句法")
   (MB (set-attr*
        (&Table
         ($true "逻辑真")
         ($false "逻辑假")
         ((&conj $p $q) "合取")
         ((&disj $p $q) "析取")
         ((&=> $p $q) "推出")
         ((&* $p $q) "分离合取")
         ((&-* $p $q) "分离推出")
         ((&= $e $f) "相等")
         ((&\|-> $e $f) "指向(堆中)")
         ($emp "空堆")
         ((∃ $x $p) "存在量化"))
        'columnalign
        "right left"))
   (P $e "和" $f "是整数表达式, "
      $x "是变量, " $p "和" $q
      "是断言.")
   (H3. "语义")
   (P "如果存储" $s "和堆" $h "满足断言" $p
      ", 那么我们记" (sh $p) ".")
   (MB (set-attr*
        (&Table
         ((sh $true) "永远为真")
         ((sh $false) "永远为假")
         ((sh (&conj $p $q))
          (: "如果" (sh $p) "且" (sh $q)))
         ((sh (&disj $p $q))
          (: "如果" (sh $p) "或" (sh $q)))
         ((sh (&=> $p $q))
          (: "如果" (sh $p) "可以推出" (sh $q)))
         ((sh (&= $e $f))
          (: "如果"
             (&= (ap (deno $e) $s)
                 (ap (deno $f) $s)))))
        'columnalign
        "right left"))
   (P "其中" (ap (deno $e) $s)
      "表示" $e "相对于" $s "求值.")
   (P "剩下的断言的语义均依赖于堆" $h ".")
   (MB (sh $emp) ", 如果" (&= $h (setE)))
   (MB (sh (&\|-> $e $f))
       ", 如果"
       (&= $h (setE (&-> (ap (deno $e) $s)
                         (ap (deno $f) $s)))))
   (P "这里表达的是堆" $h
      (Em "恰好")
      "就只有一个位置, "
      "而该位置的内容是" $f "的值.")
   (MB (sh (&* $p $q)))
   (P "分离合取的非形式化语义是: 堆" $h
      "可以被分为两个部分, " $p
      "对于其中一个部分成立, 而" $q
      "对于另外一个部分成立.")
   (MB (sh (&* $p $q))
       ", 如果"
       (∃ (&cm $h_1 $h_2)
          (&conj (@disjoint $h_1 $h_2)
                 (@= (&compose $h_1 $h_2) $h)
                 (@\|= (&cm $s $h_1) $p)
                 (@\|= (&cm $s $h_2) $q))))
   (P "这里的" (&disjoint $h_1 $h_2) "代表"
      $h_1 "和" $h_2 "互不相交, 而"
      (&= (&compose $h_1 $h_2) $h)
      "则代表" (Q "不相交函数复合") ".")
   (P "缩略记号: "
      (&\|-> $e (&cm $f_0 $f_1 $..h $f_n))
      "相当于"
      (&* (@\|-> $e $f_0)
          (@\|-> (&+ $e $1) $f_1)
          $..c
          (@\|-> (&+ $e $n) $f_n))
      ".")
   (MB (sh (&-* $p $q)))
   (P "幻灯片上只给出了非形式化的语义, "
      "语焉不详, 容易引起误解, "
      "以下是形式化的版本:")
   (MB (∀ $h^
          (&=> (@conj (@disjoint $h $h^)
                      (@\|= (&cm $s $h^) $p))
               (@\|= (&cm $s (@compose $h $h^)) $q))))
   (P "换言之, " (&-* $p $q)
      "的语义应该理解为如果要了一份资源" $h^
      "能够让" $p "成立, 那么现有的资源" $h
      "与新的资源" $h^ "合并起来就能让" $q "成立.")
   (H3. $conj "和" $* "之对比")
   (P $conj "和" $* "都是交换的: "
      (&conj $p $q) "当且仅当" (&conj $q $p) ", "
      (&* $p $q) "当且仅当" (&* $q $p) ".")
   (P "当然, 这都是相对于一个任意但固定的模型"
      (&cm $s $h) "而言的.")
   (P $true "和" $emp "分别充当了"
      $conj "和" $* "的单位元: "
      (&conj $p $true) "当且仅当" $p ", "
      (&* $p $emp) "当且仅当" $p ".")
   
   (P (&conj $p (&neg $p)) "是不可满足的, "
      "但显然" (&* $p (&neg $p))
      "则完全不是一回事了.")
   (H2. "程序的构造")
   (MB (set-attr*
        (&Table
         ((&:= $v $e) "变量赋值")
         ((&:= $v (fetch $e)) "fetch赋值")
         ((&:= (fetch $e) $f) "堆变动")
         ((&:= $v (&cons $e_0 $..h $e_n)) "分配赋值")
         ((&dispose $e) "指针弃置")
         
         )
        'columnalign
        "right left"))
   (P (B "fetch赋值: ")
      "(相对于某个存储)对于" $e
      "求值得到一个整数, "
      "这个整数应该是一个地址, "
      "如果堆中没有相应的位置则会报错, "
      "否则的话就取出地址所指向的位置里的内容, "
      "将其赋给该变量.")
   (P (B "堆变动: ")
      "(相对于某个存储)对于" $e
      "求值得到一个整数, "
      "这个整数应该是一个地址, "
      "如果堆中没有相应的位置则会报错, "
      "否则的话就(相对于某个存储)对于" $f
      "求值, 将该地址所指向的位置的内容变为"
      $f "的值.")
   (P (B "分配赋值: ")
      "找出堆中尚未分配的" $n
      "个位置, 设其地址分别为"
      (&cm $l (&+ $l $1) $..h)
      ", 对于" (&cm $e_0 $..h $e_n)
      "求值, 然后对于堆进行扩展, "
      "将这些值依次置于这些位置, "
      "最后将" $l "赋给变量" $v ".")
   (P (B "指针弃置: ")
      "对于" $e "求值得到一个地址, "
      "如果堆中没有相应的位置则会报错, "
      "否则就从堆中去除这个位置.")
   (H2. "公理和推理")
   (H3. "Hoare三元组")
   (MB (Triple $p $C $q))
   (P "此三元组有效, 如果对于任意的存储"
      $s "和堆" $h ", 如果" (sh $p)
      ", 那么" $C "在" $s "和" $h
      "上的执行不会报错, 且经过"
      $C "修改后的状态" (&cm $s^ $h^)
      "满足" $q ".")
   (H3. "一些公理")
   (MB (Triple
        (&\|-> $e $_)
        (&:= (fetch $e) $f)
        (&\|-> $e $f)))
   (MB (Triple
        (&\|-> $e $_)
        (&dispose $e)
        $emp))
   (MB (Triple
        $emp
        (&:= $x (&cons $e_0 $..h $e_n))
        (&\|-> $x (&cm $e_0 $..h $e_n)))
       ", 其中"
       (&cm $e_0 $..h $e_n)
       "不能含有(自由的)" $x)
   
   (H3. "框架规则")
   (MB (&rule
        (Triple $p $C $q)
        (Triple (&* $p $r) $C (&* $q $r))))
   (P "附加条件: " $C
      "所修改的变量不能在"
      $r "中自由出现.")
   (P "就个人而言, 观察到以下事实颇为重要: "
      "如果存储" $s "和堆" $h "满足"
      (&* $p $r) ", 那么" $h "可以分为"
      $h_1 "和" $h_2 ", 且"
      (&\|= (&cm $s $h_1) $p) "和"
      (&\|= (&cm $s $h_2) $r)
      "同时成立. 既然"
      (Triple $p $C $q)
      "是有效的, 那么"
      $C "在" $s "和" $h_1
      "上的执行就不能报错. "
      "这虽然看起来是一个平凡的推论, "
      "但是真正重要的是它限定了" $C
      "产生的副作用只可能影响到" $h_1
      ", 而不可能影响到" $h_2 ".")
   
   ))