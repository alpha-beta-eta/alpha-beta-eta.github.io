#lang racket
(provide hot_02_typechecking.html)
(require SMathML "hot_utils.rkt")
(define $unary (Miv "unary" "sans-serif"))
(define $pos (Miv "pos" "sans-serif"))
(define $1S (: $unit $S))
(define $⊗S (: $⊗:id $S))
(define $⊕S (: $⊕:id $S))
(define $⊗R (: $⊗:id $R))
(define $cut?? (Miv "cut??" "normal"))
(define Γ_I (_ Γ $I))
(define Γ_O (_ Γ $O))
(define Γ_M (_ Γ $M))
(define Ξ_ell (_ Ξ $ell))
(define $B_ell (_ $B $ell))
(define $A^_k (_ $A^ $k))
(define $y:bold^ (&prime $y:bold))
(define $B:bold^ (&prime $B:bold))
(define $⊕A (⊕* $ell $A_ell $L))
(define (Provided . x*)
  (: (% "前提是 ") (apply : x*)))
(define (Caption str)
  (P #:attr* '((style "text-align: center")) str))
(define $vellip
  (~ #:attr* '((linethickness "0") (displaystyle "true"))
     $..v (&= $nat $unary)))
(define (judgment-form)
  (&seq (UnderBrace
         (&cm (&: $y_1 $B_1) $..h (&: $y_n $B_n))
         (% "从这里读取"))
        $P
        (UnderBrace (@: $x $A) (% "写入这里"))))
(define hot_02_typechecking.html
  (TnTmPrelude
   #:title "线性类型检查"
   #:css "styles.css"
   (H1. "线性类型检查")
   (P "Frank Pfenning. 原文标题为"
      (Q "Lecture Notes on Linear Typechecking")
      ", 是CMU 15-417/817: HOT Compilation的第2讲讲义, 2025年1月16日 (星期四).")
   (P "返回" (LINK "HOT编译" "hot.html") "的索引页面; 本文的译名见"
      (LINK "术语翻译对照" "hot_glossary.html") ".")
   (H2. "线性的意义" #:id "significance")
   (P "我们首先稍微探索一下线性定型的限制. 回想一下, 判断中的每个变量"
      $x "和" $y_i "在运行时都代表一个单元的地址."
      (MB (judgment-form))
      "线性保证了每个变量" $y_i "都将被使用, 并且变量" $x
      "将被写入, 假设" $P "会终止. 执行的状态由多个语义对象"
      (&cell $c $v) " (其中所有的地址" $c "都互不相同, 而" $v
      "是一个小值) 以及一些代表正在运行的进程的" (&proc $P)
      "组成. 如果一个格局完全由内存单元组成, 我们就到达了一个终格局. "
      "回想一下这些规则:"
      (MB sax-dynamics)
      (MB sax-▷))
   (P "多重集重写中的一步将格局 (它是无序的) 的一部分与规则的左侧相匹配, "
      "并将其替换为规则的右侧. 因此, 在" (Code "read") "和" (Code "id")
      "的规则中, 左侧的单元从格局中消失了, 这对应于单元的" (Em "释放")
      ". 由于线性定型, 这意味着在计算结束时, "
      "只有从无参数过程的初始目的地可达的单元会在格局中保持分配状态. "
      "换言之, 我们不需要传统的垃圾回收器.")
   (P "一个有趣的观察是, 因为所有的类型都是正的, "
      "我们实际上可以编写释放或复制值的过程. 在现场编写的文件"
      (Code "lec02.sax")
      "中, 我们为一些类型编写了释放和复制函数. "
      "{译注: 原文此处写作" (Code "lex02.sax")
      ", 应为笔误. 讲义中没有列出这个文件, 这里附上课程提供的"
      (Code "lec02.sax") ".}"
      (CodeB "/* Lecture 2 */
/* 15-x17 Spring 2025 */
/* Live code */

type bool = +{'false : 1, 'true : 1}

proc dealloc_bool (d : 1) (x : bool) =
  read x {
  | 'false(u) => id d u
  | 'true(u) => read u ()
                write d ()
  }

proc and (d : bool) (x : bool) (y : bool) =
  read x {
  | 'false(u) => cut w : 1
                    call dealloc_bool w y
                 read w ()
                 write d 'false(u)
  | 'true(u) => read y {
                | 'false(w) => read u {
                               | () => write d 'false(w)
                               }
                | 'true(w) => read u ()
                              write d 'true(w)
                }
  }

type bin = +{'b0 : bin, 'b1 : bin, 'e : 1}

proc dealloc_bin (d : 1) (x : bin) =
  read x {
  | 'b0(x0) => call dealloc_bin d x0
  | 'b1(x1) => call dealloc_bin d x1
  | 'e(u) => id d u
  }

proc dup_bin (d : bin * bin) (x : bin) =
  // write d (x, x)
  read x {
  | 'b0(x) => cut y : bin * bin
                 call dup_bin y x
              read y (y1, y2)
              cut d1 : bin
                write d1 'b0(y1)
              cut d2 : bin
                write d2 'b0(y2)
              write d (d1, d2)
  | 'b1(x) => cut y : bin * bin
                 call dup_bin y x
              read y (y1, y2)
              cut d1 : bin
                write d1 'b1(y1)
              cut d2 : bin
                write d2 'b1(y2)
              write d (d1, d2)
  | 'e(u) => cut d1 : bin
                write d1 'e(u)
             cut d2 : bin
                cut w : 1
                   write w ()
                write d2 'e(w)
             write d (d1, d2)
  }"))
   (H2. "定型的两个问题" #:id "two-problems")
   (P "让我们只看其中两条规则, 并考察实现它们时的问题."
      (MB (&split16
           (&rull
            $id:rule
            (&seq (&: $y $A) (&id $x $y) (@: $x $A)))
           (&rull
            $cut:rule
            (&seq Γ $P (@: $x $A))
            (&seq (&cm Δ (&: $x $A)) $Q $delta)
            (&seq (&cm Γ Δ) (&cut $x $A $P $Q) $delta))))
      "第一个问题在恒等规则中可见: 我们被给定了"
      (&seq (&: $y $B) (&id $x $y) (@: $x $A))
      ", 并且如果" (&= $B $A) "的话, 我们就应该接受这个恒等是良类型的. "
      "但是两个类型何时相等呢? 在当今的许多函数式语言中, 递归类型定义是"
      (Em "生成性的") ", 因此一个类型名称只等于其自身. "
      "但是这里我们有等价递归的定义, 因此一个类型名称等于其定义项. "
      "在存在递归的情况下, 这就产生了判定两个类型是否相等的问题.")
   (P "第二个问题以切规则为例. 每个变量都应当恰好被使用一次, 但是一些变量会出现在"
      $P "中, 另一些会出现在" $Q "中, 那么我们如何" (Q "拆分")
      "结论中的上下文呢?")
   (P "我们将依次处理这两个问题.")
   (H2. "加法式算法定型" #:id "additive")
   (P "假设类型相等的问题可以解决, 以下是设计线性类型检查算法的一种方法. "
      "因为我们不知道如何拆分上下文, 我们就直接把它传给切的两个前提. "
      "对于进行检查的命令, 我们返回在其类型检查中实际用到的上下文. "
      "然后我们将两者组合起来, 并且如果 (例如) 一个变量在两侧都被使用了, 就失败. "
      "这个新的判断是" (&aseq Γ $P (@: $x $A) Ξ) ", 其中"
      (&cm Γ $P $x) "和" $A "是输入, 而如果定型成功, " Ξ "是输出. "
      Ξ "包含了在" $P "中实际用到的变量. 换言之:"
      (Blockquote
       (P "如果" (&aseq Γ $P (@: $x $A) Ξ) ", 那么"
          (&seq Ξ $P (@: $x $A)) "."))
      "反过来:"
      (Blockquote
       (P "如果" (&seq Ξ $P (@: $x $A)) ", 那么对于所有与" Ξ "不相交的" Γ
          ", 都有" (&aseq (&cm Γ Ξ) $P (@: $x $A) Ξ) ".")))
   (P "于是, 切规则的第一个版本可能是"
      (MB (&rull
           $cut??
           (&aseq Γ $P (@: $x $A) Ξ_1)
           (&aseq (&cm Γ (&: $x $A)) $Q $delta Ξ_2)
           (&aseq Γ (&cut $x $A $P $Q) $delta (&\; Ξ_1 Ξ_2))))
      "其中" (&\; Ξ_1 Ξ_2)
      "合并两个不同的上下文, 并检查它们是不相交的 (见下面的定义). "
      "然而, 这并不完全正确. 我们还需要确保" $x "在" $Q
      "的定型中真的被使用了, 并且它没有逃逸出其作用域. 因此我们得到"
      (MB (&rull
           $cut:rule
           (&aseq Γ $P (@: $x $A) Ξ_1)
           (&aseq (&cm Γ (&: $x $A)) $Q $delta Ξ_2)
           (&aseq Γ (&cut $x $A $P $Q) $delta
                  (&\; Ξ_1 (@ (&∖ Ξ_2 $x))))))
      "这里, " (&∖ Ξ_2 $x) "从" Ξ_2 "中移除" (@: $x $A)
      ", 并且也验证了这样的" $x "确实在" Ξ_2 "中. "
      "{译注: 原文此处写作" (Q (: "从" Ξ "中移除"))
      ", 应为笔误; 另外, 原文接下来的一句" (Q "This means,") "没有写完.}")
   (P "我们定义合并运算" (&\; Ξ_1 Ξ_2) ":"
      (MB (set-attr*
           (&Table
            ((&\; (@ (&cm Ξ_1 (&: $x $A))) Ξ_2)
             $= (&cm (@ (&\; Ξ_1 Ξ_2)) (&: $x $A))
             (Provided (&!in $x Ξ_2)))
            ((&\; Ξ_1 (@ (&cm Ξ_2 (&: $x $A))))
             $= (&cm (@ (&\; Ξ_1 Ξ_2)) (&: $x $A))
             (Provided (&!in $x Ξ_1)))
            ((&\; (@ $·) (@ $·)) $= (@ $·) $))
           'columnalign "right center left left"))
      "在其他情况下, 合并失败. 以及减法运算:"
      (MB (set-attr*
           (&Table
            ((&∖ (@ (&cm Ξ (&: $x $A))) $x) $= Ξ $)
            ((&∖ (@ (&cm Ξ (&: $y $B))) $x)
             $= (&cm (@ (&∖ Ξ $x)) (&: $y $B))
             (Provided (&!= $y $x))))
           'columnalign "right center left left"))
      "在剩下的情况下, 减法失败.")
   (P "对于其余的规则, 我们只需要记住其定义性质: 输出上下文" Ξ
      "包含了实际用到的变量. 有一点需要注意: 在" $⊕L "的规则中, 减法"
      (&∖ Ξ_ell $y_ell) "必须在每个分支中产生相同的" Ξ ".")
   (P (MB (&split16
           (&rull
            $id:rule
            (&in (&: $y $A) Γ)
            (&aseq Γ (&id $x $y) (@: $x $A) (@: $y $A)))
           (&rull
            $cut:rule
            (&aseq Γ $P (@: $x $A) Ξ_1)
            (&aseq (&cm Γ (&: $x $A)) $Q $delta Ξ_2)
            (&aseq Γ (&cut $x $A $P $Q) $delta
                   (&\; Ξ_1 (@ (&∖ Ξ_2 $x)))))))
      (MB (&split16
           (&rull
            $unitX
            (&aseq Γ (&write $x (tu0)) (@: $x $unit) (@ $·)))
           (&rull
            $unitL
            (&in (&: $x $unit) Γ)
            (&aseq Γ $Q $delta Ξ)
            (&aseq Γ (&read $x (&=> (tu0) $Q)) $delta
                   (&\; Ξ (@: $x $unit))))))
      (MB (&rull
           $⊗X
           (&cm (&in (&: $x $A) Γ) (&in (&: $y $B) Γ))
           (&aseq Γ (&write $z (tu0 $x $y)) (@: $z (&⊗ $A $B))
                  (&\; (@: $x $A) (@: $y $B)))))
      (MB (&rull
           $⊗L
           (&in (&: $z (&⊗ $A $B)) Γ)
           (&aseq (&cm Γ (&: $x $A) (&: $y $B)) $Q $delta Ξ)
           (&aseq Γ (&read $z (&=> (tu0 $x $y) $Q)) $delta
                  (&\; (@ (&∖ Ξ $x $y)) (@: $z (&⊗ $A $B))))))
      (MB (&rull
           $⊕X
           (@ (∈ $k $L))
           (&in (&: $y $A_k) Γ)
           (&aseq Γ (&write $x (app $k $y)) (@: $x $⊕A) (@: $y $A_k))))
      (MB (&rull
           $⊕L
           (: (@ ((&split 3)
                  (&in (&: $x $⊕A) Γ)
                  (&aseq (&cm Γ (&: $y_ell $A_ell)) $Q_ell $delta Ξ_ell)
                  (&= Ξ (&∖ Ξ_ell $y_ell))))
              (@ (: $forall (∈ $ell $L))))
           (&aseq Γ (&read $x (_ (setE (&=> (app $ell $y_ell) $Q_ell))
                                 (∈ $ell $L)))
                  $delta (&\; Ξ (@: $x $⊕A)))))
      (MB (&rull
           $call:rule
           (&in (@ (&seq (&: $y:bold $B:bold) $p (@: $x $A))) Σ)
           (&aseq Γ (&subst $b:bold $y:bold) (@: $y:bold $B:bold) Ξ)
           (&aseq Γ (&call $p $a $b:bold) (@: $a $A) Ξ)))
      (MB (&rull
           #f
           (&aseq Γ $eta_1 Δ_1 Ξ_1)
           (&aseq Γ $eta_2 Δ_2 Ξ_2)
           (&aseq Γ (tu0 $eta_1 $eta_2) (@ (&cm Δ_1 Δ_2))
                  (@ (&\; Ξ_1 Ξ_2)))))
      (MB (&split16
           (&rull
            #f
            (&aseq Γ (tu0 $·) (@ $·) (@ $·)))
           (&rull
            #f
            (&in (&: $y $A) Γ)
            (&aseq Γ (&subst $y $x) (@: $x $A) (@: $y $A))))))
   (Caption "图1: 加法式算法定型")
   (P "{译注: 原文图中" $⊗L "规则的名字写作" $⊗R ", " $unitL
      "规则的前提中的" $Q "写作" $P ", 应为笔误, 这里都已改正.}")
   (P (Em "减法式") "定型的想法是将判断改为"
      (MB (&aseq Γ_I $P (@: $x $A) Γ_O))
      "其中" Γ_I "是可用的输入, 而" Γ_O "是在" $P
      "中没有被用到的前件. 于是切规则变为"
      (MB (&rull
           $cut:rule
           (&aseq Γ_I $P (@: $x $A) Γ_M)
           (&aseq (&cm Γ_M (&: $x $A)) $Q $delta Γ_O)
           (&!in $x Γ_O)
           (&aseq Γ_I (&cut $x $A $P $Q) $delta Γ_O)))
      "减法式定型的正确性定理更为复杂, 尽管在某些情况下它可能提供更好的错误信息, "
      "因为错误一般在出现时就会被立即检测到. 换言之, 通过将" Γ
      "传给切的两个前提, 我们丢失了减法式版本中存在的一些信息."
      (Blockquote
       (P "如果" (&aseq Γ_I $P (@: $x $A) Γ_O) ", 那么"
          (&seq (&- Γ_I Γ_O) $P (@: $x $A)) ".")))
   (H2. "类型相等" #:id "type-equality")
   (P "考虑类型定义"
      (MB (set-attr*
           (&Table
            ((&type $nat (⊕ $zero $unit $succ $nat)))
            ((&type $unary (⊕ $zero $unit $succ $unary))))
           'columnalign "left"))
      "那么" $nat "和" $unary "恰好寓居着相同的大值: "
      (&cm (&zero) (&succ (&zero))) ", 等等. 因此, 这两个类型应该被视为相等的. "
      "由于其定义中的递归, 我们使用这种相等的一个" (Em "余归纳")
      "定义, 允许无穷推导."
      (MB (&split16
           (&rull $1S (&= $unit $unit))
           (&rull
            $⊗S
            (&= $A_1 $B_1) (&= $A_2 $B_2)
            (&= (&⊗ $A_1 $A_2) (&⊗ $B_1 $B_2)))))
      (MB (&rull
           $⊕S
           (&= $L $K)
           (: (&= $A_ell $B_ell) (@ (: $forall (∈ $ell $L))))
           (&= $⊕A (⊕* $k $B_k $K)))))
   (P "没有关于名称的显式规则, 因为我们可以隐式地将其替换为其定义项. "
      "而这正是无穷推导的问题可能产生之处. 例如, 我们可以构造:"
      (MB (&rull
           $⊕S
           (&rull $1S (&= $unit $unit))
           (&rull
            $⊕S
            (&rull $1S (&= $unit $unit))
            $vellip
            (&= $nat $unary))
           (&= $nat $unary)))
      "那么, 既然我们无法产生无穷推导, 我们如何判定类型相等呢? "
      "关键在于产生它们的有穷表示, 即" (Em "循环推导") ". 如果一个程序中出现了"
      $n "个类型, 那么在任何分支上至多会出现" (^ $n $2)
      "个等式. 因此, 我们最终必然要么失败, 要么成功地以一条像" $1S
      "那样的公理或者一个环来封闭推导.")
   (P "对于这些规则的这种解释是在说: 无论我们尝试多久去寻找反例, 都不存在反例.")
   (H2. "子类型" #:id "subtyping")
   (P "我们的等价递归定义的概念暗示了一个直接的推广. 例如, 如果我们定义"
      (MB (&type $pos (⊕ $succ $nat)))
      "那么每个类型为" $pos "的(闭的, 大的)值也将具有类型" $nat
      ". 因此, 允许"
      (MB (&rull
           $id:rule
           (&seq (&: $y $pos) (&id $x $y) (@: $x $nat))))
      "是完全没有问题的.")
   (P "然而, 另一个方向是行不通的: 我们无法将一个类型为" $nat
      "的小值移入一个要求其内容具有类型" $pos "的单元. "
      "{译注: 原文此处写作" (Q "call") ", 根据上下文应为"
      (Q "cell") ".} 子类型的规则只是类型相等规则的一个小变体."
      (MB (&split16
           (&rull $1S (&<= $unit $unit))
           (&rull
            $⊗S
            (&<= $A_1 $B_1) (&<= $A_2 $B_2)
            (&<= (&⊗ $A_1 $A_2) (&⊗ $B_1 $B_2)))))
      (MB (&rull
           $⊕S
           (&sube $L $K)
           (: (&<= $A_ell $B_ell) (@ (: $forall (∈ $ell $L))))
           (&<= $⊕A (⊕* $k $B_k $K)))))
   (P "但是请记住, 这些规则是余归纳的, 允许无穷推导.")
   (P "然后我们修改每一条最初的定型规则, "
      "使得在我们之前检查两个类型相等的地方都允许子类型."
      (MB (&split16
           (&rull
            $id:rule
            (&<= $A^ $A)
            (&seq (&: $y $A^) (&id $x $y) (@: $x $A)))
           (&rull
            $cut:rule
            (&seq Γ $P (@: $x $A))
            (&seq (&cm Δ (&: $x $A)) $Q $delta)
            (&seq (&cm Γ Δ) (&cut $x $A $P $Q) $delta))))
      (MB (&split16
           (&rull
            $unitX
            (&seq $· (&write $x (tu0)) (@: $x $unit)))
           (&rull
            $unitL
            (&seq Γ $Q $delta)
            (&seq (&cm Γ (&: $x $unit))
                  (&read $x (&=> (tu0) $Q))
                  $delta))))
      (MB (&rull
           $⊗X
           (&<= $A^ $A) (&<= $B^ $B)
           (&seq (&cm (&: $x $A^) (&: $y $B^))
                 (&write $z (tu0 $x $y))
                 (@: $z (&⊗ $A $B)))))
      (MB (&rull
           $⊗L
           (&seq (&cm Γ (&: $x $A) (&: $y $B)) $Q $delta)
           (&seq (&cm Γ (&: $z (&⊗ $A $B)))
                 (&read $z (&=> (tu0 $x $y) $Q))
                 $delta)))
      (MB (&rull
           $⊕X
           (@ (∈ $k $L)) (&<= $A^_k $A_k)
           (&seq (&: $y $A^_k)
                 (&write $x (app $k $y))
                 (@: $x $⊕A))))
      (MB (&rull
           $⊕L
           (: (@ (&seq (&cm Γ (&: $y $A_ell)) $Q_ell $delta))
              (@ (: $forall (∈ $ell $L))))
           (&seq (&cm Γ (&: $x $⊕A))
                 (&read $x (_ (setE (&=> (app $ell $y_ell) $Q_ell))
                              (∈ $ell $L)))
                 $delta)))
      (MB (&rull
           $call:rule
           (&in (@ (&seq (&: $y:bold^ $B:bold^) $p (@: $x^ $A^))) Σ)
           (&<= $A^ $A)
           (&seq Γ (&subst $y:bold $y:bold^) (@: $y:bold^ $B:bold^))
           (&seq Γ (&call $p $x $y:bold) (@: $x $A))))
      (MB (&split16
           (&rull
            #f
            (&seq Γ_1 $sigma_1 Δ_1)
            (&seq Γ_2 $sigma_2 Δ_2)
            (&seq (&\; Γ_1 Γ_2) (tu0 $sigma_1 $sigma_2)
                  (@ (&cm Δ_1 Δ_2))))
           (&rull
            #f
            (&seq $· (tu0 $·) (@ $·)))
           (&rull
            #f
            (&<= $A $A^)
            (&seq (&: $x $A) (&subst $x $x^) (@: $x^ $A^)))))
      "{译注: 原文中" $⊗L "规则的名字同样写作" $⊗R ", 这里已改正.}")
   (P "我们将此留给你作为Lab 1的一部分: 以某种方式将子类型的能力"
      "与加法式算法定型的技术 (或者某种其他形式的算法定型) 结合起来.")
   ))
