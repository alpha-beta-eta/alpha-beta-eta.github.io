#lang racket
(provide sewpr.html)
(require SMathML)
(define tuple tupa0)
(define (make-const str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define-syntax-rule (define-const* (id str) ...)
  (begin
    (define id (make-const str))
    ...))
(define-const*
  ($true "true")
  ($false "false")
  ($if "if")
  ($mkpair "mkpair")
  ($fst "fst")
  ($snd "snd")
  (YC "Y"))
(define $. (set-compact (Mo ".")))
(define (LAM X M)
  (@ $lambda X $. M))
(define (make-blank n)
  (Mspace #:attr* `((width ,(format "~spx" n)))))
(define blank:4px (make-blank 4))
(define (APP M . N*)
  (apply @ (add-between (cons M N*) blank:4px)))
(define APP*
  (case-lambda
    ((M N) (APP M N))
    ((M N . N*) (apply APP* (APP M N) N*))))
(define boolt $t:monospace)
(define boolf $f:monospace)
(define $\| (Mo "|"))
(define $• (Mo "&bull;"))
(define $≐ (Mo "&doteq;"))
(define YV (_ $Y:monospace $V:monospace))
(define-infix*
  (&• $•)
  (&=> $=>)
  (&≐ $≐))
(define-@lized-op*
  (@• &•))
(define sewpr.html
  (TnTmPrelude
   #:title "语义工程和PLT Redex"
   #:css "styles.css"
   (H1. "语义工程和PLT Redex")
   (P "本书侧重于从抽象机器介绍编程语言, 愚以为某些内容尚待打磨, 并非易读. "
      "本书分为三个部分, 第一部分介绍了操作语义和抽象机器, "
      "第二部分引入了表达操作语义的工具PLT Redex, "
      "第三部分呈现了PLT Redex的一些实际应用.")
   (H2. "语义via句法")
   (P "描述编程语言自句法始. 正如每个程序员所知, "
      "语言的句法总以BNF (Backus-Naur Form) 语法的某种变体形式出现, "
      "其枚举了符合语法的词汇和句子. 困难之处实际上在于刻画程序的意义, "
      "即程序是如何进行计算的.")
   (P "在这本书的第一部分, 我们建立了一个基于句法的语义描述方法. 我们从这样的观察开始, "
      "即计算 (computation) 是对于算术 (calculation) 的一般化, "
      "一个孩子所接受的算术训练从" (Q $1 "加" $1 "等于" $2)
      "这样的材料开始. 诀窍在于看出这种形式的算术也可应用到程序上来.")
   (P "程序的算术意味着观察一个表达式或者语句的句法, "
      "然后将其与另一个表达式或者语句联系起来, 通常假定是更简单的. "
      "对于表达式" (&+ $1 $1) "而言, 前面的声明是容易理解的. "
      "它等于" $2 ", 即" (&+ $1 $1) "和" $2 "关联起来. "
      "即便是函数应用于参数值也可以这种方式表达 "
      "[注记: 实际上, " $+ "本身即是一个二元函数]:"
      (MB "如果定义" (&= (app $f $x) (&+ (&d* $2 $x) 55))
          ", 那么" (&= (app $f $4) (&+ (&d* $2 $4) 55)) "."))
   (P "用数学语言来说, 我们是在以句法上的关系描述编程语言的语义. "
      "对于学习函数式编程语言的人而言, 这种声明并不令人意外. "
      "我们知道函数式编程并不超出将七年级代数转换为编程语言太多, "
      "而代数定律不过就是将代数表达式相互联系起来的等式. "
      "但或许令人惊讶的是, 以这种方法我们可以描述(几乎)所有编程语言的语义, "
      "即便是那些含有命令式副作用的语言.")
   (P "现在我们将引入我们的想法, 即必要的数学元知识, "
      "从将句法定义为集合开始.")
   (H3. "定义集合")
   (P "BNF语法可有多种用途. 一种含义是字符串的集合. "
      "另一种解释是" (Q "树") "的集合, 其常被称为抽象句法(树). "
      "本书我们总是指后者.")
   (P "对于本章和下一章而言, 我们使用下面的BNF语法作为一个实际例子:"
      (eqn*
       ($B $=  boolt)
       ($  $\| boolf)
       ($  $\| (@• $B $B)))
      "我们将其当作以下施加于抽象句法树集合" $B "上的约束的缩写:"
      (MB (&Table
           ((∈ boolt $B))
           ((∈ boolf $B))
           ((&=> (: (∈ $a $B) "且" (∈ $b $B))
                 (∈ (@• $a $b) $B)))))
      "从技术上说, " $B "是满足以上约束的最小集合. "
      "为了构造这个集合, 我们先包括基本元素" boolt "和" boolf
      ", 然后归纳性地将其组合为复合元素.")
   (P "记号: 我们有时用" (Q $B) "表示" (Q "集合" $B)
      ", 但是有时" (Q $B) "又表示" (Q $B "的任意一个元素")
      ". 从上下文来看, 意义总是明确的. "
      "有时我们将下标或者撇号附到集合的名字上以表示该集合的任意元素, 例如"
      (Q $B_1) "或者" (Q $B^) ". 因此, 以上约束也可以写成"
      (MB (set-attr*
           (&Table
            ((∈ boolt $B)          "[a]")
            ((∈ boolf $B)          "[b]")
            ((∈ (@• $B_1 $B_2) $B) "[c]"))
           'columnalign "center left"))
      "在有限的空间之中枚举出" $B "的所有元素显然是不可能的:"
      (MB (&= $B (setE boolt boolf (@• boolt boolt)
                       (@• boolt boolf) $..h)))
      "然而, 给定某个树, 我们可以通过表明其满足约束而论证其的确属于" $B
      ". 例如, " (@• boolt (@• boolf boolt)) "就在" $B "中:"
      (MB (set-attr*
           (&Table
            ("1." (∈ boolt $B)                       "根据[a]")
            ((Mspace #:attr* '((height "12px"))))
            ("2." (∈ boolf $B)                       "根据[b]")
            ("3." (∈ boolt $B)                       "根据[a]")
            ("4." (∈ (@• boolf boolt) $B)            "根据2, 3, 以及[c]")
            ((Mspace #:attr* '((height "12px"))))
            ("5." (∈ (@• boolt (@• boolf boolt)) $B) "根据1, 4, 以及[c]"))
           'columnalign "left"))
      "通常这样的论证也可以安排成所谓的证明树的形式:"
      
      )
   (H3. "关系")
   (H3. "作为等价关系的语义")
   (H3. "语义via归约")
   (H3. "上下文中的归约")
   (H3. "求值函数")
   (H3. "记号总结")
   (H2. "分析句法性语义")
   (H3. "从问题到数学声明")
   (H3. "作为定理的回答")
   (H2. $lambda "演算")
   (H3. "函数和" $lambda "演算")
   (H3. $lambda "演算: 句法和归约")
   (P $lambda "演算的表达式的一般语法定义如下:"
      (eqn*
       ((&cm $M $N $L) $=  $X)
       ($              $\| (LAM $X $M))
       ($              $\| (APP $M $M))
       ((&cm $X $Y $Z) $= (: "一个变量:&nbsp;" (&cm $x $y $..h))))
      "以下是" $M "的示例成员:"
      (MB (&cm $x (APP $x $y) (LAM $x $x)
               (APP (APP $x $y) (APP $z $w))
               (LAM $y (LAM $z $y))
               (APP (LAM $y (APP $y $y))
                    (LAM $y (APP $y $y)))))
      "第一个例子, 即" $x ", 没有特定的直觉性意义, 因为"
      $x "还未被定义. 类似地, " (APP $x $y) "的意义是"
      (Q $x "应用于" $y) ", 但是我们没有更多可说的了. "
      "与之形成对比的是, 例子" (LAM $x $x) "与恒等函数相对应. "
      "这个例子和前两个例子的不同之处在于" $x
      "在前两个表达式里都是自由出现的, 而在后面的例子里是绑定出现的.")
   
   (H3. "编码布尔")
   (eqn*
    ($true  $≐ (LAM $x (LAM $y $x)))
    ($false $≐ (LAM $x (LAM $y $y)))
    ($if    $≐ (LAM $v (LAM $t (LAM $f (APP* $v $t $f))))))
   
   (H3. "编码序对")
   (eqn*
    ((tuple $M $N) $≐ (LAM $s (APP* $s $M $N)))
    ($mkpair    $≐ (LAM $x (LAM $y (LAM $s (APP* $s $x $y)))))
    ($fst       $≐ (LAM $p (APP $p $true)))
    ($snd       $≐ (LAM $p (APP $p $false))))
   (H3. "编码数字")
   (eqn*
    ($0 $≐ (LAM $f (LAM $x $x)))
    ($1 $≐ (LAM $f (LAM $x (APP $f $x))))
    ($2 $≐ (LAM $f (LAM $x (APP $f (APP $f $x)))))
    ($ $..v $)
    ($n $≐ (LAM $f (LAM $x (APP $f (APP $..h (APP $f $x) $..h))))))
   (H3. "编码和错误")
   (H3. "递归")
   (MB (&≐ YC (LAM $f (APP (LAM $x (APP $f (APP $x $x)))
                           (LAM $x (APP $f (APP $x $x)))))))
   (H3. "一致性和规范形式")
   (H3. "规范形式和归约策略")
   (H3. "历史")
   (H2. "ISWIM")
   (P "1950年代末期和1960年代早期, 人们发现了编程语言和" $lambda
      "演算的诸多方面之间的联系. 其动机从想要刻画Algol 60的语义演变为"
      "藉由已经充分理解了的数学系统来理解编程语言的面貌. "
      "也就是说, 人们想要学会如何系统地设计编程语言.")
   
   (H3. "ISWIM表达式")
   (H3. "ISWIM的计算")
   (H3. $alpha ", " $eta "和商")
   (H3. YV "组合子")
   (H3. "求值")
   (H3. "一致性")
   (H3. "观察等价")
   (H3. "历史")
   (H2. "一个抽象句法机器")
   (H3. "标准归约")
   (H3. "标准归约定理")
   (H3. "对于观察等价进行推理")
   (H3. "一致求值")
   (H3. "历史")
   (H2. "抽象寄存器机器")
   (H3. "CC机器")
   (H3. "SCC机器")
   (H3. "CK机器")
   (H3. "CEK机器")
   (H3. "历史")
   (H2. "尾调用和更多的空间节省")
   (H3. "SECD机器")
   (H3. "求值上下文的空间")
   (H3. "环境的空间")
   (H3. "历史")
   (H2. "控制: 错误, 异常和延续")
   (H2. "状态: 命令式赋值")
   (H2. "简单类型ISWIM")
   ))