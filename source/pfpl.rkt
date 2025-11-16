#lang racket
(provide pfpl.html)
(require SMathML)
(define (_prime x y)
  (_^ x y $prime))
(define $val (Mi "val" #:attr* '((mathvariant "sans-serif"))))
(define (Pred:val e)
  (: e $val))
(define (H3. #:attr* [attr* '()] #:id [id #f] #:switch? [switch? #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite
                     #:level 3 #:id id #:switch? switch?)
    ,attr* . ,html*))
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
    (Cite `(a ((href ,href)) ,name ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Lemma "引理" "lemma")
  (Remark "评注" "remark")
  (Theorem "定理" "theorem")
  (Warning "警告" "warning")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Corollary "推论" "corollary"))
(define (Subst e x e^)
  (: (bra0 (&/ e x)) e^))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (G!- . x*)
  (let-values (((l* r*) (split-at-right x* 1)))
    (&entailL (apply &cm $Gamma:normal l*)
              (car r*))))
(define (MBL label . exp*)
  (MB (Mtable
       #:attr*
       '((columnalign "left center right")
         (width "100%")
         (displaystyle "true"))
       (Mtr (Mtd (Mphantom label))
            (apply Mtd exp*)
            (Mtd label)))))
(define $ap (Mi "ap"))
(define (&ap e1 e2)
  (Appl $ap e1 e2))
(define $rec (Mi "rec"))
(define $with (Mi "with"))
(define $iter (Mi "iter"))
(define $plus (Mi "plus"))
(define $nat (Mi "nat"))
(define $arr (Mi "arr"))
(define (&arr t1 t2)
  (Appl $arr t1 t2))
(define $Typ (Mi "Typ" #:attr* '((mathvariant "sans-serif"))))
(define $Exp (Mi "Exp" #:attr* '((mathvariant "sans-serif"))))
(define (Lam x M)
  (: $lambda x M))
(define &\;
  (make-op $\;
    (lambda () $)
    (lambda (x) x)))
(define $\| (Mo "|"))
(define &\|
  (make-op $\|
    (lambda () $)
    (lambda (x) x)))
(define (Case . c*)
  (cur0 (apply &\| c*)))
(define (Rec e e_0 x y e_1)
  (: $rec e (Case (&rarrhk $z:normal e_0)
                  (: (Appl $s:normal x)
                     $with
                     (&rarrhk y e_1)))))
(define (Annotate op t)
  (ap op (cur0 t)))
(define (Appl f . x*)
  (ap f (@ (apply &\; x*))))
(define (|{;}| . x*)
  (cur0 (apply &\; x*)))
(define $. (Mo "."))
(define $rarrhk (Mo "&rarrhk;"))
(define-infix*
  (Bind $.)
  (&rarrhk $rarrhk))
(define-@lized-op*
  (@: &:))
(define pfpl.html
  (TnTmPrelude
   #:title "PFPL笔记"
   #:css "styles.css"
   (H1. "PFPL笔记")
   (P "千里之行, 始于足下.")
   (H2. "抽象句法")
   (P "抽象绑定树 (abstract binding tree) 这个概念最初令我非常困惑, "
      "后来我意识到我本来就理解了这个概念, "
      "只是Robert Harper故意使用了一种让人混淆的记号.")
   (H3. "抽象句法树")
   (H3. "抽象绑定树")
   (H3. "注记")
   (H2. "归纳定义")
   (H2. "假言判断和一般判断")
   (H2. "静态(语义)")
   (P "绝大多数语言都呈现出一种阶段的划分, 即静态阶段和动态阶段. "
      "静态阶段进行(句法分析和)类型检查, 确保程序是良形式的 (well-formed). "
      "动态阶段是对于良形式的程序进行执行. "
      "语言的安全性指的是良形式的程序在执行过程之中是良表现的 (well-behaved).")
   (P "实际上, 良表现取决于定义. "
      "例如, 一般而言, 无限执行下去而不终止并不视为非良表现的. "
      "我认为Robert Harper在这里定义的所谓安全显然是对于类型安全表述的化用. "
      "类型安全性指的是良类型的程序在执行过程之中不会出现类型错误. "
      "当然, 类型错误也取决于定义.")
   
   (H2. "动态(语义)")
   (H2. "类型安全")
   (H2. "求值动态")
   (H2. "函数定义和值")
   (H2. "高阶递归的System T")
   (H3. "静态语义")
   (P $T:sans-serif "的句法由以下语法给出:"
      (MB (set-attr*
           (&Table
            ($Typ $tau $::= $nat                      $nat                "naturals")
            ($    $    $    (&arr $tau_1 $tau_2) (&-> $tau_1 $tau_2) "function")
            ($Exp $e   $::= $x                        $x                  "variable")
            ($    $    $    $z:normal                 $z:normal           "zero")
            ($    $    $    (Appl $s:normal $e)       (Appl $s:normal $e) "successor")
            ($    $    $    (Appl (Annotate $rec $tau) $e $e_0 (Bind $x $y $e_1))
                  (Rec $e $e_0 $x $y $e_1) "recursion")
            ($    $    $    (Appl (Annotate $lambda $tau) (Bind $x $e))
                  (Lam (@: $x $tau) $e) "abstraction")
            ($    $    $    (&ap $e_1 $e_2)      (app $e_1 $e_2)     "application"))
           'columnalign "center right center left left left")))
   (P "对于表达式" (Appl $s:normal (: $..h (Appl $s:normal $z:normal)))
      "我们记" (OverBar $n) ", 其中后继被应用" (&>= $n $0) "次于零. 表达式"
      (Appl (Annotate $rec $tau) $e $e_0 (Bind $x $y $e_1)) "被称为"
      (Em "递归子(recursor)") ". 其代表了自" $e_0 "开始的变换"
      (Bind $x $y $e_1) "的" $e "重迭代. 绑定变量" $x
      "代表着前继而绑定变量" $y "代表着" $x "重迭代的结果.")
   (P "有时" (Em "迭代子(iterator)") "会被考虑作为递归子的替代, 即"
      (Appl (Annotate $iter $tau) $e $e_0 (Bind $y $e_1))
      ". 其基本上和递归子同义, 只是仅有递归调用的结果在" $e_1
      "中被绑定为" $y ", 而前继则没有绑定. 显然递归子可以被视为迭代子的特殊情形, "
      "因为我们总是可以忽略前继的绑定. 反过来, 只要我们有了积类型, "
      "递归子也可由迭代子定义. {译注: 这是The Little Typer的作者在无意间犯下的疏忽, "
      "即他们认为递归子比迭代子的表达力严格更强.} "
      "为了由迭代子定义递归子, 在迭代指定计算时我们还要计算前继.")
   (P $T:sans-serif "的静态语义由以下定型规则给出:"
      (MBL "(9.1a)" (&rule (G!- (&: $x $tau) (&: $x $tau))))
      (MBL "(9.1b)" (&rule (G!- (&: $z:normal $nat))))
      (MBL "(9.1c)" (&rule (G!- (&: $e $nat))
                           (G!- (&: (Appl $s:normal $e) $nat))))
      (MBL "(9.1d)" (&rule (G!- (&: $e $nat))
                           (G!- (&: $e_0 $tau))
                           (G!- (&: $x $nat) (&: $y $tau) (&: $e_1 $tau))
                           (G!- (&: (Appl (Annotate $rec $tau) $e $e_0 (Bind $x $y $e_1)) $tau))))
      (MBL "(9.1e)" (&rule (G!- (&: $x $tau_1) (&: $e $tau_2))
                           (G!- (&: (Appl (Annotate $lambda $tau_1) (Bind $x $e))
                                    (&arr $tau_1 $tau_2)))))
      (MBL "(9.1f)" (&rule (G!- (&: $e_1 (&arr $tau_2 $tau)))
                           (G!- (&: $e_2 $tau_2))
                           (G!- (&: (&ap $e_1 $e_2) $tau)))))
   (P "和往常一样, 替换的结构规则的兼容性是特别重要的.")
   ((Lemma)
    "如果" (G!- (&: $e $tau)) "且" (G!- (&: $x $tau) (&: $e^ $tau^))
    ", 那么" (G!- (&: (Subst $e $x $e^) $tau^)) ".")
   (H3. "动态语义")
   (P $T:sans-serif "的封闭值由以下规则定义:"
      (MBL "(9.2a)" (&rule (Pred:val $z:normal)))
      (MBL "(9.2b)" (&rule (bra0 (Pred:val $e))
                           (Pred:val (Appl $s:normal $e))))
      (MBL "(9.2c)" (&rule (Pred:val (Appl (Annotate $lambda $tau) (Bind $x $e)))))
      "(9.2b)的前提对于后继的" (Em "急切") "解释是包含在内的, 对于"
      (Em "惰性") "解释是排除在外的.")
   (P $T:sans-serif "的动态语义的转换规则如下:"
      (MBL "(9.3a)"
           (brac (&rule (&\|-> $e $e^)
                        (&\|-> (Appl $s:normal $e)
                               (Appl $s:normal $e^)))))
      (MBL "(9.3b)" (&rule (&\|-> $e_1 (_prime $e $1))
                           (&\|-> (&ap $e_1 $e_2)
                                  (&ap (_prime $e $1) $e_2))))
      (MBL "(9.3c)"
           (brac (&rule (Pred:val $e_1)
                        (&\|-> $e_2 (_prime $e $2))
                        (&\|-> (&ap $e_1 $e_2)
                               (&ap $e_1 (_prime $e $2))))))
      (MBL "(9.3d)" (&rule (bra0 (Pred:val $e_2))
                           (&\|-> (&ap (Appl (Annotate $lambda $tau) (Bind $x $e)) $e_2)
                                  (Subst $e_2 $x $e))))
      (MBL "(9.3e)" (&rule (&\|-> $e $e^)
                           (&\|-> (Appl (Annotate $rec $tau)
                                        $e $e_0 (Bind $x $y $e_1))
                                  (Appl (Annotate $rec $tau)
                                        $e^ $e_0 (Bind $x $y $e_1)))))
      (MBL "(9.3f)" (&rule (&\|-> (Appl (Annotate $rec $tau)
                                        $z:normal $e_0 (Bind $x $y $e_1))
                                  $e_0)))
      (MBL "(9.3g)"
           (&rule (Pred:val (Appl $s:normal $e))
                  (&\|-> (Appl (Annotate $rec $tau)
                               (Appl $s:normal $e) $e_0 (Bind $x $y $e_1))
                         (Subst
                          (&cm $e (Appl (Annotate $rec $tau)
                                        $e $e_0 (Bind $x $y $e_1)))
                          (&cm $x $y)
                          $e_1))))
      "用括号包裹起来的规则和前提只是针对急切后继和按值应用的, "
      "对于惰性后继和按名调用则是省略的. "
      
      )
   ((Lemma)
    (B "(Canonical Forms). ")
    "如果" (&: $e $tau) "而" (Pred:val $e) ", 那么"
    (Ol (Li "如果" (&= $tau $nat) ", 那么要么" (&= $e $z:normal)
            ", 要么对于某个" $e^ "有" (&= $e (Appl $s:normal $e^)) ".")
        (Li "如果" (&= $tau (&-> $tau_1 $tau_2)) ", 那么对于某个"
            $e_2 "有" (&= $e (Lam (@: $x $tau_1) $e_2)) ".")))
   ((Theorem)
    (B "(Safety). ")
    (Ol (Li "如果" (&: $e $tau) "而" (&\|-> $e $e^) ", 那么"
            (&: $e^ $tau) ".")
        (Li "如果" (&: $e $tau) ", 那么要么" (Pred:val $e)
            ", 要么对于某个" $e^ "有" (&\|-> $e $e^) ".")))
   (P "此即经典的类型安全的保持进展判则. "
      "一个无聊但隐式的注意点是定义的谓词" $val
      "应该与转换规则兼容, 也就是若" (&: $e $tau) "且"
      (Pred:val $e) ", 那么不存在" $e^ "使得" (&\|-> $e $e^)
      ". 换言之, 值应该是不可再转换/归约的表达式的一个子集. "
      "有人将其称为finality of values.")
   (H3. "可定义性")
   (P "一个自然数上的数学函数" (func $f $NN $NN)
      "在" $T:sans-serif "之中是" (Em "可定义的")
      ", 当且仅当存在一个具有类型" (&-> $nat $nat)
      "的表达式" $e_f "满足对于每个" (∈ $n $NN) ", 我们有"
      (MBL "(9.4)" (&: (&equiv (app $e_f (OverBar $n))
                               (OverBar (app $f $n)))
                       $nat))
      "也就是说, 数值函数" (func $f $NN $NN)
      "是可定义的, 当且仅当存在一个具有类型"
      (&-> $nat $nat) "的表达式" $e_f
      "满足, 当其被应用于参数" (∈ $n $NN)
      "的数码表示时, 应用本身和" (∈ (app $f $n) $NN)
      "所对应的数码表示定义相等 (definitionally equal).")
   (P $T:sans-serif "的定义相等记作"
      (G!- (&: (&equiv $e $e^) $tau))
      ", 是包含以下公理的最强congruence关系:"
      (MBL "(9.5a)"
           (&rule (G!- (&: $x $tau_1) (&: $e_2 $tau_2))
                  (G!- (&: $e_1 $tau_1))
                  (G!- (&: (&equiv (&ap (Appl (Annotate $lambda $tau_1) (Bind $x $e_2)) $e_1)
                                   (Subst $e_1 $x $e_2))
                           $tau_2))))
      
      )
   (H3. "不可定义性")
   (P "你无法在" $T:sans-serif "中定义一个无限循环.")
   ((Theorem)
    "如果" (&: $e $tau) ", 那么存在" (Pred:val $v) "满足"
    (&: (&equiv $e $v) $tau) ".")
   
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "")
   (H2. "System T的相等性")
   (P "函数式编程的优美之处在于函数式语言中的表达式相等遵循着令人熟悉的数学推理模式. "
      "例如, 在第9章的语言" $T:sans-serif "之中 (这个语言里我们可以将加法表达为函数"
      $plus "), 表达式"
      (MB (Lam (@: $x $nat)
               (Lam (@: $y $nat)
                    (app (app $plus $x) $y))))
      "和"
      (MB (Lam (@: $x $nat)
               (Lam (@: $y $nat)
                    (app (app $plus $y) $x))))
      "是相等的. 换言之, " (Em "在" $T:sans-serif "中所编写的")
      "加法函数是交换的.")
   (P "加法的交换性似乎是自明的, 但是" (Em "为什么") "这是真的呢? "
      "两个表达式相等又是什么意思呢? 这两个表达式并非"
      (Em "定义") "相等; 其相等性需要证明, 而不仅仅是算术. "
      "尽管如此, 这两个表达式是可互换的, "
      "因为它们当被应用于相同的数字时总能给出相同的结果. "
      "一般地, 两个函数是" (Em "逻辑等价的")
      ", 如果它们对于相等的参数给出相等的结果. "
      "我们或许会期望逻辑等价的函数在任意程序中都是可以互换的, "
      "因为逻辑等价是关于函数唯一重要的事情了. "
      "如果我们将包含这些函数的程序想成是对于函数行为的"
      (Em "观察") ", 那么这些函数被称为是" (Em "观察等价的")
      ". 本章的主要结果是观察等价和逻辑等价对于" $T:sans-serif
      "的一个变种是相合的, 这个变种里后继的求值是急切的, "
      "于是类型" $nat "的一个值是一个数码 (numeral).")
   (H3. "观察等价")
   (P "何时两个表达式是相等的? "
      "每当我们不能区分它们的时候! 似乎这么说是一种冗余重复, "
      "但是实际上并不是, 因为这取决于我们所考虑的用以区分表达式的手段. "
      "什么" (Q "实验") "是我们被允许在表达式上执行以区分它们的呢? "
      "什么样的观察 (如果对于两个表达式而言并不相同) 可以算作表达式不同的确定标志呢?")
   (P "如果我们允许我们自己考虑表达式的句法细节, 那么很少的表达式可以被认为是相等的. "
      
      )
   (H3. "")
   (H3. "")
   (H3. "")
   (H3. "注记")
   (P (Em "逻辑关系") "方法将类型解释为关系 (这里指的是等价关系), "
      "通过将每个类型构造子联系以一个关系作用, "
      "这个作用将解释(类型构造子的)参数的关系转变为"
      "解释被构造的类型的关系. 逻辑关系 (Statman, 1985) "
      "在类型论之中是一种基本工具, 并为NuPRL类型论 "
      "(Constable, 1986; Allen, 1987; Harper, 1992) "
      "的语义提供了基础. 使用逻辑关系刻画观察等价是将NuPRL的语义"
      "转换为应用在G&ouml;del的System T这一更简单的场景.")
   (P "对于类型构造子" $T ", 对于类型" $A "和" $B
      ", 如果" $R_A "和" $R_B "分别是" $A "和" $B
      "所对应的关系, 那么" $T "的关系动作将" $R_A
      "和" $R_B "变换为" (appl $T $A $B) "所对应的关系"
      (_ $R (appl $T $A $B)) ".")
   (H2. "")
   (H2. "")
   (H2. "")
   ))