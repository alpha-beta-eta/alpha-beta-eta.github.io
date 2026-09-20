#lang racket
(provide extensible_spec.html)
(require SMathML)
(define $o* (Mo "&otimes;" #:attr* '((largeop "false"))))
(define $o+ (Mo "&oplus;" #:attr* '((largeop "false"))))
(define $-o-> (Mo "&#8692;"))
(define $-->s (_ $--> $s))
(define-infix*
  (&o* $o*)
  (&o+ $o+)
  (&-o-> $-o->)
  (&-->s $-->s))
(define $environments (Mi "environments"))
(define $stores (Mi "stores"))
(define $continuations (Mi "continuations"))
(define $values (Mi "values"))
(define :5 (Code "5"))
(define :goto (Code "goto"))
(define :callcc (Code "callcc"))
(define :catch (Code "catch"))
(define :err (Code "err"))
(define :admin (Code "admin"))
(define :handler (Code "handler"))
(define :add1 (Code "add1"))
(define $phrases (Mi "phrases"))
(define $computations (Mi "computations"))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define $true (Pid "true"))
(define $false (Pid "false"))
(define $if (Pid "if"))
(define $zero? (Pid "zero?"))
(define $add1 (Pid "add1"))
(define $sub1 (Pid "sub1"))
(define $ref (Pid "ref"))
(define $! (Pid "!"))
(define $inV (Pid "inV"))
(define $inFX (Pid "inFX"))
(define $inE (Pid "inE"))
(define $inAC (Pid "inAC"))
(define $admin (Pid "admin"))
(define $handler (Pid "handler"))
(define $inN (Pid "inN"))
(define $inP (Pid "inP"))
(define $inB (Pid "inB"))
(define $inA (Pid "inA"))
(define $inPhi (Pid "in&Phi;"))
(define $setminus (Mo "&setminus;"))
(define $bigsqcup (Mo "&bigsqcup;"))
(define $inRef (Pid "inRef"))
(define $inSet (Pid "inSet"))
(define $inDer (Pid "inDer"))
(define $inL (Pid "inL"))
(define $inK (Pid "inK"))
(define $inCon (Pid "inCon"))
(define $catch (Pid "catch"))
(define $throw (Pid "throw"))
(define $new (Pid "new"))
(define $extend (Pid "extend"))
(define $lookup (Pid "lookup"))
(define $case (Pid "case"))
(define $of (Pid "of"))
(define $=> (Mo "=&gt;" #:attr* '((mathvariant "monospace"))))
(define (Ital str)
  (Mi str #:attr* '((mathvariant "italic"))))
(define $Env (: $EE (Ital "nv")))
(define $Sto (: $SS (Ital "to")))
(define $Err (Ital "Err"))
(define $Loop (Ital "Loop"))
(define $CBVM (Ital "CBVM"))
(define $controlM (Ital "controlM"))
(define $CoreScheme (Ital "CoreScheme"))
(define $Resources (Ital "Resources"))
(define $Admin (Ital "Admin"))
(define $Interpreter (Ital "Interpreter"))
(define $Handler (Ital "Handler"))
(define $exps (Ital "exps"))
(define $module (Mi "module" #:attr* '((mathvariant "bold"))))
(define $VV^ (&prime $VV))
(define $CC^ (&prime $CC))
(define $M:script^ (&prime $M:script))
(define $Var (Ital "Var"))
(define $Prog (Ital "Prog"))
(define $Expr (Ital "Expr"))
(define $err (Mi "err"))
(define $slambda
  (set-attr* (__ $lambda (Mo "_")) 'accentunder "true"))
(define $eval (Mi "eval"))
(define $Pomega (: $P:script $omega))
(define $T^omega (^ $T:bold $omega))
(define :Lambda (Span #:attr* '((class "small-caps")) "Lambda"))
(define split2 (&split 2))
(define (Ap e1 e2)
  (par0 (split2 e1 e2)))
(define (If e1 e2 e3)
  (par0 (split2 $if e1 e2 e3)))
(define (Lam x e)
  (par0 (: $lambda x $. e)))
(define $hole (bra0 $))
(define (plug E e)
  (: E (bra0 e)))
(define (subst e x v)
  (: e (bra0 (: x $/ v))))
(define (&delta f v)
  (appl $delta f v))
(define (MkRef e)
  (par0 (split2 $ref e)))
(define (Deref e)
  (par0 (split2 $! e)))
(define (Assign e1 e2)
  (par0 (split2 e1 $:= e2)))
(define (Catch x e)
  (par0 (split2 $catch x e)))
(define (Throw x e)
  (par0 (split2 $throw x e)))
(define (Conf . x*)
  (apply split2 x*))
(define (App f . x*)
  (: f (apply tu0 x*)))
(define (Sem f e)
  (: f (&db0 e)))
(define (Num n)
  (: (Mo "&ulcorn;" #:attr* '((lspace "0") (rspace "0")))
     n
     (Mo "&urcorn;" #:attr* '((lspace "0") (rspace "0")))))
(define (Inj a b)
  (_ $Phi (: a $--> b)))
(define (Proj a b)
  (_ $Psi (: a $--> b)))
(define (Cases . row*)
  (: $lc (set-attr* (&table row*) 'columnalign "left left")))
(define (Indent n x)
  (: (Mspace #:attr* `((width ,(format "~aem" n)))) x))
(define (TransSig f C1 C2)
  (define (module-rows C close)
    (list (list $ (Indent 1 (par0 (split2 $Resources $RR))))
          (list $ (Indent 1 (par0 (split2 $Admin
                                          (par0 (&--> (&c* C $RR) (&c* $VV $RR)))))))
          (list $ (Indent 1 (: (par0 (split2 $Interpreter
                                             (par0 (&--> (&c* $exps $Env) $CC))))
                               close)))))
  (MB (set-attr*
       (&table
        `((,(: $lp0 f $: $lp0) ,(: $lp0 $module))
          ,@(module-rows C1 $rp0)
          (,$ ,$-->)
          (,$ ,(: $lp0 $module))
          ,@(module-rows C2 (: $rp0 $rp0 $rp0))))
       'columnalign "left left"
       'columnspacing "0")))
(define (Clauses #:close [close $] . clause*)
  (define n (length clause*))
  (set-attr*
   (&table
    (for/list ([c clause*] [i (in-naturals 1)])
      (list (: $lb0 (car c)) $=>
            (: (cadr c) $rb0 (if (= i n) close $)))))
   'columnalign "left center left"))
(define (Admin c r)
  (: $admin (tupa0 c r)))
(define (Handler c f)
  (: (App $handler c) f))
(define $depends
  (: $..h (Mtext "依赖于语言") $..h))
(define (RLabel x)
  (par0 (if (string? x) (Mtext x) x)))
(define (grammar l)
  (MB (set-attr* (&table l) 'columnalign "right center left left")))
(define-syntax-rule (Grammar (x ...) ...)
  (grammar (list (list x ...) ...)))
(define (reductions l)
  (MB (set-attr* (&table l)
                 'width "100%"
                 'columnalign "right center left left right")))
(define-syntax-rule (Reductions (x ...) ...)
  (reductions (list (list x ...) ...)))
(define-Entry*
  (Fig "图" "fig"))
(define appendix-start 7)
(define (format-chapter n)
  (if (>= n appendix-start)
      (string (integer->char (+ (char->integer #\A) (- n appendix-start))))
      (number->string n)))
(define (format-num section index local?)
  (cond ((or (eq? (car section) '*) (not local?))
         (and index (format "~a" index)))
        (else
         (define sec (cdr (reverse section)))
         (format "~a.~a"
                 (string-join (cons (format-chapter (car sec))
                                    (map number->string (cdr sec)))
                              ".")
                 (or index "*")))))
(define (appendix-present %heading attr* letter . html*)
  `(h2 ,(attr*-set attr* 'id (%heading-id %heading))
       ,(format "附录~a " letter) . ,html*))
(define (Appendix letter . html*)
  `(,(build-%heading #:present appendix-present #:level 2)
    () ,letter . ,html*))
(define extensible_spec.html
  (parameterize ([parameter:format-num format-num])
  (TnTmPrelude
   #:title "可扩展指称性语言描述"
   #:css "styles.css"
   (H1. "可扩展指称性语言描述")
   (H2 "摘要")
   (P "传统的指称语义 (denotational semantics) 根据编程语言的其余部分, "
      "给同一个句法成分 (phrase) 赋予截然不同的含义. "
      "如果语言是纯函数式的, 数码 (numeral) 的指称是从环境到整数的函数. "
      "但在带有命令式控制算子 (imperative control operator) 的函数式语言中, "
      "数码指称从环境和延续 (continuation) 到整数的函数. "
      "本文引入一种新的指称式语言描述 (specification) 格式, "
      "即" (Em "扩展直接语义 (extended direct semantics)") ", "
      "它容许语言的正交扩展, 且不改变已有句法成分的指称. "
      "扩展直接语义总是把数码映射到同一个指称: "
      "相应的数字到值的domain中的嵌入 (injection). "
      "一般而言, 函数式语言中一个句法成分的指称, "
      "总是同一句法成分在扩展语言的语义中的指称的投影 (projection), 无论扩展是什么. "
      "基于扩展直接语义, 还可以通过组合语言片段的解释器来构造完整语言的解释器.")
   (H2. "复杂语言的指称规格")
   (P "像Scheme [25], Common LISP [32] 或ML [19] 这样的编程语言, "
      "由一个丰富的函数式核心组成, 并辅以对数据对象的破坏性操作 (destructive operation), "
      "控制构造 (control construct), 以及可能的其他命令式算子. "
      "传统的指称式语言规格 [1, 15, 27, 35] 通过把程序句法成分解释为将"
      (&c* $environments $stores $continuations) "映射到"
      (&c* $values $stores) "的函数, 来应对这些构造. "
      "然而, 程序员在对程序句法成分进行推理时, 依赖更简单的语义描述. "
      "大多数程序句法成分并不利用语言的全部一般性, "
      "从而允许用更简单的语义模型来分析它们的语义. "
      "例如, 如果一个程序句法成分是纯函数式的, "
      "并且它的自由变量总是绑定到无效应 (effect-free) 的过程, "
      "那么它可以被解释为将" $environments "映射到" $values "的函数. "
      "类似地, 如果一个命令式程序句法成分不使用像"
      :callcc ", " :goto "或" :catch
      "这样的一般控制算子, 并且它的自由变量总是绑定到符合同一约束的值和过程, "
      "那么它可以被解释为将" (&c* $environments $stores) "映射到"
      (&c* $values $stores) "的函数. "
      "不幸的是, 实用编程语言的指称定义被写成这样一种形式, "
      "它使得为一个规范的 (disciplined) 子集提取简化的定义变得困难"
      -- "更不用说证明这些定义在受限的语言上是等价的.")
   (P "描述同一问题的另一种方式, 是观察当一个语言被扩展时, "
      "像数码" :5 "这样的简单程序句法成分的含义会发生什么. "
      "在函数式语言中, 数码" :5 "指称程序员所期望的东西: "
      "一个从环境到整数" $5 "的函数. "
      "但如果我们向语言添加引用单元 (reference cell), " :5
      "指称一个从" (&c* $environments $stores) "到"
      (&c* $values $stores) "的常函数. "
      "此外, 如果我们添加控制算子 (例如" :goto "或" :callcc "), "
      :5 "的含义变为一个从" (&c* $environments $stores $continuations)
      "到" (&c* $values $stores) "的常函数. "
      "指称语义的这一恼人性质在语言研究者中是众所周知的. "
      "实际上, 在最近的一篇综述论文中, Peter Mosses [23] 论证了, "
      "这一现象一直是指称语义被接受为定义编程语言的实用工具 (vehicle) 的一大障碍.")
   (P "在本文中, 我们展示如何把指称定义表述为这样一种形式, "
      "它保持重要的语言子集 (例如函数式核心) 的简单语义. "
      "为指定子集给出的简化定义是一般定义的投影. "
      "指称定义的新形式由一个平凡的" (Em "基础 (base)")
      "定义和一系列" (Em "扩展 (extension)") "组成. "
      "基础定义描述一个恰有两个程序的平凡语言: "
      $bottom ", 它总是发散, 以及" :err
      ", 它总是中止并报告一个错误. 这些扩展都符合同一个模式 (schema).")
   (P "该模式关键地依赖于完整程序与嵌套程序句法成分之间的区别. "
      "完整程序被看作一个与外部世界 (例如文件系统) 交互, "
      "并且影响全局资源 (例如存储) 的主体 (agent). "
      "一个中央权威 (central authority) 管理这些资源. "
      "程序句法成分的含义是一个" (Em "计算 (computation)") ", "
      "它可以是一个" (Em "值 (value)") "或一个" (Em "效应 (effect)") ". "
      "如果一个程序句法成分的含义是一个" (Em "效应") ", 它被传播到中央权威. "
      "传播过程向效应包 (effect package) 添加一个函数, "
      "使得中央权威可以恢复被挂起的算术. "
      "我们把效应包的这一组成部分称为" (Em "句柄 (handle)") ", "
      "因为它提供了对该包的来源地的访问.")
   (P "中央权威通过函数" :admin "实现. "
      "它执行由效应指定的" (Em "动作 (action)") ". "
      "动作可以修改资源, 可以检查资源而不改变它们, 或者可以简单地中止程序执行. "
      "一旦动作被执行, 管理者 (administrator) 就提取" (Em "效应") "的"
      (Em "句柄") "部分, 并在必要时调用它. "
      (Em "句柄") "接着执行" (Em "效应") "的" (Em "动作")
      "之后剩余的任何计算. 因此, 在顶层, 句柄大致是一个常规的延续.")
   (P "把一个语言扩展表述到我们的框架中, 需要描述四个组成部分:"
      (Ul (Li "对该扩展的新句法构造子 (syntactic constructor) 的声明;")
          (Li "对由该扩展产生的新" (Em "值") ", " (Em "资源 (resource)")
              "和" (Em "动作") "的domain的定义;")
          (Li "对每个新句法构造子在意义函数 (meaning function) "
              $M:script "中的新子句 (clause) 的定义;")
          (Li "对新动作在函数" :admin "中的新子句的定义.")))
   (P "本质上, 语言扩展是通过向意义函数" $M:script
      "添加描述一个新构造的实例指称什么" (Em "值") "或" (Em "效应")
      "的子句来确定的. 函数" :admin "中的新子句确定"
      (Em "动作") "如何被解释为对" (Em "资源") "的变换.")
   (P "在第2章中, 我们基于我们先前关于操作语义的工作来引出 (motivate) 我们的指称框架. "
      "我们用第3章来阐明我们组织指称定义的新方法, 方式是把"
      (B "Scheme (Core ML)") "的一个理想化方言呈现为对一个平凡基础语言的若干扩展之和. "
      "接下来, 我们在第4章中展示如何利用这一新技术来编写模块化的解释器. "
      "在第5章中, 我们讨论两条相关工作的路线. "
      "附录A定义了domain方程的记号; 附录B包含实现中未在第4章中解释的部分.")
   (H2. "可扩展操作语义")
   (P "我们的可扩展语义规格框架背后的直觉, 来源于把" $lambda
      "演算的操作语义推广到带有异常, 第一级延续和赋值的完备的Scheme风格编程语言 "
      "[9, 10, 11, 12]. 这些语义的扩展是保守的, "
      "意思是扩展语言的归约关系是原始归约关系的超集. "
      "更确切地说, 核心语言的归约关系只是在更大的句法上被解释, 除此之外并未改变. "
      "新的语言设施 (linguistic facility) 通过添加新的归约来解释.")
   (P "我们通过考察一个简单的函数式编程语言Pure Scheme, "
      "来阐明我们的可扩展操作语义框架. "
      (Ref "pure-scheme") "包含Pure Scheme的句法和归约语义的完整规格. "
      "Pure Scheme的项语言包含数值常量, 布尔常量和函数常量, 变量, "
      $lambda "抽象 (用户定义的过程), " $if "表达式, 以及应用. "
      "表达式" (Lam $x $e) (Em "绑定") $e "中的" $x ". "
      "不被外围的" (: $lambda $x $.) "绑定的变量" $x "的出现是"
      (Em "自由的") ". 一个表达式是" (Em "封闭的") ", 如果它不包含自由变量. "
      "如果" $e "和" $e^ "是表达式, 其中" $x "可能在" $e "中自由出现, 那么"
      (subst $e $x $e^) "是把" $e "中" $x "的所有自由出现替换为" $e^
      ", 而不捕获后者中的自由变量的结果.")
   ((Fig #:id "pure-scheme")
    (B "[Pure Scheme及其归约语义]")
    (P (B "句法")
       (Grammar
        ($c $::= (&\| $0 $1 $-1 $2 $-2 $..h) "(数码)")
        ($  $\|  (&\| $true $false)          "(布尔值)")
        ($f $::= (&\| $zero? $add1 $sub1)    "(数值函数)")
        ($v $::= (&\| $c $f)                 "(常量)")
        ($  $\|  $x                          "(变量)")
        ($  $\|  (Lam $x $e)                 "(抽象)")
        ($e $::= $v                          "(值)")
        ($  $\|  (If $e $e $e)               "(分支)")
        ($  $\|  (Ap $e $e)                  "(应用)")))
    (P (B "辅助句法") " (求值上下文)"
       (Grammar
        ($E $::= $hole         "(洞)")
        ($  $\|  (Ap $E $e)    "(求值函数位置)")
        ($  $\|  (Ap $v $E)    "(求值实参位置)")
        ($  $\|  (If $E $e $e) "(求值测试位置)")))
    (P (B "标准归约")
       (Reductions
        ((plug $E (Ap $f $v)) $--> (plug $E (&delta $f $v))
         (: "如果" (&delta $f $v) "有定义") (RLabel $delta))
        ((plug $E (Ap (Lam $x $e) $v)) $--> (plug $E (subst $e $x $v))
         $ (RLabel $beta_v))
        ((plug $E (If $true $e_1 $e_2)) $--> (plug $E $e_1)
         $ (RLabel "iftrue"))
        ((plug $E (If $false $e_1 $e_2)) $--> (plug $E $e_2)
         $ (RLabel "iffalse"))))
    (P (B "常量解释")
       (eqn*
        ((&delta $zero? $0) $= $true)
        ((&delta $zero? (&+ $n $1)) $= $false)
        ((&delta $add1 $n) $= (&+ $n $1))
        ((&delta $sub1 $n) $= (&- $n $1)))))
   (P "Pure Scheme的归约语义确定了一个从程序到值的部分函数, "
      "其中程序是一个封闭表达式. 有两个" (Em "值") "的集合: 过程和常量. "
      "遵循" $lambda "演算的传统, " $eval
      "函数的规格基于标准归约函数的传递闭包. "
      "后者是这样一种归约, 它把任何非值划分为一个" (Em "求值上下文")
      "和一个可约式 (redex), 归约该可约式, "
      "并用缩约式 (contractum) 填充求值上下文的洞. "
      "求值上下文是一种特殊的上下文. "
      "求值上下文中洞的位置确定了接下来必须对哪个子项求值. "
      "因此, 一个语言的求值上下文集合的定义确定了求值的顺序.")
   (P "在Pure Scheme这一函数式语言中, 在可约式被归约为值之前, "
      "它的求值上下文不起任何作用. 例如, 表达式"
      (plug $E (Ap (Lam $x (Ap $add1 (If $x $0 $1))) $true))
      "归约为" (plug $E $1) ", 无论" $E "是什么. "
      "更一般地, (带递归的) 数学语言中的所有 (封闭) 表达式"
      "要么产生一个值, 要么发出一个错误信号, 要么发散. "
      "因此, 指称语义可以把这样一种语言中的表达式映射到数字, 布尔值, 函数, "
      "错误或底 (bottom).")
   (P "一旦Pure Scheme被扩展为带有ML风格引用单元的语言 (见"
      (Ref "state-scheme") "), 封闭表达式的归约就可能产生一个"
      (Q "卡住的表达式 (stuck expression)")
      ", 它未来的行为依赖于并影响程序的其他部分. 考虑以下表达式:"
      (MB (MkRef (Lam $x $x)) ".")
      "它是一个封闭表达式, 它不发散, 不是值, 而且不影响上下文就不能被归约为值. "
      "我们把这样的结果称为" (Em "效应") ".")
   ((Fig #:id "state-scheme")
    (B "[State Scheme及其归约语义]")
    (P (B "附加句法")
       (Grammar
        ($e $::= (MkRef $e)      "(引用分配)")
        ($  $\|  (Deref $e)      "(引用解引用)")
        ($  $\|  (Assign $e $e)  "(引用更新 (中缀))")
        ($s $::= (&* (cur0 (tu0 $x $v)))
            "(存储 (标识符-值绑定))")))
    (P (B "附加辅助句法")
       (Grammar
        ($E $::= (MkRef $E)     "(求值子表达式)")
        ($  $\|  (Deref $E)     "(求值子表达式)")
        ($  $\|  (Assign $E $e) "(求值地址位置)")
        ($  $\|  (Assign $v $E) "(求值值位置)")))
    (P (B "标准归约")
       (Reductions
        ((Conf $s (plug $E (MkRef $v))) $-->
         (Conf $s (tu0 $l $v) (plug $E $l))
         (: "如果" $l "未在" $s "或" (plug $E $v) "中使用")
         (RLabel "alloc"))
        ((Conf $s (tu0 $l $v) $s^ (plug $E (Deref $l))) $-->
         (Conf $s (tu0 $l $v) $s^ (plug $E $v))
         $ (RLabel "lookup"))
        ((Conf $s (tu0 $l $v) $s^ (plug $E (Assign $l $v^))) $-->
         (Conf $s (tu0 $l $v^) $s^ (plug $E $l))
         $ (RLabel "update")))))
   (P "效应最容易被理解为一个子表达式与一个管理程序全局资源的中央权威之间的交互. "
      "这种资源的例子有存储, 堆, 文件系统, 程序最终结果的存放处, "
      "以及其他输入/输出通道. 给定一个管理者, "
      "效应可以被看作一条发给中央权威的消息加上足以恢复被挂起的算术的信息. "
      "在操作语义中, 那个" (Q "卡住的表达式") ", 即贯穿始终的例子中的"
      (MkRef (Lam $x $x)) ", 可以扮演消息的前一半的角色; "
      "求值上下文则充当消息的第二部分. "
      "在扩展存储, 查找一个值或修改一个位置的内容之后, 算术过程就可以继续. "
      "从技术上讲, 操作语义通过用某个数据填充求值上下文来做到这一点, "
      "这个数据也把信息从管理者传达给被挂起的表达式. "
      "通过把这一操作语义翻译到一个数学框架中, "
      "我们可以为编程语言设计一个可扩展的指称语义.")
   (H2. "可扩展指称规格")
   (P "编程语言的指称规格由两部分组成. "
      "第一部分定义语言的句法domain和语义domain. "
      "这些domain是某个universal domain的subdomain, 例如"
      $Pomega " [29], " $T^omega " [24], " $U:normal " [28] 或" $TT " [17]. "
      "句法domain规定语言的句法成分; 语义domain包含各种句法成分的指称. "
      "第二部分是一个函数式解释器, 它把句法domain的元素映射到语义domain中的元素. "
      "它用像" :Lambda " [29] 或KL [17] 这样的通用编程语言定义, "
      "并满足组合性定律, 即, 一个句法成分的解释是其子句法成分的解释的函数. "
      "为了追踪自由变量的指称, 解释器还以一个环境参数参数化. "
      "从代数上讲, 解释器 (大致) 是一个从句法到语义的同态.")
   (P "我们通过为Pure Scheme定义一个语义来阐明可扩展指称规格的方法论. "
      "我们首先用破坏性的引用单元扩展该语义, 然后再用第一级延续对象扩展它. "
      "尽管Pure Scheme是一个句法上简单的语言, 只有一种句法成分类型 (phrase type), "
      "它在语义上却很丰富, 而且它的扩展提出了使传统指称式语言规格复杂化的大多数问题. "
      "第一小节给出Pure Scheme及其扩展的可扩展指称规格的一般模式. "
      "第二, 第三和第四小节分别包含Pure Scheme, State Scheme和Control Scheme的规格. "
      "后两者从字面上讲是Pure Scheme语义的扩大. "
      "最后一小节表明, 一个句法成分的指称相对于所有可能的扩展都是稳定的.")
   (H3. "语义框架: 扩展直接语义")
   (P "遵循上一节的想法, 句法成分的指称的domain" $CC
      "由两个不相交的部分组成: 值指称的subdomain" $VV
      "和效应消息 (effect message) (效应) 的subdomain. "
      (Ref "ext-direct") "的第一部分展示了domain方程的一般模式. "
      "一般而言, 值的domain是由" $VV "和" $CC "构造出来的domain的和; "
      "我们在" (Ref "ext-direct") "中用记号" (appl $Sigma_1 $VV $CC) "表示这一点. "
      "对于像Pure Scheme这样的语言, 值的domain包含数字, 布尔值, "
      "从值到计算的函数, 以及底 (bottom) (发散表达式的指称). "
      "对于State Scheme, 值的domain还包含一个位置 (location) 的domain, "
      "位置解释引用单元.")
   (P "效应消息的domain总是两个domain的积. "
      "这个积的第一个分量表示求值上下文, 另一个分量指定管理者必须执行的动作. "
      "求值上下文的一个直截了当的表示是一个从值到计算的函数, "
      "这直接对应于它在操作上作为从句法值到表达式的函数的用法. "
      "效应domain的动作分量" $AA "是由" $VV "和" $CC "构建的domain的和; "
      "在" (Ref "ext-direct") "中我们为此使用表达式" (appl $Sigma_2 $VV $CC) ". "
      "对于管理者必须执行的每一种动作, 它都包含一个被加项 (summand). "
      "对于平凡的基础语言, 基本的动作domain只包含一个错误动作 "
      "(因为我们相信发出错误信号是最基本的效应). "
      "对于Pure Scheme, 不需要额外的动作. "
      "对于State Scheme, 有三个额外的动作: 分配一个填有某个值的引用单元, "
      "对一个单元解引用, 以及修改一个引用单元.")
   ((Fig #:id "ext-direct")
    (B "[语义框架]")
    (P (B "语义domain")
       (MB (set-attr*
            (&Table
             ("(值)" $VV $= (appl $Sigma_1 $VV $CC) "(依赖于语言)")
             ("(计算)" $CC $=
              (&o+ (App $inV $VV)
                   (App $inFX (&o* (_ (par0 (&--> $VV $CC)) $bottom) $AA)))
              $)
             ("(动作)" $AA $= (&o+ (App $inE $EE) (appl $Sigma_2 $VV $CC))
              "(依赖于语言)")
             ("(错误)" $EE $= (setE $bottom $err) $)
             ("(资源)" $RR $= (appl $Pi $VV $CC $..h) "(依赖于语言)")
             ("(环境)" $Env $= (&-o-> $Var $VV) $))
            'columnalign "left right center left left")))
    (P (B "语义函数")
       (eqn*
        ($P:script $: (&--> $Prog (par0 (&c* (par0 (&o+ $VV $EE)) $RR))))
        ($M:script $: (&--> $Expr $Env $CC))
        ($ $ $)
        ((Sem $P:script $P) $= (Admin (: (Sem $M:script $P) $bottom) $r_0))
        ($ $ $)
        ((: (Sem $M:script $Omega) $rho) $= (&= (App $inV $bottom) $bottom))
        ((: (Sem $M:script $err) $rho) $= (App $inAC (App $inE $err)))
        ((: (Sem $M:script $e) $rho) $= $depends)))
    (P (B "辅助语义函数")
       (eqn*
        ($admin $: (&--> (&c* $CC $RR) (par0 (&c* (par0 (&o+ $VV $EE)) $RR))))
        ($handler $: (&--> $CC (par0 (&--> $VV $CC)) $CC))
        ($inAC $: (&--> $AA $CC))
        ($ $ $)
        ((Admin $bottom $r) $= $bottom)
        ((Admin (App $inV $v) $r) $= (tupa0 $v $r))
        ((Admin (App $inFX $k (App $inE $err)) $r) $= (tupa0 $err $r))
        ((Admin (App $inFX $k $p) $r) $= $depends)
        ($ $ $)
        ((Handler $bottom $f) $= $bottom)
        ((Handler (App $inV $v) $f) $= (app $f $v))
        ((Handler (App $inFX $k $p) $f) $=
         (App $inFX
               (bra0 (: $slambda $v $: $VV $. (Handler (app $k $v) $f)))
               $p))
        ($ $ $)
        ((App $inAC $e) $= (App $inFX $inV $e)))))
   (P "一般而言, 解释器把" (&c* $phrases $environments) "映射到" $computations ". "
      "为了解释绑定变量的含义, 需要环境. "
      "给定这些模式化的domain定义, 解释器可以处理两个简单的编程构造: "
      $Omega ", 它代表发散, 以及" :err ", 它发出一个错误信号. "
      "其他种类的句法成分需要对解释器进行扩展.")
   (P "复杂句法成分的子句法成分通过对解释器的递归调用来求值. "
      "由于这样一个递归调用的结果是一个计算, 有必要检查结果的标签 (tag). "
      "如果它是一个普通值, 值分量可以在局部被消耗. "
      "然而, 如果它是一个效应, 它必须被传播到中央管理者, 由后者解释它的含义. "
      "为了统一地处理这种情况, 我们引入一个函数" :handler
      ", 它把一个计算和其最终值的消费者 (consumer) 映射到计算. "
      "消费者是一个从值到一个计算的函数. "
      "{原注: 在没有计算效应和错误的情况下, 这一模式实际上退化为直接语义的模式.}")
   (P "函数" :handler "执行一个简单的标签检查: "
      "如果第一个参数在值subdomain中, 它就把它的第二个参数应用于该值. "
      "否则, 它创建一个新的效应消息, 这个消息通过一个修改过的句柄来容纳额外的消费者. "
      "因此一个效应消息从" :handler "传播到" :handler
      ", 直到它最终到达中央管理者. "
      "管理者" :admin "是一个有两个参数的函数: 一个计算和一个 (开放式的) 资源列表 ("
      (appl $Pi $VV $CC $..h) "). "
      "如果计算是一个值, 那么值和资源作为程序的结果返回. "
      "一个需要" (Q "错误") "动作的计算产生与当前资源配对的错误. "
      "程序的含义被定义为解释器与管理者的复合.")
   (P (Ref "ext-direct") "展示了一个一般语言定义的模式. "
      "下面所有的定义都符合这个模式. "
      "每个语言片段都要求在五个地方修改这个大纲: 值subdomain的和 ("
      $Sigma_1 "), 动作的和 (" $Sigma_2 "), 资源的列表, 意义函数"
      $M:script "的子句, 以及管理者函数" :admin "的子句. "
      "其他一切保持不变, 例如, 解释器的类型, " :handler "函数, 顶层意义函数, 等等. "
      "由于解释器把句法成分和环境映射到它们的含义, "
      "我们把这种风格的语义称为" (Em "扩展直接语义") ".")
   (H3. "Pure Scheme")
   (P "我们首先用一个简单的算术语言来阐明第一小节的抽象想法:"
      (MB (split2 (: $M $::= (&\| (Num $n) (Ap $add1 $M) (Ap $sub1 $M) $..h) $cm)
                  (&in $n $NN))
          ".")
      "这个语言的值domain只包含整数和底. 效应消息只为传播错误而存在. "
      "因此, domain方程是:"
      (eqn*
       ($VV $= (App $inN (_ $NN $bottom)))
       ($AA $= (App $inE $EE)))
      "而意义函数把数码映射到整数:"
      (MB (&= (: (Sem $M:script (Num $n)) $rho) (App $inV (App $inN $n))) "."))
   (P "一个" :add1 "表达式的解释, 是解释它的子表达式, "
      "并用一个对所有数值输入都输出其后继的函数来处理结果, 所得到的结果:"
      (eqn*
       ((: (Sem $M:script (Ap $add1 $e)) $rho) $=
        (App $handler (: (Sem $M:script $e) $rho)))
       ($ $ (: $lp0 $slambda $m $: $VV $. (split2 $case $m $of)))
       ($ $ (Indent 2 (Clauses
                       #:close $rp0
                       (list (App $inN $n) (App $inV (App $inN (&+ $n $1))))
                       (list $m (App $inAC (App $inE $err)))))))
      "对于潜在的非数值输入, 消费者函数输出错误. "
      "其他复杂表达式以同样的方式解释.")
   (P "为了把算术语言扩展为完整的Pure Scheme, 我们添加" (Ref "pure-scheme")
      "的句法以及值的domain的另一个被加项: 过程指称的domain. "
      "Pure Scheme过程把非底的值映射到计算. "
      "因此, 新的值的domain是从值到计算的严格函数 (strict function) 的domain:"
      (MB (&= $VV (&o+ (App $inN (_ $NN $bottom))
                       (App $inP (_ (par0 (&-->s $VV $CC)) $bottom))))
          ".")
      "完整的Pure Scheme不需要新的效应.")
   (P "语义函数 (见" (Ref "pure-spec") ") 以通常的方式把变量映射到值, 把过程映射到函数. "
      "对于一个应用" (Ap $M $N) ", 解释器首先递归地确定" $M "的含义, "
      "并用" :handler "把它强制转换为一个值" $f ". "
      "相应的消费者接着以类似的方式确定" $N "的值" $a ". "
      "第二个消费者函数在" $f "是函数时把" $f "应用于" $a
      ", 否则发出一个错误信号.")
   (P "布尔值和" $if "表达式需要对算术语言进行类似于上面所描述的扩展. "
      (Ref "pure-spec") "包含Pure Scheme的完整规格.")
   ((Fig #:id "pure-spec")
    (B "[Pure Scheme的一个扩展直接语义]")
    (P (B "语义domain")
       (eqn*
        ($VV $= (&o+ (App $inN (_ $NN $bottom))
                     (App $inB (_ $TT $bottom))
                     (App $inP (_ (par0 (&-->s $VV $CC)) $bottom))))
        ($AA $= (App $inE $EE))))
    (P (B "语义函数")
       (eqn*
        ((Sem $M:script (Num $n)) $= (App $inV (App $inN $n)))
        ((Sem $M:script $true) $= (App $inV (App $inB $true)))
        ((Sem $M:script $false) $= (App $inV (App $inB $false)))
        ((: (Sem $M:script $add1) $rho) $=
         (: $inV $lp0 $inP $lp0 $slambda $m $: $VV $.))
        ($ $ (Indent 2 (split2 $case $m $of)))
        ($ $ (Indent 3 (Clauses
                        #:close (: $rp0 $rp0)
                        (list (App $inN $n) (App $inV (App $inN (&+ $n $1))))
                        (list $m (App $inAC $err)))))
        ($..h $ $..h)
        ((: (Sem $M:script $x) $rho) $= (App $inV (app $rho $x)))
        ((: (Sem $M:script (Lam $x $e)) $rho) $=
         (App $inV (App $inP (: $slambda $d $: $VV $.
                                (: (Sem $M:script $e) (subst $rho $x $d))))))
        ((: (Sem $M:script (Ap $e_1 $e_2)) $rho) $=
         (App $handler (: (Sem $M:script $e_1) $rho)))
        ($ $ (Indent 1 (: $lp0 $slambda $f $: $VV $.
                          (App $handler (: (Sem $M:script $e_2) $rho)))))
        ($ $ (Indent 2 (: $lp0 $slambda $a $: $VV $. (split2 $case $f $of))))
        ($ $ (Indent 4 (Clauses
                        #:close (: $rp0 $rp0)
                        (list (App $inP $g) (app $g $a))
                        (list $g (App $inAC $err)))))
        ((: (Sem $M:script (If $e_1 $e_2 $e_3)) $rho) $=
         (App $handler (: (Sem $M:script $e_1) $rho)))
        ($ $ (Indent 1 (: $lp0 $slambda $t $: $VV $.)))
        ($ $ (Indent 2 (split2 $case $t $of)))
        ($ $ (Indent 3 (Clauses
                        #:close $rp0
                        (list (App $inB $true) (: (Sem $M:script $e_2) $rho))
                        (list (App $inB $false) (: (Sem $M:script $e_3) $rho))
                        (list $f (App $inAC $err))))))))
   (H3. "State Scheme")
   (P "向Pure Scheme添加引用单元需要" (Ref "state-scheme")
      "的句法, 以及一类用于解释引用单元的新值. "
      "遵循传统, 我们把这些值称为" (Em "位置") ". "
      "位置在" (Em "存储") " (" $Sto ") 中被绑定, 存储是全局资源池的一部分. "
      "和所有其他资源一样, 存储由管理者管理. "
      "因此, 创建一个单元需要一条发给中央管理者的消息. "
      "它分配一个新的位置, 并把它与分配消息的值关联起来. "
      "分配的结果是新位置" $l ", 也就是说, 管理者用句柄把" $l "返回给程序. "
      "类似地, 对单元解引用和修改单元内容的动作也需要程序句法成分与管理者之间的消息交换.")
   (P "翻译成我们的指称框架的语言, State Scheme的定义需要值的domain的一个新被加项, "
      "即位置的domain, 以及动作的domain (效应消息的第二个分量) 的三个新被加项:"
      (Ol (Li (App $inRef $VV) ", 它表示一条分配消息;")
          (Li (App $inSet (_ $LL $bottom) $VV) ", 它表示一条更新消息; 以及")
          (Li (App $inDer (_ $LL $bottom)) ", 它表示一条解引用消息."))
      "此外, 资源列表 (到目前为止为空) 需要用一个存储来扩展. "
      "扩展后的domain的详细规格见" (Ref "state-spec") "的第一部分.")
   (P "三个新句法形式的解释是直截了当的. "
      "对于每个形式, 解释器用一个最终发出关于一个位置的分配, 修改或解引用的效应消息的"
      "处理器 (handler), 递归地确定子表达式的计算. "
      "扩展后的管理者恰好接受这三个新的效应消息. "
      "如果效应消息是一条分配消息, 它按照本节开头所描述的那样进行. "
      "另外两个效应消息以类似的方式处理. "
      (Ref "state-spec") "的第二部分给出了解释器和管理者函数的新子句; "
      $M:script "和" :admin "中针对Pure Scheme句法和效应消息的子句保持不变!")
   ((Fig #:id "state-spec")
    (B "[State Scheme的一个扩展直接语义]")
    (P (B "语义domain")
       (MB (set-attr*
            (&Table
             ($ $VV $= (&o+ (App $inN (_ $NN $bottom))
                            (App $inB (_ $TT $bottom))
                            (App $inP (_ (par0 (&--> $VV $CC)) $bottom))
                            (App $inL (_ $LL $bottom))))
             ($ $AA $= (&o+ (App $inE $EE)
                            (App $inRef $VV)
                            (App $inDer (_ $LL $bottom))
                            (App $inSet (&o* (_ $LL $bottom) $VV))))
             ($ $RR $= $Sto)
             ("(存储)" $Sto $= (&-o-> (_ $LL $bottom) $VV))
             ("(位置)" (_ $LL $bottom) $:
              (: "一个平坦domain, 通常同构于" (_ $NN $bottom))))
            'columnalign "left right center left")))
    (P (B "语义函数")
       (eqn*
        ((: (Sem $M:script (MkRef $e)) $rho) $=
         (: (App $handler (: (Sem $M:script $e) $rho))
            (par0 (: $slambda $v $: $VV $. (App $inAC (App $inRef $v))))))
        ((: (Sem $M:script (Assign $e_1 $e_2)) $rho) $=
         (App $handler (: (Sem $M:script $e_1) $rho)))
        ($ $ (Indent 1 (: $lp0 $slambda $l $: $VV $.
                          (App $handler (: (Sem $M:script $e_2) $rho)))))
        ($ $ (Indent 2 (: $lp0 $slambda $a $: $VV $. (split2 $case $l $of))))
        ($ $ (Indent 4 (Clauses
                        #:close (: $rp0 $rp0)
                        (list (App $inL $l^) (App $inAC (App $inSet $l^ $a)))
                        (list $g (App $inAC (App $inE $err))))))
        ((: (Sem $M:script (Deref $e)) $rho) $=
         (App $handler (: (Sem $M:script $e) $rho)))
        ($ $ (Indent 1 (: $lp0 $slambda $l $: $VV $. (split2 $case $l $of))))
        ($ $ (Indent 3 (Clauses
                        #:close $rp0
                        (list (App $inL $l^) (App $inAC (App $inDer $l^)))
                        (list $g (App $inAC (App $inE $err))))))))
    (P (B "管理者函数")
       (eqn*
        ((Admin (App $inFX $k (App $inRef $v)) $r) $=
         (Admin (app $k (App $inL (App $new $r)))
                (App $extend $r (App $new $r) $v)))
        ((Admin (App $inFX $k (App $inSet $l $v)) $r) $=
         (Admin (app $k $l) (App $extend $r $l $v)))
        ((Admin (App $inFX $k (App $inDer $l)) $r) $=
         (Admin (app $k (App $lookup $r $l)) $r)))))
   (H3. "Control Scheme")
   (P "标准Scheme和SML of New Jersey提供了用第一级延续对象编程的构造. "
      "延续对象是程序的控制状态的一个抽象. "
      "获得对这一控制抽象的访问常被称为" (Em "捕获 (catch)") "一个延续. "
      "使用这一抽象, 或者用Scheme和SML的行话说, " (Em "抛出 (throw)")
      "到一个延续, 程序可以丢弃它的当前延续, 并把一个值传送给一个先前捕获的延续对象. "
      "Control Scheme采用句法" (Catch $x $e) "来捕获延续并把它绑定到" $x
      ", 并采用句法" (Throw $x $e) "来把" $e "的值抛出到绑定到" $x "的延续 [37]. "
      "按照传统方法建模第一级延续对象, 需要使用延续语义 [36, 1, 15, 27, 35]. "
      "延续语义把" (Em "每个") "句法成分类型的指称在一个" (Em "延续")
      "上参数化, 延续是一个把中间结果映射到最终答案的函数.")
   (P "在扩展直接语义的框架中, 最终答案的输出处于中央管理者的控制之下. "
      "从程序中任何位置向管理者发送一个效应消息, 按定义会构造一个句柄, "
      "它是计算的其余部分的一个函数式抽象. "
      "因此延续对象很容易被容纳进这个框架. "
      "一个" $catch "表达式指称一个包含延续的接收者 (recipient) 的效应消息, "
      "这个接收者大致是在延续对象的名字 (" $x ") 上参数化的"
      $catch "表达式的体 (" $e "). "
      "管理者把接收者应用于句柄, 从而放弃对计算的其余部分的控制. "
      "一个" $throw "表达式指称一个类似的包, 但这个包的延续接收者" (Em "忽略")
      "它从管理者那里接收的延续, 转而使用绑定到" $throw "变量的对象.")
   (P "就domain而言, 把Pure Scheme扩展为Control Scheme需要添加一个值被加项, "
      "即延续的domain, 以及一个效应, 即一个控制消息. "
      "后者包含延续的接收者, 它是一个从句柄到计算的函数. "
      "意义函数" $M:script "和管理者" :admin "的扩展实现了上述模式. "
      "细节在" (Ref "control-spec") "中给出.")
   ((Fig #:id "control-spec")
    (B "[Control Scheme的一个扩展直接语义]")
    (P (B "句法")
       (MB (: $e $::= (&\| (Catch $x $e) (Throw $x $e)))))
    (P (B "语义domain")
       (eqn*
        ($VV $= (&o+ (App $inN (_ $NN $bottom))
                     (App $inB (_ $TT $bottom))
                     (App $inP (_ (par0 (&--> $VV $CC)) $bottom))
                     (App $inK (_ (par0 (&--> $VV $CC)) $bottom))))
        ($AA $= (&o+ (App $inE $EE)
                     (App $inCon (_ (par0 (&--> (par0 (&--> $VV $CC)) $CC))
                                    $bottom))))))
    (P (B "语义函数")
       (eqn*
        ((: (Sem $M:script (Catch $x $e)) $rho) $=
         (App $inAC
              (App $inCon
                   (: $slambda $k $: (&--> $VV $CC) $.
                      (Handler (: (Sem $M:script $e) (subst $rho $x (App $inK $k)))
                               $k)))))
        ((: (Sem $M:script (Throw $x $e)) $rho) $=
         (App $handler (: (Sem $M:script $e) $rho)))
        ($ $ (Indent 1 (: $lp0 $slambda $v $: $VV $.
                          (split2 $case (app $rho $x) $of))))
        ($ $ (Indent 3 (Clauses
                        #:close $rp0
                        (list (App $inK $k)
                              (App $inAC
                                   (App $inCon
                                        (: $slambda $k^ $: (&--> $VV $CC) $.
                                           (app $k $v)))))
                        (list $k (App $inAC (App $inE $err))))))))
    (P (B "管理者函数")
       (eqn*
        ((Admin (App $inFX $k (App $inCon $f)) $r) $=
         (Admin (app $f $k) $r)))))
   (P "把State Scheme扩展为Core Scheme, 即一个包含引用单元以及"
      $catch "和" $throw "的类Scheme语言, 同样容易. "
      "有必要如下扩展State Scheme的domain:"
      (eqn*
       ($VV $= (&o+ (App $inN (_ $NN $bottom))
                    (App $inB (_ $TT $bottom))
                    (App $inP (_ (par0 (&--> $VV $CC)) $bottom))
                    (App $inL (_ $LL $bottom))
                    (App $inK (_ (par0 (&--> $VV $CC)) $bottom))))
       ($AA $= (App $inE $EE))
       ($ $ (Indent 1 (: $o+ (&o+ (App $inRef $VV)
                                  (App $inDer (_ $LL $bottom))
                                  (App $inSet (&o* (_ $LL $bottom) $VV))))))
       ($ $ (Indent 1 (: $o+ (App $inCon (_ (par0 (&--> (par0 (&--> $VV $CC)) $CC))
                                           $bottom))))))
      "意义函数" $M:script "和管理者" :admin "的新子句保持不变.")
   (P "State Scheme与Control Scheme的合并是对称的. "
      "把引用的domain合并进Control Scheme的domain, 得到与Core Scheme的domain同构的domain. "
      "由此产生的意义函数和管理者会与我们刚才描述的那些完全相同. "
      "一般而言, 添加新的正交语言构造, 就像把Pure Scheme扩展为Control Scheme, "
      "或把State Scheme扩展为Core Scheme一样直截了当. "
      "它需要在值的domain, 效应的domain, 解释器和管理者函数中添加被加项. "
      "不需要对先前的规格进行任何修改. "
      "因此我们相信, 对于我们的新风格的指称规格而言, "
      (Q "可扩展") "这一属性是完全正当的. "
      "下面的小节为这一属性的更形式化的论证提供基础.")
   (H3. "稳定指称")
   (P "回忆一下, 扩展直接定义的语义domain具有如下形式 "
      "{译注: 原文中动作一行的左边误作" $CC ".}"
      (MB (set-attr*
           (&Table
            ("(值)" $VV $= (appl $Sigma_1 $VV $CC))
            ("(动作)" $AA $= (&o+ (App $inE $EE) (appl $Sigma_2 $VV $CC)))
            ("(资源)" $RR $= (appl $Pi $VV $CC $..h)))
           'columnalign "left right center left"))
      "其中" (appl $Sigma_1 $VV $CC) "和" (appl $Sigma_2 $VV $CC)
      "是用domain运算以及" $VV "和" $CC "的子集构造出来的项的不交和. "
      "在这个框架中写出的一个语义定义的一个正交" (Em "扩展") "由六个部分组成:"
      (Ul (Li "一个定义扩展的句法的句法构造子集合;")
          (Li "由和" (appl $Sigma_1 $VV $CC) "中的新项所指明的一组新值;")
          (Li "由和" (appl $Sigma_2 $VV $CC) "中的新项所指明的一组新动作;")
          (Li "对于每个新句法构造子, 意义函数" $M:script "中的一个新子句;")
          (Li "资源列表的一个新分量;")
          (Li "对于每种新的动作, " :admin "函数中的一个新子句."))
      "除第一个之外, 这些组成部分中的任何一个都可以省略.")
   (P "根据domain规格的形式, 一个语言扩展的语义domain是原始语言的语义domain的超集. "
      "扩展过程向语义domain添加新的元素. "
      "更确切地说, 有两对函数把原始domain" $VV ", " $CC
      "与对应的扩大的domain" $VV^ ", " $CC^ "联系起来. "
      "首先, 有一对函数" (&: (Inj $VV $VV^) (&--> $VV $VV^))
      "和" (&: (Inj $CC $CC^) (&--> $CC $CC^))
      ", 它们把较小domain中的值和计算注入到较大domain中的值和计算. "
      "这些注入实际上是恒等函数 (在universal domain上解释). "
      "其次, 有一对函数" (&: (Proj $VV^ $VV) (&--> $VV^ $VV))
      "和" (&: (Proj $CC^ $CC) (&--> $CC^ $CC))
      ", 它们把较大domain中的值和计算投影到较小domain的值和计算上. "
      "这些投影把值和计算的任何带有新标签的分量映射到" $bottom
      ", 并保持所有其他分量不变. 总之,"
      (MB (split2 (: (&compose (Inj $VV $VV^) (Proj $VV^ $VV))
                     $= (Proj $VV^ $VV) $sqsube (_ $I $VV^))
                  "并且"
                  (&= (Inj $VV $VV^) (_ $I $VV))))
      "以及"
      (MB (split2 (: (&compose (Inj $CC $CC^) (Proj $CC^ $CC))
                     $= (Proj $CC^ $CC) $sqsube (_ $I $CC^))
                  "并且"
                  (&= (Inj $CC $CC^) (_ $I $CC)))
          ",")
      "其中" (_ $I $X) "是domain" $X "上的恒等函数.")
   (P "非正式地说, 这一性质断言, 在可扩展语义的框架中, "
      "添加一个编程构造对应于向含义空间添加一个新的" (Q "维度") ". "
      "这种添加满足如下性质. 令" $M:script "是核心语言的意义函数, 并令"
      $M:script^ "是一个" (Em "任意") "扩展的意义函数. "
      "如果" $e "是核心语言中的一个表达式, " $rho "是"
      (&-o-> $Var $VV) "中的一个环境, 而" $rho^ "在" (&-o-> $Var $VV^)
      "中, 那么"
      (MB (&= (: (Sem $M:script $e) $rho)
              (app (Proj $CC^ $CC)
                   (: (Sem $M:script^ $e) (par0 (&compose (Inj $VV $VV^) $rho))))))
      "并且"
      (MB (&sqsube (app (Inj $CC $CC^)
                        (: (Sem $M:script $e) (par0 (&compose (Proj $VV^ $VV) $rho^))))
                   (: (Sem $M:script^ $e) $rho^))
          ".")
      "读者也许想把这个一般陈述与关于直接语义和延续语义之间关系的众多论文 "
      "[13, 18, 26, 30, 34] 进行对比.")
   (H2. "组合解释器")
   (P "给定Core Scheme的一个扩展直接语义, 我们应该能够以模块化的方式实现Core Scheme的解释器. "
      "一个明显的可能性是把domain被加项以及函数" $M:script "和" :admin "的子句连接起来. "
      "这种方法产生由独立的片段定义的整体式 (monolithic) 解释器, "
      "但把它们粘合在一起的方法基于程序" (Em "文本") ", "
      "因此超出了大多数编程语言的能力范围.")
   (P "一种替代方案是使用一个遵循CLOS [6] 精神的面向对象编程语言. "
      "每个语言片段会扩展一个已有语言的资源列表, 以及解释器和管理者方法. "
      "此外, 一个语言片段还会引入新的局部数据构造子.")
   (P "这里我们介绍另一种基于Rice Scheme的模块系统的替代方案. "
      "它受到Steele即将发表的关于组合单子 (monad) 的论文 [33] 的启发. "
      "本节中的" (Ref "base-module") "到" (Ref "store-module-2")
      "以及附录B包含完整的代码.")
   (P "一个语言被实现为一个导出三个绑定的模块:"
      (Ol (Li "一个解释器函数;")
          (Li "一个管理者函数; 以及")
          (Li "一个资源列表."))
      (Ref "base-module") "中的基本语言模块解释两个常量, " $Err "和" $Loop
      ", 它们具有显然的含义; 它实现了" (Ref "ext-direct")
      "的管理者, 并导出一个空的资源列表.")
   ((Fig #:id "base-module")
    (B "[基础语言的模块]")
    (CodeB "(define-const-structure (Loop))
(define-const-structure (Err))

(define baseM
  (module (Interpreter Admin Resources)

    (define Interpreter
      (lambda (exp env)
        (match exp
          [($ Loop) (printf &quot;looping~n&quot;) (Interpreter exp env)]
          [($ Err) (inExc 'error)]
          [exp (error 'Interpreter &quot;can't interpret ~s&quot; exp)])))

    (define Admin
      (lambda (comp resources)
        (match comp
          [($ Val v) (cons v resources)]
          [($ FX continuation ($ Exc n))
           (error 'Admin &quot;exception raise:~~s~n&quot; n)]
          [else (error 'Admin &quot;bad effect message: ~s&quot; comp)])))

    (define Resources '()) ))"))
   (P "每个" (Em "语言片段") "由一个包含两个部分的文件组成. "
      "第一部分是一组 (抽象) 句法构造子. "
      "为了便于语言的交互式用户访问, 句法构造子是全局的. "
      "第二部分是一个" (Em "语言变换器 (language transformer)")
      ", 即一个把一个语言实现映射到一个更丰富的语言实现的函数. "
      "语言变换器中的解释器函数为语言片段引入的每个新句法设施包含一个子句. "
      "当局部解释器函数解释一个子表达式时, 它必须调用" (Em "完整")
      "语言的解释器, 因为子表达式可能包含来自一个尚待添加的语言片段的设施. "
      "类似地, 语言变换器的局部管理者必须处理该变换器引入的所有新资源和效应. "
      "但同样地, 在动作被执行之后, 它必须调用" (Em "完整")
      "语言的管理者, 以便所有可能的计算都能被管理.")
   (P "作为一个典型的语言片段, 考虑" (Ref "cbv-module")
      ", 它定义了Pure Scheme的传值" $lambda "记号的解释. 语言变换器的签名是"
      (TransSig $CBVM (_ $CC $x) (_ $CC $x))
      "其中" (&= (_ $CC $x) (&o+ $VV (par0 (&o* (par0 (&--> $VV $CC)) $EE)))) ". "
      "也就是说, 管理者只处理基本计算, 返回一个值和一个错误. "
      "一旦所得到的语言模块被送入控制语言的语言变换器, 这一点就会改变. "
      "后一个变换器的签名清楚地展示了这一变换:"
      (TransSig $controlM (_ $CC $x) (_ $CC $c))
      "(其中"
      (&= (_ $CC $c)
          (&o+ $VV (par0 (&o* (par0 (&--> $VV $CC))
                              (par0 (&o+ $EE (par0 (&--> (par0 (&--> $VV $CC)) $CC))))))))
      "). 现在管理者还按照" (Ref "control-spec") "中的规格处理控制消息. "
      "关于语言实现的其余部分, 包括值构造子, " $Handler
      "以及环境的定义, 我们请读者参阅附录B.")
   ((Fig #:id "cbv-module")
    (B "[传值λ记号的模块]")
    (CodeB ";; Syntax: e ::= (var x) | (lam x e) | (app e e)

(define-const-structure (var x))
(define-const-structure (lam x M))
(define-const-structure (app M N))

(define CBVM
  (lambda (language)
    (import ([language (InterpPrev Interpreter) Admin Resources])
      (module (Interpreter Admin Resources)

        ;; Semantics: V = Proc(V -> C)

        (define-const-structure (Proc closure))
        (define inProc make-Proc)

        (define Interpreter
          (lambda (exp env)
            (match exp
              [($ var x)
               (inVal (env x))]
              [($ lam x exp)
               (inVal (inProc (lambda (v) (InterpTop exp (Extend env x v)))))]
              [($ app exp1 exp2)
               (Handler (InterpTop exp1 env)
                 (lambda (f)
                   (Handler (InterpTop exp2 env)
                     (lambda (a)
                       (match f
                         [($ Proc g) (g a)]
                         [_ (inExc 'error)])))))]
              [exp (InterpPrev exp env)])))))))"))
   ((Fig #:id "control-module")
    (B "[catch和throw的模块]")
    (CodeB ";; Syntax: e ::= (catch x e) | (throw x e)

(define-const-structure (catch x M))
(define-const-structure (throw x N))

(define controlM
  (lambda (language)
    (import ([language (InterpPrev Interpreter) (AdminPrev Admin) Resources])
      (module (Interpreter Admin Resources)

        ;; Semantics: V = K(V -> C); C = FX(V -> C, Con(V -> C))

        (define-const-structure (K v->c))
        (define inK make-K)

        (define-const-structure (Con v->c))
        (define inCon (lambda (x) (inCC (make-Con x))))

        (define Interpreter
          (lambda (exp env)
            (match exp
              [($ catch x exp)
               (inCon
                 (lambda (k)
                   (Handler (InterpTop exp (Extend env x (inK k))) k)))]
              [($ throw x exp)
               (Handler (InterpTop exp env)
                 (lambda (v)
                   (match (env x)
                     [($ K k) (inCon (lambda (kp) (k v)))]
                     [_ (inExc 'error)])))]
              [exp (InterpPrev exp env)])))

        (define Admin
          (lambda (comp resources)
            (match comp
              [($ FX continuation ($ Con r))
               (AdminTop (r continuation) resources)]
              [comp (AdminPrev comp resources)])))))))"))
   (P "最后, 完整语言的实现由四个定义组成:"
      (Ol (Li "完整语言的模块, 通过以函数方式复合语言片段并把它们应用于基础模块来定义;")
          (Li "完整语言的解释器的一个全局定义;")
          (Li "完整资源集合的管理者的一个全局定义; 以及")
          (Li "程序的意义函数."))
      "下面是Core Scheme的定义:"
      (CodeB "(define CoreScheme (storeM (controlM (CBVM (arithmM baseM)))))

;; The global interpreter:
(define InterpTop (import ([CoreScheme Interpreter]) Interpreter))

;; The complete administrator
(define AdminTop (import ([CoreScheme Admin]) Admin))

;; The Program Meaning Function:
(define Program
  (import ([CoreScheme Resources])
    (lambda (P)
      (AdminTop (InterpTop P Empty) Resources))))")
      "在" $CoreScheme "的定义中, 函数复合的顺序是无关紧要的; "
      "在这条链中省略一个语言片段, 就定义了Core Scheme的一个不含相应构造的子语言.")
   (H2. "相关工作")
   (P (Em "单子.") " Moggi [21, 22, 20] 最近的把计算的概念表述为单子的工作, "
      "由Wadler [38] 推广, 其部分动机是指称规格缺乏模块性, "
      "以及由此导致的对计算之间的逻辑关系缺乏理解. "
      "粗略地说, 一个单子对应于一个三元组, 它由一个把值的domain映射到计算的domain的domain构造子"
      $T ", 一个从值到计算的注入" $eta ", 以及一个用于计算的组合变换" $mu "组成. "
      "不同的单子表达不同风格的指称规格, 从而表达不同风格的计算. "
      "一些典型的单子是存储传递单子, 延续传递单子, 以及用于非确定性算术的单子.")
   (P "在单子上参数化指称直接模型, 会导向更灵活的语言规格. "
      "一个" (Q "单子语义 (monadic semantics)") "的参数化domain方程是"
      (MB (&= $VV (&o+ (_ $NN $bottom) (par0 (&-->s $VV (app $T $VV))))) ";")
      "数码的意义指派把一个数码映射到它在计算domain中的注入:"
      (MB (&= (: (Sem $M:script (Num $n)) $rho) (app $eta $n)) ".")
      "现在, 如果对于某个固定的domain" $AA ", 有"
      (&= (app $T $VV) (&--> (par0 (&--> $VV $AA)) $AA)) "并且"
      (MB (&= (app $eta $n) (: $slambda $k $: (&--> $VV $AA) $. (app $k $n))) ",")
      "那么该语义是一个延续语义. 如果"
      (&= (app $T $VV) (&--> $Sto (par0 (&o* $VV $Sto)))) "并且"
      (MB (&= (app $eta $n) (: $slambda $sigma $: $Sto $. (tupa0 $n $sigma))) ",")
      "那么实例化该语义就得到一个存储传递语义. "
      "一般而言, 给定一个在单子上参数化的直接语义, 很容易转向一个基于不同风格的语义, "
      "但结果是, 一个数码会被指派不同的domain元素" -- "取决于所选择的单子.")
   (P "不幸的是, 单子的组合带来了严重的困难. "
      "由于单子不能被直接复合, Moggi [20:ch.4] 引入了单子构造子 (monad constructor) 的概念, "
      "它们大致是单子变换器 (monad transformer), 并且可以组合. "
      "然而, 以不同的顺序组合同样的两个单子构造子, 常常产生不同的结果. "
      "一个典型的例子是异常单子与存储单子的组合. "
      "一种组合在抛出异常的过程中撤销副作用, 另一种则不会.")
   (P "为了克服与组合单子相关的问题, Steele [33] 引入了"
      (Em "伪单子 (pseudo-monad)") "的想法, 它是Moggi的概念的一种放宽. "
      "基于伪单子的函数复合, Steele展示了如何从语言片段的解释器构建语言的解释器. "
      "第4章受到了他的方法的启发. "
      "但伪单子和单子有同样的问题: 指称仍然不稳定, 组合也不对称. "
      "目前还不清楚单子方法与我们的方法之间的权衡是什么.")
   (P (Em "维也纳指称语义学派.")
      " Control Scheme的扩展直接语义与维也纳指称语义学派 [3, 4, 15:52&ndash;55] "
      "建模跳转和标签的方法有关, 这一方法被Allison [2] 部分地重新发现. "
      "实际上, 根据这一学派的思想对块结构语言中的标签和" :goto
      "的处理, 可以被看作我们的技术的一个实例. "
      "在他们的框架中, 一个语句指称一个状态变换器, "
      "它要么返回一个新的状态, 要么返回一个句法标签 (与一个状态组合在一起). "
      "返回一个状态对应于返回一个值; 返回一个标签是一个效应消息. "
      "传播效应的责任落在语句的复合函数和其他" (Q "胶水") "函数上. "
      "当一个标签到达顶层时, 一个对应于我们的管理者的异常处理器实施相应的跳转. "
      "Blikle和Tarlecki [4] {译注: 原文误作Tarleki.} 表明, 通过使用VDM技术, "
      "指称规格可以使用集合而非domain, 使用部分函数而非连续函数. "
      "维也纳学派没有把这一技术应用于其他命令式语言构造的规格 "
      "[D. Bj&oslash;rner; 信件, 1990年6月5日].")
   (H2. "结论")
   (P "扩展直接语义是一种表述编程语言的指称规格的新方法. "
      "与传统方法不同, 基于扩展直接语义的语言规格很容易用正交的语言构造来扩展. "
      "最重要的是, 这些扩展保持核心语言中句法成分的指称.")
   (P "尽管我们的阐释集中于函数式语言的扩展, 命令式语言也可以用类似的方式来规定. "
      "一个类Algol语言的指称模型会很容易构造. "
      "实际上, 鉴于存储是一个中央资源, 而不是一个被传来传去的第一级对象, "
      "这个模型可能更接近一个实现.")
   (P "对这两种指称规格方法的更全面的比较必须解决几个问题. "
      "首先, 我们应该研究扩展直接模型是否像替代的延续语义和存储语义一样精确地刻画语言, "
      "也就是说, 我们应该检查新模型是否具有不同的完全抽象 (full abstraction) 性质. "
      "其次, 我们需要理解每种方法提供了什么实现上的优势. "
      "最后, 我们应该研究语言的模块化逻辑的设计, "
      "因为指称语义的存在理由 (raison d'&ecirc;tre) 在于它对于程序推理的有用性.")
   (P (B "致谢.") " 作者感谢Mitch Wand在POPL'89上关于扩展直接模型中指称的稳定性的有益讨论. "
      "Andrew Wright的软类型系统 (Soft Type System) 是验证语言变换器的类型性质的一个重要工具. "
      "我们也感谢Dan Friedman, Brian Milnes, Amr Sabry和Steve Weeks对本文早期草稿的意见. "
      "Scheme代码是用Dorai Sitaram的SLaTeX排版的.")
   (Appendix "A" "domain规格的记号")
   (P "本文中给出的语义定义依赖于domain构造子" $o* " (粉碎积 (smash product)), "
      $o+ " (合并和 (coalesced sum)), " (_ $d* $bottom) " (提升 (lifting)), "
      $--> " (连续函数) 以及" $-o-> " (有限连续函数). "
      "在本附录中, 我们简要描述这些构造中的每一个以及相关的记号.")
   (P "domain算子" $o* ", " $o+ "和" (_ $d* $bottom) "由以下方程定义:"
      (eqn*
       ((&o* $AA $BB) $=
        (&cup (setI (tu0 $a $b)
                    (&cm (&in $a $AA) (&in $b $BB) (&!= $a $bottom) (&!= $b $bottom)))
              (setE $bottom)))
       ((&o+ $AA $BB) $=
        (&cup (setI (tu0 $true $a) (&in $a (: $AA $setminus (setE $bottom))))
              (setI (tu0 $false $b) (&in $b (: $BB $setminus (setE $bottom))))
              (setE $bottom)))
       ((_ $AA $bottom) $=
        (&cup (setE $bottom) (setI (tu0 $true $a) (&in $a $AA)))))
      "值" $true "和" $false "在复合对象的构造中被用作" (Q "标签")
      "; 无论" $a "的值是什么 (包括" (&= $a $bottom) "), 对象" (tu0 $true $a)
      "和" (tu0 $false $a) "都不同于" $bottom ". 记号"
      (MB (&o+ (App $inA $AA) (App $inB $BB)))
      "所指称的domain与"
      (MB (&o+ $AA $BB) ";")
      "完全相同; 标签" $inA "和" $inB "隐式地定义了函数"
      (&: $inA (&--> $AA (&o+ $AA $BB))) "和" (&: $inB (&--> $BB (&o+ $AA $BB)))
      ", 其中"
      (eqn*
       ((App $inA $x) $= (Cases (list (tu0 $true $x) (: "如果" (&in $x $AA)))
                                (list $bottom "否则")))
       ((App $inB $x) $= (Cases (list (tu0 $false $x) (: "如果" (&in $x $BB)))
                                (list $bottom "否则"))))
      "最后, 记号"
      (MB (App $inA $AA))
      "与"
      (MB (&o+ (App $inA $AA) (App $inPhi $Phi)))
      "的含义完全相同, 其中" $Phi "是只包含发散元素" $bottom "的domain.")
   (P "二元domain构造" $o* "和" $o+ "显然可以推广到任何有限元数" (&> $n $2) ". "
      "在这种情况下, 我们使用" $n "个不同的标签, 例如数字"
      (&cm $1 $2 $..h (&- $n $1)) ". "
      (&cm (_ $AA $1) $..h (_ $AA $n)) "的" $n "元积写作"
      (MB (&o* (_ $AA $1) $..h (_ $AA $n)) ".")
      "类似地, 带有注入" (&cm (_ $inA $1) $..h (_ $inA $n)) "的"
      (&cm (_ $AA $1) $..h (_ $AA $n)) "的" $n "元和写作"
      (MB (&o+ (App (_ $inA $1) (_ $AA $1)) $..h (App (_ $inA $n) (_ $AA $n))) "."))
   (P "由于和中标签值的选择是任意的, 我们使用" (Em "模式匹配记号 (pattern matching notation)")
      "来识别和的分量. 它也提供了一种把和投影到一个分量domain上的方便机制. 给定和"
      (MB (&o+ (App (_ $inA $1) (_ $AA $1)) $..h (App (_ $inA $n) (_ $AA $n))) ",")
      "表达式"
      (MB (set-attr*
           (&table
            (list (list (split2 $case $y $of))
                  (list (Indent 1 (set-attr*
                                   (&table
                                    (list (list (: $lb0 (App (_ $inA $1) (_ $x $1))) $=>
                                                (: (_ $e $1) $rb0))
                                          (list $..h $=> $)
                                          (list (: $lb0 (App (_ $inA $n) (_ $x $n))) $=>
                                                (: (_ $e $n) $rb0))))
                                   'columnalign "left center left")))))
           'columnalign "left"))
      "的含义是:"
      (MB (set-attr*
           (&table
            (list (list (: "如果" $y "具有形式" (App (_ $inA $1) (_ $x $1)) ", 那么" (_ $e $1)))
                  (list (: "否则" $..h))
                  (list (: "否则如果" $y "具有形式" (App (_ $inA $n) (_ $x $n))
                           ", 那么" (_ $e $n)))
                  (list (: "否则" $bottom "."))))
           'columnalign "left"))
      "{译注: 原文第三行误作" (App (_ $inA $1) (_ $x $1))
      ", 原文的case表达式末尾还多了两个右括号.} "
      "每个子表达式" (_ $e $i) "可以包含变量" (_ $x $i)
      ", 以及在包围" $case "表达式的上下文中绑定的任何变量.")
   (P "domain" (&--> $AA $BB) "是所有满足对于所有链" $X "都有"
      (&= (app $f (: $bigsqcup $X)) (: $bigsqcup (app $f $X)))
      "的" (Em "连续") "函数" (&: $f (&--> $AA $BB)) "的集合. "
      "连续函数的domain按集合包含排序 (把函数看作有序对的集合). "
      "记号" (: $slambda $x $: $AA $. $e $: $BB) "定义了匿名函数"
      (: (app $f $x) $= $e $: (&--> $AA $BB)) ".")
   (P (&-->s $AA $BB) "表示从domain" $AA "到domain" $BB "的严格函数的domain. "
      "domain" (&--> $AA $BB) "中的一个函数是严格的, 如果"
      (&= (app $f (_ $bottom $AA)) (_ $bottom $BB)) ".")
   (P "有限函数domain构造" (&-o-> $AA $BB) "只对" (Em "平坦") "domain" $AA "有定义:"
      (MB (&= (&-o-> $AA $BB)
              (setI (&in $S (&c* $AA $BB))
                    (&cm (: $S "是有限的") (&in $S (&-->s $AA $BB))))))
      "给定两个有限函数" (&in (&cm $f $g) (&-o-> $AA $BB)) ", " (&sqsube $f $g)
      ", 如果要么 (" (I "i") ") " $f "是" $bottom ", 要么 (" (I "ii") ") "
      $f "和" $g "具有相同的定义域" $D ", 并且对于所有" (&in $d $D) ", "
      (&sqsube (app $f $d) (app $g $d)) ". "
      "我们常常把" (&-o-> $AA $BB) "的元素解释为" (&--> $AA $BB) "中的函数.")
   (Appendix "B" "存储模块和全局定义")
   (P "本节给出Core Scheme的模块化解释器的其余部分. "
      (Ref "handler-defs") "和" (Ref "env-defs")
      "包含全局定义, 即, handler的定义以及处理环境的函数的定义. "
      (Ref "arith-module") "是处理算术的语言片段; "
      "一个类似的用于布尔表达式的片段因篇幅原因被省略了. "
      "最后, " (Ref "store-module") "和" (Ref "store-module-2")
      " (第1部分和第2部分) 给出了引用单元模块. "
      "{译注: 原文此处只写了图14, 实际指图13和图14两部分.}")
   ((Fig #:id "handler-defs")
    (B "[全局定义]")
    (CodeB ";; Semantic Framework:
;; V = &perp;
;; C = Val(V) + Comp(V -> C, Exc)

(define-const-structure (Val Num_n))
(define inVal make-Val)

(define-const-structure (FX V->C Receiver))
(define inCC (lambda (x) (make-FX inVal x)))

(define-const-structure (Exc num))
(define inExc (lambda (x) (inCC (make-Exc x))))

(define Handler
  (lambda (computation consumer)
    (match computation
      [($ Val v) (consumer v)]
      [($ FX continuation request)
       (make-FX
         (lambda (x) (Handler (continuation x) consumer)) request)])))"))
   ((Fig #:id "env-defs")
    (B "[环境]")
    (CodeB ";; Environments:
(define Empty
  (lambda (x)
    (error 'env &quot;Impossible: an open program ~s&quot; x)))

(define Extend
  (lambda (env x v)
    (match x
      [(? symbol?) (match-lambda [(? symbol? y) (if (eq? x y) v (env y))])]
      [else (error 'Extend &quot;not a variable: ~s&quot; x)])))"))
   ((Fig #:id "arith-module")
    (B "[算术的模块]")
    (CodeB ";; Syntax: e ::= (int n) | (add1 e) | (sub1 e)

(define-const-structure (int n))
(define-const-structure (add1 M))
(define-const-structure (sub1 M))

(define arithmM
  (lambda (language)
    (import ([language (InterpPrev Interpreter) Admin Resources])
      (module (Interpreter Admin Resources)

        ;; Semantics: V = Num(num)

        (define-const-structure (Num num))
        (define inNum make-Num)

        (define Interpreter
          (lambda (exp env)
            (match exp
              [($ int m) (inVal (inNum m))]
              [($ add1 exp) (Handler (InterpTop exp env) (op add1))]
              [($ sub1 exp) (Handler (InterpTop exp env) (op sub1))]
              [exp (InterpPrev exp env)])))

        (define op
          (lambda (f)
            (lambda (n)
              (match n
                [($ Num m) (inVal (inNum (f m)))]
                [_ (inExc 'error)]))))))))"))
   ((Fig #:id "store-module")
    (B "[引用单元的模块: 第1部分]")
    (CodeB ";; Syntax e ::= (ref e) | (deref e) | (setref e e)

(define-const-structure (ref x))
(define-const-structure (deref M))
(define-const-structure (setref M N))

(define storeM
  (lambda (language)
    (import ([language
              (InterpPrev Interpreter)
              (AdminPrev Admin)
              (ResPrev Resources)])
      (module (Interpreter Admin Resources)

        ;; Semantics:
        ;; V = Loc(L)
        ;; C = FX(V -> C, Ref(V) + Der(L) + Set(L, V))

        (define-const-structure (Loc num))
        (define inLoc make-Loc)

        (define-const-structure (Ref v))
        (define inRef (lambda (x) (inCC (make-Ref x))))

        (define-const-structure (Der l))
        (define inDer (lambda (l) (inCC (make-Der l))))

        (define-const-structure (Set l v))
        (define inSet (lambda (l v) (inCC (make-Set l v))))

        (define Interpreter
          (lambda (exp env)
            (match exp
              [($ ref exp) (Handler (InterpTop exp env) inRef)]
              [($ deref exp)
               (Handler (InterpTop exp env)
                 (lambda (v)
                   (match v
                     [($ Loc l) (inDer l)]
                     [_ (inExc 'error)])))]
              [($ setref exp1 exp2)
               (Handler (InterpTop exp1 env)
                 (lambda (l)
                   (Handler (InterpTop exp2 env)
                     (lambda (v)
                       (match l
                         [($ Loc l) (inSet l v)]
                         [_ (inExc 'error)])))))]
              [exp (InterpPrev exp env)])))"))
   ((Fig #:id "store-module-2")
    (B "[引用单元的模块: 第2部分]")
    (CodeB "        (define Admin
          (lambda (comp resources)
            (let ([store (ExtractStore resources)])
              (match comp
                [($ FX continuation ($ Ref v))
                 (AdminTop (continuation (inLoc (new store)))
                   (cons (make-StoreRes (cons (cons (new store) v) store))
                     resources))]
                [($ FX continuation ($ Der l))
                 (AdminTop (continuation (lookup store l)) resources)]
                [($ FX continuation ($ Set l v))
                 (AdminTop (continuation (inLoc l))
                   (cons (make-StoreRes (update store l v)) resources))]
                [comp (AdminPrev comp resources)]))))

        (define-const-structure (StoreRes s))

        (define Resources (cons (make-StoreRes '()) ResPrev))

        (define ExtractStore
          (lambda (resources)
            (match resources
              [(($ StoreRes s) . res) s]
              [(_ . res) (ExtractStore res)]
              ['() (error 'Extract &quot;impossible&quot;)] )))

        (define new length)

        (define lookup
          (lambda (store l)
            (let L ([store store]);; assq yields less precise types
              (match store
                [(((? number? m) . v) . s) (if (= m l) v (L s))]
                ['() (error 'lookup &quot;impossible&quot;)]))))

        (define update
          (lambda (store l v)
            (match store
              [(((? number? m) . u) . s)
               (if (= l m)
                   (cons (cons l v) s)
                   (cons (cons m u) (update s l v)))]
              ['() (error 'update &quot;impossible&quot;)]))) ))))"))

   )))
