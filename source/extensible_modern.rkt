#lang racket
(provide extensible_modern.html)
(require SMathML)
(define (Sf str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define (Tt str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define (Kw str)
  (Mi str #:attr* '((mathvariant "bold"))))
;semantic constructors
(define $Val (Sf "Val"))
(define $FX (Sf "FX"))
(define $Exc (Sf "Exc"))
(define $Num (Sf "Num"))
(define $Truth (Sf "Truth"))
(define $Proc (Sf "Proc"))
(define $Loc (Sf "Loc"))
(define $K (Sf "K"))
(define $Ref (Sf "Ref"))
(define $Der (Sf "Der"))
(define $Set (Sf "Set"))
(define $Con (Sf "Con"))
;object syntax
(define $loop (Tt "loop"))
(define $err:t (Tt "err"))
(define $add1 (Tt "add1"))
(define $sub1 (Tt "sub1"))
(define $if (Tt "if"))
(define $zero? (Tt "zero?"))
(define $#t (Tt "#t"))
(define $#f (Tt "#f"))
(define $ref (Tt "ref"))
(define $deref (Tt "deref"))
(define $setref! (Tt "setref!"))
(define $catch (Tt "catch"))
(define $throw (Tt "throw"))
(define (Tn n) (Tt (format "~a" n)))
(define $5:t (Tn 5))
(define $7:t (Tn 7))
(define $10:t (Tn 10))
;metalanguage
(define $case (Kw "case"))
(define $of (Kw "of"))
(define $ask (Mi "ask"))
(define $err (Mi "err"))
(define $admin (Mi "admin"))
(define $new (Mi "new"))
(define $Env (Mi "Env"))
(define $Sto (Mi "Sto"))
(define $Var (Mi "Var"))
(define $Exp (Mi "Exp"))
(define $Ans (Mi "Ans"))
(define $Msg (Mi "Msg"))
(define $ell (Mi "&ell;"))
(define $wild (Mi "_"))
(define $emptyset (Mi "&empty;"))
(define $inc (Mi "inc"))
(define $10 (Mn "10"))
(define $11 (Mn "11"))
(define $120 (Mn "120"))
(define $-o-> (Mo "&#8692;"))
(define-infix*
  (&-o-> $-o->))
(define split2 (&split 2))
(define $af0
  (Mo "&af;" #:attr* '((lspace "0") (rspace "0"))))
(define (Ap f . x*)
  (apply split2 f x*))
(define (Mk c . x*)
  (: c $af0 (apply tu0 x*)))
(define (S . x*)
  (par0 (apply split2 x*)))
(define (Lam* x e)
  (S $lambda (par0 x) e))
(define (Den e [r $rho])
  (Ap (&db0 e) r))
(define (Admin c r)
  (appl $admin c r))
(define (Ask a)
  (app $ask a))
(define (Ext r x v)
  (: r (bra0 (: x $\|-> v))))
(define (Bind x c)
  (: x $lArr c (Mo ";" #:attr* '((lspace "0") (rspace "0.6em")))))
(define (Case x)
  (: (split2 $case x $of) (Mspace #:attr* '((width "0.4em")))))
(define $bar
  (Mo "|" #:attr* '((form "prefix") (lspace "0") (rspace "0.5em"))))
(define (Arm p e)
  (: (Mspace #:attr* '((width "1.5em"))) $bar p $=> e))
(define (Fn x e)
  (: $lambda x $. e))
(define (Indent n x)
  (: (Mspace #:attr* `((width ,(format "~aem" n)))) x))
(define (Row n x)
  (list $ $ (Indent n x)))
(define (grammar l)
  (MB (set-attr* (&table l)
                 'columnalign "right center left left"
                 'columnspacing "0.5em 0.5em 2em")))
(define-syntax-rule (Grammar (x ...) ...)
  (grammar (list (list x ...) ...)))
(define-Entry*
  (Fig "图" "fig"))
(define extensible_modern.html
  (TnTmPrelude
   #:title "可扩展指称语义, 现代重述"
   #:css "styles.css"
   (H1. "可扩展指称语义, 现代重述")
   (P "本文是对Robert Cartwright和Matthias Felleisen的论文"
      (I "Extensible Denotational Language Specifications")
      " (TACS 1994) 的一个重述. 原文的译文见"
      (A "可扩展指称性语言描述" #:attr* '((href "extensible_spec.html")))
      ". 原文的思想至今仍然新鲜, 但它的记号 (粉碎积, 合并和, 提升) "
      "和代码 (Rice Scheme的第一级模块) 都已经很难读了. "
      "这里我们用一个可以在Racket 9上运行的实现为线索, "
      "从头讲一遍这个故事, 并尽量使用今天的程序员熟悉的记号. "
      "文中的每个公式都直接对应一段代码, 读者可以对照着读.")
   (H2 "记号约定")
   (P "本文的公式中, 字体是有含义的:"
      (Ul (Li "等宽字体表示对象语言 (即被定义的语言) 的句法, 例如"
              (S $add1 $e) ", " (S $if $e_1 $e_2 $e_3) ", 以及数码" $5:t
              ". 数码" $5:t "是句法, 而" $5 "是它所表示的整数. "
              "唯一的例外是" $lambda ": 我们把抽象写成" (Lam* $x $e)
              ", 而在程序文本中它写作" (Tt "lambda") ". "
              "黑底白字只用于Racket代码, 例如" (Code "arith-mixin") ".")
          (Li "斜体表示元变量 (metavariable), 并且我们固定以下用法: "
              $e "是表达式, " $x "是变量, " $n "是整数, "
              (&cm $v $a) "是值, " $c "是计算, " $k "是句柄, "
              $kappa "是延续, " (&cm $f $g) "是函数, " $rho "是环境, "
              $sigma "是存储, " $r "是资源, " $ell "是位置, "
              $m "是错误说明.")
          (Li "无衬线的直立字体表示" (Em "语义构造子")
              ", 即值, 计算和动作的各种形状, 例如" $Val ", " $FX ", "
              $Num ", " $Ref ". 它们与实现中的Racket结构体同名, 例如"
              (Mk $Num $5) "对应实现中的" (Code "(Num 5)") ".")
          (Li "普通的直立字体表示元语言中的函数, 例如" $admin ", "
              $ask ", " $err ".")
          (Li "粗体表示元语言的关键字, 本文只用到" $case "和" $of "."))
      "此外还有以下记号:"
      (Ul (Li (Den $e) "是表达式" $e "在环境" $rho "中的含义, "
              (Ext $rho $x $v) "是把" $x "绑定到" $v "之后得到的环境; "
              (Ext $sigma $ell $v) "同理.")
          (Li (Fn $x $e) "是元语言中的匿名函数, " (app $f $x)
              "是函数应用, " (tu0 $v $r) "是序对.")
          (Li (: (Case $x) $p $=> $e $\| $..h) "按" $x
              "的形状分情形; " $wild "匹配任何东西.")
          (Li (: (Bind $x $c) $e) "是顺序组合, 定义见"
              (Ref "sec-seq") ".")
          (Li "我们不使用domain理论: 集合与函数按通常的意思理解, "
              $bottom "表示发散. 原文中的严格性, 提升等细节对本文的论述并不重要.")))
   (H2. "问题: 数码" $5:t "指称什么?")
   (P "指称语义给每个程序句法成分指派一个数学对象. "
      "最简单的句法成分莫过于数码" $5:t ". "
      "然而它的指称取决于语言的其余部分:"
      (MB (set-attr*
           (&Table
            ((Mtext "纯函数式语言") (&= (Den $5:t) $5)
             (&: (&db0 $5:t) (&-> $Env $V)))
            ((Mtext "加上引用单元") (&= (Ap (&db0 $5:t) $rho $sigma) (tu0 $5 $sigma))
             (&: (&db0 $5:t) (&-> $Env $Sto (par0 (&c* $V $Sto)))))
            ((Mtext "再加上延续") (&= (Ap (&db0 $5:t) $rho $kappa $sigma) (Ap $kappa $5 $sigma))
             (&: (&db0 $5:t) (&-> $Env (par0 (&-> $V $Sto $Ans)) $Sto $Ans))))
           'columnalign "left left left"
           'columnspacing "2em"))
      "每向语言添加一种效应, 所有已有的子句都要重写一遍, "
      "包括那些与新效应毫无关系的子句, 例如数码的子句. "
      "更糟糕的是, 程序员在推理一段纯函数式代码时, "
      "心里想的明明是第一行的简单含义, 但语言的正式定义给的却是第三行; "
      "要从后者中恢复前者, 就必须证明一个不平凡的定理.")
   (P "本文的目标是给出一种指称语义的写法, 在这种写法中"
      (MB (&= (Den $5:t) (Mk $Val (Mk $Num $5))))
      "在" (Em "每一个") "扩展中都成立, "
      "并且添加一种新的语言构造只需要" (Em "添加") "新的定义, "
      "而不需要修改任何旧的定义.")
   (H2. "直觉: 进程与内核")
   (P "设想一个操作系统. 进程自己做算术, 自己调用函数; "
      "但它不能自己去读写磁盘, 而是发出一个" (Em "系统调用")
      ", 把控制权交给内核. 内核完成请求之后, 再把结果交还给进程, 让它从中断处继续.")
   (P "扩展直接语义 (extended direct semantics) 就是这样组织一个语言的. "
      "程序的每个句法成分的含义是一个" (Em "计算 (computation)")
      ", 它要么是一个" (Em "值") ", 要么是一个" (Em "请求") ". "
      "请求由两部分组成:"
      (Ul (Li "一个" (Em "动作 (action)") ", 说明要求做什么, "
              "例如 " (Q "分配一个存有7的单元") ";")
          (Li "一个" (Em "句柄 (handle)") ", 即得到回复之后的其余计算, "
              "它是一个从值到计算的函数."))
      "所有的全局资源 (例如存储) 都只归一个" (Em "中央管理者")
      $admin "所有. 句法成分之间不传递存储, 也不传递延续; "
      "只有管理者知道它们.")
   (P "这一想法来自操作语义. 在带引用单元的" $lambda "演算中, 表达式"
      (MB (: $E (bra0 (S $ref $v))))
      "不能被局部地归约: 分配一个新单元必须知道整个存储. "
      "但我们可以把它看作一条消息: " (S $ref $v) " 是请求, "
      "求值上下文" $E "就是句柄. 管理者分配一个新位置" $ell
      ", 然后用" $ell "填充" $E "的洞, 计算继续进行. "
      "下面的定义只是把这幅图景翻译成函数.")
   (H2. "框架")
   (H3. "计算")
   (P "设" $V "是值的集合, " $A "是动作的集合 (两者都依赖于语言, 是可扩展的). "
      "计算的集合" $C "由以下两种形状组成:"
      (Grammar
       ($C $::= (Mk $Val $v) (: (Mtext "一个值, 其中") (&in $v $V)))
       ($ $\| (Mk $FX $k $a) (: (Mtext "一个请求, 其中") (&cm (&in $a $A) (&: $k (&-> $V $C))))))
      "这就是框架的全部. 数, 过程, 位置, 延续等等都是" $V
      "的扩展; 错误, 分配, 读写, 控制转移等等都是" $A "的扩展. "
      $C "本身的定义永远不变.")
   (P "我们给两个常用的构造起名字:"
      (eqn*
       ((Ask $a) $:= (Mk $FX $Val $a))
       ((app $err $m) $:= (Ask (Mk $Exc $m))))
      $ask "发出一个动作, 并且约定把管理者的回复直接当作值; "
      (Mk $Exc $m) "是唯一一个所有语言都有的动作: 带着说明" $m "的错误. "
      "说明只是为了调试方便, 下文的公式中我们省略它, 只写" $err ".")
   ((Fig #:id "fig-framework")
    (B "[计算 (framework.rkt)]")
    (CodeB "(struct Val (v))            ; a value
(struct FX (k action))      ; a request: handle k and an action
(struct Exc (msg))          ; the action: error

(define inVal Val)
(define (inAC action) (FX inVal action))   ; i.e. ask
(define (inExc msg) (inAC (Exc msg)))      ; i.e. err"))
   (H3. "顺序组合" #:id "sec-seq")
   (P "一个复合句法成分需要先计算它的子句法成分. "
      "如果子计算给出一个值, 就在局部用掉它; 如果给出一个请求, "
      "就必须把它" (Em "原样上交") ", 只是在句柄的末尾接上 "
      (Q "拿到值之后我还要做的事") ". 我们把这一操作写成"
      (MB (: (Bind $x $c) $e))
      "读作 " (Q "令" $x "为" $c "的值, 然后计算" $e)
      ". 它由下面两个方程定义:"
      (eqn*
       ((: (Bind $x (Mk $Val $v)) $e) $= (: $e (bra0 (: $x $\|-> $v))))
       ((: (Bind $x (Mk $FX $k $a)) $e) $=
        (Mk $FX (Fn $y (: (Bind $x (app $k $y)) $e)) $a)))
      "第一个方程说值被直接交给" $e "; 第二个方程说请求向上传播, "
      "并且它的句柄被扩大了. 一个请求就这样一层一层地向外传播, "
      "最终抵达管理者; 而它携带的句柄恰好就是" (Q "整个程序的其余部分") ".")
   (P "熟悉Haskell的读者会认出, " (: $x $lArr $c $\; $e)
      "就是" (Tt "do") "记号, 而" $C "连同这一操作构成一个单子. "
      "确切地说, " $C "是动作的" (Em "自由单子 (free monad)")
      ": 它对动作本身什么也不解释, 只是忠实地把它们记录下来. "
      "解释动作是管理者的事.")
   ((Fig #:id "fig-handle")
    (B "[顺序组合 (framework.rkt)]")
    (CodeB "(define (handle comp consumer)
  (match comp
    [(Val v)    (consumer v)]
    [(FX k act) (FX (lambda (x) (handle (k x) consumer)) act)]))

;; (handle* ([f c1] [a c2]) body)
;;   = (handle c1 (lambda (f) (handle c2 (lambda (a) body))))
(define-syntax handle*
  (syntax-rules ()
    [(_ () body ...) (let () body ...)]
    [(_ ([x comp] more ...) body ...)
     (handle comp (lambda (x) (handle* (more ...) body ...)))]))"))
   (H3. "管理者与程序的含义")
   (P "管理者" $admin "接受一个计算和当前的资源" $r
      ", 返回程序的最终答案: 一个值 (或错误) 连同最终的资源. "
      "对于不带任何资源的基础语言, 它只有两个子句:"
      (eqn*
       ((Admin (Mk $Val $v) $r) $= (tu0 $v $r))
       ((Admin (Mk $FX $k (Mk $Exc $m)) $r) $= (tu0 (app $err $m) $r)))
      "注意错误的句柄" $k "被直接丢弃了: 这就是" (Q "中止程序") ". "
      "程序" $P "的含义是解释器与管理者的复合:"
      (MB (&= (&db0 $P) (Admin (Den $P $emptyset) $r_0)) ",")
      "其中" $r_0 "是初始资源. 基础语言只有两个程序: 发散的"
      $loop "和报错的" $err:t ":"
      (eqn*
       ((Den $loop) $= $bottom)
       ((Den $err:t) $= $err))
      "任何真正的语言都是这个平凡语言的扩展.")
   (H3. "扩展的形状")
   (P "一个语言片段 (language fragment) 由以下六部分组成, 每一部分都只是"
      (Em "添加") "而不是修改:"
      (Ol (Li "新的句法形式;")
          (Li "新的值, 即" $V "的新形状;")
          (Li "新的动作, 即" $A "的新形状;")
          (Li "新的资源;")
          (Li "解释器" (&db0 $d*) "的新子句, 每个新句法形式一个;")
          (Li "管理者" $admin "的新子句, 每个新动作一个."))
      "下一章依次给出算术, " $lambda "演算, 布尔值, 引用单元和延续这五个片段. "
      "它们都具有同样的形状; 读者读完前两个之后, 其余的大概可以自己写出来.")
   (H2. "语言片段")
   (H3. "算术")
   (P "这个片段添加数码和两个算术运算, 以及一种新的值, 整数:"
      (Grammar
       ($e $::= (&\| $n (S $add1 $e) (S $sub1 $e)) $)
       ($V $::= (&\| $..h (Mk $Num $n)) (&in $n $ZZ)))
      "其中" $V "的定义里的" $..h "表示其他片段添加的形状. 解释器的新子句是"
      (eqn*
       ((Den $n) $= (Mk $Val (Mk $Num $n)))
       ((Den (S $add1 $e)) $= (Bind $x (Den $e)))
       ($ $ (Case $x))
       ($ $ (Arm (Mk $Num $n) (Mk $Val (Mk $Num (&+ $n $1)))))
       ($ $ (Arm $wild $err))))
   (P "这正是我们在第一章许诺的: 数码的含义是" (Mk $Val (Mk $Num $n))
      ", 其中没有存储, 也没有延续. " (S $add1 $e)
      "先计算" $e ", 如果得到一个数就加一, 否则报错. "
      (S $sub1 $e) "同理. 算术片段没有新的动作, 也没有新的资源, "
      "因而不需要修改管理者.")
   ((Fig #:id "fig-arith-code")
    (B "[算术片段 (arith.rkt)]")
    (CodeB ";; syntax
(struct Int (n))
(struct Add1 (e))
(struct Sub1 (e))
;; new value
(struct Num (n))

(define arith-mixin
  (mixin (language&lt;%&gt;) ()
    (super-new)
    (define/override (interpret exp env)
      (match exp
        [(Int n)  (inVal (Num n))]
        [(Add1 e) (handle (interpret e env) (op add1))]
        [(Sub1 e) (handle (interpret e env) (op sub1))]
        [_ (super interpret exp env)]))))

(define ((op f) v)
  (match v
    [(Num n) (inVal (Num (f n)))]
    [_ (inExc \"not a number\")]))"))
   (P "代码中的" (Code "mixin") ", " (Code "super")
      "等等将在" (Ref "sec-compose") "解释; 现在只需知道, "
      (Code "(interpret e env)") "是对" (Em "完整") "语言的解释器的递归调用, "
      "而" (Code "(super interpret exp env)")
      "把本片段不认识的句法交给之前的语言.")
   (H3. "传值" $lambda "演算")
   (P "这个片段添加变量, 抽象和应用, 以及一种新的值, 过程:"
      (Grammar
       ($e $::= (&\| $x (Lam* $x $e) (S $e_1 $e_2)) $)
       ($V $::= (&\| $..h (Mk $Proc $f)) (&: $f (&-> $V $C))))
      "解释器的新子句是"
      (eqn*
       ((Den $x) $= (Mk $Val (app $rho $x)))
       ((Den (Lam* $x $e)) $= (Mk $Val (Mk $Proc (Fn $v (Den $e (Ext $rho $x $v))))))
       ((Den (S $e_1 $e_2)) $= (Bind $f (Den $e_1)))
       ($ $ (Bind $a (Den $e_2)))
       ($ $ (Case $f))
       ($ $ (Arm (Mk $Proc $g) (app $g $a)))
       ($ $ (Arm $wild $err))))
   (P "过程的指称是一个从值到" (Em "计算") "的函数, "
      "因为过程体可能发出请求. 应用" (S $e_1 $e_2)
      "从左到右计算算子和实参, 这正是" $lArr "在这里规定的求值顺序. "
      "这个片段同样不需要新的动作或资源.")
   (P "有了这两个片段我们已经得到了一个函数式语言. 注意它的解释器是一个"
      (Em "直接") "语义: 环境是唯一的额外参数, 没有存储, 也没有延续. "
      "如果从来没有请求发生 (除了错误), " $lArr
      "就退化为普通的" (Code "let") ", 而整个定义就是教科书上的直接语义.")
   ((Fig #:id "fig-cbv-code")
    (B "[传值" $lambda "演算片段 (cbv.rkt)]")
    (CodeB ";; syntax
(struct Var (x))
(struct Lam (x body))
(struct App (rator rand))
;; new value
(struct Proc (f))

(define cbv-mixin
  (mixin (language&lt;%&gt;) ()
    (super-new)
    (define/override (interpret exp env)
      (match exp
        [(Var x) (inVal (lookup env x))]
        [(Lam x body)
         (inVal (Proc (lambda (v) (interpret body (extend env x v)))))]
        [(App e1 e2)
         (handle* ([f (interpret e1 env)]
                   [a (interpret e2 env)])
           (match f
             [(Proc g) (g a)]
             [_ (inExc \"not a procedure\")]))]
        [_ (super interpret exp env)]))))"))
   (H3. "布尔值")
   (P "原文的代码因篇幅原因省略了这个片段, 但没有它就写不出递归程序. "
      "它与算术片段如出一辙:"
      (Grammar
       ($e $::= (&\| $#t $#f (S $if $e_1 $e_2 $e_3) (S $zero? $e)) $)
       ($V $::= (&\| $..h (Mk $Truth $b)) (&in $b (setE $#t $#f))))
      (eqn*
       ((Den (S $if $e_1 $e_2 $e_3)) $= (Bind $t (Den $e_1)))
       ($ $ (Case $t))
       ($ $ (Arm (Mk $Truth $#t) (Den $e_2)))
       ($ $ (Arm (Mk $Truth $#f) (Den $e_3)))
       ($ $ (Arm $wild $err)))
      (S $zero? $e) "的子句留给读者. 至此我们得到了原文的Pure Scheme. "
      "用Z组合子可以写出递归函数. 下面在Racket中用" (Code "run") "运行一个加法的例子:")
   (CodeB "(define Z
  '(lambda (f)
     ((lambda (x) (f (lambda (v) ((x x) v))))
      (lambda (x) (f (lambda (v) ((x x) v)))))))

(run `(((,Z (lambda (add)
              (lambda (m)
                (lambda (n)
                  (if (zero? m) n (add1 ((add (sub1 m)) n)))))))
        20)
       22))
;; =&gt; (answer (Num 42) ...)")
   (H3. "引用单元")
   (P "现在来到第一个真正的效应. 存储是全局资源, 因此"
      (Em "句法成分不能自己读写存储") ", 只能请求管理者代劳. "
      "于是这个片段除了新的句法和新的值, 还添加了三个动作:"
      (Grammar
       ($e $::= (&\| (S $ref $e) (S $deref $e) (S $setref! $e_1 $e_2)) $)
       ($V $::= (&\| $..h (Mk $Loc $ell)) (&in $ell $NN))
       ($A $::= (&\| $..h (Mk $Ref $v) (Mk $Der $ell) (Mk $Set $ell $v)) $))
      "以及一种新的资源: 存储" (&: $sigma (&-o-> $NN $V))
      ", 即有限的从位置到值的映射. 解释器的新子句只是把请求发出去:"
      (eqn*
       ((Den (S $ref $e)) $= (Bind $v (Den $e)))
       ($ $ (Ask (Mk $Ref $v)))
       ((Den (S $deref $e)) $= (Bind $l (Den $e)))
       ($ $ (Case $l))
       ($ $ (Arm (Mk $Loc $ell) (Ask (Mk $Der $ell))))
       ($ $ (Arm $wild $err))
       ((Den (S $setref! $e_1 $e_2)) $= (Bind $l (Den $e_1)))
       ($ $ (Bind $v (Den $e_2)))
       ($ $ (Case $l))
       ($ $ (Arm (Mk $Loc $ell) (Ask (Mk $Set $ell $v))))
       ($ $ (Arm $wild $err)))
      "真正的工作由管理者完成:"
      (eqn*
       ((Admin (Mk $FX $k (Mk $Ref $v)) $sigma) $=
        (: (Admin (app $k (Mk $Loc $ell)) (Ext $sigma $ell $v)) $cm
           (Mspace #:attr* '((width "1em")))
           (Mtext "其中") (&= $ell (ver0 $sigma))))
       ((Admin (Mk $FX $k (Mk $Der $ell)) $sigma) $=
        (Admin (app $k (app $sigma $ell)) $sigma))
       ((Admin (Mk $FX $k (Mk $Set $ell $v)) $sigma) $=
        (Admin (app $k (Mk $Loc $ell)) (Ext $sigma $ell $v))))
      "为简洁起见, 这里只写出了资源中的存储分量" $sigma
      "; 实现中资源是一张表, 存储是其中名为" (Code "'store") "的一项, "
      "其他片段可以往表中添加别的资源而互不干扰. "
      (ver0 $sigma) "是存储中已有位置的个数; 位置从不回收, "
      "因此它总是一个新位置.")
   (P "对比引言中的第二行: 那里" (Em "每个") "子句都要接收和返回存储, "
      "而这里只有管理者的三个子句提到了存储. 算术片段和"
      $lambda "演算片段的子句一个字也没有改.")
   (P "让我们跟踪一个小例子 " (S $deref (S $ref $7:t)) ". 解释器给出"
      (eqn*
       ((Den (S $ref $7:t)) $= (Ask (Mk $Ref (Mk $Num $7))))
       ((Den (S $deref (S $ref $7:t))) $= (Mk $FX $k (Mk $Ref (Mk $Num $7))))
       ($ $ (: (Mtext "其中") $k $=
               (Fn $l (: (Case $l) (Mk $Loc $ell) $=> (Ask (Mk $Der $ell)) $\| $..h)))))
      "也就是说, 整个程序的含义是一个分配请求, 其句柄是" (Q "拿到位置之后去读它")
      ". 管理者接着处理它:"
      (MB (deriv (Admin (Mk $FX $k (Mk $Ref (Mk $Num $7))) $emptyset)
                 (Admin (app $k (Mk $Loc $0)) (en0 (: $0 $\|-> (Mk $Num $7))))
                 (Admin (Mk $FX $Val (Mk $Der $0)) (en0 (: $0 $\|-> (Mk $Num $7))))
                 (Admin (Mk $Val (Mk $Num $7)) (en0 (: $0 $\|-> (Mk $Num $7))))
                 (tu0 (Mk $Num $7) (en0 (: $0 $\|-> (Mk $Num $7))))))
      "程序与管理者交换了两次消息: 一次分配, 一次读取.")
   ((Fig #:id "fig-store-code")
    (B "[引用单元片段的管理者 (store.rkt)]")
    (CodeB "(define/override (admin comp resources)
  (define store (hash-ref resources 'store))
  (define (with-store s) (hash-set resources 'store s))
  (match comp
    [(FX k (Ref v))
     (define l (hash-count store))             ; a fresh location
     (admin (k (Loc l)) (with-store (hash-set store l v)))]
    [(FX k (Der l))
     (admin (k (hash-ref store l)) resources)]
    [(FX k (Set l v))
     (admin (k (Loc l)) (with-store (hash-set store l v)))]
    [_ (super admin comp resources)]))

(define/override (initial-resources)
  (hash-set (super initial-resources) 'store (hasheqv)))"))
   (P "注意管理者的每个子句在执行完动作之后都调用" (Code "admin")
      "本身, 即" (Em "完整") "语言的管理者, 因为句柄" $k
      "继续计算下去时可能发出任何片段的请求.")
   (H3. "第一级延续")
   (P "传统上, 加入" (Code "call/cc") "这样的控制算子意味着把整个语义改写成延续传递风格. "
      "在我们的框架中却几乎不需要做什么, 因为延续早就在那里了: "
      "一个请求抵达管理者时, 它的句柄" $k "恰好就是" (Q "程序的其余部分")
      ", 也就是当前延续. 我们只需要一个动作, 让句法成分能够拿到它.")
   (P "我们采用原文的句法: " (S $catch $x $e) "把当前延续绑定到" $x
      "然后计算" $e "; " (S $throw $x $e) "把" $e
      "的值交给" $x "所绑定的延续, 并且丢弃当前延续."
      (Grammar
       ($e $::= (&\| (S $catch $x $e) (S $throw $x $e)) $)
       ($V $::= (&\| $..h (Mk $K $kappa)) (&: $kappa (&-> $V $C)))
       ($A $::= (&\| $..h (Mk $Con $f)) (&: $f (&-> (par0 (&-> $V $C)) $C))))
      "解释器和管理者的新子句是"
      (eqn*
       ((Den (S $catch $x $e)) $=
        (Ask (Mk $Con (Fn $kappa (: (Bind $v (Den $e (Ext $rho $x (Mk $K $kappa))))
                                    (app $kappa $v))))))
       ((Den (S $throw $x $e)) $= (Bind $v (Den $e)))
       ($ $ (Case (app $rho $x)))
       ($ $ (Arm (Mk $K $kappa) (Ask (Mk $Con (Fn $wild (app $kappa $v))))))
       ($ $ (Arm $wild $err))
       ((Admin (Mk $FX $k (Mk $Con $f)) $r) $= (Admin (app $f $k) $r)))
      "控制消息" (Mk $Con $f) "携带一个" (Em "延续的接收者") $f
      ", 管理者做的唯一一件事就是把当前延续交给它. "
      (S $catch $x $e) "的接收者把延续以" (Mk $K $kappa) "的形式放进环境, "
      "计算" $e ", 然后正常返回到" $kappa "; "
      (S $throw $x $e) "的接收者则" (Em "忽略") "收到的当前延续, "
      "转而把值交给先前捕获的" $kappa ".")
   (P "跟踪程序 " (S $add1 (S $catch $k (S $sub1 (S $throw $k $10:t))))
      ". 为方便起见, 记"
      (eqn*
       ($f $= (Fn $kappa (: (Bind $v (Den (S $sub1 (S $throw $k $10:t)) (Ext $rho $k (Mk $K $kappa))))
                            (app $kappa $v))))
       ((app $inc $v) $= (: (Case $v) (Mk $Num $n) $=> (Mk $Val (Mk $Num (&+ $n $1))) $\| $..h)))
      "解释器给出"
      (MB (&= (Den (S $add1 (S $catch $k $..h)))
              (: (Bind $x (Ask (Mk $Con $f))) (app $inc $x))
              (Mk $FX $inc (Mk $Con $f))) ".")
      "管理者把句柄" $inc "交给" $f ". 在" $f "的体中, " (S $throw $k $10:t)
      "发出请求" (Mk $Con (Fn $wild (app $inc (Mk $Num $10))))
      "; 它经过" $sub1 "和" $f "的" $lArr "向外传播时, 句柄被扩大了两次, "
      "但这无关紧要, 因为接收者根本不看它:"
      (MB (deriv (Admin (Mk $FX $inc (Mk $Con $f)) $r)
                 (Admin (app $f $inc) $r)
                 (Admin (Mk $FX $k^ (Mk $Con (Fn $wild (app $inc (Mk $Num $10))))) $r)
                 (Admin (app $inc (Mk $Num $10)) $r)
                 (tu0 (Mk $Num $11) $r)))
      "其中" $k^ "是被扩大了的句柄, 它里面的" $sub1 "从未执行.")
   ((Fig #:id "fig-control-code")
    (B "[延续片段 (control.rkt)]")
    (CodeB "(struct K (k))               ; new value: continuation object
(struct Con (receiver))      ; new action: control message

(define/override (interpret exp env)
  (match exp
    [(Catch x body)
     (inAC (Con (lambda (k) (handle (interpret body (extend env x (K k))) k))))]
    [(Throw x e)
     (handle* ([v (interpret e env)])
       (match (lookup env x)
         [(K k) (inAC (Con (lambda (_current-k) (k v))))]
         [_ (inExc \"not a continuation\")]))]
    [_ (super interpret exp env)]))

(define/override (admin comp resources)
  (match comp
    [(FX k (Con receiver)) (admin (receiver k) resources)]
    [_ (super admin comp resources)]))"))
   (P "这些延续是真正第一级的: 它们可以存进引用单元, 在" $catch
      "返回之后再被调用, 从而" (Q "回到过去") ". 例如下面的程序中"
      $catch "返回了两次, 第二次返回时" (Tt "n") "为" $0 ":")
   (CodeB "(run '((lambda (r)
         ((lambda (n)
            (if (zero? n)
                100
                ((lambda (k) (throw k 0)) (deref r))))
          (catch k ((lambda (_) 1) (setref! r k)))))
       (ref 0)))
;; =&gt; (answer (Num 100) ...)")
   (P "注意存储和延续这两个片段从未提到对方, 但它们的组合自然而然地给出了正确的行为.")
   (H2. "组合解释器" #:id "sec-compose")
   (P "现在的问题是: 如何在程序中把这些片段" (Q "加") "起来? "
      "原文使用Rice Scheme的第一级模块: 一个语言是导出解释器, "
      "管理者和资源列表的模块, 一个片段是从模块到模块的函数, 称为"
      (Em "语言变换器 (language transformer)") ". "
      "今天的Racket里, 与此最贴近而又最容易读的工具是"
      (Em "mixin") ", 即从类到类的函数.")
   (P "一个语言是一个类, 它有方法" (Code "interpret") ", "
      (Code "admin") ", " (Code "initial-resources")
      " (以及用于读入程序的" (Code "parse") "). 一个片段的写法总是:")
   (CodeB "(define foo-mixin
  (mixin (language&lt;%&gt;) ()
    (super-new)
    (define/override (interpret exp env)
      (match exp
        [new-syntax ... (interpret sub-exp env) ...]
        [_ (super interpret exp env)]))
    (define/override (admin comp resources)
      (match comp
        [(FX k new-action) ... (admin (k reply) new-resources) ...]
        [_ (super admin comp resources)]))))")
   (P "这里有一个微妙之处, 也是原文花了不少笔墨的地方. "
      "一个片段在解释子表达式时, 必须调用" (Em "完整")
      "语言的解释器, 因为子表达式中可能出现尚未添加的片段的句法; "
      "但它把不认识的句法交出去时, 应该交给" (Em "之前")
      "的语言. 原文为此引入了一对名字" (Code "InterpTop") "和"
      (Code "InterpPrev") ", 而" (Code "InterpTop")
      "只能在所有片段组合完毕之后才定义, 作为一个全局变量. "
      "用类来实现时, 这对区别是自带的:"
      (Ul (Li (Code "(interpret ...)") "是" (Code "(send this interpret ...)")
              ", 由动态分派找到完整语言的方法, 即" (Code "InterpTop") ";")
          (Li (Code "(super interpret ...)") "调用之前的语言, 即"
              (Code "InterpPrev") "."))
      "这正是面向对象中所谓的开放递归 (open recursion). "
      "于是那两个全局变量就不需要了, 程序中也可以同时存在多个不同的语言:")
   (CodeB "(define (make-language . mixins)
  (new ((apply compose1 values mixins) base%)))

(define pure-scheme
  (make-language cbv-mixin bool-mixin arith-mixin))
(define core-scheme
  (make-language store-mixin control-mixin cbv-mixin bool-mixin arith-mixin))")
   (P "原文的组合是 "
      (Code "(storeM (controlM (CBVM (arithmM baseM))))")
      ", 与此逐字对应. 原文还断言" (Em "组合的顺序无关紧要")
      ", 这是因为每个片段只处理自己的句法和动作, 其余的一律交给"
      (Code "super") ". 我们的测试对五个片段的全部" (&= (&fact $5) $120)
      "种排列验证了这一点.")
   (P "顺带一提, 为了方便测试, 实现中每个片段还提供一个" (Code "parse")
      "方法, 把S-表达式读成自己的抽象语法, 不认识的同样交给"
      (Code "super") ". 这里有一个小陷阱: 变量和应用没有专属的关键字, 若"
      $lambda "演算片段排在算术片段之前, 它就会把" (S $add1 $5:t)
      "读成一个应用. 解决办法是让每个片段声明自己的关键字, 而"
      $lambda "演算片段在读入变量和应用时避开" (Em "完整")
      "语言的全部关键字. 这又是一次开放递归.")
   (H2. "稳定指称")
   (P "现在可以精确地说出引言中的目标在什么意义上达到了. "
      "设" (&db0 $d*) "是某个语言的解释器, " (&prime (&db0 $d*))
      "是它的" (Em "任意") "扩展的解释器. 扩展只是给" $V "和" $A
      "添加了新的形状, 因此旧的值和计算原封不动地也是新的值和计算. "
      "反过来, 把新形状一律映为" $bottom "就得到一个投影" $pi
      ". 原文证明了:"
      (MB (&= (Den $e) (app $pi (Ap (&prime (&db0 $e)) $rho))) ",")
      "其中" $e "是旧语言中的表达式, " $rho "是只含旧值的环境. "
      "在我们的实现中这一点几乎是显然的: 旧语言的句法由" (Em "同一段代码")
      "解释, 而在只含旧值的环境中这段代码不会遇到新的值, 也就不会发出新的请求. "
      "测试也检验了这一点: 纯函数式的程序在Pure Scheme和Core Scheme中给出相同的值.")
   (P "如果环境中含有新的值, 例如一个会分配单元的过程, "
      "那么旧的表达式调用它时当然会发出新的请求. 此时等式变为不等式"
      (MB (&sqsube (app $iota (Den $e (par0 (&compose $pi $rho))))
                   (Ap (&prime (&db0 $e)) $rho)) ",")
      "其中" $iota "是把旧计算看作新计算的嵌入. "
      "它的意思是: 旧语义给出的每个确定的结果, 新语义都给出同样的结果; "
      "新语义只是在旧语义看来发散的地方可能做了更多的事.")
   (H2. "与今天的方法相比")
   (P (B "单子与单子变换器.")
      " 原文写作时, Moggi的单子已经提出, 而用单子变换器组合解释器 "
      "(Liang, Hudak和Jones, POPL 1995) 稍后才出现. "
      "单子变换器的一个众所周知的问题是组合不对称: "
      "异常变换器与状态变换器以不同的顺序叠放, 得到的是不同的语言, "
      "其中一种在抛出异常时撤销副作用, 另一种不撤销. "
      "在扩展直接语义中不存在这个问题, 因为所有的效应都由同一个管理者解释, "
      "片段的组合顺序不影响结果. 代价是语义被固定了: "
      "我们的实现中, 错误之前对存储的修改总是保留在最终的资源中.")
   (P (B "代数效应与处理器.")
      " 今天的读者大概会觉得这套框架很眼熟. "
      (Mk $FX $k $a) "就是一个代数操作" $a "连同它的续体 (resumption) " $k
      "; 管理者就是一个效应处理器 (effect handler). "
      "Plotkin和Power的代数效应 (2000年代初) 与Plotkin和Pretnar的效应处理器 (2009) "
      "都晚于原文. 最直接继承这篇论文的是Kiselyov, Sabry和Swords的"
      (I "Extensible Effects: An Alternative to Monad Transformers")
      " (Haskell 2013), 它明确把本文的思想作为出发点, 并用类型系统记录一个计算可能发出哪些请求. "
      "与这些后来的工作相比, 本文的管理者是唯一的, 位于顶层, 不能嵌套; "
      "一个程序不能在局部为某种请求安装自己的处理器. "
      "延续片段说明了这一限制并不像看起来那么严重: 有了第一级延续, 局部的控制可以由全局的管理者实现.")
   (P (B "可扩展的句法.")
      " 片段同时扩展句法 (新的变体) 和解释器 (新的子句), "
      "这正是Wadler所说的表达式问题 (expression problem). "
      "Swierstra的" (I "Data types à la carte")
      " (JFP 2008) 在类型化的设定下解决了同样的问题. "
      "我们的实现是动态类型的, 因而只用了最朴素的办法: "
      (Code "match") "失败时交给" (Code "super") ".")
   (P (B "模块与mixin.")
      " 原文使用的Rice Scheme模块系统与后来Racket的单元 (unit) 一脉相承 "
      "(Flatt和Felleisen, PLDI 1998); 同年Flatt, Krishnamurthi和Felleisen又给出了"
      "mixin的形式化 (POPL 1998). 用单元重写本文的代码会更接近原文, "
      "但mixin更为人所熟知, 而开放递归恰好就是原文需要的东西.")
   (H2. "结语")
   (P "扩展直接语义的核心只有三个定义: 计算" $C "的两种形状, 顺序组合"
      $lArr ", 以及管理者" $admin ". 每种效应都被表示为一个请求, "
      "由唯一的管理者在顶层解释; 句法成分之间既不传递存储, 也不传递延续. "
      "其结果是, 一个句法成分的指称不随语言的扩展而改变, "
      "而一个语言的解释器可以由互不知晓的片段组合而成.")
   ))
