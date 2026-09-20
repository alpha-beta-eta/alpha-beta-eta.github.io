#lang racket
(provide dsh2.html)
(require SMathML)
(define App (&split 2))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define (Rn str)
  (Mi str #:attr* '((mathvariant "normal"))))
(define $let (Pid "let"))
(define $in (Pid "in"))
(define (Let b e)
  (App $let b $in e))
(define (Lam x t)
  (: $lambda x $. t))
(define (∀ x P)
  (: $forall x $. P))
(define (∃ x P)
  (: $exists x $. P))
(define $id (Mi "id"))
(define $End (Mi "End"))
(define (&End Γ)
  (app $End Γ))
(define $pr (Mi "pr"))
(define (&pr i) (_ $pr i))
(define $dom (Mi "dom"))
(define (&dom f)
  (app $dom f))
(define (ext f x v)
  (: f (bra0 (&\|-> x v))))
(define $\\ (Mo "&setminus;"))
(define $p-> (Mo "&rharu;"))
(define $simeq (Mo "&simeq;"))
(define $approx (Mo "&approx;"))
(define $diamond (Mo "&diamond;"))
(define $sim (Mo "&sim;"))
(define $prec (Mo "&prec;"))
(define $:<=> (Mo ":&hArr;"))
(define-infix*
  (&:<=> $:<=>)
  (&sim $sim)
  (&prec $prec)
  (&\\ $\\)
  (&p-> $p->)
  (&simeq $simeq)
  (&approx $approx)
  (&diamond $diamond))
(define-@lized-op*
  (@p-> &p->)
  (@diamond &diamond)
  (@\|-> &\|->))
;effects
(define $eta_Γ (_ $eta Γ))
(define $Maybe (Mi "Maybe"))
(define (&Maybe A) (App $Maybe A))
(define $PEff (Mi "PEff"))
(define (&PEff Γ) (App $PEff Γ))
(define $bind (Mi "bind"))
(define (&bind a k) (appl $bind a k))
;services
(define $St (Mi "S"))
(define $Op (Mi "Op"))
(define $Out (Mi "Out"))
(define $Tok (Mi "Tok"))
(define $run (Mi "run"))
(define (&run a t s) (App $run a t s))
(define $None (Rn "None"))
(define $exec (Mi "exec"))
;observation
(define $LF (Mi "LF"))
(define $LI (Mi "LI"))
(define $runTest (Mi "runTest"))
;programs
(define $Store (Mi "Store"))
(define $setk (Mi "set"))
(define (&setk k s) (appl $setk k s))
(define $lift (Mi "lift"))
(define (&lift k a t) (App (_ $lift k) a t))
(define $Prog (Mi "Prog"))
(define $Done (Mi "Done"))
(define $Fail (Mi "Fail"))
(define $Provide (Mi "Provide"))
(define $Spawn (Mi "Spawn"))
(define $Call (Mi "Call"))
(define (Den P t) (_ (&db0 P) t))
;calculus
(define $Component (Mi "Component"))
(define $body (Mi "body"))
(define $reacts (Mi "reacts"))
(define $State (Mi "State"))
(define $Fiber (Mi "Fiber"))
(define $Binding (Mi "Binding"))
(define $next (Mi "next"))
(define $root (Rn "root"))
(define $Inactive (Mi "Inactive"))
(define $Loading (Mi "Loading"))
(define $Active (Mi "Active"))
(define $Unloading (Mi "Unloading"))
(define $UOp (Mi "UOp"))
(define $UProv (Mi "UProv"))
(define $USpawn (Mi "USpawn"))
(define $pid (Mi "pid"))
(define $owner (Mi "owner"))
(define $acc (Mi "acc"))
(define $tok (Mi "tok"))
(define $visible (Mi "visible"))
(define (&visible k) (app $visible k))
(define $target (Mi "target"))
(define (&target n) (app $target n))
(define $resolves (Mi "resolves"))
(define (&resolves m p) (appl $resolves m p))
(define $installed (Mi "installed"))
(define (&installed n) (app $installed n))
(define $quiet (Mi "quiet"))
(define $settled (Mi "settled"))
(define (Con c . x*) (apply appl c x*))
(define (Inactive z) (app $Inactive z))
(define (Loading P a w) (appl $Loading P a w))
(define (Active a w) (appl $Active a w))
(define (Unloading a w z) (appl $Unloading a w z))
(define (Cons x l) (: x (Mo "::") l))
(define $nil (Mo "[]"))
(define θ_n (_ $theta $n))
(define τ_n (_ $tau $n))
(define c_n (_ $c $n))
(define π_m (_ $pi $m))
(define τ_m (_ $tau $m))
(define (Step γ ℓ γ^)
  (: γ (^^ $--> ℓ) γ^))
(define (Rule str) (Mi str))
(define $O-Insert (Rule "O-Insert"))
(define $O-Dispose (Rule "O-Dispose"))
(define $O-Reset (Rule "O-Reset"))
(define $O-Restart (Rule "O-Restart"))
(define $O-React (Rule "O-React"))
(define $L-Begin (Rule "L-Begin"))
(define $L-Step (Rule "L-Step"))
(define $L-Divert (Rule "L-Divert"))
(define $L-Leave (Rule "L-Leave"))
(define $L-Undo (Rule "L-Undo"))
(define $L-RetireChild (Rule "L-RetireChild"))
(define $L-Unloaded (Rule "L-Unloaded"))
(define $L-Remove (Rule "L-Remove"))
(define (Lab r . x*) (apply App r x*))
(define (RuleLi name pre* . eff*)
  (apply Li (B name) ": 若" (append pre* (list ", 则") eff*)))
(define (Row . x*)
  (apply Tr (map Td x*)))
(define (HRow . x*)
  (apply Tr (map Th x*)))
(define (Tab . row*)
  (apply Table
         #:attr* '((align "center")
                   (border "1")
                   (style "border-collapse: collapse;"))
         row*))
(define $ell (Mi "&ell;"))
(define (&exec τ s) (appl $exec τ s))
(define (&runTest w s) (appl $runTest w s))
(define $store (Mi "store"))
(define (&store γ) (app $store γ))
(define $ops (Mi "ops"))
(define (&ops k P) (app (_ $ops k) P))
(define $meet (Mi "meet"))
(define (&meet k a b) (appl (_ $meet k) a b))
(define $RI (Mi "RevertIndep"))
(define $After (Mi "After"))
(define $pend (Mi "pend"))
(define $shadow (^^ $sigma (Mo "&Hat;")))
(define (&settled γ) (app $settled γ))
(define (&quiet γ) (app $quiet γ))
(define (Steps γ γ^) (: γ (^ $--> $*) γ^))
(define $_ (Mi "_"))
(define $prog (Mi "prog"))
(define $step (Mi "step"))
(define $Label (Mi "Label"))
(define $WF (Mi "WF"))
(define $EUser (Rn "EUser"))
(define $EConflict (Rn "EConflict"))
(define $Register (Mi "Register"))
(define $Lookup (Mi "Lookup"))
;appendices
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
(define ((appendix-cite letter) %heading)
  `(a ((href ,(string-append "#" (%heading-id %heading))))
      ,(format "附录~a" letter)))
(define (Appendix letter #:id [id #f] . html*)
  `(,(build-%heading #:present appendix-present
                     #:cite (appendix-cite letter)
                     #:level 2 #:id id)
    () ,letter . ,html*))
(define dsh2.html
  (parameterize ([parameter:format-num format-num])
  (TnTmPrelude
   #:title "deepseek harness笔记2"
   #:css "styles.css"
   (H1. "deepseek harness笔记2")
   (P "本文重新组织论文" (I "A Programming Paradigm for Spatiotemporal Composability")
      "的第3章和第4章, 并以插件运行时Cordis及其在dsh中的使用为背景. "
      "这不是翻译: 定义和定理的形状按本文的需要重新安排, 论文的说法只在对照时提到. "
      "目标是验证插件的生命周期性质, 插件的业务逻辑不在范围之内.")
   (P "重构遵循以下几条原则.")
   (Ol (Li "论文和Cordis都不是权威. 每个设计点都选合理, 直接, 简单的方案; "
           "Cordis的做法若体现了局部的最佳实践就跟着它.")
       (Li "所有跨组件共享的可变状态都是某个服务的状态, 不设另外的环境部分.")
       (Li "程序是归纳类型, 看不到状态, 只能通过操作的结果决定下一步. "
           "所以组件只碰自己声明过的东西, 这是由构造保证的.")
       (Li "服务状态从一开始就配备等价关系" $simeq
           ", 控制部分 (生命周期, 名字, 绑定的身份) 用严格相等.")
       (Li "定理的最终形状是: 若每个服务满足它的规格, 每个插件满足它的义务, "
           "则整个系统满足生命周期性质 (" (Ref "thm-summary") ")."))
   (P "出发点是: 插件卸载时, 它对共享状态做过的每件事都必须撤掉, "
      "所以每个改动共享状态的操作都要给出它的逆; 逆可以依赖于操作作用前的状态, "
      "比如" (Q "把" $x "设为5") "的逆是" (Q "把" $x "改回原来的值")
      ". 另一方面, 插件通过服务相互依赖, 提供者撤走时依赖者必须先停下.")
   (P "第1章到第5章给出模型: 服务, 程序, 组件与fiber, 以及生命周期规则. "
      "第6章证明元理论, 每个定理需要的服务假设在它所在的节里引入. "
      "附录A给出单个组件的语义 (论文3.1节的对应物), 附录B说明服务的等价关系应该怎样取, "
      "附录C逐条对照论文, 附录D列出各定理在Rocq形式化中的对应. 正文的定理不依赖附录; "
      "论文中本文的论证用不到的部分不再复述, 只在对照表中列出.")
   (P "主要结果如下, 其中的术语 (fiber, Active, 撤销独立, 秩等) 在正文中定义.")
   (Ul (Li (B "保持性与顺序") ": 不变式被每条规则保持; fiber只针对Active的提供者开始加载, "
           "提供者比依赖者活得久 (" (Ref "thm-preservation") ", " (Ref "thm-ordering") ").")
       (Li (B "恢复精确性") ": 若服务的操作良好, 且可能相遇的操作撤销独立, "
           "则卸载一个fiber的结果, 等于同一次执行中略去它的全部操作所得的结果 ("
           (Ref "thm-recovery") ").")
       (Li (B "进展与终止") ": 若依赖与spawn关系良基, 没有规则可用的状态都是稳定的; "
           "再有秩和程序长度的界, 就没有无限的执行 ("
           (Ref "thm-progress") ", " (Ref "thm-termination") ").")
       (Li (B "合流") ": 再设可能相遇的操作可交换, 且每条到达Done的路径提供组件可能提供的全部键, "
           "则不同调度到达的静止终态在重命名意义下一致, 并与静态组装的结果一致 ("
           (Ref "thm-dynamic") ").")
       (Li (B "汇总") ": 以上性质按服务规格的两部分和组件义务的各条重新组织为一个定理: "
           "服务一侧只依赖一个操作上的关系, 插件一侧只需逐条检查义务 ("
           (Ref "thm-summary") ")."))

   (H2. "服务" #:id "sec-service")
   (P "所有跨组件共享的可变状态都放在服务里. 服务是一个记录, 包含状态类型, 等价关系和操作.")
   ((Definition)
    "一个服务" Σ "由以下资料组成: 状态类型" $St ", 其上的等价关系" $simeq
    ", 操作类型" $Op " (参数已经放进操作里), 结果类型" $Out ", 以及"
    (MB (&: $run (&-> $Op $Tok $St (&Maybe (@c* $St (&End $St) $Out)))) ".")
    "其中" (&:= (&End $St) (&-> $St $St)) ", " (&Maybe $A) "是" $A
    "加上表示失败的" $None ", " (&= $Tok $NN) "是运行时交给这次操作的新鲜令牌, "
    "用于事件钩子这类生成出来的身份 (JS中由对象身份充当). ")
   (P "全系统只有一个服务" Σ ", 不是每个键一个; 需要多种服务时先取它们的和, "
      "这样避开了依赖类型的映射. 操作作用在状态上, 给出新状态, 这一步的逆, 以及结果. "
      "结果是程序获得信息的唯一途径, 也是观察者区分状态的依据, 见"
      (Ref "sec-observe") ".")
   ((Definition)
    "操作" $a "在令牌" $t "下良好, 当且仅当"
    (Ol (Li "带见证: 若" (&= (&run $a $t $s) (tu0 $s^ $g $o)) ", 则"
            (&simeq (app $g $s^) $s) ";")
        (Li "尊重" $simeq ": 若" (&simeq $s_1 $s_2) ", 则" (&run $a $t $s_1) "与"
            (&run $a $t $s_2) "都是" $None ", 或分别是" (tu0 (_prime $s $1) $g_1 $o)
            "和" (tu0 (_prime $s $2) $g_2 $o) ", 其中"
            (&simeq (_prime $s $1) (_prime $s $2)) "且"
            (∀ $x (&simeq (app $g_1 $x) (app $g_2 $x))) ";")
        (Li "给出的逆尊重" $simeq ": 若" (&= (&run $a $t $s) (tu0 $s^ $g $o))
            ", 则" (∀ (&cm $x $y) (&=> (&simeq $x $y) (&simeq (app $g $x) (app $g $y))))
            ".")))
   ((Example #:id "ex-registry")
    "按名字索引的注册表 (如" (Code "ctx.tools") "): 状态是名字到可选值的映射, 逐点比较. "
    (appl $Register $x $v) "在" $x "已被占用时失败, 逆是删去" $x "; "
    (app $Lookup $x) "读出" $x "处的值, 逆是恒等. 每个操作都良好. "
    "名字不同的操作互不干扰; 同名的两次登记中后一次失败, 所以它们谁先谁后是看得出来的. ")
   ((Example #:id "ex-bus")
    "有序事件总线 (如" (Code "ctx.on") "): 状态是钩子的列表, 每个钩子是令牌和回调的对. "
    "以令牌" $t "注册回调" $c "就是在表尾加入" (tu0 $t $c) ", 逆是删去令牌为" $t
    "的钩子; 分发按顺序返回回调的列表, 逆是恒等. 两次注册不可交换, 因为分发看得到顺序 ("
    (Ref "sec-observe") "说明不论取哪个等价关系都是如此). "
    "令牌用在逆上: Cordis的旧做法按回调注销, 即删去第一个回调为" $c "的钩子; 若" $c
    "已经注册过一次, 撤销第二次注册删掉的是第一次的钩子, 所以它不是精确的逆. "
    "去掉分发并忽略顺序, 就得到监听器注册: 状态是回调的列表, 按多重集比较, "
    "注册的逆是删去第一个" $c ". 在多重集的意义下删去哪一个" $c "都一样, 所以这里不需要令牌. ")

   (H2. "程序" #:id "sec-programs")
   (P "组件的行为是: 每一段执行一个操作, 再根据结果选择后续. "
      "我们把它直接做成一个归纳类型. 程序在存储上的语义见" (Ref "sec-effects")
      ", 正文只用到它的语法.")
   ((Definition)
    "设" $K "是键的集合. 对键集" (&sube (&cm $d $p) $K) ", 程序" (∈ $P (_ $Prog (&cm $d $p)))
    "由以下文法生成:"
    (MB (&::= $P (&\| $Done $Fail (Con $Call $k $a $kappa)
                      (Con $Provide $k $s $P) (Con $Spawn $c $P))) ",")
    "其中" (Con $Call $k $a $kappa) "要求" (∈ $k (&union $d $p)) ", "
    (Con $Provide $k $s $P) "要求" (∈ $k $p) ", 延续"
    (&: $kappa (&-> $Out (_ $Prog (&cm $d $p)))) "和子程序都在"
    (_ $Prog (&cm $d $p)) "中. " (Con $Call $k $a $kappa)
    "在键" $k "的服务状态上执行" $a ", 把结果交给" $kappa "; "
    (Con $Provide $k $s $P) "以初始状态" $s "提供键" $k "; "
    (Con $Spawn $c $P) "注册子组件" $c ". ")
   (P "这就是一个free monad, " $kappa "是延续. 键的限制写在构造子里, "
      "所以程序只能操作它声明的依赖和它可能提供的键, 这由构造保证, 不需要另外检查; "
      "即便" $kappa "无穷分叉, 也不必遍历它的路径. 程序是良基的 (每条路径都有限, "
      "但路径长度未必有界), 看不到状态, "
      "能得到的信息只有操作的结果. 名字和提供编号由规则层分配, 程序不接触.")

   (H2. "组件, fiber与状态" #:id "sec-state")
   (P "演算的粒度是: 每个服务操作是一个原子步, 步与步之间都可以放弃, 任何一步都可能失败. "
      "组件的一个实例称为fiber, 这个词沿用Cordis (取自React), 与fibration无关.")
   ((Definition)
    "一个组件是四元组"
    (MB (tupa0 (&sube $d $K) (&sube $p $K) (∈ $body (_ $Prog (&cm $d $p))) $reacts) ",")
    "分别是依赖, 可能提供的键, 启动程序, 以及事件处理器 (组件编号的列表). "
    "组件放在状态之外的程序表" (&: $prog (&-> $C $Component)) "里, 其中" $C
    "是组件编号的集合. 对" (∈ $c $C) ", 把" (app $prog $c) "的四个分量记作"
    (&cm (app $d $c) (app $p $c) (app $body $c) (app $reacts $c))
    "; 对fiber " $n ", 记" c_n "为它的组件 (见下一个定义), " (app $p $n) "指" (app $p c_n) ", 余者类推. "
    $body "的类型已经保证它只操作" (&union $d $p) "中的键, 只提供" $p "中的键. ")
   (P "与Cordis不同, 提供的键是静态声明的. 这使得不同fiber的" $p
      "两两不相交可以在插入时检查, 依赖关系是否无环也可以静态判定; 进展和合流都需要无环.")
   ((Definition)
    "状态是" (tupa0 $next $F $B) ", 其中" $next "是新鲜名字, 提供编号和令牌共用的计数器, "
    (&: $F (&p-> $NN $Fiber)) "是注册表, " (&: $B (&p-> $K $Binding)) "是当前的服务绑定:"
    (MB (eqn*
         ($Fiber $::= (tupa0 (∈ $c $C) (∈ $pi (&+ $NN (setE $root)))
                             (∈ $tau $BB) (∈ $theta $Theta)))
         ($Binding $::= (tupa0 $pid $owner (∈ $s $St)))
         ($theta $::= (&\| (Inactive $zeta) (Loading $P $acc $omega)
                           (Active $acc $omega) (Unloading $acc $omega $zeta)))
         ($u $::= (&\| (appl $UOp $k $pid $tok $g) (appl $UProv $k $pid) (appl $USpawn $m)))))
    "各部分的含义如下."
    (Ul (Li "fiber: " $c "是它的组件, " $pi "是父fiber (根fiber为" $root "), "
            $tau "是" (Q "已请求移除") "标记, " $theta "是生命周期状态.")
        (Li "绑定: " $pid "是提供编号 (下文记作" $iota "), " $owner "是提供者的名字, "
            $s "是服务状态.")
        (Li "错误: " $zeta "是可选的错误, " (&= $zeta $bottom) "表示没有错误, 非" $bottom
            "的错误记作" $xi ". 具体的错误值不影响本文的结论; 规则中出现的有程序执行"
            $Fail "时的" $EUser "和键冲突时的" $EConflict ".")
        (Li "累加器: " $acc "是撤销项" $u "的列表, 表头是最近的一项.")
        (Li "撤销项: " (appl $UOp $k $pid $tok $g) "记下在键" $k "上, 提供编号为" $pid
            "的绑定上以令牌" $tok "做过的操作, " $g "是它给出的逆; "
            (appl $UProv $k $pid) "记下这个fiber建立的绑定; "
            (appl $USpawn $m) "记下这个fiber插入的子fiber " $m ".")
        (Li "视图: " (&: $omega (&p-> $K $NN)) "的定义域恰为该fiber的组件的" $d
            ", 记录每个依赖解析到的提供编号.")))
   (P "几点说明.")
   (Ul (Li "四个生命周期状态代替Cordis的六个: FAILED就是"
           (Inactive $xi) ", DISPOSED就是条目不存在. 下文称"
           (&= θ_n (Inactive $xi)) "的fiber " $n " 是" (B "失败的") ".")
       (Li "累加器是" (Ref "sec-effects") "中复合给出的逆, "
           "只是拆成撤销项的列表, 以便逐项撤销, 逐项设守卫. "
           $UOp "项记下操作用的令牌, 这是恢复精确性的证明所需要的.")
       (Li "每次" $Provide "步建立绑定时取一个新的提供编号, 以它作为这次提供 (provision) 的身份. 提供者撤掉再重新提供时编号改变, "
           "依赖它的fiber会重新加载, 绝不会拿着过期的绑定继续运行.")
       (Li "绑定中的服务状态被提供者和所有依赖者共同操作, 是共享且可变的."))

   (H2. "派生概念" #:id "sec-derived")
   ((Definition)
    "以下记" (&= (app $F $n) (tupa0 c_n (_ $pi $n) τ_n θ_n))
    ", 并在" θ_n "为Loading, Active或Unloading时把其中的视图记作" (_ $omega $n) "."
    (MB (eqn*
         ((&installed $n) $:<=> (&!= θ_n (Inactive $_)))
         ((&visible $k) $:=
          (Choice0 ($iota ", 若" (&= (app $B $k) (tupa0 $iota $o $_)) "且"
                       (&= (_ $theta $o) (Active $_ $_)))
                   ($bottom ", 否则")))
         ((&target $n) $:=
          (Choice0 ($bottom ", 若" τ_n "或" (∃ (∈ $k (app $d c_n)) (&= (&visible $k) $bottom)))
                   ((&\|-> (∈ $k (app $d c_n)) (&visible $k)) ", 否则")))
         ((&resolves $m $iota) $:<=>
          (&conj (&installed $m) (∃ $k (&= (app (_ $omega $m) $k) $iota))))
         ((&quiet $gamma) $:<=>
          (∀ $n (∀ (∈ $ell (app $L $n)) (&= (appl $step $gamma $ell) $None))))))
    "其中" (app $L $n) "是" $n "上的八种生命周期标签, " $step
    "是规则给出的后继函数 (见" (Ref "sec-rules") "). 称" $gamma (B "静止") ", 记作" (&quiet $gamma) ", 指没有生命周期规则可用. ")
   (P (&target $n) "是整个演算的驱动力: 生命周期规则不断让每个fiber的实际状态追赶它的"
      $target ", 规则以" $target "的变化为前提, 不需要单独的通知机制. "
      $visible "只计入Active的提供者, 所以提供者一进入Unloading, "
      "它的服务就对新的解析不可见; 已经解析到它的依赖者仍持有视图, "
      "可以继续读写, 直到自己卸载完毕.")

   (H2. "规则" #:id "sec-rules")
   (P "规则写作" (Step $gamma $ell $gamma^) ", 标签" $ell "决定唯一的后继 (若存在), "
      "所以演算就是一个确定性的函数"
      (&: $step (&-> $State $Label (&Maybe $State)))
      ", 其中" $State "是状态的集合, " $Label "是全部标签 (编排标签与生命周期标签) 的集合. 选择哪个标签是调度的事, 不在演算之内. "
      "凡是从" $next "取名字, 编号或令牌的规则, 都随后把" $next "加一. "
      "规则中" (&<- $x $v) "表示把状态的这一部分改为" $v ", 其余部分不变; "
      (&:= $x $e) "则只是给" $e "起名字" $x ", 比如" (&:= $m $next) "表示新fiber的名字取" $next
      "的当前值. 全文中" $:= "都表示定义, " $<- "只用于规则和影子存储 ("
      (Ref "def-shadow") ") 对状态的更新.")
   (P "规则分两类. 名字以O开头的五条是" (B "编排规则") ", 代表外部输入 (插入, 移除, 重置, 重启, 事件反应), "
      "一串编排标签称为" (B "编排输入") "; 名字以L开头的八条加载与卸载规则合称" (B "生命周期规则")
      ", 它们的标签就是" (app $L $n) "中的标签, 按生命周期规则走的一步称为" (B "生命周期步") ". "
      "生命周期规则由演算自己驱动, 目的是让每个fiber追上它的" $target ".")
   (H3. "编排规则")
   (Ul (RuleLi (Lab $O-Insert $c)
               (list (app $p $c) "与所有现存fiber的" $p "不相交 (下文简称" (app $p $c) (B "不冲突") ")")
               "以" (&:= $m $next) "为名插入" (tupa0 $c $root $false (Inactive $bottom))
               ", " $next "加一.")
       (RuleLi (Lab $O-Dispose $n) (list (∈ $n (&dom $F)))
               (&<- τ_n $true) ".")
       (RuleLi (Lab $O-Reset $n) (list (&= θ_n (Inactive $xi)))
               (&<- θ_n (Inactive $bottom)) ".")
       (RuleLi (Lab $O-Restart $n)
               (list θ_n "为" (Loading $_ $acc $omega) "或" (Active $acc $omega)
                  ", 且" (&neg τ_n))
               (&<- θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $O-React $n $i)
               (list (&= θ_n (Active $acc $omega)) ", " (&neg τ_n) ", "
                  (app $reacts c_n) "的第" $i "项为" $c ", 且" (app $p $c) "不冲突")
               "插入子fiber" (tupa0 $c $n $false (Inactive $bottom)) ", 名为"
               (&:= $m $next) ", 并令" (&<- θ_n (Active (Cons (appl $USpawn $m) $acc) $omega)) "."))
   (P $O-Reset "对应Cordis在失败后用" (Code "update") "重试; "
      $O-Restart "对应" (Code "restart") ", 卸载后由" $L-Begin "重新加载; "
      $O-React "把一次事件反应建模为子fiber, 反应的操作就是子fiber的操作, "
      "构成一个运行期效应, 单独撤销就是对子fiber执行" $O-Dispose ".")
   (H3. "加载")
   (Ul (RuleLi (Lab $L-Begin $n)
               (list (&= θ_n (Inactive $bottom)) "且" (&= (&target $n) (&!= $omega $bottom)))
               (&<- θ_n (Loading (app $body c_n) $nil $omega)) ".")
       (RuleLi (Lab $L-Divert $n)
               (list (&= θ_n (Loading $P $acc $omega)) "且" (&!= (&target $n) $omega))
               (&<- θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $L-Step $n)
               (list (&= θ_n (Loading $P $acc $omega)) "且" (&= (&target $n) $omega))
               "按" $P "分情况:"
               (Ul (Li $Done ": " (&<- θ_n (Active $acc $omega)) ";")
                   (Li $Fail ": " (&<- θ_n (Unloading $acc $omega $EUser)) ";")
                   (Li (Con $Call $k $a $kappa) ": 若" (∈ $k (app $d c_n)) ", 查视图" $omega
                       "; 否则查当前绑定" $B " (owner必须是" $n "). 得到编号" $iota
                       "; 取令牌" (&:= $t $next) ", 在该绑定的状态" $s "上运行"
                       (&run $a $t $s) ". 失败则进入Unloading并带错误; 成功得到"
                       (tu0 $s^ $g $o) "时写回" $s^ ", 并令"
                       (&<- θ_n (Loading (app $kappa $o) (Cons (appl $UOp $k $iota $t $g) $acc) $omega))
                       ". 解析失败视为失败;")
                   (Li (Con $Provide $k $s $P^) ": 要求" (∈ $k (app $p c_n))
                       "且" (app $B $k) "未定义; 以" (&:= $iota $next) "为编号令"
                       (&<- (app $B $k) (tupa0 $iota $n $s)) ", "
                       (&<- θ_n (Loading $P^ (Cons (appl $UProv $k $iota) $acc) $omega))
                       ". 要求不满足时进入Unloading并带错误" $EConflict ";")
                   (Li (Con $Spawn $c $P^) ": 要求" (app $p $c)
                       "不冲突; 插入子fiber" (tupa0 $c $n $false (Inactive $bottom))
                       ", 名为" (&:= $m $next) ", 并令"
                       (&<- θ_n (Loading $P^ (Cons (appl $USpawn $m) $acc) $omega))
                       ". 冲突时进入Unloading并带错误" $EConflict "."))))
   (P "所以" $L-Step "在" (&= (&target $n) $omega) "时总是适用的, "
      "Loading的fiber不会卡住. 键" $k "同时在" $d "和" $p "中时以依赖为准, 查视图. "
      "这样的组件提供" $k "时, " $k "已经由它依赖的提供者绑定, 所以必然冲突; 并且它在自己之上 ("
      (&prec $c $c) ", " $prec "见" (Ref "def-prec") "), 违反" (Ref "def-obligation") "中的有秩. 所以满足义务的组件表中"
      $d "与" $p "不相交.")
   (H3. "卸载")
   (Ul (RuleLi (Lab $L-Leave $n)
               (list (&= θ_n (Active $acc $omega)) "且" (&!= (&target $n) $omega))
               (&<- θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $L-Undo $n)
               (list (&= θ_n (Unloading (Cons $u $acc) $omega $zeta)) "且下面按" $u "分的前提成立")
               (&<- θ_n (Unloading $acc $omega $zeta)) ", 并且:"
               (Ul (Li (appl $UOp $k $iota $t $g) ": 若绑定" $iota "仍在, 把它的状态" $s "改为"
                       (app $g $s) ";")
                   (Li (appl $UProv $k $iota) ": " (B "守卫") ", 没有"
                       (&!= $m $n) "满足" (&resolves $m $iota) "; 删除" (app $B $k) ";")
                   (Li (appl $USpawn $m) ": 要求" (&!in $m (&dom $F))
                       ", 即子fiber已被移除.")))
       (RuleLi (Lab $L-RetireChild $n)
               (list (&= θ_n (Unloading (Cons (appl $USpawn $m) $_) $_ $_)) ", "
                  (∈ $m (&dom $F)) "且" (&neg τ_m))
               (&<- τ_m $true) ".")
       (RuleLi (Lab $L-Unloaded $n) (list (&= θ_n (Unloading $nil $omega $zeta)))
               (&<- θ_n (Inactive $zeta)) ".")
       (RuleLi (Lab $L-Remove $n)
               (list τ_n ", " (&= θ_n (Inactive $_)) ", 且没有" $m "满足" (&= π_m $n))
               "删除" (app $F $n) "."))
   (P "于是父fiber按LIFO撤到" (appl $USpawn $m) "时, 先请求子fiber退出, "
      "然后等它卸载并被移除, 再继续撤销更早的项; 在spawn之后登记的效应此时已经撤销完毕. "
      "提供者撤到" (appl $UProv $k $iota) "时, 要等所有解析到" $iota "的依赖者先卸载. "
      "这两处是演算中仅有的等待.")
   (H3. "设计决定")
   (P "主要的设计决定如下, 与论文的差别见" (Ref "sec-compare") ".")
   (Ul (Li (B "移除") ": 只有一个外部请求" $O-Dispose ", 条目在"
           (Q "已请求移除, 已Inactive, 没有子fiber") "时由" $L-Remove
           "自动删除. 外部不必等待卸载完成再发第二个请求, 条目也不会在子fiber还在时被删掉.")
       (Li (B "卸载逐项进行") ": 累加器逐项撤销, 这样"
           "守卫才能精确到每次提供, " $USpawn "才能等待子fiber.")
       (Li (B "LIFO串行") ": Cordis用" (Code "Promise.all") "并发执行disposer. "
           "后登记的效应往往依赖先登记的, LIFO不需要任何额外假设就是对的.")
       (Li (B "放弃加载") ": 每个服务操作是原子步, 步与步之间都可以" $L-Divert
           ". JS的await只出现在操作之间, 所以没有需要" (Q "落地") "的在途操作.")
       (Li (B "失败之后") ": 失败的fiber停在" (Inactive $xi) ", 由外部用" $O-Reset "决定是否重试.")
       (Li (B "运行期效应") ": 不把累加器改成树, 而是用" $O-React
           "把反应编码为子fiber. 子fiber本身就是一棵子效应树.")
       (Li (B "隔离与可用性检查") ": 不加规则. 键是服务名加作用域标签, "
           "隔离就是对组件表做键重命名, 见" (Ref "sec-meta-derived") "; "
           "在提供时求值的检查是程序自己就能做的判断."))

   (H2. "元理论" #:id "sec-meta")
   (P "这一章陈述演算的性质: 良构性被保持, 依赖顺序被遵守, 卸载精确地撤销效应, "
      "系统不会死锁且会终止, 以及终态与执行顺序无关. 最后一节把它们汇总为一个定理.")
   (P "本章频繁使用" (Ref "sec-state") "和" (Ref "sec-derived") "的记号, 先回顾一下.")
   (Ul (Li "状态是" (tupa0 $next $F $B) ". " $next "是名字, 提供编号和令牌共用的计数器. "
           $F "是注册表, " (&= (app $F $n) (tupa0 c_n (_ $pi $n) τ_n θ_n))
           "依次是fiber " $n " 的组件, 父fiber, " (Q "已请求移除") "标记和生命周期状态; "
           θ_n "为Loading, Active或Unloading时, 其中的视图记作" (_ $omega $n)
           ", 它把" $n "的每个依赖映到所解析的提供编号.")
       (Li $B "是当前的服务绑定. " (app $B $k) "有定义时形如" (tupa0 $iota $o $s)
           ", 表示键" $k "此刻由fiber " $o " 提供, 这次提供的编号是" $iota
           ", 提供者和依赖者共享的服务状态是" $s "; 无定义表示此刻没有fiber提供" $k
           ". 绑定由" $Provide "步建立, 在提供者撤销" (appl $UProv $k $iota) "时删除.")
       (Li "累加器中的撤销项有三种: " (appl $UOp $k $iota $t $g)
           " (在编号为" $iota "的绑定上以令牌" $t "做过的操作, 逆为" $g "), "
           (appl $UProv $k $iota) " (这个fiber建立的绑定), "
           (appl $USpawn $m) " (这个fiber插入的子fiber " $m ").")
       (Li (&= θ_n (Inactive $xi)) " (" (&!= $xi $bottom) ") 的fiber是失败的; "
           (&installed $n) "指" $n "不处于Inactive; " (&resolves $m $iota) "指已安装的"
           $m "的视图中有编号" $iota "; " (&target $n) "是" $n "此刻应有的视图, 为"
           $bottom "表示" $n "不该运行; " (app $L $n) "是" $n "上的八种生命周期标签."))
   (H3. "保持性")
   ((Theorem #:id "thm-preservation")
    "不变式" $WF "在初始状态成立, 并被每条规则保持. 它由以下几条组成:"
    (Ul (Li "现存名字, 编号, " $USpawn "中的子名字都小于" $next ";")
        (Li "子fiber的父fiber存在, 已安装, 且累加器中有对应的" $USpawn ";")
        (Li "现存fiber的" $p "两两不相交;")
        (Li "已解析的每个依赖都有绑定, 编号与视图一致, owner不是自己;")
        (Li "已安装fiber的视图的键序列恰为" $d ";")
        (Li "每个绑定的owner存在, 已安装, " (∈ $k (app $p $owner))
            ", 且owner的累加器中有对应的" $UProv ";")
        (Li "累加器中的每个" (appl $UProv $k $iota) "都对应一个现存绑定, 编号为"
            $iota ", owner为自己;")
        (Li "一个累加器中每个键至多有一个" $UProv ".")))
   ((proof)
    "对规则逐条检查. 有意思的情形如下."
    (Ul (Li $O-Insert ", " $O-React "和" $Spawn "步都检查新组件的" $p
            "与现存fiber不冲突, 所以不相交性保持.")
        (Li $Provide "步要求" (∈ $k (app $p c_n)) "且" (app $B $k)
            "未定义, 并同时在累加器里压入" $UProv ", 所以绑定与累加器的对应保持.")
        (Li "撤销" $UProv "时守卫保证没有其他已安装fiber解析到这个编号, 所以删除绑定不会破坏"
            (Q "已解析的依赖都有绑定") ".")
        (Li $L-Remove "要求没有子fiber, 所以父fiber存在的条件保持.")
        (Li $L-Begin "取的视图就是" $target ", 它的键序列恰为" $d
            ", 并且只解析到Active的提供者.")))
   (P "推论: 有子fiber或绑定的fiber一定已安装; 一个fiber卸载完毕时, "
      "它的子fiber和绑定都已经不存在了.")
   (H3. "顺序")
   ((Theorem #:id "thm-ordering")
    "设" (Step $gamma $ell $gamma^) ", 并且" $gamma "满足不变式."
    (Ol (Li "若" (&= $ell (Lab $L-Begin $n)) ", 则" (&= (&target $n) $omega)
            ", 对每个" (&= (app $omega $k) $iota) "有"
            (∃ $o (&conj (&= (app $B $k) (tupa0 $iota $o $_)) (&= (_ $theta $o) (Active $_ $_))))
            ", 并且在" $gamma^ "中" (&= θ_n (Loading (app $body c_n) $nil $omega))
            ". 即" $n "的全部依赖都由Active的提供者绑定, " $n "针对恰好这些提供开始加载.")
        (Li (B "提供者比依赖者活得久") ": 对每个状态, 若" (&installed $m) "且"
            (&= (app (_ $omega $m) $k) $iota) ", 则"
            (∃ (&!= $o $m) (&conj (&= (app $B $k) (tupa0 $iota $o $_)) (&installed $o))) ".")
        (Li "若" (&= θ_n (Unloading (Cons (appl $UProv $k $iota) $acc) $omega $zeta)) "且"
            (&= $ell (Lab $L-Undo $n)) ", 则" (∀ (&!= $m $n) (&neg (&resolves $m $iota)))
            "; 反过来, 只要还有这样的" $m ", 这一步就不适用.")
        (Li "若" (&= θ_n (Unloading (Cons (appl $USpawn $m) $acc) $omega $zeta)) "且"
            (&= $ell (Lab $L-Undo $n)) ", 则" (&!in $m (&dom $F)) ".")
        (Li "若" (&= $ell (Lab $L-Step $n)) ", 则" (&= θ_n (Loading $P $acc $omega))
            "且" (&= (&target $n) $omega) ". 即fiber只在视图仍等于" $target "时继续加载.")
        (Li (Lab $L-Divert $n) "或" (Lab $L-Leave $n) "可用, 当且仅当" $n
            "处于Loading或Active, 且" (&target $n) "不等于它的视图" $omega
            ". 即fiber只在" $target "偏离视图时放弃加载或离开; 一旦偏离, 由上一条, 它也不能再继续加载.")))
   ((proof)
    "第1条: " $L-Begin "的前提是" (&= (&target $n) $omega)
    ", 而" $visible "只在owner为Active时给出编号. 第2条由不变式中"
    (Q "已解析的依赖都有绑定, 编号一致, owner不是自己") "和"
    (Q "绑定的owner已安装") "两条得到. 第3条是撤销" $UProv
    "的守卫. 第4条是撤销" $USpawn "的前提. 第5条是" $L-Step "的前提, 第6条是" $L-Divert "和" $L-Leave "的前提.")
   (P "顺序定理不断言依赖者读到的值不变: 服务状态按设计是共享且可变的, "
      "固定的是提供的身份.")
   (H3. "恢复精确性" #:id "sec-recovery")
   ((Definition #:id "def-revert-indep")
    "称" (tu0 $a $t_a) "对" (tu0 $b $t_b) "撤销独立, 记作" (appl $RI $a $t_a $b $t_b)
    ", 如果对" $a "可能给出的每个逆" $g
    (Ol (Li "对每个" $x ", " (&run $b $t_b $x) "与" (&run $b $t_b (app $g $x))
            "都是" $None "; 或者"
            (&= (&run $b $t_b $x) (tu0 $x_1 $h_1 $o)) ", "
            (&= (&run $b $t_b (app $g $x)) (tu0 $x_2 $h_2 $o))
            ", 其中" (&simeq $x_2 (app $g $x_1)) "且"
            (∀ $y (&simeq (app $h_2 $y) (app $h_1 $y))) ";")
        (Li "对" $b "可能给出的每个逆" $h ", "
            (∀ $x (&simeq (app $g (app $h $x)) (app $h (app $g $x)))) "."))
    "第一条说" $b "不在乎" $a "的效应是否已被撤掉, 第二条说两者的逆可以交换. ")
   (P "撤销独立正是" (Q "撤掉" $a "的效应而让" $b "的效应留着")
      "所需要的条件; 它不要求" $a "和" $b "本身可交换 (先" $a "后" $b "与先" $b "后" $a "结果相同). 下面的引理在单个绑定上说明这一点.")
   ((Lemma #:id "thm-omission")
    "设" $a "带见证且" (&= (&run $a $t $s_0) (tu0 $s_1 $g $o))
    ". 设" $tau "是一串其他fiber的步骤 (它们的操作, 以及它们撤销时施加的逆), 每一步都与" $g
    "相容 (即满足" (Ref "def-revert-indep") "中相应的一条), 记" (&exec $tau $s) "为从" $s "出发执行"
    $tau "所到达的状态. 那么"
    (MB (&simeq (app $g (&exec $tau $s_1)) (&exec $tau $s_0)) ",")
    "并且" $tau "中其他fiber的操作在两次执行中得到相同的结果. 换言之, 在终点撤销" $a
    ", 等于" $a "从来没有执行过. ")
   ((proof)
    "记实际执行的状态为" $x ", 不执行" $a "的执行的状态为" $y
    ", 对步骤归纳证明" (&simeq (app $g $x) $y) "并且两边其他fiber的操作看到相同的结果."
    (Ul (Li "起点: " (&simeq (app $g $s_1) $s_0) "是" $a "的见证条件.")
        (Li "其他fiber的操作" $b ": 相容性说" $b "在" $x "和" (app $g $x)
            "上同时失败或同时成功, 后者的后继" $simeq "前者的后继再施" $g
            ", 结果相同; 再由" $b "尊重" $simeq ", 把" (app $g $x) "换成" $y "也一样.")
        (Li "其他fiber撤销时施加的逆" $h ": "
            (&simeq (app $g (app $h $x)) (app $h (app $g $x)) (app $h $y))
            ", 第一步是相容性, 第二步是" $h "尊重" $simeq ".")))
   ((Definition #:id "def-shadow")
    "对状态" $gamma ", 记" (&store $gamma) "为它的存储: "
    (&= (app (&store $gamma) $k) $s) "当键" $k "当前绑定的服务状态为" $s ", 即"
    (&= (app $B $k) (tupa0 $_ $_ $s))
    ", " (app $B $k) "无定义时" (app (&store $gamma) $k) "也无定义. "
    "称" $n "在" $gamma "中有" (B "待撤销操作") ", 如果" $n "在" $gamma
    "中存在, 处于Loading, Active或Unloading, 并且它的累加器中有" $UOp
    "项, 即它对服务状态做过, 但还没有撤销的操作; " $n "不存在, 或处于Inactive (尚未加载或已卸载完毕) 时, 它没有待撤销操作. "
    "设" (Steps $gamma_0 $gamma) "是一次执行, " $n "是一个名字. 沿这次执行对" $n
    "定义影子存储" $shadow ": 起点处" (&<- $shadow (&store $gamma_0))
    ", 此后每一步按它对绑定所做的事更新:"
    (Ul (Li "某个fiber " $m " 的" $L-Step "在键" $k "上以令牌" $t "成功运行了" $a ": 若"
            (&= $m $n) ", " $shadow "不变; 否则把" (app $shadow $k) "换成"
            (&run $a $t (app $shadow $k)) "的后继;")
        (Li "某个fiber " $m " 的" $L-Step "在键" $k "上运行" $a "失败: " $shadow
            "不变. 在" (Ref "thm-recovery") "的假设下, 操作在影子上失败当且仅当它在实际执行中失败 (见那里的证明), "
            "所以影子不需要单独处理" (Q "实际成功而影子失败") "的情形;")
        (Li "某个fiber " $m " 撤销" (appl $UOp $k $iota $t $g) "并作用于现存的绑定: 若"
            (&= $m $n) ", " $shadow "不变; 否则把" (app $shadow $k) "换成"
            (app $g (app $shadow $k)) ";")
        (Li $Provide "步以初始状态" $s "建立键" $k "的绑定: 令" (&<- (app $shadow $k) $s)
            "; 撤销" (appl $UProv $k $iota) "删除绑定: 令" (app $shadow $k) "无定义. "
            "这两条对所有fiber都照常进行, 包括" $n ";")
        (Li "其余的步不改动" $shadow "."))
    "所以影子跳过的只是" $n "自己的服务操作和它们的撤销, 不是" $n
    "的存在: " $n "的提供照样建立和删除, 其他fiber照样解析到它.")
   ((Definition)
    "记" (&ops $k $P) "为程序" $P "中对键" $k "的操作之集, 即在" $P "的某条路径上以"
    (Con $Call $k $a $kappa) "的形式出现的" $a "之集. "
    "称" (tu0 $a $b) "在键" $k "上可能相遇, 记作" (&meet $k $a $b) ", 如果存在组件" $c "和" $c^
    ", 使得" $a "作为对" $k "的操作出现在" (app $body $c) "中, " $b "出现在"
    (app $body $c^) "中, 并且当" (&= $c $c^) "时" $c
    "不提供任何键:"
    (MB (&:<=> (&meet $k $a $b)
               (∃ (&cm $c $c^)
                  (&conj (∈ $a (&ops $k (app $body $c)))
                         (∈ $b (&ops $k (app $body $c^)))
                         (@=> (&= $c $c^) (&= (app $p $c) $empty))))) ".")
    "假设(H)是: 可能相遇的操作对在不同令牌下撤销独立,"
    (MB (&=> (&conj (&meet $k $a $b) (&!= $t_a $t_b))
             (appl $RI $a $t_a $b $t_b)) ".")
    "定义中的蕴涵是说: 同一组件的两个操作, 只在该组件不提供任何键时才算可能相遇. "
    "提供键的组件同时至多有一个实例 (各fiber的" $p "两两不相交), "
    "它的两个操作来自同一个fiber, 按LIFO撤销, 不需要假设.")
   ((Theorem #:id "thm-recovery")
    "设所有服务操作良好, 且(H)成立. 设" $gamma_0 "满足不变式和下面证明中的辅助不变式 (两者在每个可达状态都成立), " $n
    "在其中没有待撤销操作, " (Steps $gamma_0 $gamma) "是一次执行, " $shadow
    "是沿这次执行对" $n "得到的影子存储 (" (Ref "def-shadow") "), 并且" $n "在" $gamma
    "中也没有待撤销操作 (比如卸载完毕). 那么对每个键" $k ", "
    (app (&store $gamma) $k) "与" (app $shadow $k) "同时无定义, 或者"
    (MB (&simeq (app (&store $gamma) $k) (app $shadow $k)) ".")
    "换言之, 卸载" $n "的结果, 就是同一次执行中略去" $n
    "全部操作所得的结果, 不论其他fiber同时做了什么. ")
   ((proof)
    "对键" $k ", 设它当前的绑定为" (&= (app $B $k) (tupa0 $iota $_ $s))
    " (编号" $iota ", 服务状态" $s "), 记" (_ $pend $k) "为" $n
    "的累加器中所有" (appl $UOp $k $iota $_ $g) "项的" $g "组成的列表, 最近的在前, 并记"
    (app (_ $pend $k) $s) "为把它们依次 (最近的先) 作用于" $s "的结果. 沿执行维护两个不变式."
    (Ol (Li (B "主不变式") ": 对每个键" $k ", " (app $B $k) "与" (app $shadow $k)
            "同时无定义, 或者" (&simeq (app (_ $pend $k) $s) (app $shadow $k)) ".")
        (Li (B "辅助不变式") ": 每个fiber " $m " 的每个待撤销项" (appl $UOp $k $iota $t $g)
            "都来自" $m "的组件的程序中对" $k "的某个操作" $a ", 即" $g "是" (tu0 $a $t)
            "给出的逆; 其中" (&< $t $next) ", 并且不同fiber的撤销项所用的令牌互不相同."))
    "辅助不变式由规则直接保持: 令牌取自" $next ", 每取一次" $next "加一. "
    "它的作用是在下面的情形中让(H)适用: " $n "的待撤销项中的" $g "来自" $n
    "的组件的程序中的某个" $a "; 若另一个fiber在" $k "上运行" $b ", 则" $b
    "出现在它的组件的程序中; 若另一个fiber撤销它的一项" $h ", 则由辅助不变式, " $h
    "是它的组件的程序中某个" $b "给出的逆. 两种情形都有" (&meet $k $a $b) ", 且两者令牌不同. "
    "起点处" $n "没有待撤销操作, 每个" (_ $pend $k) "都是空列表, 主不变式就是"
    (&= $shadow (&store $gamma_0)) ". 逐类检查步骤:"
    (Ul (Li $n "自己的操作: 把状态从" $s "改为" $s^ ", 并压入逆" $g
            ". 因为" (&simeq (app $g $s^) $s) ", 先施新压入的逆就回到原来的情形, 而影子不动.")
        (Li $n "撤销一项: 实际状态施加了这个逆, 待撤销的列表少了这一项, 两边的作用抵消.")
        (Li "其他fiber在同一绑定上以令牌" (_ $t $b) "执行操作" $b ": 实际状态" $s
            "变为" $b "在" $s "上的后继, 影子" (&simeq (app $shadow $k) (app (_ $pend $k) $s))
            "也执行" $b ". 需要比较" $b "在" $s "上和在" (app (_ $pend $k) $s)
            "上的运行. 对" (_ $pend $k) "中的每个" $g ", 由辅助不变式和(H), 撤销独立的第一条成立; "
            "对列表的长度归纳, 第一条对整个复合" (_ $pend $k) "也成立: 每用一次, " $b
            "两边给出的结果相同, 给出的逆逐点" $simeq ", 所以可以接着用下一个. 于是"
            $b "在" $s "上和在" (app (_ $pend $k) $s) "上同时失败或同时成功, 成功时若" $b "在" $s
            "上的后继为" $s^ ", 则它在" (app (_ $pend $k) $s) "上的后继" $simeq (app (_ $pend $k) $s^)
            "; 再由" $b "尊重" $simeq ", 把" (app (_ $pend $k) $s) "换成影子也一样. "
            "失败时实际的fiber进入Unloading, 绑定不变, 影子也不变 (" (Ref "def-shadow")
            "); 成功时主不变式在新状态上成立.")
        (Li "其他fiber的撤销项" $h ": 两边都施加" $h ". 对" (_ $pend $k) "中的每个" $g
            ", 由辅助不变式和(H), 撤销独立的第二条给出"
            (∀ $y (&simeq (app $g (app $h $y)) (app $h (app $g $y))))
            "; 同样对列表长度归纳, 得到"
            (&simeq (app (_ $pend $k) (app $h $s)) (app $h (app (_ $pend $k) $s)))
            ", 再由" $h "尊重" $simeq "得到主不变式.")
        (Li "绑定的创建与删除: 在两边同时发生; " $n
            "仍有待撤销操作的绑定不会被删除, 这由守卫保证."))
    "最后" $n "没有待撤销操作时, 每个" (_ $pend $k) "都是空列表, 主不变式说的就是实际存储" $simeq "影子存储.")
   (P "这里比较的是同一次执行, 不是" (Q $n "从未存在") "的另一次执行. 影子中" $n
      "的提供照样存在过, 因它而加载的依赖者和它spawn的子fiber照样运行过, "
      "并且由" (Ref "thm-omission") ", 其他fiber的操作在实际执行和影子中得到相同的结果, "
      "所以它们走的是同一条路径. 若" $n "从未存在, 这些依赖者根本不会加载, 调度也可能完全不同, "
      "本定理对那样的执行不作断言. 依赖者自己的操作并不包括在" $n
      "的操作之中; 在" $n "撤销提供之前它们已经卸载, 它们的操作由它们各自的卸载撤销, "
      "对每个依赖者分别应用本定理即可.")
   ((Corollary #:id "cor-runtime")
    "运行期效应的撤销也是精确的: 从效应出现之前到它的子fiber被移除之后, "
    "每个绑定的状态都" $simeq "略去该效应全部操作所得的状态. ")
   ((proof)
    "取" $n "为" $O-React "插入的子fiber, " $gamma_0 "为插入之前的状态, " $gamma
    "为它被移除之后的状态. " $n "在" $gamma_0 "和" $gamma "中都不存在, "
    "所以都没有待撤销操作, 由" (Ref "thm-recovery") "即得.")
   (P "注意这里只要求可能相遇的操作对撤销独立, 不要求操作本身可交换 (" (Ref "def-commute") "); "
      "同一fiber内部的操作按LIFO撤销, 不需要任何假设.")
   (H3. "进展与终止")
   ((Definition #:id "def-prec")
    "称" $c^ "在" $c "之上, 记作" (&prec $c $c^) ", 如果"
    (MB (&!= (&cap (app $p $c) (app $d $c^)) $empty) ",")
    "或者" $c "可能spawn " $c^ ", 即" $c^ "出现在" $c "的程序的某个spawn中, 或是"
    $c "的事件处理器. "
    "称" (&: $r (&-> $C $NN)) "是以" $M "为界的" (B "秩") ", 如果"
    (MB (&=> (&prec $c $c^) (&< (app $r $c) (app $r $c^) $M)) ".")
    "一个状态是" (B "稳定") "的, 记作" (&settled $gamma) ", 如果每个fiber " $n "满足下列之一:"
    (MB (Choice0 ((&conj (&= θ_n (Inactive $xi)) (&neg τ_n)) ", 失败并停下")
                 ((&conj (&= θ_n (Inactive $bottom)) (&neg τ_n) (&= (&target $n) $bottom))
                  ", 不该运行")
                 ((&conj (&= θ_n (Active $_ $omega)) (&= (&target $n) $omega))
                  ", 该运行且视图正确")))
    "特别地, 稳定状态中没有Loading或Unloading的fiber. ")
   (P "关系" $prec "说的是谁必须先于谁: 若" (&prec $c $c^) ", 则" $c
      "的fiber要先加载, " $c^ "的fiber才能加载 (提供者先于依赖者, 父fiber先于它spawn的子fiber), 卸载时次序相反. "
      "秩就是组件在这个层级中的高度: 提供者和父组件的秩比依赖者和子组件低, 并且所有组件的秩都小于" $M
      ". 对有限的组件表, 秩存在当且仅当" $prec "没有环: 有环时沿环走一圈秩要严格增加, 不可能; "
      "无环时取" (app $r $c) "为以" $c "结尾的最长" $prec "链的长度, " $M
      "取组件个数即可. 秩在后文有两个用处. 第一, 它严格上升且有界, 所以" $prec
      "没有无限上升链, 于是可以沿" $prec "归纳; 后文" (Q "沿秩归纳")
      "指对" (app $r $c) "从小到大归纳, 先处理提供者和父组件, 再处理依赖者和子组件. "
      "第二, 终止的证明把度量按秩分层, 低秩的层优先 (" (Ref "thm-termination") "). "
      "规则一节中" (Q "键同时在" $d "和" $p "中的组件") "满足" (&prec $c $c)
      ", 所以没有秩.")
   ((Theorem #:id "thm-progress")
    "设" $prec "没有无限上升链" (&prec (_ $c $0) (_ $c $1) $..c)
    ". 对满足不变式的" $gamma ","
    (MB (&cm (&=> (&quiet $gamma) (&settled $gamma))
             (&disj (&settled $gamma)
                    (∃ (&cm $n (∈ $ell (app $L $n)) $gamma^) (Step $gamma $ell $gamma^)))) ".")
    "即没有生命周期规则可用的状态一定是稳定的, 不稳定的状态总有生命周期规则可走. ")
   ((proof)
    "设没有生命周期规则可用, 但状态不稳定. 先按生命周期状态检查一个不稳定的fiber " $n ":"
    (Ul (Li (Inactive $_) ": 若" τ_n ", 则" $L-Remove "可用 (由不变式, 未安装的fiber没有子fiber); 否则不稳定意味着"
            (&= θ_n (Inactive $bottom)) "且" (&!= (&target $n) $bottom) ", 于是" $L-Begin "可用.")
        (Li "Loading: 视" (&target $n) "是否等于视图, " $L-Step "或" $L-Divert "可用.")
        (Li "Active: 不稳定意味着" (&target $n) "不等于视图, 于是" $L-Leave "可用.")
        (Li "Unloading: 累加器为空时" $L-Unloaded "可用; 表头为" $UOp "时" $L-Undo
            "总是可用; 表头为" (appl $USpawn $m) "且" $m "不存在时" $L-Undo "可用, "
            $m "存在但未被请求移除时" $L-RetireChild "可用."))
    "所以只剩下证明静止状态中没有Unloading的fiber. 沿" $prec
    "作良基归纳 (" $prec "没有无限上升链): 设组件在" c_n "之上的fiber都不处于Unloading, 证明" $n
    "也不处于Unloading. 若" $n "处于Unloading, 由上面的检查它只可能卡在两处之一:"
    (Ol (Li "撤销" (appl $UProv $k $iota) "时, 还有已安装的" $m "解析到" $iota
            ". 这时" $n "提供了" $m "声明的键, 所以" (&prec c_n (_ $c $m)) ". 又" $n
            "已经不是Active, " $iota "不再可见, 所以" (&= (&target $m) $bottom) ". " $m
            "已安装, 若它处于Loading, " $L-Divert "可用; 若处于Active, " $L-Leave
            "可用; 所以它处于Unloading, 与归纳假设矛盾.")
        (Li "撤销" (appl $USpawn $m) "时, 子fiber " $m " 还在并且已被请求移除 (否则"
            $L-RetireChild "可用). 这时" (&prec c_n (_ $c $m)) ". 若" $m "未安装, 它没有子fiber, "
            (Lab $L-Remove $m) "可用; 若已安装, 由" (_ $tau $m) "有" (&= (&target $m) $bottom)
            ", 与前一种情形同理, " $m "处于Unloading, 与归纳假设矛盾."))
    "所以静止状态中没有Unloading的fiber; 结合上面的检查, 每个fiber都稳定.")
   ((Theorem #:id "thm-termination")
    "设有以" $M "为界的秩" $r " (" (Ref "def-prec") "), "
    "并且每个程序的任一路径长度都不超过常数" $N ". 那么从满足不变式的状态出发, 不存在无限的执行"
    (MB (: (Step $gamma_0 (_ $ell $0) $gamma_1) (^^ $--> (_ $ell $1)) $..c) ",")
    "其中每个" (_ $ell $i) "都是生命周期标签. ")
   ((proof)
    "给每个fiber " $n " 一个自然数度量" (_ $mu $n) ", 它是" $target
    "不变时" $n "还能走的生命周期步数的上界. 取常数" $D
    ", 使它大于一次加载和随后的卸载所需的步数 (由" $N "和键的个数决定). 按生命周期状态:"
    (Ul (Li (Inactive $xi) ": 已请求移除时为1 (还剩" $L-Remove "), 否则为0.")
        (Li (Inactive $bottom) ": " $target "有值时为" $D " (还要整个加载一遍), 否则同上.")
        (Li "Loading, 视图等于" $target ": " $D "减去累加器的长度. 每个" $L-Step
            "都压入一项或结束加载, 所以严格下降.")
        (Li "Active, 视图等于" $target ": 0.")
        (Li "Unloading, 以及视图不等于" $target "的Loading和Active: 累加器的权重 (每项为1, "
            (appl $USpawn $m) "在子fiber还在且未请求移除时再加1, 留给" $L-RetireChild
            "), 加上收尾的步数; 若卸载后还会重新加载 (没有错误且" $target "有值), 再加" $D
            ". Loading和Active的情形再加1, 留给" $L-Divert "或" $L-Leave "."))
    "状态的度量是向量" (app $Phi $gamma) ", 它的第" $i "个分量是秩为" $i
    "的fiber的度量之和; 向量按字典序比较, 低秩的分量优先. 秩有界, 所以这个字典序是良基的. "
    "设一步生命周期规则由fiber " $n " 走出, 它的秩为" $i ". 逐条检查规则可得:"
    (Ol (Li $n "自己的度量严格下降 (被" $L-Remove "删除时记为0);")
        (Li "其他秩不超过" $i "的fiber的度量不变. 这一步可能让秩更低的fiber的某条规则变得可用 "
            "(依赖者卸载后提供者才能撤销提供, 子fiber移除后父fiber才能撤销spawn), "
            "但不改变它们的状态, 也就不改变它们的度量;")
        (Li "新出现的fiber (spawn的子fiber) 的秩高于" $i "."))
    "于是" (app $Phi $gamma) "的第" $i "个分量严格下降, 更低的分量不变, 更高的分量可能增加, "
    "整体按字典序严格下降. 良基序上没有无限下降链, 所以没有无限的执行.")
   (H3. "合流")
   (P "合流分五层证明, 每层建立在前一层的基础上.")
   ((Definition #:id "def-commute")
    "称" (tu0 $a $t_a) "与" (tu0 $b $t_b) "可交换, 如果对每个" $s ", 先" $a "后" $b
    "与先" $b "后" $a "都失败, 或都成功并给出" $simeq "相关的终态, 且" $a "和" $b
    "各自的结果相同. 恢复精确性只需要撤销独立 (" (Ref "def-revert-indep")
    "), 合流还需要可交换: 不同的调度会让操作以不同的顺序作用在同一个绑定上.")
   ((Definition #:id "def-confhyps")
    "称服务满足合流假设, 如果"
    (Ol (Li "每个操作都良好;")
        (Li "操作给出的逆与所在的状态无关: 若" (&= (&run $a $t $s_1) (tu0 $_ $g_1 $_))
            ", " (&= (&run $a $t $s_2) (tu0 $_ $g_2 $_)) ", 则"
            (∀ $x (&simeq (app $g_1 $x) (app $g_2 $x))) ";")
        (Li "令牌不影响后继(在" $simeq "意义下)和结果;")
        (Li "可能相遇的操作在不同令牌下可交换 (" (Ref "def-commute") "), 并且满足撤销独立第一条的一个弱形式: "
            "对" (tu0 $a $t_a) "给出的每个逆" $g ", 每个" (∈ $x (app $After (tu0 $a $t_a)))
            ", 以及与" $a "可能相遇的" (tu0 $b $t_b) " (" (&!= $t_b $t_a) "), 若" $b "在" $x
            "上成功, 则" $b "在" (app $g $x) "上也成功, 并且两次运行满足第一条中成功情形的关系. 这里"
            (app $After (tu0 $a $t_a)) "是满足下面两条的最小状态集合:"
            (Ul (Li (tu0 $a $t_a) "在某个状态上成功运行的后继属于它;")
                (Li "若" $w "属于它, " $b "与" $a "可能相遇, " (&!= $t_b $t_a) ", 且"
                    (&run $b $t_b $w) "成功给出后继" $w^ ", 则" $w^ "属于它."))
            "即" $a "之后, 只经过与它可能相遇的其他操作所能到达的状态."))
    "第1条已在恢复精确性中用到; 第4条中撤销独立的部分是(H)所要求的第一条的弱化 (只在"
    (app $After (tu0 $a $t_a)) "上, 只约束" $b "成功的情形); 第2, 3条和第4条里的可交换是合流新加的.")
   (P "第2条排除了逆依赖于起点的操作, 比如引言中的" (Q "把" $x "设为5")
      ". 原因是两次执行中同一个操作可能在不同的状态上运行, 若逆随状态而变, "
      "终态中待撤销的逆就不再是同一组. 所以状态相关的逆只对恢复精确性有意义, "
      "合流需要的是与状态无关的逆; 按名字注册和监听器注册都属于后者. "
      "第3条是因为令牌由调度决定, 两次执行中同一个操作拿到的令牌可能不同.")
   ((Theorem #:id "thm-control")
    (B "[控制层, 无spawn]: ") "设组件都不spawn, 有" (Ref "thm-termination")
    "中的秩" $r "和界" (&cm $M $N) ", 并且每个组件的程序的每条到达" $Done
    "的路径都提供了它可能提供的所有键. 设" $gamma_0 "满足不变式, "
    (Steps $gamma_0 $gamma_1) "和" (Steps $gamma_0 $gamma_2)
    "只走生命周期规则, " $gamma_1 "和" $gamma_2 "都静止且没有失败的fiber. 用"
    (_ $F $i) ", " (_ $B $i) "表示" $gamma_i "的注册表和绑定, "
    (_ $c (&cm $n $i)) ", " (_ $theta (&cm $n $i)) "表示" (app (_ $F $i) $n)
    "的组件和生命周期状态, 则"
    (MB (eqn*
         ((&dom (_ $F $1)) $= (&dom (_ $F $2)))
         ((_ $c (&cm $n $1)) $= (_ $c (&cm $n $2)))
         ((&= (_ $theta (&cm $n $1)) (Active $_ $_)) $<=> (&= (_ $theta (&cm $n $2)) (Active $_ $_)))
         ((app $owner (app (_ $B $1) $k)) $= (app $owner (app (_ $B $2) $k)))))
    "第二, 三式对所有" (∈ $n (&dom (_ $F $1))) "成立, 第四式对所有" $k "成立. ")
   ((proof)
    "论证不比较两次执行的过程, 而是刻画终态. 在没有失败fiber的静止状态中, "
    "一个fiber是Active当且仅当它没有被请求移除, 且它声明的每个键都由一个Active的fiber提供. "
    "这里用到了关于" $Done "路径的前提: Active的fiber提供了它可能提供的所有键. "
    "这样, Active的fiber之集合是一个不动点方程的解, 方程只依赖起点. "
    "沿秩归纳可知这个方程只有一个解. 秩的前提不能去掉: 若" $c "提供" (_ $k $1)
    "而依赖" (_ $k $2) ", " $c^ "提供" (_ $k $2) "而依赖" (_ $k $1)
    ", 则两者都Active和两者都不Active都是这个方程的解.")
   ((Theorem #:id "thm-state")
    (B "[服务状态, 无spawn]: ") "在上一个定理的设定下, 再设服务满足合流假设 ("
    (Ref "def-confhyps") "). 则对每个键" $k ", 若"
    (&= (app (_ $B $1) $k) (tupa0 $_ $_ $s_1)) "且" (&= (app (_ $B $2) $k) (tupa0 $_ $_ $s_2))
    ", 则" (&simeq $s_1 $s_2) ". ")
   ((proof)
    "分三步."
    (Ol (Li "在一次执行中, 每个绑定的状态" $simeq
            "它的初始状态再依令牌顺序执行它上面尚未撤销的操作.")
        (Li "每个已加载的fiber的累加器对应一条" (B "理想轨迹")
            ": 单独运行它的程序, 每个操作的回答都在它解析到的那次提供的初始状态上"
            "依次执行这个fiber自己在该键上先前的操作得到, 不计其他fiber的操作. "
            "可能相遇的操作可交换, 所以其他fiber穿插进来的操作不改变这里看到的结果, "
            "所以理想轨迹只由程序和初始状态决定. 沿秩归纳, 两个终态中对应的Active fiber有相同的轨迹.")
        (Li "于是每个键上待撤销的操作在两个终态中是同一组, 只是顺序可能不同; "
            "由可交换性, 重排不改变终态的" $simeq "类.")))
   ((Theorem #:id "thm-spawn")
    (B "[带spawn]: ") "设定同上, 但允许组件spawn, 并且启动时spawn的子fiber不被直接" $O-Dispose ". "
    "fiber的名字依赖于调度, 所以给每个fiber一个" (B "规范名") ": 根fiber保留自己的名字, "
    "子fiber由父fiber的规范名和它在父fiber的spawn中的位置命名. "
    "那么两个终态在重命名的意义下一致: 按规范名, fiber, 组件, 是否Active, 绑定都一一对应. 不论两个终态如何到达, "
    "只要根fiber对应, 它们就一致. ")
   ((proof)
    "父fiber是否spawn, spawn什么, 取决于它的操作的结果, 所以对应关系要沿秩归纳建立: "
    "对应的fiber有相同的组件和理想轨迹, 因而spawn出对应的子fiber. "
    "其余部分与无spawn的情形相同. 这里还需要一个不变式: Loading和Active的fiber的子fiber都还活着; "
    "对启动时spawn的子fiber直接" $O-Dispose "会破坏它, 见下文.")
   ((Theorem #:id "thm-dynamic")
    (B "[编排与生命周期交错]: ") "从初始状态出发, 接受相同编排输入"
    " (以插入次序而不是名字指称根fiber), 并以任意方式穿插生命周期步的两次执行, "
    "若都到达静止且没有失败fiber, 则在重命名意义下一致. "
    "特别地, 终态与静态组装 (先全部插入再运行) 的结果一致. ")
   ((proof)
    "插入得到的名字依赖于调度, 所以输入以插入次序指称根fiber. "
    "沿执行记录一本账: 对每个插入次序, 若它没有被" $O-Dispose ", 终态中恰有一个根fiber, "
    "组件就是插入的那个. 早先的插入, 移除请求, 重启, 重置除了留下的根fiber之外不留痕迹. "
    "于是两个终态的根fiber按插入次序一一对应, 再用上一个定理中"
    (Q "不论终态如何到达, 只要根fiber对应就一致") "的那一半即可.")
   ((Theorem #:id "thm-compose")
    (B "[服务的组合]: ") (Ref "thm-state") ", " (Ref "thm-spawn") "和" (Ref "thm-dynamic") "中对服务的要求恰是合流假设 (" (Ref "def-confhyps")
    "), 所以它们对任何满足合流假设的服务成立. 两个满足合流假设的服务之和也满足它. ")
   ((proof)
    "前一半是把三个定理的服务前提逐条对照. 和的状态属于两者之一, "
    "一方的操作在另一方的状态上失败; 不同部分的操作从不作用在同一个状态上, "
    "所以它们可交换, 也互不干扰. 同一部分的操作对按该部分的假设处理.")
   (P "合流不覆盖" $O-React ", 也只允许对根fiber用" $O-Dispose ". 原因是: 启动时spawn的子fiber若被直接" $O-Dispose ", "
      "父fiber在一次执行里保留原生命期, 在另一次执行里重新加载并重新spawn, "
      "两次的终态确实不同; 这在Cordis中同样成立, 不是模型的人为限制. "
      "保持性, 顺序, 恢复精确性, 进展, 终止都覆盖" $O-React ".")
   (H3. "派生形式" #:id "sec-meta-derived")
   ((Theorem)
    "隔离是对组件表的键重命名: 键取为服务名加作用域标签, "
    "被隔离的子树换一个标签. 重命名保持各定理对组件的假设: 程序有界, "
    "每条到达" $Done "的路径提供组件可能提供的所有键, "
    "秩的单调性, 以及独立性 (此时要对同一服务名上任意两个组件的操作成立, "
    "因为同一组件可以在多个作用域中同时运行). ")
   (P "所以对任意组件表陈述的定理都直接适用于隔离, 不需要另外的规则.")
   (H3. "汇总" #:id "sec-summary")
   (P "前面各定理的假设分散在各节. 本节把它们分成两组: 服务的规格只谈服务自身和一个操作上的关系"
      $G ", 不提任何程序; 组件的义务只谈组件表, 其中一条要求可能相遇的操作对落在" $G
      "之内. 服务的作者对" $G "证明规格, 插件的作者检查义务, 两者在" $G "处对接.")
   ((Definition #:id "def-spec")
    "设" $G "是操作上的二元关系. 称服务" Σ "相对" $G
    (Ol (Li (B "可撤销") ", 如果每个操作在每个令牌下良好, 并且"
            (MB (&=> (&conj (appl $G $a $b) (&!= $t_a $t_b)) (appl $RI $a $t_a $b $t_b)) ";"))
        (Li (B "可合流") ", 如果它满足合流假设 (" (Ref "def-confhyps") "), "
            "其中第4条的" (Q "可能相遇的操作") "换成" $G "相关的操作.")))
   ((Definition #:id "def-obligation")
    "称组件表" $prog "相对" $G "满足义务, 如果"
    (Ol (Li (B "有界") ": 存在" $N ", 每个程序的任一路径长度都不超过" $N ";")
        (Li (B "有秩") ": 存在以某个" $M "为界的秩" $r " (" (Ref "def-prec") "), 即"
            (&=> (&prec $c $c^) (&< (app $r $c) (app $r $c^) $M)) ";")
        (Li (B "相遇受控") ": " (&=> (&meet $k $a $b) (appl $G $a $b)) ";")
        (Li (B "完整提供") ": 每个组件的程序的每条到达" $Done "的路径都提供它可能提供的所有键."))
    "称编排输入是" (B "根编排") ", 如果它只由" $O-Insert "以及对根fiber (以插入次序指称) 的"
    $O-Dispose ", " $O-Restart ", " $O-Reset "组成.")
   ((Example #:id "ex-tiers")
    "下面三个服务说明规格的两部分之间的关系."
    (Ol (Li (B "注册表") " (" (Ref "ex-registry") "). 取" $G
            "关联名字不同的两次登记, 名字不同的登记与查询, 以及任意两次查询, 两部分都满足. "
            "相遇受控此时说的是: 可能相遇的两次登记, 以及登记与查询, 名字都不同, "
            "在插件的层面就是" (Q "不同插件不登记同名的工具") ". "
            "同名的两次登记不撤销独立: 后一次在实际执行中失败, 撤掉前一次之后却会成功.")
        (Li (B "监听器注册") " (" (Ref "ex-bus") "末尾). " $G
            "取全关系时可合流, 但不可撤销: 设" $c "不在列表" $x
            "中, 两个fiber各注册一次" $c ", 撤销独立的第一条要求" $x "加上" $c "之后" $simeq
            $x ", 不成立. 若" $G "只关联回调不同的注册, 两部分都满足, "
            "这时相遇受控要求可能相遇的两次注册回调不同. 即使回调相同, "
            "删去第一个" $c "在这里仍然给出正确的结果, 所以撤销独立是充分条件而非必要条件.")
        (Li (B "变量存储") ". 状态是变量到值的映射, 逐点比较; 操作" (Q "把" $x "设为" $v)
            "的逆是把" $x "改回原来的值, 也就是引言中的例子. 取" $G
            "关联不同变量上的操作, 它可撤销. 但它的逆依赖于原来的值, 违反合流假设的第2条, "
            "而这一条与" $G "无关, 所以不论" $G "怎样取, 它都不可合流."))
    "前两个说明可合流推不出可撤销, 第三个说明可撤销推不出可合流.")
   ((Theorem #:id "thm-summary")
    "设组件表相对" $G "满足有界, 有秩和相遇受控. 设" $gamma
    "是从初始状态经任意标签序列 (包括" $O-React "和对子fiber的" $O-Dispose ") 可达的状态."
   (Ol (Li (B "安全") " (不需要假设): " $gamma "满足不变式, 因而"
           (Ref "thm-ordering") "的各条在" $gamma "成立.")
       (Li (B "活性") ": 从" $gamma "出发只走生命周期步, 没有无限的执行; "
           "走到没有生命周期规则可用时, 状态是稳定的.")
       (Li (B "恢复") " (服务可撤销): 设执行从初始状态到达" $gamma ", "
           $shadow "是沿它略去fiber " $n "全部操作所得的影子存储. 若" $n "在" $gamma
           "中不存在或没有安装, 则对每个键" $k ", " (app (&store $gamma) $k) "与"
           (app $shadow $k) "同时无定义, 或者"
           (&simeq (app (&store $gamma) $k) (app $shadow $k))
           ". 取" $n "为一次反应的子fiber, 就是运行期效应的精确撤销.")
       (Li (B "合流") " (服务可合流, 组件表还满足完整提供): 两次执行从初始状态出发, "
           "接受相同的根编排输入, 以任意方式穿插生命周期步; 若都到达静止且没有失败的fiber, "
           "则两个终态在重命名意义下一致, 并且与静态组装 (只按插入次序插入留下的组件) 到达的静止终态一致.")))
   ((proof)
    (Ol (Li "安全: 即" (Ref "thm-preservation") ".")
        (Li "活性: 有秩给出" $prec "上严格上升且有界的度量, 所以" $prec
            "没有无限上升链; 由有界和有秩, " (Ref "thm-termination")
            "排除无限的生命周期执行. 生命周期步的终点仍是可达状态, 若它静止, 则由"
            (Ref "thm-progress") "它是稳定的.")
        (Li "恢复: 即" (Ref "thm-recovery") ", 取" $gamma_0 "为初始状态 (" $n
            "在其中没有待撤销操作), 而" $n "不存在或未安装时也没有待撤销操作. "
            "所需的(H)由可撤销和相遇受控得到.")
        (Li "合流: 即" (Ref "thm-dynamic") ". 由相遇受控, 可合流中对" $G
            "相关的操作所作的要求, 特别地包括了对可能相遇的操作的要求, 这正是合流假设 ("
            (Ref "def-confhyps") ") 的第4条; 有界, 有秩和完整提供是"
            (Ref "thm-control") "到" (Ref "thm-dynamic") "对组件的前提.")))
   (P "几点说明.")
   (Ul (Li (B "为什么分两部分") ". 恢复精确性要撤销独立的两条在所有状态上成立; "
           "合流只用到第一条, 并且只在" (app $After (tu0 $a $t_a)) "上, 却另外要求可交换, "
           "逆与状态无关, 令牌无关. 由" (Ref "ex-tiers") ", 两部分互不蕴涵.")
       (Li (B "义务的粒度") ". 有界和完整提供是对单个组件的要求. 有秩和相遇受控是对整张组件表的要求: "
           "前者对有限的表就是" (Q "依赖与spawn关系无环") ", 后者在插件的层面读作"
           (Q "不同插件不登记同名的工具") "这一类约定. 两者本质上是全局的, "
           "因为它们说的是插件之间的关系.")
       (Li (B "失败不是义务") ". 合流以两次执行都没有失败的fiber为前提. "
           "失败可能来自" $Fail ", 操作失败或键冲突, 取决于运行时的状态, 不是组件表的静态性质. "
           "实际使用时的检查方式是: 静态组装一次, 看它是否无失败地到达静止; "
           "由第4条, 任何其他调度到达的静止终态都与它一致.")
       (Li (B "编排的范围") ". 第1到3条对任意编排成立, 包括" $O-React
           "和对子fiber的" $O-Dispose "; 只有第4条限于根编排, 原因见合流一节.")
       (Li (B "隔离") ". 由" (Ref "sec-meta-derived")
           ", 键重命名保持有界, 有秩和完整提供; 相遇受控则要对同一服务名上任意两个组件的操作成立. "
           "满足这一点时, 本定理直接适用于隔离后的组件表."))

   (Appendix "A" #:id "sec-effects" "单个组件的语义")
   (P "本附录对应论文的3.1节, 以及3.3.2节中关于等价的部分. 它把一个程序解释为存储上带见证的部分效应, "
      "从而说明一个组件单独卸载时存储在等价意义下复原, 并且演算中累加器的撤销项按LIFO复合起来就是这个效应的逆. "
      "正文的定理不依赖本附录. "
      "效应可能失败 (例如重复提供同一个键), 而恢复只能在某个等价关系下成立: "
      "free不会恢复malloc之前的堆布局. 下面固定一个类型" Γ "及其上的等价关系" $approx
      ", 并记" (&:= (&End Γ) (&-> Γ Γ)) ".")
   ((Definition)
    "定义部分效应"
    (MB (&:= (&PEff Γ) (&-> Γ (&Maybe (@c* Γ (&End Γ))))) ",")
    (&= (app $e $gamma) (tu0 $delta $g)) "给出新状态" $delta "和这一步的逆" $g
    ". 两个结果相关, 当且仅当都是" $None ", 或者状态相关且逆逐点相关. 一个映射"
    $g "尊重" $approx ", 当且仅当" (&=> (&approx $x $y) (&approx (app $g $x) (app $g $y)))
    ". ")
   ((Definition #:id "def-pwitnessed")
    (∈ $e (&PEff Γ)) "带见证, 当且仅当"
    (Ol (Li "若" (&= (app $e $gamma) (tu0 $delta $g)) ", 则"
            (&approx (app $g $delta) $gamma) ";")
        (Li "上述" $g "尊重" $approx ";")
        (Li $e "本身尊重" $approx ": 相关的输入给出相关的结果."))
    "第一条只要求" $g "在" $e "被应用的那一点撤销它, 不要求" $g "是全局的左逆. "
    "这是论文的定义8和定义37. ")
   ((Definition)
    "对于" (∈ $e_1 $e_2 (&PEff Γ)) ", 定义先" $e_2 "后" $e_1 "的复合"
    (MB (&:= (app (@diamond $e_1 $e_2) $gamma)
             (Let (&:= (tu0 $delta $g) (app $e_2 $gamma))
                  (Let (&:= (tu0 $epsilon $h) (app $e_1 $delta))
                       (tu0 $epsilon (&compose $g $h))))) ",")
    "任一步为" $None "时结果为" $None "; 单位元是"
    (&:= (app $eta_Γ $gamma) (tu0 $gamma $id))
    ". 对" (&: $a (&-> Γ (&Maybe (@c* Γ (&End Γ) $Out))))
    "和" (&: $kappa (&-> $Out (&PEff Γ))) ", 按结果绑定为"
    (MB (&:= (app (&bind $a $kappa) $gamma)
             (Let (&:= (tu0 $delta $g $o) (app $a $gamma))
                  (Let (&:= (tu0 $epsilon $h) (app* $kappa $o $delta))
                       (tu0 $epsilon (&compose $g $h))))) ".")
    "这就是幺半群" (tu0 (&End Γ) $compose) "上的writer monad (再叠一层" $Maybe
    ") 的Kleisli复合: 日志是逆, 新的一项接在右边, 撤销时最先施加. "
    "按结果绑定是论文的式(36): 每一段执行一个操作, 再根据结果选择后续. ")
   ((Theorem #:id "thm-closure")
    "若" $e_1 ", " $e_2 "带见证, 则" (&diamond $e_1 $e_2) "带见证; "
    "若" $a "的前两个分量带见证, " $a "在相关的输入上给出相同的结果" $o
    ", 且每个" (app $kappa $o) "带见证, 则" (&bind $a $kappa)
    "带见证. 特别地, 依次执行带见证的" (&cm $e_1 $..h $e_n) "之后, "
    "按LIFO顺序施加它们各自给出的逆, 回到的状态" $approx "起点, "
    "不需要任何关于效应之间关系的假设. 这是论文的定理10, 11, 16. ")
   ((proof)
    (Ul (Li "复合: 逐条检查" (Ref "def-pwitnessed") "的三个条件."
            (Ol (Li "见证条件: 若" (&= (app $e_2 $gamma) (tu0 $delta $g)) ", "
                    (&= (app $e_1 $delta) (tu0 $epsilon $h)) ", 则"
                    (&approx (app $g (app $h $epsilon)) (app $g $delta) $gamma)
                    ", 第一步要求" $g "尊重" $approx ";")
                (Li "复合的逆是尊重" $approx "的映射的复合, 仍然尊重" $approx ";")
                (Li "复合本身尊重" $approx ", 因为相关的输入在每一段都给出相关的结果.")))
        (Li "按结果绑定: 相关的输入给出相同的结果, 所以选择的是同一个后续, 论证与复合相同.")
        (Li "LIFO: 对" $n "归纳使用复合的情形, "
            (&diamond $e_n $..c $e_1) "给出的逆正是" (&compose $g_1 $..c $g_n) ".")))
   (P "论文3.1节的其余构造, 即扭曲复合, 效应上下文与track/recover, 提升effect, "
      "以及效应的独立性与任意顺序的撤销, 本文都不需要. "
      "演算的累加器是撤销项的列表, 按LIFO逐项撤销 (" (Ref "sec-state")
      "); 撤销与其他fiber的交错由操作级的假设处理 (" (Ref "sec-recovery") ").")
   (P "下面把部分效应用到" (Ref "sec-programs") "的程序上: 先定义存储及其上的两个基本效应, "
      "再给出程序的解释.")
   ((Definition)
    "存储是" (&:= $Store (&p-> $K $St)) ", 两个存储相关当且仅当定义域相同且逐键"
    $simeq " (论文的定义33). 定义"
    (MB (eqn*
         ((app (&setk $k $s) $sigma) $:=
          (Choice0 ((tu0 (ext $sigma $k $s) (Lam $sigma^ (&\\ $sigma^ $k)))
                    ", 若" (&!in $k (&dom $sigma)))
                   ($None ", 否则")))
         ((app (par0 (&lift $k $a $t)) $sigma) $:=
          (Let (&:= (tu0 $s^ $g $o) (&run $a $t (app $sigma $k)))
               (tu0 (ext $sigma $k $s^)
                    (Lam $sigma^ (ext $sigma^ $k (app $g (app $sigma^ $k))))
                    $o)))))
    "后者在" (&!in $k (&dom $sigma)) "或" $run "失败时为" $None
    ". 提升后的逆在" (&!in $k (&dom $sigma^)) "时保持" $sigma^ "不变, 所以它是全函数. "
    "第一个是论文定义23的set, 第二个是式(23)的提升. 提升后的逆作用在"
    (Em "撤销时") "的存储上, 只改动键" $k ", 所以别的组件在中间做过的事会保留下来. ")
   ((Definition)
    "程序的解释" (&: (Den $P $t) (&PEff $Store)) " (" $t "是起始令牌) 定义为"
    (MB (eqn*
         ((Den $Done $t) $= $eta)
         ((Den $Fail $t) $= (Lam $sigma $None))
         ((Den (Con $Call $k $a $kappa) $t) $=
          (&bind (&lift $k $a $t) (Lam $o (Den (app $kappa $o) (&+ $t $1)))))
         ((Den (Con $Provide $k $s $P) $t) $=
          (&diamond (Den $P (&+ $t $1)) (&setk $k $s)))
         ((Den (Con $Spawn $c $P) $t) $= (Den $P (&+ $t $1)))))
    "spawn不是存储上的效应, 所以解释为恒等. ")
   ((Theorem #:id "thm-denote")
    "若服务的操作都良好, 则每个程序的解释都是带见证的部分效应; 特别地,"
    (MB (&=> (&= (app (Den $P $t) $sigma) (tu0 $sigma^ $g))
             (&simeq (app $g $sigma^) $sigma)) ".")
    "换言之, 一个组件单独卸载时, 存储在" $simeq "意义下复原. ")
   ((proof)
    "先看两个基本效应."
    (Ul (Li (&setk $k $s) "带见证: 在" (&!in $k (&dom $sigma)) "时, "
            (&= (&\\ (par0 (ext $sigma $k $s)) $k) $sigma)
            "; 它的逆尊重存储的等价, 因为存储是逐键比较的.")
        (Li (&lift $k $a $t) "带见证: 它的逆只改动键" $k
            ", 在该键上就是操作" $a "的逆, 由" $a "良好即得."))
    "然后对" $P "的结构归纳:"
    (Ul (Li $Done "和" $Fail ": 平凡.")
        (Li $Call ": 由" (&lift $k $a $t) "带见证和按结果绑定的封闭性 (" (Ref "thm-closure") ").")
        (Li $Provide ": 由" (&setk $k $s) "带见证和" $diamond "的封闭性.")
        (Li $Spawn ": 就是归纳假设.")))
   ((Theorem)
    "解释与演算的步骤一致: 一个操作步或provide步恰好是解释的一段, "
    "它的撤销项施加这一段的逆. 特别地, 设组件" $c "的fiber从存储" $sigma
    "出发单独 (没有其他fiber走步) 加载到Active, 开始时计数器为" $t ", 则"
    (&= (app (Den (app $body $c) $t) $sigma) (tu0 $sigma^ $g))
    "有定义, 结束时的存储" $simeq $sigma^ ", 并且它累加器中新增的撤销项按LIFO复合起来逐点"
    $simeq $g ". ")
   ((proof)
    "对" (app $body $c) "的路径归纳, 逐个比较" $L-Step "的情形与解释的一段. "
    "单独加载时, 视图" $omega "解析到的编号就是当前绑定" $B "中的编号 (" (Ref "thm-ordering")
    "第2条), 所以无论键是依赖还是自己提供的, 操作看到的都是存储在该键上的状态. 前进的一侧:"
    (Ul (Li (Con $Call $k $a $kappa) ": 规则取令牌" (&= $t $next) ", 运行" (&run $a $t $s)
            ", 写回后继并压入" (appl $UOp $k $iota $t $g) "; 这正是" (&lift $k $a $t)
            ", 并且" $next "加一, 与解释中延续" (app $kappa $o) "从" (&+ $t $1) "开始一致.")
        (Li (Con $Provide $k $s $P) ": 规则建立绑定并压入" $UProv
            ", 存储上就是" (&setk $k $s) "; 由于加载成功, 没有发生冲突, 与" (&setk $k $s)
            "有定义一致.")
        (Li (Con $Spawn $c $P) ": 规则插入子fiber并压入" $USpawn
            ", 不改动存储, 与解释为恒等一致."))
    "撤销项的一侧:"
    (Ul (Li $UOp "施加" $g ", 就是" (&lift $k $a $t) "的逆;")
        (Li $UProv "删除绑定, 就是" (&setk $k $s) "的逆;")
        (Li $USpawn "不改动存储."))
    "累加器的表头是最近的一项, 撤销时最先施加, 与复合中新的逆接在右边一致.")

   (Appendix "B" #:id "sec-observe" "观察等价")
   (P "服务的" $simeq "应该取什么? 论文的回答是: 观察者只能运行操作并读取结果, "
      "两个状态相关当且仅当没有观察者能区分它们.")
   ((Definition)
    "一个测试是字母的有限序列. 字母或者是" (appl $LF $a $t)
    ", 运行操作并记录结果 (操作失败则整个测试无定义); 或者是" (appl $LI $g)
    ", 施加某个操作可能给出的逆" $g ". 记" (&runTest $w $s)
    "为测试" $w "在" $s "上记录的结果序列 (无定义时为" $None "). 定义不可区分"
    (MB (&:<=> (&sim $s $s^) (∀ $w (&= (&runTest $w $s) (&runTest $w $s^)))) ".")
    "一个等价关系" $R "可容许, 当且仅当每个操作都尊重它 (即"
    (appl $R $s $s^) "时, " (&run $a $t $s) "与" (&run $a $t $s^)
    "同为" $None ", 或给出" $R "相关的后继和相同的结果), 并且每个可能给出的逆"
    $g "都满足" (&=> (appl $R $x $y) (appl $R (app $g $x) (app $g $y))) ". ")
   ((Theorem)
    $sim "是可容许的, 并且对每个可容许的" $R "有" (&sube $R $sim)
    ". 换言之, " $sim "是操作所尊重的最粗的等价关系. 特别地, 操作良好的服务有"
    (&sube $simeq $sim) ". ")
   ((proof)
    "可容许: 设" $s "与" $s^ "不可区分. 只有一个字母" (appl $LF $a $t)
    "的测试说明" $a "在两者上同时失败或同时成功, 且结果相同; 在一个测试前面加上这个字母"
    "仍是测试, 所以两个后继也不可区分. 逆字母同理. "
    "包含: 设" $R "可容许, " (appl $R $s $s^) ". 对测试的长度归纳: 每个字母都把"
    $R "相关的状态送到" $R "相关的状态, 并给出相同的结果, 所以整个测试在两边的结果相同.")
   (P "这就是最大互模拟: 测试每走一个字母, 两边同时走一步, 比较定义性与结果.")
   ((Theorem)
    "在带分发的有序事件总线上, 设" (&!= $c_1 $c_2) "是两个回调, 分别用令牌"
    (&!= $t_1 $t_2) "注册; 从同一状态出发, 记"
    (_ $s (Mn "12")) "为先注册" $c_1 "再注册" $c_2 "所得的状态, "
    (_ $s (Mn "21")) "为相反顺序所得的状态. 则对每个可容许的" $R ", "
    (&neg (appl $R (_ $s (Mn "12")) (_ $s (Mn "21"))))
    ". 换言之, 不论取哪个可容许的等价关系, 两次注册都不可交换. ")
   ((proof)
    "设两种顺序得到的状态被某个可容许的等价关系联系起来. 由上一个定理, "
    "它们不可区分. 但只含一个分发字母的测试按注册顺序返回回调的列表, "
    "两种顺序给出的列表不同, 矛盾.")
   (P "分发会暴露注册的顺序. 所以要让注册可交换, 只能像" (Ref "ex-bus")
      "末尾那样去掉分发, 改用监听器注册; 这是被迫的, 而不是随意的选择.")

   (Appendix "C" #:id "sec-compare" "与论文的对照")
   (P "关系一栏: 忠实指与论文的陈述一致; 改写指对应物形式不同; "
      "更强, 更弱指结论或假设比论文强或弱; 无对应指本文的论证路线用不到它们.")
   (Tab (HRow "论文" "本文" "关系")
        (Row "定义8, 9, 定理10, 11, 16" (Ref "sec-effects") "忠实, 读作部分且在等价下")
        (Row "定义1–7, 12–15, 17–21" -- "无对应: 扭曲复合, track/recover, 提升effect, 效应独立性")
        (Row "定义22–23" (Span (Ref "sec-programs") ", " (Ref "sec-state") ", " (Ref "sec-effects")) "改写: 演算中由绑定承载; 附录A中的set与提升与论文一致")
        (Row "定义24" (Ref "sec-service") "改写: 单一服务, 多一个令牌")
        (Row "定义25–26" (Ref "sec-derived") "改写: 不单独定义notify")
        (Row "定义27, 30–32" -- "未讨论")
        (Row "定义28–29" (Ref "sec-meta-derived") "改写: 键重命名")
        (Row "定义33" (Ref "sec-effects") "忠实")
        (Row "定义34, 引理35" (Ref "sec-observe") "忠实")
        (Row "定义36–37, 引理38" (Ref "sec-effects") "部分: 只需复合与绑定的封闭性")
        (Row "定义41" (Ref "sec-programs") "改写: 按键集索引的归纳类型")
        (Row "定义39, 定理40, 42" -- "无对应: 演算直接以操作级的假设(H)与合流假设陈述")
        (Row "4.1–4.3节" (Span (Ref "sec-state") ", " (Ref "sec-rules")) "改写")
        (Row "定理59" (Ref "thm-preservation") "更强: 不变式更多")
        (Row "定理61, 推论62" (Ref "thm-recovery") "更强")
        (Row "定理63" (Ref "thm-ordering") "更弱: 去掉第3条")
        (Row "定理64" (Ref "thm-ordering") "忠实")
        (Row "定理66" (Span (Ref "thm-progress") ", " (Ref "thm-termination")) "更弱: 没有定量界")
        (Row "引理54–57, 71, 72" -- "无对应")
        (Row "定理73(1)" (Ref "thm-dynamic") "更弱")
        (Row "定理73(2)" (Ref "thm-dynamic") "部分: 不含O-React")
        (Row -- (Ref "sec-rules") "新增: O-Reset, O-Restart, O-React"))
   (P "下面按正文的顺序列出与论文的具体差别.")
   (Ul (Li (Ref "sec-service") ": 论文的定义24把键" $k "上的余效应定义为三元组 (值类型, 等价关系, 操作集合), "
           "每个键一个. 本文整理成一个记录, 有两处不同: 多了令牌参数; 全系统只有一个服务, "
           "需要多种服务时取和. 除去" $Maybe "和令牌, 操作的形状"
           (&-> $St (&c* $St (&End $St) $Out)) "与论文的式(22)相同.")
       (Li (Ref "sec-programs") ": 论文的定义41把组件的效应函数刻画为"
           (Q "每一段执行一个操作, 再根据结果选择后续") "的闭包, 本文直接做成归纳类型.")
       (Li (Ref "sec-state") ", " (Ref "sec-rules") ": 论文的演算先假设迁移是原子的, 瞬时的, 不会失败的 (4.2节), "
           "再在4.3节逐一去掉这些假设; 本文直接给出去掉之后的版本. 设计决定中与论文的差别有: "
           "只有一个移除请求, 条目由" $L-Remove "自动删除, 代替论文的Retire加Remove; "
           "累加器逐项撤销, 论文一步应用整个累加器; 不保留论文中" (Q "让在途迭代落地")
           "的分支; 增加" $O-Reset ", 论文中失败的组件永不重入.")
       (Li (Ref "sec-derived") ": " $target "是论文定义25, 26 (满足谓词与notify) 的操作化. "
           "论文把每次迁移分类为activating, deactivating, neutral, 本文不单独定义notify, "
           "而是让规则以" $target "的变化为前提.")
       (Li (Ref "thm-preservation") ": 论文的保持性 (定理59) 只有4条不变式.")
       (Li (Ref "thm-ordering") ": 去掉了论文定理63的第3条 (依赖者读到的值不变).")
       (Li (Ref "thm-recovery") ": 论文的定理61要求操作可交换; 本文只要求可能相遇的操作对撤销独立, "
           "同一fiber内部不需要任何假设, 并且覆盖运行期效应.")
       (Li (Ref "thm-termination") ": 论文的定理66给出了定量的界, 本文没有.")
       (Li (Ref "thm-control") ": 关于" $Done "路径的前提是论文的定义69. 论文的引理68, 70 (支持集) "
           "只在无spawn的控制层有对应, 有spawn时由规范名对应.")
       (Li (Ref "thm-dynamic") ": 论文的定理73(1)构造了一个规范执行, 其中各组件的episode按依赖顺序排列; "
           "本文只断言终态与静态组装一致, 并把静态组装能否无失败地到达静止作为前提.")
       (Li (Ref "sec-meta-derived") ": 隔离不需要论文定义28的领域表. "
           "论文的拦截 (定义30, 31) 视为传给服务的数据, 不影响生命周期, 本文不讨论."))

   (Appendix "D" #:id "sec-rocq" "Rocq形式化")
   (P "本文的演算和各定理在Rocq中形式化 (" (Code "theories") "目录), 其中没有" (Code "Admitted")
      ", 也没有公理; 定理的前提都以" (Code "Hypothesis") "或" (Code "Variable")
      "的形式写出. 正文的证明是这些形式证明的概要, 下表给出对应关系.")
   (Tab (HRow "本文" "Rocq" "文件")
        (Row (Ref "thm-preservation") (Code "preservation, wf_reachable") (Code "Preservation.v"))
        (Row (Ref "thm-ordering") (Code "begin_requires_deps, provider_outlives, withdraw_waits, …") (Code "Ordering.v"))
        (Row (Ref "thm-omission") (Code "omission") (Code "Service.v"))
        (Row (Ref "thm-recovery") (Code "recovery, recovery_exact, recovery_from_init") (Code "Recovery.v"))
        (Row (Ref "cor-runtime") (Code "effect_exact, effect_exact_inactive") (Code "Runtime.v"))
        (Row (Ref "thm-progress") (Code "progress, quiet_settled") (Code "Progress.v"))
        (Row (Ref "thm-termination") (Code "termination, phi_drop, no_infinite_run") (Code "Termination.v"))
        (Row (Ref "thm-control") (Code "control_confluence") (Code "Confluence.v"))
        (Row (Ref "thm-state") (Code "state_confluence") (Code "StateConf.v"))
        (Row (Ref "thm-spawn") (Code "spawn_confluence") (Code "SpawnConf.v"))
        (Row (Ref "thm-dynamic") (Code "dynamic_confluence, static_assembly") (Code "Dynamic.v"))
        (Row (Ref "thm-compose") (Code "sum_hyps, service_confluence") (Code "Compose.v"))
        (Row (Ref "thm-summary") (Code "summary_wf, summary_live, summary_recovery, summary_confluence, summary_static") (Code "Summary.v"))
        (Row (Ref "thm-closure") (Code "witnessed_submonoid, pdiamond_witnessed, pbind_witnessed") (Span (Code "EffectFun.v") ", " (Code "Bridge.v")))
        (Row (Ref "thm-denote") (Code "denote_witnessed") (Code "Bridge.v"))
        (Row "附录A的最后一个定理" (Code "lone_denote") (Code "Bridge.v"))
        (Row (Ref "sec-observe") (Code "indist_admissible, admissible_indist, wb_indist, bus_no_commute") (Code "Observe.v")))
   (P "几点说明.")
   (Ul (Li "恢复精确性证明中的两个不变式在Rocq中分别是" (Code "R") "和" (Code "Prov")
           "; 把撤销独立的两条推广到一串逆的引理是" (Code "shifted_all") "和" (Code "commute_all") ".")
       (Li "合流假设 (" (Ref "def-confhyps") ") 是" (Code "Compose.v") "中的" (Code "ConfHyps")
           ", 第4条中的状态集合是" (Code "ServiceSeq.v") "中归纳定义的" (Code "After") ".")
       (Li "终止证明中的度量是" (Code "Termination.v") "中的" (Code "mu") "和" (Code "Phi")
           ", 字典序是" (Code "lexR") ".")
       (Li "服务状态合流证明中的理想轨迹是" (Code "Episode.v") "中的" (Code "Tr") ".")
       (Li (Code "Bridge.v") "还证明了论文的定义39蕴涵本文的假设(H)和合流假设 ("
           (Code "paper_hind, paper_confhyps") ").")
       (Li "形式化的覆盖范围与正文一致: 合流的结果都不含" $O-React ", 只允许对根fiber用" $O-Dispose
           "; 控制层和服务状态两层假设没有spawn; 附录A最后一个定理只对单独加载的fiber成立."))
   )))
