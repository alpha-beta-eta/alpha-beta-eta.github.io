#lang racket
(provide dsh0.html)
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
(define $acc (Mi "acc"))
(define $visible (Mi "visible"))
(define (&visible k) (app $visible k))
(define $target (Mi "target"))
(define (&target n) (app $target n))
(define $resolves (Mi "resolves"))
(define (&resolves m p) (appl $resolves m p))
(define $held (Mi "held"))
(define (&held n a) (appl $held n a))
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
(define $nil (Mi "[]"))
(define θ_n (_ $theta $n))
(define τ_n (_ $tau $n))
(define c_n (_ $c $n))
(define π_m (_ $pi $m))
(define τ_m (_ $tau $m))
(define θ^_n (_^ $theta $n $prime))
(define acc^_n (_^ $acc $n $prime))
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
(define $EOp (Rn "EOp"))
(define $EUnresolved (Rn "EUnresolved"))
(define $Register (Mi "Register"))
(define $q (Mi "q"))
(define $gamma_init (_ $gamma (Rn "init")))
(define $Lookup (Mi "Lookup"))
(define $Name (Mi "Name"))
(define $Lb (Mi "Tag"))
(define $Scope (Mi "Scope"))
(define $key (Mi "key"))
(define $enter (Mi "enter"))
(define $ren (Mi "ren"))
(define $progS (^ $prog (Mi "sc")))
(define $rho_c (_ $rho $c))
(define $share (Mi "share"))
(define $chk (Mi "chk"))
(define $ProvideChk (Mi "ProvideChk"))
(define $abar (Mi "&#x0101;"))
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
;lifecycle diagram
;(x, y) is the baseline point, as for a text element
(define (:label x y #:anchor [anchor "middle"] . html*)
  (define width 200)
  (define left
    (case anchor
      (("start") x)
      (("end") (- x width))
      (else (- x (/ width 2)))))
  (define justify
    (case anchor
      (("start") "flex-start")
      (("end") "flex-end")
      (else "center")))
  (ForeignObject
   #:attr* `((x ,(number->string left))
             (y ,(number->string (- y 15)))
             (width ,(number->string width))
             (height "22"))
   (Div #:attr* `((style ,(string-append
                           "display: flex; height: 100%; align-items: center; "
                           "justify-content: " justify "; white-space: nowrap;")))
        (apply Span html*))))
(define (:node x y . html*)
  (G (Rect #:attr* `((x ,(number->string (- x 58)))
                     (y ,(number->string (- y 17)))
                     (width "116")
                     (height "34")
                     (rx "8")
                     (fill "white")
                     (stroke "black")))
     (apply :label x (+ y 5) html*)))
(define (:edge d #:orchestration? [orchestration? #f])
  (Path #:attr* `((d ,d)
                  (fill "none")
                  (stroke "black")
                  (marker-end "url(#lifecycle-arrow)")
                  . ,(if orchestration? '((stroke-dasharray "5 3")) '()))))
(define lifecycle.svg
  (Svg
   #:attr* '((viewBox "0 0 740 345")
             (width "740")
             (font-size "13")
             (style "display: block; margin: auto; max-width: 100%; height: auto;"))
   (Defs (Marker #:attr* '((id "lifecycle-arrow")
                           (viewBox "0 0 10 10")
                           (refX "10")
                           (refY "5")
                           (markerWidth "8")
                           (markerHeight "8")
                           (orient "auto"))
                 (Path #:attr* '((d "M 0 0 L 10 5 L 0 10 z")))))
   (Circle #:attr* '((cx "50") (cy "80") (r "16") (fill "white") (stroke "black")))
   (:label 50 85 $empty)
   (:node 210 80 (Inactive $bottom))
   (:node 420 80 $Loading)
   (:node 660 80 $Active)
   (:node 420 260 $Unloading)
   (:node 210 260 (Inactive $xi))
   (:edge "M 64 72 L 152 72" #:orchestration? #t)
   (:label 108 64 $O-Insert)
   (:edge "M 152 90 L 63 90")
   (:label 108 106 $L-Remove)
   (:edge "M 152 250 L 58 95")
   (:label 104 190 $L-Remove #:anchor "end")
   (:edge "M 268 80 L 362 80")
   (:label 315 72 $L-Begin)
   (:edge "M 400 63 C 390 25, 450 25, 440 63")
   (:label 420 22 $L-Step)
   (:edge "M 478 80 L 602 80")
   (:label 540 72 $L-Step " (" $Done ")")
   (:edge "M 640 63 C 630 25, 690 25, 680 63" #:orchestration? #t)
   (:label 660 22 $O-React)
   (:edge "M 410 97 L 410 243")
   (:label 402 160 $L-Divert #:anchor "end")
   (:label 402 178 $L-Step " (失败)" #:anchor "end")
   (:edge "M 430 97 L 430 243" #:orchestration? #t)
   (:label 438 140 $O-Restart #:anchor "start")
   (:edge "M 640 97 L 470 243")
   (:label 512 200 $L-Leave #:anchor "end")
   (:edge "M 665 97 L 478 258" #:orchestration? #t)
   (:label 585 180 $O-Restart #:anchor "start")
   (:edge "M 400 277 C 390 315, 450 315, 440 277")
   (:label 420 332 $L-Undo ", " $L-RetireChild)
   (:edge "M 370 243 L 262 97")
   (:label 300 165 $L-Unloaded #:anchor "end")
   (:edge "M 362 260 L 268 260")
   (:label 315 252 $L-Unloaded)
   (:edge "M 210 243 L 210 97" #:orchestration? #t)
   (:label 202 175 $O-Reset #:anchor "end")))
(define dsh0.html
  (parameterize ([parameter:format-num format-num])
  (TnTmPrelude
   #:title "deepseek harness笔记0"
   #:css "styles.css"
   (H1. "deepseek harness笔记0")
   (P "本文重新组织论文" (I "A Programming Paradigm for Spatiotemporal Composability")
      " (arXiv:2608.25512v1) 的第3章和第4章, 并以插件运行时Cordis及其在deepseek harness中的使用为背景. "
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
   (P "出发点是: 插件卸载时, 它对共享状态做过的每件事都必须撤销, "
      "所以每个改动共享状态的操作都要给出它的逆; 逆可以依赖于操作作用前的状态, "
      "比如" (Q "把" $x "设为5") "的逆是" (Q "把" $x "改回原来的值")
      ". 另一方面, 插件通过服务相互依赖, 提供者卸载时依赖者必须先停下.")
   (P "下图是一个fiber (组件的一个实例) 的生命周期, 规则的精确陈述见" (Ref "sec-rules") ".")
   lifecycle.svg
   (P "实线是生命周期规则, 由演算自己驱动; 虚线是编排规则, 代表外部输入; "
      "空心的" (Q "∅") "表示条目不在fiber表中. " $L-Step "在加载中每次执行一个操作, "
      "程序到达" $Done "时进入Active, 失败时进入Unloading. "
      $L-Unloaded "在没有错误时回到" (Inactive $bottom) ", 否则停在" (Inactive $xi)
      ", 由" $O-Reset "决定是否重试. "
      "图中略去" $O-Dispose ": 它只设置" (Q "已请求移除") "标记, 在每个状态都可用, "
      "之后由" $L-Divert ", " $L-Leave "和" $L-Remove "完成移除.")
   (P "第1章到第5章给出模型: 服务, 程序, 组件与fiber, 派生概念, 以及生命周期规则. "
      "第6章证明元理论, 每个定理需要的服务假设在它所在的节里引入. "
      "附录A给出单个组件的语义 (论文3.1节的对应物), 附录B说明服务的等价关系应该怎样取, "
      "附录C逐条对照论文, 附录D列出各定理在Rocq形式化中的对应. 正文的定理不依赖附录; "
      "论文中本文的论证用不到的部分不再复述, 只在对照表中列出. "
      "模型的三项扩展 (可选依赖, 编排者, 交集生命期) 见" (A "笔记1" #:attr* '((href "dsh1.html"))) ".")
   (P "主要结果如下, 其中的术语 (fiber, Active, 撤销独立, 秩等) 在正文中定义.")
   (Ul (Li (B "保持性与顺序") ": 良构性被每条规则保持; fiber只针对Active的提供者开始加载, "
           "提供者比依赖者活得久 (" (Ref "thm-preservation") ", " (Ref "thm-outlive") ", " (Ref "thm-ordering") ").")
       (Li (B "恢复精确性") ": 若服务的操作良好, 且可能相遇的操作撤销独立, "
           "则卸载一个fiber的结果, 等于同一次执行中略去它的全部操作所得的结果 ("
           (Ref "thm-recovery") ").")
       (Li (B "进展与终止") ": 若依赖与spawn关系没有无限上升链, 则没有生命周期规则可用的可达状态都是稳定的; "
           "再有秩和程序路径长度的界, 从可达状态出发就没有只走生命周期步的无限执行 ("
           (Ref "thm-progress") ", " (Ref "thm-termination") ").")
       (Li (B "合流") ": 再设可能相遇的操作可交换 (连同合流假设的其余几条), 且每条到达" $Done
           "的路径提供组件可能提供的全部键, 则不同调度到达的终态, 只要都静止且没有失败的fiber, "
           "就在重命名意义下一致, 并与静态组装的结果一致 ("
           (Ref "thm-dynamic") ").")
       (Li (B "汇总") ": 以上性质按服务规格的两部分和组件义务的各条重新组织为一个定理: "
           "服务一侧只依赖一个操作上的关系, 插件一侧只需逐条检查义务 ("
           (Ref "thm-summary") ")."))

   (H2. "服务" #:id "sec-service")
   (P "所有跨组件共享的可变状态都放在服务里. 服务是一个记录, 包含状态类型, 等价关系和操作.")
   ((Definition)
    "一个服务" Σ "由以下部分组成: 状态类型" $St ", 其上的等价关系" $simeq
    ", 操作类型" $Op " (参数已经放进操作里), 结果类型" $Out ", 以及"
    (MB (&: $run (&-> $Op $Tok $St (&Maybe (@c* $St (&End $St) $Out)))) ".")
    "其中" (&:= (&End $St) (&-> $St $St)) ", " (&Maybe $A) "是" $A
    "加上表示失败的" $None ", " (&= $Tok $NN) "是运行时交给这次操作的新鲜令牌, "
    "用于事件钩子这类生成出来的身份 (JavaScript中由对象身份充当).")
   (P "全系统只有一个服务" Σ ", 不是每个键一个; 需要多种服务时先取它们的和, "
      "这样避开了依赖类型的映射. 操作作用在状态上, 给出新状态, 这一步的逆, 以及结果. "
      "结果是程序获得信息的唯一途径, 也是观察者区分状态的依据, 见"
      (Ref "sec-observe") ".")
   ((Definition #:id "def-pointwise")
    "设" (&: (&cm $f $f^) (&-> $St $St)) ". 我们称" $f "与" $f^ (B "逐点相关") ", 如果对每个" (∈ $x $St) "有"
    (&simeq (app $f $x) (app $f^ $x)) "; 称" $f (B "尊重") $simeq ", 如果" (&simeq $x $y) "时"
    (&simeq (app $f $x) (app $f $y)) ".")
   ((Definition #:id "def-wb")
    "我们称操作" $a "在令牌" $t "下" (B "良好") ", 如果"
    (Ol (Li (B "带见证") ": 若" (&= (&run $a $t $s) (tu0 $s^ $g $o)) ", 则"
            (&simeq (app $g $s^) $s) ";")
        (Li (B "尊重") ": 若" (&simeq $s_1 $s_2) ", 则" (&run $a $t $s_1) "与"
            (&run $a $t $s_2) "都是" $None ", 或分别是" (tu0 (_prime $s $1) $g_1 $o)
            "和" (tu0 (_prime $s $2) $g_2 $o) ", 其中"
            (&simeq (_prime $s $1) (_prime $s $2)) ", 且" $g_1 "与" $g_2 "逐点相关 (" (Ref "def-pointwise") ");")
        (Li (B "逆尊重") ": 若" (&= (&run $a $t $s) (tu0 $s^ $g $o))
            ", 则" $g "尊重" $simeq ".")))
   ((Example #:id "ex-registry")
    "按名字索引的注册表 (如" (Code "ctx.tools") "): 状态是名字到可选值的映射, 逐点比较. "
    (appl $Register $x $v) "在" $x "已被占用时失败, 逆是删去" $x "; "
    (app $Lookup $x) "读出" $x "处的值, 逆是恒等. 每个操作都良好. "
    "名字不同的操作互不干扰; 同名的两次登记中后一次失败, 所以它们谁先谁后是看得出来的.")
   ((Example #:id "ex-bus")
    "有序事件总线 (如" (Code "ctx.on") "): 状态是钩子的列表, 每个钩子是令牌和回调的对. "
    "以令牌" $t "注册回调" $f ": 若表中已有令牌为" $t "的钩子, 注册失败; 否则在表尾加入" (tu0 $t $f) ", 逆是删去令牌为" $t
    "的钩子. 分发按顺序返回回调的列表, 逆是恒等. 两次注册不可交换, 因为分发看得到顺序 ("
    (Ref "sec-observe") "说明不论取哪个等价关系都是如此). "
    "令牌用在逆上: Cordis的旧做法按回调注销, 即删去第一个回调为" $f "的钩子; 若" $f
    "已经注册过一次, 撤销第二次注册删掉的是第一次的钩子, 所以它不是精确的逆. "
    "去掉分发并忽略顺序, 就得到监听器注册: 状态是回调的列表, 按多重集比较, "
    "注册的逆是删去第一个" $f ". 在多重集的意义下删去哪一个" $f "都一样, 所以这里不需要令牌.")

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
    (Con $Spawn $c $P) "注册子组件" $c ".")
   (P "这就是一个free monad, " $kappa "是延续. 键的限制写在构造子里, "
      "所以程序只能操作它声明的依赖和它可能提供的键, 这由构造保证, 不需要另外检查; "
      "即便" $kappa "无穷分叉, 也不必遍历它的路径. 程序是良基的 (每条路径都有限, "
      "但路径长度未必有界), 看不到状态, "
      "能得到的信息只有操作的结果. 名字和提供编号由规则层分配, 程序不接触.")

   (H2. "组件, fiber与状态" #:id "sec-state")
   (P "演算的粒度是: 每个服务操作是一个原子步, 步与步之间都可以放弃, 任何一步都可能失败. "
      "组件的一个实例称为fiber, 这个词沿用Cordis (取自React), 与fibration无关.")
   ((Definition #:id "def-component")
    "一个" (B "组件") "是四元组"
    (MB (tupa0 (&sube $d $K) (&sube $p $K) (∈ $body (_ $Prog (&cm $d $p))) $reacts) ",")
    "分别是依赖, 可能提供的键, 启动程序, 以及事件处理器 (组件标识的列表). "
    (B "组件表") "是映射" (&: $prog (&-> $C $Component)) ", 其中" $C "是组件标识的集合. 对"
    (∈ $c $C) ", 把" (app $prog $c) "的四个分量记作"
    (&cm (app $d $c) (app $p $c) (app $body $c) (app $reacts $c)) ".")
   (P "组件表不属于状态. 除非另外说明, 以下总设组件表" $prog "固定. "
      $body "的类型已经保证它只操作" (&union $d $p) "中的键, 只提供" $p "中的键. "
      "与Cordis不同, 提供的键是静态声明的. 这使得不同fiber的" $p
      "两两不相交可以在插入时检查. 若组件表有限, 并且每个程序中出现的" $Spawn
      "可以有效地列举 (例如程序只有有限条路径), 依赖与spawn关系 (" (Ref "def-prec")
      ") 是否无环也可以静态判定; 进展和合流都需要无环.")
   ((Definition #:id "def-state")
    "演算的" (B "状态") "是三元组" (&= $gamma (tupa0 $next $F $B)) ", 其中"
    (Ul (Li (∈ $next $NN) "是新鲜名字, 提供编号和令牌共用的计数器;")
        (Li (&: $F (&p-> $NN $Fiber)) "是" (B "fiber表") ", 把fiber的名字映到fiber;")
        (Li (&: $B (&p-> $K $Binding)) "是当前的" (B "服务绑定") "."))
    "其中的各部分由下面的文法给出:"
    (MB (eqn*
         ($Fiber $::= (tupa0 (∈ $c $C) (∈ $pi (&+ $NN (setE $root))) (∈ $tau $BB) $theta))
         ($Binding $::= (tupa0 (∈ $iota $NN) (∈ $q $NN) (∈ $s $St)))
         ($theta $::= (&\| (Inactive $zeta) (Loading $P $acc $omega)
                           (Active $acc $omega) (Unloading $acc $omega $zeta)))
         ($zeta $::= (&\| $bottom $EUser $EOp $EUnresolved $EConflict))
         ($u $::= (&\| (appl $UOp $k $iota $t $g) (appl $UProv $k $iota) (appl $USpawn $m)))))
    "各部分的含义如下."
    (Ul (Li "fiber " (tupa0 $c $pi $tau $theta) ": " $c "是它的组件, " $pi "是父fiber的名字 (根fiber为"
            $root "), " $tau "是" (Q "已请求移除") "标记, " $theta "是" (B "生命周期状态") ".")
        (Li "绑定 " (tupa0 $iota $q $s) ": " $iota "是这次提供的" (B "提供编号") ", " $q
            "是提供者的名字, " $s "是服务状态.")
        (Li "错误 " $zeta ": " (&= $zeta $bottom) "表示没有错误. 以下" $xi
            "总表示非" $bottom "的错误. 它们依次在程序执行" $Fail ", 操作失败, 解析失败, 键冲突时产生; "
            "具体的错误值不影响本文的结论.")
        (Li (B "累加器") " " $acc ": 撤销项的列表, 表头是最近的一项.")
        (Li (B "撤销项") " " $u ": " (appl $UOp $k $iota $t $g) "记下在键" $k "上, 提供编号为" $iota
            "的绑定上以令牌" $t "做过的操作, " $g "是它给出的逆; "
            (appl $UProv $k $iota) "记下这个fiber建立的, 键" $k "上编号为" $iota "的绑定; "
            (appl $USpawn $m) "记下这个fiber插入的子fiber " $m ".")
        (Li (B "视图") " " (&: $omega (&p-> $K $NN)) ": 定义域恰为该fiber的组件的依赖, "
            "记录每个依赖解析到的提供编号."))
    (B "初始状态") "是" (&:= $gamma_init (tupa0 $0 $empty $empty)) ".")
   ((Definition #:id "def-fiber-notation")
    "设" (&= $gamma (tupa0 $next $F $B)) "是状态, " (∈ $n (&dom $F)) ". 记"
    (MB (&= (app $F $n) (tupa0 c_n (_ $pi $n) τ_n θ_n)) ",")
    "我们称"
    (Ul (Li $n (B "失败") ", 如果" (&= θ_n (Inactive $xi)) ";")
        (Li $n (B "已安装") ", 记作" (&installed $n) ", 如果" (&!= θ_n (Inactive $_))
            ", 即" $n "处于Loading, Active或Unloading."))
    $n "已安装时, " θ_n "中恰有一个累加器和一个视图 (见上面" $theta "的文法), 把它们分别记作"
    (_ $acc $n) "和" (_ $omega $n) ". "
    "这些记号都相对于状态" $gamma ". 以下谈到状态" $gamma "时, 总把它的分量记作"
    $next ", " $F ", " $B ", 不再每次写出. 涉及其他状态时, 记号随状态一起加装饰: 对"
    (&= $gamma^ (tupa0 (^ $next $prime) $F^ $B^)) ", 相应的记号记作" (_^ $c $n $prime) ", " θ^_n ", " acc^_n
    ", " (_^ $omega $n $prime) "等; 对" (&= $gamma_i (tupa0 (_ $next $i) (_ $F $i) (_ $B $i)))
    "同理.")
   (P "几点说明.")
   (Ul (Li "四个生命周期状态代替Cordis的六个: " (Code "FAILED") "就是"
           (Inactive $xi) ", " (Code "DISPOSED") "就是条目不存在.")
       (Li "累加器是" (Ref "sec-effects") "中复合给出的逆, "
           "只是拆成撤销项的列表, 以便逐项撤销. "
           $UOp "项记下操作用的令牌, 这是恢复精确性的证明所需要的.")
       (Li "每次" $Provide "步建立绑定时取一个新的提供编号, 以它作为这次提供 (provision) 的身份. "
           "提供者撤销提供再重新提供时编号改变, "
           "依赖它的fiber会重新加载, 绝不会拿着过期的绑定继续运行.")
       (Li "绑定中的服务状态被提供者和所有依赖者共同操作, 是共享且可变的."))

   (H2. "派生概念" #:id "sec-derived")
   ((Definition #:id "def-derived")
    "设" $gamma "是状态 (记号见" (Ref "def-fiber-notation") "). 定义"
    (MB (eqn*
         ((&visible $k) $:=
          (Choice0 ($iota ", 若" (&= (app $B $k) (tupa0 $iota $q $_)) "且"
                       (&= (_ $theta $q) (Active $_ $_)))
                   ($bottom ", 否则")))
         ((&target $n) $:=
          (Choice0 ($bottom ", 若" τ_n "或" (∃ (∈ $k (app $d c_n)) (&= (&visible $k) $bottom)))
                   ((&\|-> (∈ $k (app $d c_n)) (&visible $k)) ", 否则")))
         ((&resolves $m $iota) $:<=>
          (&conj (&installed $m) (∃ $k (&= (app (_ $omega $m) $k) $iota))))
         ((&held $n $acc) $:<=>
          (∃ (∈ (appl $UProv $k $iota) $acc) (∃ (&!= $m $n) (&resolves $m $iota))))))
    "并称组件" $c "在" $gamma "中" (B "不冲突") ", 如果对每个" (∈ $n (&dom $F)) "有"
    (&= (&cap (app $p $c) (app $p c_n)) $empty) ". 这些概念同样相对于" $gamma ", 涉及其他状态时按" (Ref "def-fiber-notation")
    "的约定加装饰.")
   (P (&visible $k) "是此刻对新的解析可见的提供编号, " (&target $n) "是" $n
      "此刻应有的视图, 为" $bottom "表示" $n "不该运行, " (&resolves $m $iota) "说" $m
      "的视图中有编号" $iota ", " (&held $n $acc) "说" $n "的累加器" $acc
      "中尚未撤回的某个提供仍被别的已安装fiber解析. " $target "是整个演算的驱动力: 生命周期规则不断让每个fiber的实际状态追赶它的"
      $target ", 规则以" $target "的变化为前提, 不需要单独的通知机制. "
      $visible "只计入Active的提供者, 所以提供者一进入Unloading, "
      "它的服务就对新的解析不可见. 已经解析到它的依赖者仍持有视图, 但它们的" $target
      "随之改变, 因此加载中的依赖者不能完成加载; 它们对这个绑定剩下的动作, 只有在途迭代的落地 (见" $L-Step
      ") 和卸载时" $L-Undo "施加的逆. 卸载的守卫" (&neg (&held $n $acc)) " (" (Ref "sec-rules")
      ") 保证它们卸载完毕之前, 提供者什么都不撤: 绑定和提供者的其他效应都还在.")

   (H2. "规则" #:id "sec-rules")
   ((Definition #:id "def-step")
    (B "标签") "分两类:"
    (Ul (Li (B "编排标签") ": " (Lab $O-Insert $c) ", " (Lab $O-Dispose $n) ", " (Lab $O-Reset $n) ", "
            (Lab $O-Restart $n) ", " (Lab $O-React $n $i) ", 代表外部输入 (插入, 移除, 重置, 重启, 事件反应);")
        (Li (B "生命周期标签") ": 对名字" $n ", " (app $L $n) "是以下八个标签的集合: "
            (Lab $L-Begin $n) ", " (Lab $L-Divert $n) ", " (Lab $L-Step $n) ", "
            (Lab $L-Leave $n) ", " (Lab $L-Undo $n) ", " (Lab $L-RetireChild $n) ", "
            (Lab $L-Unloaded $n) ", " (Lab $L-Remove $n) "."))
    "记" $State "为状态的集合, " $Label "为标签的集合. 下面的规则定义一个函数"
    (MB (&: $step (&-> (@c* $State $Label) (&Maybe $State))) ",")
    "每条规则给出一个标签的后继; 规则的前提不满足时" (&= (appl $step $gamma $ell) $None)
    ". 若" (&= (appl $step $gamma $ell) $gamma^) ", 记作" (Step $gamma $ell $gamma^)
    "; 称" $ell "在" $gamma (B "可用") ", 如果" (&!= (appl $step $gamma $ell) $None)
    ". 编排标签的规则 (名字以O开头) 称为" (B "编排规则") ", 生命周期标签的规则 (名字以L开头) 称为"
    (B "生命周期规则") "; 按生命周期规则走的一步称为" (B "生命周期步") ".")
   ((Definition #:id "def-reach")
    "一次" (B "执行") "是一串步" (: (Step $gamma_0 (_ $ell $1) $gamma_1) (^^ $--> (_ $ell $2)) $..c (^^ $--> (_ $ell $j)) (_ $gamma $j))
    ", 记作" (Steps $gamma_0 (_ $gamma $j)) ". 我们称" $gamma (B "可达") ", 如果存在执行"
    (Steps $gamma_init $gamma) ". 称" $gamma (B "静止") ", 记作" (&quiet $gamma) ", 如果没有生命周期标签在"
    $gamma "可用:"
    (MB (&:<=> (&quiet $gamma)
               (∀ $n (∀ (∈ $ell (app $L $n)) (&= (appl $step $gamma $ell) $None)))) ".")
    "一串编排标签称为" (B "编排输入") ". 编排输入以插入次序而不是名字指称根fiber (插入得到的名字依赖于调度): "
    "第" $i "个" $O-Insert "插入的根fiber称为第" $i "个根fiber. 只由" $O-Insert
    "以及对根fiber的" $O-Dispose ", " $O-Restart ", " $O-Reset "组成的编排输入称为" (B "根编排输入")
    "; 一次执行" (B "接受") "一个编排输入, 如果它的编排步依次就是这个输入, 生命周期步可以任意穿插在其间. "
    "我们称" $gamma (B "根可达") ", 如果存在从" $gamma_init "出发, 接受某个根编排输入的执行"
    (Steps $gamma_init $gamma) ". 称" $gamma (B "干净") ", 如果它静止, 并且没有失败的fiber (失败见"
    (Ref "def-fiber-notation") ").")
   (P "选择哪个标签是调度的事, 不在演算之内. "
      "凡是从" $next "取名字, 编号或令牌的规则, 都随后把" $next "加一. "
      "规则中" (&<- $x $v) "表示把状态的这一部分改为" $v ", 其余部分不变; "
      (&:= $x $e) "则只是给" $e "起名字" $x ", 比如" (&:= $m $next) "表示新fiber的名字取" $next
      "的当前值. 全文中" $:= "都表示定义, " $<- "只用于规则和影子存储 ("
      (Ref "def-shadow") ") 对状态的更新. "
      "生命周期规则由演算自己驱动, 目的是让每个fiber追上它的" $target ".")
   (H3. "编排规则")
   (Ul (RuleLi (Lab $O-Insert $c)
               (list "组件" $c "不冲突 (" (Ref "def-derived") ")")
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
                  (app $reacts c_n) "的第" $i "项为" $c ", 且" $c "不冲突")
               "插入子fiber " (tupa0 $c $n $false (Inactive $bottom)) ", 名为"
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
               (list (&= θ_n (Loading $P $acc $omega)) ", 并且" $P "为" $Done "时还要求"
                  (&= (&target $n) $omega))
               "按" $P "分情况:"
               (Ul (Li $Done ": " (&<- θ_n (Active $acc $omega)) ";")
                   (Li $Fail ": " (&<- θ_n (Unloading $acc $omega $EUser)) ";")
                   (Li (Con $Call $k $a $kappa) ": 依次"
                       (Ul (Li (B "解析") $k ": 若" (∈ $k (app $d c_n)) ", 取"
                               (&:= $iota (app $omega $k)) "; 否则要求" (&= (app $B $k) (tupa0 $iota $n $_))
                               " (即" $k "由" $n "自己提供), 取这个" $iota ". 解析失败, 或"
                               (app $B $k) "不是编号为" $iota "的绑定时, 进入Unloading并带错误" $EUnresolved ";")
                           (Li (B "运行") ": 设" (&= (app $B $k) (tupa0 $iota $q $s)) ", 取令牌"
                               (&:= $t $next) ", 运行" (&run $a $t $s) ". 运行失败时进入Unloading并带错误" $EOp ";")
                           (Li (B "写回") ": 运行得到" (tu0 $s^ $g $o) "时, 令"
                               (&<- (app $B $k) (tupa0 $iota $q $s^)) ", 并令"
                               (&<- θ_n (Loading (app $kappa $o) (Cons (appl $UOp $k $iota $t $g) $acc) $omega))
                               ";")))
                   (Li (Con $Provide $k $s $P^) ": 要求" (∈ $k (app $p c_n))
                       "且" (app $B $k) "未定义; 以" (&:= $iota $next) "为编号令"
                       (&<- (app $B $k) (tupa0 $iota $n $s)) ", "
                       (&<- θ_n (Loading $P^ (Cons (appl $UProv $k $iota) $acc) $omega))
                       ". 要求不满足时进入Unloading并带错误" $EConflict ";")
                   (Li (Con $Spawn $c $P^) ": 要求" $c
                       "不冲突; 插入子fiber " (tupa0 $c $n $false (Inactive $bottom))
                       ", 名为" (&:= $m $next) ", 并令"
                       (&<- θ_n (Loading $P^ (Cons (appl $USpawn $m) $acc) $omega))
                       ". 冲突时进入Unloading并带错误" $EConflict "."))))
   (P "因此Loading的fiber不会卡住: " $L-Step "除收尾外总是可用的, 收尾时若"
      (&!= (&target $n) $omega) ", 则" $L-Divert "可用. " $target
      "偏离视图之后, 除" $Done "外的各步照样可走, 这是在途迭代的" (Q "落地")
      ": 已经开始的一步不因" $target "改变而作废, 它给出的逆照样进入累加器. "
      "Cordis正是这样: 异步迭代器正在" (Code "await") "的那一步返回后, 它的逆照样收下, "
      "下一步之前才检查是否放弃; 同步迭代器则一口气跑完. 何时放弃由调度决定, "
      "所以规则允许落地任意多步, " $L-Divert "随时可用; 只有收尾要求" $target
      "仍等于视图, 这与Cordis在加载结束时的做法一致: " $target "已经偏离时直接卸载, 不进入Active.")
   (P "不过, 这一对应以操作的逻辑原子性为前提. 演算中一个操作是一步, 而真实Cordis的启动函数可以在操作中途"
      (Code "await") ": 比如操作先写" $x ", 等待之后再写" $y ", 其他组件就能在中间看到只改了一半的状态, "
      "这不是演算的任何一次执行. 要让异步实现符合演算, 实现方须满足下面几条, 演算本身不检查它们:")
   (Ul (Li "操作和它给出的逆各自的行为可以解释为单个原子步, 例如对共享状态的修改集中在一个同步片段中;")
       (Li "操作失败时不留下未登记的修改;")
       (Li "在途操作提交后, 运行时先登记它的逆, 再完成卸载."))
   (P "对良构状态, 解析得到的编号总对应一个现存的绑定 (" (Ref "thm-outlive")
      "; 自己提供的键由解析的定义即得), 所以" $Call "情形中" (Q (app $B $k) "不是编号为" $iota "的绑定")
      "不会发生. 键" $k "同时在" (app $d c_n) "和" (app $p c_n) "中时以依赖为准, 查视图. "
      "这时" $n "提供" $k ", " $k "已经由它依赖的提供者绑定, 必然冲突; 并且组件" c_n "在自己之上 ("
      (&prec c_n c_n) ", " $prec "见" (Ref "def-prec") "), 从而没有秩 (" (Ref "def-rank") "), 违反" (Ref "def-obligation") "中的有秩. 因此在满足义务的组件表中, 每个组件"
      $c "都有" (&= (&cap (app $d $c) (app $p $c)) $empty) ".")
   (H3. "卸载")
   (Ul (RuleLi (Lab $L-Leave $n)
               (list (&= θ_n (Active $acc $omega)) "且" (&!= (&target $n) $omega))
               (&<- θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $L-Undo $n)
               (list (&= θ_n (Unloading (Cons $u $acc) $omega $zeta)) ", "
                     (&neg (&held $n (Cons $u $acc))) ", 且满足下面按" $u "分情况给出的前提")
               (&<- θ_n (Unloading $acc $omega $zeta)) ", 并且:"
               (Ul (Li (appl $UOp $k $iota $t $g) ": 若" (&= (app $B $k) (tupa0 $iota $q $s)) ", 则令"
                       (&<- (app $B $k) (tupa0 $iota $q (app $g $s))) "; 否则" $B "不变;")
                   (Li (appl $UProv $k $iota) ": 删除" (app $B $k)
                       " (由守卫, 这时没有" (&!= $m $n) "满足" (&resolves $m $iota) ");")
                   (Li (appl $USpawn $m) ": 要求" (&!in $m (&dom $F))
                       ", 即子fiber已被移除.")))
       (RuleLi (Lab $L-RetireChild $n)
               (list (&= θ_n (Unloading (Cons (appl $USpawn $m) $acc) $_ $_)) ", "
                  (&neg (&held $n (Cons (appl $USpawn $m) $acc))) ", "
                  (∈ $m (&dom $F)) "且" (&neg τ_m))
               (&<- τ_m $true) ".")
       (RuleLi (Lab $L-Unloaded $n) (list (&= θ_n (Unloading $nil $omega $zeta)))
               (&<- θ_n (Inactive $zeta)) ".")
       (RuleLi (Lab $L-Remove $n)
               (list τ_n ", " (&= θ_n (Inactive $_)) ", 且没有" $m "满足" (&= π_m $n))
               "删除" (app $F $n) "."))
   (P (&neg (&held $n $acc)) "是卸载的" (B "守卫") ", 它作用于整个卸载, 而不只是"
      $UProv "这一项: 只要" $n "尚未撤回的某个提供仍被别的已安装fiber解析, " $n
      "就既不撤销任何一项, 也不请子fiber退出. 所以依赖者卸载时, 提供者的全部效应 "
      "(不只是提供本身, 还有提供之后登记的操作和spawn的子fiber) 都还在. "
      "这与Cordis一致: 卸载先等全部提供的依赖者退出, 再启动任何disposer. "
      "于是父fiber按后进先出 (LIFO) 撤销到" (appl $USpawn $m) "时, 先请求子fiber退出, "
      "然后等它卸载并被移除, 再继续撤销更早的项; 在spawn之后登记的效应此时已经撤销完毕. "
      "等依赖者离开和等子fiber被移除, 是演算中仅有的两处等待.")
   (H3. "设计决定")
   (P "主要的设计决定如下, 与论文的差别见" (Ref "sec-compare") ".")
   (Ul (Li (B "移除") ": 只有一个外部请求" $O-Dispose ", 条目在"
           (Q "已请求移除, 已Inactive, 没有子fiber") "时由" $L-Remove
           "自动删除. 外部不必等待卸载完成再发第二个请求, 条目也不会在子fiber还在时被删掉.")
       (Li (B "卸载逐项进行") ": 累加器逐项撤销, 这样" $USpawn "才能等待子fiber.")
       (Li (B "守卫作用于整个卸载") ": 只在" $UProv "这一项上设守卫也能保证提供本身比依赖者活得久, "
           "但提供者会在依赖者离开之前先撤掉提供之后登记的效应, 依赖者卸载时看到的是被拆了一部分的服务. "
           "Cordis先等全部依赖者再启动disposer, 本文照此处理.")
       (Li (B "LIFO串行") ": Cordis用" (Code "Promise.all") "并发执行disposer. "
           "后登记的效应往往依赖先登记的, LIFO不需要任何额外假设就是对的.")
       (Li (B "放弃与落地") ": 每个服务操作是原子步. " $target "偏离视图后, 步与步之间都可以"
           $L-Divert ", 也可以先让剩下的迭代落地, 只有收尾的" $Done "要求" $target
           "等于视图. 论文的" $L-Divert "有放弃在途迭代和让它落地两支, 本文两支都保留, 落地由"
           $L-Step "承担; 论文的异步宿主只落地一次迭代, 这里允许任意多步, Cordis的异步和同步迭代器都在其中.")
       (Li (B "失败之后") ": 失败的fiber停在" (Inactive $xi) ", 由外部用" $O-Reset "决定是否重试.")
       (Li (B "运行期效应") ": 不把累加器改成树, 而是用" $O-React
           "把反应编码为子fiber. 子fiber本身就是一棵子效应树.")
       (Li (B "隔离与可用性检查") ": 不需要新规则. 键是服务名加作用域标记, "
           "隔离就是对组件表做键重命名, 见" (Ref "sec-meta-derived") "; "
           "在提供时求值的检查是程序自己就能做的判断."))

   (H2. "元理论" #:id "sec-meta")
   (P "这一章陈述演算的性质: 良构性被保持, 依赖顺序被遵守, 卸载精确地撤销效应, "
      "系统不会死锁且会终止, 以及终态与执行顺序无关. 最后一节把它们汇总为一个定理.")
   (H3. "保持性")
   ((Definition #:id "def-wf")
    "我们称状态" $gamma (B "良构") ", 记作" (app $WF $gamma) ", 如果以下各条成立:"
    (Ol (Li (B "新鲜") ": " (&dom $F) "中的名字, " $B "中的提供编号, 以及各累加器的"
            $USpawn "中的名字都小于" $next ";")
        (Li (B "子fiber有父") ": 若" (∈ $m (&dom $F)) "且" (&!= π_m $root) ", 则" (∈ π_m (&dom $F))
            ", " (&installed π_m) ", 并且" (appl $USpawn $m) "在" (_ $acc π_m) "中;")
        (Li (B "提供不相交") ": 对" (∈ (&!= $m $n) (&dom $F)) ", " (&= (&cap (app $p (_ $c $m)) (app $p c_n)) $empty) ";")
        (Li (B "解析有绑定") ": 若" (&installed $m) "且" (&= (app (_ $omega $m) $k) $iota) ", 则"
            (&= (app $B $k) (tupa0 $iota $q $_)) ", 其中" (&!= $q $m) ";")
        (Li (B "视图完整") ": 若" (&installed $m) ", 则" (&= (&dom (_ $omega $m)) (app $d (_ $c $m))) ";")
        (Li (B "绑定有提供者") ": 若" (&= (app $B $k) (tupa0 $iota $q $_)) ", 则" (∈ $q (&dom $F)) ", "
            (&installed $q) ", " (∈ $k (app $p (_ $c $q))) ", 并且" (appl $UProv $k $iota) "在" (_ $acc $q) "中;")
        (Li (B "提供项有绑定") ": 若" (appl $UProv $k $iota) "在" (_ $acc $n) "中, 则"
            (&= (app $B $k) (tupa0 $iota $n $_)) ";")
        (Li (B "提供项唯一") ": 一个累加器中, 每个键至多出现在一个" $UProv "项中.")))
   ((Theorem #:id "thm-preservation")
    (app $WF $gamma_init) "成立; 若" (app $WF $gamma) "且" (Step $gamma $ell $gamma^) ", 则"
    (app $WF $gamma^) ". 所以每个可达状态都良构.")
   ((proof)
    "对规则逐条检查. 有意思的情形如下."
    (Ul (Li $O-Insert ", " $O-React "和" $Spawn "步都要求新组件不冲突, 所以" (Q "提供不相交") "保持.")
        (Li $Provide "步要求" (∈ $k (app $p c_n)) "且" (app $B $k)
            "未定义, 并同时在累加器里压入" $UProv ", 所以" (Q "绑定有提供者") ", " (Q "提供项有绑定") "和" (Q "提供项唯一") "保持.")
        (Li "撤销" $UProv "时守卫保证没有其他已安装fiber解析到这个编号, 所以删除绑定不破坏" (Q "解析有绑定") ".")
        (Li $L-Remove "要求没有子fiber, 所以" (Q "子fiber有父") "保持.")
        (Li $L-Begin "取的视图就是" $target ", 它的定义域恰为" (app $d c_n)
            ", 并且只解析到Active的提供者, 所以" (Q "解析有绑定") "和" (Q "视图完整") "保持.")))
   ((Corollary #:id "cor-wf")
    "设" (app $WF $gamma) ". 有子fiber或绑定的fiber一定已安装; 一个fiber卸载完毕 (处于Inactive) 时, "
    "它的子fiber和它提供的绑定都已经不存在了.")
   (H3. "顺序")
   ((Theorem #:id "thm-outlive")
    (B "提供者比依赖者活得久") ": 设" (app $WF $gamma) ". 若" (&installed $m) "且"
    (&= (app (_ $omega $m) $k) $iota) ", 则有" (&!= $q $m) "使" (&= (app $B $k) (tupa0 $iota $q $_))
    "且" (&installed $q) ".")
   ((proof)
    "由" (Ref "def-wf") "中的" (Q "解析有绑定") ", " (&= (app $B $k) (tupa0 $iota $q $_)) ", 其中" (&!= $q $m)
    "; 由" (Q "绑定有提供者") ", " (&installed $q) ".")
   ((Theorem #:id "thm-ordering")
    "设" $gamma "是状态, " (∈ $n (&dom $F)) "."
    (Ol (Li "若" (Lab $L-Begin $n) "在" $gamma "可用, 记" (&:= $omega (&target $n)) ", 则" (&!= $omega $bottom)
            ", 并且对" (&dom $omega) "中的每个" $k ", " (&= (app $B $k) (tupa0 (app $omega $k) $q $_))
            ", 其中" (&= (_ $theta $q) (Active $_ $_)) ". 走这一步到达" $gamma^ "之后"
            (&= θ^_n (Loading (app $body c_n) $nil $omega))
            ". 即" $n "的全部依赖都由Active的提供者绑定, " $n "针对恰好这些提供开始加载.")
        (Li "设" (&= θ_n (Unloading $acc $omega $zeta)) ", " (appl $UProv $k $iota) "在" $acc
            "中, 且有已安装的" (&!= $m $n) "满足" (&resolves $m $iota) ". 则" (Lab $L-Undo $n) "和"
            (Lab $L-RetireChild $n) "在" $gamma "都不可用. 即只要依赖者还在, 提供者就不撤销任何效应: "
            "提供者的全部效应比依赖者活得久. 特别地, 设"
            (&= θ_n (Unloading (Cons (appl $UProv $k $iota) $acc) $omega $zeta)) ", 则"
            (Lab $L-Undo $n) "可用, 当且仅当" (&neg (&held $n (Cons (appl $UProv $k $iota) $acc))) ".")
        (Li "设" (&= θ_n (Unloading (Cons (appl $USpawn $m) $acc) $omega $zeta)) ". 则"
            (Lab $L-Undo $n) "在" $gamma "可用, 当且仅当" (&neg (&held $n (Cons (appl $USpawn $m) $acc)))
            "且" (&!in $m (&dom $F)) ".")
        (Li (Lab $L-Step $n) "在" $gamma "可用, 当且仅当" (&= θ_n (Loading $P $acc $omega))
            ", 并且" $P "为" $Done "时" (&= (&target $n) $omega) ". 每一步都按视图" $omega
            "执行, 不论" $target "此后变成什么; 只有收尾要求视图仍等于" $target ".")
        (Li (Lab $L-Divert $n) "或" (Lab $L-Leave $n) "在" $gamma "可用, 当且仅当" $n
            "处于Loading或Active, 且" (&!= (&target $n) (_ $omega $n))
            ". 即fiber只在" $target "偏离视图时放弃加载或离开; 由第4条, 这时它不能收尾, 剩下的迭代仍可落地.")))
   ((proof)
    "第1条: " $L-Begin "的前提是" (&= (&target $n) $omega) ", 而由" $target "和" $visible
    "的定义 (" (Ref "def-derived") "), " $omega "的值都是Active的提供者的编号. 第2条: 守卫是" $L-Undo "和" $L-RetireChild
    "共同的前提, 而" (&held $n $acc) "由" (appl $UProv $k $iota) "和" $m "成立; " (appl $UProv $k $iota)
    "情形除守卫外没有别的前提. 第3条: 除守卫外, " (&!in $m (&dom $F)) "是" (appl $USpawn $m) "情形的唯一前提. 第4, 5条: 它们就是"
    $L-Step ", " $L-Divert "和" $L-Leave "的前提.")
   ((Theorem #:id "thm-moves")
    (B "值只被声明或提供该键的fiber改变") ": 设" $gamma "可达, " (Step $gamma $ell $gamma^) ", "
    (&= (app $B $k) (tupa0 $iota $q $s)) ", " (&= (app $B^ $k) (tupa0 $iota $q^ $s^)) ". 若"
    (&!= (tupa0 $iota $q^ $s^) (tupa0 $iota $q $s)) ", 则" $ell "是某个fiber " $n "的"
    (Lab $L-Step $n) "或" (Lab $L-Undo $n) ", 并且" (∈ $k (&union (app $d c_n) (app $p c_n))) ".")
   ((proof)
    "编号为" $iota "的绑定只由" $Provide "步建立, 只由撤销" (appl $UProv $k $iota)
    "删除, 所以相邻两步之间编号不变而内容改变的, 只有执行一个操作的" $L-Step
    "和撤销一项" (appl $UOp $k $iota $t $g) "的" $L-Undo ". 前者: " $body
    "的类型保证操作的键在" (&union (app $d c_n) (app $p c_n)) "中 (" (Ref "sec-programs")
    "). 后者: 累加器中的" $UOp "项只由" $L-Step "压入, 压入时它的键就是这次操作的键, "
    "其余规则只弹出累加器的项或不改动它, 新fiber的累加器为空; 所以对执行归纳, "
    "每个" (appl $UOp $k $_ $_ $_) "都在一个满足" (∈ $k (&union (app $d c_n) (app $p c_n)))
    "的fiber " $n "的累加器中.")
   (P "顺序定理不断言依赖者读到的值不变: 服务状态按设计是共享且可变的, "
      "固定的是提供的身份. 能改变它的只有声明或提供该键的fiber (" (Ref "thm-moves") ").")
   (H3. "恢复精确性" #:id "sec-recovery")
   ((Definition #:id "def-revert-indep")
    "我们称" (tu0 $a $t_a) "对" (tu0 $b $t_b) "撤销独立, 记作" (appl $RI $a $t_a $b $t_b)
    ", 如果对" $a "可能给出的每个逆" $g
    (Ol (Li (B "不在乎撤销") ": 对每个" $x ", " (&run $b $t_b $x) "与" (&run $b $t_b (app $g $x))
            "都是" $None "; 或者"
            (&= (&run $b $t_b $x) (tu0 $x_1 $h_1 $o)) ", "
            (&= (&run $b $t_b (app $g $x)) (tu0 $x_2 $h_2 $o))
            ", 其中" (&simeq $x_2 (app $g $x_1)) "且"
            $h_2 "与" $h_1 "逐点相关;")
        (Li (B "逆可交换") ": 对" $b "可能给出的每个逆" $h ", "
            (&compose $g $h) "与" (&compose $h $g) "逐点相关."))
    "前者说" $b "不在乎" $a "的效应是否已被撤销, 后者说两者的逆可以交换.")
   (P "撤销独立正是" (Q "撤销" $a "的效应而让" $b "的效应留着")
      "所需要的条件; 它不要求" $a "和" $b "本身可交换 (先" $a "后" $b "与先" $b "后" $a "结果相同). 下面的引理在单个绑定上说明这一点.")
   ((Lemma #:id "thm-omission")
    "设" $a "在令牌" $t "下带见证 (" (Ref "def-wb") "中的" (Q "带见证") "), 并且"
    (&= (&run $a $t $s_0) (tu0 $s_1 $g $o)) ". 设" $rho
    "是一串步骤, 每一步或者是以令牌" $t_b "运行操作" $b ", 或者是施加一个映射" $h
    ", 并且每一步都与" $g (B "相容") ":"
    (Ul (Li "操作" (tu0 $b $t_b) "尊重" $simeq " (" (Ref "def-wb") "中的" (Q "尊重") "), 并满足"
            (Ref "def-revert-indep") "中关于" $g "的" (Q "不在乎撤销") ";")
        (Li "映射" $h "尊重" $simeq ", 并满足" (Ref "def-revert-indep") "中关于" $g "的" (Q "逆可交换") "."))
    "记" (&exec $rho $s) "为从" $s "出发依次执行" $rho "所到达的状态 (某个操作失败时无定义). 那么"
    (&exec $rho $s_1) "与" (&exec $rho $s_0) "同时有定义或同时无定义, 有定义时"
    (MB (&simeq (app $g (&exec $rho $s_1)) (&exec $rho $s_0)) ",")
    "并且" $rho "中的每个操作在两次执行中得到相同的结果. 换言之, 在终点撤销" $a
    ", 等于" $a "从来没有执行过.")
   ((proof)
    "记从" $s_1 "出发的执行的状态为" $x ", 从" $s_0 "出发的执行的状态为" $y
    ", 对步骤归纳证明" (&simeq (app $g $x) $y) ", 并且两边的操作看到相同的结果."
    (Ul (Li "起点: " (&simeq (app $g $s_1) $s_0) "是" $a "的见证条件.")
        (Li "操作" $b ": 由相容性, " $b "在" $x "和" (app $g $x)
            "上同时失败或同时成功, 后者的后继" $simeq "前者的后继再施" $g
            ", 结果相同; 再由" $b "尊重" $simeq ", 把" (app $g $x) "换成" $y "也一样.")
        (Li "映射" $h ": "
            (&simeq (app $g (app $h $x)) (app $h (app $g $x)) (app $h $y))
            ", 第一步是相容性, 第二步是" $h "尊重" $simeq ".")))
   ((Definition #:id "def-store")
    "设" $gamma "是状态. " $gamma "的" (B "存储")
    "是部分映射" (&: (&store $gamma) (&p-> $K $St)) ": 若" (&= (app $B $k) (tupa0 $_ $_ $s))
    ", 则" (&:= (app (&store $gamma) $k) $s) "; " (app $B $k) "无定义时" (app (&store $gamma) $k)
    "也无定义. 我们称名字" $n "在" $gamma "中有" (B "待撤销操作") ", 如果" (&installed $n)
    "并且" (_ $acc $n) "中有" $UOp "项. 所以" $n "不存在或处于Inactive时, 它没有待撤销操作.")
   ((Definition #:id "def-shadow")
    "设" (Steps $gamma_0 $gamma) "是一次执行, " $n "是一个名字. 沿这次执行对" $n
    "定义" (B "影子存储") " " (&: $shadow (&p-> $K $St)) ": 起点处" (&<- $shadow (&store $gamma_0))
    ", 此后每一步按它对绑定所做的事更新:"
    (Ul (Li "某个fiber " $m "的" $L-Step "在键" $k "上以令牌" $t "成功运行了" $a ": 若"
            (&!= $m $n) ", " (app $shadow $k) "有定义, 并且" (&= (&run $a $t (app $shadow $k)) (tu0 $s^ $_ $_))
            ", 则令" (&<- (app $shadow $k) $s^) "; 否则" $shadow "不变;")
        (Li "某个fiber " $m "的" $L-Step "在键" $k "上运行" $a "失败: " $shadow "不变;")
        (Li "某个fiber " $m "撤销" (appl $UOp $k $iota $t $g) ", 且" (app $B $k) "的编号是" $iota ": 若"
            (&= $m $n) ", " $shadow "不变; 否则把" (app $shadow $k) "换成"
            (app $g (app $shadow $k)) ";")
        (Li $Provide "步以初始状态" $s "建立键" $k "的绑定: 令" (&<- (app $shadow $k) $s)
            "; 撤销" (appl $UProv $k $iota) "删除绑定: 令" (app $shadow $k) "无定义. "
            "这两条对所有fiber都照常进行, 包括" $n ";")
        (Li "其余的步不改动" $shadow "."))
    "所以影子跳过的只是" $n "自己的服务操作和它们的撤销, 不是" $n
    "的存在: " $n "的提供照样建立和删除, 其他fiber照样解析到它. 定义中" (Q "实际成功而影子上失败")
    "时影子不变, 只是为了让定义完整; 在" (Ref "thm-recovery") "的假设下这种情形不会出现 (见那里的证明).")
   ((Definition #:id "def-meet")
    "对程序" $P "和键" $k ", 记" (&ops $k $P) "为在" $P "的某条路径上以"
    (Con $Call $k $a $kappa) "的形式出现的" $a "之集. "
    "我们称操作" $a "和" $b "在键" $k "上" (B "可能相遇") ", 记作" (&meet $k $a $b) ", 如果"
    (MB (&:<=> (&meet $k $a $b)
               (∃ (&cm $c $c^)
                  (&conj (∈ $a (&ops $k (app $body $c)))
                         (∈ $b (&ops $k (app $body $c^)))
                         (@=> (&= $c $c^) (&= (app $p $c) $empty))))) ".")
    "这个关系依赖于组件表" $prog ".")
   (P "定义中的蕴涵是说: 同一组件的两个操作, 只在该组件不提供任何键时才算可能相遇. "
      "提供键的组件同时至多有一个实例 (" (Ref "def-wf") "中的" (Q "提供不相交") "), "
      "它的两个操作来自同一个fiber, 按LIFO撤销, 不需要假设.")
   ((Definition #:id "def-H")
    (B "假设(H)") "是: 可能相遇的操作对在不同令牌下撤销独立,"
    (MB (&=> (&conj (&meet $k $a $b) (&!= $t_a $t_b))
             (appl $RI $a $t_a $b $t_b)) "."))
   ((Definition #:id "def-prov")
    "我们称状态" $gamma "满足" (B "来源不变式") ", 如果"
    (Ol (Li (B "程序来源") ": 每个处于" (Loading $P $_ $_) "的fiber " $m "的剩余程序" $P
            "是" (app $body (_ $c $m)) "的后段, 即从" (app $body (_ $c $m))
            "沿某条路径走若干步得到;")
        (Li (B "逆的来源") ": 每个fiber " $m "的累加器中的每个" (appl $UOp $k $iota $t $g) "都满足"
            (&< (&cm $iota $t) $next) ", 并且存在" (∈ $a (&ops $k (app $body (_ $c $m))))
            "和状态" $s ", 使" (&= (&run $a $t $s) (tu0 $_ $g $_)) ", 即" $g "是" (tu0 $a $t) "给出的逆;")
        (Li (B "令牌互异") ": 不同fiber的" $UOp "项所用的令牌互不相同."))
    "初始状态满足来源不变式. 若" $gamma "良构且满足来源不变式, 则它经一步到达的状态也满足: " $L-Step
    "只把剩余程序换成它的后段; 令牌取自" $next ", 每取一次" $next "加一; " (Q "逆的来源") "中的" (&< $iota $next)
    "用到了良构性 (" (Ref "def-wf") "中的" (Q "新鲜") "). 由" (Ref "thm-preservation") ", 每个可达状态都满足来源不变式.")
   ((Theorem #:id "thm-recovery")
    "设所有服务操作良好, 且(H)成立 (" (Ref "def-H") "). 设" $gamma_0 "良构 (" (Ref "def-wf")
    ") 并满足来源不变式 (" (Ref "def-prov") "), 名字" $n "在" $gamma_0 "中没有待撤销操作 ("
    (Ref "def-store") "), " (Steps $gamma_0 $gamma) "是一次执行, " $shadow
    "是沿这次执行对" $n "得到的影子存储 (" (Ref "def-shadow") "), 并且" $n "在" $gamma
    "中也没有待撤销操作 (比如卸载完毕). 那么对每个键" $k ", "
    (app (&store $gamma) $k) "与" (app $shadow $k) "同时无定义, 或者"
    (MB (&simeq (app (&store $gamma) $k) (app $shadow $k)) ".")
    "换言之, 卸载" $n "的结果, 就是同一次执行中略去" $n
    "全部操作所得的结果, 不论其他fiber同时做了什么.")
   ((proof)
    "对执行中的状态" $gamma^ "和键" $k ", 若" (&= (app $B^ $k) (tupa0 $iota $_ $_)) ", 记"
    (_^ $pend $k $prime) "为" acc^_n "中所有" (appl $UOp $k $iota $_ $g)
    "项的" $g "组成的列表, 最近的在前, 并记" (app (_^ $pend $k $prime) $s)
    "为把它们依次 (最近的先) 作用于" $s "的结果; " $n "未安装时" (_^ $pend $k $prime) "是空列表. "
    "撇号表示它相对于" $gamma^ " (" (Ref "def-fiber-notation") "); 同样, 把执行到达" $gamma^
    "时的影子存储记作" (^ $shadow $prime) ". 沿执行证明" (B "主不变式") ": 对执行中的每个状态"
    $gamma^ "和每个键" $k ", "
    (app $B^ $k) "与" (app (^ $shadow $prime) $k)
    "同时无定义, 或者" (&= (app $B^ $k) (tupa0 $_ $_ $s)) "且"
    (&simeq (app (_^ $pend $k $prime) $s) (app (^ $shadow $prime) $k)) ". "
    "沿执行的每个状态都良构且满足来源不变式 (" (Ref "thm-preservation") ", " (Ref "def-prov")
    "). 来源不变式的作用是在下面的情形中让(H)适用: " $n "的" $UOp "项中的" $g "是"
    (tu0 $a $t_a) "给出的逆, 其中" (∈ $a (&ops $k (app $body c_n))) "; 若另一个fiber " $m
    "以令牌" $t_b "在" $k "上运行" $b ", 则" $b "出现在" $m "的剩余程序中, 由来源不变式中的" (Q "程序来源") ", "
    (∈ $b (&ops $k (app $body (_ $c $m)))) "; 若" $m "撤销它的一项" (appl $UOp $k $_ $t_b $h)
    ", 则由来源不变式中的" (Q "逆的来源") ", " $h "是某个" (∈ $b (&ops $k (app $body (_ $c $m))))
    "给出的逆. 两种情形都有" (&meet $k $a $b) ", 并且由来源不变式中的" (Q "令牌互异") "和令牌的新鲜性, "
    (&!= $t_a $t_b) ". "
    "取" (&= $gamma^ $gamma_0) ", 这时" $n "没有待撤销操作, 每个" (_^ $pend $k $prime) "都是空列表, 主不变式就是"
    (&= (^ $shadow $prime) (&store $gamma_0)) ". 下面检查执行中的一步, 其中" (_^ $pend $k $prime) "和" (^ $shadow $prime) "取这一步之前的值. 逐类检查步骤:"
    (Ul (Li $n "自己的操作: 把状态从" $s "改为" $s^ ", 并压入逆" $g
            ". 因为" (&simeq (app $g $s^) $s) ", 先施新压入的逆就回到原来的情形, 而影子不动.")
        (Li $n "撤销一项: 实际状态施加了这个逆, 待撤销的列表少了这一项, 两边的作用抵消.")
        (Li "其他fiber在同一绑定上以令牌" $t_b "执行操作" $b ": 实际状态" $s
            "变为" $b "在" $s "上的后继, 影子" (&simeq (app (^ $shadow $prime) $k) (app (_^ $pend $k $prime) $s))
            "也执行" $b ". 需要比较" $b "在" $s "上和在" (app (_^ $pend $k $prime) $s)
            "上的运行. 对" (_^ $pend $k $prime) "中的每个" $g ", 由上面的讨论和(H), 撤销独立中的" (Q "不在乎撤销") "成立; "
            "对列表的长度归纳, 它对整个复合" (_^ $pend $k $prime) "也成立: 每用一次, " $b
            "两边给出的结果相同, 给出的逆逐点" $simeq ", 所以可以接着用下一个. 于是"
            $b "在" $s "上和在" (app (_^ $pend $k $prime) $s) "上同时失败或同时成功, 成功时若" $b "在" $s
            "上的后继为" $s^ ", 则它在" (app (_^ $pend $k $prime) $s) "上的后继" $simeq (app (_^ $pend $k $prime) $s^)
            "; 再由" $b "尊重" $simeq ", 把" (app (_^ $pend $k $prime) $s) "换成影子也一样. "
            "失败时实际的fiber进入Unloading, 绑定不变, 影子也不变 (" (Ref "def-shadow")
            "); 成功时主不变式在新状态上成立.")
        (Li "其他fiber的撤销项" $h ": 两边都施加" $h ". 对" (_^ $pend $k $prime) "中的每个" $g
            ", 由上面的讨论和(H), 撤销独立中的" (Q "逆可交换") "给出"
            (∀ $y (&simeq (app $g (app $h $y)) (app $h (app $g $y))))
            "; 同样对列表长度归纳, 得到"
            (&simeq (app (_^ $pend $k $prime) (app $h $s)) (app $h (app (_^ $pend $k $prime) $s)))
            ", 再由" $h "尊重" $simeq "得到主不变式. 这里和上一种情形都用到了: " (_^ $pend $k $prime)
            "中的每个逆都尊重" $simeq ", 依据是来源不变式中的" (Q "逆的来源") "和" (Ref "def-wb") "中的" (Q "逆尊重") ".")
        (Li "绑定的创建与删除: 在两边同时发生; " $n
            "仍有待撤销操作的绑定不会被删除, 这由守卫保证."))
    "最后取" (&= $gamma^ $gamma) ", 这时" $n "没有待撤销操作, 每个" (_^ $pend $k $prime) "都是空列表, 并且" (&= (^ $shadow $prime) $shadow) ", 主不变式说的就是实际存储" $simeq "影子存储.")
   (P "这里比较的是同一次执行, 不是" (Q $n "从未存在") "的另一次执行. 影子中" $n
      "的提供照样存在过, 因它而加载的依赖者和它spawn的子fiber照样运行过, "
      "并且由" (Ref "thm-omission") ", 其他fiber的操作在实际执行和影子中得到相同的结果, "
      "所以它们走的是同一条路径. 若" $n "从未存在, 这些依赖者根本不会加载, 调度也可能完全不同, "
      "本定理对那样的执行不作断言. 依赖者自己的操作并不包括在" $n
      "的操作之中; 在" $n "撤销任何效应之前它们已经卸载, 它们的操作由它们各自的卸载撤销, "
      "对每个依赖者分别应用本定理即可.")
   ((Corollary #:id "cor-runtime")
    "设服务的操作都良好, 且(H)成立. 运行期效应的撤销也是精确的: 在从可达状态出发的执行中, 从效应出现之前到它的子fiber被移除之后, "
    "每个绑定的状态都" $simeq "略去该效应全部操作所得的状态.")
   ((proof)
    "取" $n "为" $O-React "插入的子fiber, " $gamma_0 "为插入之前的状态 (设它可达), " $gamma
    "为它被移除之后的状态. " $n "在" $gamma_0 "和" $gamma "中都不存在, "
    "所以都没有待撤销操作, 由" (Ref "thm-recovery") "即得.")
   (P "注意这里只要求可能相遇的操作对撤销独立, 不要求操作本身可交换 (" (Ref "def-commute") "); "
      "同一fiber内部的操作按LIFO撤销, 不需要任何假设.")
   (H3. "进展与终止")
   ((Definition #:id "def-prec")
    "设" (∈ (&cm $c $c^) $C) ". 我们称" $c (B "可能spawn") " " $c^ ", 如果" $c^ "出现在" (app $body $c)
    "的某条路径上的某个" (Con $Spawn $c^ $_) "中, 或者是" (app $reacts $c) "的某一项. 称" $c^ "在" $c
    (B "之上") ", 记作" (&prec $c $c^) ", 如果"
    (&!= (&cap (app $p $c) (app $d $c^)) $empty) ", 或者" $c "可能spawn " $c^ ".")
   ((Definition #:id "def-rank")
    "设" (∈ $M $NN) ". 我们称" (&: $r (&-> $C $NN)) "是以" $M "为界的" (B "秩") ", 如果"
    (MB (&cm (&=> (&prec $c $c^) (&< (app $r $c) (app $r $c^)))
             (∀ $c (&< (app $r $c) $M))) ".")
    "另外, 对" (∈ $N $NN) ", 我们称组件表" (B "以" $N "为界") ", 如果每个程序的任一路径长度都不超过" $N
    "; 称它" (B "有界") ", 如果它以某个" $N "为界.")
   ((Definition #:id "def-settled")
    "我们称状态" $gamma (B "稳定") ", 记作" (&settled $gamma) ", 如果每个" (∈ $n (&dom $F)) "满足下列之一:"
    (MB (Choice0 ((&conj (&= θ_n (Inactive $xi)) (&neg τ_n)) ", 失败并停下")
                 ((&conj (&= θ_n (Inactive $bottom)) (&neg τ_n) (&= (&target $n) $bottom))
                  ", 不该运行")
                 ((&conj (&= θ_n (Active $_ $omega)) (&= (&target $n) $omega))
                  ", 该运行且视图正确")))
    "特别地, 稳定状态中没有Loading或Unloading的fiber.")
   (P "关系" $prec "说的是谁必须先于谁. 若" (&prec $c $c^) ", 则" $c
      "的fiber在前, " $c^ "的fiber在后: 提供者要先成为Active, 依赖者才能开始加载; 子fiber要由父fiber先spawn出来 "
      "(子fiber开始加载时, 父fiber可以仍处于Loading). 撤销时次序相反. "
      "秩就是组件在这个层级中的高度: 提供者和父组件的秩比依赖者和子组件低, 并且所有组件的秩都小于" $M
      ". 对有限的组件表, 秩存在当且仅当" $prec "没有环: 有环时沿环走一圈秩要严格增加, 不可能; "
      "无环时取" (app $r $c) "为以" $c "结尾的最长" $prec "链的长度, " $M
      "取组件个数即可. 秩在后文有两个用处. 第一, 它严格上升且有界, 所以" $prec
      "没有无限上升链, 于是可以沿" $prec "归纳; 后文" (Q "沿秩归纳")
      "指对" (app $r $c) "从小到大归纳, 先处理提供者和父组件, 再处理依赖者和子组件. "
      "第二, 终止的证明把度量按秩分层, 低秩的层优先 (" (Ref "thm-termination") "). "
      "规则一章中提到, 若" (&!= (&cap (app $d $c) (app $p $c)) $empty) ", 则" (&prec $c $c)
      ", 所以没有秩.")
   ((Definition #:id "def-inv2")
    "我们称状态" $gamma "满足" (B "编号与spawn不变式") ", 如果"
    (Ol (Li (B "编号唯一") ": 不同绑定的提供编号不同, 即若" (&= (app $B $k) (tupa0 $iota $_ $_)) "且"
            (&= (app $B $k^) (tupa0 $iota $_ $_)) ", 则" (&= $k $k^) ";")
        (Li (B "spawn有据") ": 若" (appl $USpawn $m) "在" (_ $acc $n) "中, 并且" (∈ $m (&dom $F)) ", 则"
            c_n "可能spawn " (_ $c $m) " (" (Ref "def-prec") ")."))
    "初始状态满足它, 并且它被每条规则保持: 提供编号取自" $next "; " $USpawn "项只由"
    $Spawn "步和" $O-React "压入, 压入的子组件正是" c_n "可能spawn的. 所以每个可达状态都满足它.")
   ((Theorem #:id "thm-progress")
    "设" $prec "没有无限上升链" (&prec (_ $c $0) (_ $c $1) $..c)
    ". 对每个可达状态" $gamma " (" (Ref "def-reach") "),"
    (MB (&cm (&=> (&quiet $gamma) (&settled $gamma))
             (&disj (&settled $gamma)
                    (∃ (&cm $n (∈ $ell (app $L $n)) $gamma^) (Step $gamma $ell $gamma^)))) ".")
    "即: 没有生命周期规则可用的可达状态一定是稳定的. 反过来, 不稳定的可达状态总有生命周期规则可走.")
   ((proof)
    "可达状态良构 (" (Ref "thm-preservation") "), 并满足编号与spawn不变式 (" (Ref "def-inv2") "). "
    "设没有生命周期规则可用, 但状态不稳定. 先按生命周期状态检查一个不稳定的fiber " $n ":"
    (Ul (Li (Inactive $_) ": 若" τ_n ", 则" $L-Remove "可用 (由" (Ref "cor-wf") ", 未安装的fiber没有子fiber); 否则不稳定意味着"
            (&= θ_n (Inactive $bottom)) "且" (&!= (&target $n) $bottom) ", 于是" $L-Begin "可用.")
        (Li "Loading: 剩余程序不是" $Done "时" $L-Step "可用; 是" $Done "时视" (&target $n) "是否等于视图, " $L-Step "或" $L-Divert "可用.")
        (Li "Active: 不稳定意味着" (&target $n) "不等于视图, 于是" $L-Leave "可用.")
        (Li "Unloading: 累加器为空时" $L-Unloaded "可用; 否则若守卫不挡 (" (&neg (&held $n (_ $acc $n)))
            "), 表头为" $UOp "或" $UProv "时" $L-Undo "可用, 表头为" (appl $USpawn $m) "且" $m "不存在时"
            $L-Undo "可用, " $m "存在但未被请求移除时" $L-RetireChild "可用."))
    "所以只剩下证明静止状态中没有Unloading的fiber. 沿" $prec
    "作良基归纳 (" $prec "没有无限上升链): 对fiber " $n ", 设每个满足" (&prec c_n (_ $c $m)) "的fiber " $m "都不处于Unloading, 证明" $n
    "也不处于Unloading. 若" $n "处于Unloading, 由上面的检查它只可能卡在两处之一:"
    (Ol (Li "守卫挡住: " (_ $acc $n) "中有" (appl $UProv $k $iota) ", 还有已安装的" (&!= $m $n) "解析到" $iota
            ". 由" $resolves "的定义, 有" $k^ "使" (&= (app (_ $omega $m) $k^) $iota) "; 由" (Ref "def-wf") "中的" (Q "解析有绑定") "和" (Q "提供项有绑定") ", " (&= (app $B $k^) (tupa0 $iota $_ $_))
            "且" (&= (app $B $k) (tupa0 $iota $n $_)) "; 由" (Ref "def-inv2") "中的" (Q "编号唯一") ", " (&= $k^ $k) ", 即" $n "提供了" $m "声明的键, 所以" (&prec c_n (_ $c $m)) ". 又" $n
            "已经不是Active, " $iota "不再可见, 于是" (&= (&target $m) $bottom) ". " $m
            "已安装, 若它处于Loading, " $L-Divert "可用; 若处于Active, " $L-Leave
            "可用; 因此它处于Unloading, 与归纳假设矛盾.")
        (Li "撤销" (appl $USpawn $m) "时, 子fiber " $m "还在并且已被请求移除 (否则"
            $L-RetireChild "可用). 由" (Ref "def-inv2") "中的" (Q "spawn有据") ", " (&prec c_n (_ $c $m)) ". 若" $m "未安装, 它没有子fiber, "
            (Lab $L-Remove $m) "可用; 若已安装, 由" (_ $tau $m) "有" (&= (&target $m) $bottom)
            ", 与前一种情形同理, " $m "处于Unloading, 与归纳假设矛盾."))
    "所以静止状态中没有Unloading的fiber; 结合上面的检查, 每个fiber都稳定.")
   ((Theorem #:id "thm-termination")
    "设有以" $M "为界的秩" $r ", 并且组件表以" $N "为界 (" (Ref "def-rank") "). 那么从任何可达状态 (" (Ref "def-reach") ") 出发, 都不存在无限的执行"
    (MB (: (Step $gamma_0 (_ $ell $1) $gamma_1) (^^ $--> (_ $ell $2)) $..c) ",")
    "其中每个" (_ $ell $i) "都是生命周期标签.")
   ((proof)
    (B "思路") ": 每个fiber有一个度量, 它自己走一步时严格下降; 别的fiber走一步不改变它的度量, "
    "只可能让秩更高的fiber的度量增加, 比如新spawn出的子fiber. 所以把度量按秩分层, "
    "低秩的层优先, 按字典序比较, 每一步生命周期步都使它严格下降. "
    "给每个fiber " $n "一个自然数度量" (_ $mu $n) ", 它是" $target
    "不变时" $n "还能走的生命周期步数的上界. 卸载的步数按累加器的实际权重计, 不用常数界定: "
    $O-React "可以在Active的fiber的累加器中留下任意多条" $USpawn ". 常数" $D
    "只需覆盖从Inactive开始加载, 并可能在加载中途失败转入卸载的情形; 这时累加器的长度不超过" $N
    ", 所以这样的" $D "存在. 按生命周期状态:"
    (Ul (Li (Inactive $xi) ": 已请求移除时为1 (还剩" $L-Remove "), 否则为0.")
        (Li (Inactive $bottom) ": " $target "有值时为" $D " (还要整个加载一遍), 否则同上.")
        (Li "Loading, 视图等于" $target ": " (&- $D $1 (: (Mo "|") $acc (Mo "|")))
            ". " $L-Begin "从" $D "降到" (&- $D $1) "; 每个" $L-Step
            "都压入一项或结束加载, 所以严格下降.")
        (Li "Loading, 视图不等于" $target ": "
            (&- (&+ (: $2 $N) $1 $N) (: (Mo "|") $acc (Mo "|")))
            ", 再加上空累加器卸载的度量 (见最后一条). 其中" (&+ (: $2 $N) $1)
            "加上后者, 是此刻放弃 (一步" $L-Divert ") 及之后卸载所需步数的上界 (累加器每项的权重至多为2, 长度不超过" $N
            "), " (&- $N (: (Mo "|") $acc (Mo "|"))) "是还能落地的步数. 落地一步压入一项, 使它减一 (程序以"
            $N "为界, 所以还能走时" (&< (: (Mo "|") $acc (Mo "|")) $N) "); 落地时失败, 或者" $L-Divert
            ", 都转入卸载, 度量降到这个上界之下.")
        (Li "Active, 视图等于" $target ": 0.")
        (Li "Unloading, 以及视图不等于" $target "的Active: 累加器的权重 (每项为1, "
            (appl $USpawn $m) "在子fiber还在且未请求移除时再加1, 留给" $L-RetireChild
            "), 加上收尾的步数; 若卸载后还会重新加载 (没有错误且" $target "有值), 再加" $D
            ". Active的情形再加1, 留给" $L-Leave "."))
    "状态的度量是向量" (app $Phi $gamma) ", 它的第" $i "个分量是秩为" $i
    "的fiber的度量之和; 向量按字典序比较, 低秩的分量优先. 秩有界, 所以这个字典序是良基的. "
    "设这一步生命周期步由fiber " $n "走出, 它的秩为" $i ". 逐条检查规则可得:"
    (Ol (Li $n "自己的度量严格下降 (被" $L-Remove "删除时记为0);")
        (Li "其他秩不超过" $i "的fiber的度量不变. 这一步可能让秩更低的fiber的某条规则变得可用 "
            "(依赖者卸载后提供者才能开始撤销, 子fiber移除后父fiber才能撤销spawn), "
            "但不改变它们的状态, 也就不改变它们的度量;")
        (Li "新出现的fiber (spawn的子fiber) 的秩高于" $i "."))
    "于是" (app $Phi $gamma) "的第" $i "个分量严格下降, 更低的分量不变, 更高的分量可能增加, "
    "整体按字典序严格下降. 良基序上没有无限下降链, 所以没有无限的执行.")
   (H3. "合流")
   (P "合流分四层证明, 每层建立在前一层的基础上: 控制层 (" (Ref "thm-control") "), 服务状态 ("
      (Ref "thm-state") "), 带spawn (" (Ref "thm-spawn") "), 编排与生命周期交错 (" (Ref "thm-dynamic")
      "). 另有服务的组合 (" (Ref "thm-compose") "), 它与这四层并列, 说明服务一侧的假设对服务的和封闭. "
      "下面先给出服务一侧的假设和删步引理 (" (Ref "thm-delete") "), 再给出组件一侧的假设.")
   ((Definition #:id "def-commute")
    "我们称" (tu0 $a $t_a) "与" (tu0 $b $t_b) (B "可交换") ", 如果对每个" $s ", 先" $a "后" $b
    "与先" $b "后" $a "都失败, 或都成功并给出" $simeq "相关的终态, 且" $a "和" $b
    "各自的结果在两种顺序下相同.")
   (P "恢复精确性只需要撤销独立 (" (Ref "def-revert-indep")
      "), 合流还需要可交换: 不同的调度会让操作以不同的顺序作用在同一个绑定上.")
   ((Definition #:id "def-after")
    "设" $R "是操作上的二元关系. 对操作" $a "和令牌" $t_a ", 定义" (appl (_ $After $R) $a $t_a)
    "为满足下面两条的最小状态集合:"
    (Ol (Li "若" (&= (&run $a $t_a $s) (tu0 $s^ $_ $_)) ", 则" (∈ $s^ (appl (_ $After $R) $a $t_a)) ";")
        (Li "若" (∈ $w (appl (_ $After $R) $a $t_a)) ", " (appl $R $a $b) ", " (&!= $t_b $t_a) ", 且"
            (&= (&run $b $t_b $w) (tu0 $w^ $_ $_)) ", 则" (∈ $w^ (appl (_ $After $R) $a $t_a)) "."))
    "即" $a "之后, 只经过与它" $R "相关的其他操作所能到达的状态. " $R "越大, 这个集合越大.")
   ((Definition #:id "def-confhyps")
    "设" $R "是操作上的二元关系. 我们称服务满足" (B "相对" $R "的合流假设") ", 如果"
    (Ol (Li (B "良好") ": 每个操作在每个令牌下都良好 (" (Ref "def-wb") ");")
        (Li (B "逆与状态无关") ": 操作给出的逆与所在的状态无关, 即若" (&= (&run $a $t $s_1) (tu0 $_ $g_1 $_))
            ", " (&= (&run $a $t $s_2) (tu0 $_ $g_2 $_)) ", 则"
            $g_1 "与" $g_2 "逐点相关;")
        (Li (B "令牌无关") ": 令牌不影响后继 (在" $simeq "意义下) 和结果: 对任意令牌" $t ", " $t^ "和状态" $s ", "
            (&run $a $t $s) "与" (&run $a $t^ $s) "都是" $None ", 或分别是" (tu0 $s_1 $_ $o) "和" (tu0 $s_2 $_ $o)
            ", 其中" (&simeq $s_1 $s_2) ";")
        (Li (B "相关可交换") ": 若" (appl $R $a $b) "且" (&!= $t_a $t_b) ", 则" (tu0 $a $t_a) "与" (tu0 $b $t_b)
            "可交换 (" (Ref "def-commute") "), 并且满足撤销独立中" (Q "不在乎撤销") "的一个弱形式: 对" (tu0 $a $t_a)
            "给出的每个逆" $g "和每个" (∈ $x (appl (_ $After $R) $a $t_a)) " (" (Ref "def-after")
            "), 若" $b "在" $x "上成功, 则" $b "在" (app $g $x)
            "上也成功, 并且两次运行满足" (Ref "def-revert-indep") "中" (Q "不在乎撤销") "在成功情形下的关系."))
    "我们称服务满足" (B "合流假设") ", 如果对每个键" $k ", 它满足相对" (_ $meet $k) " ("
    (Ref "def-meet") ") 的合流假设.")
   (P (Q "良好") "已在恢复精确性中用到; " (Q "相关可交换") "中撤销独立的部分是(H)所要求的" (Q "不在乎撤销") "的弱化 (只在"
      (appl (_ $After $R) $a $t_a) "上, 只约束" $b "成功的情形); " (Q "逆与状态无关") ", " (Q "令牌无关") "和" (Q "相关可交换") "里的可交换是合流新加的. "
      (Q "逆与状态无关") "排除了逆依赖于起点的操作, 比如引言中的" (Q "把" $x "设为5")
      ". 原因是两次执行中同一个操作可能在不同的状态上运行, 若逆随状态而变, "
      "终态中待撤销的逆就不再是同一组, 本文的证明就无法把两次执行的逆逐一对应. "
      "所以为证明合流, 本文额外要求同一操作给出的逆逐点相关. 这是证明所用的充分条件, 不是合流的必要条件: "
      "只有一个组件, 它提供自己的变量并执行" (Q "把" $x "设为5") "时, 逆保存旧值, 依赖原状态, "
      "但没有别的组件与它交错, 稳定终态仍然唯一. 按名字注册和监听器注册都满足" (Q "逆与状态无关") ". "
      (Q "令牌无关") "是因为令牌由调度决定, 两次执行中同一个操作拿到的令牌可能不同.")
   ((Lemma #:id "thm-delete")
    (B "[删步]: ") "设服务满足相对" $R "的合流假设 (" (Ref "def-confhyps") "). 设"
    (&= (&run $a $t_a $s) (tu0 $s_1 $g $_)) ", 并且对" (&= $i (&cm $1 $..h $j)) "有"
    (&= (&run (_ $b $i) (_ $t $i) (_ $x (&- $i $1))) (tu0 (_ $x $i) (_ $h $i) (_ $o $i)))
    ", 其中" (&:= (_ $x $0) $s_1) ", " (appl $R $a (_ $b $i)) ", " (&!= (_ $t $i) $t_a)
    ". 那么从" $s "出发依次运行" (&cm (tu0 (_ $b $1) (_ $t $1)) $..h (tu0 (_ $b $j) (_ $t $j)))
    "都成功, 第" $i "步的结果是" (_ $o $i) ", 给出的逆与" (_ $h $i) "逐点相关 (" (Ref "def-pointwise")
    "), 终态" $simeq (app $g (_ $x $j)) ". 换言之, 从一串成功的运行中删去" $a
    ", 其后那些与" $a "有关系" $R "的运行仍然成功, 并且结果不变.")
   ((proof)
    "由" (Ref "def-after") ", 每个" (_ $x $i) "都属于" (appl (_ $After $R) $a $t_a)
    ". 记从" $s "出发的状态为" (_ $y $i) ", " (&:= (_ $y $0) $s) ". 对" $i "归纳证明"
    (&simeq (_ $y $i) (app $g (_ $x $i))) ". " $i "为0时这是" $a "的见证条件. 设"
    (&simeq (_ $y (&- $i $1)) (app $g (_ $x (&- $i $1)))) ". 由合流假设中的" (Q "相关可交换") ", " (_ $b $i) "在"
    (app $g (_ $x (&- $i $1))) "上成功, 后继" $simeq (app $g (_ $x $i)) ", 结果是" (_ $o $i)
    ", 给出的逆与" (_ $h $i) "逐点相关; 再由" (_ $b $i) "尊重" $simeq " (" (Ref "def-wb")
    "中的" (Q "尊重") "), 在" (_ $y (&- $i $1)) "上也是如此.")
   ((Definition #:id "def-total")
    "我们称组件" $c (B "完整提供") ", 如果" (app $body $c) "的每条到达" $Done "的路径都对"
    (app $p $c) "中的每个键执行过" $Provide ". 称组件" $c (B "不spawn") ", 如果"
    (app $body $c) "中没有" $Spawn ".")
   ((Definition #:id "def-canon")
    "设" $gamma "是状态, " $l "把" $gamma "的每个根fiber映到一个自然数 (称为它的" (B "标号")
    "). fiber的" (B "规范名") "归纳地定义为: 根fiber " $n "的规范名是" (app (Rn "root") (app $l $n))
    "; 若" (&= π_m $n) ", 并且" (appl $USpawn $m) "是" (_ $acc $n) "中按时间顺序的第" $j "个" $USpawn
    "项 (从0数起), 则" $m "的规范名是" (appl (Rn "child") $x $j) ", 其中" $x "是" $n "的规范名. "
    "设" (_ $l $1) ", " (_ $l $2) "分别是" (_ $gamma $1) ", " (_ $gamma $2) "的标号. 我们称" (_ $gamma $1) "与"
    (_ $gamma $2) (B "在重命名意义下一致") ", 如果"
    (Ol (Li "两个状态中每个fiber都有唯一的规范名, 并且同一状态中不同fiber的规范名不同;")
        (Li "对每个规范名, 它在" (_ $gamma $1) "中命名某个fiber当且仅当它在" (_ $gamma $2)
            "中命名某个fiber, 并且这两个fiber的组件相同, 是否Active也相同;")
        (Li "对每个键" $k ", " (app (_ $B $1) $k) "有定义当且仅当" (app (_ $B $2) $k)
            "有定义, 此时两者的提供者有相同的规范名, 服务状态" $simeq "相关."))
    "这里要求" (_ $l $1) ", " (_ $l $2) "都是单射. 两个状态都从同一个状态出发时, 标号取根fiber自己的名字.")
   ((Theorem #:id "thm-control")
    (B "[控制层, 无spawn]: ") "设组件表中的组件都不spawn, 都完整提供 (" (Ref "def-total")
    "), 有界, 并有以某个" $M "为界的秩 (" (Ref "def-rank") "). 设" $gamma_0 "根可达 ("
    (Ref "def-reach") "), " (Steps $gamma_0 $gamma_1) "和" (Steps $gamma_0 $gamma_2)
    "只走生命周期步, " $gamma_1 "和" $gamma_2 "都干净. 那么对每个名字" $n "和键" $k ":"
    (Ol (Li (∈ $n (&dom (_ $F $1))) "当且仅当" (∈ $n (&dom (_ $F $2))) ";")
        (Li "若" (∈ $n (&dom (_ $F $1))) ", 则" (app (_ $F $1) $n) "与" (app (_ $F $2) $n)
            "的组件相同, 并且同时处于Active或同时不处于Active;")
        (Li (&= (app (_ $B $1) $k) (tupa0 $_ $q $_)) "当且仅当" (&= (app (_ $B $2) $k) (tupa0 $_ $q $_)) ".")))
   ((proof)
    "论证不比较两次执行的过程, 而是刻画终态. 在干净的状态中, "
    "一个fiber是Active当且仅当它没有被请求移除, 且它声明的每个键都由一个Active的fiber提供. "
    "这里用到了完整提供: Active的fiber提供了它可能提供的所有键. "
    "这样, Active的fiber之集合是一个不动点方程的解, 方程只依赖" $gamma_0 ". "
    "沿秩归纳可知这个方程只有一个解. 秩的前提不能去掉: 若" $c "提供" (_ $k $1)
    "而依赖" (_ $k $2) ", " $c^ "提供" (_ $k $2) "而依赖" (_ $k $1)
    ", 则两者都Active和两者都不Active都是这个方程的解.")
   ((Theorem #:id "thm-state")
    (B "[服务状态, 无spawn]: ") "在" (Ref "thm-control") "的全部前提下, 再设服务满足合流假设 ("
    (Ref "def-confhyps") "). 则对每个键" $k ", 若"
    (&= (app (_ $B $1) $k) (tupa0 $_ $_ $s_1)) "且" (&= (app (_ $B $2) $k) (tupa0 $_ $_ $s_2))
    ", 则" (&simeq $s_1 $s_2) ".")
   ((proof)
    (B "思路") ": 不逐步比较两次执行, 而是把每个绑定的状态写成它的初始状态上一串尚未撤销的操作的重放, "
    "再说明两个干净终态中这串操作是同一组, 只是先后不同. 前者靠删步引理 (" (Ref "thm-delete")
    "); 后者靠每个fiber的轨迹只由它的程序和它所用绑定的初始状态决定, 再沿秩归纳. "
    "下面对每个键" $k "取" (&:= $R (_ $meet $k)) ". 同一绑定上不同fiber的两个操作总是" (_ $meet $k)
    "相关: 两个fiber的组件不同时由定义即得; 组件相同时, 由" (Ref "def-wf") "中的" (Q "提供不相交") ", 这个组件不提供任何键. "
    "分四步."
    (Ol (Li (B "重放") ": 在一次执行中, 每个绑定的服务状态" $simeq
            "它的初始状态依次运行它上面尚未撤销的操作 (按执行中的先后) 所得的状态, 并且每个操作得到的结果与实际执行中相同. "
            "对执行的步数归纳. 新的操作直接接在序列末尾. 撤销一个操作" $a "时, 由LIFO, 序列中" $a
            "之后只有其他fiber的操作, 它们都与" $a "相关, 由" (Ref "thm-delete") "可以把" $a
            "从序列中删去, 其余操作的结果不变; 由合流假设中的" (Q "逆与状态无关") ", 实际施加的逆与重放中" $a "给出的逆逐点相关.")
        (Li (B "理想轨迹") ": 对一个已加载的fiber " $n ", 从它解析到的那次提供的初始状态出发, 只依次运行"
            $n "自己在该绑定上的操作, 得到的结果与实际执行中相同. 由第1步, 从重放的序列出发, "
            "从右向左逐个删去其他fiber的操作: 删去其中一个时, 它之后剩下的只有" $n
            "的操作, 都与它相关, 由" (Ref "thm-delete") "这些结果不变. 由合流假设中的" (Q "令牌无关") ", 令牌不影响后继和结果. "
            "所以" $n "的整条轨迹 (每个操作的结果, 因而它的程序走过的路径) 只由它的程序和它所用绑定的初始状态决定.")
        (Li (B "秩归纳") ": 沿秩归纳, 两个终态中对应的Active fiber有相同的轨迹. 依赖者所用绑定的初始状态由提供者的"
            $Provide "给出, 而提供者的轨迹决定它提供什么; 提供者的秩更低, 由归纳假设它们在两个终态中相同.")
        (Li (B "终态") ": 在干净的终态中, 每个fiber或是Active, 或是累加器为空的Inactive. 所以每个绑定上"
            "尚未撤销的操作, 恰是Active的fiber的轨迹中在这个绑定上的操作. 由第3步, 两个终态中这些操作是同一组, "
            "只是先后可能不同; 由合流假设中" (Q "相关可交换") "的可交换, 重排不改变由第1步得到的终态的" $simeq "类.")))
   ((Theorem #:id "thm-spawn")
    (B "[带spawn]: ") "设组件表中的组件都完整提供, 组件表有界, 有以某个" $M "为界的秩, "
    "服务满足合流假设. 设" (_ $gamma $1) ", " (_ $gamma $2) "都根可达且干净, 标号分别为" (_ $l $1) ", " (_ $l $2) " (" (Ref "def-canon") ")."
    (Ol (Li "若两者的根fiber按标号一一对应且组件相同, 则" (_ $gamma $1) "与" (_ $gamma $2)
            "在重命名意义下一致.")
        (Li "特别地, 若" (Steps $gamma_0 (_ $gamma $1)) "和" (Steps $gamma_0 (_ $gamma $2))
            "只走生命周期步, 而" $gamma_0 "根可达, 则" (_ $gamma $1) "与"
            (_ $gamma $2) "在重命名意义下一致 (标号取根fiber自己的名字).")))
   ((proof)
    "第1条: 父fiber是否spawn, spawn什么, 取决于它的操作的结果, 所以对应关系要沿秩归纳建立: "
    "对应的fiber有相同的组件和理想轨迹 (见" (Ref "thm-state") "的证明). 归纳中轨迹的一致有两个来源: "
    "父fiber的轨迹决定子fiber是否存在, 以及它在父fiber的spawn中的序号, 从而决定它的规范名; "
    "提供者的轨迹决定依赖者所用绑定的初始状态. 两者的秩都比所讨论的fiber低. "
    "其余部分与无spawn的情形相同. 这里还需要一个不变式: Loading和Active的fiber的子fiber都还活着. "
    "它在接受根编排输入的执行中保持; 对启动时spawn的子fiber直接" $O-Dispose "会破坏它, 见下文. "
    "第2条: 生命周期步不插入根fiber, 只可能由" $L-Remove "删除已请求移除的根fiber, 而组件不变. "
    "在静止的终态中, " $gamma_0 "里已请求移除的根fiber都已被删除 (否则它处于卸载中, 或" $L-Remove
    "可用), 其余的根fiber都还在. 所以" (_ $gamma $1) "与" (_ $gamma $2) "留下的根fiber相同, 组件也相同, 由第1条即得.")
   ((Theorem #:id "thm-dynamic")
    (B "[编排与生命周期交错]: ") "设组件表和服务满足" (Ref "thm-spawn") "的前提. "
    "我们称根编排输入中的一次插入" (B "留下") ", 如果输入中没有对它的" $O-Dispose
    ". 设两次执行都从初始状态出发, 分别接受根编排输入" (_ $I $1) "和" (_ $I $2)
    ", 留下的组件按插入次序排成的序列相同, 并且都到达干净的状态. "
    "以根fiber在留下的插入中的序号为标号, 两个终态在重命名意义下一致 (" (Ref "def-canon") "). 特别地:"
    (Ol (Li "接受相同根编排输入的两次执行, 终态一致;")
        (Li "一次执行的终态, 与只按次序插入它留下的组件的执行 (" (B "静态组装")
            ") 的终态一致, 只要后者也到达干净的状态.")))
   ((proof)
    "沿执行记录一本账: 对每个留下的插入, 终态中恰有一个根fiber, "
    "组件就是插入的那个. 早先的插入, 移除请求, 重启, 重置除了留下的根fiber之外不留痕迹. "
    "于是两个终态的根fiber按它们在留下的插入中的序号一一对应, 由" (Ref "thm-spawn") "的第1条即得.")
   ((Theorem #:id "thm-compose")
    (B "[服务的组合]: ") (Ref "thm-state") ", " (Ref "thm-spawn") "和" (Ref "thm-dynamic") "中对服务的要求恰是合流假设 (" (Ref "def-confhyps")
    "), 所以它们对任何满足合流假设的服务成立. 若服务" (_ Σ $1) "满足相对" (_ $R $1) "的合流假设, "
    (_ Σ $2) "满足相对" (_ $R $2) "的合流假设, 则它们的和满足相对" (&union (_ $R $1) (_ $R $2))
    "的合流假设, 其中" (_ $R $i) "只关联" (_ Σ $i) "的操作.")
   ((proof)
    "前一半是把三个定理的服务前提逐条对照. 和的状态属于两者之一, "
    "一方的操作在另一方的状态上失败; 不同部分的操作从不作用在同一个状态上, "
    "所以它们可交换, 也互不干扰. 同一部分的操作对按该部分的假设处理.")
   (P "合流不覆盖" $O-React ", 也只允许对根fiber用" $O-Dispose ". 原因是: 启动时spawn的子fiber若被直接" $O-Dispose ", "
      "父fiber在一次执行里保留原生命期, 在另一次执行里重新加载并重新spawn, "
      "两次的终态确实不同; 这在Cordis中同样成立, 不是模型的人为限制. "
      "保持性, 顺序, 恢复精确性, 进展, 终止都覆盖" $O-React ".")
   (H3. "派生形式" #:id "sec-meta-derived")
   (P "Cordis的隔离 (" (Code "ctx.isolate") ") 和提供时的可用性检查都不需要新的规则: "
      "隔离是组件表的一个变换, 可用性检查是程序的一个缩写. 本节证明两者都保持第6章各定理对组件表的假设, "
      "所以那些定理直接适用.")
   ((Definition #:id "def-scoped")
    "设" $Name "是服务名的集合, " $Lb "是作用域标记的集合. 映射" (&: $phi (&-> $Name $Lb))
    "称为" (B "作用域") "; 作用域的集合记作" $Scope ". 对作用域" $phi ", 定义"
    (&: (_ $key $phi) (&-> $Name (@c* $Name $Lb))) ", " (&:= (app (_ $key $phi) $nu) (tu0 $nu (app $phi $nu)))
    ", 即在作用域" $phi "中, 服务名" $nu "对应的键是" (tu0 $nu (app $phi $nu))
    "; 它作用在键集上时取逐元素的像. 设" (&: $prog (&-> $C $Component)) "是以" $Name
    "为键集的组件表, 并给定映射" (&: $enter (&-> (@c* $C $Scope) $Scope)) ", "
    (appl $enter $c^ $phi) "是作用域" $phi "中的fiber spawn " $c^ "时, " $c^
    "所在的作用域. 对作用域" $phi ", 归纳定义程序的" (B "重命名") " " (_ $ren $phi) ":"
    (MB (eqn*
         ((app (_ $ren $phi) $Done) $= $Done)
         ((app (_ $ren $phi) $Fail) $= $Fail)
         ((app (_ $ren $phi) (Con $Call $nu $a $kappa)) $=
          (Con $Call (app (_ $key $phi) $nu) $a (Lam $o (app (_ $ren $phi) (app $kappa $o)))))
         ((app (_ $ren $phi) (Con $Provide $nu $s $P)) $=
          (Con $Provide (app (_ $key $phi) $nu) $s (app (_ $ren $phi) $P)))
         ((app (_ $ren $phi) (Con $Spawn $c^ $P)) $=
          (Con $Spawn (tu0 (appl $enter $c^ $phi) $c^) (app (_ $ren $phi) $P)))))
    "若" (∈ $P (_ $Prog (&cm $d $p))) ", 则"
    (∈ (app (_ $ren $phi) $P) (_ $Prog (&cm (app (_ $key $phi) $d) (app (_ $key $phi) $p))))
    ". 由" $prog "和" $enter "得到的" (B "带作用域的组件表") " " $progS
    "以" (@c* $Name $Lb) "为键集, 以" (&c* $Scope $C) "为组件标识的集合, 定义为"
    (MB (&:= (appl $progS $phi $c)
             (tupa0 (app (_ $key $phi) (app $d $c)) (app (_ $key $phi) (app $p $c))
                    (app (_ $ren $phi) (app $body $c)) $rho_c)) ",")
    "其中" $rho_c "是把" (app $reacts $c) "中的每一项" $c^ "换成" (tu0 (appl $enter $c^ $phi) $c^) "所得的列表.")
   (P "普通的spawn保持作用域, 即" (&= (appl $enter $c^ $phi) $phi) "; 隔离一棵子树, 就是让它的根进入一个新的作用域, "
      "其中被隔离的服务名换了标记. " (_ $key $phi) "的第一个分量就是服务名, 所以不同作用域中同一个服务名的键"
      "只在标记上不同, 标记不同时它们是不同的键. 这正是隔离的含义. "
      "注意" $enter "只依赖组件和父作用域, 所以同一父作用域中对同一组件的多次spawn落在同一个作用域里: "
      "这里建模的是静态分域. 若要每次实例化都分配新的作用域, 需要扩展模型.")
   ((Theorem #:id "thm-scoped")
    "设" $progS "是由" $prog "和" $enter "得到的带作用域的组件表 (" (Ref "def-scoped")
    "). 在下面各条中, 涉及" (tu0 $phi $c) "的记号相对于" $progS ", 涉及" $c "的相对于" $prog "."
    (Ol (Li (B "有界") ": 若" (app $body $c) "的每条路径的长度都不超过" $N ", 则对每个作用域" $phi ", "
            (app (_ $ren $phi) (app $body $c)) "的每条路径的长度也都不超过" $N ".")
        (Li (B "完整提供") " (" (Ref "def-total") "): 若" $c "完整提供, 则对每个作用域" $phi ", "
            (tu0 $phi $c) "完整提供.")
        (Li (B "之上") " (" (Ref "def-prec") "): 若" (&prec (tu0 $phi $c) (tu0 $phi^ $c^))
            ", 则" (&prec $c $c^) ". 所以若" $r "是" $prog "的以" $M "为界的秩 (" (Ref "def-rank") "), 则"
            (Lam (tu0 $phi $c) (app $r $c)) "是" $progS "的以" $M "为界的秩.")
        (Li (B "不spawn") " (" (Ref "def-total") "): 若" $c "不spawn, 则对每个作用域" $phi ", " (tu0 $phi $c) "不spawn.")
        (Li (B "可能相遇") " (" (Ref "def-meet") "): 若在" $progS "中"
            (&meet (tu0 $nu $chi) $a $b) ", 则存在" (∈ (&cm $c $c^) $C) ", 使"
            (∈ $a (&ops $nu (app $body $c))) "且" (∈ $b (&ops $nu (app $body $c^))) ".")))
   ((proof)
    "先对程序" $P "的结构归纳, 得到" (_ $ren $phi) "的三条性质: 它保持每条路径的长度; "
    (Con $Provide $nu $s $_) "出现在" $P "的某条路径上, 当且仅当" (Con $Provide (app (_ $key $phi) $nu) $s $_)
    "出现在" (app (_ $ren $phi) $P) "的对应路径上, 对" $Call "也一样; " (Con $Spawn (tu0 $psi $c^) $_)
    "出现在" (app (_ $ren $phi) $P) "中, 仅当" (Con $Spawn $c^ $_) "出现在" $P "中. "
    "第1条是第一条性质. 第2条由第二条性质和" (_ $key $phi) "是单射得到. 第4条是第三条性质. "
    "第3条: 若" (&!= (&cap (app (_ $key $phi) (app $p $c)) (app (_ $key $phi^) (app $d $c^))) $empty)
    ", 比较第一个分量得" (&!= (&cap (app $p $c) (app $d $c^)) $empty)
    "; 若" (tu0 $phi $c) "可能spawn " (tu0 $phi^ $c^) ", 由第三条性质和" $rho_c "的定义, " $c "可能spawn " $c^
    ". 秩的两个条件因此对" $progS "成立. 第5条: 由第二条性质, "
    (∈ $a (&ops (tu0 $nu $chi) (app (_ $ren $phi) $P))) "当且仅当" (&= (app $phi $nu) $chi) "且" (∈ $a (&ops $nu $P))
    "; 再按" (Ref "def-meet") "展开, 并舍去其中" (&= $c $c^) "时的附加条件即得.")
   ((Definition #:id "def-anymeet")
    "我们称操作" $a "和" $b "在服务名" $nu "上" (B "可能共用") ", 记作" (appl (_ $share $nu) $a $b) ", 如果存在"
    (∈ (&cm $c $c^) $C) ", 使" (∈ $a (&ops $nu (app $body $c))) "且" (∈ $b (&ops $nu (app $body $c^)))
    ". 这里的" $prog "是以" $Name "为键集的组件表. 与" (Ref "def-meet") "相比, 它不豁免" (&= $c $c^) "的情形.")
   ((Corollary #:id "cor-scoped")
    "设" $prog "以" $N "为界, 有以" $M "为界的秩, 其中的组件都完整提供. "
    "设" $progS "是由" $prog "和任意" $enter "得到的带作用域的组件表."
    (Ol (Li $progS "满足同样的三条: 以" $N "为界, 有以" $M "为界的秩, 组件都完整提供;")
        (Li "若服务的操作都良好, 并且" (&=> (&conj (appl (_ $share $nu) $a $b) (&!= $t_a $t_b)) (appl $RI $a $t_a $b $t_b))
            " (对所有" $nu ", " $a ", " $b ", " $t_a ", " $t_b "), 则" $progS "满足假设(H) (" (Ref "def-H") ");")
        (Li "若服务满足相对关系" (&:<=> (appl $R $a $b) (∃ $nu (appl (_ $share $nu) $a $b)))
            "的合流假设 (" (Ref "def-confhyps") "), 则对" $progS "的每个键" (tu0 $nu $chi) ", 服务满足相对" (_ $meet (tu0 $nu $chi)) "的合流假设, 其中" (_ $meet (tu0 $nu $chi)) "按" $progS "定义."))
    "所以对" $progS ", " (Ref "thm-recovery") "在第2条的服务假设下成立, " (Ref "thm-progress") "和" (Ref "thm-termination")
    "成立, " (Ref "thm-spawn") "和" (Ref "thm-dynamic") "在第3条的服务假设下成立; 若" $prog "中的组件还都不spawn, "
    (Ref "thm-control") "和" (Ref "thm-state") "也在第3条的服务假设下成立 (" (Ref "thm-scoped") "的第4条).")
   ((proof)
    "第1条是" (Ref "thm-scoped") "的第1至3条. 由" (Ref "thm-scoped") "的第5条, 在" $progS "中"
    (&meet (tu0 $nu $chi) $a $b) "蕴涵" (appl (_ $share $nu) $a $b) ", 由此得第2条; 它还给出"
    (&sube (_ $meet (tu0 $nu $chi)) $R) ", 而" (_ $After $R) "随" $R "增大, 所以相对" $R "的合流假设蕴涵相对"
    (_ $meet (tu0 $nu $chi)) "的合流假设, 这是第3条.")
   (P "推论对独立性的要求比(H)强. (H)只要求可能相遇的操作对撤销独立, 并豁免了同一个提供键的组件, "
      "因为这样的组件同时至多有一个实例. 隔离之后, 同一个组件可以在几个作用域中同时运行, "
      "它的两个副本操作同一服务名的键, 而这两个键只在标记不同时才分开. 所以豁免不再成立.")
   ((Definition #:id "def-checked")
    "对谓词" (&: $chk (&-> $St $BB)) ", 键" $k ", 状态" (∈ $s $St) "和程序" $P ", 定义"
    (MB (&:= (appl $ProvideChk $chk $k $s $P)
             (Choice0 ((Con $Provide $k $s $P) ", 若" (app $chk $s) "为真")
                      ($P ", 否则"))))
    "它对应Cordis中带可用性检查的" (Code "provide(name, value, check)") ", 检查在提供时对要提供的值求值.")
   ((Lemma #:id "thm-checked")
    "设" $P "的每条路径的长度都不超过" $j ", 则" (appl $ProvideChk $chk $k $s $P)
    "的每条路径的长度都不超过" (&+ $j $1) ". 若" (app $chk $s) "为假时" $P
    "的每条到达" $Done "的路径都对" $k "执行过" $Provide ", 则" (appl $ProvideChk $chk $k $s $P)
    "的每条到达" $Done "的路径都对" $k "执行过" $Provide ".")
   ((proof)
    "按" (app $chk $s) "分情况. 为真时程序是" (Con $Provide $k $s $P) ", 多出的一步本身就提供了" $k
    "; 为假时程序就是" $P ".")
   (P "所以可用性检查是程序自己就能做的判断; 由" (Ref "thm-checked") ", 有界和完整提供在上述条件下保持. "
      "Cordis在通知依赖者时还会对当前的值重新求值检查, 本文不建模这一点.")
   (H3. "汇总" #:id "sec-summary")
   (P "前面各定理的假设分散在各节. 本节把它们分成两组: 服务的规格只谈服务自身和一个操作上的关系"
      $G ", 不提任何程序; 组件的义务只谈组件表, 其中一条要求可能相遇的操作对落在" $G
      "之内. 服务的作者对" $G "证明规格, 插件的作者检查义务, 两者在" $G "处对接.")
   ((Definition #:id "def-spec")
    "设" $G "是操作上的二元关系. 我们称服务" Σ "相对" $G
    (Ol (Li (B "可撤销") ", 如果每个操作在每个令牌下良好, 并且"
            (MB (&=> (&conj (appl $G $a $b) (&!= $t_a $t_b)) (appl $RI $a $t_a $b $t_b)) ";"))
        (Li (B "可合流") ", 如果它满足相对" $G "的合流假设 (" (Ref "def-confhyps") ").")))
   ((Definition #:id "def-obligation")
    "我们称组件表" $prog "相对" $G "满足义务, 如果"
    (Ol (Li (B "有界") ": 组件表有界 (" (Ref "def-rank") ");")
        (Li (B "有秩") ": 存在以某个" $M "为界的秩" $r " (" (Ref "def-rank") "), 即"
            (&=> (&prec $c $c^) (&< (app $r $c) (app $r $c^))) "且"
            (∀ $c (&< (app $r $c) $M)) ";")
        (Li (B "相遇受控") ": " (&=> (&meet $k $a $b) (appl $G $a $b)) ";")
        (Li (B "完整提供") ": 每个组件都完整提供 (" (Ref "def-total") ").")))
   ((Example #:id "ex-tiers")
    "下面三个服务说明规格的两部分之间的关系."
    (Ol (Li (B "注册表") " (" (Ref "ex-registry") "). 取" $G
            "关联名字不同的两次登记, 名字不同的登记与查询, 以及任意两次查询, 两部分都满足. "
            "相遇受控此时说的是: 可能相遇的两次登记, 以及登记与查询, 名字都不同, "
            "在插件的层面就是" (Q "不同插件不登记同名的工具, 也不查询别的插件登记的同名工具") ". "
            "同名的两次登记不满足撤销独立: 后一次在实际执行中失败, 撤销前一次之后却会成功. "
            "但若" $G "再关联同名的两次登记, 注册表仍然可合流: 两种顺序中都是后一次失败, 因而它们可交换; "
            "在第一次登记之后, 只经过与它相关的操作所到达的状态中, 这个名字总是已被占用 ("
            (Ref "def-after") "), 同名的登记在那里总是失败, 而合流假设中" (Q "相关可交换") "的撤销独立部分只约束成功的情形. "
            "所以这样取" $G "时, 注册表可合流, 但不可撤销.")
        (Li (B "监听器注册") " (" (Ref "ex-bus") "末尾). " $G
            "取全关系时可合流, 但不可撤销: 设" $f "不在列表" $x
            "中, 两个fiber各注册一次" $f ", 撤销独立的第一条要求" $x "加上" $f "之后" $simeq
            $x ", 不成立. 若" $G "只关联回调不同的注册, 两部分都满足, "
            "这时相遇受控要求可能相遇的两次注册回调不同. 即使回调相同, "
            "删去第一个" $f "在这里仍然给出正确的结果, 所以撤销独立是充分条件而非必要条件.")
        (Li (B "变量存储") ". 状态是变量到值的映射, 逐点比较; 操作" (Q "把" $x "设为" $v)
            "的逆是把" $x "改回原来的值, 也就是引言中的例子. 取" $G
            "关联不同变量上的操作, 它可撤销. 但它的逆依赖于原来的值, 违反合流假设中的" (Q "逆与状态无关") ", "
            "而这一条与" $G "无关, 因此不论" $G "怎样取, 它都不可合流."))
    "前两个说明可合流推不出可撤销, 第三个说明可撤销推不出可合流.")
   ((Theorem #:id "thm-summary")
    "设组件表相对" $G "满足有界, 有秩和相遇受控. 设" $gamma
    "是可达状态 (" (Ref "def-reach") "), 到达它的标签序列可以包含" $O-React "和对子fiber的" $O-Dispose "."
    (Ol (Li (B "安全") " (不需要假设): " $gamma "良构 (" (Ref "def-wf") "), 因而"
           (Ref "thm-outlive") "在" $gamma "成立.")
       (Li (B "活性") ": 从" $gamma "出发只走生命周期步, 没有无限的执行; "
           "走到没有生命周期规则可用时, 状态是稳定的.")
       (Li (B "恢复") " (服务可撤销): 设执行从初始状态到达" $gamma ", "
           $shadow "是沿它略去fiber " $n "全部操作所得的影子存储. 若" $n "在" $gamma
           "中不存在或未安装, 则对每个键" $k ", " (app (&store $gamma) $k) "与"
           (app $shadow $k) "同时无定义, 或者"
           (&simeq (app (&store $gamma) $k) (app $shadow $k))
           ". 取" $n "为一次反应的子fiber, 就是运行期效应的精确撤销.")
       (Li (B "合流") " (服务可合流, 组件表还满足完整提供): 两次执行从初始状态出发, "
           "接受相同的根编排输入 (" (Ref "def-reach") "); 若都到达干净的状态, "
           "则两个终态在重命名意义下一致; 并且若静态组装 (只按插入次序插入留下的组件) "
           "也到达干净的状态, 它的终态与它们一致.")))
   ((proof)
    (Ol (Li "安全: 即" (Ref "thm-preservation") ".")
        (Li "活性: 有秩给出" $prec "上严格上升且有界的度量, 所以" $prec
            "没有无限上升链; 由有界和有秩, " (Ref "thm-termination")
            "排除无限的生命周期执行. 生命周期步的终点仍是可达状态, 若它静止, 则由"
            (Ref "thm-progress") "它是稳定的.")
        (Li "恢复: 即" (Ref "thm-recovery") ", 取" $gamma_0 "为初始状态 (" $n
            "在其中没有待撤销操作), 而" $n "不存在或未安装时也没有待撤销操作. "
            "所需的(H)由可撤销和相遇受控得到.")
        (Li "合流: 即" (Ref "thm-dynamic") ". 由相遇受控, 对每个键" $k "有"
            (&sube (_ $meet $k) $G) "; 而" (_ $After $R) "随" $R "增大, 所以相对" $G
            "的合流假设蕴涵相对" (_ $meet $k) "的合流假设, 即合流假设 (" (Ref "def-confhyps")
            "). 有界, 有秩和完整提供是" (Ref "thm-spawn") "对组件的前提.")))
   (P "几点说明.")
   (Ul (Li (B "为什么分两部分") ". 恢复精确性要撤销独立的两条在所有状态上成立; "
           "合流只用到" (Q "不在乎撤销") ", 并且只在" (appl (_ $After $G) $a $t_a) "上, 却另外要求可交换, "
           "逆与状态无关, 令牌无关. 由" (Ref "ex-tiers") ", 两部分互不蕴涵.")
       (Li (B "义务的粒度") ". 有界和完整提供是对单个组件的要求. 有秩和相遇受控是对整张组件表的要求: "
           "前者对有限的表就是" (Q "依赖与spawn关系无环") ", 后者在插件的层面读作"
           (Q "不同插件不登记同名的工具") "这一类约定. 两者本质上是全局的, "
           "因为它们说的是插件之间的关系.")
       (Li (B "失败不是义务") ". 合流以两次执行都没有失败的fiber为前提. "
           "失败可能来自" $Fail ", 操作失败或键冲突, 取决于运行时的状态, 不是组件表的静态性质. "
           "实际使用时的检查方式是: 静态组装一次, 看它是否到达干净的状态; "
           "由第4条, 任何其他调度到达的干净终态都与它一致. 其他调度仍可能失败 (例如程序显式执行"
           $Fail "), 这需要单独检查.")
       (Li (B "编排的范围") ". 第1到3条对任意编排成立, 包括" $O-React
           "和对子fiber的" $O-Dispose "; 只有第4条限于根编排, 原因见合流一节.")
       (Li (B "隔离") ". 由" (Ref "cor-scoped") ", 带作用域的组件表保持有界, 有秩和完整提供; "
           "相遇受控则要换成: 对每个服务名" $nu ", " (&sube (_ $share $nu) $G) " (" (Ref "def-anymeet")
           "). 满足这一点时, 本定理直接适用于隔离后的组件表."))

   (Appendix "A" #:id "sec-effects" "单个组件的语义")
   (P "本附录对应论文的3.1节, 以及3.3.2节中关于等价的部分. 它把一个程序解释为存储上良好的部分效应, "
      "从而说明一个组件单独卸载时存储在等价意义下复原, 并且演算中累加器的撤销项按LIFO复合起来就是这个效应的逆. "
      "正文的定理不依赖本附录. "
      "效应可能失败 (例如重复提供同一个键), 而恢复只能在某个等价关系下成立: "
      (Code "free") "不会恢复" (Code "malloc") "之前的堆布局. 下面固定一个类型" Γ "及其上的等价关系" $approx
      ", 并记" (&:= (&End Γ) (&-> Γ Γ)) ".")
   ((Definition)
    "定义部分效应"
    (MB (&:= (&PEff Γ) (&-> Γ (&Maybe (@c* Γ (&End Γ))))) ",")
    (&= (app $e $x) (tu0 $y $g)) "给出新状态" $y "和这一步的逆" $g
    ". 我们称两个结果相关, 如果它们都是" $None ", 或者状态相关且逆逐点相关. 称映射"
    $g "尊重" $approx ", 如果" (&=> (&approx $x_1 $x_2) (&approx (app $g $x_1) (app $g $x_2)))
    ".")
   ((Definition #:id "def-pwitnessed")
    "我们称" (∈ $e (&PEff Γ)) (B "良好") ", 如果"
    (Ol (Li (B "带见证") ": 若" (&= (app $e $x) (tu0 $y $g)) ", 则"
            (&approx (app $g $y) $x) ";")
        (Li (B "逆尊重") ": 上述" $g "尊重" $approx ";")
        (Li (B "尊重") ": 相关的输入给出相关的结果."))
    "这三条与" (Ref "def-wb") "的三条相对应. " (Q "带见证") "只要求" $g "在" $e "被应用的那一点撤销它, 不要求" $g "是全局的左逆. "
    "这是论文的定义8和定义36, 论文称之为witnessed.")
   ((Definition)
    "对于" (∈ $e_1 $e_2 (&PEff Γ)) ", 定义先" $e_2 "后" $e_1 "的复合"
    (MB (&:= (app (@diamond $e_1 $e_2) $x)
             (Let (&:= (tu0 $y $g) (app $e_2 $x))
                  (Let (&:= (tu0 $z $h) (app $e_1 $y))
                       (tu0 $z (&compose $g $h))))) ",")
    "任一步为" $None "时结果为" $None "; 单位元是"
    (&:= (app $eta_Γ $x) (tu0 $x $id))
    ". 对" (&: $a (&-> Γ (&Maybe (@c* Γ (&End Γ) $Out))))
    ", 记" (&: $abar (&PEff Γ)) "为" $a "去掉结果所得的部分效应: " (&= (app $a $x) (tu0 $y $g $o))
    "时" (&:= (app $abar $x) (tu0 $y $g)) ", " (&= (app $a $x) $None) "时" (&:= (app $abar $x) $None)
    ". 对这样的" $a "和" (&: $kappa (&-> $Out (&PEff Γ))) ", 按结果绑定为"
    (MB (&:= (app (&bind $a $kappa) $x)
             (Let (&:= (tu0 $y $g $o) (app $a $x))
                  (Let (&:= (tu0 $z $h) (app (app $kappa $o) $y))
                       (tu0 $z (&compose $g $h))))) ".")
    "这是monad " (&:= (app $M $A) (&-> Γ (&Maybe (@c* Γ (&End Γ) $A))))
    " 的Kleisli复合, 它组合了三层: 状态 (前一段产生的状态" $y "传给后一段), 幺半群"
    (tu0 (&End Γ) $compose) "上的writer (日志是逆, 新的一项接在右边, 撤销时最先施加), 以及失败 ("
    $Maybe "). "
    "按结果绑定是论文的式(32): 每一段执行一个操作, 再根据结果选择后续.")
   ((Theorem #:id "thm-closure")
    "若" $e_1 ", " $e_2 "良好, 则" (&diamond $e_1 $e_2) "良好; "
    "若" $abar "良好, " $a "在相关的输入上给出相同的结果" $o
    ", 且每个" (app $kappa $o) "良好, 则" (&bind $a $kappa)
    "良好. 特别地, 依次执行良好的" (&cm $e_1 $..h $e_j) "之后, "
    "按LIFO顺序施加它们各自给出的逆, 回到的状态" $approx "起点, "
    "不需要任何关于效应之间关系的假设. 复合的情形是论文的定理11(1), LIFO的情形是定理16; "
    "按结果绑定的封闭性论文没有单独陈述.")
   ((proof)
    (Ul (Li "复合: 逐条检查" (Ref "def-pwitnessed") "的三个条件."
            (Ol (Li "见证条件: 若" (&= (app $e_2 $x) (tu0 $y $g)) ", "
                    (&= (app $e_1 $y) (tu0 $z $h)) ", 则"
                    (&approx (app $g (app $h $z)) (app $g $y) $x)
                    ", 第一步要求" $g "尊重" $approx ";")
                (Li "复合的逆是尊重" $approx "的映射的复合, 仍然尊重" $approx ";")
                (Li "复合本身尊重" $approx ", 因为相关的输入在每一段都给出相关的结果.")))
        (Li "按结果绑定: 相关的输入给出相同的结果, 所以选择的是同一个后续, 论证与复合相同.")
        (Li "LIFO: 对" $j "归纳使用复合的情形, "
            (&diamond $e_j $..c $e_1) "给出的逆正是" (&compose $g_1 $..c $g_j) ".")))
   (P "论文3.1节的其余构造, 即扭曲复合, 效应上下文与track/recover, 效应函数变换" (Code "effect")
      ", 以及3.4.1节中任意顺序的撤销 (定理43), 本文都不需要; 效应的独立性 (定义42) 只用到其中一部分, 即"
      (Ref "def-revert-indep") ". "
      "演算的累加器是撤销项的列表, 按LIFO逐项撤销 (" (Ref "sec-state")
      "); 撤销与其他fiber的交错由操作级的假设处理 (" (Ref "sec-recovery") ").")
   (P "下面把部分效应用到" (Ref "sec-programs") "的程序上: 先定义存储及其上的两个基本效应, "
      "再给出程序的解释.")
   ((Definition)
    "存储是" (&:= $Store (&p-> $K $St)) "; 我们称两个存储相关, 如果它们定义域相同且逐键"
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
    "第一个是论文定义20的set, 第二个是式(31)的提升. 提升后的逆作用在"
    (Em "撤销时") "的存储上, 只改动键" $k ", 所以别的组件在中间做过的事会保留下来.")
   ((Definition)
    "程序的解释" (&: (Den $P $t) (&PEff $Store)) " (" $t "是起始令牌) 定义为"
    (MB (eqn*
         ((Den $Done $t) $= (_ $eta $Store))
         ((Den $Fail $t) $= (Lam $sigma $None))
         ((Den (Con $Call $k $a $kappa) $t) $=
          (&bind (&lift $k $a $t) (Lam $o (Den (app $kappa $o) (&+ $t $1)))))
         ((Den (Con $Provide $k $s $P) $t) $=
          (&diamond (Den $P (&+ $t $1)) (&setk $k $s)))
         ((Den (Con $Spawn $c $P) $t) $= (Den $P (&+ $t $1)))))
    "spawn不是存储上的效应, 所以解释为恒等.")
   ((Theorem #:id "thm-denote")
    "若服务的操作都良好, 则每个程序的解释都是良好的部分效应; 特别地,"
    (MB (&=> (&= (app (Den $P $t) $sigma) (tu0 $sigma^ $g))
             (&simeq (app $g $sigma^) $sigma)) ".")
    "换言之, 一个组件单独卸载时, 存储在" $simeq "意义下复原.")
   ((proof)
    "先看两个基本效应."
    (Ul (Li (&setk $k $s) "良好: 在" (&!in $k (&dom $sigma)) "时, "
            (&= (&\\ (par0 (ext $sigma $k $s)) $k) $sigma)
            "; 它的逆尊重存储的等价, 因为存储是逐键比较的.")
        (Li (&lift $k $a $t) "良好: 它的逆只改动键" $k
            ", 在该键上就是操作" $a "的逆, 由" $a "良好即得."))
    "然后对" $P "的结构归纳:"
    (Ul (Li $Done "和" $Fail ": 平凡.")
        (Li $Call ": 由" (&lift $k $a $t) "良好和按结果绑定的封闭性 (" (Ref "thm-closure") ").")
        (Li $Provide ": 由" (&setk $k $s) "良好和" $diamond "的封闭性.")
        (Li $Spawn ": 就是归纳假设.")))
   ((Theorem #:id "thm-lone")
    "设服务的操作都良好. 把撤销项读作存储上的映射: " (appl $UOp $k $iota $t $g) "读作"
    (Lam $sigma^ (ext $sigma^ $k (app $g (app $sigma^ $k)))) " (" (&!in $k (&dom $sigma^)) "时为恒等), "
    (appl $UProv $k $iota) "读作删去" $k ", " (appl $USpawn $m) "读作恒等. 这种读法不看提供编号; "
    "同一个键被重新提供之后, 按这种读法得到的映射与演算中撤销项的实际作用可能不同, "
    "但在单独加载中不会发生这种情形. "
    "解释与演算的步骤一致: 一个" $Call "步或" $Provide "步恰好是解释的一段, "
    "它的撤销项施加这一段的逆. 特别地, 设组件" $c "的fiber从存储" $sigma
    "出发单独 (没有其他fiber走步) 加载到Active, 开始时计数器为" $t ", 则"
    (&= (app (Den (app $body $c) $t) $sigma) (tu0 $sigma^ $g))
    "有定义, 结束时的存储" $simeq $sigma^ ", 并且它累加器中新增的撤销项按上述读法, 以LIFO复合起来, 与" $g
    "在每个存储上给出" $simeq "相关的结果.")
   ((proof)
    "对" (app $body $c) "的路径归纳, 逐个比较" $L-Step "的情形与解释的一段. "
    "单独加载时, 视图" $omega "解析到的编号就是当前绑定" $B "中的编号 (" (Ref "thm-outlive")
    "), 所以无论键是依赖还是自己提供的, 操作看到的都是存储在该键上的状态. 前进的一侧:"
    (Ul (Li (Con $Call $k $a $kappa) ": 规则取令牌" (&:= $t $next) ", 运行" (&run $a $t $s)
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
    "我们称等价关系" $R (B "可容许") ", 如果每个操作都尊重它 (即"
    (appl $R $s $s^) "时, " (&run $a $t $s) "与" (&run $a $t $s^)
    "同为" $None ", 或给出" $R "相关的后继和相同的结果), 并且每个可能给出的逆"
    $g "都满足" (&=> (appl $R $x $y) (appl $R (app $g $x) (app $g $y))) ".")
   ((Theorem)
    $sim "是可容许的, 并且对每个可容许的" $R "有" (&sube $R $sim)
    ". 换言之, " $sim "是操作所尊重的最粗的等价关系. 特别地, 操作良好的服务有"
    (&sube $simeq $sim) ".")
   ((proof)
    "可容许: 设" $s "与" $s^ "不可区分. 只有一个字母" (appl $LF $a $t)
    "的测试说明" $a "在两者上同时失败或同时成功, 且结果相同; 在一个测试前面加上这个字母"
    "仍是测试, 所以两个后继也不可区分. 逆字母同理. "
    "包含: 设" $R "可容许, " (appl $R $s $s^) ". 对测试的长度归纳: 每个字母都把"
    $R "相关的状态送到" $R "相关的状态, 并给出相同的结果, 所以整个测试在两边的结果相同.")
   (P "这就是最大互模拟: 测试每走一个字母, 两边同时走一步, 比较定义性与结果.")
   ((Theorem)
    "在带分发的有序事件总线上, 设" (&!= (_ $f $1) (_ $f $2)) "是两个回调, 分别用令牌"
    (&!= $t_1 $t_2) "注册; 从空的钩子列表出发, 记"
    (_ $s (Mn "12")) "为先注册" (_ $f $1) "再注册" (_ $f $2) "所得的状态, "
    (_ $s (Mn "21")) "为相反顺序所得的状态. 则对每个可容许的" $R ", "
    (&neg (appl $R (_ $s (Mn "12")) (_ $s (Mn "21"))))
    ". 换言之, 不论取哪个可容许的等价关系, 两次注册都不可交换.")
   ((proof)
    "设两种顺序得到的状态被某个可容许的等价关系联系起来. 由上一个定理, "
    "它们不可区分. 但只含一个分发字母的测试按注册顺序返回回调的列表, "
    "两种顺序给出的列表不同, 矛盾.")
   (P "按注册次序返回回调的分发会暴露注册的顺序. 所以要让注册可交换, 就不能保留这样的分发: 或者像"
      (Ref "ex-bus") "末尾那样去掉分发, 改用监听器注册, 或者让分发只返回回调的多重集. "
      "这是被迫的, 而不是随意的选择.")

   (Appendix "C" #:id "sec-compare" "与论文的对照")
   (P "对照的是arXiv:2608.25512v1, 下文的编号都按这一版. "
      "关系一栏: 忠实指与论文的陈述一致; 改写指对应物形式不同; "
      "更强, 更弱指结论或假设比论文强或弱; 部分指只对应其中一部分; 新增指论文中没有对应物; "
      "无对应指本文的论证路线用不到它们.")
   (Tab (HRow "论文" "本文" "关系")
        (Row "定义8, 9, 定理10(1), 11(1), 16" (Ref "sec-effects") "忠实, 改为部分函数并在等价意义下陈述")
        (Row "定义1–3, 6, 12, 17, 18, 40, 定理4, 5, 7, 10(2), 11(2), 13–15, 引理41" -- "无对应: 扭曲复合, track/recover, 效应函数变换effect, 效应迭代器, 变换幺半群")
        (Row "定义42, 定理43" (Span (Ref "def-revert-indep") ", " (Ref "thm-omission")) "改写: 撤销独立只取定义42中所需的部分, 假设更弱; 只对应定理43证明中略去一个效应的两步, 任意顺序的撤销本身用不到")
        (Row "定义19–20, 式(31)" (Span (Ref "sec-programs") ", " (Ref "sec-state") ", " (Ref "sec-effects")) (Span "改写: 演算中由绑定承载; 附录A中的" $setk "与提升同论文, 只是逆取为全函数"))
        (Row "定义29" (Ref "sec-service") "改写: 单一服务, 多一个令牌, 逆是全函数, 等价关系作为服务的一部分给出")
        (Row "定义21–22" -- "无对应: 论文第4章的规则也不用notify")
        (Row "定义53" (Ref "sec-derived") "忠实: 视图记录提供编号而非提供者")
        (Row "定义23, 26–28" -- "无对应: 未讨论")
        (Row "定义24–25" (Ref "sec-meta-derived") "改写: 键重命名")
        (Row "定义33" (Ref "sec-effects") "部分: 只取全部键上的等价")
        (Row "定义31, 引理32" (Ref "sec-observe") "改写: 论文以不可区分定义等价关系, 本文证明可容许的等价关系都包含于不可区分")
        (Row "定义34–37, 引理35, 38" (Ref "sec-effects") "部分: 只需全部键上复合与绑定的封闭性")
        (Row "定义30" (Ref "sec-programs") "改写: 按键集索引的归纳类型")
        (Row "定义44" (Span (Ref "def-revert-indep") ", " (Ref "def-confhyps")) (Span "更强的假设: 定义44蕴涵(H), 加上合流假设中的" (Q "良好") ", " (Q "逆与状态无关") "和" (Q "令牌无关") "蕴涵合流假设 (" (Ref "sec-rocq") ")"))
        (Row "定义46" -- "无对应: 论文要求每个键都可交换, 本文只对可能相遇的操作对提出假设")
        (Row "定理45, 47" -- "无对应: 演算直接以操作级的假设陈述")
        (Row "4.1–4.2节" (Span (Ref "sec-state") ", " (Ref "sec-rules")) "改写")
        (Row "4.4节Asynchrony" (Ref "sec-rules") (Span "更强: 覆盖的执行更多. 落地由" $L-Step "承担, 可以落地任意多步, 论文的异步宿主只落地一次迭代"))
        (Row "4.4节Failure" (Ref "sec-rules") (Span "改写: 失败在本文的核心中, 论文作为扩展; 重试用" $O-Reset ", 论文用修订"))
        (Row "4.4节Isolation" (Ref "sec-meta-derived") "改写: 键重命名, 与论文把键读成 (键, 领域) 对同一思路")
        (Row "4.4节Configuration" (Ref "sec-rules") (Span "改写: 重启是一条规则" $O-Restart ", 论文是规则的组合"))
        (Row "定理64" (Ref "thm-preservation") "更强: 不变式更多")
        (Row "定理68, 推论69" (Ref "thm-recovery") "改写: 比较的时点与略去的步骤不同, 假设更弱")
        (Row "定理70" (Span (Ref "thm-outlive") ", " (Ref "thm-ordering") ", " (Ref "thm-moves")) "改写: 第3条中的声明该键读作声明或提供该键")
        (Row "定理71" (Ref "thm-ordering") (Span "部分: 只对应式(58), 且只对收尾一步成立; 其余的步可能是落地, 对应论文" $L-Divert "的落地分支, 两边都按已经不成立的解析执行"))
        (Row "定理73" (Span (Ref "thm-progress") ", " (Ref "thm-termination")) "更弱: 没有定量界")
        (Row "引理59–62, 66, 67, 78, 79" -- "无对应")
        (Row "定理80(1)" (Ref "thm-dynamic") "更弱")
        (Row "定理80(2)" (Ref "thm-dynamic") (Span "部分: 不含" $O-React))
        (Row -- (Ref "sec-rules") (Span "新增: " $O-React))
        (Row -- (Ref "sec-observe") "新增: 有序事件总线的两次注册不可交换"))
   (P "下面按正文的顺序列出与论文的具体差别.")
   (Ul (Li (Ref "sec-service") ": 论文的定义29把键" $k "上的余效应定义为二元组 (值类型, 操作集合), 每个键一个, "
           "等价关系由操作生成的测试导出 (定义31). 本文整理成一个记录, 有三处不同: 多了令牌参数; "
           "等价关系作为记录的一部分给出 (附录B说明它总包含于不可区分); 全系统只有一个服务, "
           "需要多种服务时取和. 操作的形状也与论文的式(30)不同. 式(30)里有两个部分函数: "
           "操作本身, 以及它给出的逆. 本文只保留外层的部分性, 用" $Maybe "表示; 逆的类型是"
           (&= (&End $St) (&-> $St $St)) ", 是全函数. 另外, 论文的参数类型"
           $X_a "在本文中并进了操作类型" $Op ". 论文5.1.1节说, 逆是否撤销了它所伴随的效应, "
           "键上发布的操作是否可交换 (定义46的见证), 都是作者的义务, 运行时不检查; "
           "本文同样把它们作为服务的规格, 落在服务作者一侧, 不在演算中检查.")
       (Li (Ref "sec-programs") ": 论文的定义30把经上下文的迭代器刻画为"
           (Q "每一段执行一个操作或一次提供, 再根据结果选择后续") "的闭包, 本文直接做成归纳类型.")
       (Li (Ref "sec-state") ", " (Ref "sec-rules") ": 论文的演算直接以效应迭代器给出 (4.2节), "
           "再在4.4节把异步, 失败, 隔离和配置作为扩展; 本文把失败放进核心. 设计决定中与论文的差别有: "
           "只有一个移除请求, 条目由" $L-Remove "自动删除, 代替论文的Retire加Remove; "
           "累加器逐项撤销, 论文一步应用整个累加器; 论文" $L-Divert "的两支 (放弃在途迭代, 让它落地) 都保留, "
           "但落地由" $L-Step "承担: 目标视图改变之后, 除收尾外的各步照样可走, 可以落地任意多步, "
           "论文的异步宿主只落地一次迭代 (4.4节); 增加" $O-Reset
           ", 论文中失败的fiber要经修订 (退役, 移除, 重新插入) 才能重试; 父fiber撤销到"
           (appl $USpawn $m) "时等待子fiber被移除, 论文中父fiber不等待 (定义52); "
           "卸载的守卫按fiber整体判断, 与论文的守卫 (定义54) 一样, 只是" (Q "是否仍被依赖")
           "按提供编号判断. 相同的一处是" $O-Insert ": 它要求新组件不冲突, 与论文" $O-Insert
           "的最后一个前提相同 (定义48, 4.2.1节), Cordis则到提供时才报冲突.")
       (Li (Ref "sec-derived") ": " $target "就是论文定义53的目标视图, 只是视图记录提供编号而非提供者. "
           "论文第4章的规则同样以视图与目标视图的比较为前提; 定义22的notify (把迁移分类为activating, deactivating, neutral) "
           "只出现在3.2.2节, 本文不需要它.")
       (Li (Ref "thm-preservation") ": 论文的保持性 (定理64) 只有4条不变式.")
       (Li (Ref "thm-ordering") ": 论文定理70的第3条说, 在依赖者的episode中, 提供者在该键上的表项存在, "
           "并且只被声明该键的fiber在该键上的操作改变. 前半由" (Ref "thm-outlive") "给出, 后半由" (Ref "thm-moves")
           "给出, 只是本文的fiber也可以操作自己提供的键, 所以读作声明或提供该键. "
           "本文的服务状态按设计是共享且可变的, 固定的是提供的身份.")
       (Li (Ref "thm-recovery") ": 论文的定理68不另设独立性的前提: 定义46要求每个键都可交换, "
           "由此引理66证明每次执行中的步都两两独立 (定义65), 即一方的每个正向映射和逆都与另一方的可交换. "
           "本文只要求可能相遇的操作对撤销独立, 不要求正向可交换, "
           "并且覆盖运行期效应. 两者对同一fiber内部都不需要假设, 都按LIFO撤销. 比较的对象也不同: "
           "论文的定理68在" $n "的episode中途比较, 把" $n "当前的累加器施加于当前状态, 与略去" $n
           "的步骤后从episode起点执行其他步骤所得的状态相比; 本文要求终点" $n
           "没有待撤销操作, 比较的影子存储只略去" $n "的服务操作及其撤销, 保留它的提供及撤销提供.")
       (Li (Ref "thm-termination") ": 论文的定理73假设执行涉及的名字集合有限, 并给出每个fiber的步数的定量界; "
           "本文以有界的秩和程序路径长度证明生命周期执行终止, 没有定量的界.")
       (Li (Ref "thm-control") ": 关于" $Done "路径的前提是论文的定义76. 论文的引理75, 77 (支持集) "
           "只在无spawn的控制层有对应, 有spawn时由规范名对应.")
       (Li (Ref "thm-dynamic") ": 论文的定理80(1)构造了一个规范执行: 支持集中的每个fiber取一个episode (一次从加载到卸载的生命期), "
           "按支持关系 (提供与注册) 的线性化排列; "
           "本文只断言终态与静态组装一致, 并把静态组装能否到达干净的状态作为前提. "
           "论文的合流同样排除失败的fiber (4.4节).")
       (Li (Ref "sec-meta-derived") ": 隔离不需要论文定义24的领域表, 与论文4.4节把键读成 (键, 领域) 对是同一思路. "
           "论文的拦截 (定义26, 27) 视为传给服务的数据, 不影响生命周期, 本文不讨论."))

   (Appendix "D" #:id "sec-rocq" "Rocq形式化")
   (P "本文的演算和各定理在Rocq中形式化 (" (Code "theories") "目录), 其中没有" (Code "Admitted")
      ". 形式化引入了函数外延性, 但只有" (Code "EffectFun.v") "和" (Code "Twisted.v")
      "中几个关于论文3.1节其余构造 (扭曲复合, 效应函数变换等) 的引理依赖它, 正文不用这些构造, 下表列出的定理都不依赖任何公理. "
      "定理的前提写成节变量或显式参数. 正文的证明是这些形式证明的概要, 下表给出对应关系.")
   (Tab (HRow "本文" "Rocq" "文件")
        (Row (Ref "thm-preservation") (Code "preservation, wf_reachable") (Code "Preservation.v"))
        (Row (Ref "thm-outlive") (Code "provider_outlives") (Code "Ordering.v"))
        (Row (Ref "thm-ordering") (Code "begin_requires_deps, unload_waits, …") (Code "Ordering.v"))
        (Row (Ref "thm-moves") (Code "value_moves, decl_reachable") (Code "Ordering.v"))
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
        (Row (Ref "thm-scoped") (Code "scoped_bounded, scoped_total, scoped_above, scoped_rank, scoped_nospawn, scoped_copair") (Code "Derived.v"))
        (Row (Ref "cor-scoped") (Code "scoped_ind") (Code "Derived.v"))
        (Row (Ref "thm-checked") (Code "checked_bounded, checked_total") (Code "Derived.v"))
        (Row (Ref "thm-summary") (Code "summary_wf, summary_live, summary_recovery, summary_confluence, summary_static") (Code "Summary.v"))
        (Row (Ref "thm-closure") (Code "witnessed_submonoid, pdiamond_witnessed, pbind_witnessed") (Span (Code "EffectFun.v") ", " (Code "Bridge.v")))
        (Row (Ref "thm-denote") (Code "denote_witnessed") (Code "Bridge.v"))
        (Row (Ref "thm-lone") (Code "lone_denote") (Code "Bridge.v"))
        (Row (Ref "sec-observe") (Code "indist_admissible, admissible_indist, wb_indist, bus_no_commute") (Code "Observe.v")))
   (P "几点说明.")
   (Ul (Li "恢复精确性证明中的两个不变式在Rocq中分别是" (Code "R") "和" (Code "Prov")
           "; 把撤销独立的两条 (" (Q "不在乎撤销") "和" (Q "逆可交换") ") 推广到一串逆的引理是" (Code "shifted_all") "和" (Code "commute_all") ".")
       (Li "合流假设 (" (Ref "def-confhyps") ") 是" (Code "Compose.v") "中的" (Code "ConfHyps")
           ", " (Q "相关可交换") "中的状态集合是" (Code "ServiceSeq.v") "中归纳定义的" (Code "After") ".")
       (Li "终止证明中的度量是" (Code "Termination.v") "中的" (Code "mu") "和" (Code "Phi")
           ", 字典序是" (Code "lexR") ".")
       (Li "服务状态合流证明中的理想轨迹是" (Code "Episode.v") "中的" (Code "Tr") ".")
       (Li (Code "Bridge.v") "还证明了: 论文的定义44蕴涵本文的假设(H) (" (Code "paper_hind")
           "); 定义44加上合流假设中的" (Q "良好") ", " (Q "逆与状态无关") "和" (Q "令牌无关") "蕴涵合流假设 (" (Code "paper_confhyps")
           "), 定义44本身只给出" (Q "相关可交换") ".")
       (Li "形式化中的程序不按键集索引 (" (Code "Calculus.v") "中的" (Code "Prog")
           "), 键的限制是" $L-Step "中的检查: 操作的键不在" $d "中且不归自己提供时带错误"
           $EUnresolved ", 提供的键不在" $p "中时带错误" $EConflict
           ". 正文按键集索引的程序是其特例: 对它们, 操作的键总在" (&union $d $p)
           "中, 提供的键总在" $p "中.")
       (Li "进展, 终止和合流的形式证明以可达状态满足的若干不变式为前提 ("
           (Code "Inv2") ", " (Code "TInv") ", " (Code "CInv") ", " (Code "PInv") ", " (Code "SLInv")
           "), 正文直接陈述为可达状态; 对应的推论是" (Code "progress_reachable")
           ", " (Code "termination_reachable") ", 以及" (Code "Dynamic.v") "中的" (Code "drun_inv") ".")
       (Li (Ref "cor-scoped") "的第2条在Rocq中是" (Code "scoped_ind") "; 第3条 (合流假设的传递) 只在正文中证明.")
       (Li "形式化的覆盖范围与正文一致: 合流的结果都不含" $O-React ", 只允许对根fiber用" $O-Dispose
           "; 控制层和服务状态两层假设没有spawn; " (Ref "thm-lone") "只对单独加载的fiber成立."))
   )))
