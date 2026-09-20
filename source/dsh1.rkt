#lang racket
(provide dsh1.html)
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
(define $twisted $T:fraktur)
(define (&twisted Γ)
  (_ $twisted Γ))
(define $partial
  (Mo "&part;"
      #:attr* '((lspace "0") (rspace "0"))))
(define (&partial Γ)
  (ap $partial Γ))
(define $track (Mi "track"))
(define (&track f g)
  (appl $track f g))
(define $recover (Mi "recover"))
(define $eff $E:fraktur)
(define (&eff Γ) (_ $eff Γ))
(define (&eff* Γ)
  (_^ $eff Γ $*))
(define $effect (Mi "effect"))
(define (&effect e)
  (app $effect e))
(define $eta_Γ (_ $eta Γ))
(define $ulcorner
  (Mo "&ulcorner;" #:attr* '((stretchy "false") (lspace "0") (rspace "0"))))
(define $urcorner
  (Mo "&urcorner;" #:attr* '((stretchy "false") (lspace "0") (rspace "0"))))
(define (Induced f g)
  (: $ulcorner (&cm f g) $urcorner))
(define $Gen $G:fraktur)
(define $tsmonoid $M:fraktur)
(define (&tsmonoid e)
  (app $tsmonoid e))
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
(define $shadow (^^ $sigma (Mo "&Hat;")))
(define (&settled γ) (app $settled γ))
(define (&quiet γ) (app $quiet γ))
(define (Steps γ γ^) (: γ (^ $--> $*) γ^))
(define $_ (Mi "_"))
(define $dummy (Mi "&minus;"))
(define $prog (Mi "prog"))
(define $step (Mi "step"))
(define $Label (Mi "Label"))
(define $WF (Mi "WF"))
(define $EUser (Rn "EUser"))
(define $EConflict (Rn "EConflict"))
(define $Register (Mi "Register"))
(define $Lookup (Mi "Lookup"))
(define dsh1.html
  (TnTmPrelude
   #:title "deepseek harness笔记1"
   #:css "styles.css"
   (H1. "deepseek harness笔记1")
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
           "则整个系统满足生命周期性质."))
   (P "第1章讲效应怎样撤销, 第2章到第5章讲组件怎样通过服务相互依赖, "
      "第6章到第9章给出生命周期演算及其元理论, 第10章逐条对照论文.")

   (H2. "可撤销效应" #:id "sec-effects")
   (P "这一章对应论文的3.1节. "
      "本章用严格相等和全函数, 与论文的陈述一致. 出发点是: 插件卸载时, "
      "它对共享状态做过的每件事都必须撤掉, 所以每个效应都要带着它的逆. "
      "下面固定一个类型" Γ ", 作为世界的状态, 并记" (&:= (&End Γ) (&-> Γ Γ)) ".")
   (H3. "效应上下文")
   ((Definition)
    "定义" (&:= (&twisted Γ) (&c* (&End Γ) (&End Γ)))
    ", 其中的元素" (tu0 $f $g) "由前进函数" $f "和撤销函数" $g "组成. 复合定义为"
    (MB (&:= (&compose (tu0 $f_1 $g_1) (tu0 $f_2 $g_2))
             (tu0 (&compose $f_1 $f_2) (&compose $g_2 $g_1))) ".")
    "前进部分按通常的顺序复合, 撤销部分倒过来: 后做的事先撤. ")
   ((Proposition)
    (&twisted Γ) "在上述复合下是幺半群, 单位元是" (tu0 $id $id) ". ")
   ((Definition)
    "定义效应上下文" (&:= (&partial Γ) (&c* Γ (&End Γ)))
    ". 其中的元素" (tu0 $gamma $phi) "由当前状态" $gamma "和累加器" $phi
    "组成, " $phi "的用意是把一切撤回到起点. 定义"
    (&: $track (&-> (&twisted Γ) (&End (&partial Γ)))) "为"
    (MB (&:= (appl (&track $f $g) $gamma $phi)
             (tu0 (app $f $gamma) (&compose $phi $g))) ",")
    "定义" (&: $recover (&End (&partial Γ))) "为"
    (MB (&:= (appl $recover $gamma $phi)
             (tu0 (app $phi $gamma) $id)) ".")
    $track "在前进的同时把" $g "接到累加器的内侧, " $recover "一次用掉累加器. ")
   ((Proposition)
    $track "是幺半群同态, 并且"
    (&= (&compose (&pr $1) (&track $f $g)) (&compose $f (&pr $1)))
    ". ")
   ((Theorem #:id "thm-recover-track")
    "若" (&= (app $g (app $f $gamma)) $gamma) ", 则"
    (&= (app $recover (appl (&track $f $g) $gamma $phi))
        (appl $recover $gamma $phi))
    ". 特别地, 若" (&= (&compose $g $f) $id) ", 则"
    (&= (&compose $recover (&track $f $g)) $recover) ". ")
   ((proof)
    "直接计算:"
    (MB (eqn*
         ((app $recover (appl (&track $f $g) $gamma $phi))
          $= (appl $recover (app $f $gamma) (&compose $phi $g)))
         ($ $= (tu0 (app $phi (app $g (app $f $gamma))) $id))
         ($ $= (tu0 (app $phi $gamma) $id))
         ($ $= (appl $recover $gamma $phi))))
    "第三个等号用到了前提. 若" (&= (&compose $g $f) $id)
    ", 则前提对每个" $gamma "都成立.")
   (P "注意前提只要求" $g "在" $f "被应用的那一点撤销它, 不要求" $g "是全局的左逆.")
   ((Theorem #:id "thm-recover-tracks")
    "设" (∈ (tu0 $f_1 $g_1) $..h (tu0 $f_n $g_n) (&twisted Γ))
    ", 从" $gamma_0 "出发令" (&:= $gamma_i (app $f_i (_ $gamma (&- $i $1))))
    ", 并设每个" (&= (app $g_i $gamma_i) (_ $gamma (&- $i $1)))
    ". 则对任意" $phi
    (MB (&= (app $recover (appl (@compose (&track $f_n $g_n) $..c (&track $f_1 $g_1))
                                $gamma_0 $phi))
            (appl $recover $gamma_0 $phi)) ".")
    "特别地, 取" (&= $phi $id) "时右边就是" (tu0 $gamma_0 $id)
    ". 这是论文的式(11). ")
   ((proof)
    "对" $n "归纳. " (&= $n $0) "时没有什么要证明的. 设结论对" (&- $n $1)
    "个对子成立. 第一个对子把" (tu0 $gamma_0 $phi) "送到"
    (tu0 $gamma_1 (&compose $phi $g_1))
    ", 剩下的" (&- $n $1) "个对子从" $gamma_1
    "出发同样满足前提, 由归纳假设, 依次track它们再recover的结果是"
    (appl $recover $gamma_1 (&compose $phi $g_1))
    ". 再由" (Ref "thm-recover-track") ", 它等于" (appl $recover $gamma_0 $phi) ".")
   ((Definition)
    "称" (∈ (tu0 $gamma $phi) (&partial Γ)) "相对" $gamma_0 "健全, 如果"
    (&= (app $phi $gamma) $gamma_0)
    ". 健全等价于" (&= (appl $recover $gamma $phi) (tu0 $gamma_0 $id))
    "; 并且若" (tu0 $gamma $phi) "健全且" (&= (app $g (app $f $gamma)) $gamma)
    ", 则" (appl (&track $f $g) $gamma $phi) "也健全. ")
   (H3. "带见证的效应函数")
   (P "对子" (tu0 $f $g) "的撤销函数是固定的. 但很多效应的逆依赖于起点: "
      (Q "把" $x "设为5") "的逆是" (Q "把" $x "改回原来的值")
      ", 原来的值要看效应作用前的状态. 所以逆应该作为效应的输出.")
   ((Definition)
    "定义" (&:= (&eff Γ) (&-> Γ (&partial Γ)))
    ", 其中的元素称为效应函数: " (&= (app $e $gamma) (tu0 $delta $g))
    "给出新状态" $delta "和这一步的逆" $g ". 带见证的效应函数构成"
    (MB (&:= (&eff* Γ)
             (setI (∈ $e (&eff Γ))
                   (∀ $gamma (Let (&:= (tu0 $delta $g) (app $e $gamma))
                                  (&= (app $g $delta) $gamma))))) ".")
    "每个" (∈ (tu0 $f $g) (&twisted Γ)) "导出"
    (&:= (Induced $f $g) (@\|-> $gamma (tu0 (app $f $gamma) $g)))
    "; 若" (&= (&compose $g $f) $id) ", 则" (∈ (Induced $f $g) (&eff* Γ)) ". ")
   ((Definition)
    "对于" (∈ $e_1 $e_2 (&eff Γ)) ", 定义先" $e_2 "后" $e_1 "的复合"
    (MB (&:= (app (@diamond $e_1 $e_2) $gamma)
             (Let (&:= (tu0 $delta $s) (app $e_2 $gamma))
                  (Let (&:= (tu0 $epsilon $t) (app $e_1 $delta))
                       (tu0 $epsilon (&compose $s $t))))) ",")
    "以及" (&:= (app $eta_Γ $gamma) (tu0 $gamma $id))
    ". 这就是幺半群" (tu0 (&End Γ) $compose) "上的writer monad的Kleisli复合: "
    "日志是函数, 新的一项接在右边, 也就是最先被施加. ")
   ((Theorem)
    (&eff Γ) "在" $diamond "下是幺半群, 单位元是" $eta_Γ "; "
    (&: (Induced $dummy $dummy) (&-> (&twisted Γ) (&eff Γ))) "是幺半群同态; "
    (&eff* Γ) "是子幺半群. 这是论文的定理10, 11. ")
   ((proof)
    "结合律和单位律逐点展开定义即可. 对同态, "
    (&= (app (@diamond (Induced $f_1 $g_1) (Induced $f_2 $g_2)) $gamma)
        (tu0 (app $f_1 (app $f_2 $gamma)) (&compose $g_2 $g_1)))
    ", 正好是扭曲复合. 对子幺半群, " $eta_Γ "显然带见证. 设" $e_1 "和" $e_2
    "带见证, 令" (&= (tu0 $delta $s) (app $e_2 $gamma)) ", "
    (&= (tu0 $epsilon $t) (app $e_1 $delta)) ", 则"
    (MB (&= (app (@compose $s $t) $epsilon) (app $s (app $t $epsilon))
            (app $s $delta) $gamma) ",")
    "所以" (&diamond $e_1 $e_2) "也带见证.")
   ((Definition)
    "定义提升" (&: $effect (&-> (&eff Γ) (&eff (&partial Γ)))) "为"
    (MB (&:= (appl (&effect $e) $gamma $phi)
             (Let (&:= (tu0 $delta $g) (app $e $gamma))
                  (tu0 (tu0 $delta (&compose $phi $g))
                       (&track $g (&compose (&pr $1) $e))))) ".")
    "它把效应搬到带累加器的世界上: 状态前进, 累加器接上这一步的逆, "
    "同时给出提升后的逆. ")
   ((Theorem)
    $effect "是幺半群同态:"
    (MB (&cm (&= (&effect (&diamond $e_1 $e_2))
                 (&diamond (&effect $e_1) (&effect $e_2)))
             (&= (&effect $eta_Γ) (_ $eta (&partial Γ)))) ".")
    "并且记" (&:= $f (&compose (&pr $1) $e)) ", "
    (&:= (tu0 $delta $g) (app $e $gamma)) ", "
    (&:= (tu0 Δ $G) (appl (&effect $e) $gamma $phi)) ", 有"
    (MB (&cm (&= (&compose (&pr $1) (&pr $1) (&effect $e)) (&compose $f (&pr $1)))
             (&= (&compose (&pr $1) $G) (&compose $g (&pr $1)))) ",")
    "即提升后的前进函数和逆在第一个分量上分别就是" $f "和" $g
    ". 这是论文的定理13, 14. ")
   ((Theorem)
    "设" (∈ $e (&eff* Γ)) ", " (&:= $f (&compose (&pr $1) $e)) ", "
    (&:= (tu0 $delta $g) (app $e $gamma)) ", "
    (&:= (tu0 Δ $G) (appl (&effect $e) $gamma $phi)) ". 则"
    (MB (&= (app $G Δ) (tu0 $gamma (&compose $phi $g $f))) ".")
    "此外"
    (MB (&<=> (∈ (&effect $e) (&eff* (&partial Γ)))
              (∀ $gamma (&= (&compose (app* (&pr $2) $e $gamma) $f) $id))) ",")
    "即" $e "在每一点给出的逆都是" $f "的全局左逆. 这是论文的定理15. ")
   ((proof)
    "按定义, " (&= $G (&track $g $f)) ", " (&= Δ (tu0 $delta (&compose $phi $g)))
    ", 所以"
    (MB (&= (app $G Δ) (tu0 (app $g $delta) (&compose $phi $g $f))
            (tu0 $gamma (&compose $phi $g $f))) ",")
    "最后一步用到" $e "带见证. 于是" (&effect $e) "带见证当且仅当对所有"
    $gamma "和" $phi "有" (&= (&compose $phi $g $f) $phi)
    ". 取" (&= $phi $id) "即得" (&= (&compose $g $f) $id)
    "; 反过来, 这个等式显然推出前者.")
   ((Theorem #:id "thm-lifo")
    "设" (∈ $e_1 $..h $e_n (&eff* Γ)) ", 记" (&:= $E (&diamond $e_n $..c $e_1))
    ", " (&:= (tu0 $gamma_n $s) (app $E $gamma_0)) ". 从" (tu0 $gamma_0 $id)
    "出发依次运行" (&effect $e_1) ", " $..h ", " (&effect $e_n) ", 则"
    (MB (&cm (&= (app (&pr $1) (appl (@diamond (&effect $e_n) $..c (&effect $e_1))
                                     $gamma_0 $id))
                 (tu0 $gamma_n $s))
             (&= (app $s $gamma_n) $gamma_0)) ",")
    "并且途中每一步都相对" $gamma_0 "健全. 其中" (&= $s (&compose $g_1 $..c $g_n))
    ", " $g_i "是" $e_i "在第" $i "步给出的逆. 换言之, 按LIFO顺序撤销是精确的, "
    "不需要任何关于效应之间关系的假设. 这是论文的定理16. ")
   ((proof)
    "因为" $effect "是同态, 依次运行" (&effect $e_1) ", " $..h ", " (&effect $e_n)
    "就是运行" (&effect $E) ", 其中" (&:= $E (&diamond $e_n $..c $e_1))
    ". 由封闭性, " $E "带见证. 记" (&= (tu0 $gamma_n $s) (app $E $gamma_0))
    ", 按" $effect "的定义, 到达的状态是" (tu0 $gamma_n (&compose $id $s))
    ", 而" (&= (app $s $gamma_n) $gamma_0) "正是" $E "的见证条件. "
    "对每个前缀" (&diamond $e_i $..c $e_1) "做同样的论证, 就得到每一步都健全. "
    "注意整个论证只用到了每个" $e_i "各自带见证.")
   (H3. "效应的独立性")
   (P "LIFO总是可行的, 但组件的卸载顺序由系统决定, 不一定是加载顺序的逆. "
      "要按别的顺序撤销, 效应之间必须互不干扰.")
   ((Definition)
    "对于" (∈ $e (&eff Γ)) ", 定义生成元集合"
    (MB (&:= (app $Gen $e)
             (&union (setE (&compose (&pr $1) $e))
                     (setI (app* (&pr $2) $e $gamma) (∈ $gamma Γ)))) ",")
    "以及变换幺半群" (&:= (&tsmonoid $e) (ang0 (app $Gen $e)))
    ", 即" (&End Γ) "中由" (app $Gen $e) "生成的子幺半群. 对"
    (∈ (tu0 $f $g) (&twisted Γ)) "有" (&= (app $Gen (Induced $f $g)) (setE $f $g)) ". ")
   ((Lemma)
    (Ol (Li "对" (&sube (&cm $S $T) (&End Γ)) ", 若"
            (∀ (∈ $a $S) (∀ (∈ $b $T) (&= (&compose $a $b) (&compose $b $a))))
            ", 则"
            (∀ (∈ $a (ang0 $S)) (∀ (∈ $b (ang0 $T)) (&= (&compose $a $b) (&compose $b $a))))
            ";")
        (Li (&sube (&tsmonoid (&diamond $e_1 $e_2))
                   (ang0 (&union (app $Gen $e_1) (app $Gen $e_2))))
            "."))
    "这是论文的引理18. ")
   ((proof)
    "(1) 对固定的" $b ", 与" $b "交换的元素之集合包含" $id
    "且对复合封闭, 是一个子幺半群. 所以若它包含一个生成元集合, 就包含其生成的子幺半群. "
    "把这个论证对两边各用一次即可. "
    "(2) " (&compose (&pr $1) (@diamond $e_1 $e_2))
    "等于" (&compose (&pr $1) $e_1) "与" (&compose (&pr $1) $e_2)
    "的复合; " (&diamond $e_1 $e_2) "在" $gamma "处给出的逆是" $e_2 "在" $gamma
    "处和" $e_1 "在某点处给出的逆的复合. 所以" (&diamond $e_1 $e_2)
    "的生成元都落在右边的子幺半群里, 由它们生成的" (&tsmonoid (&diamond $e_1 $e_2))
    "也是.")
   ((Definition #:id "def-independent")
    $e_1 "和" $e_2 "独立, 如果"
    (Ol (Li (&tsmonoid $e_1) "与" (&tsmonoid $e_2) "的元素两两交换;")
        (Li "对每个" (∈ $h (&tsmonoid $e_2)) "和" $gamma ", "
            (&= (app* (&pr $2) $e_1 (app $h $gamma)) (app* (&pr $2) $e_1 $gamma))
            ", 即" $e_2 "的变换不改变" $e_1 "给出的逆; 反之亦然."))
    "对" (Induced $f_1 $g_1) "和" (Induced $f_2 $g_2)
    ", 第一条就是四对函数两两交换, 第二条自动成立. 这是论文的定义19. ")
   ((Theorem)
    "设" (∈ $e_1 $..h $e_n (&eff* Γ)) "两两独立, " (∈ $gamma_0 Γ) ". 记"
    (MB (eqn*
         ($f_i $:= (&compose (&pr $1) $e_i))
         ($gamma_i $:= (app $f_i (_ $gamma (&- $i $1))))
         ($g_i $:= (app* (&pr $2) $e_i (_ $gamma (&- $i $1))))))
    "固定" (∈ $j (setE $1 $..h $n)) ", 用" (_prime $gamma $i)
    "表示从序列中略去" $e_j "后的状态: " (&:= (_prime $gamma $j) (_ $gamma (&- $j $1)))
    ", 对" (&< $j $i) "令" (&:= (_prime $gamma $i) (app $f_i (_prime $gamma (&- $i $1))))
    ". 那么对" (&<= $j $i $n) "有"
    (MB (&cm (&= $gamma_i (app $f_j (_prime $gamma $i)))
             (&= (app $g_j $gamma_i) (_prime $gamma $i))))
    "并且对" (: $j $< $i $<= $n) "有"
    (MB (&= (app* (&pr $2) $e_i (_prime $gamma (&- $i $1))) $g_i) ".")
    "换言之, 略去" $e_j "后, 每一步施加" $f_j "就回到原序列的状态, 施加" $g_j
    "就回到略去后的状态, 而之后每个效应给出的逆不变. 这是论文的定理20. ")
   ((proof)
    "对" $i "归纳. " (&= $i $j) "时, " (&= $gamma_j (app $f_j (_ $gamma (&- $j $1))))
    "是定义, " (&= (app $g_j $gamma_j) (_ $gamma (&- $j $1))) "是" $e_j
    "的见证条件. 归纳步:"
    (MB (eqn*
         ((_ $gamma (&+ $i $1)) $= (app (_ $f (&+ $i $1)) (app $f_j (_prime $gamma $i))))
         ($ $= (app $f_j (app (_ $f (&+ $i $1)) (_prime $gamma $i))))
         ($ $= (app $f_j (_prime $gamma (&+ $i $1))))))
    "第二个等号用到" (∈ (_ $f (&+ $i $1)) (&tsmonoid (_ $e (&+ $i $1))))
    "与" (∈ $f_j (&tsmonoid $e_j)) "交换; 同理, " (∈ $g_j (&tsmonoid $e_j))
    "与" (_ $f (&+ $i $1)) "交换给出第二式. 至于逆不变:"
    (MB (&= $g_i (app* (&pr $2) $e_i (_ $gamma (&- $i $1)))
            (app* (&pr $2) $e_i (app $f_j (_prime $gamma (&- $i $1))))
            (app* (&pr $2) $e_i (_prime $gamma (&- $i $1)))))
    "最后一步正是独立性的第二条, 取" (∈ (&= $h $f_j) (&tsmonoid $e_j)) ".")
   ((Corollary)
    "在同样的假设下, 对" (setE $1 $..h $n) "的任意排列" $pi ", "
    (MB (&= (app (@compose (_ $g (app $pi $n)) $..c (_ $g (app $pi $1))) $gamma_n)
            $gamma_0) ",")
    "即在终点按任意顺序施加这" $n "个逆都回到" $gamma_0 ". 这是论文的推论21. ")
   ((proof)
    "把上述定理从略去一个效应推广到略去任意一个集合" $R
    "中的效应, 证明相同. 于是在略去" $R "的终点施加" $e_j "的逆 (" (&!in $j $R)
    "), 得到的是略去" (&union $R (setE $j)) "的终点. 按排列依次施加, 最后得到略去全部"
    $n "个效应的终点, 也就是" $gamma_0 ".")
   (P "演算并不直接使用这条推论. 它假设了效应的前进函数也两两交换, "
      "而恢复精确性需要的比这少, 只要求撤销独立; 见" (Ref "sec-service")
      "和" (Ref "sec-recovery") ".")

   (H2. "在等价意义下, 部分地读" #:id "sec-partial")
   (P "论文3.3.2节指出, 3.1节中的" (Q "恢复到原状态")
      "是理想化的: free不会恢复malloc之前的堆布局, 被撤销的新鲜名字下次也不会再用. "
      "所以等式都要读成某个等价关系. 另外, set这类操作带有前条件, "
      "效应函数应该是部分的. 这一章把两件事一起做.")
   ((Definition)
    "设" Γ "带有等价关系" $approx ". 定义"
    (MB (&:= (&PEff Γ) (&-> Γ (&Maybe (@c* Γ (&End Γ))))) ".")
    "两个结果相关, 当且仅当都是" $None ", 或者状态相关且逆逐点相关. 一个映射"
    $g "尊重" $approx ", 当且仅当" (&=> (&approx $x $y) (&approx (app $g $x) (app $g $y)))
    ". ")
   ((Definition #:id "def-pwitnessed")
    (∈ $e (&PEff Γ)) "带见证, 当且仅当"
    (Ol (Li "若" (&= (app $e $gamma) (tu0 $delta $g)) ", 则"
            (&approx (app $g $delta) $gamma) ";")
        (Li "上述" $g "尊重" $approx ";")
        (Li $e "本身尊重" $approx ": 相关的输入给出相关的结果."))
    "这是论文定义37在" $Maybe "中的读法. ")
   ((Definition)
    "部分的" $diamond "与全函数版本相同, 只是任一步为" $None "时结果为" $None
    ". 对" (&: $a (&-> Γ (&Maybe (@c* Γ (&End Γ) $B))))
    "和" (&: $kappa (&-> $B (&PEff Γ))) ", 按结果绑定为"
    (MB (&:= (app (&bind $a $kappa) $gamma)
             (Let (&:= (tu0 $delta $g $b) (app $a $gamma))
                  (Let (&:= (tu0 $epsilon $h) (app* $kappa $b $delta))
                       (tu0 $epsilon (&compose $g $h))))) ".")
    "这是论文的式(36): 每一段执行一个操作, 再根据结果选择后续. ")
   ((Theorem)
    "若" $e_1 ", " $e_2 "带见证, 则" (&diamond $e_1 $e_2) "带见证; "
    "若" $a "的前两个分量带见证, " $a "在相关的输入上给出相同的结果" $b
    ", 且每个" (app $kappa $b) "带见证, 则" (&bind $a $kappa)
    "带见证. 取" $approx "为相等且效应为全函数时, " (Ref "def-pwitnessed")
    "恰好退回" (&eff* Γ) ". ")
   ((proof)
    "与全函数的情形相同, 只是每一处等式换成" $approx
    ". 见证条件: 若" (&= (app $e_2 $gamma) (tu0 $delta $s)) ", "
    (&= (app $e_1 $delta) (tu0 $epsilon $t)) ", 则"
    (&approx (app $s (app $t $epsilon)) (app $s $delta) $gamma)
    ", 第一步要求" $s "尊重" $approx ", 这正是定义中的第二条. "
    "复合的逆是尊重" $approx "的映射的复合, 仍然尊重" $approx
    "; 复合本身尊重" $approx "是因为相关的输入在每一段都给出相关的结果. "
    "按结果绑定时, 相关的输入给出相同的结果, 所以选择的是同一个后续, 论证不变.")
   (P "论文的引理38断言3.1节的全部等式都可以读到" $approx
      "下. 这里只给出演算用得到的部分 (上面两条封闭性). "
      "定理20和推论21的" $approx "版本本文不讨论: 演算的恢复和合流不经过它们, "
      "而是直接以操作级的假设证明.")

   (H2. "服务" #:id "sec-service")
   (P "论文的定义24把键" $k "上的余效应定义为三元组, 包含值类型, 等价关系和操作集合. "
      "我们把它整理成一个记录, 称为服务.")
   ((Definition)
    "一个服务" Σ "由以下资料组成: 状态类型" $St ", 其上的等价关系" $simeq
    ", 操作类型" $Op " (参数已经放进操作里), 结果类型" $Out ", 以及"
    (MB (&: $run (&-> $Op $Tok $St (&Maybe (@c* $St (&End $St) $Out)))) ".")
    "其中" (&= $Tok $NN) "是运行时交给这次操作的新鲜令牌, "
    "用于事件钩子这类生成出来的身份 (JS中由对象身份充当). ")
   (P "与论文相比有两处不同. 第一, 多了令牌参数. 第二, 全系统只有一个服务"
      Σ ", 不是每个键一个; 需要多种服务时先取它们的和, "
      "这样避开了依赖类型的映射. 除去" $Maybe "和令牌, 操作的形状"
      (&-> $St (&c* $St (&End $St) $Out))
      "与论文的式(22)相同: 前两个分量是值上的效应函数, 第三个分量是结果. "
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
   ((Definition #:id "def-revert-indep")
    "称" (tu0 $a $t_a) "对" (tu0 $b $t_b) "撤销独立, 如果对" $a "可能给出的每个逆" $g
    (Ol (Li "对每个" $x ", " (&run $b $t_b $x) "与" (&run $b $t_b (app $g $x))
            "都是" $None "; 或者"
            (&= (&run $b $t_b $x) (tu0 $x_1 $h_1 $o)) ", "
            (&= (&run $b $t_b (app $g $x)) (tu0 $x_2 $h_2 $o))
            ", 其中" (&simeq $x_2 (app $g $x_1)) "且"
            (∀ $y (&simeq (app $h_2 $y) (app $h_1 $y))) ";")
        (Li "对" $b "可能给出的每个逆" $h ", "
            (∀ $x (&simeq (app $g (app $h $x)) (app $h (app $g $x)))) "."))
    "第一条说" $b "不在乎" $a "的效应是否已被撤掉, 第二条说两者的逆可以交换. "
    "称它们可交换, 如果对每个" $s ", 先" $a "后" $b "与先" $b "后" $a
    "都失败, 或都成功并给出" $simeq "相关的终态, 且" $a "和" $b "各自的结果相同. ")
   (P "撤销独立正是" (Q "撤掉" $a "的效应而让" $b "的效应留着")
      "所需要的条件; 它不要求" $a "和" $b "的前进函数交换. "
      "恢复精确性只需要撤销独立, 合流还需要可交换.")
   ((Theorem #:id "thm-omission")
    "设" $a "带见证且" (&= (&run $a $t $s_0) (tu0 $s_1 $g $o))
    ". 设" $tau "是一串他人的步骤 (他人的操作, 以及他人的逆), 每一步都与" $g
    "相容 (即满足" (Ref "def-revert-indep") "中相应的一条), 记" (&exec $tau $s) "为从" $s "出发执行"
    $tau "所到达的状态. 那么"
    (MB (&simeq (app $g (&exec $tau $s_1)) (&exec $tau $s_0)) ",")
    "并且" $tau "中他人的操作在两次执行中得到相同的结果. 换言之, 在终点撤销" $a
    ", 等于" $a "从来没有执行过. ")
   ((proof)
    "记实际执行的状态为" $x ", 不执行" $a "的执行的状态为" $y
    ", 对步骤归纳证明" (&simeq (app $g $x) $y) "并且两边他人的操作看到相同的结果. "
    "起点处" (&simeq (app $g $s_1) $s_0) "是" $a "的见证条件. 对他人的操作" $b
    ": 相容性说" $b "在" $x "和" (app $g $x) "上同时失败或同时成功, 后者的后继"
    $simeq "前者的后继再施" $g ", 结果相同; 再由" $b "尊重" $simeq
    ", 把" (app $g $x) "换成" $y "也一样. 对他人的逆" $h ": "
    (&simeq (app $g (app $h $x)) (app $h (app $g $x)) (app $h $y))
    ", 第一步是相容性, 第二步是" $h "尊重" $simeq ".")
   ((Example)
    "按名字索引的注册表 (如" (Code "ctx.tools") "): 状态是名字到可选值的映射, 逐点比较. "
    (appl $Register $x $v) "在" $x "已被占用时失败, 逆是删去" $x "; "
    (app $Lookup $x) "读出" $x "处的值, 逆是恒等. 每个操作都良好. "
    "名字不同的登记, 以及不会遇到同名登记的查询, 满足合流所需的全部假设. ")
   ((Example #:id "ex-bus")
    "有序事件总线 (如" (Code "ctx.on") "): 状态是带令牌的钩子列表. "
    "两次注册不可交换; 按回调注销 (Cordis的旧做法) 也不是精确的逆. "
    "若启动模型中没有分发操作, 并且忽略顺序 (比较多重集), 监听器注册满足全部假设. ")

   (H2. "观察等价" #:id "sec-observe")
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
   (P "分发会暴露注册的顺序. 所以" (Ref "ex-bus")
      "中把分发排除在启动模型之外是被迫的, 而不是随意的选择.")

   (H2. "程序及其解释" #:id "sec-programs")
   (P "论文的定义41把组件的效应函数刻画为: 每一段执行一个操作, 再根据结果选择后续. "
      "我们把这个闭包直接做成一个归纳类型, 并把它解释回" (Ref "sec-partial")
      "的部分效应.")
   ((Definition)
    "程序由以下文法生成:"
    (MB (&::= $P (&\| $Done $Fail (Con $Op $k $a $kappa)
                      (Con $Provide $k $s $P) (Con $Spawn $c $P))) ",")
    "其中" (&: $kappa (&-> $Out $Prog)) ". " (Con $Op $k $a $kappa)
    "在键" $k "的服务状态上执行" $a ", 把结果交给" $kappa "; "
    (Con $Provide $k $s $P) "以初始状态" $s "提供键" $k "; "
    (Con $Spawn $c $P) "注册子组件" $c ". ")
   (P "这就是一个free monad, " $kappa "是续延. 程序是良基的 (每条路径都有限, "
      "但" $kappa "可以无穷分叉, 所以路径长度未必有界), 看不到状态, "
      "能得到的信息只有操作的结果. 名字和provision编号由规则层分配, 程序不接触.")
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
         ((Den (Con $Op $k $a $kappa) $t) $=
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
    "对" $P "的结构归纳. " $Done "和" $Fail "的情形是平凡的. "
    (&setk $k $s) "带见证: 在" (&!in $k (&dom $sigma)) "时, "
    (&= (&\\ (par0 (ext $sigma $k $s)) $k) $sigma)
    "; 它的逆尊重存储的等价, 因为存储是逐键比较的. "
    (&lift $k $a $t) "带见证: 它的逆只改动键" $k
    ", 在该键上就是操作" $a "的逆, 由" $a "良好即得. "
    "于是" $Op "的情形由按结果绑定的封闭性得到, " $Provide "的情形由"
    $diamond "的封闭性得到, " $Spawn "的情形就是归纳假设.")
   ((Theorem)
    "解释与演算的步骤一致: 一个操作步或provide步恰好是解释的一段, "
    "它的撤销项施加这一段的逆. 特别地, 设组件" $c "的fiber从存储" $sigma
    "出发单独 (没有其他fiber走步) 加载到Active, 开始时计数器为" $t ", 则"
    (&= (app (Den (app $body $c) $t) $sigma) (tu0 $sigma^ $g))
    "有定义, 结束时的存储" $simeq $sigma^ ", 并且它累加器中新增的撤销项按LIFO复合起来逐点"
    $simeq $g ". ")
   ((Definition #:id "def-pindep")
    "独立性在" $Maybe "中的读法: 两个部分效应 (或两个操作在各自令牌下) 独立, 如果"
    "它们的生成元在部分函数的意义下两两交换, 并且一方的生成元既不改变另一方的定义域, "
    "也不扰动另一方给出的逆和结果. 只在生成元上要求. "
    "用于操作时, 这就是论文的定义39(键可交换)的读法. ")
   ((Theorem)
    "若" (&!= $k $k^) ", 则" (&lift $k $a $t_a) "与" (&lift $k^ $b $t_b)
    "在" (Ref "def-pindep") "的意义下独立. 这是论文的定理40. ")
   ((proof)
    (&lift $k $a $t) "的前进函数和它给出的逆都只读写键" $k
    ", 其余键原样保留. 两个分别只读写" $k "和" $k^
    "的映射, 在" (&!= $k $k^) "时逐点交换, 再由" (Ref "sec-effects")
    "中的引理, 生成的子幺半群也交换. 另外, 操作在" $sigma
    "处是否有定义, 给出的逆和结果都只依赖于" (app $sigma $k)
    ", 而另一方的生成元不改变它.")
   ((Theorem #:id "thm-paper-hyps")
    "定义39蕴涵撤销独立和可交换; 因此论文自己的假设推出合流所需的服务假设 (见"
    (Ref "thm-compose") "), 以及恢复精确性所需的假设 (见" (Ref "thm-recovery") "). ")
   (P "反过来不成立: 演算要求得更少, 只对两个fiber可能在同一个键上相遇的操作对, "
      "并且只在令牌不同时要求. 论文的定理42(相遇处键可交换则效应独立)因此不必经过"
      "效应函数的独立性, 而是直接推出了演算的操作级假设.")

   (H2. "组件, fiber与状态" #:id "sec-state")
   (P "从这里开始对应论文的第4章. 论文的演算先假设迁移是原子的, 瞬时的, 不会失败的 (4.2节), "
      "再在4.3节逐一去掉这些假设. 我们直接给出去掉之后的版本: "
      "每个服务操作是一个原子步, 步与步之间都可以放弃.")
   ((Definition)
    "一个组件是四元组"
    (MB (tupa0 (&sube $d $K) (&sube $p $K) (∈ $body $Prog) $reacts) ",")
    "分别是依赖, 可能提供的键, 启动程序, 以及事件处理器 (组件编号的列表). "
    "组件放在状态之外的程序表" (&: $prog (&-> $C $Component)) "里. ")
   ((Definition #:id "def-obligation")
    "组件的义务 (静态, 可机械检查): " $body "中每个" (Con $Op $k $a $kappa)
    "满足" (∈ $k (&union $d $p)) ", 每个" (Con $Provide $k $s $P) "满足"
    (∈ $k $p) ".")
   (P "与Cordis不同, 提供的键是静态声明的. 这使得不同fiber的" $p
      "两两不相交可以在插入时检查, 依赖关系是否无环也可以静态判定; 进展和合流都需要无环.")
   ((Definition)
    "状态是" (tupa0 $next $F $B) ", 其中" $next "是新鲜名字, provision编号和令牌共用的计数器, "
    (&: $F (&p-> $NN $Fiber)) "是注册表, " (&: $B (&p-> $K $Binding)) "是当前的服务绑定:"
    (MB (eqn*
         ($Fiber $::= (tupa0 (∈ $c $C) (∈ $pi (&+ $NN (setE $root)))
                             (∈ $tau $BB) (∈ $theta $Theta)))
         ($Binding $::= (tupa0 $pid $owner (∈ $s $St)))
         ($theta $::= (&\| (Inactive $zeta) (Loading $P $acc $omega)
                           (Active $acc $omega) (Unloading $acc $omega $zeta)))
         ($u $::= (&\| (appl $UOp $k $pid $tok $g) (appl $UProv $k $pid) (appl $USpawn $m)))))
    "这里" $pi "是父fiber, " $tau "是" (Q "已请求移除") "标记, " $zeta
    "是可选的错误 (" (&= $zeta $bottom) "表示没有错误, 非" $bottom "的错误记作" $xi
    "), 累加器" $acc
    "是撤销项" $u "的列表, 表头是最近的一项; "
    (&: $omega (&p-> $K $NN)) "是视图, 定义域恰为该fiber的组件的" $d
    ", 记录每个依赖解析到的provision编号. ")
   (P "几点说明.")
   (Ul (Li "四个生命周期状态代替Cordis的六个: FAILED就是"
           (Inactive $xi) ", DISPOSED就是条目不存在.")
       (Li "累加器就是" (Ref "sec-effects") "的" $phi
           ", 只是拆成撤销项的列表, 以便逐项撤销, 逐项设守卫. "
           $UOp "项记下操作用的令牌, 这是恢复精确性的证明所需要的.")
       (Li "以provision编号作为提供的身份. 提供者撤掉再重新提供时编号改变, "
           "依赖它的fiber会重新加载, 绝不会拿着过期的绑定继续运行.")
       (Li "绑定中的服务状态被提供者和所有依赖者共同操作, 是共享且可变的."))

   (H2. "派生概念" #:id "sec-derived")
   ((Definition)
    "以下记" (&= (app $F $n) (tupa0 c_n (_ $pi $n) τ_n θ_n)) "."
    (MB (eqn*
         ((&installed $n) $:<=> (&!= θ_n (Inactive $_)))
         ((&visible $k) $:=
          (Choice0 ($p ", 若" (&= (app $B $k) (tupa0 $p $o $_)) "且"
                       (&= (_ $theta $o) (Active $_ $_)))
                   ($bottom ", 否则")))
         ((&target $n) $:=
          (Choice0 ($bottom ", 若" τ_n "或" (∃ (∈ $k (app $d c_n)) (&= (&visible $k) $bottom)))
                   ((&\|-> (∈ $k (app $d c_n)) (&visible $k)) ", 否则")))
         ((&resolves $m $p) $:<=>
          (&conj (&installed $m) (∃ $k (&= (app (_ $omega $m) $k) $p))))
         ((&quiet $gamma) $:<=>
          (∀ $n (∀ (∈ $ell (app $L $n)) (&= (appl $step $gamma $ell) $None))))))
    "其中" (app $L $n) "是" $n "上的八种生命周期标签 (见" (Ref "sec-rules")
    "), 所以" (&quiet $gamma) "说的是没有生命周期规则可用. ")
   (P (&target $n) "是整个演算的驱动力: 生命周期规则不断让每个fiber的实际状态追赶它的"
      $target ". 这是论文定义25, 26(满足谓词与notify)的操作化. "
      "论文对每次迁移分类为activating, deactivating, neutral; "
      "我们不单独定义notify, 而是让规则以" $target "的变化为前提. "
      $visible "只计入Active的提供者, 所以提供者一进入Unloading, "
      "它的服务就对新的解析不可见; 已经解析到它的依赖者仍持有视图, "
      "可以继续读写, 直到自己卸载完毕.")

   (H2. "规则" #:id "sec-rules")
   (P "规则写作" (Step $gamma $ell $gamma^) ", 标签" $ell "决定唯一的后继 (若存在), "
      "所以演算就是一个确定性的函数"
      (&: $step (&-> $State $Label (&Maybe $State)))
      ". 选择哪个标签是调度的事, 不在演算之内. "
      "凡是从" $next "取名字, 编号或令牌的规则, 都随后把" $next "加一. ")
   (H3. "编排规则")
   (P "编排规则是外部输入.")
   (Ul (RuleLi (Lab $O-Insert $c)
               (list (app $p $c) "与所有现存fiber的" $p "不相交")
               "以" (&:= $m $next) "为名插入" (tupa0 $c $root $false (Inactive $bottom))
               ", " $next "加一.")
       (RuleLi (Lab $O-Dispose $n) (list (∈ $n (&dom $F)))
               (&:= τ_n $true) ".")
       (RuleLi (Lab $O-Reset $n) (list (&= θ_n (Inactive $xi)))
               (&:= θ_n (Inactive $bottom)) ".")
       (RuleLi (Lab $O-Restart $n)
               (list θ_n "为" (Loading $_ $acc $omega) "或" (Active $acc $omega)
                  ", 且" (&neg τ_n))
               (&:= θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $O-React $n $i)
               (list (&= θ_n (Active $acc $omega)) ", " (&neg τ_n) ", "
                  (app $reacts c_n) "的第" $i "项为" $c ", 且" (app $p $c) "不冲突")
               "插入子fiber" (tupa0 $c $n $false (Inactive $bottom)) ", 名为"
               (&:= $m $next) ", 并令" (&:= θ_n (Active (Cons (appl $USpawn $m) $acc) $omega)) "."))
   (P $O-Reset "对应Cordis在失败后用" (Code "update") "重试; "
      $O-Restart "对应" (Code "restart") ", 卸载后由" $L-Begin "重新加载; "
      $O-React "把一次事件反应建模为子fiber, 反应的操作就是子fiber的操作, "
      "构成一个运行期效应, 单独撤销就是对子fiber执行" $O-Dispose ".")
   (H3. "加载")
   (Ul (RuleLi (Lab $L-Begin $n)
               (list (&= θ_n (Inactive $bottom)) "且" (&= (&target $n) (&!= $omega $bottom)))
               (&:= θ_n (Loading (app $body c_n) $nil $omega)) ".")
       (RuleLi (Lab $L-Divert $n)
               (list (&= θ_n (Loading $P $acc $omega)) "且" (&!= (&target $n) $omega))
               (&:= θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $L-Step $n)
               (list (&= θ_n (Loading $P $acc $omega)) "且" (&= (&target $n) $omega))
               "按" $P "分情况:"
               (Ul (Li $Done ": " (&:= θ_n (Active $acc $omega)) ";")
                   (Li $Fail ": " (&:= θ_n (Unloading $acc $omega $EUser)) ";")
                   (Li (Con $Op $k $a $kappa) ": 依赖键查视图" $omega
                       ", 自己提供的键查" $B " (owner必须是" $n "), 得到编号" $p
                       "; 取令牌" (&:= $t $next) ", 在该绑定的状态" $s "上运行"
                       (&run $a $t $s) ". 失败则进入Unloading并带错误; 成功得到"
                       (tu0 $s^ $g $o) "时写回" $s^ ", 并令"
                       (&:= θ_n (Loading (app $kappa $o) (Cons (appl $UOp $k $p $t $g) $acc) $omega))
                       ". 解析失败视为失败;")
                   (Li (Con $Provide $k $s $P^) ": 要求" (∈ $k (app $p c_n))
                       "且" (app $B $k) "未定义; 以" (&:= $p $next) "为编号令"
                       (&:= (app $B $k) (tupa0 $p $n $s)) ", "
                       (&:= θ_n (Loading $P^ (Cons (appl $UProv $k $p) $acc) $omega))
                       ". 要求不满足时进入Unloading并带错误" $EConflict ";")
                   (Li (Con $Spawn $c $P^) ": 要求" (app $p $c)
                       "不冲突; 插入子fiber" (tupa0 $c $n $false (Inactive $bottom))
                       ", 名为" (&:= $m $next) ", 并令"
                       (&:= θ_n (Loading $P^ (Cons (appl $USpawn $m) $acc) $omega))
                       ". 冲突时进入Unloading并带错误" $EConflict "."))))
   (P "所以" $L-Step "在" (&= (&target $n) $omega) "时总是适用的, "
      "Loading的fiber不会卡住.")
   (H3. "卸载")
   (Ul (RuleLi (Lab $L-Leave $n)
               (list (&= θ_n (Active $acc $omega)) "且" (&!= (&target $n) $omega))
               (&:= θ_n (Unloading $acc $omega $bottom)) ".")
       (RuleLi (Lab $L-Undo $n)
               (list (&= θ_n (Unloading (Cons $u $acc) $omega $zeta)) "且下面按" $u "分的前提成立")
               (&:= θ_n (Unloading $acc $omega $zeta)) ", 并且:"
               (Ul (Li (appl $UOp $k $p $t $g) ": 若绑定" $p "仍在, 把它的状态" $s "改为"
                       (app $g $s) ";")
                   (Li (appl $UProv $k $p) ": " (B "守卫") ", 没有"
                       (&!= $m $n) "满足" (&resolves $m $p) "; 删除" (app $B $k) ";")
                   (Li (appl $USpawn $m) ": 要求" (&!in $m (&dom $F))
                       ", 即子fiber已被移除.")))
       (RuleLi (Lab $L-RetireChild $n)
               (list (&= θ_n (Unloading (Cons (appl $USpawn $m) $_) $_ $_)) ", "
                  (∈ $m (&dom $F)) "且" (&neg τ_m))
               (&:= τ_m $true) ".")
       (RuleLi (Lab $L-Unloaded $n) (list (&= θ_n (Unloading $nil $omega $zeta)))
               (&:= θ_n (Inactive $zeta)) ".")
       (RuleLi (Lab $L-Remove $n)
               (list τ_n ", " (&= θ_n (Inactive $_)) ", 且没有" $m "满足" (&= π_m $n))
               "删除" (app $F $n) "."))
   (P "于是父fiber按LIFO撤到" (appl $USpawn $m) "时, 先请求子fiber退出, "
      "然后等它卸载并被移除, 再继续撤销更早的项; 在spawn之后登记的效应此时已经撤销完毕. "
      "提供者撤到" (appl $UProv $k $p) "时, 要等所有解析到" $p "的依赖者先卸载. "
      "这两处是演算中仅有的等待.")
   (H3. "设计决定")
   (P "与论文和Cordis相比, 主要的决定如下.")
   (Ul (Li (B "移除") ": 只有一个外部请求" $O-Dispose ", 条目在"
           (Q "已请求移除, 已Inactive, 没有子fiber") "时由" $L-Remove
           "自动删除. 比论文的Retire加Remove简单, 也不会因为过早删除而泄漏.")
       (Li (B "卸载逐项进行") ": 论文一步应用整个累加器; 我们逐项撤销, "
           "守卫才能精确到每个provision, " $USpawn "才能等待子fiber.")
       (Li (B "LIFO串行") ": Cordis用" (Code "Promise.all") "并发执行disposer. "
           "后登记的效应往往依赖先登记的, LIFO不需要任何额外假设就是对的.")
       (Li (B "放弃加载") ": 每个服务操作是原子步, 步与步之间都可以" $L-Divert
           ". 不保留论文中" (Q "让在途迭代落地") "的分支, 因为JS的await只出现在操作之间.")
       (Li (B "失败之后") ": 增加" $O-Reset ", 由外部决定是否重试, 比论文的永不重入实用.")
       (Li (B "运行期效应") ": 不把累加器改成树, 而是用" $O-React
           "把反应编码为子fiber. 子fiber本身就是一棵子效应树.")
       (Li (B "隔离与可用性检查") ": 不加规则. 键是服务名加作用域标签, "
           "隔离就是对组件表做键重命名, 见" (Ref "sec-meta-derived") "; "
           "在提供时求值的检查是程序自己就能做的判断."))

   (H2. "元理论" #:id "sec-meta")
   (P "这一章陈述演算的性质: 良构性被保持, 依赖顺序被遵守, 卸载精确地撤销效应, "
      "系统不会死锁且会终止, 以及终态与执行顺序无关.")
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
        (Li "累加器中的每个" (appl $UProv $k $p) "都对应一个现存绑定, 编号为"
            $p ", owner为自己;")
        (Li "一个累加器中每个键至多有一个" $UProv ".")))
   ((proof)
    "对规则逐条检查. 有意思的情形如下. " $O-Insert ", " $O-React "和"
    $Spawn "步都检查新组件的" $p "与现存fiber不冲突, 所以不相交性保持. "
    $Provide "步要求" (∈ $k (app $p c_n)) "且" (app $B $k)
    "未定义, 并同时在累加器里压入" $UProv ", 所以绑定与累加器的对应保持. "
    "撤销" $UProv "时守卫保证没有其他已安装fiber解析到这个编号, 所以删除绑定不会破坏"
    (Q "已解析的依赖都有绑定") ". " $L-Remove "要求没有子fiber, 所以父fiber存在的条件保持. "
    $L-Begin "取的视图就是" $target ", 它的键序列恰为" $d
    ", 并且只解析到Active的提供者.")
   (P "推论: 有子fiber或绑定的fiber一定已安装; 一个fiber卸载完毕时, "
      "它的子fiber和绑定都已经不存在了. 论文的保持性只有4条不变式.")
   (H3. "顺序")
   ((Theorem #:id "thm-ordering")
    "设" (Step $gamma $ell $gamma^) ", 并且" $gamma "满足不变式."
    (Ol (Li "若" (&= $ell (Lab $L-Begin $n)) ", 则" (&= (&target $n) $omega)
            ", 对每个" (&= (app $omega $k) $p) "有"
            (∃ $o (&conj (&= (app $B $k) (tupa0 $p $o $_)) (&= (_ $theta $o) (Active $_ $_))))
            ", 并且在" $gamma^ "中" (&= θ_n (Loading (app $body c_n) $nil $omega))
            ". 即" $n "的全部依赖都由Active的提供者绑定, " $n "针对恰好这些provision开始加载.")
        (Li (B "提供者比依赖者活得久") ": 对每个状态, 若" (&installed $m) "且"
            (&= (app (_ $omega $m) $k) $p) ", 则"
            (∃ (&!= $o $m) (&conj (&= (app $B $k) (tupa0 $p $o $_)) (&installed $o))) ".")
        (Li "若" (&= θ_n (Unloading (Cons (appl $UProv $k $p) $acc) $omega $zeta)) "且"
            (&= $ell (Lab $L-Undo $n)) ", 则" (∀ (&!= $m $n) (&neg (&resolves $m $p)))
            "; 反过来, 只要还有这样的" $m ", 这一步就不适用.")
        (Li "若" (&= θ_n (Unloading (Cons (appl $USpawn $m) $acc) $omega $zeta)) "且"
            (&= $ell (Lab $L-Undo $n)) ", 则" (&!in $m (&dom $F)) ".")
        (Li "若" (&= $ell (Lab $L-Step $n)) ", 则" (&= θ_n (Loading $P $acc $omega))
            "且" (&= (&target $n) $omega) "; " $target "变化后只能" $L-Divert "或"
            $L-Leave ", 反之, 只有" $target "变化时才能这样做.")))
   ((proof)
    "第1条: " $L-Begin "的前提是" (&= (&target $n) $omega)
    ", 而" $visible "只在owner为Active时给出编号. 第2条由不变式中"
    (Q "已解析的依赖都有绑定, 编号一致, owner不是自己") "和"
    (Q "绑定的owner已安装") "两条得到. 第3条是撤销" $UProv
    "的守卫. 第4条是撤销" $USpawn "的前提. 第5条是" $L-Step ", "
    $L-Divert ", " $L-Leave "的前提.")
   (P "论文定理63的第3条(依赖者读到的值不变)被有意去掉: 服务状态按设计是共享且可变的, "
      "固定的是provision的身份.")
   (H3. "恢复精确性" #:id "sec-recovery")
   (P "选定fiber " $n ", 沿执行维护一个影子存储: 其他fiber的操作与撤销, "
      "所有绑定的创建与删除都照常进行, 只跳过" $n "自己的操作与撤销. "
      "不变式是: 对每个键, 把" $n "在该绑定上尚未撤销的逆按LIFO依次作用于实际状态, 结果"
      $simeq "影子状态.")
   ((Definition)
    "称" (tu0 $a $b) "在键" $k "上可能相遇, 如果存在组件" $c "和" $c^
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
    "设所有服务操作良好, 且(H)成立. 设" $gamma_0 "满足不变式且" $n
    "在其中没有待撤销操作, " (Steps $gamma_0 $gamma) "是一次执行, " $shadow
    "是沿这次执行得到的影子存储, 并且" $n "在" $gamma
    "中也没有待撤销操作 (比如卸载完毕). 那么对每个键" $k ", "
    (app (&store $gamma) $k) "与" (app $shadow $k) "同时无定义, 或者"
    (MB (&simeq (app (&store $gamma) $k) (app $shadow $k)) ",")
    "其中" (&= (app (&store $gamma) $k) $s) "当" (&= (app $B $k) (tupa0 $_ $_ $s))
    ". 换言之, 卸载" $n "的结果, 就是同一次执行中略去" $n
    "全部操作所得的结果, 不论其他fiber同时做了什么. ")
   ((proof)
    "沿执行维护本节开头的不变式. 逐类检查步骤: " $n
    "自己的操作把状态从" $s "改为" $s^ ", 并压入逆" $g
    "; 因为" (&simeq (app $g $s^) $s) ", 先施新压入的逆就回到原来的情形, 而影子不动. "
    $n "撤销一项时, 实际状态施加了这个逆, 待撤销的列表少了这一项, 两边的作用抵消. "
    "其他fiber在同一绑定上执行操作" $b "时, 影子也执行" $b
    "; 需要把" $n "的待撤销逆移到" $b "之后, 这正是撤销独立的第一条, 而(H)保证"
    $n "的操作与" $b "可能相遇时满足它 (令牌不同, 因为令牌各自新鲜). "
    "其他fiber的撤销项" $h "需要与" $n "的逆交换, 这是撤销独立的第二条. "
    "绑定的创建与删除在两边同时发生; " $n "仍有待撤销操作的绑定不会被删除, 这由守卫保证. "
    "最后" $n "没有待撤销操作时, 不变式说的就是实际存储" $simeq "影子存储.")
   ((Theorem)
    "运行期效应的撤销也是精确的: 从效应出现之前到它的子fiber被移除之后, "
    "每个绑定的状态都" $simeq "略去该效应全部操作所得的状态. ")
   (P "与论文的定理61相比, 这里只要求可能相遇的操作对撤销独立, 不要求前进函数交换, "
      "同一fiber内部不需要任何假设 (LIFO); 并且覆盖运行期效应.")
   (H3. "进展与终止")
   ((Definition)
    "称" $c^ "在" $c "之上, 记作" (&prec $c $c^) ", 如果"
    (MB (&!= (&cap (app $p $c) (app $d $c^)) $empty) ",")
    "或者" $c "可能spawn " $c^ ", 即" $c^ "出现在" $c "的程序的某个spawn中, 或是"
    $c "的事件处理器. "
    "一个状态是稳定的, 如果每个fiber " $n "满足下列之一:"
    (MB (Choice0 ((&conj (&= θ_n (Inactive $xi)) (&neg τ_n)) ", 失败并停下")
                 ((&conj (&= θ_n (Inactive $bottom)) (&neg τ_n) (&= (&target $n) $bottom))
                  ", 不该运行")
                 ((&conj (&= θ_n (Active $_ $omega)) (&= (&target $n) $omega))
                  ", 该运行且视图正确")))
    "特别地, 稳定状态中没有Loading或Unloading的fiber. ")
   ((Theorem #:id "thm-progress")
    "设" $prec "没有无限上升链" (&prec (_ $c $0) (_ $c $1) $..c)
    ". 对满足不变式的" $gamma ","
    (MB (&cm (&=> (&quiet $gamma) (&settled $gamma))
             (&disj (&settled $gamma)
                    (∃ (&cm $n (∈ $ell (app $L $n)) $gamma^) (Step $gamma $ell $gamma^)))) ".")
    "即没有生命周期规则可用的状态一定是稳定的, 不稳定的状态总有生命周期规则可走. ")
   ((proof)
    "设没有生命周期规则可用, 但状态不稳定. 检查每种生命周期状态可以发现, "
    "不稳定的fiber只可能处于Unloading, 并且卡在两处之一: 撤销" (appl $UProv $k $p)
    "时还有已安装的" $m "解析到" $p "; 或撤销" (appl $USpawn $m)
    "时子fiber " $m "还在. 前一种情形, " $n "提供了" $m "声明的键, 后一种情形, "
    $n "可能spawn " $m ", 两种情形下" $m "的组件都在" $n "的组件之上. "
    "并且" $m "也不稳定: 前一种情形" $n "已经不是Active, " $p
    "不再可见, 所以" (&target $m) "与" $m "的视图不同; 后一种情形" $m
    "已被请求移除 (否则" $L-RetireChild "可用), 稳定的fiber不会带着这个标记. "
    "于是" $m "也卡在Unloading, 如此得到一条无限上升链, 与假设矛盾. "
    "形式上, 这是沿" $prec "的良基归纳.")
   ((Theorem #:id "thm-termination")
    "设有" (&: $r (&-> $C $NN)) "和常数" (&cm $M $N) ", 使得"
    (MB (&=> (&prec $c $c^) (&< (app $r $c) (app $r $c^) $M)) ",")
    "并且每个程序的任一路径长度都不超过" $N ". 那么从满足不变式的状态出发, 不存在无限的执行"
    (MB (: (Step $gamma_0 (_ $ell $0) $gamma_1) (^^ $--> (_ $ell $1)) $..c) ",")
    "其中每个" (_ $ell $i) "都是生命周期标签. ")
   ((proof)
    "给每个fiber一个度量 (在" $target "不变时它还能走的步数的上界), "
    "状态的度量是按秩分层求和得到的向量, 低秩的分量在字典序中优先, "
    "每一步生命周期规则都使它按字典序严格下降. 关键事实是: 一个fiber的步骤"
    "可能使秩更低的fiber的某条规则变得可用 (依赖者卸载后提供者才能撤销provision, "
    "子fiber移除后父fiber才能撤销spawn), 但不会增加它们的度量; "
    "它可能增加的只有秩更高的fiber的度量, 比如新spawn的子fiber.")
   (P "与论文的定理66相比, 没有定量的界.")
   (H3. "合流")
   (P "合流分五层证明, 每层在前一层之上.")
   ((Theorem #:id "thm-control")
    (B "[控制层, 无spawn]: ") "设组件都不spawn, 有" (Ref "thm-termination")
    "中的秩" $r "和界" (&cm $M $N) ", 并且每个组件的程序的每条到达" $Done
    "的路径都提供了它可能提供的所有键 (论文的定义69). 设" $gamma_0 "满足不变式, "
    (Steps $gamma_0 $gamma_1) "和" (Steps $gamma_0 $gamma_2)
    "只走生命周期规则, " $gamma_1 "和" $gamma_2 "都quiet且没有失败的fiber. 用"
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
    "论证不比较两次执行的过程, 而是刻画终态. 在没有失败fiber的quiet状态中, "
    "一个fiber是Active当且仅当它没有被请求移除, 且它声明的每个键都由一个Active的fiber提供. "
    "这里用到了关于" $Done "路径的前提: Active的fiber提供了它可能提供的所有键. "
    "这样, Active的fiber之集合是一个不动点方程的解, 方程只依赖起点. "
    "沿秩归纳可知这个方程只有一个解. 秩的前提不能去掉: 若" $c "提供" (_ $k $1)
    "而依赖" (_ $k $2) ", " $c^ "提供" (_ $k $2) "而依赖" (_ $k $1)
    ", 则两者都Active和两者都不Active都是这个方程的解.")
   ((Theorem #:id "thm-state")
    (B "[服务状态, 无spawn]: ") "在上一个定理的设定下, 再设服务满足合流假设 (见"
    (Ref "thm-compose") "). 则对每个键" $k ", 若"
    (&= (app (_ $B $1) $k) (tupa0 $_ $_ $s_1)) "且" (&= (app (_ $B $2) $k) (tupa0 $_ $_ $s_2))
    ", 则" (&simeq $s_1 $s_2) ". ")
   ((proof)
    "分三步. 第一, 在一次执行中, 每个绑定的状态" $simeq
    "它的初始状态再依令牌顺序执行它上面尚未撤销的操作. 第二, 每个已加载的fiber的累加器"
    "对应一条理想轨迹: 单独运行它的程序, 每个操作的回答都在它解析到的provision的初始状态上"
    "依次执行这个fiber自己在该键上先前的操作得到, 不计其他fiber的操作. "
    "可能相遇的操作可交换, 所以其他fiber穿插进来的操作不改变这里看到的结果, "
    "所以理想轨迹只由程序和初始状态决定; 沿秩归纳, 两个终态中对应的Active fiber有相同的轨迹. "
    "第三, 于是每个键上待撤销的操作在两个终态中是同一组, 只是顺序可能不同; "
    "由可交换性, 重排不改变终态的" $simeq "类.")
   ((Theorem #:id "thm-spawn")
    (B "[带spawn]: ") "设定同上, 但允许组件spawn, 并且启动时spawn的子fiber不被直接dispose. "
    "则两个终态在重命名的意义下一致: 存在规范名, "
    "使得fiber, 组件, 是否Active, 绑定都一一对应. 不论两个终态如何到达, "
    "只要根fiber对应, 它们就一致. ")
   ((proof)
    "名字依赖于调度, 所以先给每个fiber一个规范名: 根fiber保留自己的名字, "
    "子fiber由父fiber的规范名和它在父fiber的spawn中的位置命名. "
    "父fiber是否spawn, spawn什么, 取决于它的操作的结果, 所以对应关系要沿秩归纳建立: "
    "对应的fiber有相同的组件和理想轨迹, 因而spawn出对应的子fiber. "
    "其余部分与无spawn的情形相同. 这里还需要一个不变式: Loading和Active的fiber的子fiber都还活着; "
    "直接dispose启动时spawn的子fiber会破坏它, 见下文.")
   ((Theorem #:id "thm-dynamic")
    (B "[编排与生命周期交错]: ") "从初始状态出发, 接受相同编排输入"
    " (以插入次序而不是名字指称根fiber), 并以任意方式穿插生命周期步的两次执行, "
    "若都到达quiet且没有失败fiber, 则在重命名意义下一致. "
    "特别地, 终态与静态组装(先全部插入再运行)的结果一致. ")
   ((proof)
    "插入得到的名字依赖于调度, 所以输入以插入次序指称根fiber. "
    "沿执行记录一本账: 对每个插入次序, 若它没有被dispose, 终态中恰有一个根fiber, "
    "组件就是插入的那个. 早先的插入, dispose, 重启, 重置除了留下的根fiber之外不留痕迹. "
    "于是两个终态的根fiber按插入次序一一对应, 再用上一个定理中"
    (Q "不论终态如何到达, 只要根fiber对应就一致") "的那一半即可.")
   ((Theorem #:id "thm-compose")
    (B "[服务的组合]: ") "称服务满足合流假设, 如果"
    (Ol (Li "每个操作都良好;")
        (Li "操作给出的逆与所在的状态无关: 若" (&= (&run $a $t $s_1) (tu0 $_ $g_1 $_))
            ", " (&= (&run $a $t $s_2) (tu0 $_ $g_2 $_)) ", 则"
            (∀ $x (&simeq (app $g_1 $x) (app $g_2 $x))) ";")
        (Li "令牌不影响后继(在" $simeq "意义下)和结果;")
        (Li "可能相遇的操作在不同令牌下可交换, 并且满足撤销独立的第一条 (只需在"
            $a "之后可能出现的状态上)."))
    "则上面三个定理对任何满足合流假设的服务成立; 两个满足合流假设的服务之和也满足它. ")
   (P "合流不覆盖" $O-React ", 也只允许dispose根fiber. 原因是: 启动时spawn的子fiber若被直接dispose, "
      "父fiber在一次执行里保留原生命期, 在另一次执行里重新加载并重新spawn, "
      "两次的终态确实不同; 这在Cordis中同样成立, 不是模型的人为限制. "
      "保持性, 顺序, 恢复精确性, 进展, 终止都覆盖" $O-React ".")
   (P "论文的定理73(1)构造了按依赖顺序排列episode的规范执行; 我们只断言终态与静态组装一致, "
      "并把静态组装能否无失败地到达quiet作为前提. 引理68, 70(支持集)只在无spawn的控制层有对应"
      ", 有spawn时由规范名对应.")
   (H3. "派生形式" #:id "sec-meta-derived")
   ((Theorem)
    "隔离是对组件表的键重命名: 键取为服务名加作用域标签, "
    "被隔离的子树换一个标签. 重命名保持各定理对组件的假设: 程序有界, "
    "每条到达" $Done "的路径提供组件可能提供的所有键, "
    "秩的单调性, 以及独立性 (此时要对同一服务名上任意两个组件的操作成立, "
    "因为同一组件可以在多个作用域中同时运行). ")
   (P "所以对任意组件表陈述的定理都直接适用于隔离, 不需要论文定义28的领域表. "
      "论文的拦截 (定义30, 31) 视为传给服务的数据, 不影响生命周期, 本文不讨论.")

   (H2. "与论文的对照")
   (P "关系一栏: 忠实指与论文的陈述一致; 改写指对应物形式不同; "
      "更强, 更弱指结论或假设比论文强或弱; 无对应指本文的论证路线用不到它们.")
   (Tab (HRow "论文" "本文" "关系")
        (Row "定义1–推论21" (Ref "sec-effects") "忠实")
        (Row "定义22–23" (Span (Ref "sec-programs") ", " (Ref "sec-state")) "改写: 由绑定承载")
        (Row "定义24" (Ref "sec-service") "改写: 单一服务, 多一个令牌")
        (Row "定义25–26" (Ref "sec-derived") "改写: 不单独定义notify")
        (Row "定义27, 30–32" -- "未讨论")
        (Row "定义28–29" (Ref "sec-meta-derived") "改写: 键重命名")
        (Row "定义33" (Ref "sec-programs") "忠实")
        (Row "定义34, 引理35" (Ref "sec-observe") "忠实")
        (Row "定义36–37, 引理38" (Ref "sec-partial") "部分")
        (Row "定义39" (Ref "sec-programs") "改写: Maybe读法, 只在生成元上")
        (Row "定理40" (Ref "sec-programs") "忠实")
        (Row "定义41, 定理42" (Ref "sec-programs") "改写")
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
   ))
