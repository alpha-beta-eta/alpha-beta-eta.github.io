#lang racket
(provide dsh.html)
(require SMathML)
(define $dep $D:script)
(define (&dep Σ)
  (_ $D:script Σ))
(define $Set (Mi "Set"))
(define (&Set K) (app $Set K))
(define $notify (Mi "notify"))
(define (&notify d σ σ^)
  (appl (_ $notify d) σ σ^))
(define $activating (Mi "activating"))
(define $deactivating (Mi "deactivating"))
(define $neutral (Mi "neutral"))
(define (Lam x t)
  (: $lambda x $. t))
(define $get (Mi "get"))
(define $set (Mi "set"))
(define $dom (Mi "dom"))
(define (&dom f)
  (app $dom f))
(define (ext f x v)
  (: f (bra0 (&\|-> x v))))
(define $Type (Mi "Type"))
(define Δ_i (_ Δ $i))
(define $Gen $G:fraktur)
(define &Gen
  (case-lambda
    ((e) (app (_ $Gen Γ) e))
    ((Γ e) (app (_ $Gen Γ) e))))
(define (Gen e)
  (&union (setE (&compose (&pr $1) e))
          (setI (app* (&pr $2) e $gamma)
                (∈ $gamma Γ))))
(define (Generate X) (ang0 X))
(define $tsmonoid $M:fraktur)
(define (&tsmonoid e)
  (app $tsmonoid e))
(define $_ (Mi "_"))
(define $dummy (Mi "-"))
(define $eta_Γ (_ $eta Γ))
(define $ulcorner
  (Mo "&ulcorner;" #:attr* '((stretchy "false") (lspace "0") (rspace "0"))))
(define $urcorner
  (Mo "&urcorner;" #:attr* '((stretchy "false") (lspace "0") (rspace "0"))))
(define (Induced f g)
  (: $ulcorner (&cm f g) $urcorner))
(define App (&split 2))
(define (∀ x P)
  (: $forall x $. P))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define $let (Pid "let"))
(define $in (Pid "in"))
(define (Let b e)
  (App $let b $in e))
(define $End (Mi "End"))
(define (&End Γ)
  (app $End Γ))
(define $id (Mi "id"))
(define (&id Γ) (_ $id Γ))
(define $twisted $T:fraktur)
(define (&twisted Γ)
  (_ $twisted Γ))
(define (Twisted Γ)
  (&c* (@-> Γ Γ) (@-> Γ Γ)))
(define $partial
  (Mo "&part;"
      #:attr* '((lspace "0") (rspace "0"))))
(define &partial
  (case-lambda
    ((Γ) (ap $partial Γ))
    ((n Γ) (ap (^ $partial n) Γ))))
(define (∂ Γ)
  (&c* Γ (@-> Γ Γ)))
(define $track (Mi "track"))
(define (&track f g)
  (appl $track f g))
(define ((Track f g) γ ϕ)
  (tu0 (app f γ) (&compose ϕ g)))
(define $recover (Mi "recover"))
(define (&recover γ ϕ)
  (appl $recover γ ϕ))
(define (Recover γ ϕ)
  (tu0 (app ϕ γ) $id))
(define $pr (Mi "pr"))
(define (&pr i) (_ $pr i))
(define $eff $E:fraktur)
(define (&eff Γ) (_ $eff Γ))
(define (Eff Γ)
  (&-> Γ (&partial Γ)))
(define (&eff* Γ)
  (_^ $eff Γ $*))
(define $effect (Mi "effect"))
(define (&effect e)
  (app $effect e))
(define $diamond (Mo "&diamond;"))
(define $p-> (Mo "&rharu;"))
(define $V:script_k
  (_ $V:script $k))
(define $\\ (Mo "\\"))
(define $!= $vDash)
(define-infix*
  (&!= $!=)
  (&\\ $\\)
  (&p-> $p->)
  (&diamond $diamond))
(define-@lized-op*
  (@!= &!=)
  (@p-> &p->)
  (@diamond &diamond)
  (@\|-> &\|->))
(define dsh.html
  (TnTmPrelude
   #:title "deepseek harness笔记"
   #:css "styles.css"
   (H1. "deepseek harness笔记")
   (H2. "可撤销效应")
   ((Definition)
    (&:= (&twisted Γ) (Twisted Γ)) ".")
   ((Definition)
    "对于" (∈ (tu0 $f_1 $g_1) (tu0 $f_2 $g_2) (&twisted Γ))
    ", 定义其复合为"
    (MB (&= (&compose (tu0 $f_1 $g_1)
                      (tu0 $f_2 $g_2))
            (tu0 (&compose $f_1 $f_2)
                 (&compose $g_2 $g_1))) ".")
    (&twisted Γ) "在" $compose
    "下构成了一个幺半群, 其单位元是"
    (tu0 (&id Γ) (&id Γ)) ".")
   ((Definition)
    (&:= (&partial Γ) (∂ Γ)) ".")
   ((Definition)
    "定义"
    (&: $track (&-> (&twisted Γ)
                    (&End (&partial Γ))))
    "为"
    (MB (&:= (appl (&track $f $g) $gamma $phi)
             ((Track $f $g) $gamma $phi)) ".")
    $track "是一个幺半群同态. 并且, 注意到"
    (MB (&= (&compose (&pr $1) (&track $f $g))
            (&compose $f (&pr $1))) "."))
   ((Definition)
    "定义" (&: $recover (&End (&partial Γ))) "为"
    (MB (&= (&recover $gamma $phi)
            (Recover $gamma $phi)) ".")
    "注意到如果"
    (&= (&compose $g $f) (&id Γ))
    ", 那么"
    (&= (&compose $recover (&track $f $g))
        $recover) ".")
   ((Definition)
    (&:= (&eff Γ) (Eff Γ)) ".")
   ((Definition)
    (&:= (&eff* Γ)
         (setI (∈ $e (&eff Γ))
               (∀ (∈ $gamma Γ)
                  (Let (&:= (tu0 $delta $g)
                            (app $e $gamma))
                       (&= (app $g $delta) $gamma)))))
    ". 换言之, " (&eff* Γ) "是" (&eff Γ)
    "中那些符合撤销的直觉的元素. 每个"
    (∈ (tu0 $f $g) (&twisted Γ))
    "都可以导出一个"
    (∈ (&:= (Induced $f $g)
            (@\|-> $gamma (tu0 (app $f $gamma) $g)))
       (&eff Γ))
    ". 如果" (&= (&compose $g $f) (&id Γ))
    ", 那么" (∈ (Induced $f $g) (&eff* Γ)) ".")
   ((Definition)
    "对于" (∈ $e_1 $e_2 (&eff Γ))
    ", 我们定义其复合"
    (&diamond $e_1 $e_2) "为"
    (MB (&:= (&diamond $e_1 $e_2)
             (&\|-> $gamma
                    (Let (&:= (tu0 $delta $s)
                              (app $e_2 $gamma))
                         (Let (&:= (tu0 $epsilon $t)
                                   (app $e_1 $delta))
                              (tu0 $epsilon
                                   (&compose $s $t)))))) "."))
   ((Proposition)
    (&eff Γ) "在运算" $diamond
    "下是一个幺半群, 其单位元是"
    (&:= $eta_Γ (&\|-> $gamma
                       (tu0 $gamma (&id Γ)))) ".")
   ((Proposition)
    (&: (Induced $dummy $dummy)
        (&-> (&twisted Γ) (&eff Γ)))
    "是一个幺半群同态.")
   ((Proposition)
    (&eff* Γ) "是" (&eff Γ) "的一个子幺半群.")
   ((Definition)
    "定义"
    (&: $effect (&-> (&eff Γ)
                     (&eff (&partial Γ))))
    "为"
    (MB (&:= (appl (&effect $e) $gamma $phi)
             (Let (&:= (tu0 $delta $g)
                       (app $e $gamma))
                  (tu0 (tu0 $delta (&compose $phi $g))
                       (&track $g (&compose (&pr $1) $e))))) "."))
   ((Proposition)
    "对于" (∈ $e_1 $e_2 (&eff Γ)) ", 我们有"
    (MB (!commute &effect &diamond $e_1 $e_2) "."))
   ((Proposition)
    "令" (∈ $e (&eff* Γ)) "而"
    (&:= $f (&compose (&pr $1) $e))
    ", 对于" (∈ (tu0 $gamma $phi) (&partial Γ))
    ", 令" (&:= (tu0 $delta $g) (app $e $gamma))
    ", " (&:= (tu0 Δ $G) (appl (&effect $e) $gamma $phi))
    ", 我们有"
    (MB (&= (app $G Δ)
            (tu0 $gamma (&compose $phi $g $f))) ".")
    "另外, 我们知道"
    (&= (app (@compose $phi $g $f) $gamma)
        (app $phi $gamma))
    ". 除此之外, 还有"
    (∈ (&effect $e) (&eff* (&partial Γ)))
    "当且仅当"
    (MB (∀ (∈ $gamma Γ)
           (Let (&:= (tu0 $_ $g_gamma)
                     (app $e $gamma))
                (&= (&compose $g_gamma $f)
                    (&id Γ)))) "."))
   ((Comment)
    "对于" (∈ $e_1 $..h $e_n (&eff* Γ))
    "以及" (∈ (tu0 $gamma_0 (&id Γ)) (&partial Γ))
    ", 我们有"
    (eqn*
     ((appl (&effect $e_1) $gamma_0 (&id Γ))
      $= (tu0 (tu0 $gamma_1 $g_1)
              (&track $g_1 $f_1)))
     ((appl (@diamond (&effect $e_2) (&effect $e_1))
            $gamma_0 (&id Γ))
      $= (tu0 (tu0 $gamma_2 (&compose $g_1 $g_2))
              (&compose (&track $g_1 $f_1)
                        (&track $g_2 $f_2))))
     ($ $..v $)
     ((appl (@diamond (&effect $e_n) $..c (&effect $e_1))
            $gamma_0 (&id Γ))
      $= (tu0 (tu0 $gamma_n (&compose $g_1 $..c $g_n))
              (&compose (&track $g_1 $f_1)
                        $..c
                        (&track $g_n $f_n)))))
    "其中"
    (eqn*
     ($f_i $:= (&compose (&pr $1) $e_i))
     ($gamma_i $:= (app $f_i (_ $gamma (&- $i $1))))
     ($g_i $:= (app* (&pr $2) $e_i (_ $gamma (&- $i $1)))))
    "而且"
    (eqn*
     ((app (@compose $g_1 $..c $g_i) $gamma_i)
      $= $gamma_0)
     ((app $G_i Δ_i)
      $= (tu0 $gamma_0
              (&compose $g_1 $..c $g_i
                        $f_i $..c $f_1))))
    "其中"
    (eqn*
     (Δ_i $:= (tu0 $gamma_i (&compose $g_1 $..c $g_i)))
     ($G_i $:= (&compose (&track $g_1 $f_1)
                         $..c
                         (&track $g_i $f_i)))))
   ((Definition)
    "对于" (∈ $e (&eff Γ)) ", 定义"
    (MB (&:= (&Gen $e) (Gen $e)) "."))
   ((Definition)
    "对于" (∈ $e (&eff Γ)) ", 变换幺半群"
    (MB (&:= (&tsmonoid $e)
             (Generate (&Gen $e))))
    "是" (&End Γ) "的一个子幺半群. "
    "另外, 对于" (∈ (tu0 $f $g) (&twisted Γ))
    ", 我们有"
    (MB (&= (&Gen (Induced $f $g))
            (setE $f $g)) "."))
   ((Proposition)
    "对于" (∈ $e_1 $e_2 (&eff Γ))
    ", 如果" (&Gen $e_1) "与"
    (&Gen $e_2) "交换, 那么"
    (&tsmonoid $e_1) "与"
    (&tsmonoid $e_2) "交换.")
   ((Proposition)
    "对于" (∈ $e_1 $e_2 (&eff Γ))
    ", 我们有"
    (&sube (&tsmonoid (&diamond $e_1 $e_2))
           (Generate
            (&union (&Gen $e_1)
                    (&Gen $e_2)))) ".")
   ((Definition)
    (∈ $e_1 $e_2 (&eff Γ))
    "是独立的, 如果"
    (Ol (Li (&tsmonoid $e_1) "和"
            (&tsmonoid $e_2) "交换;")
        (Li (∀ (&cm (∈ $g (&tsmonoid $e_2))
                    (∈ $gamma Γ))
               (&= (app* (&pr $2) $e_1 $g $gamma)
                   (app* (&pr $2) $e_1 $gamma)))
            ". 将" $e_1 "和" $e_2
            "的地位交换也是一样.")))
   ((Proposition)
    "对于两两独立的"
    (∈ $e_1 $..h $e_n (&eff* Γ))
    "和" (∈ $gamma_0 Γ)
    ", 我们定义"
    (eqn*
     ($f_i $:= (&compose (&pr $1) $e_i))
     ($gamma_i $:= (app $f_i (_ $gamma (&- $i $1))))
     ($g_i $:= (app* (&pr $2) $e_i (_ $gamma (&- $i $1)))))
    "固定某个"
    (∈ $j (setE $1 $..h $n))
    ", 对于每个" $i "满足"
    (&<= $j $i $n) ", 定义"
    (MB (&:= (_prime $gamma $i)
             (app (@compose $f_i $..c
                            (_ $f (&+ $j $1)))
                  (_ $gamma (&- $j $1)))))
    "那么对于每个" $u "满足"
    (&<= $j $u $n) ", 我们有"
    (eqn*
     ($gamma_u $= (app $f_j (_prime $gamma $u)))
     ((app $g_j $gamma_u) $= (_prime $gamma $u)))
    "另外, 对于每个" $i "满足"
    (: $j $< $i $<= $n) ", 我们有"
    (MB (&= (app* (&pr $2) $e_i
                  (_prime $gamma (&- $i $1)))
            $g_i) "."))
   
   (H2. "反应式余效应")
   ((Definition)
    "给定一个类型族"
    (&: $V:script (&-> $K $Type))
    ", 定义"
    (&:= Σ (&p-> (@: $k $K) $V:script_k))
    ". 对于" (&: $sigma Σ) ", 我们记"
    (Ul (Li (&sube (&dom $sigma) $K)
            "为该有限部分函数的定义域;")
        (Li (ext $sigma $k $v)
            "在" $k "处的值为" $v
            ", 其余地方与" $sigma
            "保持一致;")
        (Li (&\\ $sigma $k)
            "用于限制, 前提是"
            (∈ $k (&dom $sigma)) ".")))
   ((Definition)
    "定义"
    (&: $get (&-> (@: $k $K)
                  (&p-> Σ $V:script_k)))
    "为"
    (MB (&:= (app (app $get $k) $sigma)
             (app $sigma $k)) ".")
    "定义"
    (&: $set (&-> (&c* (@: $k $K) $V:script_k)
                  (&p-> Σ (&c* Σ (@p-> Σ Σ)))))
    "为"
    (MB (&:= (app (appl $set $k $v) $sigma)
             (tu0 (ext $sigma $k $v)
                  (Lam $sigma^
                       (&\\ $sigma^ $k)))))
    "其要求" (&!in $k (&dom $sigma))
    "作为前条件.")
   ((Definition)
    
    )
   ((Definition)
    (&:= (&dep Σ) (&Set $K))
    ". 个人猜测" (&Set $K)
    "指的是" $K "的有限子集之集合.")
   ((Definition)
    "对于" (∈ $d (&dep Σ)) "和"
    (∈ $sigma Σ) ", 定义"
    (&:= (&!= $sigma $d)
         (∀ (∈ $k $d)
            (∈ $k (&dom $sigma)))) ".")
   ((Definition)
    "给定" (∈ $d (&dep Σ)) "和"
    (∈ $sigma $sigma^ Σ) ", 定义"
    (MB (&:= (&notify $d $sigma $sigma^)
             (Choice0
              ($activating
               ", 如果"
               (&neg (@!= $sigma $d)) "且"
               (&!= $sigma^ $d))
              ($deactivating
               ", 如果"
               (&!= $sigma $d) "且"
               (&neg (@!= $sigma^ $d)))
              ($neutral ", 其余情况")))))
   ((Definition)
    
    )
   ((Definition)
    
    )
   ((Definition)
    
    )
   ))