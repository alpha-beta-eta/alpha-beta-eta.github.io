#lang racket
(provide dsh0.html)
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
(define ((Track f g) γ φ)
  (tu0 (app f γ) (&compose φ g)))
(define $recover (Mi "recover"))
(define (&recover γ φ)
  (appl $recover γ φ))
(define (Recover γ φ)
  (tu0 (app φ γ) $id))
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
(define dsh0.html
  (TnTmPrelude
   #:title "deepseek harness笔记0"
   #:css "styles.css"
   (H1. "deepseek harness笔记0")
   (H2. "可撤销效应")
   (P "以下对应于原文的定义8, 但是不太一样.")
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
   (P "以下对应于定理15, 但原文的表述不太严谨.")
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
   (P "以下对应于定理20, 但我扩写了一下.")
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
   (P "作者认为"
      (∈ (appl $set $k $v) (&eff* Σ))
      ", 个人认为不太严谨.")
   
   ))