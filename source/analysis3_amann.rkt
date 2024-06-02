#lang racket
(provide analysis3_amann.html)
(require SMathML)
(define openBall
  (case-lambda
    ((x r) (appl $BB x r))
    ((X x r) (appl (_ $BB X) x r))))
(define (powerset X)
  (app $PPP X))
(define Union
  (case-lambda
    ((a b X) (: (__^^ $Union a b) X))
    ((a X) (: (__ $Union a) X))
    ((X) (: $Union X))))
(define Intersect
  (case-lambda
    ((a b X) (: (__^^ $Cap a b) X))
    ((a X) (: (__ $Cap a) X))
    ((X) (: $Cap X))))
(define (genσ S)
  (app (_ $A:script $sigma) S))
(define (Borel X)
  (app $B:script X))
(define-infix*
  (&cup $union)
  )
(define analysis3_amann.html
  (TmPrelude
   #:title "分析三 (Amann &amp; Escher)"
   #:css "styles.css"
   (H1 "分析三 (Amann &amp; Escher)")
   (H2 "第九章 测度论基础")
   (H3 "第9.1节 可测空间")
   (P "本节的" (&cm $X $X_1 $X_2) "代表非空集合.")
   (H4 $sigma "代数")
   (P "我们公理化地引入之后于其上定义&quot;测度&quot;的集族: "
      (powerset $X) "的一个子集" $A:script "被称为" $X
      "上的一个" $sigma "代数, 如果其满足以下性质:"
      (Ol #:attr* '((type "i"))
          (Li (∈ $X $A:script) ";")
          (Li (∈ $A $A:script) "可以推出"
              (∈ $A^c $A:script) ";")
          (Li "对于" (∈ (@ $A_j) (^ $A:script $NN))
              ", " (∈ (Union (∈ $j $NN) $A_j) $A:script) "."))
      "如果" $A:script "是" $X "上的一个" $sigma "代数, "
      (tu0 $X $A:script) "则被称为一个可测空间, 而每个" (∈ $A $A:script)
      "都被称作是" $A:script "可测的.")
   (P "我们称" (&sub $S:script (powerset $X)) "是" $X
      "上的一个代数, 如果其满足以下性质:"
      (Ol #:attr* '((type "i"))
          (Li (∈ $X $S:script) ";")
          (Li (∈ $A $S:script) "可以推出" (∈ $A^c $S:script) ";")
          (Li (∈ $A $B $S:script) "可以推出"
              (∈ (&cup $A $B) $S:script) ".")))
   ((remark)
    "设" (&sub $S:script (powerset $X)) "含有元素" $X "."
    (Ol #:attr* '((type "a"))
        (Li $S:script "是一个代数当且仅当其在有限集合操作下封闭.")
        (Li $S:script "是一个" $sigma "代数当且仅当其在可数集合操作下封闭.")))
   ((example)
    (Ol #:attr* '((type "a"))
        (Li (setE $empty $X) "和" (powerset $X) "是" $sigma "代数.")
        (Li (setI (&sub $A $X) (: $A "或者" $A^c "可数"))
            "是一个" $sigma "代数.")
        (Li (setI (&sub $A $X) (: $A "或者" $A^c "有限"))
            "是一个代数, 其为" $sigma "代数当且仅当" $X "有限.")
        (Li "设" $A:sans-serif "是一个非空指标集, 对于每个" (∈ $alpha $A:sans-serif)
            "有" (_ $A:script $alpha) "是" $X "上的一个" $sigma "代数, 那么"
            (Intersect (∈ $alpha $A:sans-serif) $ (_ $A:script $alpha))
            "也是" $X "上的一个" $sigma "代数.")
        (Li "设" $Y "是一个非空集合, " (∈ $f $Y^X) ", " $A:script "和"
            $B:script "分别是" $X "和" $Y "上的" $sigma "代数, 那么"
            (MB (&:= (app (inv $f) $B:script)
                     (setI (app (inv $f) $B) (∈ $B $B:script)))
                "和"
                (&:= (app (_ $f $*) $A:script)
                     (setI (&sub $B $Y) (∈ (app (inv $f) $B) $A:script))))
            "分别是" $X "和" $Y "上的" $sigma "代数. " (app (inv $f) $B:script)
            "被称为" $B:script "在" $f "下的逆像, " (app (_ $f $*) $A:script)
            "被称为" $A:script "在" $f "下的前推.")))
   (H4 "Borel " $sigma "代数")
   (P "令" $S:script "是" (powerset $X) "的一个非空子集, 那么"
      (MB (&:= (genσ $S:script)
               (Intersect
                (setI (&sub $A:script (powerset $X))
                      (: (&sup $A:script $S:script) "是" $X
                         "上的一个" $sigma "代数")))))
      "是由" $S:script "生成的" $sigma "代数.")
   ((remark)
    (Ol #:attr* '((type "a"))
        (Li (genσ $S:script) "良定且是包含" $S:script
            "的最小" $sigma "代数.")
        (Li (&sub $S:script $T:script) "可以推出"
            (&sub (genσ $S:script) (genσ $T:script)) ".")
        (Li "对于" (&= $S:script (setE $A)) ", 我们有"
            (&= (genσ $S:script) (setE $empty $A $A^c $X)) ".")))
   (P "令" (&:= $X (tu0 $X $T:script)) "是一个拓扑空间. 既然" $T:script
      "是非空的, 它生成了一个良定的" $sigma "代数, 其被称为" $X
      "的Borel " $sigma "代数, 记作" (Borel $X) ". " (Borel $X)
      "的元素被称为" $X "的Borel子集. 作为简写, 我们记"
      (&:= (^ $B:script $n) (Borel $RR^n)) ".")
   (P $X "的一个子集" $A "被称为一个" $G_delta
      ", 如果存在开集" $O_j "使得" (&= $A (Intersect (∈ $j $NN) $O_j))
      ". 集合" $A "被称为一个" $F_sigma ", 如果其是可数闭集之并. "
      $A "是一个" $F_sigma "当且仅当" $A^c "是一个" $G_delta ".")
   ((example)
    (Ol #:attr* '((type "a"))
        (Li "对于" (&:= $F:script (setI (&sub $A $X) (: $A "是闭集")))
            ", 我们有" (&= (Borel $X) (genσ $F:script)) ".")
        (Li "每个" $G_delta "和" $F_sigma "都是Borel集.")
        (Li "每个闭区间" $I "既是" $F_sigma "也是" $G_delta ".")))
   (H4 "第二可数性条件")
   (P "令" (tu0 $X $T:script) "是一个拓扑空间. 我们称" (&sub $M:script $T:script)
      "是" $T:script "的一个基, 如果对于每个" (∈ $O $T:script) ", 存在"
      (&sub (&prime $M:script) $M:script) "使得"
      (&= $O (Union (&prime $M:script))) ". 换言之, 每个开集都可以表达为"
      $M:script "中的一些集合之并. 一个拓扑空间" (tu0 $X $T:script)
      "满足第二可数性条件, 如果" $T:script "拥有一个可数的基. "
      (tu0 $X $T:script) "被称为一个Lindelöf空间, 如果每个" $X
      "的开覆盖都有一个可数的子覆盖. 显然, 每个紧空间都是Lindelöf空间.")
   ((remark)
    (Ol #:attr* '((type "a"))
        (Li (&sub $M:script $T:script) "是" $T:script "的一个基当且仅当对于每个点"
            (∈ $x $X) "和" $x "的每个邻域" $U ", 存在" (∈ $M $M:script) "满足"
            (&in $x (&sub $M $U)) ".")
        (Li "任意满足第二可数性条件的拓扑空间也满足第一可数性条件.")
        (Li "反之则不然.")))
   ((lemma)
    "设" $X "是一个度量空间而" (&sub $A $X) "在" $X "中稠密, 并令"
    (&:= $M:script (setI (openBall $a $r) (&cm (∈ $a $A) (∈ $r (^ $QQ $+)))))
    ", 那么每个" $X "中的开集都可以写成来自于" $M:script "的集合之并.")
   ((proposition)
    "令" $X "是一个度量空间, 以下陈述等价:"
    (Ol #:attr* '((type "i"))
        (Li $X "满足第二可数性条件.")
        (Li $X "是一个Lindelöf空间.")
        (Li $X "是一个可分空间.")))
   ((corollary)
    (Ol #:attr* '((type "i"))
        (Li "设" $X "是一个可分度量空间而" $A "是" $X "的一个可数稠密子集, 那么"
            (MB (&= (Borel $X)
                    (genσ (setI (openBall $a $r)
                                (&cm (∈ $a $A) (∈ $r (^ $QQ $+)))))) "."))
        (Li "设" (&sub $X $RR^n) "非空, 那么度量空间" $X "有可数的基.")))
   ((corollary)
    "设" $X "是一个具有可数基的拓扑空间, 那么" $X "是可分的且是Lindelöf的.")
   (H4 "使用区间生成Borel " $sigma "代数")
   (P "我们赋予" $RR^n "其上的自然序, 即对于" (∈ $a $b $RR^n)
      ", " (&<= $a $b) "当且仅当" (&cm (&<= $a_k $b_k) (&<= $1 $k $n))
      ". "
      )
   ))