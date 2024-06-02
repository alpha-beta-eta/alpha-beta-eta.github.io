#lang racket
(provide exercises_in_lattice_theory.html)
(require SMathML)
(define $\\ (Mo "\\"))
(define $sube:id (Mi "&sube;"))
(define (GI S)
  (: $lp S $rb))
(define (GF S)
  (: $lb S $rp))
(define (Neigh x)
  (app $N:script x))
(define (ONeigh x)
  (app $U:script x))
(define (Open X)
  (app $O:script X))
(define (|(]| a b)
  (: $lp0 a $cm b $rb0))
(define (|()| a b)
  (tu0 a b))
(define (|[)| a b)
  (: $lb0 a $cm b $rp0))
(define (|[]| a b)
  (li0 a b))
(define $Fin (Mi "Fin"))
(define (&Fin X)
  (app $Fin X))
(define $CoFin (Mi "CoFin"))
(define (&CoFin X)
  (app $CoFin X))
(define (∃ Q P)
  (: $exists Q $cm P))
(define (^-> X) (^ X $->))
(define (^<- X) (^ X $<-))
(define $id (Mi "id"))
(define (&id x) (_ $id x))
(define $Idl (Mi "Idl"))
(define $Fil (Mi "Fil"))
(define (&Idl P)
  (app $Idl P))
(define (PIdl P)
  (&\\ (&Idl P) (setE P)))
(define (&Fil P)
  (app $Fil P))
(define (PFil P)
  (&\\ (&Fil P) (setE P)))
(define $darr:id (Mi "&darr;"))
(define (↓ x)
  (: $darr:id x))
(define $uarr:id (Mi "&uarr;"))
(define (↑ x)
  (: $uarr:id x))
(define $Im (Mi "Im"))
(define (&Im f) (app $Im f))
(define $<_P (_ $< $P))
(define $<=_P (_ $<= $P))
(define $<=_Q (_ $<= $Q))
(define $<=:id (Mi "&le;"))
(define $<=:id_P (_ $<=:id $P))
(define $<=:id_Q (_ $<=:id $Q))
(define $dashv (Mo "&dashv;"))
(define $RightVector (Mo "&RightVector;"))
(define $meet $conj)
(define $join $disj)
(define Meet Conj)
(define Join Disj)
(define-infix*
  (&\\ $\\)
  (&<_P $<_P)
  (&<=_P $<=_P)
  (&<=_Q $<=_Q)
  (&meet $meet)
  (&join $join)
  (&dashv $dashv)
  (&⊣ $dashv)
  (&RightVector $RightVector))
(define-@lized-op*
  (@meet &meet)
  (@join &join))
(define exercises_in_lattice_theory.html
  (parameterize ([separate-class? #t])
    (TnTmPrelude
     #:title "格论练习和笔记"
     #:css "styles.css"
     (H1. "格论练习和笔记")
     (P "注意, 这本书默认偏序集是非空的. 另外, "
        (&= $NN (setE $1 $2 $3 $..h)) ".")
     (H2. "偏序集与格")
     (H3. "偏序集")
   
     (H3. "格与完备格")
     ((Definition)
      )
     ((Remark)
      )
     ((Definition)
      )
     ((Theorem)
      )
     ((Example)
      )
     ((Theorem)
      )
     ((Theorem)
      )
     ((Definition)
      )
     ((Example)
      )
     ((Definition)
      )
     ((Theorem)
      )
     ((Corollary)
      )
     ((Definition)
      )
     ((Theorem)
      )
     ((Definition)
      "设" $P "是一个偏序集, "
      (func $f $P $P)
      "是一个自映射. 如果"
      (Ol (Li "单调性/保序性: "
              (&<= $x $y) "可以推出"
              (&<= (app $f $x) (app $f $y))
              ", 对于任意的"
              (∈ $x $y $P) ";")
          (Li "增值性: "
              (&<= $x (app $f $x))
              ", 对于任意的"
              (∈ $x $P) ";")
          (Li "幂等性: "
              (&= (&compose $f $f) $f) ","))
      "那么则称" $f "是" $P
      "上的一个闭包算子 (closure operator).")
     (H3. "序同构和格同构")
     (H3. "分配格和Boole代数")
     ((Theorem)
      "设" $L "是一个格, 那么下面两个有限分配律是等价的:"
      (Ol (Li "对于任意的" (∈ $x $y $z $L) ", "
              (distributeR $x &meet $y &join $z) ";")
          (Li "对于任意的" (∈ $x $y $z $L) ", "
              (distributeR $x &join $y &meet $z) ".")))
     ((proof)
      )
     ((Definition)
    
      )
     (H3. "理想和滤子")
     (P "设" $P "是一个偏序集, " (&sube $S $P)
        ". 如果" $S "非空, 且对于任意的"
        (∈ $x $y $S) "都存在" (∈ $z $S)
        "使得" (&<= (&cm $x $y) $z) ", 则称"
        $S "是" $P "的有向子集/定向子集 (directed subset); 如果"
        $S "非空且对于任意的" (∈ $x $y $S)
        "都存在" (∈ $z $S) "使得"
        (&<= $z (&cm $x $y)) ", 则称"
        $S "是" $P "的滤过子集/可滤子集 (filtered subset).")
     ((Definition)
      "设" $P "是一个偏序集, " (&sube (&cm $I $F) $P) "."
      (Ol (Li "如果" $I "是一个定向下集, 则称" $I
              "是" $P "的理想 (ideal);")
          (Li "如果" $F "是一个滤过上集, 则称" $F
              "是" $P "的滤子 (filter).")))
     (P "如果" $S "是" $P "的一个理想 (respectively, 滤子), 且"
        (&!= $S $P) ", 那么称" $S "是" $P
        "的真理想 (proper ideal) (respectively, 真滤子). "
        "对于任意的" (∈ $x $P) ", 子集" (↓ $x)
        " (respectively, " (↑ $x) ") 是一个理想 "
        " (respectively, 滤子), 其被称为关于" $x
        "的主理想 (principal ideal) "
        "(respectively, 主滤子 (principal filter)). "
        "我们记" (&Idl $P) "为" $P "的所有理想构成的集合, "
        (&Fil $P) "为" $P "的所有滤子构成的集合. "
        "注意, 理想和滤子首先都是非空子集.")
     ((Example)
      (Ol (Li "对于任意的" (∈ $a $RR) ", "
              (|(]| (&- $inf) $a) "和"
              (|()| (&- $inf) $a)
              "都是" $RR "的理想, "
              (|[)| $a (&+ $inf)) "和"
              (|()| $a (&+ $inf))
              "都是" $RR "的滤子.")
          (Li "设" $X "是一个非空集合, "
              (&= (&Fin $X)
                  (setI (&sube $A $X)
                        $A "有限"))
              "是" (powset $X) "的理想, "
              (&= (&CoFin $X)
                  (setI (&sube $A $X)
                        $A^ "有限"))
              "是" (powset $X) "的滤子.")
          (Li "设" (tu0 $X (Open $X))
              "是一个拓扑空间, 对于任意的"
              (∈ $x $X) ", " (Neigh $x)
              "是" (tu0 (powset $X) $sube:id)
              "的滤子, " (ONeigh $x) "是"
              (tu0 (Open $X) $sube:id)
              "的滤子, 其中"
              (MB (&= (Neigh $x)
                      (setI (&sube $V $X)
                            (∃ (∈ $U (Open $X))
                               (∈ $x (&sube $U $V))))))
              "而"
              (MB (&= (ONeigh $x)
                      (setI (∈ $U (Open $X))
                            (∈ $x $U)))))))
     ((Theorem)
      "设" $L "是一个格, " $I "是" $L "的非空子集, "
      "则下列条件等价:"
      (Ol (Li $I "是理想;")
          (Li $I "是对于" $join "封闭的下集;")
          (Li $I "对于" $join "封闭, 对于" $meet
              "吸收 (即对于" (∈ $a $I) "和"
              (∈ $b $L) ", 都有"
              (∈ (&meet $a $b) $I) ").")))
     ((proof)
      "由1推出2: 对于" (∈ $x $y $I)
      ", 我们知道存在" (∈ $z $I)
      "满足" (&<= (&cm $x $y) $z)
      ". 换言之, " $z "是" $x "和" $y
      "的一个上界, 那么"
      (&<= (&join $x $y) $z)
      ". 鉴于" $I "是一个理想 (故为下集), 那么"
      (∈ (&join $x $y) $I)
      ", 即" $I "对于" $join "封闭." (Br)
      "由2推出3: 对于" (∈ $a $I) "和"
      (∈ $b $L) ", 我们知道"
      (&<= (&meet $a $b) $a)
      ". 鉴于" $I "是一个下集, 所以"
      (∈ (&meet $a $b) $I)
      ", 也就是" $I "对于" $meet "吸收." (Br)
      "由3推出1: 由于" $I "对于" $join
      "封闭, 故" $I "是一个定向子集. "
      "如果" (∈ $y $I) "而" (&<= $x $y)
      ", 那么根据吸收性质, 有"
      (∈ (&meet $x $y) $I)
      ", 由因为" (&<= $x $y) "等价于"
      (&= (&meet $x $y) $x)
      ", 所以说" (∈ $x $I)
      ", 也就是说" $I "是一个下集.")
     (P "对偶地, 我们有以下定理.")
     ((Theorem)
      "设" $L "是一个格, " $F "是" $L "的非空子集, "
      "则下列条件等价:"
      (Ol (Li $F "是滤子;")
          (Li $F "是对于" $meet "封闭的上集;")
          (Li $F "对于" $meet "封闭, 对于" $join "吸收.")))
     ((Theorem)
      "设" $L "是一个格, " $S "是" $L
      "的一个非空子集, 令"
      (MB (&= (GI $S)
              (setI (∈ $a $L)
                    "存在" (&cm (∈ $n $NN) (∈ $x_1 $..h $x_n $S))
                    "满足" (&<= $a (&join $x_1 $..c $x_n)))))
      (MB (&= (GF $S)
              (setI (∈ $a $L)
                    "存在" (&cm (∈ $n $NN) (∈ $x_1 $..h $x_n $S))
                    "满足" (&<= (&meet $x_1 $..c $x_n) $a))))
      "则" (GI $S) "是包含" $S "的最小理想, 称为" $S
      "的生成理想; " (GF $S) "是包含" $S
      "的最小滤子, 称为" $S "的生成滤子.")
     ((proof)
      
      )
     ((Theorem)
      "设" $L "是一个有界格, 那么"
      (Ol (Li (&Idl $L) "是完备格, 其最小元是" (setE $0)
              ", 最大元是" $L ", 对于"
              (&sube (setI $I_k (∈ $k $K))
                     (&Idl $L))
              ", " (&!= $K $empty) ", 我们有"
              (MB (&cm (&= (Meet (∈ $k $K) $I_k)
                           (Cap (∈ $k $K) $I_k))
                       (&= (Join (∈ $k $K) $I_k)
                           (GI (Cup (∈ $k $K) $I_k)))) ";"))
          (Li (&Fil $L) "是完备格, 其最小元是" (setE $1)
              ", 最大元是" $L ", 对于"
              (&sube (setI $F_k (∈ $k $K))
                     (&Fil $L))
              ", " (&!= $K $empty) ", 我们有"
              (MB (&cm (&= (Meet (∈ $k $K) $F_k)
                           (Cap (∈ $k $K) $F_k))
                       (&= (Join (∈ $k $K) $F_k)
                           (GF (Cup (∈ $k $K) $F_k)))) "."))))
     ((proof)
      
      )
     ((Theorem)
      "设" $L "是一个格 (不必有界), 那么"
      (Ol (Li "对于任意的" (∈ $I_1 $I_2 (&Idl $L))
              ", 我们有"
              (MB (&= (&join $I_1 $I_2)
                      (setI (∈ $a $L)
                            "存在" (&cm (∈ $x_1 $I_1) (∈ $x_2 $I_2))
                            "满足" (&<= $a (&join $x_1 $x_2)))))
              "如果" $L "是分配格, 那么"
              (MB (&= (&join $I_1 $I_2)
                      (setI (&join $i_1 $i_2)
                            (&cm (∈ $i_1 $I_1)
                                 (∈ $i_2 $I_2))))))
          (Li "对于任意的" (∈ $F_1 $F_2 (&Fil $L))
              ", 我们有"
              (MB (&= (&join $F_1 $F_2)
                      (setI (∈ $a $L)
                            "存在" (&cm (∈ $x_1 $F_1) (∈ $x_2 $F_2))
                            "满足" (&<= (&meet $x_1 $x_2) $a))))
              "如果" $L "是分配格, 那么"
              (MB (&= (&join $F_1 $F_2)
                      (setI (&meet $f_1 $f_2)
                            (&cm (∈ $f_1 $F_1)
                                 (∈ $f_2 $F_2))))))))
     ((proof)
      
      )
     ((Corollary)
      
      )
     ((Definition)
      "设" $L "是一个格, " $F "是一个真滤子, "
      $I "是一个真理想."
      (Ol (Li "如果对于任意的" (∈ $x $y $L)
              ", " (∈ (&meet $x $y) $I)
              "可以推出" (∈ $x $I) "或"
              (∈ $y $I) ", 则称" $I
              "为" $L "的素理想 (prime ideal).")
          (Li "如果对于任意的" (∈ $x $y $L)
              ", " (∈ (&join $x $y) $F)
              "可以推出" (∈ $x $F) "或"
              (∈ $y $F) ", 则称" $F
              "为" $L "的素滤子 (prime filter).")))
     ((Theorem)
      "设" $S "是一个格, " (&sube $S $L)
      ", 则下列条件等价:"
      (Ol (Li $S "是素理想;")
          (Li $S^ "是素滤子;")
          (Li "存在格同态" 
              )
          )
      )
     ((proof)
      
      )
     ((Theorem)
      "设" $L "是一个分配格, " (∈ $I (&Idl $L))
      ", " (∈ $F (&Fil $L)) ", 且"
      (&= (&cap $I $F) $empty)
      ", 则存在素理想" $P "使得"
      (&sube $I $P) "且"
      (&= (&cap $P $F) $empty) ".")
     ((proof)
      
      )
     ((Definition)
      "设" $P "是一个偏序集, " (PIdl $P)
      "中的极大元被称为" $P "的极大理想, "
      (PFil $P) "中的极大元被称为" $P
      "的极大滤子. {译注: 原文存在笔误, 将"
      (PIdl $P) "写成了" (PIdl $L) ", "
      (PFil $P) "写成了" (PFil $L) ".}")
     (P "注意, 素理想, 素滤子, 极大理想, 极大滤子"
        "首先都是非空真子集.")
     ((Theorem)
      "设" $L "是一个格, 那么"
      (Ol (Li "若" $L "是分配格, 则极大理想都是素理想, 极大滤子都是素滤子;")
          (Li "若" $L "是补格, 则素理想都是极大理想, 素滤子都是极大滤子;")
          (Li "若" $L "是Boole代数, 则极大理想等同于素理想, 极大滤子等同于素滤子.")))
     ((proof)
      
      )
     (H3. "格中的特殊元素")
     ((Definition)
      "设" $L "是一个格, " (∈ $a $L) "但不是最大元, "
      (∈ $b $L) "但不是最小元. 注意, 我们并没有假定"
      $L "是一个有界格."
      (Ol (Li "若对于任意的" (∈ $x $y $L)
              ", " (&<= (&meet $x $y) $a) "可以推出"
              (&<= $x $a) "或" (&<= $y $a)
              ", 则称" $a "为" $L
              "的交素元 (meet-prime element).")
          (Li "若对于任意的" (∈ $x $y $L)
              ", " (&= (&meet $x $y) $a) "可以推出"
              (&= $x $a) "或" (&= $y $a)
              ", 则称" $a "为" $L
              "的交既约元 (meet-irreducible element).")
          (Li "若对于任意的" (∈ $x $y $L)
              ", " (&<= $b (&join $x $y)) "可以推出"
              (&<= $b $x) "或" (&<= $b $y)
              ", 则称" $b "为" $L
              "的并素元 (join-prime element).")
          (Li ""
              )
          )
      )
     ((Theorem)
      "设" $L "是一个格, 则"
      (Ol (Li ""
              )
          )
      )
     ((proof)
      
      )
     ((Theorem)
      
      )
     ((proof)
      
      )
     (H3. "习题" #:auto? #f)
     ((Exercise)
      "找出所有的" $4 "元偏序集和" $5 "元格.")
     ((answer)
    
      )
     ((Exercise)
      "设" (tu0 $P $<=:id_P) "和" (tu0 $Q $<=:id_Q)
      "是两个偏序集, 在笛卡尔积" (&c* $P $Q)
      "上定义二元关系如下:"
      (MB (&<==>
           (&<= (tu0 $x_1 $y_1) (tu0 $x_2 $y_2))
           (: (&<_P $x_1 $x_2) "或"
              (&cm (&= $x_1 $x_2)
                   (&<=_Q $y_1 $y_2)))) ".")
      "证明: " $<= "是" (&c* $P $Q)
      "上的一个偏序, 其被称为" (&c* $P $Q)
      "上的字典序 (lexicographic order), 且"
      $<= "是全序当且仅当" $<=_P
      "和" $<=_Q "都是全序.")
     ((proof)
      "对于每个" (∈ (tu0 $x $y) (&c* $P $Q))
      ", 我们有" (&= $x $x) "和"
      (&<=_Q $y $y) ", 故"
      (&<= (tu0 $x $y) (tu0 $x $y))
      ", 这就说明了自反性. "
      "对于任意的" (&<= (tu0 $x_1 $y_1) (tu0 $x_2 $y_2))
      "和" (&<= (tu0 $x_2 $y_2) (tu0 $x_3 $y_3))
      ", 鉴于" $<= "的两个条件是互斥的, "
      "所以说其实就是四种情况. "
      )
     ((Exercise)
      "设" $P "是一个偏序集, "
      (func (&cm $f $g) $P $P) "是闭包算子, "
      "证明下列条件等价:"
      (Ol (Li (&<= $f $g) ";")
          (Li (&= (&compose $f $g) $g) ";")
          (Li (&= (&compose $g $f) $g) ";")
          (Li (&sube (&Im $g) (&Im $f)) ".")))
     ((proof)
      (Ol (Li "由1推出2: "
              "根据" $f "的增值性, "
              (&<= (app $g $x) (app $f (app $g $x)))
              ". 根据" (&<= $f $g) ", "
              (&<= (app $f (app $g $x))
                   (app $g (app $g $x)))
              ". 又根据" $g "的幂等性, "
              (&<= (app $f (app $g $x))
                   (app $g $x))
              ". 综上所述, "
              (&= (app $f (app $g $x))
                  (app $g $x))
              ". 鉴于" $x "的任意性, 故"
              (&= (&compose $f $g) $g) ".")
          (Li "由2推出3: "
              "根据" $f "的增值性, "
              (&<= $x (app $f $x))
              ". 根据" $g "的单调性, "
              (&<= (app $g $x) (app $g (app $f $x)))
              ". 根据" (&= (&compose $f $g) $g)
              "和" $g "的幂等性, 可以推出"
              (&= (app $g (app $f (app $g $x)))
                  (app $g (app $g $x))
                  (app $g $x))
              ". 根据" $g "的增值性, "
              (&<= $x (app $g $x))
              ". 根据" $g "和" $f "的单调性, "
              (&<= (app $g (app $f $x))
                   (app $g (app $f (app $g $x))))
              ", 即"
              (&<= (app $g (app $f $x))
                   (app $g $x))
              ". 综上所述, "
              (&= (app $g (app $f $x)) (app $g $x))
              ". 鉴于" $x "的任意性, 故"
              (&= (&compose $g $f) $g) ".")
          (Li "由3推出1: "
              "根据" $g "的增值性, "
              (&<= (app $f $x) (app $g (app $f $x)))
              ". 又根据"
              (&= (&compose $g $f) $g)
              ", 可以得到"
              (&<= (app $f $x) (app $g $x))
              ". 鉴于" $x "的任意性, 有"
              (&<= $f $g) "."))
      "根据以上推理, 可知1, 2, 3是等价的."
      (Ol (Li "由2推出4: "
              "根据"
              (&= (&compose $f $g) $g)
              ", 可知"
              (&= (&Im (&compose $f $g))
                  (&Im $g))
              ". 又因为"
              (&sube (&Im (&compose $f $g))
                     (&Im $f))
              ", 故"
              (&sube (&Im $g) (&Im $f)) ".")
          (Li "由4推出2: "
              "因为"
              (&sube (&Im $g) (&Im $f))
              ", 所以"
              (&in (app $g $x) (&Im $f))
              ". 于是, 存在" (∈ $y $P)
              "使得"
              (&= (app $g $x) (app $f $y))
              ". 那么, 根据" $f
              "的幂等性, 可以推出"
              (&= (app $f (app $g $x))
                  (app $f (app $f $y))
                  (app $f $y)
                  (app $g $x))
              ". 鉴于" $x "的任意性, 可知"
              (&= (&compose $f $g) $g) ".")))
     ((Exercise)
      )
     ((Exercise)
      )
     (H2. "Galois伴随和Galois连接")
     (H3. "Galois伴随")
     ((Definition)
      "设" (func $f $P $Q) "和" (func $g $Q $P)
      "是偏序集之间的两个保序映射, 如果对于任意的"
      (&cm (∈ $a $P) (∈ $b $Q)) "都有"
      (MB (&<==> (&<= (app $f $a) $b)
                 (&<= $a (app $g $b))))
      "那么则称序对" (tu0 $f $g) "是从" $P
      "到" $Q "的一个Galois伴随 (Galois correspondence或"
      "Galois adjunction), 记作"
      (MB (&: (&dashv $f $g)
              (&RightVector $P $Q)) ".")
      "如果" (&= $P $Q) ", 则称" (tu0 $f $g)
      "是" $P "上的一个Galois伴随.")
     ((Example)
      (Ol (Li "设" (&sube $R (&c* $X $Y)) "是一个二元关系, 分别定义"
              (func (^-> $R) (powset $X) (powset $Y)) "和"
              (func (^<- $R) (powset $Y) (powset $X)) "为"
              (MB (&= (app (^-> $R) $A)
                      (setI (∈ $y $Y)
                            (∃ (∈ $x $A)
                               (∈ (tu0 $x $y) $R)))))
              (MB (&= (app (^<- $R) $B)
                      (setI (∈ $x $X)
                            (&=> (∈ (tu0 $x $y) $R)
                                 (∈ $y $B)))))
              "则"
              (&: (&dashv (^-> $R) (^<- $R))
                  (&RightVector (powset $X) (powset $Y)))
              ". {译注: 以上两个操作可以视为映射所诱导的image和preimage概念"
              "在关系上的推广.}")
          (Li "设" (&sube $R (&c* $X $Y)) "是一个二元关系, 分别定义"
              (func (Mmultiscripts $R (Mprescripts) $ $->)
                    (powset $X) (powset $Y))
              "和"
              (func (Mmultiscripts $R (Mprescripts) $ $<-)
                    (powset $Y) (powset $X))
              "为"
              (MB (&= (app (Mmultiscripts $R (Mprescripts) $ $->) $A)
                      (setI (∈ $y $Y)
                            (&=> (∈ (tu0 $x $y) $R)
                                 (∈ $x $A)))))
              (MB (&= (app (Mmultiscripts $R (Mprescripts) $ $<-) $B)
                      (setI (∈ $x $X)
                            (∃ (∈ $y $B)
                               (∈ (tu0 $x $y) $R)))))
              "则"
              (&: (&dashv (Mmultiscripts $R (Mprescripts) $ $<-)
                          (Mmultiscripts $R (Mprescripts) $ $->))
                  (&RightVector (powset $Y) (powset $X)))
              ".")
          (Li "设" (func $f $X $Y) "是一个映射, 将" $f "视为二元关系"
              (setI (tu0 $x (app $f $x)) (∈ $x $X)) ", 那么"
            
              )
          )
      )
     ((Theorem)
      "设" (func $f $P $Q) "和" (func $g $Q $P)
      "是偏序集之间的两个保序映射, 则下列条件等价:"
      (Ol (Li (&: (&dashv $f $g)
                  (&RightVector $P $Q)) ";")
          (Li (&<= (&id $P) (&i* $g $f)) ", "
              (&<= (&i* $f $g) (&id $Q)) ".")))
     ((proof)
      )
     ((Theorem)
      "设" (&: (&dashv $f $g) (&RightVector $P $Q))
      ", 那么"
      (Ol (Li (&= (&i* $f $g $f) $f) ", "
              (&= (&i* $g $f $g) $g) ";")
          (Li (func (&i* $g $f) $P $P)
              "是闭包算子, "
              (func (&i* $f $g) $Q $Q)
              "是内部算子.")))
     
     )))