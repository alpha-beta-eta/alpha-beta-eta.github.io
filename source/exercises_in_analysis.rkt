#lang racket
(provide exercises_in_analysis.html)
(require SMathML)
(define (H4. #:attr* [attr* '()] #:id [id #f] #:switch? [switch? #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (let ((l (length section)))
         (cond
           ((= l 2) (format "~s.~s" (car section) index))
           ((= l 3) (format "~s.~s" (cadr section) index))
           ((= l 4) (format "~s.~s" (caddr section) index))
           (else (error 'format-num "unknown format of section: ~s" section))))))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B (format "~a~a. " name num))
        (B (format "~a. " name)))))
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
    (Cite name `(a ((href ,href)) ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Remark "评注" "remark")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise")
  (Theorem "定理" "theorem")
  (Corollary "推论" "corollary")
  
  )
(define $\; (Mo ";"))
(define &\;
  (make-op
   $\;
   (lambda () $)
   (lambda (x) x)))
(define (tup0 . x*)
  (par0 (apply &\; x*)))
(define (Conti X Y)
  (ap $C (tup0 X Y)))
(define L^pΩ
  (app $L^p $Omega:normal))
(define $Hat (Mo "&Hat;"))
(define (Hat x) (^^ x $Hat))
(define (make-d d)
  (lambda (x y)
    (appl d x y)))
(define-syntax-rule (define-d* (id d) ...)
  (begin
    (define id (make-d d))
    ...))
(define-d*
  (d $d)
  (dp (_ (Hat $d) $p))
  (d∞ (_ (Hat $d) $inf))
  (dX $d_X)
  (dXk (_ $d $X_k))
  (dR $d_RR))
(define @sum (compose pare sum))
(define (∀ . arg*)
  (: $forall (apply ∈ arg*)))
(define (distance x y)
  (&abs (&- $x $y)))
(define exercises_in_analysis.html
  (TnTmPrelude
   #:title "分析学练习"
   #:css "styles.css"
   (H1. "分析学练习")
   (P "这是我阅读Exercises in Analysis (Part 1) 所做的笔记, "
      "当然很大一部分其实是翻译.")
   (H2. "度量空间")
   (H3. "导论")
   (H4. "基本定义和记号")
   ((Definition)
    "度量空间的定义而已, 略去.")
   ((Remark)
    "若是修改度量空间的定义, 允许互异的元素之间的距离为零, "
    "那么我们就得到了伪度量空间, 或者说半度量空间, "
    "此时的距离函数被称为伪度量, 半度量, 或者ecart.")
   ((Example)
    (Ol #:attr* '((type "a"))
        (Li "设" (_^ (setE (tu0 $X_k (_ $d $X_k))) (&= $k $1) $N)
            "是度量空间, 置"
            (MB (&= $X (prod (&= $k $1) $N $X_k)))
            "且"
            (MB (&cm (&= (dp $x $y)
                         (^ (@sum (&= $k $1) $N
                                  (^ (dXk $x_k $y_k) $p))
                            (~ $1 $p)))
                     (∀ $x $y $X)))
            "而"
            (MB (&cm (&= (d∞ $x $y)
                         (ap $max
                             (setI (dXk $x_k $y_k)
                                   (&<= $1 $k $N))))
                     (∀ $x $y $X)))
            "其中" (&< (&<= $1 $p) $inf) ", "
            (∈ (&= $x (_^ (@ $x_k) (&= $k $1) $N))
               (&= $y (_^ (@ $y_k) (&= $k $1) $N)) $X)
            ". 那么, " (tu0 $X (_ (Hat $d) $p)) "和"
            (tu0 $X (_ (Hat $d) $inf)) "是度量空间. "
            "以下是一个例子. 如果"
            (MB (&cm (&= (dR $x $y) (distance $x $y))
                     (∀ $x $y $RR)))
            "那么" $d_RR "是" $RR "上的一个度量. "
            "对于" (&>= $N $1) ", 如果每个"
            (&= $X_k $RR) ", 那么" (_ (Hat $d) $2)
            "是" $RR^N "上所谓的Euclid度量.")
        (Li ""
            )
        )
    )
   ((Proposition)
    
    )
   ((Remark)
    
    )
   ((Definition)
    
    )
   (H4. "序列和完备度量空间")
   ((Definition)
    
    )
   (H4. "度量空间的拓扑")
   ((Definition)
    
    )
   ((Proposition)
    
    )
   ((Proposition)
    
    )
   ((Proposition)
    
    )
   ((Definition)
    
    )
   ((Definition)
    
    )
   ((Theorem)
    
    )
   ((Proposition)
    
    )
   ((Definition)
    
    )
   ((Remark)
    
    )
   ((Proposition)
    
    )
   ((Corollary)
    
    )
   ((Definition)
    
    )
   ((Definition)
    
    )
   ((Proposition)
    
    )
   ((Remark)
    
    )
   ((Proposition)
    
    )
   (H4. "Baire纲定理")
   (P "度量空间最重要的性质之一是完备性, 而许多分析学的基础结果都严重依赖于该性质. "
      "完备性是藉由所谓的Baire纲定理而成为强大的工具的.")
   ((Definition)
    
    )
   ((Theorem)
    
    )
   ((Corollary)
    
    )
   ((Theorem)
    
    )
   (H4. "连续函数和一致连续函数")
   (H4. "度量空间的完备化: 度量的等价")
   (H4. "映射的逐点收敛和一致收敛")
   (H4. "紧度量空间")
   (H4. "单位分解")
   (H4. "度量空间的积")
   (H4. "辅助概念")
   (H3. "练习")
   ((Exercise)
    
    )
   (H3. "解答")
   (H2. "拓扑空间")
   (H3. "导论")
   ((Definition) "拓扑空间的定义.")
   ((Remark)
    ""
    )
   ((Definition)
    ""
    )
   ((Definition)
    "令" (tu0 $X $tau) "是一个拓扑空间."
    (Ol #:attr* '((type "a"))
        (Li $X "是一个" $T_0 "空间, 或者说Kolmogorov空间, 如果对于不同的点"
            (∈ $x $y $X) ", 我们可以找到一个" (∈ $U $tau) "满足"
            (&cm (∈ $x $U) (&!in $y $U)) "或者"
            (&cm (&!in $x $U) (∈ $y $U)) ". [注记: 原文有误.]")
        (Li $X "是一个" $T_1 "空间, 或者说Hausdorff空间, 或者说分离空间, "
            "如果对于不同的点" (∈ $x $y $X) ", 我们可以找到" (∈ $U $V $tau)
            "满足" (&cm (∈ $x $U) (&!in $y $U)) "且"
            (&cm (&!in $x $V) (∈ $y $V)) ".")
        (Li $X "是一个" $T_2 "空间, 如果对于不同的点" (∈ $x $y $X)
            ", 我们可以找到" (∈ $U $V $tau) "满足"
            (&cm (∈ $x $U) (∈ $y $V)) "且" (&= (&cap $U $V) $empty) ".")
        (Li $X "是一个" $T_3 "空间, 或者说正则空间, 或者说Vietoris空间, "
            "如果对于每个" (∈ $x $X) "和每个闭集" (&sube $C $X)
            ", 我们可以找到" (∈ $U $V $tau) "满足"
            (&cm (∈ $x $U) (&sube $C $V)) "且"
            (&= (&cap $U $V) $empty) ".")
        (Li $X "是一个" $T_4 "空间, 或者说正规空间, 或者说Tietze空间, "
            "如果对于不相交的闭集" (&sube (&cm $C $D) $X)
            ", 我们可以找到" (∈ $U $V $tau) "满足"
            (&cm (&sube $C $U) (&sube $D $V)) "且"
            (&= (&cap $U $V) $empty) ".")))
   ((Remark)
    ""
    )
   ((Remark)
    ""
    )
   ((Definition)
    ""
    )
   
   (H4. "基本定义和记号")
   (H4. "拓扑基和子基")
   (H4. "网")
   (H4. "连续函数和半连续函数")
   (H4. "开映射和闭映射: 同胚")
   (H4. "弱拓扑和强拓扑 (或者说始拓扑和终拓扑)")
   (H4. "紧拓扑空间")
   (H4. "连通性")
   (H4. "Urysohn延拓定理和Tietze延拓定理")
   (H4. "仿紧空间和Baire空间")
   (H4. "波兰集合和Suslin集合")
   (H4. "Michael选择定理")
   (H4. (Conti $X $Y) "空间")
   (H4. "代数拓扑基础I: 同伦")
   (H4. "代数拓扑基础II: 同调")
   (H3. "练习")
   (H3. "解答")
   (H2. "测度, 积分, 鞅")
   (H3. "导论")
   (H4. "基本定义和记号")
   (H4. "测度和外测度")
   (H4. "Lebesgue测度")
   (H4. "原子测度和非原子测度")
   (H4. "积测度")
   (H4. "Lebesgue-Stieltjes测度")
   (H4. "可测函数")
   (H4. "Lebesgue积分")
   (H4. "收敛定理")
   (H4. $L^p "空间")
   (H4. "多重积分: 换元法")
   (H4. "一致可积: Modes of Convergence")
   (H4. "带符号的测度")
   (H4. "Radon-Nikodym定理")
   (H4. "极大函数和Lyapunov凸定理")
   (H4. "条件期望和鞅")
   (H3. "练习")
   (H3. "解答")
   (H2. "测度和拓扑")
   (H3. "导论")
   (H4. "Borel " $sigma "代数和Baire " $sigma "代数")
   (H4. "正则测度和Radon测度")
   (H4. "连续函数的Riesz表示定理")
   (H4. "概率测度的空间: Prohorov定理")
   (H4. "波兰空间, Suslin空间, Borel空间")
   (H4. "可测多值函数: 选择定理")
   (H4. "投影定理")
   (H4. "对于" (&<= $1 $p $inf) "的" L^pΩ "的对偶")
   (H4. "测度的序列: " L^pΩ "中的弱收敛")
   (H4. "覆盖定理")
   (H4. "Lebesgue微分定理")
   (H3. "练习")
   (H3. "解答")
   (H2. "泛函分析")
   (H3. "导论")
   (H3. "练习")
   (H3. "解答")
   ))