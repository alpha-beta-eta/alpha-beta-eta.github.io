#lang racket
(provide kailai.html)
(require SMathML)
(define $+:compact (set-compact $+))
(define $-:compact (set-compact $-))
(define (Left x) (: x $-:compact))
(define (Right x) (: x $+:compact))
(define kailai.html
  (TnTmPrelude
   #:title "概率论教程"
   #:css "styles.css"
   (H1. "概率论教程")
   (P "这是个人阅读钟开莱所著的概率论教程一书的笔记.")
   (H2. "分布函数")
   (H3. "单调函数")
   (P "本书的正负增减均为广义, 即"
      (Q $x "为正") "指的是" (Q (&>= $x $0))
      ". 若要使用传统意义, 则须加"
      (Q "严格") "二字.")
   (P "设" (func $f $RR $RR) "是一个增函数, "
      "则对于任意两个实数" $x_1 "和" $x_2
      ", 我们有"
      (MBL "(1)"
           (&=> (&< $x_1 $x_2)
                (&<= (app $f $x_1)
                     (app $f $x_2))) ".")
      "当然了, 这也等价于"
      (MB (&=> (&<= $x_1 $x_2)
               (&<= (app $f $x_1)
                    (app $f $x_2))) "."))
   (P "我们使用" (&uarr $t $x)
      "表示" $t "从左侧趋近于" $x
      ", " (&darr $t $x)
      "表示" $t "从右侧趋近于" $x ".")
   ((Proposition #:id "increasing-unilateral-limits")
    "对于每个" $x ", " $f "在" $x
    "处的左极限和右极限均(严格)存在, 其记号分别为"
    (MBL "(2)"
         (&= (app $f (Left $x))
             (lim (&uarr $t $x)
                    (app $f $t)))
         "和"
         (&= (app $f (Right $x))
             (lim (&darr $t $x)
                    (app $f $t))) ".")
    $f "在无穷远处的极限亦(广义)存在, "
    "其记号分别为"
    (MB (&= (app $f (&- $inf))
            (lim (&darr $t (&- $inf))
                   (app $f $t)))
        "和"
        (&= (app $f (&+ $inf))
            (lim (&uarr $t (&+ $inf))
                   (app $f $t))) ".")
    "前者可能为" (&- $inf)
    ", 后者可能为" (&+ $inf) ".")
   ((proof)
    "对于任意的实数" $x ", 设"
    (MB (&= $a
            (supremum
             (&< $t $x)
             (app $f $t))) ".")
    "根据单调性和确界原理, "
    "这个上确界是良定的. "
    "对于每个" (&> $epsilon $0)
    ", 存在" (&> $delta $0) "满足"
    (MB (&> (app $f (&- $x $delta))
            (&- $a $epsilon)) ".")
    "根据单调性, 对于每个实数" $t "满足"
    (&< (&- $x $delta) $t $x)
    ", 我们有"
    (MB (&> (app $f $t)
            (&- $a $epsilon)) ".")
    "并且, 我们还知道"
    (MB (&<= (app $f $t) $a) ".")
    "因此, 可以推出"
    (MB (&< (&abs (&- (app $f $t) $a))
            $epsilon) ".")
    "这告诉我们" $f "在" $x
    "处的左极限存在, 且"
    (MB (&= (app $f (Left $x))
            (supremum
             (&< $t $x)
             (app $f $t))) ".")
    "如法炮制, 我们可以证明"
    (MB (&= (app $f (Right $x))
            (infimum
             (&> $t $x)
             (app $f $t))) ".")
    "对于趋向无穷远处的极限, 证明也是类似的.")
   ((Proposition #:id "increasing-continuous")
    "对于每个实数" $x ", " $f "在"
    $x "处连续当且仅当"
    (MB (&= (app $f (Left $x))
            (app $f $x)
            (app $f (Right $x))) "."))
   ((proof)
    "函数" $f "在" $x "处连续当且仅当"
    (MB (&= (lim $t $x (app $f $t))
            (app $f $x)) ".")
    "而这又当且仅当"
    (MB (&= (app $f (Left $x))
            (app $f $x))
        "且"
        (&= (app $f (Right $x))
            (app $f $x)) ".")
    "另外, 原书的证明方法给出了"
    (MBL "(3)"
         (&<= (app $f (Left $x))
              (app $f $x)
              (app $f (Right $x))))
    "作为中间步骤.")
   (P "一般而言, 我们称" $f "在" $x
      "处有一个" (Em "跳跃")
      ", 如果(2)中的两个极限均存在且不相等. "
      $f "在" $x "处的值本身, 即" (app $f $x)
      ", 则可以是任意的. 但对于增函数" $f
      "而言, 关系(3)必然成立. 作为"
      (Ref "increasing-unilateral-limits")
      "和" (Ref "increasing-continuous")
      "的推论, 我们有以下结果.")
   ((Proposition)
    "一个增函数的唯一可能的非连续性种类为跳跃. "
    "{读者还应该自问, 对于一般的函数, "
    "还存在哪些种类的非连续性.}")
   
   (H3. "分布函数")
   (H3. "绝对连续分布和奇异分布")
   (H2. "测度论")
   (H3. "集类")
   
   (H3. "概率测度及其分布函数")
   
   (H2. "随机变量 期望 独立性")
   
   (H2. "收敛概念")
   (H3. "各种收敛方式")
   (H2. "大数定律 随机级数")
   (H2. "特征函数")
   (H2. "中心极限定理及其分歧")
   (H2. "随机游走")
   (H2. "条件期望 Markov性 鞅")
   ))