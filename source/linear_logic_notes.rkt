#lang racket
(provide linear_logic_notes.html)
(require SMathML)
(define $\|- (Mo "&vdash;"))
(define (&\|- . x*)
  (let-values (((y* z*) (split-at-right x* 1)))
    (: (apply &cm y*) $\|- (car z*))))
(define $UnderBrace (Mo "&UnderBrace;"))
(define UnderBrace
  (case-lambda
    ((x) (Munder x $UnderBrace))
    ((x y) (Munder (UnderBrace x) y))))
(define Δ $Delta:normal)
(define Δ^ (&prime Δ))
(define $lolli (Mo "&multimap;"))
(define $-o $lolli)
(define $o* (Mo "&otimes;"))
(define $o*:id (Mi "&otimes;"))
(define $rule:⊗R (: $o*:id $R))
(define $rule:⊗L (: $o*:id $L))
(define $dumb (Mi "-"))
(define $eph (Mi "eph" #:attr* '((mathvariant "italic"))))
(define (&eph prop)
  (: prop (&space 2) $eph))
(define (Menclose #:attr* [attr* '()] . xml*)
  `(menclose ,attr* . ,xml*))
(define (Menclose:updiagonalstrike #:attr* [attr* '()] . xml*)
  `(menclose ,(attr*-set attr* 'notation "updiagonalstrike") . ,xml*))
(define UpStrike Menclose:updiagonalstrike)
(define const:a $a:sans-serif)
(define const:b $b:sans-serif)
(define const:c $c:sans-serif)
(define const:d $d:sans-serif)
(define const:e $e:sans-serif)
(define (Const str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $edge (Const "edge"))
(define (&edge x y)
  (appl $edge x y))
(define $node (Const "node"))
(define (&node x)
  (app $node x))
(define $path (Const "path"))
(define (&path x y)
  (appl $path x y))
(define $sym (Const "sym"))
(define $refl (Const "refl"))
(define $trans (Const "trans"))
(define (&rulel #:label [label #f] . j*)
  (if label
      (: (apply &rule j*) label)
      (apply &rule j*)))
(define $e:rule (Const "e"))
;do not compose well, need refactoring
(define (rule:sym x y)
  (&rulel
   #:label $sym
   (&edge x y)
   (&edge y x)))
(define (rule:e x y)
  (&rulel
   #:label $e:rule
   (&edge x y)
   (&path x y)))
(define (rule:trans x y z)
  (&rulel
   #:label $trans
   (&path x y) (&path y z)
   (&path x z)))
(define (make-proof-instance proof conclusion)
  (vector 'proof-instance proof conclusion))
(define (proof-instance-proof proof-instance)
  (vector-ref proof-instance 1))
(define (proof-instance-conclusion proof-instance)
  (vector-ref proof-instance 2))
(define (instantiate-prop prop)
  (make-proof-instance prop prop))
(define (make-rule-instance premise* label conclusion)
  (vector 'rule-instance premise* label conclusion))
(define (rule-instance-premise* rule-instance)
  (vector-ref rule-instance 1))
(define (rule-instance-label rule-instance)
  (vector-ref rule-instance 2))
(define (rule-instance-conclusion rule-instance)
  (vector-ref rule-instance 3))
(define (make-rule:e-instance x y)
  (make-rule-instance
   (list (&edge x y)) ;premise*
   $e:rule ;label
   (&path x y) ;conclusion
   ))
(define (make-rule:sym-instance x y)
  (make-rule-instance
   (list (&edge x y))
   $sym
   (&edge y x)))
(define (make-rule:trans-instance x y z)
  (make-rule-instance
   (list (&path x y) (&path y z))
   $trans
   (&path x z)))
(define (direct-instantiate rule-instance)
  (let ((premise* (rule-instance-premise* rule-instance))
        (label (rule-instance-label rule-instance))
        (conclusion (rule-instance-conclusion rule-instance)))
    (make-proof-instance
     (keyword-apply
      &rulel
      '(#:label) (list label)
      (append premise* (list conclusion)))
     conclusion)))
(define (check-validity proof-instance* rule-instance)
  (let ((conclusion* (map proof-instance-conclusion proof-instance*))
        (premise* (rule-instance-premise* rule-instance)))
    (unless (equal? conclusion* premise*);?
      (error 'compose-proof-instance*
             "invalid proof composition:\n~s\n~s"
             conclusion* premise*))))
(define (compose-proof-instance* rule-instance . proof-instance*)
  (check-validity proof-instance* rule-instance)
  (let ((proof* (map proof-instance-proof proof-instance*))
        (label (rule-instance-label rule-instance))
        (conclusion (rule-instance-conclusion rule-instance)))
    (make-proof-instance
     (keyword-apply
      &rulel
      '(#:label) (list label)
      (append proof* (list conclusion)))
     conclusion)))
(define (lookup label env)
  (cond ((assq label env) => cdr)
        (else (error 'render-proof-tree
                     "unknown rule ~s" label))))
(define (render-proof-tree env tree)
  (define (interp-rule rule)
    (match rule
      ((,label . ,arg*)
       (apply (lookup label env) arg*))))
  (define (interp tree)
    (match tree
      ((<= ,rule . ,proof*)
       (define r (interp-rule rule))
       (define p* (map interp proof*))
       (apply compose-proof-instance* r p*))
      ((prop ,prop)
       (instantiate-prop prop))
      (,rule
       (direct-instantiate
        (interp-rule rule)))))
  (proof-instance-proof (interp tree)))
(define $succ $s:sans-serif)
(define (&succ n)
  (app $succ n))
(define $even (Const "even"))
(define $odd (Const "odd"))
(define (&even n)
  (app $even n))
(define (&odd n)
  (app $odd n))
(define $zero
  (Mn "0" #:attr* '((mathvariant "sans-serif"))))
(define (make-even:0-instance)
  (make-rule-instance
   (list)
   #f
   (&even $zero)))
(define (make-evenodd-instance x)
  (make-rule-instance
   (list (&even x))
   #f
   (&odd (&succ x))))
(define (make-oddeven-instance x)
  (make-rule-instance
   (list (&odd x))
   #f
   (&even (&succ x))))
(define env:nat
  `((even:0 . ,make-even:0-instance)
    (evenodd . ,make-evenodd-instance)
    (oddeven . ,make-oddeven-instance)))
(define $nickel (Const "n"))
(define $dime (Const "d"))
(define $quarter (Const "q"))
(define $opportunity (Const "opportunity"))
(define $knocks (Const "knocks"))
(define (&knocks x)
  (app $knocks x))
(define $at (Const "at"))
(define (&at x)
  (app $at x))
(define $~> (Mo "&rarrw;"))
(define-infix*
  (&~> $~>)
  (&-o $-o)
  (&lolli $lolli)
  (&o* $o*)
  
  )
(define $step (Const "step"))
(define (make-step-instance x y)
  (make-rule-instance
   (list (&at x) (&edge x y))
   $step
   (&at y)))
(define env:graph
  `((e . ,make-rule:e-instance)
    (sym . ,make-rule:sym-instance)
    (trans . ,make-rule:trans-instance)
    (step . ,make-step-instance)))
(define (--> x0 x1 . x*)
  (let iter ((at x1)
             (proof `(step ,x0 ,x1))
             (x* x*))
    (if (null? x*)
        proof
        (let ((x (car x*))
              (x* (cdr x*)))
          (iter x
                `(<= (step ,at ,x)
                     ,proof
                     (prop ,(&edge at x)))
                x*)))))
(define linear_logic_notes.html
  (TnTmPrelude
   #:title "线性逻辑笔记"
   #:css "styles.css"
   (H1. "线性逻辑笔记")
   (P "学习"
      (A "CMU 15-816"
         #:attr* '((href "https://www.cs.cmu.edu/~fp/courses/15816-s12/schedule.html")))
      "的一些翻译和笔记.")
   (H2. "演绎推理")
   (H3. "例子: 对于图进行推理")
   (P "作为第一个例子, 我们考虑图 (graph). 我们将结点 (node) (顶点, vertex) 表示为"
      (Em "常量(constant)") " (" const:a ", " const:b
      ", ...) , 而边 (edge) 表示为一个二元" (Em "谓词(predicate)") $edge
      ", 其将相互连接的结点联系起来.")
   
   (P "以上的示例图可以被表示为" (Em "命题(proposition)") " (原文的命题为复数形式)"
      (MB (&cm (&edge const:a const:b)
               (&edge const:b const:c)
               (&edge const:a const:c)
               (&edge const:a const:d)))
      "读者可能会立即注意到一点不太匹配的地方, 即图片中的边似乎是无向的 (undirected), "
      "而对于边的表示并非对称 (例如, " (&edge const:b const:a)
      "就并不存在). 我们可以修复这点不足之处, 通过提供一条"
      (Em "推理规则(rule of inference)") "以要求" $edge "关系是对称的 (symmetric)."
      (MB (&rulel
           #:label $sym
           (&edge $x $y)
           (&edge $y $x)))
      "我们可以应用这条推理规则于事实" (&edge const:a const:b) "以推导出 (deduce) "
      (&edge const:b const:a) ". 在这个应用里, 我们实例化 (instantiate) 了"
      (Em "模式变量(schematic variable)") $x "和" $y "以" const:a "和"
      const:b ". 模式变量会以斜体排版, 以将其与常量区分. 水平线之上的命题被称为规则的"
      (Em "前提(premise)") ", 而水平线之下的命题则被称为"
      (Em "结论(conclusion)") ". 这条作为例子的规则只有一个前提和一个结论. "
      $sym "是这条规则的" (Em "名字(name)") "或者说" (Em "标签(label)")
      ". 我们经常省略规则的名字, 如无引用规则的特别需要的话.")
   (P "根据这条单一规则和描述初始图的事实, 我们现在可以推导出以下额外的事实:"
      (MB (&cm (&edge const:b const:a)
               (&edge const:c const:b)
               (&edge const:c const:a)
               (&edge const:d const:a)))
      "此时此刻我们尚不能在图与其逻辑表示之间来回切换, "
      "因为一个孤立的结点不会出现在边关系里. "
      "因此, 我们需要第二个谓词" $node
      ", 其对于图中的每个结点成立."
      (MB (&cm (&node const:a)
               (&node const:b)
               (&node const:c)
               (&node const:d))))
   (P "既已设计了图的一种逻辑表示, 现在我们定义图上的一个关系. "
      "如果图中存在一条从" $x "到" $y "的路径 (path), 我们记"
      (&path $x $y) ". 对于图而言常见的情况是, "
      "我们不想考虑平凡的从一个结点到自身的零长度路径. "
      "如果我们的确要考虑的话, 那将会是以下规则 "
      "(写在方括号里以指示这仅是假想性的):"
      (MB (brac
           (&rulel
            #:label $refl
            (&node $x)
            (&path $x $x))))
      "如果我们省略了这条规则的前提, 那么这个规则是成问题的, "
      "因为其可以用于甚至并非图的结点的对象" $x
      ", 导致毫无意义 (nonsensical) 的结论.")
   (P "现在以下的两条规则定义了路径的概念. 第一条"
      $e:rule "是说每条边都代表了一条合法 (valid) 的路径, "
      "第二条" $trans "是说路径可以被复合 (compose), 使得"
      $path "成为传递关系."
      (MB ((&split 16)
           (rule:e $x $y)
           (rule:trans $x $y $z)))
      "根据对于我们示例图的表示, 现在可以提供以下存在从"
      const:c "到" const:d "的路径的证明:"
      (MB (render-proof-tree
           env:graph
           `(<= (trans ,const:c ,const:a ,const:d)
                (<= (e ,const:c ,const:a)
                    (sym ,const:a ,const:c))
                (e ,const:a ,const:d))))
      "我们可以检视这个证明, 然后发现其携带了一些信息. "
      "它不仅仅是为了说服我们存在一条从" const:c "到"
      const:d "的路径, 而是告诉了我们路径是什么. "
      "这条路径从" const:c "出发走到" const:a
      ", 然后从" const:a "走到" const:d
      ". 这是一个关于证明中的"
      (Em "构造性内容(constructive content)")
      "的例子, 而我们之后将会看到诸多其他的例子. "
      "对于目前我们已有的系统而言, "
      "从一般角度来说, 我们的确可以从证明中读出路径, "
      "并且如果我们心中有一条路径, "
      "则总是可以构造一个证明. "
      "但是, 以我们选择的规则而言, "
      "一些路径并不对应于唯一的证明. "
      "[译注: 这最后一句话里的路径 (path) 指的是实际所走的路线, "
      "例如对于结点" $x "和" $y ", 如果我们拥有对于"
      (&path $x $y) "的一个证明, 那么说明存在一条实际的可以从"
      $x "走到" $y "的路. 但是, 对于这条实际的路线而言, "
      "也可以存在不同的证明. 读者需要理解的是, "
      (&path $x $y) "存在证明只是说明至少存在一条路线, "
      "但是可以有各种不同的路线, 而且甚至不同的证明"
      "可以对应于相同的路线, 或者说相同的构造性内容. "
      "原文没有特别区分路径和实际所走的路线. "
      "而且, 根据后文, 其实每种路线都可以有无限多不同的证明.]")
   (P "实际上, 歧义的来源不止一处. "
      "[译注: 这里的歧义指同一路径的不同证明可以拥有相同的实际路线.] "
      "一方面是我们可以从" (&edge const:c const:a)
      "回到" (&edge const:a const:c) "然后再回到"
      (&edge const:c const:a) ", 如此反复下去, "
      "可以产生无限多的对于" (&edge const:c const:a)
      "的证明. 另一方面, 具有多于三个结点的路径"
      "可以被分解为不同的子路径, 由此以不同的方式使用传递性. "
      "不同的证明可以拥有相同的构造性内容并没有使得其解释不合理, "
      "但是我们应该意识到这件事情的存在.")
   (P "既然我们排除了自反性, 在何种条件下我们仍然可以" (Em "证明")
      (&path $x $x) "呢? 因为我们考虑的是无向图, 存在一条从"
      $x "到" $x "的路径恰当" $x "至少存在一个邻居 (neighbor), "
      "正如以下证明所示:"
      (MB (render-proof-tree
           env:graph
           `(<= (trans ,$x ,$y ,$x)
                (e ,$x ,$y)
                (<= (e ,$y ,$x)
                    (sym ,$x ,$y)))))
      "这个证明存在诸多有趣的方面. 例如, 它不依赖于"
      $x "和" $y "到底是什么. 换言之, 其对于"
      $x "和" $y "是" (Em "模式性的(schematic)")
      ". 另一值得注意的方面在于其用了" (&edge $x $y)
      "两次, 而这在直觉上是成立的: 离开" $x
      "然后回到" $x "的一般方法是先去到任意相邻的"
      $y ", 然后立即返回" $x ", 复用相同的边. "
      "我们可以将以上的推导 (deduction) 总结为一条单独的"
      (Em "导出推理规则(derived rule of inference)") ":"
      (MB (&rule (&edge $x $y)
                 (&path $x $x)))
      "这条推理规则是合理的 (justified), "
      "因为我们可以将其任意的特定实例替换以"
      "我们上面给出的模式性证明的一个实例. "
      "我们将在之后的讲座里看到, "
      "导出推理规则在逻辑学中扮演着非常重要的角色.")
   (H3. "例子: 自然数")
   (P "作为第二个例子, 我们考虑自然数"
      (&cm $0 $1 $2 $..h)
      ". 一种构造自然数的便捷方法是通过迭代应用后继函数"
      $succ "于" $zero ", 记作"
      (MB (&cm $zero
               (&succ $zero)
               (&succ (&succ $zero))
               $..h))
      "我们将" $succ "称为一个" (Em "构造子(constructor)")
      ". 现在我们可以通过以下三条规则定义偶数和奇数."
      (MB ((&split 16)
           (&rule $ (&even $zero))
           (&rule (&even $x)
                  (&odd (&succ $x)))
           (&rule (&odd $x)
                  (&even (&succ $x)))))
      "作为导出推理规则的例子, 我们可以将左边的证明总结为右边的规则:"
      (MB ((&split 16)
           (render-proof-tree
            env:nat
            `(<= (oddeven ,(&succ $x))
                 (evenodd ,$x)))
           (&rule (&even $x)
                  (&even (&succ (&succ $x))))))
      "这些例子里的证明的结构都不是特别有趣, "
      "因为数 (number, 这里是动词) 数字"
      $n "为奇还是偶的证明不过就是遵循数字" $n "的结构罢了.")
   (H3. "例子: 硬币交换")
   (P "到目前为止, 演绎推理总是在积累知识, "
      "因为我们已经所意识到为真的命题仍然保持为真. "
      "线性逻辑起源于一个简单的观察:"
      (Blockquote
       (Em "Truth is ephemeral."))
      "例如, 在进行这个讲座时"
      (Q (Em "Frank is holding a piece of chalk"))
      "为真, 而现在(很可能)不是了. 因此, 真性随着时间而变化, "
      "而这种现象以" (Em "时态逻辑(temporal logic)")
      "研究. 在" (Em "线性逻辑(linear logic)")
      "中, 我们关心的则是随着" (Em "状态改变(change of state)")
      "的真性改变 (change of truth). "
      "我们以简单的方式对此进行模拟: "
      "当一条推理规则被应用时, 我们" (Em "消费(consume)")
      "了用作前提的命题, 而" (Em "产生(produce)")
      "了结论中的命题, 因此引发了状态的总体性变化.")
   (P "作为一个例子, 我们考虑值" $5 "分钱的" (Em "nickel")
      ", 值" 10 "分钱的" (Em "dime") ", 值" 25
      "分钱的" (Em "quarter")
      ". 我们拥有如下用于在它们之间进行交换的规则:"
      (MB ((&split 16)
           (&rule $dime $dime $nickel
                  $quarter)
           (&rule $quarter
                  ((&split 8)
                   $dime $dime $nickel))
           (&rule $nickel $nickel
                  $dime)
           (&rule $dime
                  ((&split 8)
                   $nickel $nickel))))
      "第二条和第四条规则是我们第一次看到具有多于一个结论的规则. "
      "现在推理可以改变状态. 例如, 如果我们有三个dime和一个nickel, "
      "那么状态可以写成"
      (MB (&cm $dime $dime $dime $nickel))
      "应用第一条规则, 我们可以将两个dime和一个nickel"
      "转换为一个quarter以得到状态"
      (MB (&cm $dime $quarter))
      "注意到总钱数" 35 "分保持未变, 这是硬币交换的要义. "
      "一种写下这样的推理的方式在于划去被消费的命题而加上产生的命题. "
      "对于以上例子而言, 我们可以写成"
      (MB (&~> (&cm $dime $dime $dime $nickel)
               (&cm (UpStrike $dime) (UpStrike $dime)
                    $dime (UpStrike $nickel) $quarter)))
      "为了理解证明的意义, 考虑如何将三个dime换为一个quarter和一个nickel: "
      "首先, 我们将一个dime换为两个nickel, 然后再将剩下来的两个dime和"
      "其中一个nickel换为一个quarter. 正如以下两次状态转移所示:"
      (MB (&~> (&cm $dime $dime $dime)
               (&cm $dime $dime (UpStrike $dime)
                    $nickel $nickel)
               (&cm (UpStrike $dime) (UpStrike $dime) (UpStrike $dime)
                    (UpStrike $nickel) $quarter $nickel)))
      "使用推理规则的记号, 这个推导显示在左边, 而相应的导出推理规则在右边."
      (MB ((&split 16)
           (&rule $dime $dime $nickel
                  $quarter)
           (&rule $dime
                  ((&split 8) $nickel $nickel))
           (&rule $dime $dime $dime
                  ((&split 8)
                   $quarter $nickel))))
      "[译注: 左边两个证明其实是一个证明的组件, "
      "但是限于MathML的排版能力, "
      "我实在不知道存在什么优雅的方式可以将它们组合起来, "
      "只好暂时作罢.]")
   (P "总结一下: 我们可以改变推理的本质 (very nature), 如果我们"
      (Em "消费") "前提中所用的命题以" (Em "产生")
      "结论中的命题. 这是线性逻辑的基础, 因此我们将其称为"
      (Em "线性推理(linear inference)")
      ". 以下是提醒我们的精辟之语:"
      (Blockquote
       (Em "Linear inference can change the world.")))
   (H3. "例子: 画图 (Graph Drawing)")
   (P "我们继续来看一个牵涉线性推理的稍微复杂一点的例子. "
      "之前我们使用了常规的演绎推理以定义路径的概念. "
      "这次我们想要模拟不提起笔画图. "
      "这与遍历整个图而每条边恰走一次是等价的. "
      "[译注: 这里说的画图其实就是" (Q "一笔画") "问题.] "
      "这种重述暗示了以下的想法: "
      "当我们沿着一条边走的时候, 我们就" (Em "消费")
      "了这条边, 于是我们不能再走一次这条边. "
      "我们也需要追踪我们在哪里, 因此我们引入了新的谓词"
      $at ", 其满足" (&at $x) "为真, 如果我们在结点"
      $x "这个位置上. 然后, 唯一的" (Em "线性")
      "推理规则是"
      (MB (render-proof-tree
           env:graph
           `(step ,$x ,$y)))
      "和前一节一样我们从某个初始状态出发: 对于每条从"
      $x "到" $y "的边都有一个" (&edge $x $y)
      ", 对称规则成立 (因为是无向图), 起始位置表示为" (&at $x_0)
      ". [译注: 这里一条(无向)边只对应于一个" (Q "命题")
      ", 原文的说法其实有点问题. 另外, 对称规则现在也是线性的了, "
      "因为我们需要避免重复走过相同的(无向)边.] "
      "我们可以看到我们每走一步 (通过应用上述的" $step
      "规则), 我们都是消耗了一个事实" (&at $x)
      ", 然后产生了另一个事实" (&at $y)
      ", 因此状态里总是恰好存在一个具有" (&at $dumb)
      "形式的事实. 并且, 每一步我们也消耗了一个"
      (&edge $dumb $dumb) "事实, 因此我们最多能走的步数"
      "和初始图中的边数相等. 当然了, 如果我们位于一个点"
      $x ", 那么可能有许多条出边, 而若我们选择了错误的那条, "
      "可能就不能完成一笔画了, 但是至少在每个点处"
      "我们可以尝试的选择是有限的. "
      "如果我们能够抵达这样一个状态, 即没有形式为"
      (&edge $dumb $dumb) "的事实, 那么我们就成功了, "
      "或者说找到了不提起笔而画完图的方法, "
      "并且最终的位置为" (&at $x_n) ".")
   (P "以下的示例图来源于一首德国童谣, "
      "并且如果从" const:b "或者" const:c
      "出发我们就可以一笔画, 但是如果从"
      const:a ", " const:d ", 或" const:e
      "出发则不行."
      (MB (render-proof-tree
           env:graph
           (--> const:b const:a const:e const:d
                const:c const:b const:d const:a
                const:c)))
      "这是从" const:b "出发的一笔画的证明树."
      (MB (render-proof-tree
           env:graph
           (--> const:c const:d const:e const:a
                const:b const:c const:a const:d
                const:b)))
      "这是从" const:c "出发的一笔画的证明树.")
   
   (H3. "例子: 图遍历")
   
   (H3. "例子: 积木世界")
   (P "接下来我们考虑" (Em "积木世界(blocks world)")
      ", 这是人工智能的历史里一个重要的例子. "
      )
   (H3. "例子: King Richard III")
   
   (H3. "例子: 机会")
   (P "一条常见的谚语如下:"
      (Blockquote
       (Em "Opportunity doesn't knock twice.")
       " &mdash;Anonymous")
      "又一次, 让我们固定一个词汇表:"
      (MB (set-attr*
           (&Table
            ($opportunity "opportunity")
            ((&knocks $x) (: $x "&nbsp;knocks")))
           'columnalign "left"))
      
      )
   (H3. "练习")
   
   (H2. "从规则到命题")
   (H3. "例子: 生成树")
   (H3. "例子: 乞丐")
   (H3. "同时合取")
   (P "线性推理规则可以拥有多个前提和多个结论. "
      "如果我们试着将水平线想成是某种形式的二元联结词 "
      "(实际上那应该会是" (&-o $A $B)
      "), 那么我们需要一种方法将前提打包成单独一个命题, "
      "并且也要将结论打包成单独一个命题. "
      "[译注: 因为前提和结论都有可能不止一个.] "
      "这是" (Em "同时合取(simultaneous conjunction)")
      "或者说" (Em "乘性合取(multiplicative conjunction)")
      (&o* $A $B) "的目的所在了. " (&o* $A $B)
      "为真, 如果" $A "和" $B "在相同的状态里都为真. "
      "因此, 如果我们拥有" (&o* $A $B) ", 我们可以将其替换以"
      $A "和" $B ":"
      (MB (&rule (&eph (&o* $A $B))
                 ((&split 8)
                  (&eph $A) (&eph $B))))
      "另一个方向似乎也是直接的: 我们可以得到"
      (&o* $A $B) ", 如果我们既有" $A "也有" $B ":"
      (MB (&rule (&eph $A) (&eph $B)
                 (&eph (&o* $A $B))))
      "但是这已经造成了问题, 比如说如果我们想要表明"
      (MB (&rule (&eph (&o* $A $B))
                 (&eph (&o* $B $A))))
      "是一条导出推理规则, 那么证明可能会写成以下这样:"
      (MB (&rule*
           (&eph (&o* $A $B))
           ((&split 8)
            (&eph $A) (&eph $B))
           (&eph (&o* $B $A))))
      "一点小的恼人之处在于最后一条规则的前提的顺序是错误的. "
      "然而, 更重要的地方在于, "
      )
   (H3. "资源和目标")
   (P "现在我们转移到一种记号上来, 其主要的判断显式追踪了"
      "我们在推理过程中所有使用了的瞬态命题. 我们写下"
      (MB (&\|-
           (UnderBrace (&cm (&eph $A_1) $..h (&eph $A_n)) Δ)
           (&eph $C)))
      "将" Δ "的部分视为" (Em "资源(resources)") "而" $C
      "当成我们要达成的目标 (goal). "
      "为了证明这个东西, 我们需要在我们可以达成" $C
      "的证明中使用" Δ "的所有资源" (Em "恰好一次(exactly once)")
      ". 这是来源于Gentzen的" (Em "相继式演算(sequent calculus)")
      "的" (Em "相继式(sequent)") "的一个例子, "
      "[Gen35]这篇开创性论文标志着证明论作为研究主题的开始. "
      "然而, Gentzen的论文具有允许我们复制或者擦除假设的结构规则, "
      "但是在这里被有意省略了. [译注: 结构规则从指称或者语义角度来看是显然的, "
      "然而从证明论的角度来看反而是相继式演算里最重要的部分. "
      "线性逻辑实际上可以说始于这样的观察.]")
   (P "以相继式的记号, 我们现在可以写下"
      (MB (&rulel
           #:label $rule:⊗R
           (&\|- Δ (&eph $A))
           (&\|- Δ^ (&eph $B))
           (&\|- Δ Δ^ (&eph (&o* $A $B)))))
      "在结论中, 我们将证明" $A "所需要的资源" Δ
      "和证明" $B "所需要的资源" Δ^ "组合起来. "
      "这两个子证明之间不能共享资源, "
      "因为这将违背资源的瞬态本质. "
      "[译注: 这里的共享其实含义有点微妙, "
      "因为有的时候在某种意义上一些资源可以是相同的, "
      "这样的资源可能有许多份, 然而这并非共享, "
      "因为共享指的是包含本质上相同 (或者说同一/等同) 的资源. "
      "当然了, 这也是在说我们不能在组合的时候随意复制或者擦除, 要保留原样.] "
      "另一方面, 资源的顺序并不重要, 因此我们允许其自由地重新排列.")
   (P "以上是" (Em "右规则(right rule)") "的一个例子, "
      "其表明了该如何证明一个命题 (也就是说, 达成一个目标). "
      "[译注: 因此, " $rule:⊗R "的意思是如何得到一个同时合取呢?] "
      "反过来, 我们不得不刻画如何" (Em "使用")
      "一个命题. 我们以" (Em "左规则(left rule)")
      "来完成此事, 其分解当前资源集合里的一个资源. "
      "这里的左规则是直截了当的."
      (MB (&rulel
           #:label $rule:⊗L
           (&\|- Δ (&eph $A) (&eph $B) (&eph $C))
           (&\|- Δ (&eph (&o* $A $B)) (&eph $C))))
      "[译注: 这是相继式演算和自然演绎不同的地方, "
      "右规则相当于自然演绎的引入规则, 左规则相当于自然演绎的消去规则.] "
      "我们不能随意地发明这样的联结词的左规则和右规则. "
      "最终我们希望我们的系统里的逻辑命题有着符合期望的含义, "
      "不论是从直觉角度还是从形式角度. "
      "接下来的一节解释了一些我们可以运用的标准.")
   (H3. "identity和cut")
   (P "从基础上来讲, 我们需要在左侧的资源和右侧的目标之间达成平衡. "
      "这种平衡独立于我们所拥有的特定的联结词集合&mdash;&mdash;"
      "其应该对于任意的命题成立.")
   
   (H3. "cut归约")
   (H3. "线性implication")
   (H2. "和谐")
   (H3. "乘性单位元")
   (H3. "和谐的失败")
   (H3. "另一种合取")
   (H3. "加性单位元")
   (H3. "析取")
   (H3. "析取单位元")
   (H3. "永恒事实")
   ))