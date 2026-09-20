#lang racket
(provide session_mailbox_01.html)
(require SMathML "session_mailbox_utils.rkt")
(define $times:err (Mo "&times;"))
(define $x^+ (^ $x $+))
(define $x^- (^ $x $-))
(define $z_2 (_ $z $2))
(define $v_2 (_ $v $2))
(define $x_2 (_ $x $2))
(define $y_2 (_ $y $2))
(define $P_1 (_ $P $1))
(define session_mailbox_01.html
  (TnTmPrelude
   #:title "会话类型与邮箱类型 第1讲"
   #:css "styles.css"
   (H1. "会话类型与邮箱类型 第1讲")
   (P "Simon Gay. 原文标题为" (Q "Session Types and Mailbox Types")
      ", 是俄勒冈编程语言暑期学校 (OPLSS) 的第1讲讲义, 2026年7月1日. "
      "讲义由Billy Bai, Henry Olson, Xusheng Zhi整理. 下一讲见"
      (Href "session_mailbox_02.html" "第2讲") ".")
   (H2 "翻译术语对照")
   (set-attr*
    (SimpleTable
     ("英文" "中文")
     ("behavioural type" "行为类型")
     ("session type" "会话类型")
     ("mailbox type" "邮箱类型")
     ("channel / endpoint" "信道 / 端点")
     ("actor model" "演员模型")
     ("external / internal choice" "外部 / 内部选择")
     ("branch / select" "分支 / 选取")
     ("close / wait" "关闭 / 等待")
     ("dual, duality" "对偶")
     ("continuation" "后续")
     ("delegation" "委托")
     ("unique ownership" "唯一所有权")
     ("communication mismatch" "通信失配")
     ("session fidelity" "会话保真性")
     ("multiparty session types" "多方会话类型")
     ("inaction" "无动作")
     ("parallel composition" "并行复合")
     ("scope restriction" "作用域限制")
     ("structural congruence" "结构同余")
     ("prefix (process)" "前缀 (进程)")
     ("subject" "主体")
     ("canonical form" "典范形式")
     ("redex" "可约式")
     ("deadlock / self-blocking" "死锁 / 自阻塞")
     ("typing judgement" "定型判断"))
    'align "center")
   (H2. "引言" #:id "introduction")
   (P "会话类型和邮箱类型都是" (Em "行为类型")
      "的例子. 这个短语并没有精确的形式定义; 它是一种启发式的说法, "
      "用来描述那些目的不止于检查普通数据形状的类型系统. "
      "一个行为类型描述了程序如何行为的某个方面. "
      "在会话类型以及其余的讲座中, 我们关注的行为是通信.")
   (P "会话类型的核心思想是把一个通信协议表达为一个类型. "
      "这一讲所考虑的协议是" (B "双向, 点对点")
      "信道上的协议. 一个信道有两个端点, "
      "而一个端点处的类型描述了该端点的拥有者所必须执行的通信动作的序列. "
      "于是, 静态类型检查就能在程序运行之前验证程序中面向通信的部分.")
   (P "邮箱类型 (在课程稍后出现) 处理的是另一种通信模型: 演员模型. "
      "在那种设定下, 每个演员都有一个邮箱, 许多进程都可以向它发送消息, "
      "而接收者一般不能依赖于消息以某种特定的顺序送达.")
   (H2. "作为协议的会话类型: 启发性的例子" #:id "protocols")
   (P "第一个例子是协议" (Q "发送一个整数, 然后关闭信道")
      ". 它的会话类型是"
      (MB (Out $int:ty $end!))
      "构造子" (: $! $int:ty $.0) "表示下一个动作是输出一个整数; 点表示顺序; 而"
      $end! "表示这个端点将关闭信道. "
      "还有一个对偶的协议: 接收一个整数, 然后等待另一个端点关闭信道,"
      (MB (In $int:ty $end?))
      "这两种终止形式都是会话类型. 其区别并不在于一个端点"
      (Q "已经结束") "而另一个没有; 相反, 结束本身就是一个同步动作. "
      "一个端点执行关闭, 另一个端点则等待这个关闭.")
   (P "协议可以涉及多条消息, 并且可以改变方向. 例如,"
      (MB (Out $int:ty (Out $int:ty (In $int:ty $end!))))
      "描述了一个端点, 它发送两个整数, 接收一个整数, 然后关闭.")
   (P "会话类型记录了每条消息的方向和类型, 但不记录数据值之间的语义关系. "
      "如果一个客户端发送两个整数并期望得到它们的积或和作为回应, "
      "那么普通的会话类型本身并不表达这一算术规约.")
   (P "协议还可以包含选择. 一个简单的数学服务器可能提供加法或取反:"
      (MB (&= $S (Ext $plus (In $int:ty (In $int:ty (Out $int:ty $end?)))
                      $neg:lab (In $int:ty (Out $int:ty $end?)))))
      "外部选择构造子" (: $&:id (setE $..c))
      "意味着由环境通过发送一个标签来选择一个分支. "
      "然后, 服务器遵循附着于该标签的协议. 对偶的客户端类型使用内部选择:"
      (MB (&= $T (Int $plus (Out $int:ty (Out $int:ty (In $int:ty $end!)))
                      $neg:lab (Out $int:ty (In $int:ty $end!)))))
      "这里由客户端选择分支, 并把所选的标签发送给服务器. "
      "在这些例子中, 是客户端关闭信道而服务器等待, "
      "但这一约定并不是数学所强制的; "
      "重要的一点是两个端点使用了对偶的终止动作.")
   (P "现实中的服务常常需要反复运行. 我们可以用方程"
      (MB (&= $S (Ext $plus (In $int:ty (In $int:ty (Out $int:ty $S)))
                      $neg:lab (In $int:ty (Out $int:ty $S))
                      $quit $end?)))
      "来描述数学服务器的一个递归版本. "
      "对应的客户端类型可以通过对选择, 方向和终止取对偶而得到:"
      (MB (&= $T (Int $plus (Out $int:ty (Out $int:ty (In $int:ty $T)))
                      $neg:lab (Out $int:ty (In $int:ty $T))
                      $quit $end!)))
      "这一讲主体部分所建立的形式系统集中于有穷行为, "
      "但对于这种形式的服务而言, 递归类型是所需的自然扩展.")
   (P "消息不必是普通的数据. 它们本身可以是信道, "
      "在这种情况下, 它们的消息类型是另一个会话类型. 例如,"
      (MB (Out (tu0 (In $int:ty $end?)) $end!))
      "意味着: 发送一个信道端点, 其未来的行为是接收一个整数并等待关闭, "
      "然后关闭原来的信道. 这种对于信道的传递被称为" (Em "委托")
      ". 一个端点的拥有者可以运行协议的一部分, "
      "然后把该协议其余部分的责任转交给另一个进程. "
      "注意, 对外层协议取对偶并不会对所发送消息的类型取对偶:"
      (MB (In (tu0 (In $int:ty $end?)) $end?))
      "接收者得到的信道端点恰好具有发送者所发送的类型.")
   (H2. "通信模型与所有权" #:id "model")
   (P "这一讲的通信模型是并发或分布式的" (Em "点对点")
      "通信. 一个进程可以使用许多信道, 但每个信道都"
      (B "恰好") "有两个端点.")
   (P "信道是" (Em "双向的") ": " (B "任意一个")
      "端点都可以发送或接收, 这取决于协议当前所处的位置. "
      "我们假定通信是可靠的, 并且在某个特定信道上发送的消息以相同的顺序被接收.")
   (P "同一个信道中的消息顺序得到保持, 正是这一点使得会话类型的顺序结构有意义. "
      "然而, 不同信道中的消息顺序仍然是相互独立的.")
   (P "讲座中的操作性表述使用同步通信: 一次发送和对应的接收同时发生, "
      "发送者会阻塞直到接收者就绪. 会话类型的异步版本也是可能的, "
      "并且它更接近于基于套接字和缓冲区的实现模型, "
      "但它需要更复杂的操作语义. 主要的定型思想在同步的设定下就已经可以看到.")
   (P "一条核心的纪律是端点的" (Em "唯一所有权")
      ". 如果两个进程可以随意共享同一个端点, 它们的动作就可能相互干扰. "
      "例如, 一个类型可能说只发送一个整数, 但共享该端点的两个线程可能都试图发送. "
      "基本的会话类型系统通过使用线性类型论中的技术来防止这种情况: "
      "信道端点是一种资源, 它在同一时刻必须只属于一个线程. "
      "委托遵守这一纪律, 因为发送一个端点就转移了所有权; "
      "发送者的后续不能再使用它已经发送出去的端点.")
   (P "主要的安全性保证是不存在通信失配. 如果一个端点发送一条消息, "
      "那么另一个端点已准备好接收一条类型正确的消息; 反过来, "
      "如果一个端点正在等待接收, 那么另一个端点已准备好发送所期望的那种消息. "
      "会话类型的安全性所保证的另一个性质是" (Em "会话保真性")
      ": 运行时观察到的消息的顺序和类型与静态地指派的会话类型相吻合.")
   (H2. "会话类型简史" #:id "history")
   (P "会话类型最初的一系列工作始于本田耕平 (Kohei Honda) 的CONCUR 1993论文"
      (Q "Types for Dyadic Interaction")
      ", 随后是与竹内格 (Kaku Takeuchi) 和久保誠 (Makoto Kubo) 合作的PARLE 1994论文, "
      "以及与Vasco Vasconcelos和久保誠合作的ESOP 1998论文. "
      "这些论文引入了对结构化的通信协议进行定型的想法, "
      "起初是在进程演算的设定下, 然后逐渐转向编程语言原语.")
   (P "Simon Gay (本讲的讲者) 把他与Malcolm Hole合作的ESOP 1999论文"
      (Q "Types and Subtypes for Client-Server Interactions")
      "描述为第一篇出自最初那个研究组之外的会话类型论文. "
      "这项工作贡献了子类型, 对信道两个端点的仔细区分, "
      "以及使用线性定型的思想来强制信道端点的唯一所有权. "
      "它显然也是第一篇使用" (Q "会话类型")
      "这个短语本身的论文: 更早的论文谈到了会话和类型, "
      "但还没有使用这个如今已成为标准的名字.")
   (P "后来的发展包括将会话类型与函数式编程相结合的工作, "
      "用于两个以上参与者的协议的多方会话类型, "
      "以及会话类型与线性逻辑之间的Curry-Howard对应. "
      "前三讲的参考资料是Simon Gay和Vasco Vasconcelos最近出版的书.")
   (H2. "用于会话的" $pi "演算" #:id "pi-calculus")
   (P "这一讲为一个小型的" $pi "演算给出会话定型. "
      "从一个无类型的进程语言出发是有用的, "
      "因为这样我们就可以直接定义运行时错误, "
      "并证明良类型的进程永远不会到达这些错误. "
      "对于有穷行为, 进程的语法是"
      (MB (set-attr*
           (&Table
            ($P $::= $inact (% "无动作"))
            ($ $\| (Wait $x $P) (% "等待关闭"))
            ($ $\| (Close $x $P) (% "关闭"))
            ($ $\| (Recv $x $y $P) (% "接收"))
            ($ $\| (Send $x $y $P) (% "发送"))
            ($ $\| (Par $P $P) (% "并行复合"))
            ($ $\| (Res $x $y $P) (% "作用域限制")))
           'columnalign "right center left left")))
   (P "进程" $inact "意味着该进程已经终止. 进程" (Wait $x $P)
      "等待" $x "的另一个端点关闭, 然后作为" $P "继续; "
      (Close $x $P) "关闭" $x ", 然后作为" $P "继续. 进程"
      (Recv $x $y $P) "在" $x "上接收一条消息, 将其" (B "绑定")
      "到" $y ", 然后作为" $P "继续. 进程" (Send $x $y $P) "在"
      $x "上发送" $y ", 然后作为" $P "继续. 并行复合" (Par $P $Q)
      "并发地运行" $P "和" $Q ". 最后, " (Res $x $y $P)
      "创建一个私有信道, 其两个端点是" $x "和" $y ", 作用域为" $P
      ". 这个双重约束子是有意地不按标准来的: 它使两个端点变得显式. "
      "也有其他约束两个端点的方式, 例如" (: (tu0 (: $nu $x^+ $x^-)) $P)
      ". 但事实证明, 对于会话类型而言, 双重约束子更容易处理.")
   (P "进程是在结构同余的意义下考虑的. 讲座中所使用的主要的结构同余规则有:"
      (MB (set-attr*
           (&Table
            ((&equiv (Par $P $Q) (Par $Q $P))
             (Rn "(C-Comm)"))
            ((&equiv (Par (tu0 (Par $P $Q)) $M) (Par $P (tu0 (Par $Q $M))))
             (Rn "(C-Assoc)"))
            ((&equiv (Par $P $inact) $P)
             (Rn "(C-Neut)"))
            ((&equiv (Res $x $y (tu0 (Par $P $Q))) (Par (Res $x $y $P) $Q))
             (Rn "(C-Scope)"))
            ((&equiv (Res $x $y $P) (Res $y $x $P))
             (Rn "(C-SwapC)"))
            ((&equiv (: (ν $x $y) (ν $z $w) $P) (: (ν $z $w) (ν $x $y) $P))
             (Rn "(C-SwapB)")))
           'columnalign "center right"
           'columnspacing "4em")))
   (P "还有一些同余规则说的是结构同余被进程构造子所保持, 例如:"
      (MB (set-attr*
           (&Table
            ((&equiv $inact $inact)
             (Rn "(C-Inact)"))
            ((&rule (&equiv $P $Q) (&equiv (Res $x $y $P) (Res $x $y $Q)))
             (Rn "(C-Res)")))
           'columnalign "center right"
           'columnspacing "4em"))
      "并行复合是可交换的和可结合的, " $inact
      "是它的单位元, 并且在保持约束名字新鲜的情况下, "
      "限制可以被重新排序或向外移动. "
      "这使得归约可以忽略无关的句法顺序, 并把潜在的通信伙伴放到一起.")
   (P "两条主要的归约规则是关闭/等待交互和消息传递:"
      (MB (&-> (Res $x $y (tu0 (Par (Close $x $P) (Wait $y $Q))))
               (Res $x $y (tu0 (Par $P $Q)))))
      "以及, 示意性地,"
      (MB (&-> (Res $x $y (tu0 (Par (Send $x $v $P) (Recv $y $z $Q))))
               (Res $x $y (tu0 (Par $P (Subst $Q $v $z)))))
          ".")
      "还有一些上下文规则, 允许在并行复合和限制的内部进行归约, "
      "以及一条结构规则, 允许在通过结构同余重新排列之后进行归约. "
      "选择增加了两种进程形式:"
      (MB (Branch* $x $l_i $P_i) $space4 (% "和") $space4
          (Sel $x $l $P) ".")
      "前者提供了一个对带标签分支的外部选择; 后者选取这些标签中的一个. "
      "当所提供的标签和所选取的标签一致时, 归约规则选择相匹配的分支.")
   (P "作为一个例子, 一个一次性的数学服务器可以如下与一个客户端相连接:"
      (MB (set-attr*
           (&Table
            ((: (ν $x $y) $lp0)
             (Branch $x
                     $plus (Recv $x $a (Recv $x $b (Send $x (tu0 (&+ $a $b)) (Wait $x $inact))))
                     $neg:lab (Recv $x $a (Send $x (tu0 (: $- $a)) (Wait $x $inact)))))
            ($
             (: $\| (Sel $y $plus (Send $y $2 (Send $y $3 (Recv $y $u (Close $y (app $P $u))))))
                $rp0 ".")))
           'columnalign "right left"))
      "客户端选取" $plus ", 发送" $2 "和" $3 ", 接收" $5
      ", 关闭信道, 然后作为" (app $P $5)
      "继续, 这里假定算术表达式以所期望的方式被求值.")
   (H2. "运行时错误与死锁" #:id "errors")
   (P "这个类型系统旨在排除通信失配. 典型的坏格局包括: "
      "接收面对选取, 接收面对关闭, 两个接收相互面对, "
      "或者一个分支提供某个标签, 而另一个端点选取了另一个不同的标签:"
      (MB (set-attr*
           (&Table
            ($times:err (: (Res $x $y (tu0 (Par (Recv $x $z $P) (Sel $y $l $Q)))) ","))
            ($times:err (: (Res $x $y (tu0 (Par (Recv $x $z $P) (Close $y $inact)))) ","))
            ($times:err (: (Res $x $y (tu0 (Par (Recv $x $z $P) (Recv $y $w $Q)))) ","))
            ($times:err (: (Res $x $y (tu0 (Par (Branch $x $l $P) (Sel $y $k $inact))))
                           $space4 (tu0 (&!= $k $l)) ".")))
           'columnalign "center left")))
   (P "为了形式地定义这一点, 讲座引入了" (Em "前缀进程")
      ": 其第一个动作是接收, 发送, 分支, 选取, 等待或关闭的进程. "
      "这样一个前缀的" (Em "主体") "是第一个动作发生于其上的端点.")
   (P "每个进程都结构同余于一个" (Em "典范形式")
      (MB (: (ν $x_1 $y_1) $..c (ν $x_n $y_n) (tu0 (Par $P_1 $..c $P_m))) ",")
      "其中诸" $P_i "是前缀, 或者在没有前缀时是" $inact ". 一个"
      (Em "可约式") "是位于相对端点上的一对能够通信的前缀: "
      "等待与关闭, 接收与发送, 或者选取与包含所选标签的分支. "
      "一个运行时错误是这样的典范形式, "
      "它包含位于同一个受限信道的相对端点上的前缀, 而它们并不构成一个可约式.")
   (P "还有其他种类的进程, 即死锁和自阻塞, 它们不符合运行时错误的定义, "
      "超出了基本会话类型的安全性保证的范围. 死锁指的是循环依赖, 例如"
      (MB (: (ν $x_1 $y_1) (ν $x_2 $y_2)
             (tu0 (Par (Recv $x_1 $z_1 (Recv $x_2 $z_2 (Wait $x_1 (Wait $x_2 $inact))))
                       (Send $y_2 $v_1 (Send $y_1 $v_2 (Close $y_1 (Close $y_2 $inact))))))))
      "自阻塞发生于接收和发送在同一个进程中被顺序排列的时候"
      (MB (Res $x $y (Recv $x $z (Send $y $v (Wait $x (Close $y $inact))))))
      "因此, 基本会话类型中所证明的安全性定理并不是最强意义上的"
      (Q "良类型的程序总是能够取得进展")
      ". 它是一个更具体的断言, 即良类型的程序不会到达刚才所描述的通信失配. "
      "也有Wen Kokke等人的工作[1], 把死锁也加入到会话类型所要避免的行为之中.")
   (H2. "类型, 对偶与定型规则" #:id "typing")
   (P "对于有穷演算, 会话类型的语法是"
      (MB (set-attr*
           (&Table
            ($T $::= $end? (% "等待关闭"))
            ($ $\| $end! (% "关闭"))
            ($ $\| (In $T $U) (: (% "输入一个类型为") $T (% "的值, 然后表现得如同") $U))
            ($ $\| (Out $T $U) (: (% "输出一个类型为") $T (% "的值, 然后表现得如同") $U))
            ($ $\| (Ext* $l_i $T_i) (% "外部选择"))
            ($ $\| (Int* $l_i $T_i) (% "内部选择")))
           'columnalign "right center left left")))
   (P "与线程的对偶类似, 对偶也出现在会话类型之中. "
      "对偶关联起那些可以安全地出现在一个信道的两个端点处的类型. "
      "关闭与等待对偶, 输入与具有相同消息类型的输出对偶, "
      "而外部选择与具有相匹配的标签和对偶的后续的内部选择对偶. "
      "等价地, 对于非递归类型:"
      (MB (set-attr*
           (&Table
            ((Dual $end?) $= (: $end! ",") $space4
             (Dual $end!) $= (: $end? ","))
            ((Dual (tu0 (In $T $U))) $= (: (Out $T (Dual $U)) ",") $space4
             (Dual (tu0 (Out $T $U))) $= (: (In $T (Dual $U)) ","))
            ((Dual (tu0 (Ext* $l_i $T_i))) $= (: (Int* $l_i (Dual $T_i)) ",") $space4
             (Dual (tu0 (Int* $l_i $T_i))) $= (: (Ext* $l_i (Dual $T_i)) ".")))
           'columnalign "right center left center right center left"))
      "{译注: 原文此处写作" (&= (Dual (tu0 (In $T $U))) (Out $T $U)) "和"
      (&= (Dual (tu0 (Out $T $U))) (In $T $U))
      ", 漏掉了后续" $U "上的对偶, 这里已改正. 参见前文中递归的数学服务器的客户端类型.} "
      "对于递归类型, 对偶通常更适合以余归纳的方式表述.")
   (P "定型判断具有形式"
      (MB (&entailL Γ $P) ",")
      "其中" Γ "将变量映射到会话类型. 一个进程本身并不被指派类型; "
      "相反, 它是对照它所使用的资源而被检查的. 在基本系统中, 环境"
      Γ "恰好包含" $P "的自由端点.")
   (H2 "参考文献")
   (Ol (Li "Wen Kokke and Ornela Dardha. Deadlock-free session types in linear Haskell. In "
           (I "Proceedings of the 14th ACM SIGPLAN International Symposium on Haskell")
           ", pages 1&ndash;13, 2021."))
   ))
