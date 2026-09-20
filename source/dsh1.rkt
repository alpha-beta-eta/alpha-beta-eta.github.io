#lang racket
(provide dsh1.html)
(require SMathML)
(define App (&split 2))
(define (Rn str)
  (Mi str #:attr* '((mathvariant "normal"))))
(define $simeq (Mo "&simeq;"))
(define $isin (Mo "&isin;"))
(define $cap (Mo "&cap;"))
(define $++ (Mo "++"))
(define-infix*
  (&++ $++))
;components
(define $Prog (Mi "Prog"))
(define $Done (Mi "Done"))
(define $Bool (Mi "Bool"))
(define $deps (Mi "deps"))
(define $body (Mi "body"))
(define $xdeps (Mi "xdeps"))
(define $xopt (Mi "xopt"))
(define $xprovs (Mi "xprovs"))
(define $xbody (Mi "xbody"))
(define $lift (Mi "lift"))
(define $prog (Mi "prog"))
(define $prog^ (^ $prog $prime))
(define $present (Mi "present"))
(define (&present c) (app (_ $present $gamma) c))
(define $held (Mi "held"))
(define $Inactive (Mi "Inactive"))
(define $Unloading (Mi "Unloading"))
(define $None (Rn "None"))
(define (Inactive z) (app $Inactive z))
(define $entry (Mi "entry"))
(define (&entry a) (app $entry a))
(define $nil (Mi "[]"))
(define $S^ (^ $S $prime))
(define $c^ (^ $c $prime))
(define (Rule str) (Mi str))
(define $O-Restart (Rule "O-Restart"))
(define $O-React (Rule "O-React"))
(define $L-Begin (Rule "L-Begin"))
(define $L-Step (Rule "L-Step"))
(define $L-Divert (Rule "L-Divert"))
(define $L-Leave (Rule "L-Leave"))
(define $X-Begin (Rule "X-Begin"))
(define $X-Step (Rule "X-Step"))
(define $X-Divert (Rule "X-Divert"))
(define $X-Leave (Rule "X-Leave"))
(define $I-React (Rule "I-React"))
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
(define (Note0 . x*)
  (apply A x* #:attr* '((href "dsh0.html"))))
(define dsh1.html
  (TnTmPrelude
   #:title "deepseek harness笔记1"
   #:css "styles.css"
   (H1. "deepseek harness笔记1")
   (P (Note0 "笔记0") "给出了插件生命周期的核心模型和它的元理论, "
      "最后把结果汇总成一个定理: 若服务满足规格, 插件满足义务, 系统就有安全, 活性, 恢复, 合流四层保证. "
      "随后我们按这四层审计了npm上下载量最高的26个第三方dsh插件: "
      "0个到第3层, 5个到第2层, 7个到第1层, 14个不在模型之内. "
      "不在模型内的一部分是真正的bug (猴补丁, " (Code "globalThis") ", " (Code "internal/*")
      "钩子), 应当保持排除; 另一部分是模型缺了机制. 缺的主要有三样, 按影响的插件数排列:")
   (Ol (Li (B "可选依赖") ": 有就用, 没有不等. 过半插件读未注入的服务, 并按它在不在决定启动时登记什么.")
       (Li (B "编排者") ": 插件市场, 自更新, bundle这类插件安装, 移除, 重启别的插件或自己.")
       (Li (B "交集生命期") ": 效应挂在另一个fiber (如agent的上下文) 上, 任一方结束即撤销."))
   (P "本文给出这三项扩展, 每项都说明规则, 插件的义务, 以及在义务之下哪几层保证仍然成立. "
      "三项都和核心隔离: 核心的规则和定理不改动, 扩展只单向依赖核心. "
      "做法是把扩展的每一步归约为核心的一步: 要么就是核心在某张按状态选定的组件表下的一步, "
      "要么是一个不碰服务状态, 也不碰累加器的簿记步. "
      "核心的定理都对任意组件表陈述, 所以单步的结论 (保持性, 顺序, 进展) 直接搬过来, "
      "只有核心没有的步需要新的证明.")
   (P "术语和记号沿用笔记0. 扩展在Rocq中形式化 (cordis-rocq仓库的" (Code "theories/Ext/")
      "), 对应关系见" (Ref "sec-rocq") "; 可运行的原型是" (Code "programs/cordis3.rkt")
      ", 下面的例子都取自它.")

   (H2. "可选依赖" #:id "sec-opt")
   (P "审计中最常见的写法是这样的:")
   (CodeB "const llm = ctx.get('llm')   // 没有 inject
if (llm) { ... 登记 A ... } else { ... 登记 B ... }")
   (P "作者这样写, 是为了在缺少" (Code "llm") "的宿主上不停在PENDING. "
      "但核心模型只有必需依赖, " (Code "ctx.get") "读未声明的服务不在保证范围之内; "
      "而且" (Code "llm") "此刻在不在取决于它的提供者是否恰好先激活, "
      "所以登记了什么取决于调度, 合流不成立.")
   ((Definition #:id "def-xcomp")
    "带可选依赖的组件由以下部分组成: 必需依赖" (app $xdeps $c) ", 可选依赖" (app $xopt $c)
    ", 可能提供的键" (app $xprovs $c) ", 事件处理器, 以及启动程序"
    (MB (&: (app $xbody $c) (&-> (@-> $K $Bool) $Prog)) ",")
    "它按" (Q "哪些可选依赖在场") "选择变体.")
   ((Definition #:id "def-present")
    "状态" $gamma "下组件" $c "的" (B "在场集") (&present $c) "是" (app $xopt $c)
    "中当前可见 (提供者Active) 的键, 按声明的顺序排列.")
   (P "语义的关键是在场集变化时怎么办. 我们规定: fiber在开始加载时取定在场集" $S
      ", 运行变体" (app (app $xbody $c) (: $isin $S)) "; 在场的可选依赖进入它的视图, "
      "程序可以在其上执行操作. 加载中或Active的fiber, 只要当前的在场集不再等于" $S
      ", 就中途放弃或离开, 之后按新的在场集重新加载. 出现和消失两个方向都重载. "
      "若出现时不重载, 终态中的" $S "就取决于提供者是否恰好先激活, 合流必然失败. "
      "可选依赖从不阻止激活: fiber是否运行只取决于必需依赖.")
   ((Definition #:id "def-xrules")
    "扩展的规则如下, 未列出的标签都是核心规则."
    (Ul (RuleLi (Lab $X-Begin $n) (list $n "是" (Inactive $None)) $n "先取" (&= $S (&present $c))
                ", 再走核心的" $L-Begin ".")
        (RuleLi (Lab $X-Step $n) (list "在场集仍是" $S) "走核心的" $L-Step
                " (核心目标改变后, 剩下的迭代照样可以落地, 见" (Note0 "笔记0") "; 在场集改变之后的落地不在扩展之内).")
        (RuleLi (Span (Lab $X-Divert $n) ", " (Lab $X-Leave $n))
                (list "核心的" $L-Divert "或" $L-Leave "可走, 或者在场集已不是" $S)
                $n "进入" $Unloading ".")))
   (P "这些规则可以直接读成核心规则. 让fiber的组件是二元组" (tu0 $c $S)
      ", 即基组件和它开始加载时的在场集, 核心的组件表取")
   (MB (&= (app $deps (tu0 $c $S)) (&++ (app $xdeps $c) (@ (: $S $cap (app $xopt $c))))) ",")
   (MB (&= (app $body (tu0 $c $S)) (app $lift (app (app $xbody $c) (: $isin $S)))) ",")
   (P "提供的键与" $S "无关, " $lift "把程序中spawn的组件" $c^ "记为" (tu0 $c^ $nil)
      ", 事件处理器列表中的每一项" $c^ "同样记为" (tu0 $c^ $nil) ". 于是:")
   ((Theorem #:id "thm-xcore")
    "扩展的每一步是以下三者之一: 核心在上述组件表下的一步; 给一个Inactive的fiber换上当前的在场集, "
    "再走核心的" $L-Begin "; 核心的" $O-Restart ". 没有任何可选依赖时 (每个fiber记下的在场集都为空), "
    "扩展的每一步就是核心的一步.")
   (P "第三种情况对应" (Q "可选依赖出现了, 核心目标却没有变") ": fiber此时未被请求移除, "
      "否则核心目标已是⊥, 走的是核心的" $L-Divert "或" $L-Leave ". "
      "换组件只发生在Inactive的fiber上, 而核心的良构性只在fiber已安装时读它的依赖, 所以换组件保持良构性. "
      "原型里这一点更直接: 在场的可选依赖记在视图中, 在场集一变, 视图就与应有的视图不符, "
      "核心已有的" (Q "视图不符就卸载") "规则自然使fiber重载.")
   ((Definition #:id "def-xobl")
    "带可选依赖的组件表满足义务, 如果"
    (Ol (Li (B "有界") ": 每个变体的每条路径都不超过某个界;")
        (Li (B "有秩") ": 有一个有界的秩, 沿提供, spawn, 事件处理器, " (Em "以及可选依赖") "的边严格递增;")
        (Li (B "完整提供") ": " (Em "每个") "变体到达" $Done "的每条路径都提供组件可能提供的全部键.")))
   (P "有秩一条蕴涵组件不把自己提供的键列为自己的可选依赖, 否则它的秩严格小于自己. "
      "这正是需要的: 否则fiber一激活, 自己就出现在自己的在场集里, 随即重载, 撤回, 再加载, 无限循环. "
      "两个互为可选依赖的组件同理.")
   ((Theorem #:id "thm-opt")
    "设组件表满足" (Ref "def-xobl") "的义务, 服务满足笔记0中相应层的规格."
    (Ol (Li (B "安全") " (不需要义务): 扩展执行可达的状态都良构, 因而笔记0中的顺序性质 "
            "(提供者比依赖者活得久, 整个卸载等依赖者离开等) 都成立.")
        (Li (B "活性") ": 从可达状态出发, 扩展的生命周期步没有无限执行; "
            "没有扩展规则可用时, 状态按核心的意义是稳定的, 并且每个Active的fiber的在场集都是当前的.")
        (Li (B "恢复") " (服务可撤销): 笔记0的恢复精确性对任意扩展执行成立.")
        (Li (B "合流") " (服务可合流, 并且被spawn或作为处理器运行的组件不声明可选依赖): "
            "两次扩展执行若编排输入留下的基组件按插入次序排成的序列相同, 终于静止且没有失败的fiber, "
            "则把Inactive的fiber过时的在场集换成终态的在场集之后, 两个终态在重命名意义下一致; "
            "对应的Active的fiber运行同一个变体.")))
   (P "证明的要点按层说.")
   (Ul (Li (B "安全") "由" (Ref "thm-xcore") "和换组件保持良构性得到.")
       (Li (B "进展") "要用核心的" (Q "静止即稳定") ". 它要求子fiber的组件是父组件的程序可能spawn的, "
           "而给子fiber换上在场集之后, 它的组件" (tu0 $c^ $S^) "不再字面地等于父程序中的"
           (tu0 $c^ $nil) ". 办法是换一张组件表: 它与" $prog^
           "只差在事件处理器列表上, 把当前状态中子fiber的组件都列进去. "
           "生命周期规则不读事件处理器列表, 所以两张表走同样的生命周期步, 而这张表满足核心的前提.")
       (Li (B "终止") "需要新的度量. 核心的度量读核心目标, 而可选依赖刚出现的fiber核心目标没变, 却必须重载. "
           "新的度量形状与核心相同, 只改两处: 加载中或Active的fiber算作" (Q "在目标上")
           ", 要求在场集仍是当前的且核心目标等于它的视图; 卸载将尽或Inactive的fiber会不会再加载, "
           "按它换上当前在场集之后的目标判断. 一个fiber的一步, 不改变秩不高于它的fiber所读的键 "
           "(必需的和可选的) 的可见性, 因而也不改变它们的在场集; 这里用到秩沿可选边递增. "
           "于是按秩分层的度量向量仍然按字典序下降.")
       (Li (B "恢复") ": 换组件不碰存储, 也不碰任何累加器, 所以保持核心的影子不变式; 其余步都是核心步.")
       (Li (B "合流") "的核心是: 静止时一个fiber是Active, 当且仅当它没有被请求移除, 且它的" (Em "必需") "依赖都由Active的fiber提供. "
           "否则它是" (Inactive $None) ", 而换上当前在场集之后它的目标可解析, " $X-Begin
           "可走, 与静止矛盾. 这个不动点方程只涉及基组件, 沿秩只有一个解, 所以两个终态的Active集合相同; "
           "在场集由Active集合读出, 所以也相同. 服务状态一层沿用笔记0的理想轨迹; "
           "含spawn时, 规范名的秩归纳只改根的情形: 两个对应的根起初只知道基组件相同, "
           "在场集相同由更低秩上的对应得到, 因为每个可选键的提供者秩更低."))
   ((Remark #:id "rem-child")
    "合流要求子组件不声明可选依赖, 原因在笔记0的理想轨迹不变式: 它按组件的完整值检查父fiber spawn出的子fiber, "
    "而父程序里写死的spawn目标是" (tu0 $c^ $nil) ", 子fiber的组件却要记下它的在场集. "
    "两者不可兼得, 换编码也绕不过去. 去掉这一限制要把这条检查改为按基组件比较, "
    "并重证沿执行保持它的部分. 这一限制只影响合流; 审计中读可选服务的多是顶层插件.")
   ((Example #:id "ex-assistant")
    "组件assistant可选依赖llm. 有llm时它往llm登记自己, 并提供用llm的summarize; "
    "没有时提供退化的summarize. 两个变体都提供skills. 先装assistant, 后装llm时, 原型的记录是:"
    (CodeB "实例 25 (插件 assistant) 开始加载, 依赖 (服务 . 编号): (), 在场的可选依赖: ()
...
实例 28 (插件 llm) 开始加载, 依赖 (服务 . 编号): ()
提供 llm (编号 29)
加载完毕 -> active
实例 25 (插件 assistant) 开始卸载, 原因: 依赖变化, 原视图 (), 现在应为 ((llm . 29)); 在场的可选依赖从 () 变为 (llm)
...
实例 25 (插件 assistant) 开始加载, 依赖 (服务 . 编号): ((llm . 29)), 在场的可选依赖: (llm)")
    "按相反的顺序安装则一开始就选用llm的变体. 两种顺序 (以及随机的调度) 终于同一个状态. "
    "再移除llm时, llm的卸载被守卫挡住, assistant先卸载, 撤掉它在llm上登记的一项, 再按退化的变体加载.")

   (H2. "编排者" #:id "sec-orch")
   (P "审计中有6到7个插件本质上是编排者: 插件市场装卸插件, 插件自更新, bundle装配一组子插件. "
      "它们装上的插件不随编排者卸载而消失: 安装写在配置里, 下次启动仍在. "
      "这与spawn不同, spawn出的子fiber是父fiber的效应. 所以编排者不建模为父fiber, "
      "而是建模为" (B "输入源") ": 它发出编排输入 (插入, 移除, 重启, 重置), "
      "按插入序号指称根fiber, 交给笔记0中与生命周期步交错的执行. 它依据的状态是外部持久的配置, 不是服务. "
      "这一项扩展不加任何规则.")
   ((Definition #:id "def-orch")
    "编排者的义务:"
    (Ol (Li (B "只编排根") ": 只对插入的根fiber发移除, 重启, 重置, 从不直接移除启动时spawn的子fiber. "
            "输入按插入序号指称, 这一条是内建的.")
        (Li (B "有限") ": 在一次会话中只发出有限多个输入.")
        (Li (B "结果确定") ": 存活的组件 (按多重集) 只由配置决定, 不由调度决定.")
        (Li (B "不在启动中自我重启") ": 自更新放在反应中, 或交给loader.")))
   (P "此外还有一条要求, 它在模型之外: 编排要经loader的条目进行.改loader条目再由loader协调, "
      "或者经宿主的" (Code "pluginManager.installBundle") ", " (Code "hmr.reconcileProfile()")
      " (它们先装包, 再由loader按配置重组条目), 都在模型内; " (Code "internal/*")
      "钩子, 给自己进程发SIGTERM, 自己清模块缓存热替换自己, 则是在修改运行时本身.")
   ((Theorem #:id "thm-orch")
    "编排执行本身就是演算的一次执行, 所以笔记0汇总定理的前三层对它成立, 不需要额外的义务; "
    "活性一层说的是最后一个输入之后. 对合流: 设两次编排执行留下的存活组件是同一个多重集, "
    (Em "顺序可以不同") ", 都终于静止且没有失败的fiber, 则存在存活位置的一个置换, "
    "按它给第二次执行的根重新标号之后, 两个终态在重命名意义下一致.")
   (P "笔记0中与编排交错的合流要求存活组件的顺序也相同. 去掉这一要求的证明很短: "
      "两个列表互为排列, 就有一个单射把一边的位置送到另一边; 两边的位置是同一个有限集, 有限集到自身的单射即满射; "
      "用它给根标号, 其余交给笔记0中与执行方式无关的终态比较. "
      "插件市场按下载完成的任意顺序安装同一组插件, 终态一致, 这正是论文5.2.1节对loader的说法: "
      (Q "whatever instantiations and retirements the loader performs on the way, and in whatever order")
      ", 系统都停在从头加载最终配置所得的状态.")
   ((Example #:id "ex-update")
    "自更新是移除旧版本, 再插入新版本. 次序要紧: 旧版本还在时插入新版本被拒绝, "
    "因为两者提供同一个键; 必须等旧版本卸载并被移除 (它的依赖者先行卸载), 再插入新版本. "
    "这与论文4.4节的修订一致: 退役, 让生命周期规则卸载, 移除, 再重新插入. "
    "原型的测试检查了终态与直接安装新版本相同.")

   (H2. "交集生命期" #:id "sec-inter")
   (P "插件P给agent A的上下文挂一个效应, 例如限制A可用的工具, 要求P与A任一方结束时撤销. "
      "审计中的插件都用私有的表手工回收, 常常只处理一个方向: 有的只在agent结束时释放, 插件卸载时就泄漏了.")
   (P "核心已经能表达这件事. 效应做成P的反应子fiber; 它声明依赖A的条目键" (&entry $a)
      " (笔记0中" (Q "条目即键") "的编码). P结束时, 它先等运行期子fiber退出; "
      "A结束时, A离开Active, 子fiber的目标变为⊥, 子fiber卸载. "
      "而且只要子fiber还解析着A的条目, A的卸载就整体停住, 一项也不撤 (笔记0中卸载的守卫" $held
      "). 所以效应总在A的上下文的任何部分拆除之前撤销.")
   (P "核心缺的只有一样: agent是运行时的数据, 而事件处理器是静态的列表, 不能为每个agent列一个组件.")
   ((Definition #:id "def-ireact")
    "事件处理器可以是一族" (&-> $E $C) ", 其中" $E "是事件载荷 (如agent的编号). 标签"
    (Lab $I-React $n $i $e) "运行" $n "的第" $i "个处理器族在" $e
    "处的实例; 它就是核心的" (Lab $O-React $n $i) ", 只是用的组件表把处理器族实例化在" $e
    "处. 其余标签都是核心规则, 用不带处理器的组件表.")
   (P "两张表只差在处理器列表上, 而除" $O-React "外没有规则读它. 一族处理器的实例各是一个组件, "
      "这与论文4.4节" (Q "一个组件绑定两个载荷就是两个组件") "是同一个想法; "
      "区别在于论文的载荷在插入时绑定 (配置), 这里在反应时绑定.")
   ((Theorem #:id "thm-inter")
    "设组件表 (含每个处理器实例) 有界, 且秩沿提供, spawn和" (Em "每个处理器实例") "严格递增."
    (Ol (Li (B "安全与包含") " (不需要假设): 可达状态都良构; 已安装的fiber的生命期在它的父fiber, "
            "以及它解析的每个提供的提供者的生命期之内; "
            "只要有别的fiber还解析着一个Unloading的fiber的某个提供, 后者既不撤销任何一项, "
            "也不请子fiber退出.")
        (Li (B "活性") ": 生命周期步没有无限执行, 静止即稳定.")
        (Li (B "恢复") " (服务可撤销): 一个效应, 从它的fiber出现之前到它不再安装之后 "
            "(agent结束, 插件卸载, 或被直接移除), 每个绑定都" $simeq
            "同一次执行中略去它的全部操作所得的结果, 不论别的fiber同时做了什么.")))
   (P "合流不在其中, 这与笔记0一样: 合流不含运行期的反应. 恢复所需的独立性对任意两个组件 "
      "(含任意处理器实例) 可能在同一键上执行的操作陈述, 这本来就遍历所有组件; "
      "同一处理器族的不同实例两两相遇受控, 通常由登记名带上agent编号得到. "
      "活性的证明与可选依赖一样换一张组件表: 它的处理器列表含当前这一步的实例和现存子fiber的组件.")
   ((Example #:id "ex-agent")
    "agent3提供键" (Code "(agent 3)") "; guard的处理器是restrict这一族, restrict3依赖"
    (Code "(agent 3)") "并在其中登记一项. guard以载荷3反应之后移除agent3, 原型的记录是:"
    (CodeB "实例 35 (插件 agent3) 开始卸载, 原因: 被要求移除
实例 38 (插件 restrict3) 开始卸载, 原因: 依赖变化, 原视图 (((agent 3) . 37)), 现在有依赖不可用
撤销 (agent 3) (编号 37) 上的一次操作 (令牌 39)
卸载完毕 -> inactive (正常卸载)
撤销提供 (agent 3) (编号 37)")
    "agent3开始卸载之后, 只要restrict3还解析着它, agent3就一步也走不了; 限制先撤销, agent3才撤回自己的上下文. "
    "若改为移除guard, 限制随guard撤销并移除, agent3照常运行, 上下文完整.")
   (P "限制撤销之后, 子fiber停在Inactive, 直到插件卸载才被移除; 若同一编号的agent重建, 它会重新加载. "
      "dsh的agent编号不复用, 所以这不会发生; 需要一次性的语义时, 插件在" (Q "agent结束")
      "的处理器里移除这个子fiber即可, 不必加规则.")

   (H2. "汇总" #:id "sec-xsummary")
   (P "仿照笔记0的汇总定理, 下表列出每项扩展对组件表 (或编排者) 的义务换来哪几层保证. "
      "服务一侧沿用笔记0的两档规格: 可撤销与可合流.")
   (Tab (HRow "层" "可选依赖" "编排者" "交集生命期")
        (Row "0 安全" "无假设" "无假设" "无假设; 另有包含性质")
        (Row "1 活性" "有界, 秩含可选边" "笔记0的义务; 输入有限" "有界, 秩含处理器实例")
        (Row "2 恢复" "可撤销" "可撤销" "可撤销")
        (Row "3 合流" (Span "可合流; 每个变体完整提供; 子组件无可选依赖")
             (Span "可合流; 存活组件相同, 顺序不限") "不覆盖"))
   (P "下一章按这张表重新给审计过的26个插件分层.")

   (H2. "按扩展重新分层" #:id "sec-regrade")
   (P "审计按笔记0的四层给26个插件打过分, 也估计过每个插件按怎样的升级路径能到哪一层; "
      "其中8个插件只有等模型扩展之后才有希望进入模型. 有了三项扩展, 我们逐个重看: "
      "哪项扩展适用, 要满足什么前提, 在原有的升级路径之外再用上扩展能到哪一层. "
      "逐条证据 (文件与行号) 在cordis-rocq仓库的" (Code "docs/PLUGIN-AUDIT.md")
      "及其" (Code "docs/plugin-audit/") "目录里.")
   (P "判断时有几条要点.")
   (Ul (Li (B "可选依赖只解决时机") ". 分支若取决于文件, 网络, 环境变量或端口就绪的时刻, "
           "重载帮不上, 仍然算外部世界.")
       (Li (B "编排是否在模型内, 看走哪条路") ". 改loader的条目再由loader协调, "
           "或者经宿主的" (Code "pluginManager.installBundle") ", " (Code "updateBundle") ", "
           (Code "hmr.reconcileProfile()") ", 都在模型内: 后几个先装包, 再由loader按配置停止, 替换, 启动条目. "
           (Code "internal/*") "钩子, 给自己进程发SIGTERM, 自己清模块缓存再热替换自己, "
           "直接移除启动时spawn的子fiber, 都在模型外.")
       (Li (B "自更新拆成两半") ": 下载新版本属于外部世界; 让新版本生效应当是loader对条目的重启或替换.")
       (Li (B "插件自己创建的agent不算交集生命期") ": 效应的生命期就是插件的, 核心的子fiber已经够用."))
   (P "下表中" (Q "原估计") "是扩展之前审计估计的可达层, " (Q "现在") "是再用上扩展之后的可达层, 加粗的是可达层或覆盖范围变了的. "
      "那8个插件的原估计本来就以扩展为前提, 所以其中多数数字不变, 变的是扩展部分现在已经证明. "
      (Q "启动部分") "的第3层指合流对不含运行期反应的执行成立, 与笔记0相同.")
   (Tab (HRow "插件" "用到的扩展" "前提与剩余障碍" "原估计" "现在")
        (Row "dsh-token-diet" "可选依赖"
             "按connection在不在二选一传输; 拆成两个inject子fiber会在两者都在时都登记, 改变行为; 可选依赖正好表达二选一, 不必改行为"
             "3" "3")
        (Row "dsh-agent-compare" "可选依赖"
             "启动时按agents或subagents二选一runner; 两者都不在时不能抛错, 否则失败的fiber不会因依赖出现而重载"
             "3" "3")
        (Row "dsh-remote" "三项"
             "/remote命令按commands在不在登记; 每个agent上的工具限制; 自热重载要改为经loader; 原估计把限制作例外, 现在限制留在0–2层内"
             "3" (B "启动部分3"))
        (Row "dsh-skills-manager" "交集生命期, 可选依赖"
             "每个agent上登记的skill provider改为依赖agent条目键的反应子fiber; 启动部分可到3"
             "2" "2")
        (Row "dsh-onebot-qq" "编排者"
             "设置经configEditor改写自己的条目再重启, 是经loader的编排"
             "2" (B "3"))
        (Row "dsh-qqbot" "编排者, 可选依赖"
             "写cordis.patch.yml由loader热重载; 需约定唯一的审批拦截者; 凭据来自配置与环境变量"
             "2" (B "对固定环境3"))
        (Row "dsh-im" "编排者"
             "自更新改为下载加经loader重启; 去掉父fiber对子fiber的等待; 通道按本地文件启动"
             "2" (B "对固定环境3"))
        (Row "dshmarket" "编排者, 可选依赖"
             "改条目, 在处理器中热挂载插件 (即反应子fiber), 经hmr.reconcileProfile的自更新都在模型内; 须去掉internal/plugin钩子, SIGTERM, 子进程装卸, 启动后自动重装; 原来只有组件部分到2"
             "2" (B "2"))
        (Row "dsh-mnemon" "编排者, 交集生命期"
             "bundle经loader的group; 每个agent上的钩子改为反应子fiber; 须去掉internal/config钩子与CLI安装; 原来只有本体到2"
             "2" (B "2"))
        (Row "dsh-free-search" "编排者"
             "改写web条目必须只在值不同时写, 否则自我触发, 违反输入有限; 不再改别人的服务实例; 自升级经loader; 编排部分已在模型内"
             "2, 固定环境3" "同左")
        (Row "ds-harness-remote" "编排者, 可选依赖"
             "自更新经pluginManager; 按connection条件provide违反每个变体完整提供; 换服务器时移除启动期效应"
             "2" "2")
        (Row "dsh-ops" "交集生命期"
             "五处agent.ctx.plugin与每个agent的工具限制改为反应子fiber; 仍需宿主提供tools/change事件; 扩展部分已证"
             "2" "2")
        (Row "dsh-windows-mcp" "交集生命期"
             "每个agent的工具限制在插件卸载时泄漏, 改为反应子fiber后两个方向都撤销; 另需修丢弃的路由disposer; 扩展部分已证"
             "2" "2")
        (Row "dsh-mobile-plugin" "可选依赖 (到第2层)"
             "唯一依赖时机的分支在被spawn的子组件里; 换代时移除自己spawn的子fiber; 仍需宿主的路由分发原语"
             "1到2" "1到2")
        (Row "dsh-prime-memory" "可选依赖"
             "settings的挂载时机由重载解决, 不再需要internal/service; 数据目录属外部世界"
             "2" "2")
        (Row (Span "dsh-image-gen, dsh-whale-widget, dsh-plugin-wallpaper-engine") "可选依赖"
             "为兼容旧宿主而软读的服务声明成可选依赖; 第3层的障碍是外部世界"
             "不变" "不变")
        (Row (Span "其余8个") "无"
             "障碍是猴补丁, globalThis, internal/*, 泄漏, 撞名或外部世界, 三项扩展都不涉及"
             "不变" "不变"))
   (P "汇总起来:")
   (Ul (Li (B "第3层多了3个") ": dsh-onebot-qq无条件; dsh-qqbot要先约定唯一的审批拦截者, "
           "dsh-im要先去掉对子fiber的等待, 两者都只对固定的外部环境合流. "
           "它们原来都卡在" (Q "编排者") ", 而它们的编排本来就经loader的条目.")
       (Li (B "原来要等扩展的8个中有6个可以到第2层") ", 扩展部分都已证明: "
           "dshmarket与dsh-mnemon从只覆盖一部分扩大到整个插件, dsh-free-search与ds-harness-remote靠编排者, "
           "dsh-ops与dsh-windows-mcp靠交集生命期.")
       (Li (B "可选依赖") "让dsh-token-diet不必改行为就到第3层, 让dsh-prime-memory不再需要"
           (Code "internal/service") "; 对多数插件, 它把" (Q "为兼容旧宿主而软读") "变成正式的声明.")
       (Li (B "仍要宿主新增原语") "的: dsh-mobile-plugin (路由分发与换代), dsh-cost-meter (计费事件), "
           "dsh-ops (" (Code "tools/change") "), dsh-context (工具所有者查询). "
           "交集生命期还要宿主把agent的上下文暴露成键, 事件带上agent的编号.")
       (Li (B "剩下的主要障碍是外部持久的世界") ": 配置, 凭据, 缓存, 日志不属于上下文, "
           "恢复精确性只对上下文陈述; B类插件第3层的障碍大多是它, 这一项还没有写进模型."))
   (P "审计文件中有五处原来没写清, 已按插件的发布包和宿主的源码逐一复核. "
      "dsh-agent-compare的runner确实在启动时选; " (Code "pluginManager") "的装包接口先装包, "
      "再由loader按配置重组条目, 所以经它的编排在模型内, dshmarket的自更新同理; "
      "dsh-mnemon移动的只是已禁用的配置行, 不涉及运行中的fiber; "
      "dsh-windows-mcp直接改写配置文件, 由下次启动执行, 但绕过了插件管理器的文件锁; "
      "dsh-im对" (Code "connection") "的检查是对必需依赖接口形状的检查, 不是存在性的分支. "
      "复核之后各插件的可达层不变, 只有dsh-agent-compare的第3层多了上表中的那个前提.")

   (H2. "与论文的关系" #:id "sec-xpaper")
   (P "论文 (arXiv:2608.25512v1) 4.4节给出四个扩展: 异步, 失败, 隔离, 配置. "
      "它们在笔记0的核心中已有对应, 都不是本文的三项. 重合与互补在这几处:")
   (Ul (Li "论文5.2.1节说, 由定理80, loader不论以什么顺序实例化和退役, 系统都停在从头加载最终配置的状态. "
           "笔记0中与编排交错的合流要求存活组件顺序相同, 覆盖不到" (Q "任意顺序") "; "
           (Ref "thm-orch") "补上了这一点.")
       (Li "论文4.4节的修订 (退役, 卸载, 移除, 重新插入) 就是" (Ref "ex-update") "中的自更新; "
           "论文在同一名字上重新插入, 这里用新的名字, 合流本来就在重命名意义下成立.")
       (Li "论文说组件修改自己的配置或禁用自己时, 改动写回它的条目. 这与把编排者建模为依据持久配置的输入源一致; "
           "论文没有讨论插件本身充当编排者时的义务.")
       (Li "交集生命期依赖论文同样采用的L-Unload守卫: 效应在agent上下文的任何部分拆除之前撤销, "
           "正是因为整个卸载被守卫停住.")
       (Li "可选依赖是论文没有覆盖的. 论文5.1.4节的代理访问对未声明的键抛" (Code "UNDECLARED_ACCESS")
           ", 而" (Code "ctx.get") "从不失败; 审计中读可选服务的写法正是绕道" (Code "ctx.get")
           ". 论文5.2.1节说, 组件只在部分配置下装上它声明的键时, 加载结果也随这些配置而定; "
           "这对应于可选依赖要求每个变体都完整提供."))

   (H2. "Rocq对应" #:id "sec-rocq")
   (P "文件都在cordis-rocq仓库的" (Code "theories/Ext/") "下, 核心不引用它们.")
   (Tab (HRow "本文" "Rocq" "文件")
        (Row (Ref "thm-xcore") (Code "xstep_core, xstep_conservative") (Code "Optional.v"))
        (Row (Span (Ref "thm-opt") " 安全") (Code "wf_recomp, xwf_reachable") (Code "Optional.v"))
        (Row (Span (Ref "thm-opt") " 活性") (Code "xquiet_settled, xtermination") (Span (Code "OptProgress.v") ", " (Code "OptTermination.v")))
        (Row (Span (Ref "thm-opt") " 恢复") (Code "xrecovery_exact") (Code "OptRecovery.v"))
        (Row (Span (Ref "thm-opt") " 合流") (Code "xcontrol_confluence, xstate_confluence, xspawn_confluence, xdynamic_agree")
             (Span (Code "OptConfluence.v") ", " (Code "OptState.v") ", " (Code "OptSpawn.v") ", " (Code "OptDynamic.v")))
        (Row (Ref "thm-orch") (Code "orch_wf, orch_live, orch_recovery, dynamic_agree_perm") (Code "Orchestrator.v"))
        (Row (Ref "thm-inter") (Code "iwf_reachable, effect_within, unload_blocked_i, iquiet_settled, ieffect_exact, itermination") (Span (Code "Intersect.v") ", " (Code "IntersectLive.v")))
        (Row (Ref "sec-xsummary") (Code "xsum_*, osum_confluence, isum_*") (Code "ExtSummary.v"))
        (Row (Span (Ref "ex-assistant") ", " (Ref "ex-agent")) (Code "OptEx, AgentEx") (Code "Examples.v")))
   (P "可选依赖的合流和" (Ref "thm-xcore") "的保守性 (" (Code "xstep_conservative")
      ") 用到核心已引入的函数外延性, 其余定理不依赖任何公理.")
   ))
