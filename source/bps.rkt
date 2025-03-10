#lang racket
(provide bps.html)
(require SMathML)
(define bps.html
  (TnTmPrelude
   #:title "构建问题解决器"
   #:css "styles.css"
   (H1. "构建问题解决器")
   (H2. "前言")
   (H2. "导引")
   (H2. "传统问题求解")
   (H3. "问题空间模型")
   (H3. "CPS设计")
   (P "从概念上说, CPS由两部分构成: "
      "一个是用户用以提供问题空间的接口, 另一个则是搜索引擎. "
      "显然, 我们需要操纵状态和行动的能力. "
      "但是, 何种操作是我们所需要的呢?")
   (P "以下是三种我们需要能够对于状态施行的操作."
      (Ol (Li "目标检测: 判断给定状态是否满足目标要求.")
          (Li "状态同一性: 检测两个对于状态的描述是否指的是同一个状态.")
          (Li "状态显示: 产生给定状态的人类可读的描述.")))
   (P "目标检测的重要性是不言自明的. 状态同一性之所以重要, "
      "是因为重新探索已经检视过的状态并不能算是搜索的进展. "
      "显示状态的重要性在于即便CPS的结果只是由另外的程序使用, "
      "这样的显示对于debug而言往往也是必要的.")
   (P "针对行动的接口应该长什么样? 从概念上说, "
      "存在四种不同的必要操作:"
      (Ol (Li "识别什么样的行动是可用的.")
          (Li "确定一个给定行动是否可以应用于一个特定的状态.")
          (Li "给定一个状态和一个可以应用于其的行动, "
              "判断出这个行动于该状态上所有可能的实例化方式.")
          (Li "获取将一个实例化了的行动应用于一个状态而得到的新状态.")))
   (P "这些操作经常被合称为扩展(一个状态), "
      "而一个后继状态皆已计算完毕的状态被称为是扩展了的.")
   (P "搜索引擎本身需要做什么呢? 这种程序的设计对于读者而言应该是相当熟悉了的. "
      "基本上来说, 存在一个队列, 其初始状态即是包含搜索的初始状态. "
      "搜索过程按照以下步骤进行:"
      (Ol (Li "从队列中弹出一个状态.")
          (Li "如果该状态满足目标要求, 则返回成功的路径以表示搜索已经结束且成功.")
          (Li "否则的话, 计算可以应用于当前状态的行动, 以及由这些行动所产生的新的状态. "
              "以此更新队列, 然后从头开始.")
          (Li "如果队列为空, 那么返回且发出失败的信号.")))
   (P "队列组织和更新的细节决定了搜索所遵循的策略. "
      "一个无序的FIFO队列对应于广度优先搜索, "
      "而一个无序的LIFO队列对应于深度优先搜索. "
      "给定一个到达目标状态的剩余距离的启发式估计, "
      "更为强大的搜索策略则是可能的. "
      "将队列按照到达目标的最小预估(总)距离排序构成了最佳有限搜索. "
      "[注记: 最佳优先搜索 (best-first search), 指的是根据某种标准或者说函数, "
      "总是优先扩展当前(估计)最佳的状态或者说路径, "
      "一致代价搜索和A*搜索都是最佳优先搜索的实例.] "
      "beam search是最佳有限搜索的资源限制版本.")
   
   (H3. "CPS实现的事项")
   (H3. "CPS实现")
   (H3. "Boston地铁导航")
   (H3. "解决代数问题")
   (H3. "搜索之荣光已逝?")
   (H3. "反向指针")
   (P "这里的模式匹配器和化简器受到了Gerald Sussman"
      "出于研究目的所写的Scheme版本的启发.")
   (H3. "练习")
   (H3. "参考文献")
   (H2. "模式导向的推理系统")
   (H3. "模式导向的推理系统模型")
   (H4. "断言")
   (H4. "规则")
   (H4. "PDIS主题的各种变体")
   (H3. "TRE的设计")
   (H3. "TRE的实现")
   (H3. "自然演绎")
   (H3. "结论")
   (H3. "反向指针")
   (H3. "练习")
   (H3. "参考文献")
   (H2. "扩展模式导向的推理系统")
   (H3. "为了方便, 效率, 性能的设计")
   (H3. "实现FTRE")
   (H3. "例子")
   (H3. "重奏 (reprise)")
   (H3. "反向指针")
   (H3. "练习")
   (H3. "参考文献")
   (H2. "事实维护系统导引")
   (H3. "为什么使用TMS?")
   (H3. "什么是TMS?")
   (H3. "事实维护系统的基本")
   (H3. "澄清 (justification) 是如何帮忙的")
   (H3. "TMS的命题描述")
   (H3. "TMS族")
   (H3. "练习")
   (H3. "参考文献")
   (H2. "基于澄清 (justification) 的事实维护系统")
   (H3. "JTMS结点性质")
   (H2. "使得JTMS运作")
   (H2. "基于逻辑的事实维护系统")
   (H2. "使得LTMS运作")
   (H2. "实现定性过程理论")
   (H2. "基于假设的事实维护系统")
   (H2. "改善事实维护系统的完备性")
   (H2. "使得ATMS运作")
   (H2. "前件约束语言")
   (H2. "基于假设的约束语言")
   (H2. "一个小型诊断引擎")
   (H2. "符号松弛系统")
   (H2. "一些前沿")
   ))