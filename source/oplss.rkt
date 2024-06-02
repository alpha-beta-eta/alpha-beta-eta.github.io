#lang racket
(provide oplss.html)
(require SMathML)
(define oplss.html
  (TmPrelude
   #:title "OPLSS"
   #:css "styles.css"
   (H1 "OPLSS 俄勒冈编程语言暑期学校")
   (P "不懂编程语言, 也不懂数学和逻辑, 我该怎么办呢?")
   (P "白鹤衔苦桃, 千里作一息. 欲往蓬莱山, 将此充粮食. "
      "未达毛摧落, 离群心惨恻. 却归旧来巢, 妻子不相识.")
   (P "随便记点笔记, 但是我不能保证这里的笔记的准确性.")
   (H2 "会话类型并发编程")
   (Ul (Li "演员模型 (actor model)")
       (Li "会话类型 (session types)")
       (Li "线性逻辑 (linear logic)")
       )
   (H3 "消息传递并发")
   (H4 "通过沿信道交换消息进行计算的进程")
   (Ul (Li $n "-ary信道")
       (Li "非确定性")
       (Li "形式化基础: 进程演算, 例如" $pi "演算")
       )
   (H4 "消息传递队列")
   (Ul (Li "移动性 (mobility)")
       (Li "高阶信道 (higher-order channel)")
       )
   (H4 "消息交换协议的类型")
   (P "会话类型 (Honda, Kohei. 1993. 'Types for Dyadic Interaction'. "
      "In CONCUR'93, edited by Eike Best, 715:509–523. Lecture Notes in Computer Science. Springer, Berlin, Heidelberg.)")
   (CodeB "A ::= ?[T].A'
   |  ![T].A'
   |  &amp;{l_1: A_1; ...; l_n: A_n}  (external choice)
   |  &oplus;{l_1: A_1; ...; l_n: A_n}  (internal choice)
   |  end
   |  X
   |  μX.A'
T ::= A
   |  basic types such as int, char, ...")
   (CodeB "queue = &amp;{enq: ?[char].queue;
          deq: &oplus;{none: end; some: ![char].queue}}")
   (CodeB "[client] ---(q: queue)--- [O]-[P]-[L]-[ ]

q: ... (queue)
  send 'enq' along q
q: ?[char].queue
  send 'S' along q
q: queue")
   (P "观察: 信道/进程的类型随着消息的交换而改变")
   (H4 "会话类型中的保持 (" (Q "session fidelity") ")")
   (P "guarantees that expectation of client matches with "
      "the one of providers if they do initially")
   (H2 "线性逻辑")
   (P "线性逻辑拒绝弱化规则和收缩规则, 这是因为在线性逻辑中命题相当于资源, "
      "我们不能随意添加或删除. 弱化和丢弃资源有关, 收缩和复制资源有关.")
   
   ))