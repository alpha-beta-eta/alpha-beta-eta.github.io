#lang racket
(provide abstract_sl.html)
(require SMathML)
#;
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define split2 (&split 2))
(define (Hoare P C Q)
  (split2 (cur0 P) C (cur0 Q)))
(define (Label str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $FrameRule
  (Label "FrameRule"))
(define $ConcurrencyRule
  (Label "ConcurrencyRule"))
(define $par (Mo "&parallel;"))
(define $bull (Mo "&bull;"))
(define $rharu (Mo "&rharu;"))
(define $dummy (Mi "&sdot;"))
(define $# (Mo "#"))
(define $pre (Mo "&pre;"))
(define-infix*
  (&pre $pre)
  (&# $#)
  (&rharu $rharu)
  (&bull $bull)
  (&par $par))
(define (pfunc f A B)
  (&: f (&rharu A B)))
(define abstract_sl.html
  (TnTmPrelude
   #:title "局部动作和抽象分离逻辑"
   #:css "styles.css"
   (H1. "局部动作和抽象分离逻辑")
   (H2 "摘要")
   (P "分离逻辑 (separation logic) 是Hoare逻辑的一种扩展, "
      "它支持以一种局部的方式对修改内存的程序进行推理. "
      "我们给出了对隐藏在该逻辑背后的语义结构的一项研究. "
      "其核心思想是局部动作 (local action), "
      "即一种以局部的方式修改状态的状态变换器. "
      "我们针对一类称为分离代数 (separation algebra) 的模型表述了局部动作, "
      "这类模型是对分离逻辑相关工作中所使用的RAM以及其他特定具体模型的抽象. "
      "局部动作为一种推广形式的 (顺序) 分离逻辑提供了语义. "
      "我们还表明, 我们对局部动作所施加的条件使得一个"
      "并发分离逻辑能够有一个一般的可靠性证明, 该逻辑在任意分离代数上解释.")
   (H2. "引论")
   (P "分离逻辑是Hoare逻辑的一种扩展, "
      "它已被用来攻克对内存中数据结构的修改"
      "进行推理这一老问题 [33, 17, 23, 34]. "
      "分离逻辑的能力来自分离合取连接词 "
      "(separating conjunction connective) " $*
      "与使用" $* "的命令的证明规则之间的相互作用. "
      "这些规则中最主要的是框架规则 (frame rule) [17, 23] "
      "和并发规则 (concurrency rule) [22]."
      (MB (&rull $FrameRule
                 (Hoare $p $C $q)
                 (Hoare (&* $p $r) $C (&* $q $r))))
      (MB (&rull $ConcurrencyRule
                 (Hoare $p_1 $C_1 $q_1)
                 (Hoare $p_2 $C_2 $q_2)
                 (Hoare (&* $p_1 $p_2)
                        (&par $C_1 $C_2)
                        (&* $q_1 $q_2))))
      "框架规则把局部推理的一种直觉编纂成文. "
      "其想法是, 如果我们确立了一个给定的Hoare三元组, "
      "那么前条件包含了该命令在计算期间将会访问的所有资源 "
      "(命令开始之后分配的资源除外). "
      "因此, 任何额外的状态都将保持不变; "
      "于是规则中的不变断言" $R
      " (框架公理 (frame axiom)) "
      "可以被自由地附加到前条件和后条件上. "
      "类似地, 并发规则陈述了, "
      "在分离的资源上操作的进程可以被独立地推理.")
   (P "句法上, 并发规则和框架规则是直截了当的. "
      "但是, 它们可靠性的原因是微妙的, "
      "并且依赖于关于命令式程序运作的局部方式的观察 [37]. "
      "通常, 一个程序访问一组有界限的资源; "
      "例如, 执行期间访问的内存单元 "
      "(内存足迹 (memory footprint)). "
      "我们在本文中的目的是以一种一般的方式描述这些语义假设, "
      "针对一组对分离逻辑相关工作中所使用的RAM"
      "以及其他具体模型进行抽象的模型.")
   (P "通过为一类模型分离出界限原则 (circumscription principle), "
      "为该逻辑提供正当性所需的本质假设变得更加清晰. "
      "特别地, 我们对并发的处理表明, "
      "分离逻辑的一个并发版本的可靠性只依赖于程序语言中原始动作 "
      "(primitive action) (基本命令) 的局部性性质. "
      "并发逻辑的可靠性最初是非常难以得到的, "
      "即便是对于一个特定的分离代数 (RAM模型); "
      "它是在Brookes的一项杰出工作 [12] 中被证明的. "
      "我们对并发的处理建立在Brookes最初的洞见之上, "
      "但在表述中做出了若干不同的选择, "
      "这些选择容许一个更一般的证明, "
      "该证明适用于任意分离代数. "
      "我们表明, 只要一个语言中的原始命令满足框架规则, "
      "就能得到一个并发逻辑的模型. "
      "这与Brookes最初的论文以及后续关于"
      "并发分离逻辑的论文 [10, 11, 16, 15] 形成对比, "
      "这些论文全都是针对特定模型证明可靠性的, "
      "其方式依赖于对原始动作的非常特定的解释, "
      "而不是针对一类模型.")
   (P "本文不会包含任何使用该逻辑的例子; "
      "参见例如 [36, 28, 5, 4] 及其参考文献.")
   (P (B "警告. ")
      "在本文中, 我们避免传统Hoare逻辑中"
      "把程序变量双关地当作逻辑变量的做法, "
      "以避免证明规则中令人讨厌的附加条件; "
      "进一步的讨论参见 [9, 27]. "
      "这是为了理论上的简洁; "
      "我们的结果可以被扩展以涵盖变量修改, "
      "正如大多数分离逻辑论文中所做的那样, "
      "连同它们相应的modifies子句. (此外, "
      "避免这种双关更符合像C或ML这样的真实语言, "
      "即便它稍微偏离了程序逻辑中的理论传统.)")
   (H2. "分离代数与谓词")
   (P "大多数关于分离逻辑的论文都使用一个堆 (heap) 的域, "
      "该域配备了一个部分运算符, "
      "用于把在某种意义上分离的堆粘合在一起 "
      "(文献中出现过各种不同的意义). "
      "我们用下面的定义对这一情形进行抽象.")
   ((definition #:n "1. 分离代数")
    "一个分离代数 (separation algebra) "
    "是一个可消的部分交换幺半群" (tu0 Σ $bull $u)
    ". 一个部分交换幺半群由一个部分二元运算给出, "
    "对于该运算, 单位律, 交换律和结合律对于这样一种等式成立: "
    "该等式意味着两边都有定义且相等, 或者两边都无定义. "
    "可消性质说的是, 对于每个" (∈ $sigma Σ) ", 部分函数"
    (pfunc (&bull $sigma (@ $dummy)) Σ Σ)
    "是单射. 诱导出的分离性 (separateness) ("
    $# ") 关系和子状态 (substate) ("
    $pre ") 关系由下式给出:"
    
    )
   ))