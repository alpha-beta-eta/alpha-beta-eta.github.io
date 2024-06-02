#lang racket
(provide seplogic.html)
(require SMathML)
(define $varepsilon
  (Mi "&varepsilon;"))
(define :i (Ms "i"))
(define :j (Ms "j"))
(define :list (Ms "list"))
(define :nil (Ms "nil"))
(define :a (Ms "a"))
(define (separate x* k)
  (let-values (((x* y*) (split-at-right x* 1)))
    (k x* (car y*))))
(define (∃ . x*)
  (separate
   x* (λ (x* P)
        (: $exists (apply &cm x*)
           $. P))))
(define App (&split 2))
(define (&list α i)
  (App :list α i))
(define concat &d*)
(define $dagger (Mi "&dagger;"))
(define pointsTo &@->)
(define-@lized-op*
  (@list &list)
  (@concat concat)
  (@pointsTo pointsTo))
(define seplogic.html
  (TnTmPrelude
   #:title "分离逻辑"
   #:css "styles.css"
   (H1. "分离逻辑: 共享可变数据结构的逻辑")
   (H2 "摘要")
   (P "在与Peter O'Hearn等人的合作研究中, "
      "基于Burstall的早期思想, "
      "我们发展了Hoare逻辑的一种扩展, "
      "它使得对使用共享可变数据结构的"
      "低层次命令式程序进行推理成为可能.")
   (P "我们对简单的命令式程序设计语言进行了扩展, "
      "加入了用于访问和修改共享结构, "
      "以及用于显式分配和释放存储的命令 (而非表达式). "
      "断言部分则通过引入一个" (Q "分离合取")
      "得到扩展, 该合取断言其各子公式"
      "在堆的互不相交的部分上成立; "
      "与之密切相关的还有一个" (Q "分离推出")
      ". 结合抽象数据结构上谓词的归纳定义, "
      "这一扩展使得对具有受控共享的结构"
      "给出简洁而灵活的描述成为可能.")
   (P "在本文中, 我们将综述该程序逻辑目前的发展状况, "
      "包括那些允许无限制地址算术, "
      "动态分配数组以及递归过程的扩展. "
      "我们还将讨论若干有前景的未来方向.")
   (H2. "引论")
   (P "共享可变数据结构, "
      "即其中某个可更新字段可以从不止一处被引用的结构, "
      "其使用在系统程序设计与人工智能等诸多不同领域中都十分普遍. "
      "对这一技术进行推理的各种方法已被研究了三十年, "
      "但所得到的结果要么适用范围有限, 要么极为复杂, "
      "并且即便对于规模中等的程序也难以良好地扩展. "
      "(参考文献[28]中给出了一份不完整的文献目录.)")
   (P "这些方法所面临的问题在于, 一个改变数据结构的程序, "
      "其正确性通常依赖于对这些结构中共享关系的复杂限制. "
      "为了说明这一问题以及我们对其解法的思路, "
      "考虑一个简单的例子. "
      "下面的程序对一个列表执行原地反转:"
      (CodeB "j := nil;
while i != nil do
  (k := [i + 1]; [i + 1] := j; j := i; i := k)")
      "这里的记号" (bra0 $e) "代表地址" $e
      "处的存储的内容. "
      "{译注: 原文的代码本来用的是不等号, "
      "但是我将其改成了" (Code "!=")
      ", 这是C语言的习惯.}")
   (P "该程序的不变量必须要陈述以下内容: "
      :i "和" :j "是分别表示两个序列"
      $alpha "和" $beta
      "的列表, 其满足初值" $alpha_0
      "的反转可由" $alpha "的反转在" $beta
      "上拼接得到. 也就是说, 我们有"
      (MB (∃ $alpha $beta
             (&conj (@list $alpha :i)
                    (@list $beta :j)
                    (@= (_^ $alpha $0 $dagger)
                        (concat
                         (^ $alpha $dagger)
                         $beta)))))
      "其中谓词" (&list $alpha :i)
      "归纳定义于" $alpha "的长度之上:"
      (eqn*
       ((&list $varepsilon :i)
        $def=
        (@= :i :nil))
       ((&list (@concat :a $alpha) :i)
        $def=
        (∃ :j (&conj
               (@pointsTo :i (&cm :a :j))
               (@list $alpha :j)))))
      "而" $@-> "可以读作" (Q "指向") ".")
   (P "然而不幸的是, 这还不够, 因为一旦"
      
      )
   (H2. "编程语言")
   
   ))