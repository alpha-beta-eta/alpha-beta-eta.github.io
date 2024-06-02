#lang racket
(provide cmu15150.html)
(require SMathML)
(define (seq . x*)
  (ang0 (apply &cm x*)))
(define $darr (Mo "&darr;"))
(define $->hk (Mo "&rarrhk;"))
(define $==> (Mo "&xrArr;"))
(define-infix*
  (&darr $darr)
  (&->hk $->hk)
  (&==> $==>))
(define cmu15150.html
  (TnTmPrelude
   #:title "CMU 15-150笔记"
   #:css "styles.css"
   (H1. "CMU 15-150笔记")
   (P "主要是对于CMU 15-150课程的幻灯片等材料的翻译.")
   (H2. "求值和定型, 绑定和作用域")
   (H3. "引入")
   (P "今天我们给出了课程的大纲及其目标, 包括引用透明, 外延等价, 以及并行. "
      "我们从观察其类型开始探索SML语言.")
   (P (B "关键概念")
      (Ul (Li "计算是函数式的")
          (Li "编程作为阐释性的过程")
          (Li "类型, 值, 表达式")
          (Li "定型规则和求值规则")
          (Li "integer, real, boolean, product")
          (Li "并行, work, span")
          (Li "外延等价")))
   (H3. "幻灯片")
   (H4. "课程哲学")
   (Ul (Li (B "计算") "是函数式的")
       (Li (B "编程") "是阐释性的语言过程"))
   (H4. "计算是函数式的")
   (Ul (Li (B "值") "相对于" (B "类型") "被分类")
       (Li (B "表达式") "{译注: 也有类型}")
       (Li (B "函数") "将值映射为值"))
   (H4. "命令式 vs. 函数式")
   (Table
    (Tr (Td "命令") (Td "表达式"))
    (Tr (Td "被执行") (Td "被求值"))
    (Tr (Td "具有副作用") (Td "无副作用"))
    (Tr (Td (Code "x := 5"))
        (Td (Code "3 + 5")))
    (Tr (Td "(新状态)") (Td "(新值)")))
   (H4. "编程作为阐释")
   (Ul (Li "问题陈述"
           (Ul (Li "不变量")
               (Li "规格")
               (Li "正确性证明")))
       (Li "分析, 分解和适应, 证明"))
   (H4. "并行")
   (MB (&Table
        ((seq $1 $0 $0 $1 $1) $-> $3)
        ((seq $1 $0 $1 $0 $1) $-> $3)
        ((seq $1 $1 $0 $1 $1) $-> $4)
        ((seq $0 $0 $0 $1 $1) $-> $2)
        ($ $ $darr)
        ($ $ 12)))
   (CodeB "sum: int sequence -> int
type row = int sequence
type room = row sequence
fun count (class: room): int = sum (map sum class)")
   (H4. "分析")
   (Ul (Li "做这个计算需要多长时间?")
       (Li "如何改进" (Code "count") "的运行时间?"
           (Blockquote "分而治之")))
   (H4. "代价分析")
   (P (B "work")
      (Ul (Li "顺序计算")
          (Li "总顺序时间; 操作数目")))
   (P (B "span")
      (Ul (Li "并行计算")
          (Li "在可用处理器数目不受限制时的所需时间; "
              "最长关键 (critical) 路径的长度")))
   (H4. "介绍ML")
   (Ul (Li "类型" $t)
       (Li "表达式" $e)
       (Li "值" $v " (作为表达式的子集)"))
   (H4. "例子")
   (CodeB "     (3 + 4) * 2
=1=> 7 * 2
=1=> 14")
   (CodeB "     (3 + 4) * (2 + 1)
=3=> 21")
   (P "如果使用并行, 第二个需要多少步骤?")
   (CodeB "    &quot;the &quot; ^ &quot;walrus&quot;
==> &quot;the walrus&quot;")
   (CodeB "&quot;the walrus&quot; ^ 1
ill-typed")
   (P "SML从不会求值病态类型表达式!")
   (H4. "类型, 表达式, 值")
   (Ul (Li "类型是对于表达式的值的" (Q "预测")
           ", 如果表达式最终确有一个值的话 "
           "{译注: 求值过程可能因为无限进行而发散}")
       (Li "一个表达式是" (B "良类型")
           "的, 如果其至少具有一个类型, 否则就是"
           (B "病态类型") "的.")
       (Li "良类型表达式具有一个类型, "
           "可能具有一个值, 可能具有一个副作用 "
           "(但是现在的effect-free部分当然是没有的)"))
   (P "每个良形式的ML表达式" $e
      (Ul (Li "具有类型" $t ", 记作" (&: $e $t))
          (Li "可能具有一个值, 记作" (&->hk $e $v)
              ", 或者" (&==> $e $v))
          (Li "可能具有一个副作用 "
              "(但是现在的effect-free部分当然是没有的)")))
   (P "例子:"
      (CodeB "(3 + 4) * 2 : int")
      (CodeB "(3 + 4) * 2 ==> 14"))
   (P "{译注: 结合前文, 看起来" $==> "是归约关系而非求值至的关系.}")
   (H4. "ML中的类型")
   (Ul (Li "基本类型"
           (Ul (Li (Code "int") ", "
                   (Code "real") ", "
                   (Code "bool") ", "
                   (Code "char") ", "
                   (Code "string"))))
       (Li "构造类型"
           (Ul (Li "积类型")
               (Li "函数类型")
               (Li "用户定义的类型"))))
   (H4. "整数(类型), 表达式")
   (Ul (Li "类型: " (Code "int"))
       (Li "值: ..., " (Code "~1") ", "
           (Code "0") ", " (Code "1") ", ...")
       (Li "表达式: " (Code $e_1 " + " $e_2)
           ", " (Code $e_1 " - " $e_2)
           ", " (Code $e_1 " * " $e_2)
           ", " (Code $e_1 " div " $e_2)
           ", " (Code $e_1 " mod " $e_2)
           ", ...")
       (Li "例子: " (Code "~4 * 3")))
   (H4. "整数, 定型")
   (Ul (Li "定型规则"
           (Ul (Li (Code $n ": int"))
               (Li (Code $e_1 " + " $e_2 ": int")
                   ", 如果" (Code $e_1 ": int")
                   "且" (Code $e_2 ": int"))
               (Li "其他运算也是类似的"))))
   (CodeB "(3 + 4) * 2 : int
because (3 + 4): int and 2: int
(3 + 4): int because 3: int and 4: int")
   (H4. "整数, 求值")
   
   (H2. "函数")
   (H3. "引入")
   
   (H3. "幻灯片")
   (H4. "上次内容")
   (Ul (Li "类型, 表达式, 值")
       (Li "外延等价")
       (Li "声明"))
   (H4. "今天的目标")
   (Ul (Li "编写匿名函数 (lambda表达式)")
       (Li "声明有名字的函数")
       (Li "陈述什么是一个函数闭包")
       (Li "对于牵涉函数应用的表达式进行求值")
       (Li "使用模式"
           (Ul (Li "clausal的函数声明")
               (Li "case表达式"))))
   (H4. "声明")
   (CodeB "val     pi :       real = 3.14
keyword identifier type   value")
   (P "这引入了一个绑定, 将" (Code "pi")
      "绑定至" (Code "3.14") ", 也可以写成"
      (Code "[3.14/pi]")
      ". {译注: 其实就是替换的写法.}")
   (P "词法静态作用域 (lexically statically scoped)")
   (H4. "绑定")
   (CodeB "val x : int = 8 - 5 [3/x] 
val y : int = x + 1 [4/y]")
   (H4. "绑定")
   
   (H2. "递归和归纳")
   
   (H2. "列表上的递归和结构归纳, 尾递归")
   
   (H2. "数据类型, 树上的递归和结构归纳")
   (P "我们将数据类型声明作为抽象形式引入, "
      "其旨在将程序员从跟踪低级hacking约定的细节之中解放出来.")
   (P "我们考虑了预先定义了的数据类型" (Code "int option")
      ", 其有一个常量构造子" (Code "None") "和一个非常量构造子"
      (Code "Some") ", 其接受一个" (Code "i : int")
      "以构造出" (Code "Some i : int option")
      ". 我们也作成了一些我们自己的数据类型, 包括用于模拟颜色的类型, "
      "多返回值类型, 以及一个二叉树类型.")
   (P "我们展示了如何使用结构归纳证明关于树的定理.")
   (P (B "关键概念")
      (Ul (Li "数据类型声明")
          (Li "常量构造子")
          (Li "接受参数的构造子")
          (Li "递归定义类型")
          (Li "树类型")
          (Li ""
              )
          ))
   (H2. "渐进分析")

   (H2. "渐进分析")

   (H2. "排序列表")

   (H2. "多态和类型推导")

   (H2. "高阶函数")

   
   ))