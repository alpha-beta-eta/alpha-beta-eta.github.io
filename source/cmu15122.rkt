#lang racket
(provide cmu15122.html)
(require SMathML)
(define cmu15122.html
  (TnTmPrelude
   #:title "命令式编程原理笔记"
   #:css "styles.css"
   (H1. "命令式编程原理笔记")
   (P "CMU 15-122一般是与CMU 15-150并行展开的课程, "
      "前者教授C语言和命令式编程, 后者教授Standard ML和函数式编程. "
      "这两门课程为之后的15-210, 15-213, 15-214做好准备. "
      "15-122的特色或许在于从一开始就引入Hoare logic的概念, "
      "要求学生掌握如何对于命令式程序进行推理.")
   ((definition)
    "前条件 (pre-condition) 描述一个函数的合法 (valid) 的输入应该满足的要求.")
   ((definition)
    "后条件 (post-condition) 描述一个函数的返回值应该满足的要求, "
    "其依赖于该函数的输入和前条件. 从另一个角度来说, 后条件描述了函数所做的事情.")
   ((definition)
    "循环不变量 (loop invariant) 描述了对于某个循环而言每次迭代时不变的性质. "
    "如果一个循环不变量在进入循环之前得到满足, 并且在假设满足该循环不变量的条件下"
    "可以推出其在经过一次迭代后仍然保持正确, 那么我们可以断言该循环不变量"
    "在退出循环时仍然为真.")
   (H2. "")
   (H2. "Ints")
   (H2. "Arrays")
   (H3. "使用数组")
   (P "如果" (Code "t") "是一个类型, 那么" (Code "t[]")
      "是元素类型为" (Code "t") "的数组的类型. 注意到"
      (Code "t") "是任意的: 我们可以有整数的数组"
      (Code "int[]") ", 布尔值的数组" (Code "bool[]")
      ", 或者字符数组的数组" (Code "char[][]")
      ". 这种数组类型句法其实类似于Java, 而和C有点不同.")
   (P "每个数组都有固定的长度, 并且必须使用表达式"
      (Code "alloc_array(t, n)")
      "显式分配. 这里的" (Code "t")
      "是数组元素的类型, 而" (Code "n")
      "是数组元素的数目. 以这个操作, "
      "C0将会保留一块内存区域. "
      "让我们在coin里试试:"
      (CodeB "% coin
--> int[] A = alloc_array(int, 10);
A is 0xECE2FFF0 (int[] with 10 elements)
-->"))
   (P "结果可能有点令人惊讶: " (Code "A")
      "是一个具有10个元素的整数数组, "
      "但是这里说" (Code "A") "是"
      (Code "0xECE2FFF0")
      "是什么意思呢? 正如我们之前在关于整数的讲座里所讨论的, "
      "变量只能存储具有一个比较小的固定大小的值, "
      "这个固定大小即所谓的机器的" (Em "字长")
      ". 具有10个整数的数组将会十倍于这个大小, "
      "所以我们不能将其直接存放在变量" (Code "A")
      "里面. 转而变量" $A "存放的是数组元素"
      "在内存中实际存储位置的" (Em "地址")
      ". 只是在这个情况下地址刚好是"
      (Code "0xECE2FFF0")
      "而已, 但是并不能保证下次你运行coin还能得到相同的地址. "
      "不过, 幸运的是你不能直接将这个地址当作数字使用, "
      "也不必这么做. 转而你可以通过句法" (Code "A[i]")
      "来访问数组的元素, 其中" (: $0 $<= $i $< $n)
      ", 而" $n "是数组的长度. 换言之, " (Code "A[0]")
      "是数组的第0个元素, " (Code "A[1]")
      "是数组的第1个元素. 数组索引是从零开始计的, 也就是"
      (Em "zero-based") ", 例如:"
      (CodeB "--> A[0];
0 (int)
--> A[1];
0 (int)
--> A[2];
0 (int)
--> A[10];
Error: accessing element 10 in 10-element array
Last position: &lt;stdio>:1.1-1.6
--> A[-1];
Error: accessing negative element in 10-element array
Last position: &lt;stdio>:1.1-1.6
-->"))
   (P "我们注意到在分配数组之后, 所有的元素似乎都是0. "
      "这是由实现所保证的, 其会初始化数组的每个元素"
      "以一个依赖于类型的默认值. 类型" (Code "int")
      "的默认值是" (Code "0") ". 一般而言, "
      )
   (H2. "Searching Arrays")
   (H2. "Big-" $O)
   (H2. "Binary Search")
   (H2. "Quicksort")
   (H2. "Libraries")
   (P "标题似乎实则为Data Structures.")
   (H3. "结构体")
   (P "到目前位置本课程里我们已经与五种不同的C0类型打交道了, 即"
      (Code "int") ", " (Code "bool") ", "
      (Code "char") ", " (Code "string")
      "以及数组" (Code "t[]")
      " (对于每个类型" (Code "t")
      "都有一个数组类型" (Code "t[]")
      "). "
      )
   (H2. "Stacks and Queues")
   (H2. "Unbounded Arrays")
   (H2. "Hashing")
   (H2. "Hash Dictionaries")
   ))