#lang racket
(provide cmu15122.html)
(require SMathML)
(define cmu15122.html
  (TmPrelude
   #:title "命令式编程原理笔记"
   #:css "styles.css"
   (H1 "命令式编程原理笔记")
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
   (H2 "Lecture 2: Ints")
   (H2 "Lecture 3: Arrays")
   (H2 "Lecture 4: Searching Arrays")
   (H2 "Lecture 5: Big-" $O)
   (H2 "Lecture 6: Binary Search")
   (H2 "Lecture 7: Quicksort")
   (H2 "Lecture 8: Libraries")
   (H2 "Lecture 9: Stacks and Queues")
   (H2 "Lecture 11: Unbounded Arrays")
   (H2 "Lecture 12: Hashing")
   (H2 "Lecture 13: Hash Dictionaries")
   ))