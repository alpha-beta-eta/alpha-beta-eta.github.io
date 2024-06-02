#lang racket
(provide cmu.html)
(require SMathML)
(define cmu.html
  (Prelude
   #:title "CMU课程笔记"
   #:css "styles.css"
   (H1 "CMU课程笔记")
   (P "CMU的CS课程给我留下了深刻的印象, "
      "因为其中许多课程都强调从基本原理和逻辑学出发, "
      "最终呈现出自然优雅的趣味.")
   (H2 (A "CMU 15-122: 命令式编程原理" #:attr* '((href "cmu15122.html"))))
   (P "这门课是CMU为大一下学期的学生开设的, 目的在于教授C语言. "
      "它首先使用了一个叫做C0的语言, 在某种意义上它有些类似于Pascal, "
      "但是添加了表达前条件, 后条件, 不变量等的机制. "
      "这门课的主体内容是教授Hoare逻辑的基本, "
      "实现一系列基本的算法和数据结构, "
      "并使用Hoare逻辑证明代码的正确性.")
   (H2 (A "CMU 15-150: 函数式编程" #:attr* '((href "cmu15150.html"))))
   (P "这门课也是CMU为大一下学期学生开设的, "
      "目的在于教授Standard ML语言和函数式编程的基本. "
      "这也为之后用到Standard ML语言的CMU 15-210数据结构和算法课作准备.")
   (H2 (A "CMU 15-210: 并行数据结构和算法"
          #:attr* '((href "cmu15210.html"))))
   (P "这门课是Guy Blelloch所设计的, "
      "他的研究专注于并行编程语言的设计, 实现和分析. "
      "因此, 这虽然是入门课程, 但是并不容易. "
      "这门课的特色在于将顺序视为并行的特殊情况.")
   (H2 (A "CMU 15-317: 构造性逻辑"
          #:attr* '((href "constructive_logic_notes.html"))))
   (P "这是Frank Pfenning开设的本科生逻辑学基础课程, "
      "强调其与编程语言的联系.这门课的内容与Pfenning本人的研究强相关. ")
   (H2 (A "CMU 15-836: 亚结构逻辑"
          #:attr* '((href "substructural.html"))))
   (P "这是Frank Pfenning开设的研究生的亚结构逻辑课程, "
      "其承接了CMU 15-317, 内容相当丰富. 这门课的前身是"
      (A "CMU 15-816: 线性逻辑"
         #:attr* '((href "linear_logic_notes.html")))
      ", 而线性逻辑是亚结构逻辑的一种.")
   
   ))