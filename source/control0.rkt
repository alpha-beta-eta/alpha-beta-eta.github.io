#lang racket
(provide control0.html)
(require SMathML)
(define (∃ x p)
  (: $exists x $. p))
(define (∀ x p)
  (: $forall x $. p))
(define (Trd . x*)
  (apply Tr (map Td x*)))
(define (domain h) (ap $D h))
(define $dummy (Mi "_"))
(define pointsTo &\|->)
(define $emp (Mi "emp"))
(define (heapEmpty h)
  (&= (domain h) $empty))
(define (embed P) (ang0 P))
(define $wand (Mo "&minus;&#8270;"))
(define (disjoint h1 h2)
  (&= (&cap (domain h1) (domain h2)) $empty))
(define-infix*
  (&wand $wand))
(define control0.html
  (TnTmPrelude
   #:title "编程语言中的控制结构"
   #:css "styles.css"
   (H1. "编程语言中的控制结构: 从goto到代数效应")
   (H2 "术语表")
   (Table
    #:attr* '((align "center"))
    (Trd "effect" "效应")
    (Trd "effect handler" "效应处理器")
    (Trd "algebraic effect" "代数效应")
    (Trd "effect system" "效应系统")
    (Trd "continuation" "延续")
    (Trd "monad" "单子")
    (Trd "implication" "后承 (翻译成蕴涵或推出都不太合理)")
    (Trd "alias" "别名共享"))
   (H2. "早期编程语言")
   (H2. "结构化编程")
   (H2. "非局部控制")
   (H2. "控制反转")
   (H2. "函数式语言")
   (H2. "延续和CPS变换")
   (H2. "延续编程")
   (H2. "控制算子")
   (H2. "异常")
   (H2. "用户定义效应的效应处理器")
   (H2. "单子")
   (H2. "代数效应")
   (H2. "类型和效应系统")
   (H2. "控制结构的Hoare逻辑")
   (H2. "控制算子的分离逻辑")
   (H3. "分离逻辑备忘")
   (P "分离逻辑由Reynolds (2002) 引入, "
      "作为Hoare逻辑的扩展, "
      "其对于使用指针和动态内存分配的程序有着更好的支持. "
      "这样的程序可能会碰到一些难于"
      "单独使用平直Hoare逻辑排除的问题 (issue). "
      "例如, 别名 (指向相同块的不同指针变量) "
      "可能会导致藉由某个指针变量的写影响藉由另外指针变量的读. "
      "人们可能会忘了释放一个不再需要的动态分配的块, "
      "这会导致内存泄露. 人们可能会在某个块释放之后访问, "
      "这会导致未定义行为. "
      "除了内存块之外, 诸多类似的" (Em "资源")
      "都会导致类似的问题, 例如系统资源 (文件描述符, 等等), "
      "security capabilities, 甚至是某些类型的延续, "
      "这我们会在本章之后看到.")
   (P "在Hoare逻辑之中, 断言具有真值并描述了程序的可变变量的当前状态. "
      "在分离逻辑之中, 断言既有真值, 也有一个" (Em "足迹")
      ", 这是一集资源, 既描述了这些资源的当前状态, "
      "也描述了其由该断言唯一拥有的事实. "
      "这就使得定义资源敏感 (resource-aware) 逻辑联结词成为可能, "
      "例如分离合取" (&* $A $B) ", 其成立若" $A "和" $B
      "断言均成立, 且它们的足迹互不相交. 因此, 如果由"
      $A "拥有的资源被修改了, " $B "仍然保持有效, 因为"
      $B "所拥有的资源仍然未被修改.")
   (P "在资源是动态分配内存块的情形下, "
      "分离逻辑的断言是内存堆上的谓词. "
      "以下是常常使用的断言, 以及它们作为堆"
      $h "上的谓词的形式化定义. 对于" $h
      "的domain, 我们记" (domain $h)
      ", 即" $h "中的有效位置的集合."
      (MB (set-attr*
           (&Table
            ((pointsTo $l $dummy) (: "位置" $l "是有效的"))
            ($ (&= (domain $h) (setE $l)))
            ((pointsTo $l $v) (: "位置" $l "含有" $v))
            ($ (&conj (&= (domain $h) (setE $l))
                      (&= (app $h $l) $v)))
            ($emp "堆为空")
            ($ (heapEmpty $h))
            ((embed $P) (: "逻辑命题" $P "成立且堆为空"))
            ($ (&conj (heapEmpty $h) $P))
            ((&* $A $B) "分离合取")
            ($ (∃ (&cm $h_1 $h_2)
                  (&conj (disjoint $h_1 $h_2)
                         (&= $h (&cup $h_1 $h_2))
                         (app $A $h_1)
                         (app $B $h_2))))
            ((&wand $A $B) "分离后承, 或称魔杖")
            ($ (∀ $h^ (&=> (&conj (disjoint $h $h^)
                                  (app $A $h^))
                           (app $B (&cup $h $h^))))))
           'columnalign "left"
           'columnlines "solid"
           'rowlines "solid"
           'frame "solid")))
   (P "例如, 考虑断言"
      (MB (∃ (&cm $n_1 $n_2)
             (&* (pointsTo $l_1 $n_1)
                 (pointsTo $l_2 $n_2)
                 (embed (&= (&+ $n_1 $n_2) 10)))))
      "其陈述了" $l_1 "和" $l_2 "是合法的位置, "
      "其分别含有整数" $n_1 "和" $n_2
      ", 而它们之和为" 10
      ". 因为该断言使用了分离合取, 所以这也保证了"
      (&!= $l_1 $l_2) ", 即指针" $l_1 "和" $l_2
      "之间没有别名共享.")
   (P "因此, 如果我们对于" $l_1 "的内容进行增量, "
      "我们可以安全地断言"
      (MB (∃ (&cm $n_1 $n_2)
             (&* (pointsTo $l_1 $n_1)
                 (pointsTo $l_2 $n_2)
                 (embed (&= (&+ $n_1 $n_2) 11)))))
      "如果没有" (&!= $l_1 $l_2) "这一保证, "
      "那么这个结论就是无效的了; "
      "若这两个指针别名共享 (alias), 对于"
      $l_1 "的内容进行增量也会对于" $l_2
      "的内容进行增量, 导致和为" 12 ".")
   (H3. "一个函数式命令式语言的一种分离逻辑")
   (P (B "FUNREF语言. ")
      "图15.1展示了FUNREF的抽象句法, 这是本章所采用的玩具语言. "
      "FUNREF是一个函数式语言, 其也通过可以就地修改的引用而支持命令式编程, "
      "这类似于OCaml以及其他ML族的语言. "
      "引用藉由四个操作呈现:"
      (Ul (Li (Code "alloc") "创建一个新鲜的未初始化的引用;")
          (Li (Code "x := v") "将值" $v "存储于引用" $x ";")
          (Li "")
          )
      )
   (H2 "参考文献")
   ))