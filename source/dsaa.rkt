#lang racket
(provide dsaa.html)
(require SMathML)
(define $log (Mi "log"))
(define &log
  (case-lambda
    ((n) (app $log n))
    ((a n) (app (_ $log a) n))))
(define (big-O x)
  (app $O x))
(define (\[\) a b)
  (: $lb0 a $cm b $rp0))
(define (\[\] a b)
  (: $lb0 a $cm b $rb0))
(define $lfloor (Mo "&lfloor;"))
(define $rfloor (Mo "&rfloor;"))
(define (&floor x)
  (: $lfloor x $rfloor))
(define dsaa.html
  (TnTmPrelude
   #:title "数据结构和算法笔记"
   #:css "styles.css"
   (H1 "数据结构和算法笔记")
   (P "我完全不懂数据结构和算法. 另外, 这里是一些关于数据结构和算法的笔记. "
      "当我认为我懂了之后, 或许会写一本书.")
   (H2 "什么是数据结构?")
   (P "以下内容翻译自Purely Functional Data Structures.")
   (P "任何对于数据结构的讨论都充斥着误解的潜在可能, 因为术语"
      (Em "数据结构") "至少具有四个不同但相互关联的含义."
      (Ul (Li (Em "一个抽象数据类型 (即一个类型和一集该类型上的函数). ")
              "我们将会称其为一个" (Em "抽象") ".")
          (Li (Em "一个抽象数据类型的一个具体实现. ")
              "我们将会称其为一个" (Em "实现")
              ", 但是注意一个实现未必要被具体化为代码&mdash;&mdash;"
              "一个具体的设计就足够了.")
          (Li (Em "一个数据类型的一个实例, 例如一个特定的列表或树. ")
              "我们将会称这样的一个实例为一个" (Em "对象") "或者一个"
              (Em "版本") ". 然而, 特定的数据类型可能有着它们自己的命名法. "
              "例如, 我们将会直接称栈或队列对象为栈或队列.")
          (Li (Em "一个在更新下保持不变的独特身份(identity). ")
              "例如, 在一个基于栈的解释器里, 我们经常非正式地说"
              (Q "栈 (the stack)") ", 就好像只存在一个栈似的, "
              "而不是不同的时间有着不同的版本. "
              "我们将会称这个身份为一个" (Em "可持久化身份")
              ". 这个事项主要出现在可持久化数据结构的上下文中; "
              "当我们言及相同数据结构的不同版本时, "
              "我们的意思是这些不同版本共享着一个相同的可持久化身份."))
      "大致说来, 抽象对应于Standard ML的签名, 实现对应于结构或函子, "
      "对象或版本对应于值. 至于可持久化身份, Standard ML中"
      "没有贴切的对应概念 (good analogue).")
   (H2 "IOI 1994 The Triangle")
   (P "算是简单的动态规划问题.")
   (CodeB "(define ref
  (case-lambda
    ((v i) (vector-ref v i))
    ((v i . j*) (apply ref (vector-ref v i) j*))))
(define (solve-IOI94P1 T)
  (define N (vector-length T))
  (define S (make-vector N))
  (let iter ((i 0))
    (cond
      ((= i N) (apply max (vector->list S)))
      (else
       (let iter ((j i))
         (cond
           ((= j 0)
            (vector-set! S 0 (+ (ref T i 0) (ref S 0))))
           (else
            (vector-set! S j (+ (ref T i j)
                                (max (ref S (- j 1)) (ref S j))))
            (iter (- j 1)))))
       (iter (+ i 1))))))")
   (CodeB "> (solve-IOI94P1
   '#(#(7)
      #(3 8)
      #(8 1 0)
      #(2 7 4 4)
      #(4 5 2 6 5)))
30")
   (P "题解网上很容易找到, 也没有必要再写什么. 注意"
      (Code "make-vector") "默认将每个元素置为" (Code "0") ".")
   (H2 "辅助过程")
   (CodeB "(define (vector-swap! v i j)
  (define t (vector-ref v i))
  (vector-set! v i (vector-ref v j))
  (vector-set! v j t))")
   (H2 "插入排序")
   (P "插入排序相当简单 (但实际上对于没有接触过的人而言又不那么简单), "
      "但也相当重要. 在数组长度比较短的时候, 插入排序往往是效率最高的.")
   (CodeB "(define (insertion-sort! v)
  (define l (vector-length v))
  (unless (&lt; l 2)
    (let iter ((i 1))
      (unless (= i l)
        (insert! v i)
        (iter (+ i 1))))))
(define (insert! v i)
  (define x (vector-ref v i))
  (let iter ((j (- i 1)))
    (cond
      ((= j -1) (vector-set! v 0 x))
      ((> (vector-ref v j) x)
       (vector-set!
        v (+ j 1) (vector-ref v j))
       (iter (- j 1)))
      (else
       (vector-set! v (+ j 1) x)))))")
   (P "插入排序的时间复杂度估计非常简单, 每次插入最坏的情况就是移动到开头, "
      "并且插入次数大致上是" $n ", 故上界为" (big-O $n^2) ".")
   (H2 "归并排序")
   (P "注意一下, " (Code "(merge-sort! v lo hi)")
      "排序的是" (Code "v") "从" (Code "lo") "到"
      (Code "hi") "的部分, 包含" (Code "lo")
      "但不包含" (Code "hi") ".")
   (CodeB "(define (merge-sort! v lo hi)
  (unless (&lt;= (- hi lo) 1)
    (define mi (quotient (+ lo hi) 2))
    (merge-sort! v lo mi)
    (merge-sort! v mi hi)
    (merge! v lo mi hi)))
(define (merge! v lo mi hi)
  (define l (vector-copy v lo mi))
  (define r (vector-copy v mi hi))
  (define a (- mi lo))
  (define b (- hi mi))
  (let iter ((k lo) (i 0) (j 0))
    (cond
      ((and (&lt; i a) (&lt; j b))
       (cond
         ((&lt; (vector-ref l i)
             (vector-ref r j))
          (vector-set! v k (vector-ref l i))
          (iter (+ k 1) (+ i 1) j))
         (else
          (vector-set! v k (vector-ref r j))
          (iter (+ k 1) i (+ j 1)))))
      ((= i a)
       (let iter ((k k) (j j))
         (unless (= k hi)
           (vector-set! v k (vector-ref r j))
           (iter (+ k 1) (+ j 1)))))
      ((= j b)
       (let iter ((k k) (i i))
         (unless (= k hi)
           (vector-set! v k (vector-ref l i))
           (iter (+ k 1) (+ i 1))))))))")
   (P "归并排序的时间复杂度估计不难理解, 大致上每个层次归并所用的时间是"
      (big-O $n) ", 而大约有" (&log $2 $n) "层, 故上界估计为"
      (big-O (&i* $n (&log $2 $n))) ".")
   (H2 "冒泡排序")
   (P "不知道为什么, 编写冒泡排序是一件很有压力的事情, 而实际上我的确也写了很长时间. "
      "在编写冒泡排序的时候, 我的脑子里的所思所想是这样的: "
      "后段已排序, 后段的每个元素都大于等于前段的所有元素, "
      "每轮交换可以将前段的最大元素移至最后的位置, "
      "如果在一轮中没有发生交换, 则说明整个数组是已排序好了的. "
      "当然, 也可以从逆序归约的角度思考问题.")
   (CodeB "(define (bubble-sort! v)
  (define l (vector-length v))
  (let loop ((i 1))
    (unless (>= i l)
      (let iter ((j 0) (flag #f))
        (cond
          ((= j (- l i))
           (when flag
             (loop (+ i 1))))
          ((> (vector-ref v j)
              (vector-ref v (+ j 1)))
           (vector-swap! v j (+ j 1))
           (iter (+ j 1) #t))
          (else
           (iter (+ j 1) flag)))))))")
   (H2 "快速排序")
   (P "这里的约定和归并排序那里一致. 主元的选取是任意的, 我只是选取了第一个元素.")
   (CodeB "(define (quick-sort! v lo hi)
  (unless (&lt;= (- hi lo) 1)
    (define p (partition! v lo hi))
    (quick-sort! v lo p)
    (quick-sort! v (+ p 1) hi)))
(define (partition! v lo hi)
  (define pivot (vector-ref v lo))
  (let iter ((left (+ lo 1)) (right hi))
    (cond ((= left right)
           (vector-swap! v lo (- left 1))
           (- left 1))
          ((> (vector-ref v left) pivot)
           (vector-swap! v left (- right 1))
           (iter left (- right 1)))
          (else
           (iter (+ left 1) right)))))")
   (P "根据原地 (in-place) 的定义的不同, 快速排序可以是原地的, "
      "也可以不是原地的, 因为它的确要消耗栈的空间.")
   (H2 "二分搜索")
   (P "据说二分搜索很少有人能够书写正确, 即便是算法研究者也不例外. "
      "这里仍然遵循着左闭右开的原则, 以后不再赘述.")
   (CodeB "(define (binary-search x v lo hi)
  (if (>= lo hi)
      #f
      (let ((mi (quotient (+ lo hi) 2)))
        (cond
          ((= (vector-ref v mi) x) mi)
          ((> (vector-ref v mi) x)
           (binary-search x v lo mi))
          (else
           (binary-search x v (+ mi 1) hi))))))")
   (H2 "杂凑 (哈希)")
   (H2 "3SUM问题")
   (P (Code "3SUM") "的前条件是" (Code "v")
      "已从小到达排序且元素互异.")
   (CodeB "(define (3SUM v)
  (define l (vector-length v))
  (let loop ((i 0) (result '()))
    (cond ((= i l) result)
          ((>= (vector-ref v i) 0) result)
          (else
           (let iter ((j (+ i 1)) (k (- l 1)) (r '()))
             (if (&lt; j k)
                 (let* ((a (vector-ref v i))
                        (b (vector-ref v j))
                        (c (vector-ref v k))
                        (s (+ a b c)))
                   (cond
                     ((= s 0) (iter (+ j 1)
                                    (- k 1)
                                    (cons (list a b c) r)))
                     ((&lt; s 0) (iter (+ j 1) k r))
                     (else (iter j (- k 1) r))))
                 (loop (+ i 1) (append r result))))))))")
   (H2 "parallel和concurrent的区别")
   (P "parallel指的是一种同时运行多个任务的实际能力, "
      "而concurrent是一种编程抽象, "
      "它维护了一种同时运行多个任务的illusion. "
      "concurrent的程序可能的确以parallel的方式运行, "
      "但也可能只是以顺序运行的方式来模拟parallelism.")
   (P "以上是一种实用的表面说法, 在某种意义上其实有点似是而非. "
      "concurrency的本质在于一种" (Q "不确定性")
      ", 我们无法控制事件发生的时间和顺序, "
      "但仍然企图获得对于这样的物理本性的一种(可复合的)控制. "
      "parallelism和concurrency不同的地方在于, "
      "它强调的同时运行的能力可以从一开始得到控制, "
      "而且我们从一开始的企图就是从中获得效率和性能. "
      "当然, 或许我需要说明的是, 并行不是自然或者天生就是可控的, "
      "而是我们希望去控制而可以控制, "
      "因而这里重要的事情是确定子计算的依赖关系.")
   (H2 "zipper")
   (P "zipper可以有多种理解, 但是从具体的数据结构来说, "
      "zipper可以理解为附加了" (Q "历史记录")
      "的各种各样的数据结构. 这个历史记录描述了我们是以何种方式进入相应的数据结构的. "
      "通过历史(和现在), 我们也可以还原本来数据结构的面貌.")
   (H2 "关于" $d "叉树的基本事实")
   (P "若以数组存储" $d "叉树 (且根结点的下标为" $1 "), 那么下标为"
      $k "的结点的子结点的下标范围是"
      (MB (\[\) (&+ (&i* $d (@- $k $1)) $2) (&+ (&i* $d $k) $2)) ",")
      "而下标为" $k " (其中" (&> $k $1) ") 的结点的父结点的下标为"
      (MB (&floor (~ (&+ $k (&- $d $2)) $d)) ".")
      "具有" $n "层的满" $d "叉树的元素总数为"
      (MB (&= (&+ $d^0 $d^1 $..c (^ $d (&- $n $1)))
              (~ (&- $d^n $1) (&- $d $1))) ".")
      "具有" (&+ $n $1) "层 (即高度为" $n ") 的完全" $d "叉树的元素数目范围是"
      (MB (\[\] (&+ (~ (&- $d^n $1) (&- $d $1)) $1)
                (~ (&- (^ $d (&+ $n $1)) $1) (&- $d $1)))
          "或者写成"
          (\[\) (~ (&+ $d^n (&- $d $2)) (&- $d $1))
                (~ (&+ (^ $d (&+ $n $1)) (&- $d $2)) (&- $d $1)))
          ".")
      "给这闭区间的左右范围乘上" (&- $d $1) ", 那么可以得到"
      (MB (&sube (\[\] (&+ $d^n (&- $d $2))
                       (&- (^ $d (&+ $n $1)) $1))
                 (\[\) $d^n (^ $d (&+ $n $1)))) ",")
      "这暗示了具有" $k "个元素的完全" $d "叉树的高度可以表示为"
      (MB (&floor (&log $d (&i* (@- $d $1) $k))) "."))
   
   ))