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
   (H2 "堆排序")
   (P "堆排序分为两个阶段, 第一阶段自底而上建最大堆, "
      "第二阶段不断地将最大元素和未排序部分的最后一个元素交换 "
      "(相当于取出最大元素放在堆的后面), "
      "并仍然需要维护堆的性质.")
   (P "自底而上建堆可以被称为Floyd的技巧, "
      "它比起逐个将数组元素加入堆中具有显然的性能优势. "
      "当然了, 另外一个可以被称为Floyd技巧的东西出现在"
      "取出堆的优先元素时, 其需要将堆的最后元素放到堆顶, "
      "然后通过下沉维护堆的性质. "
      "既然最后的元素可以想见优先级非常低, "
      "所以说一般而言其会沉到相当深的位置. "
      "Floyd的想法是, 不如先将其沉到底部, 然后再上浮, "
      "如此可以减少比较次数. 若比较代价很大, 那么性能提升会比较明显. "
      "不过, 我没有在这里的堆排序里实现后一个Floyd技巧.")
   (CodeB "(define (left-child lo k)
  (- (+ (* 2 k) 1) lo))
(define (heap-sort! v lo hi)
  (unless (&lt;= (- hi lo) 1)
    (let iter ((i (quotient (+ lo hi -1) 2)))
      (unless (&lt; i lo)
        (sink! v lo hi i)
        (iter (- i 1))))
    (let iter ((i (- hi 1)))
      (unless (&lt; i lo)
        (vector-swap! v lo i)
        (sink! v lo i lo)
        (iter (- i 1))))))
(define (sink! v lo hi i)
  (let iter ((i i))
    (define l (left-child lo i))
    (unless (>= l hi)
      (define j
        (cond ((>= (+ l 1) hi) l)
              ((&lt; (vector-ref v l)
                  (vector-ref v (+ l 1)))
               (+ l 1))
              (else l)))
      (unless (>= (vector-ref v i)
                  (vector-ref v j))
        (vector-swap! v i j)
        (iter j)))))")
   (P "自底而上建堆时, 无需从最后一个元素开始, "
      "因为它们都是叶子, 第一个不是叶子的位置是"
      (Code "(quotient (+ lo hi -1) 2)") ".")
   (P "对于" (Code "(sink! v lo hi i)")
      "而言, " (Code "lo")
      "主要是为了确定后继的位置, 而" (Code "hi")
      "当然是为了划定堆的范围.")
   (H2 "minimax")
   (P "对于两人回合制完美信息零和游戏而言, "
      "形容minimax的最简明扼要的语言大概是"
      (Em "事后诸葛")
      ". 假设现在我们拥有一个终局评估函数, "
      "其给出了一局游戏的结果, 以数值表示. "
      "例如, 对于" $A "和" $B
      "参与的一局游戏, 或许" $0
      "表示" $B "赢, " $1 "表示" $A
      "赢 (假设这种游戏没有平局或者其他终局情况). "
      "那么, 以正常人的角度来看, " $A
      "肯定是希望最大化终局函数的值, "
      $B "肯定是希望最小化终局函数的值. "
      "如果我们扩展整个游戏树, 包含所有可能的对局情况, "
      "那么倒着看游戏, 就很容易理解" $A
      "或者" $B "该采取什么样的策略. "
      "也就是说, 它们都应该采取对于自己最有利的策略. "
      "再换句话说, 假设每个人都下最优的步骤且"
      "当前局面下所有步骤的未来结果已知 (也就是一个终局函数的值), "
      "那么" $A "只需要选择最大的一步就行, 而"
      $B "则需要选择最小的一步. 以这种方式, "
      "我们可以自后往前地构建出对于" $A
      "和" $B "而言的任何局面下的最佳步骤.")
   (H2 "红黑树")
   (P "以下是关于红黑树的高度和其元素数目之间关系的实验, "
      "代码可能写得很糟糕, 但是不用在意这种细节问题.")
   (CodeB "#lang racket
(require &quot;match.rkt&quot;)
;&lt;rbt> ::= E
;       |  (R &lt;rbt> &lt;val> &lt;rbt>)
;       |  (B &lt;rbt> &lt;val> &lt;rbt>)
(define E 'E)
(define (R a x b) `(R ,a ,x ,b))
(define (B a x b) `(B ,a ,x ,b))
(define (balance s)
  (match s
    ((B (R (R ,a ,x ,b) ,y ,c) ,z ,d)
     (R (B a x b) y (B c z d)))
    ((B (R ,a ,x (R ,b ,y ,c)) ,z ,d)
     (R (B a x b) y (B c z d)))
    ((B ,a ,x (R (R ,b ,y ,c) ,z ,d))
     (R (B a x b) y (B c z d)))
    ((B ,a ,x (R ,b ,y (R ,c ,z ,d)))
     (R (B a x b) y (B c z d)))
    (,else s)))
(define (blacken s)
  (match s
    ((R ,a ,x ,b) (B a x b))
    (,else s)))
(define (insert x s)
  (define (ins s)
    (match s
      ((,c ,a ,y ,b)
       (cond ((&lt; x y)
              (balance `(,c ,(ins a) ,y ,b)))
             ((= x y) s)
             ((> x y)
              (balance `(,c ,a ,y ,(ins b))))))
      (,else (R E x E))))
  (blacken (ins s)))
(define (insert-random s)
  (insert (random) s))
(define (height s)
  (match s
    ((,c ,a ,x ,b)
     (add1 (max (height a) (height b))))
    (,else 0)))
(define (repeated f n)
  (cond ((= n 0) identity)
        ((= n 1) f)
        (else (compose f (repeated f (- n 1))))))
(define (experiment-average thunk n)
  (let iter ((rest n) (sum 0))
    (if (= rest 0)
        (/ sum n)
        (iter (- rest 1) (+ sum (thunk))))))
(define (ex-on-n n)
  (let ((f (repeated insert-random n)))
    (experiment-average
     (lambda ()
       (height (f E)))
     1000.0)))
(define (enum a b)
  (if (> a b)
      '()
      (cons a (enum (+ a 1) b))))
(map (lambda (n)
       (list n (ex-on-n n)))
     (enum 7 63))")
   (P "实验结果:")
   (CodeB "'((7 3.648)
  (8 4.0)
  (9 4.0)
  (10 4.158)
  (11 4.419)
  (12 4.645)
  (13 4.811)
  (14 4.93)
  (15 5.04)
  (16 5.119)
  (17 5.202)
  (18 5.293)
  (19 5.398)
  (20 5.515)
  (21 5.581)
  (22 5.714)
  (23 5.819)
  (24 5.899)
  (25 5.975)
  (26 6.042)
  (27 6.111)
  (28 6.177)
  (29 6.228)
  (30 6.294)
  (31 6.368)
  (32 6.468)
  (33 6.517)
  (34 6.614)
  (35 6.661)
  (36 6.675)
  (37 6.751)
  (38 6.824)
  (39 6.87)
  (40 6.896)
  (41 6.949)
  (42 6.999)
  (43 7.031)
  (44 7.076)
  (45 7.104)
  (46 7.151)
  (47 7.187)
  (48 7.22)
  (49 7.246)
  (50 7.273)
  (51 7.299)
  (52 7.368)
  (53 7.361)
  (54 7.42)
  (55 7.457)
  (56 7.46)
  (57 7.512)
  (58 7.543)
  (59 7.617)
  (60 7.648)
  (61 7.683)
  (62 7.719)
  (63 7.776))")
   (H2 "(算法性)图论")
   (P "图论是一个很大的主题, 算法性的图论也是一个很大的主题. "
      "我几乎完全不懂图论, 但是又经常被偶尔用到的图论里的微妙定义折磨. "
      "据我观察, 很少有涉及图论的书能给出完全正确(且一致)的定义.")
   (H3 "第1节 无向图")
   (P "一个图" $G "是由一个顶点的集合" $V "和一个边的集合"
      $E "构成的. 对于每条边" (∈ $e $E)
      ", 都存在两个(相互之间没有顺序的)顶点" (&cm $x $y)
      "与之对应. 从直觉上说, 这条边" $e "连接了"
      $x "和" $y ". 我们并不排除" (&= $x $y)
      "的情况, 此时" $e "被称为一个自环 (loop). "
      "一个顶点是孤立的, 如果不存在能够连接到该顶点的边. "
      "一个图是简单的, 如果它没有自环, "
      "也没有两条不同但是却连接了相同顶点的边. "
      "两条非自环的不同边如果连接了相同的顶点, "
      "那么我们就称这两条边是平行的. "
      "非简单的图有时被称为多图 (multigraph). "
      "如果一个图的顶点集合和边集合均为有限的, "
      "那么我们就称这个图是有限的. "
      "基本上我们总是关心有限的图而已.")
   (P "顶点之间的连通关系是由边导出的连接关系所生成的等价关系. "
      "连通关系下的等价类被称为连通分量. "
      )
   ))