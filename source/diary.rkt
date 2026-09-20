#lang racket
(provide diary.html)
(require SMathML)
(define diary.html
  (TnTmPrelude
   #:title "无聊的日记"
   #:css "styles.css"
   (H1. "无聊的日记")
   (H2. "2026年7月28日")
   (P "今日回看之前写的生命游戏实现:"
      (CodeB "(define conway-kernel
  (&lt;&lt; (Array #(1 1 1 1 0 1 1 1 1))
      (rho2 3 3)))
(define (neighbor grid)
  (&lt;&lt; conway-kernel
      (Convolve2 grid '(1 1))))
(define (step grid)
  (Materialize2
   ((Zip-with
     (λ (x n)
       (if (= x 1)
           (if (or (= n 2) (= n 3)) 1 0)
           (if (= n 3) 1 0))))
    grid (neighbor grid))))")
      "参考APL的通常写法改了一个版本:"
      (CodeB "(define kernel
  (array '(3 3) (const 1)))
(define (sum9 grid)
  (&lt;&lt; kernel (Convolve2 grid '(1 1))))
(define (step^ grid)
  (Materialize2
   ((Zip-with
     (λ (x n)
       (if (or (= n 3)
               (and (= x 1) (= n 4)))
           1 0)))
    grid (sum9 grid))))")
      "根据测试, 新的版本比旧的版本用时平均长" (&/ $1 $8)
      ", 这个差距大概是由于原本的卷积核里有一个零, "
      "现在的卷积核里没有零而导致的. "
      "其他的细节几乎没有产生速度上的差异. "
      "不过, 就字数而言, 新版的确比旧版更少. "
      "然而, 新版也比旧版更难理解. "
      "大概新的版本只有热爱节约字数的APL人才会喜欢就是了.")
   (H2. "2026年8月27日")
   (P "昨日阅读和翻译Pfenning的构造性逻辑讲义翻译得头昏脑胀, "
      "不仅是因为其言辞精妙深微, "
      "更是因为反复出现的各种自然演绎的图形让我手忙脚乱. "
      "最后我实在忍无可忍, "
      "决心设计和实现一个DSL用于绘制自然演绎. "
      "这个(E)DSL基于Curry-Howard对应, "
      "所以也可以算是一个proof checker. "
      "不过, 它不能绘制原文那些中间步骤 "
      "(实际上或许也可以, 但是我有意没有实现), "
      "只是绘制最终的完整的自然演绎图形."
      (CodeB "(define (type-of var env)
  (cond ((assoc var env) => cdr)
        (else (error 'type-of &quot;unknown variable ~s&quot; var))))
(define (extend-env var type env)
  (cons (cons var type) env))
(define (reify t)
  (define (reify t)
    (match t
      ((-> ,t1 ,t2) (&amp;impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (&amp;conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (&amp;disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (define (@reify t)
    (match t
      ((-> ,t1 ,t2) (@impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (@conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (@disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (reify t))
(define (VAR u)
  (lambda (env)
    (define t (type-of u env))
    (cons t (assume u (&amp;true (reify t))))))
(define (CONS a b)
  (lambda (env)
    (define pa (a env))
    (define pb (b env))
    (define ta (car pa))
    (define tb (car pb))
    (define ca (cdr pa))
    (define cb (cdr pb))
    (define type `(conj ,ta ,tb))
    (cons type
          (&amp;rull $conjI ca cb
                 (&amp;true (reify type))))))
(define (CAR a)
  (lambda (env)
    (define pa (a env))
    (define ta (car pa))
    (define ca (cdr pa))
    (match ta
      ((conj ,t1 ,t2)
       (cons t1 (&amp;rull $conjE1 ca
                       (&amp;true (reify t1))))))))
(define (CDR a)
  (lambda (env)
    (define pa (a env))
    (define ta (car pa))
    (define ca (cdr pa))
    (match ta
      ((conj ,t1 ,t2)
       (cons t2 (&amp;rull $conjE2 ca
                       (&amp;true (reify t2))))))))
(define (LAM u t body)
  (lambda (env)
    (define pbody
      (body (extend-env u t env)))
    (define tbody (car pbody))
    (define cbody (cdr pbody))
    (define type `(-> ,t ,tbody))
    (cons type
          (&amp;rull (&amp;implI u) cbody
                 (&amp;true (reify type))))))
(define (APP a b)
  (lambda (env)
    (define pa (a env))
    (define pb (b env))
    (define ta (car pa))
    (define tb (car pb))
    (define ca (cdr pa))
    (define cb (cdr pb))
    (match ta
      ((-> ,t1 ,t2)
       (unless (equal? t1 tb)
         (error 'APP &quot;type mismatch&quot;))
       (cons t2 (&amp;rull $implE ca cb
                       (&amp;true (reify t2))))))))
(define (CASE a u1 b1 u2 b2)
  (lambda (env)
    (define pa (a env))
    (define ta (car pa))
    (define ca (cdr pa))
    (match ta
      ((disj ,t1 ,t2)
       (let* ((pb1 (b1 (extend-env u1 t1 env)))
              (pb2 (b2 (extend-env u2 t2 env)))
              (tb1 (car pb1))
              (tb2 (car pb2))
              (cb1  (cdr pb1))
              (cb2  (cdr pb2)))
         (unless (equal? tb1 tb2)
           (error 'CASE &quot;branch type mismatch&quot;))
         (cons tb1 (&amp;rull (&amp;disjE u1 u2) ca cb1 cb2
                          (&amp;true (reify tb1)))))))))
(define (INL tb a)
  (lambda (env)
    (define pa (a env))
    (define ta (car pa))
    (define ca (cdr pa))
    (define type `(disj ,ta ,tb))
    (cons type (&amp;rull $disjI1 ca
                      (&amp;true (reify type))))))
(define (INR ta b)
  (lambda (env)
    (define pb (b env))
    (define tb (car pb))
    (define cb (cdr pb))
    (define type `(disj ,ta ,tb))
    (cons type (&amp;rull $disjI2 cb
                      (&amp;true (reify type))))))
(define (ND proof)
  (cdr (proof '())))")
      "当然这里我有意省略了"
      "所有并不直接算是这个DSL的实现的次要代码, "
      "那些代码实际上都是用于绘制自然演绎的定义.")
   (H2. "2026年9月17日")
   (P "今日根据Mike Gordon的Hoare逻辑讲义"
      "写了一个最小的验证条件生成器."
      (CodeB "(define (subst Q E V)
  (cond ((pair? Q) (cons (subst (car Q) E V)
                         (subst (cdr Q) E V)))
        ((eq? Q V) E)
        (else Q)))
(define (:= V E)
  (case-lambda
    ((P Q) (list `(=> ,P ,(subst Q E V))))
    ((Q) (values (subst Q E V) '()))))
(define ((IF S C1 C2) P Q)
  (append (C1 `(and ,P ,S) Q)
          (C2 `(and ,P (not ,S)) Q)))
(define ((WHILE S R C) P Q)
  `((=> ,P ,R)
    (=> (and ,R (not ,S)) ,Q)
    . ,(C `(and ,R ,S) R)))
(define ((PRE R C) Q)
  (values R (C R Q)))
(define ((SEQ C . C*) P Q)
  (let iter ((RC* (reverse C*))
             (R Q) (VC '()))
    (if (null? RC*)
        (append (C P R) VC)
        (let-values (((R VC0) ((car RC*) R)))
          (iter (cdr RC*) R (append VC0 VC))))))
(define (Hoare P C Q) (C P Q))")
      "以下是一个经典的例子."
      (CodeB "(Hoare
 '(and (= X x) (= Y y))
 (SEQ (:= 'R 'X)
      (:= 'X 'Y)
      (:= 'Y 'R))
 '(and (= Y x) (= X y)))")
      "再写一点注记. "
      "我这里完全是按照经典Hoare逻辑来的. "
      "根据归纳可以证明, 如果这些验证条件是可证的, "
      "那么原本的带注解规约在Hoare逻辑下就是可证的. "
      "这里其实藏着很多细节. "
      "例如, 验证条件采用的是何种逻辑? "
      "其次, Hoare三元组的语义如何定义? "
      "虽然本质上来说我们这里考虑的是句法问题, "
      "但是实际上语义设计会反过来影响句法. "
      "这里的情况是有的语义选择可能会迫使"
      "Hoare逻辑 (句法概念) 本身变得更为复杂. "
      "这些思考相当微妙, 令人憔悴. "
      "不过, 逻辑学本身就是这样, "
      "无怪乎许多逻辑学家都疯了.")
   (H2. "2026年9月19日")
   (P "最近三个星期我开始工作, 这是我人生中的第一份工作. "
      "我感觉真的很糟糕, 但是我也学到了许多, 以下是我的感想:"
      (Ol (Li "编程语言和计算机科学中的理论往往有很多corner cases, "
              "最好要有心理准备. 但是, 不要独自一人苦思冥想, "
              "多看看前人的处理方法, 从中获取灵感或者教益.")
          (Li "如果你发现你写的程序或者理论或者形式化存在问题, "
              "无需太过慌张. 首先看看是可以修补的小问题, "
              "还是根本性的困难. 其次, "
              "最好也不要立即着手思考甚至是修改, "
              "而是先标记一下, 之后慢慢再想. "
              "如果是已经成熟的领域, "
              "最好像第1条说的那样参考一下前人的经验.")
          (Li "对于非确定性很强的工作, 或者是开放性工作, "
              "第一步要做的既不是学习, 也不是立即着手做事, "
              "而是应该花较长的时间思考自己究竟该做什么. "
              "另外, 当想不出来自己能做什么的时候, "
              "最好要做两件事情: "
              "一是和同事多沟通和交流, "
              "二是先尝试为自己设计一些简单的确定性任务做一下. "
              "第二点有可能也是陷阱, "
              "但是要义在于不要让自己陷入抑郁或者恐慌.")
          (Li "不要因为工作压力太大而暴饮暴食, 熬夜, 自慰, "
              "或者是放弃日常该做的基本事情. "
              "运动对于人类的基本健康而言也是必要的.")
          (Li "正确使用AI可以减少压力, "
              "但只是提需求什么也不管只会增大压力. "
              "并不存在所谓正确的AI使用方法.")
          (Li "与其依赖别人, 不如自己主动思考和解决问题. "
              "特别是长远项目, 依赖别人完全是错误的.")
          )
      )
   ))