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
   
   ))