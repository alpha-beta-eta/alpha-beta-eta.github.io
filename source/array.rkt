#lang racket
(provide array.html)
(require SMathML)
(define array.html
  (TnTmPrelude
   #:title "关于数组语言的随想"
   #:css "styles.css"
   (H1. "关于数组语言的随想")
   (H2. "一个数组语言 (和一些图像处理)")
   (P "首先要说明的是, 这里的Racket代码来源于对于以下链接中所包含的内容中的OCaml代码的忠实翻译. "
      "不过, 当然我做了许多符合Racket语言习惯的调整. "
      "比如说, OCaml中的多维数组的索引函数接受的是tuple, "
      "而在Racket之中, 索引函数则接受多个参数."
      (Blockquote
       (A "Image Processing Playground with Array Languages"
          #:attr* '((href "https://okmij.org/ftp/image/ArrayL/")))))
   (CodeB "(struct array (shape indexf) #:transparent)
(define (rho A) (array-shape A))
(define (get A) (array-indexf A))
(define (fork f g h)
  (λ x (f (apply g x) (apply h x))))
(define >>
  (case-lambda
    ((f g) (compose g f))
    ((f g . h*) (apply >> (compose g f) h*))))
(define &lt;&lt;
  (case-lambda
    ((x f) (f x))
    ((x f . g*) (apply &lt;&lt; (f x) g*))))
(define ((zip-with op) A B)
  (match-define (array shapeA indexfA) A)
  (match-define (array shapeB indexfB) B)
  (unless (equal? shapeA shapeB)
    (error 'zip-with &quot;different shapes: ~s, ~s&quot;
           shapeA shapeB))
  (array shapeA (fork op indexfA indexfB)))
(define ((Map f) A)
  (match A
    ((array shape indexf)
     (array shape (compose f indexf)))))
(define ((Mapi f) A)
  (match A
    ((array shape indexf)
     (array shape
            (λ indices
              (apply f (apply indexf indices) indices))))))
(define ((Reduce f) A)
  (match A
    ((array i indexf)
     (unless (and (integer? i) (> i 0))
       (error 'Reduce &quot;bad shape: ~s&quot; i))
     (let iter ((x 1) (a (indexf 0)))
       (if (= x i)
           a
           (iter (+ x 1) (f a (indexf x))))))))
(define ((row A) i)
  (match A
    ((array (list _ j) indexf)
     (array j (curry indexf i)))))
(define (rows A)
  (match A
    ((array (list i _) _)
     (array i (row A)))))
(define (Reduce2 f)
  (>> rows
      (Map (Reduce f))
      (Reduce f)))
(define ((rho2 i j) A)
  (match A
    ((array k indexf)
     (unless (and (integer? k)
                  (= (* i j) k))
       (error 'rho2 &quot;shape mismatch: ~s, ~s, ~s&quot; i j k))
     (array (list i j)
            (λ (x y)
              (indexf (+ (* x j) y)))))))
(define (materialize1 A)
  (match A
    ((array i indexf)
     (define vec (build-vector i indexf))
     (array i (curry vector-ref vec)))))
(define (build-matrix i j p)
  (build-vector
   i (λ (i)
       (build-vector
        j (λ (j) (p i j))))))
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))
(define (materialize2 A)
  (match A
    ((array (list i j) indexf)
     (define mat (build-matrix i j indexf))
     (array (list i j) (curry matrix-ref mat)))))
(define (of-array vec)
  (array (vector-length vec)
         (curry vector-ref vec)))
(define (iota n)
  (array n identity))
(define ((for-each1 f) A)
  (match A
    ((array i indexf)
     (let iter ((x 0))
       (unless (= x i)
         (f (indexf x))
         (iter (+ x 1)))))))
(define (for-each2 f)
  (>> rows
      (for-each1
       (for-each1 f))))
(define print-2d
  (>> rows
      (for-each1
       (λ (r)
         (&lt;&lt; r (for-each1
                (curry printf &quot;~s &quot;)))
         (newline)))))
(define ((repeated n) f)
  (if (zero? n)
      identity
      (compose f ((repeated (sub1 n)) f))))
(define (zero-array A)
  (array (rho A) (const 0)))
(define (transpose A)
  (match A
    ((array (list i j) indexf)
     (array (list j i)
            (λ (j i) (indexf i j))))))
(define (duplicate f)
  (λ (x) (f x x)))
(define minmax2
  (>> (Map (duplicate cons))
      (Reduce2
       (match-lambda*
         ((list (cons a b) (cons c d))
          (cons (min a c) (max b d)))))))
(define ((normalize bound) A)
  (match-define (cons min max) (minmax2 A))
  (define range (- max min))
  (if (zero? range)
      (zero-array A)
      (&lt;&lt; A (Map (λ (x)
                   (quotient
                    (* (- x min) bound)
                    range))))))
(struct color (R G B) #:transparent)
(define colors 256)
(define color-max (sub1 colors))
(define cmap:reds
  (array colors (λ (i) (color i 0 0))))
(define cmap:blues
  (array colors (λ (i) (color 0 0 i))))
(define cmap:whites
  (array colors (λ (i) (color i i i))))
(define cmap1
  (array colors
         (λ (i)
           (color
            (min color-max (* i 2))
            i
            (min color-max (* i 2))))))
(define (write-color c)
  (printf &quot;~s ~s ~s\\n&quot;
          (color-R c)
          (color-G c)
          (color-B c)))
(define (PPM cmap A)
  (match A
    ((array (list i j) _)
     (printf &quot;P3\\n~s ~s\\n~s\\n&quot; j i color-max)
     (&lt;&lt; A
         (normalize color-max)
         (Map (get (materialize1 cmap)))
         (for-each2 write-color)))))
(define (emitPPM cmap A path)
  (with-output-to-file path
    (λ () (PPM cmap A))
    #:exists 'replace))
(define (rand)
  (- (random 7) 3))
(define (noise A)
  (match A
    ((array shape _)
     (array shape
            (λ (i j)
              (if (and (even? i) (even? j))
                  0
                  (rand)))))))
(define ((mul a) b)
  (exact-round (* a b)))
(define (expander scaler nsf)
  (>> (Map (mul nsf))
      scaler
      (fork (zip-with +) identity noise)
      materialize2))
(define (scale-twice A)
  (match A
    ((array (list i j) indexf)
     (array (list (* i 2) (* j 2))
            (λ (i j)
              (indexf (quotient i 2)
                      (quotient j 2)))))))
(define (list- l1 l2)
  (map - l1 l2))
(define (restrict x low high)
  (min (max low x) high))
(define (clamp ref shape)
  (map (lambda (x i)
         (restrict x 0 (- i 1)))
       ref shape))
(define ((shift2 s) A)
  (match A
    ((array shape indexf)
     (array shape
            (λ indices
              (apply indexf
                     (clamp (list- indices s)
                            shape)))))))
(define (convolve2 A center)
  (>> (Mapi
       (λ (n . indices)
         (if (zero? n)
             (zero-array A)
             (&lt;&lt; A (Map (curry * n))
                 (shift2 (list- center indices))))))
      (Reduce2 (zip-with +))))
(define (interleave2 #:orig orig
                     #:vert vert
                     #:horz horz
                     #:diag diag)
  (define origf (get orig))
  (define vertf (get vert))
  (define horzf (get horz))
  (define diagf (get diag))
  (match orig
    ((array (list i j) _)
     (array (list (* i 2) (* j 2))
            (λ (i j)
              (define a (quotient i 2))
              (define b (quotient j 2))
              (if (even? i)
                  (if (even? j)
                      (origf a b)
                      (horzf a b))
                  (if (even? j)
                      (vertf a b)
                      (diagf a b))))))))
(define (scale-twice-bilinear orig)
  (define vert
    (&lt;&lt; (array '(2 1) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 2))))
  (define horz
    (&lt;&lt; (array '(1 2) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 2))))
  (define diag
    (&lt;&lt; (array '(2 2) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 4))))
  (interleave2
   #:orig orig
   #:vert vert
   #:horz horz
   #:diag diag))
(define bicubic-kernel
  (of-array #(-1 9 9 -1)))
(define (⊗ A B)
  (match-define (array i fA) A)
  (match-define (array j fB) B)
  (array (list i j)
         (λ (i j)
           (* (fA i) (fB j)))))
(define bicubic-bikernel
  (⊗ bicubic-kernel bicubic-kernel))
(define (scale-twice-bc orig)
  (define vert
    (&lt;&lt; bicubic-kernel
        (rho2 4 1)
        (convolve2 orig '(1 0))
        (Map (curryr quotient 16))))
  (define horz
    (&lt;&lt; bicubic-kernel
        (rho2 1 4)
        (convolve2 orig '(0 1))
        (Map (curryr quotient 16))))
  (define diag
    (&lt;&lt; bicubic-bikernel
        (convolve2 orig '(1 1))
        (Map (curryr quotient 256))))
  (interleave2
   #:orig orig
   #:vert vert
   #:horz horz
   #:diag diag))
(define (scale-twice-sd orig)
  (define diamond
    (&lt;&lt; (of-array #(1 2 1 1 2 1))
        (rho2 2 3)))
  (define vert
    (&lt;&lt; diamond
        (convolve2 orig '(0 1))
        (Map (curryr quotient 8))))
  (define horz
    (&lt;&lt; (transpose diamond)
        (convolve2 orig '(1 0))
        (Map (curryr quotient 8))))
  (define diag
    (&lt;&lt; (array '(2 2) (const 1))
        (convolve2 orig '(0 0))
        (Map (curryr quotient 4))))
  (interleave2
   #:orig orig
   #:vert vert
   #:horz horz
   #:diag diag))")
   (H2. "一些奇怪的随想")
   (H3. "APL的风格, OCaml的风格, Racket的风格")
   (P "这个列表语言在精神上与APL达成了某种微妙的一致性. "
      "也就是说, 它偏好一种point-free的组合子风格编程方式. "
      
      )
   (H3. "更为一般的数组处理?")
   (P "OCaml代码的数组原语模仿的是类型化函数式语言的列表处理. "
      "尽管这种列表处理来源于Lisp, "
      "但是有时的确不如Lisp系语言一般. "
      "例如, Scheme/Racket里的" (Code "map")
      "可以接受任意多个列表, 那么" (Code "zip-with")
      "就是没有必要的了. 实际上, 对于这个数组语言, "
      "实现Lisp式的一般map完全可行. "
      "并且, 似乎其他原语也可以得到泛化?")
   (CodeB "(define (fork f . g*)
  (λ x (apply f (map (λ (g) (apply g x)) g*))))
(define ((Map f) A . A*)
  (array (rho A)
         (apply fork f (get A) (map get A*))))")
   (P "例如, 这个" (Code "Map")
      "就遵循了Lisp风格, 可以处理任意多个数组, 从而"
      (Code "zip-with") "就变成了一种特殊情况. "
      "当然了, 因为验证形状相同比较麻烦, 这里就省略了.")
   (CodeB "(define ((row A) i)
  (match A
    ((array (cons _ j) indexf)
     (array j (curry indexf i)))))")
   (P "这个版本的" (Code "row")
      "变成了一般的降维函数, "
      "但是这个定义也要求我们改变形状的表示. "
      "具体来说, 以后一维数组的形状也应该是列表了. "
      "并且, 为了一致性, 最好引入零维数组, "
      "其在概念上相当于标量. "
      "当然了, 还有另一种选择, "
      "就是使用improper list, "
      "那么一维数组的形状又可以使用一个数字来表示了, "
      "这种选择比较实际.")
   (CodeB "(define (rows A)
  (match A
    ((array (cons i _) _)
     (array (list i) (row A)))))")
   (CodeB "(define (rows A)
  (match A
    ((array (cons i _) _)
     (array i (row A)))))")
   (P "不同的形状表示会导致不同的" (Code "rows")
      "定义. 之后我想采用更为实际的表示.")
   (CodeB "(define ((Reduce1 f) A)
  (match A
    ((array i indexf)
     (unless (and (integer? i) (> i 0))
       (error 'Reduce &quot;bad shape: ~s&quot; i))
     (let iter ((x 1) (a (indexf 0)))
       (if (= x i)
           a
           (iter (+ x 1) (f a (indexf x))))))))
(define ((Reduce f) A)
  (match A
    ((array (cons _ _) _)
     (&lt;&lt; (rows A)
         (Map (Reduce f))
         (Reduce1 f)))
    (_ ((Reduce1 f) A))))")
   (P "这里的" (Code "Reduce1")
      "可以处理一维数组, " (Code "Reduce")
      "则可以处理任意多维数组. "
      "老实说, 写到这里我开始畏怯了, "
      "因为我感到我需要先找到"
      "rank polymorphism的组织原则.")
   
   (H3. "代价评估和效率问题")
   (P "数组语言本质上来说是惰性的, "
      "或者说是按需计算的. "
      "(原谅我使用这个词, 因为我知道它的确也有特定的含义.) "
      "一个典型的想法自然的浮现了, "
      "那就是如果要对于某一处的值进行多次计算, "
      "那么这反而是低效的. 所以说, "
      "Oleg Kiselyov等人的代码里有许多materialize函数的身影. "
      "call-by-name和call-by-need已经是很久以前就出现的古老概念了, "
      "所以说或许我们应该要提供某种自动备忘化机制?")
   ))