#lang racket
(provide picture.html)
(require SMathML "painter3d.rkt")
(define picture.html
  (TmPrelude
   #:title "摆弄SICP图形语言"
   #:css "styles.css"
   (H1 "摆弄SICP图形语言")
   (P "一日闲着无聊, 想着是否能将SICP中所描述的图形语言改造成三维的, "
      "以下是草就而成的程序.")
   (CodeB "#lang racket
(require SMathML)
(define-struct vec3
  (x y z)
  #:transparent)
(define-struct vec2
  (x y)
  #:transparent)
(define-struct pt3
  (x y z)
  #:transparent)
(define-struct pt2
  (x y)
  #:transparent)
(define-struct frame
  (o x y z)
  #:transparent)
(define (build-frame ox oy oz
                     xx xy xz
                     yx yy yz
                     zx zy zz)
  (make-frame
   (make-pt3 ox oy oz)
   (make-vec3 xx xy xz)
   (make-vec3 yx yy yz)
   (make-vec3 zx zy zz)))
(define vec3+
  (case-lambda
    ((v1 v2)
     (make-vec3
      (+ (vec3-x v1) (vec3-x v2))
      (+ (vec3-y v1) (vec3-y v2))
      (+ (vec3-z v1) (vec3-z v2))))
    ((v1 v2 . v*)
     (apply vec3+ (vec3+ v1 v2) v*))))
(define (vec3* k v)
  (make-vec3
   (* k (vec3-x v))
   (* k (vec3-y v))
   (* k (vec3-z v))))
(define (pt3+ p v)
  (make-pt3
   (+ (pt3-x p) (vec3-x v))
   (+ (pt3-y p) (vec3-y v))
   (+ (pt3-z p) (vec3-z v))))
(define (pt2+ p v)
  (make-pt2
   (+ (pt2-x p) (vec2-x v))
   (+ (pt2-y p) (vec2-y v))))
(define (pt3- p1 p2)
  (make-vec3
   (- (pt3-x p1) (pt3-x p2))
   (- (pt3-y p1) (pt3-y p2))
   (- (pt3-z p1) (pt3-z p2))))
(define ((cmap f) p)
  (pt3+ (frame-o f)
        (vec3+ (vec3* (pt3-x p) (frame-x f))
               (vec3* (pt3-y p) (frame-y f))
               (vec3* (pt3-z p) (frame-z f)))))
(define (painterT f)
  (define o (frame-o f))
  (define x (frame-x f))
  (define y (frame-y f))
  (define z (frame-z f))
  (define ox (pt3-x o))
  (define oy (pt3-y o))
  (define oz (pt3-z o))
  (define xx (vec3-x x))
  (define xy (vec3-y x))
  (define xz (vec3-z x))
  (define yx (vec3-x y))
  (define yy (vec3-y y))
  (define yz (vec3-z y))
  (define zx (vec3-x z))
  (define zy (vec3-y z))
  (define zz (vec3-z z))
  (lambda (painter)
    (lambda (frame)
      (define o0 (frame-o frame))
      (define x0 (frame-x frame))
      (define y0 (frame-y frame))
      (define z0 (frame-z frame))
      (define (lc a b c)
        (vec3+ (vec3* a x0)
               (vec3* b y0)
               (vec3* c z0)))
      (painter
       (make-frame
        (pt3+ o0 (lc ox oy oz))
        (lc xx xy xz)
        (lc yx yy yz)
        (lc zx zy zz))))))
(define (painterT* ox oy oz
                   xx xy xz
                   yx yy yz
                   zx zy zz)
  (painterT
   (build-frame
    ox oy oz
    xx xy xz
    yx yy yz
    zx zy zz)))")
   (P "为了方便起见, 现在所谓的painter在接受一个frame之后, "
      "将产生一个绘制指令的列表, 而非绘制图形的副作用. "
      "这种中间表示将允许我们方便地将其转换为其他格式. "
      "因此, 以下将painter简单组合在一起的函数" (Code "over")
      "现在是将列表" (Code "append") "在一起.")
   (CodeB "(define ((over . p*) f)
  (apply append
         (map (lambda (p)
                (p f))
              p*)))")
   (P "我们可以定义一些基本的变换过程, "
      "但是我不确定这里我的命名是否恰当.")
   (CodeB "(define beside
  (let ((t1 (painterT* 0 0 0
                       1 0 0
                       0 1/2 0
                       0 0 1))
        (t2 (painterT* 0 1/2 0
                       1 0 0
                       0 1/2 0
                       0 0 1)))
    (lambda (p1 p2)
      (over (t1 p1) (t2 p2)))))
(define before
  (let ((t1 (painterT* 0 0 0
                       1/2 0 0
                       0 1 0
                       0 0 1))
        (t2 (painterT* 1/2 0 0
                       1/2 0 0
                       0 1 0
                       0 0 1)))
    (lambda (p1 p2)
      (over (t1 p1) (t2 p2)))))
(define mirror-xy
  (painterT* 0 0 1
             1 0 0
             0 1 0
             0 0 -1))
(define mirror-xz
  (painterT* 0 1 0
             1 0 0
             0 -1 0
             0 0 1))
(define mirror-yz
  (painterT* 1 0 0
             -1 0 0
             0 1 0
             0 0 1))")
   (P "我们也可以编写一些创造原始painter的过程, "
      "比如就像原本的SICP书中的绘制数个线段的painter.")
   (CodeB "(define (make-line*-painter lst)
  (lambda (frame)
    (define m (cmap frame))
    (map
     (lambda (pair)
       (match pair
         (((,x1 ,y1 ,z1)
           (,x2 ,y2 ,z2))
          `(line ,(m (make-pt3 x1 y1 z1))
                 ,(m (make-pt3 x2 y2 z2))))))
     lst)))")
   (P "当然了, 这里我只是在生成一种中间表示, "
      "至于具体该如何解释指令则是后人的事情. "
      "一种直接的呈现方法是将其转换为SVG, "
      "然后让浏览器进行渲染. "
      "注意到我们这里的绘制指令里的点仍然是三维的, "
      "所以说" (Q "编译") "绘制指令的过程"
      "需要以一种投影过程为参数, 其将三维的点转换为二维的点.")
   (CodeB "(define ((compile-pict proj #:attr* [attr* '()]) pict)
  (keyword-apply
   Svg
   '(#:attr*) (list attr*)
   (map (lambda (instr)
          (match instr
            ((line ,p1 ,p2)
             ((compile-line proj) p1 p2))
            ((dot ,p)
             ((compile-dot proj) p))
            ))
        pict)))")
   (P "以上是一个用以刻画想法的非常不完善的例子, "
      "显然读者还可以添加更多对于指令的解释方式.")
   (P "当前实现的一个固有限制在于很难正确处理"
      (Q "面") "的渲染, 因为存在遮挡顺序问题. "
      "不以SVG格式而以某种立体格式输出可能是正确的解决方法, "
      "虽然这只是将渲染的问题转嫁给了别人来解决就是了.")
   (P "想说的大概就是这么多了, 以下是一些例子.")
   example0.svg
   example1.svg
   ))