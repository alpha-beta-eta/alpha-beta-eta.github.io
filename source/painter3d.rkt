#lang racket
(provide example0.svg
         example1.svg)
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
(define (test-painterT* ox oy oz
                        xx xy xz
                        yx yy yz
                        zx zy zz)
  (lambda (frame)
    (define o0 (frame-o frame))
    (define x0 (frame-x frame))
    (define y0 (frame-y frame))
    (define z0 (frame-z frame))
    (define (lc a b c)
      (vec3+ (vec3* a x0)
             (vec3* b y0)
             (vec3* c z0)))
    (make-frame
     (pt3+ o0 (lc ox oy oz))
     (lc xx xy xz)
     (lc yx yy yz)
     (lc zx zy zz))))
(define (painterT* ox oy oz
                   xx xy xz
                   yx yy yz
                   zx zy zz)
  (painterT
   (build-frame
    ox oy oz
    xx xy xz
    yx yy yz
    zx zy zz)))
(define ((over . p*) f)
  (append-map
   (lambda (p)
     (p f))
   p*))
(define above
  (let ((t1 (painterT* 0 0 0
                       1 0 0
                       0 1 0
                       0 0 1/2))
        (t2 (painterT* 0 0 1/2
                       1 0 0
                       0 1 0
                       0 0 1/2)))
    (lambda (p1 p2)
      (over (t1 p1) (t2 p2)))))
(define (general-above . p*)
  (define n (length p*))
  (define 1/n (/ 1 n))
  (let iter ((i 0) (p* p*) (P* '()))
    (if (null? p*)
        (apply over (reverse P*))
        (iter (+ i 1)
              (cdr p*)
              (cons
               ((painterT*
                 0 0 (/ i n)
                 1 0 0
                 0 1 0
                 0 0 1/n)
                (car p*))
               P*)))))
(define beside
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
(define (general-beside . p*)
  (define n (length p*))
  (define 1/n (/ 1 n))
  (let iter ((i 0) (p* p*) (P* '()))
    (if (null? p*)
        (apply over (reverse P*))
        (iter (+ i 1)
              (cdr p*)
              (cons
               ((painterT*
                 0 (/ i n) 0 
                 1 0 0
                 0 1/n 0
                 0 0 1)
                (car p*))
               P*)))))
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
(define (general-before . p*)
  (define n (length p*))
  (define 1/n (/ 1 n))
  (let iter ((i 0) (p* p*) (P* '()))
    (if (null? p*)
        (apply over (reverse P*))
        (iter (+ i 1)
              (cdr p*)
              (cons
               ((painterT*
                 (/ i n) 0 0 
                 1/n 0 0
                 0 1 0
                 0 0 1)
                (car p*))
               P*)))))
(define (scale x y z)
  (painterT*
   0 0 0
   x 0 0
   0 y 0
   0 0 z))
(define (translate x y z)
  (painterT*
   x y z
   1 0 0
   0 1 0
   0 0 1))
(define ((cmap f) p)
  (pt3+ (frame-o f)
        (vec3+ (vec3* (pt3-x p) (frame-x f))
               (vec3* (pt3-y p) (frame-y f))
               (vec3* (pt3-z p) (frame-z f)))))
(define (make-line*-painter lst)
  (lambda (frame)
    (define m (cmap frame))
    (map
     (lambda (pair)
       (match pair
         (((,x1 ,y1 ,z1)
           (,x2 ,y2 ,z2))
          `(line ,(m (make-pt3 x1 y1 z1))
                 ,(m (make-pt3 x2 y2 z2))))))
     lst)))
(define (make-polygon*-painter lst)
  (lambda (frame)
    (define m (cmap frame))
    (map
     (lambda (p*)
       (cons 'polygon
             (map (lambda (p)
                    (match p
                      ((,x1 ,y1 ,z1)
                       (m (make-pt3 x1 y1 z1)))))
                  p*)))
     lst)))
(define base-frame
  (build-frame
   256 -1 -129
   256 0 0
   0 256 0
   0 0 256))
(define (isometric p)
  (define x (pt3-x p))
  (define y (pt3-y p))
  (define z (pt3-z p))
  (make-pt2
   (/ (* (sqrt 3) (- x y)) 2)
   (- (* 1/2 (+ x y)) z)))
(define (n2s n)
  (~r n #:precision 2))
(define ((lerp t) a b)
  (+ a (* t (- b a))))
(define ((biased-random t) n)
  (exact-round ((lerp t) (random n) (- n 1))))
(define (base-convert n b)
  (let iter ((n n) (r '()))
    (if (= n 0)
        r
        (iter (quotient n b)
              (cons (remainder n b) r)))))
(define (dec->hex n)
  (define vec
    #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
          #\A #\B #\C #\D #\E #\F))
  (list->string
   (map (curry vector-ref vec)
        (base-convert n 16))))
(define (random-hex n)
  (dec->hex (random n)))
(define ((biased-random-hex t) n)
  (dec->hex ((biased-random t) n)))
(define (random-color t)
  (format "#~a~a~a"
          ((biased-random-hex t) 256)
          ((biased-random-hex t) 256)
          ((biased-random-hex t) 256)))
(define ((compile-line proj) p1 p2)
  (define P1 (proj p1))
  (define P2 (proj p2))
  (Line #:attr*
        `((x1 ,(n2s (pt2-x P1)))
          (y1 ,(n2s (pt2-y P1)))
          (x2 ,(n2s (pt2-x P2)))
          (y2 ,(n2s (pt2-y P2))))))
(define ((compile-polygon proj) p*)
  (define P* (map proj p*))
  (Polygon #:attr*
           `((points
              ,(apply
                string-append
                (map (lambda (P)
                       (format "~a,~a "
                               (n2s (pt2-x P))
                               (n2s (pt2-y P))))
                     P*)))
             (fill ,(random-color 0.75)))))
(define ((compile-pict proj #:attr* [attr* '()]) pict)
  (keyword-apply
   Svg
   '(#:attr*) (list attr*)
   (map (lambda (instr)
          (match instr
            ((line ,p1 ,p2)
             ((compile-line proj) p1 p2))
            ((polygon . ,p*)
             ((compile-polygon proj) p*))))
        pict)))
(define cube-skeleton
  (make-line*-painter
   '(((0 0 0) (1 0 0))
     ((1 0 0) (1 1 0))
     ((1 1 0) (0 1 0))
     ((0 1 0) (0 0 0))
     ((0 0 0) (0 0 1))
     ((1 0 0) (1 0 1))
     ((1 1 0) (1 1 1))
     ((0 1 0) (0 1 1))
     ((0 0 1) (1 0 1))
     ((1 0 1) (1 1 1))
     ((1 1 1) (0 1 1))
     ((0 1 1) (0 0 1)))))
(define half-cube
  (make-polygon*-painter
   '(((1 0 0)
      (1 1 0)
      (1 1 1)
      (1 0 1))
     ((0 1 0)
      (1 1 0)
      (1 1 1)
      (0 1 1))
     ((0 0 1)
      (1 0 1)
      (1 1 1)
      (0 1 1)))))
(define empty-painter
  (lambda (frame)
    '()))
(define painter0
  (above
   half-cube
   ((translate 1/4 1/4 0) ((scale 1/2 1/2 1) half-cube))))
(define painter1
  ((scale 1 1 1/2) half-cube))
(define (four p)
  (define q (before p p))
  (beside q q))
(define (above-double p)
  (above p p))
(define (before-double p)
  (before p p))
(define (beside-double p)
  (beside p p))
(define (repeated f n)
  (if (zero? n)
      identity
      (compose f (repeated f (- n 1)))))
(define painter2
  (before-double (before-double half-cube)))
(define painter3
  (before (before half-cube empty-painter)
          (before empty-painter half-cube)))
(define painter4
  (beside
   (beside painter2 painter3)
   (beside painter3 painter2)))
(define (prob-half-cube x)
  (lambda (frame)
    (if (> (random) x)
        (half-cube frame)
        '())))
(define painter5
  (general-above
   painter4
   painter4
   painter4
   painter4
   painter4))
(define (painter6 n x)
  ((compose
    (repeated above-double n)
    (repeated beside-double n)
    (repeated before-double n))
   (prob-half-cube x)))
(define (painter7 n)
  (define a (/ 1 n))
  (let iter ((h a) (r '()))
    (if (> h 1)
        (apply general-above r)
        (iter (+ h a)
              (cons ((translate (/ (- 1 h) 2)
                                (/ (- 1 h) 2)
                                0)
                     ((scale h h 1) half-cube))
                    r)))))
(define (painter8 n)
  (before (beside (painter7 n)
                  (painter7 (* n 2)))
          (beside (painter7 (* n 3))
                  (painter7 (* n 4)))))
(define (compile-example-painter p)
  ((compile-pict
    isometric
    #:attr*
    '((width "496")
      (height "516")
      (stroke "black")
      (style "display: block; margin: auto;")))
   (p base-frame)))
(define example0.svg
  (compile-example-painter
   (painter8 4)))
(define (x-extend p n)
  (if (= n 0)
      p
      (let* ((q (x-extend p (- n 1)))
             (r (beside q q)))
        (before p (above r r)))))
(define (y-extend p n)
  (if (= n 0)
      p
      (let* ((q (y-extend p (- n 1)))
             (r (above q q)))
        (beside p (before r r)))))
(define example1.svg
  (compile-example-painter
   (y-extend cube-skeleton 3)))