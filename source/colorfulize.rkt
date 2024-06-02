#lang racket
(provide colorfulize)
(require SMathML)
(define ((lerp t) a b)
  (+ (* (- 1 t) a) (* t b)))
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
(define colorfulize
  (T `((td ,(lambda (tag attr* . html*)
              `(,tag ,(attr*-set
                       attr* 'style
                       (format "background-color: ~a"
                               (random-color 0.7)))
                     . ,html*))))))