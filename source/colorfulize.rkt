#lang racket
(provide colorfulize)
(require SMathML)
(define ((lerp t) a b)
  (+ a (* t (- b a))))
(define ((biased-random t) n)
  (exact-round ((lerp t) (random n) (- n 1))))
(define (random-color t)
  (format "rgb(~a,~a,~a)"
          ((biased-random t) 256)
          ((biased-random t) 256)
          ((biased-random t) 256)))
(define colorfulize
  (T `((td ,(lambda (tag attr* . html*)
              `(,tag ,(attr*-set
                       attr* 'style
                       (format "background-color: ~a"
                               (random-color 0.9)))
                     . ,html*))))))
