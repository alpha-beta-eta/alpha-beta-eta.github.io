#lang racket
(provide colorfulize)
(require SMathML)
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
(define (random-color)
  (format "#~a~a~a"
          (random-hex 256)
          (random-hex 256)
          (random-hex 256)))
(define colorfulize
  (T `((td *preorder*
           ,(lambda (tag attr* . html*)
              `(,tag ,(attr*-set
                       attr* 'style
                       (format "background-color: ~a"
                               (random-color)))
                     . ,html*))))))