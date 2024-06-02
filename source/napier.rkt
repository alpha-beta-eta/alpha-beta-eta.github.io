#lang racket
(provide napier.html)
(require SMathML)
(define (char->mn c)
  (Mn (string c)))
(define (add0 l)
  (if (null? (cdr l))
      (cons #\0 l)
      l))
(define (cell ab)
  (define-values (c d)
    (apply values
           (map char->mn
                (add0 (string->list
                       (number->string (apply * ab)))))))
  (Menclose
   #:attr* '((notation "updiagonalstrike"))
   (Mtable
    #:attr* '((frame "solid"))
    (Mtr (Mtd c) (Mtd $))
    (Mtr (Mtd $) (Mtd d)))))
(define 1--9 (range 1 10))
(define (product l1 l2)
  (map (lambda (x)
         (map (lambda (y) (list x y))
              l2))
       l1))
(define (tmap f t)
  (map (lambda (l) (map f l)) t))
(define (list->table lst)
  (keyword-apply
   Mtable
   '(#:attr*)
   '(((rowspacing "0")
      (columnspacing "0")))
   (map (lambda (row)
          (apply Mtr (map Mtd row)))
        lst)))
(define (napier m n)
  (list->table
   (tmap cell (product m n))))
(define napier.html
  (TnTmPrelude
   #:title "Napier筹"
   #:css "styles.css"
   (H1. "Napier筹")
   (P "Napier筹是一种计算工具, 可以用来计算乘法. "
      "这里我倒不是为了专门说明或演示Napier筹, "
      "而是为了测试SMathML的表达能力.")
   (P (MB (napier 1--9 1--9))
      "这是一个完整的Napier筹表.")
   (P (MB (napier '(4 6 7 8 5 3 9 9)
                  '(9 6 4 3 1)))
      (MB (&= (&c* 46785399 96431)
              (* 46785399 96431))))
   
   ))