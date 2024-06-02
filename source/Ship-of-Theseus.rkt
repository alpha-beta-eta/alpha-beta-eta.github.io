#lang racket
(provide Ship-of-Theseus.html)
(require SMathML)
(define ((example #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "example")))
   (B (format "Example ~a." n)) " " x*))
(define Ship-of-Theseus.html
  (Prelude
   #:title "The Ship of Theseus"
   #:css "styles.css"
   (H1 "The Ship of Theseus")
   (P "As a Schemer, I think there is an easy explanation to the Ship of Theseus.")
   ((example #:n "1")
    (CodeB "> (define Ship-of-Theseus (cons 'a 'b))
> Ship-of-Theseus
(a . b)
> (set-car! Ship-of-Theseus 'x)
> Ship-of-Theseus
(x . b)
> (set-cdr! Ship-of-Theseus 'y)
> Ship-of-Theseus
(x . y)")
    "The " (Code "Ship-of-Theseus") " before and after the mutations are " (Code "eq?")
    " but not " (Code "equal?") "."
    )
   ((example #:n "2")
    (CodeB "> (define Ship-of-Theseus (cons 'a 'b))
> Ship-of-Theseus
(a . b)
> (set! Ship-of-Theseus (cons 'a 'b))
> Ship-of-Theseus
(a . b)")
    "The " (Code "Ship-of-Theseus") " before and after the assignment are " (Code "equal?")
    " but not " (Code "eq?") "."
    )
   ))