#lang racket
(provide (all-defined-out))
(require "xml.rkt" "css.rkt")
(define (size x)
  (cond ((pair? x) (+ (size (car x)) (size (cdr x))))
        ((null? x) 0)
        ((string? x) (string-length x))
        (else 1)))
(define ((emit proc) exp path)
  (printf "size of [~a]: ~a\n" path (size exp))
  (with-output-to-file path
    (lambda () (proc exp))
    #:exists 'replace))
(define emitXml (emit Xml))
(define emitCss (emit Css))