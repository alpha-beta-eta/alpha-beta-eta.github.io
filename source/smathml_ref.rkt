#lang racket
(provide smathml_ref.html)
(require SMathML)
(define smathml_ref.html
  (TnTmPrelude
   #:title "SMathML Reference"
   #:css "styles.css"
   (H1 "SMathML Reference")
   (H2 "1&nbsp;&nbsp;&nbsp;&nbsp;Introduction")
   (P "MathML is a markup language based on XML for describing mathematics. "
      "Though MathML is human-readable-and-writable, it is human-readable-and-writable "
      "in the same sense that XML is human-readable-and-writable. So we have to provide "
      "users with methods of combination and abstraction to help them organize ideas "
      "about mathematical presentation and content.")
   (P "SMathML is a DSL embedded in Scheme for expressing MathML (and also HTML), "
      "which is based on a hierarchical structure:"
      (MB (&-> (bra0 "user-defined abstractions")
               (bra0 "primitive abstractions")
               (bra0 "SXML") (bra0 "HTML/XML")) ".")
      "It also provides a general way inspired by Oleg Kiselyov's SXSLT to create transformations of SXML, "
      "which is just a representation of HTML/XML as S-expressions. The boundary between primitive abstractions "
      "and user-defined abstractions is intended to be vague and the users are encouraged to create "
      "fundamental abstractions to his own taste."
      )
   (H2 "2&nbsp;&nbsp;&nbsp;&nbsp;Basic Elements of SMathML")
   
   (H2 "3&nbsp;&nbsp;&nbsp;&nbsp;Transformations of SMathML")
   
   (H2 "4&nbsp;&nbsp;&nbsp;&nbsp;Examples")
   (H3 "4.1&nbsp;&nbsp;&nbsp;The Fibonacci Sequence")
   (CodeB "(MB (let ((fib (lambda (n)
                 (let iter ((a 0) (b 1) (c 0))
                   (if (= c n)
                       a
                       (iter b (+ a b) (+ c 1))))))
          (&amp;fib (lambda (n) (app 'fib n)))
          (seq (range 13)))
      (set-attr*
       (&amp;table (list (map &amp;fib seq) (map fib seq)))
       'frame &quot;solid&quot; 'rowlines &quot;solid&quot; 'columnlines &quot;solid&quot;)))")
   (MB (let ((fib (lambda (n)
                    (let iter ((a 0) (b 1) (c 0))
                      (if (= c n)
                          a
                          (iter b (+ a b) (+ c 1))))))
             (&fib (lambda (n) (app 'fib n)))
             (seq (range 13)))
         (set-attr*
          (&table (list (map &fib seq) (map fib seq)))
          'frame "solid" 'rowlines "solid" 'columnlines "solid")))
   (H3 "4.2&nbsp;&nbsp;&nbsp;The Binomial Theorem")
   (CodeB "(MB (&amp;= (^ (@+ $x $y) $n)
        (sum (&amp;= $k $0) $n
             (&amp;i* (comb $n $k) (^ $x (&amp;- $n $k)) $y^k))))")
   (MB (&= (^ (@+ $x $y) $n)
           (sum (&= $k $0) $n
                (&i* (comb $n $k) (^ $x (&- $n $k)) $y^k))))
   (H2 "5&nbsp;&nbsp;&nbsp;&nbsp;Remarks")
   
   ))