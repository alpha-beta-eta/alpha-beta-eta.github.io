#lang racket
(provide smathml_guide0.html)
(require SMathML)
(define smathml_guide0.html
  (TnTmPrelude
   #:title "SMathML指南"
   #:css "styles.css"
   (H1. "SMathML指南")
   (P "和" (LINK "SMathML参考" "smathml0.html")
      "相比, SMathML指南意在提供即时上手的指引, "
      "以及菜谱式的解决方案.")
   (P "读者应该具备最基本的Racket程序的阅读和编写能力.")
   (H2. "初等数学中的例子")
   (H3. "二项式定理")
   (CodeB "(MB (&amp;= (^ (@+ $x $y) $n)
        (sum (&amp;= $k $0) $n
             (&amp;i* (comb $n $k)
                  (^ $x (&amp;- $n $k))
                  $y^k))))")
   (MB (&= (^ (@+ $x $y) $n)
           (sum (&= $k $0) $n
                (&i* (comb $n $k)
                     (^ $x (&- $n $k))
                     $y^k))))
   (Details
    (Summary "MathML输出参照")
    (CodeB "&lt;math display=&quot;block&quot;>
  &lt;mrow>
    &lt;msup>
      &lt;mrow>&lt;mo stretchy=&quot;false&quot;>(&lt;/mo>
        &lt;mrow>&lt;mi>x&lt;/mi>&lt;mo>+&lt;/mo>&lt;mi>y&lt;/mi>&lt;/mrow>
        &lt;mo stretchy=&quot;false&quot;>)&lt;/mo>&lt;/mrow>
      &lt;mi>n&lt;/mi>&lt;/msup>
    &lt;mo>=&lt;/mo>
    &lt;mrow>
      &lt;munderover>&lt;mo>&amp;sum;&lt;/mo>
        &lt;mrow>&lt;mi>k&lt;/mi>&lt;mo>=&lt;/mo>&lt;mn>0&lt;/mn>&lt;/mrow>
        &lt;mi>n&lt;/mi>&lt;/munderover>
      &lt;mrow>
        &lt;mrow>&lt;mo>(&lt;/mo>
          &lt;mfrac linethickness=&quot;0&quot;>&lt;mi>n&lt;/mi>&lt;mi>k&lt;/mi>&lt;/mfrac>
          &lt;mo>)&lt;/mo>&lt;/mrow>
        &lt;mo>&amp;it;&lt;/mo>
        &lt;msup>&lt;mi>x&lt;/mi>
          &lt;mrow>&lt;mi>n&lt;/mi>&lt;mo>&amp;minus;&lt;/mo>&lt;mi>k&lt;/mi>&lt;/mrow>
        &lt;/msup>
        &lt;mo>&amp;it;&lt;/mo>
        &lt;msup>&lt;mi>y&lt;/mi>&lt;mi>k&lt;/mi>&lt;/msup>
      &lt;/mrow>
    &lt;/mrow>
  &lt;/mrow>
&lt;/math>"))
   (H2. "分析学中的例子")
   (H3. "一些简单极限")
   
   (H2. "代数学中的例子")
   
   ))
