#lang racket
(provide smathml.html)
(require SMathML)
(define smathml.html
  (TmPrelude
   #:title "SMathML参考"
   #:css "styles.css"
   (H1 "SMathML参考")
   (H2 "一. 原理")
   (P "SMathML的基本原理请参见"
      (A "SMathML论文 (draft)"
         #:attr* '((href "notes/SMathML.pdf"))) ".")
   (H2 "二. 元素参考")
   (H3 "2.1 基本元素")
   (P "根据MathML Core标准, 目前SMathML提供如下基本元素以供构造MathML表达式."
      (CodeB
       "Math Merror Mfrac Mi Mmultiscripts Mn Mo Mover Mpadded Mphantom Mroot Mrow
Ms Mspace Msqrt Mstyle Msub Msubsup Msup Mtable Mtd Mtext Mtr Munder Munderover")
      )
   (H4 (Code "Math") "参考")
   (P "根据MathML标准, MathML表达式必须被包裹于" (Code "&lt;math>") "元素之中. "
      "尽管如此, SMathML提供了一种手段, 可以免除读者疲于添加" (Code "Math")
      "的辛劳. 在" (Code "slt.rkt") "之中, 我们提供了构造树变换的基本抽象" (Code "T") ". "
      "据此, 我们在" (Code "math-styles.rkt") "之中提供了一个过程" (Code "Tm") ", "
      "其可以自动为数学内容添加" (Code "&lt;math>") "标签. 我们也为过程" (Code "Math")
      "提供了一个简短的别名" (Code "M") ".")
   (P "MathML内容有两种呈现风格, 一种是" (Code "inline") ", 另一种是" (Code "block")
      ", 让我们在以下两个例子之中对于它们进行比较.")
   ((example #:n "1")
    (B (Code "inline") "风格.") " "
    "欧拉公式"
    (Math #:attr* '((display "inline"))
          (Mrow (Msup (Mi "e") (Mrow (Mi "i") (Mo "&it;") (Mi "&pi;"))) (Mo "+") (Mn "1") (Mo "=") (Mn "0")))
    "看上去是很有趣的.")
   ((example #:n "2")
    (B (Code "block") "风格.") " "
    "欧拉公式"
    (Math #:attr* '((display "block"))
          (Mrow (Msup (Mi "e") (Mrow (Mi "i") (Mo "&it;") (Mi "&pi;"))) (Mo "+") (Mn "1") (Mo "=") (Mn "0")))
    "看上去是很有趣的.")
   (P "以上两个例子的MathML部分如下."
      (CodeB "(Math #:attr* '((display &quot;inline&quot;))
      (Mrow (Msup (Mi &quot;e&quot;) (Mrow (Mi &quot;i&quot;) (Mo &quot;&amp;it;&quot;) (Mi &quot;&amp;pi;&quot;))) "
             "(Mo &quot;+&quot;) (Mn &quot;1&quot;) (Mo &quot;=&quot;) (Mn &quot;0&quot;)))")
      (CodeB "(Math #:attr* '((display &quot;block&quot;))
      (Mrow (Msup (Mi &quot;e&quot;) (Mrow (Mi &quot;i&quot;) (Mo &quot;&amp;it;&quot;) (Mi &quot;&amp;pi;&quot;))) "
             "(Mo &quot;+&quot;) (Mn &quot;1&quot;) (Mo &quot;=&quot;) (Mn &quot;0&quot;)))")
      "在不明确指出呈现风格的情况下, MathML默认的呈现风格是" (Code "inline") "."
      )
   (H4 (Code "Mfrac") "参考")
   (P (Code "Mfrac") "构造一个分式, 例如"
      (CodeB "(Mfrac (Mn &quot;1&quot;) (Mrow (Mi &quot;x&quot;) (Mo &quot;+&quot;) (Mn &quot;1&quot;)))")
      (CodeB "&lt;mfrac>&lt;mn>1&lt;/mn>&lt;mrow>&lt;mi>x&lt;/mi>&lt;mo>+&lt;/mo>&lt;mn>1&lt;/mn>&lt;/mrow>&lt;/mfrac>")
      (MB (Mfrac (Mn "1") (Mrow (Mi "x") (Mo "+") (Mn "1"))))
      )
   (H3 "2.2 衍生元素与复合元素")
   (P "显然, 这部分是为了让编写数学内容的生活更加简单.")
   (H4 (Code "$a") ", " (Code "$b") ", " (Code "$c") ", ...参考")
   (P (Code "$a") "不过就是" (Code "(Mi &quot;a&quot;)") "的缩写, 依此类推.")
   (H4 (Code "$alpha") ", " (Code "$beta") ", " (Code "$gamma") ", ...参考")
   (P (Code "$alpha") "不过就是" (Code "(Mi &quot;&amp;alpha;&quot;)")
      "的缩写, 依此类推.")
   (H4 (Code "$a:normal") ", " (Code "$a:bold") ", " (Code "$a:script") ", ...参考")
   (P (Code "$a:normal") "不过就是"
      (Code "(Mi &quot;a&quot; #:attr* '((mathvariant &quot;normal&quot;)))")
      "的缩写, 依此类推.")
   
   (H2 "三. 变换")
   (P "SMathML提供了一些构造变换的手段和两个实用的变换.")
   (H3 (Code "set-attr*") "参考")
   
   (H3 (Code "Ttable") "参考")
   (P (Code "Ttable") "为用户调整表格提供了方便的抽象. "
      )
   (H3 (Code "T") "参考")
   (P (Code "T") "是对于一般树变换的抽象. "
      )
   (H3 (Code "Tm") "参考")
   (P (Code "Tm") "为用户编写数学内容提供了许多方便."
      )
   (H3 (Code "Tn") "参考")
   (P (Code "Tn") "为用户提供了一种进行自动编号与引用的手段. "
      )
   (H2 "四. 注记")
   (P "此注记内容和MathML标准的细节与具体的浏览器实现有关, 因而有可能存在过时的情况.")
   (Ol (Li "根据MathML标准, 撇 (prime) 的正当做法是使用上标, 例如"
           (CodeB "(Msup $x $prime)")
           (CodeB "&lt;msup>&lt;mi>x&lt;/mi>&lt;mo>&amp;prime;&lt;/mo>&lt;/msup>")
           (MB (Msup $x $prime))
           "而不是"
           (CodeB "(Mrow $x $prime)")
           (CodeB "&lt;mrow>&lt;mi>x&lt;/mi>&lt;mo>&amp;prime;&lt;/mo>&lt;/mrow>")
           (MB (Mrow $x $prime))
           "尽管如此, 目前Firefox对此支持较好, 而Chrome按照正当做法则会造成偏上的情况. "
           "更大的问题在于给具有下标的元素添加一撇, 例如"
           (CodeB "(_^ $x $1 $prime)")
           (CodeB "&lt;msubsup>&lt;mi>x&lt;/mi>&lt;mn>1&lt;/mn>&lt;mo>&amp;prime;&lt;/mo>&lt;/msubsup>")
           (MB (_^ $x $1 $prime))
           "此时Firefox一切正常, 但是Chrome不论怎样变通都无法得到想要达成的输出结果."
           )
       (Li "虽然不知出于何种原因, 但是目前根式的渲染似乎是不正常的, 例如"
           (CodeB "(Mroot $n $x)")
           (CodeB "&lt;mroot>&lt;mi>n&lt;/mi>&lt;mi>x&lt;/mi>&lt;/mroot>")
           (MB (Mroot $n $x))
           )
       (Li "对于Chrome而言, 除非括号内包裹的是较大的数学表达式, 那么就必须为括号添加"
           (Code "stretchy=&quot;false&quot;") "属性以使其看起来符合常理, 例如"
           (CodeB "(&amp;cm (pare $x) (par0 $x))")
           (CodeB "&lt;mrow>&lt;mrow>&lt;mo>(&lt;/mo>&lt;mi>x&lt;/mi>&lt;mo>)&lt;/mo>&lt;/mrow>&lt;mo>,&lt;/mo>
&lt;mrow>&lt;mo stretchy=&quot;false&quot;>(&lt;/mo>&lt;mi>x&lt;/mi>&lt;mo stretchy=&quot;false&quot;>)&lt;/mo>&lt;/mrow>&lt;/mrow>")
           (MB (&cm (pare $x) (par0 $x)))
           )
       (Li "目前Chrome对于标签" (Code "&lt;mover>") "的渲染不是很符合预期, 例如"
           (CodeB "(OverBar (&+ $x $y))")
           (CodeB "&lt;mover>&lt;mrow>&lt;mi>x&lt;/mi>&lt;mo>+&lt;/mo>&lt;mi>y&lt;/mi>&lt;/mrow>&lt;mo>&amp;OverBar;&lt;/mo>&lt;/mover>")
           (MB (OverBar (&+ $x $y)))
           )
       (Li "目前Chrome对于数学表格的属性支持极其有限, 因此即便连对齐也无法控制."
           )
       
       )
   (H2 "五. 例子, 或者说画廊")
   (H3 "二项式公式")
   (MB (&= (^ (@ (&+ $x $y)) $n)
           (sum (&= $k $0) $n
                (&i* (comb $n $k) (^ $x (&- $n $k)) $y^k))))
   (H3 "一个经典的极限")
   (let ((&sin (lambda (x) (ap (Mi "sin") x))))
     (MB (&= (lim $x $0 (~ (&sin $x) $x)) $1)))
   (H3 (M $e) "的定义")
   (MB (&:= $e (lim $n $inf (^ (pare (&+ $1 (~ $1 $n))) $n)))
       "或者" (&:= $e (sum (&= $n $0) $inf (~ $1 (&fact $n)))))
   (H3 "希腊字母")
   (MB (&Table
        ($alpha
         $beta
         $gamma
         $delta
         $epsilon
         $zeta
         $eta
         $theta
         $iota
         $kappa
         $lambda
         $mu
         $nu
         $xi
         $omicron
         $pi
         $rho
         $sigma
         $tau
         $upsilon
         $phi
         $chi
         $psi
         $omega)
        ($Alpha
         $Beta
         $Gamma
         $Delta
         $Epsilon
         $Zeta
         $Eta
         $Theta
         $Iota
         $Kappa
         $Lambda
         $Mu
         $Nu
         $Xi
         $Omicron
         $Pi
         $Rho
         $Sigma
         $Tau
         $Upsilon
         $Phi
         $Chi
         $Psi
         $Omega)))
   (H3 "不同的字体")
   (MB (&Table
        ("normal"
         "bold"
         "italic"
         "bold-italic"
         "double-struck"
         "bold-fraktur"
         "script"
         "bold-script")
        ($C:normal
         $C:bold
         $C:italic
         $C:bold-italic
         $C:double-struck
         $C:bold-fraktur
         $C:script
         $C:bold-script)))
   (MB (&Table
        ("fraktur"
         "sans-serif"
         "bold-sans-serif"
         "sans-serif-italic"
         "sans-serif-bold-italic"
         "monospace")
        ($C:fraktur
         $C:sans-serif
         $C:bold-sans-serif
         $C:sans-serif-italic
         $C:sans-serif-bold-italic
         $C:monospace)))
   (H3 "Fibonacci序列")
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
   
   ))