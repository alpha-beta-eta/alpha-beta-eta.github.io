#lang racket
(provide brzozowski.html)
(require SMathML)
(define (&D_c L) (app $D_c L))
(define brzozowski.html
  (TmPrelude
   #:title "Brzozowski导数"
   #:css "styles.css"
   (H1 "Brzozowski导数")
   ((definition)
    "一个语言是一个字符串的集合, 一个字符串是一个字符的有限序列.")
   ((notation)
    $epsilon "代表空字符串 (显然只有一个). " (&:= $0 $empty) ". "
    (&:= $1 (setE $epsilon)) ". 对于字符" $c ", " (&:= $c (setE $c))
    " (注意到我们将字符" $c "当成字符串" $c ", 这有些模糊, 却是数学实践的惯例).")
   ((definition)
    "对于字符" $c "和语言" $L ", " $L "相对于" $c "的Brzozowski导数"
    (&D_c $L) "是一个语言, 其是" $L "中所有以" $c "开头的字符串"
    "去掉开头的" $c "得到的.")
   ((definition)
    "给定语言" $L_1 "和" $L_2 ", " (&:= (&+ $L_1 $L_2) (&union $L_1 $L_2)) ", "
    (&:= (&c* $L_1 $L_2)
         (setI (: $w_1 $w_2)
               (&cm (&in $w_1 $L_1)
                    (&in $w_2 $L_2)))) ".")
   ((definition)
    "根据乘法的定义, 我们可以递归地定义语言的幂. 对于语言" $L ", " (&:= $L^0 $1)
    ", " (&:= (^ $L (&+ $i $1)) (&c* $L $L^i)) ".")
   ((definition)
    "根据幂的定义, 我们可以定义Kleene星号. 对于语言" $L ", "
    (&:= (^ $L $*) (: (_ $Union (&in $i $NN)) $L^i))
    ", 注意这里的" $NN "包括自然数" $0 ".")
   ((definition)
    "正则语言可以递归地被定义"
    (Ol (Li $0 "是一个正则语言;")
        (Li $1 "是一个正则语言;")
        (Li "如果" $L_1 "和" $L_2 "是正则语言, 那么" (&+ $L_1 $L_2)
            "是正则语言;")
        (Li "如果" $L_1 "和" $L_2 "是正则语言, 那么" (&c* $L_1 $L_2)
            "是正则语言;")
        (Li "如果" $L "是一个正则语言, 那么" (^ $L $*) "是一个正则语言.")))
   ((definition)
    "正则表达式是描述正则语言的表达式, 其可以由以下的BNF定义"
    (CodeB "&lt;exp> ::= zero
       |  one
       |  &lt;char>
       |  (plus &lt;exp> &lt;exp>)
       |  (times &lt;exp> &lt;exp>)
       |  (star &lt;exp>)"))
   ((theorem)
    "Brzozowski导数具有以下性质"
    (Ol (Li (M (&= (&D_c $0) $0)) ";")
        (Li (M (&= (&D_c $1) $0)) ";")
        (Li (M (&= (&D_c $c) $1)) ";")
        (Li (M (&= (&D_c (&prime $c)) $0))
            ", 如果" (M (&!= $c (&prime $c))) ";")
        (Li (M (&= (&D_c (&+ $L_1 $L_2))
                   (&+ (&D_c $L_1) (&D_c $L_2)))) ";")
        (Li (M (&= (&D_c (&c* $L_1 $L_2))
                   (&c* (&D_c $L_1) $L_2)))
            ", 如果" (M (&!in $epsilon $L_1)) ";")
        (Li (M (&= (&D_c (&c* $L_1 $L_2))
                   (&+ (@ (&c* (&D_c $L_1) $L_2))
                       (&D_c $L_2))))
            ", 如果" (M (&in $epsilon $L_1)) ";")
        (Li (M (&= (&D_c (^ $L $*))
                   (&c* (&D_c $L) (^ $L $*)))) "."))
    "其中" (M (&cm $c (&prime $c))) "是字符, "
    (M (&cm $L $L_1 $L_2)) "是语言." (Br)
    "实际上, 这给我们提供了计算正则语言的Brzozowski导数的递归方法.")
   ((program)
    "以下过程" (Code "D") "可以计算Brzozowski导数."
    (CodeB "(define (D exp c)
  (match exp
    (zero zero)
    (one zero)
    (,c^
     (guard (char? c^))
     (if (char=? c^ c) one zero))
    ((plus ,e1 ,e2)
     (plus (D e1 c) (D e2 c)))
    ((times ,e1 ,e2)
     (define e3
       (times (D e1 c) e2))
     (if (delta? e1)
         (plus e3 (D e2 c))
         e3))
    ((star ,e)
     (times (D e c) (star e)))))")
    "其中"
    (CodeB "(define zero 'zero)
(define one 'one)
(define (plus e1 e2)
  (cond ((eq? e1 zero) e2)
        ((eq? e2 zero) e1)
        (else `(plus ,e1 ,e2))))
(define (times e1 e2)
  (cond ((eq? e1 zero) zero)
        ((eq? e2 zero) zero)
        ((eq? e1 one) e2)
        ((eq? e2 one) e1)
        (else `(times ,e1 ,e2))))
(define (star exp)
  (cond ((eq? exp zero) one)
        ((eq? exp one) one)
        (else `(star ,exp))))
(define (delta? exp)
  (match exp
    (zero #f)
    (one #t)
    (,c (guard (char? c)) #f)
    ((plus ,e1 ,e2)
     (or (delta? e1) (delta? e2)))
    ((times ,e1 ,e2)
     (and (delta? e1) (delta? e2)))
    ((star ,e) #t)))"))
   ((theorem)
    "对于字符" (M $c) "和字符串" (M $w) ", "
    "对于语言" (M $L) ", "
    (M (&in (: $c $w) $L)) "当且仅当"
    (M (&in $w (&D_c $L))) ".")
   ((program)
    "以上定理给我们提供了一个判断字符串是否在语言之中的方法, "
    "根据过程" (Code "D") ", 我们可以编写一个判断字符串是否"
    "在 (由正则表达式刻画的) 正则语言之中的谓词" (Code "in?") "."
    (CodeB "(define (in? str exp)
  (define l (string-length str))
  (let iter ((i 0) (exp exp))
    (cond ((= i l) (delta? exp))
          ((eq? exp zero) #f)
          (else
           (iter (+ i 1)
                 (D exp (string-ref str i)))))))"))
   ((example)
    "以下是简单的例子, 刻画了" (Code "in?") "的用法"
    (CodeB "> (define L-pair-elim
    (times #\\c
           (times (plus #\\a #\\d)
                  (times (star (plus #\\a #\\d))
                         #\\r))))
> (in? &quot;cadar&quot; L-pair-elim)
#t
> (in? &quot;cr&quot; L-pair-elim)
#f
> (in? &quot;cada&quot; L-pair-elim)
#f
> (in? &quot;cader&quot; L-pair-elim)
#f"))
   (H2 "附录: Standard ML实现的版本")
   (P "鉴于有的读者可能看不得Lisp, 我也准备了Standard ML的版本.")
   (CodeB "datatype exp =
    Zero
  | One
  | Char of char
  | Plus of exp * exp
  | Times of exp * exp
  | Star of exp

fun deltap(Zero) = false
  | deltap(One) = true
  | deltap(Char(c)) = false
  | deltap(Plus(e1,e2)) =
  deltap(e1) orelse deltap(e2)
  | deltap(Times(e1,e2)) =
  deltap(e1) andalso deltap(e2)
  | deltap(Star(e)) = true

fun ZeroC() = Zero;
fun OneC() = One;
fun PlusC(Zero,e2) = e2
  | PlusC(e1,Zero) = e1
  | PlusC(e1,e2) = Plus(e1,e2)
fun TimesC(Zero,e2) = Zero
  | TimesC(e1,Zero) = Zero
  | TimesC(One,e2) = e2
  | TimesC(e1,One) = e1
  | TimesC(e1,e2) = Times(e1,e2)
fun StarC(Zero) = One
  | StarC(One) = One
  | StarC(exp) = Star(exp)

fun D(Zero,c) = Zero
  | D(One,c) = Zero
  | D(Char(c0),c) =
  if (c0 = c) then One else Zero
  | D(Plus(e1,e2),c) =
  PlusC(D(e1,c),D(e2,c))
  | D(Times(e1,e2),c) =
  let
   val e3 = TimesC(D(e1,c),e2)
  in
   if deltap(e1)
   then PlusC(e3,D(e2,c))
   else e3
  end
  | D(Star(e),c) =
  TimesC(D(e,c),StarC(e))

fun matchp(str,exp) =
  let
   val l = size(str)
   fun iter(i,exp) =
   if (i = l) then deltap(exp)
   else if (exp = Zero) then false
   else iter(i+1,D(exp,String.sub(str,i)))
  in
   iter(0,exp)
  end

val L_pair_elim =
Times(Char(#&quot;c&quot;),
  Times(Plus(Char(#&quot;a&quot;),Char(#&quot;d&quot;)),
    Times(Star(Plus(Char(#&quot;a&quot;),Char(#&quot;d&quot;))),
      Char(#&quot;r&quot;))))")
   (P "并请读者阅读以下交互.")
   (CodeB "- D(L_pair_elim,#&quot;c&quot;);
val it =
  Times
    (Plus (Char #&quot;a&quot;,Char #&quot;d&quot;),
     Times (Star (Plus (Char #&quot;a&quot;,Char #&quot;d&quot;)),Char #&quot;r&quot;)) : exp
- D(it,#&quot;a&quot;);
val it = Times (Star (Plus (Char #&quot;a&quot;,Char #&quot;d&quot;)),Char #&quot;r&quot;) : exp
- D(it,#&quot;d&quot;);
val it = Times (Star (Plus (Char #&quot;a&quot;,Char #&quot;d&quot;)),Char #&quot;r&quot;) : exp
- D(it,#&quot;r&quot;);
val it = One : exp")
   
   ))