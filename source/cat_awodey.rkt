#lang racket
(provide cat_awodey.html)
(require SMathML)
(define $CT (Mi "CT"))
(define $CT^* (&* $CT))
(define Σ $Sigma:normal)
(define Δ $Delta:normal)
(define Σ* (&* Σ))
(define Δ* (&* Δ))
(define $thetav (Mi "&thetav;"))
(define $thetav_X (_ $thetav $X))
(define $! (Mi "!"))
(define (Family X i I)
  (_ (@ (_ X i)) (∈ i I)))
(define (λfst #:v [v $z])
  (Lam v (&fst v)))
(define (λsnd #:v [v $z])
  (Lam v (&snd v)))
(define (λpair a d #:v [v $x])
  (Lam v (tupa0 (ap a v) (ap d v))))
(define (λcompose c b #:v [v $y])
  (Lam v (ap c (@ap b v))))
(define (λcomp c b #:v [v $y])
  (Lamb v (@ap c (@ap b v))))
(define (subst termA termB var)
  (: termA (bra0 (&/ termB var))))
(define $.:compact
  (Mo "." #:attr* '((lspace "0") (rspace "0"))))
(define (Lam x M)
  (: $lambda x $.:compact M))
(define (Lamb x M)
  (: $lambda x M))
(define (map2* f . x*)
  (map2 f x*))
(define $c*^ (&prime $c*))
(define (&lambda f)
  (ap $lambda f))
(define (LeftRightDiagram A x B y C)
  (MB A (^^ $<- (pad x)) B
      (^^ $-> (pad y)) C))
(define $<=:id (Mi "&le;"))
(define $meet $conj)
(define $join $disj)
(define $0^ (&prime $0))
(define (_@ X a)
  (_ (@ X) a))
(define (restrict f A)
  (: f (_ $lv A)))
(define $+:id (Mi "+"))
(define (map* proc . x*)
  (map proc x*))
(define (n2s n)
  (~r n #:precision 2))
(define arrow-head
  (Marker
   #:attr*
   '((id "arrow-head")
     (viewbox "0 0 10 10")
     (refX "5")
     (refY "5")
     (markerWidth "6")
     (markerHeight "6")
     (orient "auto-start-reverse"))
   (Path #:attr* '((d "M 0 2 L 6 5 L 0 8 z")))))
(define-struct vec2
  (x y)
  #:transparent)
(define-struct pt2
  (x y)
  #:transparent)
(define (vec2* k v)
  (make-vec2
   (* k (vec2-x v))
   (* k (vec2-y v))))
(define (pt2+ p v)
  (make-pt2
   (+ (pt2-x p) (vec2-x v))
   (+ (pt2-y p) (vec2-y v))))
(define (pt2- p1 p2)
  (make-vec2
   (- (pt2-x p1) (pt2-x p2))
   (- (pt2-y p1) (pt2-y p2))))
(define ((lerp t) p1 p2)
  (pt2+ p1 (vec2* t (pt2- p2 p1))))
(define zero-vec2
  (make-vec2 0 0))
(define (:FO position #:offset [offset zero-vec2] #:width [width 100] #:height [height 30] . x*)
  (define pos
    (pt2+ position offset))
  (keyword-apply
   ForeignObject
   '(#:attr*)
   `(((x ,(n2s (pt2-x pos)))
      (y ,(n2s (pt2-y pos)))
      (width ,(n2s width))
      (height ,(n2s height))))
   x*))
(define (:line start end)
  (define x1 (n2s (pt2-x start)))
  (define y1 (n2s (pt2-y start)))
  (define x2 (n2s (pt2-x end)))
  (define y2 (n2s (pt2-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1)
          (x2 ,x2)
          (y2 ,y2))))
(define (:arrow start end)
  (set-attr*
   (:line start end)
   'marker-end
   "url(#arrow-head)"))
(define (:arrow-prop start end #:prop [prop 0.7])
  (define t (/ (- 1 prop) 2))
  (define start^ ((lerp t) start end))
  (define end^ ((lerp (- 1 t)) start end))
  (:arrow start^ end^))
(define (set-dotted line)
  (set-attr* line 'stroke-dasharray "2 2"))
(define (concat . x*)
  (apply : x*))
(define (forget x)
  (&abs x))
(define (free-monoid A)
  (app $M A))
(define $A^* (&* $A))
(define $dummy (Mi "&minus;"))
(define $empty-word (Mi "-"))
(define $*:id (Mi "&#8270;"))
(define $g_* (_ $g $*))
(define (long-arrow length)
  (^^ $-> (Mspace #:attr* `((width ,length)))))
(define pad
  (case-lambda
    ((x)
     (: (Mspace #:attr* `((width "2em")))
        x
        (Mspace #:attr* `((width "2em")))))
    ((x length)
     (: (Mspace #:attr* `((width ,length)))
        x
        (Mspace #:attr* `((width ,length)))))))
(define (slice Cat C)
  (&/ Cat C))
(define (coslice C Cat)
  (&/ C Cat))
(define $op (Mi "op"))
(define (&op C)
  (^ C $op))
(define $<=_A (_ $<= $A))
(define $<=_B (_ $<= $B))
(define $& (Mo "&amp;"))
(define $Star (Mo "&Star;"))
(define $rlarr (Mo "&rlarr;"))
(define $\; (Mo ";"))
(define $d*_M (_ $d* $M))
(define $d*_N (_ $d* $N))
(define $cong (Mo "&cong;"))
(define $rrarr (Mo "&rrarr;"))
(define $~ (Mo "&Tilde;"))
(define-infix*
  (&<=_A $<=_A)
  (&<=_B $<=_B)
  (&& $&)
  (&rlarr $rlarr)
  (&\; $\;)
  (&d*_M $d*_M)
  (&d*_N $d*_N)
  (&cong $cong)
  (&rrarr $rrarr)
  (&meet $meet)
  (&join $join)
  (&c*^ $c*^)
  (&~ $~)
  (&=> $=>))
(define $QQ^+ (^ $QQ $+))
(define (powerset X)
  (app $P:script X))
(define $.
  (Mo "." #:attr* '((lspace "0"))))
(define (card A)
  (&abs A))
(define (open X)
  (app $O:script X))
(define (∃ Q P)
  (: $exists Q $. P))
(define (_cm A . x*)
  (_ A (apply &cm x*)))
(define (_prime A x)
  (_^ A x $prime))
(define (_inv A x)
  (_^ A x $-1))
(define todo.svg
  (Svg
   #:attr* '((width "320")
             (height "160")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Path #:attr* '((x "0")
                   (y "0")
                   (d "M 0 0 h 320 v 160 h -320 z")
                   (fill "none")))
   (Text #:attr* '((x "130") (y "80")) "欠一张图")))
(define (Arrow f A B)
  (&: f (&-> A B)))
(define (map-toggle flag proc lst)
  (cond ((null? lst) '())
        (flag (cons (proc (car lst))
                    (map-toggle (not flag) proc (cdr lst))))
        (else (cons (car lst)
                    (map-toggle (not flag) proc (cdr lst))))))
(define (ARROW . arg*)
  (apply : (map-toggle
            #f
            (lambda (f) (^^ $-> f))
            arg*)))
(define (Functor F C D)
  (&: F (&-> C D)))
(define (Mono f A B)
  (&: f (&>-> A B)))
(define (Epi f A B)
  (&: f (&->> A B)))
(define $Hom (Mi "Hom"))
(define Hom
  (case-lambda
    ((X Y) (appl $Hom X Y))
    ((C X Y) (appl (_ $Hom C) X Y))))
(define $Aut (Mi "Aut"))
(define Aut
  (case-lambda
    ((X) (app $Aut X))
    ((C X) (app (_ $Aut C) X))))
(define app*
  (case-lambda
    ((f x) (app f x))
    ((f g . arg*) (app f (apply app* g arg*)))))
(define-syntax-rule (define-simple* (&id $id str) ...)
  (begin
    (define $id (Mi str))
    ...
    (define (&id x) (app $id x))
    ...))
(define-simple*
  (&range $range "range")
  (&dom $dom "dom")
  (&cod $cod "cod")
  (&darr $darr "&darr;")
  (&fst $fst "fst")
  (&snd $snd "snd"))
(define-@lized-op*
  (@op &op)
  (@compose &compose)
  (@_cm _cm)
  (@c* &c*)
  (@\|-> &\|->)
  (@cm &cm)
  (@: &:)
  (@Lam Lam)
  (@Lamb Lamb)
  (@λcompose λcompose)
  (@λcomp λcomp)
  (@λfst λfst)
  (@λsnd λsnd)
  (@fst &fst)
  (@snd &snd)
  (@-> &->)
  (@Arrow Arrow))
(define (make-cat str)
  (Mi str #:attr* '((mathvariant "bold"))))
(define-syntax-rule (define-cat* (id str) ...)
  (begin
    (define id (make-cat str))
    ...))
(define-cat*
  (Sets "Sets")
  (Pos "Pos")
  (Rel "Rel")
  (Cat0 "0")
  (Cat1 "1")
  (Cat2 "2")
  (Cat3 "3")
  (CatC "C")
  (CatD "D")
  (Cat "Cat")
  (Dis "Dis")
  (Mon "Mon")
  (Groups "Groups")
  (CatP "P")
  (Graphs "Graphs")
  (Top "Top")
  (Rings "Rings")
  (BA "BA"))
(define (free-category G)
  (app CatC G))
(define $fin (Mi "fin"))
(define Sets_fin (_ Sets $fin))
(define Sets_* (_ Sets $*))
(define CatC^-> (^ CatC $->))
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign "left center right")
                (width "100%"))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))
(define (H3. #:attr* [attr* '()] #:id [id #f] #:switch? [switch? #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite
                     #:level 3 #:id id #:switch? switch?)
    ,attr* . ,html*))
(define (H4. #:attr* [attr* '()] #:id [id #f] #:switch? [switch? #f] . html*)
  `(,(build-%heading #:present heading-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?)
    ,attr* . ,html*))
(define (format-num section index)
  (and index
       (format "~a.~a" (cadr (reverse section)) index)))
(define (format-head name section index)
  (let ((num (format-num section index)))
    (if num
        (B name (format "~a. " num))
        (B name ". "))))
(define (Entry name class)
  (define (present %entry attr* . html*)
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define head (format-head name section index))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define id (%entry-id %entry))
    (define href (string-append "#" id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define num (format-num section index))
    (Cite `(a ((href ,href)) ,name ,num)))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto? #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Definition "定义" "definition")
  (Remark "评注" "remark")
  (Theorem "定理" "theorem")
  (Warning "警告" "warning")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Corollary "推论" "corollary"))
(define ((tcomment #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "tcomment")))
   (B (format "译者注记~a." n)) " " x*))
(define UMP:free-monoid.svg
  (Svg
   #:attr* '((width "480")
             (height "300")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs arrow-head)
   (:FO (make-pt2 0 0) (Em "在" Mon "之中:"))
   (:FO (make-pt2 120 30) (free-monoid $A))
   (:FO (make-pt2 360 30) $N)
   (set-dotted
    (:arrow-prop (pt2+ (make-pt2 120 30)
                       (make-vec2 20 10))
                 (pt2+ (make-pt2 360 30)
                       (make-vec2 20 10))))
   (:FO ((lerp 1/2) (make-pt2 120 30) (make-pt2 360 30))
        #:offset (make-vec2 10 -10)
        (OverBar $f))
   (:FO (make-pt2 0 60) (Em "在" Sets "之中:"))
   (:FO (make-pt2 120 90)
        #:offset (make-vec2 -4 0)
        (forget (free-monoid $A)))
   (:FO (make-pt2 360 90)
        #:offset (make-vec2 -4 0)
        (forget $N))
   (:FO (make-pt2 120 280)
        #:offset (make-vec2 14 0)
        $A)
   (:arrow-prop (pt2+ (make-pt2 120 90)
                      (make-vec2 20 10))
                (pt2+ (make-pt2 360 90)
                      (make-vec2 20 10)))
   (:arrow-prop #:prop 0.8
                (pt2+ (make-pt2 120 280)
                      (make-vec2 20 8))
                (pt2+ (make-pt2 120 90)
                      (make-vec2 20 8)))
   (:arrow-prop #:prop 0.85
                (pt2+ (make-pt2 120 280)
                      (make-vec2 18 10))
                (pt2+ (make-pt2 360 90)
                      (make-vec2 18 10)))
   (:FO ((lerp 1/2) (make-pt2 120 90) (make-pt2 360 90))
        #:offset (make-vec2 8 -10)
        (forget (OverBar $f)))
   (:FO ((lerp 1/2) (make-pt2 120 90) (make-pt2 120 280))
        #:offset (make-vec2 6 0)
        $i)
   (:FO ((lerp 1/2) (make-pt2 120 280) (make-pt2 360 90))
        #:offset (make-vec2 18 10)
        $f)))
(define UMP:free-category.svg
  (Svg
   #:attr* '((width "480")
             (height "300")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs arrow-head)
   (:FO (make-pt2 0 0) (Em "在" Cat "之中:"))
   (:FO (make-pt2 120 30) (free-category $G))
   (:FO (make-pt2 360 30) CatD)
   (set-dotted
    (:arrow-prop (pt2+ (make-pt2 120 30)
                       (make-vec2 20 10))
                 (pt2+ (make-pt2 360 30)
                       (make-vec2 20 10))))
   (:FO ((lerp 1/2) (make-pt2 120 30) (make-pt2 360 30))
        #:offset (make-vec2 10 -10)
        (OverBar $h))
   (:FO (make-pt2 0 60) #:width 120 (Em "在" Graphs "之中:"))
   (:FO (make-pt2 120 90)
        #:offset (make-vec2 -4 0)
        (forget (free-category $G)))
   (:FO (make-pt2 360 90)
        #:offset (make-vec2 -4 0)
        (forget CatD))
   (:FO (make-pt2 120 280)
        #:offset (make-vec2 14 0)
        $G)
   (:arrow-prop (pt2+ (make-pt2 120 90)
                      (make-vec2 20 10))
                (pt2+ (make-pt2 360 90)
                      (make-vec2 20 10)))
   (:arrow-prop #:prop 0.8
                (pt2+ (make-pt2 120 280)
                      (make-vec2 20 8))
                (pt2+ (make-pt2 120 90)
                      (make-vec2 20 8)))
   (:arrow-prop #:prop 0.85
                (pt2+ (make-pt2 120 280)
                      (make-vec2 18 10))
                (pt2+ (make-pt2 360 90)
                      (make-vec2 18 10)))
   (:FO ((lerp 1/2) (make-pt2 120 90) (make-pt2 360 90))
        #:offset (make-vec2 8 -10)
        (forget (OverBar $h)))
   (:FO ((lerp 1/2) (make-pt2 120 90) (make-pt2 120 280))
        #:offset (make-vec2 6 0)
        $i)
   (:FO ((lerp 1/2) (make-pt2 120 280) (make-pt2 360 90))
        #:offset (make-vec2 18 10)
        $h)))
(define describe-a-category.svg
  (Svg
   #:attr*
   '((width "240")
     (height "80")
     (stroke "black")
     (style "display: block; margin: auto;"))
   (Defs arrow-head)
   (:FO (make-pt2 30 30) $C_2)
   (:FO (make-pt2 120 30) $C_1)
   (:FO (make-pt2 210 30) $C_0)
   (:arrow-prop
    (pt2+ (make-pt2 30 30)
          (make-vec2 8 8))
    (pt2+ (make-pt2 120 30)
          (make-vec2 8 8)))
   (:arrow-prop
    (pt2+ (make-pt2 210 30)
          (make-vec2 8 8))
    (pt2+ (make-pt2 120 30)
          (make-vec2 8 8)))
   (:arrow-prop
    (pt2+ (make-pt2 120 30)
          (make-vec2 8 -7))
    (pt2+ (make-pt2 210 30)
          (make-vec2 8 -7)))
   (:arrow-prop
    (pt2+ (make-pt2 120 30)
          (make-vec2 8 23))
    (pt2+ (make-pt2 210 30)
          (make-vec2 8 23)))
   (:FO
    ((lerp 1/2)
     (make-pt2 30 30)
     (make-pt2 120 30))
    #:offset (make-vec2 4 -10)
    $compose)
   (:FO
    ((lerp 1/2)
     (make-pt2 120 30)
     (make-pt2 210 30))
    #:offset (make-vec2 4 -1)
    $i)
   (:FO
    ((lerp 1/2)
     (make-pt2 120 30)
     (make-pt2 210 30))
    #:offset (make-vec2 -4 -25)
    $cod)
   (:FO
    ((lerp 1/2)
     (make-pt2 120 30)
     (make-pt2 210 30))
    #:offset (make-vec2 -6 24)
    $dom)))
(define set-product.svg
  (Svg
   #:attr* '((width "360")
             (height "195")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs arrow-head)
   (:FO (make-pt2 180 15) $1)
   (:FO (make-pt2 25 165) $A)
   (:FO (make-pt2 330 165) $B)
   (:FO (make-pt2 160 165) (&c* $A $B))
   (:arrow-prop (pt2+ (make-pt2 180 5)
                      (make-vec2 10 10))
                (pt2+ (make-pt2 20 165)
                      (make-vec2 10 10)))
   (:arrow-prop (pt2+ (make-pt2 180 5)
                      (make-vec2 0 10))
                (pt2+ (make-pt2 340 165)
                      (make-vec2 0 10)))
   (:arrow-prop (pt2+ (make-pt2 163 165)
                      (make-vec2 8 5))
                (pt2+ (make-pt2 25 165)
                      (make-vec2 8 5)))
   (:arrow-prop (pt2+ (make-pt2 163 165)
                      (make-vec2 17 5))
                (pt2+ (make-pt2 330 165)
                      (make-vec2 17 5))
                #:prop 0.65)
   (set-dotted
    (:arrow-prop (pt2+ (make-pt2 180 15)
                       (make-vec2 3 5))
                 (pt2+ (make-pt2 180 165)
                       (make-vec2 3 5))))
   (:FO (make-pt2 92 80) $a)
   (:FO (make-pt2 270 80) $b)
   (:FO (make-pt2 190 90) (tu0 $a $b))
   (:FO (make-pt2 95 175) $pi_1)
   (:FO (make-pt2 250 175) $pi_2)))
(define UMP:product.svg
  (Svg
   #:attr* '((width "360")
             (height "195")
             (stroke "black")
             (style "display: block; margin: auto;"))
   (Defs arrow-head)
   (:FO (make-pt2 175 15) $X)
   (:FO (make-pt2 25 165) $A)
   (:FO (make-pt2 330 165) $B)
   (:FO (make-pt2 175 165) $P)
   (:arrow-prop (pt2+ (make-pt2 180 5)
                      (make-vec2 10 10))
                (pt2+ (make-pt2 20 165)
                      (make-vec2 10 10)))
   (:arrow-prop (pt2+ (make-pt2 180 5)
                      (make-vec2 0 10))
                (pt2+ (make-pt2 340 165)
                      (make-vec2 0 10)))
   (:arrow-prop (pt2+ (make-pt2 163 165)
                      (make-vec2 8 5))
                (pt2+ (make-pt2 25 165)
                      (make-vec2 8 5)))
   (:arrow-prop (pt2+ (make-pt2 163 165)
                      (make-vec2 17 5))
                (pt2+ (make-pt2 330 165)
                      (make-vec2 17 5))
                #:prop 0.65)
   (set-dotted
    (:arrow-prop (pt2+ (make-pt2 180 15)
                       (make-vec2 3 5))
                 (pt2+ (make-pt2 180 165)
                       (make-vec2 3 5))))
   (:FO (make-pt2 85 80) $x_1)
   (:FO (make-pt2 270 80) $x_2)
   (:FO (make-pt2 190 90) $u)
   (:FO (make-pt2 95 175) $p_1)
   (:FO (make-pt2 250 175) $p_2)))
(define cat_awodey.html
  (TnTmPrelude
   #:title "范畴论笔记"
   #:css "styles.css"
   (H1. "范畴论笔记")
   (P "现在的是重写的版本, 但在某种意义上更接近于翻译"
      "Category Theory (Steve Awodey) 而非笔记了.")
   (H2 "翻译术语讨论")
   (P "翻译这样一本关于范畴论的书籍, 不论如何在术语方面都值得斟酌再三, "
      "我将我的一些选择写在这里.")
   (P "对于collection, 在某种意义上它其实就是集合论里的class, "
      "只不过在使用时作者又不想太过形式化而已. 也就是说, collection可能指的是set, "
      "也可能指的是proper class. 我有意将collection翻译成了" (Q "合集")
      ", 许多书籍将其同set一样也翻译为集合, 这不免有时给读者带来疑惑. "
      "当然, 在本书的第一章, 作者Steve Awodey是有意避免一开始就变得technical的, "
      "所以熟悉数学基础和集合论的读者对于开头一些成问题的表述也不要感到惊讶, "
      "然而作者还是不免在第一章的最后讨论了一些数学基础方面的事情, "
      "这完全是必要的也无法省略的内容.")
   (P "英文里某些简单表达在中文里没有那么一致的对应, 我有点倾向于机械化翻译, "
      "以使读者能够辨识出来. 当然了, 不机械化的翻译也有很多. "
      "首先是" (Q "either...or...") ", 虽然我将其翻译为了"
      (Q "要么..., 要么...") ", 但是这并不意味着这二者只居其一, "
      "实际上就是" (Q "或者") "的意思. 其次是" (Q "in particular, ...")
      ", 它的实际用法相当于接下来要举一个符合情况的但是具体且特殊的例子, "
      "我以前总是将其翻译为" (Q "特别地, ...") ", 但是并不通顺, 而且也不容易理解. "
      "现在有时我也会意译, 例如将其翻译为" (Q "举一个例子, ...") ".")
   (P (Q "underlying set") "是容易理解的, 大概就是某个结构或者范畴的集合部分, "
      "或者说遗忘了结构而剩下的集合. 不过, 我觉得并不容易翻译妥当, 最终我选择翻译为"
      (Q "潜在集") ", 我想这不至于引起什么误解. 类似地, 一个结构化集合之间的同态, "
      "在遗忘了结构之后, 可以成为一个" (Q "潜在函数") ".")
   (H2 "勘误")
   (P "每次进行什么翻译工作的时候, 我总是到很后来才意识到整理勘误的必要性. "
      "然而, 每次到那个时候, 之前找到的笔误往往都已经全然忘记了. "
      "这真是令人没有办法的事情. 当然, 整理总比不整理好.")
   
   (H2 "前言")
   (P "当我们已经有了Mac Lane的" (Em "Categories for the Working Mathematician")
      ", 为什么还要写一本新的范畴论教科书呢? "
      "简而言之, 因为Mac Lane的书是为工作的数学家准备的. "
      "在范畴论渗透进各种各样的领域并出现在课程里30年之后, "
      "现在需要的是一本供所有人阅读的书籍.")
   (P "这本书成长于我过去十年间在CMU教授的范畴论课程. "
      "在这段时间里, 我已经为计算机科学, 数学, 逻辑学的本科生和研究生"
      "教授了无数的讲座课程或者高级讨论班. "
      "基于本书材料的讲座课程由15个星期每周两节的90分钟讲座构成. "
      "这些讲座(材料)的胚芽是我自己的研究生笔记, "
      "来源于Mac Lane在芝加哥大学所教授的范畴论课程. "
      "在教授我自己的课程时, 我很快发现CMU这里的混合的学生群体"
      "和芝加哥大学的数学研究生的需求非常不同, "
      "而我寻找满足这些需求的合适教科书的过程表明"
      "现有的文献和学生的需求之间存在严重的沟壑. "
      "我的讲义随着时间逐渐演化以填补这个沟壑, "
      "补充并最终替代了我尝试使用的诸多教材.")
   (P "我的课程的学生往往没有什么数学背景, "
      "只上过一门离散数学, 以及一些微积分或者线性代数"
      "或者是一两门逻辑学课程. 然而, 当学生最终成为计算机科学或者逻辑学的研究者时, "
      "许多人都需要熟悉范畴论的基本概念, 而且是在没有接受许多深入的数学训练的情况下. "
      "数学系的本科生其实也是类似的: 在数学上很有天分, "
      "并且因其与他们之后所需要学习的东西有着明显的联系而备受鼓舞, "
      "尽管如此还是不能跟着Mac Lane的书, 因为他们仍然缺乏必要的数学准备. "
      "我的大多数学生甚至还不知道自由群是什么, "
      "所以说当他们知道这是伴随的一个例子时也不会受到启发.")
   (P "因此, 这本书意在成为范畴论的教科书和参考, "
      "不仅面向数学系学生, 而且也面向计算机科学, 逻辑学, 语言学, 认知科学, 哲学"
      "以及任何需要用到范畴论的领域的研究者和学生. "
      "对于我来说挑战在于能够使得基本定义, 定理, 以及证明技术被这样的读者群体理解, "
      "因此无法假定读者熟悉范畴论在代数学和拓扑学中的主要 (或者说至少是原初的) 应用. "
      "然而这不意味着我会在真空中建立主题, 简单跳过例子和应用. "
      "这种抽象层次的材料在没有应用和例子使之变得鲜活的情况下直接无法理解.")
   (P "面对这种两难的境地, 我采取了这样的策略. 首先从最开始细致地建立几个基本的例子, "
      "例如偏序集和幺半群, 然后带着这些例子一起, 并在整本书里一直使用它们. "
      "这在教学上有诸多值得提及的优点: 偏序集和幺半群本身就是特殊种类的范畴, "
      "并且在某种意义上代表了一个一般性的范畴所具有的两个" (Q "维度")
      " (对象和箭头). 诸多范畴里出现的现象当作来源于偏序集或者幺半群"
      "的什么东西的泛化来理解是最好的. 从另一个角度来说, "
      "偏序集 (和单调映射) 和幺半群 (和同态) 的范畴提供了两个更进一步的"
      "而且又相当不同的范畴的例子, 其可以用来考虑各种各样的概念. "
      "例如, 极限的概念既可以在一个给定的偏序集里考虑, 也可以在偏序集的范畴里考虑.")
   (P "当然了, 许多偏序集和幺半群之外的其他例子也有处理. "
      "例如, 关于群和范畴的一章建立了群论的最初几步, 直至核, 商群, 以及同态定理, "
      "作为等化子和余等化子的例子. 这里, 以及偶尔别的什么地方 (例如与Stone对偶联系起来), "
      "我有意涵盖了更多的一点数学, 超出了用于刻画手头概念所严格必要的范围. "
      "我的想法在于当一些学生将要参加更加高等的数学课程时, "
      "这些或许是离他们最近的, 因此他们应该能够通过收获一些低垂的果实"
      "来从学习范畴论的努力中获益.")
   (P "尽管所需要的数学预备知识比Mac Lane的书要低得多, "
      "(我希望)标准的严格性并没有妥协. "
      "所有重要命题和定理的完整证明都已给出, "
      "只是偶尔常规的引理会留作练习 (然后它们通常被列在一章的最后). "
      "材料的选取是容易的, 存在必须涵盖的标准核心: "
      "范畴, 函子, 自然变换, 等价, 极限和余极限, 函子范畴, "
      "可表, Yoneda引理, 伴随, 以及单子. "
      "这近乎填满了一个课程. 这里所涵盖的唯一算是" (Q "可选")
      "的话题是笛卡尔闭范畴和" $lambda "演算, "
      "但是这对于计算机科学家, 逻辑学家, 语言学家而言又是必要的. "
      "一些显然更加深入的话题被有意地忽略了: 2-范畴, 意象 (topos) (任何深度上), "
      "幺半范畴. 这些话题在Mac Lane中得到处理, "
      "而学生在完整参与这个课程之后应该有能力阅读此书.")
   (H2. "范畴")
   (H3. "引论")
   (P (Em "什么是范畴论?") " 作为第一次近似, "
      "我们可以说范畴论是对于" (Em "函数的代数")
      "的研究. 正如群论是对于集合的置换或者"
      "几何对象的对称变换这一类系统的思想的抽象, "
      "范畴论来源于一些对象之间的函数所构成的系统的理念.")
   (P "我们将复合" (&compose $g $f) "想成是某种函数"
      $f "和" $g "的" (Q "积") ", 然后考虑由函数的合集所引申出的那种抽象"
      (Q "代数") ". 一个范畴正是这样一种" (Q "代数")
      ", 其由对象" (&cm $A $B $C $..h) "和箭头"
      (&cm (Arrow $f $A $B) (Arrow $g $B $C) $..h)
      "构成, 而且箭头在函数复合之下封闭并满足一些对于函数复合而言"
      "十分典型的特定条件. 精确的定义在本章的之后给出.")
   todo.svg
   (P "作为抽象代数的分支, 范畴论的发明遵循着Felix Klein的"
      (Em "Erlanger纲领") "的传统, 作为一种基于数学结构之间的"
      (Q "可容许 (admissible) 变换") "来对于不同数学结构进行研究和刻画的方法. "
      "范畴这种一般性的概念提供了对于" (Q "保持结构的变换")
      "这一想法的刻画, 并由此提供了对于承载这样的变换的一类结构的刻画.")
   (P "这个学科的历史发展大致如下:"
      (Ul (Li "1945: Eilenberg和Mac Lane的"
              (Q "General theory of natural equivalences")
              "是原初的论文, 这是对于范畴论的最早描述.")
          (Li "1940年代末: 主要的应用最初是代数拓扑, "
              "特别是同调论和抽象代数.")
          (Li "1950年代: A. Grothendieck et al.开始使用范畴论, "
              "在代数几何领域取得了极大的成功.")
          (Li "1960年代: F. W. Lawvere和其他人开始应用范畴论于逻辑学, "
              "揭示了一些深刻而令人意外的联系.")
          (Li "1970年代: 范畴论的应用已经出现于计算机科学, "
              "语言学, 认知科学, 哲学, 以及其他诸多领域.")))
   (P "关于这个领域的一件非常引人注目的事情在于其有着如此广阔的应用. "
      "事实上, 它就像集合论一样是一种普遍性的数学语言. "
      "由于拥有各种各样的应用, 范畴论往往也能揭示不同领域之间的特定联系"
      "&mdash;&mdash;就像逻辑和几何. 举个例子, " (Em "伴随函子")
      "这一重要概念在逻辑中以存在量化子出现, "
      "在拓扑中则以沿着连续函数取像 (image) 的运算出现. "
      "从范畴的角度而言, 这些本质上是相同的操作.")
   (P "实际上, 伴随函子应该是读者学习本书时所应该掌握的主要东西之一. "
      "这个纯粹范畴论的概念已经被证明是第一级的概念工具&mdash;&mdash;"
      "其重要性可与连续函数的想法比肩.")
   (P "实际上, 正如拓扑空间的想法起源于其与连续函数的联系, "
      "范畴的概念是为了定义什么是函子而产生的, "
      "至少根据范畴论的发明者之一的说法是这样. "
      "故事还可以继续下去, 函子的概念是为了定义自然变换而产生的. "
      "我们或许还可以继续下去, 自然变换是为了定义伴随而生的:"
      (Table #:attr* '((style "margin: auto; text-align: center;"))
             (Tr (Td "范畴"))
             (Tr (Td "函子"))
             (Tr (Td "自然变换"))
             (Tr (Td "伴随")))
      "诚然如此, 而这给出了本书的大纲.")
   (P "在正式学习之前, 或许我们应该问问为什么范畴论有着如此广泛而深远的应用. "
      "既然我们说过其实函数的抽象理论, 所以说答案就是:"
      (Div #:attr* '((style "text-align: center;"))
           (Em "函数无处不在!"))
      "并且哪里有函数, 哪里就有范畴. 的确如此, 也许这个学科应该叫做"
      (Em "抽象函数论") ", 或者更好的名字是: " (Em "箭术(archery)") ".")
   (H3. "集合之间的函数" #:id "functions-of-sets")
   (P "我们从考虑集合之间的函数开始. "
      "我不打算在这里说明什么是函数, 就像我也不会说明什么是集合一样, "
      "转而我们将会默认读者拥有对于这些术语的实际可行的理解. "
      "它们实际上可以使用范畴论来" (Em "定义")
      ", 但是这并非我们在这里的目的.")
   (P "令" $f "是从一个集合" $A "到另一个集合" $B "的一个函数, 我们记作"
      (MB (Arrow $f $A $B) ".")
      "以显式的语言来说, 这意味着" $f "定义在整个的" $A "上而"
      $f "的所有值都在" $B "之中. 以集合论术语而言, 就是"
      (MB (&sube (&range $f) $B) ".")
      "现在设我们也有一个函数" (Arrow $g $B $C) ","
      todo.svg
      "那么存在着一个作为复合的函数"
      (Arrow (&compose $g $f) $A $C) ", 由"
      (MBL "(1.1)"
           (&cm (&= (app (@compose $g $f) $a)
                    (app $g (app $f $a)))
                (∈ $a $A)))
      "给出. 现在这个函数复合的运算" (Q $compose)
      "是结合性的, 在于如果我们还有一个函数"
      (Arrow $h $C $D)
      todo.svg
      "并构成" (&compose $h $g) "和" (&compose $g $f)
      ", 那么我们像以上图表所指示的那样比较"
      (&compose (@compose $h $g) $f) "和"
      (&compose $h (@compose $g $f))
      ". 实际上, 这两个函数总是等同的,"
      (MB (associate &compose $h $g $f))
      "因为对于任意的" (∈ $a $A) ", 我们有"
      (MB (&= (app (@compose (@compose $h $g) $f) $a)
              (app* $h $g $f $a)
              (app (@compose $h (@compose $g $f)) $a)))
      "这使用了(1.1).")
   (P "顺便值得一提的是, 两个函数相等的含义: "
      "对于每个参数, 它们有着相同的值.")
   (P "最后, 注意到每个集合" $A "都拥有一个恒等函数"
      (MB (Arrow $1_A $A $A))
      "其由"
      (MB (&= (app $1_A $a) $a))
      "给出.")
   (P "这些恒等函数表现为复合运算" $compose "的" (Q "单位元")
      ", 抽象代数意义下的. 也就是说, 对于任意的"
      (Arrow $f $A $B) ", 我们有"
      (MB (&= (&compose $f $1_A)
              $f
              (&compose $1_B $f)) "."))
   todo.svg
   (P "所有这些关于集合之间的函数的性质是我们想要对于函数的"
      (Em "抽象") "概念所考虑的: 复合与恒元. "
      "因此, 可以说现在我们想要" (Q "抽象掉")
      "其他所有东西, 这正是由以下定义所完成的.")
   (H3. "范畴的定义")
   ((Definition)
    "一个" (Em "范畴") "由以下数据构成:"
    (Ul (Li (Em "对象") ": " (&cm $A $B $C $..h))
        (Li (Em "箭头") ": " (&cm $f $g $h $..h))
        (Li "对于每个箭头" $f ", 可以给出两个对象"
            (MB (&cm (&dom $f) (&cod $f)))
            "其被称为" $f "的" (Em "定义域(domain)")
            "和" (Em "陪域(codomain)") ". 我们记"
            (MB (Arrow $f $A $B))
            "以指明" (&= $A (&dom $f)) "而"
            (&= $B (&cod $f))
            ". {译注: 定义域和陪域是一个箭头所内蕴的信息.}")
        (Li "给定箭头" (Arrow $f $A $B) "和" (Arrow $g $B $C)
            ", 即满足"
            (MB (&= (&cod $f) (&dom $g)))
            "那么可以给出一个箭头"
            (MB (Arrow (&compose $g $f) $A $C))
            "其被称为" $f "和" $g "的"
            (Em "复合(composite)") ".")
        (Li "对于每个对象" $A ", 可以给出一个箭头"
            (MB (Arrow $1_A $A $A))
            "其被称为" $A "的"
            (Em "恒等箭头(identity arrow)") "."))
    "这些数据必须满足以下法则:"
    (Ul (Li "结合性:"
            (MB (associate &compose $h $g $f))
            "对于所有"
            (&cm (Arrow $f $A $B)
                 (Arrow $g $B $C)
                 (Arrow $h $C $D))
            "成立.")
        (Li "单位元:"
            (MB (&= (&compose $f $1_A)
                    $f
                    (&compose $1_B $f)))
            "对于所有" (Arrow $f $A $B) "成立."))
    "一个范畴可以是满足这个定义的" (Em "任意东西")
    "&mdash;&mdash;并且很快我们就要拥有大量的例子. "
    "暂时我想要强调的是, 和" (Ref "functions-of-sets")
    "中的情况不同, 对象不必是集合, 箭头也不必是函数. "
    "一个范畴在这种意义下是函数或者说" (Q "箭头")
    " (有时也称为" (Q "态射") ") 的一个" (Em "抽象")
    "代数, 以复合运算" (Q $compose)
    "为原语. 如果你熟悉群, 那么你或许可以将一个范畴"
    "想成是某种一般化了的群.")
   (H3. "范畴的例子" #:id "examples-of-categories")
   (Ol (Li "我们已经遇到过了集合和函数的范畴" Sets
           ". 这里也有另一个范畴"
           (MB Sets_fin)
           "其由所有的有限集合以及其间函数构成." (Br)
           "诚然, 还有许多像这样的范畴, "
           "其由限制能够成为对象的集合与能够成为箭头的函数给出. "
           "例如, 取有限集合为对象而单射函数为箭头. "
           "既然单射函数的复合仍是单射函数, "
           "并且恒等函数是单射的, "
           "所以说这也给出了一个范畴." (Br)
           "如果我们取集合为对象而取满足以下条件的函数"
           (Arrow $f $A $B) "为箭头: 对于每个"
           (∈ $b $B) ", 子集"
           (MB (&sube (app (inv $f) $b) $A))
           "至多只有两个元素 (而不是一个), "
           "这仍然是一个范畴吗? "
           "{译注: 显然不是, 因为这样的函数对于复合运算不封闭.} "
           "若取" (app (inv $f) $b) "有限的函数呢? "
           "无限的情况呢? 存在着许多这样的"
           "由集合与函数构成的限制范畴.")
       (Li "另一种我们经常在数学中见到的例子是"
           (Em "结构化集合(structured set)") "的范畴, "
           "即由带有更进一步" (Q "结构") "的集合和"
           (Q "保持这结构") "的函数构成的范畴, "
           "其中这些概念以某种独立的方式确定. "
           "你可能会熟悉的这类例子有"
           (Ul (Li "群和群同态;")
               (Li "向量空间和线性映射;")
               (Li "图和图同态;")
               (Li "实数集" $RR "和连续函数" (&-> $RR $RR) ";")
               (Li "开集" (&sube $U $RR) "和定义于其上的连续函数"
                   (Arrow $f $U (&sube $V $RR)) ";")
               (Li "拓扑空间和连续映射;")
               (Li "可微流形和光滑映射;")
               (Li "自然数集" $NN "和所有的递归函数" (&-> $NN $NN)
                   ", 或者就像连续函数的例子一样, "
                   "我们可以取定义于子集" (&sube $U $NN)
                   "上的部分递归函数;")
               (Li "偏序集和单调函数."))
           "即便你还不熟悉以上的其中一些例子, 也请不要担心, "
           "暂且让我们仅是更细致地考虑以上的最后一个例子.")
       (Li "一个偏序集 (partially ordered set) 或者说"
           (Em "poset") "是一个装备了一个二元关系"
           (&<=_A $a $b) "的集合" $A ", 其对于所有的"
           (∈ $a $b $c $A) "满足以下条件:"
           (Ul (Li "自反性: " (&<=_A $a $a) ";")
               (Li "传递性: 如果" (&<=_A $a $b) "且"
                   (&<=_A $b $c) ", 那么" (&<=_A $a $c) ";")
               (Li "反对称性: 如果" (&<=_A $a $b) "且"
                   (&<=_A $b $a) ", 那么" (&= $a $b) "."))
           "例如, 装备有通常的序关系" (&<= $a $b)
           "的实数集" $RR "构成了一个偏序集, 其也是"
           (Em "线性(linearly)") "序的: 对于任意的"
           (&cm $x $y) ", 要么" (&<= $x $y)
           ", 要么" (&<= $y $x) ". {译注: 亦可兼而有之, "
           "当且仅当" (&= $x $y) "时.}" (Br)
           "从一个偏序集" $A "到一个偏序集" $B
           "的一个箭头是一个函数"
           (MB (Arrow $m $A $B))
           "其为" (Em "单调的(monotone)")
           ", 意即对于所有的" (∈ $a $a^ $A) ", 我们有"
           (MB (&<=_A $a $a^) "可以推出"
               (&<=_B (app $m $a) (app $m $a^)) ".")
           "要使得这成为一个范畴需要满足什么条件呢? "
           "我们需要知道" (Arrow $1_A $A $A)
           "是单调的, 但这是显然的, 因为"
           (&<=_A $a $a^) "可以推出" (&<=_A $a $a^)
           ". 我们也需要知道, 如果" (Arrow $f $A $B)
           "和" (Arrow $g $B $C) "是单调的, 那么"
           (Arrow (&compose $g $f) $A $C)
           "也是单调的. 这同样成立, 因为"
           (&<= $a $a^) "可以推出"
           (&<= (app $f $a) (app $f $a^)) "可以推出"
           (&<= (app* $g $f $a) (app* $g $f $a^)) "可以推出"
           (&<= (app (@compose $g $f) $a)
                (app (@compose $g $f) $a^))
           ". 因此, 我们有偏序集和单调函数的范畴" Pos ".")
       (Li "到目前为止我们已经考虑了的范畴是有时叫做"
           (Em "具体范畴(concrete category)")
           "的东西的例子. 非形式化地说, "
           "存在着这样的范畴, 其对象是集合, "
           "可能装备有某种结构, 而箭头是特定的函数, "
           "可能保持结构 (我们将在之后看到这个想法"
           "并非逻辑一致的; 见" (Ref "concrete-remark")
           "). 但是, 实际上一种理解范畴论的方式全然在于"
           (Q "不用元素 (doing without elements)") "而代之以箭头. "
           "让我们来看看一些例子, "
           "其中这种观点并不仅是可选的, 而是不可避免的." (Br)
           "令" Rel "是以下范畴: 取集合为对象而二元关系为箭头. "
           "也就是说, 一个箭头" (Arrow $f $A $B)
           "是一个任意的子集" (&sube $f (&c* $A $B))
           ". 一个集合" $A "上的恒等箭头即是恒等关系,"
           (MB (&= $1_A
                   (&sube (setI (∈ (tu0 $a $a)
                                   (&c* $A $A))
                                (∈ $a $A))
                          (&c* $A $A))) ".")
           "给定" (&sube $R (&c* $A $B)) "和"
           (&sube $S (&c* $B $C)) ", 我们可以根据"
           (MB (∈ (tu0 $a $c) (&compose $S $R))
               "当且仅当"
               (∃ $b (&& (∈ (tu0 $a $b) $R)
                         (∈ (tu0 $b $c) $S))))
           "定义复合" (&compose $S $R)
           ", 即" $R "和" $S "的" (Q "关系积 (relative product)")
           ". 我们将表明" Rel "确为一个范畴的工作留作练习. "
           "(要做什么呢?)" (Br)
           "要举另外一个箭头并非" (Q "函数") "的范畴的例子, "
           "令对象为有限集合" (&cm $A $B $C) "而一个箭头"
           (Arrow $F $A $B) "是一个由自然数构成的矩阵"
           (&= $F (_cm (@_cm $n $i $j) (&< $i $a) (&< $j $b)))
           ", 其中" (&= $a (card $A)) "而" (&= $b (card $B))
           ". 符号" (card $C) "表示一个集合" $C "的元素数目. "
           "箭头的复合无非是通常的矩阵乘法, "
           "而恒等箭头即是通常的单位矩阵. "
           "这里的对象的目的仅是为了保证矩阵乘法有定义, "
           "但是矩阵并非对象之间的函数.")
       (Li (Em "有限范畴") (Br)
           "当然了, 范畴的对象也不必是集合. "
           "以下是一些非常简单的例子:"
           (Ul (Li "范畴" Cat1 "模样如下:"
                   (MB $*)
                   "其有一个对象和一个该对象的恒等箭头, "
                   "恒等箭头我们没有画出来.")
               (Li "范畴" Cat2 "模样如下:"
                   (MB $* (long-arrow "6em") $Star)
                   "其有两个对象, 必要的恒等箭头, 以及恰好一个"
                   "(不同)对象之间的箭头.")
               (Li "范畴" Cat3 "模样如下:"
                   todo.svg
                   "其有三个对象, 必要的恒等箭头, "
                   "恰好一个从第一个对象到第二个对象的箭头, "
                   "恰好一个从第二个对象到第三个对象的箭头, 以及"
                   "恰好一个从第一个对象到第三个对象的箭头 "
                   "(因而其为前两个箭头的复合).")
               (Li "范畴" Cat0 "模样如下:"
                   (MB "")
                   "其既没有对象, 也没有箭头."))
           "和上面一样, 从现在开始绘制范畴时我们都会省略恒等箭头." (Br)
           "描述有限范畴是容易的&mdash;&mdash;只需要取一些对象"
           "并在这些对象之间放置箭头, 但是请务必保证放置了"
           "由范畴论的公理所要求的必要的恒等箭头还有复合. "
           "而且, 如果存在任何的环路 (loop) 的话, "
           "那么我们就需要通过等式来截断环路以保持范畴的有限性. "
           "例如, 考虑以下描述:"
           (MB $A (__^^ $rlarr (pad $g) (pad $f)) $B)
           "除非我们明确规定诸如" (&= (&i* $g $f) $1_A)
           "这样一个等式 {译注: 大概还需要规定"
           (&= (&i* $f $g) $1_B)
           "}, 不然我们就会拥有无穷多的箭头: "
           (&cm (&i* $g $f) (&i* $g $f $g $f)
                (&i* $g $f $g $f $g $f) $..h)
           ". 当然, 这仍然是一个范畴, 但并非一个"
           (Em "有限") "范畴. 本章之后讨论自由范畴时, "
           "我们还会回到这个(无限的)情况上来.")
       (Li "范畴论的一个重要口号是"
           (Div #:attr* '((style "text-align: center;"))
                (Em "箭头才是真正重要的东西!"))
           "因此, 我们也应该检视范畴之间的箭头或者"
           (Q "映射") ". " (Q "范畴的同态") "被称为函子."))
   ((Definition)
    "范畴" CatC "和范畴" CatD "之间的一个" (Em "函子")
    (MB (Functor $F CatC CatD))
    "是一个从对象到对象而从箭头到箭头的映射, 其满足"
    (Ol #:attr* '((type "a"))
        (Li (&= (app $F (Arrow $f $A $B))
                (Arrow (app $F $f) (app $F $A) (app $F $B))) ";")
        (Li (&= (app $F $1_A) (_ $1 (app $F $A))) ";")
        (Li (&= (app $F (&compose $g $f))
                (&compose (app $F $g) (app $F $f))) "."))
    "换言之, " $F "保持定义域和陪域, 恒等箭头, 以及复合. "
    "因此, 一个函子" (Functor $F CatC CatD)
    "给出了某种" CatC "于" CatD "中的(可能扭曲的)"
    (Q "图景(picture)") "."
    todo.svg
    "现在, 我们可以轻易地看出函子能够按照预期的方式复合, "
    "而每个范畴" CatC "都拥有一个恒等函子"
    (Functor (_ $1 CatC) CatC CatC)
    ". 于是, 我们有了另一个范畴的例子, 叫做"
    Cat ", 其是由所有的范畴和函子构成的范畴.")
   (Ol #:attr* '((start "7"))
       (Li "一个" (Em "预序(preorder)")
           "是一个装备了一个满足自反性和传递性的二元关系"
           (&<= $p $q) "的集合: " (&<= $a $a)
           ", 并且如果" (&<= $a $b) "且" (&<= $b $c)
           ", 那么" (&<= $a $c) ". 任意的预序" $P
           "都可以被视为一个范畴, 只需要取对象为"
           $P "的元素而两个元素之间唯一可能的箭头是"
           (MBL "(1.2)"
                (&-> $a $b) "当且仅当" (&<= $a $b) ".")
           $<= "的自反和传递条件保证了这的确是一个范畴." (Br)
           "从另一个方向上来说, 如果一个范畴满足其任意两个对象"
           "之间至多只有一个箭头, 那么其就确定了一个预序, "
           "只需要根据(1.2)来定义一个对象上的二元关系"
           $<= "即可.")
       (Li "一个偏序集显然是一个满足额外的反对称条件的预序: "
           "如果" (&<= $a $b) "且" (&<= $b $a) ", 那么"
           (&= $a $b) ". 因此, 一个偏序集也是一个范畴. "
           "这样的" (Em "偏序集范畴(poset category)")
           "很是常见; 例如, 对于任意的集合" $X
           ", 幂集" (powerset $X) "在通常的" $X
           "的子集" (&cm $U $V) "之间的包含关系"
           (&sube $U $V) "下是一个偏序集." (Br)
           "偏序集范畴" $P "和" $Q "之间的函子"
           (Functor $F $P $Q) "是什么样的呢? "
           "其必须满足恒等律和复合律... 显然, "
           "这些只是之前已经考虑过了的单调函数." (Br)
           "将一个范畴想成是某种" (Em "广义偏序集(generalized poset)")
           "常常是有用的, 也就是带有比" (&<= $p $q)
           "带有" (Q "更多结构") "的东西. 因此, "
           "我们也可以将函子想成是某种广义单调映射.")
       (Li (Em "来源于拓扑的一个例子")
           ": 令" $X "是一个以" (open $X)
           "为开集族的拓扑空间. 以包含关系排序, "
           (open $X) "就成了一个偏序集范畴. "
           "而且, " $X "的点可以根据" (Em "特殊化程度(specialization)")
           "进行预序化, 通过设置" (&<= $x $y)
           "当且仅当对于每个开集" $U ", "
           (∈ $x $U) "可以推出" (∈ $y $U)
           ", 也就是" $y "包含于每个包含" $x
           "的开集之中. 如果" $X "是充分分离的 ("
           (Q $T_1) "), 那么这个序关系是平凡的, "
           "但是其他情况下可能会变得相当有趣, "
           "例如对于代数几何和指称语义的空间而言. "
           "作为练习, 请证明" $T_0
           "空间在此特殊化序下实际上是偏序集. "
           "{译注: 特殊化程度可以这么理解, 因为包含"
           $y "的开集比包含" $x "的开集更多, 所以"
           $y "比" $x "更加特殊.}")
       (Li (Em "来源于逻辑的一个例子")
           ": 给定一个逻辑演绎系统, 存在一个与之相关的"
           (Em "证明范畴(category of proofs)")
           ", 其中的对象为公式:"
           (MB (&cm $phi $psi $..h))
           "从" $phi "到" $psi "的一个箭头是一个从"
           "(uncanceled)假设" $phi "到" $psi "的推导."
           (MB (&rule* $phi $..v $psi))
           "箭头的复合即将这样的推导以显然的方式并置, "
           "当然这是结合性的. (恒等箭头" $1_phi
           "应该是什么呢?) 我们观察到可以存在诸多不同的箭头"
           (MB (Arrow $p $phi $psi))
           "因为或许存在着诸多不同的证明. "
           "这种范畴实际上有着丰富的结构, "
           "之后我们将与" $lambda "演算一起考虑.")
       (Li (Em "来源于计算机科学的一个例子")
           ": 给定一个函数式语言" $L
           ", 存在一个与之相关的范畴, 其对象是"
           $L "的数据类型, 而箭头是" $L
           "的可计算函数 (" (Q "进程 (process)")
           ", " (Q "过程 (procedure)") ", "
           (Q "程序 (program)") "). 两个这样的程序的复合"
           (ARROW $X $f $Y $g $Z) "由应用" $g
           "于" $f "的输出得到, 有时亦记作"
           (MB (&= (&compose $g $f) (&\; $f $g)) ".")
           "恒等箭头即" (Q "什么也不做 (do nothing)")
           "的程序." (Br)
           "这样的范畴对于编程语言的指称语义的想法而言是基本的. "
           "例如, 如果" (app CatC $L) "是刚才定义的范畴, "
           "那么语言" $L "于一个Scott domain的范畴" CatD
           "中的指称语义不过就是一个函子"
           (MB (Functor $S (app CatC $L) CatD))
           "因为" $S "赋予了" $L "的类型以domain, "
           "而程序以连续函数. 这个例子和前一个例子都和"
           "我们之后要考虑的"
           (Q "笛卡尔闭范畴 (cartesian closed category)")
           "的概念有关. {译注: 这里的domain有特殊的含义, "
           "其来源于指称语义的理论, "
           "并不是(函数或者箭头的)定义域或者(逻辑学的)论域的意思, "
           "故我倾向于保留原文不动.}")
       (Li "令" $X "是一个集合. 我们可以将" $X
           "当作一个范畴" (app Dis $X) ", 其取"
           $X "的元素为对象而箭头仅是必要的恒等箭头, "
           "也就是对于每个" (∈ $x $X) "都有一个. "
           "这种箭头仅是恒等箭头的范畴被称为是"
           (Em "离散的(discrete)")
           ". 注意到离散范畴非常不过是非常特殊的偏序集.")
       (Li "一个" (Em "幺半群(monoid)")
           " (有时也被称为" (Em "semigroup with unit")
           ") 是一个集合" $M ", 其装备了一个二元运算"
           (Arrow $d* (&c* $M $M) $M) "和一个突出的"
           (Q "单位元") (∈ $u $M) ", 满足对于所有的"
           (∈ $x $y $z $M) ", 都有"
           (MB (associate &d* $x $y $z))
           "和"
           (MB (&= (&d* $u $x) $x (&d* $x $u)))
           "成立. 等价地, 一个幺半群不过就是一个仅有一个对象的范畴. "
           "这个范畴的箭头即是幺半群的元素. "
           "特别地, 恒等箭头即是单位元素" $u
           ". 箭头的复合即是幺半群的二元运算" (&d* $m $n) "." (Br)
           "幺半群是非常普遍的. 这里有数字构成的幺半群, "
           "例如装备了加法和" $0 "或者乘法和" $1 "的"
           $NN ", " $QQ ", 还有" $RR ". 不过, 对于任意的集合"
           $X ", 由所有从" $X "到" $X "的函数构成的集合, 记作"
           (MB (Hom Sets $X $X))
           "在复合运算下也是一个幺半群. 更一般地, 对于任意范畴"
           CatC "中的对象" $C ", 从" $C "到" $C "的箭头的集合, 记作"
           (Hom CatC $C $C) ", 在" CatC "的复合运算下是一个幺半群." (Br)
           "既然幺半群是结构化集合, 那么存在一个范畴" Mon
           ", 其对象是幺半群而箭头是保持幺半群结构的函数. "
           "更细致地说, 一个从幺半群" $M "到幺半群" $N
           "的同态是一个函数" (Arrow $h $M $N)
           ", 其满足对于所有的" (∈ $m $n $M) ", 都有"
           (MB (&= (app $h (&d*_M $m $n))
                   (&d*_N (app $h $m) (app $h $n))))
           "和"
           (MB (&= (app $h $u_M) $u_N))
           "成立. 我们观察到一个从" $M "到" $N
           "的幺半群同态和一个从被视为范畴的" $M
           "到被视为范畴的" $N "的函子是相同的东西. "
           "在这种意义下, 范畴也是广义的幺半群 (generalized monoid), "
           "而函子是广义的同态."))
   (H3. "同构" #:id "isomorphism")
   ((Definition)
    "在任意的范畴" CatC "中, 一个箭头" (Arrow $f $A $B)
    "被称为一个" (Em "同构(isomorphism)")
    ", 如果存在另一个箭头" (Arrow $g $B $A) "满足"
    (MB (&= (&compose $g $f) $1_A) "且"
        (&= (&compose $f $g) $1_B) ".")
    "既然逆是唯一的 (请证明!), 我们记" (&= $g (inv $f))
    ". 我们称" $A "同构于" $B ", 记作" (&cong $A $B)
    ", 如果它们之间存在一个同构.")
   (P "同构的定义我们第一个对于重要的概念进行"
      (Em "抽象") "的范畴论式的定义的例子. "
      "抽象的含义在于其只使用了范畴论的概念, "
      "而并不诉诸于关于对象和箭头的一些额外信息. "
      "相较于其他可能的定义, 其优点在于它适用于任意的范畴. "
      "例如, 人们有时将集合 (幺半群, 等等) 的同构定义为"
      (Em "双射(bijective)") "函数 (相应地, 双射的同态), 即满足"
      (Q "1-1和onto") "的东西&mdash;&mdash;这用到了对象的"
      (Em "元素") ". {译注: " (Q "1-1和onto")
      "是英语曾经对于单射和满射的表达, "
      "但是自从Bourbaki学派兴起之后, "
      "一般人都用injective和surjective了.} "
      "在某种情形下, 这" (Em "等价于")
      "我们的定义, 例如集合和幺半群. "
      "但是我们也应该注意到, 例如在" Pos
      "之中, 非同构的偏序集之间也存在着"
      (Q "双射的同态") ". 而且, 在诸多情形之下, "
      (Em "只有") "抽象的定义能够成立 (make sense), "
      "例如将幺半群视为范畴的情形.")
   ((Definition)
    "一个" (Em "群(group)") $G "是一个幺半群, "
    "其每个元素" $g "都有一个逆" (inv $g)
    ". 因此, " $G "是一个只有一个对象的范畴, "
    "其每个箭头都是一个同构.")
   (P "自然数集" $NN "在加法或者乘法下都不能构成一个群, "
      "但是整数集" $ZZ "在加法下能够形成一个群, "
      "正有理数集" $QQ^+ "在乘法下也能够形成一个群. "
      "对于任意的集合" $X ", 我们有由" $X
      "的自同构 (或者说" (Q "置换") ") 构成的群"
      (Aut $X) ", 即由同构" (Arrow $f $X $X)
      "构成的群. (为什么这在" (Q $compose)
      "下封闭?) 一个" (Em "置换群(group of permutations)")
      "是对于某个集合" $X "而言的子群"
      (&sube $G (Aut $X)) ", 也就是由" $X
      "的(某些)自同构构成的群. 因此, 集合"
      $G "必须满足以下条件:"
      (Ol (Li $X "上的恒等函数" $1_X
              "在" $G "之中.")
          (Li "如果" (∈ $g $g^ $G)
              ", 那么" (∈ (&compose $g $g^) $G) ".")
          (Li "如果" (∈ $g $G)
              ", 那么" (∈ (inv $g) $G) ".")))
   (P "一个群的" (Em "同态") (Arrow $h $G $H)
      "不过就是一个幺半群同态, "
      "由此则必然保持逆 (请证明!).")
   (P "现在请让我们考虑以下关于抽象群的基本而经典的结果.")
   ((Theorem #:auto? #f)
    (B "(Cayley). ")
    (Em "每个群" $G "都同构于一个置换群."))
   ((proof)
    "(证明大纲)"
    (Ol (Li "首先, 定义" $G "的Cayley表示"
            (OverBar $G) "为以下的集合的置换的群: "
            "这个集合就是" $G "自身, 而对于每个元素" (∈ $g $G)
            ", 我们有置换" (Arrow (OverBar $g) $G $G)
            ", 其定义于所有" (∈ $h $G) "之上, 相当于"
            (Q "作用于左 (acting on the left)") ":"
            (MB (&= (app (OverBar $g) $h)
                    (&d* $g $h)) ".")
            "这的确是一个置换, 因为其有着" (inv $g)
            "的作用为逆.")
        (Li "接着, 根据" (&= (app $i $g) (OverBar $g))
            "定义同态" (Arrow $i $G (OverBar $G))
            ", 而根据"
            (&= (app $j (OverBar $g))
                (app (OverBar $g) $u))
            "定义同态" (Arrow $j (OverBar $G) $G) ".")
        (Li "最后, 表明" (&= (&compose $i $j) (_ $1 (OverBar $G)))
            "而" (&= (&compose $j $i) $1_G) ".")))
   ((Warning)
    "注意到Cayley定理的证明之中存在着两种不同层次的同构. "
    "我们既有由" $G "的元素构成的集合的置换, 其是" Sets
    "中的同构, 也有" $G "和" (OverBar $G) "之间的同构, "
    "其在由群和群同态构成的范畴" Groups "之中.")
   (P "Cayley定理言称任意的抽象群都可以由某个"
      (Q "具体") "群所表示, 即某个集合的一个置换群. "
      "实际上, 这个定理可以被推广以表明任意不"
      (Q "太大 (too big)") "的范畴都可以由一个"
      (Q "具体") "范畴表示, 即一个由集合和函数构成的范畴. "
      "(不" (Q "太大") "的技术性含义于" (Ref "foundation")
      "引入.)")
   ((Theorem)
    (Em "每个以箭头的集合为态射类的范畴" CatC
        "都同构于一个以集合为对象而函数为箭头的范畴.")
    " {译注: " (Q "每个以箭头的集合为态射类的范畴" CatC)
    "的原文是" (Q "every category " CatC " with a set of arrows")
    ", 这里译者采取了意译而非直译. 换言之, 即范畴"
    CatC "是小范畴.}")
   ((proof)
    "(证明大纲) 定义" CatC "的Cayley表示" (OverBar CatC)
    "为以下具体范畴:"
    (Ul (Li "对象是每个具有形式"
            (MB (&= (OverBar $C)
                    (setI (∈ $f CatC)
                          (&= (&cod $f) $C))))
            "的集合, 其中" (∈ $C CatC) ";")
        (Li "箭头是对于" CatC "中的每个箭头"
            (Arrow $g $C $D)
            "而言的函数"
            (MB (apply Arrow
                       (map OverBar
                            (list $g $C $D))))
            "其定义于" (OverBar $C) "中的任意箭头"
            (Arrow $f $X $C) "之上, 由"
            (&= (app (OverBar $g) $f)
                (&compose $g $f))
            "给出."
            todo.svg)))
   ((Remark #:id "concrete-remark")
    "这向我们表明关于由集合和函数所构成的"
    (Q "具体") "范畴的朴素概念的" (Em "错误之处")
    ": 尽管不是每个范畴都以特殊的集合和函数为其"
    "对象和箭头, 然而每个范畴都同构于一个这样的范畴. "
    "因此, 这样的范畴所能拥有的特殊性质仅可能是"
    "那些和范畴无关的性质, 例如对象所不能影响箭头分毫的那种特性 "
    "(就像构造为Dedekind分割或者Cauchy序列的实数系之间的区别). "
    "为了捕获" (Q "具体") "范畴这个相当模糊的想法所意图表达的东西, "
    "更好的尝试为以下的刻画: 任意的箭头" (Arrow $f $C $D)
    "完全由其与箭头们" (Arrow $x $T $C) "的复合确定, "
    "其中的" $T "是某个" (Q "测试对象") ", 意即如果对于所有这样的"
    $x "都有" (&= (&i* $f $x) (&i* $g $x)) ", 那么就可以推出"
    (&= $f $g) ". 我们之后将会看到, 这相当于考虑由" $T
    "所确定的对于范畴的一种特定表示. 那么, "
    "一个范畴被称为是" (Q "具体") "的, 如果这个条件对于某个"
    (Q "终对象") $T "成立, 终对象的含义见" (Ref "initial-and-terminal")
    "; 不过, 也有很好的理由考虑其他的对象" $T
    ", 如我们在" (Ref "abstract-structures") "中所见." (Br)
    "注意到" CatC "的态射类为箭头的" (Em "集合")
    "这个条件是必要的, 其是为了保证合集"
    (setI (∈ $f CatC) (&= (&cod $f) $C))
    "的确是" (Em "集合") "&mdash;&mdash;我们在"
    (Ref "foundation") "还会回到这个问题上来.")
   (H3. "范畴的构造")
   (P "既然我们有了一些可以摆弄的范畴, 现在我们可以考虑一些从旧范畴产生新范畴的构造了."
      (Ol (Li "两个范畴之" (Em "积") ", 记作"
              (MB (&c* CatC CatD))
              "其有着形式为" (tu0 $C $D) "的对象, 其中" (∈ $C CatC) "且"
              (∈ $D CatD) ", 而有着形式为"
              (MB (apply Arrow
                         (map2 tu0
                               (list $f $g $C $D $C^ $D^))))
              "的箭头, 其中" (∈ (Arrow $f $C $C^) CatC) "而"
              (∈ (Arrow $g $D $D^) CatD) ". 复合和单位元都是逐分量定义的, 即"
              (MB (&= (apply &compose
                             (map2 tu0
                                   (list $f^ $g^ $f $g)))
                      (apply tu0
                             (map2 &compose
                                   (list $f^ $f $g^ $g)))))
              (MB (&= (_ $1 (tu0 $C $D))
                      (tu0 $1_C $1_D)))
              "存在着两个显然的" (Em "投影函子(projection functor)")
              (MB CatC (^^ $<- (pad $pi_1)) (&c* CatC CatD)
                  (^^ $-> (pad $pi_2)) CatD)
              "其由" (&= (appl $pi_1 $C $D) $C) "和"
              (&= (appl $pi_1 $f $g) $f) "定义, 而" $pi_2 "是类似的." (Br)
              "熟悉群论的作者应该能够识别出来, 对于群" $G "和" $H
              ", 其积范畴" (&c* $G $H) "不过是通常的群的(直)积.")
          (Li "一个范畴" CatC "的" (Em "相反(opposite)")
              " (或者说" (Q "对偶 (dual)") ") 范畴"
              (&op CatC) "和" CatC "有着相同的对象, 而"
              (&op CatC) "中的一个箭头" (Arrow $f $C $D) "是"
              CatC "中的一个箭头" (Arrow $f $D $C)
              ". 也就是说, " (&op CatC) "不过就是将"
              CatC "中的所有箭头在形式上调转方向而已." (Br)
              "使用一种记号将" CatC "中的对象和" (&op CatC)
              "中的相同对象进行区分是方便的, 箭头亦是如此. "
              "因此, 让我们将" CatC "中的" (Arrow $f $C $D)
              "在" (&op CatC) "中的相应箭头记为"
              (MB (apply Arrow (map &* (list $f $D $C))) ".")
              "以此记号, 我们可以基于" CatC "中的复合和单位元来定义"
              (&op CatC) "中的相应运算, 即"
              (MB (&= (_ $1 (&* $C)) (&* (@ $1_C))))
              (MB (&= (&compose (&* $f) (&* $g))
                      (&* (@compose $g $f))))
              "因此, " CatC "中的一个图表"
              todo.svg
              "在" (&op CatC) "中看起来就像以下图表"
              todo.svg
              "诸多数学的" (Q "对偶性")
              "定理不过是表达了一个范畴是另一个范畴的相反范畴"
              "(的一个子范畴). 这种定理的一个例子是我们之后要证明的"
              Sets "对偶于完备原子布尔代数的范畴.")
          (Li "一个范畴" CatC "的" (Em "箭头范畴") CatC^->
              "以" CatC "的箭头为对象, 而" CatC^->
              "中一个从" (Arrow $f $A $B) "到"
              (apply Arrow (map &prime (list $f $A $B)))
              "的箭头" $g "是一个" (Q "交换方块")
              todo.svg
              "其中的" $g_1 "和" $g_2 "是" CatC "中的箭头. "
              "也就是说, 这样一个箭头是" CatC "中的一对箭头"
              (&= $g (tu0 $g_1 $g_2)) ", 其满足"
              (MB (&= (&compose $g_2 $f)
                      (&compose $f^ $g_1)) ".")
              "一个对象" (Arrow $f $A $B) "上的恒等箭头"
              $1_f "是序对" (tu0 $1_A $1_B)
              ". 箭头的复合是逐分量进行的:"
              (MB (&= (&compose (tu0 $h_1 $h_2)
                                (tu0 $g_1 $g_2))
                      (tu0 (&compose $h_1 $g_1)
                           (&compose $h_2 $g_2))))
              "读者应该绘制相应的交换图表来验证这的确能够成立." (Br)
              "观察到这里存在着两个函子:"
              (MB CatC (^^ $<- (pad $dom)) CatC^->
                  (^^ $-> (pad $cod)) CatC)
              "{译注: 函子" $dom "的定义为" (&= (&dom (Arrow $f $A $B)) $A)
              "而" (&= (&dom $g) $g_1) ", 其中" (&= $g (tu0 $g_1 $g_2))
              "; 函子" $cod "的定义与" $dom "类似.}")
          (Li "一个范畴" CatC "于一个对象" (∈ $C CatC)
              "上的" (Em "切片范畴(slice category)") (slice CatC $C) "拥有"
              (Ul (Li "对象: 所有满足" (&= (&cod $f) $C)
                      "的箭头" (∈ $f CatC) ";")
                  (Li "箭头: 从" (Arrow $f $X $C) "到"
                      (Arrow $f^ $X^ $C) "的一个箭头" $a
                      "是" CatC "中的一个箭头"
                      (Arrow $a $X $X^) ", 其满足"
                      (&= (&compose $f^ $a) $f) ", 如下图所示"
                      todo.svg
                      "恒等箭头和复合继承自" CatC
                      ", 这与箭头范畴的情况类似. "
                      "注意到存在着一个函子"
                      (Functor $U (slice CatC $C) CatC)
                      ", 其" (Q "遗忘了基对象" $C) "." (Br)
                      "如果" (Arrow $g $C $D)
                      "是任意的箭头, 那么存在着一个复合函子"
                      (MB (Functor $g_* (slice CatC $C) (slice CatC $D)))
                      "其由" (&= (app $g_* $f) (&compose $g $f)) "定义"
                      todo.svg
                      "而对于" (slice CatC $C) "中的箭头也是类似的. "
                      "显然, 整个构造是一个函子"
                      (MB (Functor (slice CatC (@ $dummy)) CatC Cat))
                      "读者可以轻易验证这个事实. 和Cayley表示相比, "
                      "这个函子给出了" CatC "作为由范畴和函子构成的范畴的一种"
                      (Q "表示") "&mdash;&mdash;而非由集合与函数构成的范畴. "
                      "当然了, Cayley表示不过就是其接着一个遗忘函子"
                      (Functor $U Cat Sets)
                      ", 其取范畴为它潜在 (underlying) 的对象集. "
                      "{译注: 这个遗忘函子将" Cat "里的箭头, 即范畴间的函子, "
                      "变为潜在对象集之间的函数.}"(Br)
                      "如果" (&= CatC CatP) "是一个偏序集范畴而"
                      (∈ $p CatP) ", 那么"
                      (MB (&cong (slice CatP $p) (&darr $p)))
                      "切片范畴" (slice CatP $p) "不过就是"
                      (Q "主理想") (&darr $p) ", 其由所有满足"
                      (&<= $q $p) "的元素" (∈ $q CatP) "构成. "
                      "我们很快还会遇到更多切片范畴的例子." (Br)
                      "一个范畴" CatC "在一个" CatC "的对象" $C
                      "下的" (Em "余切片范畴(coslice category)")
                      (coslice $C CatC) "以所有满足" (&= (&dom $f) $C)
                      "的" CatC "中箭头" $f "为对象, 而一个从"
                      (Arrow $f $C $X) "到" (Arrow $f^ $C $X^)
                      "的箭头是一个箭头" (Arrow $h $X $X^)
                      ", 其满足" (&= (&compose $h $f) $f^)
                      ". 读者现在应该仿照切片范畴的定义来"
                      "展开剩余的关于余切片范畴的定义. "
                      "如何根据切片范畴和相反构造来定义余切片范畴呢?")))))
   ((Example)
    (Em "带点集合(pointed set)") "的范畴" Sets_*
    "以带有一个突出元素" (∈ $a $A) "的集合" $A
    "为对象, 而箭头" (Arrow $f (tu0 $A $a) (tu0 $B $b))
    "则是保持" (Q "点") "的函数" (Arrow $f $A $B)
    ", 即" (&= (app $f $a) $b) ". 这同构于一个余切片范畴, 即"
    (MB (&cong Sets_* (coslice $1 Sets)))
    "其中" $1 "是任意的单元集" (setE $*:id)
    ". 诚然如此, 函数" (Arrow $a $1 $A) "唯一地与元素"
    (&= (app $a $*:id) (∈ $a $A)) "相对应, 而箭头"
    (Arrow $f (tu0 $A $a) (tu0 $B $b)) "恰与交换三角形"
    todo.svg
    "相对应.")
   (H3. "自由范畴")
   (P (B "自由幺半群 (free monoid).")
      " 我们从一个" (Q "字母") (&cm $a $b $c $..h)
      "的" (Q "表 (alphabet)") $A "开始, 即一个集合"
      (MB (&= $A (setE $a $b $c $..h)) ".")
      $A "上的一个" (Em "词(word)")
      "是一个字母的有限序列:"
      (MB (&cm "thisword"
               "categoriesarefun"
               "asddjbnzzfj"
               $..h))
      "我们将空词记为" (Q $empty-word)
      ". " $A "的" (Q "Kleene闭包")
      "被定义为集合"
      (MB (&= $A^*
              (cur0 (: $A "上的词"))) ".")
      "我们定义一个" $A^* "上的二元运算"
      (Q $*) "为" (&= (&* $w $w^) (concat $w $w^))
      ", 其中词" (∈ $w $w^ $A^*) ". 因此, "
      (Q $*) "不过就是" (Em "拼接(concatenation)")
      ". 于是, 运算" (Q $*) "是结合性的, 并且空词"
      (Q $empty-word) "是其单位元. "
      "所以说, " $A^* "是一个幺半群&mdash;&mdash;"
      "其被称为集合" $A "上的"
      (Em "自由幺半群(free monoid)")
      ". 元素" (∈ $a $A) "可以被视为长度为一的词, "
      "也就是说我们有了一个函数"
      (MB (Arrow $i $A $A^*))
      "其定义是" (&= (app $i $a) $a)
      ", 并被称为" (Q "生成元的嵌入 (insertion of generators)")
      ". 我们说" $A "的元素" (Q "生成") "了这自由范畴, "
      "其意在于每个" (∈ $w $A^*) "都是" $a
      "的一个" $* "积, 即对于" $A "中的某些"
      (&cm $a_1 $a_2 $..h $a_n) "我们有"
      (&= $w (&* $a_1 $a_2 $..c $a_n))
      ". {译注: 嵌入是自动的, 没有以额外的符号标示.}")
   (P "现在我们想问的是这里的" (Q "自由")
      "是什么意思? 能猜一下吗? "
      "有时读者会在" (Q "宝宝代数 (baby algebra)")
      "书籍中看到其定义以如下文字呈现:")
   (P "一个幺半群" $M "由" $M "的一个子集" $A
      (Em "自由生成") ", 如果其满足以下条件:"
      (Ol (Li "每个元素" (∈ $m $M) "都可以写成" $A "的元素之积:"
              (MB (&cm (&= $m (&d*_M $a_1 $..h $a_n))
                       (∈ $a_i $A)) "."))
          (Li "没有" (Q "非平凡 (nontrivial)")
              "的关系可以在" $M "中成立, 即若"
              (&= (&i* $a_1 $..h $a_j)
                  (&i* (_^ $a $1 $prime)
                       $..h
                       (_^ $a $k $prime)))
              ", 那么这是由幺半群的公理所要求的."))
      "第一个条件有时被称为" (Q "没有垃圾 (no junk)")
      ", 而第二个条件有时被称为" (Q "没有杂讯 (no noise)")
      ". 因此, " $A "上的自由幺半群是一个包含" $A
      "且既无垃圾也无杂讯的幺半群. "
      "你对于该自由幺半群的定义作何感想呢?")
   (P "我反对这第二个条件所涉及的" (Q "可证明性 (provability)")
      "或者其他什么. {译注: 译者也不太能完全理解这句话, "
      "大概的意思就是第二个条件太过模糊, 编写证明时难以使用.} "
      "欲使其作为定义而成功, 那么其就必须变得更加精确. "
      "在范畴论中, 我们可以给出" (Q "自由")
      "的精确定义&mdash;&mdash;其捕获了上述内容的实质"
      "&mdash;&mdash;但是避免了这种模糊.")
   (P "首先, 每个幺半群" $N "都有一个潜在集"
      (forget $N) ", 并且每个幺半群同态"
      (Arrow $f $N $M) "都有一个潜在函数 (underlying function) "
      (apply Arrow (map forget (list $f $N $M)))
      ". 很容易看出来这是一个函子, 可以称为"
      (Q "遗忘函子") ". 根据定义, 一个集合" $A "上的自由幺半群"
      (free-monoid $A) "是带有以下所谓"
      (Em "泛映射性质(universal mapping property)")
      "或者说UMP的" (Q "那个 (the)") "幺半群!")
   (P (Em (free-monoid $A) "的泛映射性质") (Br)
      "存在着一个函数" (Arrow $i $A (forget (free-monoid $A)))
      ", 对于任意的幺半群" $N "和任意的函数" (Arrow $f $A (forget $N))
      ", 存在" (Em "唯一") "的幺半群同态"
      (Arrow (OverBar $f) (free-monoid $A) $N)
      "使得" (&= (&compose (forget (OverBar $f)) $i) $f)
      ", 如以下图表所示:"
      UMP:free-monoid.svg)
   ((Proposition)
    (Em $A^* "具有" $A "上的自由幺半群的UMP."))
   ((proof)
    "给定函数" (Arrow $f $A (forget $N)) ", 由"
    (MB (&cm (&= (app (OverBar $f) $empty-word) $u_N)
             (: $N "的单位元")))
    (MB (&= (app (OverBar $f) (&i* $a_1 $..h $a_i))
            (&d*_N (app $f $a_1) $..h
                   (app $f $a_i))))
    "定义" (Arrow (OverBar $f) $A^* $N)
    ", 那么" (OverBar $f) "显然是一个满足"
    (MB (&cm (&= (app (OverBar $f) $a)
                 (app $f $a))
             (: "对于所有的" (∈ $a $A))))
    "的同态. 如果" (Arrow $g $A^* $N) "也满足对于所有的"
    (∈ $a $A) "都有" (&= (app $g $a) (app $f $a))
    ", 那么对于所有的" (∈ (&i* $a_1 $..h $a_i) $A^*) ":"
    (MB (deriv (app $g (&i* $a_1 $..h $a_i))
               (app $g (&* $a_1 $..h $a_i))
               (&d*_N (app $g $a_1) $..h (app $g $a_i))
               (&d*_N (app $f $a_1) $..h (app $f $a_i))
               (&d*_N (app (OverBar $f) $a_1)
                      $..h
                      (app (OverBar $f) $a_i))
               (app (OverBar $f) (&* $a_1 $..h $a_i))
               (app (OverBar $f) (&i* $a_1 $..h $a_i))))
    "因此, 正如泛映射性质所要求的, " (&= $g (OverBar $f))
    ". {译注: 这个证明里的某些地方需要诉诸我们对于有限字符序列 (字符串) "
    "的直觉, 但是的确是可以严格形式化的.}")
   (P "请想一想为什么上述的UMP精确捕获了" (Q "没有垃圾")
      "和" (Q "没有杂讯") "的意义. 更确切地说, "
      "UMP的存在性部分捕获了" (Q "没有杂讯")
      "这个模糊的概念 (因为生成元的代数组合之间成立的任意等式"
      "也必然在任意其可以被映射至的地方成立, 也就是所有的地方), "
      "而唯一性的部分精确化了" (Q "没有垃圾")
      "的想法 (因为任意不是由生成元组合而成的额外元素都可以被"
      "自由地映射至" (Em "不同") "的值.")
   (P "使用这个UMP, 我们可以很容易地表明自由幺半群"
      (free-monoid $A) "在同构下唯一确定, 其含义如下:")
   ((Proposition)
    (Em "给定幺半群" $M "和" $N ",以及附带的函数"
        (Arrow $i $A (forget $M)) "和"
        (Arrow $j $A (forget $N)) ",每个都满足"
        $A "上的自由幺半群的UMP,那么存在一个(唯一的)幺半群同构"
        (&: $h (&cong $M $N)) "满足"
        (&= (&i* (forget $h) $i) $j) "且"
        (&= (&i* (forget (inv $h)) $j) $i) "."))
   ((proof)
    "根据" $j "和" $M "的UMP, 我们有" (Arrow (OverBar $j) $M $N)
    ", 其满足" (&= (&i* (forget (OverBar $j)) $i) $j)
    ". 根据" $i "和" $N "的UMP, 我们有" (Arrow (OverBar $i) $N $M)
    ", 其满足" (&= (&i* (forget (OverBar $i)) $j) $i)
    ". 复合给出了一个同态"
    (Arrow (&compose (OverBar $i) (OverBar $j)) $M $M)
    ", 其满足"
    (&= (&i* (forget (&compose (OverBar $i) (OverBar $j))) $i) $i)
    ". {译注: 遗忘是一个函子, 故保持复合.} 鉴于"
    (Arrow $1_M $M $M) "也具有此性质, 根据" $M
    "的UMP的唯一性部分, 我们有"
    (&= (&compose (OverBar $i) (OverBar $j)) $1_M)
    ". 交换" $M "和" $N "的角色可以表明"
    (&= (&compose (OverBar $j) (OverBar $i)) $1_N)
    " {译注: " $h "的唯一性也可由自由幺半群的UMP的唯一性部分直接得到}:"
    todo.svg)
   (P "例如, 任意单元素集合上的自由幺半群很容易看出来与自然数集"
      $NN "在加法下的幺半群同构 (其" (Q "生成元")
      "是数字" $1 "). 因此, 作为一个幺半群, "
      "根据自由幺半群的UMP, " $NN "在同构下唯一确定. "
      "{译注: 和自由幺半群同构的幺半群是自由的.}")
   (P (B "自由范畴 (free category).")
      " 现在, 我们想要对于一般的范畴 (而非仅是幺半群) "
      "进行相同的事情. 范畴拥有的是潜在图 (underlying graph) "
      "而不是潜在集合, 故让我们先回顾图论.")
   (P "一个" (Em "有向图(directed graph)") "由顶点和边构成, "
      "其中每条边都是有向的, 也就是每条边都有一个"
      (Q "源头 (source)") "和一个" (Q "目标 (target)") "顶点. "
      "{译注: 在范畴论中, domain有时也被称为source, "
      "codomain有时也被称为target.}"
      todo.svg
      "我们就像绘制范畴一样绘制图, 但是这里没有边的复合, "
      "也没有恒等边 (identity). "
      "{译注: 绘制范畴的图表时, 我们会默认每个对象上都有一个恒等箭头"
      "而不画出来. 在图 (graph) 中, 我们没有与之对应的恒等边的概念. "
      "不过, 就像一个范畴可以有从某个对象到其自身的非恒等的箭头, "
      "一个图里也可以存在着从某个顶点到其自身的边 (即自环, loop). "
      "另外, 虽然边和边不能复合为新的边, "
      "但是两个由边构成的路径可以复合为新的路径, "
      "这也是下文就要提到的.}")
   (P "因此, 一个图由两个集合" $E " (边) 和"
      $V " (顶点), 以及两个函数" (Arrow $s $E $V)
      " (源头) 和" (Arrow $t $E $V)
      " (目标) 构成. 于是, 在" Sets "中, 一个图不过是具有形式"
      (MB $E (__^^ $rrarr (pad $t) (pad $s)) $V)
      "的一个由对象和箭头构成的配置 (configuration).")
   (P "现在, 每个图" $G "都" (Q "生成") "了一个范畴"
      (free-category $G) ", 即" $G "上的"
      (Em "自由范畴") ". 其取" $G "的顶点为对象, 而"
      $G "中的" (Em "路径(path)") "为箭头, "
      "其中一个路径是一个边的有限序列"
      (&cm $e_1 $..h $e_n) ", 其满足对于每个"
      (&= $i (&cm $1 $..h (&- $n $1))) "都有"
      (&= (app $t $e_i) (app $s (_ $e (&+ $i $1))))
      ". 我们记" (free-category $G) "的箭头以形式"
      (concat $e_n (_ $e (&- $n $1)) $..h $e_1) "."
      ;也可以算作某种形式的concat
      (MB $v_0 (^^ $-> (pad $e_1))
          $v_1 (^^ $-> (pad $e_2))
          $v_2 (^^ $-> (pad $e_3))
          $..c (^^ $-> (pad $e_n))
          $v_n)
      "置"
      (eqn*
       ((app $dom (concat $e_n $..h $e_1))
        $=
        (app $s $e_1))
       ((app $cod (concat $e_n $..h $e_1))
        $=
        (app $t $e_n)))
      "并以拼接定义复合:"
      (MB (&= (&compose
               (concat $e_n $..h $e_1)
               (concat (_^ $e $m $prime)
                       $..h
                       (_^ $e $1 $prime)))
              (concat
               (concat $e_n $..h $e_1)
               (concat (_^ $e $m $prime)
                       $..h
                       (_^ $e $1 $prime)))) ".")
      "对于每个顶点" $v ", 我们都有一个"
      (Q "空路径 (empty path)")
      ", 记作" $1_v ", 其为" $v "处的恒等箭头.")
   (P "我们注意到如果" $G "只有一个顶点, 那么"
      (free-category $G) "不过就是" $G
      "的边的集合上的自由幺半群. 另外, 如果" $G
      "只有顶点(而没有边), 那么" (free-category $G)
      "是" $G "的顶点集合上的离散范畴.")
   (P "之后我们将会拥有一个对于" (Q "自由")
      "的一般性定义, 暂时先让我们看到"
      (free-category $G) "也有一个UMP. "
      "首先, 以显然的方式定义遗忘函子"
      (MB (Functor $U Cat Graphs))
      "一个范畴" CatC "的潜在图以" CatC
      "的箭头为边, 而以对象为顶点, 其中"
      (&= $s $dom) "且" (&= $t $cod)
      ". " $U "于函子上的动作也是同等清晰的, "
      "至少很快就会变得清晰起来, "
      "一旦我们定义了" Graphs "中的箭头.")
   (P "一个图的同态当然是一个"
      (Q "无需满足恒元和复合上的条件的函子")
      ". {译注: 读者明白其实一个图上没有恒等箭头和箭头复合的类似物, "
      "但是作者是类比函子的定义而说出这句话的.} "
      "也就是说, 其是一个保持源头和目标的从边到边而从顶点到顶点的映射. "
      "现在我们从稍微不同的角度来描述图的同态, 这在之后会变得有用.")
   (P "首先, 观察到我们可以用如下图表描述一个范畴" CatC ":"
      describe-a-category.svg
      "其中" $C_0 "是" CatC
      "的对象的合集, " $C_1
      "是箭头的合集, " $i
      "是恒等箭头操作, " $C_2
      "是合集"
      (MB (setI
           (∈ (tu0 $f $g) (&c* $C_1 $C_1))
           (&= (&cod $f) (&dom $g))) ".")
      "{译注: 译者感觉这里顺序似乎颠倒了, 但是实际上也有人采取这样的约定, "
      "不过我不知道作者是怎么想的, 或许只是笔误而已.}")
   (P "然后, 从" CatC "到一个范畴" CatD
      "的一个函子" (Functor $F CatC CatD)
      "是一对函数"
      (MB (Arrow $F_0 $C_0 $D_0))
      (MB (Arrow $F_1 $C_1 $D_1))
      "其使得以下图表中的每个类似标记的方块 "
      "(similarly labeled square) 都交换:"
      (Svg
       #:attr*
       '((width "250")
         (height "180")
         (stroke "black")
         (style "display: block; margin: auto;"))
       (Defs arrow-head)
       (:FO (make-pt2 30 30) $C_2)
       (:FO (make-pt2 120 30) $C_1)
       (:FO (make-pt2 210 30) $C_0)
       (:arrow-prop
        (pt2+ (make-pt2 30 30)
              (make-vec2 8 8))
        (pt2+ (make-pt2 120 30)
              (make-vec2 8 8)))
       (:arrow-prop
        (pt2+ (make-pt2 210 30)
              (make-vec2 8 8))
        (pt2+ (make-pt2 120 30)
              (make-vec2 8 8)))
       (:arrow-prop
        (pt2+ (make-pt2 120 30)
              (make-vec2 8 -7))
        (pt2+ (make-pt2 210 30)
              (make-vec2 8 -7)))
       (:arrow-prop
        (pt2+ (make-pt2 120 30)
              (make-vec2 8 23))
        (pt2+ (make-pt2 210 30)
              (make-vec2 8 23)))
       (:FO
        ((lerp 1/2)
         (make-pt2 30 30)
         (make-pt2 120 30))
        #:offset (make-vec2 4 -10)
        $compose)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 30)
         (make-pt2 210 30))
        #:offset (make-vec2 4 -1)
        $i)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 30)
         (make-pt2 210 30))
        #:offset (make-vec2 -4 -25)
        $cod)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 30)
         (make-pt2 210 30))
        #:offset (make-vec2 -6 24)
        $dom)
       (:FO (make-pt2 30 130) $D_2)
       (:FO (make-pt2 120 130) $D_1)
       (:FO (make-pt2 210 130) $D_0)
       (:arrow-prop
        (pt2+ (make-pt2 30 130)
              (make-vec2 8 8))
        (pt2+ (make-pt2 120 130)
              (make-vec2 8 8)))
       (:arrow-prop
        (pt2+ (make-pt2 210 130)
              (make-vec2 8 8))
        (pt2+ (make-pt2 120 130)
              (make-vec2 8 8)))
       (:arrow-prop
        (pt2+ (make-pt2 120 130)
              (make-vec2 8 -7))
        (pt2+ (make-pt2 210 130)
              (make-vec2 8 -7)))
       (:arrow-prop
        (pt2+ (make-pt2 120 130)
              (make-vec2 8 23))
        (pt2+ (make-pt2 210 130)
              (make-vec2 8 23)))
       (:FO
        ((lerp 1/2)
         (make-pt2 30 130)
         (make-pt2 120 130))
        #:offset (make-vec2 4 10)
        $compose)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 130)
         (make-pt2 210 130))
        #:offset (make-vec2 4 -1)
        $i)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 130)
         (make-pt2 210 130))
        #:offset (make-vec2 -4 -25)
        $cod)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 130)
         (make-pt2 210 130))
        #:offset (make-vec2 -6 24)
        $dom)
       (:arrow-prop
        (pt2+ (make-pt2 30 30)
              (make-vec2 10 9))
        (pt2+ (make-pt2 30 130)
              (make-vec2 10 9)))
       (:arrow-prop
        (pt2+ (make-pt2 120 30)
              (make-vec2 10 9))
        (pt2+ (make-pt2 120 130)
              (make-vec2 10 9)))
       (:arrow-prop
        (pt2+ (make-pt2 210 30)
              (make-vec2 10 9))
        (pt2+ (make-pt2 210 130)
              (make-vec2 10 9)))
       (:FO
        ((lerp 1/2)
         (make-pt2 30 30)
         (make-pt2 30 130))
        #:offset (make-vec2 -12 0)
        $F_2)
       (:FO
        ((lerp 1/2)
         (make-pt2 120 30)
         (make-pt2 120 130))
        #:offset (make-vec2 -12 0)
        $F_1)
       (:FO
        ((lerp 1/2)
         (make-pt2 210 30)
         (make-pt2 210 130))
        #:offset (make-vec2 15 0)
        $F_0))
      "其中"
      (&= (ap $F_2 (tu0 $f $g))
          (tu0 (app $F_1 $f)
               (app $F_1 $g))) ".")
   (P "现在让我们来描述" (Em "图的同态")
      (MB (Arrow $h $G $H) ".")
      "我们需要一对函数"
      (&cm (Arrow $h_0 $G_0 $H_0)
           (Arrow $h_1 $G_1 $H_1))
      "来作成在以下图表中交换的两个方块"
      " (一个和" $t "有关, 一个和" $s "有关):"
      (Svg
       #:attr*
       '((width "180")
         (height "180")
         (stroke "black")
         (style "display: block; margin: auto;"))
       (Defs arrow-head)
       (:FO (make-pt2 30 30) $G_1)
       (:FO (make-pt2 150 30) $G_0)
       (:FO (make-pt2 30 150) $H_1)
       (:FO (make-pt2 150 150) $H_0)
       (:arrow-prop
        (pt2+ (make-pt2 30 30)
              (make-vec2 8 5))
        (pt2+ (make-pt2 150 30)
              (make-vec2 8 5)))
       (:arrow-prop
        (pt2+ (make-pt2 30 30)
              (make-vec2 8 11))
        (pt2+ (make-pt2 150 30)
              (make-vec2 8 11)))
       (:arrow-prop
        (pt2+ (make-pt2 30 150)
              (make-vec2 8 5))
        (pt2+ (make-pt2 150 150)
              (make-vec2 8 5)))
       (:arrow-prop
        (pt2+ (make-pt2 30 150)
              (make-vec2 8 11))
        (pt2+ (make-pt2 150 150)
              (make-vec2 8 11)))
       (:arrow-prop
        (pt2+ (make-pt2 30 30)
              (make-vec2 10 9))
        (pt2+ (make-pt2 30 150)
              (make-vec2 10 9)))
       (:arrow-prop
        (pt2+ (make-pt2 150 30)
              (make-vec2 10 9))
        (pt2+ (make-pt2 150 150)
              (make-vec2 10 9)))
       (:FO
        ((lerp 1/2)
         (make-pt2 30 30)
         (make-pt2 150 30))
        #:offset (make-vec2 4 -12)
        $t)
       (:FO
        ((lerp 1/2)
         (make-pt2 30 30)
         (make-pt2 150 30))
        #:offset (make-vec2 4 10)
        $s)
       (:FO
        ((lerp 1/2)
         (make-pt2 30 150)
         (make-pt2 150 150))
        #:offset (make-vec2 4 -12)
        $t)
       (:FO
        ((lerp 1/2)
         (make-pt2 30 150)
         (make-pt2 150 150))
        #:offset (make-vec2 4 10)
        $s)
       (:FO
        ((lerp 1/2)
         (make-pt2 30 30)
         (make-pt2 30 150))
        #:offset (make-vec2 -10 0)
        $h_1)
       (:FO
        ((lerp 1/2)
         (make-pt2 150 30)
         (make-pt2 150 150))
        #:offset (make-vec2 14 0)
        $h_0))
      "以这些概念, 我们可以很容易地描述遗忘函子"
      (MB (Functor $U Cat Graphs))
      "为将范畴"
      describe-a-category.svg
      "送至其潜在图 {译注: "
      $dom "相当于" $s ", " $cod "相当于" $t "}"
      (MB $C_1
          (__^^ $rrarr (pad $dom) (pad $cod))
          $C_0)
      "而类似地对于函子, " $U
      "的效果可以被描述为擦除图表里的一些部分 "
      "(这用粉笔演示更加容易!). 再一次让我们对于一个范畴"
      CatC "的潜在图记"
      (&= (forget CatC) (app $U CatC))
      ", 诸如此类, 这与之前幺半群的情形是类似的.")
   (P "现在, 我们可以说一个图上的自由范畴拥有以下UMP.")
   (P (Em (free-category $G) "的泛映射性质") (Br)
      "存在一个图同态"
      (Arrow $i $G (forget (free-category $G)))
      ", 对于任意的范畴" CatD "和任意的图同态"
      (Arrow $h $G (forget CatD))
      ", 存在" (Em "唯一") "的函子"
      (Functor (OverBar $h) (free-category $G) CatD)
      "使得" (&= (&compose (forget (OverBar $h)) $i) $h) "."
      UMP:free-category.svg)
   (P "只有一个顶点的图上的自由范畴不过是其边集合上的自由幺半群. "
      "只有两个顶点和其间的一条边的图上的自由范畴是有限范畴" Cat2
      ". 在具有形式"
      (MB $A (__^^ $rlarr (pad $f) (pad $e)) $B)
      "的一个图上的自由范畴(在恒等箭头之外)拥有无穷多个箭头:"
      (MB (&cm $e $f (concat $e $f) (concat $f $e)
               (concat $e $f $e) (concat $f $e $f)
               (concat $e $f $e $f) $..h)))
   (H3. "基础: 大, 小, 以及局部小" #:id "foundation")
   (P "本节让我们以区分下列概念开始:"
      (Ol #:attr* '((type "i"))
          (Li "数学的范畴论基础;")
          (Li "范畴论的数学基础.")))
   (P "之于第一个点, 读者有时可能听说过范畴论可以用来提供"
      (Q "数学的基础") ", 作为集合论的替代. "
      "的确如此, 但是这并非我们现在要做的事情. "
      "在集合论中, 我们往往以诸如"
      (Q "存在一个无限的集合")
      "这样的外延性的公理开始, 然后根据诸如"
      (Q "每个集合都有一个幂集")
      "这样的公理导出更多的集合, "
      "由此构建出一个数学对象 (即集合) 的宇宙, "
      "在原则上这对于" (Q "所有的数学") "是足够了的. "
      "我们的诸如每个箭头都有一个定义域和一个陪域的公理"
      "不应该同诸如每个集合都有一个幂集这样的公理以相同的方式理解! "
      "其区别在于集合论之中&mdash;&mdash;至少按照其通常的意义"
      "&mdash;&mdash;公理被认为是指向 (或者说确定) "
      "一个单一的集合宇宙. 与之相对的是, "
      "范畴论中的公理是对于某种东西的一个" (Em "定义")
      ", 即关于范畴的概念. 这就如同群论或者拓扑中的情况, "
      "其中公理用于定义研究的对象. 实际上, "
      "这些对象被认为是存在于某个" (Q "背景")
      "或者说" (Q "基础") "系统之中, 例如集合论 (或者类型论). "
      "而集合理论自身, 又可能是使用范畴论确定的, 或者其他什么方式.")
   (P "这将我们引向了第二点: 我们假定我们的范畴就像绝大多数"
      "数学对象一样以这样或那样的方式由集合和函数构成, "
      "并将刚才所作出的关于范畴论(或者其他)基础的可能性的评注考虑在内. "
      "但是, 在范畴论中, 我们有时会遇到集合论的通常实践里也会遇到的困难. "
      "绝大多数情况下这是关于" (Q "大小") "的问题; 一些范畴" (Q "太大")
      "以至于无法在常规的集合论中舒服地处理. 当我们在" (Ref "isomorphism")
      "里考虑Cayley表示时, 我们就已经遇到了这种问题. "
      "那里我们需要我们所考虑的范畴的态射类(至多)只是箭头的集合. "
      "然而, (尽管我们通常会限制群之类的对象的大小, )"
      "我们并不希望在一般情况下施加这种限制; "
      "否则的话, 甚至连" (Q "范畴") Sets "都不能是一个真正的范畴了, "
      "其他诸多我们一定想要研究的范畴也是如此.")
   (P "存在着各种各样的形式化设备用于解决这些问题, "
      "而它们在Mac Lane的书中有所讨论. "
      "{译注: 所谓形式化设备 (formal device), "
      "读者可以理解为形式化的技术细节.} "
      "对于我们当前的目的, 以下的区分是有用的.")
   ((Definition)
    "一个范畴" CatC "被称为是" (Em "小(small)")
    "的, 如果" CatC "的对象合集" (_ CatC $0) "和"
    CatC "的箭头合集" (_ CatC $1) "都是集合. "
    "否则的话, " CatC "就被称为是" (Em "大(large)")
    "的.")
   (P "例如, 所有的有限范畴都显然是小的, "
      "由有限集合和函数构成的范畴" Sets_fin "亦是如此. "
      "(实际上, 我们甚至可以规定集合只能由其他有限集合构成, "
      "一路递降到底也是如此, 即它们是"
      (Q "遗传有限 (hereditarily finite)")
      "的. {译注: 这里对于遗传有限性的描述并不那么明确, "
      "实际上遗传有限集合可以被定义为每个元素都是遗传有限集合的有限集合.}) "
      "另一方面, 偏序集的范畴" Pos ", 群的范畴" Groups
      ", 以及集合的范畴" Sets "都是大的. 我们令"
      Cat "是由所有" (Em "小范畴(small category)") "构成的范畴, "
      "其本身是一个大范畴. 那么, 特别地, " CatC
      "不是其自身的对象, 这对于某些读者而言可能是一种迟来的宽慰.")
   (P "这并没有真正解决我们所有的困难. 甚至对于诸如"
      Groups "和" Sets "这样的大范畴, "
      "我们也想要考虑诸如由所有从一个范畴到"
      "另一个范畴的函子构成的范畴这样的构造 "
      "(之后我们会定义这种" (Q "函子范畴 (functor category)")
      "). 但是, 如果这些范畴不是小的, "
      "那么常规的集合论就没有提供直接处理它们的手段 "
      "(这些范畴就" (Q "太大") "了). 因此, "
      "我们需要一种关于" (Q "类 (class)")
      "的更加精致的理论来处理这样的构造. "
      "我们不会太过担心这个问题, 因为它只是关于技术性基础的事情. "
      "(Mac Lane I.6处理了这件事情.) 然而, "
      "与之相关的一个非常有用的概念如下.")
   ((Definition)
    "一个范畴" CatC "被称为是" (Em "局部小(locally small)")
    "的, 如果对于" CatC "中所有的对象" (&cm $X $Y) ", 合集"
    (&= (Hom CatC $X $Y)
        (setI (∈ $f (_ CatC $1))
              (Arrow $f $X $Y)))
    "是一个" (Em "集合") " (其被称为一个"
    (Em "同态集(hom-set)") "). {译注: " (Arrow $f $X $Y)
    "这个条件的意思即" (&= (&dom $f) $X) "且"
    (&= (&cod $f) $Y) ".}")
   (P "诸多我们想要考虑的大范畴实际上是局部小的. "
      Sets "是局部小的, 鉴于" (&= (Hom Sets $X $Y) $Y^X)
      "是由所有从" $X "到" $Y "的函数构成的" (Em "集合")
      ". 类似地, " Pos ", " Top ", " Groups
      "都是局部小的 (" Cat "呢?). "
      "{译注: 尽管前文尚未提及, " Top "是拓扑空间的范畴.} "
      "当然, 任何小范畴都是局部小的.")
   ((Warning)
    "不要混淆" (Em "具体") "和" (Em "小")
    "的概念. 称一个范畴是具体的是说这个范畴的"
    (Em "对象") "是(结构化的)集合而这个范畴的箭头是(特定的)函数. "
    "称一个范畴是小的是说这个范畴的" (Em "所有对象构成的合集")
    "是一个集合, 所有箭头构成的合集也是一个集合. 实数集"
    $RR "在被视为一个偏序集范畴时是小的但并非具体. "
    "由所有偏序集构成的范畴" Pos "是具体的但并非小的.")
   (H3. "练习")
   (Ol (Li Rel "的对象是集合, 而一个箭头" (&-> $A $B)
           "是一个从" $A "到" $B "的关系, 即一个子集"
           (&sube $R (&c* $A $B)) ". 相等关系"
           (setI (∈ (tupa0 $a $a) (&c* $A $A))
                 (∈ $a $A))
           "是集合" $A "上的恒等箭头. " Rel
           "中的复合由"
           (MB (&= (&compose $S $R)
                   (setI (∈ (tupa0 $a $c) (&c* $A $C))
                         (∃ $b (&& (∈ (tupa0 $a $b) $R)
                                   (∈ (tupa0 $b $c) $S))))))
           "给出, 其中" (&sube $R (&c* $A $B)) "而"
           (&sube $S (&c* $B $C))
           ". {译注: 一个关系实际上不仅是集合的笛卡尔积的一个子集, "
           "它还需要记住自己的定义域和陪域 (或者说源头和目标), "
           "换言之定义域和陪域是其元信息. 因此, 记号"
           (&sube $R (&c* $A $B)) "应该理解为"
           (&= (&dom $R) $A) "且" (&= (&cod $R) $B) ".}"
           (Ol #:attr* '((type "a"))
               (Li "表明" Rel "是一个范畴.")
               (Li "也表明存在一个函子" (Functor $G Sets Rel)
                   "将每个对象送至自身而将每个函数"
                   (Arrow $f $A $B) "送至其图 (graph), 即"
                   (MB (&= (app $G $f)
                           (setI (∈ (tupa0 $a (app $f $a))
                                    (&c* $A $B))
                                 (∈ $a $A))) "."))
               (Li "最后, 表明存在一个函子"
                   (Functor $C (&op Rel) Rel)
                   ", 其取每个关系" (&sube $R (&c* $A $B))
                   "为其逆" (&sube $R^c (&c* $B $A))
                   ", 其中"
                   (MB (&<=>
                        (∈ (tupa0 $a $b) $R^c)
                        (∈ (tupa0 $b $a) $R)) ".")
                   "{译注: 这个问题的描述十分糟糕, "
                   "译者花了相当长的时间才理解清楚. "
                   (&op Rel) "中的关系" (&sube $R (&c* $A $B))
                   "实际上是一个从" $B "到" $A
                   "的箭头, 而" Rel "中的"
                   (&sube $R^c (&c* $B $A))
                   "也是一个从" $B "到" $A "的箭头. "
                   (&<=>
                    (∈ (tupa0 $a $b) $R^c)
                    (∈ (tupa0 $b $a) $R))
                   "这个记号太过具有误导性了, 实际上其中"
                   (∈ $b $A) "而" (∈ $a $B) ". 译者认为改成"
                   (&<=>
                    (∈ (tupa0 $b $a) $R^c)
                    (∈ (tupa0 $a $b) $R))
                   "更合理一些.}")))
       (Li "考虑以下范畴的同构并判断是否成立."
           (Ol #:attr* '((type "a"))
               (Li (&cong Rel (&op Rel)))
               (Li (&cong Sets (&op Sets)))
               (Li "对于一个固定的集合" $X
                   ", 其幂集是" (powerset $X)
                   ", 那么作为偏序集范畴的"
                   (&cong (powerset $X)
                          (&op (powerset $X)))
                   ", 其中" (powerset $X)
                   "里的箭头是对于所有"
                   (&sube (&cm $A $B) $X)
                   "而言的子集包含"
                   (&sube $A $B)
                   ". {译注: 这里我基本上采取直译, "
                   "不过不熟悉英文和数学习惯的人不太容易理解, "
                   "其实作者想说的是这个偏序集范畴的偏序不过就是"
                   (powerset $X) "上的子集包含关系, "
                   "其显然是自反, 传递, 反对称的.}")))
       (Li (Ol #:attr* '((type "a"))
               (Li "表明在" Sets "之中, 同构恰为双射.")
               (Li "表明在" Mon "之中, 同构恰为双射的同态.")
               (Li "表明在" Pos "之中, 同构和双射的同态是"
                   (Em "不") "同的.")))
       (Li "令" $X "是一个拓扑空间"
           )
       )
   (H2. "抽象结构" #:id "abstract-structures")
   (P "我们从一些关于范畴论定义的评注开始. "
      "这些定义是对于一个范畴中的对象和箭头的性质的刻画, "
      "其仅基于其他的对象和箭头, 也就是说, "
      "只使用范畴论的语言. 这样的定义或可以说成是抽象的, "
      "结构化的, 操作性的, 关系式的, 或者外部的 "
      "(与内部的相对). 想法在于对象和箭头是根据"
      "它们在范畴中扮演的角色确定的, "
      "藉由它们与其他对象和箭头的关系, "
      "也就是说, 根据它们在结构中的位置而非"
      "某种绝对意义上的它们" (Q "是什么") "或者"
      (Q "由什么构成") ". 前一章的自由幺半群或者自由范畴的构造"
      "是这种定义的一个例子, 而我们之后将会看到更多这样的例子; "
      "暂时, 我们从简单的开始. 让我们称其为"
      (Em "抽象刻画(abstract characterization)")
      ". 我们将会看到给出这样一个抽象刻画的基本方式之一是"
      "藉由一个泛映射性质 (UMP).")
   (H3. "满态射和单态射")
   (P "回忆一下, 在" Sets "中, 一个函数" (Arrow $f $A $B) "被称为"
      (Ul (Li (Em "单射的(injective)")
              ", 如果对于所有的" (∈ $a $a^ $A)
              ", " (&= (app $f $a) (app $f $a^))
              "可以推出" (&= $a $a^) ";")
          (Li (Em "满射的(surjective)")
              ", 如果对于所有的" (∈ $b $B)
              ", 存在某个" (∈ $a $A) "使得"
              (&= (app $f $a) $b) "."))
      "我们有着以下对于这些性质的抽象刻画.")
   ((Definition)
    "在任意的范畴" CatC "中, 一个箭头"
    (MB (Arrow $f $A $B))
    "被称为是一个"
    (Ul (Li (Em "单态射(monomorphism)")
            ", 如果对于任意的"
            (Arrow (&cm $g $h) $C $A) ", "
            (&= (&i* $f $g) (&i* $f $h))
            "可以推出" (&= $g $h) ","
            (MB $C (__^^ $rrarr (pad $h) (pad $g))
                $A (^^ $-> (pad $f)) $B ";"))
        (Li (Em "满态射(epimorphism)")
            ", 如果对于任意的"
            (Arrow (&cm $i $j) $B $D) ", "
            (&= (&i* $i $f) (&i* $j $f))
            "可以推出" (&= $i $j) ","
            (MB $A (^^ $-> (pad $f)) $B
                (__^^ $rrarr (pad $j) (pad $i))
                $D ".")))
    "如果" $f "是一个单态射, 那么我们经常将其记为"
    (Mono $f $A $B) "; 如果" $f
    "是一个满态射, 那么我们经常将其记为"
    (Epi $f $A $B) ".")
   ((Proposition)
    (Em "一个集合之间的函数" (Arrow $f $A $B)
        "是单态射恰当其为单射."))
   ((proof)
    "设" (Mono $f $A $B) ", 令" (∈ $a $a^ $A)
    "满足" (&!= $a $a^) "而令" (setE $x)
    "是任意给定的单元素集合. 考虑函数"
    (MB (Arrow (&cm (OverBar $a)
                    (OverBar $a^))
               (setE $x) $A))
    "其中"
    (MB (&cm (&= (app (OverBar $a) $x) $a)
             (&= (app (OverBar $a^) $x) $a^)) ".")
    "既然" (&!= (OverBar $a) (OverBar $a^))
    ", 根据" $f "是一个单态射的事实, 可以推出"
    (&= (&i* $f (OverBar $a))
        (&i* $f (OverBar $a^)))
    ". 因此,"
    (MB (&!= (&= (app $f $a)
                 (app (@i* $f (OverBar $a)) $x))
             (&= (app (@i* $f (OverBar $a^)) $x)
                 (app $f $a^))))
    "于是" $f "是单射的." (Br)
    "反过来, 如果" $f "是单射的而"
    (Arrow (&cm $g $h) $C $A) "是函数满足"
    (&!= $g $h) ", 那么对于某个" (∈ $c $C)
    ", " (&!= (app $g $c) (app $h $c))
    ". 既然" $f "是单射, 我们可以推出"
    (&!= (app* $f $g $c) (app* $f $h $c))
    ", 于是" (&!= (&i* $f $g) (&i* $f $h)) ".")
   ((Example)
    "在许多" (Q "结构化集合") "的范畴之中, "
    "诸如幺半群的范畴, 单态射恰是" (Q "单射的同态")
    ". 更准确地说, 一个幺半群的同态" (Arrow $h $M $N)
    "是单态射恰当其潜在函数"
    (apply Arrow (map* forget $h $M $N))
    "是单态射, 根据前述命题即为单射. "
    "为了证明这点, 设" $h "是一个单态射并取两个不同的"
    (Q "元素") (Arrow (&cm $x $y) $1 (forget $M))
    ", 其中" (&= $1 (setE $*:id))
    "是任意的一个单元素集合. 根据自由幺半群"
    (free-monoid $1) "的UMP, 存在与之对应的不同同态"
    (Arrow (&cm (OverBar $x) (OverBar $y))
           (free-monoid $1) $M)
    ", 其与" $h "的复合"
    (&: (&cm (&compose $h (OverBar $x))
             (&compose $h (OverBar $y)))
        (&-> (free-monoid $1) $M $N))
    "亦不同, 鉴于" $h "是单态的. 因此, 相应的" (Q "元素")
    (Arrow (&cm (&i* $h $x) (&i* $h $y)) $1 $N)
    "也是不同的, 这又一次用到了" (free-monoid $1)
    "的UMP. {译注: 实际上, "
    (Arrow (&cm (&i* $h $x) (&i* $h $y)) $1 $N)
    "更严格来说应该写成是"
    (Arrow (&cm (&i* (forget $h) $x)
                (&i* (forget $h) $y))
           $1 (forget $N))
    ", 鉴于它们是" Sets "而非" Mon
    "之中的箭头. 另外, 最后这里用到UMP的意思是, 因为"
    (&compose $h (OverBar $x)) "是由" $N "和"
    (&i* (forget $h) $x) "在" (free-monoid $1)
    "的UMP {译注: 注意到其包含一个隐式但固定的嵌入映射} "
    "下确定的唯一同态, 而"
    (&compose $h (OverBar $y))
    "是由" $N "和" (&i* (forget $h) $y)
    "确定的唯一同态, 那么如果"
    (&= (&i* (forget $h) $x) (&i* (forget $h) $y))
    ", 也应该有"
    (&= (&compose $h (OverBar $x))
        (&compose $h (OverBar $y)))
    ", 否则的话就违背了唯一性, 由此推得"
    (&!= (&i* (forget $h) $x) (&i* (forget $h) $y))
    ". 最后, 还有一点值得提及, 虽然大概只是废话, "
    "之所以称映射" (&cm $x $y) "为" (Q "元素")
    ", 是因为确定" (&cm $x $y) "的只是集合"
    (forget $M) "里的一个元素, 而"
    (&cm (&i* (forget $h) $x) (&i* (forget $h) $y))
    "也是同理, 鉴于其定义域是单元素集合.}"
    (MB (free-monoid $1)
        (__^^ $rrarr
              (pad (OverBar $y))
              (pad (OverBar $x)))
        $M (^^ $-> (pad $h)) $N)
    (MB $1 (__^^ $rrarr (pad $y) (pad $x))
        (forget $M)
        (^^ $-> (pad (forget $h)))
        (forget $N))
    "反过来, 如果"
    (apply Arrow (map* forget $h $M $N))
    "是(" Sets "中的)单态射并且"
    (Arrow (&cm $f $g) $X $M)
    "是任意不同的(幺半群)同态, 那么"
    (Arrow (&cm (forget $f) (forget $g))
           (forget $X) (forget $M))
    "是不同的函数, 于是"
    (&: (&cm (&compose (forget $h) (forget $f))
             (&compose (forget $h) (forget $g)))
        (apply &-> (map* forget $X $M $N)))
    "也是不同的, 鉴于" (forget $h)
    "是一个单态射. 既然"
    (&!= (&= (forget (&compose $h $f))
             (&compose (forget $h) (forget $f)))
         (&= (&compose (forget $h) (forget $g))
             (forget (&compose $h $g))))
    ", 我们必然也有"
    (&!= (&compose $h $f) (&compose $h $g)) ".")
   (P "完全类似的情况依然成立, "
      "诸如对于群, 环, 向量空间, 以及偏序集. "
      "我们将会看到这个事实可以由每个这些范畴之中的"
      "特定对象 (例如这里是自由幺半群" (free-monoid $1)
      ") 的存在 (presence) 推出.")
   ((Example)
    "在一个偏序集" CatP
    "之中, 每个箭头既是单态射又是满态射. 为什么呢?")
   (P "现在, 和前文对偶地, " Sets "中的满态射恰为满射的函数 (练习!); "
      "然而, 作为对比的是, 在许多令人熟悉的范畴之中, "
      "满态射和满射的同态并不等价, 如以下例子所示.")
   ((Example #:id "N2Z")
    "在幺半群和幺半群同态的范畴" Mon
    "之中, 存在一个单态的同态"
    (MB (&>-> $NN $ZZ))
    "其中" $NN "是由自然数构成的加性幺半群"
    (tu0 $N $+:id $0) "而" $ZZ
    "是由整数构成的加性幺半群"
    (tu0 $Z $+:id $0)
    ". 我们表明这个映射, 由集合的嵌入 (inclusion) "
    (&sub $N $Z) "给出, 在" Mon
    "之中也是一个满态射, 这是藉由证明以下命题成立完成的:"
    (Blockquote
     "给定任意的幺半群同态"
     (Arrow (&cm $f $g)
            (tu0 $Z $+:id $0)
            (tu0 $M $*:id $u))
     ", 如果其于" $N "上的限制相等, 即"
     (&= (restrict $f $N) (restrict $g $N))
     ", 那么" (&= $f $g)
     ". {译注: " (restrict $f $N) "其实就相当于"
     (&compose $f $i) ", 其中" $i "是前述的嵌入.}"))
   ((tcomment)
    "这个例子其实更加特殊, 因为它给出了一个既是单态射"
    "又是满态射但却并非同构的例子. 不过, "
    "另外一个方向倒是正确的.")
   (P "首先, 我们注意到"
      (MB (deriv (app $f (&- $n))
                 (app $f (&+ (_ (@ $-1) $1)
                             (_ (@ $-1) $2)
                             $..c
                             (_ (@ $-1) $n)))
                 (&* (ap $f (_ (@ $-1) $1))
                     (ap $f (_ (@ $-1) $2))
                     $..c
                     (ap $f (_ (@ $-1) $n)))))
      "而对于" $g "也是类似的. 这足以表明"
      (&= (app $f $-1) (app $g $-1)) ", 无非"
      (MB (deriv (app $f $-1)
                 (&* (app $f $-1) $u)
                 (&* (app $f $-1) (app $g $0))
                 (&* (app $f $-1) (app $g (&- $1 $1)))
                 (&* (app $f $-1) (app $g $1) (app $g $-1))
                 (&* (app $f $-1) (app $f $1) (app $g $-1))
                 (&* (app $f (&+ $-1 $1)) (app $g $-1))
                 (&* (app $f $0) (app $g $-1))
                 (&* $u (app $g $-1))
                 (app $g $-1))))
   (P "我们应该注意到, 从代数的角度来看, 一个态射"
      $e "是满态射当且仅当" $e "可以从右侧消去: "
      (&= (&i* $x $e) (&i* $y $e)) "能够推出"
      (&= $x $y) ". 对偶地, " $m "是单态射当且仅当"
      $m "可以从左侧消去: " (&= (&i* $m $x) (&i* $m $y))
      "能够推出" (&= $x $y) ".")
   ((Proposition)
    (Em "每个同构既是单态射又是满态射."))
   ((proof)
    "考虑以下图表:"
    todo.svg
    "如果" $m "是一个同构并且其逆为" $e
    ", 那么" (&= (&i* $m $x) (&i* $m $y))
    "可以推出" (&= $x (&i* $e $m $x) (&i* $e $m $y) $y)
    ". 因此, " $m "是一个单态射. 类似地, "
    $e "可以从右侧消去, 因而也是一个满态射.")
   (H4. "Sections and retractions")
   (P "我们刚才注意到任意的同构既是单态射又是满态射. "
      "更一般地, 如果一个箭头"
      (MB (Arrow $f $A $B))
      "具有一个左逆"
      (MB (&cm (Arrow $g $B $A)
               (&= (&i* $g $f) $1_A)))
      "那么根据相同的论证方法, "
      $f "必然是单态射而" $g "必然是满态射.")
   ((Definition)
    "一个" (Em "split") "的单态射 (满态射) 是一个具有一个左 (右) 逆的箭头. "
    "对于箭头" (Arrow $e $X $A) "和" (Arrow $s $A $X)
    "满足" (&= (&i* $e $s) $1_A) ", 箭头" $s
    "被称为" $e "的一个" (Em "section") "或者"
    (Em "splitting") ", 而箭头" $e "被称为" $s
    "的一个" (Em "retraction") ". 对象" $A
    "被称为" $X "的一个" (Em "retract")
    ". {译注: 此定义的术语众多, 我想还是分开翻译比较清晰, "
    "其中section可以翻译为截面, retraction可以翻译为收缩, "
    "其他术语大概没有通行的翻译.}")
   (P "既然函子保持恒等箭头, 那么函子也保持" (Em "split")
      " epi和" (Em "split") " mono. {译注: 当然, "
      "其实也需要函子的其他保持性质.} "
      "请将其与之前" Mon "中的" (Ref "N2Z")
      "进行比较, 那里面的遗忘函子"
      (MB (&-> Mon Sets))
      "并没有保持例子里的满态射" (&-> $NN $ZZ) ".")
   ((Example)
    "在" Sets "之中, 每个单态射都能split, 除了具有形式"
    (MB (&>-> $empty $A))
    "的单态射. {译注: 更准确地说, " Sets
    "中的单态射能够split的充分必要条件是"
    "定义域为空集可以推出陪域为空集, 鉴于"
    (&-> $empty $empty) "是可逆的.} "
    (Em "每个满态射都能split")
    "的条件相当于范畴论版本的选择公理. "
    "的确如此, 考虑一个满态射"
    (MB (Epi $e $E $X) ".")
    "我们有以下由非空集合构成的族:"
    (MB (&cm (&= $E_x (ap (inv $e) (setE $x)))
             (∈ $x $X)) ".")
    "这个族" (_@ $E_x (∈ $x $X))
    "的一个选择函数恰是" $e
    "的一个splitting, 即一个函数"
    (Arrow $s $X $E) "使得"
    (&= (&i* $e $s) $1_X)
    ", 因为这意味着对于每个" (∈ $x $X)
    "都有" (∈ (app $s $x) $E_x) "." (Br)
    "反过来, 给定一个由非空集合构成的族"
    (MB (_@ $E_x (∈ $x $X)))
    "取"
    (&= $E (setI (tu0 $x $y)
                 (&cm (∈ $x $X)
                      (∈ $y $E_x))))
    "并定义满态射" (Epi $e $E $X)
    "为" (&\|-> (tu0 $x $y) $x)
    ". 那么, " $e "的一个splitting " $s
    "确定了这个族的一个选择函数. "
    "{译注: 从某种意义上说, " $E
    "是由" (Q "依赖序对") "构成的.}")
   (P (Q "对象的族") (_@ $E_x (∈ $x $X))
      "可以藉由一个单独的箭头"
      (Arrow $e $E $X)
      "通过" (Q "纤维")
      (ap (inv $e) (setE $x))
      "表示的想法有着远比这里广泛的应用, "
      "我们将于第7.10节进一步考虑这个东西.")
   (P "与" (Q "选择函数") "的存在性有关的一个概念是"
      (Q "投影性 (projective)") ": 一个对象" $P
      "被称为是" (Em "投影性")
      "的, 如果对于任意的满态射"
      (Epi $e $E $X) "和箭头" (Arrow $f $P $X)
      ", 存在某个(未必唯一的)箭头"
      (Arrow (OverBar $f) $P $E) "满足"
      (&= (&compose $e (OverBar $f)) $f)
      ", 如以下图表所示:"
      todo.svg
      "我们称" (Em $f " lifts across " $e)
      ". 任意进入某个投影对象的满态射都显然可以split. "
      "{译注: 进入的意思是以该对象为陪域.} "
      "投影对象或许可以被认为是拥有一种更为"
      (Q "自由") "的结构, 因而允许"
      (Q "更多的箭头") ".")
   (P "选择公理可以推出所有的集合都是投影性的, "
      "因而在许多(但并非全部!)的代数范畴之中"
      "自由对象也是投影性的. 读者应该能够证明, "
      "在任意的范畴之中, 一个投影对象的任何"
      "retract也都是投影性的.")
   ((tcomment)
    "现在我们证明一下以上的论断. 对于任意的范畴"
    CatC "中的一个投影对象" $P ", 如果" $Q "是" $P
    "的一个retract, 那么根据定义, 存在一对箭头"
    (Arrow $a $Q $P) "和" (Arrow $b $P $Q)
    "使得" (&= (&compose $b $a) $1_Q)
    ". 对于满态射" (Epi $e $E $X) "和任意的箭头"
    (Arrow $g $Q $X) ", 我们知道"
    (Arrow (&compose $g $b) $P $X)
    ". 鉴于" $P "是一个投影对象, 那么存在一个箭头"
    (Arrow (OverBar (&compose $g $b)) $P $E)
    "使得"
    (&= (&compose $e (OverBar (&compose $g $b)))
        (&compose $g $b))
    ", 这可以推出"
    (MB (deriv
         (&compose
          $e (@compose
              (OverBar (&compose $g $b))
              $a))
         (&compose
          (@compose
           $e (OverBar (&compose $g $b)))
          $a)
         (&compose (@compose $g $b) $a)
         (&compose $g (@compose $b $a))
         (&compose $g $1_Q)
         $g))
    "令" (Arrow (OverBar $g) $Q $E) "为"
    (&compose (OverBar (&compose $g $b)) $a)
    ", 那么" (&= (&compose $e (OverBar $g)) $g)
    ", 这告诉我们" $Q "也是一个投影对象.")
   (H3. "始对象和终对象" #:id "initial-and-terminal")
   (P "我们现在考虑对于范畴" Sets "中的空集和单元素集"
      "以及一般性的范畴之中具有类似结构的对象进行抽象刻画.")
   ((Definition)
    "在任意的范畴" CatC "之中, 一个对象"
    (Ul (Li $0 "是" (Em "始(initial)")
            "的, 如果对于任意的对象" $C
            ", 存在唯一的态射"
            (MB (&-> $0 $C) ";"))
        (Li $1 "是" (Em "终(terminal)")
            "的, 如果对于任意的对象" $C
            ", 存在唯一的态射"
            (MB (&-> $C $1) "."))))
   (P "和单态射和满态射的情况一样, "
      "我们应该注意到以上的定义中存在着某种"
      (Q "对偶性") ". 精确地说, " CatC
      "中的一个终对象恰是" (&op CatC)
      "中的一个始对象. 第3章中我们将会系统地考虑这种对偶性.")
   (P "首先, 观察到始对象和终对象的概念是简单的UMP, "
      "这样的对象在同构下是唯一确定的, 就像之前的自由幺半群.")
   ((Proposition)
    (Em "始对象(终对象)在同构意义下是唯一确定的."))
   ((proof)
    "实际上, 如果" $C "和" $C^ "都是同一范畴中的始对象(或者终对象), "
    "那么存在" (Em "唯一") "的同构" (&-> $C $C^)
    ". 的确如此, 设" $0 "和" $0^ "都是某个范畴" CatC
    "之中的始对象; 那么, 以下的图表使得" $0 "和" $0^
    "是唯一同构的这一事实变得清晰起来:"
    todo.svg
    "至于终对象, 应用前述论证于" (&op CatC) ".")
   ((Example)
    (Ol (Li "在" Sets "之中, 空集是始对象而任意的单元素集都是终对象. "
            "观察到" Sets "只有一个始对象而却有许多终对象 "
            "(这回答了是否有" (&cong Sets (&op Sets)) "的问题).")
        (Li "在" Cat "之中, 范畴" Cat0 " (没有对象也没有箭头) "
            "是始对象而范畴" Cat1 " (一个对象和它的恒等箭头) "
            "是终对象.")
        (Li "在" Groups "之中, 单元素的群" (Em "既是")
            "始对象" (Em "又是") "终对象 (向量空间和线性变换的范畴也是类似的, "
            "幺半群和幺半群同态的范畴亦是如此). 但是, 在" Rings
            " (含幺交换环的范畴) 之中, 整数环" $ZZ
            "是始对象 (而" (&= $0 $1) "的单元素环是终对象).")
        (Li "一个" (Em "布尔代数") "是一个偏序集" $B
            ", 其装备了两个突出的元素" $0 "和" $1
            ", 二元运算" (Q "join") (&join $a $b) "和"
            (Q "meet") (&meet $a $b) ", 以及一个幺元运算"
            (Q "补") (&neg $b) ". 以下是其必须满足的条件:"
            (eqn*
             ($ (&<= $0 $a) $)
             ($ (&<= $a $1) $)
             ((: (&<= $a $c) "且" (&<= $b $c))
              "当且仅当"
              (&<= (&join $a $b) $c))
             ((: (&<= $c $a) "且" (&<= $c $b))
              "当且仅当"
              (&<= $c (&meet $a $b)))
             ((&<= $a (&neg $b))
              "当且仅当"
              (&= (&meet $a $b) $0))
             ($ (&= (&neg (&neg $a)) $a) $))
            "这里也存在着一种等价的定义, "
            "其为全然等式性的刻画而不牵涉序关系. "
            "布尔代数的一个典型例子是一个集合"
            $X "的所有子集" (&sube $A $X)
            "构成的幂集" (powerset $X)
            ", 其由包含关系" (&sube $A $B)
            "有序化, 而布尔运算为空集"
            (&= $0 $empty) ", 全集"
            (&= $1 $X) ", 并集与交集作为join和meet, "
            "以及相对补" (&- $X $A) "作为"
            (&neg $A) ". 一个令人熟悉的特别例子是二元素的布尔代数"
            (&= $2 (setE $0 $1)) " (其或可以取幂集"
            (powerset $1) "), 有时其也被视为" (Q "真值集")
            ", 以逻辑运算析取, 合取, 否定作为布尔运算. "
            "这在布尔代数的范畴" BA "里是一个始对象. "
            BA "的箭头是布尔同态, 即保持额外结构的函子"
            (Functor $h $B $B^) ", 意即满足"
            (&= (app $h $0) $0) ", "
            (&= (app $h (&join $a $b))
                (&join (app $h $a) (app $h $b)))
            ", 诸如此类. 单元素的布尔代数 (例如"
            (powerset $0) ") 是终对象.")
        (Li "在一个偏序集之中, 一个对象是始对象当且仅当其是最小元素, "
            "一个对象是终对象当且仅当其是最大元素, "
            "这是平然的. 因此, 例如, 任意的布尔代数两者皆有. "
            "显然, 一个范畴" (Em "不必拥有")
            "始对象或者终对象; 例如, 偏序集"
            (tu0 $ZZ $<=:id) "两者皆无.")
        (Li "对于任意的范畴" CatC "和任意的对象"
            (∈ $X CatC) ", 恒等箭头"
            (Arrow $1_X $X $X)
            "在切片范畴" (slice CatC $X)
            "里是一个终对象而在余切片范畴"
            (coslice $X CatC)
            "里是一个始对象.")))
   (H3. "广义元素")
   (P "让我们考虑进出始对象和终对象的箭头. "
      "显然只有其中一些是令人感兴趣的, "
      "但是令人感兴趣的一般的都是尤其重要的.")
   (P "一个集合" $A "拥有一个进入始对象的箭头"
      (&-> $A $0) "恰当其自身是始对象, "
      "而这对于偏序集来说也是一样. "
      "与之相对的是, 在幺半群和群中, "
      "每个对象都有一个到始对象的唯一箭头, "
      "鉴于始对象也是终对象.")
   (P "然而, 在布尔代数的范畴" BA "之中, "
      "情况就相当不同了. 进入始布尔代数"
      $2 "的映射" (Arrow $p $B $2)
      "唯一地对应于所谓" $B
      "中的" (Em "超滤") $U
      ". 一个布尔代数" $B "中的一个" (Em "滤子")
      "是一个向上封闭且在meet下封闭的非空子集"
      (&sube $F $B) ":"
      (MB (&Table
           ((∈ $a $F) "且" (&<= $a $b) "可以推出" (∈ $b $F))
           ((∈ $a $F) "且" (∈ $b $F) "可以推出" (∈ (&meet $a $b) $F))))
      "一个滤子" $F "是" (Em "极大") "的, 如果唯一严格更大的滤子"
      (&sub $F $F^) "只有" (Q "非真 (improper)")
      "滤子, 即整个" $B ". {译注: 因为不等于" $B "的滤子叫做" $B
      "的真 (proper) 滤子.} 一个" (Em "超滤") "是一个极大滤子. "
      "并不难看出, 一个滤子" $F "是一个超滤恰当对于每个元素"
      (∈ $b $B) ", 要么" (∈ $b $F) ", 要么" (∈ (&neg $b) $F)
      ", 但是不能两者同时存在 (练习!). 现在如果"
      (Arrow $p $B $2) ", 令" (&= $U_p (app (inv $p) $1))
      "以得到一个超滤" (&sub $U_p $B) ". 并且, 对于一个超滤"
      (&sub $U $B) ", 定义" (&= (app $p_U $b) $1) "当且仅当"
      (∈ $b $U) "以得到一个布尔同态" (Arrow $p_U $B $2)
      ". 这很容易验证, 因为事实上这些操作是互逆的. "
      "布尔同态" (&-> $B $2) "也用于构造我们在逻辑中遇到的"
      (Q "真值表") ". 诚然如此, 一个真值表里的一行对应于"
      "一个公式的布尔代数 (Boolean algebra of formulas) "
      "上的这样一个同态. {译注: 一个真值表里的一行"
      "相当于一个赋值.}")
   (P "进入始环" $ZZ "的环同态" (&-> $A $ZZ)
      "在代数几何中扮演着类似而同等重要的角色. "
      "它们对应于所谓的" (Em "素理想")
      ", 其是超滤的环论推广.")
   (P "现在让我们来考虑从终对象出发的一些箭头. "
      "例如, 对于任意的集合" $X ", 我们有一个元素"
      (∈ $x $X) "和箭头" (Arrow (OverBar $x) $1 $X)
      " (其由" (&= (app (OverBar $x) $*:id) $x)
      "确定, 而从某个终对象" (&= $1 (setE $*:id))
      "出发) 之间的同构"
      (MB (&cong $X (Hom Sets $1 $X)) ".")
      "我们已经用过这种对应多次了. "
      "类似的情形在偏序集 (和拓扑空间) 中也成立, "
      "其中的箭头" (&-> $1 $P)
      "对应于偏序集 (或者空间) " $P
      "的潜在集合的元素. 在任意的具有一个终对象"
      $1 "的范畴之中, 这样的箭头" (&-> $1 $A)
      "经常被称为" $A "的" (Em "全局元素")
      ", 或者说" (Em "点") ", 或者说" (Em "常量")
      ". 在集合, 偏序集, 以及空间之中, "
      "一般的箭头" (&-> $A $B) "由其对于"
      $A "的点的所作所为确定, 其意义是两个箭头"
      (Arrow (&cm $f $g) $A $B)
      "相等, 如果对于每个点" (Arrow $a $1 $A)
      ", 复合都相等, 即"
      (&= (&i* $f $a) (&i* $g $a)) ".")
   (P "但是, 请小心; 并非总是如此! "
      "在幺半群的范畴里, 一个对象" $M
      "有多少个点呢? 也就是说, 对于一个给定的幺半群"
      $M ", 存在多少个具有形式" (&-> $1 $M)
      "的箭头呢? 只有一个! 另外, "
      "一个布尔代数有多少个点呢?")
   (P "鉴于一般情形下一个对象并不由其点所确定, 引入"
      (Em "广义元素(generalized element)")
      "这一设备是方便的, 其是任意的箭头"
      (MB (Arrow $x $X $A))
      "(带有任意的定义域" $X "), 可以被视为" $A
      "的" (Em "泛化(generalized)") "或者"
      (Em "可变(variable)")
      "元素. 计算机科学家和逻辑学家有时将箭头"
      (&-> $1 $A) "想成是常量或者封闭项, "
      "而将一般箭头" (&-> $X $A)
      "想成是任意的项. 总结:")
   ((Example)
    (Ol (Li "考虑" Pos "中的箭头"
            (Arrow (&cm $f $g) $P $Q)
            ", 那么" (&= $f $g)
            "当且仅当对于所有的"
            (Arrow $x $1 $P)
            ", 我们都有"
            (&= (&i* $f $x) (&i* $g $x))
            ". 在这种意义下, 可以说偏序集"
            (Q "拥有足够多的点")
            "用来分离箭头.")
        (Li "与之相对的是, 在" Mon
            "之中, 对于同态"
            (Arrow (&cm $h $j) $M $N)
            ", 我们总有"
            (&= (&i* $h $x) (&i* $j $x))
            ", 其中" (Arrow $x $1 $M)
            "是任意的箭头, 这是因为只存在着一个这样的点"
            $x ". 因此, 幺半群"
            (Q "并没有足够多的点") ".")
        (Li "但是在任意的范畴" CatC
            "之中, 对于任意的箭头"
            (Arrow (&cm $f $g) $C $D)
            ", 我们总有" (&= $f $g)
            "当且仅当对于所有的"
            (Arrow $x $X $C) "都有"
            (&= (&i* $f $x) (&i* $g $x))
            "成立 (为什么?). 因此, "
            "可以说所有的对象都拥有足够多的广义元素.")
        (Li "实际上, 往往考虑具有某种特定形式"
            (&-> $T $A) "的广义元素就够了, "
            "也就是对于特定的" (Q "测试") "对象" $T
            ". 我们很快就要考虑这件事情.")))
   (P "广义元素也对于" (Q "测试") "各种各样的条件有用. "
      "例如, 考虑具有以下形状的图表:"
      (MB $X (__^^ $rrarr (pad $x^) (pad $x))
          $A (^^ $-> (pad $f)) $B)
      "箭头" $f "是单态射当且仅当" (&!= $x $x^)
      "可以推出" (&= (&i* $f $x) (&i* $f $x^))
      ", 也就是说, 恰当" $f
      (Q "在广义元素上是单射") ".")
   (P "类似地, 在任意的范畴" CatC "之中, "
      "为了测试一个方块是否交换,"
      todo.svg
      "我们将有" (&= (&i* $alpha $f) (&i* $beta $g))
      ", 恰当对于所有的广义元素" (Arrow $x $X $A)
      "都有" (&= (&i* $alpha $f $x) (&i* $beta $g $x))
      " (仅取" (&= $x (Arrow $1_A $A $A)) "即可看出).")
   ((Example)
    "广义元素和常量元素相比可以用来"
    (Q "揭示更多的结构") ". 例如, 考虑以下的偏序集"
    $X "和" $A ":"
    (MB (&= $X (setE (&cm (&<= $x $y) (&<= $x $z)))))
    (MB (&= $A (setE (&<= $a $b $c))))
    "存在着一个保序的双射" (Arrow $f $X $A) ", 其定义为"
    (MB (&cm (&= (app $f $x) $a)
             (&= (app $f $y) $b)
             (&= (app $f $z) $c)) ".")
    "很容易看出来" $f "在范畴" Pos
    "之中既是单态射又是满态射; "
    "然而, 显然其也并非同构. "
    "{译注: 前半句容易是因为这个保序映射(通过遗忘)也是映射, "
    "而其作为映射既是单射又是满射; "
    "后半句看" $f "作为映射的逆是否保序即可.} "
    "我们想说" $X "和" $A "是" (Q "不同的结构")
    ", 而诚然它们并非同构恰是说明此事. "
    "但是既然如此, 如何" (Em "证明")
    "它们" (Em "并非") "同构的呢? "
    "(难道说要藉由某个其他的" (&-> $X $A) "吗?) "
    "在一般情形下, 这种事情可能相当困难.")
   (P "一种证明两个对象并非同构的方法是使用"
      (Q "不变量") ": 同构所保持的属性. "
      "如果两个对象根据某个不变量是不同的, "
      "那么它们就不可能是同构的. "
      "广义元素提供了一种简单的方式来定义不变量. "
      "例如, " $X "和" $A "的全局元素的数目是相同的, "
      "即这两个集合都有三个元素. "
      "但是, 如果转而考虑" (Q $2 "-元素")
      ", 即以偏序集" (&= $2 (setE (&<= $0 $1)))
      "作为一个" (Q "测试对象") ", 那么"
      $X "有" $5 "个这样的元素, " $A
      "却有" $6 "个. 既然这些数目是不变量, "
      "那么这两个偏序集就不可能是同构的. "
      "更细致地说, 我们可以定义对于任意的偏序集"
      $P "而言的数值不变量"
      (MB (&= (card (Hom $2 $P))
              (: (Hom $2 $P)
                 "的元素数目")) ".")
      "那么, 如果" (&cong $P $Q)
      ", 很容易看出来"
      (&= (card (Hom $2 $P))
          (card (Hom $2 $Q)))
      ", 因为任意的同构"
      (MB $P (__^^ $rlarr (pad $j) (pad $i)) $Q)
      "也给出了另一个同构"
      (MB (Hom $2 $P)
          (__^^ $rlarr (pad (_ $j $*:id))
                (pad (_ $i $*:id)))
          (Hom $2 $Q))
      "其由复合定义:"
      (MB (&= (app (_ $i $*:id) $f)
              (&i* $i $f)))
      (MB (&= (app (_ $j $*:id) $g)
              (&i* $j $g)))
      "其中" (Arrow $f $2 $P) "而"
      (Arrow $g $2 $Q)
      ". 的确如此, 实际上其是"
      (Hom $X $dummy)
      "总为一个函子这一非常一般性的事实的特殊情形, "
      "而函子总是保持同构的.")
   ((Example)
    "正如在之前的例子里, " (Q "坐落于")
    "某个特定对象" $T "的广义元素"
    (Arrow $t $T $A) "往往是特别具有" (Q "揭露性的")
    ". 我们可以几何地将这样的元素想成是"
    (Q "形状" $T "在" $A "中的轮廓")
    ", 正如偏序集的范畴中的一个箭头" (&-> $2 $P)
    "是形状" (&<= $p $p^) "在" $P
    "中的一个轮廓. 例如, 正如我们已经看到了的, "
    "在幺半群的范畴之中, 从终幺半群出发的箭头提供不了任何信息, "
    "而那些从一个生成元上的自由幺半群" (free-monoid $1)
    "出发的箭头足够用以区分同态, 意即两个同态"
    (Arrow (&cm $f $g) $M $M^)
    "相等, 如果它们与所有这样的箭头的复合都相等. "
    "{译注: " (Q "一个生成元上的自由幺半群")
    "的原文是" (Q "the free monoid on one generator")
    ", 意即由单独某个生成元所生成的自由幺半群. "
    "当然这些幺半群都是同构的, 所以我们可以将它们视为同一个, "
    "于是原文使用的冠词是" (Q "the") ".} "
    "既然我们知道" (&= (free-monoid $1) $NN)
    ", 即自然数的幺半群, 我们可以将坐落在" (free-monoid $1)
    "的广义元素" (&-> (free-monoid $1) $M) "想成是"
    (Q "形状" $NN "在" $M "中的轮廓")
    ". 实际上, 根据" (free-monoid $1)
    "的UMP, 潜在集" (forget $M) "因而是 (或者应该说同构于) "
    "由所有这样的轮廓构成的集合" (Hom Mon $NN $M) ", 鉴于"
    (MB (&cong (forget $M)
               (Hom Sets $1 (forget $M))
               (Hom Mon $NN $M)) ".")
    "{译注: 我们已经知道" (&\|-> $f (OverBar $f))
    "是一个单射, 而对于"
    (Arrow $m (free-monoid $1) $M)
    ", 容易看出"
    (&= (OverBar (&compose (forget $m) $i)) $m)
    ".} 在这种意义下, 从某个幺半群出发的箭头"
    "由其施加于所有的形状" $NN
    "在这个幺半群中的轮廓上的影响 (effect) "
    "所确定. {译注: 这句话和前一句话没有十分紧密的逻辑联系, "
    "更像是对于例子开头引入幺半群时的话语的重述. "
    "不过, 或许可以再解释一下, 其实根据UMP, "
    "我们知道" (Arrow $m $NN $M) "完全由值"
    (app $m $1) "确定, 这里的" $1
    "是自然数, 而且其可以是任意的" $M
    "的元素, 明白这点就足以知道为什么从"
    (free-monoid $1) "出发的箭头足以用来区分同态"
    (Arrow (&cm $f $g) $M $M^) "了.}")
   (H3. "积")
   (P "接下来, 我们将看到对于一个范畴之中的两个对象之积的范畴论定义. "
      "这首先是由Mac Lane在1950年给出的, "
      "而这可能是范畴论用来定义数学基本概念的最早例子.")
   (P "所谓" (Q "定义") ", 我这里的意思是一个抽象构造, "
      "这已经在之前有所涉及, 其基于一个范畴中的对象和箭头. "
      "并且和之前一样, 我们所要做的是给出一个UMP, "
      "其在同构意义下确定了当前所关心的结构, "
      "范畴论中通常都是这样. 在之后的章节里, "
      "我们还会有诸多其他的这种构造的例子.")
   (P "让我们从考虑集合的积开始. 对于集合" $A "和" $B
      ", " $A "和" $B "的" (Em "笛卡尔积")
      "是序对的集合"
      (MB (&= (&c* $A $B)
              (setI (tu0 $a $b)
                    (&cm (∈ $a $A)
                         (∈ $b $B)))) ".")
      "观察到存在着两个" (Q "坐标投影")
      (MB $A (^^ $<- (pad $pi_1)) (&c* $A $B)
          (^^ $-> (pad $pi_2)) $B)
      "其满足"
      (MB (&cm (&= (appl $pi_1 $a $b) $a)
               (&= (appl $pi_2 $a $b) $b)) ".")
      "的确如此, 对于任意的元素" (∈ $c (&c* $A $B))
      ", 我们都有"
      (MB (&= $c (tu0 (ap $pi_1 $c) (ap $pi_2 $c))) ".")
      "这种情况由以下图表所精确捕获:"
      set-product.svg
      "将元素代之以广义元素, 我们就得到了以下定义.")
   ((Definition)
    "在任意的范畴" CatC "中, 对于对象" $A "和" $B
    "而言, 一个" (Em "积图表(product diagram)")
    "由一个对象" $P "和箭头"
    (LeftRightDiagram $A $p_1 $P $p_2 $B)
    "构成, 其满足以下的UMP:" (Br)
    "对于任意的具有形式"
    (LeftRightDiagram $A $x_1 $X $x_2 $B)
    "的图表, 都存在着唯一的" (Arrow $u $X $P)
    "使得以下图表"
    UMP:product.svg
    "交换. 换言之, 即" (&= $x_1 (&i* $p_1 $u))
    "而" (&= $x_2 (&i* $p_2 $u)) ".")
   ((Remark)
    "和其他UMP一样, 其具有两个部分:"
    (Ul (Li (Em "存在性")
            ": 存在着某个" (Arrow $u $X $P)
            "使得" (&= $x_1 (&i* $p_1 $u))
            "而" (&= $x_2 (&i* $p_2 $u)) ".")
        (Li (Em "唯一性")
            ": 对于任意的" (Arrow $v $X $P)
            ", 如果" (&= $x_1 (&i* $p_1 $v))
            "且" (&= $x_2 (&i* $p_2 $v))
            ", 那么" (&= $v $u) ".")))
   ((Proposition #:id "product-uniqueness")
    (Em "积在同构下唯一确定."))
   ((proof)
    "设"
    (LeftRightDiagram $A $p_1 $P $p_2 $B)
    "和"
    (LeftRightDiagram $A $q_1 $Q $q_2 $B)
    "是" $A "和" $B "之积, 那么既然" $Q
    "是一个积, 存在着唯一的"
    (Arrow $i $P $Q) "使得"
    (&= (&compose $q_1 $i) $p_1) "而"
    (&= (&compose $q_2 $i) $p_2)
    ". 类似地, 既然" $P "是一个积, 那么存在着唯一的"
    (Arrow $j $Q $P) "使得"
    (&= (&compose $p_1 $j) $q_1) "而"
    (&= (&compose $p_2 $j) $q_2) "."
    todo.svg
    "通过复合, 我们得到" (&= (&compose $p_1 $j $i) $p_1)
    "而" (&= (&compose $p_2 $j $i) $p_2)
    ". 既然也有" (&= (&compose $p_1 $1_P) $p_1) "而"
    (&= (&compose $p_2 $1_P) $p_2)
    ", 根据唯一性条件可知" (&= (&compose $j $i) $1_P)
    ". 类似地, 我们可以证明" (&= (&compose $i $j) $1_Q)
    ". 因此, " (Arrow $i $P $Q) "是一个同构.")
   (P "如果" $A "和" $B "具有一个积, "
      "那么我们对于一个这样的积记下"
      (LeftRightDiagram
       $A $p_1 (&c* $A $B) $p_2 $B)
      "然后, 之于定义中出现的" (&cm $X $x_1 $x_2)
      ", 我们将"
      (MB (Arrow $u $X (&c* $A $B))
          "记为"
          (tupa0 $x_1 $x_2) "."))
   (P "然而, 我们应该注意到对于一对对象, "
      "范畴里可能存在着许多不同的积. "
      "例如, 给定一个积" (&cm (&c* $A $B) $p_1 $p_2)
      "和任意的同构" (Arrow $h $Q (&c* $A $B)) ", 图表"
      (&cm $Q (&compose $p_1 $h) (&compose $p_2 $h))
      "也是" $A "和" $B "的一个积. {译注: 原文写的是"
      (Arrow $h (&c* $A $B) $Q) ", 但是实际上应该是"
      (Arrow $h $Q (&c* $A $B)) ", 不然的话都没有办法进行复合.}")
   ((tcomment)
    "让我们对于前一段作简单的补充说明, 即为何"
    (&cm $Q (&compose $p_1 $h) (&compose $p_2 $h))
    "也是" $A "和" $B "的一个积. 对于箭头"
    (Arrow $x_1 $X $A) "和" (Arrow $x_2 $X $B)
    ", 我们知道存在唯一的箭头"
    (Arrow (tupa0 $x_1 $x_2) $X (&c* $A $B))
    "使得"
    (&= (&compose $p_1 (tupa0 $x_1 $x_2)) $x_1) "而"
    (&= (&compose $p_2 (tupa0 $x_1 $x_2)) $x_2)
    ". 现在设" $h "的逆为" (Arrow $g (&c* $A $B) $Q)
    ", 那么" (Arrow (&compose $g (tupa0 $x_1 $x_2)) $X $Q)
    "满足"
    (MB (deriv
         (&compose (@compose $p_1 $h)
                   (@compose $g (tupa0 $x_1 $x_2)))
         (&compose $p_1 (@compose $h $g)
                   (tupa0 $x_1 $x_2))
         (&compose $p_1 (tupa0 $x_1 $x_2))
         $x_1))
    "而"
    (MB (deriv
         (&compose (@compose $p_2 $h)
                   (@compose $g (tupa0 $x_1 $x_2)))
         (&compose $p_2 (@compose $h $g)
                   (tupa0 $x_1 $x_2))
         (&compose $p_2 (tupa0 $x_1 $x_2))
         $x_2))
    "这解决了存在性的部分, 那么现在让我们来说明唯一性. "
    "假如对于" (Arrow $x_1 $X $A) "和" (Arrow $x_2 $X $B)
    ", 箭头" (Arrow (&cm $f_1 $f_2) $X $Q)
    "都符合我们的要求, 那么我们发现"
    (MB (&= (&compose (@compose $p_1 $h) $f_1)
            $x_1
            (&compose $p_1 (@compose $h $f_1))))
    (MB (&= (&compose (@compose $p_2 $h) $f_1)
            $x_2
            (&compose $p_2 (@compose $h $f_1))))
    (MB (&= (&compose (@compose $p_1 $h) $f_2)
            $x_1
            (&compose $p_1 (@compose $h $f_2))))
    (MB (&= (&compose (@compose $p_2 $h) $f_2)
            $x_2
            (&compose $p_2 (@compose $h $f_2))))
    "鉴于" (&cm (&c* $A $B) $p_1 $p_2)
    "也是一个积, 根据唯一性条件, 我们可以得到"
    (MB (&= (&compose $h $f_1)
            (&compose $h $f_2)))
    "于是"
    (MB (deriv $f_1
               (&compose $1_Q $f_1)
               (&compose (@compose $g $h) $f_1)
               (&compose $g (@compose $h $f_1))
               (&compose $g (@compose $h $f_2))
               (&compose (@compose $g $h) $f_2)
               (&compose $1_Q $f_2)
               $f_2))
    "这就说明了唯一性. 当然了, " (&= $f_1 $f_2)
    "也可以通过" (Q "同构也是单态射") "直接推得.")
   (P "现在一个" (Em "进入") "某个积的箭头"
      (MB (Arrow $f $X (&c* $A $B)))
      "和一对箭头"
      (MB (&cm (Arrow $f_1 $X $A)
               (Arrow $f_2 $X $B)))
      "是" (Q "相同的东西")
      ". 因此, 基本上我们可以遗忘这样的箭头 {译注: 指"
      $f "这样的}, 在于它们由箭头的序对唯一地确定. "
      "但是, 如果一个范畴具有积, 那么我们可以获得有用的东西; "
      "即, 让我们来考虑从积" (Em "出发") "的箭头"
      (MB (Arrow $g (&c* $A $B) $Y))
      "这样一个" $g "是一个" (Q "具有两个变元的函数")
      "; 对于任意的两个广义元素" (Arrow $f_1 $X $A)
      "和" (Arrow $f_2 $X $B) ", 我们有一个元素"
      (Arrow (&i* $g (tupa0 $f_1 $f_2)) $X $Y)
      ". 这样的箭头" (Arrow $g (&c* $A $B) $Y)
      "不可" (Q "归约") "至任何更为基本的东西, "
      "但是进入积的箭头却可以 (实际上, 它们和"
      (Q "指数对象") $Y^B "的概念有关, 通过"
      (Q "currying") (Arrow (&lambda $g) $A $Y^B)
      "; 我们将于第6章进一步讨论这个东西).")
   ((tcomment)
    "对于前一段, 或许有些该解释的东西. 首先, 开头的"
    (MB (Arrow $f $X (&c* $A $B)))
    "和"
    (MB (&cm (Arrow $f_1 $X $A)
             (Arrow $f_2 $X $B)))
    "是" (Q "相同的东西")
    ", 这里的意思是" $f "和" (&cm $f_1 $f_2)
    "之间可以相互确定. 由" (&cm $f_1 $f_2)
    "确定" $f "是直接的UMP. 而反过来, 如果我们拥有的是"
    $f ", 那么可以取" (&= $f_1 (&compose $p_1 $f))
    "和" (&= $f_2 (&compose $p_2 $f))
    ", 此情形下" $f "则是由" $f_1 "和" $f_2
    "在UMP下确定的, 并且易知如果" (_prime $f $1)
    "和" (_prime $f $2) "所确定的箭头也是"
    $f ", 那么" (&= $f_1 (_prime $f $1))
    "且" (&= $f_2 (_prime $f $2))
    ", 鉴于UMP中出现的复合操作的确是一个运算, "
    "也就是说只能给出同一个结果. 另外, "
    "原文这里最后的括号或许有点令人迷惑, "
    "一个是" (Q "它们") "指的是"
    (Q "像" $g "这样的箭头")
    ", 另一个是原文的" (Q (&lambda $f))
    "在译者看来应该是一个笔误, 其实应该是"
    (Q (&lambda $g)) ", 也不知为何会这样.")
   (H3. "积的例子")
   (Ol (Li "我们已经见过了集合的笛卡尔积. "
           "注意到如果我们对于序对"
           (tupa0 $a $b) "选择了不同的定义, "
           "那么我们就得到了不同的集合"
           (MB (&c* $A $B) "和"
               (&c*^ $A $B))
           "它们每个都是一个积(的一部分), "
           "因而它们是同构的. "
           "例如, 我们可以置"
           (MB (&= (tupa0 $a $b)
                   (setE (setE $a) (setE $a $b))))
           (MB (&= (&prime (tupa0 $a $b))
                   (tupa0 $a (tupa0 $a $b)))))
       (Li "诸如幺半群或者群这样的" (Q "结构化集合")
           "的积经常可以被构造为其潜在集合的积, "
           "而运算则是" (Em "逐分量的")
           ": 例如, 如果" $G "和" $H
           "是群, 那么" (&c* $G $H)
           "可以按照以下方式构造出来: 取"
           (&c* $G $H) "的潜在集为集合"
           (setI (tupa0 $g $h)
                 (&cm (∈ $g $G) (∈ $h $H)))
           ", 并定义二元运算为"
           (MB (&= (apply &d* (map2* tupa0 $g $h $g^ $h^))
                   (apply tupa0 (map2* &d* $g $g^ $h $h^))))
           "单位元为"
           (MB (&= $u (tupa0 $u_G $u_H)))
           "逆为"
           (MB (&= (inv (tupa0 $a $b))
                   (tupa0 (inv $a) (inv $b))))
           "投影同态" (&-> (&c* $G $H) $G)
           "显然是" (&\|-> (tupa0 $g $h) $g)
           ", 另一个投影同态是类似的.")
       (Li "类似地, 对于范畴" CatC "和" CatD
           ", 我们已经定义过了由对象和序对和箭头的序对构成的范畴"
           (MB (&c* CatC CatD))
           "和显然的投影函子一道, 这的确构成了"
           Cat "中的一个积 (当" CatC "和" CatD
           "都为小范畴时). (检查这个事实: "
           "对于这样定义的积范畴验证UMP.) "
           "{译注: 范畴之积是更为一般的操作, 而"
           Cat "仅由小范畴(和其间函子)构成.}" (Br)
           "作为特殊情形, 我们可以将偏序集的积和幺半群的积"
           "当作范畴的积. (检查这个事实: "
           "鉴于投影和唯一的配对函数总是单调的, "
           Cat "中我们所构造的偏序集之积也是" Pos
           "中的一个积, " Mon "的情况是类似的.)")
       (Li "令" $P "是一个偏序集并考虑元素"
           (∈ $p $q $P) "的一个积. 我们必然有"
           (MB (&<= (&c* $p $q) $p))
           (MB (&<= (&c* $p $q) $q))
           "并且如果"
           (MB (&<= $x $p) "且" (&<= $x $q))
           "那么我们需要"
           (MB (&<= $x (&c* $p $q)))
           "你看出来这个操作" (&c* $p $q)
           "是什么了吗? 它不过就是通常被称为"
           (Em "最大下界") "的东西: "
           (&= (&c* $p $q) (&meet $p $q))
           ". 我们之后将会看到, "
           "许多其他格论概念也是范畴论概念的特殊情形.")
       (Li "(对于那些知道点关于拓扑的东西的人.) "
           "让我们来说明如通常所定义的那样的两个"
           (Em "拓扑空间") (&cm $X $Y)
           "之积, 的确是" Top "中的一个积, "
           Top "即拓扑空间和连续函数的范畴. "
           "因此, 设我们拥有空间" $X "和" $Y
           ", 以及积空间" (&c* $X $Y)
           "和它的投影"
           (LeftRightDiagram
            $X $p_1 (&c* $X $Y) $p_2 $Y)
           "回忆一下, " (open (&c* $X $Y))
           "由具有形式" (&c* $U $V)
           "的基本开集生成, 其中"
           (∈ $U (open $X)) "而"
           (∈ $V (open $Y))
           ", 因而每个"
           (∈ $W (open (&c* $X $Y)))
           "都是一个这样的基本开集之并."
           (Ul (Li "显然, " $p_1 "是连续的, 因为"
                   (&= (ap (_inv $p $1) $U)
                       (&c* $U $Y)) ".")
               (Li "对于任意的连续函数"
                   (Arrow $f_1 $Z $X) "和"
                   (Arrow $f_2 $Z $Y) ", 令"
                   (Arrow $f $Z (&c* $X $Y))
                   "是函数" (&= $f (tupa0 $f_1 $f_2))
                   ". 我们正需要看出" $f "何以是连续的.")
               (Li "对于任意的"
                   (∈ (&= $W (Union $i (@c* $U_i $V_i)))
                      (open (&c* $X $Y)))
                   ", "
                   (&= (app (inv $f) $W)
                       (Union $i (app (inv $f) (&c* $U_i $V_i))))
                   ", 故证明" (app (inv $f) (&c* $U $V))
                   "是开集就足够了. 然而, 我们有"
                   (MB (deriv
                        (app (inv $f) (&c* $U $V))
                        (app (inv $f)
                             (&cap (@c* $U $Y)
                                   (@c* $X $V)))
                        (&cap (app (inv $f) (&c* $U $Y))
                              (app (inv $f) (&c* $X $V)))
                        (&cap (app (@compose (inv $f) (_inv $p $1)) $U)
                              (app (@compose (inv $f) (_inv $p $2)) $V))
                        (&cap (app (_inv $f $1) $U)
                              (app (_inv $f $2) $V))))
                   "鉴于" $f_1 "和" $f_2 "都是连续函数, "
                   (app (_inv $f $1) $U) "和"
                   (app (_inv $f $2) $V) "都是开集. "
                   "{译注: 其实更严谨地说, 还需要论证" $f
                   "的唯一性, 只不过这是显然的.}"
                   (Br)
                   "以下的图表精确地捕获了我们手头上的情况:"
                   todo.svg)))
       (let ((poly (&+ $x^2 (&i* $2 $y))))
         (Li "(对于那些熟悉类型论的人.) "
             "让我们来考虑(简单类型)" $lambda
             "演算的" (Em "类型的范畴")
             ". " $lambda "演算是对于函数的描述和操作的一种形式化, "
             "其基于" (Q "变量绑定") "和函数 (functional) 求值的概念. "
             "例如, 对于实多项式函数的表达式" poly ", 在"
             $lambda "演算中对于函数" (&\|-> $y poly)
             " (对于每个固定的值" $x ") 我们写成"
             (Lam $y poly) ", 而对于函数值函数"
             (&\|-> $x (@\|-> $y poly)) "我们写成"
             (Lamb $x (Lam $y poly)) "." (Br)
             "形式地, " $lambda "演算由以下资料构成."
             (Ul (Li "类型: "
                     (&c* $A $B) ", "
                     (&-> $A $B) ", ... (由一些基本类型生成)")
                 (Li "项:"
                     (MB (set-attr*
                          (&Table
                           ((&: (&cm $x $y $z $..h) $A)
                            (: "(对于每个类型" $A "的变量)"))
                           ((&cm (&: $a $A) (&: $b $B) $..h)
                            "(可能有一些具类型的常量)")
                           ((&: (tupa0 $a $b) (&c* $A $B))
                            (@cm (&: $a $A) (&: $b $B)))
                           ((&: (&fst $c) $A) (@: $c (&c* $A $B)))
                           ((&: (&snd $c) $B) (@: $c (&c* $A $B)))
                           ((&: (ap $c $a) $B)
                            (@cm (&: $c (&-> $A $B))
                                 (&: $a $A)))
                           ((&: (Lam $x $b) (&-> $A $B))
                            (@cm (&: $x $A) (&: $b $B))))
                          'columnalign "right left")))
                 (Li "等式:"
                     (eqn*
                      ((&fst (tupa0 $a $b)) $= $a)
                      ((&snd (tupa0 $a $b)) $= $b)
                      ((tupa0 (&fst $c) (&snd $c)) $= $c)
                      ((ap (@Lam $x $b) $a)
                       $= (subst $b $a $x))
                      ((Lam $x (ap $c $x))
                       $= (: $c (&space 6) "(" $x "不在" $c "之中)")))))
             "项上的关系" (&~ $a $b)
             " (其通常被称为" (Em $beta $eta "-等价")
             ") 被定义为由等式和绑定变量重命名生成的等价关系, "
             "其中绑定变量重命名指的是"
             (MB (&= (Lam $x $b)
                     (Lam $y (subst $b $y $x)))
                 (&space 6)
                 "(" $y "不在" $b "之中)")
             "类型的范畴" (app CatC $lambda) "现在定义如下:"
             (Ul (Li "对象: 即类型,")
                 (Li "箭头" (&-> $A $B)
                     ": 封闭项" (&: $c (&-> $A $B))
                     ", 被视为等同若" (&~ $c $c^) ",")
                 (Li "恒元: " (&= $1_A (Lam $x $x))
                     " (其中" (&: $x $A) "),")
                 (Li "复合: "
                     (&= (&compose $c $b)
                         (Lam $x (ap $c (@ap $b $x)))) "."))
             "让我们验证一下这的确是一个良定义的范畴:" (Br)
             "恒元律:"
             (MB (&= (&compose $c $1_B)
                     (Lamb $x (@ap $c (@ap (@Lam $y $y) $x)))
                     (Lamb $x (@ap $c $x))
                     $c))
             (MB (&= (&compose $1_C $c)
                     (Lamb $x (@ap (@Lam $y $y) (@ap $c $x)))
                     (Lamb $x (@ap $c $x))
                     $c))
             "结合律:"
             (MB (deriv
                  (&compose $c (@compose $b $a))
                  (Lamb $x (@ap $c (@ap (@compose $b $a) $x)))
                  (Lamb $x (@ap $c (@ap (@Lam $y (ap $b (@ap $a $y))) $x)))
                  (Lamb $x (@ap $c (@ap $b (@ap $a $x))))
                  (Lamb $x (@ap (Lamb $y (@ap $c (@ap $b $y)))
                                (@ap $a $x)))
                  (Lamb $x (@ap (@compose $c $b) (@ap $a $x)))
                  (&compose (@compose $c $b) $a)))
             "这个范畴具有二元积. 的确如此, 对于类型" $A "和" $B ", 令"
             (MB (&cm (&= $p_1 (Lam $z (&fst $z)))
                      (&= $p_2 (Lam $z (&snd $z))))
                 (&space 6)
                 (@: $z (&c* $A $B)) ".")
             "然后, 给出" $a "和" $b "如图"
             todo.svg
             "令"
             (MB (&= (tu0 $a $b)
                     (Lam $x (tupa0 (ap $a $x) (ap $b $x)))))
             "那么"
             (MB (deriv
                  (&compose $p_1 (tu0 $a $b))
                  (Lam $x (@ap $p_1 (@ap (@Lam $y
                                               (tupa0 (ap $a $y) (ap $b $y)))
                                         $x)))
                  (Lam $x (@ap $p_1 (tupa0 (ap $a $x) (ap $b $x))))
                  (Lam $x (@ap $a $x))
                  $a))
             "类似地, " (&= (&compose $p_2 (tu0 $a $b)) $b) "." (Br)
             "最后, 如果" (Arrow $c $X (&c* $A $B)) "也具有性质"
             (MB (&cm (&= (&compose $p_1 $c) $a)
                      (&= (&compose $p_2 $c) $b)))
             "那么"
             (MB (deriv
                  (tu0 $a $b)
                  (λpair $a $b)
                  (λpair (@compose $p_1 $c)
                         (@compose $p_2 $c))
                  (λpair (@λcomp $p_1 $c)
                         (@λcomp $p_2 $c))
                  (λpair (@λcomp (@λfst) $c)
                         (@λcomp (@λsnd) $c))
                  (λpair (Lamb $y (@fst (ap $c $y)))
                         (Lamb $y (@snd (ap $c $y))))
                  (Lam $x (tupa0 (&fst (ap $c $x))
                                 (&snd (ap $c $x))))
                  (Lam $x (@ap $c $x))
                  $c))
             "{译注: 这个条目里所说的" (Q "不在")
             ", 其实应该理解为不在" (Q "自由变量集")
             "之中.}")))
   ((Remark)
    $lambda "演算有着另一个令人惊讶的解释, "
    "即作为命题演算中的证明的一个记号系统; "
    "其被称为" (Q "Curry-Howard对应")
    ". 简而言之, 想法在于我们将类型解释为命题 "
    "(其中" (&c* $A $B) "是合取而" (&-> $A $B)
    "是implication) 而将项" (&: $a $A)
    "解释为命题" $A "的证明. 那么, 诸如"
    (MB (&rule (&: $a $A) (&: $b $B)
               (&: (tupa0 $a $b) (&c* $A $B))))
    "这样的项形成规则可以读作带注解的推理规则, "
    "展示了如何归纳地构筑证明的标签. "
    "{译注: 所谓标签, 指的是这里冒号之前的内容, "
    "其可以视为证明的具体内容.} "
    "因此, 举个例子, 诸如"
    (MB (&rule*
         (&rule (bra0 $A) (bra0 $B)
                (&c* $A $B))
         (&-> $B (@c* $A $B))
         (&-> $A (@-> $B (@c* $A $B)))))
    "这样的一个证明, 其中的方括号指明了假设的取消, "
    "标签化如下:"
    (MB (&rule*
         (&rule (bra0 (&: $x $A))
                (bra0 (&: $y $B))
                (&: (tupa0 $x $y) (&c* $A $B)))
         (&: (Lam $y (tupa0 $x $y))
             (&-> $B (@c* $A $B)))
         (&: (Lamb $x (Lam $y (tupa0 $x $y)))
             (&-> $A (@-> $B (@c* $A $B))))))
    "最终的" (Q "证明项")
    (Lamb $x (Lam $y (tupa0 $x $y)))
    "因而记录了对于" (Q "命题")
    (&-> $A (@-> $B (@c* $A $B)))
    "的一个特定证明, 而对于相同命题的不同证明"
    "将会给出一个不同的项." (Br)
    "尽管人们总是谈论由之而来的逻辑和类型论之间的" (Q "同构")
    ", 但是实际上我们这里所拥有的仅仅是一个从"
    "带有合取与implication的命题演算中的证明的范畴 "
    "(如在" (Ref "examples-of-categories")
    "的例子10里定义的那样) 到" $lambda
    "演算的类型的范畴的一个函子. "
    "这个函子一般并非同构, "
    "除非我们强加 (impose) 一些更进一步的证明之间的等式.")
   (H3. "带有积的范畴")
   (P "令" CatC "是一个范畴, 其对于每对对象都有一个积图表. "
      "设我们有对象和箭头"
      todo.svg
      "带有如图所示的积. 然后, 对于"
      (MB (&= (&c* $f $f^)
              (tupa0 (&compose $f $p_1)
                     (&compose $f^ $p_2))))
      "我们记"
      (MB (Arrow (&c* $f $f^)
                 (&c* $A $A^)
                 (&c* $B $B^)))
      "{译注: 译者对于这句话的表达感到相当困惑, "
      "不过或许只是一个定义而已.} "
      "那么, 以下图表中的两个方块都是交换的:"
      todo.svg
      "以这种方式, 如果我们对于每对对象选择一个积, "
      "那么我们就得到了一个函子"
      (MB (Functor $c* (&c* CatC CatC) CatC))
      "读者可以使用积的UMP来轻易验证这个事实. "
      "对于每对对象都有一个积的范畴被称为是"
      (Em "具有二元积") ".")
   (P "我们也可以定义三元积"
      (MB (&c* $A_1 $A_2 $A_3))
      "其UMP可以类比于二元的情形 (存在三个投影"
      (Arrow $p_i (&c* $A_1 $A_2 $A_3) $A_i)
      ", 对于任意的对象" $X "和三个箭头"
      (Arrow $x_i $X $A_i) ", 存在唯一的箭头"
      (Arrow $u $X (&c* $A_1 $A_2 $A_3))
      "使得" (&= (&i* $p_i $u) $x_i)
      "对于" (&= $i (&cm $1 $2 $3))
      "成立). 平凡地, 这样一种条件可以之于任意数目的因子进行刻画.")
   (P "然而, 显然如果一个范畴具有二元积, "
      "那么其也具有所有的有限积 (带有两个或更多的因子); "
      "例如, 我们可以置"
      (MB (&= (&c* $A $B $C) (&c* (@c* $A $B) $C)))
      "以满足三元积的UMP. 从另一方面来说, "
      "我们本也可以取" (&c* $A (@c* $B $C))
      ". 这表明二元积运算" (&c* $A $B)
      "在同构下是结合性的, 因为根据三元积的UMP我们必然有"
      (MB (&cong (&c* (@c* $A $B) $C)
                 (&c* $A (@c* $B $C))) "."))
   (P "我们也应该观察到一个终对象是一个" (Q "零元")
      "积, 也就是没有对象的积:"
      (Blockquote
       "(考虑到零元积是没有对象的) "
       "存在一个对象" $1 ", 不需要映射, 对于任意的对象"
       $X ", 也不需要映射, 存在唯一的箭头"
       (MB (Arrow $! $X $1))
       "不需要进一步的交换条件.")
      "类似地, 任意的对象" $A "都是" $A
      "自身一个的" (Em "幺元积") ".")
   (P "最后, 我们也可以定义由" (Em "任意")
      "集合" $I "索引的一族对象" (Family $C $i $I)
      "的积, 只需要类比于零元积, 幺元积, 二元积, "
      $n "元积的UMP给出" (Q $I "元积")
      "的UMP就好. 我们将对于这个UMP的精确表述留作练习.")
   ((Definition)
    "一个范畴" CatC "被称为是" (Em "具有所有的有限积")
    ", 如果其有一个终对象和所有的二元积 "
    "(由此任何有限基数的积均存在). 范畴" CatC
    (Em "具有所有的(小)积") ", 如果每个由" CatC
    "的对象构成的集合都有一个积. "
    "{译注: 实际上, 这里考虑的应该是由集合索引的对象族.}")
   (H3. "同态集")
   (P "本节我们假定所有的范畴都是局部小的.")
   (P "回忆一下, 在任意的范畴" CatC "中, 对于任意的对象"
      $A "和" $B ", 我们记"
      (MB (&= (Hom $A $B)
              (setI (∈ $f CatC)
                    (Arrow $f $A $B))))
      "并将这样一个由箭头构成的集合称为一个" (Em "同态集")
      ". 注意到" CatC "中的任意箭头" (Arrow $g $B $B^)
      "都能够导出一个函数"
      (MB (Arrow (Hom $A $g) (Hom $A $B) (Hom $A $B^)))
      (MB (&\|-> (@Arrow $f $A $B)
                 (@: (&compose $g $f)
                     (&-> $A $B $B^))))
      "因此, " (&= (app (Hom $A $g) $f) (&compose $g $f))
      "; 我们有时记" $g_* "而非" (Hom $A $g) ", 那么"
      (MB (&= (app $g_* $f) (&compose $g $f)) "."))
   (let ((F (lambda (X) (Hom $A X)))
         (id (lambda (X) (_ $1 X))))
     (P "让我们表明这确定了一个函子"
        (MB (Functor (F $dummy) CatC Sets))
        "其被称为" $A "的(协变)" (Em "可表函子")
        ". 我们需要证明"
        (MB (&= (F (id $X)) (id (F $X))))
        "以及"
        (MB (&= (F (&compose $g $f))
                (&compose (F $g) (F $f))) ".")
        "(任)取一个参数" (Arrow $x $A $X)
        ", 显然我们有"
        (MB (deriv
             (app (F (id $X)) $x)
             (&compose (id $X) $x)
             $x
             (app (id (F $X)) $x)))
        "以及"
        (MB (deriv
             (app (F (&compose $g $f)) $x)
             (&compose (@compose $g $f) $x)
             (&compose $g (@compose $f $x))
             (app (F $g) (app (F $f) $x))
             (app (@compose (F $g) (F $f)) $x)))))
   (P "之后我们将会远为细致地研究这样的可表函子. "
      "暂时我们只想看看如何使用同态集给出积的定义的另一种表述.")
   (P "对于任意的对象" $P ", 一对箭头" (Arrow $p_1 $P $A)
      "和" (Arrow $p_2 $P $B) "确定了集合"
      (MB (&c* (Hom $P $A) (Hom $P $B)))
      "的一个元素" (tu0 $p_1 $p_2)
      ". 现在对于任意的箭头"
      (MB (Arrow $x $X $P))
      "将其与" $p_1 "和" $p_2 "进行复合给出了一对箭头"
      (&= $x_1 (Arrow (&compose $p_1 $x) $X $A)) "和"
      (&= $x_2 (Arrow (&compose $p_2 $x) $X $B))
      ", 如以下图表所示:"
      todo.svg
      "以这种方式, 我们有了一个函数"
      (MB (&= $thetav_X
              (Arrow (tu0 (Hom $X $p_1) (Hom $X $p_2))
                     (Hom $X $P)
                     (&c* (Hom $X $A) (Hom $X $B)))))
      "其被定义为"
      (MBL "(2.1)" (&= (app $thetav_X $x) (tu0 $x_1 $x_2)))
      "这个函数" $thetav_X "可以用来按照如下方式"
      "精确地表达成为一个积的条件.")
   ((Proposition #:id "product-condition")
    (Em "具有形式"
        (MB (LeftRightDiagram $A $p_1 $P $p_2 $B))
        "的一个图表是" $A "和" $B
        "的一个积当且仅当对于每个对象" $X
        ",(2.1)中所给出的canonical函数" $thetav_X
        "是一个同构,即"
        (MB (&: $thetav_X
                (&cong (Hom $X $P)
                       (&c* (Hom $X $A) (Hom $X $B)))) ".")))
   ((proof)
    "检视积的UMP: 它恰是在说对于每个元素"
    (∈ (tu0 $x_1 $x_2) (&c* (Hom $X $A) (Hom $X $B)))
    ", 存在唯一的" (∈ $x (Hom $X $P)) "使得"
    (&= (app $thetav_X $x) (tu0 $x_1 $x_2))
    ", 即" $thetav_X "是双射的.")
   ((Definition)
    "令" CatC ", " CatD "是具有二元积的范畴. 一个函子"
    (Functor $F CatC CatD) "被称为是" (Em "保持二元积")
    ", 如果其将" CatC "中的每个积图表"
    (MB (LeftRightDiagram $A $p_1 (&c* $A $B) $p_2 $B))
    "送至" CatD "中的一个积图表"
    (MB (apply
         LeftRightDiagram
         (map* (curry app $F)
               $A $p_1 (&c* $A $B) $p_2 $B)))
    "根据这个定义我们可以知道" $F "保持(二元)积恰当"
    (MB (&cong (app $F (&c* $A $B))
               (&c* (app $F $A) (app $F $B))))
    (Q "canonically") ", 也就是说, 当且仅当"
    CatD "中的canonical" (Q "比较箭头")
    (MB (Arrow (tupa0 (app $F $p_1) (app $F $p_2))
               (app $F (&c* $A $B))
               (&c* (app $F $A) (app $F $B))))
    "是一个同构. {译注: 请读者回忆"
    (Ref "product-uniqueness")
    "和之后的讨论, 从那里我们得知"
    (MB (apply
         LeftRightDiagram
         (map* (curry app $F)
               $A $p_1 (&c* $A $B) $p_2 $B)))
    "是一个积图表等价于"
    (MB (Arrow (tupa0 (app $F $p_1) (app $F $p_2))
               (app $F (&c* $A $B))
               (&c* (app $F $A) (app $F $B))))
    "是一个同构.}")
   (P "例如, 遗忘函子" (Functor $U Mon Sets) "保持二元积.")
   ((Corollary)
    (Em "对于具有(二元)积的一个范畴" CatC "中的任意对象" $X
        ",(协变)可表函子"
        (MB (Functor (Hom CatC $X $dummy) CatC Sets))
        "保持(二元)积."))
   ((proof)
    "对于任意的" (∈ $A $B CatC) ", 之前的"
    (Ref "product-condition")
    "告诉我们存在着一个canonical同构:"
    (let ((F (lambda (A) (Hom CatC $X A))))
      (MB (&cong (F (&c* $A $B))
                 (&c* (F $A) (F $B))))))
   (H3. "练习")
   (Ol (Li "证明集合之间的一个函数是一个满态射当且仅当其是一个满射. "
           "总结一下, " Sets "中的同构恰是满单态射 (epi-mono). "
           "{译注: epi-mono意即既是满态射又是单态射.}")
       (Li "表明在一个偏序集范畴之中, 所有的箭头既是单态射又是满态射.")
       (Li "(逆是唯一的.) 如果一个箭头" (Arrow $f $A $B)
           "有着逆" (Arrow (&cm $g $g^) $B $A)
           " (即" (&= (&compose $g $f) $1_A) "且"
           (&= (&compose $f $g) $1_B) ", " $g^
           "也是类似), 那么" (&= $g $g^) ".")
       (Li ""
           )
       )
   (H2. "对偶性")
   (P "我们已经见过了几个展现" (Q "对偶性 (duality)")
      "的例子和陈述, 例如始对象和终对象, 满态射和单态射. "
      "现在我们希望更加系统地考虑这种对偶性. "
      "尽管我们对于对偶性的第一印象或许是平凡的, "
      "然而这的确是数学结构的范畴方法的一个深刻而强大的方面.")
   (H3. "对偶原理")
   (P "首先, 让我们再一次观察范畴的形式定义: "
      "存在两个种类的东西, 对象" (&cm $A $B $C $..h)
      "和箭头" (&cm $f $g $h $..h) "; 四种操作"
      (&dom $f) ", " (&cod $f) ", " $1_A ", " (&compose $g $f)
      "; 并且这些资料满足以下七条公理:"
      (MBL "(3.1)"
           (set-attr*
            (&Table
             ((&= (&dom $1_A) $A)
              $
              (&= (&cod $1_A) $A))
             ((&= (&compose $f (_ $1 (&dom $f))) $f)
              $
              (&= (&compose (_ $1 (&cod $f)) $f) $f))
             ((&= (&dom (&compose $g $f)) (&dom $f))
              $
              (&= (&cod (&compose $g $f)) (&cod $g)))
             ((&compose $h (@compose $g $f))
              $=
              (&compose (@compose $h $g) $f)))
            'columnalign "right center left"))
      "操作" (Q (&compose $g $f)) "只在"
      (MB (&= (&dom $g) (&cod $f)))
      "时才有定义, 所以说这个的适切形式应该作为每个包含"
      $compose "的等式的条件出现, 如"
      (MB (&=> (&= (&dom $g) (&cod $f))
               (&= (&dom (&compose $g $f))
                   (&dom $f))) "."))
   (P "现在对于范畴论的初等语言里的任意句子" Σ
      ", 我们可以通过以下替换构造"
      (Q "对偶陈述") Σ* ":"
      (eqn*
       ((&compose $f $g)
        "for"
        (&compose $g $f))
       ($cod "for" $dom)
       ($dom "for" $cod))
      "很容易看出来" Σ* "也会是良形式的句子. "
      "接下来, 设我们有了一个句子" Σ
      "蕴涵 (entail) 一个" Δ " (即" (&=> Σ Δ)
      ") 而没有使用范畴论公理中的任何一条, "
      "那么我们显然也有" (&=> Σ* Δ*)
      ", 因为被替换的项仅仅被视为未加定义的常量. "
      "但是, 现在我们可以观察到范畴论 (" $CT
      ") 的诸公理本身是" (Q "自对偶 (self-dual)")
      "的, 其意为我们有"
      (MB (&= $CT^* $CT) ".")
      "因此, 我们有如下" (Em "对偶原理") ".")
   ((Proposition)
    (B "(形式对偶性). ")
    (Em "对于范畴论语言之中的任意句子" Σ
        ",如果" Σ "由范畴论的公理推出,那么其对偶"
        Σ* "亦可由范畴论的公理推出:"
        (MB (&=> $CT Σ) "可以推出"
            (&=> $CT Σ*) ".")))
   (P "从更加概念化的角度来看, "
      "注意到如果一个陈述牵涉某个由对象和箭头构成的图表"
      todo.svg
      "那么其对偶陈述则牵涉由该图表反转箭头方向和复合顺序所得到的新图表"
      todo.svg
      "回忆一下一个范畴" CatC "的相反范畴" (&op CatC)
      ", 我们看到对于" CatC "中的一个陈述" Σ
      "的一个解释将自动给出对于" (&op CatC)
      "中的" Σ* "的一个解释.")
   (P "现在设一个陈述" Σ "对于所有的范畴" CatC
      "都成立, 那么其也在所有的范畴" (&op CatC)
      "中成立, 于是" Σ* "也在所有的范畴"
      (&op (@op CatC)) "中成立. 但是既然对于每个范畴"
      CatC ", 都有"
      (MBL "(3.2)"
           (&= (&op (@op CatC)) CatC))
      "我们发现" Σ* "也在所有的范畴" CatC
      "中成立. 因此, 我们就有了以下的对偶原理的概念形式.")
   ((Proposition)
    (B "(概念对偶性). ")
    (Em "对于任意的关于范畴的陈述" Σ
        ",如果" Σ "对于所有的范畴成立,那么对偶陈述"
        Σ* "也对于所有的范畴成立."))
   (P "似乎只有非常简单或者平凡的陈述, 例如"
      (Q "终对象在同构意义下唯一")
      "服从于这种对偶性, 但是实际上远非如此. "
      "我们将会看到, 范畴对偶性实际上是极其强大且影响深远的现象. "
      "就像射影几何里点和线的对偶一样, "
      "它有效地对于每个证明产生两个定理, 真是"
      (Q "物超所值 (bang for the buck)") ".")
   
   (H3. "余积")
   (H3. "等化子")
   (H3. "余等化子")
   (H3. "练习")
   (H2. "群和范畴")
   (H3. "范畴中的群")
   (H3. "群的范畴")
   (H3. "作为范畴的群")
   (H3. "有限表现范畴")
   (H3. "练习")
   (H2. "极限和余极限")
   (H3. "子对象")
   (H3. "拉回")
   (H3. "拉回的性质")
   (H3. "极限")
   (H3. "极限的保持")
   (H3. "余极限")
   (H3. "练习")
   (H2. "指数")
   (H3. "范畴中的指数")
   (H3. "笛卡尔闭范畴")
   (H3. "Heyting代数")
   (H3. "命题演算")
   (H3. "CCC的等式性定义")
   (H3. $lambda "演算")
   
   (H2. "自然性")
   (H2. "图表的范畴")
   (H2. "伴随")
   (H2. "单子和代数")
   ))