#lang racket
(provide (all-defined-out))
(require SMathML)
(define $... (Mo "&ctdot;" #:attr* '((lspace "1ex") (rspace "1ex"))))
(define (&rule:labelled label . x*)
  (: (apply &rule x*) label))
(define $rule:L⊥ (% "(L" $bottom ")"))
(define $rule:Ax (% "(Ax)"))
(define $rule:LW (% "(LW)"))
(define $rule:RW (% "(RW)"))
(define $rule:LX (% "(LX)"))
(define $rule:RX (% "(RX)"))
(define $rule:LC (% "(LC)"))
(define $rule:RC (% "(RC)"))
(define $rule:Lc (% "(L" $conj ")"))
(define $rule:Rc (% "(R" $conj ")"))
(define $rule:Ld (% "(L" $disj ")"))
(define $rule:Rd (% "(R" $disj ")"))
(define $rule:Li (% "(L" $-> ")"))
(define $rule:Ri (% "(R" $-> ")"))
(define $rule:Cut (% "(Cut)"))
(define $IPC (Mi "IPC"))
(define (&IPC . op*)
  (apply appl $IPC (map set-compact op*)))
(define $rule:->I (% "(" $-> "I)"))
(define $rule:->E (% "(" $-> "E)"))
(define $rule:bottomE (% "(" $bottom "E)"))
(define $rule:conjI (% "(" $conj "I)"))
(define $rule:conjE (% "(" $conj "E)"))
(define $rule:disjI (% "(" $disj "I)"))
(define $rule:disjE (% "(" $disj "E)"))
(define (ineg p)
  (&-> p $bottom))
(define $log (Mi "log"))
(define (&log x y)
  (: (_ $log x) y))
(define $->:compact (set-compact $->))
(define $disj:compact (set-compact $disj))
(define $conj:compact (set-compact $conj))
(define $-:compact (set-compact $-))
(define $IPC:->
  (app $IPC $->:compact))
(define (setEc . x*)
  (apply setE (map set-compact x*)))
(define $Del (Mo "&Del;"))
(define (&Del x t)
  (: $Del x t))
(define first-order-syntax
  (_ $Phi:normal $Sigma:normal))
(define (forall x t)
  (: $forall x t))
(define (exists x t)
  (: $exists x t))
(define $sqcap (Mo "&sqcap;"))
(define i.sqcap (Mi "&sqcap;"))
(define i.- (Mi "&minus;"))
(define i.<= (Mi "&le;"))
(define i.disj (Mi "&or;"))
(define i.conj (Mi "&and;"))
(define i.neg (Mi "&not;"))
(define i.<-> (Mi "&harr;"))
(define $<-> (Mo "&harr;"))
(define $NJ (Mi "NJ"))
(define (&NJ . op*)
  (apply appl $NJ (map set-compact op*)))
(define $NJ:-> (app $NJ $->:compact))
(define $NK (Mi "NK"))
(define (&NK . op*)
  (apply appl $NK (map set-compact op*)))
(define (&<-> a b)
  (Mrow a $<-> b))
(define $sqcup (Mo "&sqcup;"))
(define i.sqcup (Mi "&sqcup;"))
(define i.-> (Mi "&rarr;"))
(define (val x) (_ (&db0 x) $v))
(define (val0 x v) (_ (&db0 x) v))
(define $PV (Mi "PV"))
(define $c^^ (&Prime $c))
(define $. (Mi "."))
(define $phantom. (Mphantom $.))
(define (tlam x t M)
  (: $lambda x $: t $. M))
(define (&lam x . x*M)
  (let-values (((x* M*) (split-at-right x*M 1)))
    (let ((M (car M*)))
      (if (null? x*)
          (: $lambda x $phantom. M)
          (: $lambda (apply : x x*) $. M)))))
(define (&lam0 x M)
  (: $lambda x M))
(define (&lam1 x M)
  (: $lambda x $. M))
(define (subst P x Q)
  (: P (bra0 (&:= x Q))))
(define NF (Mi "NF"))
(define NF_beta (_ NF $beta))
(define $->>beta (_ $->> $beta))
(define i.+ (Mi "+"))
(define $->beta+ (_^ $-> $beta i.+))
(define $=beta (_ $= $beta))
(define (G!- #:env [env $Gamma:normal] . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&entailL (apply &cm env b*) (car x*))))
(define (!- . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&entailL (apply &cm b*) (car x*))))
(define (G!!- #:env [env $Gamma:normal] . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&Vdash (apply &cm env b*) (car x*))))
(define (!!- . x*)
  (let-values (((b* x*) (split-at-right x* 1)))
    (&Vdash (apply &cm b*) (car x*))))
(define (&label x . t*)
  (Table #:attr* '((class "label") (align "center"))
         (Tr (Td #:attr* '((align "center")) x))
         (Tr (keyword-apply
              Td '(#:attr*) '(((align "center")))
              t*))))
(define $sc (Mo "&sc;"))
(define i.sc (Mi "&sc;"))
(define $->beta (_ $-> $beta))
(define (&if P Q R)
  (: "if&nbsp;" P "&nbsp;then&nbsp;" Q "&nbsp;else&nbsp;" R))
(define $true (Mi "true"))
(define $false (Mi "false"))
(define lam:true (&lam $x $y $x))
(define lam:false (&lam $x $y $y))
(define $0-bold
  (Mn "0" #:attr* '((mathvariant "bold"))))
(define $1-bold
  (Mn "1" #:attr* '((mathvariant "bold"))))
(define $2-bold
  (Mn "2" #:attr* '((mathvariant "bold"))))
(define i.succ (Mi "succ"))
(define i.add (Mi "add"))
(define i.mult (Mi "mult"))
(define i.exp (Mi "exp"))
(define i.zero (Mi "zero"))
(define $FV (Mi "FV"))
(define (&FV M)
  (app $FV M))
(define lambda-> (_ $lambda i.->))
(define (@lam x . x*M)
  (@ (apply &lam x x*M)))
(define (@lam0 x M)
  (@ (&lam0 x M)))
(define (@lam1 x M)
  (@ (&lam1 x M)))
(define $=>beta (_ $=> $beta))
(define $=alpha (_ $= $alpha))
(define Lambda- (^ $Lambda:normal i.-))
(define i.ext (Mi "ext"))
(define $=ext (_ $= i.ext))
(define (eqc X ~)
  (_ (bra0 X) ~))
(define $=beta-eta
  (_ $= (: $beta $eta)))
(define $WN (Mi "WN"))
(define $WN_beta (_ $WN $beta))
(define $SN (Mi "SN"))
(define $SN_beta (_ $SN $beta))
(define $inf_beta (_ $inf $beta))
(define $->small
  (Mo "&rarr;" #:attr* '((scriptlevel "+1"))))
(define (&vec x)
  (^^ x $->small))
(define $l->beta
  (_ (^^ $-> $l) $beta))
(define $h->beta
  (_ (^^ $-> $h) $beta))
(define $i->beta
  (_ (^^ $-> $i) $beta))
(define $Phi->
  (_ $Phi:normal i.->))
(define $dom (Mi "dom"))
(define (&dom f)
  (app $dom f))
(define $rg (Mi "rg"))
(define (&rg f)
  (app $rg f))
(define (@-> . p*)
  (@ (apply &-> p*)))
(define Mx:=N
  (subst $M $x $N))
(define $G:env $Gamma:normal)
(define $<=G (_ $<= $G:env))
(define $G:env^
  (&prime $G:env))
(define $G:env_1
  (_ $G:env $1))
(define $G:env_2
  (_ $G:env $2))
(define (&G:env x)
  (app $G:env x))
(define $thetav
  (Mi "&thetav;"))
(define $~
  (Mo "&Tilde;"))
(define $~G
  (_ $~ $G:env))
(define $cup $union)
(define $!-_N (_ $entailL $N))
(define-infix*
  (&sqcap $sqcap)
  (&sqcup $sqcup)
  (&disj $disj)
  (&conj $conj)
  (&sc $sc)
  (&->beta $->beta)
  (&->>beta $->>beta)
  (&=beta $=beta)
  (&vert $lv)
  (&::= $::=)
  (&=alpha $=alpha)
  (&=> $=>)
  (&=>beta $=>beta)
  (&=ext $=ext)
  (&=beta-eta $=beta-eta)
  (&l->beta $l->beta)
  (&h->beta $h->beta)
  (&i->beta $i->beta)
  (&~ $~)
  (&~G $~G)
  (&cup $cup)
  (&<=G $<=G)
  (&!-_N $!-_N)
  
  )
(define GMt
  (G!- (&: $M $tau)))
(define (@: . x*)
  (@ (apply &: x*)))
(define (subst* M . xN*)
  (if (null? xN*)
      M
      (apply subst* (subst M (car xN*) (cadr xN*))
             (cddr xN*))))
(define Tstyle1
  (Ttable
   (lambda (d i j)
     (cond ((= j 0) (set-attr* d 'columnalign "right"))
           ((= j 2) (set-attr* d 'columnalign "left"))
           (else d)))))
(define (Cite0 #:attr* [attr* '()] . id*)
  `(cite ,attr* . ,(fenced (map Ref id*))))
(define (default-entry:bib-present %entry:bib attr* . xml*)
  (define index (%entry-index %entry:bib))
  (define numbering (format "[~s] " index))
  (define id (%entry-id %entry:bib))
  (keyword-apply
   Div '(#:attr*) (list (attr*-set attr* 'class "bibliography" 'id id))
   numbering xml*))
(define (default-entry:bib-cite %entry:bib)
  (define id (%entry-id %entry:bib))
  (define index (%entry-index %entry:bib))
  (define numbering (format "~s" index))
  (define href (string-append "#" id))
  `(a ((href ,href)) ,numbering))
(define (build-%entry:bib id)
  (build-%entry #:local? #f #:class "bibliography" #:id id
                #:present default-entry:bib-present
                #:cite default-entry:bib-cite))
(define (Bib #:id [id #f] #:attr* [attr* '()] . xml*)
  `(,(build-%entry:bib id) ,attr* . ,xml*))
(define (format-numbering section index)
  (define sec (cdr (reverse (cons index section))))
  (define numbering
    (apply string-append
           (add-between (map number->string sec) ".")))
  numbering)
(define (format-section section level)
  (define sec (cdr (reverse section)))
  (define numbering
    (apply string-append
           (add-between (map number->string sec) ".")))
  (cond ((= level 2) (format "第~a章" numbering))
        ((= level 3) (format "第~a节" numbering))
        ((= level 4) (format "第~a小节" numbering))
        (else (error 'format-section "invalid level ~s" level))))
(define (default-heading0-present %heading0 attr* . xml*)
  (define level (%heading-level %heading0))
  (define auto? (%heading-auto? %heading0))
  (define section (%heading-section %heading0))
  (define id (%heading-id %heading0))
  (if (<= 2 level 4)
      (let ((tag (string->symbol (format "h~s" level))))
        (if auto?
            `(,tag ,(attr*-set attr* 'id id) ,(format-section section level)
                   " " . ,xml*)
            `(,tag ,(attr*-set attr* 'id id) . ,xml*)))
      (error 'default-heading0-present "invalid level ~s" level)))
(define (default-heading0-cite %heading0)
  (define id (%heading-id %heading0))
  (define href (string-append "#" id))
  (define auto? (%heading-auto? %heading0))
  (define level (%heading-level %heading0))
  (define section (%heading-section %heading0))
  (if auto?
      `(a ((href ,href)) ,(format-section section level))
      (if section
          `(a ((href ,href)) ,section)
          `(a ((href ,href)) ,id))))
(define (build-%heading0 level auto? id section)
  (build-%heading #:level level #:auto? auto? #:id id #:section section
                  #:present default-heading0-present
                  #:cite default-heading0-cite))
(define (Heading0 #:level [level 2] #:auto? [auto? #t] #:id [id #f] #:section [section #f]
                  #:attr* [attr* '()] . xml*)
  `(,(build-%heading0 level auto? id section) ,attr* . ,xml*))
(define (Cite1 #:attr* [attr* '()] id)
  `(cite ,attr* ,(Ref id)))
(define (build-%entry:_ id auto? index present cite)
  (build-%entry #:id id #:auto? auto? #:index index #:present present #:cite cite))
(define (make-entry:_ class name)
  (define (_-present %entry:_ attr* . xml*)
    (define auto? (%entry-auto? %entry:_))
    (define id (%entry-id %entry:_))
    (define Attr*
      (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry:_))
    (define index (%entry-index %entry:_))
    (if auto?
        (let* ((n (format-numbering section index))
               (head (format "~a." n)))
          `(div ,Attr* ,(B head " " name ".") " " . ,xml*))
        `(div ,Attr* (b () ,name ".") " " . ,xml*)))
  (define (_-cite %entry:_)
    (define auto? (%entry-auto? %entry:_))
    (define id (%entry-id %entry:_))
    (define href (string-append "#" id))
    (define section (%entry-section %entry:_))
    (define index (%entry-index %entry:_))
    (if auto?
        (if (pair? section) ;section may be a list, a string, or #f.
            `(a ((href ,href)) ,name ,(format-numbering section index))
            (if (string? section)
                `(a ((href ,href))
                    ,section ,(format "的~a~s" name index))
                `(a ((href ,href)) ,(format "某~a~s" name index))))
        (if index
            `(a ((href ,href)) ,index)
            `(a ((href ,href)) ,id))))
  (lambda (#:id [id #f] #:auto? [auto? #t] #:index [index #f])
    (lambda (#:attr* [attr* '()] . xml*)
      (cons (build-%entry:_ id auto? index _-present _-cite)
            (cons attr* xml*)))))
(define-syntax-rule (define-entry:_* (id class name) ...)
  (begin (define id (make-entry:_ class name))
         ...))
(define-entry:_*
  (Definition "definition" "定义")
  (Remark "remark" "评注")
  (Theorem "theorem" "定理")
  (Example "example" "例子")
  (Convention "convention" "约定")
  (Lemma "lemma" "引理")
  (Notation "notation" "记号")
  (Proposition "proposition" "命题")
  
  )
(define-@lized-op*
  (@disj &disj)
  (@conj &conj)
  (@compose &compose)
  (@Del &Del)
  (@neg &neg)
  (@ineg ineg)
  (@in &in)
  (@<-> &<->)
  
  )
(define (insert-before-last x lst)
  (let r ((a (car lst)) (d (cdr lst)))
    (if (null? (cdr d))
        (cons a (cons x d))
        (cons a (r (car d) (cdr d))))))
(define (&..cm . arg*)
  (apply &cm (insert-before-last $..h arg*)))
(define marker0
  (Marker
   #:attr*
   '((id "arrow")
     (viewbox "0 0 10 10")
     (refX "5")
     (refY "5")
     (markerWidth "6")
     (markerHeight "6")
     (orient "auto-start-reverse"))
   (Path #:attr* '((d "M 0 2 L 6 5 L 0 8 z")))))
(define (:arrc start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (marker-start "url(#tail)")
          (stroke-width "1.2px"))))
(define (:arrowc start end #:prop [prop 0.8] #:offset [vec vec:zero])
  (define t (/ (- 1 prop) 2))
  (define p1 ((lerp t) (pt+ start vec) (pt+ end vec)))
  (define p2 ((lerp (- 1 t)) (pt+ start vec) (pt+ end vec)))
  (:arrc p1 p2))
(define (n2s n)
  (format "~s" (exact-round n)))
(define (:dot cx cy #:r [r 2])
  (Circle
   #:attr*
   `((cx ,(n2s cx)) (cy ,(n2s cy)) (r ,(n2s r)))))
(define (::dot pt #:r [r 2])
  (:dot (pt-x pt) (pt-y pt) #:r r))
(define (make-pt x y) (vector 'pt x y))
(define (pt-x pt) (vector-ref pt 1))
(define (pt-y pt) (vector-ref pt 2))
(define (make-vec x y) (vector 'vec x y))
(define (vec-x vec) (vector-ref vec 1))
(define (vec-y vec) (vector-ref vec 2))
(define (pt+ p v)
  (make-pt (+ (pt-x p) (vec-x v))
           (+ (pt-y p) (vec-y v))))
(define (vec* k v)
  (make-vec (* k (vec-x v)) (* k (vec-y v))))
(define (vec+ u v)
  (make-vec (+ (vec-x u) (vec-x v))
            (+ (vec-y u) (vec-y v))))
(define vec:zero (make-vec 0 0))
(define vec:down (make-vec 0 1))
(define vec:up (make-vec 0 -1))
(define vec:left (make-vec -1 0))
(define vec:right (make-vec 1 0))
(define offset:down (make-vec -7 -4))
(define offset:up (make-vec -7 -13))
(define offset:left (make-vec -14 -9))
(define offset:right (make-vec -2 -9))
(define (:FO pos #:offset [offset 'down] #:scale [scale 10] #:width [w "100"] #:height [h "30"] . x*)
  (define offset-vec
    (case offset
      ((down) (vec+ (vec* scale vec:down) offset:down))
      ((up) (vec+ (vec* scale vec:up) offset:up))
      ((left) (vec+ (vec* scale vec:left) offset:left))
      ((right) (vec+ (vec* scale vec:right) offset:right))
      ((none) (make-vec 0 0))))
  (define position (pt+ pos offset-vec))
  (keyword-apply
   ForeignObject
   '(#:attr*)
   `(((x ,(n2s (pt-x position)))
      (y ,(n2s (pt-y position)))
      (width ,w)
      (height ,h)))
   x*))
(define (pt- p1 p2)
  (make-vec
   (- (pt-x p1) (pt-x p2))
   (- (pt-y p1) (pt-y p2))))
(define ((lerp t) p1 p2)
  (pt+ p1 (vec* t (pt- p2 p1))))
(define (:line start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (stroke-width "1.2px"))))
(define (:arr start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (stroke-width "1.2px"))))
(define (:darr start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1) (x2 ,x2) (y2 ,y2)
          (marker-end "url(#arrow)")
          (marker-start "url(#arrow)")
          (stroke-width "1.2px"))))
(define (:arrow start end #:prop [prop 0.8] #:offset [vec vec:zero])
  (define t (/ (- 1 prop) 2))
  (define p1 ((lerp t) (pt+ start vec) (pt+ end vec)))
  (define p2 ((lerp (- 1 t)) (pt+ start vec) (pt+ end vec)))
  (:arr p1 p2))
(define (set-dotted line)
  (set-attr* line 'stroke-dasharray "2 2"))
