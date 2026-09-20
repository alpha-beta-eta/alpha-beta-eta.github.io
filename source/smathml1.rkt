#lang racket
(provide smathml1.html)
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
(define $!- (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define (Lam x t)
  (: $lambda x $. t))
(define App (&split 2))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define (&rull label . x*)
  (: (apply &rule x*) label))
(define (type-of var env)
  (cond ((assoc var env) => cdr)
        (else (error 'type-of "unknown variable ~s" var))))
(define (extend-env var type env)
  (cons (cons var type) env))
(define (reify t)
  (define (reify t)
    (match t
      ((-> ,t1 ,t2) (&impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (&conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (&disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (define (@reify t)
    (match t
      ((-> ,t1 ,t2) (@impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (@conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (@disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (reify t))
(struct result (t c) #:transparent)
(define (VAR u)
  (lambda (env)
    (define t (type-of u env))
    (result t (assume u (&true (reify t))))))
(define (CONS a b)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match-define (result tb cb) (b env))
    (define type `(conj ,ta ,tb))
    (result type
            (&rull $conjI ca cb
                   (&true (reify type))))))
(define (CAR a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((conj ,t1 ,t2)
       (result t1 (&rull $conjE1 ca
                         (&true (reify t1))))))))
(define (CDR a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((conj ,t1 ,t2)
       (result t2 (&rull $conjE2 ca
                         (&true (reify t2))))))))
(define (LAM u t body)
  (lambda (env)
    (match-define (result tbody cbody)
      (body (extend-env u t env)))
    (define type `(-> ,t ,tbody))
    (result type
            (&rull (&implI u) cbody
                   (&true (reify type))))))
(define (APP a b)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match-define (result tb cb) (b env))
    (match ta
      ((-> ,t1 ,t2)
       (unless (equal? t1 tb)
         (error 'APP "type mismatch"))
       (result t2 (&rull $implE ca cb
                         (&true (reify t2))))))))
(define (INL tb a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (define type `(disj ,ta ,tb))
    (result type (&rull $disjI1 ca
                        (&true (reify type))))))
(define (INR ta b)
  (lambda (env)
    (match-define (result tb cb) (b env))
    (define type `(disj ,ta ,tb))
    (result type (&rull $disjI2 cb
                        (&true (reify type))))))
(define (CASE a u1 b1 u2 b2)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((disj ,t1 ,t2)
       (match-define (result tb1 cb1)
         (b1 (extend-env u1 t1 env)))
       (match-define (result tb2 cb2)
         (b2 (extend-env u2 t2 env)))
       (unless (equal? tb1 tb2)
         (error 'CASE "branch type mismatch"))
       (result tb1 (&rull (&disjE u1 u2) ca cb1 cb2
                          (&true (reify tb1))))))))
(define (ND proof)
  (result-c (proof '())))
(define &split2 (&split 2))
(define $true
  (Mi #:attr* '((mathvariant "italic"))
      "true"))
(define (&true A)
  (&split2 A $true))
(define (assume u t)
  (&rull u t))
(define $impl $sup)
(define-infix*
  (&impl $impl))
(define-@lized-op*
  (@impl &impl))
(define $disj:id (Mi "&or;"))
(define $conj:id (Mi "&and;"))
(define $impl:id (Mi "&sup;"))
(define $disjI1 (: $disj:id $I_1))
(define $disjI2 (: $disj:id $I_2))
(define (&disjE u w)
  (: $disj:id (^ $E (&cm u w))))
(define $conjI (: $conj:id $I))
(define $conjE1 (: $conj:id $E_1))
(define $conjE2 (: $conj:id $E_2))
(define (&implI u)
  (: $impl:id (^ $I u)))
(define $implE
  (: $impl:id $E))
(define smathml1.html
  (TnTmPrelude
   #:title "SMathML Reference"
   #:css "styles.css"
   (H1. "SMathML Reference")
   (P "This is the English translation of the "
      (A "Chinese version" #:attr* '((href "smathml0.html")))
      " of this reference.")
   (P "SMathML is still a fast-moving project, "
      "so this reference may be out of date. "
      "Where the two disagree, the implementation is authoritative.")
   (H2. "Introduction")
   (P "SMathML is not a typesetting engine; "
      "at present it delegates all typesetting work to the browser. "
      "In principle, typesetting could be delegated not only to the browser "
      "but also to other backends such as LaTeX, "
      "through a transformation or output process "
      "(in which case output is itself a kind of transformation). "
      "Its model, however, is based on the browser. "
      "That is, what it provides are means of expressing HTML and MathML. "
      "(It also takes CSS and SVG into account, "
      "but that support is far from complete and hardly usable.) "
      "Even if someone did design and implement another backend, "
      "it would be indirect, and it was never the author's intention.")
   (P "The purpose of SMathML is to provide a mature way "
      "of writing mathematical content on the web. "
      "Even so, the popularity of other software and libraries such as MathJax "
      "might make SMathML seem unnecessary. "
      "The problem is that almost none of these popular choices "
      "provides the necessary means of abstraction and combination. "
      "This problem has not received much attention, "
      "because LaTeX, the de facto standard for mathematical typesetting, "
      "is already hard to praise when it comes to abstraction and combination. "
      "(Moreover, most software and libraries offer only a " (Em "restricted")
      " imitation of LaTeX, which weakens abstraction and combination even further.) "
      "Nevertheless, the problem is real, "
      "and that is why the author designed and implemented SMathML.")
   (P "SMathML is currently a DSL embedded in Racket. "
      "In other words, users can enjoy the whole Racket ecosystem without restriction, "
      "in sharp contrast with some other typesetting systems, such as Typst. "
      "Embedding it in other Scheme implementations may be possible, "
      "but since SMathML freely uses Racket's own keyword argument mechanism, "
      "a compatibility layer may not be easy to implement. "
      "Users could, however, choose to start afresh "
      "and change some of SMathML's own means of expression.")
   (P "SMathML is based on SXML, which can be understood as "
      "a direct list representation of XML in Scheme/Lisp. "
      "SXML could perhaps also be understood as a syntax, "
      "but that view is both unnecessary and too restrictive, "
      "so we do not adopt it. "
      "The reason is that SMathML's expressive power comes from "
      "its ability to manipulate and transform SXML directly. "
      "Our SXML is not exactly the same as Oleg Kiselyov's SXML, "
      "whereas the SXML library provided by Racket follows Oleg Kiselyov's design. "
      "The difference is that in our SXML the attribute list is mandatory: "
      "when there are no attributes, an empty list is used, "
      "which makes the representation more uniform. "
      "This does make writing bare SXML more tedious. "
      "However, since many basic functions are provided, "
      "this is no problem at all in actual use of SMathML. "
      "SMathML also provides a transformation mechanism similar to "
      "Oleg Kiselyov's SXSLT, but it offers only a small part of SXSLT's functionality. "
      "For now, at least, the author considers that sufficient.")
   (H2. "Basic Functions")
   (P "This chapter is a reference for the most basic functions, "
      "which express MathML and HTML in SXML form. "
      "There is nothing surprising here; "
      "a quick skim is enough to master it completely.")
   (H3. "The MathML Part")
   (P (CodeB "(define (Math #:attr* [attr* '()] . mathml*)
  `(math ,attr* . ,mathml*))")
      "Let us look at this procedure first; "
      "all the remaining procedures in this chapter follow exactly the same pattern. "
      "It takes one keyword argument, the attributes, "
      "which defaults to empty. Our naming convention is that "
      "the name of the SMathML procedure corresponding to a MathML or HTML element "
      "is the element name with its first letter capitalized. "
      "That is all there is to say.")
   (P "Here are all the basic functions of the MathML part."
      (CodeB "(define (Math #:attr* [attr* '()] . mathml*)
  `(math ,attr* . ,mathml*))
(define (Merror #:attr* [attr* '()] . mathml*)
  `(merror ,attr* . ,mathml*))
(define (Mfrac #:attr* [attr* '()] . mathml*)
  `(mfrac ,attr* . ,mathml*))
(define (Mi #:attr* [attr* '()] . mathml*)
  `(mi ,attr* . ,mathml*))
(define (Mmultiscripts #:attr* [attr* '()] . mathml*)
  `(mmultiscripts ,attr* . ,mathml*))
(define (Mprescripts #:attr* [attr* '()] . mathml*)
  `(mprescripts ,attr* . ,mathml*))
(define (Mn #:attr* [attr* '()] . mathml*)
  `(mn ,attr* . ,mathml*))
(define (Mo #:attr* [attr* '()] . mathml*)
  `(mo ,attr* . ,mathml*))
(define (Mover #:attr* [attr* '()] . mathml*)
  `(mover ,attr* . ,mathml*))
(define (Mpadded #:attr* [attr* '()] . mathml*)
  `(mpadded ,attr* . ,mathml*))
(define (Mphantom #:attr* [attr* '()] . mathml*)
  `(mphantom ,attr* . ,mathml*))
(define (Mroot #:attr* [attr* '()] . mathml*)
  `(mroot ,attr* . ,mathml*))
(define (Mrow #:attr* [attr* '()] . mathml*)
  `(mrow ,attr* . ,mathml*))
(define (Ms #:attr* [attr* '()] . mathml*)
  `(ms ,attr* . ,mathml*))
(define (Mspace #:attr* [attr* '()] . mathml*)
  `(mspace ,attr* . ,mathml*))
(define (Msqrt #:attr* [attr* '()] . mathml*)
  `(msqrt ,attr* . ,mathml*))
(define (Mstyle #:attr* [attr* '()] . mathml*)
  `(mstyle ,attr* . ,mathml*))
(define (Msub #:attr* [attr* '()] . mathml*)
  `(msub ,attr* . ,mathml*))
(define (Msubsup #:attr* [attr* '()] . mathml*)
  `(msubsup ,attr* . ,mathml*))
(define (Msup #:attr* [attr* '()] . mathml*)
  `(msup ,attr* . ,mathml*))
(define (Mtable #:attr* [attr* '()] . mathml*)
  `(mtable ,attr* . ,mathml*))
(define (Mtd #:attr* [attr* '()] . mathml*)
  `(mtd ,attr* . ,mathml*))
(define (Mtext #:attr* [attr* '()] . mathml*)
  `(mtext ,attr* . ,mathml*))
(define (Mtr #:attr* [attr* '()] . mathml*)
  `(mtr ,attr* . ,mathml*))
(define (Munder #:attr* [attr* '()] . mathml*)
  `(munder ,attr* . ,mathml*))
(define (Munderover #:attr* [attr* '()] . mathml*)
  `(munderover ,attr* . ,mathml*))
(define (Maction #:attr* [attr* '()] . mathml*)
  `(maction ,attr* . ,mathml*))
(define (Menclose #:attr* [attr* '()] . mathml*)
  `(menclose ,attr* . ,mathml*))
(define (Mfenced #:attr* [attr* '()] . mathml*)
  `(mfenced ,attr* . ,mathml*))")
      "In fact they were not entirely written by hand, but generated. "
      "From a software engineering point of view, "
      "I ought to have abstracted them with a macro, "
      "so the current implementation is not very satisfying. "
      "Still, the current definitions are at least clear and easy to modify.")
   (H4. "Some Useful Supplementary Definitions")
   (P "There are also some useful supplementary definitions "
      "that deserve a place in this chapter."
      (CodeB "(define (MathB #:attr* [attr* '()] . mathml*)
  `(math ((display &quot;block&quot;) . ,attr*) . ,mathml*))
(define M Math)
(define MB MathB)
(define (MBL label . exp*)
  (MB (Mtable #:attr*
              '((columnalign &quot;left center right&quot;)
                (displaystyle &quot;true&quot;)
                (width &quot;100%&quot;))
              (Mtr (Mtd (Mphantom label))
                   (apply Mtd exp*)
                   (Mtd label)))))")
      "Here " (Code "MathB") " sets the " (Code "display")
      " attribute to " (Code "block")
      ", while " (Code "M") " and " (Code "MB")
      " are abbreviations of " (Code "Math") " and " (Code "MathB")
      ", respectively. Note that the " (Code "Math")
      " element has two rendering modes, "
      (Code "inline") " and " (Code "block")
      ", the default being " (Code "inline")
      "; anyone who has used LaTeX should find this easy to understand. As for "
      (Code "MBL") ", it builds on " (Code "MB")
      " by adding a label to the formula.")
   (P (CodeB "(define $ (Mrow))
(define ^ Msup)
(define _ Msub)
(define _^ Msubsup)
(define __ Munder)
(define ^^ Mover)
(define __^^ Munderover)
(define ~ Mfrac)
(define % Mtext)")
      "Except for " (Code "$")
      ", these are all short aliases for basic elements. "
      "As for " (Code "$")
      ", its only job is to be a placeholder, nothing more.")
   (H3. "The HTML Part")
   (P "The HTML part is just like the MathML part."
      (CodeB "(define (A #:attr* [attr* '()] . html*)
  `(a ,attr* . ,html*))
(define (Abbr #:attr* [attr* '()] . html*)
  `(abbr ,attr* . ,html*))
(define (Address #:attr* [attr* '()] . html*)
  `(address ,attr* . ,html*))
(define (Area #:attr* [attr* '()] . html*)
  `(area ,attr* . ,html*))
(define (Article #:attr* [attr* '()] . html*)
  `(article ,attr* . ,html*))
(define (Aside #:attr* [attr* '()] . html*)
  `(aside ,attr* . ,html*))
(define (Audio #:attr* [attr* '()] . html*)
  `(audio ,attr* . ,html*))
(define (B #:attr* [attr* '()] . html*)
  `(b ,attr* . ,html*))
(define (Base #:attr* [attr* '()] . html*)
  `(base ,attr* . ,html*))
(define (Bdi #:attr* [attr* '()] . html*)
  `(bdi ,attr* . ,html*))
(define (Bdo #:attr* [attr* '()] . html*)
  `(bdo ,attr* . ,html*))
(define (Blockquote #:attr* [attr* '()] . html*)
  `(blockquote ,attr* . ,html*))
(define (Body #:attr* [attr* '()] . html*)
  `(body ,attr* . ,html*))
(define (Br #:attr* [attr* '()] . html*)
  `(br ,attr* . ,html*))
(define (Button #:attr* [attr* '()] . html*)
  `(button ,attr* . ,html*))
(define (Canvas #:attr* [attr* '()] . html*)
  `(canvas ,attr* . ,html*))
(define (Caption #:attr* [attr* '()] . html*)
  `(caption ,attr* . ,html*))
(define (Cite #:attr* [attr* '()] . html*)
  `(cite ,attr* . ,html*))
(define (Code #:attr* [attr* '()] . html*)
  `(code ,attr* . ,html*))
(define (Col #:attr* [attr* '()] . html*)
  `(col ,attr* . ,html*))
(define (Colgroup #:attr* [attr* '()] . html*)
  `(colgroup ,attr* . ,html*))
(define (Data #:attr* [attr* '()] . html*)
  `(data ,attr* . ,html*))
(define (Datalist #:attr* [attr* '()] . html*)
  `(datalist ,attr* . ,html*))
(define (Dd #:attr* [attr* '()] . html*)
  `(dd ,attr* . ,html*))
(define (Del #:attr* [attr* '()] . html*)
  `(del ,attr* . ,html*))
(define (Details #:attr* [attr* '()] . html*)
  `(details ,attr* . ,html*))
(define (Dfn #:attr* [attr* '()] . html*)
  `(dfn ,attr* . ,html*))
(define (Dialog #:attr* [attr* '()] . html*)
  `(dialog ,attr* . ,html*))
(define (Div #:attr* [attr* '()] . html*)
  `(div ,attr* . ,html*))
(define (Dl #:attr* [attr* '()] . html*)
  `(dl ,attr* . ,html*))
(define (Dt #:attr* [attr* '()] . html*)
  `(dt ,attr* . ,html*))
(define (Em #:attr* [attr* '()] . html*)
  `(em ,attr* . ,html*))
(define (Embed #:attr* [attr* '()] . html*)
  `(embed ,attr* . ,html*))
(define (Fieldset #:attr* [attr* '()] . html*)
  `(fieldset ,attr* . ,html*))
(define (Figcaption #:attr* [attr* '()] . html*)
  `(figcaption ,attr* . ,html*))
(define (Figure #:attr* [attr* '()] . html*)
  `(figure ,attr* . ,html*))
(define (Footer #:attr* [attr* '()] . html*)
  `(footer ,attr* . ,html*))
(define (Form #:attr* [attr* '()] . html*)
  `(form ,attr* . ,html*))
(define (H1 #:attr* [attr* '()] . html*)
  `(h1 ,attr* . ,html*))
(define (H2 #:attr* [attr* '()] . html*)
  `(h2 ,attr* . ,html*))
(define (H3 #:attr* [attr* '()] . html*)
  `(h3 ,attr* . ,html*))
(define (H4 #:attr* [attr* '()] . html*)
  `(h4 ,attr* . ,html*))
(define (H5 #:attr* [attr* '()] . html*)
  `(h5 ,attr* . ,html*))
(define (H6 #:attr* [attr* '()] . html*)
  `(h6 ,attr* . ,html*))
(define (Head #:attr* [attr* '()] . html*)
  `(head ,attr* . ,html*))
(define (Header #:attr* [attr* '()] . html*)
  `(header ,attr* . ,html*))
(define (Hgroup #:attr* [attr* '()] . html*)
  `(hgroup ,attr* . ,html*))
(define (Hr #:attr* [attr* '()] . html*)
  `(hr ,attr* . ,html*))
(define (Html #:attr* [attr* '()] . html*)
  `(html ,attr* . ,html*))
(define (I #:attr* [attr* '()] . html*)
  `(i ,attr* . ,html*))
(define (Iframe #:attr* [attr* '()] . html*)
  `(iframe ,attr* . ,html*))
(define (Img #:attr* [attr* '()] . html*)
  `(img ,attr* . ,html*))
(define (Input #:attr* [attr* '()] . html*)
  `(input ,attr* . ,html*))
(define (Ins #:attr* [attr* '()] . html*)
  `(ins ,attr* . ,html*))
(define (Kbd #:attr* [attr* '()] . html*)
  `(kbd ,attr* . ,html*))
(define (Label #:attr* [attr* '()] . html*)
  `(label ,attr* . ,html*))
(define (Legend #:attr* [attr* '()] . html*)
  `(legend ,attr* . ,html*))
(define (Li #:attr* [attr* '()] . html*)
  `(li ,attr* . ,html*))
(define (Link #:attr* [attr* '()] . html*)
  `(link ,attr* . ,html*))
(define (Main #:attr* [attr* '()] . html*)
  `(main ,attr* . ,html*))
(define (Map #:attr* [attr* '()] . html*)
  `(map ,attr* . ,html*))
(define (Mark #:attr* [attr* '()] . html*)
  `(mark ,attr* . ,html*))
(define (Menu #:attr* [attr* '()] . html*)
  `(menu ,attr* . ,html*))
(define (Meta #:attr* [attr* '()] . html*)
  `(meta ,attr* . ,html*))
(define (Meter #:attr* [attr* '()] . html*)
  `(meter ,attr* . ,html*))
(define (Nav #:attr* [attr* '()] . html*)
  `(nav ,attr* . ,html*))
(define (Noscript #:attr* [attr* '()] . html*)
  `(noscript ,attr* . ,html*))
(define (Object #:attr* [attr* '()] . html*)
  `(object ,attr* . ,html*))
(define (Ol #:attr* [attr* '()] . html*)
  `(ol ,attr* . ,html*))
(define (Optgroup #:attr* [attr* '()] . html*)
  `(optgroup ,attr* . ,html*))
(define (Option #:attr* [attr* '()] . html*)
  `(option ,attr* . ,html*))
(define (Output #:attr* [attr* '()] . html*)
  `(output ,attr* . ,html*))
(define (P #:attr* [attr* '()] . html*)
  `(p ,attr* . ,html*))
(define (Picture #:attr* [attr* '()] . html*)
  `(picture ,attr* . ,html*))
(define (Pre #:attr* [attr* '()] . html*)
  `(pre ,attr* . ,html*))
(define (Progress #:attr* [attr* '()] . html*)
  `(progress ,attr* . ,html*))
(define (Q #:attr* [attr* '()] . html*)
  `(q ,attr* . ,html*))
(define (Rp #:attr* [attr* '()] . html*)
  `(rp ,attr* . ,html*))
(define (Rt #:attr* [attr* '()] . html*)
  `(rt ,attr* . ,html*))
(define (Ruby #:attr* [attr* '()] . html*)
  `(ruby ,attr* . ,html*))
(define (S #:attr* [attr* '()] . html*)
  `(s ,attr* . ,html*))
(define (Samp #:attr* [attr* '()] . html*)
  `(samp ,attr* . ,html*))
(define (Script #:attr* [attr* '()] . html*)
  `(script ,attr* . ,html*))
(define (Search #:attr* [attr* '()] . html*)
  `(search ,attr* . ,html*))
(define (Section #:attr* [attr* '()] . html*)
  `(section ,attr* . ,html*))
(define (Select #:attr* [attr* '()] . html*)
  `(select ,attr* . ,html*))
(define (Slot #:attr* [attr* '()] . html*)
  `(slot ,attr* . ,html*))
(define (Small #:attr* [attr* '()] . html*)
  `(small ,attr* . ,html*))
(define (Source #:attr* [attr* '()] . html*)
  `(source ,attr* . ,html*))
(define (Span #:attr* [attr* '()] . html*)
  `(span ,attr* . ,html*))
(define (Strong #:attr* [attr* '()] . html*)
  `(strong ,attr* . ,html*))
(define (Style #:attr* [attr* '()] . html*)
  `(style ,attr* . ,html*))
(define (Sub #:attr* [attr* '()] . html*)
  `(sub ,attr* . ,html*))
(define (Summary #:attr* [attr* '()] . html*)
  `(summary ,attr* . ,html*))
(define (Sup #:attr* [attr* '()] . html*)
  `(sup ,attr* . ,html*))
(define (Table #:attr* [attr* '()] . html*)
  `(table ,attr* . ,html*))
(define (Tbody #:attr* [attr* '()] . html*)
  `(tbody ,attr* . ,html*))
(define (Td #:attr* [attr* '()] . html*)
  `(td ,attr* . ,html*))
(define (Template #:attr* [attr* '()] . html*)
  `(template ,attr* . ,html*))
(define (Textarea #:attr* [attr* '()] . html*)
  `(textarea ,attr* . ,html*))
(define (Tfoot #:attr* [attr* '()] . html*)
  `(tfoot ,attr* . ,html*))
(define (Th #:attr* [attr* '()] . html*)
  `(th ,attr* . ,html*))
(define (Thead #:attr* [attr* '()] . html*)
  `(thead ,attr* . ,html*))
(define (Time #:attr* [attr* '()] . html*)
  `(time ,attr* . ,html*))
(define (Title #:attr* [attr* '()] . html*)
  `(title ,attr* . ,html*))
(define (Tr #:attr* [attr* '()] . html*)
  `(tr ,attr* . ,html*))
(define (Track #:attr* [attr* '()] . html*)
  `(track ,attr* . ,html*))
(define (U #:attr* [attr* '()] . html*)
  `(u ,attr* . ,html*))
(define (Ul #:attr* [attr* '()] . html*)
  `(ul ,attr* . ,html*))
(define (Var #:attr* [attr* '()] . html*)
  `(var ,attr* . ,html*))
(define (Video #:attr* [attr* '()] . html*)
  `(video ,attr* . ,html*))
(define (Wbr #:attr* [attr* '()] . html*)
  `(wbr ,attr* . ,html*))"))
   (H4. "Some Useful Supplementary Definitions")
   (P "The HTML part likewise has some useful supplementary definitions."
      (CodeB "(define (Prelude #:title [title &quot;index&quot;] #:css [css '()] . body*)
  (Html
   (apply
    Head
    (Meta #:attr* '((charset &quot;utf-8&quot;)))
    (Title title)
    (map (lambda (stylesheet)
           (Link #:attr* `((href ,stylesheet) (rel &quot;stylesheet&quot;))))
         (if (string? css) (list css) css)))
   (apply Body body*)))")
      (Code "Prelude") " lets users construct a web page more easily; "
      "it is a simple abstraction over the basic case.")
   (P (CodeB "(define (CodeB . str*)
  (Pre (apply Code str*)))")
      (Code "CodeB") " is used for code blocks. "
      "Custom attributes might be worthwhile for it as well, "
      "but I have never needed them.")
   (H2. "Extended Basic Definitions")
   (P "This chapter provides many definitions that, "
      "although not the most basic, are still quite important. "
      "That is why we call them "
      (Q "extended basic definitions")
      ". Without them, "
      "many simple cases would become rather complicated.")
   (H3. "The MathML Part")
   (P "First we define the mathematical identifiers for the basic letters."
      (CodeB "(define $a (Mi &quot;a&quot;))
(define $b (Mi &quot;b&quot;))
(define $c (Mi &quot;c&quot;))
(define $d (Mi &quot;d&quot;))
(define $e (Mi &quot;e&quot;))
(define $f (Mi &quot;f&quot;))
(define $g (Mi &quot;g&quot;))
(define $h (Mi &quot;h&quot;))
(define $i (Mi &quot;i&quot;))
(define $j (Mi &quot;j&quot;))
(define $k (Mi &quot;k&quot;))
(define $l (Mi &quot;l&quot;))
(define $m (Mi &quot;m&quot;))
(define $n (Mi &quot;n&quot;))
(define $o (Mi &quot;o&quot;))
(define $p (Mi &quot;p&quot;))
(define $q (Mi &quot;q&quot;))
(define $r (Mi &quot;r&quot;))
(define $s (Mi &quot;s&quot;))
(define $t (Mi &quot;t&quot;))
(define $u (Mi &quot;u&quot;))
(define $v (Mi &quot;v&quot;))
(define $w (Mi &quot;w&quot;))
(define $x (Mi &quot;x&quot;))
(define $y (Mi &quot;y&quot;))
(define $z (Mi &quot;z&quot;))
(define $A (Mi &quot;A&quot;))
(define $B (Mi &quot;B&quot;))
(define $C (Mi &quot;C&quot;))
(define $D (Mi &quot;D&quot;))
(define $E (Mi &quot;E&quot;))
(define $F (Mi &quot;F&quot;))
(define $G (Mi &quot;G&quot;))
(define $H (Mi &quot;H&quot;))
(define $I (Mi &quot;I&quot;))
(define $J (Mi &quot;J&quot;))
(define $K (Mi &quot;K&quot;))
(define $L (Mi &quot;L&quot;))
(define $M (Mi &quot;M&quot;))
(define $N (Mi &quot;N&quot;))
(define $O (Mi &quot;O&quot;))
(define $P (Mi &quot;P&quot;))
(define $Q (Mi &quot;Q&quot;))
(define $R (Mi &quot;R&quot;))
(define $S (Mi &quot;S&quot;))
(define $T (Mi &quot;T&quot;))
(define $U (Mi &quot;U&quot;))
(define $V (Mi &quot;V&quot;))
(define $W (Mi &quot;W&quot;))
(define $X (Mi &quot;X&quot;))
(define $Y (Mi &quot;Y&quot;))
(define $Z (Mi &quot;Z&quot;))
(define $aa (Mi &quot;&amp;aopf;&quot;))
(define $bb (Mi &quot;&amp;bopf;&quot;))
(define $cc (Mi &quot;&amp;copf;&quot;))
(define $dd (Mi &quot;&amp;dopf;&quot;))
(define $ee (Mi &quot;&amp;eopf;&quot;))
(define $ff (Mi &quot;&amp;fopf;&quot;))
(define $gg (Mi &quot;&amp;gopf;&quot;))
(define $hh (Mi &quot;&amp;hopf;&quot;))
(define $ii (Mi &quot;&amp;iopf;&quot;))
(define $jj (Mi &quot;&amp;jopf;&quot;))
(define $kk (Mi &quot;&amp;kopf;&quot;))
(define $ll (Mi &quot;&amp;lopf;&quot;))
(define $mm (Mi &quot;&amp;mopf;&quot;))
(define $nn (Mi &quot;&amp;nopf;&quot;))
(define $oo (Mi &quot;&amp;oopf;&quot;))
(define $pp (Mi &quot;&amp;popf;&quot;))
(define $qq (Mi &quot;&amp;qopf;&quot;))
(define $rr (Mi &quot;&amp;ropf;&quot;))
(define $ss (Mi &quot;&amp;sopf;&quot;))
(define $tt (Mi &quot;&amp;topf;&quot;))
(define $uu (Mi &quot;&amp;uopf;&quot;))
(define $vv (Mi &quot;&amp;vopf;&quot;))
(define $ww (Mi &quot;&amp;wopf;&quot;))
(define $xx (Mi &quot;&amp;xopf;&quot;))
(define $yy (Mi &quot;&amp;yopf;&quot;))
(define $zz (Mi &quot;&amp;zopf;&quot;))
(define $AA (Mi &quot;&amp;Aopf;&quot;))
(define $BB (Mi &quot;&amp;Bopf;&quot;))
(define $CC (Mi &quot;&amp;Copf;&quot;))
(define $DD (Mi &quot;&amp;Dopf;&quot;))
(define $EE (Mi &quot;&amp;Eopf;&quot;))
(define $FF (Mi &quot;&amp;Fopf;&quot;))
(define $GG (Mi &quot;&amp;Gopf;&quot;))
(define $HH (Mi &quot;&amp;Hopf;&quot;))
(define $II (Mi &quot;&amp;Iopf;&quot;))
(define $JJ (Mi &quot;&amp;Jopf;&quot;))
(define $KK (Mi &quot;&amp;Kopf;&quot;))
(define $LL (Mi &quot;&amp;Lopf;&quot;))
(define $MM (Mi &quot;&amp;Mopf;&quot;))
(define $NN (Mi &quot;&amp;Nopf;&quot;))
(define $OO (Mi &quot;&amp;Oopf;&quot;))
(define $PP (Mi &quot;&amp;Popf;&quot;))
(define $QQ (Mi &quot;&amp;Qopf;&quot;))
(define $RR (Mi &quot;&amp;Ropf;&quot;))
(define $SS (Mi &quot;&amp;Sopf;&quot;))
(define $TT (Mi &quot;&amp;Topf;&quot;))
(define $UU (Mi &quot;&amp;Uopf;&quot;))
(define $VV (Mi &quot;&amp;Vopf;&quot;))
(define $WW (Mi &quot;&amp;Wopf;&quot;))
(define $XX (Mi &quot;&amp;Xopf;&quot;))
(define $YY (Mi &quot;&amp;Yopf;&quot;))
(define $ZZ (Mi &quot;&amp;Zopf;&quot;))")
      "The identifiers whose letter is doubled are the "
      (Q "blackboard bold")
      " versions; for example, " (&cm $QQ $RR $CC)
      " are the usual notations for the sets of rational, real and complex numbers.")
   (P "For Greek letters we also have a complete set of definitions."
      (CodeB "(define $alpha (Mi &quot;&amp;alpha;&quot;))
(define $beta (Mi &quot;&amp;beta;&quot;))
(define $gamma (Mi &quot;&amp;gamma;&quot;))
(define $delta (Mi &quot;&amp;delta;&quot;))
(define $epsilon (Mi &quot;&amp;epsilon;&quot;))
(define $zeta (Mi &quot;&amp;zeta;&quot;))
(define $eta (Mi &quot;&amp;eta;&quot;))
(define $theta (Mi &quot;&amp;theta;&quot;))
(define $iota (Mi &quot;&amp;iota;&quot;))
(define $kappa (Mi &quot;&amp;kappa;&quot;))
(define $lambda (Mi &quot;&amp;lambda;&quot;))
(define $mu (Mi &quot;&amp;mu;&quot;))
(define $nu (Mi &quot;&amp;nu;&quot;))
(define $xi (Mi &quot;&amp;xi;&quot;))
(define $omicron (Mi &quot;&amp;omicron;&quot;))
(define $pi (Mi &quot;&amp;pi;&quot;))
(define $rho (Mi &quot;&amp;rho;&quot;))
(define $sigma (Mi &quot;&amp;sigma;&quot;))
(define $tau (Mi &quot;&amp;tau;&quot;))
(define $upsilon (Mi &quot;&amp;upsilon;&quot;))
(define $phi (Mi &quot;&amp;phi;&quot;))
(define $chi (Mi &quot;&amp;chi;&quot;))
(define $psi (Mi &quot;&amp;psi;&quot;))
(define $omega (Mi &quot;&amp;omega;&quot;))
(define $Alpha (Mi &quot;&amp;Alpha;&quot;))
(define $Beta (Mi &quot;&amp;Beta;&quot;))
(define $Gamma (Mi &quot;&amp;Gamma;&quot;))
(define $Delta (Mi &quot;&amp;Delta;&quot;))
(define $Epsilon (Mi &quot;&amp;Epsilon;&quot;))
(define $Zeta (Mi &quot;&amp;Zeta;&quot;))
(define $Eta (Mi &quot;&amp;Eta;&quot;))
(define $Theta (Mi &quot;&amp;Theta;&quot;))
(define $Iota (Mi &quot;&amp;Iota;&quot;))
(define $Kappa (Mi &quot;&amp;Kappa;&quot;))
(define $Lambda (Mi &quot;&amp;Lambda;&quot;))
(define $Mu (Mi &quot;&amp;Mu;&quot;))
(define $Nu (Mi &quot;&amp;Nu;&quot;))
(define $Xi (Mi &quot;&amp;Xi;&quot;))
(define $Omicron (Mi &quot;&amp;Omicron;&quot;))
(define $Pi (Mi &quot;&amp;Pi;&quot;))
(define $Rho (Mi &quot;&amp;Rho;&quot;))
(define $Sigma (Mi &quot;&amp;Sigma;&quot;))
(define $Tau (Mi &quot;&amp;Tau;&quot;))
(define $Upsilon (Mi &quot;&amp;Upsilon;&quot;))
(define $Phi (Mi &quot;&amp;Phi;&quot;))
(define $Chi (Mi &quot;&amp;Chi;&quot;))
(define $Psi (Mi &quot;&amp;Psi;&quot;))
(define $Omega (Mi &quot;&amp;Omega;&quot;))")
      "They are used quite frequently in mathematics.")
   (P "For numbers, we only define " $0 " through " $9 "."
      (CodeB "(define $0 (Mn &quot;0&quot;))
(define $1 (Mn &quot;1&quot;))
(define $2 (Mn &quot;2&quot;))
(define $3 (Mn &quot;3&quot;))
(define $4 (Mn &quot;4&quot;))
(define $5 (Mn &quot;5&quot;))
(define $6 (Mn &quot;6&quot;))
(define $7 (Mn &quot;7&quot;))
(define $8 (Mn &quot;8&quot;))
(define $9 (Mn &quot;9&quot;))"))
   (H3. "The HTML Part")
   (P "The HTML part also has some extended basic definitions, "
      "though not many. "
      "Moreover, many of them perhaps do not belong here.")
   (P (CodeB "(define (make-table #:attr* [attr* '()] lst)
  (keyword-apply
   Table '(#:attr*) (list attr*)
   (map (lambda (row)
          (apply Tr (map Td row)))
        lst)))")
      (Code "make-table")
      " is a convenient procedure for defining tables. "
      "It adds " (Code "Tr")
      " and " (Code "Td")
      " automatically, while still letting you customize the table's attributes.")
   (P (CodeB "(define (map2 proc lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (proc (car lst))))
        (else (cons (proc (car lst) (cadr lst))
                    (map2 proc (cddr lst))))))")
      (Code "map2") " is like " (Code "map")
      ", except that the procedure argument is applied to "
      "two consecutive elements of the list at a time. "
      "This definition is quite handy and comes up in many situations.")
   (P (CodeB "(define (columnize . xml*)
  (make-table
   #:attr* '((width &quot;100%&quot;))
   (map2 list xml*)))")
      "This definition is not really that basic, "
      "but I use it often on my blog "
      "to lay out elements neatly in two columns.")
   (P (CodeB "(define -- &quot;&amp;mdash;&amp;mdash;&quot;)")
      "This constant, too, is just something I use often on my blog, "
      "but I suspect readers may find frequent use for it as well.")
   (P (CodeB "(define (enum . x*)
  (apply Span (add-between x* &quot;, &quot;)))")
      (Code "enum") " separates its arguments with commas.")
   (H2. "Practical Definitions")
   (P "Starting with this chapter, we present a number of practical definitions. "
      "Some of them serve as basic tools, "
      "which users are free to use as well. "
      "Others are built on top of these tools.")
   (H3. "Operators and Infix Operators")
   (P "Operators are one of the basic ingredients of mathematical notation, "
      "so we provide a number of convenient tools "
      "along with definitions of many common operators.")
   (P (CodeB "(define ((make-op op n u) . x*)
  (cond
    ((null? x*) (n))
    ((null? (cdr x*)) (u (car x*)))
    (else
     (let iter ((x (car x*)) (x* (cdr x*)) (r '()))
       (if (null? x*)
           (apply Mrow (reverse (cons x r)))
           (iter (car x*)
                 (cdr x*)
                 (cons op (cons x r))))))))
(define (err0 id)
  (lambda ()
    (error id &quot;&quot;)))
(define (err1 id)
  (lambda (x)
    (error id &quot;~s&quot; x)))")
      (Code "make-op")
      " defines the procedure corresponding to an operator, "
      "which accepts any number of arguments. "
      "With two or more arguments, "
      "it behaves as an infix operator. "
      "The cases of zero and one argument "
      "are left for the user to define separately. "
      "If the user wants those cases to signal an error, "
      (Code "err0") " and " (Code "err1") " are available.")
   (P (CodeB "(define (make-infix op id)
  (make-op op
    (err0 id)
    (err1 id)))
(define-syntax define-infix*
  (syntax-rules ()
    ((_ (id op) ...)
     (begin
       (define id (make-infix op 'id))
       ...))))")
      "We also have the " (Code "make-infix")
      " procedure and the " (Code "define-infix*")
      " macro, which make it easy to define infix operator procedures "
      "that only accept two or more arguments.")
   (P "Let us look at some examples to see how these tools are used."
      (CodeB "(define &amp;cm
  (make-op $cm
    (lambda () $)
    (lambda (x) x)))")
      "The definition of " (Code "&amp;cm")
      " depends on " (Code "$cm")
      ", which is defined as"
      (CodeB "(define $cm (Mo &quot;,&quot;))")
      "That is, " (Code "&amp;cm")
      " is a procedure that separates its arguments with commas. "
      "Given a single argument, it returns that argument itself. "
      "Given no arguments, it returns the placeholder " (Code "$")
      ". Note that, as a general rule, we use the "
      (Code "$") " prefix for things that are not functions, "
      "and the " (Code "&amp;")
      " prefix for the procedure that expresses the corresponding notation.")
   (P "Here is another example."
      (CodeB "(define &amp;-
  (make-op $-
    (err0 '&amp;-)
    (lambda (x) (Mrow $- x))))")
      (Code "&amp;-")
      " is the procedure for subtraction; "
      "with a single argument it is negation. "
      "Using it, SMathML provides some negative number constants."
      (CodeB "(define $-1 (&amp;- $1))
(define $-2 (&amp;- $2))
(define $-3 (&amp;- $3))
(define $-4 (&amp;- $4))
(define $-5 (&amp;- $5))
(define $-6 (&amp;- $6))
(define $-7 (&amp;- $7))
(define $-8 (&amp;- $8))
(define $-9 (&amp;- $9))"))
   (H3. "Big Operators")
   (P "There is another special kind of operator that we use often, "
      "which might be called " (Q "big operators") ".")
   (P (CodeB "(define (make-bigop op)
  (case-lambda
    ((u o e) (Mrow (Munderover op u o) e))
    ((u e) (Mrow (Munder op u) e))
    ((e) (Mrow op e))))
(define-syntax define-bigop*
  (syntax-rules ()
    ((_ (id op) ...)
     (begin
       (define id (make-bigop op))
       ...))))")
      (Code "make-bigop")
      " defines the procedure corresponding to a big operator, and "
      (Code "define-bigop*")
      " is a macro for defining many big operator procedures at once.")
   (P "A big operator procedure always accepts one, two or three arguments. "
      "With one argument, the big operator has no limits above or below it. "
      "With two, it has only a lower limit. "
      "With three, it has both. "
      "This matches exactly how big operators are customarily used. "
      "Note, however, that the limits of a big operator are rendered differently "
      "in inline mode and in block mode, "
      "which is in fact much the same as in LaTeX. "
      "In inline mode the limits become sub- and superscripts, "
      "while in block mode they really do appear above and below the operator. "
      "To get the block-mode style in inline mode, "
      "set the " (Code "displaystyle") " attribute to "
      (Code "true") ".")
   (P (CodeB "(define-bigop*
  (sum $sum)
  (prod $prod)
  (Union $Union)
  (Cup $Union)
  (Intersect $Cap)
  (Cap $Cap)
  (Disj $Disj)
  (Conj $Conj))")
      "These are the big operator procedures SMathML currently provides. "
      "Perhaps " (Code "sum") " and " (Code "prod")
      " should have been capitalized, but by now the habit is too entrenched to change.")
   (H3. "Bracketing Procedures and List Enumeration Procedures")
   (P "Brackets and list-style enumerations are among the most common "
      "mathematical expressions, so it is worth providing all the usual variants.")
   (P "First, though, let us look at a few constant definitions."
      (CodeB "(define $lp (Mo &quot;(&quot;))
(define $rp (Mo &quot;)&quot;))
(define $lp0
  (Mo &quot;(&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rp0
  (Mo &quot;)&quot; #:attr* '((stretchy &quot;false&quot;))))")
      "First I must admit that names like " (Code "$lp")
      " and " (Code "$rp") " are neither intuitive "
      "nor easy to remember; " (Code "$\\(")
      " and " (Code "$\\)") " would be better, but by now the habit is too entrenched. "
      "There is, however, one more thing worth noticing: the " (Code "stretchy")
      " attribute. It is actually there to work around a problem in Chrome "
      "that does not exist in Firefox. "
      "Unless " (Code "stretchy") " is set to " (Code "false")
      ", ordinary parentheses are always stretched when Chrome renders them, "
      "which is not what users expect. That said, the " (Code "stretchy")
      " attribute itself controls whether an operator is stretched, "
      "which is rarely needed in mathematical expressions but quite useful when it is. "
      "So even though we mainly care about how things render in Firefox, "
      "it is still worth providing both kinds of bracketing procedures."
      (CodeB "(define (pare x)
  (Mrow $lp x $rp))
(define (par0 x)
  (Mrow $lp0 x $rp0))")
      "Of course, I am now not very happy with the names " (Code "pare")
      " and " (Code "par0") " either.")
   (P "With the bracketing procedures in place, "
      "we can define the accompanying list enumeration procedures."
      (CodeB "(define (tup . x*)
  (pare (apply &amp;cm x*)))
(define (tu0 . x*)
  (par0 (apply &amp;cm x*)))"))
   (P "The above uses only parentheses. For square brackets, curly braces, "
      "angle brackets, vertical bars and double vertical bars, "
      "we have similar definitions."
      (CodeB "(define $lb (Mo &quot;[&quot;))
(define $rb (Mo &quot;]&quot;))
(define $lb0
  (Mo &quot;[&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rb0
  (Mo &quot;]&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $lc (Mo &quot;{&quot;))
(define $rc (Mo &quot;}&quot;))
(define $lc0
  (Mo &quot;{&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rc0
  (Mo &quot;}&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $lv (Mo &quot;|&quot;))
(define $rv (Mo &quot;|&quot;))
(define $lv0
  (Mo &quot;|&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rv0
  (Mo &quot;|&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $la (Mo &quot;&amp;lang;&quot;))
(define $ra (Mo &quot;&amp;rang;&quot;))
(define $la0
  (Mo &quot;&amp;lang;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $ra0
  (Mo &quot;&amp;rang;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $ld (Mo &quot;&amp;Vert;&quot;))
(define $rd (Mo &quot;&amp;Vert;&quot;))
(define $ld0
  (Mo &quot;&amp;Vert;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rd0
  (Mo &quot;&amp;Vert;&quot; #:attr* '((stretchy &quot;false&quot;))))")
      (CodeB "(define (brac x)
  (Mrow $lb x $rb))
(define (bra0 x)
  (Mrow $lb0 x $rb0))
(define (curb x)
  (Mrow $lc x $rc))
(define (cur0 x)
  (Mrow $lc0 x $rc0))
(define (vert x)
  (Mrow $lv x $rv))
(define (ver0 x)
  (Mrow $lv0 x $rv0))
(define (angb x)
  (Mrow $la x $ra))
(define (ang0 x)
  (Mrow $la0 x $ra0))
(define (dver x)
  (Mrow $ld x $rd))
(define (dve0 x)
  (Mrow $ld0 x $rd0))")
      (CodeB "(define (lis . x*)
  (brac (apply &amp;cm x*)))
(define (li0 . x*)
  (bra0 (apply &amp;cm x*)))
(define (enu . x*)
  (curb (apply &amp;cm x*)))
(define (en0 . x*)
  (cur0 (apply &amp;cm x*)))
(define (tupan . x*)
  (angb (apply &amp;cm x*)))
(define (tupa0 . x*)
  (ang0 (apply &amp;cm x*)))")
      "Note that there are no list enumeration procedures for vertical bars "
      "and double vertical bars, but then I have never seen or used such a notation.")
   (P "Later, when writing about denotational semantics, "
      "I often needed double square brackets, "
      "which led to the following definitions."
      (CodeB "(define $ldb (Mo &quot;&amp;LeftDoubleBracket;&quot;))
(define $rdb (Mo &quot;&amp;RightDoubleBracket;&quot;))
(define (&amp;db x)
  (Mrow $ldb x $rdb))
(define $ldb0 (Mo &quot;&amp;LeftDoubleBracket;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define $rdb0 (Mo &quot;&amp;RightDoubleBracket;&quot; #:attr* '((stretchy &quot;false&quot;))))
(define (&amp;db0 x)
  (Mrow $ldb0 x $rdb0))"))
   (P "We also have some definitions for set-builder notation, "
      "and this section seems a fitting place for them."
      (CodeB "(define setE en0)
(define (setI a b)
  (cur0 (Mrow a $lv0 b)))
(define (setEnum a b)
  (setE a $..h b))")
      "Here " (Code "E") " stands for extension and "
      (Code "I") " for intension. Perhaps "
      (Code "setI") " should accept a variable number of arguments, "
      "but the author has not changed it yet.")
   (H3. "Parenthesization")
   (P "We often need to put parentheses around an expression, "
      "so SMathML provides a parenthesizing procedure "
      "that turns an ordinary notation procedure into a parenthesized version."
      (CodeB "(define (@ . x*)
  (par0 (apply : x*)))
(define (@lize &amp;) (compose @ &amp;))
(define-syntax define-@lized-op*
  (syntax-rules ()
    ((_ (id op) ...)
     (begin
       (define id (@lize op))
       ...))))")
      "The " (Code ":") " used here is similar to "
      (Code "Mrow") " and is defined as follows:"
      (CodeB "(define (: . x*)
  (cond
    ((null? x*) $)
    ((null? (cdr x*)) (car x*))
    (else (apply Mrow x*))))")
      (Code "@lize") " defines one such procedure, and "
      (Code "define-@lized-op*")
      " defines many of them at once.")
   (P (CodeB "(define-@lized-op*
  (@+ &amp;+)
  (@- &amp;-)
  (@* &amp;*)
  (@i* &amp;i*)
  (@c* &amp;c*)
  (@d* &amp;d*)
  (@t* &amp;t*)
  (@w* &amp;w*)
  (@compose &amp;compose)
  (@conj &amp;conj)
  (@disj &amp;disj)
  (@sqcup &amp;sqcup)
  (@sqcap &amp;sqcap)
  (@: &amp;:)
  (@neg &amp;neg)
  (@-> &amp;->)
  (@=> &amp;=>)
  (@= &amp;=)
  (@ap ap)
  (@cong &amp;cong)
  (@~ &amp;~)
  (@sqsube &amp;sqsube)
  (@rarr &amp;rarr)
  (@larr &amp;larr)
  (@uarr &amp;uarr)
  (@darr &amp;darr)
  (@rArr &amp;rArr)
  (@lArr &amp;lArr)
  (@uArr &amp;uArr)
  (@dArr &amp;dArr))")
      "For now these are the only parenthesized procedures we define; "
      "there are probably many more that should be. "
      "Readers should, however, take note of the naming convention: "
      "the " (Code "@")
      " prefix denotes the parenthesized version.")
   (H3. "Function Application")
   (P "Standard prefix function application is one of "
      "the most frequently used notations in mathematics, "
      "so we provide the following procedures."
      (CodeB "(define $af (Mo &quot;&amp;af;&quot;))
(define (ap f x)
  (Mrow f $af x))
(define (app f . x*)
  (ap f (apply @ x*)))
(define app*
  (case-lambda
    ((f x) (app f x))
    ((f g . arg*) (app f (apply app* g arg*)))))
(define (appl f . x*)
  (ap f (apply tu0 x*)))")
      "Inserting " (Code "$af") " is what the official documentation does, "
      "and I chose not to deviate from that standard. "
      (Code "ap") " is for plain application, "
      "which is uncommon in mathematics but does occur. "
      (Code "app") " is for application with the argument in parentheses, "
      "the most common form in mathematics. "
      (Code "app*") " is the iterated version of " (Code "app")
      ", which is occasionally very useful. "
      (Code "appl") " is for applying a function of several arguments, "
      "and uses " (Code "tu0") ".")
   (P "We also provide " (Code "@ap")
      ", the parenthesized version of " (Code "ap") ".")
   (H3. "Table Procedures")
   (P "MathML tables resemble HTML tables in some respects, "
      "but they retain many traditional table attributes. "
      "These attributes are already deprecated for HTML tables, "
      "and Chrome has no support at all for them on MathML tables. "
      "Firefox, at least, does support them. "
      "So far I have only considered how things render in Firefox; "
      "for compatibility, this should perhaps be changed to suit other browsers. "
      "Chrome recommends that MathML users control MathML tables with CSS.")
   (P "I think the details of this part are written terribly, "
      "to an appalling degree. "
      "Even so, I have lately had no energy to rework them. "
      "These procedures and macros are still worth using. "
      "Tables are very flexible: ordinary arrangements, case distinctions, "
      "matrices and determinants can all be expressed with them."
      (CodeB "(define (&amp;table l)
  (apply Mtable
         (map (lambda (r)
                (apply Mtr (map Mtd r)))
              l)))
(define-syntax &amp;Table
  (syntax-rules ()
    ((_ (x ...) ...)
     (&amp;table (list (list x ...) ...)))))
(define (choice l)
  (Mrow $lc (&amp;table l)))
(define-syntax Choice
  (syntax-rules ()
    ((_ (x ...) ...)
     (choice (list (list x ...) ...)))))
(define (choice0 l)
  (Mrow
   $lc
   (keyword-apply
    Mtable
    '(#:attr*) '(((displaystyle &quot;true&quot;)))
    (map (lambda (r)
           (Mtr (Mtd (car r))
                (keyword-apply
                 Mtd '(#:attr*) '(((columnalign &quot;left&quot;)))
                 (cdr r))))
         l))))
(define-syntax Choice0
  (syntax-rules ()
    ((_ (x ...) ...)
     (choice0 (list (list x ...) ...)))))
(define (mat l)
  (brac (&amp;table l)))
(define-syntax Mat
  (syntax-rules ()
    ((_ (x ...) ...)
     (mat (list (list x ...) ...)))))
(define (ma0 l)
  (pare (&amp;table l)))
(define-syntax Ma0
  (syntax-rules ()
    ((_ (x ...) ...)
     (ma0 (list (list x ...) ...)))))
(define (det l)
  (vert (&amp;table l)))
(define-syntax Det
  (syntax-rules ()
    ((_ (x ...) ...)
     (det (list (list x ...) ...)))))")
      "One current difficulty is that the attributes cannot be adjusted "
      "directly and flexibly, but readers can at least use the convenient procedure "
      (Code "set-attr*")
      " introduced later, "
      "so this is no obstacle to expressing things.")
   (H3. "Primes and Double Primes")
   (P "SMathML provides procedures for primes and double primes, "
      "which are often used to denote derivatives or simply different names."
      (CodeB "(define $prime (Mo &quot;&amp;prime;&quot;))
(define $Prime (Mo &quot;&amp;Prime;&quot;))
(define (&amp;prime x)
  (Msup x $prime))
(define (&amp;Prime x)
  (Msup x $Prime))
(define (_prime x i)
  (_^ x i $prime))
(define (_Prime x i)
  (_^ x i $Prime))")
      "Here " (Code "_prime") " and " (Code "_Prime")
      " are for the case of both a prime and a subscript, which comes up from time to time.")
   (H3. "Combinations of Constants")
   (P "Constants combined via " (Code "^") " and " (Code "_")
      " appear frequently in mathematics, "
      "so for users' convenience "
      "we have a large number of predefined combinations. "
      "These, too, were generated automatically.")
   (P "Primed versions of the constants, i.e. the results of applying "
      (Code "&amp;prime")
      ", are also used often. Our naming convention is to append " (Code "^")
      ". Double-primed versions should really be added too, "
      "but we left them out; their naming convention would be to append "
      (Code "^^") ".")
   (P "We provide all the " (Code "mathvariant")
      " variants of every letter. The naming convention is to append "
      (Code ":") " followed by the name of the variant, for example:"
      (MB (set-attr*
           (&Table
            ($P:normal (Ms "$P:normal"))
            ($P:bold (Ms "$P:bold"))
            ($P:italic (Ms "$P:italic"))
            ($P:bold-italic (Ms "$P:bold-italic"))
            ($P:script (Ms "$P:script"))
            ($P:bold-script (Ms "$P:bold-script"))
            ($P:fraktur (Ms "$P:fraktur"))
            ($P:bold-fraktur (Ms "$P:bold-fraktur"))
            ($P:sans-serif (Ms "$P:sans-serif"))
            ($P:bold-sans-serif (Ms "$P:bold-sans-serif"))
            ($P:sans-serif-italic (Ms "$P:sans-serif-italic"))
            ($P:sans-serif-bold-italic (Ms "$P:sans-serif-bold-italic"))
            ($P:double-struck (Ms "$P:double-struck"))
            ($P:monospace (Ms "$P:monospace")))
           'columnalign "center left"
           'frame "solid"
           'rowlines "solid"
           'columnlines "solid"))
      "Readers should note that Chrome's MathML Core does not support "
      (Code "mathvariant") " at all.")

   (H3. "Entries")
   (P "Mathematical literature is full of entries of various kinds: "
      "axioms, definitions, propositions, theorems, lemmas, corollaries and so on. "
      "It is therefore worth providing a tool for them, "
      "along with some predefined entry procedures."
      (CodeB "(define (entry-make class template)
  (lambda (#:n [n &quot;&quot;])
    (lambda x*
      (keyword-apply
       Div '(#:attr*) `(((class ,class)))
       (B (format template n) &quot; &quot;) x*))))
(define-syntax-rule
  (define-entry* (id class template) ...)
  (begin
    (define id (entry-make class template))
    ...))"))
   (P "For now we provide the following predefined entries."
      (CodeB "(define-entry*
  (definition &quot;definition&quot; &quot;定义~a.&quot;)
  (lemma &quot;lemma&quot; &quot;引理~a.&quot;)
  (theorem &quot;theorem&quot; &quot;定理~a.&quot;)
  (corollary &quot;corollary&quot; &quot;推论~a.&quot;)
  (example &quot;example&quot; &quot;例子~a.&quot;)
  (notation &quot;notation&quot; &quot;记号~a.&quot;)
  (comment &quot;comment&quot; &quot;注记~a.&quot;)
  (program &quot;program&quot; &quot;程序~a.&quot;)
  (exercise &quot;exercise&quot; &quot;练习~a.&quot;)
  (proposition &quot;proposition&quot; &quot;命题~a.&quot;)
  (remark &quot;remark&quot; &quot;评注~a.&quot;)
  (convention &quot;convention&quot; &quot;约定~a.&quot;)
  (tcomment &quot;tcomment&quot; &quot;译者注记~a.&quot;)
  (answer &quot;answer&quot; &quot;解答~a.&quot;))"))
   
   (H3. "Abstractions over Laws and Axioms")
   (P "Part of SMathML's flexibility lies in its ability "
      "to express many higher-order abstractions. "
      "One direct application is expressing laws and axioms, "
      "such as commutativity and associativity. "
      "The current definitions are not polished enough, "
      "and readers are generally advised to define their own as needed; "
      "what follows is only meant to get the ball rolling."
      (CodeB "(define (commute* a b)
  (&amp;= (&amp;i* a b) (&amp;i* b a)))
(define (commute+ a b)
  (&amp;= (&amp;+ a b) (&amp;+ b a)))
(define (commute &amp; a b)
  (&amp;= (&amp; a b) (&amp; b a)))")
      (CodeB "(define (assoc* a b c)
  (&amp;= (&amp;i* a (@ (&amp;i* b c)))
      (&amp;i* (@ (&amp;i* a b)) c)))
(define (assoc+ a b c)
  (&amp;= (&amp;+ a (@ (&amp;+ b c)))
      (&amp;+ (@ (&amp;+ a b)) c)))
(define (associate &amp; a b c)
  (&amp;= (&amp; (@ (&amp; a b)) c) (&amp; a (@ (&amp; b c)))))")
      (CodeB "(define (disL a b c)
  (&amp;= (&amp;i* (@ (&amp;+ a b)) c)
      (&amp;+ (&amp;i* a c)
          (&amp;i* b c))))
(define (disR a b c)
  (&amp;= (&amp;i* a (@ (&amp;+ b c)))
      (&amp;+ (&amp;i* a b)
          (&amp;i* a c))))
(define (distributeL a + b * c)
  (&amp;= (* (@ (+ a b)) c)
      (+ (@ (* a c)) (@ (* b c)))))
(define (distributeR a * b + c)
  (&amp;= (* a (@ (+ b c)))
      (+ (@ (* a b)) (@ (* a c)))))")
      "Readers may find that they sometimes need to swap "
      "the two sides of the equation, "
      "which is one of the oversights in these procedures.")
   (P "There is one more procedure that, "
      "although not yet part of SMathML, may be worth a look:"
      (CodeB "(define (!commute f g . x*)
  (&amp;= (f (apply g x*))
      (apply g (map f x*))))")
      "This, too, is a kind of " (Q "commuting")
      " in the usual mathematical sense, and " (Code "!commute")
      " is quite flexible, adapting to many different situations. "
      "Here is a simple example."
      (CodeB "(MB (!commute (curry app $F)
              &amp;compose
              $f $g))")
      (MB (!commute (curry app $F)
                    &compose
                    $f $g))
      (Em "Update: it has now been added."))
   (H3. "Derivations")
   (P "Derivations are actually implemented with tables as well, "
      "but the author preferred to give them a section of their own."
      (CodeB "(define (make-row . x*)
  (apply Mtr (map Mtd x*)))
(define (deriv x y . z*)
  (keyword-apply
   Mtable
   '(#:attr*) '(((displaystyle &quot;true&quot;) (columnalign &quot;right center left&quot;)))
   (cons (make-row x $= y)
         (map (lambda (z) (make-row $ $= z)) z*))))
(define (deriv0 x -> y . z*)
  (keyword-apply
   Mtable
   '(#:attr*) '(((displaystyle &quot;true&quot;) (columnalign &quot;right center left&quot;)))
   (cons (make-row x -> y)
         (map2 (lambda (-> w) (make-row $ -> w)) z*))))")
      (Code "deriv") " is for derivations whose steps are all joined by equals signs, while "
      (Code "deriv0") " lets the user choose the connectives. "
      "Originally both definitions were rather complicated, "
      "chiefly because " (Code "columnalign")
      " used to be set cell by cell; "
      "only later did the author realize that this was unnecessary.")
   (P "Building on " (Code "set-attr*")
      ", introduced later, SMathML also provides some derived procedures."
      (CodeB "(define-syntax-rule (eqn* (x ...) ...)
  (MB (set-attr*
       (&amp;Table (x ...) ...)
       'columnalign &quot;right center left&quot;
       'displaystyle &quot;true&quot;)))")
      (Code "eqn*") " aligns things the same way as " (Code "deriv")
      ", but allows rows other than the first to have something on the left as well."
      (CodeB "(define (deriv^ x . y*)
  (set-attr*
   (apply
    Mtable
    (Mtr (Mtd $) (Mtd x))
    (map (lambda (y)
           (Mtr (Mtd $=) (Mtd y)))
         y*))
   'displaystyle &quot;true&quot;
   'columnalign &quot;center left&quot;))
(define (deriv^0 x . y*)
  (set-attr*
   (apply
    Mtable
    (Mtr (Mtd $) (Mtd x))
    (map2 (lambda (= y)
            (Mtr (Mtd =) (Mtd y)))
          y*))
   'displaystyle &quot;true&quot;
   'columnalign &quot;center left&quot;))")
      (Code "deriv^") " and " (Code "deriv^0")
      " are similar to " (Code "deriv") " and "
      (Code "deriv0") " respectively; the difference is that "
      "what used to be on the left of the first row is folded over to the right. "
      "For extremely long formulas, "
      "these two procedures are indispensable.")
   (H3. "Spacing")
   (P "We often need to list some mathematical expressions with a certain spacing, "
      "so SMathML provides procedures for that too."
      (CodeB "(define (&amp;space n)
  (Mspace #:attr* `((width ,(format &quot;~sex&quot; n)))))
(define (&amp;split n)
  (make-op (&amp;space n)
    (lambda () $)
    (lambda (x) x)))")
      (Code "&amp;space")
      " creates a constant of a given width, while "
      (Code "&amp;split")
      " constructs a procedure that inserts the given spacing between its arguments.")
   (H3. "Arrows")
   (P "Arrows of all kinds are also used constantly in mathematics; "
      "see the following table:"
      (MB (set-attr*
           (&Table
            ($rarr (Ms "$rarr") (&rarr $a $b) (Ms "(&amp;rarr $a $b)"))
            ($-> (Ms "$->") (&-> $a $b) (Ms "(&amp;-> $a $b)"))
            ($larr (Ms "$larr") (&larr $a $b) (Ms "(&amp;larr $a $b)"))
            ($<- (Ms "$&lt;-") (&<- $a $b) (Ms "(&amp;&lt;- $a $b)"))
            ($uarr (Ms "$uarr") (&uarr $a $b) (Ms "(&amp;uarr $a $b)"))
            ($darr (Ms "$darr") (&darr $a $b) (Ms "(&amp;darr $a $b)"))
            ($harr (Ms "$harr") (&harr $a $b) (Ms "(&amp;harr $a $b)"))
            ($<-> (Ms "$&lt;->") (&<-> $a $b) (Ms "(&amp;&lt;-> $a $b)"))
            ($rArr (Ms "$rArr") (&rArr $a $b) (Ms "(&amp;rArr $a $b)"))
            ($=> (Ms "$=>") (&=> $a $b) (Ms "(&amp;=> $a $b)"))
            ($lArr (Ms "$lArr") (&lArr $a $b) (Ms "(&amp;lArr $a $b)"))
            ($uArr (Ms "$uArr") (&uArr $a $b) (Ms "(&amp;uArr $a $b)"))
            ($dArr (Ms "$dArr") (&dArr $a $b) (Ms "(&amp;dArr $a $b)"))
            ($hArr (Ms "$hArr") (&hArr $a $b) (Ms "(&amp;hArr $a $b)"))
            ($<=> (Ms "$&lt;=>") (&<=> $a $b) (Ms "(&amp;&lt;=> $a $b)"))
            ($xrarr (Ms "$xrarr") (&xrarr $a $b) (Ms "(&amp;xrarr $a $b)"))
            ($--> (Ms "$-->") (&--> $a $b) (Ms "(&amp;--> $a $b)"))
            ($xlarr (Ms "$xlarr") (&xlarr $a $b) (Ms "(&amp;xlarr $a $b)"))
            ($<-- (Ms "$&lt;--") (&<-- $a $b) (Ms "(&amp;&lt;-- $a $b)"))
            ($xharr (Ms "$xharr") (&xharr $a $b) (Ms "(&amp;xharr $a $b)"))
            ($<--> (Ms "$&lt;-->") (&<--> $a $b) (Ms "(&amp;&lt;--> $a $b)"))
            ($xrArr (Ms "$xrArr") (&xrArr $a $b) (Ms "(&amp;xrArr $a $b)"))
            ($==> (Ms "$==>") (&==> $a $b) (Ms "(&amp;==> $a $b)"))
            ($xlArr (Ms "$xlArr") (&xlArr $a $b) (Ms "(&amp;xlArr $a $b)"))
            ($<== (Ms "$&lt;==") (&<== $a $b) (Ms "(&amp;&lt;== $a $b)"))
            ($xhArr (Ms "$xhArr") (&xhArr $a $b) (Ms "(&amp;xhArr $a $b)"))
            ($<==> (Ms "$&lt;==>") (&<==> $a $b) (Ms "(&amp;&lt;==> $a $b)"))
            ($rrarr (Ms "$rrarr") (&rrarr $a $b) (Ms "(&amp;rrarr $a $b)"))
            ($llarr (Ms "$llarr") (&llarr $a $b) (Ms "(&amp;llarr $a $b)"))
            ($uuarr (Ms "$uuarr") (&uuarr $a $b) (Ms "(&amp;uuarr $a $b)"))
            ($ddarr (Ms "$ddarr") (&ddarr $a $b) (Ms "(&amp;ddarr $a $b)")))
           'columnalign "center left center left"
           'frame "solid"
           'rowlines "solid"
           'columnlines "solid"))
      "Here " (Code "->") " is effectively an alias for " (Code "rarr") ", "
      (Code "&lt;->") " for " (Code "harr") ", "
      (Code "=>") " for " (Code "rArr") ", "
      (Code "&lt;=>") " for " (Code "hArr")
      ", and so on.")
   (H3. "Ellipses")
   (P "SMathML provides several common ellipsis constants."
      (MB (set-attr*
           (&Table
            ($..h (Ms "$..h"))
            ($..c (Ms "$..c"))
            ($..v (Ms "$..v"))
            ($..u (Ms "$..u"))
            ($..d (Ms "$..d")))
           'columnalign "center left"
           'frame "solid"
           'rowlines "solid"
           'columnlines "solid")))
   (H3. "Overlines and Underlines")
   (P "SMathML provides procedures for overlines and underlines. "
      "In the future, procedures for underbraces and the like should perhaps be added as well."
      (CodeB "(define $OverBar (Mo &quot;&amp;OverBar;&quot;))
(define (OverBar x)
  (^^ x $OverBar))
(define $UnderBar (Mo &quot;&amp;UnderBar;&quot;))
(define (UnderBar x)
  (__ x $UnderBar))")
      "As for how they look, see the following table:"
      (MB (set-attr*
           (&Table
            ((OverBar (&+ $x $y)) (Ms "(OverBar (&amp;+ $x $y))"))
            ((UnderBar (&+ $x $y)) (Ms "(UnderBar (&amp;+ $x $y))")))
           'columnalign "center left"
           'frame "solid"
           'rowlines "solid"
           'columnlines "solid"))
      "Remark. Firefox currently has a strange bug in rendering overlines, "
      "one that did not exist when the author first started using MathML: "
      "the overline turns into a left arrow.")
   (H3. "Function Notation")
   (P "SMathML provides some common notations related to functions."
      (CodeB "(define (func f A B)
  (&amp;: f (&amp;-> A B)))")
      (Code "func") " states the type of a function, for example "
      (func $f $A $B) "."
      (CodeB "(define $\\|-> (Mo &quot;&amp;mapsto;&quot;))")
      "The procedure " (Code "&amp;\\|->")
      " corresponding to the constant " (Code "$\\|->")
      " is used in mathematics to define anonymous functions, "
      "much like the " $lambda " notation in programming languages and logic; for example, "
      (&\|-> $x $x^2) " denotes the squaring function."
      (CodeB "(define (func:def f A B x fx)
  (&amp;: f (&amp;cm (&amp;-> A B) (&amp;\\|-> x fx))))")
      "Sometimes the type of a function is written side by side "
      "with its mapping rule, for example "
      (func:def $f (&c* $RR $RR) $RR
                (tu0 $x $y) (Msqrt (&+ $x^2 $y^2))) ".")
   (H3. "Limit Notation")
   (P "Limit notation is used all the time in analysis."
      (CodeB "(define $lim (Mi &quot;lim&quot;))
(define lim
  (case-lambda
    ((p f) (: (__ $lim p) f))
    ((x a f) (: (__ $lim (&amp;->0 x a)) f))))")
      "Note that the operator " (Code "$->0")
      " underlying the procedure " (Code "&amp;->0")
      " is a compact version of the arrow " (Code "$->") ".")
   (H3. "Inference Rules")
   (P "Logic and programming languages often call for typesetting "
      "inference rules and proof trees. "
      "SMathML's support for this is based entirely on " (Code "Mfrac")
      "; the result is not especially pleasing to the eye, "
      "but at least it is readable.")
   (P (CodeB "(define (&amp;rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (if (null? j*)
        (car j1)
        (~ #:attr* '((displaystyle &quot;true&quot;))
           (apply (&amp;split n) j*) (car j1)))))")
      (Code "&amp;rule")
      " constructs a rule, but omits the horizontal line when given only one argument. "
      "Many books actually prefer to keep the line, "
      "in which case readers may need to write their own definition. "
      "Admittedly, we probably ought to define some parameters for this, "
      "but the author had not thought of it at the time.")
   (P (CodeB "(define (&amp;rule* j . j*)
  (if (null? j*)
      j
      (apply &amp;rule*
             (~ #:attr* '((displaystyle &quot;true&quot;))
                j (car j*))
             (cdr j*))))")
      (Code "&amp;rule*")
      " is somewhat like " (Code "&amp;rule")
      ", but expresses a chain of successive rule applications.")

   (H2. "Transformations")
   (P "Transformations are the core of SMathML; "
      "without them many abstractions would be hard to express.")
   (H3. "The General Transformation " (Code "T"))
   (P "Let us start with the code of " (Code "T") "."
      (CodeB "(define (T style*)
  (define default-handler
    (let ((default-binding (assoc '(default) style*)))
      (match default-binding
        (`((default) *preorder* ,handler) handler)
        (`((default) ,handler)
         (lambda (tag attr* . xml*)
           (apply handler tag attr* (map transform xml*))))
        (else
         (lambda (tag attr* . xml*)
           `(,tag ,attr* . ,(map transform xml*)))))))
  (define text-handler
    (let ((text-binding (assoc '(text) style*)))
      (if text-binding
          (cadr text-binding)
          identity)))
  (define Style*
    (map
     (lambda (binding)
       (match binding
         (`(,tag *preorder* ,handler)
          (cons tag handler))
         (`(,tag ,handler)
          (cons tag
                (lambda (tag attr* . xml*)
                  (apply handler tag attr*
                         (map transform xml*)))))))
     (filter (lambda (style) (symbol? (car style)))
             style*)))
  (define (transform xml)
    (match xml
      (`(,tag ,attr* . ,xml*)
       (let ((binding (assq tag Style*)))
         (if binding
             (apply (cdr binding) tag attr* xml*)
             (apply default-handler tag attr* xml*))))
      (str (text-handler str))))
  transform)")
      "To understand this code, we should first look at its comments."
      (CodeB ";&lt;xml> ::= &lt;string> | &lt;symbol> | &lt;number> | &lt;boolean> | &lt;vector>
;       |  (&lt;tag> (&lt;attr>*) &lt;xml>*)
;&lt;tag> ::= &lt;symbol> | &lt;a scheme value other than symbol>
;&lt;attr> ::= (&lt;symbol> &lt;string>)
;&lt;binding> ::= (&lt;symbol> &lt;handler>)
;           |  (&lt;symbol> *preorder* &lt;handler>)
;           |  ((default) &lt;handler>)
;           |  ((default) *preorder* &lt;handler>)
;           |  ((text) &lt;text-handler>)
;&lt;handler> : &lt;symbol> * &lt;attr>* * &lt;xml>* -> &lt;xml>
;&lt;text-handler> : &lt;string> -> &lt;string>"))
   (P "The current design of " (Code "T") " has one awkward point: "
      "it provides no syntax for handling several possible symbols in the same way. "
      "There are various workarounds, however, so it is not that this cannot be expressed. "
      "This should be changed in the future.")

   (H3. "Modifying Attributes")
   (P "SMathML provides two procedures for modifying attributes: "
      (Code "attr*-set") " and "
      (Code "set-attr*") ". In fact, "
      (Code "set-attr*") " is built on "
      (Code "attr*-set") "."
      (CodeB "(define (attr*-set attr* . xv*)
  (define (s a* x v)
    (if (eq? v #f)
        a*
        (let s ((a* a*) (x x) (v v))
          (cond ((null? a*)
                 (list (list x v)))
                ((eq? (caar a*) x)
                 (cons (list x v) (cdr a*)))
                (else
                 (cons (car a*)
                       (s (cdr a*) x v)))))))
  (if (null? xv*)
      attr*
      (let iter ((x (car xv*))
                 (v (cadr xv*))
                 (xv* (cddr xv*))
                 (a* attr*))
        (if (null? xv*)
            (s a* x v)
            (iter (car xv*) (cadr xv*) (cddr xv*)
                  (s a* x v))))))")
      (Code "attr*-set")
      " modifies the association list representing the attributes, "
      "and can take several attributes to modify at once. "
      "The procedure is currently not very efficient; "
      "a simple " (Code "append")
      " might well be the better practical choice. "
      "That would subtly change the semantics, though, "
      "since currently a modification appearing later can override an earlier one. "
      "Then again, in my own use of SMathML "
      "I seem never to have done that. "
      "Of course, the current implementation has a merit of its own: "
      "it produces the most compact attribute lists."
      (CodeB "(define (set-attr* xml . xv*)
  (match xml
    (`(,tag ,attr* . ,xml*)
     `(,tag ,(apply attr*-set attr* xv*) . ,xml*))
    ((? string? str)
     `(div ,(apply attr*-set '() xv*) ,str))))")
      (Code "set-attr*")
      " modifies an SXML element; "
      "if given something that is not an element, it automatically wraps it in a " (Code "div")
      ". To the author, " (Code "span")
      " now seems the more sensible choice, "
      "but the habit is too entrenched to change. "
      "(In fact, the author can no longer remember whether this special case "
      "was ever used on the blog.)")
   (H3. (Code "Tm") ": An Auxiliary Transformation for Mathematical Content")
   (P (Code "Tm") " is a core transformation procedure; "
      "almost every page of the author's blog uses " (Code "Tm")
      ". In fact, whenever a page contains mathematical content, "
      "it should use " (Code "Tm") "; this is the recommended practice.")
   (P (Code "Tm") " fixes some of the awkwardness of using SMathML. "
      "For example, without " (Code "Tm")
      ", even a MathML element meant to be inline "
      "would have to be wrapped in " (Code "Math")
      " or " (Code "M") ", which is clearly unreasonable. "
      "Merely adding " (Code "Math")
      " is easy to automate: one only needs to detect MathML elements left bare. "
      "But expressions already wrapped in a MathML root element "
      "must be left alone, and so must their attributes; "
      "otherwise, for instance, switching between inline and block mode would break. "
      (Code "Tm") " also smooths over some other awkward points. "
      "For example, using text inside MathML used to require adding "
      (Code "Mtext") " by hand, which " (Code "Tm") " now automates completely. "
      (Code "Tm") " also transforms some other constants that would otherwise be invalid, "
      "for example wrapping integers in " (Code "Mn")
      ". Moreover, for a bare integer, " (Code "Tm")
      " does not forget to add " (Code "Math")
      ". Symbols and fractions are also handled by " (Code "Tm")
      " as intuitively as possible.")
   (P "Here is our implementation of " (Code "Tm") "."
      (CodeB "(define (Mid id) (Mi (symbol->string id)))
(define (Mnum n) (Mn (number->string n)))
(define (Mint n)
  (if (&lt; n 0)
      (&amp;- (Mnum (- n)))
      (Mnum n)))
(define (Mrat n)
  (let* ((a (numerator n))
         (b (denominator n)))
    (if (&lt; a 0)
        (&amp;- (~ (Mnum (- a)) (Mnum b)))
        (~ (Mnum a) (Mnum b)))))
(define mathml-tag*
  (list->seteq
   '(merror
     mfrac mi mmultiscripts mn mo mover
     mpadded mphantom mroot mrow
     ms mspace msqrt mstyle msub msubsup
     msup mtable mtd mtext mtr munder
     munderover maction menclose mfenced
     #;mprescripts)))
(define math-style*
  `(((default)
     *preorder*
     ,(lambda (tag attr* . xml*)
        (cond ((eq? tag 'math) `(,tag ,attr* . ,(map Tx xml*)))
              ((set-member? mathml-tag* tag)
               (Math (Tx `(,tag ,attr* . ,xml*))))
              (else `(,tag ,attr* . ,(map Tm xml*))))))
    ((text) ,(lambda (text)
               (cond ((string? text) text)
                     ((symbol? text) (Math (Mid text)))
                     ((integer? text) (Math (Mint text)))
                     ((rational? text) (Math (Mrat text)))
                     (else (error 'Tm &quot;unknown element ~s&quot; text)))))
    ))
;token elements: mtext mi mn mo mspace ms
(define token-tag*
  (seteq 'mtext 'mi 'mn 'mo 'mspace 'ms))
(define mtext-style*
  `(((default)
     *preorder*
     ,(lambda (tag attr* . xml*)
        (cond ((memq tag '(mi mn mo ms mspace mtext))
               `(,tag ,attr* . ,xml*))
              (else `(,tag ,attr* . ,(map Tx xml*))))))
    ((text) ,(lambda (text)
               (cond ((string? text) (% text))
                     ((symbol? text) (Mid text))
                     ((integer? text) (Mint text))
                     ((rational? text) (Mrat text))
                     (else (error 'Tm &quot;unknown element ~s&quot; text)))))))
(define Tm (T math-style*))
(define Tx (T mtext-style*))")
      "The naming here is rather arbitrary, but the author could not come up with anything better. "
      "Attentive readers may notice that the author does not use " (Code "token-tag*")
      "; this is mainly because the author found that, with only a few elements, "
      "a plain list seems to be more efficient.")
   (P "Based on " (Code "Tm") ", we define "
      (Code "TmPrelude") " as a straightforward composition."
      (CodeB "(define TmPrelude
  (compose Tm Prelude))")
      "Racket's own " (Code "compose")
      " has one great advantage: it preserves keyword arguments.")
   (H4. "The Refactored Version")
   (P "Recently the author refactored " (Code "Tm")
      " so that it no longer uses " (Code "T")
      ", which should lower the cognitive load of fully understanding it."
      (CodeB "(define (Tm xml)
  (define (wrap text)
    (cond
      ((symbol? text) (Mid text))
      ((integer? text) (Mint text))
      ((rational? text) (Mrat text))
      (else (error 'Tm &quot;unknown element ~s&quot; text))))
  (define (G xml)
    (match xml
      (`(,tag ,attr* . ,xml*)
       (cond ((eq? tag 'math)
              `(,tag ,attr* . ,(map M xml*)))
             ((set-member? mathml-tag* tag)
              (Math (M xml)))
             (else
              `(,tag ,attr* . ,(map G xml*)))))
      (text
       (if (string? text)
           text
           (Math (wrap text))))))
  (define (M xml)
    (match xml
      (`(,tag ,attr* . ,xml*)
       (if (memq tag '(mi mn mo ms mspace mtext))
           xml
           `(,tag ,attr* . ,(map M xml*))))
      (text
       (if (string? text)
           (Mtext text)
           (wrap text)))))
  (G xml))"))
   (H3. "Table Transformation")
   (P "In the early days of using SMathML, "
      "the author felt the need for a position-sensitive table transformation. "
      "Now, however, the author thinks it is better to do the transformation "
      "while constructing the table."
      (CodeB "(define (mapi f l)
  (let m ((i 0) (l l))
    (if (null? l)
        '()
        (cons (f (car l) i)
              (m (+ i 1) (cdr l))))))
(define (Ttable Td)
  (define (Tt tag attr* . r*)
    `(mtable ,attr* . ,(mapi Tr r*)))
  (define (Tr r i)
    (apply
     (lambda (tag attr* . d*)
       `(mtr ,attr* .
             ,(mapi (lambda (d j) (Td d i j)) d*)))
     r))
  (T `((mtable *preorder* ,Tt))))")
      "In other words, the author has not used "
      (Code "Ttable") " for a while.")
   (H2. "Automatic Numbering and Cross-References")
   (P "Automatic numbering and cross-referencing are essential in practice. "
      "SMathML now has preliminary support for them, "
      "though it is rather cumbersome to use.")
   (P "The rough idea is as follows. We use special vector tags "
      "whose fields are, naturally, mutable. "
      "First we accumulate information in a linear traversal, "
      "constructing the corresponding headings and entries, "
      "together with their citation forms, along the way. "
      "We also build an association list from strings to citation forms. "
      "Then a second traversal "
      "replaces references made by strings with the corresponding citation forms.")
   (P "This mechanism is not elegant, but at least it is serviceable; "
      "it will quite likely be rewritten substantially in the future.")
   (H3. "Implementation")
   (P (CodeB "(define (pass0 exp)
  (define citation-table '())
  (define (extend-table! id citation)
    (if (assoc id citation-table)
        (error 'automatic-numbering-pass0 &quot;id conflict!&quot;)
        (set! citation-table
              (cons (cons id citation) citation-table))))
  (let iterate ((henv '(0)) (section #f) (genv '()) (lenv '()) (rest exp) (result '()))
    (if (null? rest)
        (cons citation-table (reverse result))
        (let ((current (car rest)) (rest (cdr rest)))
          (match current
            ((,tag ,attr* . ,xml*)
             (cond
               ((symbol? tag)
                (iterate henv section genv lenv rest (cons current result)))
               ((%heading? tag)
                (define level (%heading-level tag))
                (define id (%heading-id tag))
                (define auto? (%heading-auto? tag))
                (define present (%heading-present tag))
                (define cite (%heading-cite tag))
                (define switch? (%heading-switch? tag))
                (cond (auto? (define section (henv-next henv level))
                             (set-%heading-section! tag section)
                             (when id (extend-table! id (cite tag)))
                             (iterate section section genv
                                      (if switch? '() lenv) rest
                                      (cons (apply present tag attr* xml*) result)))
                      (else (define section (henv-* henv level))
                            (set-%heading-section! tag section)
                            (when id (extend-table! id (cite tag)))
                            (iterate henv section genv
                                     (if switch? '() lenv) rest
                                     (cons (apply present tag attr* xml*) result)))))
               ((%entry? tag)
                (define local? (%entry-local? tag))
                (define class (%entry-class tag))
                (define id (%entry-id tag))
                (define auto? (%entry-auto? tag))
                (define present (%entry-present tag))
                (define cite (%entry-cite tag))
                (set-%entry-section! tag section)
                (cond (auto? (let-values (((g/lenv nval)
                                           (g/lenv-next (if local? lenv genv) class)))
                               (set-%entry-index! tag nval)
                               (when id (extend-table! id (cite tag)))
                               (if local?
                                   (iterate henv section genv g/lenv rest
                                            (cons (apply present tag attr* xml*) result))
                                   (iterate henv section g/lenv lenv rest
                                            (cons (apply present tag attr* xml*)
                                                  result)))))
                      (else (when id (extend-table! id (cite tag)))
                            (iterate henv section genv lenv rest
                                     (cons (apply present tag attr* xml*) result)))))
               (else
                (iterate henv section genv lenv rest (cons current result)))))
            (,else (iterate henv section genv lenv rest (cons current result))))))))")
      "where"
      (CodeB "(define (make-compatible henv level)
  (define len (length henv))
  (define (aux henv i)
    (if (= i level)
        henv
        (aux (cons 0 henv) (add1 i))))
  (cond ((= len level) henv)
        ((> len level) (take-right henv level))
        (else (aux henv len))))
(define (henv-inc henv)
  (cons (add1 (car henv)) (cdr henv)))
(define (henv-next henv level)
  (henv-inc (make-compatible henv level)))
(define (henv-* henv level)
  (define comp (make-compatible henv level))
  (cons '* (cdr comp)))
(define (extend-g/lenv class index g/lenv)
  (cons (cons class (box index)) g/lenv))
(define (g/lenv-next g/lenv class)
  (define binding (assoc class g/lenv))
  (if binding
      (let* ((box0 (cdr binding))
             (nval (add1 (unbox box0))))
        (set-box! box0 nval)
        (values g/lenv nval))
      (values (extend-g/lenv class 1 g/lenv) 1)))")
      (Code "pass0") " is fairly complicated. One thing to note is that "
      "it traverses only the top level linearly and does not descend further.")
   (P (CodeB ";pass1
(define (Ref id) `(ref () ,id))
(define (pass1 exp)
  (define citation-table (car exp))
  (define (reify id)
    (cond ((assoc id citation-table) => cdr)
          (else (error 'pass1 &quot;unknown id ~s&quot; id))))
  (define Tr
    (T `((ref *preorder* ,(lambda (ref empty id) (reify id))))))
  (define xml* (cdr exp))
  (map Tr xml*))")
      (Code "pass1") " is much simpler, "
      "since what it does is essentially just substitution. "
      (Code "Ref") " is a simple API provided by SMathML "
      "for referring to headings and entries.")
   (P (CodeB ";automatic numbering
(define numbering-style*
  `((body
     *preorder*
     ,(lambda (tag attr* . xml*)
        `(,tag ,attr* . ,(pass1 (pass0 xml*)))))))
(define Tn
  (T numbering-style*))")
      "Finally, putting " (Code "pass0") " and " (Code "pass1")
      " together gives us " (Code "Tn") ".")
   (P "Analogous to " (Code "TmPrelude") ", SMathML also provides "
      (Code "TnTmPrelude") "."
      (CodeB "(define TnTmPrelude
  (compose Tn Tm Prelude))"))
   (H3. "Default Auxiliary Procedures")
   (P (Code "Tn") " is only an overall framework; "
      "for headings and entries, "
      "the present and cite functions have to be written before they can be used. "
      "SMathML therefore also provides some default auxiliary procedures.")
   (P (CodeB "(define (format-section section)
  (define sec (cdr (reverse section)))
  (apply string-append
         (add-between (map number->string sec) &quot;.&quot;)))
(define (heading-present %heading attr* . html*)
  (define level (%heading-level %heading))
  (define auto? (%heading-auto? %heading))
  (define section (%heading-section %heading))
  (define id (%heading-id %heading))
  (cond ((= level 4) `(h4 ,(attr*-set attr* 'id id)
                          ,(format &quot;第~a小节 &quot; (format-section section))
                          . ,html*))
        ((= level 3) `(h3 ,(attr*-set attr* 'id id)
                          ,(format &quot;第~a节 &quot; (format-section section))
                          . ,html*))
        ((= level 2) `(h2 ,(attr*-set attr* 'id id)
                          ,(format &quot;第~a章 &quot; (format-section section))
                          . ,html*))
        ((= level 1) `(h1 ,(attr*-set attr* 'id id) . ,html*))
        (else (error 'heading-present &quot;invalid level ~s&quot; level))))
(define (h4-present %heading attr* . html*)
  (define auto? (%heading-auto? %heading))
  (define id (%heading-id %heading))
  (if auto?
      (let ((section (%heading-section %heading)))
        `(h4 ,(attr*-set attr* 'id id)
             ,(format &quot;第~a小节 &quot; (format-section section))
             . ,html*))
      `(h4 ,(attr*-set attr* 'id id) . ,html*)))
(define (h3-present %heading attr* . html*)
  (define auto? (%heading-auto? %heading))
  (define id (%heading-id %heading))
  (if auto?
      (let ((section (%heading-section %heading)))
        `(h3 ,(attr*-set attr* 'id id)
             ,(format &quot;第~a节 &quot; (format-section section))
             . ,html*))
      `(h3 ,(attr*-set attr* 'id id) . ,html*)))
(define (h2-present %heading attr* . html*)
  (define auto? (%heading-auto? %heading))
  (define id (%heading-id %heading))
  (if auto?
      (let ((section (%heading-section %heading)))
        `(h2 ,(attr*-set attr* 'id id)
             ,(format &quot;第~a章 &quot; (format-section section))
             . ,html*))
      `(h2 ,(attr*-set attr* 'id id) . ,html*)))
(define (h1-present %heading attr* . html*)
  (define id (%heading-id %heading))
  `(h1 ,(attr*-set attr* 'id id) . ,html*))
(define (heading-cite %heading)
  (define id (%heading-id %heading))
  (define href (string-append &quot;#&quot; id))
  (define section (%heading-section %heading))
  (define level (%heading-level %heading))
  (define ref
    `(a ((href ,href)) ,(format-section section)))
  (cond ((= level 4) (Cite &quot;第&quot; ref &quot;小节&quot;))
        ((= level 3) (Cite &quot;第&quot; ref &quot;节&quot;))
        ((= level 2) (Cite &quot;第&quot; ref &quot;章&quot;))
        (else
         (error 'heading-cite &quot;invalid level ~s&quot; level))))
(define (H1. #:attr* [attr* '()] #:id [id #f] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h1-present #:level 1
                     #:id id #:auto? auto?)
    ,attr* . ,html*))
(define (H2. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #t] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h2-present #:cite heading-cite
                     #:level 2 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (H3. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #t] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h3-present #:cite heading-cite
                     #:level 3 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))
(define (H4. #:attr* [attr* '()] #:id [id #f]
             #:switch? [switch? #t] #:auto? [auto? #t] . html*)
  `(,(build-%heading #:present h4-present #:cite heading-cite
                     #:level 4 #:id id #:switch? switch?
                     #:auto? auto?)
    ,attr* . ,html*))")
      "There are some design mistakes here as well; "
      "for example, many things ought to be parameterized, "
      "but for now readers can only override these definitions with new ones of the same name. "
      "Also, the function " (Code "heading-present")
      " is a historical leftover. It has not been removed yet out of concern for compatibility, "
      "but it will most likely be removed in the future.")
   (P (Del "As for entries, SMathML does not yet provide any procedures for them. "
           "While writing the blog, the author did develop a standard set of routines, "
           "but the author always has to tweak them slightly for each page, "
           "so this code has not yet been merged into SMathML."))
   (P "SMathML now provides a standard set of procedures for defining entries, "
      "and lets users adjust the numbering format through parameters. "
      "Users can also choose whether each kind of entry is numbered separately. "
      "The one flaw is that the citation format is fixed "
      "as the name of the entry kind followed by its number; "
      "perhaps the citation format should be parameterized in the future as well."
      (CodeB "(define (format-num section index local?)
  (cond ((or (eq? (car section) '*)
             (not local?))
         (if index
             (format &quot;~a&quot; index)
             #f))
        (else
         (format &quot;~a.~a&quot;
                 (apply string-append
                        (add-between
                         (map number->string
                              (cdr (reverse section))) &quot;.&quot;))
                 (if index index &quot;*&quot;)))))
(define (format-head name section index local?)
  (let ((num ((parameter:format-num)
              section index local?)))
    (if num
        (B name (format &quot;~a. &quot; num))
        (B name &quot;. &quot;))))
(define local-entry?
  (make-parameter #t))
(define separate-class?
  (make-parameter #f))
(define parameter:format-num
  (make-parameter format-num))
(define parameter:format-head
  (make-parameter format-head))
(define (Entry name class)
  (define (present %entry attr* . html*)
    (define local? (%entry-local? %entry))
    (define id (%entry-id %entry))
    (define Attr* (attr*-set attr* 'class class 'id id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define head ((parameter:format-head)
                  name section index local?))
    `(div ,Attr* ,head . ,html*))
  (define (cite %entry)
    (define local? (%entry-local? %entry))
    (define id (%entry-id %entry))
    (define href (string-append &quot;#&quot; id))
    (define section (%entry-section %entry))
    (define index (%entry-index %entry))
    (define num ((parameter:format-num)
                 section index local?))
    (if num
        (Cite name `(a ((href ,href)) ,num))
        (Cite `(a ((href ,href)) &quot;某&quot; ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (make-%entry
             (local-entry?)
             (if (separate-class?) class #f)
             id auto? #f #f present cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Axiom &quot;公理&quot; &quot;axiom&quot;)
  (Definition &quot;定义&quot; &quot;definition&quot;)
  (Lemma &quot;引理&quot; &quot;lemma&quot;)
  (Theorem &quot;定理&quot; &quot;theorem&quot;)
  (Corollary &quot;推论&quot; &quot;corollary&quot;)
  (Example &quot;例子&quot; &quot;example&quot;)
  (Notation &quot;记号&quot; &quot;notation&quot;)
  (Comment &quot;注记&quot; &quot;comment&quot;)
  (Proposition &quot;命题&quot; &quot;proposition&quot;)
  (Remark &quot;评注&quot; &quot;remark&quot;)
  (Exercise &quot;练习&quot; &quot;exercise&quot;)
  (Convention &quot;约定&quot; &quot;convention&quot;)
  (Warning &quot;警告&quot; &quot;warning&quot;))")
      "Note that " (Code "format-num") " and "
      (Code "format-head") " are not provided. "
      "This mechanism in SMathML now also takes globally numbered entries into account. "
      "However, since many globally numbered entries (bibliography items, for example) "
      "are presented and cited differently from ordinary locally numbered entries, "
      "we should perhaps seriously consider how to offer parameterized customization for them.")
   (H2. "Output")
   (P "For SMathML to be of any actual use, we must provide a set of output procedures.")
   (H3. "Outputting HTML/XML")
   (P "To convert SXML into actual HTML/XML, we provide a procedure "
      (Code "Xml") ", implemented as follows:"
      (CodeB ";&lt;xml> ::= &lt;string>
;       |  (&lt;tag> (&lt;attr>*) &lt;xml>*)
;&lt;tag> ::= &lt;symbol>
;&lt;attr> ::= (&lt;symbol> &lt;string>)
(define (Attr* attr*)
  (for-each
   (lambda (attr)
     (printf &quot; ~s=~s&quot; (car attr) (cadr attr)))
   attr*))
(define (Xml xml)
  (match xml
    (`(,tag ,attr* . ,xml*)
     #:when (symbol? tag)
     (if (null? xml*)
         (begin
           (printf &quot;&lt;~s&quot; tag)
           (Attr* attr*)
           (printf &quot;/>&quot;))
         (begin
           (printf &quot;&lt;~s&quot; tag)
           (Attr* attr*)
           (printf &quot;>&quot;)
           (for-each Xml xml*)
           (printf &quot;&lt;/~s>&quot; tag))))
    ((? string? str)
     (printf &quot;~a&quot; str))))")
      "Note that although we allow non-symbol tags and non-string leaves "
      "during intermediate transformations, by the time we output with " (Code "Xml")
      " we must be back to the most primitive definition.")
   (H3. "Outputting CSS")
   (P (CodeB ";&lt;attr> ::= (&lt;symbol> &lt;string>)
;&lt;style> ::= (&lt;string> &lt;attr>*)
;&lt;css> ::= (&lt;style>*)
(define (Attr* attr*)
  (for-each
   (lambda (attr)
     (printf &quot;~s: ~a;&quot; (car attr) (cadr attr)))
   attr*))
(define (Style style)
  (printf &quot;~a {&quot; (car style))
  (Attr* (cdr style))
  (printf &quot;}\\n&quot;))
(define (Css css)
  (for-each Style css))")
      "SMathML provides no real means of abstraction and combination for CSS, "
      "only a procedure for outputting it.")
   (H3. "Outputting to Files")
   (P "Of course, outputting alone is not enough; "
      "we also provide some auxiliary procedures for outputting to files."
      (CodeB "(define (size x)
  (cond ((pair? x) (+ (size (car x)) (size (cdr x))))
        ((null? x) 0)
        ((string? x) (string-length x))
        (else 1)))
(define replace? (make-parameter #f))
(define compute-size? (make-parameter #f))
(define ((emit proc) exp path)
  (define (emit-file)
    (when (compute-size?)
      (printf &quot;size of [~a]: ~a\\n&quot; path (size exp)))
    (with-output-to-file path
      (lambda () (proc exp))
      #:exists 'replace))
  (if (file-exists? path)
      (if (replace?)
          (emit-file)
          (printf &quot;~s exists, emit cancelled\\n&quot; path))
      (emit-file)))
(define ((emit-thunk proc) thunk path)
  (define (emit-file)
    (define exp (thunk))
    (when (compute-size?)
      (printf &quot;size of [~a]: ~a\\n&quot; path (size exp)))
    (with-output-to-file path
      (lambda () (proc exp))
      #:exists 'replace))
  (if (file-exists? path)
      (if (replace?)
          (emit-file)
          (printf &quot;~s exists, emit cancelled\\n&quot; path))
      (emit-file)))
(define ((emit-promise proc) promise path)
  (define (emit-file)
    (define exp (force promise))
    (when (compute-size?)
      (printf &quot;size of [~a]: ~a\\n&quot; path (size exp)))
    (with-output-to-file path
      (lambda () (proc exp))
      #:exists 'replace))
  (if (file-exists? path)
      (if (replace?)
          (emit-file)
          (printf &quot;~s exists, emit cancelled\\n&quot; path))
      (emit-file)))")
      "We could in fact merge " (Code "emit") ", " (Code "emit-thunk")
      " and " (Code "emit-promise")
      " into a single procedure, but the current design may reduce the chance of mistakes. "
      "These procedures do just what their names suggest: "
      (Code "emit") " simply outputs to a file; " (Code "emit-thunk")
      " outputs a thunk (a function of no arguments), which therefore has to be called first; "
      (Code "emit-promise")
      " outputs a promise, which therefore has to be forced first.")
   (P "We provide both " (Code "emit-thunk") " and "
      (Code "emit-promise")
      " because a promise caches the result of its computation. "
      "Sometimes, however, we may want to run benchmarks, "
      "and then a plain thunk suits our needs better. "
      "Either way, both procedures can save some work when many files need to be output. "
      "(This is of course not automated at all; the user has to decide by hand.)")
   (P "Based on " (Code "emit") " and " (Code "emit-thunk")
      ", we provide the following procedures:"
      (CodeB "(define emitXml (emit Xml))
(define emitCss (emit Css))
(define emitXml-thunk (emit-thunk Xml))
(define emitCss-thunk (emit-thunk Css))
(define emitXml-promise
  (emit-promise Xml))
(define emitCss-promise
  (emit-promise Css))")
      (Del "We really should also provide "
           (Code "emitXml-promise") " and "
           (Code "emitCss-promise")
           ", but the author forgot them, which was a mistake. "
           "They will probably be added in the future."))

   (H2. "Miscellaneous and Experimental Features")
   (P "This chapter records various odds and ends provided by SMathML "
      "that do not fit into any system.")
   (H3. "Escaping")
   (P "SMathML provides a procedure of no arguments called " (Code "escape*")
      ". Calling it starts an interactive session: "
      "paste the text to be escaped and press Enter, "
      "and you get the escaped text back."
      (CodeB "(define (escape s)
  (define l (string-length s))
  (let iter ((i 0))
    (unless (= i l)
      (let ((c (string-ref s i)))
        (case c
          ((#\\&quot;) (printf &quot;&amp;quot;&quot;))
          ((#\\&lt;) (printf &quot;&amp;lt;&quot;))
          ((#\\&amp;) (printf &quot;&amp;amp;&quot;))
          ((#\\\\) (printf &quot;\\\\\\\\&quot;))
          (else (printf &quot;~a&quot; c))))
      (iter (+ i 1)))))
(define (escape*)
  (escape (read-line))
  (newline)
  (escape*))"))
   (H3. "Variants of " (Code "map"))
   (P "The following variants of " (Code "map")
      " were all distilled from experience using SMathML."
      (CodeB "(define (map2 proc lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) (list (proc (car lst))))
        (else (cons (proc (car lst) (cadr lst))
                    (map2 proc (cddr lst))))))
(define (map* f . x*) (map f x*))
(define (map-toggle flag proc lst)
  (cond ((null? lst) '())
        (flag (cons (proc (car lst))
                    (map-toggle (not flag) proc (cdr lst))))
        (else (cons (car lst)
                    (map-toggle (not flag) proc (cdr lst))))))")
      (Code "map2") " has already been discussed; "
      (Code "map*") " is a convenience for enumerating elements; and "
      (Code "map-toggle") " applies the procedure to every other element.")
   (H3. "Dates")
   (P (CodeB "(define now (current-date))
(define (current-date-string #:format [format 'rfc2822])
  (parameterize ((date-display-format format))
    (date->string now #t)))")
      "Using this is no longer recommended. "
      "All it really does is provide a string for the current date.")
   (H3. "Upright Capital Greek Letters")
   (P "Since upright capital Greek letters come up from time to time, "
      "SMathML provides the following definitions:"
      (CodeB "(define Γ $Gamma:normal)
(define Δ $Delta:normal)
(define Θ $Theta:normal)
(define Λ $Lambda:normal)
(define Ξ $Xi:normal)
(define Π $Pi:normal)
(define Σ $Sigma:normal)
(define Φ $Phi:normal)
(define Ψ $Psi:normal)
(define Ω $Omega:normal)"))
   (H3. "Maximum and Minimum")
   (P "Mathematics of course often needs to express maxima and minima "
      "(as well as suprema and infima). "
      "Their forms vary so much, however, "
      "that SMathML provides only the following definitions:"
      (CodeB "(define $max (Mi &quot;max&quot;))
(define (&amp;max . x*)
  (apply appl $max x*))
(define $min (Mi &quot;min&quot;))
(define (&amp;min . x*)
  (apply appl $min x*))")
      "This is of course quite inadequate, but useful enough.")
   (P "Suprema and infima have now been added too; "
      "their definitions are equivalent to the following:"
      (CodeB "(define $supremum (Mi &quot;sup&quot;))
(define $infimum (Mi &quot;inf&quot;))
(define-bigop*
  (supremum $supremum)
  (infimum $infimum))"))
   (H3. "Indexing Matrix Entries and Its Generalization")
   (P (CodeB "(define (mref A i j)
  (_ A (&amp;cm i j)))
(define (_cm A . x*)
  (_ A (apply &amp;cm x*)))")
      "The notation " (Code "mref") " usually denotes the entry of a matrix "
      "at a given row and column, and "
      (Code "_cm") " can be seen as a more general version of " (Code "mref") ".")
   (H3. "The " (Code "set-") " Family of Functions")
   (P "SMathML provides some convenience functions whose names begin with " (Code "set-")
      ", for modifying attributes, for example"
      (CodeB "(define (set-compact op)
  (set-attr* op 'lspace &quot;0&quot; 'rspace &quot;0&quot;))")
      (Code "set-compact")
      " turns an operator into a compact version. "
      "This is mainly for listing operators inside a tuple. "
      "The author has not yet figured out, though, whether it is better to modify the attributes "
      "or to define an " (Code "Mi")
      " version of the operator; the MathML standard says nothing about this."
      (CodeB "(define (set-mathvariant i v)
  (set-attr* i 'mathvariant v))")
      (Code "set-mathvariant")
      " can be used to modify the "
      (Code "mathvariant") " of a mathematical identifier."
      (CodeB "(define (set-style x style)
  (set-attr* x 'style style))")
      (Code "set-style")
      " modifies the style of an HTML element inline."
      )
   (H3. "RSS")
   (P "SMathML provides some procedures for writing RSS, "
      "which have to be imported with " (Code "(require SMathML/rss)")
      "."
      (CodeB "(define (Rss #:version [version &quot;2.0&quot;] channel)
  `(rss ((version ,version)) ,channel))
(define (Title str) `(title () ,str))
(define (Link str) `(link () ,str))
(define (Description str) `(description () ,str))
(define (Channel #:title title #:link link #:description description . xml*)
  `(channel () ,(Title title) ,(Link link) ,(Description description) . ,xml*))
(define (RC #:version [version &quot;2.0&quot;]
            #:title title #:link link #:description description
            . xml*)
  (Rss #:version version
       (keyword-apply
        Channel '(#:description #:link #:title) (list description link title)
        xml*)))
(define (Item . xml*) `(item () . ,xml*))
(define (PubDate str) `(pubDate () ,str))"))
   (H3. "General Folds over SXML")
   (P "The folds given here are, in a sense, generalizations of the list operations "
      (Code "fold-left") " and " (Code "fold-right")
      " to SXML."
      (CodeB "(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc (proc init (car lst)) (cdr lst))))
(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst)
            (fold-right proc init (cdr lst)))))
(define (Fold fdown fup fhere seed xml)
  (match xml
    (`(,tag ,attr* . ,xml*)
     (define entering
       (fdown seed tag attr* xml*))
     (define enjoying
       (fold-left
        (lambda (seed xml)
          (Fold fdown fup fhere seed xml))
        entering xml*))
     (fup seed enjoying tag attr* xml*))
    (else (fhere seed xml))))
(define (Fold* fdown fup fhere seed xml)
  (match xml
    (`(,tag ,attr* . ,xml*)
     (define-values (entering new*)
       (fdown seed tag attr* xml*))
     (define enjoying
       (fold-left
        (lambda (seed xml)
          (Fold* fdown fup fhere seed xml))
        entering new*))
     (fup seed enjoying tag attr* xml*))
    (else (fhere seed xml))))")
      "The idea of " (Code "Fold") " and " (Code "Fold*")
      " comes from the context of SSAX, "
      "but they operate directly on SXML "
      "rather than on text that has not yet been parsed.")
   (H3. "HTML Pretty-Printing")
   (P #:attr* '((style "color: red"))
      "All the code in this part was generated by an LLM "
      "and has not been reviewed by a human with full understanding. "
      "Use it with caution, and never use it in any production environment. "
      "That said, the " (Code "XML")
      " provided here is arguably more conservative than most HTML pretty-printers. "
      "In principle, however, the correct indentation behavior of HTML depends on its CSS, "
      "so our pretty-printer cannot be completely correct.")
   (P "Based on " (Code "Fold")
      " from the previous section, we provide an HTML pretty-printer here. "
      "You can of course also use it to pretty-print XML, "
      "and we provide the parameter " (Code "inline-tags")
      " to control indentation."
      (CodeB "(struct state (idt ctx) #:transparent)
(define inline-tags
  (make-parameter
   (seteq
    'pre 'textarea 'script 'style
    'b 'i 'em 'strong 'span 'a 'u 's
    'code 'kbd 'samp 'var
    'abbr 'cite 'q 'sub 'sup 'small 'mark
    'del 'ins 'time 'wbr 'br
    'bdi 'bdo 'data 'ruby 'rt 'rp
    'img 'input 'output 'select 'button 'label
    'mi 'mn 'mo 'ms 'mtext 'mspace 'annotation
    'tspan 'textPath
    'math 'svg)))
(define (inline? tag)
  (set-member? (inline-tags) tag))
(define (inline-element? tag attr*)
  (and (inline? tag)
       (not (and (eq? tag 'math)
                 (for/or ([a (in-list attr*)])
                   (and (eq? (car a) 'display)
                        (equal? (cadr a) &quot;block&quot;)))))
       (not (and (eq? tag 'svg)
                 (for/or ([a (in-list attr*)])
                   (and (eq? (car a) 'style)
                        (string-contains? (cadr a) &quot;display: block&quot;)))))))
(define (XML xml)
  (define (fdown seed tag attr* xml*)
    (define idt (state-idt seed))
    (define ctx (state-ctx seed))
    (define child-inline?
      (inline-element? tag attr*))
    (cond
      [(eq? ctx 'inline)
       (void)]
      [(eq? ctx 'block-nl)
       (display (make-string idt #\\space))]
      [else
       (unless child-inline?
         (newline)
         (display (make-string idt #\\space)))])
    (printf &quot;&lt;~a&quot; tag)
    (for ([a (in-list attr*)])
      (printf &quot; ~a=~s&quot; (car a) (cadr a)))
    (cond
      [(null? xml*)
       (display &quot;/>&quot;)
       (when (and (not (eq? ctx 'inline)) (not child-inline?))
         (newline))
       seed]
      [else
       (display &quot;>&quot;)
       (cond
         [(eq? ctx 'inline) (state idt 'inline)]
         [child-inline?     (state idt 'inline)]
         [else              (state (+ idt 2) 'block)])]))
  (define (fup seed enjoying tag attr* xml*)
    (when (pair? xml*)
      (when (eq? (state-ctx enjoying) 'block-nl)
        (display (make-string (state-idt seed) #\\space)))
      (printf &quot;&lt;/~a>&quot; tag)
      (when (and (not (eq? (state-ctx seed) 'inline))
                 (not (inline-element? tag attr*)))
        (newline)))
    (cond
      [(eq? (state-ctx seed) 'inline) seed]
      [(inline-element? tag attr*)
       (state (state-idt seed) 'mixed)]
      [else
       (state (state-idt seed) 'block-nl)]))
  (define (fhere seed xml)
    (define ctx (state-ctx seed))
    (when (eq? ctx 'block-nl)
      (display (make-string (state-idt seed) #\\space)))
    (display xml)
    (if (eq? ctx 'inline)
        seed
        (state (state-idt seed) 'mixed)))
  (Fold fdown fup fhere (state 0 'block-nl) xml))")
      "The " (Code "inline-element?")
      " in this code is mainly there to fit how SMathML is used. "
      "For general HTML and XML pretty-printing, "
      "it can be dropped in favor of testing with " (Code "inline?")
      ".")
   (H3. (Code "LINK") " and " (Code "TITLE"))
   (P "These functions were added only because the author's blog uses them a lot."
      (CodeB "(define (LINK title link)
  (A title #:attr* `((href ,link))))
(define (TITLE title link)
  (H2 (LINK title link)))")
      "Still, " (Code "LINK")
      " at least is useful in general.")
   (H3. "Helpers for Defining XML Tags")
   (P (Code "make-xml-tag")
      " defines the SXML constructor for the tag named by its argument, while "
      (Code "define-xml-tag*")
      " is a helper macro for defining many SXML constructors at once."
      (CodeB "(define (make-xml-tag name)
  (lambda (#:attr* [attr* '()] . xml*)
    `(,name ,attr* . ,xml*)))
(define-syntax-rule
  (define-xml-tag* (id name) ...)
  (begin
    (define id (make-xml-tag 'name))
    ...))"))
   
   (H2. "SVG Drawing")
   (P "SMathML's SVG drawing facilities are still very primitive and nearly unusable. "
      "Still, something is better than nothing.")
   (H3. "Basic Functions")
   (P (CodeB "(define (A #:attr* [attr* '()] . svg*)
  `(a ,attr* . ,svg*))
(define (Animate #:attr* [attr* '()] . svg*)
  `(animate ,attr* . ,svg*))
(define (AnimateMotion #:attr* [attr* '()] . svg*)
  `(animateMotion ,attr* . ,svg*))
(define (AnimateTransform #:attr* [attr* '()] . svg*)
  `(animateTransform ,attr* . ,svg*))
(define (Circle #:attr* [attr* '()] . svg*)
  `(circle ,attr* . ,svg*))
(define (ClipPath #:attr* [attr* '()] . svg*)
  `(clipPath ,attr* . ,svg*))
(define (Defs #:attr* [attr* '()] . svg*)
  `(defs ,attr* . ,svg*))
(define (Desc #:attr* [attr* '()] . svg*)
  `(desc ,attr* . ,svg*))
(define (Ellipse #:attr* [attr* '()] . svg*)
  `(ellipse ,attr* . ,svg*))
(define (FeBlend #:attr* [attr* '()] . svg*)
  `(feBlend ,attr* . ,svg*))
(define (FeColorMatrix #:attr* [attr* '()] . svg*)
  `(feColorMatrix ,attr* . ,svg*))
(define (FeComponentTransfer #:attr* [attr* '()] . svg*)
  `(feComponentTransfer ,attr* . ,svg*))
(define (FeComposite #:attr* [attr* '()] . svg*)
  `(feComposite ,attr* . ,svg*))
(define (FeConvolveMatrix #:attr* [attr* '()] . svg*)
  `(feConvolveMatrix ,attr* . ,svg*))
(define (FeDiffuseLighting #:attr* [attr* '()] . svg*)
  `(feDiffuseLighting ,attr* . ,svg*))
(define (FeDisplacementMap #:attr* [attr* '()] . svg*)
  `(feDisplacementMap ,attr* . ,svg*))
(define (FeDistantLight #:attr* [attr* '()] . svg*)
  `(feDistantLight ,attr* . ,svg*))
(define (FeDropShadow #:attr* [attr* '()] . svg*)
  `(feDropShadow ,attr* . ,svg*))
(define (FeFlood #:attr* [attr* '()] . svg*)
  `(feFlood ,attr* . ,svg*))
(define (FeFuncA #:attr* [attr* '()] . svg*)
  `(feFuncA ,attr* . ,svg*))
(define (FeFuncB #:attr* [attr* '()] . svg*)
  `(feFuncB ,attr* . ,svg*))
(define (FeFuncG #:attr* [attr* '()] . svg*)
  `(feFuncG ,attr* . ,svg*))
(define (FeFuncR #:attr* [attr* '()] . svg*)
  `(feFuncR ,attr* . ,svg*))
(define (FeGaussianBlur #:attr* [attr* '()] . svg*)
  `(feGaussianBlur ,attr* . ,svg*))
(define (FeImage #:attr* [attr* '()] . svg*)
  `(feImage ,attr* . ,svg*))
(define (FeMerge #:attr* [attr* '()] . svg*)
  `(feMerge ,attr* . ,svg*))
(define (FeMergeNode #:attr* [attr* '()] . svg*)
  `(feMergeNode ,attr* . ,svg*))
(define (FeMorphology #:attr* [attr* '()] . svg*)
  `(feMorphology ,attr* . ,svg*))
(define (FeOffset #:attr* [attr* '()] . svg*)
  `(feOffset ,attr* . ,svg*))
(define (FePointLight #:attr* [attr* '()] . svg*)
  `(fePointLight ,attr* . ,svg*))
(define (FeSpecularLighting #:attr* [attr* '()] . svg*)
  `(feSpecularLighting ,attr* . ,svg*))
(define (FeSpotLight #:attr* [attr* '()] . svg*)
  `(feSpotLight ,attr* . ,svg*))
(define (FeTile #:attr* [attr* '()] . svg*)
  `(feTile ,attr* . ,svg*))
(define (FeTurbulence #:attr* [attr* '()] . svg*)
  `(feTurbulence ,attr* . ,svg*))
(define (Filter #:attr* [attr* '()] . svg*)
  `(filter ,attr* . ,svg*))
(define (ForeignObject #:attr* [attr* '()] . svg*)
  `(foreignObject ,attr* . ,svg*))
(define (G #:attr* [attr* '()] . svg*)
  `(g ,attr* . ,svg*))
(define (Image #:attr* [attr* '()] . svg*)
  `(image ,attr* . ,svg*))
(define (Line #:attr* [attr* '()] . svg*)
  `(line ,attr* . ,svg*))
(define (LinearGradient #:attr* [attr* '()] . svg*)
  `(linearGradient ,attr* . ,svg*))
(define (Marker #:attr* [attr* '()] . svg*)
  `(marker ,attr* . ,svg*))
(define (Mask #:attr* [attr* '()] . svg*)
  `(mask ,attr* . ,svg*))
(define (Metadata #:attr* [attr* '()] . svg*)
  `(metadata ,attr* . ,svg*))
(define (Mpath #:attr* [attr* '()] . svg*)
  `(mpath ,attr* . ,svg*))
(define (Path #:attr* [attr* '()] . svg*)
  `(path ,attr* . ,svg*))
(define (Pattern #:attr* [attr* '()] . svg*)
  `(pattern ,attr* . ,svg*))
(define (Polygon #:attr* [attr* '()] . svg*)
  `(polygon ,attr* . ,svg*))
(define (Polyline #:attr* [attr* '()] . svg*)
  `(polyline ,attr* . ,svg*))
(define (RadialGradient #:attr* [attr* '()] . svg*)
  `(radialGradient ,attr* . ,svg*))
(define (Rect #:attr* [attr* '()] . svg*)
  `(rect ,attr* . ,svg*))
(define (Script #:attr* [attr* '()] . svg*)
  `(script ,attr* . ,svg*))
(define (Set #:attr* [attr* '()] . svg*)
  `(set ,attr* . ,svg*))
(define (Stop #:attr* [attr* '()] . svg*)
  `(stop ,attr* . ,svg*))
(define (Style #:attr* [attr* '()] . svg*)
  `(style ,attr* . ,svg*))
(define (Svg #:attr* [attr* '()] . svg*)
  `(svg ,attr* . ,svg*))
(define (Switch #:attr* [attr* '()] . svg*)
  `(switch ,attr* . ,svg*))
(define (Symbol #:attr* [attr* '()] . svg*)
  `(symbol ,attr* . ,svg*))
(define (Text #:attr* [attr* '()] . svg*)
  `(text ,attr* . ,svg*))
(define (TextPath #:attr* [attr* '()] . svg*)
  `(textPath ,attr* . ,svg*))
(define (Title #:attr* [attr* '()] . svg*)
  `(title ,attr* . ,svg*))
(define (Tspan #:attr* [attr* '()] . svg*)
  `(tspan ,attr* . ,svg*))
(define (Use #:attr* [attr* '()] . svg*)
  `(use ,attr* . ,svg*))
(define (View #:attr* [attr* '()] . svg*)
  `(view ,attr* . ,svg*))")
      "Note that some of these functions clash with basic functions of the HTML part, so "
      (Code "A") ", " (Code "Script") ", "
      (Code "Style") " and " (Code "Title")
      " are not exported when users use SMathML. "
      "They are, however, equivalent to the HTML versions.")
   (H3. "Graphics Library")
   (P "Since there is not yet a good design that is workable in practice, "
      "the graphics library provides only the most basic functions, which cannot go wrong. "
      "To the author, the Penrose library seems the ideal way to lay out diagrams."
      (CodeB "(struct pt (x y) #:transparent)
(struct vec (x y) #:transparent)
(define (vec+ u v)
  (vec (+ (vec-x u) (vec-x v))
       (+ (vec-y u) (vec-y v))))
(define (vec- u v)
  (vec (- (vec-x u) (vec-x v))
       (- (vec-y u) (vec-y v))))
(define (vec* s v)
  (vec (* s (vec-x v))
       (* s (vec-y v))))
(define (pt+ p v)
  (pt (+ (pt-x p) (vec-x v))
      (+ (pt-y p) (vec-y v))))
(define (pt- p q)
  (vec (- (pt-x p) (pt-x q))
       (- (pt-y p) (pt-y q))))
(define (dot u v)
  (+ (* (vec-x u) (vec-x v))
     (* (vec-y u) (vec-y v))))
(define (vec-len v)
  (sqrt (dot v v)))
(define (vec-normalize v)
  (vec* (/ 1 (vec-len v)) v))
(define (pt-lerp p q t)
  (pt+ p (vec* t (pt- q p))))"))
   (P "For now we have added the following primitives as well; "
      "clearly more will be needed in the future."
      (CodeB "(define (n2s n)
  (~r n #:precision 2))
(define stroke-width
  (make-parameter &quot;1px&quot;))
(define (:line start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1)
          (x2 ,x2)
          (y2 ,y2)
          (stroke-width
           ,(stroke-width)))))
(define arrow-head
  (Marker
   #:attr*
   '((id &quot;arrow-head&quot;)
     (viewbox &quot;0 0 10 10&quot;)
     (refX &quot;5&quot;)
     (refY &quot;5&quot;)
     (markerWidth &quot;6&quot;)
     (markerHeight &quot;6&quot;)
     (orient &quot;auto-start-reverse&quot;))
   (Path #:attr* '((d &quot;M 0 2 L 6 5 L 0 8 z&quot;)))))
(define (:arrow start end)
  (define x1 (n2s (pt-x start)))
  (define y1 (n2s (pt-y start)))
  (define x2 (n2s (pt-x end)))
  (define y2 (n2s (pt-y end)))
  `(line ((x1 ,x1)
          (y1 ,y1)
          (x2 ,x2)
          (y2 ,y2)
          (stroke-width
           ,(stroke-width))
          (marker-end
           &quot;url(#arrow-head)&quot;))))"))
   
   (H2. "Gallery")
   (P "This chapter offers practical examples of SMathML, "
      "to spark readers' imagination and help them make better use of SMathML.")
   (H3. "The Binomial Theorem")
   (P "This is the simplest and most basic example, "
      "involving no high-level abstraction."
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
      "If you were to write the MathML by hand, however, "
      "it would look like this:"
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
&lt;/math>")
      "This is the whole point of SMathML, and of many other Lisp/Scheme applications: "
      "removing the friction between thought and its expression through abstraction and combination. "
      "Moreover, the syntactic uniformity of Lisp-family languages "
      "spares us the trouble of designing a syntax.")
   (H3. "Napier's Bones")
   (P "Napier's bones are a calculating tool devised by Napier, the inventor of logarithms; "
      "their most basic use is multiplication. Below we write code "
      "to typeset the process of multiplying with Napier's bones."
      (CodeB "(define (char->mn c)
  (Mn (string c)))
(define (add0 l)
  (if (null? (cdr l))
      (cons #\\0 l)
      l))
(define (cell ab)
  (define-values (c d)
    (apply values
           (map char->mn
                (add0 (string->list
                       (number->string (apply * ab)))))))
  (Menclose
   #:attr* '((notation &quot;updiagonalstrike&quot;))
   (Mtable
    #:attr* '((frame &quot;solid&quot;))
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
   '(((rowspacing &quot;0&quot;)
      (columnspacing &quot;0&quot;)))
   (map (lambda (row)
          (apply Mtr (map Mtd row)))
        lst)))
(define (napier m n)
  (list->table
   (tmap cell (product m n))))"))
   (P "Let us look at a few examples.")
   (CodeB "(P (MB (napier 1--9 1--9))
   &quot;This is a complete table of Napier's bones.&quot;)")
   (P (MB (napier 1--9 1--9))
      "This is a complete table of Napier's bones.")
   (CodeB "(P (MB (napier '(4 6 7 8 5 3 9 9)
               '(9 6 4 3 1)))
   (MB (&amp;= (&amp;c* 46785399 96431)
           (* 46785399 96431))))")
   (P (MB (napier '(4 6 7 8 5 3 9 9)
                  '(9 6 4 3 1)))
      (MB (&= (&c* 46785399 96431)
              (* 46785399 96431))))
   (H3. "The Simply Typed " $lambda "-Calculus")
   (P "The " $lambda "-calculus is an intensional theory of functions. "
      "The typing rules of the (Church-style) simply typed " $lambda
      "-calculus are as follows:"
      (CodeB "(MB (&amp;rull &quot;(Var)&quot;
           (&amp;= (app Γ $x) $tau)
           (G!- (&amp;: $x $tau))))")
      (MB (&rull "(Var)"
                 (&= (app Γ $x) $tau)
                 (G!- (&: $x $tau))))
      (CodeB "(MB (&amp;rull &quot;(Abs)&quot;
           (G!- (&amp;: $x $tau_1)
                (&amp;: $e $tau_2))
           (G!- (&amp;: (Lam (&amp;: $x $tau_1) $e)
                    (&amp;-> $tau_1 $tau_2)))))")
      (MB (&rull "(Abs)"
                 (G!- (&: $x $tau_1)
                      (&: $e $tau_2))
                 (G!- (&: (Lam (&: $x $tau_1) $e)
                          (&-> $tau_1 $tau_2)))))
      (CodeB "(MB (&amp;rull &quot;(App)&quot;
           (G!- (&amp;: $e_1 (&amp;-> $tau_1 $tau_2)))
           (G!- (&amp;: $e_2 $tau_1))
           (G!- (&amp;: (App $e_1 $e_2) $tau_2))))")
      (MB (&rull "(App)"
                 (G!- (&: $e_1 (&-> $tau_1 $tau_2)))
                 (G!- (&: $e_2 $tau_1))
                 (G!- (&: (App $e_1 $e_2) $tau_2))))
      "where"
      (CodeB "(define $!- (Mo &quot;&amp;vdash;&quot;))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &amp;cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define (Lam x t)
  (: $lambda x $. t))
(define App (&amp;split 2))
(define (&amp;rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle &quot;true&quot;))
       (apply (&amp;split n) j*) (car j1))))
(define (&amp;rull label . x*)
  (: (apply &amp;rule x*) label))")
      "Parts of this code are a bit awkward, "
      "which shows that SMathML still has room for improvement.")
   (H3. "Natural Deduction (for Intuitionistic Propositional Logic)")
   (P "Here we present an EDSL built on SMathML "
      "that checks a proof and then generates the corresponding natural deduction tree. "
      "This example would probably be rather hard to implement in other typesetting systems."
      (CodeB "(define (type-of var env)
  (cond ((assoc var env) => cdr)
        (else (error 'type-of &quot;unknown variable ~s&quot; var))))
(define (extend-env var type env)
  (cons (cons var type) env))
(define (reify t)
  (define (reify t)
    (match t
      ((-> ,t1 ,t2) (&amp;impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (&amp;conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (&amp;disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (define (@reify t)
    (match t
      ((-> ,t1 ,t2) (@impl (@reify t1) (@reify t2)))
      ((conj ,t1 ,t2) (@conj (@reify t1) (@reify t2)))
      ((disj ,t1 ,t2) (@disj (@reify t1) (@reify t2)))
      (bot $bottom)
      (top $top)
      (,else t)))
  (reify t))
(struct result (t c) #:transparent)
(define (VAR u)
  (lambda (env)
    (define t (type-of u env))
    (result t (assume u (&amp;true (reify t))))))
(define (CONS a b)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match-define (result tb cb) (b env))
    (define type `(conj ,ta ,tb))
    (result type
            (&amp;rull $conjI ca cb
                   (&amp;true (reify type))))))
(define (CAR a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((conj ,t1 ,t2)
       (result t1 (&amp;rull $conjE1 ca
                         (&amp;true (reify t1))))))))
(define (CDR a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((conj ,t1 ,t2)
       (result t2 (&amp;rull $conjE2 ca
                         (&amp;true (reify t2))))))))
(define (LAM u t body)
  (lambda (env)
    (match-define (result tbody cbody)
      (body (extend-env u t env)))
    (define type `(-> ,t ,tbody))
    (result type
            (&amp;rull (&amp;implI u) cbody
                   (&amp;true (reify type))))))
(define (APP a b)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match-define (result tb cb) (b env))
    (match ta
      ((-> ,t1 ,t2)
       (unless (equal? t1 tb)
         (error 'APP &quot;type mismatch&quot;))
       (result t2 (&amp;rull $implE ca cb
                         (&amp;true (reify t2))))))))
(define (INL tb a)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (define type `(disj ,ta ,tb))
    (result type (&amp;rull $disjI1 ca
                        (&amp;true (reify type))))))
(define (INR ta b)
  (lambda (env)
    (match-define (result tb cb) (b env))
    (define type `(disj ,ta ,tb))
    (result type (&amp;rull $disjI2 cb
                        (&amp;true (reify type))))))
(define (CASE a u1 b1 u2 b2)
  (lambda (env)
    (match-define (result ta ca) (a env))
    (match ta
      ((disj ,t1 ,t2)
       (match-define (result tb1 cb1)
         (b1 (extend-env u1 t1 env)))
       (match-define (result tb2 cb2)
         (b2 (extend-env u2 t2 env)))
       (unless (equal? tb1 tb2)
         (error 'CASE &quot;branch type mismatch&quot;))
       (result tb1 (&amp;rull (&amp;disjE u1 u2) ca cb1 cb2
                          (&amp;true (reify tb1))))))))
(define (ND proof)
  (result-c (proof '())))")
      "Here we omit the minor code, "
      "which only concerns constructing the graphics for rendering.")
   (P "Now let us look at some examples."
      (CodeB "(MB (ND (LAM $u `(-> (disj ,$A ,$B) ,$C)
             (CONS (LAM $w $A
                        (APP (VAR $u)
                             (INL $B (VAR $w))))
                   (LAM $x $B
                        (APP (VAR $u)
                             (INR $A (VAR $x))))))))")
      (MB (ND (LAM $u `(-> (disj ,$A ,$B) ,$C)
                   (CONS (LAM $w $A
                              (APP (VAR $u)
                                   (INL $B (VAR $w))))
                         (LAM $x $B
                              (APP (VAR $u)
                                   (INR $A (VAR $x))))))))
      (CodeB "(MB (ND (LAM $u `(conj (-> ,$A ,$C) (-> ,$B ,$C))
             (LAM $w `(disj ,$A ,$B)
                  (CASE (VAR $w)
                        $x (APP (CAR (VAR $u)) (VAR $x))
                        $y (APP (CDR (VAR $u)) (VAR $y)))))))")
      (MB (ND (LAM $u `(conj (-> ,$A ,$C) (-> ,$B ,$C))
                   (LAM $w `(disj ,$A ,$B)
                        (CASE (VAR $w)
                              $x (APP (CAR (VAR $u)) (VAR $x))
                              $y (APP (CDR (VAR $u)) (VAR $y)))))))
      (CodeB "(let ((Neg (lambda (A) `(-> ,A bot))))
  (MB (ND (LAM $u (Neg `(disj ,$A ,(Neg $A)))
               (APP (LAM $v (Neg $A)
                         (APP (VAR $u)
                              (INR $A (VAR $v))))
                    (LAM $w $A
                         (APP (VAR $u)
                              (INL (Neg $A) (VAR $w)))))))))")
      (let ((Neg (lambda (A) `(-> ,A bot))))
        (MB (ND (LAM $u (Neg `(disj ,$A ,(Neg $A)))
                     (APP (LAM $v (Neg $A)
                               (APP (VAR $u)
                                    (INR $A (VAR $v))))
                          (LAM $w $A
                               (APP (VAR $u)
                                    (INL (Neg $A) (VAR $w)))))))))
      "This is a proof of the so-called intuitionistic law of excluded middle.")
   (H2. "Comparison with Other Tools")
   (P "The items chosen for comparison here overlap with SMathML "
      "either in functionality or in design decisions. "
      "I had not heard of many of them before designing and implementing SMathML; "
      "I only learned of them recently, with the help of LLMs.")
   (H3. "Oleg Kiselyov's SXML")
   (P "SXML is not just SXML itself; it also comprises a family of tools "
      "for processing SXML, including SSAX, SXPath and SXSLT. "
      "The names SXML, SSAX, SXPath and SXSLT "
      "suggest their connection with XML, SAX, XPath and XSLT. "
      "This does not mean, however, that these S-prefixed tools work merely at the level of syntax, "
      "that is, that they merely translate the conventional syntax "
      "into the parenthesized syntax of Lisp-family languages.")

   (H3. "DSSSL")
   (P "DSSSL is similar in function to XSLT, but was originally designed for SGML, "
      "though it later came to support XML as well. "
      "What is most distinctive about DSSSL is that it is based on Scheme. "

      )
   (H3. "LAML")
   (P "LAML and SMathML share many ideas, "
      "yet they also differ in many details. "
      "For example, LAML lets symbols serve as attribute names, "
      "with the strings that follow them serving as attribute values. "
      "This (concrete) syntax might also be adopted "
      "in ports of SMathML to other Scheme dialects and implementations. "
      "LAML also chooses to insert implicit spaces between pieces of content. "
      "These differences open a wide gap between LAML's surface syntax "
      "and the underlying syntax tree, "
      "which I personally do not like. "
      )
   (H3. "(Scheme) Scribe")
   (H3. "Skribe")
   (H3. "Skribilo")
   (H3. "Beautiful Report Language")
   (H3. "Scribble")
   (H3. "TeXmacs")
   (H3. "LaTeX")
   (P "LaTeX is the de facto standard for mathematical documents. "
      "Almost all mathematics journals and preprint servers require submissions in LaTeX, "
      "although in academic mathematics Word is often accepted as an alternative. "

      )
   (H3. "Typst")
   (H3. "SATysFi")
   (H3. "MarkDown")
   (P "MarkDown may originally have been meant only as an easy way to write simple HTML. "
      "Because of its popularity, however, "
      "today's many MarkDown-based schemes carry far too many requirements "
      "that were never originally considered. "
      "Given MarkDown's inherent shortcomings in syntax design and other respects, "
      "these extensions strike me personally as exceptionally ugly. "
      )
   ))