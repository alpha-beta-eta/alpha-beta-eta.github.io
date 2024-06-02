#lang racket
(provide linear_logic_notes.html)
(require SMathML)
(define $empty $d*)
(define $! (Mo "!"))
(define (&! A) (: $! A))
(define Γ $Gamma:normal)
(define $\; (Mo ";"))
(define $unit:disj
  (Mi "0" #:attr* '((mathvariant "bold"))))
(define $o+ (Mo "&oplus;"))
(define $equiv (Mo "&equiv;"))
(define $≡ $equiv)
(define (NU var exp)
  (: (@ $nu var) exp))
(define $\| (Mo "|"))
(define (para-compose P Q)
  (@\| P Q))
(define (subst term exp var)
  (: term (cur0 (&/ exp var))))
(define $:: (Mo "::"))
(define $& (Mo "&"))
(define $unit (Mi "1" #:attr* '((mathvariant "bold"))))
(define &cm*
  (case-lambda
    (() $)
    ((a) a)
    ((a b . c*) (apply &cm* (&cm a b) c*))))
(define $->E (_ $-> $E))
(define $->R (_ $-> $R))
(define $\|- (Mo "&vdash;"))
(define (&\|- . x*)
  (let-values (((y* z*) (split-at-right x* 1)))
    ;use &cm* instead of &cm, tricky!
    (: (apply &cm* y*) $\|- (car z*))))
(define $UnderBrace (Mo "&UnderBrace;"))
(define UnderBrace
  (case-lambda
    ((x) (Munder x $UnderBrace))
    ((x y) (Munder (UnderBrace x) y))))
(define Δ $Delta:normal)
(define Δ^ (&prime Δ))
(define Δ^^ (&Prime Δ))
(define $lolli (Mo "&multimap;"))
(define $-o $lolli)
(define $o* (Mo "&otimes;"))
(define $o*:id (Mi "&otimes;"))
(define $rule:⊗R (: $o*:id $R))
(define $rule:⊗L (: $o*:id $L))
(define $dumb (Mi "-"))
(define $eph (Mi "eph" #:attr* '((mathvariant "italic"))))
(define (&eph prop)
  (: prop (&space 2) $eph))
(define $pers (Mi "pers" #:attr* '((mathvariant "italic"))))
(define (&pers prop)
  (: prop (&space 2) $pers))
(define (Menclose #:attr* [attr* '()] . xml*)
  `(menclose ,attr* . ,xml*))
(define (Menclose:updiagonalstrike #:attr* [attr* '()] . xml*)
  `(menclose ,(attr*-set attr* 'notation "updiagonalstrike") . ,xml*))
(define UpStrike Menclose:updiagonalstrike)
(define const:a $a:sans-serif)
(define const:b $b:sans-serif)
(define const:c $c:sans-serif)
(define const:d $d:sans-serif)
(define const:e $e:sans-serif)
(define (Const str)
  (Mi str #:attr* '((mathvariant "sans-serif"))))
(define $copy (Const "copy"))
(define $valid (Const "valid"))
(define $id (Const "id"))
(define (&id A) (_ $id A))
(define $cut (Const "cut"))
(define (&cut A) (_ $cut A))
(define $edge (Const "edge"))
(define (&edge x y)
  (appl $edge x y))
(define $node (Const "node"))
(define (&node x)
  (app $node x))
(define $path (Const "path"))
(define (&path x y)
  (appl $path x y))
(define $sym (Const "sym"))
(define $refl (Const "refl"))
(define $trans (Const "trans"))
(define (&rulel #:label [label #f] . j*)
  (if label
      (: (apply &rule j*) label)
      (apply &rule j*)))
(define $e:rule (Const "e"))
;do not compose well, need refactoring
(define (rule:sym x y)
  (&rulel
   #:label $sym
   (&edge x y)
   (&edge y x)))
(define (rule:e x y)
  (&rulel
   #:label $e:rule
   (&edge x y)
   (&path x y)))
(define (rule:trans x y z)
  (&rulel
   #:label $trans
   (&path x y) (&path y z)
   (&path x z)))
(define (make-proof-instance proof conclusion)
  (vector 'proof-instance proof conclusion))
(define (proof-instance-proof proof-instance)
  (vector-ref proof-instance 1))
(define (proof-instance-conclusion proof-instance)
  (vector-ref proof-instance 2))
(define (instantiate-prop prop)
  (make-proof-instance prop prop))
(define (instantiate-fake fake)
  (make-proof-instance (Mover fake (Mi "??")) fake))
(define (make-rule-instance premise* label conclusion)
  (vector 'rule-instance premise* label conclusion))
(define (rule-instance-premise* rule-instance)
  (vector-ref rule-instance 1))
(define (rule-instance-label rule-instance)
  (vector-ref rule-instance 2))
(define (rule-instance-conclusion rule-instance)
  (vector-ref rule-instance 3))
(define (make-rule:e-instance x y)
  (make-rule-instance
   (list (&edge x y)) ;premise*
   $e:rule ;label
   (&path x y) ;conclusion
   ))
(define (make-rule:sym-instance x y)
  (make-rule-instance
   (list (&edge x y))
   $sym
   (&edge y x)))
(define (make-rule:trans-instance x y z)
  (make-rule-instance
   (list (&path x y) (&path y z))
   $trans
   (&path x z)))
(define (direct-instantiate rule-instance)
  (let ((premise* (rule-instance-premise* rule-instance))
        (label (rule-instance-label rule-instance))
        (conclusion (rule-instance-conclusion rule-instance)))
    (make-proof-instance
     (keyword-apply
      &rulel
      '(#:label) (list label)
      (if (null? premise*)
          (list $ conclusion) ;tricky!
          (append premise* (list conclusion))))
     conclusion)))
(define (check-validity proof-instance* rule-instance)
  (let ((conclusion* (map proof-instance-conclusion proof-instance*))
        (premise* (rule-instance-premise* rule-instance)))
    (unless (equal? conclusion* premise*);?
      (error 'compose-proof-instance*
             "invalid proof composition:\n~s\n~s"
             conclusion* premise*))))
(define (compose-proof-instance* #:check? [check? #t] rule-instance . proof-instance*)
  (when check?
    (check-validity proof-instance* rule-instance))
  (let ((proof* (map proof-instance-proof proof-instance*))
        (label (rule-instance-label rule-instance))
        (conclusion (rule-instance-conclusion rule-instance)))
    (make-proof-instance
     (keyword-apply
      &rulel
      '(#:label) (list label)
      (if (null? proof*)
          (list $ conclusion) ;tricky!
          (append proof* (list conclusion))))
     conclusion)))
(define (lookup label env)
  (cond ((assq label env) => cdr)
        (else (error 'render-proof-tree
                     "unknown rule ~s" label))))
(define (render-proof-tree env tree #:check? [check? #t])
  (define (interp-rule rule)
    (match rule
      ((,label . ,arg*)
       (apply (lookup label env) arg*))))
  (define (interp tree)
    (match tree
      ((<= ,rule . ,proof*)
       (define r (interp-rule rule))
       (define p* (map interp proof*))
       (keyword-apply
        compose-proof-instance*
        '(#:check?) (list check?)
        r p*))
      ((prop ,prop)
       (instantiate-prop prop))
      ((fake ,fake)
       (instantiate-fake fake))
      (,rule
       (direct-instantiate
        (interp-rule rule)))))
  (proof-instance-proof (interp tree)))
(define $succ $s:sans-serif)
(define (&succ n)
  (app $succ n))
(define $even (Const "even"))
(define $odd (Const "odd"))
(define (&even n)
  (app $even n))
(define (&odd n)
  (app $odd n))
(define $zero
  (Mn "0" #:attr* '((mathvariant "sans-serif"))))
(define (make-even:0-instance)
  (make-rule-instance
   (list)
   #f
   (&even $zero)))
(define (make-evenodd-instance x)
  (make-rule-instance
   (list (&even x))
   #f
   (&odd (&succ x))))
(define (make-oddeven-instance x)
  (make-rule-instance
   (list (&odd x))
   #f
   (&even (&succ x))))
(define env:nat
  `((even:0 . ,make-even:0-instance)
    (evenodd . ,make-evenodd-instance)
    (oddeven . ,make-oddeven-instance)))
(define $nickel (Const "n"))
(define $dime (Const "d"))
(define $quarter (Const "q"))
(define $opportunity (Const "opportunity"))
(define $knocks (Const "knocks"))
(define (&knocks x)
  (app $knocks x))
(define $at (Const "at"))
(define (&at x)
  (app $at x))
(define $~> (Mo "&rarrw;"))
(define-infix*
  (&~> $~>)
  (&-o $-o)
  (&lolli $lolli)
  (&o* $o*)
  (&->E $->E)
  (&->R $->R)
  (&& $&)
  (&:: $::)
  (&\| $\|)
  (&equiv $equiv)
  (&≡ $≡)
  (&o+ $o+)
  (&\; $\;)
  
  )
(define $step (Const "step"))
(define (make-step-instance x y)
  (make-rule-instance
   (list (&at x) (&edge x y))
   $step
   (&at y)))
(define env:graph
  `((e . ,make-rule:e-instance)
    (sym . ,make-rule:sym-instance)
    (trans . ,make-rule:trans-instance)
    (step . ,make-step-instance)))
(define (--> x0 x1 . x*)
  (let iter ((at x1)
             (proof `(step ,x0 ,x1))
             (x* x*))
    (if (null? x*)
        proof
        (let ((x (car x*))
              (x* (cdr x*)))
          (iter x
                `(<= (step ,at ,x)
                     ,proof
                     (prop ,(&edge at x)))
                x*)))))
(define make-id-instance
  (lambda (prop)
    (make-rule-instance
     (list) (&id prop)
     (&\|- prop prop))))
(define make-cut-instance
  (lambda (Δ A Δ^ C)
    (if (equal? Δ $)
        (make-rule-instance
         (list (&\|- $d* A)
               (&\|- Δ^ A C))
         (&cut A)
         (&\|- Δ^ C))
        (make-rule-instance
         (list (&\|- Δ A)
               (&\|- Δ^ A C))
         (&cut A)
         (&\|- Δ Δ^ C)))))
(define make-⊗R-instance
  (lambda (Δ A Δ^ B)
    (if (equal? Δ $)
        (make-rule-instance
         (list (&\|- $d* A)
               (&\|- Δ^ B))
         $rule:⊗R
         (&\|- Δ^ (&o* A B)))
        (if (equal? Δ^ $)
            (make-rule-instance
             (list (&\|- Δ A)
                   (&\|- $d* B))
             $rule:⊗R
             (&\|- Δ (&o* A B)))
            (make-rule-instance
             (list (&\|- Δ A)
                   (&\|- Δ^ B))
             $rule:⊗R
             (&\|- Δ Δ^ (&o* $A $B)))))))
(define make-⊗L-instance
  (lambda (Δ A B C)
    (if (equal? Δ $)
        (make-rule-instance
         (list (&\|- A B C))
         $rule:⊗L
         (&\|- (&o* A B) C))
        (make-rule-instance
         (list (&\|- Δ A B C))
         $rule:⊗L
         (&\|- Δ (&o* A B) C)))))
(define $-o:id (Mi "&multimap;"))
(define $rule:-oL (: $-o:id $L))
(define make--oL-instance
  (lambda (Δ A Δ^ B C)
    (if (equal? Δ^ $)
        (make-rule-instance
         (list (&\|- Δ A)
               (&\|- B C))
         $rule:-oL
         (&\|- Δ (&-o A B) C))
        (make-rule-instance
         (list (&\|- Δ A)
               (&\|- Δ^ B C))
         $rule:-oL
         (&\|- Δ Δ^ (&-o A B) C)))))
(define $rule:-oR (: $-o:id $R))
(define make--oR-instance
  (lambda (Δ A B)
    (make-rule-instance
     (list (&\|- Δ A B))
     $rule:-oR
     (&\|- Δ (&-o A B)))))
(define $rule:unitR
  (: $unit $R))
(define make-unitR-instance
  (lambda ()
    (make-rule-instance
     (list $)
     $rule:unitR
     (&\|- $d* $unit))))
(define $rule:unitL
  (: $unit $L))
(define make-unitL-instance
  (lambda (Δ C)
    (if (equal? Δ $)
        (make-rule-instance
         (list (&\|- $d* C))
         $rule:unitL
         (&\|- $unit C))
        (make-rule-instance
         (list (&\|- Δ C))
         $rule:unitL
         (&\|- Δ $unit C)))))
(define $rule:⊗L1??
  (: $o*:id $L_1 (Mi "??")))
(define $rule:⊗L2??
  (: $o*:id $L_2 (Mi "??")))
(define make-⊗L1??-instance
  (lambda (Δ A B C)
    (if (equal? Δ $)
        (make-rule-instance
         (list (&\|- A C))
         $rule:⊗L1??
         (&\|- (&o* A B) C))
        (make-rule-instance
         (list (&\|- Δ A C))
         $rule:⊗L1??
         (&\|- Δ (&o* A B) C)))))
(define make-⊗L2??-instance
  (lambda (Δ A B C)
    (if (equal? Δ $)
        (make-rule-instance
         (list (&\|- B C))
         $rule:⊗L2??
         (&\|- (&o* A B) C))
        (make-rule-instance
         (list (&\|- Δ B C))
         $rule:⊗L2??
         (&\|- Δ (&o* A B) C)))))
(define $weaken (Const "weaken"))
(define make-weaken-instance
  (lambda (Δ A C)
    (make-rule-instance
     (list (&\|- Δ C))
     $weaken
     (&\|- Δ A C))))
(define $&:id (Mi "&"))
(define $rule:&R (: $&:id $R))
(define (rule:&R Δ A B)
  (make-rule-instance
   (list (&\|- Δ A) (&\|- Δ B))
   $rule:&R
   (&\|- Δ (&& A B))))
(define $rule:&L1 (: $&:id $L_1))
(define (rule:&L1 Δ A B C)
  (if (equal? Δ $)
      (make-rule-instance
       (list (&\|- A C))
       $rule:&L1
       (&\|- (&& A B) C))
      (make-rule-instance
       (list (&\|- Δ A C))
       $rule:&L1
       (&\|- Δ (&& A B) C))))
(define $rule:&L2 (: $&:id $L_2))
(define (rule:&L2 Δ A B C)
  (if (equal? Δ $)
      (make-rule-instance
       (list (&\|- B C))
       $rule:&L2
       (&\|- (&& A B) C))
      (make-rule-instance
       (list (&\|- Δ B C))
       $rule:&L2
       (&\|- Δ (&& A B) C))))
(define $rule:⊤R (: $top $R))
(define (rule:⊤R Δ)
  (make-rule-instance
   (list)
   $rule:⊤R
   (&\|- Δ $top)))
(define $rule:⊤L (: $top $L))
(define $oplus:id (Mi "&oplus;"))
(define $rule:⊕R1 (: $oplus:id $R_1))
(define $rule:⊕R2 (: $oplus:id $R_2))
(define $rule:⊕L (: $oplus:id $L))
(define (rule:⊕R1 Δ A B)
  (make-rule-instance
   (list (&\|- Δ A))
   $rule:⊕R1
   (&\|- Δ (&oplus A B))))
(define (rule:⊕R2 Δ A B)
  (make-rule-instance
   (list (&\|- Δ B))
   $rule:⊕R2
   (&\|- Δ (&oplus A B))))
(define (rule:⊕L Δ A B C)
  (make-rule-instance
   (list (&\|- Δ A C)
         (&\|- Δ B C))
   $rule:⊕L
   (&\|- Δ (&oplus A B) C)))
(define $rule:0L
  (: $unit:disj $L))
(define $rule:0R
  (: $unit:disj $R))
(define (rule:0L Δ C)
  (make-rule-instance
   (list)
   $rule:0L
   (&\|- Δ $unit:disj C)))
(define $!:id (Mi "!"))
(define $rule:!L (: $!:id $L))
(define $rule:!R (: $!:id $R))
(define env:linear
  `((id . ,make-id-instance)
    (cut . ,make-cut-instance)
    (⊗R . ,make-⊗R-instance)
    (⊗L . ,make-⊗L-instance)
    (-oL . ,make--oL-instance)
    (-oR . ,make--oR-instance)
    (1R . ,make-unitR-instance)
    (1L . ,make-unitL-instance)
    (⊗L1?? . ,make-⊗L1??-instance)
    (⊗L2?? . ,make-⊗L2??-instance)
    (&R . ,rule:&R)
    (&L1 . ,rule:&L1)
    (&L2 . ,rule:&L2)
    (⊤R . ,rule:⊤R)
    ;though, linear logic does not include the weakening rule!
    (weaken . ,make-weaken-instance)
    (⊕L . ,rule:⊕L)
    (⊕R1 . ,rule:⊕R1)
    (⊕R2 . ,rule:⊕R2)
    (0L . ,rule:0L)
    
    ))
(define-@lized-op*
  (@o* &o*)
  (@\| &\|)
  
  )
(define $fn (Const "fn"))
(define (&fn P) (app $fn P))
(define linear_logic_notes.html
  (TnTmPrelude
   #:title "线性逻辑笔记"
   #:css "styles.css"
   (H1. "线性逻辑笔记")
   (P "学习"
      (A "CMU 15-816"
         #:attr* '((href "https://www.cs.cmu.edu/~fp/courses/15816-s12/schedule.html")))
      "的一些翻译和笔记.")
   (H2. "演绎推理")
   (H3. "例子: 对于图进行推理")
   (P "作为第一个例子, 我们考虑图 (graph). 我们将结点 (node) (顶点, vertex) 表示为"
      (Em "常量(constant)") " (" const:a ", " const:b
      ", ...) , 而边 (edge) 表示为一个二元" (Em "谓词(predicate)") $edge
      ", 其将相互连接的结点联系起来.")
   
   (P "以上的示例图可以被表示为" (Em "命题(proposition)") " (原文的命题为复数形式)"
      (MB (&cm (&edge const:a const:b)
               (&edge const:b const:c)
               (&edge const:a const:c)
               (&edge const:a const:d)))
      "读者可能会立即注意到一点不太匹配的地方, 即图片中的边似乎是无向的 (undirected), "
      "而对于边的表示并非对称 (例如, " (&edge const:b const:a)
      "就并不存在). 我们可以修复这点不足之处, 通过提供一条"
      (Em "推理规则(rule of inference)") "以要求" $edge "关系是对称的 (symmetric)."
      (MB (&rulel
           #:label $sym
           (&edge $x $y)
           (&edge $y $x)))
      "我们可以应用这条推理规则于事实" (&edge const:a const:b) "以推导出 (deduce) "
      (&edge const:b const:a) ". 在这个应用里, 我们实例化 (instantiate) 了"
      (Em "模式变量(schematic variable)") $x "和" $y "以" const:a "和"
      const:b ". 模式变量会以斜体排版, 以将其与常量区分. 水平线之上的命题被称为规则的"
      (Em "前提(premise)") ", 而水平线之下的命题则被称为"
      (Em "结论(conclusion)") ". 这条作为例子的规则只有一个前提和一个结论. "
      $sym "是这条规则的" (Em "名字(name)") "或者说" (Em "标签(label)")
      ". 我们经常省略规则的名字, 如无引用规则的特别需要的话.")
   (P "根据这条单一规则和描述初始图的事实, 我们现在可以推导出以下额外的事实:"
      (MB (&cm (&edge const:b const:a)
               (&edge const:c const:b)
               (&edge const:c const:a)
               (&edge const:d const:a)))
      "此时此刻我们尚不能在图与其逻辑表示之间来回切换, "
      "因为一个孤立的结点不会出现在边关系里. "
      "因此, 我们需要第二个谓词" $node
      ", 其对于图中的每个结点成立."
      (MB (&cm (&node const:a)
               (&node const:b)
               (&node const:c)
               (&node const:d))))
   (P "既已设计了图的一种逻辑表示, 现在我们定义图上的一个关系. "
      "如果图中存在一条从" $x "到" $y "的路径 (path), 我们记"
      (&path $x $y) ". 对于图而言常见的情况是, "
      "我们不想考虑平凡的从一个结点到自身的零长度路径. "
      "如果我们的确要考虑的话, 那将会是以下规则 "
      "(写在方括号里以指示这仅是假想性的):"
      (MB (brac
           (&rulel
            #:label $refl
            (&node $x)
            (&path $x $x))))
      "如果我们省略了这条规则的前提, 那么这个规则是成问题的, "
      "因为其可以用于甚至并非图的结点的对象" $x
      ", 导致毫无意义 (nonsensical) 的结论.")
   (P "现在以下的两条规则定义了路径的概念. 第一条"
      $e:rule "是说每条边都代表了一条合法 (valid) 的路径, "
      "第二条" $trans "是说路径可以被复合 (compose), 使得"
      $path "成为传递关系."
      (MB ((&split 16)
           (rule:e $x $y)
           (rule:trans $x $y $z)))
      "根据对于我们示例图的表示, 现在可以提供以下存在从"
      const:c "到" const:d "的路径的证明:"
      (MB (render-proof-tree
           env:graph
           `(<= (trans ,const:c ,const:a ,const:d)
                (<= (e ,const:c ,const:a)
                    (sym ,const:a ,const:c))
                (e ,const:a ,const:d))))
      "我们可以检视这个证明, 然后发现其携带了一些信息. "
      "它不仅仅是为了说服我们存在一条从" const:c "到"
      const:d "的路径, 而是告诉了我们路径是什么. "
      "这条路径从" const:c "出发走到" const:a
      ", 然后从" const:a "走到" const:d
      ". 这是一个关于证明中的"
      (Em "构造性内容(constructive content)")
      "的例子, 而我们之后将会看到诸多其他的例子. "
      "对于目前我们已有的系统而言, "
      "从一般角度来说, 我们的确可以从证明中读出路径, "
      "并且如果我们心中有一条路径, "
      "则总是可以构造一个证明. "
      "但是, 以我们选择的规则而言, "
      "一些路径并不对应于唯一的证明. "
      "[译注: 这最后一句话里的路径 (path) 指的是实际所走的路线, "
      "例如对于结点" $x "和" $y ", 如果我们拥有对于"
      (&path $x $y) "的一个证明, 那么说明存在一条实际的可以从"
      $x "走到" $y "的路. 但是, 对于这条实际的路线而言, "
      "也可以存在不同的证明. 读者需要理解的是, "
      (&path $x $y) "存在证明只是说明至少存在一条路线, "
      "但是可以有各种不同的路线, 而且甚至不同的证明"
      "可以对应于相同的路线, 或者说相同的构造性内容. "
      "原文没有特别区分路径和实际所走的路线. "
      "而且, 根据后文, 其实每种路线都可以有无限多不同的证明.]")
   (P "实际上, 歧义的来源不止一处. "
      "[译注: 这里的歧义指同一路径的不同证明可以拥有相同的实际路线.] "
      "一方面是我们可以从" (&edge const:c const:a)
      "回到" (&edge const:a const:c) "然后再回到"
      (&edge const:c const:a) ", 如此反复下去, "
      "可以产生无限多的对于" (&edge const:c const:a)
      "的证明. 另一方面, 具有多于三个结点的路径"
      "可以被分解为不同的子路径, 由此以不同的方式使用传递性. "
      "不同的证明可以拥有相同的构造性内容并没有使得其解释不合理, "
      "但是我们应该意识到这件事情的存在.")
   (P "既然我们排除了自反性, 在何种条件下我们仍然可以" (Em "证明")
      (&path $x $x) "呢? 因为我们考虑的是无向图, 存在一条从"
      $x "到" $x "的路径恰当" $x "至少存在一个邻居 (neighbor), "
      "正如以下证明所示:"
      (MB (render-proof-tree
           env:graph
           `(<= (trans ,$x ,$y ,$x)
                (e ,$x ,$y)
                (<= (e ,$y ,$x)
                    (sym ,$x ,$y)))))
      "这个证明存在诸多有趣的方面. 例如, 它不依赖于"
      $x "和" $y "到底是什么. 换言之, 其对于"
      $x "和" $y "是" (Em "模式性的(schematic)")
      ". 另一值得注意的方面在于其用了" (&edge $x $y)
      "两次, 而这在直觉上是成立的: 离开" $x
      "然后回到" $x "的一般方法是先去到任意相邻的"
      $y ", 然后立即返回" $x ", 复用相同的边. "
      "我们可以将以上的推导 (deduction) 总结为一条单独的"
      (Em "导出推理规则(derived rule of inference)") ":"
      (MB (&rule (&edge $x $y)
                 (&path $x $x)))
      "这条推理规则是合理的 (justified), "
      "因为我们可以将其任意的特定实例替换以"
      "我们上面给出的模式性证明的一个实例. "
      "我们将在之后的讲座里看到, "
      "导出推理规则在逻辑学中扮演着非常重要的角色.")
   (H3. "例子: 自然数")
   (P "作为第二个例子, 我们考虑自然数"
      (&cm $0 $1 $2 $..h)
      ". 一种构造自然数的便捷方法是通过迭代应用后继函数"
      $succ "于" $zero ", 记作"
      (MB (&cm $zero
               (&succ $zero)
               (&succ (&succ $zero))
               $..h))
      "我们将" $succ "称为一个" (Em "构造子(constructor)")
      ". 现在我们可以通过以下三条规则定义偶数和奇数."
      (MB ((&split 16)
           (&rule $ (&even $zero))
           (&rule (&even $x)
                  (&odd (&succ $x)))
           (&rule (&odd $x)
                  (&even (&succ $x)))))
      "作为导出推理规则的例子, 我们可以将左边的证明总结为右边的规则:"
      (MB ((&split 16)
           (render-proof-tree
            env:nat
            `(<= (oddeven ,(&succ $x))
                 (evenodd ,$x)))
           (&rule (&even $x)
                  (&even (&succ (&succ $x))))))
      "这些例子里的证明的结构都不是特别有趣, "
      "因为数 (number, 这里是动词) 数字"
      $n "为奇还是偶的证明不过就是遵循数字" $n "的结构罢了.")
   (H3. "例子: 硬币交换")
   (P "到目前为止, 演绎推理总是在积累知识, "
      "因为我们已经所意识到为真的命题仍然保持为真. "
      "线性逻辑起源于一个简单的观察:"
      (Blockquote
       (Em "Truth is ephemeral."))
      "例如, 在进行这个讲座时"
      (Q (Em "Frank is holding a piece of chalk"))
      "为真, 而现在(很可能)不是了. 因此, 真性随着时间而变化, "
      "而这种现象以" (Em "时态逻辑(temporal logic)")
      "研究. 在" (Em "线性逻辑(linear logic)")
      "中, 我们关心的则是随着" (Em "状态改变(change of state)")
      "的真性改变 (change of truth). "
      "我们以简单的方式对此进行模拟: "
      "当一条推理规则被应用时, 我们" (Em "消费(consume)")
      "了用作前提的命题, 而" (Em "产生(produce)")
      "了结论中的命题, 因此引发了状态的总体性变化.")
   (P "作为一个例子, 我们考虑值" $5 "分钱的" (Em "nickel")
      ", 值" 10 "分钱的" (Em "dime") ", 值" 25
      "分钱的" (Em "quarter")
      ". 我们拥有如下用于在它们之间进行交换的规则:"
      (MB ((&split 16)
           (&rule $dime $dime $nickel
                  $quarter)
           (&rule $quarter
                  ((&split 8)
                   $dime $dime $nickel))
           (&rule $nickel $nickel
                  $dime)
           (&rule $dime
                  ((&split 8)
                   $nickel $nickel))))
      "第二条和第四条规则是我们第一次看到具有多于一个结论的规则. "
      "现在推理可以改变状态. 例如, 如果我们有三个dime和一个nickel, "
      "那么状态可以写成"
      (MB (&cm $dime $dime $dime $nickel))
      "应用第一条规则, 我们可以将两个dime和一个nickel"
      "转换为一个quarter以得到状态"
      (MB (&cm $dime $quarter))
      "注意到总钱数" 35 "分保持未变, 这是硬币交换的要义. "
      "一种写下这样的推理的方式在于划去被消费的命题而加上产生的命题. "
      "对于以上例子而言, 我们可以写成"
      (MB (&~> (&cm $dime $dime $dime $nickel)
               (&cm (UpStrike $dime) (UpStrike $dime)
                    $dime (UpStrike $nickel) $quarter)))
      "为了理解证明的意义, 考虑如何将三个dime换为一个quarter和一个nickel: "
      "首先, 我们将一个dime换为两个nickel, 然后再将剩下来的两个dime和"
      "其中一个nickel换为一个quarter. 正如以下两次状态转移所示:"
      (MB (&~> (&cm $dime $dime $dime)
               (&cm $dime $dime (UpStrike $dime)
                    $nickel $nickel)
               (&cm (UpStrike $dime) (UpStrike $dime) (UpStrike $dime)
                    (UpStrike $nickel) $quarter $nickel)))
      "使用推理规则的记号, 这个推导显示在左边, 而相应的导出推理规则在右边."
      (MB ((&split 16)
           (&rule $dime $dime $nickel
                  $quarter)
           (&rule $dime
                  ((&split 8) $nickel $nickel))
           (&rule $dime $dime $dime
                  ((&split 8)
                   $quarter $nickel))))
      "[译注: 左边两个证明其实是一个证明的组件, "
      "但是限于MathML的排版能力, "
      "我实在不知道存在什么优雅的方式可以将它们组合起来, "
      "只好暂时作罢.]")
   (P "总结一下: 我们可以改变推理的本质 (very nature), 如果我们"
      (Em "消费") "前提中所用的命题以" (Em "产生")
      "结论中的命题. 这是线性逻辑的基础, 因此我们将其称为"
      (Em "线性推理(linear inference)")
      ". 以下是提醒我们的精辟之语:"
      (Blockquote
       (Em "Linear inference can change the world.")))
   (H3. "例子: 画图 (Graph Drawing)")
   (P "我们继续来看一个牵涉线性推理的稍微复杂一点的例子. "
      "之前我们使用了常规的演绎推理以定义路径的概念. "
      "这次我们想要模拟不提起笔画图. "
      "这与遍历整个图而每条边恰走一次是等价的. "
      "[译注: 这里说的画图其实就是" (Q "一笔画") "问题.] "
      "这种重述暗示了以下的想法: "
      "当我们沿着一条边走的时候, 我们就" (Em "消费")
      "了这条边, 于是我们不能再走一次这条边. "
      "我们也需要追踪我们在哪里, 因此我们引入了新的谓词"
      $at ", 其满足" (&at $x) "为真, 如果我们在结点"
      $x "这个位置上. 然后, 唯一的" (Em "线性")
      "推理规则是"
      (MB (render-proof-tree
           env:graph
           `(step ,$x ,$y)))
      "和前一节一样我们从某个初始状态出发: 对于每条从"
      $x "到" $y "的边都有一个" (&edge $x $y)
      ", 对称规则成立 (因为是无向图), 起始位置表示为" (&at $x_0)
      ". [译注: 这里一条(无向)边只对应于一个" (Q "命题")
      ", 原文的说法其实有点问题. 另外, 对称规则现在也是线性的了, "
      "因为我们需要避免重复走过相同的(无向)边.] "
      "我们可以看到我们每走一步 (通过应用上述的" $step
      "规则), 我们都是消耗了一个事实" (&at $x)
      ", 然后产生了另一个事实" (&at $y)
      ", 因此状态里总是恰好存在一个具有" (&at $dumb)
      "形式的事实. 并且, 每一步我们也消耗了一个"
      (&edge $dumb $dumb) "事实, 因此我们最多能走的步数"
      "和初始图中的边数相等. 当然了, 如果我们位于一个点"
      $x ", 那么可能有许多条出边, 而若我们选择了错误的那条, "
      "可能就不能完成一笔画了, 但是至少在每个点处"
      "我们可以尝试的选择是有限的. "
      "如果我们能够抵达这样一个状态, 即没有形式为"
      (&edge $dumb $dumb) "的事实, 那么我们就成功了, "
      "或者说找到了不提起笔而画完图的方法, "
      "并且最终的位置为" (&at $x_n) ".")
   (P "以下的示例图来源于一首德国童谣, "
      "并且如果从" const:b "或者" const:c
      "出发我们就可以一笔画, 但是如果从"
      const:a ", " const:d ", 或" const:e
      "出发则不行."
      (MB (render-proof-tree
           env:graph
           (--> const:b const:a const:e const:d
                const:c const:b const:d const:a
                const:c)))
      "这是从" const:b "出发的一笔画的证明树."
      (MB (render-proof-tree
           env:graph
           (--> const:c const:d const:e const:a
                const:b const:c const:a const:d
                const:b)))
      "这是从" const:c "出发的一笔画的证明树.")
   
   (H3. "例子: 图遍历")
   
   (H3. "例子: 积木世界")
   (P "接下来我们考虑" (Em "积木世界(blocks world)")
      ", 这是人工智能的历史里一个重要的例子. "
      )
   (H3. "例子: King Richard III")
   
   (H3. "例子: 机会")
   (P "一条常见的谚语如下:"
      (Blockquote
       (Em "Opportunity doesn't knock twice.")
       " &mdash;Anonymous")
      "又一次, 让我们固定一个词汇表:"
      (MB (set-attr*
           (&Table
            ($opportunity "opportunity")
            ((&knocks $x) (: $x "&nbsp;knocks")))
           'columnalign "left"))
      
      )
   (H3. "练习")
   
   (H2. "从规则到命题")
   (H3. "例子: 生成树")
   (H3. "例子: 乞丐")
   (H3. "同时合取")
   (P "线性推理规则可以拥有多个前提和多个结论. "
      "如果我们试着将水平线想成是某种形式的二元联结词 "
      "(实际上那应该会是" (&-o $A $B)
      "), 那么我们需要一种方法将前提打包成单独一个命题, "
      "并且也要将结论打包成单独一个命题. "
      "[译注: 因为前提和结论都有可能不止一个.] "
      "这是" (Em "同时合取(simultaneous conjunction)")
      "或者说" (Em "乘性合取(multiplicative conjunction)")
      (&o* $A $B) "的目的所在了. " (&o* $A $B)
      "为真, 如果" $A "和" $B "在相同的状态里都为真. "
      "因此, 如果我们拥有" (&o* $A $B) ", 我们可以将其替换以"
      $A "和" $B ":"
      (MB (&rule (&eph (&o* $A $B))
                 ((&split 8)
                  (&eph $A) (&eph $B))))
      "另一个方向似乎也是直接的: 我们可以得到"
      (&o* $A $B) ", 如果我们既有" $A "也有" $B ":"
      (MB (&rule (&eph $A) (&eph $B)
                 (&eph (&o* $A $B))))
      "但是这已经造成了问题, 比如说如果我们想要表明"
      (MB (&rule (&eph (&o* $A $B))
                 (&eph (&o* $B $A))))
      "是一条导出推理规则, 那么证明可能会写成以下这样:"
      (MB (&rule*
           (&eph (&o* $A $B))
           ((&split 8)
            (&eph $A) (&eph $B))
           (&eph (&o* $B $A))))
      "一点小的恼人之处在于最后一条规则的前提的顺序是错误的. "
      "然而, 更重要的地方在于, " (&eph (&o* $A $B))
      "在最后一条规则的两个前提的证明里都有出现, "
      "这似乎违背了线性推理的基础: 瞬态事实只能使用一次! "
      "[译注: 这段话其实有点令我迷惑, "
      "因为使用" (&eph (&o* $A $B)) "其实产生了两个新的命题, "
      "这并没有什么不对的地方. 当然了, 这可能是为了后文引入"
      "管理推理 (其中包括推理中的命题) 的相继式演算作铺垫.]")
   (P "或许人们可以试图提供什么标准, "
      "用以判断类似于上面的紧凑结构的确是合法的证明. "
      "但是, 这样的标准通常相当复杂, "
      "而且与线性逻辑不完全契合 (don't scale well to all of linear logic). "
      "一种看起来更有前途也更加一般的替代选择是急剧地 (drastically) "
      "改变我们的推理记号, 那正是我们下一节要做的事情. "
      "[译注: 之前的推理都是一种原始自然演绎式的, "
      "其规则有多个结论的情况和自然演绎的树结构不太契合. "
      "当然了, 不是说没有线性逻辑的自然演绎, "
      "但是线性逻辑的自然演绎还是要用到相继式的 (不过那不叫相继式演算).]")
   (H3. "资源和目标")
   (P "现在我们转移到一种记号上来, 其主要的判断 "
      "[译注: 其实就是相继式] 显式追踪了"
      "我们在推理过程中所有使用了的瞬态命题. 我们写下"
      (MB (&\|-
           (UnderBrace (&cm (&eph $A_1) $..h (&eph $A_n)) Δ)
           (&eph $C)))
      "将" Δ "的部分视为" (Em "资源(resources)") "而" $C
      "当成我们要达成的目标 (goal). "
      "为了证明这个东西, 我们需要在我们可以达成" $C
      "的证明中使用" Δ "的所有资源" (Em "恰好一次(exactly once)")
      ". 这是来源于Gentzen的" (Em "相继式演算(sequent calculus)")
      "的" (Em "相继式(sequent)") "的一个例子, "
      "[Gen35]这篇开创性论文标志着证明论作为研究主题的开始. "
      "然而, Gentzen的论文具有允许我们复制或者擦除假设的结构规则, "
      "但是在这里被有意省略了. [译注: 结构规则从指称或者语义角度来看是显然的, "
      "然而从证明论的角度来看反而是相继式演算里最重要的部分. "
      "线性逻辑实际上可以说始于这样的观察.]")
   (P "以相继式的记号, 我们现在可以写下"
      (MB (&rulel
           #:label $rule:⊗R
           (&\|- Δ (&eph $A))
           (&\|- Δ^ (&eph $B))
           (&\|- Δ Δ^ (&eph (&o* $A $B)))))
      "在结论中, 我们将证明" $A "所需要的资源" Δ
      "和证明" $B "所需要的资源" Δ^ "组合起来. "
      "这两个子证明之间不能共享资源, "
      "因为这将违背资源的瞬态本质. "
      "[译注: 这里的共享其实含义有点微妙, "
      "因为有的时候在某种意义上一些资源可以是相同的, "
      "这样的资源可能有许多份, 然而这并非共享, "
      "因为共享指的是包含本质上相同 (或者说同一/等同) 的资源. "
      "当然了, 这也是在说我们不能在组合的时候随意复制或者擦除, 要保留原样.] "
      "另一方面, 资源的顺序并不重要, 因此我们允许其自由地重新排列.")
   (P "以上是" (Em "右规则(right rule)") "的一个例子, "
      "其表明了该如何证明一个命题 (也就是说, 达成一个目标). "
      "[译注: 因此, " $rule:⊗R "的意思是如何得到一个同时合取呢?] "
      "反过来, 我们不得不刻画如何" (Em "使用")
      "一个命题. 我们以" (Em "左规则(left rule)")
      "来完成此事, 其分解当前资源集合里的一个资源. "
      "这里的左规则是直截了当的."
      (MB (&rulel
           #:label $rule:⊗L
           (&\|- Δ (&eph $A) (&eph $B) (&eph $C))
           (&\|- Δ (&eph (&o* $A $B)) (&eph $C))))
      "[译注: 这是相继式演算和自然演绎不同的地方, "
      "右规则相当于自然演绎的引入规则, 左规则相当于自然演绎的消去规则.] "
      "我们不能随意地发明这样的联结词的左规则和右规则. "
      "最终我们希望我们的系统里的逻辑命题有着符合期望的含义, "
      "不论是从直觉角度还是从形式角度. "
      "接下来的章节里解释了一些我们可以运用的标准.")
   (H3. "identity和cut")
   (P "从基础上来讲, 我们需要在左侧的资源和右侧的目标之间达成平衡. "
      "这种平衡独立于我们所拥有的特定的联结词集合&mdash;&mdash;"
      "其应该对于任意的命题成立.")
   (P "第一条规则, 叫做" (Em "identity")
      ", 陈述了一个资源" $A "自身应该总是足够能达成目标" $A "."
      (MB (&rulel
           #:label (&id $A)
           $
           (&\|- (&eph $A) (&eph $A))))
      "在这条规则里, 我们必须足够谨慎, 不应该引入任何额外的未使用资源, "
      "因为其解释是严苛的 (tight): 任何资源都必须恰好用到一次. "
      "我们经常将规则所应用于的命题附到规则名称的下标上, "
      "因为这样的信息在我们对于相继式演算的研究中是重要的.")
   (P "第二条规则, 叫做" (Em "cut") ", 陈述了相反方向的事实: "
      "达成一个目标" $A "允许 (license) 我们假设 (assume) "
      $A "作为一个资源."
      (MB (&rulel
           #:label (&cut $A)
           (&\|- Δ (&eph $A))
           (&\|- Δ^ (&eph $A) (&eph $C))
           (&\|- Δ Δ^ (&eph $C))))
      "在这条规则里, 我们需要小心地将来源于两段前提的假设组合起来, "
      "这当然也是因为前提中的资源都必须恰好被使用一次 "
      "(不论是在对于" $A "的证明中还是在使用了" $A
      "的对于" $C "的证明中).")
   (P "这两条规则有时被称为" (Em "判断规则(judgmental rule)")
      ", 因为其关心的是判断的本质 "
      "(这里指的是judgments of being a resource and a goal), 或者说"
      (Em "结构规则(structural rule)")
      ", 因为其并不检视命题, 而只检视相继式的结构.")
   (P "本次讲座的剩余部分里我们将会省略判断注记 (judgment annotation) "
      $eph ", 因为暂时这些注记总是相同的.")
   (H3. "identity展开")
   (P "接下来我们开始讨论应用于检查我们对于联结词的定义的标准, "
      "在于左规则和右规则是否相互一致. "
      "第一条标准是检查我们是否能够通过对于更小类型的identity规则的使用"
      "来消除对于复合类型的identity规则的使用. "
      "这意味着左规则和右规则配合得足够充分以至于我们可以推导出identity规则的实例."
      (MB (&->E (render-proof-tree
                 env:linear
                 `(id ,(&o* $A $B)))
                (render-proof-tree
                 env:linear
                 `(<= (⊗L ,$ ,$A ,$B ,(&o* $A $B))
                      (<= (⊗R ,$A ,$A ,$B ,$B)
                          (id ,$A)
                          (id ,$B))))))
      "我们使用" $->E "表示展开一个identity规则"
      "为一个使用更小的命题的identity规则的证明. "
      "这之所以被称为展开 (expansion), "
      "是因为证明变得更大, 即便命题变得更小.")
   (P "请注意我们是如何使用" (&o* $A $B)
      "的左规则和右规则的, "
      "并且它们的确配合良好以能够推导出对于"
      (&o* $A $B) "而言的identity规则.")
   (H3. "cut归约")
   (P "另一种判断规则是cut, 其也可以用来判断"
      "相同联结词的左规则和右规则是否配合良好. "
      "这次我们需要表明我们可以归约a cut at "
      (&o* $A $B) "为cuts at " $A " and " $B "."
      (MB (render-proof-tree
           env:linear
           `(<= (cut ,(&cm Δ Δ^) ,(&o* $A $B)
                     ,Δ^^ ,$C)
                (⊗R ,Δ ,$A ,Δ^ ,$B)
                (⊗L ,Δ^^ ,$A ,$B ,$C))))
      (MB $->R)
      (MB (render-proof-tree
           #:check? #f ;attention!
           env:linear
           `(<= (cut ,Δ^ ,$B ,(&cm Δ Δ^^) ,$C)
                (prop ,(&\|- Δ^ $B))
                (cut ,Δ ,$A ,(&cm Δ^^ $B) ,$C))))
      "[译注: 译者实现了一个用于排版证明树的DSL, "
      "但是相当受限, 尤其是其检查证明的合法性的机制过于严格, "
      "以至于我这里必须放弃合法性检查. "
      "另外, 这个证明树和原文稍有区别, 因为它没有调整其中一些顺序. "
      "严格地说, 交换顺序应该编码为结构规则, "
      "但是这个讲义为了简单起见只是非形式化地处理交换.] "
      "我们再次看到了资源处于平衡状态: "
      "我们在证明并使用" (&o* $A $B)
      "时并不会获得或者损失任何的资源.")
   (P "之后的讲座里我们将会看到, "
      "cut归约实际上是线性逻辑的一些计算性解释背后的引擎.")
   (H3. "线性implication")
   (P "译注: 我故意不翻译implication的原因在于翻译了之后"
      "就总是容易和entailment, deduction甚至inference打架, "
      "然而它们的区别从英文看是一目了然的. "
      "其实我发现最早翻译逻辑学文献的人可能就没有真的理解这些概念, "
      "导致了一些非常奇怪的问题, 比如说implication和entailment"
      "被很多人都翻译成了同一个词, 即" (Q "蕴涵")
      ". 当然了, 逻辑学自身的用语和符号有时就令人迷惑, "
      "比如说相继式 (sequent) 和句法/逻辑蕴涵 "
      "(syntatic/logical entailment) 往往都使用相同的符号" $\|- ".")
   (P "最后, 我们回到表达推理规则的原始问题上来. "
      "想法在于用一个quarter交换两个dime和一个nickel的规则"
      (MB (&rule
           $quarter
           ((&split 8)
            $dime $dime $nickel)))
      "变成了"
      (MB (&-o $quarter (&o* $dime $dime $nickel)))
      "除了 (except that) 规则本身是永恒的, 而命题不是, "
      "除非我们采取特殊安排.")
   (P (&-o $A $B) "的意思是"
      (Q (Em "如果我们有了一个" $A ", 我们可以获得一个" $B))
      ". [译注: 这里原文说了大声朗读 (笑). 另外, "
      "这里的获得当然指的是用" $A "交换" $B ".] "
      "因此, 如果我们有(一个)" (&-o $A $B)
      ", 并且我们可以获得一个" $A ", 那么这是我们使用"
      $B "的许可 (license)."
      (MB (render-proof-tree
           env:linear
           `(-oL ,Δ ,$A ,Δ^ ,$B ,$C)))
      "反过来, 为了表明我们可以达成" (&-o $A $B)
      ", 我们需要表明如何在额外假设我们拥有一个" $A
      "的条件下达成" $B "."
      (MB (render-proof-tree
           env:linear
           `(-oR ,Δ ,$A ,$B)))
      "让我们来检查它们是否协调一致. 首先, identity展开:"
      (MB (&->E
           (render-proof-tree
            env:linear
            `(id ,(&-o $A $B)))
           (render-proof-tree
            #:check? #f ;attention!
            env:linear
            `(<= (-oR ,(&-o $A $B) ,$A ,$B)
                 (<= (-oL ,$A ,$A ,$ ,$B ,$B)
                     (id ,$A)
                     (id ,$B))))))
      "接着是cut归约:"
      (MB (render-proof-tree
           env:linear
           `(<= (cut ,Δ ,(&-o $A $B) ,(&cm Δ^ Δ^^) ,$C)
                (-oR ,Δ ,$A ,$B)
                (-oL ,Δ^ ,$A ,Δ^^ ,$B ,$C))))
      (MB $->R)
      (MB (render-proof-tree
           env:linear
           `(<= (cut ,(&cm Δ^ Δ) ,$B ,Δ^^ ,$C)
                (cut ,Δ^ ,$A ,Δ ,$B)
                (prop ,(&\|- Δ^^ $B $C)))))
      "[译注: 又来写点无聊的译注了, 首先作者稍微调整了一下"
      (&cm Δ Δ^ Δ^^) "的出现顺序, 这可能是为了让cut归约前的"
      "证明树看起来更漂亮一点 (其实也没有), 不过其实也没有必要. "
      "下面的证明树我没有关闭检查, 因此呈现细节和原文不同 "
      "(相继式里命题出现的顺序不同), 但当然没有实质区别.] "
      "幸运的是, 这两条规则的确相当和谐 (in harmony).")
   (P "之后的讲座将会继续引入新的联结词和规则.")
   (H2. "和谐")
   (P "之前的讲座里我们开始建立线性逻辑的命题联结词. "
      "具体来说, 我们引入了同时合取" (&o* $A $B)
      "和线性implication " (&-o $A $B)
      ". 每种联结词都以相继式演算的左规则和右规则确定, "
      "其刻画了如何使用一个资源以及如何证明带有这种联结词的目标. "
      "我们也阐述了两种用以判断逻辑联结词是否具备意义的测试: "
      "identity展开和cut归约. 如果它们都成立, "
      "那么我们就称这个联结词的左规则和右规则是"
      (Em "和谐的(in harmony)")
      ". 现在我们继续建立新的联结词, "
      "并探索左规则和右规则之间和谐的失败会导致什么后果.")
   (H3. "乘性单位元")
   (P "每当我们有了一个二元运算符时, 目前的情况是" (&o* $A $B)
      ", 我们应该考虑是否它具有单位元. 在当前的情况下, "
      "这个单位元记作" $unit ", 其具有" (&o* $A $unit)
      "和" (&o* $unit $A) "都等价于" $A "的性质. "
      "我们可以试着系统地推导" $unit "的性质, "
      "通过将其想成是零元的乘性合取. "
      "以下, 我们先展示二元的规则, 然后再展示零元的规则."
      (MB ((&split 16)
           (render-proof-tree
            env:linear
            `(⊗R ,Δ ,$A ,Δ^ ,$B))
           (render-proof-tree
            env:linear
            `(1R))))
      (&o* $A $B) "具有两个部分 (component), 因此"
      $rule:⊗R "具有两个前提. 对应地, " $unit
      "没有部分, " $rule:unitR "也没有前提. "
      "这也意味着我们要求这里不应该存在资源, 由"
      (Q $d*) "指明. 总结一下, 如果我们没有资源, 那么"
      $unit "作为目标成立.")
   (P "现在来看左规则:"
      (MB ((&split 16)
           (render-proof-tree
            env:linear
            `(⊗L ,Δ ,$A ,$B ,$C))
           (render-proof-tree
            env:linear
            `(1L ,Δ ,$C))))
      (&o* $A $B) "具有两个部分, 因此它们都在前提里作为新资源出现. 鉴于"
      $unit "没有部分, 故前提中没有新资源出现.")
   (P $unit "的左规则和右规则是平衡的. 首先, 局部展开:"
      (MB (&->E
           (render-proof-tree
            env:linear
            `(id ,$unit))
           (render-proof-tree
            env:linear
            `(<= (1L ,$ ,$unit)
                 (1R)))))
      "接着, 归约:"
      (MB (&->R
           (render-proof-tree
            env:linear
            `(<= (cut ,$ ,$unit ,Δ^ ,$C)
                 (1R)
                 (1L ,Δ^ ,$C)))
           (&\|- Δ^ $C))))
   (P "我们应该验证" (&o* $A $unit)
      "等价于" $A ". 但是, 这是何意呢? "
      "我们将其视为" (&\|- (&o* $A $unit) $A)
      "且" (&\|- $A (&o* $A $unit))
      ". 我们只给出一个示例证明, 另一个方向"
      "和其他版本应该是容易证明的."
      (MB (render-proof-tree
           env:linear
           `(<= (⊗L ,$ ,$A ,$unit ,$A)
                (<= (1L ,$A ,$A)
                    (id ,$A)))))
      "以下是译者补充的另一个方向的证明树."
      (MB (render-proof-tree
           env:linear
           `(<= (⊗R ,$A ,$A ,$ ,$unit)
                (id ,$A)
                (1R)))))
   (P "乘性单位元在对于消去 (eliminate) 资源的描述中是有用的, "
      "使用习语 (idiom) " (&-o $A $unit) ".")
   (H3. "和谐的失败")
   (P "我们现在以一种情况检视和谐的失败所导致的后果. "
      "让我们假定我们将同时合取的左规则替换为了以下两条规则:"
      (MB ((&split 16)
           (render-proof-tree
            env:linear
            `(⊗L1?? ,Δ ,$A ,$B ,$C))
           (render-proof-tree
            env:linear
            `(⊗L2?? ,Δ ,$A ,$B ,$C))))
      "[译注: 这样的规则显然在线性逻辑中并不正确, 因为它们类似于弱化, 引入了冗余的资源.] "
      "首先我们注意到identity展开是失败的. 以下展示了我们的一次尝试, "
      "其他的也会以类似的方式失败. [译注: 其他的大概指的是cut归约吧.]"
      (MB (&->E
           (render-proof-tree
            env:linear
            `(id ,(&o* $A $B)))
           (render-proof-tree
            env:linear
            `(<= (⊗L1?? ,$ ,$A ,$B ,(&o* $A $B))
                 (<= (⊗R ,$A ,$A ,$ ,$B)
                     (id ,$A)
                     (fake ,(&\|- $d* $B)))))))
      "实际上, 根据这两条不正确的规则, "
      (Em "弱化(weakening)")
      (MB (render-proof-tree
           env:linear ;!!!
           `(weaken ,Δ ,$A ,$C)))
      "将会成为导出推理规则. "
      "当然, 这违反了我们关于瞬态资源必须恰被使用一次的基本原则, 因为"
      $A "的确没有被使用.")
   (MB (render-proof-tree
        env:linear
        `(<= (cut ,$A ,(&o* $A $unit) ,Δ ,$C)
             (<= (⊗R ,$A ,$A ,$ ,$unit)
                 (id ,$A)
                 (1R))
             (<= (⊗L2?? ,Δ ,$A ,$unit ,$C)
                 (1L ,Δ ,$C)))))
   (H3. "alternative合取")
   (P "使用我们对于硬币交换的规则, 我们可以证明"
      (&\|- $quarter (&o* $dime $dime $nickel))
      "以及" (&\|- $quarter $quarter)
      ". 但是, 显然我们不能证明"
      (&\|- $quarter (&o* (@o* $dime $dime $nickel) $quarter))
      ", 因为这样我们就要在证明中使用(作为这个相继式前提的)"
      $quarter "两次了. 不过, 根据资源" $quarter
      ", 我们实际上仍然可以证明这两者! "
      "[译注: 这里指的大概是通过资源" $quarter ", 既可以证明"
      (&o* $dime $dime $nickel) ", 也可以证明"
      $quarter ", 但不能同时证明.] "
      "为了表达这种情况, 在逻辑里我们将其写作一个联结词"
      (&& $A $B) ", 读作" (Q (Em $A " with " $B))
      ". 它可以被称为" (Em "alternative合取") "或者"
      (Em "加性合取(additive conjunction)")
      ". 以这个联结词, 现在我们可以证明"
      (&\|- $quarter (&& (@o* $dime $dime $nickel) $quarter))
      ". 我们可以由资源" Δ "达成" (&& $A $B) "作为目标恰当我们可以由"
      Δ "达成" $A "且由" Δ "达成" $B "."
      (MB (render-proof-tree
           env:linear
           `(&R ,Δ ,$A ,$B)))
      "这表面上违反了线性 (linearity): 似乎" Δ
      "的资源被复制了. 但是我们是安全的, 因为当我们拥有一个资源"
      (&& $A $B) "时, 我们不得不在" $A "和" $B
      "之间选择一个. 我们不能两个都要. 这意味着"
      Δ "并没有真的被复制, 因为两个前提中只有一个将会被使用, "
      "很快我们就会验证这个事实."
      (MB ((&split 16)
           (render-proof-tree
            env:linear
            `(&L1 ,Δ ,$A ,$B ,$C))
           (render-proof-tree
            env:linear
            `(&L2 ,Δ ,$A ,$B ,$C))))
      Δ "的" (Q "复制") "并不会破坏linearity的非正式理由"
      "反映在identity展开和cut归约之中. 在归约中体现得更清楚, "
      "因此我们先展示cut归约."
      (MB (&->R
           (render-proof-tree
            env:linear
            `(<= (cut ,Δ ,(&& $A $B) ,Δ^ ,$C)
                 (&R ,Δ ,$A ,$B)
                 (&L1 ,Δ^ ,$A ,$B ,$C)))
           (render-proof-tree
            env:linear
            `(cut ,Δ ,$A ,Δ^ ,$C))))
      "实际上, 还有另外一个对称的归约, "
      "其cut的第二个前提由" $rule:&L2
      "推得. 因为很简单, 所以这里译者补充一下."
      (MB (&->R
           (render-proof-tree
            env:linear
            `(<= (cut ,Δ ,(&& $A $B) ,Δ^ ,$C)
                 (&R ,Δ ,$A ,$B)
                 (&L2 ,Δ^ ,$A ,$B ,$C)))
           (render-proof-tree
            env:linear
            `(cut ,Δ ,$B ,Δ^ ,$C))))
      "identity展开为"
      (MB (&->E
           (render-proof-tree
            env:linear
            `(id ,(&& $A $B)))
           (render-proof-tree
            env:linear
            `(<= (&R ,(&& $A $B) ,$A ,$B)
                 (<= (&L1 ,$ ,$A ,$B ,$A)
                     (id ,$A))
                 (<= (&L2 ,$ ,$A ,$B ,$B)
                     (id ,$B)))))))
   (H3. "加性单位元")
   (P "就像乘性合取具有单位元" $unit
      "一样, 加性合取也存在零元的版本. "
      "这个单位元是" $top " (读作" (Q (Em "top"))
      "). 和之前一样, 其规则也可由加性合取的规则系统地推导出来. "
      "右规则具有零个前提, 将资源" Δ
      "传播到它们所有身上. [译注: 原文是all of them, "
      "指的是每个作为前提的相继式的右侧的命题. "
      "因为现在没有前提, 所以它们的加性合取应该是" $top
      ".] " (&& $A $B) "具有两个conjunct, "
      "因此存在两条左规则; " $top "具有零个conjunct, "
      "因此只有零条左规则."
      (MB ((&split 16)
           (render-proof-tree
            env:linear
            `(⊤R ,Δ))
           (: "没有" $rule:⊤L "规则")))
      "当然了, 现在没有cut归约, 因为" $top
      "没有左规则. [译注: 因此, 没有办法在相继式的左侧引入" $top
      ".] 但是, 我们仍然有一个identity展开."
      (MB (&->E
           (render-proof-tree
            env:linear
            `(id ,$top))
           (render-proof-tree
            env:linear
            `(⊤R ,$top)))))
   (H3. "析取")
   (P "我们已经看到alternative合取代表了某种选择. "
      "如果我们拥有一个资源" (&& $A $B)
      ", 那么我们可以在我们的证明中选择使用" $A
      "或者" $B ". 如果我们有了一个目标" (&& $A $B)
      ", 那么我们需要考虑两种使用方式.")
   (P "析取和alternative合取是对称的. "
      "如果我们有了一个资源" (&o+ $A $B)
      ", 那么环境必须提供" $A "或者" $B
      ", 因此我们需要考虑两种可能性. "
      "[译注: 读者应该结合下面的规则理解这句话.] "
      "反过来, 如果我们有了一个目标" (&o+ $A $B)
      ", 我们可以通过提供" $A "或者提供" $B
      "来满足这个目标."
      (MB ((&split 16)
           (render-proof-tree
            env:linear
            `(⊕R1 ,Δ ,$A ,$B))
           (render-proof-tree
            env:linear
            `(⊕R2 ,Δ ,$A ,$B))
           (render-proof-tree
            env:linear
            `(⊕L ,Δ ,$A ,$B ,$C))))
      "我们将identity展开和cut归约留作练习5.")
   (H3. "析取单位元")
   (P "也存在着一个析取的单位元" $unit:disj
      ", 其是一种falsehood的形式. "
      "我们可以通过将其视为零元版本的析取来得到其规则."
      (MB ((&split 16)
           (: "不存在" $rule:0R "规则")
           (render-proof-tree
            env:linear
            `(0L ,Δ ,$C))))
      "我们将其性质留作练习6.")
   (H3. "永恒事实")
   (P "要添加的最困难的联结词是为了将永恒事实内化 (internalize) "
      "为一个命题. 实际上出于许多目的, 保留判断"
      (&pers $A) "是比较方便的, 尽管这并非唯一的选择. "
      "这使得一个将瞬态事实判断和永恒事实判断联系起来的判断性规则显得必要. "
      "但是, 首先我们需要对于相继式进行泛化, "
      "将瞬态命题和永恒命题分离. 我们记"
      (MB (&\|-
           (&\; (UnderBrace
                 (&cm (&pers $B_1) $..h (&pers $B_k))
                 $Gamma:normal)
                (UnderBrace
                 (&cm (&eph $A_1) $..h (&eph $A_n))
                 Δ))
           (&eph $C)))
      "现在我们的规则将需要尊重相继式的瞬态本性. "
      "[译注: 原文这句话有缺漏, 或许译者的理解也并不正确.] "
      "永恒和瞬态资源现在由分号" (Q $\;)
      "隔开以至于我们可以通过资源在相继式中的位置来判断其状态 (status).")
   (P "首先, 我们有这样一条判断性规则, 其允许我们作出一个永恒资源的瞬态复制."
      (MB (&rulel
           #:label $copy
           (&\|-
            (&\; (&cm Γ (&pers $A))
                 (&cm Δ (&eph $A)))
            (&eph $C))
           (&\|-
            (&\; (&cm Γ (&pers $A))
                 Δ)
            (&eph $C))))
      "这条规则可以应用于任意的" $A
      ". 可能读者想问是否也存在一条对于目标" (&pers $C)
      "而言的相应结构规则. 如果真有这样的规则, 那么其会是 "
      "[译注: 和之前一样, " (Q $d*) "代表没有资源.]"
      (MB (brac
           (&rulel
            #:label $valid
            (&\|- (&\; Γ $empty) (&eph $A))
            (&\|- (&\; Γ $empty) (&pers $A)))))
      
      )
   (H3. "Of Course!")
   (P "既已定下涵盖永恒性的相继式扩展, 我们现在可以将其集成于命题之中. "
      "对于表达了" $A "永恒地为真的命题, 我们写下" (&! $A)
      " (读作" (Q (Em "of course " $A)) "或者"
      (Q (Em "bang " $A)) "). 我们也将" (&! $A)
      "称为" (Em "指数模态(exponential modality)")
      ". 其左规则取" (&! $A) "的一个瞬态资源, 将其标记为永恒的."
      (MB (&rulel
           #:label $rule:!L
           (&\|- (&\; (&cm Γ (&pers $A)) Δ)
                 (&eph $C))
           (&\|- (&\; Γ (&cm Δ (&eph (&! $A))))
                 (&eph $C))))
      "右规则要求" $A "永恒地为真, 其相当于在没有瞬态资源的情况下"
      $A "瞬态地为真."
      (MB (&rulel
           #:label $rule:!R
           (&\|- (&\; Γ $empty) (&eph $A))
           (&\|- (&\; Γ $empty) (&eph (&! $A)))))
      "这两条规则之间是和谐的吗? 让我们检查一下. "
      "从现在开始我们省略" $eph "和" $pers
      ", 因为我们总能根据其在相继式中的位置进行分辨. "
      "首先是identity展开:"
      (MB (&->E
           (&rulel
            #:label (&id (&! $A))
            $
            (&\|- (&\; $empty (&! $A)) (&! $A)))
           $))
      )
   (H2. "作为计算的cut归约")
   (P "本次讲座我们将会检视以相继式形式化 (formulation) "
      "的线性逻辑的一种计算性解释. "
      "很大程度上我们遵循了一篇最近的论文Caires et al. [CPT12] "
      "的进路, 这篇论文包含了额外的细节和更深的参考. "
      "基本的想法在于命题对应于" (Em "会话类型(session type)")
      " [Hon93], 证明对应于" $pi "演算 [MPW92] 中的进程表达式 "
      "(process expression), 而cut归约对应于进程归约. "
      "我们并不假定你已经了解了" $pi
      "演算, 尽管如果你了解的话显然理解讲座会变得更轻松一些.")
   (H3. "解释判断")
   (P "在函数式的背景 (functional setting) 下, 基本的判断一般具有"
      (&: $M $A) "的形式, 其含义要么是" $M "为" $A
      "的一个证明, 要么是" $M "为类型" $A "的一个项. "
      "在通信进程 (communicating process) 的背景下, 言称"
      (Q (Em $P "为类型" $A "的一个进程"))
      "是意味不明的. 进程与它们的环境进行交流, 故我们转而写下"
      (&:: $P (&: $x $A)) ", 意为" $P "是一个进程, 其沿着信道"
      $x "提供服务" $A ". 这里的信道被认为是一个以" $P
      "为作用域 (scope) 的变量, 因此我们可以将其重命名为"
      (&:: (subst $P $y $x) (&: $y $A))
      ", 只要" $y "还没有在" $P "中用到. "
      "根据惯例, 合适的时机我们将会悄无声息地执行这样的换名.")
   (P "进程是无趣的, 除非它们被置于这样的上下文下, "
      "其中进程不仅" (Em "提供") "服务, 也" (Em "使用")
      "其他进程提供的服务. 我们写下一个相继式"
      (MB (&\|- (&: $x_1 $A_1) $..h (&: $x_n $A_n)
                (&:: $P (&: $x $A))))
      "以表达当进程" $P "与沿着" $x_i "提供"
      $A_i "的进程" $P_i " (" (&<= $1 $i $n)
      ") 一起时, 其沿着信道" $x "提供服务" $A
      ". 所有的变量" $x_i "都必须是互异的. "
      "这其实只是对于一个相继式" (&\|- Δ $A)
      "的装饰, 其唯一地标记了结论以及" Δ
      "中的所有资源. 对于资源我们仍然写下"
      Δ ", 不过现在它们被标记以信道.")
   (P "提供和使用服务是相对应的 (counterparts), "
      "但是它们并不相同. 因此, 形式上, "
      "相继式右侧的判断" (&: $x $A)
      "和相继式左侧的判断" (&: $x_i $A_i)
      "应该被认为是不同的. "
      "既然我们总是可以凭借位置来分辨判断的意图, "
      "那么两者我们都会使用相同的记号.")
   (P "进程随着沿信道的交互演化. 故信道" $x_i
      "上的交互会产生 (engender) 状态上的变化, "
      "而相同的信道不能再以相同的类型被使用了. "
      "因此, 这里的turnstile符号" (Q $\|-) "代表的是"
      (Em "线性假言判断(linear hypothetical judgment)")
      " [CCP03], 其中每个前件 (antecedent) "
      "都必须恰被使用一次. 也就是说, "
      "上下文并不服从于弱化或者收缩规则, "
      "但是重新排列是允许的, "
      "因为前件被标识以唯一的名字.")
   (P "即便还没有定义任何特定种类的服务, "
      "一些原则应该是对于一般性的判断成立. "
      "我们先讨论这些原则, "
      "因为它们对于剩余部分的发展也是重要的指引.")
   (H3. "作为复合的cut")
   (P "当一个进程" $P "沿着" $x (Em "提供") "服务"
      $A ", 且另一个进程" $Q "沿着" $x
      (Em "使用") "服务" $A
      "时, 这两者可以被复合以至于它们沿着"
      $x "进行通信."
      (MB (&rulel
           #:label $cut
           (&\|- Δ (&:: $P (&: $x $A)))
           (&\|- Δ^ (&: $x $A)
                 (&:: $Q (&: $z $C)))
           (&\|- Δ Δ^
                 (&:: (NU $x (para-compose $P $Q))
                      (&: $z $C)))))
      "复合的进程表达式将" $P "和" $Q
      "并置于一个" (Em "并行复合(parallel composition)")
      (para-compose $P $Q) "之中, 其共享着" $x
      "作为一个" (Em "私有(private)")
      "信道, 这是由" (Em "名称限制(name restriction)")
      (@ $nu $x) "所指示的. "
      "注意到这条规则蕴含着某种隐式的重命名, "
      "因为" $P "提供" $A "所沿着的信道必须与"
      $Q "使用" $A "所沿着的信道等同起来. 在"
      $pi "演算中, " (NU $x $P) "是以" $P
      "为作用域的变量" $x "的绑定子 (binder).")
   (P "从纯粹逻辑的角度看待, 这不过就是线性逻辑中的"
      (Em "cut") "规则:"
      (MB (render-proof-tree
           env:linear
           `(cut ,Δ ,$A ,Δ^ ,$C)))
      "我们使用线性逻辑的一种直觉主义版本实际上是有点不同寻常的, "
      "也就是说, 每个相继式的右侧只有一个 (singleton). "
      "这并非绝对本质性的 (见[Abr93], 其提供了一个相关的古典逻辑对应物), "
      "但是它精简了系统的判断澄清 (judgmental justification). "
      "它也反映了提供和使用服务之间内蕴 (intrinsic) 的不对称性, "
      "尽管我们将会看到它们的关联是相当紧密的. "
      "这也有助于建立依赖类型论.")
   (H3. "作为forwarding的identity")
   
   (H3. "将cut复合")
   (P "多重并行复合应该只在其间交互需要时同步 (synchronize). "
      "从证明论的角度来说, 这意味着相继cut的顺序应该是无足轻重的. "
      "相对应的律 (law) 作为重写规则并无固有的方向 (orientation), "
      "因此我们应该将其想成是结构证明等价. 我们留给读者写出"
      "简单的proof figures. 在进程演算方面, 我们得到了相应的"
      (Em "structural congruence")
      ". [译注: 我不太会翻译congruence, 它总是在各种地方出现, "
      "然后总是指代一种等价关系.]"
      (eqn*
       ((NU $x (para-compose (NU $y (para-compose $P $Q)) $R))
        $≡
        (NU $y (para-compose $P (NU $x (para-compose $Q $R)))))
       ($ $ (: "只要" (&cm (&!in $x (&fn $P)) (&!in $y (&fn $R)))))
       ((NU $x (para-compose $P (NU $y (para-compose $Q $R))))
        $≡
        (NU $y (para-compose $Q (NU $x (para-compose $P $R)))))
       ($ $ (: "只要" (&cm (&!in $x (&fn $Q)) (&!in $y (&fn $P))))))
      "这些可由更基本的结构等价推导出来, "
      "也就是并行复合的结合律和作用域挤出 (scope extrusion)."
      (eqn*
       ((para-compose
         (para-compose $P $Q) $R)
        $≡
        (para-compose
         $P (para-compose $Q $R))
        "结合律")
       ((para-compose $P $Q)
        $≡
        (para-compose $Q $P)
        "交换律")
       ((para-compose
         $P (NU $x $Q))
        $≡
        (NU $x (para-compose $P $Q))
        "作用域挤出")
       ($ $ $ (: "只要" (&!in $x (&fn $P)))))
      
      )
   (H3. "输入")
   (H3. "归约")
   (H3. "例子: 硬币交换")
   (H2. "Choice and Replication")
   (H2. "量化")
   (H3. "全称量化")
   (H2. "cut消除")
   (H2. "Identity and Inversion")
   (H2. "Chaining and Focusing")
   (H2. "自然演绎")
   (H2. "Ordered Logic")
   (H2. "Ordered Forward Chaining")
   (H2. "Backward Chaining")
   (H2. "资源管理")
   (H2. "合一")
   (H2. "Monadic Logic Programming")
   (H2. "并发逻辑框架")
   (H2. "资源语义")
   (H2. "嵌入线性逻辑于直觉主义逻辑")
   (H2. "古典线性逻辑")
   
   ))