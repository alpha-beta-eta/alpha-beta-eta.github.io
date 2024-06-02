#lang racket
(provide stone.html)
(require SMathML)
(define Δ $Delta:normal)
(define $varphi (Mi "&varphi;"))
(define -- "&mdash;&mdash;")
(define (arr a b)
  (&join (&complement a) b))
(define $darr (Mi "&darr;"))
(define (&darr a)
  (app $darr a))
(define (symdiff a b)
  (&join (@meet a (&complement b))
         (@meet b (&complement a))))
(define ((tcomment #:n [n ""]) . x*)
  (keyword-apply
   Div '(#:attr*) '(((class "tcomment")))
   (B (format "译者注记~a." n)) " " x*))
(define-syntax eqn*
  (syntax-rules ()
    ((_ (x ...) ...)
     (MB (set-attr*
          (&Table (x ...) ...)
          'columnalign "right center left left")))))
(define (powerset X)
  (ap $P X))
(define (∀ domain statement)
  (: $forall domain (@ statement)))
(define $Join (Mo "&Vee;"))
(define Join (make-bigop $Join))
(define $Meet (Mo "&Wedge;"))
(define Meet (make-bigop $Meet))
(define $complement $neg)
(define (&complement x)
  (ap $complement x))
(define $join $disj)
(define $meet $conj)
(define-infix*
  (&Δ Δ)
  (&join $disj)
  (&meet $conj))
(define-@lized-op*
  (@-> &->)
  (@arr arr)
  (@join &join)
  (@meet &meet))
(define (format-num section index)
  (cond ((eq? (car section) '*)
         (if index
             (format "~a" index)
             #f))
        (else
         (if index
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     index)
             (format "~a.~a"
                     (apply string-append
                            (add-between
                             (map number->string
                                  (cdr (reverse section))) "."))
                     "*")))))
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
    (if num
        (Cite `(a ((href ,href)) ,name ,num))
        (Cite `(a ((href ,href)) "某" ,name))))
  (lambda (#:id [id #f] #:auto? [auto? #t])
    (lambda (#:attr* [attr* '()] . html*)
      (cons (build-%entry #:id id #:auto? auto?
                          #:present present #:cite cite)
            (cons attr* html*)))))
(define-syntax-rule (define-Entry* (id name class) ...)
  (begin (define id (Entry name class))
         ...))
(define-Entry*
  (Lemma "引理" "lemma")
  (Definition "定义" "definition")
  (Theorem "定理" "theorem")
  (Example "例子" "example")
  (Proposition "命题" "proposition")
  (Exercise "练习" "exercise")
  (Remark "评注" "remark")
  (Corollary "推论" "corollary"))
(define stone.html
  (TnTmPrelude
   #:title "Stone空间笔记"
   #:css "styles.css"
   (H1. "Stone空间笔记")
   (P "本书的前言, 读者建议, 引论部分有一些内容使用了机器翻译, "
      "由此造成的风格不一致或者翻译不准确乃至其他问题, "
      "都只能由阅读这些内容的读者见谅了. "
      "当然, 即便是机器翻译, 尤其是数学内容, "
      "我也不得不作出大量修改, 因为GenAI的确不擅长数学. "
      "尽管其他部分没有使用机器翻译, 我也做了许多其他改动.")
   (H2 "前言")
   (P "1977年夏天的时候我第一次意识到并不存在一本单独的书籍"
      "使得人们能够获得对于自Stone表示定理流出的所有数学推论"
      "的平衡看法, 而存在这样的一种书籍是很有用的. "
      "然而, 那时相对来说我并没有太刻意地追求这个想法; "
      "我唯一做的事情只是写下了一份暂定的章名列表, "
      "而这和最终面世的书籍相比并没有什么类似之处. "
      "我在1978年秋天有了一个更为认真的开端, 当时我在剑桥讲授了一门题为" (Q "Stone空间")
      "的Part III (研究生) 课程, 涵盖了第I至第IV章的大部分内容 (若非时间不足, 本可涵盖更多). "
      "1979年1月, 我有机会将第II章和第III章中的大量内容加以重新利用, 作为一门题为"
      (Q "内部与外部Locale") "的课程的一部分, 该课程是应邀在比利时鲁汶天主教大学讲授的, "
      "但该课程进而探讨了Locale的topos理论应用 (整理为[Johnstone 1979]), "
      "而这部分内容从未打算纳入本书之中.")
   (P "前三章的正文 (第III章第4节除外) 写于1979年夏天; "
      "在随后的秋天, 我(为原来听众中的一部分人)讲授了" (Q "Stone空间")
      "课程的续篇, 涵盖了第VI章和第VII章的大部分内容. "
      "其余正文的写作主要完成于此后的两个夏天: "
      "第IV章以及第III章第4节和第V章第1节写于1980年, 其余部分写于1981年. "
      "在正文完成之后, 排版开始之前, "
      "我借助在蒙特利尔麦吉尔大学休假期间所讲授的另一门课程的机会, "
      "对一两处内容作了进一步的打磨, 那是在1982年的冬天.")
   (P "在撰写这样一本书的过程中, 所积累的感激之情难以在简短的序言中一一道尽. "
      "我首先要感谢上述各门课程的听众: 尤其是我的同事Martin Hyland和我的学生Andrew Pitts, "
      "他们对这一项目始终如一的热情给了我莫大的坚持下去的动力; "
      "感谢Francis Borceux, 是他促成了我受邀前往Louvain-la-Neuve; "
      "以及感谢Michael Barr和Marta Bunge, 是他们邀请我前往蒙特利尔. "
      "在众多对我研习本书所涵盖领域有所裨益的人士之中, 仅挑选三位实属不公, "
      "但我仍要在此点名道谢: Bernhard Banaschewski, John Isbell和André Joyal. "
      "前两位的影响可从他们的名字在各章末尾注记中出现的频率以及他们在参考文献中条目的篇幅中略见一斑; "
      "Joyal的贡献则无法以他本人的著述来衡量, "
      "但他对我关于locale的思考所产生的影响无疑是深远的 (参见[Johnstone 1983]).")
   (P "打字稿的预印本被分发给了若干同事 "
      "(包括Michael Fourman, Rudolf Hoffmann, Dana Scott, Harold Simmons和Myles Tierney), "
      "其中几位提出了宝贵的意见和改进建议; "
      "在此我尤其要感谢Saunders Mac Lane在历史问题上给予的专业指导. "
      "(然而, 无论是他还是其他任何被提及的人, 都不应为书中残留的任何错误负责, 那些错误皆由我一人承担.) "
      "最后, 同样不可或缺的是, 我要记录下这样一件事: "
      "自从我开始从事这一项目以来, 能够与Marshall Stone本人相识, 使我的人生得到了极大的丰富. "
      "他温文尔雅的款待, 以及他对自己1930年代那些奠基性定理在当代所衍生出的成果所抱有的浓厚兴趣, "
      "对我而言意义深远.")
   (P "最后, 我还要感谢David Tranah及其剑桥大学出版社的同事们在本书制作过程中所展现出的高效, "
      "感谢他们对我在排版风格上种种无理要求的慷慨接纳, "
      "以及感谢他们一丝不苟地在出版流程的每一个阶段与我保持联络" --
      "尽管在我旅居蒙特利尔期间, 加拿大邮政竭尽全力地从中作梗.")
   (H2 "读者建议")
   (P "就和数学领域诸多研究级别的书籍一样, 本书是学生的教科书和专家的参考书之间的一种紧张的妥协. "
      "专家想必不需要帮助也可以从书本中找到他们想要的东西 (如果的确存在的话), "
      "因此这些注记主要是面向学生的, 或者是那些考虑将本书用作某个研究生课程的基础的讲师.")
   (P "首先谈谈预备知识: 读者被假定掌握的代数与一般拓扑学知识, 大致相当于英国本科课程中所能习得的水平. "
      "特别地, 读者被假定 (至少在第IV章和第V章中) 对交换环有一定的了解; "
      "但另一方面, 本书对格的处理是完全自足的. (不过, 本书不应被视为一本格论教材" --
      "它遗漏了太多重要的概念, 尤其是模性的概念.) "
      "范畴的处理 (范畴在全书中被自由使用) 则不是自足的; "
      "此前未曾接触过范畴的读者将需要阅读一些背景材料, 以充实第I章第3节中的纲要性内容. "
      "尽管如此, 以本书为基础的课程完全可以与一门范畴论入门课程同步进行, "
      "我期望两者能够在很大程度上相互促进. 类似的说明也适用于层理论, 它仅在第V章中出现; "
      "该章第一节并非一篇自足的层论导引, 但若与一门层论入门课程结合使用, "
      "应当足以为理解该章其余内容提供充分的基础.")
   
   (H2 "引论: 历史视角下的Stone定理")
   (P "本书是关于某个特定定理 (即Boole代数的Stone表示定理) 以及由此45年来所发展的一些数学结果的. "
      "准备以此方式描绘某个数学想法发展的书籍的作者总是无可避免地面对着两种方法之间的妥协的必然性: "
      "一种是历史性方法, 其或多或少试图以年代顺序追逐每条线索的发展 "
      "(但是可能会忽略不同线索之间的某些内在联系); 另一种是逻辑性方法, 或者[Mac Lane 1980]"
      "所说的基因性方法, 其以事后诸葛的方式采取最为经济和无痛的路线到达主要结果 "
      "(但是因而可能会失去一些关于这些结果何以被视为重要的洞察).")
   (P "我所采用的折衷方案是: 在正文中相当彻底地采用逻辑化的处理方式 "
      "(我们最终在第II.4节证明Stone定理所走的路线, 至少在历史眼光看来是相当反常的), "
      "但以一篇引言开始本书, 引言首先试图将表示定理置于Stone证明它时的历史背景下, "
      "然后指出此后的发展脉络" -- "正是这些发展使得我所采用的阐述路线, "
      "可以被看作 (至少我自己这样认为) 是覆盖某一相当多样化的数学知识体系的一种高效而统一的方式. "
      "(为了强化本引言的旨意, 每章末尾还附有历史与文献注记.)")
   (P "我们的历史回顾从抽象代数的诞生讲起, "
      "这一历史最近已由Saunders Mac Lane在一篇精彩的文章[1981]中作了记述. "
      "Mac Lane将代数的抽象/公理化方法的第一个清晰例证追溯到Cayley [1854]关于群论的一篇论文. "
      "然而, 群论并不处于本世纪早年代数抽象化运动的最前沿, "
      "这或许是因为Cayley的表示定理 [1878]证明了每个抽象群都与一个"
      (Q "具体") "的置换群抽象同构, 从而在很长一段时间内消除了对群论进行抽象发展的必要.")
   (P "如果说群论是抽象代数中最古老的分支, 那么布尔代数则有充分理由被称为第二古老的分支. "
      "当然, Boole [1847, 1854]和Peirce [1880]实际上只关注命题(或类)的具体代数, "
      "但Whitehead [1898]和Huntington [1904]都采取了抽象的方法. "
      "然而, 在1930年以前, 人们对非布尔格似乎兴趣寥寥 "
      "(Dedekind [1897, 1900]的卓越论文除外, 但那些论文同样是关于具体格的" --
      "具体而言是理想的格), 即便是布尔理论本身, 也几乎没有超出单纯公理推演的实质性发展.")
   (P "现在, 尽管Cayley的表示定理或许延缓了抽象群论的发展, 但它至少通过证明这些公理确实足以刻画"
      (Q "置换代数") "而稳定了该学科的公理体系. 在布尔代数中, 同样迫切需要一个类似的表示定理, "
      "以说明其公理已捕捉到" (Q "类的代数") "的本质; 但这样的定理并未随即出现.")
   (P "当然, 我们不应期望这样的定理断言每个布尔代数都同构于某个集合的全部子集所构成的代数; "
      "正如完整的置换群具有某些并非所有群都共享的群论性质 "
      "(例如, 排除二阶群后, 具有平凡中心这一性质), "
      "完整的幂集代数同样具有某些格论性质, 而这些性质并非所有布尔代数都享有. "
      "让我们简要考察其中两个性质.")
   (P "在一个集合的全部子集所构成的代数中, 除了并与交这两个二元运算 (分别由格运算"
      $join "和" $meet "表示) 之外, 还额外具有对无穷子集族取并与交的可能性. "
      "若一个格具有与上述集合论运算相对应的无穷运算" $Join "和" $Meet
      ", 则称该格为完备的; 不难给出非完备布尔代数的例子. "
      "另外, 在某个集合" $X "的完整幂集" (powerset $X)
      "之中, 单元素子集" (&cm (setE $x) (∈ $x $X))
      "扮演着特别的角色: 它们不等于最小元素" $empty
      ", 但是其和" $empty "之间没有任何元素" --
      "等价地, " (setE $x) "不可能表示为严格更小的子集之并. "
      "布尔代数中具有此性质的元素称为原子 (atom); " (powerset $X)
      "中的原子的丰富性体现在如下事实: 对每个" (&!= $Y $empty)
      ", 存在一个原子" $Z "满足" (&sube $Z $Y)
      ". 具有此性质的布尔代数称为原子的 (atomic); "
      "同样, 不难给出非原子布尔代数的例子.")
   (P "现在令" $B "是一个抽象的布尔代数, 而令" $X
      "代表由" $B "的所有原子构成的集合. 我们可以定义一个映射"
      (func $varphi $B (powerset $X)) ", 令"
      (&= (app $varphi $b) (setI (∈ $x $X) (&<= $x $b)))
      ". 很容易根据原子的定义推出一个原子" $x "满足"
      (&<= $x (&join $b $c)) "当且仅当" (&<= $x $b) "或"
      (&<= $x $c) "; 由此我们可以得出" $varphi
      "是一个布尔代数的同态. 而且, 如果" $B "是原子的, 那么"
      $varphi "是单射, 因为如果" (&!= $b $c) ", 那么对称差"
      (&Δ $b $c) "落于某个原子之上, 这个原子只会位于"
      (app $varphi $b) "和" (app $varphi $c)
      "其中之一; 另外, 如果" $B "是完备的, 那么"
      $varphi "是一个满射, 因为" $X "的任意子集" $Y
      "都是其在" $B "中的join在" $varphi "下的像. "
      "因此, 我们已经证明了")
   ((theorem)
    "一个布尔代数同构于某个集合的所有子集构成的代数"
    "当且仅当其是完备且原子的.")
   (P "这个定理首先由逻辑学家A. Lindenbaum和A. Tarski证明 "
      )
   (H2. "预备")
   (H3. "格")
   ((Definition)
    "令" $A "是一个集合. " $A "上的一个" (Em "偏序")
    "是一个二元关系" $<= ", 其满足"
    (Ol #:attr* '((type "i"))
        (Li "自反性: 对于所有的" (∈ $a $A)
            ", " (&<= $a $a) ";")
        (Li "传递性: 如果" (&<= $a $b) "且" (&<= $b $c)
            ", 那么" (&<= $a $c) ";")
        (Li "反对称性: 如果" (&<= $a $b) "且" (&<= $b $a)
            ", 那么" (&= $a $b) "."))
    "一个偏序集是一个装备了某个偏序的集合.")
   ((Definition)
    "令" $A "是一个偏序集, " $S "是" $A "的一个子集. "
    "我们称某个元素" (∈ $a $A) "是" $S "的一个"
    (Em "join") ", 或者说" (Em "最小上界")
    ", 并记作" (&= $a (Join $S)) ", 如果"
    (Ol #:attr* '((type "i"))
        (Li $a "是" $S "的一个上界, 即对于所有的"
            (∈ $s $S) "都有" (&<= $s $a) ";")
        (Li "如果" (∀ (∈ $s $S) (&<= $s $b))
            ", 那么" (&<= $a $b) "."))
    "反对称公理确保了" $S "的join一旦存在则是唯一的. "
    "如果" $S "是一个具有两个元素的集合"
    (setE $s $t) ", 那么我们对于" (Join (setE $s $t))
    "记" (&join $s $t) ". {译注: 实际上, 即便"
    (&= $s $t) ", 我们仍然可以使用这种记号.} "
    "如果" $S "是空集" $empty
    ", 那么我们对于" (Join $empty)
    "记" $0 "&mdash;&mdash;显然" $0
    "是" $A "的最小元素.")
   ((Proposition)
    "令" $A "是一个偏序集且其每个有限子集均有一个join, "
    "那么上述的二元运算" $join "以及元素" $0 "满足等式"
    (Ol #:attr* '((type "i"))
        (Li (&= (&join $a $a) $a))
        (Li (&= (&join $a $b) (&join $b $a)))
        (Li (&= (&join $a (@join $b $c))
                (&join (@join $a $b) $c)))
        (Li (&= (&join $a $0) $a)))
    "其中" (∈ $a $b $c $A)
    ". 简而言之, 我们可以说" (tu0 $A $join $0)
    "是一个交换幺半群, 且其中每个元素都是幂等的. "
    "{译注: 或者直接说交换幂等幺半群.} "
    "反过来, 我们有")
   ((Theorem #:auto? #f)
    "令" (tu0 $A $join $0) "是一个交换幺半群, "
    "且其中每个元素都是幂等的, 那么" $A
    "上存在唯一的偏序使得" (&join $a $b)
    "是" $a "和" $b "的join, 且" $0
    "是最小元.")
   ((proof)
    "显然, 如果这样一个偏序存在, 我们必然有"
    (&<= $a $b) "当且仅当" (&= (&join $a $b) $b)
    ". {译注: 这说明了若该偏序存在则唯一.} "
    "反过来, 取这个作为" $<= "的定义, 那么"
    $<= "的自反性可由等式i直接推得, "
    "而反对称性可由定义的形式得到. "
    "为了证明传递性, 设" (&<= $a $b)
    "且" (&<= $b $c) ", 那么"
    (eqn*
     ((&join $a $c) $= (&join $a (@join $b $c)) (: "因为" (&<= $b $c)))
     ($             $= (&join (@join $a $b) $c) "根据等式iii")
     ($             $= (&join $b $c)            (: "因为" (&<= $a $b)))
     ($             $= $c                       (: "因为" (&<= $b $c))))
    "于是" (&<= $a $c) "." (Br)
    "现在令" (&cm $a $b) "是" $A "的任意两个元素, 那么"
    (&= (&join $a (@join $a $b))
        (&join (@join $a $a) $b)
        (&join $a $b))
    ", 即" (&<= $a (&join $a $b))
    ", 类似地可以证明 (使用等式ii) "
    (&<= $b (&join $a $b))
    ". 如果" (&<= $a $c) "且" (&<= $b $c) ", 那么"
    (&= (&join (@join $a $b) $c)
        (&join $a (@join $b $c))
        (&join $a $c)
        $c)
    ", 即" (&<= (&join $a $b) $c)
    ". 因此, " (&join $a $b) "是" $a
    "和" $b "的join. 最后, 等式iv立即告诉我们"
    $0 "是" $A "的最小元.")
   (P "一个集合若是带有上面定理中所描述的结构则被称为一个"
      (Em "半格") "或者说" (Em "join半格")
      ". 这个定理是说半格的概念既可以基于序关系定义, "
      "也可以基于join运算定义; "
      "但是当我们开始考虑同态 (保持结构的映射) 时, "
      "两者存在重要的差异. "
      "一个半格同态" (func $f $A $B)
      " (即保持突出元素" $0 "和运算" $join
      "的映射) 必然是一个保序映射, "
      "但是半格之间的保序映射不一定是同态. "
      "{译注: 保序映射即偏序集之间的同态, 也称单调映射.}")
   ((Exercise #:auto? #f)
    
    )
   ((Definition)
    "对偶地, 在任意偏序集中我们可以考虑" (Em "meet")
    "或者说" (Em "最大下界") "的概念, "
    "其定义是通过反转join定义中的所有不等式得到的. 参照"
    (&cm (Join $S) (&join $a $b) $0)
    "我们可以定义" (&cm (Meet $S) (&meet $a $b) $1)
    ". 一个" (Em "格") "是一个偏序集" $A
    "满足其每个有限子集均有join和meet. "
    "根据前述定理, 这等价于说" $A
    "是一个装备了两个二元运算" $join "和" $meet
    "以及两个突出元素" $0 "和" $1 "的集合, 并且"
    (tu0 $A $join $0) "和" (tu0 $A $meet $1)
    "都是半格, 而且由这两个半格结构所导出的"
    $A "上的两个偏序是对立的.")
   ((Proposition #:auto? #f)
    "设" (tu0 $A $join $0) "和" (tu0 $A $meet $1)
    "都是半格, 那么" (tu0 $A $join $meet $0 $1)
    "是一个格当且仅当" (Em "吸收律")
    (MB (&cm (&= (&meet $a (@join $a $b)) $a)
             (&= (&join $a (@meet $a $b)) $a)))
    "对于所有" (∈ $a $b $A) "成立.")
   ((proof)
    "设吸收律成立, 那么" (&= (&join $a $b) $b) "可以推出"
    (&= (&meet $a $b) (&meet $a (@join $a $b)) $a)
    ", 反之亦然. 也就是说, 两个" $A "上的偏序是相合的. "
    "相反方向的证明是容易的.")
   ((tcomment)
    "这个命题本身的陈述稍有些模糊. 实际上, "
    (tu0 $A $join $meet $0 $1)
    "是一个格的更为精确的表述为由"
    (tu0 $A $join $0) "导出的偏序和"
    (tu0 $A $meet $1) "导出的偏序是相同的. "
    "然而, 这里的导出的含义和前面的定理又不完全相同. "
    "我说的是" (tu0 $A $meet $1)
    "的情形, 此时应该定义" (&<= $a $b)
    "为" (&= (&meet $a $b) $a)
    ". 若是仍然死板地按照之前的定理, "
    "那么原文也有提到, 这两种导出偏序应该恰好相反.")
   (P "因此, 我们对于格的形式定义为"
      (Q "一个集合带有两个二元运算" $join "和"
         $meet ", 以及两个突出元素" $0 "和" $1
         ", 其满足" $join "是结合的交换的幂等的, "
         "且" $0 "是其单位元, " $meet
         "也是结合的交换的幂等的, 且"
         $1 "是其单位元, 另外" $join
         "和" $meet "还满足吸收律."))
   ((Remark #:auto? #f)
    
    )
   ((Definition)
    "在绝大多数我们将会遇到的格里, 运算"
    $join "和" $meet "还会满足一个额外的等式, 即"
    (Em "分配律")
    (MB (&= (&meet $a (@join $b $c))
            (&join (@meet $a $b)
                   (@meet $a $c))))
    "如果我们将" $meet "想成乘法而" $join
    "想成加法, 那么这就是通常算术的分配律.")
   ((Lemma #:auto? #f)
    "如果一个格中分配律成立, 那么其对偶也成立, 即等式"
    (MB (&= (&join $a (@meet $b $c))
            (&meet (@join $a $b)
                   (@join $a $c)))))
   ((proof)
    
    )
   ((Proposition)
    "令" (&cm $a $b $c) "是某个分配格" $A
    "的三个元素, 那么至多存在一个" (∈ $x $A)
    "满足" (&= (&meet $x $a) $b) "且"
    (&= (&join $x $a) $c) ".")
   ((proof)
    "设" $x "和" $y "均满足这个条件, 那么"
    )
   (P "在任何格中, 一个元素" $x "若满足"
      (&= (&meet $x $a) $0) "且"
      (&= (&join $x $a) $1) "则被称为"
      $a "的一个" (Em "补")
      ". 这个命题告诉我们分配格中补若存在则唯一. "
      "一个" (Em "Boole代数") "是一个分配格" $A
      ", 其装备了一个额外的幺元运算"
      (func $complement $A $A) "使得"
      (&complement $a) "是" $a
      "的补. 既然" $complement
      "由定义中的其他数据唯一确定, "
      "那么可以推得Boole代数之间的任何格同态"
      (func $f $A $B) "实际上都是Boole代数同态, 即"
      $f "与" $complement "可以交换.")
   ((tcomment)
    "如果" (func $f $A $B) "是两个Boole代数之间的格同态, 并且"
    (∈ $a $b $A) "满足" (&= (&join $a $b) $1_A)
    "和" (&= (&meet $a $b) $0_A) ", 那么"
    (&= (&join (app $f $a) (app $f $b)) $1_B) "且"
    (&= (&meet (app $f $a) (app $f $b)) $0_B)
    ". 换言之, " (&= $a (&complement $b)) "而"
    (&= (app $f $a) (&complement (app $f $b))) ", 亦即"
    (&= (app $f (&complement $b))
        (&complement (app $f $b))) ".")
   ((Example)
    "是时候举一些例子了."
    (Ol #:attr* '((type "a"))
        (Li "对于任意的集合" $X ", 幂集" (powerset $X)
            "是一个格, 其中" $<= "被解释为包含关系, "
            $join "和" $meet "分别是子集的并和交, "
            $0 "和" $1 "分别是空集和整个" $X
            ". 而且, " (powerset $X)
            "是分配的, 因为对于" $X "的子集"
            (&cm $A $B $C) ", 我们有"
            
            )
        )
    )
   ((Definition)
    "接下来, 我们要草绘Boole代数和Boole环之间的等价性. "
    "在任意的Boole代数" $A "之中, 我们定义"
    (Em "对称差") "运算" $+ "为"
    (MB (&= (&+ $a $b)
            (&join (@meet $a (&complement $b))
                   (@meet $b (&complement $a)))) "."))
   ((Lemma #:auto? #f)
    "分配律"
    (&= (&meet $a (@+ $b $c))
        (&+ (@meet $a $b) (@meet $a $c)))
    "成立.")
   ((proof)
    
    )
   (P "我们将对于结合律"
      (MB (&= (&+ $a (@+ $b $c))
              (&+ (@+ $a $b) $c)))
      "的验证留给读者作为练习. 现在对于任意的"
      $a ", 我们有"
      (MB (&= (&+ $a $a)
              (&join (@meet $a (&complement $a))
                     (@meet $a (&complement $a)))
              (&join $0 $0)
              $0))
      "以及"
      (MB (&= (&+ $a $0)
              (symdiff $a $0)
              (&join $a $0)
              $a))
      "因此, " (tu0 $A $+ $0) "是一个群 "
      "(而且, 根据" $+ "的定义, 这个群显然是交换的), "
      (tu0 $A $+ $meet $0 $1)
      "是一个含幺交换环. "
      "{译注: 这当然也是幂等环.}")
   ((Definition)
    "反过来, 令" $A "是一个带有幺元" $1
    "的环, 且满足" (&= $a^2 $a)
    " (此时我们称" $A "为一个Boole环), 那么")
   ((Lemma #:auto? #f)
    (Ol #:attr* '((type "i"))
        (Li $A "是交换环.")
        (Li "对于每个" (∈ $a $A) ", "
            (&= (&+ $a $a) $0) ".")))
   ((tcomment)
    "之前受到Halmos误导, 他将满足" (&= (&+ $a $a) $0)
    "的环称为特征为二的环. 现在我想了一下, "
    "实际上特征为二这个性质在含幺环上才有意义. "
    "不过, 的确对于含幺环而言, "
    (&= (&+ $a $a) $0) "和" (&= (&+ $1 $1) $0)
    "是等价的. 当然, 鉴于两者涵义的确并不完全相同, "
    "所以我想还是不要混淆为好.")
   ((proof)
    
    )
   (P "故乘性结构" (tu0 $A $d* $1)
      "是一个半格 (换言之, 幂等交换幺半群), "
      "其偏序被定义为" (&<= $a $b) "当且仅当"
      (&= (&i* $a $b) $a)
      ". {译注: 原文要读者参考之前的定理, "
      "不过实际上是之前定理的对偶版本.} "
      "并且, 显然" $0 "在此偏序之下是"
      $A "中的最小元素.")
   (P "现在考虑" (&+ $a $b (&i* $a $b)) ". 我们有"
      (MB (&= (&i* $a (@+ $a $b (&i* $a $b)))
              (&+ $a (&i* $a $b) (&i* $a $b))
              $a))
      "以及"
      (MB (&= (&i* $b (@+ $a $b (&i* $a $b)))
              (&+ (&i* $b $a) $b (&i* $a $b))
              $b))
      "于是" (&+ $a $b (&i* $a $b))
      "是" $a "和" $b "的一个上界. "
      "但是, 如果" $c "是" $a "和" $b
      "的一个上界, 那么"
      (MB (&= (&i* (@+ $a $b (&i* $a $b)) $c)
              (&+ (&i* $a $c) (&i* $b $c)
                  (&i* $a $b $c))
              (&+ $a $b (&i* $a $b))))
      "故" (&+ $a $b (&i* $a $b))
      "实际上是最小上界. 如果我们定义"
      (&join $a $b) "为" (&+ $a $b (&i* $a $b))
      ", 那么我们就有了一个格结构"
      (tu0 $A $join $d* $0 $1)
      ". 而且, 根据类似于引理1.8的论证, "
      "我们可以验证" $d* "对于" $join
      "分配; 并且, 也很容易验证"
      (&+ $1 $a) "是" $a "的一个补. "
      "因此, " $A "是一个布尔代数.")
   (P "这个布尔代数中的对称差运算是什么? 我们有"
      
      )
   ((theorem)
    "布尔代数的范畴同构于布尔环的范畴.")
   ((Definition)
    "另一类我们将会遇到的格是Heyting代数. "
    "为了引入这个概念, 令" $a "和" $b
    "是某个布尔代数的元素, 并考虑元素"
    (arr $a $b) ", 我们有"
    (&<= $c (arr $a $b)) "可以推出"
    (MB (deriv0
         (&meet $c $a)
         $<=
         (&meet $a (@arr $a $b))
         $=
         (&join (@meet $a (&complement $a))
                (@meet $a $b))
         $=
         (&join $0 (@meet $a $b))
         $=
         (&meet $a $b)
         $<= $b))
    "反过来, " (&<= (&meet $c $a) $b) "可以推出"
    (MB (deriv0
         (arr $a $b)
         $>=
         (&join (&complement $a)
                (@meet $c $a))
         $=
         (&meet (@join (&complement $a) $c)
                (@join (&complement $a) $a))
         $=
         (&meet (@join (&complement $a) $c) $1)
         $>= $c))
    "因此, " (arr $a $b) "是满足"
    (&<= (&meet $c $a) $b) "的元素"
    $c "中最大的那个. 一个格" $A
    "被称为是一个" (Em "Heyting代数")
    ", 如果对于每对元素" (tu0 $a $b)
    ", 存在一个元素" (&-> $a $b)
    "使得" (&<= $c (@-> $a $b))
    "当且仅当" (&<= (&meet $c $a) $b) ".")
   ((lemma)
    "令" $A "是一个格, " $-> "是" $A
    "上的一个二元运算, 那么" $->
    "使得" $A "成为一个Heyting代数当且仅当"
    (Ol #:attr* '((type "i"))
        (Li (&= (&-> $a $a) $1))
        (Li (&= (&meet $a (@-> $a $b))
                (&meet $a $b)))
        (Li (&= (&meet $b (@-> $a $b)) $b))
        (Li (&= (&-> $a (@meet $b $c))
                (&meet (@-> $a $b)
                       (@-> $a $c)))))
    "对于所有的" (∈ $a $b $c $A) "成立.")
   ((proof)
    "设这些等式都成立, 那么如果"
    )
   (H3. "理想和滤子")
   (P "将布尔代数和特定的环等同起来允许我们将环论的想法导入布尔代数, "
      "以及更为一般的格论. 本节我们将检视理想和素理想的格论类似物.")
   ((Definition)
    "join半格" $A "的一个子集" $I "被称为是一个"
    (Em "理想") ", 如果"
    (Ol #:attr* '((type "i"))
        (Li $I "是" $A "的一个子join半格. 换言之, "
            (∈ $0 $I) ", 并且" (∈ $a $b $I)
            "可以推出" (∈ (&join $a $b) $I) ".")
        (Li $I "是一个下集, 即" (∈ $a $I) "和"
            (&<= $b $a) "可以推出" (∈ $b $I) ".")))
   ((Example #:auto? #f)
    "对于任意的" (∈ $a $A) ", "
    (&= (&darr $a) (setI (∈ $b $A) (&<= $b $a)))
    "是" $A "的一个理想. 并且, 这也显然是包含" $a
    "的最小理想. 因此, 类比于环论, 我们将其称为由"
    $a "生成的" (Em "主理想") ".")
   ((Lemma #:auto? #f)
    (Ol #:attr* '((type "i"))
        (Li "令" (func $f $A $B) "是一个半格同态, 那么"
            $f "的核" (setI (∈ $a $A) (&= (app $f $a) $0))
            "是" $A "的一个理想.")
        (Li "令" $I "是join半格" $A "的一个理想, "
            "那么存在一个核为" $I "的半格同态"
            (func $f $A $B) ".")
        (Li "如果ii中的" $A "实际上是一个分配格, "
            "那么" $B "可以是分配格而" $f
            "是一个格同态.")))
   ((proof)
    
    )
   (H3. "一些范畴概念")
   (H3. "自由格")
   (H2. "locale导引")
   (H3. "frame和locale")
   (H2. "紧Hausdorff空间")
   (H2. "连续实值函数")
   (H2. "环的表示")
   (H2. "profiniteness和对偶")
   (H2. "连续格")
   ))