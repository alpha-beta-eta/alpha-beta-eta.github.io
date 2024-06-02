#lang racket
(provide sos.html)
(require SMathML)
(define $forall (Mo "&forall;"))
(define $exists (Mo "&exist;"))
(define $. (Mo "."))
(define $->* (^ $-> $*))
(define (&->* a b)
  (Mrow a $->* b))
(define $sc (Mo ";"))
(define $m^^ (&Prime $m))
(define-infix*
  (!- $entailL)
  (&sc $sc)
  )
(define &exists
  (case-lambda
    ((x p) (Mrow $exists x $. p))
    ((x y . arg*)
     (Mrow $exists x $. (apply &forall y arg*)))))
(define &forall
  (case-lambda
    ((x p) (Mrow $forall x $. p))
    ((x y . arg*)
     (Mrow $forall x $. (apply &forall y arg*)))))
(define $nrarr (Mo "&nrarr;"))
(define (&nrarr a b)
  (Mrow a $nrarr b))
(define (&->l a op b)
  (Mrow a (^^ $-> op) b))
(define (kleene cat)
  (Msup cat $*))
(define $!-* (kleene $entailL))
(define (&!-* a b)
  (Mrow a $!-* b))
(define sos.html
  (TnTmPrelude
   #:title "操作语义的结构方法"
   #:css "styles.css"
   (H1 "操作语义的结构方法")
   (H2 "第1章 转换系统与解释自动机")
   ((definition #:n "1")
    "一个转换系统 (ts) 仅仅是一个结构" (tupa0 $Gamma:normal $->0)
    ", 其中" $Gamma:normal "是配置" $gamma "的集合, "
    (&sube $->0 (&c* $Gamma:normal $Gamma:normal))
    "被称为转换关系. " (&-> $gamma $gamma^)
    "应该被读作从配置" $gamma "到配置" $gamma^ "的转换.")
   ((definition #:n "2")
    "一个终止转换系统 (tts) 是一个结构" (tupa0 $Gamma:normal $->0 $T)
    ", 其中" (tupa0 $Gamma:normal $->0) "是一个ts, 而终配置集"
    (&sube $T $Gamma:normal) "满足"
    (&forall (&in $y $T) (&in $y^ $Gamma:normal) (&nrarr $y $y^)) ".")
   ((definition #:n "6")
    "一个标签转换系统 (lts) 是一个结构" (tupa0 $Gamma:normal $A $->0)
    ", 其中" $Gamma:normal "是配置的集合, " $A "是动作 (或者说标签或操作) 的集合, 并且"
    (&sube $->0 (&c* $Gamma:normal $A $Gamma:normal)) "是转换关系. 我们将一个转换记成"
    (&->l $gamma $a $gamma^) ", 其中" $gamma "和" $gamma^ "是配置, " $a
    "是动作. 动作给出了转换的内部或外部信息.")
   ((definition)
    "一个有限自动机是五元组" (&= $M (tupa0 $Q $Sigma:normal $delta $q_0 $F)) ", 其中"
    (Ul (Li $Q "是状态的有限集合")
        (Li $Sigma:normal "是有限的输入字母表")
        (Li (func $delta (&c* $Q $Sigma:normal) (powset $Q))
            "是状态转换关系")
        (Li (&in $q_0 $Q) "是初状态")
        (Li (&sube $F $Q) "是终状态的集合"))
    "为了获得一个转换系统, 我们令"
    (MB (&= $Gamma:normal (&c* $Q (kleene $Sigma:normal))))
    "于是任意的配置" (&= $gamma (tupa0 $q $w))
    "的资料由一个状态分量" $q "和一个控制分量" $w "构成." (Br)
    "对于转换, 每当" (&in $q^ (appl $delta $q $a)) ", 我们置"
    (MB (&entailL (tupa0 $q (: $a $w)) (tupa0 $q^ $w)))
    "有限自动机的行为不过就是其所接受的字符串的集合" (app $L $M) ":"
    (MB (&= (app $L $M)
            (setI (&in $w (kleene $Sigma:normal))
                  (&exists (&in $q $F)
                           (&!-* (tupa0 $q_0 $w) (tupa0 $q $epsilon))))))
    "当然我们也可以定义终配置为"
    (MB (&= $T (setI (tupa0 $q $epsilon) (&in $q $F))))
    "那么"
    (MB (&= (app $L $M)
            (setI (&in $w (kleene $Sigma:normal))
                  (&exists (&in $gamma $T)
                           (&!-* (tupa0 $q_0 $w) $gamma)))))
    "实际上我们可以更抽象一点. 令" (tupa0 $Gamma:normal $->0 $T)
    "是一个tts, 输入函数为" (func (Mi "in") $I $Gamma:normal)
    "而其所接受的语言是" (&sube (app $L $Gamma:normal) $I) ", 其中"
    (MB (&= (app $L $Gamma:normal)
            (setI (&in $i $I)
                  (&exists (&in $gamma $T)
                           (&->* (app (Mi "in") $i) $gamma)))))
    "对于有限自动机而言, 我们取" (&= $I (kleene $Sigma:normal))
    "而" (&= (app (Mi "in") $w) (tupa0 $q_0 $w)) ".")
   ((example #:n "3")
    "机器:"
    (MB (^^ $-> (Mi "start"))
        (^^ $p (^^ (Mo "&curarr;") $1))
        (__^^ (Mo "&LeftArrowRightArrow;") $0 $1)
        (^^ $q (^^ (Mo "&curarr;") $0))
        (^^ $-> $0)
        (^^ $r:bold (^^ (Mo "&curarr;") (&cm $0 $1))))
    "一个转换序列:"
    (MB (!- (tupa0 $p (: $0 $1 $0 $0 $1))
            (tupa0 $q (: $1 $0 $0 $1))
            (tupa0 $p (: $0 $0 $1))
            (tupa0 $q (: $0 $1))
            (tupa0 $r:bold $1)
            (tupa0 $r:bold $epsilon))))
   ((example #:n ". 三计数器机器")
    "我们拥有三个计数器" $C ", 分别为" (&cm $I:normal $J:normal $K:normal)
    ". 而指令" $O ", 具有如下四种形式:"
    (Ul (Li "增量: " (Code "inc" $C ":" $m))
        (Li "减量: " (Code "dec" $C ":" $m))
        (Li "零测: " (Code "zero" $C ":" (&/ $m $n)))
        (Li "停止: " (Code "stop")))
    "程序不过就是指令序列" (&= $P (&cm $O_1 $..h $O_l))
    ". 现在固定" $P ", 配置集为:"
    (MB (&= $Gamma:normal
            (setI (tupa0 $m $i $j $k)
                  (&sc (&<= $1 $m $l)
                       (&in (&cm $i $j $k) $NN)))))
    "转换关系是分类定义的:"
    (Ul (Li "情形II: " (: $O_m $=) (Code "inc" $I:normal ":" $m^)
            (MB (!- (tupa0 $m $i $j $k)
                    (tupa0 $m^ (&+ $i $1) $j $k))))
        (Li "情形ID: " (: $O_m $=) (Code "dec" $I:normal ":" $m^)
            (MB (!- (tupa0 $m (&+ $i $1) $j $k)
                    (tupa0 $m^ $i $j $k))))
        (Li "情形IZ: " (: $O_m $=) (Code "zero" $I:normal ":" (&/ $m^ $m^^))
            (MB (set-attr*
                 (&Table
                  ((!- (tupa0 $m $0 $j $k)
                         (tupa0 $m^ $0 $j $k)))
                  ((!- (tupa0 $m (&+ $i $1) $j $k)
                         (tupa0 $m^^ (&+ $i $1) $j $k))))
                 'columnalign "left"))))
    $J:normal "和" $K:normal "的情形是类似的."
    )
   (H2 "第2章 简单表达式与命令")

   (H2 "第3章 定义与声明")

   (H2 "第4章 函数, 过程与类")
   
   ))