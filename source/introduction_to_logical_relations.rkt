#lang racket
(provide introduction_to_logical_relations.html)
(require SMathML)
(define $int (Mi "int"))
(define (∃ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $exists (apply &cm x*) $. (car P*))))
(define (∀ . x*)
  (let-values (((x* P*) (split-at-right x* 1)))
    (: $forall (apply &cm x*) $. (car P*))))
(define $!- (Mo "&vdash;"))
(define (!- . x*)
  (let-values (((a* b*) (split-at-right x* 1)))
    (: (apply &cm a*) $!- (car b*))))
(define (G!- . x*)
  (apply !- Γ x*))
(define (RLabel x)
  (Mtext #:attr* '((class "small-caps")) x))
(define (&rull label . x*)
  (if label
      (: (apply &rule x*) label)
      (apply &rule x*)))
(define App (&split 2))
(define introduction_to_logical_relations.html
  (TnTmPrelude
   #:title "逻辑关系引论"
   #:css "styles.css"
   (H1. "逻辑关系引论")
   (H2. "引论")
   (P "逻辑关系这一术语源自Gordon Plotkin于1973年撰写的备忘录"
      (Em "Lambda-definability and logical relations")
      ". 然而, 这一证明方法的精神可以追溯到William W. Tait, "
      "他在1967年使用该方法证明了System T的强规范化性质.")
   (P "名称是一件有趣的事情. 当我说" (Q "椅子")
      "时, 你脑海中会立刻浮现出一把椅子的画面. 如果我说"
      (Q "桌子") ", 你就会想象一张桌子. "
      "你之所以会这样做, 是因为我们用"
      (Q "椅子") "来指代椅子, 用" (Q "桌子")
      "来指代桌子, 但我们完全可以用" (Q "长颈鹿")
      "来表示椅子, 用" (Q "佛陀")
      "来表示桌子. 当我们遇到一个由已知词汇组合而成的新词时, "
      "很自然地会尝试通过组合各组成部分的含义来推断其意思. "
      "假设我们第一次遇到" (Q "桌布 (tablecloth)")
      "这个词, 如果我们知道" (Q "桌子 (table)") "和"
      (Q "布 (cloth)") "分别指代什么, "
      "就可以猜到它是一块铺在桌子上的布. "
      "然而, 这种方法并不总是奏效. 例如, "
      (Q "摩天大楼 (skyscraper)")
      "并不是一个用来刮擦天空的刮刀 (scraper). "
      "逻辑关系也是如此, 试图从名称的各个组成部分中寻找含义可能是一种徒劳. "
      "逻辑关系确实是关系, 所以名称中的这一部分是合理的. "
      "它们的定义方式也与逻辑有些许相似之处, "
      "但仅仅从名称的各个部分来理解逻辑关系并不能帮助你真正理解它们. "
      "一个更贴切的名称或许是"
      (Q "类型索引归纳关系 (Type Indexed Inductive Relations)")
      ". 不过, " (Q "逻辑关系") "已经是一个广为接受的名称, "
      "而且更容易说出口, 所以我们将沿用它 "
      "(毕竟没有人会接受用" (Q "长颈鹿") "来指代椅子).")
   (P "本文的大部分内容基于Amal Ahmed在2015年俄勒冈编程语言暑期学校上的系列讲座.")
   (H3. "简单类型lambda演算")
   (P "我们用来呈现逻辑谓词和逻辑关系的语言是简单类型lambda演算 (STLC). "
      "在第一节中, 它将以其基本形式使用, "
      "之后当我们研究新的构造和特性时, 它将作为基础语言使用. "
      "在后文中, 我们将默认省略说明所扩展的基础语言是STLC. "
      "STLC的定义见图1.")
   (P "对于不熟悉推理规则的读者: 一条规则"
      (MB (&rule $A $B
                 (&conj $A $B)))
      "读作如果" $A "和" $B
      "的确如此, 那么我们就可以得出" (&conj $A $B)
      ". 这意味着应用的定型规则"
      (MB (&rull (RLabel "T-App")
                 (G!- (&: $e_1 (&-> $tau_2 $tau)))
                 (G!- (&: $e_2 $tau_2))
                 (G!- (&: (App $e_1 $e_2) $tau))))
      "是说当" $e_2 "在定型上下文" Γ
      "下有着类型" $tau_2 "而" $e_1 "在" Γ
      "下有着类型" (&-> $tau_2 $tau)
      "时应用" (App $e_1 $e_2)
      "有着类型" $tau ".")
   (H3. "逻辑关系")
   (P "逻辑关系是一种证明方法, 可以用来证明"
      "以特定编程语言写成的程序的性质. "
      "编程语言性质的证明通常以"
      "定型或者求值判断上的归纳进行. "
      "逻辑关系添加了非直接的层次, "
      "通过构造具有我们所感兴趣的性质的程序的集合. "
      "{译注: 这是幺元关系, 或者说谓词.} "
      "接下来我们将会更仔细地检视这一点. "
      "作为动机, 以下是可由逻辑关系证明的诸多性质:"
      (Ul (Li "终止性 (强规范化)")
          (Li "类型安全性")
          (Li "程序等价"
              (Ul (Li "程序正确性")
                  (Li "表示独立性")
                  (Li "参数性和免费定理, 例如"
                      (MB (&: $f (∀ $alpha (&-> $alpha $alpha))))
                      "该程序无法检视" $alpha
                      ", 因为其不知道它将会是哪个类型, 于是"
                      $f "必然是恒等函数."
                      (MB (∀ $alpha (&-> $int $alpha)))
                      
                      ))
              )
          )
      )
   (H3. "逻辑关系的范畴")
   (H2. "简单类型lambda演算的规范化")
   (H3. "STLC的强规范化")
   (H3. "练习")
   (H2. "STLC的类型安全性")
   (H3. "类型安全" -- "经典处理")
   (H3. "类型安全" -- "使用逻辑谓词")
   (H3. "练习")
   (H2. "全称类型和关系替换")
   (H3. "System F (具有全称类型的STLC)")
   (H3. "System F的性质: 免费定理")
   (H3. "上下文等价")
   (H3. "System F的一种逻辑关系")
   (H3. "可计算性引理")
   (H3. "练习")
   (H2. "存在类型")
   (H3. "具有存在类型的STLC")
   (H3. "例子")
   (H3. "逻辑关系")
   (H2. "递归类型和步骤索引")
   (H3. "具有递归类型的简单类型lambda演算")
   (H3. "一种步骤索引逻辑谓词")
   (H3. "练习")
   (H2. "引用和世界")
   (H3. "具有引用的STLC")
   (H3. "具有引用的STLC之性质")
   (H3. "逻辑谓词")
   (H3. "安全性证明")
   (H3. "练习")
   (H3. "深入阅读")
   (H2 "附录A: Landin之结")
   ))