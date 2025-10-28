#lang racket
(provide useless.html)
(require SMathML "colorfulize.rkt")
(define useless.html
  (Prelude
   #:title "一些没用的笔记"
   #:css "styles.css"
   (H1 "一些没用的笔记")
   (columnize
    (H2 (A "摆弄SICP图形语言" #:attr* '((href "picture.html"))))
    (H2 (A "The Ship of Theseus" #:attr* '((href "Ship-of-Theseus.html"))))
    (H2 (A "语义工程和PLT Redex笔记" #:attr* '((href "sewpr.html"))))
    #;
    (H2 (A "Miscellaneous Notes" #:attr* '((href "misc.html"))))
    (H2 (A "可计算性和计算复杂度" #:attr* '((href "jones.html"))))
    (H2 (A "证明与类型" #:attr* '((href "prot.html"))))
    #;
    (H2 (A "分析笔记" #:attr* '((href "analysis_notes.html"))))
    (H2 (A "线性代数笔记"
           #:attr* '((href "linear_algebra_notes.html"))))
    #;
    (H2 (A "组合学笔记" #:attr* '((href "combinatorics_notes.html"))))
    (H2 (A "绘制数学图形" #:attr* '((href "illustrations.html"))))
    #;
    (H2 (A "计算机辅助设计的几何编程" #:attr* '((href "cad.html"))))
    #;
    (H2 (A "Topos: 逻辑的范畴分析" #:attr* '((href "topos.html"))))
    (H2 (A "Abel定理" #:attr* '((href "abel.html"))))
    (H2 (A "数学家学法语" #:attr* '((href "french.html"))))
    #;
    (H2 (A "Geometric Algebra (E. Artin) 翻译"
           #:attr* '((href "geometric_algebra.html"))))
    (H2 (A "函数式编程的domain论基础" #:attr* '((href "fp_domain.html"))))
    (H2 (A "结构证明论" #:attr* '((href "spt.html"))))
    #;
    (H2 (A "OPLSS" #:attr* '((href "oplss.html"))))
    (H2 (A "归纳定义导论" #:attr* '((href "induc.html"))))
    #;
    (H2 (A "元对象协议艺术" #:attr* '((href "mop.html"))))
    (H2 (A "Lem编辑器使用笔记" #:attr* '((href "lem_editor.html"))))
    (H2 (A "部分求值" #:attr* '((href "pe_jones.html"))))
    (H2 (A "SICM笔记" #:attr* '((href "sicm.html"))))
    (H2 (A "数据结构和算法笔记" #:attr* '((href "dsaa.html"))))
    #;
    (H2 (A "线性逻辑: 其句法和语义" #:attr* '((href "synsem.html"))))
    #;
    (H2 (A $lambda "演算: 其句法和语义" #:attr* '((href "lambda.html"))))
    #;
    (H2 (A "灵活软件设计" #:attr* '((href "sdf.html"))))
    #;
    (H2 (A "函数式微分几何" #:attr* '((href "fdg.html"))))
    (H2 (A "盲点" #:attr* '((href "blind.html"))))
    #;
    (H2 (A "高阶范畴逻辑导论" #:attr* '((href "hocl.html"))))
    (H2 (A "代数: 第0章" #:attr* '((href "algebra.html"))))
    (H2 (A "Common Lisp面向对象编程" #:attr* '((href "clos.html"))))
    #;
    (H2 (A "游戏设计" #:attr* '((href "game_design.html"))))
    #;
    (H2 (A "并行和顺序算法" #:attr* '((href "cmu15210.html"))))
    #;
    (H2 (A "命令式编程原理笔记" #:attr* '((href "cmu15122.html"))))
    (H2 (A "同伦类型论" #:attr* '((href "hott.html"))))
    #;
    (H2 (A "格论笔记" #:attr* '((href "lattice_theory_notes.html"))))
    (H2 (A "关于宏的笔记" #:attr* '((href "macro_notes.html"))))
    #;
    (H2 (A "证明论和逻辑复杂度" #:attr* '((href "proof_theory.html"))))
    #;
    (H2 (A "Kerodon翻译" #:attr* '((href "kerodon.html"))))
    #;
    (H2 (A "范畴逻辑和类型论" #:attr* '((href "cltt.html"))))
    (H2 (A "逻辑学习指南" #:attr* '((href "logic.html"))))
    #;
    (H2 (A "句法闭包" #:attr* '((href "synclo.html"))))
    #;
    (H2 (A "卫生宏技术" #:attr* '((href "macro.html"))))
    #;
    (H2 (A "Core War介绍" #:attr* '((href "corewar.html"))))
    (H2 (A "理解Maxima" #:attr* '((href "maxima.html"))))
    (H2 (A "Scheme的三种实现模型" #:attr* '((href "timp.html"))))
    #;
    (H2 (A "把东西粘在一起" #:attr* '((href "scheme_cg.html"))))
    #;
    (H2 (A "Standard ML的历史" #:attr* '((href "sml-history.html"))))
    #;
    (H2 (A "构建问题解决器" #:attr* '((href "bps.html"))))
    #;
    (H2 (A "乌龟几何" #:attr* '((href "tg.html"))))
    #;
    (H2 (A "操作语义的结构方法" #:attr* '((href "sos.html"))))
    #;
    (H2 (A "指称语义学" #:attr* '((href "ds.html"))))
    (H2 (A "LearningZIL笔记" #:attr* '((href "zil.html"))))
    #;
    (H2 (A "Computation Structures笔记"
           #:attr* '((href "computation_structures.html"))))
    (H2 (A "协调抽象和高性能: MetaOCaml方法"
           #:attr* '((href "metaocaml.html"))))
    #;
    (H2 (A "Zork论文翻译" #:attr* '((href "zork.html"))))
    (H2 (A "monad的Schemer之见" #:attr* '((href "monad.html"))))
    #;
    (H2 (A "高效多项式计算" #:attr*
           '((href "polynomial_computation.html"))))
    (H2 (A "测度论" #:attr* '((href "measure.html"))))
    #;
    (H2 (A "古典AI笔记" #:attr* '((href "classical_ai.html"))))
    (H2 (A "Stone空间笔记" #:attr* '((href "stone.html"))))
    (H2 (A "无点拓扑笔记" #:attr* '((href "pointless.html"))))
    (H2 (A "REFAL" #:attr* '((href "refal.html"))))
    #;
    (H2 (A "AIM和AITR考古" #:attr* '((href "aimtr.html"))))
    #;
    (H2 (A "自动微分的简单本质翻译" #:attr* '((href "simple-ad.html"))))
    (H2 (A "线性逻辑笔记" #:attr* '((href "linear_logic_notes.html"))))
    #;
    (H2 (A "定性表示笔记" #:attr* '((href "qualitative_representations.html"))))
    (H2 (A "布尔代数导引" #:attr* '((href "boolean.html"))))
    #;
    (H2 (A "游戏语义" #:attr* '((href "game_semantics.html"))))
    #;
    (H2 (A "PiHKAL翻译" #:attr* '((href "pihkal.html"))))
    #;
    (H2 (A "延续和自然语言" #:attr* '((href "continuation.html"))))
    (H2 (A "离散微分几何笔记" #:attr* '((href "cmu15458.html"))))
    (H2 (A "范畴逻辑引论" #:attr* '((href "catlog.html"))))
    (H2 (A "隐函数定理" #:attr* '((href "implicit.html"))))
    #;
    (H2 (A "集合论" #:attr* '((href "set_theory.html"))))
    #;
    (H2 (A "没有(太多)眼泪的Gödel" #:attr* '((href "godel.html"))))
    (H2 (A "对于宏的恐惧" #:attr* '((href "fear-of-macros.html"))))
    (H2 (A "lambda到SKI" #:attr* '((href "ski.html"))))
    (H2 (A "有一个作用" #:attr* '((href "having_an_effect.html"))))
    
    )
   ))