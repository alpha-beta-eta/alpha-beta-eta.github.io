#lang racket
(provide useless.html)
(require SMathML "utils.rkt")
(define useless.html
  (Prelude
   #:title "一些没用的笔记"
   #:css "styles.css"
   (H1 "一些没用的笔记")
   (columnize
    (TITLE "语义工程和PLT Redex笔记" "sewpr.html")
    #;
    (TITLE "Miscellaneous Notes" "misc.html")
    (TITLE "可计算性和计算复杂度" "jones.html")
    (TITLE "证明与类型" "prot.html")
    #;
    (TITLE "分析笔记" "analysis_notes.html")
    (TITLE "线性代数笔记" "linear_algebra_notes.html")
    #;
    (TITLE "组合学笔记" "combinatorics_notes.html")
    (TITLE "绘制数学图形" "illustrations.html")
    #;
    (TITLE "计算机辅助设计的几何编程" "cad.html")
    (TITLE "Topos: 逻辑的范畴分析" "topos.html")
    (TITLE "Abel定理" "abel.html")
    (TITLE "数学家学法语" "french.html")
    #;
    (TITLE "Geometric Algebra (E. Artin) 翻译" "geometric_algebra.html")
    (TITLE "函数式编程的domain论基础" "fp_domain.html")
    (TITLE "结构证明论" "spt.html")
    #;
    (TITLE "OPLSS" "oplss.html")
    (TITLE "归纳定义导论" "induc.html")
    #;
    (TITLE "元对象协议艺术" "mop.html")
    (TITLE "Lem编辑器使用笔记" "lem_editor.html")
    (TITLE "部分求值" "pe_jones.html")
    (TITLE "SICM笔记" "sicm.html")
    (TITLE "数据结构和算法笔记" "dsaa.html")
    #;
    (TITLE "线性逻辑: 其句法和语义" "synsem.html")
    #;
    (H2 (A $lambda "演算: 其句法和语义" #:attr* '((href "lambda.html"))))
    #;
    (TITLE "灵活软件设计" "sdf.html")
    #;
    (TITLE "函数式微分几何" "fdg.html")
    (TITLE "盲点" "blind.html")
    #;
    (TITLE "高阶范畴逻辑导论" "hocl.html")
    (TITLE "代数: 第0章" "algebra.html")
    (TITLE "Common Lisp面向对象编程" "clos.html")
    #;
    (TITLE "游戏设计" "game_design.html")
    #;
    (TITLE "并行和顺序算法" "cmu15210.html")
    #;
    (TITLE "命令式编程原理笔记" "cmu15122.html")
    (TITLE "同伦类型论" "hott.html")
    #;
    (TITLE "格论笔记" "lattice_theory_notes.html")
    (TITLE "关于宏的笔记" "macro_notes.html")
    #;
    (TITLE "证明论和逻辑复杂度" "proof_theory.html")
    #;
    (TITLE "Kerodon翻译" "kerodon.html")
    #;
    (TITLE "范畴逻辑和类型论" "cltt.html")
    (TITLE "逻辑学习指南" "logic.html")
    #;
    (TITLE "句法闭包" "synclo.html")
    #;
    (TITLE "卫生宏技术" "macro.html")
    #;
    (TITLE "Core War介绍" "corewar.html")
    (TITLE "理解Maxima" "maxima.html")
    (TITLE "Scheme的三种实现模型" "timp.html")
    #;
    (TITLE "把东西粘在一起" "scheme_cg.html")
    #;
    (TITLE "Standard ML的历史" "sml-history.html")
    #;
    (TITLE "构建问题解决器" "bps.html")
    #;
    (TITLE "乌龟几何" "tg.html")
    #;
    (TITLE "操作语义的结构方法" "sos.html")
    #;
    (TITLE "指称语义学" "ds.html")
    (TITLE "LearningZIL笔记" "zil.html")
    #;
    (TITLE "Computation Structures笔记" "computation_structures.html")
    (TITLE "协调抽象和高性能: MetaOCaml方法" "metaocaml.html")
    #;
    (TITLE "Zork论文翻译" "zork.html")
    #;
    (TITLE "高效多项式计算" "polynomial_computation.html")
    (TITLE "测度论" "measure.html")
    #;
    (TITLE "古典AI笔记" "classical_ai.html")
    (TITLE "Stone空间笔记" "stone.html")
    (TITLE "无点拓扑笔记" "pointless.html")
    (TITLE "REFAL" "refal.html")
    #;
    (TITLE "AIM和AITR考古" "aimtr.html")
    #;
    (TITLE "自动微分的简单本质翻译" "simple-ad.html")
    (TITLE "线性逻辑笔记" "linear_logic_notes.html")
    #;
    (TITLE "定性表示笔记" "qualitative_representations.html")
    (TITLE "布尔代数导引" "boolean.html")
    #;
    (TITLE "游戏语义" "game_semantics.html")
    #;
    (TITLE "PiHKAL翻译" "pihkal.html")
    #;
    (TITLE "延续和自然语言" "continuation.html")
    (TITLE "离散微分几何笔记" "cmu15458.html")
    (TITLE "范畴逻辑引论" "catlog.html")
    (TITLE "隐函数定理" "implicit.html")
    #;
    (TITLE "集合论" "set_theory.html")
    #;
    (TITLE "没有(太多)眼泪的Gödel" "godel.html")
    (TITLE "对于宏的恐惧" "fear-of-macros.html")
    (TITLE "lambda到SKI" "ski.html")
    (TITLE "有一个作用" "having_an_effect.html")
    (TITLE "抽象解释 (翻译)" "abstract.html")
    (TITLE "CMU课程笔记" "cmu.html")
    (TITLE "音乐的Haskell学派" "haskell_music.html")
    (TITLE "编程语言中的控制结构" "control.html")
    (TITLE "Edinburgh LCF" "lcf.html")
    (TITLE "如何编写金融合约" "contract.html")
    (TITLE "编程习题集" "exercises_in_programming.html")
    
    )))