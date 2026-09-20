#lang racket
(provide semantics_of_type_systems.html)
(require SMathML)
(define $== (Mo "=="))
(define $euclid (Mi "euclid"))
(define (&euclid a b)
  (appl $euclid a b))
(define $mod (Mi "mod"))
(define (&mod a b)
  (appl $mod a b))
(define $div (Mo "div"))
(define $wand (Mo "&minus;&#8270;"))
(define split2 (&split 2))
(define (If a b c)
  (split2 $if a $then b $else c))
(define-infix*
  (&== $==)
  (&div $div)
  (&wand $wand))
(define-@lized-op*
  (@div &div))
(define semantics_of_type_systems.html
  (TnTmPrelude
   #:title "类型系统的语义"
   #:css "styles.css"
   (H1. "类型系统的语义")
   (H2. "简单类型lambda演算")
   (H2. "System F: 多态和存在类型")
   (H2. "递归类型")
   (H2. "可变状态")
   (H2. "程序逻辑")
   (P "对于到目前为止的每一种语言, "
      "我们都是先定义其操作语义, "
      "然后利用操作语义来推理该语言中的项. "
      "现在我们转向另一种方法, "
      "有时被称为公理语义: "
      "公理语义以程序逻辑的形式为程序赋予意义, "
      "即一组可以用来对程序进行模块化推理的规则.")
   (P (B "为什么需要公理语义? ")
      "与基于操作语义和类型的语言模型不同, "
      "公理语义旨在实现复合式的程序验证. "
      "例如, 考虑验证用于计算最大公约数的Euclid算法:"
      (eqn*
       ((&euclid $a $b)
        $:= (If (&== $b $0) $a
                (&euclid $b (&mod $a $b))))
       ((&mod $a $b)
        $:= (&- $a (&* (@div $a $b) $b))))
      )
   (H3. "Hoare逻辑")
   (H3. "分离逻辑")
   (H2. "iris")
   (P "我们已经看到了如何用程序逻辑来验证简单的程序. "
      "遗憾的是, 上一章中的分离逻辑相当有限: "
      "(1) 我们不支持(可能)无限的循环, "
      "(2) 我们不支持所有权共享, 是"
      "(3) 我们必须手动完成每一个证明步骤, "
      "(4) 我们不支持并发. "
      "为了验证更大, 更复杂的程序, "
      "我们现在转向一种强大得多且更易用的分离逻辑: Iris [6].")
   (P "Iris在第5.2节的分离逻辑基础上扩展了若干联结词:"
      
      )
   (H3. "魔杖")
   
   (H2. "逻辑关系")
   (H2. "幽灵状态")
   (H2. "iris的模型")
   (H2. "并发")
   ))