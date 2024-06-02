#lang racket
(provide automated_ch5)
(require (except-in SMathML H3. H4. $!=)
         "automated_utils.rkt")
(define automated_ch5
  (Tm*
   (H2. "可判定问题")
   (H3. "判定问题")
   (H3. "AE片段")
   (H3. "Miniscoping and the monadic fragment")
   (H3. "三段论")
   (H3. "有限模型性质")
   (H3. "量词消去")
   (H3. "Presburger算术")
   (H3. "复数")
   (H3. "实数")
   (H3. "环, 理想和word问题")
   (H3. "Gr&ouml;bner基")
   (H3. "几何定理证明")
   (H3. "将判定过程进行组合")
   (H3. "深入阅读")
   (P "许多逻辑学教材讨论了判定问题. "
      "关于判定问题在逻辑有效性方面可判定与不可判定的情形, "
      "可参见 Börger, Grädel和Gurevich (2001), Ackermann (1954) "
      "以及Dreben和Goldfarb (1979), "
      "此外Hilbert和Ackermann (1950) 对此也有简要论述. "
      "需要注意的是, 判定问题通常从可满足性而非有效性的对偶角度加以处理, "
      "因此在将此类文献与本文的讨论相对照时, 需要在量词前缀中互换"
      $forall "与" $exists "的角色. Rabin (1991) 对可判定理论进行了综述, "
      "其中一些我们已在本章中加以讨论.")
   (P "三段论在诸多逻辑史著作中有详尽讨论, "
      "如Bocheński (1961), Dumitriu (1977), "
      "Kneale和Kneale (1962) 以及Kneebone (1963).")
   (P "文献中还有若干其他数学理论的量词消去结果. "
      "两个较为困难的例子是Abel群理论 (Szmielew 1955) "
      "与Boole代数理论 (Tarski 1949). "
      "Kreisel和Krivine (1971) 有一章专门讨论量词消去, "
      "内容涵盖可分Boole代数理论 (以及作为特例的原子Boole代数). "
      "其他标准模型论教材, 如Chang和Keisler (1992), "
      "Hodges (1993b) 以及Marcja和Toffalori (2003), "
      "也讨论了量词消去及模型完备性, " $o "-极小性等相关概念; "
      "模型完备性的一种表述 (A. Robinson 1963; MacIntyre 1991) 为: "
      "对于理论" $T ", 每个公式都" $T "-等价于一个纯全称公式 "
      "(或者等价地, 纯存在公式). "
      "Ershov, Lavrov, Taimanov和Taitslin (1965) "
      "在文末对量词消去方法成功应用的各类理论进行了综述. "
      "Soloray (私人通信) 也向本书作者介绍了一种适用于"
      "各类实与复向量空间的量词消去过程.")
   (P "Presburger算术及若干相关理论的处理可见于Enderton (1972), "
      "而Smoryński (1980) 则对Presburger与Skolem"
      "的不同量词消去程序作了详细阐述. "
      "该书包含大量相关主题的内容, "
      "其中包括对相应乘法理论的讨论. "
      "Smoryński (1981) 给出了Presburger算术量词消去的一个精彩应用. "
      "Yap (2000) 进一步深入探讨了相关可判定性问题, "
      "并包含许多其他相关材料. "
      "Presburger算术的其他处理方法还包括"
      "Omega测试 (Pugh 1992) 与Williams (1976) 的方法. "
      "Weispfenning (1999) 给出了一种适用于"
      "实数与整数混合线性算术的量词消去过程.")

   (H3. "练习")
   ))
