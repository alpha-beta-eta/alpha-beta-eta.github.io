#lang racket
(provide french.html)
(require SMathML)
(define (Item label . x*)
  (apply P (B label ":") " " x*))
(define french.html
  (TnTmPrelude
   #:title "数学家学法语"
   #:css "styles.css"
   (H1 "数学家学法语: 语言学方法")
   (H3 "为什么数学家应该能够阅读另一种语言?")
   (P "能够以原始语言阅读古老论文和书籍, 尤其是那些具有特别历史重要性的经典论文, 对于数学家而言是一项优势. "
      "尽管那些古老作品里的绝大部分内容都得以消化并融入了经过精心打造的现代理论之中, 在注重效率的教科书之中呈现, "
      "然而当人们阅读最初的一篇论文时还总会发现令人惊讶的元素. "
      )
   (H3 "本课程的目的")
   (P "具有良好英语知识的数学家已经掌握了阅读法语数学文献90%所需的知识, 而本课程的目的在于用很短的时间补充剩下的10%:"
      (Ul (Li "一些重要的词汇;")
          (Li "一些规则用以识别法语中和英语同源的词汇, 并藉此猜测含义. 这需要介绍历史语言学;")
          (Li "一些法语语法和句法的基础知识以分析复杂的句子;")
          (Li "你能用很少的努力来阅读法语数学文献的信心.")))
   (H2 "第一部分. 语言学")
   (H3 "约定")
   (P ""
      )
   (H3 "语言学001")
   (P ""
      )
   (H3 "印欧语系")
   (P ""
      )
   (H3 "英语和法语简史")
   
   (H3 "法语/英语的印欧语系同源词")
   
   (H2 "第二部分. 词汇")
   (H3 "词汇I: 陈述与证明")
   (Item "希腊语同源词" "th&eacute;or&egrave;me = theorem; lemme = lemma; axiome = axiom.")
   (Item "拉丁语同源词" "proposition = proposition; d&eacute;monstration = demonstration; d&eacute;finition = definition; "
         "scholie = scholium; postulat = postulate; conjecture = conjecture.")
   (Item "古法语同源词" "remarque = remark; preuve = proof; esquisse = sketch; exemple = example; contre-exemple = counter-example.")
   (P "多简单啊!")
   (H3 "词汇II: 数学领域的名字")
   (Item "希腊语同源词" "logique = logic; math&eacute;matiques = mathematics; physique = physics; analyse = analysis; "
         "g&eacute;om&eacute;trie = geometry; topologie = topology; arithm&eacute;tique = arithmetic.")
   (Item "拉丁语同源词" "combinatoire = combinatorics; calcul diff&eacute;rentiel = differential calculus; "
         "probabilit&eacute;s = probabilities.")
   (Item "古法语同源词" "math. appliqu&eacute;es = applied math; math. pures = pure math.")
   (Item "其他" "alg&egrave;bre = algebra (来源于阿拉伯语).")
   ((exercise)
    "翻译: th&eacute;orie g&eacute;om&eacute;trique des groupes, g&eacute;om&eacute;trie diff&eacute;rentielle, "
    "combinatoire &eacute;num&eacute;rative.")
   (H3 "词汇III: 初等数学")
   
   (H3 "词汇IV: 代数学")

   (H3 "词汇V: 拓扑学")

   (H3 "词汇VI: 代数拓扑, 同调代数")

   (H3 "词汇VII: 分析学")

   (H3 "词汇VIII: 几何学")

   (H3 "词汇IX: 概率论")

   (H3 "词汇X: 数学术语杂项")

   
   ))