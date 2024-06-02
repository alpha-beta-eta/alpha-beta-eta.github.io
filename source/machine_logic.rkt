#lang racket
(provide machine_logic.html)
(require SMathML)
(define machine_logic.html
  (TnTmPrelude
   #:title "机器逻辑博客翻译"
   #:css "styles.css"
   (H1. "机器逻辑博客翻译")
   (P (A #:attr* '((href "https://lawrencecpaulson.github.io/")) "Machine Logic")
      "是Lawrence Paulson的个人博客.")
   (H2. "为什么你不使用依赖类型? [2025年11月2日]")
   (P "为了公平起见, (我必须要说)没有人问过我这个确切的问题. "
      "但是人们经常问我为什么Isabelle摒弃了证明对象. "
      "这两个问题本质上是相同的, 因为证明对象内蕴于所有通常的类型理论之中. "
      "它们也是完全不必要的, 且是对于空间的巨大浪费. 正如在"
      (Ref "vs") "中所描述的那样, " (Em "实现语言")
      " (而非逻辑) 中的类型检查可以保证只有合法的证明步骤能够执行. "
      "Robin Milner在五十年前就得到了这一基础性的洞察, "
      "给我们带来了LCF架构及其证明核心. "
      "但是对于原本问题的最佳回答或许就是这个: "
      "我的确使用依赖类型, 多年以来.")
   (H3. "我与AUTOMATH的时光")
   (P "当N G de Bruijn于1977年来到加州理工学院进行关于AUTOMATH的讲座时, "
      "我有幸与他私下里进行讨论. 我从未真正上手用过这个系统. "
      
      )
   (H3. "Martin-Löf类型论")
   (P "为了回应Bengt Nordström和Kent Petersson的友善邀请, "
      "我多次访问Gothenburg的Chalmers大学以学习了解Martin-Löf类型论. "
      
      )
   (H2. "de Bruijn准则 vs LCF架构 [2022年1月5日]" #:id "vs")
   
   ))