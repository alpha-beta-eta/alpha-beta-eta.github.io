#lang racket
(provide haskell_music.html)
(require SMathML)
(define haskell_music.html
  (TnTmPrelude
   #:title "音乐的Haskell学派"
   #:css "styles.css"
   (H1. "音乐的Haskell学派")
   (P "这本书根据" (Q "第一原理")
      "讨论了数字音乐的创作和实现, "
      "当然顺便教授了Haskell. "
      "不过, 与标题不符的是, "
      "这本书的内容肯定并不局限于Haskell.")
   (H2. "计算机音乐, Euterpea, 以及Haskell")
   (H2. "简单音乐")
   (H2. "多态函数和高阶函数")
   (H2. "音乐幕间")
   (H2. "句法魔法")
   (H2. "更多音乐")
   (H2. "Qualified Types和类型类")
   (H2. "从音乐到MIDI")
   (H2. "解释和演出")
   (H2. "自相似音乐")
   (H2. "归纳证明")
   (H2. "音乐代数")
   (H3. "音乐等价")
   (P "设我们有了两个值" (Code "m1 :: Music Pitch")
      "和" (Code "m2 :: Music Pitch")
      ", 并且我们想要知道他们是否是相等的. "
      "如果我们将其仅仅视为Haskell的值, "
      "那么我们可以轻易地编写一个函数, "
      "其递归地进行比较以看出它们是否在每个层次上都是相同的, "
      "一路向下直至休止和音符" (Em "原语")
      ". 这实际上正是Haskell函数" (Code "==")
      "所做的事情. 例如, 如果"
      
      )
   (H2. "L系统和生成文法")
   (H2. "随机数, 概率分布, 以及Markov链")
   (H2. "基本输入/输出")
   (H2. "高阶类型和Monads")
   (H2. "音乐用户界面")
   (H2. "声音和信号")
   (H2. "Euterpea的信号函数")
   (H2. "谱分析")
   (H2. "加性和减性合成")
   
   ))