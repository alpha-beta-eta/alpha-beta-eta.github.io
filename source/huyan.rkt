#lang racket
(provide huyan.html)
(require SMathML)
(define huyan.html
  (TnTmPrelude
   #:title "胡言乱语"
   #:css "styles.css"
   (H1 "Quotes")
   (Blockquote
    "Thus, programs must be written for people to read, "
    "and only incidentally for machines to execute. "
    "&mdash; Preface to the First Edition, "
    "Structure and Interpretation of Computer Programs")
   (Blockquote
    "Recursion is the root of computation since it trades description for time. "
    "&mdash; Alan Perlis, EPIGRAMS IN PROGRAMMING")
   (Blockquote
    "Ce n'est pas une nation que nous habitons, mais une langue. "
    "Ne vous méprenez pas; notre langue maternelle est notre véritable patrie. "
    "&mdash; Emil Cioran")
   (Blockquote
    "At a time when mathematical fashion despises generality (seen as gratuitous " (Q "generalities")
    ", i.e. vacuities) I affirm the principal force in all my work has been the quest for the "(Q "general.")
    " In truth I prefer to accent " (Q "unity") " rather than " (Q "generality.")
    " But for me these are two aspects of one quest. Unity represents the profound aspect, "
    "and generality the superficial aspect. (A. Grothendieck, Récoltes et Semailles, 1985-87)")
   (H1 "人工语言?")
   (P "试图让机器分析和理解自然语言是全然不可能的事情, 这种不可能性深植于"
      "自然语言本身. 我们要做的事情, 不该是探究现存的自然语言的规律, "
      "而应该是设计新的自然语言, 它能够克服自然诞生的自然语言的缺陷. "
      "或许这个说法并不够准确, 因为的确已经有许多人工的自然语言了, 最经典的"
      "当然是Esperanto. 但是这些语言的问题在于, 它们在各个方面都模仿"
      "已有的自然语言. 这阻碍了真正的探索和进步. 比如说, 我不认为语言必须"
      "写成字符序列的形式, 或许来源于Lisp的S-expression就是一种很好的替代. "
      "(当然, S-expression也得以字符序列的形式写出来, 但是读者应该明白我的意思.) "
      "我们不应该试图一开始就设计庞然巨物, 而应该从一系列小的语言开始. "
      "这些小的语言应该试图呈现各种各样的语言机制. 接下来我们应该试图理解"
      "如何将这些小的语言复合起来. 我们必须区分什么样的机制是正交的或者可加的, "
      "而什么样的机制与其他所有机制都耦合在一起. 在此基础之中, 我们必须开始设计"
      "丰富的语言, 在这一步的时候, 我们应该充分理解所有的设计决定会造成的影响. "
      "我不认为一种语言可以完成所有目的, 事实上我们应该设计各种各样的丰富的语言. "
      "而这种努力的结果应该是设计语言的成熟方法论, 它应该允许对于语言没有深刻理解"
      "的人也能够设计语言达成自己各种各样的目的, 并且这样的语言也应该很容易为"
      "他人所掌握. 这是我所认为的语言学接下来的任务.")
   (H1 "算法和想法")
   (P "当前有一种极端不正常的风气, 尤其流行于程序员和计算机科学学生群体, 那就是算法(和数据结构)"
      "至高无上论. 我实在无法理解就是了.")
   (P "首先, 我会想到Alan Perlis的经典名言:"
      (Blockquote
       "You think you know when you can learn, are more sure when you can write, "
       "even more when you can teach, but certain when you can program. ")
      "这句话其实反映了计算机科学, 编程语言和算法的先进性, 然而却被相当多的人误解了. "
      "这种先进性在于编程语言迫使人类精确/准确地表达自己的想法, 从而扫除本来隐藏于人类"
      "心智中的模糊和gap. 其后果在于使得编程语言可以成为学习和思想交流的媒介. 然而, "
      "&quot;算法至上&quot;主义者似乎完全错误地理解了这句话. 实际上, Alan Perlis的"
      "名言还可以补充一下, 即这种可以编程的情况要排除只是机械复制粘贴代码.")
   (P "的确如此, 我甚至不是第一次遇到这种情况. 他们洋洋得意, 但是其实什么也不懂. 举个例子, 比如说"
      "Dijkstra算法, 应该绝大多数人都有所耳闻或者编写过相应的程序. 对于本人而言, 其实Dijkstra算法"
      "应该被称为Dijkstra实现, 它是对于uniform-cost search这个想法的实现. 可以说, "
      "虽然许多人都写下了对于Dijkstra算法的理解, 但是我发现他们其实并不真的理解. "
      "他们所谓的理解其实就是观察Dijkstra实现的运行情况, 试图从这种运行情况中总结出什么"
      "所谓高深莫测的观点或者性质, 或者是, 他们把验证/证明算法的正确性当作理解算法本身. "
      "然而这只是徒劳而已, 因为他们连uniform-cost search这样最简单的想法也没能领会. "
      "实际上, Alan Perlis的名言只是最理想的情况, 不能理解想法却能够机械编写程序"
      "才是真切的现实. 这似乎是计算机科学好的一面? 然而, 我觉得不如说这恰恰是其缺陷.")
   (P "当然, 这不仅限于计算机科学, 毕竟计算机科学也只是人类的一个领域. 比如说数学, "
      "数学系的学生知道自己很多时候可以验证证明的正确性并将它们记下来却不知道为什么"
      "这样证明, 或者迷失于作者的思路之海当中. 当然, 他们也知道当他们真正理解作者"
      "背后的想法时, 那些看起来复杂的技术性细节就没有那么令人生畏了, 甚至他们可以发现更简单的证明. "
      "对于非数学系但经常使用数学的人而言, 情况又是另外一种. 他们往往满足于句法和符号操作 (其实也就是算法), "
      "但对于其背后的语义和涵义并不感兴趣. 目前的现实是句法和语义之间存在很多gap, "
      "在并不真正理解的情况下, 很容易产生(离谱的)错误却不自知. 不过, 我想现在越来越多的人"
      "也对于数学本身产生了兴趣, 这或许要归功于当前的人工智能热潮和数学科普人的努力.")
   (P "虽然说了这么多, 但读者应该看出了我想强调的观点, 即想法是第一义的, 而算法不过是想法的自然而然的果实. "
      "当前的情况却是颠倒的, 很多人认为算法是至高无上的, 想法却是次要的, 甚至他们把各种各样本来不是"
      "算法的东西也称为算法, 其后果就是他们往往完全忽略了算法背后的想法. "
      "比如说编译器构造这样的领域, 我觉得很多人学不会并不是能力问题, 而是因为"
      "他们从一开始就错误地理解了这个领域最基本的内容 (而且, 编译原理是一个很烂的具有误导性的别名). "
      "典型地, 有人把词法分析和句法分析当成编译器构造的主要研究对象. 这好比一个厨师认为他最主要的工作是"
      "研究怎么制造菜刀和铁锅. 研究制造菜刀和铁锅当然有助于研究怎么做菜, 但是这和做菜本身存在很大的偏差. "
      "(可能我也有点敏感了, 但我听到像递归下降算法这样的话时就挺不舒服的, 因为递归下降本身是个自然的想法, "
      "其根源是follow the syntax/grammar/structure这样的理念.) 那么, 编译器构造研究什么呢? "
      "它研究如何弥合两种不同语言的差异, 研究不同的语言之间保持语义的变换. 当然, 它的目的往往在于让程序"
      "跑得更快. 此时我想到了Kent Dybvig的PhD论文, 我觉得那算是一种典范 (或许其内容更贴近"
      "&quot;编译原理&quot;这样的名字). Kent Dybvig的风格往往就是我想一下我要做什么, 然后我就正确地"
      "以切实的方式做了. (这篇论文还不能完全体现他的风格, 因为论文的目的是刻画编译器构造的原理, "
      "而不是实际写一个编译器.)")
   (P "好像有点离题了, 那就写这么多吧.")
   ))