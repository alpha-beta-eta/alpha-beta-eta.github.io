#lang racket
(provide delimited_control.html)
(require SMathML)
(define okmij "https://okmij.org/ftp/")
(define (okmij/ path) (string-append okmij path))
(define (Ref-link title path)
  (LINK title (okmij/ path)))
(define :shift (Code "shift"))
(define :reset (Code "reset"))
(define :control (Code "control"))
(define :shift0 (Code "shift0"))
(define :control0 (Code "control0"))
(define :cupto (Code "cupto"))
(define :goto (Code "goto"))
(define :Cont (Code "Cont"))
(define :delimcc (Code "delimcc"))
(define :tex-only-numbers-content (Code "\\only&lt;numbers&gt;{content}"))
(define :content (Code "content"))
(define :numbers (Code "numbers"))
(define :tex-note-X-Y (Code "\\note&lt;X&gt;{Y}"))
(define :tex-only-X-note-Y (Code "\\only&lt;X&gt;{\\note{Y}}"))
(define :tex-only (Code "\\only"))
(define :frame (Code "frame"))
(define :make_overlays (Code "make_overlays"))
(define :only (Code "only"))
(define :framer (Code "framer"))
(define :ppx (Code "ppx"))
(define :note-numbers-content (Code "note [numbers] content"))
(define :FramerState (Code "FramerState"))
(define :FramerCont (Code "FramerCont"))
(define :Lit-talk (Code "Lit \"This is my talk\""))
(define :Lit-points (Code "Lit \"It has several points\""))
(define :List (Code "List"))
(define :Lit-then (Code "Lit \"Then it comes to that\";"))
(define :k (Code "k"))
(define :only-numbers-content (Code "only numbers content"))
(define :failure (Code "failure"))
(define :mplus (Code "mplus"))
(define :choose (Code "choose"))
(define :choose^ (Code "choose'"))
(define :SearchTree (Code "SearchTree"))
(define :io-type (Code "[(Int,Int)]"))
(define :Int->Int (Code "Int-&gt;Int"))
(define :an_exp (Code "an_exp"))
(define :Exp (Code "Exp"))
(define :io1 (Code "[(0,1), (1,1), (2,3)]"))
(define :exp1 (Code "K 1 :+ (X :* (K (-1) :+ X))"))
(define :fun1 (Code "1 + x*(x-1)"))
(define :io2 (Code "[(0,1), (1,1), (2,3), (-1,3)]"))
(define :io3 (Code "[(0,1), (1,3), (-1,3), (2,15)]"))
(define :loop (Code "loop"))
(define :fun-Omega (Code "fun () -&gt; Omega"))
(define :unit->a (Code "unit -&gt; 'a"))
(define :loop-unit (Code "loop ()"))
(define :delta (Code "delta"))
(define :eval* (Code "eval*"))
(define :teval (Code "teval"))
(define :RecType (Code "RecType"))
(define :unwrap.wrap (Code "(unwrap . wrap)"))
(define $# (Mo "#" #:attr* '((lspace "0") (rspace "0"))))
(define $control (Mi "control"))
(define $FX (Mi "FX"))
(define $Int (Mi "Int"))
(define split1 (&split 0.5))
(define (App f . e*)
  (apply split1 f e*))
(define $-->* (^ $--> $*))
(define (Prompt e)
  (: $# (par0 e)))
(define (Control k e)
  (: (split1 $control k) $. (par0 e)))
(define (Seq e1 e2)
  (: e1 $\; e2))
(define (Lam x e)
  (: $lambda x $. e))
(define (Mu X T)
  (: $mu X $. T))
(define (plug F X)
  (: F (bra0 X)))
(define kd (App $k $d))
(define kdkd (Prompt (Seq kd kd)))
(define C0 (Control $k (App $f kdkd)))
(define fx-body (App $f (par0 (Lam $x (App kdkd $x)))))
(define C1 (Control $k fx-body))
(define delimited_control.html
  (TnTmPrelude
   #:title "延续和定界控制"
   #:css "styles.css"
   (H1. "延续和定界控制")
   (P "本文译自Oleg Kiselyov的"
      (Ref-link "Continuations and Delimited Control" "continuations/")
      ".")
   (Ul (Li (LINK "用shift和reset编程导论" "#tutorial"))
       (Li (Ref-link (B "定界控制在OCaml, Haskell和Scheme中的实现")
                     "continuations/implementations.html"))
       (Li (LINK "定界延续的一个子结构类型系统" "#delimcc-type"))
       (Li (Ref-link "流和Iteratee" "Streams.html"))
       (Li (LINK "如何移除动态提示符: 静态和动态的定界延续算子是同等可表达的"
                 "#impromptu-shift"))
       (Li (Ref-link "一个反对call/cc的论证" "continuations/against-callcc.html"))
       (Li (LINK "泛型Zipper: 遍历的上下文" "generic_zipper.html"))
       (Li (LINK "操作系统中的定界延续" "generic_zipper.html#context-OS"))
       (Li (LINK "定界控制与广度优先, 深度优先以及迭代加深搜索" "#reify-search"))
       (Li (Ref-link "直接在OCaml中实现Eff" "Denotational.html#eff"))
       (Li (Ref-link "动态绑定和定界控制效应"
                     "Computation/dynamic-binding.html#DDBinding"))
       (Li (LINK "不流泪的回答类型修改: 有类型定界控制算子的提示符传递风格翻译"
                 "#multi-atm"))
       (Li (Ref-link "来自迭代的递归:" "continuations/undelimited.html#iter-recur")
           "程序推导的一个练习")
       (Li (Ref-link "一个时间旅行的故事" "continuations/map-story.html"))
       (Li (Ref-link "LogicT: 公平且富有表达力的回溯单子变换器"
                     "Computation/monads.html#LogicT"))
       (Li (Ref-link "纯函数式的惰性非确定性编程"
                     "Computation/monads.html#lazy-sharing-nondet"))
       (Li (Ref-link "shell管道的本质," "ML/myawk/index.html")
           "或者说: CPS用起来可以很方便")
       (Li (Ref-link "无界延续不是函数" "continuations/undelimited.html"))
       (Li (Ref-link "定界控制的类型可靠性的机械化证明"
                     "formalizations/index.html#shift"))
       (Li (LINK "用于带有嵌套事务的CGI编程的持久定界延续" "#shift-cgi"))
       (Li (Ref-link "如何zip折叠:" "Streams.html#zip-folds")
           "表示为两个延续的列表的一个完整的库")
       (Li (LINK "带有有类型提示符的定界控制的简单类型lambda演算不是强规范化的"
                 "#cupto-nontermination"))
       (Li (Ref-link "多提示符定界控制的可执行直接指称语义"
                     "continuations/implementations.html#denot"))
       (Li (LINK "通过定界延续得到一般递归类型" "#delimcc-rectype"))
       (Li (Ref-link "OCaml中的非确定性选择 (amb)" "ML/ML.html#amb"))
       (Li (LINK "来自有类型prompt/control的不动点组合子" "#typed-control-fix"))
       (Li (Ref-link "OCaml中的嵌入式概率编程" "kakuritu/Hansei.html"))
       (Li (Ref-link "通过求值规范化循环体," "lift-reduce/lift-reduce.html")
           "多提示符定界延续的一个应用")
       (Li (LINK "Slide-Effect: 幻灯片叠层和定界控制" "#slide-effect"))
       (Li (Ref-link "没有痛苦, 恐惧或者内疚的let插入"
                     "meta-programming/index.html#let-insert"))
       (Li (Ref-link "带有定界控制的分阶段"
                     "meta-programming/calculi.html#circle-shift"))
       (Li (Ref-link "有类型的分阶段Future:" "meta-future/")
           "作为分阶段的半隐式批量远程代码执行")
       (Li (Ref-link "定界延续做dynamic-wind"
                     "continuations/implementations.html#dynamic-wind"))
       (Li (Ref-link "多变元函数和关键字参数: 对于上下文的类型的模式匹配"
                     "Haskell/polyvariadic.html"))
       (Li (Ref-link "作为局部异常的提示符" "ML/ML.html#poly-exn"))
       (Li (Ref-link "可恢复的异常" "ML/ML.html#resumable-exn") "在ML中")
       (Li (Ref-link "作为绿色fork的shift" "continuations/green-fork.html"))
       (Li (Ref-link "POSIX C和OCaml中的概率编程:" "continuations/shift-v-fork.html")
           :shift "对" (Code "fork(2)"))
       (Li (Ref-link "对解析器求导" "continuations/differentiating-parsers.html"))
       (Li (LINK "传名的有类型shift/reset演算" "#cbn-shift"))
       (Li (Ref-link "延续层级和量词辖域:" "gengo/index.html#hier")
           "灵活的延续层级")
       (Li (Ref-link "作为call/cc的不动点的自应用"
                     "continuations/undelimited.html#fix-callcc"))
       (Li (Ref-link "完全的流处理器及其对于所有无穷流的应用"
                     "Haskell/AlgorithmsH1.html#total-sp"))
       (Li (Ref-link "为什么CPS中的程序特化得更好"
                     "meta-programming/Generative.html#bti"))
       (Li (Ref-link "作为变量绑定环境的栈" "continuations/stack-env.html"))
       (Li (Ref-link "自然语言语义中的定界延续" "gengo/")))
   (H2. "用shift和reset编程导论" #:id "tutorial")
   (P "这个关于定界延续的教程是与浅井健一 (Kenichi Asai, 御茶水女子大学, 日本) "
      "一起在2011年延续研讨会 (Continuation Workshop 2011) 前夜给出的.")
   (P "延续的概念在编程中自然地出现: 条件分支从两个可能的未来中选择一个延续; "
      "引发一个异常丢弃延续的一部分; 一个尾调用或者" :goto
      "以该延续继续. 尽管延续在每个语言中都被隐式地操纵, "
      "将它们作为第一级对象显式地操纵却很少被使用, 因为人们觉得这很困难.")
   (P "这个教程旨在给出延续的一个平缓的导论, 以及使用控制算子" :shift "和" :reset
      "以第一级定界延续进行编程的初步体验. 不假定有关于延续的预备知识, "
      "这个教程帮助参与者编写简单的协程和非确定性搜索. "
      "这个教程应该使得理解和欣赏CW 2011上的报告变得更容易.")
   (P "我们假定读者对于函数式编程语言有基本的了解, "
      "例如OCaml, Standard ML, Scheme和Haskell. 不需要关于延续的预备知识. "
      "我们鼓励参与者带上笔记本电脑并跟着编程.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为2011年9月27日.")
    (Dt (B "参考文献"))
    (Dd
     "CW2011教程环节. 2011年9月23日"
     (Br) (LINK "&lt; http://logic.cs.tsukuba.ac.jp/cw2011/tutorial.html &gt;"
                "http://logic.cs.tsukuba.ac.jp/cw2011/tutorial.html")
     (Br) (LINK "&lt; http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/ &gt;"
                "http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/")
     (P "OchaCaml和Haskell的教程讲义"
        (Br) (LINK "&lt; http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf &gt;"
                   "http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf")
        (Br) (Ref-link "Haskell-tutorial.pdf" "continuations/Haskell-tutorial.pdf")
        " [86K]")
     (P (Ref-link "ContExample.hs" "continuations/ContExample.hs") " [4K]"
        (Br) "Haskell中的一个shift/reset示例代码, 位于" :Cont
        "单子之中" -- "这是定界控制的单子")
     (P (Ref-link "ContTutorial.hs" "continuations/ContTutorial.hs") " [9K]"
        (Br) "教程的Haskell部分的完整代码")))
   (H2. "Slide-Effect: 幻灯片叠层和定界控制" #:id "slide-effect")
   (P "或许每个做过PowerPoint, Beamer或者普通老式幻灯片演示的人都尝试过叠层 (overlay): "
      "一种通过逐步显示或者隐藏幻灯片的某些内容来让幻灯片动起来的简单方式. "
      "然而, 可能没有多少人意识到他们这样做时体验到了定界控制. "
      "何以如此" -- "本文对此加以阐述. 我们将会看到定界控制是相当普遍的. "
      "它甚至可以在编程语言或者系统中没有专门的定界控制设施的情况下实现. "
      "我们也会看到这种设施" -- "定界控制算子" -- "如果真的可用的话, 能带来什么. "
      "定界控制的专家在这里或许也能看到一些东西: 多发 (multi-shot) 定界控制的一个现实的例子"
      -- "这种定界控制被认为很少被使用也很少有用, 因此在一些系统中甚至不被提供.")
   (P "我们的贯穿始终的例子是一个相当典型的带有叠层的Beamer幻灯片:"
      (CodeB "    \\begin{frame}{My Slide}

    This is my talk

    It has several points

    \\begin{itemize}
    \\item First, me have to mention this
    \\only&lt;2,3,4&gt;{\\item Then it comes to that}
    \\only&lt;3&gt;{\\item We have to stress that}
    \\only&lt;4&gt;{\\item Or, to put it better,}
    \\end{itemize}

    \\note&lt;1&gt;{To expand on 1}
    \\note&lt;2,3,4&gt;{Note on 2}
    \\note&lt;4&gt;{Note on 4}
    \\note&lt;3,4&gt;{Note on 3}
    \\end{frame}
")
      "这里, " :tex-only-numbers-content "的意思是" :content "只出现在" :numbers
      "中列出的叠层上 (" :tex-note-X-Y "本质上是" :tex-only-X-note-Y
      "的简写). 因此, 这个幻灯片有四个叠层 (因为4是所提及的最大的叠层编号). "
      "在第一个叠层上, 列表只有单独一个, 也就是最上面的, 项 "
      "(附带着第一个注记, 显示与否取决于Beamer的设置). "
      "这个项以及其上方的文本一直保留到最后. "
      "第二个叠层带来了第二个列表项 (以及相关联的注记), 它们也会持续存在. "
      "下一个叠层添加了第三个项, 它在最后一个叠层中被替换. 当然, "
      "Beamer还有其他叠层算子" -- "它们要么可以用" :tex-only
      "来实现, 要么可以类似地实现. 因此, 我们将自己限制于" :tex-only ".")
   (P "我们描述" :tex-only "的两个实现: 不带有定界控制的和带有定界控制的, "
      "并对它们加以对比. Beamer是用TeX写的, "
      "而TeX并不是呈现算法 (或者一般地编程) 的最佳语言. 我们转而使用OCaml. "
      "OCaml (至少到4.14版本为止) 具有一个通用的定界控制设施 ("
      :delimcc "库), 它在第二个实现中被使用.")
   (P "Beamer接受上面的幻灯片TeX代码并创建输出的若干pdf页面, 每个叠层一页. "
      "在我们的OCaml实现中, 我们专注于叠层展开, 跳过排版和pdf渲染. "
      "也就是说, 我们接受一个带有叠层算子的帧 (frame) 并将其展开为一个"
      (Q "渲染好的") "帧的列表, 其中没有叠层算子. "
      "一个帧在OCaml中以或许最为朴素的方式被表示, 即作为" :frame "数据结构:"
      (CodeB "    type frame = Frame of string * content list
     and content =
      | Lit of string
      | List of content list
      | Note of content
      | Empty

    let frame t c = Frame(t,c)
")
      "帧的内容由字面字符串, 列表和注记构成" -- "刚好足够我们的贯穿始终的例子. "
      "人们可以很容易地将其渲染为markdown, html或者任何其他格式.")
   (P "对于叠层, 我们假定最为极简的接口:"
      (CodeB "    module type framer = sig
      val make_overlays : (unit -&gt; frame) -&gt; frame list
      val only : int list -&gt; content -&gt; content
    end
")
      "函数" :make_overlays "接受一个可能包含" :only "的帧" (Em "表达式")
      " (表示为一个thunk)" -- "并返回完成了的帧的列表, 每个叠层一个. "
      "可能出现在叠层表达式中的函数" :only "只在所列出的叠层上显示其" :content
      ". 以下是示例Beamer幻灯片在OCaml中的表示方式" --
      "这相当于一个用于带有叠层的幻灯片的简单嵌入式DSL."
      (CodeB "    module Ex(S:framer) = struct
      open S

      let r = make_overlays @@ fun () -&gt;
      frame \"My Slide\" [
       Lit \"This is my talk\";
       Lit \"It has several points\";

       List [
         Lit \"First, we have to mention this\";
         only [2;3;4] @@ Lit \"Then it comes to that\";
         only [3] @@ Lit \"We have to stress that\";
         only [4] @@ Lit \"Or, to put it better,\";
       ];

       only [1] @@ Note (Lit \"To expand on 1\");
       only [2;3;4] @@ Note (Lit \"Note on 2\");
       only [4] @@ Note (Lit \"Note on 4\");
       only [3;4] @@ Note (Lit \"Note on 3\");
     ]
    end
")
      "显然, 用OCaml写幻灯片比用TeX写要不方便得多 "
      "(这种不方便可以通过带有可选参数的智能构造器在一定程度上得到缓解, "
      "或者, 更好的是, 使用" :ppx "预处理器). "
      "我们显式地以" :framer "接口的实现参数化了代码: 我们将会有两个实现.")
   (P :framer "的第一个实现是叠层展开在Beamer中很可能的实现方式. "
      "想法很简单: 不断地重新执行帧表达式, 在不同的动态环境中: "
      "带有不同的当前叠层编号. 算子" :tex-only-numbers-content
      " (或者, 在OCaml中, " :note-numbers-content
      "{译注: 此处应为" :only-numbers-content "}) 会省略其" :content
      ", 除非当前叠层编号列在" :numbers "之中."
      (CodeB "    module FramerState : framer = struct
      let curr_frame = ref 0
      let max_frames = ref 0

      let make_overlays th =
        curr_frame := 1;
        max_frames := 1;
        let rec loop f =
          if !curr_frame &gt;= !max_frames then [f] else
          (incr curr_frame; f :: loop (th ()))
        in loop (th ())

      let only ixs c =
        let mx = List.fold_left max 1 ixs in
        if !max_frames &lt; mx then max_frames := mx;
        if List.mem !curr_frame ixs then c else Empty
    end
")
      :FramerState "字面上实现了这个想法: 它是一个可执行的规格. "
      "其唯一的复杂之处在于叠层的数目并不是事先已知的, 而是必须在展开期间确定: "
      "它是" :only "中所提及的最大的叠层编号.")
   (P "其缺点, 即全局可变状态, 也是明显的. 考虑如果" :make_overlays
      "在帧内容之中被递归地调用会发生什么: 这可能是因为失误而发生的; "
      "这也是正当的, 用以格式化一个" (Q "迷你帧")
      ". 尽管可变状态可以经过一些加工而被隐藏起来, 它却是不可避免的: 它是"
      :make_overlays "和" :only "之间的通信渠道.")
   (P :FramerState "最大的缺点, 不得不承认, 是审美上的: "
      "一遍又一遍地从头重新渲染一切, 就是让人感觉不对. "
      "在我们的示例幻灯片上, 第一个列表项以及其上方的文本保持不动. "
      "这一部分肯定可以一劳永逸地渲染一次, 然后在所有的输出页面之间共享吧? "
      "类似地, 列表中的第二个项出现在叠层2, 3和4上. "
      "渲染到这个列表为止一次, 然后在输出中共享, 难道不好吗?")
   (P "正是这样的思路引向了定界控制. 考虑来自我们的贯穿始终的例子的帧表达式, "
      "在下面重复如下:"
      (CodeB "    frame \"My Slide\" [
     Lit \"This is my talk\";
     Lit \"It has several points\";

     List [
      Lit \"First, we have to mention this\";
      only [2;3;4] @@ Lit \"Then it comes to that\";
      only [3] @@ Lit \"We have to stress that\";
      ...
     ]
")
      "在从左到右的求值中, 我们首先求值" :Lit-talk ", 然后是" :Lit-points
      ", 然后进入" :List "表达式并求值其第一项. 求值在堆上构建" :content
      "数据结构, 将它们链接起来并从栈上引用它们. 现在我们来到第一个" :only
      ". 叠层的公共部分在这里结束: 一些叠层以" :Lit-then
      "继续, 一些则不带有它. 无论哪种情况, 到这一点为止的栈都是共享的. "
      "定界控制恰恰就是从一个选择点恢复, 并共享在此之前获得的结果的机制."
      (CodeB "    module FramerCont : framer = struct
      open Delimcc
      type req = Done of frame
               | OnlyAt of int list * content * (content -&gt; req)

      let p = new_prompt ()
      let only ixs c = shift0 p (fun k -&gt; OnlyAt (ixs,c,k))
    ...
")
      "如同在" :FramerState "中那样, 操作" :make_overlays "和" :only
      "进行通信: 这一次, 使用的是控制而非可变状态. "
      "(顺便一提, 可变状态可以用定界控制来模拟这一点很早就被注意到了.) "
      "这种通信是单方面的且简单的: " :only "将其参数报告给接收者 "
      "(也称为处理器: " :make_overlays "的一部分) 并让它做所有的工作. "
      "关键在于, " :only "捕获了栈在其求值点处的部分, 作为延续" :k
      ". 当这个延续被调用时, 帧表达式的求值从所保存的点继续. "
      "这个延续可以被调用若干次: 所有这些恢复都始于同一个共享的栈.")
   (P "函数" :make_overlays "尝试在" :only-numbers-content
      "上继续求值两次: 一次是为了列在" :numbers "中的叠层 (因而包含"
      :content "), 一次是为了没有列出的叠层. " :only "的延续可能包含另一个"
      :only ": 另一个选择点. 然而, 并非所有的选择都是可能的, 因为" :only
      "的一个延续对叠层编号施加了约束. 函数" :make_overlays
      "追踪这些约束: 细节请参见源代码. 总而言之, " :FramerCont
      "在构建叠层的同时维持了共享并避免了不必要的重新求值.")
   (P "我们应该强调, " :only "的延续" :k "被调用 (恢复) 了两次: "
      "即所谓的多发定界控制. 从版本5开始, OCaml以代数效应的形式提供定界控制"
      -- "然而, 它是" (Q "一发") "的: 一个被捕获的延续只能被恢复一次 "
      "(常常是几乎立即恢复). 这确实是定界控制最常见的用法. "
      "多发定界控制对于表示非确定性是不可或缺的 (参见Hansei概率编程库)"
      -- "而通常认为, 对于其他任何东西则并非如此. "
      "看到多发定界控制的另一种用途, 是有些令人惊讶的 (对我来说也是如此).")
   (P "PDF本身就有叠层, 或者说过渡" -- "尽管除了Acrobat之外, "
      "似乎很少有阅读器实现了它. 如果要编写一个支持过渡的PDF阅读器, "
      "或许本文可以作为参考. 或许人们甚至会想要使用定界控制...")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为2024年10月.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "slide_effect.ml" "continuations/slide_effect.ml") " [13K]"
     (Br) "本文中的OCaml代码, 带有测试和求值追踪. 其中一部分需要"
     (Ref-link "delimcc库" "continuations/implementations.html#caml-shift") "."
     (P (Ref-link "Hansei" "kakuritu/Hansei.html") ": OCaml中的嵌入式概率编程"
        (Br) "这个嵌入式概率编程语言关键性地依赖于 (多发) 定界控制.")
     (P "David Wingate, Andreas Stuhlmüller和Noah D. Goodman: "
        (Cite "Lightweight Implementations of Probabilistic Programming Languages Via Transformational Compilation")
        ". AISTATS 2011, Revision 3. February 8, 2014"
        (Br) "这篇论文提出了一种基于重复重新求值的概率编程语言实现技术. "
        "这种技术不需要定界控制或者其他高级特性, 并且可以用于许多现有的系统: "
        "论文本身演示了一个随机化的Matlab. 许多其他概率语言都使用这种技术.")
     (P (Ref-link "嵌入式概率领域特定语言Hakaru10" "kakuritu/Hakaru10/index.html")
        (Br) "它使用重复求值, 但带有记忆化和依赖追踪; 因此只有必要的部分会被重新求值.")))
   (H2. "定界控制与广度优先, 深度优先以及迭代加深搜索" #:id "reify-search")
   (P "这份教程式的Haskell代码阐明了定界控制在非确定性搜索中的应用. "
      "我们将不同的搜索策略应用于同一个非确定性程序, 而不必重写它. "
      "一个非确定性计算被" (Em "具体化") "为一个惰性搜索树, "
      "然后可以以不同的方式检查这个搜索树. 我们将非确定性搜索策略写成标准的"
      "深度优先, 广度优先等等的树遍历.")
   (P "搜索树是普通的树数据类型, 其分支按需构造. "
      "这个树可能是无穷的, 下面的例子中就是如此."
      (CodeB "    data SearchTree a = Leaf a | Node [() -&gt; SearchTree a]
")
      "我们实现三种树遍历, 它们将来自叶结点的值收集到一个列表之中:"
      (CodeB "    dfs, bfs, iter_deepening :: SearchTree a -&gt; [a]
")
      "(实际上我们使用了广度优先搜索的若干个版本, 它们被优化到了不同的程度.)")
   (P "使用来自标准单子变换器库的" :Cont "单子及其操作" :shift "和" :reset
      ", 我们实现了两个原语: 从一个有限列表中非确定性地选择一个值, 以及将一个计算具体化为一个"
      :SearchTree ":"
      (CodeB "    choose :: [a] -&gt; Cont (SearchTree w) a
    reify  :: Cont (SearchTree a) a -&gt; SearchTree a
")
      "其他非确定性操作" -- :failure ", " :mplus " (用以连接两个计算), " :choose^
      " (用以从一个可能无穷的列表中选择)" -- "都是用" :choose "写的.")
   (P "贯穿始终的例子是一个真实的归纳编程问题的简单版本: "
      "给定一个输入-输出对的序列" :io-type ", 找到一个具有该输入-输出行为的"
      :Int->Int "函数. 要在其中搜索的函数由以下数据结构表示:"
      (CodeB "    data Exp =
      K Int                                 -- constant function
      | X                                   -- identity
      | Exp :+ Exp                          -- \\x -&gt; f x + g x
      | Exp :* Exp                          -- \\x -&gt; f x * g x
")
      "解法是熟悉的生成并测试"
      (CodeB "    induct io = reify $ do
      exp &lt;- an_exp
      if all (\\ (i,o) -&gt; eval exp i == o) io then return exp else failure
")
      "其中" :an_exp "生成一个样本函数表示, 而if表达式测试在给定输入上对其求值是否给出所欲的输出. "
      :Exp "表达式的生成器是"
      (CodeB "    an_exp =
      (fmap K $ choose numbers)   `mplus`
      (return X)                  `mplus`
      (liftM2 (:+) an_exp an_exp) `mplus`
      (liftM2 (:*) an_exp an_exp)
     where numbers = [-2..2]
")
      "深度优先搜索不能用于这个问题, 因为搜索树是无穷的. "
      "广度优先和迭代加深都是完备的策略, 如果答案存在的话, 它们都能找到答案. "
      "例如, 对于输入-输出对的序列" :io1 ", 我们找到了" :exp1
      " (其对应于函数" :fun1 "), 它确实具有给定的行为. "
      "在一个稍大一点的问题" :io2
      "上的基准测试表明, 优化了的广度优先搜索占用303MB, 而迭代加深占用64MB的内存 "
      "(并且时间大致相同). 尽管是玩具, 这个归纳编程问题并不简单. 对于输入-输出对"
      :io3 ", 广度优先搜索很快就分配了8GB并被内核杀死. "
      "迭代加深分配的速度要慢得多, 但仍然达到了8GB并且同样死掉了.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为2022年2月.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "Searches.hs" "continuations/Searches.hs") " [14K]"
     (Br) "完整的Haskell代码, 带有许多注释, 解释和测试. "
     "注释说明了为什么搜索树要像这样定义, 带有一个thunk."
     (P (Ref-link "嵌入式概率编程" "kakuritu/Hansei.html#derivation")
        (Br) "这篇论文解释了将非确定性程序具体化为惰性搜索树. "
        "我们在这里使用同样的技术, 只是在Haskell中而非OCaml中, 并且不带有概率.")
     (P (Ref-link "在 (AI) 搜索问题中防止记忆化" "Haskell/index.html#memo-off")
        (Br) "对于防止不受欢迎的隐式记忆化的技巧的解释")
     (P "Joachim Breitner: " (Cite "dup -- Explicit un-sharing in Haskell")
        (Br) "2012年7月. "
        (LINK "&lt; https://arxiv.org/abs/1207.2017 &gt;" "https://arxiv.org/abs/1207.2017")
        (Br) "对于不想要的记忆化以及防止它的方法的广泛讨论")))
   (H2. "如何移除动态提示符: 静态和动态的定界延续算子是同等可表达的"
        #:id "impromptu-shift")
   (P "这份技术报告表明, 定界延续算子" :shift ", " :control ", " :shift0
      "等等" (Em "全都") "可以用彼此宏表达. 此外, 算子" :shift ", " :control ", "
      :control0 ", " :shift0 "是单个参数化族的成员, "
      "并且标准的CPS就足以表达它们的指称语义.")
   (P "这份报告形式化地证明了经由" :shift "实现的" :control "确实具有其标准的归约语义.")
   (P "这份报告呈现了" :control ", " :shift0 "和" :control0 " (类似于" :cupto
      ") 的已知最简单的Scheme实现. 报告中的方法让我们能够设计另外700个定界控制算子, "
      "以按照人们认为合适的方式分割和复合栈片段.")
   (Dl
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "impromptu-shift-tr.pdf" "continuations/impromptu-shift-tr.pdf") " [136K]"
     (Br) (LINK "&lt; http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR611 &gt;"
                "http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR611")
     (Br) "Technical Report TR611, Department of Computer Science, Indiana University, 2005"
     (P (Ref-link "delim-cont.scm" "Scheme/delim-cont.scm") " [10K]"
        (Br) "带有用" :shift "/" :reset "实现的" :control ", " :shift0 ", "
        :control0 "的最简单实现的Scheme代码. 这份代码包含了一大组测试用例.")
     (P (Q "Lambda the Ultimate") "上的讨论串, 尤其是关于定界上下文的含义"
        (Br) (LINK "&lt; http://lambda-the-ultimate.org/node/view/606 &gt;"
                   "http://lambda-the-ultimate.org/node/view/606"))
     (P (Ref-link "impromptu-shift-tr.scm" "Scheme/impromptu-shift-tr.scm") " [61K]"
        (Br) "这份报告的主SXML文件")
     (P (Ref-link "用SXML写LaTeX/PDF数学论文" "Scheme/xml.html#Math-authoring"))))
   (H2. "不流泪的回答类型修改: 有类型定界控制算子的提示符传递风格翻译"
        #:id "multi-atm")
   (P "定界控制算子的显著特性是它们在计算期间修改回答类型的能力. "
      "这个特性, 即回答类型修改 (answer-type modification, 简称ATM), "
      "让人们能够紧凑而漂亮地表达各种有意思的程序, 例如有类型的printf, "
      "然而它也使得将这些算子嵌入标准的函数式语言变得困难. "
      "在这篇论文中, 我们呈现了一个有类型的翻译, "
      "将带有ATM的定界控制算子shift和reset翻译为一个熟悉的带有多提示符shift和reset而不带有ATM的语言, "
      "这让我们能够在标准语言中使用ATM而不必修改类型系统. "
      "我们的翻译推广了Kiselyov的有类型printf的直接风格实现, "
      "后者使用两个提示符来模拟回答类型的修改, 并在计算期间传递它们. "
      "我们证明了我们的翻译保持类型. "
      "由于朴素的提示符传递风格翻译即使对于纯的项也会生成并传递许多提示符, "
      "我们给出了一个优化了的翻译, 其只在需要时才生成提示符, 并且也是保持类型的. "
      "最后, 我们给出了一个tagless-final风格的实现, 其由构造而尊重类型.")
   (P "与小堀育男 (Ikuo Kobori) 和亀山幸義 (Yukiyoshi Kameyama) 的合作工作.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为2016年6月.")
    (Dt (B "参考文献"))
    (Dd
     "Electronic Proceedings in Theoretical Computer Science EPTCS 212 "
     "(Post-Proceedings of the Workshop on Continuations 2015) "
     "June 20, 2016, pp. 36-52 "
     (LINK "doi:10.4204/EPTCS.212.3" "http://doi.org/10.4204/EPTCS.212.3")))
   (H2. "用于带有嵌套事务的CGI编程的持久定界延续" #:id "shift-cgi")
   (P "我们呈现一个用于带有嵌套事务的Web编程的简单CGI框架. "
      "这个框架使用未经修改的OCaml系统以及一个任意的, 未经修改的Web服务器 (例如Apache). "
      "这个库使得编写Web应用 (CGI脚本) 就像使用read和printf编写交互式控制台应用一样直截了当. "
      "我们以自然的问答式, 讲故事的风格编写脚本, 充分利用词法作用域, 异常, "
      "可变数据以及其他命令式特性 (如果需要的话). "
      "这些脚本甚至可以作为交互式控制台应用被编译和运行. "
      "使用用于读和写的基本原语的一个不同实现, 控制台程序就变成了CGI脚本.")
   (P "我们的库依赖于持久定界延续的delimcc库. "
      "被捕获的定界延续可以被存储在磁盘上, 以便稍后在一个不同的进程中被加载和恢复. "
      "或者, 序列化了的被捕获的延续可以作为一个编码了的字符串被插入到响应的Web表单的一个隐藏字段中. "
      "延续的使用让我们能够避免迭代, 转而依赖于" (Q "后退按钮")
      ". 定界延续自然地支持" (Q "线程局部") "作用域, 并且序列化起来相当紧凑. "
      "这个库就这样与未经修改的OCaml系统一起工作.")
   (P "定界延续帮助我们实现嵌套事务. 简单的博客应用演示了用户可以在编辑和预览其博客文章之间"
      "反复来回, 或许是在若干个窗口中. 完成了的文章只能被提交一次.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为1.7, 2008年4月.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "Fest2008-talk.pdf" "continuations/Fest2008-talk.pdf") " [204K]"
     (Br) (Ref-link "Fest2008-talk-notes.pdf" "continuations/Fest2008-talk-notes.pdf")
     " [244K]"
     (Br) "在Continuation Fest 2008上对这个库的演示. 这个报告的扩充版本, "
     (Q "Clicking on Delimited Continuations")
     ", 于2008年7月在FLOLAC上呈现. 扩充版本包含了对于定界延续的详细导论."
     (P (Ref-link "caml-web.tar.gz" "ML/caml-web.tar.gz") " [16K]"
        (Br) "基于定界延续的, 带有表单验证和嵌套事务的CGI编程库的源代码. "
        "这个库包含了Continuation Fest演示的完整代码.")))
   (H2. "带有有类型提示符的定界控制的简单类型lambda演算不是强规范化的"
        #:id "cupto-nontermination")
   (P "简单类型lambda演算具有强规范化性质: 任何项的任何归约序列都会终止. "
      "如果我们添加带有有类型提示符的定界控制算子 (例如" :cupto
      "), 强规范化性质就不再成立. 单个有类型提示符就已经导致不终止. "
      "以下例子是由Chung-chieh Shan从带有动态绑定的简单类型lambda演算的不终止例子设计而来的. "
      "它使用了OCaml定界控制库. 函数" :loop "本质上是" :fun-Omega
      ": 其推断出的类型是" :unit->a "; 因此, " :loop-unit "的求值会永远循环下去."
      (CodeB "    let loop () =
      let p = new_prompt () in
      let delta () = shift p (fun f v -&gt; f v v) () in
      push_prompt p (fun () -&gt; let r = delta () in fun v -&gt; r) delta
"))
   (P "Chung-chieh Shan也给出了解释: 作为箭头类型的回答类型隐藏了一个递归类型. "
      "换言之, " :delta "的类型" :unit->a "隐藏了回答类型以及这个函数不纯的事实.")
   (P "Olivier Danvy好心地指出了他和Andrzej Filinski在1998年使用他们在SML/NJ中实现的shift版本"
      "设计的类似的不终止例子. 他们的例子也依赖于作为箭头类型的回答类型.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为2006年9月30日.")
    (Dt (B "参考文献"))
    (Dd
     "Carl A. Gunter, Didier R&eacute;my和Jon G. Riecke: "
     (Cite "A Generalization of Exceptions and Control in ML-Like Languages")
     (Br) "Proc. Functional Programming Languages and Computer Architecture Conf., "
     "June 26-28, 1995, pp. 12-23."
     (Br) "引入了" :cupto "的论文, 这是第一个带有显式有类型提示符的定界控制算子"
     (P (Ref-link "定界动态绑定" "Computation/dynamic-binding.html#DDBinding")
        (Br) "以shift和简单类型lambda演算进行的重新表述")
     (P (Ref-link "带有动态绑定的简单类型lambda演算不是强规范化的"
                  "Computation/dynamic-binding.html#non-termination"))
     (P (LINK "通过定界延续得到一般递归类型" "#delimcc-rectype")
        (Br) "一个以不同方式表述的证明: 表示一般递归类型")))
   (H2. "定界延续的一个子结构类型系统" #:id "delimcc-type")
   (P "我们提出" (Em "抽象地") "解释小步而非大步操作语义的类型系统. "
      "我们将一个表达式或者求值上下文视为带有假设推理的线性逻辑中的一个结构. "
      "求值顺序不仅由操作语义中熟悉的聚焦规则所调控, "
      "也由类型系统中的结构规则所表达, 因此类型更紧密地追踪控制流. "
      "绑定上下文和求值上下文是相关的, 但后者是线性的.")
   (P "我们使用这些想法为定界延续构建一个类型系统. "
      "它让控制算子能够改变回答类型或者在最近的动态包围定界符之外起作用, "
      "却不需要在判断和箭头类型中有额外的字段来记录回答类型. "
      "一个直接风格程序的定型推导将其去糖为延续传递风格.")
   (P "与Chung-chieh Shan的合作工作.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为1.1, 2007年6月.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "作为小步抽象求值的类型检查"
               "Computation/types.html#small-step-typechecking")
     (Br) "对于这篇论文的两个主要口号的详细讨论:"
     (Ul (Li "类型是抽象的表达式 (Cousot)")
         (Li "冒号是一个十字转门 (Lambek)"))
     (P (Ref-link "delim-control-logic.pdf" "continuations/delim-control-logic.pdf")
        " [250K]"
        (Br) "发表于Proc. of Int. Conf. on Typed Lambda Calculi and Applications (TLCA), "
        "Paris, June 26-28, 2007 -- LNCS volume 4583的论文的扩充版本 (带有附录).")
     (P (Ref-link "small-step-typechecking.tar.gz"
                  "Computation/small-step-typechecking.tar.gz")
        " [10K]"
        (Br) "伴随这篇论文的带注释的Twelf代码"
        (Br) "这份代码实现了类型检查" -- "作为热身的简单类型lambda演算的类型检查, "
        "以及主要的lambda-xi0演算的类型检查" -- "并且包含了大量测试和样例推导.")))
   (H2. "传名的有类型shift/reset演算" #:id "cbn-shift")
   (P "我们呈现一个Church风格的传名lambda演算, 其带有定界控制算子shift/reset以及第一级上下文. "
      "除了常规的lambda抽象" -- "其允许对一般的, 甚至是带有效应的项进行代换"
      -- "这个演算也支持严格的lambda抽象. 后者只能被应用于值. "
      "由reset和严格函数施加的对于值的需求决定了求值顺序. "
      "这个演算与熟悉的传值shift/reset演算最为紧密地对应, "
      "并在严格函数的帮助下嵌入了后者.")
   (P "这个演算是有类型的, 既给项指派类型, 也给上下文指派类型. "
      "类型抽象地解释操作语义, 因而简明地描述了一个项的求值中可能发生的所有效应. "
      "纯类型被赋予那些其求值在任何上下文中以及在绑定项的自由变量 (如果有的话) 的任何环境中"
      "都不招致效应, 即不包含shift转换的项. "
      "一个其求值可能包含shift转换的项具有一个带有效应的类型, "
      "其描述了这个项的最终回答类型以及这个项的求值所需要的定界上下文. "
      "控制算子可以改变其上下文的回答类型.")
   (Dl
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "cbn-xi-calc.elf" "gengo/cbn-xi-calc.elf") " [19K]"
     (Br) "实现了动态语义 (" :eval* "关系) 和类型推断 (" :teval "关系) 的Twelf代码. "
     :teval "关系是确定性的且终止的, 因而构造性地证明了我们的Church风格演算的类型系统是可判定的. "
     "这份代码包含了大量对项求值以及确定其类型的例子."
     (P (Ref-link "gengo-side-effects-cbn.pdf" "gengo/gengo-side-effects-cbn.pdf")
        " [161K]"
        (Br) "传名的语言学副作用"
        (Br) "ESSLLI 2008 Workshop on Symmetric calculi and Ludics for the semantic interpretation. "
        "4-7 August, 2008. Hamburg, Germany.")
     (P (Ref-link "作为句法-语义接口的由求值编译" "gengo/index.html#nl-cbe")
        (Br) "语言学被证明提供了有类型的传名shift/reset的第一个有意思的应用. "
        "这篇论文分若干步发展这个演算, 在图3中呈现最终演算的句法和动态语义, "
        "在图4和图5中呈现类型系统. 这篇论文详述了若干样例归约和类型重构, "
        "并讨论了相关工作.")
     (P (LINK "定界延续的一个子结构类型系统" "#delimcc-type")
        (Br) "那篇TLCA 2007论文引入了在定界控制的演算中重构一个项的效应类型的抽象解释技术. "
        "这种技术逐步将一个项归约为其抽象形式, 即类型. "
        "那篇TCLA论文使用了一个带有所谓动态控制算子" :shift0
        "的传值演算. 这里我们将这种技术应用于带有静态控制算子" :shift "的传名演算.")))
   (H2. "来自有类型prompt/control的不动点组合子" #:id "typed-control-fix")
   (P "在最近的论文" (Q "Typed Dynamic Control Operators for Delimited Continuations")
      "中, 亀山和米澤 (Kameyama and Yonezawa) 在他们的用于prompt/control的多态有类型演算中"
      "展示了一个发散的项. 因此, 与浅井和亀山的多态有类型shift/reset演算相对, "
      "后者这个演算不是强规范化的. 与无类型的情形不同, 有类型的control不能用shift宏表达. "
      "亀山和米澤猜想 (有类型的) 不动点算子在他们的演算中也是可表达的. "
      "这个猜想是正确的: 以下是不动点组合子的推导, 使用了他们论文的记号. "
      "然而, 这个组合子并不是完全多态有类型的: 结果类型必须是有居留元的.")
   (P "令" $f "是一个类型为" (&-> $a $a) "的纯函数, 而" $d "是一个类型为" $a
      "的值 (在论文中, " $d "写成一个黑点). 如同在论文中那样, 我们将提示符写成" $#
      ". 表达式"
      (MB (Prompt (Seq C0 C0)))
      "看起来是良类型的. 它归约为"
      (MB (deriv^0 (Prompt (Seq C0 C0))
                   $--> (Prompt (App $f kdkd))
                   $--> (Prompt (App $f (Prompt (App $f kdkd))))
                   $-->* (Prompt (App $f (Prompt (App $f (Prompt (App $f $..h))))))))
      "其中" (&= (App $k $u) (Seq $u C0)) ".")
   (P "既然我们处于一个传值语言之中, 上面的结果并不是非常有用, 但这是一个好的开始. "
      "我们只需要一个eta展开: 假设" $f "具有类型" (&-> (@-> $a $b) (@-> $a $b))
      ". 令" $d "是类型" (&-> $a $b) "的任意值: 这是返回类型有居留元的见证. 我们构建项"
      (MB (&= $FX (Prompt (Seq C1 C1))))
      "其是良类型的, 并且展开为"
      (MB (deriv^0 $FX
                   $--> (Prompt fx-body)
                   $--> fx-body))
      "其中" (&= (App $k $u) (Seq $u C1)) ". 注意到" kd "是" C1
      ", 我们得到" (App $f (par0 (Lam $x (App $FX $x)))) ".")
   (P "因此我们得到" (App $FX $x) "等于" (App $f (par0 (Lam $x (App $FX $x))) $x)
      ", 这意味着" $FX "是" $f "的传值不动点.")
   (P "在无法访问亀山和米澤的演算的实现的情况下, 我们可以使用由cupto导出的control来测试这个表达式. "
      "后者是用OCaml实现的. 我们不能测试我们的fix的类型, 因为cupto的类型系统太粗糙了. "
      "然而, 我们可以测试其动态行为. 为了避免传递结果类型有居留元的见证, "
      "我们将结果类型设为" (&-> $a $a) ", 它显然是有居留元的, 由恒等函数居留.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为1.1, 2007年10月24日.")
    (Dt (B "参考文献"))
    (Dd
     "Yukiyoshi Kameyama和Takuo Yonezawa: "
     (Cite "Typed Dynamic Control Operators for Delimited Continuations") ". FLOPS 2008."
     (P "Kenichi Asai和Yukiyoshi Kameyama: " (Cite "Polymorphic Delimited Continuations")
        (Br) "Proc. Fifth Asian Symposium on Programming Languages and Systems (APLAS 2007), LNCS"
        (Br) (LINK "&lt; http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf &gt;"
                   "http://logic.cs.tsukuba.ac.jp/~kam/paper/aplas07.pdf"))))
   (H2. "通过定界延续得到一般递归类型" #:id "delimcc-rectype")
   (P "一个一般递归类型通常被定义 (参见亀山和米澤) 为" (Mu $X (plug $F $X))
      ", 其中" $X "可能在" (plug $F $X) "中负地 (即逆变地) 出现. 如果" $X
      "只正地出现 (如同在整数列表的类型" (Mu $X (@+ $1 (&c* $Int $X)))
      "中那样), 那么所得到的类型常常被称为归纳的.")
   (P "一个一般递归类型, 例如" (Mu $X (&-> $X $Int $Int)) ", 可以由以下签名刻画:"
      (CodeB "    module type RecType = sig
     type t     (* abstract *)
     val wrap   : (t-&gt;int-&gt;int) -&gt; t
     val unwrap : t -&gt; (t-&gt;int-&gt;int)
    end
")
      "只要" :unwrap.wrap "是恒等函数. 如果我们有这个签名的一个实现, "
      "我们就可以将诸如" (Lam $x (App $x $x)) "这样的项从无类型lambda演算转写到有类型lambda演算.")
   (P "ML支持" :RecType "的一个实现, 使用的是同构递归 (数据) 类型. "
      "然而, 还有另一个实现, 使用的是ML异常. "
      "既然异常是定界控制的一个特殊情形, 我们就得到了带有类似cupto的定界控制的简单类型lambda演算"
      "不是强规范化的另一个证明.")
   (Dl
    (Dt (B "版本")) (Dd "当前版本为1.1, 2007年10月30日; 1.2, 2011年10月.")
    (Dt (B "参考文献"))
    (Dd
     (Ref-link "delimcc-rectype.ml" "ML/delimcc-rectype.ml") " [2K]"
     (Br) "完整的带注释的OCaml代码"))
   (P "Oleg Kiselyov (okmij.org). 页面最后更新于2024年10月5日.")
   ))
