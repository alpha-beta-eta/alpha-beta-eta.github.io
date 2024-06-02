#lang racket
(provide zil.html)
(require SMathML)
(define zil.html
  (Prelude
   #:title "LearningZIL笔记"
   #:css "styles.css"
   (H1 "LearningZIL笔记")
   (H2 "第1章 基本")
   (P "大致上IF由两类实体构成, 对象和例程. 对象又大致上可分为两类, "
      "房间和物品.")
   (P "既然处理了输入, 游戏就必须在适当的时候产生回复.")
   (P "parser的目的在于将用户的输入规约为动词, 直接宾语, 间接宾语. "
      "在ZIL中, 它们分别被称为" (Code "PRSA") ", " (Code "PRSO") ", "
      (Code "PRSI") ". parser的结果分为三类, 仅包含动词, 包含动词和"
      "直接宾语, 包含动词, 直接宾语和间接宾语.")
   (P "间接宾语将首先拥有处理输入的机会, 其次是直接宾语, 最后是动词. "
      "这种做法的想法在于, 它们的特殊性依次降低.")
   (H2 "第2章 创建房间")
   (P "以下是一个房间的定义:")
   (CodeB "&lt;ROOM LIVING-ROOM
  (LOC ROOMS)
  (DESC &quot;Living Room&quot;)
  (EAST TO KITCHEN)
  (WEST TO STRANGE-PASSAGE IF CYCLOPS-FLED ELSE
        &quot;The wooden door is nailed shut.&quot;)
  (DOWN PER TRAP-DOOR-EXIT)
  (ACTION LIVING-ROOM-F)
  (FLAGS RLANDBIT ONBIT SACREDBIT)
  (GLOBAL STAIRS)
  (THINGS &lt;> NAILS NAILS-PSEUDO)>")
   (P (Code "ROOM") "代表这是一个房间对象的定义. " (Code "LIVING-ROOM")
      "是这个对象的名字. " (Code "LOC") "一行给出了对象的位置, "
      "所有的房间都位于一个特别的对象之中, 其被称为" (Code "ROOMS")
      ". " (Code "DESC") "一行也给出了某种名字, 这样的名字不是供内部"
      "引用的, 而是供玩家看到的.")
   (P "接下来数行给出了房间的出口. 许多房间的定义中, 出口占据了多数内容. "
      "出口分为不同的种类, 例如" (Code "(EAST TO KITCHEN)")
      "定义了一个无条件出口 (" (Code "UEXIT")
      "). 第五行和第六行定义了一个有条件出口 (" (Code "CEXIT")
      "), 其涉及一个全局变量" (Code "CYCLOPS-FLED")
      ", 要么为真要么为假. 第七行是函数出口 (" (Code "FEXIT")
      ") 的一个例子, " (Code "PER") "标示了其为函数出口. 例程"
      (Code "TRAP-DOOR-EXIT") "将决定玩家是否能够移动, 移动到哪里, "
      "以及若不能移动该怎样回应. 还有两种出口, 虽然该房间的定义没有用到. "
      "一种是非出口 (" (Code "NEXIT") "), 用" (Code "SORRY")
      "标示, 例如"
      (CodeB "(NW SORRY &quot;The soldier at Uncle Otto's front door
informs you that only Emperor Bonaparte is allowed
through.&quot;)")
      "不论如何玩家都不能走这个方向. 另一种是门出口 (" (Code "DEXIT")
      "), 它与有条件出口类似, 只是把全局变量换成了一个门对象, 例如"
      (CodeB "(SOUTH TO GARAGE IF GARAGE-DOOR IS OPEN ELSE
 &quot;You ought to use the garage door opener.&quot;)")
      "注意到有条件出口里是没有" (Code "IS OPEN") "的.")
   (P (Code "ACTION") "一行是房间的动作例程, 这之后再说. "
      (Code "FLAGS") "一行包含了可应用于该房间的" (Code "FLAG")
      ". " (Code "RLANDBIT") "意味着房间在陆地上, 而不是在水上, "
      "或者空中. " (Code "ONBIT") "意味着该房间永远是亮着的. "
      "有的" (Code "FLAG") "每个游戏都有, 有的则是特定于游戏的. "
      "例如, " (Code "SACREDBIT") "特定于ZORK I, 它意味着神偷"
      "不会光顾这个房间. 根据命名约定, 所有的" (Code "FLAG")
      "都以" (Code "BIT") "结尾, 你可以根据自己的需要使用特殊的"
      (Code "FLAG") ".")
   (H2 "第3章 创建物品")
   (P "以下是一个物品定义:")
   (CodeB "&lt;OBJECT LANTERN
  (LOC LIVING-ROOM)
  (SYNONYM LAMP LANTERN LIGHT)
  (ADJECTIVE BRASS)
  (DESC &quot;brass lantern&quot;)
  (FLAGS TAKEBIT LIGHTBIT)
  (ACTION LANTERN-F)
  (FDESC &quot;A battery-powered lantern is on the trophy case.&quot;)
  (LDESC &quot;There is a brass lantern (battery-powered) here.&quot;)
  (SIZE 15)>")
   (P (Code "(LOC LIVING-ROOM)") "代表铜灯最初在起居室里. 随着游戏进程"
      "的推进, 其位置也有可能发生变化. 例如, 若玩家捡起铜灯, 那么它的"
      (Code "LOC") "就会变为" (Code "PLAYER") "对象, 有时" (Code "PLAYER")
      "也被称为" (Code "PROTAGONIST") ". 之后若玩家在厨房丢掉铜灯, "
      "铜灯的" (Code "LOC") "就是厨房了."
      )
   (P (Code "SYNONYM") "性质是一些可以用来引用铜灯的名字. "
      (Code "ADJECTIVE") "性质是一些用来限定引用物品的形容词, 该性质是可选的."
      )
   (P (Code "DESC") "性质自然是物品的名字, 它会出现在例程需要插入名字的时候.")
   (P "铜灯具有两个" (Code "FLAG") ", " (Code "TAKEBIT") "意味着它可以被"
      "玩家捡起, " (Code "LIGHTBIT") "意味着它可以被点亮. 铜灯现在不是开着的, "
      "一旦它被打开, 其就拥有了" (Code "ONBIT") ", 意味着它正在发着光.")
   (P (Code "ACTION") "性质将" (Code "LANTERN-F") "作为处理与铜灯相关的"
      "输入的动作例程. 例如, 若玩家输入"
      (Code "THROW THE NERF BALL AT THE BRASS LANTERN") ", 那么"
      (Code "LANTERN") "对象就是" (Code "PRSI") ", 而例程" (Code "LANTERN-F")
      "将首先拥有处理输入的机会. 如果玩家输入了"
      (Code "THROW THE BRASS LANTERN AT THE NERF BALL")
      ", 那么" (Code "LANTERN") "将会是" (Code "PRSO") ". 当nerf ball的"
      "动作例程不能处理输入的时候, " (Code "LANTERN-F")
      "也将拥有处理输入的机会. ")
   (P (Code "FDESC") "性质是一个字符串, 当玩家第一次捡起铜灯的时候, "
      "它将被用来描述铜灯. " (Code "LDESC") "是当铜灯在地上的时候用来描述"
      "铜灯的字符串. 如果一个物品没有" (Code "FDESC") "或" (Code "LDESC")
      ", 那么" (Code "DESC") "性质就会被用来描述对象.")
   (P (Code "SIZE") "属性定义了物品的大小/重量. 这帮助游戏判断玩家是否可以"
      "捡起某个东西, 还是说已经拿得太多了. 如果不给可拿的物品提供"
      (Code "SIZE") ", 通常默认的情况是" (Code "5") ".")
   (H2 "第4章 ZIL中的例程")
   (P "例程的基本形式如下:")
   (CodeB "&lt;ROUTINE ROUTINE-NAME (argument-list)
  &lt;guts of the routine>>")
   (P "命名例程有许多约定, 例如通常物品和房间的动作例程的名字以"
      (Code "-F") "结尾. 参数列表跟在例程的名字之后出现. 参数是只在"
      "特定例程中被使用的变量, 这有别于全局变量. 以下是两个简单的例程:"
      (CodeB "&lt;ROUTINE TURN-OFF-HOUSE-LIGHTS ()
  &lt;FCLEAR ,LIVING-ROOM ,ONBIT>
  &lt;FCLEAR ,DINING-ROOM ,ONBIT>
  &lt;FCLEAR ,KITCHEN ,ONBIT>>
&lt;ROUTINE INCREMENT-SCORE (NUM)
  &lt;SETG SCORE &lt;+ ,SCORE .NUM>>
  &lt;COND (,SCORE-NOTIFICATION-ON
         &lt;TELL &quot;[Your score has just gone up by &quot;
               N .NUM &quot;.]&quot; CR>)>>")
      "调用例程的时候, 例程的名字写在开头, 参数跟在后面, 用尖括号括起来.")
   (P "除了正常的参数之外, 还有两类参数, 可选参数和辅助参数, 分别以"
      (Code "&quot;OPT&quot;") "和" (Code "&quot;AUX&quot;") "标示. "
      "描述例程的参数时, 必须按照正常参数, 可选参数, 辅助参数的顺序. "
      "以下是一个例子."
      (CodeB "&lt;ROUTINE RHYME (&quot;AUX&quot; ARG1 ARG2)
  &lt;SET ARG1 30>
  &lt;SET ARG2 &quot;September&quot;>
  &lt;LINE-IN-RHYME .ARG1 .ARG2>
  &lt;SET ARG1 28>
  &lt;SET ARG2 &quot;February&quot;>
  &lt;LINE-IN-RHYME .ARG1 .ARG2>
  etc.>
&lt;ROUTINE LINE-IN-RHYME (ARG-A ARG-B)
  &lt;TELL N .ARG-A &quot; days hath &quot; .ARG-B &quot;.&quot; CR>>")
      "调用" (Code "RHYME") "的时候, 是不需要参数的, 因为"
      (Code "ARG1") "和" (Code "ARG2") "都是辅助参数. 再看一个例子."
      (CodeB "&lt;ROUTINE CALLEE (X &quot;OPT&quot; Y &quot;AUX&quot; Z)
  &lt;some-stuff>>")
      "你有两种调用" (Code "CALLEE") "的方式, 例如"
      (Code "&lt;CALLEE .FOO>") "或者" (Code "&lt;CALLEE .FOO .BAR>") ".")
   (P "编写例程的时候, 你总是需要用到条件表达式" (Code "COND")
      ", 以下是其一般格式:"
      (CodeB "&lt;COND (&lt;predicate-1>
       &lt;do-stuff-1>)
      (&lt;predicate-2>
       &lt;do-stuff-2>)
      (&lt;predicate-3>
       &lt;do-stuff-3>)>")
      "其行为就和其他Lisp方言里的差不多.")
   (P "在正常情况下, 调用一个例程返回其最后一个表达式的值, 不过读者也可以通过"
      (Code "&lt;RTRUE>") "和" (Code "&lt;RFALSE>") "提前返回真或假, 或者通过"
      (Code "RETURN") "提前返回其他什么值.")
   (P "ZIL代码中有些东西看起来像例程, 不过你在游戏程序的任何地方都找不到, 这些"
      "东西被称为ZIL指令. ZIL指令是游戏与运行在微机上的解释器交流的手段, 有时其也"
      "被称为操作码 (op-code). 关于当前ZIL指令的完整列表, 请参考附录D.")
   (H2 "第5章 简单动作例程")
   (P "假设现在你有了一个对象" (Code "AVOCADO") ", 带有性质" (Code "(ACTION AVOCADO-F)")
      ", 那么" (Code "AVOCADO-F") "就是" (Code "AVOCADO") "的动作例程了, 它长得可能像以下这样:"
      (CodeB "&lt;ROUTINE AVOCADO-F ()
  &lt;COND (&lt;VERB? EAT>
         &lt;REMOVE ,AVOCADO>
         &lt;TELL &quot;The avocado is so delicious that you
eat it all.&quot; CR>)
        (&lt;VERB? CUT OPEN>
         &lt;FSET ,AVOCADO ,OPENBIT>
         &lt;MOVE ,AVOCADO-PIT ,AVOCADO>
         &lt;TELL &quot;You halve the avocado, revealing a
gnarly pit.&quot; CR>)>>")
      (Code "(VERB? EAT)") "为真当且仅当" (Code "PRSA") "是" (Code "EAT")
      ". 若的确如此, 那么先" (Code "REMOVE") "这个鳄梨, 即将其" (Code "LOC")
      "设置为假, 然后调用" (Code "TELL") "输出想要告诉玩家的信息. "
      "如果" (Code "EAT") "不是动词, 而" (Code "CUT") "或者" (Code "OPEN")
      "是动词的话, 那么就会先调用" (Code "FSET") ", 它的意思是&quot;flag set"
      "&quot;, 将为" (Code "AVOCADO") "添加" (Code "OPENBIT") "flag. 如果你想要去除某个flag, "
      "那就调用" (Code "FCLEAR") ". 接着, 还是调用" (Code "TELL") "告诉玩家发生了什么. "
      "若动词不是以上三个, 那么" (Code "AVOCADO-F") "就没能成功处理输入. 如果"
      (Code "AVOCADO") "是" (Code "PRSI") ", 那么接下来就由" (Code "PRSO")
      "处理输入. 如果" (Code "AVOCADO") "是" (Code "PRSO") ", 那么接下来就由动词处理输入.")
   (P "除了" (Code "VERB?") ", 还有其他许多常用的谓词, 例如" (Code "EQUAL?")
      ", 请看例子:"
      (CodeB "&lt;EQUAL? ,HERE ,DRAGONS-LAIR>")
      (Code "HERE") "是一个全局变量, 其总是被设置为当前的房间. "
      "引用全局变量的值时, 前面要加上逗号. 引用局部变量的值时, 前面要加上点号. "
      "许多全局变量只是取布尔值而已, 因此它们本身就是谓词. 实际上, "
      (Code "EQUAL?") "可以接受多于两个参数.")
   (P (Code "FSET?") "判断一个对象是否拥有某个flag, 例如"
      (CodeB "&lt;FSET? ,AVOCADO ,OPENBIT>"))
   (P (Code "IN?") "可以用来检查一个对象的位置, 例如"
      (CodeB "&lt;IN? ,EGGS ,BASKET>"))
   (P (Code "AND") ", " (Code "OR") ", " (Code "NOT")
      "表现得就如同其他Lisp方言.")
   (P "房间的动作例程不是用来直接处理玩家的输入的. 一般而言, 房间的动作例程会接受一个参数"
      (Code "RARG") ", 代表room argument, 房间参数之意. 通常的"(Code "RARG") "有"
      (Code "M-LOOK") "和" (Code "M-ENTER") ". 许多时候, 处理" (Code "M-ENTER")
      "时, 房间的动作例程会做些玩家不可见的事情.")
   (H2 "第6章 事件")
   (P "不是所有的文本都为了回应玩家的输入, 有些也可能是事件的结果, 事件也被称为中断. "
      "中断的命名约定是在前面加上" (Code "I-") ", 例如" (Code "I-GUNSHOT")
      ", 以下是一个简单中断的例子:"
      (CodeB "&lt;ROUTINE I-OTTO-GOES-NUTS ()
  &lt;FSET ,UNCLE-OTTO ,LOONEYBIT>
  &lt;COND (&lt;IN? ,UNCLE-OTTO ,HERE>
         &lt;TELL
&quot;Sigh; it appears that Uncle Otto's delusion has
returned; he has begun shouting orders to unseen
troops.&quot; CR>)>>"))
   (P "在绝大多数回合里, 时间会在故事之中流逝, 例外可能有parser没能解析玩家的输入之类的情况. "
      "在每个回合的结尾, 已经处理好了玩家的输入, 时间也被推进了, 此时一个被称为" (Code "CLOCKER")
      "的例程会被调用. 它会为被安排上的中断例程倒计时, 并调用到点了的中断.")
   (P "任何进行了" (Code "TELL") "的中断应该返回真, 否则应该返回假. 这是为了动词"
      (Code "WAIT") "的方便, 其作用是忽略数个回合. 中断必须能够终止" (Code "WAIT")
      ", 以防以下事情发生:"
      (CodeB ">WAIT
Time passes ...
    A truck begins speeding toward you.
    The truck loudly honks its horn.
    Since you refuse to move out of the way, the truck
merges you into the pavement."))
   (P "中断是由作者加入的. 有可能在游戏的开头, 或许是为了响应玩家的某个动作. "
      "一个中断例程或许也会加入其他的中断例程. 调用" (Code "QUEUE")
      "加入例程, 它有两个参数, 一个是中断, 另一个是倒计时回合数:"
      (CodeB "&lt;QUEUE I-SHOOTING-STAR 10>")
      "倒计时为" (Code "1") "的, 本回合就会开始运行. 倒计时为" (Code "2")
      "的, 那就是下回合, 依此类推. 一般情况下, 一个中断只会运行一次, 除非"
      "你再次加入它. 例外情况在于, 如果倒计时为" (Code "-1") ", 那么每回合都会运行, "
      "除非你主动" (Code "DEQUEUE") "它.")
   (P "倒计时为" (Code "-1") "的一个例子是之前看到的卡车的中断" (Code "I-TRUCK") ", 它与"
      "一个被初始化为" (Code "0") "的全局变量" (Code "TRUCK-COUNTER") "协同运作:"
      (CodeB "&lt;ROUTINE I-TRUCK ()
  &lt;SETG TRUCK-COUNTER &lt;+ ,TRUCK-COUNTER 1>>
  &lt;COND (&lt;EQUAL? ,TRUCK-COUNTER 1>
         &lt;MOVE ,TRUCK ,STREET>
         &lt;TELL
&quot;A truck begins speeding toward you.&quot; CR>)
        (&lt;EQUAL? ,TRUCK-COUNTER 2>
         &lt;TELL
&quot;The truck loudly honks its horn.&quot; CR>)
        (&lt;EQUAL? ,TRUCK-COUNTER 3>
         &lt;COND (&lt;EQUAL? ,HERE ,STREET>
                &lt;JIGS-UP
&quot;Since you refuse to move out of the way, the truck
merges you into the pavement.&quot;>)
               (T ;&quot;you've gotten out of the way&quot;
                &lt;TELL
&quot;The truck blasts you with hot exhaust fumes as it
rumbles past.&quot; CR>)>)
        (T ;&quot;counter is 4&quot;
         &lt;DEQUEUE I-TRUCK>
         &lt;SETG TRUCK-COUNTER 0>
         &lt;TELL
&quot;The truck vanishes in the direction of Hoboken.&quot;
CR>)>>")
      "分号后面是注释. 例程体的第一行是在计数, 如此" (Code "I-TRUCK")
      "才知道卡车开了多远. 例程" (Code "JIGS-UP") "以一个字符串为参数, "
      "&quot;杀死&quot;玩家, 输出接受的字符串, 最后再提示一下玩家已经死了. "
      "这里只有当玩家还在" (Code "STREET") "的时候才会调用" (Code "JIGS-UP")
      ", 否则的话它就会提示卡车(安全地)过去了. 当" (Code "TRUCK-COUNTER")
      "为" (Code "4") "时, 也就是对应谓词为" (Code "T") "的分支. "
      "此时" (Code "I-TRUCK") "将被" (Code "DEQUEUE") ", 并且注意一下"
      (Code "TRUCK-COUNTER") "也被设置为" (Code "0") ", 如此未来再次运转"
      (Code "I-TRUCK") "时也能表现正确.")
   (P "在回合末, 但在" (Code "CLOCKER") "之前, 当前房间的动作例程"
      "将被自动地调用以参数" (Code "M-END") ". 以下是一个例子:"
      (CodeB "&lt;ROUTINE AIRPORT-F (RARG)
  &lt;COND (&lt;EQUAL? .RARG ,M-ENTER>
         &lt;QUEUE I-STRAFING 5>)
        (&lt;EQUAL? .RARG ,M-LOOK>
         &lt;TELL
&quot;You are on the tarmac of an airport runway...&quot; CR>)
        (&lt;EQUAL? .RARG ,M-END>
         &lt;TELL &quot;A plane zooms low overhead.&quot; CR>)>>")
      )
   (H2 "第7章 更多关于ZIL代码的说明")
   (P (Code "GLOBAL") "形式定义全局变量, 例如:"
      (CodeB "&lt;GLOBAL SECRET-PASSAGE-OPENED &lt;>>
&lt;GLOBAL SLEEPY T>
&lt;GLOBAL NUMBER-OF-MATCHES 5>"))
   (P "容器系统是ZIL的重要组成部分. 每个对象都有" (Code "LOC")
      ", 房间位于特殊的对象" (Code "ROOMS") "之中. 有的对象的"
      (Code "LOC") "可能为假. 容器系统决定了许多重要的事情. "
      "例如, 它决定一个对象是否可被引用. 一般情况下, 能够被引用的对象"
      "必须在场并且是可见的. 例如, 如果一个东西不在玩家所处的房间, "
      "或者它处于一个封闭容器之中, 或者它本身拥有一个"
      (Code "INVISIBLE") "flag, 那么它就没法被引用. 为了得到一个对象的"
      (Code "LOC") ", 只需要使用" (Code "LOC") ":"
      (CodeB "&lt;LOC ,OBJECT-NAME>")
      "你可以使用" (Code "IN?") "来检查对象的位置:"
      (CodeB "&lt;IN? ,PICKLE ,BARREL>")
      (Code "MOVE") "以改变对象的位置:"
      (CodeB "&lt;MOVE ,HORSE ,STABLE>")
      (Code "REMOVE") "以去除对象:"
      (CodeB "&lt;REMOVE ,HORSE>")
      "为了探查一个对象的内容, 你需要两种指令, " (Code "FIRST?") "和"
      (Code "NEXT?") ". 假设你现在有了一个叫做" (Code "KITCHEN-CABINET")
      "的对象, 它包含一个pitcher, 一个serving spoon, 还有一个severed head."
      (CodeB "&lt;FIRST? ,KITCHEN-CABINET>")
      "将返回对象" (Code "PITCHER") ", 然后"
      (CodeB "&lt;NEXT? ,PITCHER>")
      "将会是serving spoon, 它的" (Code "NEXT?") "又会是severed head. "
      "既然severed head是cabinet所包含的最后一个东西, 那么"
      (CodeB "&lt;NEXT? ,SEVERED-HEAD>")
      "根据定义应该是假.")
   (P "机动装置:")
   (H2 "第8章 更大的图景")
   (P "当玩家启动游戏的时候, 解释器第一件所做的事情是调用被称为" (Code "GO")
      "的例程, 它应该完全所有的准备工作, 例如调用" (Code "V-VERSION")
      ", 调用" (Code "V-LOOK") ", 加入必要的中断, 等等. " (Code "GO")
      "应该做的最后一件事情是调用被称为" (Code "MAIN-LOOP") "的例程, 它在某种意义上"
      "是整个游戏的king. "
      )
   (CodeB "&lt;ROUTINE MAIN-LOOP (argument-list-from-hell)
  &lt;REPEAT ()
    &lt;PARSER>
    &lt;COND (&lt;did-the-parser-fail?>
           &lt;AGAIN>)>
    &lt;PERFORM ,PRSA ,PRSO ,PRSI>
    &lt;COND (&lt;did-this-input-cause-time-to-pass?>
           &lt;call-room-function-with-M-END>
           &lt;CLOCKER>)>>>")
   (H2 "第9章 句法文件")
   
   (H2 "第10章 演员")

   (H2 "第11章 描述过程")

   (H2 "第12章 一些复杂的东西")

   (H2 "第13章 图形和声音")

   (H2 "第14章 组织你的ZIL文件")

   (H2 "第15章 庆祝时刻&mdash;编译你的游戏")

   (H2 "第16章 使用ZIL编写其他类型的游戏")

   (H2 "附录A: 性质")

   (H2 "附录B: flag")

   (H2 "附录C: 常用例程")

   (H2 "附录D: ZIL指令")
   
   ))