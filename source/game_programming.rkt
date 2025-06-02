#lang racket
(provide game_programming.html)
(require SMathML)
(define game_programming.html
  (TnTmPrelude
   #:title "游戏编程"
   #:css "styles.css"
   (H1. "游戏编程")
   (P "其实作者也不懂游戏编程, 但是作者希望探索游戏编程, "
      "寻找隐藏在各种编程语言和游戏引擎之下的" (Q "不变量")
      ". 基本上作者会采用Racket这种非主流编程语言, "
      "这将有助于抛开既有的套路和成见.")
   (H2. "游戏编程的基本概念")
   (H3. "游戏世界")
   (P "游戏世界即玩家游玩或者与之交互的世界. "
      "一般来说, 游戏世界需要响应玩家的操作, "
      "也需要响应时间的流逝. "
      "一些古早的命令行游戏并非如此, "
      "它们往往以命令或者有效命令的数目作为时间的度量. "
      "游戏世界的表示并没有什么魔法, "
      "只是存在于计算机器之中的数据结构. "
      "当然了, 存在不同的表示策略, "
      "其中最为人熟知也最为重要的想法大概就是面向对象了. "
      "不过, 面向对象也带来了一些世界观 (或者说架构) "
      "和性能的挑战, 因而人们也开始考虑诸如ECS的替代方案. "
      "虽然ECS常被称为解决游戏性能问题的灵丹妙药, "
      "但我想强调的是, 它也带来了另一种描述世界的方式.")
   (H3. "游戏循环")
   (P "不论命令行游戏还是图形游戏, "
      "一般的话游戏运行于一个循环之中, "
      "其被称为游戏循环. "
      "游戏循环需要响应玩家的输入, "
      "由此改变世界的状态, "
      "并给玩家提出反馈. "
      "这里的话, 实时游戏和非实时游戏稍有不同 "
      "(虽然有时它们也约等于图形游戏和命令行游戏), "
      "在于其要随着一个时钟不停地更新世界状态, "
      "并且要不停地更新反馈, "
      "当然对于图形游戏而言就是不停地渲染画面. "
      "以下是一个简单的非实时游戏循环的例子, "
      "虽然很难称得上是一个游戏就是了.")
   (CodeB "(define (game-begin)
  (let ((world 0))
    (let loop ()
      (define input
        (read-line))
      (cond
        ((string=? input &quot;add1&quot;)
         (set! world (add1 world)))
        ((string=? input &quot;sub1&quot;)
         (set! world (sub1 world)))
        (else
         (printf &quot;unknown input\\n&quot;)))
      (printf &quot;current world: ~s\\n&quot; world)
      (loop))))")
   (CodeB "> (game-begin)
add1
current world: 1
add1
current world: 2
sub1
current world: 1
add1
current world: 2
foo
unknown input
current world: 2
bar
unknown input
current world: 2
sub1
current world: 1
sub1
current world: 0")
   (H3. "游戏对象")
   (P "传统上游戏中的各种实体 (可能不仅是角色和物品, "
      "还包含其他各种各样的东西, 例如" (Q "法术")
      ") 总是被表示为对象, "
      "这不仅是组织程序的策略. "
      "实际上, 即便在面向对象还不流行的时候, "
      "虽然很多游戏并没有使用技术意义上的面向对象 "
      "(例如, 使用超长的record表示角色), "
      "但是仍然是围绕面向对象的观念来设计和实现游戏的.")
   (CodeB "(define make-obj
  (let ((id* '()))
    (define (add-id! id)
      (unless (symbol? id)
        (error 'make-obj &quot;id [~s] should be a symbol.&quot; id))
      (if (memq id id*)
          (error 'make-obj &quot;id [~s] has been used.&quot; id)
          (set! id* (cons id id*))))
    (lambda (id)
      (add-id! id)
      (lambda (msg)
        (case msg
          ((get-id) (lambda (self) id))
          (else #f))))))
(define (method? x) (procedure? x))
(define (tell obj msg . arg*)
  (define method (obj msg))
  (unless (method? method)
    (error (get-id obj) &quot;unknown message [~s]&quot; msg))
  (apply method obj arg*))
(define (get-id obj) (tell obj 'get-id))")
   (H3. "状态机")
   (P "状态机的概念提供了一套设计和实现游戏交互的方法论, "
      "并且也附带可以视为一种对于边界条件的防御措施. "
      "简而言之, 状态机规定了从一个状态怎么转移到其他状态."
      )
   (CodeB "(define world (void))
(define state (void))
(define input (void))
(define highest (void))
(define (game-begin)
  (set! state 'menu)
  (game-loop))
(define (game-loop)
  (set! input (read-line))
  (dispatch))
(define (dispatch)
  (case state
    ((menu) (handle-menu))
    ((highest) (handle-highest))
    ((game) (handle-game))
    (else
     (error 'dispatch
            &quot;unknown state ~s&quot;
            state))))
(define (handle-menu)
  (cond
    ((string=? input &quot;exit&quot;)
     (set! state 'exit)
     (printf &quot;you exit the whole game.\\n&quot;))
    ((string=? input &quot;highest&quot;)
     (if (eq? highest (void))
         (printf &quot;there is no record.\\n&quot;)
         (printf &quot;HIGHEST SCORE: ~s\\n&quot; highest))
     (set! state 'highest)
     (game-loop))
    ((string=? input &quot;start&quot;)
     (set! world 0)
     (set! state 'game)
     (printf &quot;you enter a new game.\\n&quot;)
     (game-loop))
    (else
     (printf &quot;unknown input\\n&quot;)
     (game-loop))))
(define (handle-highest)
  (cond
    ((string=? input &quot;exit&quot;)
     (set! state 'menu)
     (printf &quot;you get back to the main menu.\\n&quot;))
    (else
     (printf &quot;unknown input\\n&quot;)))
  (game-loop))
(define (handle-game)
  (cond
    ((string=? input &quot;add1&quot;)
     (set! world (add1 world))
     (printf &quot;current world: ~s\\n&quot; world))
    ((string=? input &quot;sub1&quot;)
     (set! world (sub1 world))
     (printf &quot;current world: ~s\\n&quot; world))
    ((string=? input &quot;exit&quot;)
     (if (eq? highest (void))
         (set! highest world)
         (set! highest (max highest world)))
     (set! state 'menu)
     (printf &quot;you exit the current battle.\\n&quot;))
    (else
     (printf &quot;unknown input\\n&quot;)))
  (game-loop))")
   (CodeB "> (game-begin)
highest
there is no record.
exit
you get back to the main menu.
start
you enter a new game.
add1
current world: 1
sub1
current world: 0
exit
you exit the current battle.
highest
HIGHEST SCORE: 0
exit
you get back to the main menu.
start
you enter a new game.
add1
current world: 1
add1
current world: 2
exit
you exit the current battle.
highest
HIGHEST SCORE: 2
exit
you get back to the main menu.
exit
you exit the whole game.")
   (H3. "游戏状态的保存")
   (P "某些古早游戏并不考虑游戏状态的保存, 或者说存档问题. "
      "然而, 对于绝大多数游戏而言, 这仍然是一个相当实际的问题. "
      "古早的游戏状态保存可能涉及手工设计的数据结构和数据格式, "
      "然而现在的游戏总是要用到编程语言本身或者库提供的反序列化机制. "
      
      )
   ))