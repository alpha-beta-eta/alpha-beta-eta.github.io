#lang racket
(provide monad.html)
(require SMathML)
(define $maybe (Mi "maybe"))
(define $uArr (Mo "&uArr;"))
(define $body (Mi "body"))
(define $new-a (Mi "new-a"))
(define $unit (Mi "unit"))
(define $star (Mi "star"))
(define $identity (Mi "identity"))
(define $Sequel (Mi "Sequel"))
(define $sequel (Mi "sequel"))
(define $unit_M (_ $unit $M))
(define $star_M (_ $star $M))
(define $unit_maybe (_ $unit $maybe))
(define $star_maybe (_ $star $maybe))
(define $identity_M (_ $identity $M))
(define $Sequel_M (_ $Sequel $M))
(define $sequel_M (_ $sequel $M))
(define $state (Mi "state"))
(define $unit_state (_ $unit $state))
(define $star_state (_ $star $state))
(define $space:2ex (&space 2))
(define $MA (ap $M $A))
(define $MB (ap $M $B))
(define $ma (Mi "ma"))
(define $mb (Mi "mb"))
(define $car (Mi "car"))
(define $cdr (Mi "cdr"))
(define (@@ . x*)
  (apply @ (add-between x* $space:2ex)))
(define (App f . x*)
  (apply @@ f x*))
(define (Lam x* body)
  (@@ $lambda x* body))
(define monad.html
  (TnTmPrelude
   #:title "Schemer的monad之见"
   #:css "styles.css"
   (H1. "Schemer的monad之见")
   (H2. "状态monad")
   (H3. "简谈monad")
   (P "一个monad是一对函数" $unit_M "和" $star_M
      ", 这两者可以合作完成有趣的事情. "
      "一对特定的" $unit_M "和" $star_M
      "是一个monad, 如果以下" (Em "monad律") "成立:"
      (Ul (Li (&= (App $star_M $unit_M) $identity_M))
          (Li (&= (App $compose (App $star_M $f) $unit_M) $f))
          (Li (&= (App $star_M (App $compose (App $star_M $f) $g))
                  (App $compose
                       (App $star_M $f)
                       (App $star_M $g)))))
      "其中" $compose "是" (Em "复合") "函数, 被定义为"
      (Lam (@@ $f $g)
           (Lam (@@ $x)
                (App $f (App $g $x))))
      ", 其取两个函数然后将其复合.")
   (P "如果我们所做的是给别人呈现新的monad, "
      "那么我们就不得不证明monad律对于我们所提出的"
      $unit_M "和" $star_M "成立. 但是现在, "
      "我们将只会处理已知的monad. "
      "如果我们想要令别人相信一个monad的确是一个monad, "
      "那么证明就是完全必要的.")
   (P "我们需要写下我们的代码以使得给定两个表达式, "
      "我们可以很快观察出这两个表达式之中哪一个首先出现. "
      "我们将这种编码风格称为" (Em "monad风格")
      ". 只要你能够识别出何时一个函数调用是(或不是)参数简单的尾调用, "
      "那么理解如何以monad风格编写代码也是容易的事情了. "
      "一旦定义已经被置于monad风格, "
      "那么存在一种机制用来为定义插入" (Q "操作")
      "以给出对于某种effect的刻画. "
      "在这第一次讲座之中, "
      "这将会是对于可修改变量 (settable variable) 的刻画. "
      "在第二次讲座里, 我们将会引入其他effect.")
   (P "monad风格的函数是基于两个函数之上的: "
      "一个" $unit "和一个" $star
      ", 并且其必须构成一个monad. "
      "如果你从如下递归函数" (Code "f") "的定义开始:"
      (CodeB "(define f (λ (...) body))")
      "那么monad化的相同函数看起来会是:"
      (CodeB "(define f
  (λ (unit star)
    (if (monad? unit star)
        (letrec ((f (λ (...) body*)))
          f))))"))
   (P "不幸的是, 保证" $unit "和" $star
      "构成一个monad比起编写一个简单谓词要花费更多努力. "
      "暂时, 我们将会假定程序员可以信任"
      $unit "和" $star "的确构成了monad, "
      "这简化了我们的定义."
      (CodeB "(define f
  (λ (unit star)
    (letrec ((f (λ (...) body*)))
      f)))"))
   (P "我们使用" (Code "body*")
      "来指明" (Code "body")
      "已是monad风格. 但是, "
      "我们不打算传入特定的"
      $unit "和" $star
      ", 而是准备全局地" (Code "define")
      "它们以类似于" $unit_state
      "和" $star_state "这样的唯一名字, "
      "那么" (Code "body*") "就会和之前的"
      (Code "body") "看起来如出一辙, "
      "除了会用到特定的"
      $unit "和" $star
      ". 这个决定允许我们使用" (Code "define")
      "而非" (Code "letrec") "以支持递归."
      (CodeB "(define f (λ (...) body*))"))
   (P "使用全局定义只是封装monad的诸多方式之一. "
      "我们可以将这两个函数打包成一个cons序对, "
      "一个向量, 一个可继承的类对象, 诸如此类. "
      "一般而言, 为了支持对于某种effect的刻画, "
      "我们需要一个或者更多的与" $unit "和" $star
      "协作的辅助函数. 而当我们遇到这些辅助函数的时候, "
      "我们也会指出它们. 这些辅助函数返回了和" $unit
      "的" (Em "调用") "相同种类的值. "
      "但是, 请记住monad仅是一对满足monad律的"
      $unit "和" $star ".")
   (H3. "类型和形状")
   (P "考虑三种类型的值: " (Em "纯")
      "值, 记以" (∈ $a $A) "; 参数化于" $A
      "之上的表达式, 记以" (∈ $ma $MA)
      "; 还有函数, 记以" (∈ $sequel $Sequel)
      ", 其接受一个纯值" (∈ $a $A)
      ", 然后返回了一个monadic值" (∈ $mb $MB)
      ". 函数" $unit_M "的形状类似于类型"
      $Sequel ", 但是其返回一个" $MA
      "而非一个" $MB ". " $star_M
      "接受两个(curry化了的)参数, 一个"
      $Sequel "和一个" $MA ", 然后返回一个"
      $MB ". 因此, 我们可以记" $unit_M
      "和" $star_M "的" (Em "类型") "如下."
      (eqn*
       ($Sequel_M $= (&-> $A $MB))
       ($unit_M $: (&-> $A $MA))
       ($star_M $: (&-> $Sequel_M $MA $MB))))
   (P "这里的第一行告诉我们类型" $Sequel_M
      "是类型" (&-> $A $MB)
      "的缩略, 而接下来的两行分别告诉我们了表达式"
      $unit_M "和" $star_M "的类型.")
   (P "根据monad律, 我们知道表达式"
      (App $star_M $unit_M)
      "是合法的, 尽管似乎" $star_M
      "想要的是类型" $Sequel_M
      "的一个值作为其第一个参数. "
      "因此, 我们必然知道" $unit_M
      "和一个" $Sequel_M
      "必然有着类似的形状. "
      "它们都消费一个纯值" $a
      ", 然后分别返回一个" $MA "或者一个"
      $MB ". 更进一步, "
      (App $unit_M $a) "和"
      (App (App $star_M $sequel_M) $ma)
      "返回了相同的形状, 分别是一个"
      $MA "或者一个" $MB ".")
   (H3. "状态monad")
   (P "以下是" $state
      " monad, 如此命名乃是因为其创造了对于一个单独的可变变量的刻画."
      (CodeB "(define unit_state ;A -> MA
  (λ (a)
    (λ (s) ;This function is a MA.
      `(,a . ,s))))")
      (CodeB "(define star_state ;Sequel -> MA -> MB
  (λ (sequel)
    (λ (ma)
      (λ (s) ;This function is a MB.
        (let ((p (ma s)))
          (let ((new-a (car p)) (new-s (cdr p)))
            (let ((mb (sequel new-a)))
              (mb new-s))))))))"))
   (P "让我们稍微分析一下这些定义. " $unit_state
      "接受一个纯值" (∈ $a $A) ", 然后返回一个"
      $MA ", 这个函数期望接受一个状态" (∈ $s $S)
      ". 当" $MA "得到一个状态的时候, 就返回一个序对. "
      "这个序对的" $car "部分是一个纯值, 而"
      $cdr "部分则是一个" (Em "额外")
      "的值. 这个纯值传递给一个" $Sequel "以产生一个"
      $MB ", 而这个额外的值又传递给了这个得到的" $MB ".")
   (P "对于调用" $state " " $MA "或者" $state
      " " $MB "所返回的序对, 其" $car "部分是纯值, 而"
      $cdr "部分则是额外的值. 这提醒我们这些结构总是会有一个纯值, "
      "而纯值有的时候是没有用的. " $cdr
      "里的东西是额外的值, 其用以支持各种各样的刻画.")
   (P "我们可以观察到" $star_state "接受两个(curry化了的)值, 一个函数"
      $Sequel "和一个" $MA ", 而对于状态而言" $MA
      "恰好是函数. 考虑" (Lam (@@ $ma) $body) ", 那么"
      $body "应该是一个" $MB ". 但是, 在" $star_state
      "的定义之中, " $ma "在" $sequel "之前被调用. "
      "我们令序对" $p "是应用" $ma "于某个" $s
      "的结果, 那么" $p "的" $car "是某个纯值"
      $new-a ", 其会被传递给" $sequel
      ". 应用" $sequel "于" $new-a
      "的结果是一个" $mb ", 据我们所知, 其期望着一个状态, "
      "而这个状态就在" $p "的" $cdr "之中. 因此, "
      "我们可以说" $ma "进入的时候是以一个状态, 而从"
      (∈ $mb $MB) "退出的时候则是以一个可能不同的状态.")
   (P "monad的类型信息告诉了我们该如何使用"
      $unit "和" $star "来以monad风格定义函数. "
      "那么, 现在让我们来看一个例子. "
      "我们的问题是取一个整数的嵌套(至任意深度的)列表, "
      "然后返回一对值. 这个序对的第一个项应该是同样的列表, "
      "除了其中的偶数都已经被移除, 第二个项则是被删除的偶数的数目. "
      "我们称这个函数为" (Code "remberevensXcountevens")
      ". 这里的" (Code "X") "指明函数会返回一个eXtra值.")
   (P "在我们移步至" (Code "remberevensXcountevens")
      "的monadic定义之前, 让我们先来看一个简单的直接风格的定义. "
      "我们从一个" (Q "驱动") "过程"
      (Code "remberevensXcountevens_2pass")
      "开始, 其会调用两个辅助函数"
      (Code "remberevens_pure") "和"
      (Code "countevens_pure") "."
      (CodeB "(define remberevensXcountevens_2pass
  (λ (l) `(,(remberevens_pure l) . ,(countevens_pure l))))
(define remberevens_pure
  (λ (l)
    (cond
      ((null? l) '())
      ((list?? (car l))
       (cons (remberevens_pure (car l)) (remberevens_pure (cdr l))))
      ((odd? (car l)) (cons (car l) (remberevens_pure (cdr l))))
      (else (remberevens_pure (cdr l))))))
(define countevens_pure
  (λ (l)
    (cond
      ((null? l) 0)
      ((list?? (car l))
       (+ (countevens_pure (car l)) (countevens_pure (cdr l))))
      ((odd? (car l)) (countevens_pure (cdr l)))
      (else (add1 (countevens_pure (cdr l)))))))
;原文这里存在笔误, 将两处x误写作了l
(define list??
  (λ (x)
    (or (null? x) (pair? x))))")
      (CodeB "> (remberevensXcountevens_2pass '(2 3 (7 4 5 6) 8 (9) 2))
((3 (7 5) (9)) . 5)")
      (Code "remberevensXcountevens_2pass")
      "能够给出正确的结果, 但是却非常低效: 它对于列表" (Code "l")
      "处理了两遍. 存在着一种众人皆知的解法, 其只需要处理一遍, "
      "但是需要我们将代码转换为延续传递风格."
      (CodeB "(define remberevensXcountevens_cps
  (λ (l k)
    (cond
      ((null? l) (k `(() . 0)))
      ((list?? (car l))
       (remberevensXcountevens_cps
        (car l)
        (λ (pa)
          (remberevensXcountevens_cps
           (cdr l)
           (λ (pd)
             (k `(,(cons (car pa) (car pd))
                  . ,(+ (cdr pa) (cdr pd)))))))))
      ((odd? (car l))
       (remberevensXcountevens_cps
        (cdr l)
        (λ (p)
          (k `(,(cons (car l) (car p))
               . ,(cdr p))))))
      (else
       (remberevensXcountevens_cps
        (cdr l)
        (λ (p)
          (k `(,(car p) . ,(add1 (cdr p))))))))))")
      "译注: 实际上比起这种写法, 一般人用延续传递风格编写这个过程会让"
      (Code "k") "接受两个参数, 这样会更简单一些."
      (CodeB "> (remberevensXcountevens_cps '(2 3 (7 4 5 6) 8 (9) 2) (λ (p) p))
((3 (7 5) (9)) . 5)"))
   (P "接下来我们将直接风格的" (Code "remberevens_pure")
      "转换为monad风格. " (Code "cond") "的第四个子句是一个尾调用, "
      "故保持不变就好. 对于第三个子句, 我们取(具有简单参数的)非尾调用, "
      "然后将其变为" $star_state "的第二个(curry化了的)参数."
      (CodeB "((star_state ...)
 (remberevens_pure (cdr l)))")
      "围绕非尾调用的上下文进入了" (Code "...")
      "之中, 于是我们必须要有一个变量用于绑定调用"
      (Code "(remberevens_pure (cdr l))")
      "的结果, 那么让我们称之为" (Code "d") "."
      (CodeB "((star_state (λ (d) ...))
 (remberevens_pure (cdr l)))")
      "如果我们有了一个简单表达式 (没有递归函数调用的表达式), 诸如"
      (Code "(cons (car l) d)") ", 那么为了monad化这个表达式, "
      "我们使用" $unit_state "来包裹这个简单表达式."
      (CodeB "((star_state (λ (d) (unit_state (cons (car l) d))))
 (remberevens_pure (cdr l)))")
      "考虑第二个子句. 这里我们有两个(具有简单参数的)非尾(递归)调用, "
      "于是我们需要给予它们一个顺序."
      (CodeB "((star_state (λ (a) ...))
 (remberevens_pure (car l)))")
      "在" (Code "(λ (a) ...)") "的体里, 我们进行下一个调用."
      (CodeB "((star_state (λ (a)
               ((start_state (λ (d) ...))
                (remberevens_pure (cdr l)))))
 (remberevens_pure (car l)))")
      "最后, 我们来处理递归调用的结果, 那么就是"
      (Code "(cons a d)") ", 这是一个简单表达式. "
      "又一次, 我们需要做的事情就是用" $unit_state
      "来包裹这个表达式."
      (CodeB "((star_state (λ (a)
               ((start_state (λ (d) (unit_state (cons a d))))
                (remberevens_pure (cdr l)))))
 (remberevens_pure (car l)))")
      "第一个子句是简单的, 所以说我们将" (Code "'()")
      "传递给" $unit_state ". 现在, 我们有了结果."
      (CodeB "(define remberevens
  (λ (l)
    (cond
      ((null? l) (unit_state '()))
      ((list?? (car l))
       ((star_state (λ (a)
                      ((star_state (λ (d) (unit_state (cons a d))))
                       (remberevens (cdr l)))))
        (remberevens (car l))))
      ((odd? (car l))
       ((star_state (λ (d) (unit_state (cons (car l) d))))
        (remberevens (cdr l))))
      (else
       (remberevens (cdr l))))))"))
   (P "当然了, 到现在为止我们处理的只是" (Code "remberevens")
      ", 而我们真正想要的是" (Code "remberevensXcountevens")
      ". 似乎我们只完成了一半的工作, "
      "但是monad风格的优美之处在于其实我们已经快要完成了. "
      "让我们将这个函数改名为"
      (Code "remberevensXcountevens_almost")
      ", 并让我们看看我们离目标还有多远."
      (CodeB "(define remberevensXcountevens_almost
  (λ (l)
    (cond
      ((null? l) (unit_state '()))
      ((list?? (car l))
       ((star_state (λ (a)
                      ((star_state (λ (d) (unit_state (cons a d))))
                       (remberevensXcountevens_almost (cdr l)))))
        (remberevensXcountevens_almost (car l))))
      ((odd? (car l))
       ((star_state (λ (d) (unit_state (cons (car l) d))))
        (remberevensXcountevens_almost (cdr l))))
      (else
       (remberevensXcountevens_almost (cdr l))))))"))
   (P "首先, " (Code "(remberevensXcountevens_almost l)")
      "会返回什么呢? 其会返回一个函数, 这个函数接受一个状态, "
      "然后返回一对值, 即一个纯值 (应该是"
      (Code "(remberevens_pure l)") "会返回的) 和一个额外值 "
      "(应该是移除的偶数的数目). 以下是对于"
      (Code "remberevensXcountevens_almost")
      "的一个测试."
      (CodeB "> ((remberevensXcountevens_almost '(2 3 (7 4 5 6) 8 (9) 2)) 0)
((3 (7 5) (9)) . 0)"))
   (P "这个测试里的" (Code "0") "有什么用呢? 它是状态" (Code "s")
      "的初值. 当数字的列表为空会发生什么? 其会返回"
      (Code "(unit_state '())") ", 而我们知道这是函数"
      (Code "(λ (s) `(() . ,s))") ", 通过将" (Code "a")
      "替换为空表. 然后, " (Code "s") "会被替换为"
      (Code "0") ", 这就产生了序对" (Code "(() . 0)") ".")
   (P "但是, 我们的答案只是" (Em "近乎于")
      "正确, 因为我的错误的部分仅是数目不对. "
      "什么时候我们需要计数呢? "
      "当我们知道" (Code "(car l)")
      "是一个偶数的时候. 所以说, 让我们再来看看"
      (Code "else") "子句."
      (CodeB "(remberevensXcountevens_almost (cdr l))")
      "我们该如何修订这个表达式以修正bug呢? "
      "这是一个尾调用, 所以说我们将该调用移入sequel的体内."
      (CodeB "((star_state (λ (_)
               (remberevensXcountevens_almost (cdr l))))
 ...)")
      "然后我们应该制造一个" $MA "出来, 其可以通过"
      $star_state "给出对于effect的刻画. 既然状态monad的"
      $MA "看起来像是" (Code "(λ (s) `(,a . ,s^))")
      ", 那么我们也必须使用这样的格式, "
      "并且既然我们不会关心" (Code "_")
      "被绑定到了什么值上, 所以说我们令这个纯值为符号"
      (Code "_") "也就可以了, 这给出了以下代码."
      (CodeB "((star_state (λ (_)
               (remberevensXcountevens_almost (cdr l))))
 (λ (s) `(_ . ,s^)))")
      "剩下来我们需要做的事情是确定我们想要" (Code "s^")
      "是什么. 既然进入这个" $MA "的" (Code "s")
      "是当前的计数, 那么我们可以令" (Code "s^")
      "为" (Code "(add1 s)") ", 这将我们导向了完整的"
      (Code "else") "子句."
      (CodeB "((star_state (λ (_)
               (remberevensXcountevens_almost (cdr l))))
 (λ (s) `(_ . ,(add1 s))))")
      "现在代码就是全部正确的了, "
      "所以我们可以把下标从名字里去掉."
      (CodeB "(define remberevensXcountevens
  (λ (l)
    (cond
      ((null? l) (unit_state '()))
      ((list?? (car l))
       ((star_state (λ (a)
                      ((star_state (λ (d) (unit_state (cons a d))))
                       (remberevensXcountevens (cdr l)))))
        (remberevensXcountevens (car l))))
      ((odd? (car l))
       ((star_state (λ (d) (unit_state (cons (car l) d))))
        (remberevensXcountevens (cdr l))))
      (else
       ((star_state (λ (_)
                      (remberevensXcountevens (cdr l))))
        (λ (s) `(_ . ,(add1 s))))))))")
      (CodeB "> ((remberevensXcountevens '(2 3 (7 4 5 6) 8 (9) 2)) 0)
((3 (7 5) (9)) . 5)")
      "译注: 实际上, 还可以换个写法."
      (CodeB "(define remberevensXcountevens
  (λ (l)
    (cond
      ((null? l) (unit_state '()))
      ((list?? (car l))
       ((star_state (λ (a)
                      ((star_state (λ (d) (unit_state (cons a d))))
                       (remberevensXcountevens (cdr l)))))
        (remberevensXcountevens (car l))))
      ((odd? (car l))
       ((star_state (λ (d) (unit_state (cons (car l) d))))
        (remberevensXcountevens (cdr l))))
      (else
       ((star_state (λ (d)
                      (λ (s)
                        `(,d . ,(add1 s)))))
        (remberevensXcountevens (cdr l)))))))"))
   (P "让我们来思考之前延续传递风格版本的定义. "
      "两个程序都能计算出正确的答案, 但是它们做事的方式相当不同. "
      "为了表明我们所言非虚, 让我们追踪每个版本的程序里"
      (Code "add1") "和" (Code "+") "的动向. 以下是追踪"
      (Code "remberevensXcountevens_cps") "时所发生的:"
      (CodeB "> (remberevensXcountevens_cps '(2 3 (7 4 5 6) 8 (9) 2) (λ (p) p))
(add1 0)
(add1 1)
(add1 0)
(+ 0 1)
(add1 1)
(+ 2 2)
(add1 4)
((3 (7 5) (9)) . 5)"))
   (P "从对于执行过程的追踪可以看出, " (Code "remberevensXcountevens_cps")
      "计算出" (Code "5") "是通过对于输入中的子列表计算子答案, "
      "然后使用" (Code "+") "合并子答案得到的.")
   (P "与之相对的是, 让我们看看对于monad版本程序"
      (Code "remberevensXcountevens") "的追踪:"
      (CodeB "> ((remberevensXcountevens '(2 3 (7 4 5 6) 8 (9) 2)) 0)
(add1 0)
(add1 1)
(add1 2)
(add1 3)
(add1 4)
((3 (7 5) (9)) . 5)"))
   (P "现在对于" (Code "add1") "的调用遵循着可以预测的模式, 而"
      (Code "+") "压根就没有用到! 比起我们在对于"
      (Code "remberevensXcountevens_cps")
      "的追踪中看到的由子答案构筑答案的现象, "
      "这个版本看起来我们是在对于计数器进行逐步增量.")
   (P "实际上, monad版本的计算非常类似于我们使用全局变量"
      (Code "counter") " (初始化为" (Code "0") ") 然后简单通过"
      (Code "(set! counter (add1 counter))")
      "来增长计数的情况. 但是, 我们甚至没有用到" (Code "set!")
      ". 转而, 状态monad为我们提供了对于全局可变变量的"
      (Em "刻画") ". 这是一种极其强大的想法. "
      "现在我们可以编写程序来提供对于effectful计算的忠实模拟, "
      "但是却并不需要实际执行任何side effects. "
      "也就是说, 我们得到了effectful计算的通常好处, "
      "却没有通常的缺陷.")
   (P "一个关于状态monad的最后观察是辅助函数"
      (Code "(λ (s) `(_ . ,(add1 s)))")
      "并不包含自由变量, 本可以赋予一个全局性的名字, "
      "让我们称其为" (Code "incr_state") ":"
      (CodeB "(define incr_state
  (λ (s) `(_ . ,(add1 s))))")
      "但是如果这样的话, " $sequel "和其" $ma
      (MB (&Table
           ((Ms "(λ (_) ...)") ";sequel")
           ((: $uArr (&space 8)))
           ((Ms "(λ (s) `(_ . ,(add1 s)))") ";ma")))
      "之间的关系就不甚明了了. "
      "纯值, 即符号" (Code "_")
      ", 在将状态传给" $ma "之后得到的序对的"
      $car "部分里, 然后其会被绑定至"
      $sequel "的形式变量" (Code "_")
      "上. 作成这种绑定是" $star_state
      "的工作之一.")
   (P "练习 (可以见之前的译注): "
      )
   (H3. "推导状态monad")
   (P "如果我们取" (Code "remberevensXcountevens")
      "的代码, 并将" (Code "unit_state") "和"
      (Code "star_state") "代之以它们的定义, "
      "那么对于" (Code "(let ((x e)) body)")
      "或者等价的" (Code "((λ (x) body) e)")
      "存在着将" (Code "body") "中的" (Code "x")
      "替换为" (Code "e") "的机会. 如果我们知道"
      (Code "x") "在" (Code "body")
      "之中恰出现一次, 那么这些是保持正确性和效率的变换. "
      "{译注: 依赖于代码是pure的.} "
      "我们这里所执行的变换的详细步骤见于附录 (总计三十六步), "
      "但是其结果是" (Em "状态传递风格") "的代码, "
      "其中状态作为参数进出于每个递归函数调用. "
      "我们可能写过这种代码, 但是却不知道状态monad的存在."
      (CodeB "(define remberevensXcountevens_sps
  (λ (l s)
    (cond
      ((null? l) `(() . ,s))
      ((list?? (car l))
       (let* ((p (remberevensXcountevens_sps (car l) s)))
         (let ((p^ (remberevensXcountevens_sps (cdr l) (cdr p))))
           `(,(cons (car p) (car p^)) . ,(cdr p^)))))
      ((odd? (car l))
       (let ((p (remberevensXcountevens_sps (cdr l) s)))
         `(,(cons (car l) (car p)) . ,(cdr p))))
      (else
       (let ((p (remberevensXcountevens_sps (cdr l) s)))
         `(,(car p) . ,(add1 (cdr p))))))))")
      (CodeB "> (remberevensXcountevens_sps '(2 3 (7 4 5 6) 8 (9) 2) 0)
((3 (7 5) (9)) . 5)"))
   (P "我们也可以从" (Code "remberevensXcountevens_sps")
      "出发推导" (Code "unit_state") "和" (Code "star_state")
      ", 因为这种变换是可逆的. {译注: 非常可疑的说法.}")
   (P "这就结束了第一次讲座, "
      "第二次讲座里我将呈现各种其他monad以及如何使用它们.")
   (H2. "其他一些monad")
   (H3. "可能monad (Maybe Monad)")
   (P "以下是" $maybe " monad."
      (CodeB "(define unit_maybe
  (λ (a)
    `(,a . _))) ;This MA get its type from the type of a.")
      (CodeB "(define star_maybe
  (λ (sequel)
    (λ (ma)
      (cond ;This is a MB.
        ((eq? (cdr ma) '_)
         (let ((a (car ma)))
           (sequel a)))
        (else (let ((mb ma))
                mb))))))"))
   (P $cdr "里的标记" (Code "_") "指明了纯值就在"
      $car "里, 这和之前的状态monad是一样的. "
      "我们立即发现对于这个monad而言似乎存在着多余的方面. "
      "如果你回忆一下状态monad, 会发现一切都是自足的; "
      "然而, 在这里事情并不那么明朗显然. "
      "但是, 既然" $unit_maybe "里用了符号" (Code "_")
      ", " $star_maybe "对于符号" (Code "_")
      "又有专门的分派, 所以至少monad律的前两个等式是成立的.")
   (P "如果你使用过Scheme的" (Code "assq")
      ", 那么就会明白为了检查可能的失败, "
      "程序的结构是多么得病态 (what an ill-structured mess). "
      "可能monad允许编程者在更高层次进行思考, "
      "从而忽略处理失败. 考虑" (Code "new-assq")
      ", 其类似于" (Code "assq") ". 它的工作是返回一个"
      $MA " (此时是一个序对), 其" $car "会是"
      (Code "p*") "中第一个满足" $car "匹配" (Code "v")
      "的序对的" $cdr "."
      (CodeB "(define new-assq
  (λ (v p*)
    (cond
      ((null? p*) `(_ . fail))
      ((eq? (caar p*) v)
       (unit_maybe (cdar p*)))
      (else
       ((star_maybe (λ (a) (unit_maybe a)))
        (new-assq v (cdr p*)))))))")
      "既然" (Code "(new-assq v (cdr p*))")
      "是一个尾调用, 我们可以依据" $eta
      "归约和monad律第一等式重写"
      (Code "new-assq") ", 即"
      (CodeB "(define new-assq
  (λ (v p*)
    (cond
      ((null? p*) `(_ . fail))
      ((eq? (caar p*) v)
       (unit_maybe (cdar p*)))
      (else
       (new-assq v (cdr p*))))))")
      "每个" (Code "cond")
      "的子句的右支都应该是" $MA
      ". 终结递归调用的是前两个子句. "
      "(鉴于" (Code "(_ . fail)")
      "的" $cdr "是符号" (Code "fail")
      ", 所以说你不会将其与" $cdr
      "为符号" (Code "_") "的情况混淆.) "
      "为了看看我们如何使用" (Code "new-assq")
      ", 请看以下测试."
      (CodeB "> ((star_maybe (λ (a) (new-assq a '((1 . 10) (2 . 20)))))
   ((λ (ma1 ma2)
      (cond
        ((eq? (cdr ma1) '_) ma1)
        (else ma2)))
    (new-assq 8 '((7 . 1) (9 . 3)))
    (new-assq 8 '((9 . 4) (6 . 5) (8 . 2) (7 . 3)))))"))
   (P "我们需要验证" $star_maybe
      "的第二个(curry化了的)参数的确是一个" $MA
      ". 以上的两个" (Code "cond") "子句里, 结果均是一个"
      $MA ". 这里我们在两个不同的关联列表里寻找"
      (Code "8") ". 然后, 我们取了纯值" (Code "2")
      ", 在第三个关联列表里寻找它. 这将返回"
      (Code "(20 . _)") ". 在" (Code "cond")
      "子句里, 当我们失败时, 我们将会尝试另外一个"
      $MA ", 但是如果成功, 就会使用第一个. "
      "然后, " (Code "a") "就会被绑定到纯值" (Code "2")
      ". 我们的这个定义的一个缺陷在于前两次对于"
      (Code "new-assq") "的调用都会被求值, "
      "这是因为我们并不是在类似于Haskell这样的按需调用语言里进行编程. "
      "如果我们想要获得通常Haskell的好处, "
      "就需要重新定义第二个" $MA "为一个thunk."
      (CodeB "> ((star_maybe (λ (a) (new-assq a '((1 . 10) (2 . 20)))))
   ((λ (Ma1 Ma2)
      (cond
        ((eq? (cdr Ma1) '_) Ma1)
        (else (Ma2))))
    (new-assq 8 '((7 . 1) (9 . 3)))
    (λ () (new-assq 8 '((9 . 4) (6 . 5) (8 . 2) (7 . 3))))))
(20 . _)")
      
      )
   (H3. "异常monad")
   
   ))