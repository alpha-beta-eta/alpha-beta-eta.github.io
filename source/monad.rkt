#lang racket
(provide monad.html)
(require SMathML)
(define $unit (Mi "unit"))
(define $star (Mi "star"))
(define $identity (Mi "identity"))
(define $Sequel (Mi "Sequel"))
(define $sequel (Mi "sequel"))
(define $unit_M (_ $unit $M))
(define $star_M (_ $star $M))
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
      $MB ", 而这个额外的值又传递给这个得到的" $MB ".")
   
   (H3. "推导状态monad")
   (H2. "其他一些monad")
   (H3. "可能monad (Maybe Monad)")
   (H3. "异常monad")
   
   ))