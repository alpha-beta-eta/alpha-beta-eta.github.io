#lang racket
(provide generalization.html)
(require SMathML)
(define-syntax-rule (define-code* (id str) ...)
  (begin
    (define id (Code str))
    ...))
(define-code*
  (:typing/ "typing/")
  (:parsing/parsetree.mli "parsing/parsetree.mli")
  (:typecore.ml "typecore.ml")
  (:ctype.ml "ctype.ml")
  (:types.mli "types.mli")
  (:ident.ml "ident.ml")
  (:Parsetree "Parsetree")
  (:Typedtree "Typedtree")
  (:Parsetree.expression "Parsetree.expression")
  (:Typedtree.expression "Typedtree.expression")
  (:let "let")
  (:letregion "letregion")
  (:e "e")
  (:x "x")
  (:y "y")
  (:t "t")
  (:t^ "t'")
  (:M "M")
  (:M.t "M.t")
  (:exp "exp")
  (:env "env")
  (:tv "tv")
  (:ty "ty")
  (:ty1 "ty1")
  (:ty2 "ty2")
  (:ty_x "ty_x")
  (:ty_y "ty_y")
  (:ty_z "ty_z")
  (:ty_res "ty_res")
  (:ty_x/1 "ty_x/1")
  (:ty_z/2 "ty_z/2")
  (:TVar "TVar")
  (:QVar "QVar")
  (:TArrow "TArrow")
  (:Tconstr "Tconstr(p,args)")
  (:Tsubst "Tsubst")
  (:newvar "newvar")
  (:newgenvar "newgenvar")
  (:inst "inst")
  (:typeof "typeof")
  (:gen "gen")
  (:occurs "occurs")
  (:unify "unify")
  (:repr "repr")
  (:instance "instance")
  (:generalize "generalize")
  (:sound_eager "sound_eager")
  (:sound_lazy "sound_lazy")
  (:current_level "current_level")
  (:enter_level "enter_level")
  (:leave_level "leave_level")
  (:generic_level "generic_level")
  (:marked_level "marked_level")
  (:nongen_level "nongen_level")
  (:global_level "global_level")
  (:level "level")
  (:level_old "level_old")
  (:level_new "level_new")
  (:update_level "update_level")
  (:to_be_level_adjusted "to_be_level_adjusted")
  (:force_delayed_adjustments "force_delayed_adjustments")
  (:enter_def "enter_def()")
  (:end_def "end_def()")
  (:begin_def "begin_def")
  (:Btype.repr "Btype.repr")
  (:Ctype.unify_var "Ctype.unify_var")
  (:Ctype.instance "Ctype.instance")
  (:Ctype.copy "Ctype.copy")
  (:Ctype.current_level "Ctype.current_level")
  (:Ctype.newty "Ctype.newty")
  (:Ctype.newvar "Ctype.newvar")
  (:Ctype.update_level "Ctype.update_level")
  (:Ctype.generalize_expansive "Ctype.generalize_expansive")
  (:Ident.currentstamp "Ident.currentstamp")
  (:Ident.set_current_time "Ident.set_current_time")
  (:Typecore.type_cases "Typecore.type_cases")
  (:type_expr "type_expr")
  (:type_expression "type_expression")
  (:type_let "type_let")
  (:generalize_expansive "generalize_expansive")
  (:e_source "e_source")
  (:e_typed "e_typed")
  (:exp_type "exp_type")
  (:ty.level "ty.level")
  (:body "body")
  (:?name "?name")
  (:some_type "some_type")
  (:list_type "list_type")
  (:arg "arg")
  (:p "p")
  (:ref "ref")
  (:array "array")
  (:list "list")
  (:-rectypes "-rectypes"))
(define (Href url . html*)
  (keyword-apply A '(#:attr*) `(((href ,url))) html*))
(define (Pid str)
  (Mi str #:attr* '((mathvariant "monospace"))))
(define $let (Pid "let"))
(define $in (Pid "in"))
(define split2 (&split 2))
(define (Let x e1 e2)
  (split2 $let (&= x e1) $in e2))
(define $GEN (Mi "GEN"))
(define (&GEN G t) (appl $GEN G t))
(define $FV (Mi "FV"))
(define (FV x) (app $FV x))
(define $log (Mi "log"))
(define (∀ . x*)
  (let-values (((x* t*) (split-at-right x* 1)))
    (apply : $forall (append x* (list $. (car t*))))))
(define-infix*
  (&vdash $vdash))
(define (&rule #:space [n 8] . j*)
  (let-values (((j* j1) (split-at-right j* 1)))
    (~ #:attr* '((displaystyle "true"))
       (apply (&split n) j*) (car j1))))
(define-syntax SimpleTable
  (syntax-rules ()
    ((_ (x ...) ...)
     (Table
      (Tr (Td x) ...)
      ...))))
(define generalization.html
  (TnTmPrelude
   #:title "高效而富有洞见的泛化"
   #:css "styles.css"
   (H1. "OCaml类型检查器如何工作" -- "或者说, 多态和垃圾回收有何共同之处")
   (P "Oleg Kiselyov (okmij.org). 原文标题为" (Q "Efficient and Insightful Generalization")
      ", 页面最后更新于2022年1月9日.")
   (H2 "翻译术语对照")
   (set-attr*
    (SimpleTable
     ("英文" "中文")
     ("generalization" "泛化")
     ("level" "层级")
     ("region" "区域")
     ("dominator" "支配节点")
     ("type schema" "类型模式")
     ("occurs check" "出现检查")
     ("value restriction" "值限制")
     ("nonexpansive" "非扩张的")
     ("escape" "逃逸")
     ("ownership tracking" "所有权跟踪")
     ("generational garbage collection" "分代垃圾回收")
     ("minor collection" "次要回收")
     ("sharing" "共享")
     ("path compression" "路径压缩")
     ("rank" "秩")
     ("binding time" "绑定时间")
     ("generic type" "泛型类型")
     ("covariant" "协变的"))
    'align "center")
   (H2 "序言")
   (P "Hindley-Milner类型推断不只有算法W. 1988年, Didier Rémy想要加速Caml中的类型推断, "
      "并发现了一种优雅的类型泛化方法. 它不仅快速, 避免了扫描类型环境. "
      "它还能平滑地扩展到捕获即将逃逸的局部声明类型, 扩展到全称类型和存在类型的类型检查, 甚至扩展到MLF.")
   (P "可惜的是, 这个算法及其在OCaml类型检查器中的实现都鲜为人知, 也几乎没有文档. "
      "本页旨在解释并推广Rémy的算法, 并解读OCaml类型检查器的一部分. "
      "本页也旨在保存Rémy算法的历史.")
   (P "这个算法的吸引力在于它将类型泛化洞察为依赖跟踪" --
      "与在诸如区域和分代垃圾回收这样的自动内存管理中所使用的跟踪是同一类. "
      "泛化可以被视为在带有共享类型之边的, 标注了类型的抽象语法树中寻找支配节点. "
      "Fluet和Morrisett的区域类型系统使用类型变量的泛化作为区域包含的判据. "
      "不可思议的是, Rémy的算法将区域包含视为对一个类型变量是否可泛化的测试.")
   (H2. "引言" #:id "introduction")
   (P "本页起初是为了理解OCaml类型检查代码而做的笔记, 这些代码庞大, 复杂, 并且几乎没有文档. "
      "挖掘这些代码发掘出了真正的宝石. 其中之一" -- "一种高效而优雅的类型泛化方法" --
      "是这里的焦点.")
   (P "OCaml的泛化基于跟踪类型的所谓" (Em "层级") ". 同样的层级也确保了在一个模块内定义的类型"
      "不会逃逸到更宽的作用域中. 因此, 层级为局部引入的类型构造子施行区域纪律. "
      "泛化和区域被如此统一地处理, 这是很耐人寻味的. 在OCaml类型检查器中, 层级还有更多应用, "
      "用于带有多态字段的记录和存在类型. MetaOCaml间接地依赖层级来跟踪未来阶段绑定的作用域. "
      "所有这些应用中都有一个共同的主旋律: 跟踪依赖, 计算区域包含或数据依赖图中的支配节点. "
      "人们会立刻联想到Tofte和Talpin的基于区域的内存管理. 正如Fluet和Morrisett所展示的, "
      "Tofte和Talpin的区域类型系统可以被编码到System F中, 依靠全称量化来静态地阻止已分配的数据"
      "逃逸其区域. 对偶地, 基于层级的泛化依靠检测类型变量的逃逸来确定其区域, "
      "从而确定对其进行全称量化的位置.")
   (P "OCaml的泛化是Didier Rémy早在1988年发现的算法的一个(部分)实现. "
      "其想法是在标注了类型的抽象语法树中显式地表示类型的共享. "
      "一个类型变量只能在支配该变量所有出现的节点处被量化. 泛化相当于图的支配节点的增量计算. "
      "Rémy的MLF是这个想法的自然产物.")
   (P "不幸的是, Rémy的泛化算法及其背后的想法鲜为人知. 其实现, 例如OCaml中的那个, "
      "除了OCaml源代码中几条简短而令人费解的注释之外, 似乎根本没有文档. 它们理应广为人知. "
      "为了这个目标, 本页着手 (i) 引出并解释该算法, 揭示其直觉并勾勒其实现; "
      "(ii) 帮助解读OCaml类型检查器.")
   (P "本页的第二部分旨在成为OCaml类型检查器一部分的评注, 因此相当技术性. "
      "它参照的是OCaml 4.00.1的类型检查代码, 位于OCaml发行版的" :typing/ "目录中. 文件"
      :typecore.ml "是核心类型检查器: 它用类型和类型环境标注抽象语法树的节点. 准确地说, 它将"
      :Parsetree " (定义于" :parsing/parsetree.mli ") 变换为" :Typedtree ". 文件"
      :ctype.ml "实现了合一和层级操作函数.")
   (P "我感谢Didier Rémy的评论, 解释, 洞见以及对该算法发现过程的回忆. "
      "我感谢Jacques Garrigue有益的评论以及对层级在OCaml类型检查器中更多应用的解释. "
      "感谢Matthew Fluet和Baris Aktemur提供的额外参考文献.")
   (P (B "版本"))
   (P "当前版本为2013年2月.")
   (P (B "参考文献")
      (Ul (Li "Didier Rémy: Extension of ML Type System with a Sorted Equational Theory on Types. "
              "Research Report 1766, Institut National de Recherche en Informatique et Automatique, "
              "Rocquencourt, BP 105, 78 153 Le Chesnay Cedex, France, 1992. "
              (Href "http://gallium.inria.fr/~remy/ftp/eq-theory-on-types.pdf"
                    "http://gallium.inria.fr/~remy/ftp/eq-theory-on-types.pdf"))
          (Li "Matthew Fluet and J. Gregory Morrisett: Monadic Regions. "
              "J. Functional Programming, 2006, v16, N4-5, pp. 485-545. "
              "这篇论文表明, 对于一个可靠的内存区域类型系统而言, 参数多态就是所需要的全部.")))
   (H2. "泛化" #:id "generalization")
   (P "这个背景小节回顾Hindley-Milner类型系统中的类型泛化, 强调朴素实现中的微妙之处和低效之处. "
      "这些低效之处促使Rémy发现了基于层级的泛化算法.")
   (P "回忆一下, 类型" $t "相对于类型环境" $G "的" (Em "泛化") (&GEN $G $t)
      "是对" $t "中不在" $G "中自由出现的自由类型变量进行量化. 用希腊字母写出来就是: "
      (&= (&GEN $G $t) (∀ $alpha_1 $..h $alpha_n $t)) ", 其中"
      (&= (setE $alpha_1 $..h $alpha_n) (&- (FV $t) (FV $G)))
      ". 用Hindley-Milner的术语来说, 这种量化将一个类型转换为所谓的类型模式. 泛化被用于对"
      :let "表达式进行类型检查:"
      (MB (&rule (&vdash $G (&: $e $t))
                 (&vdash (&cm $G (pare (&: $x (&GEN $G $t)))) (&: $e_2 $t_2))
                 (&vdash $G (&: (Let $x $e $e_2) $t_2))))
      "也就是说, 为let绑定的变量推断出的类型在对let表达式的体进行类型检查时被泛化. "
      "ML为泛化添加了一个条件, 即所谓的值限制: let绑定的表达式" $e
      "从外观上看必须没有可见的副作用" -- "技术上说, " $e "必须通过成为" (Em "非扩张的")
      "这一句法测试. OCaml放松了值限制, 见本页稍后.")
   (P "这是泛化的一个平凡例子:"
      (CodeB "fun x -> let y = fun z -> z in y
(* 'a -> ('b -> 'b) *)")
      "类型检查器为" (Code "fun z -> z") "推断出类型" (&-> $beta $beta) ", 其中" $beta
      "是新鲜的, 因而唯一的类型变量. 表达式" (Code "fun z -> z")
      "在句法上是一个值, 泛化得以进行, 于是" :y "得到类型" (∀ $beta (&-> $beta $beta))
      ". 由于这个多态类型, " :y "可以出现在不同类型的上下文中" --
      "可以被应用于不同类型的参数," -- "如"
      (CodeB "fun x ->
  let y = fun z -> z in
  (y 1, y true)
(* 'a -> int * bool *)"))
   (P "泛化" (&GEN $G $t) "只对" $t "中那些不出现在" $G
      "中的自由类型变量进行量化. 这个条件很微妙但至关重要: 没有它, 就会为函数"
      (CodeB "fun x -> let y = x in y")
      "推断出不可靠的类型" (&-> $alpha $beta) ". 具体来说: 为了推断该函数的类型, 我们在"
      (&: $x $alpha) "的环境中推断其体" (Code "let y = x in y") "的类型, 其中" $alpha
      "是一个新鲜的类型变量. 根据上面的" :let "规则, 为" :y "推断出的类型, 从而结果类型, 是"
      (&GEN (&: $x $alpha) $alpha) ". 显然" $alpha "确实出现在环境" (&: $x $alpha)
      "中. 如果我们仍然对它进行量化, " :y "就会得到多态类型" (∀ $alpha $alpha)
      ", 它可以被实例化为任何类型. 结果是一个表面上能把其参数转换为任意类型的值的函数.")
   (P "因此, 对于每个要量化的类型变量, 我们必须确保它不出现在类型环境中. "
      "朴素地, 我们可以扫描类型环境, 逐一查看每个绑定的类型" --
      "事实上, 最初的Caml正是这么做的. 然而类型环境可能变得非常大. 通常ML函数包含很长的"
      :let "表达式序列. 一个非递归的" :let "在其类型环境中有之前所有" :let
      "的绑定; 一个递归" :let "的环境有其所有" :let "兄弟的绑定. 作为单个" :let
      "的泛化的一部分而扫描环境, 所花的时间与函数的大小成线性关系; 那么整个程序的类型检查将是二次的. "
      "(除了病态情形之外, Hindley-Milner类型推断的规模与程序大小几乎成线性关系.) "
      "Didier Rémy回忆道, 低效的泛化是Caml编译缓慢的主要原因之一. "
      "自举编译器并对两个用于编译模式和表达式的相互递归函数进行类型检查需要20分钟.")
   (P "必定有一种办法可以避免扫描环境. 下一节给出了这个想法.")
   (H2. "作为内存管理不当的不可靠泛化" #:id "gen-mismanagement")
   (P "本节开始介绍Rémy算法背后的想法, 将它们与基于区域的内存管理联系起来. "
      "具体起见, 我们将使用一个玩具Hindley-Milner类型推断器. 在本节中, 推断器具有"
      (Em "不可靠的") "泛化函数, 它对一个类型中的自由类型变量进行量化而完全不顾环境. "
      "我们详细地对三个简单的例子进行类型检查, 并将不可靠类型的推断与手动内存管理的常见问题联系起来: "
      "释放仍在使用中的内存. 不可靠的泛化将在下一节中被修复, 其灵感来自防止资源被过早释放的标准方法.")
   (P "尽管我们的Hindley-Milner类型推断器是玩具, 但它与真实的OCaml类型检查器共享许多实现决策 "
      "(甚至一些函数名). 理解它将有助于我们在本页稍后转向OCaml内部时的理解.")
   (P "我们的玩具语言是带有" :let "的标准纯λ演算. 其表达式为:"
      (CodeB "type exp =
  | Var of varname                      (* variable                *)
  | App of exp * exp                    (* application: e1 e2      *)
  | Lam of varname * exp                (* abstraction: fun x -> e *)
  | Let of varname * exp * exp          (* let x = e in e2         *)")
      "类型由(自由的或绑定的)类型变量, 被量化的类型变量和函数类型组成:"
      (CodeB "type qname = string
type typ =
  | TVar of tv ref               (* type (schematic) variable *)
  | QVar of qname                (* quantified type variable  *)
  | TArrow of typ * typ
and tv = Unbound of string | Link of typ")
      "带有" :QVar "的类型是类型模式; 不带的则是简单类型. 类型模式, 即被量化的类型, "
      "在Hindley-Milner系统中是前束形式的 (也就是说, 全称量词都在最外面), 因此量词不必被显式地表示.")
   (P "按照Prolog的传统, 类型变量被表示为引用单元. 一个未绑定的变量包含空指针或指向自身的指针"
      -- "或者, 在我们的情形中, 为了便于打印, 包含变量的名字. 当一个自由类型变量与某个类型"
      :t^ "合一时, 引用单元被覆写为指向" :t^ "的指针. 为了防止循环的 (而且对我们来说是不可靠的) 类型, "
      "首先要进行" (Q "出现检查") ": " (Code "occurs tv t'") "遍历" :t^
      ", 如果遇到类型变量" :tv "就抛出一个异常:"
      (CodeB "let rec unify : typ -> typ -> unit = fun t1 t2 ->
  if t1 == t2 then ()                   (* t1 and t2 are physically the same *)
  else match (t1,t2) with
  | (TVar {contents = Link t1},t2)
  | (t1,TVar {contents = Link t2}) ->
      unify t1 t2
  | (TVar ({contents = Unbound _} as tv),t')
  | (t',TVar ({contents = Unbound _} as tv)) ->
      occurs tv t'; tv := Link t'
  | (TArrow (tyl1,tyl2), TArrow (tyr1,tyr2)) ->
      unify tyl1 tyr1;
      unify tyl2 tyr2
  (* everything else is error *)"))
   (P "类型检查器是完全标准的. 它在类型环境" :env "中推断表达式" :exp "的类型:"
      (CodeB "type env = (varname * typ) list
let rec typeof : env -> exp -> typ = fun env -> function
  | Var x     -> inst (List.assoc x env)
  | Lam (x,e) ->
      let ty_x = newvar () in
      let ty_e = typeof ((x,ty_x)::env) e in
      TArrow(ty_x,ty_e)
  | App (e1,e2) ->
      let ty_fun = typeof env e1 in
      let ty_arg = typeof env e2 in
      let ty_res = newvar () in
      unify ty_fun (TArrow (ty_arg,ty_res));
      ty_res
  | Let (x,e,e2) ->
      let ty_e = typeof env e in
      typeof ((x,gen ty_e)::env) e2")
      "函数" :newvar "分配一个新的, 带有唯一名字的" :TVar ". 函数" :inst
      "实例化一个类型模式, 也就是说, 将每个" :QVar "替换为一个新鲜的" :TVar
      ". 它也是标准的. 泛化函数是不可靠的: 它对类型中的所有自由变量进行量化, 而不管环境如何:"
      (CodeB "let rec gen : typ -> typ = function      (* unsound! *)
  | TVar {contents = Unbound name} -> QVar name
  | TVar {contents = Link ty}      -> gen ty
  | TArrow (ty1,ty2)               -> TArrow (gen ty1, gen ty2)
  | ty -> ty")
      "量化将一个" :TVar "替换为相应的" :QVar ". 因此原来的" :TVar
      "被隐式地释放了: 当一个自由变量被绑定时, 它就" (Q "消失") "了, 被替换为指向绑定者的"
      (Q "指针") ".")
   (P "就类型变量而言, " :typeof "分配自由变量, 合一它们, 并在量化之后释放它们. "
      "让我们对一些简单的例子进行类型检查, 观察影响自由类型变量的这三种主要操作的顺序. "
      "第一个例子是不应该出任何问题的那一个:"
      (CodeB "fun x -> let y = fun z -> z in y")
      "类型检查的轨迹如下, 只展示与类型变量有关的操作:"
      (CodeB "1    ty_x = newvar ()          (* fun x -> ...               *)
2      ty_e =                  (* let y = fun z -> z in y    *)
3         ty_z = newvar ();    (* fun z -> ...               *)
3         TArrow(ty_z,ty_z)    (* inferred for: fun z -> z   *)
2      ty_y = gen ty_e         (* ty_z remains free, and so  *)
2      deallocate ty_z         (* quantified and disposed of *)
1    TArrow(ty_x, inst ty_y)   (* inferred for: fun x -> ... *)")
      "第一列中的数字是" :typeof "递归调用的深度. 由于" :typeof
      "在抽象语法树 (AST) 的每个非叶节点上递归, 这个递归调用深度就是正在被类型检查的节点在AST中的深度. "
      "推断出的类型是" (Code "'a -> 'b -> 'b") ", 与预期一致. 没有出任何问题.")
   (P "第二个例子, 之前也见过, 是不可靠的泛化为其给出不可靠类型" (Code "'a->'b") "的那一个:"
      (CodeB "fun x -> let y = x in y")
      "像之前那样图示" :TVar "操作揭示了问题:"
      (CodeB "1    ty_x = newvar ()          (* fun x -> ...                 *)
2      ty_e =                  (* let y = x in y               *)
3         inst ty_x            (* inferred for x, same as ty_x *)
2      ty_y = gen ty_e         (* ty_x remains free, and is    *)
2      deallocate ty_x         (* quantified, and disposed of  *)
1    TArrow(ty_x, inst ty_y)   (* inferred for: fun x -> ...   *)")
      "类型变量" :ty_x "是返回类型的一部分, 在深度1处被使用" --
      "然而它在深度2处就被量化并处置了. 我们处置了仍在使用中的值.")
   (P "第三个例子也有问题. 不可靠的泛化再次给出不可靠的类型" (Code "('a->'b) -> ('c ->'d)") ":"
      (CodeB "fun x -> let y = fun z -> x z in y")
      "图示再次显示出一个内存管理问题:"
      (CodeB "1    ty_x = newvar ()          (* fun x -> ...               *)
2      ty_e =                  (* let y = ...                *)
3         ty_z = newvar ()     (* fun z -> ...               *)
4           ty_res = newvar () (* typechecking: x z          *)
4           ty_x :=            (* as the result of unify     *)
4              TArrow (ty_z,ty_res)
4           ty_res             (* inferred for: x z          *)
3         TArrow(ty_z,ty_res)  (* inferred for: fun z -> x z *)
2      ty_y = gen ty_e         (* ty_z, ty_res remain free   *)
2      deallocate ty_z         (* quantified and disposed of *)
2      deallocate ty_res       (* quantified and disposed of *)
1    TArrow(ty_x, inst ty_y)   (* inferred for: fun x -> ... *)")
      "类型变量" :ty_z "和" :ty_res "在深度2处被量化, 因而被处置, 然而它们是被赋给"
      :ty_x "的" (Code "TArrow (ty_z,ty_res)") "的一部分, 而" :ty_x "又是结果的一部分.")
   (P "所有不可靠的例子都有一个" (Q "内存管理问题") ": 释放了仍在被使用的内存 (" :TVar
      "). 这并非偶然. 当一个类型变量被量化之后, 稍后它可以被实例化为任何类型. "
      "然而, 一个出现在类型环境中的类型变量不能被替换为任何类型而不影响类型检查的其余部分. "
      "类似地, 当我们释放一块内存时, 我们就允许运行时重新分配它并用任意数据覆写它. "
      "我们程序的其余部分不应该依赖于被释放的内存之后发生了什么" --
      "前提是它真的是空闲的, 程序之后不再需要. 事实上, 人们可以将" (Q "不在使用中的内存")
      "定义为对该内存的任意改变都不影响程序的其余部分. 释放仍在使用中的内存将会影响程序的其余部分"
      -- "常常是使其崩溃. 顺便一提, 为我们的例子推断出的不可靠类型常常导致相同的结果.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/ML/generalization/unsound.ml" "unsound.ml")
              " [11K] " -- "带有不可靠泛化的玩具类型推断器的完整代码, 带有更多不可靠推断的例子.")))
   (H2. "基于层级的高效泛化" #:id "levels")
   (P "本节继续阐述Rémy算法背后的想法. 既然我们已经看到了不可靠的泛化如何与释放仍在使用中的内存相关联, "
      "我们就应用针对过早释放的标准疗法" -- "所有权跟踪, 或者说区域" --
      "并以不太大的开销治愈不可靠的泛化. 我们开发两个算法. 较简单的那个, " :sound_eager
      ", 在本节中引出并解释. 最优的" :sound_lazy ", 它捕捉了Rémy算法的主要特征, 将在下一节中介绍.")
   (P "显然, 在释放内存之前, 我们必须检查它是否仍在使用中. 朴素地, "
      "我们可以扫描所有已知在使用中的内存, 寻找对待释放候选者的引用" --
      "换言之, 进行完整的垃圾回收标记遍, 看看我们的候选者是否被标记了. "
      "这样说来, 这个检查似乎非常昂贵. 至少我们应该等到垃圾积累起来, 再一并回收. "
      "可惜的是, 在Hindley-Milner类型系统中我们不能任意地推迟量化, 因为泛化后的类型可能马上就要被使用.")
   (P "更有希望的是所有权跟踪: 将一个已分配的资源与一个所有者, 即一个对象或一次函数激活, 关联起来. "
      "只有所有者可以释放其资源. 一个类似的策略是区域, 区域是由一个词法作用域的, 所谓的"
      :letregion "原语创建的堆内存区. 当" :letregion "离开作用域时, 其整个区域被一并释放. "
      "这个想法与泛化非常契合. 在Hindley-Milner系统中, 泛化总是" :let "的一部分. 一个"
      :let "表达式" (Code "let x = e in e2") "是在推断" :e "的类型时所分配的所有类型变量的天然所有者. 当"
      :e "的类型被找到时, 所有仍由该" :let "表达式所拥有的自由类型变量都可以被处置, 也就是说, 被量化.")
   (P "这些直觉构成了可靠而高效的泛化算法的基础. 第一个是" :sound_eager
      ", 在本节余下的部分中描述. 它的代码与上一节的玩具Hindley-Milner推断器只在一些微小但重要的细节上有所不同. "
      "我们将只解释这些不同之处; 完整的代码可在下文获得. 主要的不同之处在于, 自由类型变量尽管是未绑定的, "
      "现在却是被拥有的, 并指向其所有者. 所有者, 总是一个" :let "表达式, 由一个被称为"
      (Em "层级") "的正整数来标识. 它是所有者" :let "表达式的De Bruijn层级, 或者说嵌套深度. 层级1对应于(隐式的)顶层"
      :let ". (顺便一提, 尽管" (Code "(let x = e1 in eb1, let y = e2 in eb2)") "中的两个"
      :let "都具有层级2, 但不会产生混淆, 因为两个" :let "都不在彼此的作用域中, 因此它们的区域是不相交的.) "
      :let "嵌套深度等于" :let "表达式的类型检查递归深度, 后者在一个引用单元的帮助下很容易确定."
      (CodeB "type level = int
let current_level = ref 1
let enter_level () = incr current_level
let leave_level () = decr current_level")
      "类型推断器维护" :let "类型检查深度:"
      (CodeB "let rec typeof : env -> exp -> typ = fun env -> function
  ... (* the other cases are the same as before *)
  | Let (x,e,e2) ->
      enter_level ();
      let ty_e = typeof env e in
      leave_level ();
      typeof ((x,gen ty_e)::env) e2")
      "对主类型推断函数的唯一改动是添加了" :enter_level "和" :leave_level "来跟踪层级. "
      :typeof "的其余部分与原来的玩具版本完全相同.")
   (P "自由类型变量现在带有标识其所有者的层级. 一个新分配的类型变量获得" :current_level
      ", 意思是它的所有者是正在被类型检查的最近的" :let
      ". (在基于区域的内存管理中, 所有新内存都分配在最内层的存活区域中.)"
      (CodeB "type typ =
  | TVar of tv ref               (* type (schematic) variable *)
  | QVar of qname                (* quantified type variable *)
  | TArrow of typ * typ
and tv = Unbound of string * level | Link of typ

let newvar : unit -> typ =
 fun () -> TVar (ref (Unbound (gensym (),!current_level)))"))
   (P "正如赋值可能改变一块已分配内存的所有者, 合一也可能改变一个自由类型变量的层级. 例如, 如果"
      :ty_x " (层级1) 和" :ty_y " (层级2) 都是自由的, 并且" :ty_x "与类型"
      (Code "TArrow(ty_y,ty_y)") "合一, 那么这个箭头类型及其分量就被导出到区域1中, 因此"
      :ty_y "的层级被改为1. 人们可以将上述合一视为将" :ty_x "的所有出现替换为"
      (Code "TArrow(ty_y,ty_y)") ". 由于" :ty_x
      "具有较小的层级, 因而可能出现在内层的, 层级2的" :let "之外, 在那个内层" :let
      "的被绑定表达式被类型检查之后, " :ty_y "不应该被释放. 有了更新后的" :ty_y
      "层级, 它就不会被释放. {译注: 原文此处写作" (Code "t_x") ", 应为" :ty_x "的笔误.} "
      "总而言之, 将一个自由类型变量" :ty_x "与" :t "合一, 必须将" :t "中每个自由类型变量"
      :ty_y "的层级更新为" :ty_y "和" :ty_x "的层级中较小的那个. 将一个自由类型变量与"
      :t "合一还必须进行出现检查, 它也遍历该类型. 这两次遍历可以合并. 新的" :occurs
      "进行出现检查并更新层级:"
      (CodeB "let rec occurs : tv ref -> typ -> unit = fun tvr -> function
  | TVar tvr' when tvr == tvr' -> failwith \"occurs check\"
  | TVar ({contents = Unbound (name,l')} as tv) ->
      let min_level =
        (match !tvr with Unbound (_,l) -> min l l' | _ -> l') in
      tv := Unbound (name,min_level)
  | TVar {contents = Link ty} -> occurs tvr ty
  | TArrow (t1,t2)            -> occurs tvr t1; occurs tvr t2
  | _ -> ()"))
   (P "与原来的" :occurs "代码唯一的不同是模式匹配中的第二个子句. 合一代码完全不必修改. "
      "最后, 我们修复泛化函数, 使其可靠:"
      (CodeB "let rec gen : typ -> typ = function
  | TVar {contents = Unbound (name,l)}
      when l > !current_level -> QVar name
  | TVar {contents = Link ty} -> gen ty
  | TArrow (ty1,ty2) -> TArrow (gen ty1, gen ty2)
  | ty -> ty"))
   (P "改动是极小的: 条件" (Code "when l > !current_level") ". 回忆一下新的" :typeof "代码:"
      (CodeB "let rec typeof : env -> exp -> typ = fun env -> function
  ... (* the other cases are the same as before *)
  | Let (x,e,e2) ->
      enter_level ();
      let ty_e = typeof env e in
      leave_level ();
      typeof ((x,gen ty_e)::env) e2")
      "它在为类型检查" :e "所建立的区域退出之后调用" :gen
      ". 一个仍由该区域拥有的自由类型变量将具有大于当前层级的层级. 由于该区域现在已经死亡, "
      "任何这样的类型变量都可以被释放, 也就是说, 被量化.")
   (P "这些就是" :sound_eager "相对于不可靠的玩具算法的全部改动, 它们修复了类型推断. "
      "这是那个老的有问题的例子"
      (CodeB "fun x -> let y = x in y")
      "图示" :TVar "操作现在显示没有问题了:"
      (CodeB "1  1   ty_x/1 = newvar ()          (* fun x -> ...                 *)
2  2     ty_e =                    (* let y = x in y               *)
3  2        inst ty_x/1            (* inferred for x, same as ty_x *)
2  1     ty_y = gen ty_e           (* ty_x/1 remains free, but is  *)
                                   (* level = current, can't       *)
                                   (* quantify, can't dispose      *)
1  1   TArrow(ty_x/1, inst ty_y)   (* inferred for: fun x -> ...   *)")
      "第一列数字显示" :typeof "的递归深度, 或者说正在被类型检查的AST节点的深度. 第二列中的数字是"
      :current_level ", 即" :let "嵌套深度. 我们将自由类型变量的层级写在斜杠之后, 如" :ty_x/1
      ". 该变量不再在深度2 (层级1) 处被" :gen "量化, 因为" :ty_x/1
      "属于当前的, 仍然活跃的区域1. 因此, 推断出的类型是" (Code "'a->'a") ", 与预期一致.")
   (P "在一个稍微复杂一点的例子中,"
      (CodeB "fun x -> let y = fun z -> x in y")
      "用于" :x "之类型的类型变量" :ty_x "在层级1处被分配, 而" :ty_z "在层级2处被分配. 在内层的"
      :let ", 即区域2, 结束之后, " :ty_z/2 "将被量化并处置, 但" :ty_x/1
      "不会. 因此推断出的类型是" (Code "'a->'b->'a") ". 鼓励读者图示其他例子, 以检查推断出的类型是可靠的.")
   (P "层级跟踪可能看起来像引用计数. 然而, 我们并不计算一个自由类型变量的使用者的数目, "
      "而是只跟踪一个使用者, 即具有最宽作用域的那个. 层级跟踪确实看起来很像分代垃圾回收: "
      "内存在年轻代中分配, 并在次要 (最年轻) 回收时被一并处置, 除非它被" (Q "重新设定父节点")
      "或者被栈引用. 老年代不必被扫描以寻找对新生代的引用, 因为预期不会有这样的引用" --
      "除非存在将一个年轻值 (的指针) 赋给一个老数据结构的字段的赋值. 分代垃圾回收器 (例如OCaml的GC) "
      "跟踪年轻到老的赋值. 在次要回收时, 被老数据引用的年轻数据被晋升到老年代. "
      "类型泛化确实看起来与次要GC回收非常相似.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/ML/generalization/sound_eager.ml" "sound_eager.ml")
              " [13K] " -- "带有" :sound_eager
              "泛化的玩具类型推断器的完整代码, 带有更多现在可靠的推断的例子.")))
   (H2. "更加高效的基于层级的泛化" #:id "levels-lazy")
   (P "本节继续阐述Rémy算法背后的想法, 并介绍" :sound_lazy ": 上一节中的" :sound_eager
      "的一个优化版本. " :sound_lazy
      "算法避免了在合一, 泛化和实例化期间对一个类型的重复的, 不必要的遍历, "
      "并避免复制那些不包含需要泛化或实例化的变量的部分, 从而改善了共享. "
      "该算法推迟出现检查和层级更新, 使得与一个自由类型变量的合一花费常数时间. "
      "层级被增量地, 按需地更新. 总而言之, " :sound_lazy
      "体现了Rémy算法的主要想法. 其中一些想法在OCaml类型检查器中得到了实现.")
   (P "为了进行这些优化, 我们改变类型的句法. 回忆一下, 在" :sound_eager
      "中, 类型由自由的或绑定的类型变量" :TVar ", (隐式全称地)被量化的类型变量" :QVar
      "和函数类型" :TArrow "组成. 第一个看似无原则的改变是取消" :QVar
      "这一单独的选项, 而将一个非常大的正整数" -- "它应被当作不可达序数" $omega
      "来对待" -- "专门用作" :generic_level ". 处于" :generic_level "的自由类型变量" :TVar
      "被视为被量化的类型变量. 更为实质性的是, 现在所有类型, 不仅仅是自由类型变量, 都有层级. "
      "一个复合类型 (在我们的情形中是" :TArrow ") 的层级是其分量层级的一个上界, 不一定是确切的. "
      "换言之, 如果一个类型属于一个存活的区域, 那么它所有的分量都应该是存活的. 由此立即得到, "
      "如果一个(复合)类型处于" :generic_level ", 那么它可能包含被量化的类型变量. 逆否地, 如果一个类型不处于"
      :generic_level ", 那么它不包含任何被量化的变量. 因此, 实例化这样一个类型应该原样返回该类型, 而不遍历它. "
      "类似地, 如果一个类型的层级大于当前层级, 那么它可能包含需要泛化的自由类型变量. 另一方面, "
      "泛化函数根本不应该费心去遍历一个层级等于或小于当前层级的类型. "
      "这是层级如何帮助消除对类型的过度遍历和重建, 从而改善共享的第一个例子.")
   (P "将一个类型与一个自由类型变量合一, 应该在后者层级较小时将该类型的层级更新为该类型变量的层级. "
      "对于一个复合类型, 这样的更新意味着递归地更新该类型所有分量的层级. "
      "为了推迟代价高昂的遍历, 我们给复合类型两个层级: " :level_old "是类型分量层级的一个上界; "
      :level_new ", 它小于或等于" :level_old ", 是该类型在更新之后应具有的层级. 如果"
      (Code "level_new &lt; level_old") ", 那么该类型有待处理的层级更新. 因此" :sound_lazy
      "中类型的句法是"
      (CodeB "type level = int
let generic_level = 100000000           (* as in OCaml typing/btype.ml *)
let marked_level  = -1                  (* for marking a node, to check*)
                                        (* for cycles                  *)
type typ =
  | TVar of tv ref
  | TArrow of typ * typ * levels
and tv = Unbound of string * level | Link of typ
and levels = {mutable level_old : level; mutable level_new : level}"))
   (P "我们还没有解释" :marked_level ". 每次与自由类型变量合一时进行的出现检查是昂贵的, "
      "它提高了合一和类型检查的算法复杂度. 我们现在推迟这个检查, 直到整个表达式都被类型检查完毕. "
      "与此同时, 合一可能在类型中创造出环. 类型遍历必须检查环, 否则就有发散的风险. " :marked_level
      "被临时地赋给一个复合类型的" :level_new ", 以表明该类型正在被遍历. 在遍历期间遇到"
      :marked_level "意味着检测到了一个环, 这会引发出现检查错误. 顺便一提, 在OCaml中类型一般是有环的: "
      "在对对象和多态变体进行类型检查时, 以及当设置了" :-rectypes
      "编译器选项时, 会出现(等价)递归类型. OCaml类型检查器使用一个类似的标记层级技巧来检测环并避免发散.")
   (P :sound_lazy "的合一与" :sound_eager "相比有几个重要的不同之处:"
      (CodeB "let rec unify : typ -> typ -> unit = fun t1 t2 ->
  if t1 == t2 then ()                   (* t1 and t2 are physically the same *)
  else match (repr t1,repr t2) with
  | (TVar ({contents = Unbound (_,l1)} as tv1) as t1,      (* unify two free vars *)
    (TVar ({contents = Unbound (_,l2)} as tv2) as t2)) ->
     if tv1 == tv2 then ()             (* the same variable *)
     else
       if l1 > l2 then tv1 := Link t2 else tv2 := Link t1  (* bind the higher-level var *)
  | (TVar ({contents = Unbound (_,l)} as tv),t')
  | (t',TVar ({contents = Unbound (_,l)} as tv)) ->
      update_level l t';
      tv := Link t'
  | (TArrow (tyl1,tyl2,ll), TArrow (tyr1,tyr2,lr)) ->
      if ll.level_new = marked_level || lr.level_new = marked_level then
        failwith \"cycle: occurs check\";
      let min_level = min ll.level_new lr.level_new in
      ll.level_new &lt;- marked_level; lr.level_new &lt;- marked_level;
      unify_lev min_level tyl1 tyr1;
      unify_lev min_level tyl2 tyr2;
      ll.level_new &lt;- min_level; lr.level_new &lt;- min_level
  (* everything else is the unification error *)

and unify_lev l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level l ty1;
  unify ty1 ty2")
      "其中辅助函数" :repr ", 就像OCaml的" :Btype.repr
      "一样, 追踪绑定变量的链接, 返回一个自由变量或一个构造出的类型. 与OCaml不同, 我们进行路径压缩. "
      "合一函数不再进行出现检查; 因此, 它必须努力检测意外创造出的环. "
      "与一个自由变量合一现在花费常数时间, 在一次浅层的" :update_level "之后绑定该变量.")
   (P "函数" :update_level "是优化后算法的关键部分之一. 通常, 它仅仅是承诺将一个类型的层级更新为给定的层级. "
      "它以常数时间工作, 并维持类型层级只能减小的不变式. 类型变量的层级被立即更新. 对于复合类型, "
      "如果所期望的新层级较小, 那么" :level_new "被设为该新层级. 此外, 如果先前" :level_new "和"
      :level_old "相同, 那么该类型被放入" :to_be_level_adjusted
      "队列中, 以便稍后更新其分量的层级. 这个工作队列类似于分代垃圾回收器 (例如OCaml中的那个) "
      "所维护的从年轻代到老年代的赋值的列表."
      (CodeB "let to_be_level_adjusted = ref []

let update_level : level -> typ -> unit = fun l -> function
  | TVar ({contents = Unbound (n,l')} as tvr) ->
      assert (not (l' = generic_level));
      if l &lt; l' then
        tvr := Unbound (n,l)
  | TArrow (_,_,ls) as ty ->
      assert (not (ls.level_new = generic_level));
      if ls.level_new = marked_level then failwith \"occurs check\";
      if l &lt; ls.level_new then begin
        if ls.level_new = ls.level_old then
          to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.level_new &lt;- l
      end
  | _ -> assert false"))
   (P "待处理的层级更新必须在泛化之前执行: 毕竟, 一个待处理的更新可能减小一个类型变量的层级, "
      "将其晋升到一个更宽的区域, 从而使其免于被量化. 然而, 并非所有待处理的更新都必须被强制执行"
      -- "只有那些" (Code "level_old > current_level")
      "的类型的更新才需要. 否则, 一个类型不包含在当前位置可泛化的变量, 层级更新可以被进一步推迟. "
      "所描述的强制执行算法由" :force_delayed_adjustments
      "实现, 见源代码. 顺便一提, 如果一个复合类型 (" :TArrow
      ") 的层级更新真的必须执行, 那么该类型就必须被遍历. 两个" :TArrow
      "类型的合一也必须遍历它们. 因此, 原则上, 合一也可以顺便更新层级. 然而, 这个优化目前尚未实现.")
   (P "泛化函数搜索属于一个死亡区域的 (即其层级大于当前层级的) 自由" :TVar ", 并将它们的层级设为"
      :generic_level ", 从而量化这些变量. 该函数只遍历类型中可能包含需要泛化的类型变量的那些部分. "
      "如果一个类型的(新)层级是" :current_level
      "或更小, 那么它所有的分量都属于存活的区域, 因此该类型没有什么需要泛化的. 在泛化之后, "
      "一个复合类型如果包含一个被量化的类型变量, 就获得" :generic_level
      ". 因此, 稍后实例化函数将只查看那些层级为" :generic_level "的类型."
      (CodeB "let gen : typ -> unit = fun ty ->
  force_delayed_adjustments ();
  let rec loop ty =
    match repr ty with
    | TVar ({contents = Unbound (name,l)} as tvr)
           when l > !current_level ->
      tvr := Unbound (name,generic_level)
    | TArrow (ty1,ty2,ls) when ls.level_new > !current_level ->
      let ty1 = repr ty1 and ty2 = repr ty2 in
      loop ty1; loop ty2;
      let l = max (get_level ty1) (get_level ty2) in
      ls.level_old &lt;- l; ls.level_new &lt;- l   (* set the exact level upper bound *)
    | _ -> ()
  in loop ty"))
   (P "类型检查器" :typeof "保持不变, 在类型检查一个" :let
      "表达式时进入一个新的区域. 细节请见源代码.")
   (P "我们介绍了优化的" :sound_lazy
      "类型泛化算法, 它不仅避免了在每次泛化时扫描整个类型环境, 也避免了在每次与自由类型变量合一时进行出现检查. "
      "结果是, 合一花费常数时间. 该算法消除了不必要的类型遍历和复制, 节省了时间和内存. "
      "除了自由类型变量的类型层级之外, 还有两个想法构成了这些优化的基础. "
      "第一个是为复合类型赋予层级, 让我们无需查看一个类型就能对它可能包含什么有所了解. "
      "第二个原则是推迟昂贵的动作 (类型遍历), 希望它们将来会与别的事情一起被顺带完成. "
      "换言之, 如果对一个问题的处理被推迟得足够久, 它可能就会自行消失: 拖延有时会有帮助.")
   (P (B "参考文献")
      (Ul (Li (Href "https://okmij.org/ftp/ML/generalization/sound_lazy.ml" "sound_lazy.ml")
              " [20K] " -- "优化的玩具类型推断器的完整代码, 同样带有许多例子.")))
   (H2. "OCaml中基于层级的泛化" #:id "levels-OCaml")
   (P "这个关于OCaml内部的小节描述了OCaml类型检查器中类型层级的实现, 以及它们在高效泛化中的应用. "
      "下一节展示层级如何帮助防止局部类型的逃逸以及如何对存在类型进行类型检查.")
   (P "OCaml中类型泛化背后的想法已经在前面几节中以玩具算法" :sound_eager "和" :sound_lazy
      "的形式介绍过了. 它们的代码被有意写得与OCaml类型检查器相似, 常常使用相同的函数名. "
      "OCaml类型检查器实现了" :sound_eager "算法, 并带有来自" :sound_lazy
      "的一些优化. OCaml要复杂得多: 玩具代码中的合一只占几行, 而OCaml的合一代码, 在"
      :ctype.ml "中, 占了1634行. 尽管如此, 理解这些玩具算法应该有助于解读OCaml类型检查器.")
   (P "像" :sound_eager "算法一样, OCaml类型检查器在每次与自由变量合一时进行出现检查和层级更新; "
      "从" :Ctype.unify_var "的代码中可以清楚地看到这一点. 另一方面, 像在" :sound_lazy
      "中一样, OCaml类型检查器为所有类型赋予层级, 而不仅仅是类型变量" -- "见" :types.mli "中的"
      :type_expr ". 一个原因是为了检测逃逸的局部类型构造子 (在下一节中描述). 同样像在" :sound_lazy
      "中一样, " :generic_level "区分被量化的类型变量和可能包含被量化变量的类型 (所谓的"
      (Q "泛型类型") "). 因此, 模式实例化函数" :Ctype.instance "和" :Ctype.copy
      "不会遍历和复制类型的非泛型部分, 而是原样返回它们, 这改善了共享. 处于" :generic_level
      "的类型变量被打印为" (Code "'a") "这样; 处于其他层级的则打印为" (Code "'_a")
      ". 正如在我们的玩具算法中一样, 一个可变的全局变量" :Ctype.current_level
      "跟踪当前层级, 它被赋给新创建的类型或类型变量 (见" :Ctype.newty "和" :Ctype.newvar
      "). " :current_level "由" :enter_def "增加, 由" :end_def "减少. 除了" :current_level
      "之外, 还有" :nongen_level ", 在类型检查类定义时使用, 以及" :global_level
      ", 用于类型声明中的类型变量.")
   (P "对" (Code "let x = e in body") "进行类型检查的一段非常简化的代码如下."
      (CodeB "let e_typed =
  enter_def ();
  let r = type_check env e_source in
  end_def ();
  r
in
generalize e_typed.exp_type;
let new_env = bind env x e_typed.exp_type in
type_check new_env body_source")
      "这里, " :e_source "是表达式" :e "的抽象语法树, 或者说" :Parsetree.expression ", 而"
      :e_typed "是" :Typedtree.expression ", 即每个节点都标注了其推断出的类型 (字段" :exp_type
      ") 的抽象语法树.")
   (P "因此, 在OCaml类型检查器中经常看到的总体类型泛化模式是"
      (CodeB "let ty =
    enter_def ();
    let r = ... let tv = newvar() in ... (... tv ...)
    end_def ();
    r in
generalize ty"))
   (P "如果" :tv "没有与在" :enter_def
      "之前就已存在于环境中的某个东西合一, 那么该变量将被泛化. 这段代码看起来与我们的玩具代码非常相像.")
   (P "有趣的是, 层级还有另一个用途, 即为局部类型声明施行区域纪律.")
   (H2. "类型区域" #:id "regions")
   (P "OCaml类型检查器还依靠类型层级来检查类型不在被声明之前使用, 以及局部引入的类型不逃逸到更宽的作用域中. "
      "合一, 类似于赋值, 为这两种祸害都提供了便利. 我们已经看到类型层级如何与基于区域的内存管理相关联. "
      "那么层级有助于约束合一, 防止资源管理不当" -- "这一次不是针对类型变量而是针对类型常量" --
      "也就不足为奇了.")
   (P "OCaml与SML不同, 它通过" (Code "let module")
      "形式支持局部模块, 即在局部作用域中定义的模块. 一个局部模块可以声明一个类型, 甚至可以让这个类型逃逸, 如"
      (CodeB "let y =
  let module M = struct
        type t = Foo
        let x = Foo
      end
  in M.x
     ^^^
Error: This expression has type M.t but an expression was expected of type 'a
       The type constructor M.t would escape its scope")
      "这样的逃逸必须被标记为错误. 否则, " :y "将获得类型" :M.t ", 而在" :y "所在之处, "
      :M.t "甚至" :M "都不在作用域中. 这个问题类似于从一个C函数返回一个自动局部变量的地址:"
      (CodeB "char * esc_res(void)
{
  char str [] = \"local string\";
  return str;
}")
      "一个局部声明的类型不仅可以通过结果类型逃逸, 也可以通过与一个已存在的类型变量合一而逃逸:"
      (CodeB "fun y ->
  let module M = struct
        type t = Foo
        let r = y Foo
      end
  in ()
                  ^^^
Error: This expression has type t but an expression was expected of type 'a
       The type constructor t would escape its scope")
      "这类错误对C程序员来说也很熟悉:"
      (CodeB "char * y = (char*)0;
void esc_ext(void)
{
  char str [] = \"local string\";
  y = str;
}"))
   (P "即使是顶层模块也有类型逃逸问题. 这是取自OCaml类型检查器中一条注释的例子:"
      (CodeB "let x = ref []
module M = struct
   type t
   let _ = (x : t list ref)
end")
      "变量" :x "具有非泛型类型" (Code "'_a list ref") ". 模块" :M "定义了局部类型" :t
      ". 类型标注使得在" :t "之前定义的" :x "具有类型" (Code "x : t list ref") ". 看起来" :t
      "在被定义之前就被使用了. 正如Jacques Garrigue所指出的, 这样的类型逃逸即使没有模块也可能发生:"
      (CodeB "let r = ref []
type t = Foo
let () = r := [Foo]
               ^^^
Error: This expression has type t but an expression was expected of type 'weak1
       The type constructor t would escape its scope")
      "OCaml不能让这样的逃逸不被发现. 在任何情况下, 一个类型构造子都不能在其声明的作用域之外被使用. "
      "类型层级为类型构造子施行这种类似于区域的纪律.")
   (P "为了类型泛化, OCaml类型检查器已经支持区域, 提供了用于进入新区域的操作" :begin_def
      "和用于退出 (销毁) 新区域的操作" (Code "end_def")
      ", 将类型与其所有者区域关联起来, 并在合一期间跟踪所有权的变化. "
      "剩下的就是让类型声明进入一个新的区域, 并将被声明的类型构造子与该区域关联起来. "
      "任何出现了该类型构造子的类型都必须属于类型声明区域之内的一个区域: 类型构造子的声明必须支配其所有的使用.")
   (P "如前所述, 类型区域由一个正整数, 即类型层级, 来标识: 区域的嵌套深度. 每个类型都有字段"
      :level ", 带有其所有者区域的层级. 类型构造子需要一个类似的层级标注. 结果发现, "
      "OCaml的另一个机制恰好服务于这个目的. 类型构造子, 数据构造子, 项变量可以在一个OCaml程序中被重新定义: "
      "一个类型可以被重新声明, 一个变量可以被多次重新绑定. OCaml依靠" (Em "标识符") " (见" :ident.ml
      ") 来区分同一名字的不同声明或绑定的出现. 一个标识符有名字和时间戳, 后者是一个正数. 全局可变的"
      :Ident.currentstamp "保存着" (Q "当前时间")
      ", 并在一个新的标识符被创建时, 由于一个声明或一个绑定, 推进它. 因此, 标识符的时间戳就是它的绑定时间. "
      "绑定时间是将一个标识符与一个类型区域联系起来的自然方式. 如果当前时间被设为当前层级, "
      "那么新的标识符的绑定时间将不小于当前层级: 它们将被视为由当前类型区域所拥有. "
      "那么不逃逸就意味着一个类型的层级不小于该类型中每个类型构造子的绑定时间.")
   (P "合一, 具体而言, 与一个自由类型变量的合一" -- "类似于赋值" --
      "可能改变一个类型的所有权, 因此必须相应地更新类型层级. 它也可以同时检查不逃逸性质仍然成立: 见"
      :Ctype.update_level ".")
   (P "我们现在可以理解对局部模块, 即表达式" (Code "let module name = modl in body")
      ", 进行类型检查的OCaml代码了, 下面摘录自" :typecore.ml "."
      (CodeB "| Pexp_letmodule(name, smodl, sbody) ->
    let ty = newvar() in
    (* remember the original level *)
    begin_def ();
    Ident.set_current_time ty.level;
    let context = Typetexp.narrow () in
    let modl = !type_module env smodl in
    let (id, new_env) = Env.enter_module name.txt modl.mod_type env in
    Ctype.init_def(Ident.current_time());
    Typetexp.widen context;
    let body = type_expect new_env sbody ty_expected in
    (* go back to original level *)
    end_def ();
    (* Check that the local types declared in modl don't escape
       through the return type of body
    *)
    begin try
      Ctype.unify_var new_env ty body.exp_type
    with Unify _ ->
      raise(Error(loc, Scoping_let_module(name.txt, body.exp_type)))
    end;
    re {
      exp_desc = Texp_letmodule(id, name, modl, body);
      exp_loc = loc; exp_extra = [];
      exp_type = ty;
      exp_env = env }"))
   (P "类型变量" :ty "被创建出来以接收表达式推断出的类型. 该变量在当前区域中被创建. 在此之后, 通过"
      (Code "begin_def()") "进入一个新的类型区域, 并且标识符时间戳时钟被设置为对应于新的" :current_level
      ". (时间戳时钟恰好在一个新的标识符被创建之前推进. 这就是为什么" :Ident.set_current_time "接收"
      :ty.level "而不是增加后的" :current_level "作为参数.) 因此, 在局部模块中声明的任何类型构造子都将具有"
      :current_level "或更高的绑定时间. " (Code "Ctype.init_def(Ident.current_time())")
      "将类型层级设为局部模块最后一个标识符的绑定时间. 因此, 之后在类型检查" :body
      "时创建的所有新鲜类型都将具有大于或等于任何局部模块类型构造子的绑定时间的层级. "
      "合一将看守任何层级更新都保持该不变式. 最后, 在最末尾与" :ty
      "的合一 (回忆一下, 它的区域在" (Code "let module")
      "的区域之外) 将确保没有局部类型构造子通过返回类型逃逸.")
   (P "顺便一提, 上述代码中的" (Code "Typetexp.narrow ()") "和" (Code "Typetexp.widen context")
      "为局部模块内的类型变量建立了一个新的上下文. 这就是为什么"
      (CodeB "fun (x:'a) -> let module M = struct let g (x:'a) = x end in M.g")
      "具有推断出的类型" (Code "'a -> 'b -> 'b") "而不是" (Code "'a -> 'a -> 'a")
      ". 上述代码中" (Code "'a") "的两次出现是不同的类型变量. 一个局部模块不与其周围共享任何类型变量.")
   (P "存在类型与在局部模块中声明的类型非常相似: 事实上, 存在类型可以用第一级局部模块来实现. "
      "因此, 检查由对一个存在类型的模式匹配 (或者说, 打开) 所创建的类型不逃逸出该模式匹配子句, "
      "使用的是同样的技术: 见" :Typecore.type_cases ".")
   (H2. "层级的发现" #:id "history")
   (P "Didier Rémy在做关于记录和变体的类型推断的博士研究时发现了基于层级的类型泛化算法. "
      "(顺便一提, 他把" (Q "层级") "称为秩" -- "可惜的是, " (Q "层级")
      "是现在OCaml类型检查器中所使用的术语.) 他在最初的Caml (Categorical Abstract Machine Language) "
      "中制作了他的记录推断的原型, 这个Caml是用Caml本身写成的, 运行在Le Lisp之上. "
      "那是在Caml Light之前, 更不用说OCaml了. 他必须频繁地重新编译Caml, 这需要很长时间. "
      "正如他所说, Caml的类型推断是瓶颈: "
      (Q "编译器代码的核心是两个用于编译表达式和模式的相互递归函数, 加起来几百行代码, "
         "但类型检查却要花大约20分钟! 仅这一个文件就占用了自举周期中不正常的比例. "
         "那个时候在LaTeX中重新编译字体也要花上很久, 所以我想我们已经习惯于把这类繁重的任务安排在喝咖啡休息之前"
         -- "或者反过来.")
      " Caml中的类型推断缓慢有几个原因. 首先, 类型模式的实例化会创建整个类型的一个新副本"
      -- "即使是不含被量化变量的部分, 这些部分本可以被共享. "
      "在每次自由类型变量的合一时进行出现检查 (就像在我们的eager玩具算法中那样), "
      "以及在每次泛化时扫描整个类型环境, 增加了推断的时间复杂度.")
   (P "Didier Rémy决心加速这一过程. 他说:"
      (Blockquote
       (P "所以, 当我编写对记录和变体进行类型检查的原型时 (它们是结构化的, 往往比通常的ML类型大得多), "
          "我非常小心地在复杂度方面紧贴理论.")
       (Ul (Li "我以" (app $O (&i* $n (ap $log $n)))
               "实现了图上的合一" -- "进行路径压缩并推迟出现检查;")
           (Li "我自始至终保持类型中引入的共享, 而不在泛化/实例化期间破坏它;")
           (Li "最后, 我引入了基于秩的类型泛化."))))
   (P "这个高效的类型推断算法在Rémy的博士论文 (法语) 以及1992年的技术报告中有所描述. "
      "前面解释的" :sound_lazy "算法是Rémy算法的一个非常简单的模型, 体现了其主要特征. "
      "Xavier Leroy在Caml-Light中实现了类型层级和基于层级的泛化. 然而, 出于各种原因, 他实现的是类似于"
      :sound_eager "的版本, 在每次绑定自由类型变量时都进行出现检查.")
   (P "Didier Rémy更喜欢从图的角度来看待秩, 或者说层级. "
      "如果我们在抽象语法树的每个节点上添加类型标注, 为共享的类型添加边, "
      "并添加从被量化变量到其量词的边, 我们就得到一个图. 一个自由类型变量的层级可以被看作De Bruijn层级"
      -- "指向将要量化该类型变量的AST节点的指针. 在Hindley-Milner系统中, 那个AST节点必须是一个"
      :let "节点. 合一两个自由变量就在它们之间添加了一条共享边, 这需要调整层级, "
      "以维持一个量词节点支配其绑定变量的所有使用这一不变式. (回忆一下, 图中对于节点集合" $V
      "的支配节点是这样一个节点" $d ", 使得从根到" $V "中每个节点的所有路径都经过" $d
      ".) 添加共享边可能创造出一条不再经过旧支配节点的路径, 可以说是让变量逃逸了, "
      "并使其被一个具有更宽作用域的" :let "节点所支配.")
   (P "秩的图的视角被证明是富有成效的. 基于秩的泛化很容易扩展到带有多态字段的记录的类型检查. "
      "最终这个图的视角导向了MLF. Didier Rémy评论道, "
      (Q "MLF中的主要操作" -- "提升绑定者" -- "类似于计算两个节点之间的最小秩.")
      " 下面Rémy的两个MLF报告描述了该系统, 并展示了类型检查期间秩调整的几个动画. "
      "他也指出了秩如何契合ML类型推断的基于约束的表述, 后者在"
      (Q "The Essence of ML Type Inference") "中有所解释.")
   (P (B "参考文献")
      (Ul (Li "A History of Caml. "
              (Href "http://caml.inria.fr/about/history.en.html"
                    "http://caml.inria.fr/about/history.en.html")
              ". 小节" (Q "The first implementation") "描述了最初的Caml.")
          (Li "François Pottier and Didier Rémy. The Essence of ML Type Inference. "
              "In Advanced Topics in Types and Programming Languages (Benjamin C. Pierce, editor). "
              "Chapter 10, pages 389-489. MIT Press, 2005.")
          (Li "Didier Rémy: Extension of ML Type System with a Sorted Equational Theory on Types. "
              "Research Report 1766, Institut National de Recherche en Informatique et Automatique, "
              "Rocquencourt, BP 105, 78 153 Le Chesnay Cedex, France, 1992. "
              (Href "http://gallium.inria.fr/~remy/ftp/eq-theory-on-types.pdf"
                    "http://gallium.inria.fr/~remy/ftp/eq-theory-on-types.pdf"))
          (Li "Didier Rémy: A new look on MLF. "
              (Href "http://cristal.inria.fr/~remy/mlf/portland.pdf"
                    "http://cristal.inria.fr/~remy/mlf/portland.pdf"))
          (Li "Didier Rémy: MLF for Everyone (Users, Implementers, and Designers). "
              (Href "http://cristal.inria.fr/~remy/mlf/mlf-for-everyone.pdf"
                    "http://cristal.inria.fr/~remy/mlf/mlf-for-everyone.pdf"))
          (Li "David McAllester: A logical algorithm for ML type inference. Proc. RTA'03, pp. 436-451. "
              "David McAllester在很久之后重新发现了这种高效的泛化. "
              "他还展示了对于大多数实际程序而言, ML类型推断与程序大小几乎成线性关系.")
          (Li "George Kuan and David MacQueen: Efficient ML Type Inference Using Ranked Type Variables. "
              "ML Workshop 2007. "
              (Href "http://people.cs.uchicago.edu/~gkuan/pubs/ml07-km.pdf"
                    "http://people.cs.uchicago.edu/~gkuan/pubs/ml07-km.pdf")
              ". 这篇论文比较了两个基于层级的Hindley-Milner推断算法: 一个使用" :let
              "层级, 正如本页所解释的, 而另一个依赖于lambda层级. 该论文为这两个算法开发了抽象机, "
              "并描述了它们若干有趣的形式性质. lambda层级的方法被用于SML/NJ.")
          (Li "Peter Sestoft: Programming Language Concepts. "
              "Springer Undergraduate Texts in Computer Science. xiv + 278 pages. July 2012. "
              (Href "http://www.itu.dk/people/sestoft/plc/" "http://www.itu.dk/people/sestoft/plc/")
              ". 第6章 (见上述页面上的讲义幻灯片和例子) 描述了Rémy算法的一个更简单的版本"
              -- "本质上就是" :sound_eager ".")))
   (H2. "创建新鲜类型变量" #:id "newtvar")
   (P "OCaml类型检查器提供了两个用于创建新鲜类型变量的函数. 本节阐明它们之间的区别. 这些函数定义于"
      :ctype.ml ", 具有如下签名:"
      (CodeB "newvar    : ?name:string -> unit -> type_exp
newgenvar : ?name:string -> unit -> type_exp")
      "两者都接受可选参数" :?name "来给变量命名. 否则名字将被自动选取.")
   (P "函数" :newvar "在" :current_level "处创建一个变量, 而" :newgenvar "在" :generic_level
      "处创建. 在代码"
      (CodeB "let ty1 = newvar () in
unify env ty1 some_type

let ty2 = newgenvar () in
unify env ty2 some_type")
      "中, " :ty1 "和" :ty2 "的行为相同: 类型变量将被绑定到" :some_type ". 由于" :current_level
      "对应于最内层的存活区域, " :some_type "的层级是当前层级或更小, 因此在两种情况下都保持不变.")
   (P "区别出现在以下两段代码中 (第二段经常出现在" :typecore.ml "中)"
      (CodeB "let ty1 = newvar () in
let list_type = newgenty (Tconstr(p_list, [ty1])) in
let texp = instance env list_type in
unify env texp some_type

let ty2 = newgenvar () in
let list_type = newgenty (Tconstr(p_list, [ty2])) in
let texp = instance env list_type in
unify env texp some_type")
      "函数" :instance "复制类型" -- "准确地说, 创建一个" :Tsubst "节点" --
      "仅当类型是泛型的时候. 也就是说, 在"
      (CodeB "let ty = newvar () in instance env ty")
      "中, " :instance "表现为恒等函数. 然而, 在"
      (CodeB "let ty = newgenvar () in instance env ty")
      "中, " :instance "复制该变量. 因此, 在上面的第一段代码中, 末尾的" :unify "可能通过实例化"
      :ty1 "而影响" :list_type ". 在第二段代码中, " :list_type "不可能受到影响, 因为" :unify
      "将作用于" :ty2 "的副本.")
   (H2. "泛化的真实复杂度" #:id "gen-in-full")
   (P "OCaml中的" :let "泛化比我们之前所勾勒的要复杂得多. 本节是为了帮助领会泛化的真实复杂度.")
   (P "OCaml中的" :let "表达式具有一般形式"
      (CodeB "let [rec] pattern = exp and pattern = exp ... in body")
      :let "类型检查器" :type_let " (" :typecore.ml
      "中的160行代码, 不计模式的类型检查) 接收模式-表达式对的列表以及递归标志. 这是其代码的结尾"
      (CodeB "begin_def ();
...
let exp_list =
 List.map2
   (fun (spat, sexp) (pat, slot) -> .... (* type checking of expressions *)
     type_expect exp_env sexp pat.pat_type)
   spat_sexp_list pat_slot_list in
...
end_def();
List.iter2
  (fun pat exp ->
     if not (is_nonexpansive exp) then
       iter_pattern (fun pat -> generalize_expansive env pat.pat_type) pat)
  pat_list exp_list;
List.iter
  (fun pat -> iter_pattern (fun pat -> generalize pat.pat_type) pat)
  pat_list;
(List.combine pat_list exp_list, new_env, unpacks)")
      "我们看到了熟悉的模式:"
      (CodeB "begin_def(); ... newvar () ... end_def(); generalize")
      "但是还有另一次对类型的遍历, 通过" :generalize_expansive
      ". 只有当表达式是扩张的, 即可能具有可见的效应时" -- "例如, 它是一个应用" --
      "该函数才会被调用. 函数" :Ctype.generalize_expansive "遍历其参数" :type_expression
      "; 当它遇到一个构造出的类型" :Tconstr " (例如列表类型等), 并且即将遍历一个" :arg "时, "
      :generalize_expansive "检查类型" :p "的声明中该参数的变型. 如果" :arg "是协变的, "
      :generalize_expansive "遍历" :arg ", 并将层级高于" :current_level "的分量的层级设为"
      :generic_level ". 如果" :arg "不是协变的 (例如, " :ref "和" :array "类型构造子的参数), 那么"
      :arg "中层级高于当前层级的分量被设为" :current_level ". 随后的" :generalize
      "将保持这些层级不变. 这就是所谓的放松的值限制的实现方式, 它负责为"
      (CodeB "# let x = (fun y -> print_string \"ok\"; y) [];;
ok
val x : 'a list = []")
      "推断出多态类型. 这里, " :x "被绑定到一个应用, 它在句法上不是一个值, 并且是扩张的. "
      "它的求值当然有可见的效应. 然而" :x "的类型被泛化了, 因为" :list
      "类型在其参数上是协变的. SML则不会如此.")
   (P (B "参考文献")
      (Ul (Li "Jacques Garrigue: Relaxing the Value Restriction. FLOPS 2004, pp. 196-213.")))
   ))
