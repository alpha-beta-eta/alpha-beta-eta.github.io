#lang racket
(provide cad.html)
(require SMathML)
(define $equiv (Mo "&equiv;"))
(define $dim (Mi "dim"))
(define (&dim V) (ap $dim V))
(define-infix*
  (&equiv $equiv)
  
  )
(define cad.html
  (TmPrelude
   #:title "计算机辅助设计的几何编程"
   #:css "styles.css"
   (H1 "计算机辅助设计的几何编程")
   (H2 "第一部分 编程和几何")
   (H3 "第一章 介绍FL和PLaSM")
   (P "本书意图描述的设计语言PLaSM是FL的一个子集的面向几何的扩展. "
      "FL是一种在函数层次 (Function Level) 上进行编程的高级语言, "
      "由IBM位于Almaden的研究部门的函数式编程组开发. "
      )
   (H4 "第1.1节 符号设计编程介绍")
   (P "FL语言 (可以追溯至Backus的Turing讲座 [译注: 现在有两种Turing讲座, 这个指的是Turing奖获得者给的讲座]) "
      "引入了程序上的代数, 其中函数表达式之间的一集代数等式被建立起来. "
      )
   (H5 "第1.1.1小节 计算模型")
   (P (B "程序是函数.") " "
      )
   (P (B "程序复合与应用.") " PLaSM程序的复合表现得恰如数学函数的标准复合. 例如, 复合数学函数"
      (&compose $f $g) "应用于参数" $x
      (MB (&equiv (app (@ (&compose $f $g)) $x) (app $f (app $g $x))))
      "的意思是函数" $g "先应用于" $x ", 然后函数" $f "再应用于值" (app $g $x)
      ". 之前的表达式的PLaSM记号将会是"
      (CodeB "(f~g):x" $equiv "f:(g:x)")
      "其中" (Code "~") "代表程序复合, 而" (Code "g:x") "代表将函数" (Code "g") "应用于参数"
      (Code "x") ".")
   (P (B "命名对象.") " 在PLaSM中, 一个名字通过使用" (Code "DEF") "被赋予每个由语言生成的值, "
      "可以带有或不带显式参数. "
      
      (CodeB "DEF object = (Fun3 ~ Fun2 ~ Fun1):parameters;")
      
      )
   (P (B "参数化对象.") " "
      (CodeB "DEF object (params::IsSeq) = (Fun3 ~ Fun2 ~ Fun1):params;

DEF obj1 = object:&lt;" (&cm $p_1 $p_2 $..h $p_n) ">;
DEF obj2 = object:&lt;" (&cm $q_1 $q_2 $..h $q_n) ">;")
      
      )
   (H4 "第1.2节 PLaSM准备")
   (H5 "第1.2.1小节 安装语言")
   (H6 "软件下载")
   (P "译者建议: 这部分内容有点年久失修, 建议参考PLaSM主页:"
      (CodeB "http://www.dia.uniroma3.it/plasm/")
      (Ol (Li (B "解释器.") " PLaSM解释器位于"
              (CodeB "http://www.plasm.net/download/")
              "译者建议: 本书所用的PLaSM版本, 是基于PLT Scheme实现的, 也就是现在的Racket. "
              "之前还有Common Lisp实现的版本, 现在还有Python实现的版本. 读者想用哪个版本"
              "就用哪个版本, 尽管我觉得每个实现都有点糟糕到离谱, 以至于让我怀疑作者是不是"
              "真读了他开头引用的Essentials of Programming Languages第一版.")
          (Li (B "编辑器.") " 译者建议: 官网上也说他实现了一个编辑器, 然而我怎么也没找到"
              "下载的地方, 建议不要关心了.")
          (Li (B "浏览器插件.") "译者建议: Flash是已经死了. SVG现在不用插件就能看. "
              "VRML格式建议下载专门软件观看.")
          )
      )
   (H5 "第1.2.2小节 开始")
   (P (Ol (Li "打开Xplode编辑器. [译者: 白给, 啥也没有, 只能用交互体验"
              "垃圾得要死的REPL了.]")
          (Li "输入代码:"
              (CodeB "DEF mycube = CUBOID:&lt;1,1,1> COLOR RED;
mycube;")
              "译者建议: 这东西的REPL其实是一个PLT Scheme的REPL (是不是让人想到了LCF之于ML), "
              "然后有一个解释过程, 接受字符串作为参数, 然后parse运行PLaSM程序. 我真的不是很能理解, "
              "别的语言也就算了, 在Scheme里作者为什么不把PLaSM做成embedded DSL.")
          (Li ""
              )
          )
      )
   (H6 "与语言进行交互")
   (P "作者必须明白每个PLaSM源程序是一个字符串 [译者: 不明白的话, 读者都不明白如何运行这逆天语言], "
      "即被(双)引号包裹的字符序列. "
      )
   (CodeB "(plasm &quot;(SQRT:3 + 10) / (3.5 * COS:(PI/4))&quot;)")
   
   (H4 "第1.3节 在函数层次进行编程")
   (H5 "第1.3.1小节 FL句法基础")
   (P "原始FL对象是字符, 数字和真值. 原始对象, 函数, 应用和序列是表达式. [译注: 我又想问一下作者的编程语言"
      "到底是怎么学的了.] 序列是由逗号隔开尖括号包裹的表达式:"
      (CodeB "&lt;5,fun>")
      "应用" (Code "exp1:exp2") "对于" (Code "exp1") "和" (Code "exp2") "求值, 然后将前者的值应用于"
      "后者的值. 注意到二元函数既可用前缀形式也可用中缀形式:"
      (CodeB "1+3" $equiv "+:&lt;1,3>" $equiv "4")
      "函数应用是向左结合的:"
      (CodeB "f:g:h" $equiv "(f:g):h")
      "应用比复合绑定得更紧密:"
      (CodeB "f:g~h" $equiv "(f:g)~h"))
   (H5 "第1.3.2小节 组合形式与函数")
   
   (H4 "第1.4节 PLaSM编程基础")
   (H4 "第1.5节 几何运算符")
   (H4 "第1.6节 例子")
   (H4 "第1.7节 带注解的参考 (Anotated references)")
   
   (H3 "第二章 几何编程")
   (H4 "第2.1节 基本编程")
   
   (H4 "第2.2节 基本几何编程")
   
   (H4 "第2.3节 组装形状")
   
   (H3 "第三章 线性代数基础")
   (P "译注: 你会发现这本书的数学都有点奇怪, 因为作者倾向于将具体和抽象杂糅在一起.")
   (H4 "第3.1节 向量空间")
   (P "一个域" $F:script "上的向量空间" $V:script "是一个集合, 其带有两种复合规则 (即运算)"
      (MB (&cm (func $+ (&c* $V:script $V:script) $V:script)
               (func $d* (&c* $F:script $V:script) $V:script)))
      "满足以下公理 [译注: 这应该都是常识, 我就不翻译了]:")
   ((example #:n "3.1.1. 实矩阵的向量空间")
    "令" (app (_^ $M:script $n $m) $RR) "表示域" $RR "上的" (&c* $m $n)
    "矩阵的集合. [译注: 何以成为向量空间的部分应该是常识, 我就不翻译了.] 记号:"
    (MB (&cm (&= (app (_^ $M:script $1 $m) $RR) $RR^m)
             (&= (app (_^ $M:script $n $1) $RR) $RR_n)
             (&= (app (_^ $M:script $n $m) $RR) (_^ $RR $n $m)))))
   ((definition #:n ". 子空间")
    "[译注: 定义没什么好写的了.] " $V:script "的子空间" $U:script
    "的余维数 (codimension) 被定义为" (&- (&dim $V:script) (&dim $U:script))
    ". 显然两个子空间的交仍然是子空间. [译注: 实际上不必限于有限的情况, 无限的情况也可以.]")
   ((definition #:n ". 线性组合")
    )
   (H4 "第3.2节 仿射空间")
   (P "一个点的集合" $A:script "被称为是" $V:script "上的一个仿射空间, 如果存在一个映射"
      (MB (&cm (&-> (&c* $A:script $V:script) $A:script)
               (&\|-> (tu0 $x $v) (&+ $x $v))))
      "满足以下性质:")
   ((definition #:n ". 正组合")
    "[译注: 这里的术语是positive combination, 实际上更多人将其称为conical combination, 锥组合.] "
    "对于" (∈ $v_0 $..h $v_d $RR^n) "和" (&>= (&cm $alpha_0 $..h $alpha_d) $0) ", 向量"
    (MB (&= (&+ (&i* $alpha_0 $v_0) $..c (&i* $alpha_d $v_d))
            (sum (&= $i $0) $d (&i* $alpha_i $v_i))))
    "被称为这些向量的正组合. " (setE $v_0 $..h $v_d) "的所有正组合构成的集合被称为其正包 "
    "(positive hull), 记作" (ap 'pos (setE $v_0 $..h $v_d)) ". 这个集合也被称为由这些元素生成的锥.")
   ((definition #:n ". 仿射组合")
    "令" (∈ $p_0 $..h $p_d $EE^n) "和" (∈ $alpha_0 $..h $alpha_d $RR)
    "满足" (&= (&+ $alpha_0 $..c $alpha_d) $1) ". 点"
    (MB (&= (sum (&= $i $0) $d (&i* $alpha_i $p_i))
            (&+ $p_0 (sum (&= $i $1) $d (&i* $alpha_i (@- $p_i $p_0))))))
    "被称为点" (&cm $p_0 $..h $p_d) "的一个仿射组合. " (setE $p_0 $..h $p_d)
    "的所有仿射组合构成了一个仿射空间, 记作" (ap 'aff (setE $p_0 $..h $p_d)) ". 很容易验证:"
    (MB (&= (ap 'aff (setE $p_0 $..h $p_d))
            (&+ $p_0 (ap 'lin (setE (&- $p_1 $p_0) $..h (&- $p_d $p_0))))) "."))
   ((definition #:n ". 凸组合")
    "令" (∈ $p_0 $..h $p_d $EE^n) "和" (&>= (&cm $alpha_0 $..h $alpha_d) $0)
    "满足" (&= (&+ $alpha_0 $..c $alpha_d) $1) ". 点"
    (MB (&= (&+ (&i* $alpha_0 $p_0) $..c (&i* $alpha_d $p_d))
            (sum (&= $i $0) $d (&i* $alpha_i $p_i))))
    "被称为点" (&cm $p_0 $..h $p_d) "的一个凸组合. "
    )
   (H3 "第四章 多面体几何基础")
   (H4 "第4.1节 基本概念")
   ((definition #:n ". 拓扑")
    
    )
   (H3 "第五章 微分几何基础")
   (H4 "第5.1节 曲线")
   
   (H2 "第二部分 图形学")
   (H3 "第六章 仿射变换")
   (H3 "第七章 几何原语")
   (H3 "第八章 层次结构")
   (H3 "第九章 图形管线")
   
   (H2 "第三部分 建模")
   
   ))