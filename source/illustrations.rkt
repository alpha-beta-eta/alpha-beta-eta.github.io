#lang racket
(provide illustrations.html)
(require SMathML)
(define $bull (Mo "&bull;"))
(define def:affine-transformation
  (MB (&= (Mat ((_ $x $bull) (_ $y $bull)))
          (&+ (&i* (Mat ($x $y))
                   (Mat ($a $b) ($c $d)))
              (Mat ($e $f))))))
(define $sin (Mi "sin"))
(define $cos (Mi "cos"))
(define (&sin x) (ap $sin x))
(define (&cos x) (ap $cos x))
(define illustrations.html
  (TmPrelude
   #:title "绘制数学图形"
   #:css "styles.css"
   (H1 "绘制数学图形")
   (P "本书介绍了如何在PostScript中绘制图形, 当然也必然涉及一些几何.")
   (H2 "第1章 开始")
   (H3 "第1.1节 简单绘图")
   (CodeB "GS>newpath
GS>144 144 moveto
GS>288 288 lineto
GS>stroke
GS>")
   (Ul (Li "构造一条路径始于命令" (Code "newpath") ". 这就像拿起笔准备画画.")
       (Li "路径本身始于命令" (Code "moveto") ". 这就像将笔置于路径的起点.")
       (Li "使用命令" (Code "lineto") "为路径添加线条. 这就像在纸上移动笔.")
       (Li "已经构造好了路径, 使用命令" (Code "stroke")
           "绘制它, 或者说使其变得可见.")
       (Li "PostScript总是倒着书写, 参数总是出现在运算符之前, 此即所谓逆波兰记号 "
           "(Reverse Polish Notation, RPN)."))
   (P "以下命令序列可以绘制一个边长为" $2 "英寸的正方形.")
   (CodeB "newpath
144 144 moveto
288 144 lineto
288 288 lineto
144 288 lineto
144 144 lineto
stroke")
   (P "当然PoscScript中存在许多不同的方式来绘制这个正方形.")
   (CodeB "newpath
144 144 moveto
144 0 rlineto
0 144 rlineto
-144 0 rlineto
closepath
stroke")
   (P (Code "rlineto") "是" (Code "lineto") "的相对 (relative) 位置版本. "
      (Code "closepath") "通过回到上一个应用" (Code "moveto")
      "的位置以封闭路径.")
   (CodeB "newpath
144 144 moveto
144 0 rlineto
0 144 rlineto
-144 0 rlineto
closepath
fill")
   (P (Code "fill") "和" (Code "stroke") "有着不同的效果. "
      )
   (CodeB "0.5 setgray
newpath
144 144 moveto
144 0 rlineto
0 144 rlineto
-144 0 rlineto
closepath
fill")
   (CodeB "1 0 0 setrgbcolor
newpath
144 144 moveto
144 0 rlineto
0 144 rlineto
-144 0 rlineto
closepath
fill")
   (H3 "第1.2节 简单坐标变换")
   (P "对于生活在北美的人而言, 既然默认的页面尺寸为"
      (&c* (&Prime (Mn "8.5")) (&Prime 11))
      ", 与单位英寸打交道通常来得更加容易. "
      "以下命令可以将基本的长度单位转换为英寸.")
   (CodeB "72 72 scale")
   (P "当进行缩放的时候, 需要注意默认的线宽是一个单位. "
      "如果什么都不做, 那么现在线宽就是" $1
      "英寸了.")
   (CodeB "0.01389 setlinewidth")
   (P "你可以使用" (Code "0.01389 setlinewidth")
      "将其改回来, 因为" (Mn "0.01389") "约等于"
      (&/ 1 72) ".")
   (CodeB "1 2 translate")
   (P "你可以使用" (Code "translate")
      "进行平移, 比如这里将坐标原点右移了" $1
      "个单位, 上移了" $2 "个单位.")
   (CodeB "72 72 scale
4.25 5.5 translate")
   (P "如此原点就移动了到了页面的中心.")
   (P "还有一种简单的坐标变换, 即旋转" (Code "rotate") ".")
   (CodeB "144 144 translate
30 rotate
newpath
0 0 moveto
144 0 lineto
144 144 lineto
0 144 lineto
0 0 lineto
stroke")
   (P "旋转总是围绕当前原点进行, 注意到PostScript的角度单位是角度制而不是弧度制.")
   (H3 "第1.3节 坐标框架")
   (H3 "第1.4节 PostScript中做算术")
   (P "PostScript是一种完备的编程语言. "
      )
   (CodeB "GS>3 4 add
GS&lt;1>")
   (P "你可能想问" (Code "&lt;1>") "是什么意思, 它指的是栈的深度.")
   (CodeB "GS>3 4 add
GS&lt;1>=
7
GS>")
   (P (Code "=") "移除栈顶的元素, 并将其输出. " (Code "==")
      "与" (Code "=") "类似, 但是输出在某种意义上更加美观, 所以你总是应该使用"
      (Code "==") ".")
   (CodeB "GS>3 4 add
GS&lt;1>stack
7
GS&lt;1>")
   (P (Code "stack") "输出整个栈, 类似的命令还有" (Code "pstack")
      ". " (Code "pstack") "之于" (Code "stack") "就像" (Code "==")
      "之于" (Code "=") ".")
   (CodeB "GS>3 3 mul
GS&lt;1>4 4 mul
GS&lt;2>add
GS&lt;1>sqrt
GS&lt;1>=
5.0
GS>")
   (H3 "第1.5节 错误")
   (H3 "第1.6节 ")
   (H3 "第1.7节 一些细节")
   (let ((pic (lambda (linecap)
                (Svg #:attr* '((width "500") (height "50"))
                     (Line #:attr* `((x1 "50")
                                     (y1 "25")
                                     (x2 "450")
                                     (y2 "25")
                                     (stroke "black")
                                     (stroke-width "25")
                                     (stroke-linecap ,linecap)))))))
     (Table
      (Tr (Th (Code "linecap")) (Th "样式"))
      (Tr (Td (Code "0")) (Td (pic "butt")))
      (Tr (Td (Code "1")) (Td (pic "round")))
      (Tr (Td (Code "2")) (Td (pic "square")))))
   (P "默认情况下" (Code "linecap") "为" (Code "0") ", 你可以通过"
      (Code "setlinecap") "来设置这个参数.")
   (CodeB "9 setlinewidth
% linejoin = 0 by default
newpath
0 0 moveto
0 72 lineto
72 0 lineto
stroke")
   (P (Code "linejoin") "是一个与" (Code "linecap")
      "有点联系的参数, 它控制line join的样式, 其值同样仅可能为"
      (Code "0") ", " (Code "1") ", " (Code "2")
      ", 并使用" (Code "setlinejoin") "修改.")
   (H3 "第1.8节 消除冗余的一个技巧")
   (CodeB "newpath
144 144 moveto
144 0 rlineto
0 144 rlineto
-144 0 rlineto
closepath
gsave
1 0 0 setrgbcolor
fill
grestore
0 setgray
stroke")
   (H3 "第1.9节 总结")
   (CodeB "newpath
moveto
lineto
rmoveto
rlineto
closepath
stroke
fill")
   (CodeB "translate
scale
rotate")
   (CodeB "setlinewidth
setrgbcolor
setgray
setlinejoin
setlinecap")
   (CodeB "gsave
grestore
showpage")
   (H2 "第2章 初等坐标几何")
   (H3 "第2.1节 点和向量")
   (P "如果" $P "是一个点而" $v "是一个向量, 那么" (&+ $P $v)
      "是一个点, 其是满足" (&= (&- $Q $P) $v)
      "的点" $Q ". 我们称" $Q "是" $P "平移" $v
      "得到的. 若给定实数" $t "以及点" $P "和" $Q
      ", 我们可以构造一个点" (&+ $P (&i* $t (@- $Q $P)))
      ". 若" (&<= $0 $t $1) ", 那么该点在线段" (: $P $Q)
      "上. 当" (&= $t (&/ $1 $2)) "时, 该点即" (: $P $Q)
      "的中点. 我们可以将" (&+ $P (&i* $t (@- $Q $P)))
      "写成" (&+ (&i* (@- $1 $t) $P) (&i* $t $Q))
      "的形式.")
   
   (H2 "第3章 变量和过程")
   (H3 "第3.1节 PostScript中的变量")
   (CodeB "/s 1 def

newpath
0 0 moveto
s 0 rlineto
0 s rlineto
s neg 0 rlineto
closepath
stroke")
   (H3 "第3.2节 PostScript中的过程")
   
   (H2 "第4章 坐标和条件式")
   (H3 "第4.1节 坐标")
   (P "PostScript在内部(至少是隐式地)处理三种坐标系统. 第一种是物理坐标系. "
      "物理坐标系的基本单位是像素. 第二种是页面坐标系. 其原点位于页面的左下角, "
      "单位长度为一个Adobe点, 等于" (&/ $1 72) "英寸. 页面可以被视为一种"
      "理想化了的物理设备. 第三种是用户坐标系, 这是用户绘制图像时使用的坐标系. "
      "最初页面坐标系和用户坐标系是相同的, 但是特定的操作, 例如"
      (Code "scale") ", " (Code "translate") ", " (Code "rotate")
      ", 可以改变其间的关系.")
   def:affine-transformation
   (P "仿射变换可以看成是一个线性变换加上一个平移分量, 其具有特定的性质: "
      "直线经过变换仍然是直线, 平行线经常变换仍然是平行的.")
   (H3 "第4.2节 PostScript如何存储坐标变换")
   (P "确定一个仿射坐标变换"
      def:affine-transformation
      "所需要的数据被存储在一个长度为六的数组"
      (Code "[a b c d e f]")
      "里, 其被称为matrix. 命令" (Code "matrix")
      "将数组" (Code "[1 0 0 1 0 0]")
      "置于栈上, 此即恒等变换. 若此时再使用" (Code "currentmatrix")
      "命令, 则会将当前变换置于该数组之中."
      (CodeB "GS>matrix currentmatrix ==
[1.33333337 0.0 0.0 1.33333337 0.0 0.0]
GS>")
      "[注记: 至少若是在Windows系统之中调整缩放比例, 结果是会改变的, 当然这也是相当合理的.] "
      (Code "defaultmatrix") "和" (Code "currentmatrix")
      "有些类似, 但是顾名思义, 其将从页面到物理坐标系的变换置于栈顶的数组之中. "
      "如果什么都没动, 这两个命令的效果显然是一样的. [译注: " (Code "currentmatrix")
      "当然就是从用户到物理坐标系的变换了.]")
   (P "现在我们想要解方程"
      def:affine-transformation
      "以用" (_ $x $bull) "和" (_ $y $bull) "表达" $x "和" $y
      ". 我们考虑将其写成"
      (MB (&= (_ $P $bull) (&+ (&i* $P $A) $v)))
      "的形式. 假定" $A "可逆, 我们得到"
      (MB (&= $P (&- (&i* (_ $P $bull) (inv $A))
                     (&i* $v (inv $A)))))
      "也就是说, 这个逆变换同样是仿射变换. 实际上, PostScript拥有一个计算逆变换的命令, 即"
      (Code "invertmatrix") ". 另外, " (Code "concatmatrix") "可以用来计算变换的复合. "
      "它们的用法如下."
      (CodeB "M matrix invertmatrix")
      "这可以将" (Code "M") "的逆变换置于栈上."
      (CodeB "A B matrix concatmatrix")
      "这可以将" (Code "A") "和" (Code "B") "的复合置于栈上. "
      "[译注: 先施行变换" (Code "A") ", 后施行变换" (Code "B") ".]")
   (P "显然, 我们可以藉由这些操作来计算从用户到页面坐标系的变换."
      (CodeB "/user-to-page-matrix {
  matrix currentmatrix
  matrix defaultmatrix
  matrix invertmatrix
  matrix concatmatrix
} def")
      "当前变换是" (&-> 'user 'physics) ", 默认变换是"
      (&-> 'page 'physics) ", 逆就是" (&-> 'physics 'page)
      ", 当前变换和{默认变换的逆}复合一下就得到了"
      (&-> 'user 'page) ".")
   (H3 "第4.3节 绘制坐标系")
   (H3 "第4.4节 移入三维")
   (P "注记: 本节考虑的不是三维的仿射变换的情形, 而是仿射坐标系的诸多事宜.")
   (P "实际上, 我们可以在三维的情况下用线性变换表示二维的仿射变换."
      def:affine-transformation
      "可以被重写为"
      (MB (&= (Mat ((_ $x $bull) (_ $y $bull) $1))
              (&i* (Mat ($x $y $1))
                   (Mat ($a $b $0)
                        ($c $d $0)
                        ($e $f $1)))))
      "这样之后, 仿射变换的复合就变成纯粹的矩阵乘法了.")
   (H3 "第4.5节 坐标变换是如何进行的")
   (P "现在我们可以检视如何以矩阵表示先前的几种简单变换."
      (CodeB "a b scale")
      (MB (Mat ($a $0 $0)
               ($0 $b $0)
               ($0 $0 $1)))
      (CodeB "x rotate")
      (MB (Mat ((&cos $x) (&sin $x) $0)
               ((&- (&sin $x)) (&cos $x) $0)
               ($0 $0 $1)))
      (CodeB "a b translate")
      (MB (Mat ($1 $0 $0)
               ($0 $1 $0)
               ($a $b $1)))
      "当然了, 最一般的变换如下."
      (CodeB "[a b c d e f] concat")
      (MB (Mat ($a $b $0)
               ($c $d $0)
               ($e $f $1)))
      "以上这些变换的效果都是给当前矩阵" (B "左乘") "其变换矩阵.")
   ((exercise #:n "6")
    "在变换"
    (CodeB "72 72 scale
4 5 translate
30 rotate")
    "之后, 从用户到页面坐标系的变换矩阵应该是什么?")
   (P "在Racket写了点简单的程序算一算."
      (CodeB "> (matrix-print
   (matrix* (make-scale 72 72)
            (make-translation 4 5)
            (make-rotation (/ pi 6))))
62.353829072479584 -35.99999999999999 288 
35.99999999999999 62.353829072479584 360 
0 0 1 ")
      "当然, 我的这个采用的是通行的左乘而不是PostScript的右乘, 所以结果应该是其转置."
      (CodeB "GS>/user-to-page-matrix {
  matrix currentmatrix
  matrix defaultmatrix
  matrix invertmatrix
  matrix concatmatrix
} def
GS>72 72 scale
GS>4 5 translate
GS>30 rotate
GS>user-to-page-matrix
GS&lt;1>==
[62.3538284 36.0 -36.0 62.3538284 288.0 360.0]
GS>")
      "我们再在PostScript的REPL里算一遍, 的确如此.")
   (H2 "第5章 绘制多边形: 循环和数组")
   (H2 "第6章 曲线")
   
   ))