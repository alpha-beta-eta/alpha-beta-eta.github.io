#lang racket
(provide engine.html)
(require SMathML)
(define engine.html
  (TnTmPrelude
   #:title "游戏引擎架构笔记"
   #:css "styles.css"
   (H1. "游戏引擎架构笔记")
   (H2. "引论")
   (H2. "工具")
   (H2. "游戏软件工程基础")
   (H2. "并行和并发编程")
   (H2. "游戏3D数学")
   (H2. "引擎支持系统")
   (H2. "资源以及文件系统")
   (H2. "游戏循环和实时模拟")
   (P "游戏是实时的, 动态的, 交互性的计算机模拟. "
      )
   (H3. "渲染循环")
   (H3. "游戏循环")
   (H4. "一个简单的例子: Pong")
   (H3. "游戏循环架构风格")
   (H4. "Windows消息泵")
   (H4. "回调驱动的框架")
   (P "大多数游戏引擎子系统和第三方游戏中间件包以库的形式结构化. "
      )
   (P "与之相对的是, 一些游戏引擎和游戏中间件包以框架的形式结构化. "
      "一个框架是一个部分完成的应用. "
      "程序员通过提供缺失机能的实现或者覆盖默认的行为以完成应用. "
      )
   (P "在基于框架的渲染引擎或者游戏引擎之中, "
      "主游戏循环已经写给我们了, 只是它大致上是空的. "
      "游戏程序员可以编写回调函数来填充缺失的细节. "
      "OGRE渲染引擎是库包裹于框架的一个例子. "
      "在最低层次上, OGRE提供了可由游戏引擎程序员直接调用的函数. "
      "然而, OGRE也提供了一个框架, "
      "其包裹了如何有效使用低层次的OGRE库的知识. "
      "如果程序员选择使用OGRE框架, "
      "那么其需要从" (Code "Ogre:FrameListener")
      "导出一个类并覆盖两个虚函数: "
      (Code "frameStarted()") "和" (Code "frameEnded()")
      ". 正如你可能会猜想到的, "
      "这些函数分别在主3D场景由OGRE渲染之前和之后调用. "
      "OGRE框架对于其内部游戏循环的实现看起来像是以下的伪代码."
      (CodeB "while (true)
{
    for (each frameListener)
    {
        frameListener.frameStarted();
    }

    renderCurrentScene();

    for (each frameListener)
    {
        frameListener.frameEnded();
    }

    finalizeSceneAndSwapBuffers();
}")
      "一个特定游戏的帧监听器实现看起来可能像以下这样."
      (CodeB "class GameFrameListener : public Ogre::FrameListener
{
public:
    virtual void frameStarted(const FrameEvent& event)
    {
        // Do things that must happen before the 3D scene
        // is rendered (i.e., service all game engine
        // subsystems).
        pollJoypad(event);
        updatePlayerControls(event);
        updateDynamicsSimulation(event);
        resolveCollisions(event);
        updateCamera(event);
        
        // etc.
    }

    virtual void frameEnded(const FrameEvent& event)
    {
        // Do things that must happen after the 3D scene
        // has been rendered.
        drawHud(event);

        // etc.
    }
};"))
   (H4. "基于事件的更新")
   (P "在游戏之中, 事件是游戏或其环境的状态的任何有趣改变. "
      "大多数游戏引擎有着一个事件系统, "
      "其允许诸引擎子系统订阅特定种类的事件, "
      "并在那些事件发生时进行反应. "
      "游戏的事件系统往往也非常类似于作为GUI基础的事件/消息系统.")
   (H3. "抽象时间线")
   (P "在游戏编程中, 基于抽象时间线进行思考可能是极其有用的. "
      "一个时间线是一个连续的一维的轴, "
      "其原点可以坐落于相对于系统内的其他时间线的任何位置. "
      "时间线可以简单地由时钟变量实现, "
      "其存储着整数或者浮点形式的绝对时间值.")
   (H4. "真实时间")
   (H4. "游戏事件")
   (H4. "局部和全局时间线")
   (H3. "度量和处理时间")
   (H3. "多处理器游戏循环")
   (H2. "人类接口设备")
   (H2. "debug和开发工具")
   (H2. "渲染引擎")
   (H2. "动画系统")
   (H2. "碰撞和刚体动力学")
   (H2. "音频")
   (H2. "gameplay系统引论")
   (H3. "游戏世界解剖")
   (H4. "世界元素")
   (H4. "世界chunk")
   (H4. "高层次游戏流")
   (H3. "实现动态元素: 游戏对象")
   (H4. "游戏对象模型")
   (H3. "数据驱动游戏引擎")
   (H3. "游戏世界编辑器")
   (H2. "运行时gameplay基础系统")
   (H3. "gameplay基础系统的组件")
   (H3. "运行时对象模型架构")
   (H3. "世界chunk的数据格式")
   (H3. "")
   (H3. "对象引用和世界查询")
   (H3. "实时更新游戏对象")
   (H3. "应用并发于游戏对象更新")
   (H3. "事件和消息传递")
   ))