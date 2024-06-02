#lang racket
(provide game_design.html)
(require SMathML)
(define game_design.html
  (TmPrelude
   #:title "游戏设计"
   #:css "styles.css"
   (H1 "游戏设计")
   (P "虽然我玩过的游戏很少, 但是我却对于游戏这一形式有着近乎疯狂的迷恋. "
      "很难说这种迷恋的由来, 但或许这是因为游戏是一种交互的媒介, "
      "很适合用来表达自我, 而制作游戏的过程也是同样的有趣.")
   (P "至今我还没有完整地设计和实现过任何一个游戏, "
      "但是或许我应该先试一试再说, 而且在某种意义上我应该从模仿开始.")
   (H2 "The Design of an Interactive Fiction")
   (P "当我提及interactive fiction时, 我指的是最传统的IF, 例如Zork, "
      "而不是后来的诸如80 Days这样的游戏. 换言之, "
      "游戏的一切都是通过文字的交互进行的, 这或许就是最为纯粹的游戏形式.")
   (P "即便IF看上去似乎非常简单, 但是实际上它并不缺少什么游戏的要素. "
      "例如, 你仍然需要一个世界模型. 而且, 这个世界模型可能在某些方面比"
      "一般的3D游戏更加精致和复杂. "
      )
   (H2 "The Design of a Visual Novel")

   (H2 "The Design of a Card Game")

   (H2 "The Design of a Classical JRPG")

   (H2 "The Design of an Atelier-like Game")

   (H2 "The Design of a Roguelike Game")
   ))