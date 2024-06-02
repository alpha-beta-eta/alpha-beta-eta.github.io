#lang racket
(provide scheme_cg.html)
(require SMathML)
(define scheme_cg.html
  (TnTmPrelude
   #:title "把东西粘在一起&mdash;&mdash;实时CG内容生产中的Scheme"
   #:css "styles.css"
   (H1. "把东西粘在一起&mdash;&mdash;实时CG内容生产中的Scheme")
   (H2. "引入")
   (P "对于Lisp程序员来说, Lisp用于交互式计算机图形学并不是新鲜事. "
      "实际上, Lisp曾经是做图形学的" (Em "首选(the)")
      "语言. 原因有很多, 但我们认为其中一个原因是, "
      "处理计算机图形学中的数据结构基本上就是动态地调整有向图, "
      "而Lisp在运行时处理图结构方面非常出色.")
   (H2. "舞者项目")
   (H2. "脚本引擎")
   (H2. "生产过程")
   (H2. "Scheme的角色")
   (P "在整个制作过程中, 我们在各个地方使用了Scheme, 但可以总结为以下几个主题.")
   (H3. "作为文件格式的Scheme")
   (CodeB "(define *shot-lengths* '((3  84) (4  402) (4a 48) (5  36) (6  74)
                         (7  113) (8  72) (9  120) (10 217) (12 120)
                         (13 368) (15 86) (16 128) (18 275) (18a 72)
                         ...))

(define *shot-data*
  (let ((frame 1))
    (map (lambda (len)
           (let ((result (list (car len) frame (+ frame (fps-frame len) -1))))
             (set! frame (+ frame (fps-frame len)))
             result))
         *shot-lengths*)))

(define (get-motions shots file-format-string)
  (define (read-channel file)
    (sqmo-channel-read (sq-complete-filename file)))
  (map (lambda (shot-data)
         (if (memq shot-data shots)
             (create-motion-channel
              (read-channel (format #f file-format-string
                                    (shot-number shot-data)))
              (first-frame shot-data))
             #f))
       *shot-data*))")
   (H3. "作为debug工具的Scheme")
   (H3. "用Scheme进行快速原型制作")
   (H2. "经验")
   (H2. "未来")
   ))