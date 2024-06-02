#lang racket
(provide maxima.html)
(require SMathML)
(define maxima.html
  (TnTmPrelude
   #:title "理解Maxima"
   #:css "styles.css"
   (H1 "理解Maxima")
   (P "阅读Maxima的源代码时记下的一些笔记, 仅此而已.")
   (H2 "utils.lisp")
   (P (Code "while") "宏的功能恰如其名.")
   (CodeB "(defmacro while (cond &amp;rest body)
  `(do ()
       ((not ,cond))
     ,@body))")
   (P (Code "maxima-getenv") "是平台无关的获取环境变量的值的函数.")
   (CodeB "(defun maxima-getenv (envvar)
  #+gcl     (si::getenv envvar)
  #+ecl     (si::getenv envvar)
  #+allegro (system:getenv envvar)
  #+(or cmu scl) (cdr (assoc envvar ext:*environment-list* :test #'string=))
  #+sbcl    (sb-ext:posix-getenv envvar)
  #+clisp   (ext:getenv envvar)
  #+(or openmcl mcl)     (ccl::getenv envvar)
  #+lispworks (hcl:getenv envvar)
  #+abcl (ext:getenv envvar)
  )")
   (P (Code "bye") "是平台无关的退出过程.")
   (CodeB "(defun bye (&amp;optional (exit-code 0))
  (declare (ignorable exit-code))
  #+scl       (ext:quit)
  #+clisp              (ext:quit exit-code)
  #+sbcl               (sb-ext:quit :unix-status exit-code)
  #+allegro            (excl:exit exit-code :quiet t)
  #+(or mcl openmcl)   (ccl:quit exit-code)
  #+gcl                (system::quit exit-code)
  #+ecl                (si:quit exit-code)
  #+lispworks          (lispworks:quit)
  #+abcl               (cl-user::quit)
  #+gcl                (lisp::bye)
  #+cmucl
  (handler-case (ext:quit nil exit-code)
    ;; Only the most recent versions of cmucl support an exit code.
    ;; If it doesn't, we get a program error (wrong number of args),
    ;; so catch that and just call quit without the arg.
    (program-error ()
      (ext:quit)))
  )")
   (P (Code "map2c") "的参数" (Code "f")
      "应该是一个接受两个参数的函数, 而" (Code "l")
      "应该是一个长度为偶数的列表. 其与" (Code "mapcar")
      "有类似的地方, 但是一次连续从列表中吃下两个元素, 并且最终"
      "积累结果的列表顺序是颠倒的. 对于性质列表上的迭代有用.")
   (CodeB "(defun map2c (f l)
  (do ((llt l (cddr llt)) (lans))
      ((null llt) lans)
    (push (funcall f (car llt) (cadr llt)) lans)))")
   (CodeB "> (map2c #'+ '(1 2 3 4 5 6))
(11 7 3)")
   (P (Code "andmapcar") "类似于" (Code "mapcar")
      ", 但是若从左到右依次应用" (Code "f") "途中返回" (Code "nil")
      ", 则整个函数立即返回" (Code "nil") ". (对于我这个Schemer而言, "
      "大概需要注意不要将其与许多Scheme实现提供的" (Code "andmap") "混淆.)")
   (CodeB "(defun andmapcar (f l &amp;aux d answer)
  (do ((l l (cdr l)))
      ((null l) (nreverse answer))
    (setq d (funcall f (car l)))
    (if d (push d answer) (return nil))))")
   (P (Code "xor") "即不可兼或.")
   (CodeB "(defun xor (a b)
  (or (and (not a) b) (and (not b) a)))")
   (P (Code "among") "在功能上类似于" (Code "memq")
      ", 但是其在整个列表结构中寻找对象, 而不是一个扁平的列表中. "
      "(注意, " (Code "memq") "不是Common Lisp标准中的过程, 但是Scheme标准和"
      "Emacs Lisp包含这个过程, 也有Common Lisp实现包含这个过程.)")
   (CodeB "(defun among (x l)
  (cond ((null l) nil)
        ((atom l) (eq x l))
        (t (or (among x (car l)) (among x (cdr l))))))")
   (P (Code "amongl") "类似于" (Code "among")
      ", 但是此时" (Code "x") "是一列需要寻找的对象, 找到其中一个即可.")
   (CodeB "(defun amongl (x l) 
  (cond ((null l) nil)
        ((atom l) (member l x :test #'eq))
        (t (or (amongl x (car l)) (amongl x (cdr l))))))")
   (P (Code "subtree-p") "判断一个树是否是另一个的子树. 默认情况下使用"
      (Code "eql") "作为相等谓词, 但用户也可提供自己的谓词.")
   (CodeB "(defun subtree-p (branch tree &amp;key (test #'eql))
  (or (funcall test branch tree)
      (and (not (atom tree))
           (member branch tree
                   :test (lambda (x y) (subtree-p x y :test test))))))")
   (P (Code "dot2l") "将关联列表转换为性质列表.")
   (CodeB "(defun dot2l (l)
  (cond ((null l) nil)
        (t (list* (caar l) (cdar l) (dot2l (cdr l))))))")
   (CodeB "> (dot2l '((a . b) (c . d)))
(A B C D)")
   (P (Code "cput") "类似于" (Code "putprop") ", 但是读者需要注意一下, Maxima的"
      (Code "putprop") "里的参数顺序和一般情况不太一样. 另外就是" (Code "putprop")
      "并不在Common Lisp标准之中, 但是有的Common Lisp实现会提供. 如果不提供的话, "
      "可以使用" (Code "setf") "和" (Code "get") "达成同样的目的. 其他Lisp方言及其实现"
      "有的也提供" (Code "putprop") "或者类似的函数. " (Code "cput") "和"
      (Code "putprop") "的区别在于, 若" (Code "val") "为" (Code "nil")
      ", 则从与符号相关联的性质列表中删去这个性质, 可能这会使其看上去更紧凑. 至于若"
      (Code "val") "为" (Code "nil") "时返回" (Code "nil") ", 是因为这可以让人区别"
      "两种不同的情况 (当然, 这也是为了与" (Code "putprop") "保持一致), 而"
      (Code "zl-remprop") "是根据性质的有无返回" (Code "t") "或者" (Code "nil") ".")
   (CodeB "(defun cput (bas val sel)
  (cond ((null val)
         (zl-remprop bas sel)
         nil)
        (t
         (putprop bas val sel))))")
   (H2 "sloop.lisp")
   (P "William Schelter (Maxima最终能够成功开源离不开他的努力) 编写的迭代设施, 但现已被"
      "Common Lisp提供的" (Code "loop") "宏取代.")
   (CodeB "(defmacro sloop (&amp;rest body)
  (warn (intl:gettext &quot;Using deprecated macro 'sloop'. Use 'loop' instead.&quot;))
  `(loop ,@body))")
   (H2 "mutils.lisp")
   (P (Code "$assoc") "类似于" (Code "assoc") ", "
      )
   (CodeB "(defmfun $assoc (key ielist &amp;optional default)
  (let ((elist (if (listp ielist)
                   (margs ielist)
                   (merror 
                     (intl:gettext
                       &quot;assoc: second argument must be a nonatomic expression; found: ~:M&quot;) 
                     ielist))))
    (if (every #'(lambda (x) (and (listp x) (= 3 (length x)))) elist)
        (let ((found (find key elist :test #'alike1 :key #'second)))
          (if found (third found) default))
        (merror
          (intl:gettext
            &quot;assoc: every argument must be an expression of two parts; found: ~:M&quot;)
          ielist))))")
   (P (Code "assol") "类似于" (Code "assoc") ", 但是使用" (Code "alike1") "作为相等谓词. "
      (Code "assolike") "不是返回序对, 而是直接返回对应的值.")
   (CodeB "(defun assol (item alist)
  (dolist (pair alist)
    (if (alike1 item (car pair)) (return pair))))
(defun assolike (item alist) 
  (cdr (assol item alist)))")
   (P (Code "memalike") "类似于" (Code "member") ", 但是使用" (Code "alike1")
      "比较相等性.")
   (CodeB "(defun memalike (x l)
  (do ((l l (cdr l)))
      ((null l))
    (when (alike1 x (car l)) (return l))))")
   (P (Code "find-duplicate") "寻找列表中的重复元素, 默认使用" (Code "eql")
      "进行比较. 如果提供了" (Code "key") "函数, 那就是比较键值. "
      (Code "find-duplicate") "会返回第一个发现的重复元素, 更准确地说, 是"
      "自左往右依次数, 直到碰到重复的情况.")
   (CodeB "(defun find-duplicate (list &amp;key (test #'eql) key)
  (declare (optimize (speed 3)))
  (declare (type (or function null) key)
           (type function test))
  (let ((seen nil))
    (dolist (e list)
      (let ((i (if key (funcall key e) e)))
        (when (member i seen :test test)
          (return-from find-duplicate e))
        (push i seen)))))")
   (P ""
      )
   (CodeB "(defmfun $gensym (&amp;optional x)
  (typecase x
    (null
     (intern (symbol-name (gensym &quot;$G&quot;)) :maxima))
    (string
     (intern
       (symbol-name (gensym (format nil &quot;$~a&quot; (maybe-invert-string-case x))))
       :maxima))
    ((integer 0)
     (let ((*gensym-counter* x))
       (intern (symbol-name (gensym &quot;$G&quot;)) :maxima)))
    (t
     (merror
       (intl:gettext
         &quot;gensym: Argument must be a nonnegative integer or a string. Found: ~M&quot;) x))))")
   (H2 "getopt.lisp")
   (P ""
      )
   (CodeB "(defun is-short-option (arg)
  (and (>= (length arg) 2)
       (char= #\\- (schar arg 0))
       (char/= #\\- (schar arg 1))))
(defun is-option-terminator (arg)
  (and (= 2 (length arg))
       (char= #\\- (schar arg 0))
       (char= #\\- (schar arg 1))))
(defun is-long-option (arg)
  (and (> (length arg) 2)
       (char= #\\- (schar arg 0))
       (char= #\\- (schar arg 1))
       (char/= #\\- (schar arg 2))))")
   (P ""
      )
   (CodeB "(defun analyze-arg (arg)
  &quot;Analyzes an argument. Returns option-type,base-name,argument&quot;
  (let* ((option-type (cond ((is-short-option arg) :short)
                            ((is-long-option arg) :long)
                            (t :arg))))
    (if (or (eq option-type :short) (eq option-type :long))
        (multiple-value-bind (base arg) (decompose-arg arg option-type)
          (values option-type base arg))
        (values :arg arg nil))))")
   (H2 "maxmac.lisp")
   (P "类似于" (Code "PUSH") ", 但是列表的另一端操作. " (Code "ncons")
      "实际上来源于MACLISP, " (Code "(ncons x)") "就相当于"
      (Code "(list x)") "或者" (Code "(cons x nil)")
      ". 一般的Common Lisp实现不会提供" (Code "ncons") "这个过程.")
   (CodeB "(defmacro tuchus (list object)
  `(setf ,list (nconc ,list (ncons ,object))))")
   (CodeB "> (defvar l0 '())
L0
> (tuchus l0 'foo)
(FOO)
> (tuchus l0 'bar)
(FOO BAR)
> (tuchus l0 'baz)
(FOO BAR BAZ)")
   (H2 "mlisp.lisp")
   (P ""
      )
   (CodeB "(defun margs (form)
  (if (eq (caar form) 'mqapply)
      (cddr form)
      (cdr form)))")
   ))