(in-package :symta)

(defparameter *src-depth* 0)     ; current macroexpansion depth
(defparameter *src-limit* 1000)  ; macroexpansion depth limit


(defun fn-sym? (x) ; lowercased sym?
  (when (sym? x)
    (setf x (global-to-local x))
    (let ((name (rhd (sym-path x))))
      (and (> (len name) 0) (not (upper-case-p (lhd name)))))))


(defun lambda? (x) (and (consp x) (eql (car x) 'lambda)))
(defun cl-fn? (x) (or (fn-sym? x) (lambda? x)))


(defun plain-op? (xs)
  ;; FIXME: allow user to provide his own plain ops
  (let ((x (lhd xs)))
    (or (and (= (len xs) 1) (lst? x))
        ($case x ((|+| |-| |*| |/| |%| |%%| |^| |~| |@|
                  |lst| |conc| |sconc| |abs|
                  |<<| |>>| |,| |.| |&&| |\|\|| 
                  |==| |!=| |<| |>| |<=| |>=| |not|)
                 t)))))

(defun find-curries (bs es)
  (if (or (fin? es)
          (eql (lhd es) (st |#|))
          (eql (lhd es) (st |'|)))
      (return-from find-curries es))
  (let ((po (plain-op? es)))
    ($map (fn (e)
              (let ((n ($case e (|?| 0) (|??| 1) (|???| 2))))
                (cond (n (let ((x (nth n bs)))
                           (unless x
                             (dotimes (i (+ n 1))
                               (setf x (nth i bs))
                               (unless x
                                 (setf x (unisym))
                                 (setf (nth i bs) x))))
                           x))
                      ((or po (kw? e)) (find-curries bs e))
                      (t e))))
            es)))

(defun process-curries (expr)
  (if (or (fin? expr) (= (len expr) 1))
      (return-from process-curries expr))
  (let* ((as (list nil nil nil))
         (e  (find-curries as expr)))
    (unless (car as) (return-from process-curries expr))
    (setf as (list-to-sq (remove-if #'null as)))
    (st (_fn $as $e))))

(defun rightmost-arg (e)
  (if (fin? e) e (rightmost-arg (2nd e))))

(defun process-side-effects (e)
  (if (fin? e) (return-from process-side-effects e))
  (let ((p (pos (fn-k (x) (unesc? x)) e)))
    (unless p (return-from process-side-effects e))
    (let* ((x (2nd (ind e p)))
           (e (cng e p x)))
      (st (set_l $(rightmost-arg x) $e)))))


(defun pppf (es)
  (unless es (return-from pppf nil))
  (let ((e  (lhd es))
        (es (pppf (ltl es))))
    ($case e
      (|\|>| (st (|\|| |\|>| $$es)))
      (otherwise (pre e es)))))

(defun process-ifs (e)
  (unless (eql (lhd e) (st |\|>|)) (return-from process-ifs e))
  (let* ((es (split (st |::|) (ltl e))))
    (case (len es)
      (1 (st (_br $$es n)))
      (2 (st (_br $(1st es) $(2nd es))))
      (otherwise (error "`|>`: too many `::`s")))))

(defun process-pipe (e)
  (setf e (pppf e))
  (let* ((es ($map #'process-ifs (split (st |\||) e)))
         (r (if (> (len es) 1)
                (fold (fn (a b) (suf a b)) es)
                (1st es))))
    r))

(defun process-ands (e)
  (let* ((es (split (st |&&|) e)))
    (if (> (len es) 1)
        (fold (fn (a b) (st (_and $a $b))) es)
        e)))

(defun process-ors (e)
  (let* ((es (split (st |\|\||) e)))
    (if (> (len es) 1)
        (fold (fn (a b) (st (_or $a $b))) es)
        e)))

(defun process-chain (e)
  (let* ((es (split (st |;|) e)))
    (if (> (len es) 1)
        (st (do $$es))
        e)))

(defun process-def (e)
  (if-bind ap (pos (st |->|) e)
    (st (def ($(lhd e))
             $(split (st |;|) (ltl e))))
    e))

(defun incut-keywords (as ks xs)
  (unless xs (return-from incut-keywords (conc as ks)))
  (let ((x  (lhd xs))
        (xs (ltl xs)))
    (if (kw? x)
        (setf ks (conc ks (lst (to-kw (sym-path-name (2nd x))) (3rd x))))
        (setf as (suf x as)))
    (incut-keywords as ks xs)))

(defun filter-binds (bs es)
  (let ((e (lhd es)))
    (cond ((bnd? e) (filter-binds (suf e bs) (ltl es)))
          (t        (cons bs es)))))

(defun defaulted-kw (k d)
  (st (_if (|==| $k 'unbound) $d $k)))

(defun expand-kws (ks ds body)
  (unless ks (return-from expand-kws body))
  (let ((k (lhd ks))  (ks (ltl ks))
        (d (lhd ds))  (ds (ltl ds)))
    (st ((_fn ($k) $(expand-kws ks ds body)) $d))))

(defun apply-macro (macro args)
  (funcall macro #'identity args))

;; macro unfolder
(defun expand (expr)
  (if (fin? expr) (return-from expand expr))
  (assert (< *src-depth* *src-limit*) nil "macroexpansion depth limit exceed")
  (let ((*src-depth* (+ *src-depth* 1)))
    ($list-case expr
      (|'| (x) expr) ; dont expand quoted stuff
      (set_l (p v) (st (set_l $p $(expand v))))
      (_fn (args body)
        (let ((hd (lhd args)))
          (if (lst? hd)
             (let* ((ks ($map (fn (x) (intern (sym-path-name (1st x))
                                                *cl-pkg*))
                                hd))
                    (ks2  ($map #'1st hd)) ; keywords
                    (ds   ($map #'2nd hd)) ; default values
                    (as   (conc ks (ltl args)))
                    (args (pre ks (ltl args)))
                    (*env*(env-push as *env*))
                    (ds   ($map #'defaulted-kw ks ds))
                    (body (expand-kws ks2 ds body)))
               (st (_fn $args $(expand body))))
             (let* ((*env* (env-push args *env*)))
               (st (_fn $args $(expand body)))))))
      (|:| (var val) (st (|:| $var $val)))
      (otherwise ; FIXME: allow nested head macros
       ;; FIXME: process-chain leaves no pipes
       (setf expr
             (process-ands
              (process-ors
               (process-pipe
                (process-side-effects
                 (process-chain
                  (process-curries
                   (process-def expr))))))))
       (let* ((x  (filter-binds nil expr))
              (bs (car x))
              (e  (cdr x)))
         (when bs
           (if e (setf bs (suf e bs))) ; make sure we can use parens with `:`
           (setf expr (st (do $$bs)))))

       (if-bind it (macro? (lhd expr))
         (expand (apply-macro it (ltl expr)))
         ($map #'expand (incut-keywords nil nil expr)))))))

(defun immediate? (e)
  (or (fin? e) (_fn? e) (esc? e)))

(defun cps-if (test then else k)
  (if (and (immediate? then) (immediate? else))
      (st ($k (_if $test $then $else)))
      (st (_if $test
               $(cps-main then k)
               $(cps-main else k)))))

(defparameter _k (unisym "k"))

(defun asfn? (x) (and (lst? x) (eql (lhd x) (st |asFn|))))

(defun cps-main (expr k)
  (if (fin? expr) (return-from cps-main (st ($k $expr))))
  ($list-case expr
    (_fn (args body) (st ($k (_fn $(pre _k args) $(cps-main body _k)))))
    (_if (test then else) ; FIXME: handle `if` as a function
      (if (or (fin? test) (esc? test))
          (if (sym? k) ; careful! avoid exponential duplication
              (cps-if test then else k)
              (let ((kv (unisym "k")))
                (st ((_fn ($kv) $(cps-if test then else kv)) $k))))
          (let ((truth (unisym)))
            (cps-main
             test
             (st (_fn ($truth)
                    $(if (sym? k)
                         (cps-if truth then else k)
                         (with-unisyms (kv)
                           (st ((_fn ($kv) $(cps-if truth then else kv))
                                $k))))))))))
    (|'| (x) (st ($k $expr)))
    (set_l (p v)
       (if (immediate? v)
           (progn (if (_fn? v) (setf v (2nd (cps-main v nil))))
                  (st ($k (set_l $p $v))))
           (let ((u (unisym)))
             (cps-main v (st (_fn ($u) ($k (set_l $p $u))))))))
    (|:| (dst src) (st (|:| $k $dst $src)))
    (otherwise
     (let ((h (lhd expr)))
       (when (asfn? h)
         (setf expr (st (|fc| $(2nd h) $$(ltl expr))))))
     (setf expr ($map
                 (fn (x) (or (when (and (lst? x) (eql (len x) 1))
                               (let ((x (1st x)))
                                 (if (or (and (fin? x) (not (sym? x))) (esc? x))
                                     x)))
                             x))
                 expr))
     (let* ((syms ($map (fn (x) (if (immediate? x)
                                    (if (_fn? x)
                                        (2nd (cps-main x nil))
                                        x)
                                    (unisym)))
                        expr))
            (result (ins syms 1 k)))
       ;; reverse, because of left to right order of evaluation
       ($map (fn (s a)
               (if (not (immediate? a))
                   (setf result (cps-main a (st (_fn ($s) $result))))))
             (rev syms) (rev expr))
       result))))

(defun cps (expr k)
  (cps-main expr k))

(defmacro cl-not-equ (a b) `(not (cl-equ ,a ,b)))

(defun cl-simple-type? (x)
  (or (numberp x) (symbolp x)))

(defun cl-type? (x)
  (or (numberp x)
      (and (consp x) (eql (car x) 'quote) (cl-simple-type? (cadr x)))))

(defmacro cl-equ (a b)
  (cond 
    ((eql b (deco '|y?|)) a)
    ((eql b (deco '|n?|)) `(not ,a))
    ((cl-fn? b) `(funcall ,b #'identity ,a))
    ((cl-type? b) `(eql ,a ,b))
    (t `(equ ,a ,b))))
(defmacro cl-add (x y)
  (with-gensyms (a b)
    `(let ((,a ,x) (,b ,y))
       (if (and (numberp ,a) (numberp ,b))
           (+ ,a ,b)
           (add-lists ,a ,b)))))
(defmacro cl-sub (x y)
  (with-gensyms (a b)
    `(let ((,a ,x) (,b ,y))
       (if (and (numberp ,a) (numberp ,b))
           (- ,a ,b)
           (sub-lists ,a ,b)))))
(defmacro cl-mul (x y)
  (with-gensyms (a b)
    `(let ((,a ,x) (,b ,y))
       (if (numberp ,a)
           (if (numberp ,b)
               (* ,a ,b)
               (mul-list-by-value ,b ,a))
           (if (numberp ,b)
               (mul-list-by-value ,a ,b)
               (mul-lists ,a ,b))))))
(defmacro cl-div (x y)
  (with-gensyms (a b)
    `(let ((,a ,x) (,b ,y))
       (if (numberp ,a)
           (if (numberp ,b)
               (/ ,a ,b)
               (error "can't divide scalar by a list"))
           (if (numberp ,b)
               (mul-list-by-value ,a (/ ,b))
               (mul-lists ,a ,b))))))
(defmacro cl-neg (x)
  (with-gensyms (a)
    `(let ((,a ,x))
       (if (numberp ,a)
           (- ,a)
           (neg-list ,a)))))

(defmacro cl-rem (x y)
  (with-gensyms (a b v)
    `(let ((,a ,x) (,b ,y))
       (if (numberp ,a)
           (rem ,a ,b)
           ($map (fn (,v) (rem ,v ,b)) ,a)))))

(defmacro cl-truncate (x y)
  (with-gensyms (a b v)
    `(let ((,a ,x) (,b ,y))
       (if (numberp ,a)
           (truncate ,a ,b)
           ($map (fn (,v) (truncate ,v ,b)) ,a)))))


(defmacro cl-lst (&rest xs)
  (if (<= 1 (length xs) 32)
      `(vector ,@xs)
      `(list-to-sq (list ,@xs))))


(defmacro cl-bget (i xs)
  `(ns-get ,xs ,i))

(defmacro cl-aget (i o) `(aref ,o ,i))
(defmacro cl-aset (i v o) `(progn (setf (aref ,o ,i) ,v) ,o))

(defmacro fill-hash-table (ht &body entries)
  `(progn (defparameter ,ht (make-hash-table :test 'equal))
          ,@(mapcar (fn (e) `(setf (gethash (deco ',(first e)) ,ht)
                                   ',(second e)))
                    entries)))

(fill-hash-table st-to-cl-tbl
  (|asFn|    identity)
  (|y?|      identity)
  (|n?|      not)
  (|_ind|    ind)
  (|bget|    cl-bget)
  (|aget|    cl-aget)
  (|aset|    cl-aset)
  (|len|     len)
  (|tag|     get-type)
  (|==|      cl-equ)
  (|!=|      cl-not-equ)
  (|<|       lt)
  (|>|       gt)
  (|<=|      lte)
  (|>=|      gte)
  (|pre|     pre)
  (|suf|     suf)
  (|lhd|     lhd)
  (|ltl|     ltl)
  (|rhd|     rhd)
  (|rtl|     rtl)
  (|take|    take)
  (|drop|    drop)
  (|lst|     cl-lst)
  (|_lst|    cl-lst)
  (|lst?|    lst?)
  (|fn?|     functionp)
  (|fin?|    fin?)
  (|+|       cl-add)
  (|-|       cl-sub)
  (|*|       cl-mul)
  (|/|       cl-div)
  (|%|       cl-rem)
  (|~|       cl-neg)
  (|%%|      cl-truncate)
  (|and|     logand)
  (|or|      logior)
  (|xor|     logxor)
  (|blit|    sdl::blit)
  )


(defun compile-to-cl-funcall (e)
  (let ((f  (car e))
        (k  (cadr e))
        (as (cddr e))
        (to nil))
    (cond
      ((fn-sym? f)
       (cond ((eql f (deco '|identity|)) (second e))
             ((eql f (deco '|fc|)) `(funcall ,(car as) ,k ,@(cdr as)))
             ((setf to (gethash f st-to-cl-tbl))
              (compile-to-cl-funcall `(,(cadr e) (,to ,@as))))
             (t `(funcall ,f ,k ,@as))))
      ((lambda? f) `(,f ,k ,@as))
      (t (case (length as)
           (0 `(funcall ,k ,f))
           ;; FIXME: do recursive
           (otherwise `(funcall ,k (ns-get ,f ,(car as)))))))))

(defun compile-to-cl-kw (kw) `(,kw ',(st unbound)))

(defun compile-to-cl-form (expr)
  ($list-case expr
    (_fn (args body)
       (let* ((ks (2nd args))
              (ks (if (lst? ks) ks))
              (as (if ks (st ($(lhd args) $$ks $$(drop 2 args))) args))
              (args (if ks
                        (append (sq-to-list (rm args 1))
                                (cons '&key (mapcar #'compile-to-cl-kw
                                                    (sq-to-list ks))))
                        (sq-to-list args)))
              (*env* (env-push as *env*)))
         `(lambda ,args ,(compile-to-cl body))))
    (_if (&rest xs)
         (let ((args (mapcar #'compile-to-cl xs)))
           `(if ,(first args) ,(second args) ,(third args))))
    (identity (x) (compile-to-cl x))
    (do (k &rest xs)
         (compile-to-cl-funcall
          `(,(compile-to-cl k) ; call k with last expression
            ,(compile-to-cl (car (last xs))))))
    (|'| (x) `(quote ,x))
    (|:| (k dst src)
          (let* ((_k (unisym "k")) ; don't shade current _k
                 (u  (unisym))
                 (e  (expand (st (_let (($u $src))
                                   (let (= $dst $u)
                                     cps_bottom '($k $u) )))))
                 (c  (cps e (st identity))))
            (compile-to-cl-form c)))
    (cps_esc (k x) `(funcall ,(compile-to-cl k)
                              ,(compile-to-cl (2nd x))))
    (cps_bottom (k x) `,(compile-to-cl (2nd x)))
    (set_l (p v) (if (env-sym? p)
                      `(setq ,p ,(compile-to-cl v))
                      (let ((s (local-to-global p)))
                        `(locally
                           (declare (special ,s))
                           (setq ,s ,(compile-to-cl v))))))
    (cl_esc (k c)
      (compile-to-cl-funcall `(,(compile-to-cl k)
                                ,(compile-to-cl-inline (2nd c)))))
    (name_fn (k name fn)
      (compile-to-cl-funcall
        `(,(compile-to-cl k)
          (named-fn ,(2nd name) ,@(cdr (compile-to-cl fn))))))
    (otherwise
     (compile-to-cl-funcall (mapcar #'compile-to-cl (sq-to-list expr))))))

(defun compile-to-cl-atom (e)
  (cond ((sym? e)
         (if (or (eql e nil) (eql e t) (keywordp e) (env-sym? e))
             (return-from compile-to-cl-atom e))
         (unless (symbol-package e) ; gensyms cant ref global values
           (error "Symbol references nothing: ~a" e))
         (let ((e (local-to-global e)))
           (if (boundp e)
               e
               `(locally (declare (special ,e))
                  ,e))))
        ((typed? e) (if (str? e) (sq-to-str e) `(quote ,e)))
        (t e)))

(defun compile-to-cl-inline-atom (e)
  (cond ((eql e (st |'|)) 'quote)
        ((sym? e)
         (if (or (eql e nil) (eql e t) (keywordp e) (env-sym? e))
             e
             (intern (string-upcase (sym-path-name e)) "SYMTA")))
        ((typed? e) `(quote ,e))
        (t e)))

(defun compile-to-cl-inline-form (e)
  ($list-case e
    (esc (x) x)
    (|!| (x) ($compile x))
    (otherwise (mapcar #'compile-to-cl-inline (sq-to-list e)))))

(defun compile-to-cl-inline (expr)
  (if (fin? expr)
      (compile-to-cl-inline-atom expr)
      (compile-to-cl-inline-form expr)))

(defun compile-to-cl (expr)
  (if (fin? expr)
      (compile-to-cl-atom expr)
      (compile-to-cl-form expr)))

(defun $compile (expr)
  (let ((e (expand expr)))
    (let ((c (cps e (st identity))))
      (compile-to-cl c))))

(defun %eval (expr)
  (let ((c ($compile expr)))
    (eval `(locally
             #|(declare (optimize (compilation-speed 0)
                                (speed 3)
                                (space 0)
                                ;;(safety 0)
                                (debug 0)
                                ))|#
             (muffle-compiler-note
               (muffle-compiler-warning ,c))))))

(defun $evalq (string)
  ($compile (lhd ($read string))))

(defun $eval (string &key (silent nil))
  (let* ((*read-input* string)
         (seq          *read-input*))
    (while-bind it (read-toplevel seq :single t)
      (fe (fn (x) (let ((o (%eval (parse x))))
                    (unless silent (pp o))))
          (car it))
      (setf seq (cdr it))))
  (setf *cur-pkg* *base-pkg*)
  nil)




(defmacro expanded (string)
  (let* ((*read-input* (as-list string))
         (seq          *read-input*)
         (os           nil))
    (while-bind it (read-toplevel seq :single t)
      (setf seq (cdr it))
      (let* ((expr (parse (lhd (car it))))
             (c    ($compile expr)))
        (push c os)))
    `(progn (muffle-compiler-note
              (muffle-compiler-warning
                (progn ,@os))))))

(defun $load (path)
  (let ((p (sq-to-str path)))
    (format t "Loading ~S~%" p)
    ($eval (fold-enc (load-file-bytes p)) :silent t)))
