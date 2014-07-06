(in-package :symta)



;; FIXME: here every function with funargs conflicts with continuations
;;        and can produce stack overflow


(defmacro fc (f &rest as) `(funcall (deco ,f) #'identity ,@as))

(defun update-parts (tag new parts)
  (if (eql tag (st _))
      (return-from update-parts
        (suf (lst tag new) (if (and parts (eql (lhd (rhd parts)) (st _)))
                               (rtl parts)
                               parts))))
  (unless parts (return-from update-parts (st (($tag $new)))))
  (let* ((hd (lhd parts))
         (tl (ltl parts))
         (o  (lhd hd)))
    (cond ((eql o tag) (pre (lst tag new) tl))
          ((eql o (st _)) (pre (lst tag new) (lst hd)))
          (t (pre hd (update-parts tag new tl))))))

(defmacro fc-default (sym &rest as)
  (let ((pdsym (intern (symbol-name sym) "predefs")))
    `(,pdsym #'identity ,@as)))

(defpackage :|predefs|)

(defmacro fun (name args &body body)
  (if (stringp name) (setf name (intern name)))
  (let* ((sym (st-sym name))
         (exp (sym-path-name sym))
         (as  (mapcar (fn (_) `',(unisym)) args))
         (val (gensym))
         (glb (local-to-global sym))
         (pdsym (intern (symbol-name sym) "predefs")))
    `(progn
       (let ((,val (eval '(named-fn ,sym (k ,@args)
                           (funcall k (block ,name ,@body))))))
         (defparameter ,glb ,val)
         (setf (symbol-function ',pdsym) ,val)
         (set-prop ',sym "parts" (update-parts
                                  (st _)
                                  (lst (lst ,@as (st ->)
                                            (st cl)
                                            (st fc-default)
                                            (lst (st esc) ',sym)
                                            ,@as))
                                  (get-prop ',sym "parts")))
         (pkg-relate "exports" ,exp)
         nil))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun macro-binds (p l body)
    (let ((v  (car p))
          (p  (cdr p))
          (tl (gensym)))
      (cond ((not v) body)
            ((eql v '&rest)
             (unless p (error "macro: no parameter after &rest"))
             `(let ((,(car p) ,l))  (declare (ignorable ,(car p))) ,body))
            (t `(let ((,v (lhd ,l)) (,tl (ltl ,l)))
                  (declare (ignorable ,tl) (ignorable ,v))
                  ,(macro-binds p tl body)))))))

(defmacro macro (name args &body body)
  (if (stringp name) (setf name (intern name)))
  (let* ((sym (st-sym name))
         (exp (sym-path-name sym))
         (val (gensym))
         (l (gensym)))
    `(progn
       (let ((,val (eval '(named-fn ,sym (k ,l)
                           (funcall k ,(macro-binds args l
                                                    `(block ,name ,@body)))))))
         (set-prop ',sym "macro" ,val)
         (pkg-relate "exports" ,exp)))))

(defmacro var (name val)
  (let* ((sym (st-sym name :upcase t))
         (exp (sym-path-name sym)))
    `(progn (defparameter ,(local-to-global sym) ,val)
            (pkg-relate "exports" ,exp))))


(var |PI| pi)

(fun gensym (&key (|name| "G")) (unisym |name|))

(fun package (name)
  (let ((p (str-path name)))
    (setf *cur-pkg* p)
    (unless (fnd p *pkgs*) (setf *pkgs* (suf p *pkgs*)))
    (unless (equ p *base-pkg*)
      (pkg-relate "imports" ";st" :breaked t))))

(fun import (name) (pkg-relate "imports" name :breaked t))
(fun export (name) (pkg-relate "exports" name))


(defun my-abort () (abort))
(fun abort () (my-abort))


(defun $read-from-string (input) (lhd ($read input)))
(fun st (input) ($read-from-string input))
(fun read () ($read-from-string (read-line)))

(fun eval (e &key (|optimize| nil))
  (if |optimize|
      (let ((sb-ext:*evaluator-mode* :interpret))
        (%eval e))
      (%eval e)))

(fun |symValue| (x) (symbol-value (symbol-value x)))

(defun expand-set1 (d add-s next p)
  (cond
    ((or (msg? p) (coma? p))
     (let* ((f (1st p))
            (q (2nd p))
            (m (3rd p))
            (u (unisym))
            (next (fn (x)
                    (funcall add-s (lst f x m))
                    (if (= d 0)
                        (funcall next x)
                        (st (_let (($u ($f $x $m)))
                              $(funcall next u)))))))
       (expand-set1 (+ d 1) add-s next q)))
    (t (funcall next p))))


(defun expand-set2 (us v)
  (unless us (return-from expand-set2 v))
  (let* ((u  (car us))
         (us (cdr us))
         (p  (2nd u))
         (i  (3rd u)))
    (cond
      ((msg? u)
       (let ((v (cond
                  ((sym? i) (setf i (sym-path-name i))
                            (st (bset $i $v $p)))
                  (t (st (bset $i $v $p))))))
         (expand-set2 us v)))
      ((coma? u) (expand-set2 us (st (cng $i $v $p)))))))

(defun expand-set (p v)
  (let ((us))
    (expand-set1 0
                 (fn (u) (push u us))
                 (fn (_)
                   (if (lst? v)
                       (with-unisyms (g)
                         (st (_let (($g $v))
                               $(expand-set2 us g))))
                       (expand-set2 us v)))
                 p)))

(macro set (p v) (if (sym? p) v (expand-set p v)))
(macro |=:| (p v) (st (set (! $p) $v)))
(macro tee (&rest xs)
  (with-unisyms (g)
    (st (_let (($g $(rhd xs)))
              (do $$($map (fn (p) (st (|=:| $p $g))) (rtl xs))
               $g)))))


(macro _let (binds expr)
  (unless (lst? binds) (error "_let: invalid bindings list"))
  (let ((vars   ($map #'1st binds))
        (values ($map #'2nd binds)))
    (st ((_fn $vars $expr) $$values))))

(fun del (i l) (rm l i))
(fun cng (i v l) (cng l i v))
(fun ins (i v l) (ins l i v))

(fun lcng (i v l) (lcng l i v))
(fun lins (i v l) (lins l i v))


(fun me (e &rest depth) ; macroexpand
  (setf depth (if depth (car depth) 1))
  (expand e))


(macro fn (args body)
  (lambs-to-multimethod (st (($$args -> $$body)))))

(fun fc (f &rest args) (apply f (sq-to-list args)))

(macro |namedFn| (name args body)
  (lambs-to-multimethod (st (($$args -> $$body)))
                        :name name))

(macro blk (&rest xs) (lambs-to-multimethod xs))
(macro |namedBlk| (&rest xs)
  (let* ((x    (lhd xs))
         (name (lhd x))
         (xs   (pre (ltl x) (ltl xs))))
    (lambs-to-multimethod xs :name name)))

(fun |asFn| (x) x)


(defun parse-let (rs xs)
  (unless xs (return-from parse-let (cons rs xs)))
  (let ((x (lhd xs)))
    (if (kw? x)
        (parse-let (suf (ltl x) rs) (ltl xs))
        (cons rs xs))))


(defun simple-arglist? (ys xs)
  (unless xs (return-from simple-arglist? t))
  (let ((x (lhd xs)))
    (and (sym? x)
         (not (find x ys))
         (simple-arglist? (cons x ys) (ltl xs)))))


;; destructuring let
(macro let (&rest expr)
  (let* ((x     (parse-let nil expr))
         (binds (car x))
         (body  (cdr x))
         (vars  ($map #'1st binds))
         (values ($map #'2nd binds)))
    (cond
      ((not vars) body)
      ((simple-arglist? nil vars) (st ((_fn $vars $body) $$values)))
      (t (let ((ws (dup (len vars) (st _))))
           (st ((blk ($$vars -> $$body)
                     ($$ws -> error "let: destructuring failed"))
                $$values)))))))

(macro cl (&rest xs) (st (cl_esc '$xs)))
(fun |,| (s i) (ind s i))

(macro |::| (a b) (lst a b))

(defun count-incuts (l n)
  (unless l (return-from count-incuts nil))
  (let* ((hd (lhd l))
         (tl (ltl l))
         (ics (count-incuts tl (+ n 1))))
    (if (incut? hd) (setf ics (pre (lst n (2nd hd)) ics)))
    ics))


(defun filter-incuts (l r o)
  (unless r
    (if l (setf o (suf (st (_lst $$l)) o)))
    (return-from filter-incuts o))
  (let* ((hd (lhd r))
         (r  (ltl r)))
    (if (incut? hd)
        (progn (if l (setf o (suf (st (_lst $$l)) o)))
               (filter-incuts nil r (suf (2nd hd) o)))
        (filter-incuts (suf hd l) r o))))

;; FIXME: decompose.
(defun lst-preprocess (ys ks xs)
  (unless xs (return-from lst-preprocess (cons ys ks)))
  (let ((x  (lhd xs))
        (xs (ltl xs)))
    (cond  ((rng? x) (setf x (st (|@| (_rng $$(ltl x))))))
           ((replica? x) (setf x (st (|@| (_replica $$(ltl x))))))
           ((kw? x) (return-from lst-preprocess
                      (lst-preprocess ys (suf (ltl x) ks) xs))))
    (lst-preprocess (suf x ys) ks xs)))

(defun lst-apply-ks (ks)
  (when ks
    (let* ((k  (lhd ks))
           (a  (1st k))
           (b  (2nd k))
           (a  (if (sym? a) (sym-path-name a) a))
           (ks (ltl ks)))
      (st (|\|| bset $a $b $$(lst-apply-ks ks))))))

(macro lst (&rest args)
  (let* ((tmp  (lst-preprocess nil nil args))
         (args (car tmp))
         (ks   (cdr tmp))
         (ics  (count-incuts args 1))
         (n    (len ics))
         (o    (cond
                 ((null args) nil)
                 ((= n 0) (st (_lst $$args)))
                 ((= n 1)
                  (let* ((ic (lhd ics))
                         (p  (1st ic))
                         (v  (2nd ic))
                         (l  (take (- p 1) args))
                         (ll (len l))
                         (r  (drop p args))
                         (lr (len r)))
                    (cond ((and (= ll 0) (= lr 0))
                           (cond ((and (lst? v)
                                       (or (eql (lhd v) (st _rng))
                                           (eql (lhd v) (st _replica))
                                           (eql (lhd v) (st |!|))))
                                      v)
                                 ((str? v) (st (quote $(as-list v))))
                                 (t        (st (|_lifyStr| $v)))))
                          ((and (= ll 1) (= lr 0)) (st (pre $(lhd l) $v)))
                          ((and (= ll 0) (= lr 1)) (st (suf $(lhd r) $v)))
                          (t (st (conc $$(if l (st ((_lst $$l))))
                                       $v
                                       $$(if r (st ((_lst $$r))))))))))
                 (t (st (conc $$(filter-incuts nil args nil)))))))
    (if ks (setf o (st ($o $$(lst-apply-ks ks)))))
    o))

(macro |.| (a b)
  (if (gensym? b) (setf b (st ($b))))
  (st ($a $(if (sym? b) (sym-path-name b) b))))

(macro |,| (a b)
  (if (or (fn-sym? b) (blk? b))
      (st ($b $a))
      (st (_ind $a $b))))

(fun |_ind| (a b) (ind a b))

(fun |_lifyStr| (x)
  (if (stringp x) (as-list x) x))

(macro def (name defs)
  ;; FIXME: do more robust syntax checking and provide source locations
  (setf name (1st name))
  (unless name (error "`->` missing function name from definition"))
  (let ((tag (st _)) ; `_` is default tag
        (fun-flag nil)
        (mac-flag nil)
        (exp-flag nil)
        (parts nil))
    (format t "Compiling: ~a~%" (sq-to-str (prn name)))
    (while (bnd? name)
      (let ((f (sym-path-name (2nd name))))
        (cond ((equal f "f") (setf fun-flag t)) ; function
              ((equal f "m") (setf mac-flag t)) ; macro
              ((equal f "e") (setf exp-flag t)) ; export
              (t (error "Unsupported type-flag: ~a" f))
              ))
      (setf name (3rd name)))
    (when (msg? name)
      (setf tag (2nd name))
      (setf name (3rd name)))
    (cond
     ((fn-sym? name)
      (cond
        (mac-flag
         ;; FIXME: default handler should raise syntax error!
         (setf defs ($map (fn (d) (st ((lst $$(1st d)) -> \# $(2nd d))))
                          ($map (fn (d)
                                  (let* ((s (split (st ->) d)))
                                    (lst (lhd s) (st (do $$(ltl s))))))
                                defs)))
         (setf parts nil)) ; FIXME: allow multipart macros
        (t (setf parts (get-prop name "parts"))))
      (let* ((parts (update-parts tag defs parts))
             (lambs (fold #'conc ($map #'2nd parts)))
             (mm    (lambs-to-multimethod lambs :clean t :name name))
             (r     (cond (mac-flag (st (set_prop (|'| $name) "macro" $mm)))
                          (t (set-prop name "parts" parts)
                             (st (do (|=:| $name $mm) n))))))
        (if exp-flag (pkg-relate "exports" (sym-path-name name)))
        r))
     (t (setf defs ($map (fn (d) (split (st ->) d)) defs))
        (cond
          ((and (= (len defs) 1) (not (1st (1st defs))))
           (st (|=:| $name $(2nd (1st defs)))))
          (t (setf defs
              ($map (fn (d)
                      (let ((p (fold (fn (a b) (st (|.| $a ($b))))
                                     (st ($name $$(1st d))))))
                        (st (|=:| $p $(2nd d)))))
                    defs))
             (st (do (|=:| $name n) $$defs))
             )
          )))))

(defun expand-cnd (xs)
  (unless xs (return-from expand-cnd nil))
  (let* ((x  (lhd xs))
         (xs (ltl xs))
         (test    (1st x))
         (handler (2nd x)))
    (st (_if $test $handler $(expand-cnd xs)))))

(macro cnd (cases)
  (setf cases ($map (fn (x)
                      (let ((x (split (st ->) x)))
                        (case (len x)
                          (1 (error "cnd: invalid {}"))
                          (2 x)
                          (otherwise (st ($(lhd x) (do $$(ltl x))))))))
                    (ltl cases)))
  (expand-cnd cases))

(macro |_br| (then else if)
  (st (_let ((|It| $if)) (_if |It| $then $else))))

(macro |_and| (a b)
  (with-unisyms (x)
    (st (_let (($x $a)) (_if $x $b n)))))

(macro |_or| (a b)
  (with-unisyms (x)
    (st (_let (($x $a)) (_if $x $x $b)))))

(var |identity| #'identity)
(var _ (fn (k o) (funcall k t)))

;; call-with-current-continutation
(var |cont|
     (fn (k f)
       (funcall f k (fn (a b)
                      (funcall k b))))) ; switch a to k

(defun unhandled-exception (k)
  (declare (ignorable k))
  (push #'unhandled-exception restarts) ; it was poped by fail
  (error "fail: called outside of try"))

(defparameter restarts (list #'unhandled-exception))

(var |pick|
  (fn (k choices)
    (if choices
      (progn (push (fn (a)
                     (funcall (deco |pick|) k (ltl choices)))
                   restarts)
             (funcall k (lhd choices)))
      (funcall (pop restarts) k))))

(var |bt| (fn (k) (funcall (pop restarts) k)))

(var |mark| (fn (k)
              (push (deco |bt|) restarts)
              (funcall k nil)))

(var |prune|
  (fn (k)
    (let ((hd (car restarts))
          (tl (cdr restarts)))
      (if (eql hd #'unhandled-exception)
          (error "prune: you should set mark before prune"))
      (setf restarts tl)
      (if (eql hd (deco |bt|))
          (funcall k nil)
          (funcall (deco |prune|) k)))))

(var |_try|
  (fn (k form fail)
    (let ((restarts (cons (fn (a)
                           (funcall fail k))
                          restarts)))
      (funcall form k))))

(macro try (default &rest form)
  (st (_try (_fn () $form) (_fn () $default))))


#|
(var |**|
  (fn (k f g)
    (funcall k
             (fn (k x &rest args)
               (funcall f k
                        (if args
                            (apply g #'identity x args)
                            (funcall g #'identity x)))))))
|#

(fun set_prop (s p v) (progn (set-prop s p v) v))

(defun sq-to-array (xs)
  (make-array (len xs) :initial-contents (sq-to-list xs)))

(defun add-lists (a b)
  (cond ((null a) b)
        ((null b) a)
        (t (unless (arrayp a) (setf a (sq-to-array a)))
           (unless (arrayp b) (setf b (sq-to-array b)))
           (let ((r (copy-seq a)))
             (loop as i below (length b)
                do (setf (aref r i) (+ (aref r i) (aref b i))))
             r))))

(defun neg-list (xs)
  (when xs
    (let ((r (make-array (len xs))))
      (loop as i below (length r)
         do (setf (aref r i) (- (ind xs i))))
      r)))

(defun sub-lists (a b)
  (cond ((null a) (neg-list b))
        ((null b) a)
        (t (unless (arrayp a) (setf a (sq-to-array a)))
           (unless (arrayp b) (setf b (sq-to-array b)))
           (let ((r (copy-seq a)))
             (loop as i below (length b)
                do (setf (aref r i) (- (aref r i) (aref b i))))
             r))))

(defun mul-lists (a b)
  (let ((r 0))
    (dotimes (i (len a))
      (incf r (* (ind a i) (ind b i))))
    r))

(defun mul-list-by-value (l v)
  ($map (fn (x) (fc |*| v x)) l))

(fun |+| (a b) (if (and (numberp a) (numberp b))
                   (+ a b)
                   (add-lists a b)))
(fun |-| (a b) (if (and (numberp a) (numberp b))
                   (- a b)
                   (sub-lists a b)))
(fun |*| (a b)
  (if (numberp a)
      (if (numberp b)
          (* a b)
          (mul-list-by-value b a))
      (if (numberp b)
          (mul-list-by-value a b)
          (mul-lists a b))))
(fun |/| (a b)
  (if (numberp a)
      (if (numberp b)
          (/ a b)
          (error "`/` can't divide scalar y a list"))
      (if (lst? a)
          (if (numberp b)
              (mul-list-by-value a (/ b))
              (error "`/` can't divide by a list"))
          (error "`/` can't divide ~a" (get-type a)))))
(fun |%| (a b)
  (if (numberp a)
      (rem a b)
      ($map (fn (v) (rem v b)) a)))
      
(fun |%%| (a b)
  (if (numberp a)
      (truncate a b)
      ($map (fn (v) (truncate v b)) a)))

(fun |^| (a b)
  (if (numberp a)
      (expt a b)
      (expt (mul-lists a a) (- b 1))))

(fun |~| (o)
  (if (numberp o)
      (- o)
      (neg-list o)))


(defparameter *sqrt-table* (make-array (expt 2 16) :element-type 'single-float))
(progn (loop as i below (length *sqrt-table*)
            do (setf (aref *sqrt-table* i) (sqrt i))))
(fun |sqrt| (x)
  (typecase x
      ((integer 0 65535) (aref *sqrt-table* x))
      (otherwise (sqrt x))))


;; bitwise operators
(fun |<<| (a b) (ash a b))
(fun |>>| (a b) (ash a (- b)))
(fun xor (a b) (logxor a b))
(fun and (a b) (logand a b))
(fun or (a b) (logior a b))


(fun |ptrEq| (a b) (eql a b))
(fun |==| (a b) (equ a b))
(fun |!=| (a b) (not (equ a b)))
(fun |<|  (a b) (lt a b))
(fun |>| (a b) (gt a b))
(fun |<=| (a b) (lte a b))
(fun |>=| (a b) (gte a b))

(fun len (o) (len o))
(fun conc (&rest args) (conc-list args))


(macro sconc (&rest args) (st (|asStr| (conc $$args))))
(fun sconc (a b) (if (and (stringp a) (stringp b))
                     (concatenate 'string a b)
                     (sq-to-str (conc a b))))

(macro _slst (&rest args)
  (st (|asStr| (conc $$($map (fn (x)
                               (if (str? x) x (st (aest $x))))
                             args)))))

(fun lst (&rest args) (list-to-sq args))
(fun _lst (&rest args) (list-to-sq args))


(fun pre (e s) (pre e s))
(fun suf (e s) (suf e s))
(fun lhd (s) (lhd s))
(fun ltl (s) (ltl s))
(fun rhd (s) (rhd s))
(fun rtl (s) (rtl s))
(fun take (n s) (take n s))
(fun drop (n s) (drop n s))

(fun cut (s e l)
  (incf e)
  (let ((n (len l))
        (s-pad nil)
        (e-pad nil))
    (when (< e s) (rotatef s e))
    (when (< s 0)
      (setf s-pad (- s))
      (setf s 0))
    (when (> e n)
      (setf e-pad (- e n))
      (setf e n))
    (let ((r (cut s e l)))
      (when s-pad (setf r (conc (dup s-pad nil) r)))
      (when e-pad (setf r (conc r (dup e-pad nil))))
      r)))

(fun pos (p xs) (pos p xs))
(fun fnd (p xs) (fnd p xs))
(fun |strip1| (p xs) (if-bind i (pos p xs)
                         (rm xs i)
                         xs))

;; Example: "4C 49 53 50" | split \Space | map (asBase 16 ?) | map asChr
(fun split (sep seq) (split sep seq))
(fun _seq (hd tl) (make-gs :head hd :tail tl))
(macro seq (hd tl) (st (_seq $hd (_fn () $tl))))

(fun map (fun seq &rest seqs)
  ;; FIXME: this function conflicts with continuations
  ;;        and could produce stack overflow
  (unless (functionp fun)
    (let ((x fun))
      (setf fun (fn (k i) (funcall k (ns-get x i))))))
  (unless seqs (return-from map ($map1 (fn (x) (funcall fun #'identity x))
                                       seq)))
  (let (r)
    (push seq seqs)
    (while (not (some #'null seqs))
      (setf r (suf (apply fun #'identity (mapcar #'lhd seqs)) r))
      (setf seqs (mapcar #'ltl seqs)))
    r))

(fun fe (fun seq &rest seqs) ; for each
  (apply (deco |map|) #'identity fun seq seqs)
  nil)

(fun fold (fun seq) (if seq (fold (fn (x y) (funcall fun #'identity x y)) seq)))

(fun all (p s) (all p s))
(fun any (p s) (any p s))
(fun fst (p s) (fst p s))

(fun |asStr| (o)
  (cond ((str? o) o)
        ((characterp o) (string o))
        ((sym? o) (sym-path-name o))
        ((null o) "")
        ((lst? o) (sq-to-str o))
        (t        (prn o))))
(fun |asSym| (o) ($intern (sq-to-str o)))
(fun |asList| (o) (as-list o))

(fun |asBits| (n)
  (let (r)
    (while (> n 0)
      (push (logand n 1) r)
      (setf n (ash n -1)))
    (list-to-sq r)))

(fun aget (i o) (aref o i))
(fun aset (i v o) (setf (aref o i) v) o)
(fun |Vector| (n) (make-array n :initial-element nil))


(fun |makeBytes| (n) (make-array n :element-type '(unsigned-byte 8)))
(fun |asBytes| (o)
  (if (typep o '(SIMPLE-ARRAY (UNSIGNED-BYTE 8)))
      o
      (as-byte-array o)))

(fun |makeInts| (n) (make-array n :element-type 'fixnum))
(fun |asInts| (o)
  (if (typep o '(SIMPLE-ARRAY fixnum))
      o
      (as-int-array o)))

(fun |makeFlts| (n) (make-array n :element-type 'single-float))
(fun |asFlts| (o)
  (if (typep o '(SIMPLE-ARRAY fixnum))
      o
      (as-float-array o)))

(defun as-byte-array (o)
  (if (gs? o) (setf o (lify o)))
  (make-array (len o)
              :element-type '(unsigned-byte 8)
              :initial-contents  (sq-to-list o)))

(defun as-int-array (o)
  (if (gs? o) (setf o (lify o)))
  (make-array (len o)
              :element-type 'fixnum
              :initial-contents  (sq-to-list o)))

(defun as-float-array (o)
  (if (gs? o) (setf o (lify o)))
  (make-array (len o)
              :element-type 'single-float
              :initial-contents  (sq-to-list o)))

(fun |asBase| (base o)
  (when (lst? o)
    (let ((r (make-array (len o))))
      (loop as i below (length r)
         do (setf (aref r i) (fc |asBase| base (ind o i))))
      (return-from |asBase| r)))
  (parse-integer (sq-to-str o) :radix base :junk-allowed t))

(fun |asChr| (o) (code-char o))
(fun |asFlt| (o) (coerce o 'float))
(fun |asInt| (o)
  (cond ((numberp o) (truncate o))
        ((characterp o) (char-code o))
        ((lst? o) ($map (fn (x) (fc |asInt| x)) o))
        (t (fc |asBase| 10 o))))
(fun |asNum| (o) (cond ((num? o) o)
                       ((str? o) (let ((x (lhd (lhd ($read o)))))
                                   (if (num? x) x 0)))
                       (t 0)))
(fun |asFract| (o) (multiple-value-bind (i f) (truncate o) 
                     (declare (ignorable i))
                     f))

(fun ceil (o) (ceiling o))
(fun floor (o) (floor o))
(fun round (o) (round o))


(fun |asBin| (o) (format nil "~b" o))
(fun |asOct| (o) (format nil "~o" o))
(fun |asDec| (o) (format nil "~a" o))
(fun |asHex| (o) (format nil "~x" o))

(fun |asUtf8| (o)
  (cond ((str? o) (unfold-enc (sq-to-str o)))
        ((lst? o) o)
        (t  (unfold-enc (sq-to-str (prn o))))))

(fun |utf8| (o) (fold-enc o))

(fun upcase (o)
  (cond ((characterp o) (char-upcase o))
        ((symbolp o) (fc |symbolUpcase| o))
        (t (string-upcase (sq-to-str o)))))

(fun downcase (o)
  (cond ((characterp o) (char-downcase o))
        ((symbolp o) (fc |symbolDowncase| o))
        (t (string-downcase (sq-to-str o)))))

(fun capitalize (o) (string-capitalize (sq-to-str o)))

(fun upcase? (o) (and (characterp o) (upper-case-p o)))
(fun downcase? (o) (and (characterp o) (lower-case-p o)))

(fun |nsGet| (ns key) (ns-get ns key))
(fun |nsRemove| (ns key) (ns-rm ns key))
(fun |nsSet| (ns key value) (ns-set ns key value))



(defun gen-rng (start end)
  (let (ch f)
    (when (characterp start)
      (setf start (char-code start))
      (setf ch t))
    (when (characterp end)
      (setf end (char-code end))
      (setf ch t))
    (setf f
      (if ch
          (if (< start end)
              (fn (x) (when (<= x end)
                        (gen (code-char x) (fn-k () (funcall f (+ x 1))))))
              (fn (x) (when (>= x end)
                        (gen (code-char x) (fn-k () (funcall f (- x 1)))))))
          (if (< start end)
              (fn (x) (when (<= x end)
                        (gen x (fn-k () (funcall f (+ x 1))))))
              (fn (x) (when (>= x end)
                        (gen x (fn-k () (funcall f (- x 1)))))))))
      (funcall f start)))



(defun aesthetic (o)
  (cond ((str? o) o)
        ((lst? o) (sq-to-str (prn-sq o :l "" :r "")))
        (t (prn o))))

(fun aest (o) (aesthetic o))

;; format value as text
(fun fmt (n v x &key (|c| nil))
  (let* ((align (cond (|c| :center)
                      ((< n 0) :right)
                      ((> n 0) :left)))
         (x (aesthetic x))
         (pad (- (abs n) (len x))))
    (cond ((<= pad 0) (return-from fmt x))
          (t (let ((v (1st (if (characterp v)
                      (format nil "~a" v)
                      (aesthetic v)))))
               (fc |asStr|
                   (case align
                     (:left (conc x (dup pad v)))
                     (:right (conc (dup pad v) x))
                     (otherwise (let ((a (truncate pad 2))
                                      (b (ceiling pad 2)))
                                  (conc (dup a v) x (dup b v)))))))))))

;; pad list `xs` with `v` to length `n`
(fun pad (n v xs)
  (let ((c (len xs))
        (d nil))
    (when (< n 0)
      (setf n (- n))
      (setf d t))
    (unless (< c n) (return-from pad xs))
    (let ((l (dup (- n c) v)))
      (if d (conc xs l) (conc l xs)))))

(fun say (arg &key (|nl| t) (|p| t))
  (let ((s arg))
    (unless (and (str? s) |p|) (setf s (let ((*prn-pretty* |p|)) (prn s))))
    (format t "~a" (sq-to-str s))
    (if |nl| (format t "~%"))
    arg))

(macro \# (o)
  (let (gs)
    (labels
        ((process-quasi (o)
           (cond ((autogs? o)
                   (let* ((s (2nd o))
                          (x (find-if (fn (g) (eql (car g) s)) gs)))
                     (unless x
                       (setf x (cons s (unisym (symbol-name s))))
                       (push x gs))
                     (cdr x)))
                 ((interp? o) (2nd o))
                 ((quasi? o) (st (|'| $o)))
                 ((lst? o) (st (lst $$($map #'process-quasi o))))
                 (t (st (|'| $o))))))
      (let ((r (process-quasi o)))
        (when gs
          (let ((bs (mapcar (fn (g)
                              (let ((name (sym-path-name (car g))))
                                (st ($(cdr g) (gensym (|=| name $name))))))
                            gs)))
            (setf r (st (_let $(list-to-sq bs) $r)))))
        r))))


(fun def_op_fun (match &rest opts)
  (apply #'def-operator match (mapcar #'sq-to-list opts))
  nil)

(macro def_op (match &rest opts)
  (st (def_op_fun $match
          $$($map (fn (o)
                    (st '($(to-kw (string-upcase (sym-path-name (2nd o))))
                          (3rd o))))
                  opts))))

(fun y? (x) x)
(fun n? (x) (not x))
(fun lst? (o) (lst? o))
(fun fin? (o) (fin? o))
(fun sym? (x) (symbolp x))
(fun num? (o) (numberp o))
(fun str? (x) (str? x))
(fun chr? (x) (characterp x))
(fun int? (x) (integerp x))
(fun fn? (x) (functionp x))
(fun even? (o) (and (integerp o) (evenp o)))
(fun odd? (o) (and (integerp o) (oddp o)))
(fun pos? (o) (and (numberp o) (> o 0)))
(fun neg? (o) (and (numberp o) (< o 0)))
(fun sign (o) (cond ((> o 0)  1)
                    ((< o 0) -1)
                    (t 0)))

(fun |inRng| (s e x) (and (<= s x) (< x e)))


(fun alpha? (o) (alpha? o))
(fun digit? (o) (digit? o))

(fun array? (x) (arrayp x))

(var |sort| (lambda (k x &key (|cmp| (deco |<|)) (|by| (deco |y?|)))
  (if (eql |by| (deco |y?|))
      (let ((c (fn (x y) (funcall |cmp| #'identity x y))))
        (funcall k (list-to-sq (sort (sq-to-list x) c))))
      (let* ((|by| (if (numberp |by|) (fn (k o) (ind o |by|)) |by|))
             (c (fn (x y) (funcall |cmp| #'identity
                                   (funcall |by| #'identity x)
                                   (funcall |by| #'identity y)))))
        (funcall k (list-to-sq (sort (sq-to-list x) c)))))))

;; merge sorted key-value lists
(fun merge (xs ys)
  (unless (arrayp xs) (setf xs (sq-to-list xs)))
  (unless (arrayp ys) (setf ys (sq-to-list ys)))
  (let ((r (merge 'vector xs ys #'lt :key #'lhd)))
    (setf r (reduce (fn (a b) (if (and a (lt (lhd (car a)) (lhd b)))
                                  (cons b a)
                                  (cons b (cdr a))))
                    r
                    :initial-value nil))
    (setf r (list-to-sq (nreverse r)))
    r))


(fun bget (key ns) (ns-get ns key))
(fun bset (key val ns) (ns-set ns key val))
(fun bcng (key val ns) (ns-cng ns key val))
(fun bins (key val ns) (ns-ins ns key val))
(fun bdel (key ns) (ns-rm ns key))
(fun bfnd (n xs)
  (let ((lb 0)
        (ub (len xs)))
    (while (< lb ub)
      (let* ((p (truncate (+ lb ub) 2))
             (v (ind xs p)))
        (cond ((lt n v) (setf ub p))
              ;; add 1 to exclude p from future search
              ((lt v n) (setf lb (+ p 1)))
              (t (return-from bfnd v)))))
    nil))

(fun fget (path) (load-file-bytes (sq-to-str path)))
(fun fset (path value) (save-file-bytes (sq-to-str path) (fc |asBytes| value)))

(fun ls (dir) (list-to-sq (mapcar #'namestring
                                  (list-directory (sq-to-str dir)))))
(fun pwd () (namestring (truename ".")))
(fun |asFilename| (x) (file-namestring (pathname (sq-to-str x))))
(fun |asDirname| (x) (directory-namestring (pathname (sq-to-str x))))


(fun shell (cmd) (list-to-sq (shell (sq-to-str cmd))))

(fun max (&rest xs)
  (let* ((cur (car xs)))
    (if (lst? cur) (setf cur (fc |maximize| (deco |y?|) cur)))
    (loop as x in (cdr xs) do
         (progn
           (if (lst? x) (setf x (fc |maximize| (deco |y?|) x)))
           (if (fc |>| x cur) (setf cur x))))
    cur))

(fun min (&rest xs)
  (let* ((cur (car xs)))
    (if (lst? cur) (setf cur (fc |minimize| (deco |y?|) cur)))
    (loop as x in (cdr xs) do
         (progn
           (if (lst? x) (setf x (fc |minimize| (deco |y?|) x)))
           (if (fc |<| x cur) (setf cur x))))
    cur))

(fun |newGfx| (w h) (sdl::new-gfx w h))
(fun w (gfx) (sdl::gfx-w gfx))
(fun h (gfx) (sdl::gfx-h gfx))

(fun |clearGfx| (color gfx) (sdl::clear-pixels color gfx))

(fun |loadGfx| (name) (sdl::load-image name))
(fun |saveGfx| (name gfx) (sdl::save-image name gfx))


(fun |tintGfx| (tints g &key (|brightness| 0))
  (sdl::mark-tint
   (make-array (len tints) :element-type 'sdl::uint32
               :initial-contents (sq-to-list tints))
   g
   :brightness |brightness|))

(fun |gfxMargins| (g) (sdl::gfx-margins g))


(in-package :sdl)

(defun blit (x y src dst &key
             (|rect| nil)
             (|flipX| nil)
             (|flipY| nil)
             (|alpha| 0.0) ; additional source alpha
             (|blend| t)   ; set to false to ignore alpha source
             (|color| nil) ; color source with this color
             (|mask| nil)  ; additional mask
             (|tint| nil)
             )
  (declare (ignorable |alpha| |blend| |color| |mask|))
  (unless |tint| (setf |tint| #xFFFFFF))
  (if |rect|
      (setf |rect| (sdl::make-rect :x (symta::ind |rect| 0)
                                        :y (symta::ind |rect| 1)
                                        :w (symta::ind |rect| 2)
                                        :h (symta::ind |rect| 3)))
      (setf |rect| (top-clip src)))
  (let* ((c  (top-clip dst))
         (sx (rect-x |rect|)) ; source x
         (sy (rect-y |rect|)) ; source y
         (w  (rect-w |rect|))
         (h  (rect-h |rect|))
         (ow w)
         (oh h)
         (xi 1) ; x increment
         (yi 1) ; y increment
         (ex 0)
         (ey 0)
         (d  (gfx-pixels dst))
         (s  (gfx-pixels src))
         (sw (gfx-w src))
         (dw (gfx-w dst))
         (pd 0)
         (ps 0)
         (col 0)
         (tm 0)  ; tint multiplier
         (sm 0)  ; source blending multiplier
         (dm 0)) ; destination blending multiplier
    (declare (optimize (safety 0) (speed 3))
             (gfx dst src)
             (fixnum x y sx sy w h ow oh ex ey xi yi sw dw pd ps sm dm tm)
             (cptr d s)
             (uint32 col |tint|)
             (single-float |alpha|))
    (when (or (>= x (rect-w c)) (>= y (rect-h c)))
      (return-from blit dst))
    (when (or (<= (+ x w) (rect-x c)) (<= (+ y h) (rect-y c)))
      (return-from blit dst))
    (when (< x (rect-x c))
      (if |flipX|
          (decf ow (- (rect-x c) x))
          (incf sx (- (rect-x c) x)))
      (setf w (- w (- (rect-x c) x)))
      (setf x (rect-x c)))
    (when (< y (rect-y c))
      (if |flipY|
          (decf oh (- (rect-y c) y))
          (incf sy (- (rect-y c) y)))
      (setf h (- h (- (rect-y c) y)))
      (setf y (rect-y c)))
    (setf ey (+ y h))
    (when (> (+ x w) (rect-w c))
      (setf w (- (rect-w c) x)))
    (when (> ey (rect-h c))
      (setf ey (rect-h c)))
    (when |flipX|
      (setf sx (+ sx ow -1))
      (setf xi -1))
    (when |flipY|
      (setf sy (+ sy oh -1))
      (setf yi -1))
    (unless |blend|
      (for nil (< y ey) (progn (incf y) (incf sy yi))
        (setf pd (+ (f* y dw) x))
        (setf ex (+ pd w))
        (setf ps (+ (f* sy sw) sx))
        (for nil (< pd ex) (progn (incf pd) (incf ps xi))
          (setf (mem-aref d :uint32 pd)
                (mem-aref s :uint32 ps)))))
    (when (or |color| (/= |alpha| 0.0))
      (setf col (or |color| #xFFFFFFFF))
      (w/rgba (r g b a |tint|
               cr cg cb ca col)
        (declare (fixnum r g b a cr cg cb ca))
        (setf tm (- #xFF ca))
        (setf cr (f* cr tm))
        (setf cg (f* cg tm))
        (setf cb (f* cb tm))
        (setf sm (truncate (* #xFF (- 1.0 |alpha|))))
        (setf dm (truncate (* #xFF |alpha|)))
        (setf sm (clip-range sm 0 #xFF))
        (setf dm (clip-range dm 0 #xFF))
        (for nil (< y ey) (progn (incf y) (incf sy yi))
          (setf pd (+ (f* y dw) x))
          (setf ex (+ pd w))
          (setf ps (+ (f* sy sw) sx))
          (for nil (< pd ex) (progn (incf pd) (incf ps xi))
            (let ((sc (mem-aref s :uint32 ps)))
              (w/rgba (dr dg db da (mem-aref d :uint32 pd)
                       sr sg sb sa sc)
                (cond ((= sa 0)
                       (setf sr (>> (f+ (f* sr ca) cr) 8)
                             sg (>> (f+ (f* sg ca) cg) 8)
                             sb (>> (f+ (f* sb ca) cb) 8))
                       (setf (mem-aref d :uint32 pd)
                             (rgb (>> (f+ (f* sr sm) (f* dr dm)) 8)
                                  (>> (f+ (f* sg sm) (f* dg dm)) 8)
                                  (>> (f+ (f* sb sm) (f* db dm)) 8))))
                      ((= sa 1)
                       (setf sr (>> (f* sr r) 8)
                             sg (>> (f* sr g) 8)
                             sb (>> (f* sr b) 8))
                       (setf sr (>> (f+ (f* sr ca) cr) 8)
                             sg (>> (f+ (f* sg ca) cg) 8)
                             sb (>> (f+ (f* sb ca) cb) 8))
                       (setf (mem-aref d :uint32 pd)
                             (rgb sr sg sb)))))))))
      (return-from blit dst))
    (w/rgb (r g b |tint|)
      (for nil (< y ey) (progn (incf y) (incf sy yi))
        (setf pd (+ (f* y dw) x))
        (setf ex (+ pd w))
        (setf ps (+ (f* sy sw) sx))
        (for nil (< pd ex) (progn (incf pd) (incf ps xi))
          (let ((sc (mem-aref s :uint32 ps)))
            (w/rgba (sr sg sb sa sc)
              (cond ((= sa 0) (setf (mem-aref d :uint32 pd) sc))
                    ((= sa 1) (setf (mem-aref d :uint32 pd)
                                    (rgb (>> (f* sr r) 8)
                                         (>> (f* sr g) 8)
                                         (>> (f* sr b) 8))))
                    ))))))
    dst))

(in-package :symta)

(fun |cutGfx| (r gfx)
  (let ((ng (sdl::new-gfx (3rd r) (4th r))))
    (sdl::blit 0 0 gfx ng :|blend| nil :|rect| r)
    ng))


(fun |drawLine| (color s e gfx)
  (sdl::draw-line color (1st s) (2nd s) (1st e) (2nd e) gfx))

(fun |drawRect| (color r gfx)
  (sdl::draw-rect color (1st r) (2nd r) (3rd r) (4th r) gfx))

(fun |fillRect| (color r gfx)
  (sdl::fill-rect color (1st r) (2nd r) (3rd r) (4th r) gfx))

(fun |drawCircle| (color o r gfx)
  (sdl::draw-circle color (1st o) (2nd o) r gfx))

(fun |fillCircle| (color o r gfx)
  (sdl::fill-circle color (1st o) (2nd o) r gfx))

(fun |putPixel| (xy color gfx)
  (sdl::put-pixel (1st xy) (2nd xy) color gfx)
  gfx)

(fun |getPixel| (xy gfx)
  (sdl::get-pixel (1st xy) (2nd xy) gfx))



(fun |avgTile| (gfx)
  (sdl::avg-tile gfx))


(fun rgb (r g b) (sdl::rgb r g b))
(fun rgba (r g b a) (sdl::rgba r g b a))
(fun |breakRGB| (c) (sdl::w/rgb (r g b c) (lst r g b)))
(fun |breakRGBA| (c) (sdl::w/rgba (r g b a c) (lst r g b a)))

(defun convert-input (i)
  (if (atom i)
      i
      (list-to-sq (mapcar #'convert-input i))))

(fun |runGfx| (draw-function &key (|w| 640) (|h| 480) (|title| ""))
  (sdl::run (fn (input gfx)
                   (funcall draw-function #'identity
                            (convert-input (sort input #'string< :key #'car))
                            gfx))
                 :width |w|
                 :height |h|
                 :title |title|))

(fun |stopGfx| ()
  (sdl::stop))

(fun time () (/ (get-internal-real-time) internal-time-units-per-second))
(fun sleep (n) (sleep n))

(fun |sndLoad| (name)
  (sdl::load-sound name))

(fun |sndPlay| (snd &key (|volume| 1.0))
  (sdl::play-sound |volume| snd)
  nil)


(fun |musLoad| (name) (sdl::load-music name))
(fun |musPlay| (mus) (sdl::play-music mus) nil)
(fun |musPlaying| () (sdl::music-playing))
(fun |musStop| () (sdl::stop-music))
(fun |musSetVolume| (volume) (sdl::set-music-volume volume))


(fun mod (a b) (error "`mod` is obsolete"))
(macro in (p v) (error "`in` is obsolete"))
(fun id (o) (error "`id` is obsolete"))
