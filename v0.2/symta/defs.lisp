(in-package :symta)


(defun macro-error (msg &rest args)
  (error "~a~a" (/prn-src *src-source*) (apply #'format nil msg args)))

(defun mk-sym (xs)
  (when (sym? xs) (setf xs (vector xs)))
  (unless (arrayp xs) (setf xs (sq-to-vec xs)))
  (with-output-to-string (o)
    (loop as x across xs do
      (progn (unless (sym? x) ; interpolate it nicely
               (setf x (if (arrayp x)
                           (setf x (mk-sym x))
                           (prn x))))
             (write-string x o)))))


(defmacro fc-predef (sym &rest as)
  (let ((pdsym (intern sym "predefs")))
    `(,pdsym ,@as)))

(defmacro fun (name args &body body)
  (if (stringp name) (setf name (intern name)))
  (let* ((sn   (st-sym name))
         (as   (mapcar (fn (_) `',(unisym)) args))
         (sym  (intern (local-to-global sn) *pkg*)))
    `(progn
       (defun ,sym ,args
         (block ,name ,@body))
       (set-prop ,sn "predefined" t)
       (set-prop ,sn "src"
                 (st (|<:>| (,sn ,@as = cl (fc-predef (_q ,sn) ,@as)))))
       nil)))


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
  (let* ((sn (st-sym name))
         (sym (intern sn *pkg*))
         (val (gensym))
         (l (gensym)))
    `(progn
       (let ((,val (eval '(named-fn ,sym (,l)
                           ,(macro-binds args l
                                         `(block ,name ,@body))))))
         (set-prop ,sn "macro" ,val)
         ))))

(defmacro var (name val)
  (let* ((sym (st-sym name :upcase t))
         (sym (intern (local-to-global sym) *pkg*)))
    `(progn (defparameter ,sym ,val)
            )))



(var |PI| pi)

(fun error (x)
  (error (sq-to-str x)))

(fun gensym (&rest name)
  (unisym (if name (car name) "G")))


;; example: str "LISP" | map ?,hex | infix " " | sym
(fun str (o) (if (sym? o)
                 (if (> (length o) 0) (map 'vector #'string o))
                 o))

(fun sym (xs)
  (mk-sym xs))


(fun unbind (name)
  (makunbound (local-to-global name))
  nil)

(defun my-abort () (abort))
(fun abort () (my-abort))

(defun $read-from-string (input) (/read input))
(fun sexp (input)
  (unless (stringp input) (setf input (mk-sym input)))
  ($read-from-string input))
;(fun read () (lhd ($read (read-line))))

(fun eval (e &key (|optimize| nil))
  (if |optimize|
      (let ((sb-ext:*evaluator-mode* :interpret))
        (%eval e))
      (%eval e)))

(fun load (file) ($load file))


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

(defun expand-set2 (save prev us v)
  (unless us
    (return-from expand-set2 (if save
                                 (st (set_l $prev $v))
                                 v)))
  (let* ((u  (car us))
         (us (cdr us))
         (p  (2nd u))
         (i  (3rd u)))
    (cond
      ((msg? u)
       (let ((v (cond ((fn-sym? i) (st (cl (ns-set (@ $p) (@ (_q $i)) (@ $v)))))
                      (t (st (cl (ns-set (@ $p) (@ $i) (@ $v))))))))
         (expand-set2 save p us v)))
      ((coma? u)
       (expand-set2 save p us (st (cng $i $v $p))))
      )))

(defun expand-set (save p v)
  (let ((us))
    (expand-set1 0
                 (fn (u) (push u us))
                 (fn (_)
                   (if (lst? v)
                       (with-unisyms (g)
                         (st (_let (($g $v))
                               $(expand-set2 save nil us g))))
                       (expand-set2 save nil us v)))
                 p)))

(macro |_assign| (p v)
  (cond ((sym? p) (st (set_l $p $v)))
        (t (expand-set t p v))))

(macro set (p v)
  (cond ((sym? p) v)
        (t (expand-set nil p v))))

(macro w (&rest xs)
  (st (c $(rhd xs) $$(rtl xs))))

(macro _let (binds expr)
  (unless (lst? binds) (macro-error "_let: invalid bindings list"))
  (let ((vars   ($map #'1st binds))
        (values ($map #'2nd binds)))
    (st ((_fn $vars $expr) $$values))))


(fun me (e &rest depth) ; macroexpand
  (setf depth (if depth (car depth) 1))
  (expand e))

(macro fn (args body)
  (lambs-to-multimethod (st (($$args = $$body)))))

(macro <> (xs) (lambs-to-multimethod (split-lambs xs) :name (st r)))
(macro <clean> (xs) (lambs-to-multimethod (split-lambs xs) :name nil :clean t))
(macro |<:>| (xs) (lambs-to-multimethod (split-lambs (ltl xs)) :name (lhd xs)))


(macro cl (xs) (st (cl_esc '$xs)))

(defun count-incuts (l n)
  (unless l (return-from count-incuts nil))
  (let* ((hd (lhd l))
         (tl (ltl l))
         (ics (count-incuts tl (+ n 1))))
    (if (incut? hd) (setf ics (pre (lst n (2nd hd)) ics)))
    ics))


(defun filter-incuts (l r o)
  (unless r
    (if l (setf o (suf (st (|[fn]| $$l)) o)))
    (return-from filter-incuts o))
  (let* ((hd (lhd r))
         (r  (ltl r)))
    (if (incut? hd)
        (progn (if l (setf o (suf (st (|[fn]| $$l)) o)))
               (filter-incuts nil r (suf (2nd hd) o)))
        (filter-incuts (suf hd l) r o))))

(macro |[]| (args)
  (when (fnd #'arrow? args)
    (return-from |[]|
      (st (u $$($map (fn (x)
                       (let ((x ($split "=" x)))
                         (lst ":" (lhd x) (ltl x))))
                     ($split ";" args))))))
  (let* ((ics  (count-incuts args 1))
         (n    (len ics))
         (o    (cond
                 ((null args) nil)
                 ((= n 0) (st (|[fn]| $$args)))
                 ((= n 1)
                  (let* ((ic (lhd ics))
                         (p  (1st ic))
                         (v  (2nd ic))
                         (l  (take (- p 1) args))
                         (ll (len l))
                         (r  (drop p args))
                         (lr (len r)))
                    (cond ((and (= ll 0) (= lr 0)) v)
                          ((and (= ll 1) (= lr 0)) (st (pre $(lhd l) $v)))
                          ((and (= ll 0) (= lr 1)) (st (suf $(lhd r) $v)))
                          (t (st (_conc $$(if l (st ((|[fn]| $$l))))
                                        $v
                                        $$(if r (st ((|[fn]| $$r))))))))))
                 (t (st (_conc $$(filter-incuts nil args nil)))))))
    o))

(defmacro rng-hlp (type type2 inc size value)
  `(let* ((i 0)
          (sz ,size)
          (r  (make-array sz :element-type ',type2)))
     (declare (optimize ;(safety 0)
                          (speed 3))
              (,type start end step)
              (fixnum i sz))
     (while (< i sz)
       (setf (aref r i) ,value)
       (,inc start step)
       (incf i))
     r))


(fun _rng (start step end)
  (typecase start
    (fixnum
     (if (< start end)
         (rng-hlp fixnum fixnum incf (- end start) start)
         (rng-hlp fixnum fixnum decf (- start end) start)))
    (string 
     (let ((start (char-code (char start 0)))
           (end (char-code (char end 0))))
       (rng-hlp fixnum t incf (- end start)
                (string (code-char start)))))
    (otherwise
     (unless (floatp step) (setf step (coerce step 'single-float)))
     (if (< start end)
         (rng-hlp single-float single-float incf (floor (- end start)) start)
         (rng-hlp single-float single-float decf (floor (- start end)) start))
     )))


(defun expand-del (x save)
  (let ((f (1st x))
        (a (2nd x))
        (b (3rd x)))
    (if save (setf a (st (|!| $a))))
    (cond ((equal f (st |,|)) (st (del $b $a)))
          (t (when (unisym? b) (setf b (st ($b))))
             (when (fn-sym? b) (setf b (st (\\ $b))))
             (st (bdel $b $a))))))

(macro |~@!| (x) (expand-del x t))
(macro |~@| (x) (expand-del x nil))

(fun |_symnum| (x) (symnum x))

(macro |.| (a b)
  (cond
    ((fn-sym? a) (st (_pkg (_q $a) (_q $b))))
    ((and (lst? b) (equ (1st b) "<>"))
      (let* ((b  (2nd b))
             (f  (lhd b))
             (ff (unisym)))
        (if (sym? f)
            (st (_dot (_q $f) $a $(symnum f) $$(ltl b)))
            (st (_let (($ff $f))
                  (_dot (_q $ff) $a (_symnum $ff) $$(ltl b)))))))
    (t (when (unisym? b) (setf b (st (do $b))))
       (st (cl (ns-get ($"@" $a)
                       ($"@" $(if (fn-sym? b) (st (\\ $b)) b))))))))

(macro |,| (a b)
  (cond ((or (fn-sym? b) (lamb? b)) (st ($b $a)))
        ((or (fn-sym? a) (lamb? a)) (st ($a $b)))
        (t (st (cl (ind ($"@" $a) ($"@" $b)))))))

(macro |'| (a b)
  (st (c (|.| $a $b))))

(macro |_set| (d)
  (when (arrow? (1st d)) ;; unconditional execution, instead of function binding
    (return-from |_set| (st (c (<clean> $d)))))
  (let* ((defs d)
         (name (1st defs))
         (defs (split-lambs (ltl defs)))
         (macro nil)
         )
    (while (bnd? name)
      (let ((f (2nd name)))
        (cond ((equal f "m") (setf macro t)) ; macro
              (t (macro-error "Unsupported type-flag: ~a" f))
              ))
      (setf name (3rd name)))
    (unless (or (env-sym? name) (msg? name))
      (format t "Compiling: ~a~%" (prn name)))
    ;(when (or (not (sym? name)) (equal name (st =)))
    ;  (macro-error "`Name @Args = @Body`: Name is missing"))
    (when (sym? name) (set-prop name "src" d))
    (cond
      (macro
        (st (|setProp| (_q $name) "macro"
                       $(lambs-to-multimethod defs :name name :macro t))))
      (t (cond
           ((fn-sym? name)
            (st (do (|setProp| (_q $name) "macro" n)
                    (|_assign| $name $(lambs-to-multimethod defs :name (list name (st r))))
                    n)))
           (t ; "A B C = X" should act as unquoted assignment
              ; "M.X = V" treats M as an assoc list
              ; "(makeAdder X) Y = X+Y" produces "makeAdder X = <Y = X+Y>"
              (st (|_assign| $name $(1st defs)))
              ))))))

(defun expand-{} (xs)
  (unless xs (return-from expand-{} nil))
  (split-expand-binds ($split #'arrow? (lhd xs)) t (expand-{} (ltl xs))))

(macro {} (xs)
  (expand-{} (split-lambs xs)))


(fun c (f &rest args) (apply f args))

(fun |setProp| (s p v) (progn (set-prop s p v) v))

(defun sq-to-array (xs)
  (make-array (len xs) :initial-contents (sq-to-list xs)))

(defun add-lists (a b)
  (cond ((null a) b)
        ((null b) a)
        (t (unless (arrayp a) (setf a (sq-to-array a)))
           (unless (arrayp b) (setf b (sq-to-array b)))
           (let* ((l (min (length a) (length b)))
                  (r (make-array l)))
             (loop as i below l
                do (setf (aref r i) (+ (aref a i) (aref b i))))
             r))))

(defun neg-list (xs)
  (when xs
    (let ((r (make-array (len xs))))
      (loop as i below (length r)
         do (setf (aref r i) (- (or (ind xs i) 0))))
      r)))

(defun sub-lists (a b)
  (cond ((null a) (if (numberp b) (- b) (neg-list b)))
        ((null b) a)
        (t (unless (arrayp a) (setf a (sq-to-array a)))
           (unless (arrayp b) (setf b (sq-to-array b)))
           (let* ((l (min (length a) (length b)))
                  (r (make-array l)))
             (loop as i below l
                do (setf (aref r i) (- (aref a i) (aref b i))))
             r))))

(defun dot-lists (a b)
  (let ((r 0))
    (dotimes (i (len a))
      (incf r (* (ind a i) (ind b i))))
    r))

(defun mul-lists (a b)
  (let* ((l  (min (len a) (len b)))
         (rs (make-array l)))
    (dotimes (i l)
      (setf (aref rs i) (* (ind a i) (ind b i))))
    rs))

(defun mul-list-by-value (l v)
  ($map (fn (x) (c |*| v x)) l))

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
(fun % (a b)
  (if (numberp a)
      (truncate a b)
      ($map (fn (v) (truncate v b)) a)))
(fun mod (a b)
  (if (numberp a)
      (rem a b)
      ($map (fn (v) (rem v b)) a)))

(fun |^| (a b)
  (if (numberp a)
      (expt a b)
      (let ((r a))
        (dotimes (_ (- b 1))
          (setf r (mul-lists r a)))
        r)))

(fun |~| (o)
  (if (numberp o)
      (- o)
      (neg-list o)))


(defparameter *sqrt-table* (make-array (expt 2 16) :element-type 'single-float))
(progn (loop as i below (length *sqrt-table*)
            do (setf (aref *sqrt-table* i) (sqrt i))))
(fun sqrt (x)
  (typecase x
      ((integer 0 65535) (aref *sqrt-table* x))
      (otherwise (sqrt x))))

;; bitwise operators
(fun shl (a b) (ash a b))
(fun shr (a b) (ash a (- b)))
(fun xor (a b) (logxor a b))
(fun and (a b) (logand a b))
(fun or (a b) (logior a b))
(fun not (x) (if (eq x 0) 1 0))


(fun |ptrEq| (a b) (eq a b))
(fun ≥≤ (a b) (equ a b))
(fun ≤≥ (a b) (not (equ a b)))
(fun ≤  (a b) (lt a b))
(fun ≥  (a b) (gt a b))
(fun ≤≤ (a b) (lte a b))
(fun ≥≥ (a b) (gte a b))

(fun len (o) (len o))
(fun _conc (&rest args) (conc-list args))

(fun _sconc (&rest xs)
  (apply #'concatenate 'string (mapcar #'aesthetic xs)))


(fun |[]| (&rest args) (list-to-sq args))
(fun |[fn]| (&rest args) (list-to-sq args))
(fun l (&rest args) (list-to-sq args))


(fun pre (e s) (pre e s))
(fun suf (e s) (suf e s))
(fun lhd (s) (lhd s))
(fun ltl (s) (ltl s))
(fun rhd (s) (rhd s))
(fun rtl (s) (rtl s))
(fun t (n s) (take n s))
(fun d (n s) (drop n s))

(fun split (sep xs) ($split (aref sep 0) xs))

(fun rev (xs)
  (setf xs (if (arrayp xs) (copy-seq xs) (sq-to-vec xs)))
  (nreverse xs))

(fun _seq (hd tl) (make-gs :head hd :tail tl))
(macro seq (hd tl) (st (_seq $hd (_fn () $tl))))

(fun k (f xs) (keep f xs))
(fun s (f xs) (skip f xs))

(fun m (fun seq &rest seqs)
  ;; FIXME: this function conflicts with continuations
  ;;        and could produce stack overflow
  (unless seqs (return-from m ($map1 fun seq)))
  (let (r)
    (push seq seqs)
    (while (not (some #'null seqs))
      (setf r (suf (apply fun (mapcar #'lhd seqs)) r))
      (setf seqs (mapcar #'ltl seqs)))
    r))

(fun xy (x y w h) ; list of points of a rect
  (let* ((s (* w h))
         (ex (+ x w))
         (ey (+ y h))
         (i 0)
         (r (make-array s)))
    (while (< y ey)
      (let ((x x))
        (while (< x ex)
          (setf (aref r i) (vector x y))
          (incf i)
          (incf x)))
      (incf y))    x
    r))

(fun lst (o) (as-list o))

(fun bytes (o)
  (if (typep o '(SIMPLE-ARRAY (UNSIGNED-BYTE 8)))
      o
      (as-byte-array o)))

(fun ints (o)
  (if (typep o '(SIMPLE-ARRAY fixnum))
      o
      (as-int-array o)))

(fun flts (o)
  (if (typep o '(SIMPLE-ARRAY single-float))
      o
      (as-float-array o)))

(fun u4s (o)
  (if (typep o '(SIMPLE-ARRAY u4))
      o
      (as-u4-array o)))

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

(defun as-u4-array (o)
  (if (gs? o) (setf o (lify o)))
  (make-array (len o)
              :element-type 'u4
              :initial-contents  (sq-to-list o)))

(fun base (base o)
  (unless (sym? o) (setf o (mk-sym o)))
  (parse-integer o :radix base :junk-allowed t))

(fun chr (x) (code-char x))

(fun flt (xs)
  (if (numberp xs)
      (coerce xs 'float)
      ($map (fn (x) (coerce x 'float)) xs)))

(fun int (o)
  (cond ((num? o) (truncate o))
        ((sym? o) (char-code (aref o 0)))
        ((lst? o) ($map (fn (x) (c |int| x)) o))))

(fun num (o) (cond ((num? o) o)
                   ((sym? o) (let ((x (lhd (lhd (/read o)))))
                               (if (num? x) x 0)))
                   ((characterp o)
                    (let ((c (char-code o)))
                      (cond ((digit? o) (- c (char-code #\0)))
                            #|((upper-case-p o) (+ 10 (- c (char-code #\A))))
                            ((lower-case-p o) (+ 10 (- c (char-code #\a))))|#
                            )))))
(fun frac (o) (multiple-value-bind (i f) (truncate o) 
                (declare (ignorable i))
                f))

(fun ceil (o) (ceiling o))
(fun floor (o) (floor o))
(fun round (o) (if (num? o) (round o) ($map #'round o)))

(defmacro tn-fun (name to)
  `(fun ,name (x)
     (when (stringp x)
       (return-from ,name
         (with-output-to-string (o)
           (dotimes (i (length x))
             (let ((c (char x i)))
               (format o ,to (char-code c)))))))
     (format nil ,to x)))

(tn-fun bin "~b")
(tn-fun oct "~o")
(tn-fun dec "~a")
(tn-fun hex "~x")

(fun |utf8| (o) (fold-enc o))

(fun |utf8enc| (o)
  (cond ((sym? o) (unfold-enc o))
        ((lst? o) o)
        (t  (unfold-enc (mk-sym (prn o))))))

(fun upcase (o)
  (cond ((characterp o) (char-upcase o))
        (t (string-upcase (mk-sym o)))))

(fun downcase (o)
  (cond ((characterp o) (char-downcase o))
        (t (string-downcase (mk-sym o)))))

(fun capitalize (o) (string-capitalize (mk-sym o)))

(fun upcase? (o) (and (stringp o) (all #'upper-case-p o)))
(fun downcase? (o) (and (stringp o) (all #'lower-case-p o)))




(defun aesthetic (o)
  (cond ((sym? o) o)
        ((null o) "")
        ((lst? o) (mk-sym (prn-sq o :l "" :r "")))
        (t (prn o))))

;; formats text
(fun fmt (n v x &key (|c| nil))
  (let* ((align (cond (|c| :center)
                      ((< n 0) :right)
                      ((> n 0) :left)))
         (x (aesthetic x))
         (pad (- (abs n) (length x))))
    (cond ((<= pad 0) (return-from fmt x))
          (t (let ((v (aesthetic v)))
               (case align
                 (:left (concatenate 'string x (mk-sym (dup pad v))))
                 (:right (concatenate 'string (mk-sym (dup pad v)) x))
                 (otherwise (let ((a (truncate pad 2))
                                  (b (ceiling pad 2)))
                              (concatenate 'string
                                (mk-sym (dup a v)) x (mk-sym (dup b v))
                                )))))))))

;; pad list `xs` with `v` to length `n`
;; if n is negative, then pad from end
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
  (unless (sym? arg)
    (let ((*prn-pretty* |p|)) (setf arg (prn arg))))
  (format t "~a" arg)
  (if |nl| (format t "~%"))
  (force-output)
  nil)

(macro \\ (xs)
  (when (sym? xs) (return-from \\ (st (_q $xs))))
  (let ((gs nil)
        (interps 0))
    (labels
        ((process-interp (d i o)
           (if (unquote? o)
               (if-bind r (process-interp d (+ i 1) (2nd o))
                 (if (< i d) r (lst (st |[]|) (lst (st _q) (st $)) r)))
               (if (< i d) nil o)))
         (process-quasi (d o)
           (cond ((and (autogs? o) (= d 1))
                   (let* ((s (2nd o))
                          (x (find-if (fn (g) (equal (car g) s)) gs)))
                     (unless x
                       (setf x (cons s (unisym "G")))
                       (push x gs))
                     (cdr x)))
                 ((unquote? o)
                  (if-bind i (process-interp d 0 o)
                    (progn (incf interps)
                           i)
                    (st (_q $o))))
                 ((quote? o) (st (|[]| ((_q \\)
                                        $(process-quasi (+ d 1) (2nd o))))))
                 ((lst? o)
                  (st (|[]| $($map (lambda (o) (process-quasi d o)) o))))
                 (t (st (_q $o))))))
      (let ((r (process-quasi 1 xs)))
        (unless (or (> interps 0) gs)
          (return-from \\ (st (_q $xs))))
        (when (equal (1st r) (st |@|))
          (setf r (2nd r)))
        (when gs
          (let ((bs (mapcar (fn (g)
                              (let ((name (st (\\ $(car g)))))
                                (st ($(cdr g) (gensym $name)))))
                            gs)))
            (setf r (st (_let $(list-to-sq bs) $r)))))
        r))))

(macro |$| (x)
  (%eval (lst "\\" (lst "\\" (lst "$" (lst "$" x))))))

(fun y (x) x)
(fun n (x) (not x))
(fun x? (x) (if (fin? x) x))
(fun xs? (x) (if (lst? x) x))
(fun sym? (x) (if (sym? x) x))
(fun num? (x) (if (numberp x) x))
(fun chr? (x) (if (characterp x) x))
(fun alpha? (x) (if (and (stringp x) (> (length x) 0) (alpha? (aref x 0)))
                    x))
(fun digit? (x) (if (and (stringp x) (> (length x) 0) (digit? (aref x 0)))
                    x))
(fun int? (x) (if (integerp x) x))
(fun fn? (x) (if (functionp x) x))
(fun even? (x) (if (and (integerp x) (evenp x)) x))
(fun odd? (x) (if (and (integerp x) (oddp x)) x))
(fun pos? (x) (if (and (numberp x) (> x 0)) x))
(fun neg? (x) (if (and (numberp x) (< x 0)) x))
(fun nz? (x) (if (and (numberp x) (not (zerop x))) x))

(fun |meta| (x) (meta-get x))
(fun |metaSet| (m x) (meta-set m x))

(fun sign (xs)
  (if (numberp xs)
      (signum xs)
      ($map #'signum xs)))
(fun numerator (x) (numerator x))
(fun denominator (x) (denominator x))


(fun rand (x) (random x))

(fun |inRng| (s e x) (and (<= s x) (< x e)))

(fun sort (xs &key (|cmp| (glb |≤|)) (|by| (glb |y|)))
  (let ((r (if (eql |by| (glb |y|))
               (sort (sq-to-vec xs) |cmp|)
               (let* ((|by| (if (numberp |by|) (fn (k o) (ind o |by|)) |by|))
                      (c (fn (x y) (funcall |cmp|
                                            (funcall |by| x)
                                            (funcall |by| y)))))
                 (sort (sq-to-vec xs) c)))))
    (vec-to-sq r)))

(fun shuffle (xs)
  (setf xs (sq-to-vec xs))
  (do* ((n (length xs) (- n 1)))
       ((zerop n) (vec-to-sq xs))
    (let* ((r (random n)) (x (aref xs r)))
      (setf (aref xs     r  ) (aref xs (- n 1)))
      (setf (aref xs (- n 1)) x))))

;; merge sorted key-value lists
(fun _merge (xs ys)
  (let ((r (hash xs)))
    (if (hash-table-p ys)
        (maphash (fn (k v) (setf (gethash k r) v)) ys)
        ($map1 (fn (e) (setf (gethash (1st e) r) (2nd e))) ys))
    (unless (or (hash-table-p xs) (hash-table-p ys))
      (setf r (sort (unhash r) #'lt :key #'lhd))
      (when (<= (length r) (lbp))
        (setf r (vec-to-sq r))))
    r))

(fun hget (k h) (gethash k h))
(fun hset (k v h)
  (if v
      (setf (gethash k h) v)
      (remhash k h)))

(fun del (i xs) (rm xs i))
(fun cng (i v xs) (cng xs i v))
(fun ins (i v xs) (ins xs i v))

(fun lcng (i v xs) (lcng xs i v))
(fun lins (i v xs) (lins xs i v))

(fun bset (key val ns) (ns-set ns key val))
(fun bdel (key ns) (ns-rm ns key))
;; heap insert: useful for huffman and pathfinding
(fun hins (key val ns) (ns-ins ns key val))

(fun bfnd (n xs) ;find, which does binary search
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



(fun ls (&rest dir)
  (let ((base (or (car dir) ".")))
    (list-to-sq
     (mapcar (fn (x) (namestring x))
             (remove-if (fn (x) (let ((n (file-namestring x)))
                                  (and (> (length n) 0) (char= (aref n 0) #\.))))
                        (list-directory base))))))

(fun cd (x) (cd x))
(fun pwd () (pwd))
(fun shell (cmd)
  (let ((xs (list-to-sq (shell (mk-sym cmd)))))
    (if (equal (rhd xs) "")
        (rtl xs)
        xs)))

(fun time () (/ (get-internal-real-time) internal-time-units-per-second))

