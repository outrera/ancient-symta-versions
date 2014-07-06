(in-package :symta)


(defmacro lbp () 32) ;; point at which switch from vectors to trees

;; generated sequence
(defstruct gs head tail)
(defun gs? (x) (if (typep x 'gs) x))

(defun gs-lhd (gs) (gs-head gs))
(defun gs-ltl (gs) (funcall (gs-tail gs)))

(defun gs-lify (s)
  (let (r)
    (while s
      (push (gs-lhd s) r)
      (setf s (gs-ltl s)))
    (lify (list-to-sq (nreverse r)))))

(defun gs-len (s)
  (len (gs-lify s)))

(defun gs-ind (s i)
  (if (< i 0) (return-from gs-ind (ind (gs-lify s) i)))
  (while (> i 0)
    (setf s (gs-ltl s))
    (unless s (return-from gs-ind nil))
    (decf i))
  (gs-lhd s))

(defun gen (hd tl) (make-gs :head hd :tail tl))
(defmacro lazy (hd tl) `(gen ,hd (fn () ,tl)))

(defun lazy-ints (n end)
  (if (<= n end) (lazy n (lazy-ints (+ n 1) end))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro lst? (x)
    (with-gensyms (v)
      `(let ((,v ,x))
         (typecase ,v
           (array (not (stringp ,v)))
           (cons  t)
           (gs    t)))))
  (defmacro fin? (x) `(not (lst? ,x)))
  )

(defun len (x)
  (typecase x
    (cons       (m x))
    (null       0)
    (array      (length x))
    (gs         (gs-len x))
    (hash-table (hash-table-count x))
    (otherwise (error "len: not a list `~a`" x))))

(declaim (inline array-ind))
(defun array-ind (x i)
  (let ((l (length x)))
    (when (< i 0) (setf i (+ l i)))
    (if (and (>= i 0) (< i l))
        (aref x i))))



(declaim (ftype (function (t fixnum) t) ind))
(defun ind (x i)
  (locally (declare (optimize (safety 0) (speed 3))
                    (fixnum i))
    (typecase x
      (cons   (cond ((>= i 0) (if (< i (m x)) (pt-ind x i)))
                    (t        (let ((i (+ (m x) i)))
                                (if (>= i 0) (pt-ind x i))))))
      (null   x)
      (string (if-bind it (array-ind x i)
                (string it)))
      (array  (array-ind x i))
      (gs     (gs-ind x i))
      (hash-table (ind (unhash x) i))
      (otherwise (ind (cons 1 x) i)))))

(defun vec-to-lst (xs)
  (let ((i 0))
    ($map1 (fn (x)
             (let ((v (aref xs i)))
               (incf i)
               v))
           (dup (length xs) nil))))

(defun lify (x)
  (typecase x
    (cons   x)
    (null   x)
    (string x)
    (array  (vec-to-lst x))
    (gs     (gs-lify x))
    (hash-table (lify (unhash x)))
    (otherwise x)))

(defun pre (e x)
  (typecase x
    (array  (if (< (length x) (lbp))
                (concatenate 'simple-vector (vector e) x)
                (pre e (lify x))))
    (cons   (pt-cat (cons 1 e) x))
    (null   (vector e))
    (gs     (pre e (gs-lify x)))
    (hash-table (pre e (unhash x)))
    (otherwise (vector e x))))

(defun suf (e x)
  (typecase x
    (array  (if (< (length x) (lbp))
                (concatenate 'simple-vector x (vector e))
                (suf e (lify x))))
    (cons   (pt-cat x (cons 1 e)))
    (null   (vector e))
    (gs     (suf e (gs-lify x)))
    (hash-table (suf e (unhash x)))
    (otherwise (vector x e))))

(defun lst (&rest xs)
  (let ((l (length xs)))
    (when (/= 0 l)
      (make-array l :initial-contents xs))))

(defun as-list (s) (if (or (pt? s) (null s)) s (lify s)))

(defun count-prefix (n s)
  (unless (functionp n)
    (error "take: first arg must be a number or a function"))
  (let ((l (len s)))
    (rec r ((m 0))
      (cond ((= m l) m)
            ((funcall n (ind s m)) (r (+ m 1)))
            (t m)))))

(defun take (n s)
  (unless (integerp n) (setf n (count-prefix n s)))
  (when (<= n 0)
    (if (= n 0) (return-from take nil))
    (return-from take
      (let ((n (+ (len s) n)))
        (if (<= n 0) s (drop n s)))))
  (typecase s
    (array (if (>= n (length s))
               s
               (multiple-value-bind (a start) (array-displacement s)
                 (if a
                     (displace start n a)
                     (displace 0 n s)))))
    (cons  (car (pt-cut s n)))
    (null  s)
    (gs    (let (r)
             (while (> n 0)
               (setf r (suf (gs-lhd s) r))
               (setf s (gs-ltl s))
               (unless s (return-from take r))
               (decf n))
             r))
    (hash-table (take n (unhash s)))
    (otherwise (error "take ~a ~a: not a list" n s))))

(defun drop (n s)
  (unless (integerp n) (setf n (count-prefix n s)))
  (when (< n 0)
    (return-from drop
      (let ((n (+ (len s) n)))
        (if (> n 0) (take n s)))))
  (typecase s
    (array (when (< n (length s))
             (multiple-value-bind (a start) (array-displacement s)
               (if a
                   (displace (+ start n) (- (length s) n) a)
                   (displace n (- (length s) n) s)))))
    (cons (cdr (pt-cut s n)))
    (null s)
    (gs    (while (> n 0)
             (setf s (gs-ltl s))
             (unless s (return-from drop nil))
             (decf n))
           s)
    (hash-table (drop n (unhash s)))
    (otherwise (error "take ~a ~a: not a list" n s))))

(defun conc2 (a b)
  (cond ((and (arrayp a) (arrayp b))
         (cond ((<= (+ (length a) (length b)) (lbp))
                 (concatenate 'simple-vector a b))
               (t (pt-cat (lify a) (lify b)))))
        ((null a) b)
        ((null b) a)
        ((pt? a) (cond ((pt? b) (pt-cat a b))
                       ((lst? b) (conc2 a (lify b)))
                       (t (suf b a))))
        ((pt? b) (cond ((lst? a) (conc2 (lify a) b))
                       (t (pre a b))))
        ((lst? a) (conc2 a (vector b)))
        (t (conc2 (vector a) b))))

(defun lhd (s) (ind    s  0))
(defun ltl (s) (drop   1  s))
(defun rhd (s) (ind    s -1))
(defun rtl (s) (drop  -1  s))

(defun 1st (s) (ind s 0))
(defun 2nd (s) (ind s 1))
(defun 3rd (s) (ind s 2))
(defun 4th (s) (ind s 3))

;; change value at index i to v
(defun cng (s i v)
  (declare (optimize (safety 0) (speed 3))
           (fixnum i))
  (typecase s
    (array (if (<= (length s) (lbp))
               (let ((r (copy-seq s)))
                 (setf (aref r i) v)
                 r)
               (cng (lify s) i v)))
    (cons  (if (and (>= i 0) (< i (m s)))
               (pt-cng s i v) s))
    (gs    (cng (gs-lify s) i v))
    (null s)
    (hash-table (drop n (unhash s)))
    (otherwise (error "cng ~a ~a ~a: not a list" s i v))))

;; remove element at at index i
(defun rm (s i)
  (conc2 (take i s) (drop (+ i 1) s)))

(defun lcng (s i v)
  (conc (take i s) v (drop (+ i 1) s)))

;; insert item before index position
(defun ins (s i v)
  (when (< i 0)
    (incf i)
    (return-from ins (conc2 (drop i s) (conc2 v (take i s)))))
  (conc2 (take i s) (pre v (drop i s))))

(defun lins (s i v)
  (when (< i 0)
    (incf i)
    (return-from lins (conc2 (drop i s) (conc2 v (take i s)))))
  (conc2 (take i s) (conc2 v (drop i s))))


;; listify sequence's internal structures (use for debugging)
(defun sqln (s)
  (if s (ln s)))

(defun explode-sym (x)
  (let ((l (length x))
        (o (make-array (length x))))
    (dotimes (i l)
      (setf (aref o i) (string (aref x i))))
    o))

(defun conc-list (l)
  ;; FIXME: we can speed-up conc by divide and conquer
  (let* ((hd (car l))
         (tl (when (cdr l) (conc-list (cdr l)))))
    (unless tl (return-from conc-list hd))
    (unless hd (return-from conc-list tl))
    (if (stringp hd)
        (if (stringp tl)
            (concatenate 'string hd tl)
            (concatenate 'string hd (prn tl)))
        (if (stringp tl)
            (concatenate 'string (prn hd) tl)
            (conc2 hd tl)))))

(defun conc (&rest args)
  (conc-list args))

;; reverse sequence
(defun sq-rev (xs)
  (if xs (pre (rhd xs) (sq-rev (rtl xs)))))

(defun rev (xs)
  (if (arrayp xs)
      (reverse xs)
      (sq-rev xs)))

; convert sequence to common lisp list
(defun sq-to-list (s)
  (typecase s
    (array (coerce s 'list))
    (cons  (pt-to-list s))
    (gs    (sq-to-list (gs-lify s)))
    (null s)
    (hash-table (sq-to-list (unhash s)))
    (otherwise (error "sq-to-list ~a: not a list" s))))

(defun list-to-sq (xs)
  (let ((l (length xs)))
    (cond ((= l 0) nil)
          ((<= l (lbp)) (coerce xs 'vector))
          (xs ($map1 (fn (x) (pop xs)) (dup l nil))))))

(defun sq-to-vec (o)
  (if (gs? o) (setf o (lify o)))
  (let* ((a (make-array (len o)))
         (i 0))
    (fe (fn (x) (setf (aref a i) x) (incf i) x) o)
    a))

(defun vec-to-sq (xs)
  (let ((l (length xs)))
    (when (/= l 0)
      xs)))

;; convert sequece to common lisp string
(defun sq-to-str (seq)
  (if (stringp seq) (return-from sq-to-str seq))
  (coerce (sq-to-list seq) 'string))

(defun str-to-sq (x)
  (vec-to-sq (coerce x 'simple-vector)))

(defun $map1 (f xs)
  (typecase xs
    (array
     (when (stringp xs) (setf xs (map 'vector #'string xs)))
     (map 'vector f xs))
    (cons (pt-map f xs))
    (null nil)
    (gs ($map1 f (gs-lify xs)))
    (hash-table ($map1 f (unhash xs)))
    (t (error "$map1: not a list `~a`" xs))))

(defun fe (f xs)
  (typecase xs
    (array (loop as x across xs do (funcall f x)))
    (cons (pt-map f xs))
    (null nil)
    (gs (fe f (gs-lify xs)))
    (hash-table (fe f (unhash xs)))
    (t (funcall f xs))))

;; map function over seqs
(defun $map (fun seq &rest seqs)
  (unless seqs (return-from $map ($map1 fun seq)))
  (let (r)
    (push seq seqs)
    (while (not (some #'null seqs))
      (setf r (suf (apply fun (mapcar #'lhd seqs)) r))
      (setf seqs (mapcar #'ltl seqs)))
    r))

(defun fold (i fun xs)
  (typecase xs
    (array (reduce fun xs :initial-value i))
    (cons (pt-fold i fun xs))
    (null i)
    (gs (fold i fun (gs-lify xs)))
    (hash-table (fold i fun (unhash xs)))
    (t (error "fold ~a ~a ~a: not a list" i fun xs))))

(defun all (fun xs)
  (fe (fn (x) (unless (equ x fun) (return-from all nil)))
        xs)
  t)

(defun any (fun xs)
  (fe (fn (x) (when (equ x fun) (return-from any t)))
        xs)
  nil)

(defun pos (fun xs &key (start 0))
  (fe (fn (x)
          (when (equ x fun) (return-from pos start))
          (incf start))
        (drop start xs))
  nil)

(defun fnd (fun xs &key (start 0))
  (fe (fn (x)
          (when (equ x fun) (return-from fnd x))
          (incf start))
        (drop start xs))
  nil)

(defmacro keep-hlp (name filter)
  `(defun ,name (p xs)
     (if (stringp xs) (setf xs (map 'vector #'string xs)))
     (unless (arrayp xs) (setf xs (sq-to-vec xs)))
     (let* ((r (,filter (fn (x) (equ x p)) xs))
            (l (length r)))
       (when (/= l 0) r))))

(keep-hlp keep remove-if-not)
(keep-hlp skip remove-if)
(defun cut (start end seq) (if (< start end) (drop start (take end seq))))



;; (split sep seq)
(defun $split (sep seq)
  (labels ((hlp (xs sep)
             (list-to-sq
              (loop as i = 0 then (1+ j)
                    as j = (pos sep xs :start i)
                 collect (cut i (if j j (len xs)) xs)
                 while j))))
    (funcall #'hlp seq sep)))

(defun $unsplit (sep seq)
  (fold (if (stringp sep) "" nil)
        (fn (a b) (conc a (lst sep) b)) seq))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun glb-decor (x) (concatenate 'string "glb-" x))
(defun cl-to-glb (x) (intern (glb-decor (symbol-name x)) *pkg*))

(defun local-to-global (s) (glb-decor s))
(defun global-to-local (n)
    (if (and (> (length n) 4) (string= (subseq n 0 4) "glb-"))
        (subseq n 4 (length n))
        n))



(defmacro sym? (o) `(stringp ,o))
(defmacro num? (o) `(numberp ,o))
(defun str? (o) (and (quote? o) (sym? (2nd o))))
(defun str-empty? (o) (and (str? o) (string= (2nd o) "")))

(defun wildcard? (p) (equal p "_"))

(defun fn-sym? (name) ; lowercased sym?
  (when (and (symbolp name) (not (keywordp name)))
    (setf name (symbol-name name)))
  (when (stringp name)
    (setf name (global-to-local name))
    (and (> (length name) 0)
         (not (upper-case-p (aref name 0)))
         (not (wildcard? name)))))

(defun var-sym? (e) ; uppercase sym?
  (and (sym? e) (not (fn-sym? e)) (not (wildcard? e))))


(defun st-sym (sym &key (upcase nil))
  (let ((n (symbol-name sym)))
    (if (and (not upcase) (every (complement #'lower-case-p) n))
        (setf n (string-downcase n)))
    (setf n (with-output-to-string (o)
              (dotimes (i (length n))
                (let ((c (char n i)))
                  (write-char c o)))))
   n))


;; NOTE: we cant reuse CL's backquote as it is a reader macro
(defun st-list (ys zs xs)
  (unless xs
    (if zs (setf ys (cons (cons 'lst (nreverse zs)) ys)))
    (setf ys (nreverse ys))
    (return-from st-list (if (cdr ys) (cons 'conc ys) (car ys))))
  (let ((x  (car xs))
        (xs (cdr xs)))
    (cond ((eql x '$)
           (st-list ys (cons (car xs) zs) (cdr xs)))
          ((eql x '$$)
           (if zs (setf ys (cons (cons 'lst (nreverse zs)) ys)))
           (setf ys (cons (car xs) ys))
           (st-list ys nil (cdr xs)))
          ((symbolp x)
           (let ((n (symbol-name x))
                 (p (symbol-package x)))
             (if (and (>= (length n) 2) (char= (aref n 0) #\$))
                 (if (char= (aref n 1) #\$)
                     (progn
                       (if zs (setf ys (cons (cons 'lst (nreverse zs)) ys)))
                       (setf ys (cons (intern (subseq n 2) p) ys))
                       (st-list ys nil xs))
                     (progn
                       (setf zs (cons (intern (subseq n 1) p) zs))
                       (st-list ys zs xs)))
                 (st-list ys (cons (st-hlp x) zs) xs))))
          (t  (st-list ys (cons (st-hlp x) zs) xs)))))

(defun st-hlp (o)
  (cond
    ((consp o) (st-list nil nil o))
    ((null o) nil)
    ((eql o 'n) nil)
    ((eql o 'quote) `',(st-sym '_q))
    ((stringp o) (lst (st-sym '|\\|) o))
    ((symbolp o) `',(st-sym o))
    (t o)))
) ; eval-when 

(defmacro st (x) (st-hlp x))

(defmacro c (f &rest as) `(,(cl-to-glb f) ,@as))

;; decorates symbol to global representation
(defmacro glb (x)
  (let* ((q (if (consp x) (first x)))
         (x (if (consp x) (second x) x))
         (d (cl-to-glb x))
         (d (if (and (fn-sym? (symbol-name x)) (not q)) `(function ,d) d)))
    (if q (setf d `(,q ,d)))
    d))


(defmacro $case (keyform &body cases)
  (flet ((hlp (k c) `(equal ,k ,(st-sym c))))
    (let ((k (gensym)))
      `(let ((,k ,keyform))
         (cond
           ,@(mapcar (fn (cs)
                       (let ((c  (car cs))
                             (cs (cdr cs)))
                         (cond ((eql c 'otherwise) `(t ,@cs))
                               ((consp c)
                                `((or ,@(mapcar (fn (c) (hlp k c)) c)) ,@cs))
                               (t `(,(hlp k c) ,@cs)))))
                     cases))))))

    
(defmacro $list-case (list &body cases)
  (let ((ls (gensym)) (hd (gensym)) (tl (gensym)))
    `(let* ((,ls ,list)
            (,hd (if (lst? ,ls) (lhd ,ls) (gensym)))
            (,tl (if (lst? ,ls) (ltl ,ls))))
       (cond ,@(mapcar
                (fn (x)
                  (if (eql (car x) 'otherwise)
                      `(t ,@(cdr x))
                      (bind (h vs &rest b) x
                        (let ((h (st-sym h))
                              (l (length vs)))
                          `((equal ,hd ',h)
                            ,@(unless (find '&rest vs)
                                `((assert (eql ,l (len ,tl)) nil
                                    "Invalid `~a` form" ',h)))
                            (bind ,vs (sq-to-list ,tl)
                              ,@b))))))
            cases)))))


(defmacro def-form-types (&body body)
  `(progn ,@(mapcar (fn (x) `(defun ,(first x) (o)
                               (and (lst? o)
                                    (equal (1st o) ,(st-sym (second x))))))
                    body)))

;; this crap could be avoided with $list-case macro
(def-form-types
  (_fn?     _fn )  (do?      do)  (bnd?     |:| )
  (unesc?    |!|)  (incut?  |@|)  (unquote? |$| )
  (quote?   |\\|)  (autogs? |&|)  (esc?       _q)
  (msg?    |.|)  (coma?    |,| ))

(defun lamb? (xs)
  (and (lst? xs) ($case (lhd xs) ((|<>| |<:>| |<clean>|) t))))

(defun fn? (x) (or (fn-sym? x) (lamb? x) (_fn? x)))

(defun arrow? (x) ($case x ((=) t)))
(defun has-arrow? (x) (fnd #'arrow? x))

(defun split-lambs (xs) ($split (st |;|) xs))


(defun filter-binds (es)
  (let ((e (lhd es)))
    (when (bnd? e) (lst (1st e) (2nd e) (pre (3rd e) (ltl es))))))

(defun split-expand-binds (xs cnd fail)
  (unless xs (return-from split-expand-binds))
  (let* ((x  (lhd xs))
         (xs (ltl xs))
         (b (filter-binds x)))
    (when (and b (unesc? (2nd b)))
      (setf x (st (_assign $(2nd (2nd b)) $(3rd b))))
      (setf b nil))
    (if b
        (let* ((l (meta-set (meta-get (1st b)) (copy-seq "_let")))
               (d (2nd b))
               (f (unisym "f"))
               (g (if (sym? d) d (unisym)))
               (r (if xs
                      (split-expand-binds xs nil nil)
                      g)))
          (st ($l (($g $(3rd b)))
                 $(if (sym? d)
                      (if cnd
                          (st (_if $g $r $fail))
                          r)
                      (if cnd
                          (st (_let (($f (_fn () $fail)))
                                 (_if $g
                                      (c (<clean> ($d = do $r |;| _ = c $f)) $g)
                                      (c $f))))
                          (st (c (<clean> ($d = do $r)) $g)))
                      ))))
        (if cnd
            (if xs
                (st (_if $x
                         $(split-expand-binds xs nil nil)
                         $fail))
                (if fail
                    (st (cl (or ($"@" $x) ($"@" $fail))))
                    x))
            (if xs
                (st (do $x $(split-expand-binds xs nil nil)))
                x)))))

(defun split-body (xs)
  (let* ((xs ($split #'arrow? xs))
         (x  (lhd xs))
         (xs (ltl xs)))
    (unless xs (return-from split-body (lst x)))
    (lst x (split-expand-binds xs nil nil))))

(defun get-type (x)
  (typecase x
    (cons      nil)
    (null      nil)
    (gs        nil)
    (string    (st |Sym|))
    (array     nil)
    (symbol    (st |Special|))
    (number    (st |Num|))
    (character (st |Chr|))
    (function  (st |Fn|))
    (otherwise (error "get-type: cant handle ~a" (type-of x)))))


(defmacro relate-types (a b before after &body types)
  (let ((types (mapcar (fn (xs)
                         (let ((x (car xs)))
                           (if (consp x) xs (cons (list x) (cdr xs)))))
                       types)))
    (flet ((process (f)
             (reduce (fn (ys xs)
                       (let ((zs (car xs))
                             (xs (cdr xs)))
                         (append ys (mapcar (fn (z) (cons z xs)) zs))))
                     (mapcar f types)
                     :initial-value nil)))
      `(typecase ,a
         ,@(process (fn (xs)
                      (let ((r before)
                            (ts (car xs)))
                        `(,ts (typecase ,b
                                ,@(process (fn (xs)
                                             (let ((x (car xs)))
                                               (if (find (car ts) x)
                                                   (progn (setf r after)
                                                          xs)
                                                   (list x r))))))))))))))


(defun pt-equ (a b)
  (if (/= (m a) (m b)) (return-from pt-equ nil))
  (dotimes (i (m a))
    (if (not (equ (pt-ind a i) (pt-ind b i)))
        (return-from pt-equ nil)))
  t)

(defun sq-equ (a b)
  (if a
      (and b (equ (lhd a) (lhd b)) (sq-equ (ltl a) (ltl b)))
      (null b)))

(defun sq-lt (xs ys)
  (while (and xs ys)
    (let ((x (lhd xs))
          (y (lhd ys)))
      (unless (equ x y)
        (return-from sq-lt (lt x y)))
      (setf xs (ltl xs)
            ys (ltl ys))))
  (< (len xs) (len ys)))


(defun equ (a b)
  (if (functionp b) (return-from equ (funcall b a)))
  (relate-types a b nil nil
    (null       t)
    (number     (= a b))
    (string     (string= a b))
    ((cons
      gs
      array)    (muffle-compiler-note 
                  (if (and (pt? a) (pt? b))
                      (pt-equ a b)
                      (sq-equ a b))))
    (symbol     (eql a b))
    (character  (char= a b))))

(defun lt (a b)
  (relate-types a b nil t
    (null       nil)
    (number     (< a b))
    (string     (string< a b))
    ((cons
      gs
      array)    (sq-lt a b))
    (symbol     (string< (symbol-name a) (symbol-name b)))
    (character  (char< a b))))

(defun lte (a b) (or (lt a b) (equ a b)))
(defun gt  (a b) (not (lte a b)))
(defun gte (a b) (not (lt a b)))


(defmacro ns-findm (xs n acc found else)
  (with-gensyms (lb ub ret)
    `(block ,ret
       (let* ((,lb 0)
              (,ub (len ,xs))
              (p 0)
              (x nil)
              (k nil))
         (while (< ,lb ,ub)
           (setf p (truncate (+ ,lb ,ub) 2))
           (setf x (,acc ,xs p))
           (setf k (1st x))
           (cond ((lt ,n k) (setf ,ub p))
                 ((lt k ,n) (setf ,lb (+ p 1)))
                 (t (return-from ,ret ,found))))
         ,else))))

(defun ns-get (xs i)
  (cond ((and (vectorp xs) (not (stringp xs)))
         (ns-findm xs i aref (2nd x) nil))
        ((hash-table-p xs) (gethash i xs))
        ((consp xs) (ns-findm xs i ind (2nd x) nil))
        ((null xs) nil)
        (t (error "invalid xs: ns-get xs=~a" (prn xs)))))

(defun ns-set (xs i v)
  (let ((e (vector i v)))
    (flet ((fail (p k)
             (if (lt i k)
                 (ins xs p e)
                 (ins xs (+ p 1) e))))
      (cond ((and (vectorp xs) (not (stringp xs)))
             (ns-findm xs i aref (cng xs p e) (fail p k)))
            ((consp xs) (ns-findm xs i ind (cng xs p e) (fail p k)))
            ((null xs) (vector e))
            ((hash-table-p xs) (let ((r (hash xs)))
                                 (setf (gethash i r) v)
                                 r))
            (t (error "invalid xs: ns-set xs=~a" (prn xs)))))))

(defun ns-rm (xs i)
  (cond ((vectorp xs) (ns-findm xs i aref (rm xs p) xs))
        ((consp xs) (ns-findm xs i ind (rm xs p) xs))
        ((null xs) nil)
        (t (error "invalid xs: ns-rm xs=~a" (prn xs)))))


(defun ns-ins (xs i v)
  (let ((e (vector i v)))
    (flet ((fail (p k)
             (if (lt i k)
                 (ins xs p e)
                 (ins xs (+ p 1) e))))
      (cond ((vectorp xs) (ns-findm xs i aref (ins xs p e) (fail p k)))
            ((consp xs) (ns-findm xs i ind (ins xs p e) (fail p k)))
            ((null xs) (vector e))
            (t (error "invalid xs: ns-ins xs=~a" (prn xs)))))))



(defparameter *meta-db* (make-hash-table :test 'eq))

(defun meta-set (info object)
  (setf (gethash object *meta-db*) info)
  object)

(defun meta-get (object)
  (gethash object *meta-db*))

(defun copy-hash (h)
  (let ((r (make-hash-table :test 'equal)))
    (maphash (fn (k v) (setf (gethash k r) v)) h)
    r))

(defun hash (xs)
  (if (hash-table-p xs)
      (copy-hash xs)
      (let ((h (make-hash-table :test 'equal)))
        ($map (fn (x) (setf (gethash (1st x) h) (2nd x))) xs)    
        h)))

(defun unhash (h)
  (unless (hash-table-p h) (error "unhash: not a hashtable `~a`" h))
  (let* ((l (hash-table-count h))
         (r (make-array l))
         (i 0))
    (declare (optimize (safety 0) (speed 3))
             (fixnum i))
    (maphash (fn (k v)
               (setf (aref r i) (vector k v))
               (incf i))
             h)
    r))
