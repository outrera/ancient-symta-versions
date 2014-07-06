(in-package :symta)


(defstruct typed type data)
(defun typed? (x) (if (typep x 'typed) x))

;; generated sequence
(defstruct gs head tail)
(defun gs? (x) (if (typep x 'gs) x))

(defun gs-lhd (gs) (gs-head gs))
(defun gs-ltl (gs) (funcall (gs-tail gs) #'identity))

(defun gs-lify (s)
  (let (r)
    (while s
      (push (gs-lhd s) r)
      (setf s (gs-ltl s)))
    (list-to-sq (nreverse r))))

(defun gs-len (s)
  (len (gs-lify s)))

(defun gs-ind (s i)
  (if (< i 0) (return-from gs-ind (ind (gs-lify s) i)))
  (while (> i 0)
    (setf s (gs-ltl s))
    (unless s (return-from gs-ind nil))
    (decf i))
  (gs-lhd s))

(defun gen (hd tl)
  (make-gs :head hd :tail tl))

(defmacro lazy (hd tl)
  `(gen ,hd (fn (k) (funcall k ,tl))))

(defun lazy-ints (n end)
  (if (<= n end) (lazy n (lazy-ints (+ n 1) end))))

(defun len (x)
  (typecase x
    (cons   (m x))
    (null   0)
    (typed  (len (typed-data x)))
    (gs     (gs-len x))
    (string (length x))
    (array  (length x))
    (symbol (length (symbol-name x)))
    (otherwise (error "len: cant handle ~a" (type-of x)))))

(defun array-ind (x i)
  (let ((l (length x)))
    (when (< i 0) (setf i (+ l i)))
    (if (and (>= i 0) (< i l))
        (aref x i))))

(defun ind (x i)
  (declare (optimize (safety 0) (speed 3))
           (fixnum i))
  (typecase x
    (cons   (cond ((>= i 0) (if (< i (m x)) (pt-ind x i)))
                  (t        (let ((i (+ (m x) i)))
                              (if (>= i 0) (pt-ind x i))))))
    (null   x)
    (typed  (ind (typed-data x) i))
    (gs     (gs-ind x i))
    (array  (array-ind x i))
    (symbol (array-ind (symbol-name x) i))
    (otherwise (error "ind: cant handle ~a" (type-of x)))))

(defun lify (x)
  (typecase x
    (cons   x)
    (null   x)
    (typed  (lify (typed-data x)))
    (gs     (gs-lify x))
    (string (list-to-sq (explode x)))
    (array  (list-to-sq (explode x)))
    (symbol (lify (symbol-name x)))
    (otherwise (error "lify: cant handle ~a" (type-of x)))))

(defun pre (e x)
  (typecase x
    (array (if (< (length x) 32)
               (concatenate 'simple-vector (vector e) x)
               (pre e (lify x))))
    (cons (pt-cat (cons 1 e) x))
    (null (vector e))
    (otherwise (pre e (lify x)))))

(defun suf (e x)
  (typecase x
    (array (if (< (length x) 32)
               (concatenate 'simple-vector x (vector e))
               (suf e (lify x))))
    (cons (pt-cat x (cons 1 e)))
    (null (vector e))
    (otherwise (suf e (lify x)))))

(defun lst (&rest xs) (list-to-sq xs))

(defun as-list (s) (if (or (pt? s) (null s)) s (lify s)))

(defun displace (start size array)
  (make-array size
              :element-type (array-element-type array)
              :displaced-to array :displaced-index-offset start))

(defun take (n s)
  (when (<= n 0)
    (return-from take
      (let ((n (+ (len s) n)))
        (if (<= n 0) s (drop n s)))))
  (typecase s
    (cons  (car (pt-cut s n)))
    (null  s)
    (array (if (>= n (length s))
               s
               (multiple-value-bind (a start) (array-displacement s)
                 (if a
                     (displace start n a)
                     (displace 0 n s)))))
    (gs    (let (r)
             (while (> n 0)
               (setf r (suf (gs-lhd s) r))
               (setf s (gs-ltl s))
               (unless s (return-from take r))
               (decf n))
             r))
    (otherwise (take n (lify s)))))

(defun drop (n s)
  (when (< n 0)
    (return-from drop
      (let ((n (+ (len s) n)))
        (if (> n 0) (take n s)))))
  (typecase s
    (cons (cdr (pt-cut s n)))
    (null s)
    (array (when (< n (length s))
             (multiple-value-bind (a start) (array-displacement s)
               (if a
                   (displace (+ start n) (- (length s) n) a)
                   (displace n (- (length s) n) s)))))
    (gs    (while (> n 0)
             (setf s (gs-ltl s))
             (unless s (return-from drop nil))
             (decf n))
           s)
    (otherwise (drop n (lify s)))))


(defun conc2 (a b)
  (cond ((pt? a) (if (pt? b)
                       (pt-cat a b)
                       (if (null b) a (conc2 a (lify b)))))
        ((null a) b)
        ((null b) a)
        ((and (arrayp a) (arrayp b)) (concatenate 'simple-vector a b))
        (t (conc2 (lify a) b))))

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
    (cons  (cond ((>= i 0) (if (< i (m s)) (pt-cng s i v) s))
                 (t        (let ((i (+ (m s) i)))
                             (if (>= i 0) (pt-cng s i v) s)))))
    (typed (make-typed :type (typed-type s)
                       :data (cng (typed-data s) i v)))
    (string (locally (declare ((simple-array character) s))
                (let ((r (copy-seq s)))
                  (setf (aref r i) v)
                  r)))
    (array (if (<= (length s) 32)
               (let ((r (copy-seq s)))
                 (setf (aref r i) v)
                 r)
               (cng (lify s) i v)))
    (otherwise (cng (lify s) i v))))

;; remove element at at index i
(defun rm (s i)
  (conc2 (take i s) (drop (+ i 1) s)))

(defun lcng (s i v)
  (conc (take i s) v (drop (+ i 1) s)))

;; insert item before index position
(defun ins (s i v)
  (when (< i 0)
    (incf i)
    (return-from ins
      (conc2 (drop i s) (conc2 v (take i s)))))
  (conc2 (take i s) (pre v (drop i s))))

(defun lins (s i v)
  (when (< i 0)
    (incf i)
    (return-from lins
      (conc2 (drop i s) (conc2 v (take i s)))))
  (conc2 (take i s) (conc2 v (drop i s))))


;; listify sequence's internal structures (use for debugging)
(defun sqln (s)
  (if s (ln s)))

(defun conc-list (l)
  ;; FIXME: we can speed-up conc by divide and conquer
  (let ((hd (car l))
        (tl (cdr l)))
    (cond (tl (conc2 hd (conc-list tl)))
          ((stringp hd) (lify hd))
          (t hd))))

(defun conc (&rest args)
  (conc-list args))

;; reverse sequence
(defun rev (s)
  (if s (pre (rhd s) (rev (rtl s)))))

; convert sequence to common lisp list
(defun sq-to-list (seq)
  (unless (pt? seq)
    (unless seq (return-from sq-to-list nil))
    (setf seq (cond ((gs? seq) (gs-lify seq))
                    ((characterp seq) (return-from sq-to-list (list seq)))
                    (t (as-list seq)))))
  (pt-to-list seq))

(defun list-to-sq (l)
  (if l ($map1 (fn (x) (pop l))
               (dup (length l) nil))))

;; convert sequece to common lisp string
(defun sq-to-str (seq)
  (if (stringp seq) (return-from sq-to-str seq))
  (coerce (sq-to-list seq) 'string))

(defun $map1 (f xs)
  (typecase xs
    (cons (pt-map f xs))
    (null nil)
    (array (map 'vector f xs))
    (gs ($map1 f (gs-lify xs)))
    (t ($map1 f (as-list xs)))))

(defun fe (f xs)
  (typecase xs
    (cons (pt-map f xs))
    (null nil)
    (array (loop as x across xs do (funcall f x)))
    (gs (fe f (gs-lify xs)))
    (t (fe f (as-list xs)))))

;; map function over seqs
(defun $map (fun seq &rest seqs)
  (unless seqs (return-from $map ($map1 fun seq)))
  (let (r)
    (push seq seqs)
    (while (not (some #'null seqs))
      (setf r (suf (apply fun (mapcar #'lhd seqs)) r))
      (setf seqs (mapcar #'ltl seqs)))
    r))

(defun fold (fun seq)
  (if (pt? seq)
      (pt-fold fun seq)
      (reduce fun (sq-to-list seq))))

(defun unfold (fun o)
  (if-bind it (funcall fun o)
    (fold #'conc ($map (fn (x) (unfold fun x)) it))
    (lst o)))

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

(defun fst (fun xs)
  (fe (fn (x) (if-bind v (funcall fun #'identity x) (return-from fst v)))
        xs)
  nil)


; keep matching items
(defun keep (fun seq)
  (list-to-sq (remove-if-not (fn (x) (equ x fun)) (sq-to-list seq))))

; remove matching items
(defun strip (fun seq)
  (list-to-sq (remove-if (fn (x) (equ x fun)) (sq-to-list seq))))

(defun cut (start end seq)
  (if (< start end) (drop start (take end seq))))

;; (split sep seq)
;; (split #\. "abc.def") -> ("abc" "def")
(defun split (sep seq)
  (labels ((hlp (xs sep)
             (list-to-sq
              (loop as i = 0 then (1+ j)
                    as j = (pos sep xs :start i)
                 collect (cut i (if j j (len xs)) xs)
                 while j))))
     (if (str? seq)
         ($map #'sq-to-str (funcall #'hlp seq sep))
         (funcall #'hlp seq sep))))

(defun unsplit (sep seq)
  (fold (fn (a b) (conc a (lst sep) b)) seq))

(defmacro lst? (x)
  (with-gensyms (v)
    `(let ((,v ,x))
       (typecase ,v
         (cons  t)
         (array (not (stringp ,v)))
         (gs    t)))))

(defmacro fin? (x) `(not (lst? ,x)))


;; decorates symbol to global representation
(defmacro deco (x)
  (let ((q nil))
    (when (lst? x)
      (setf q t)
      (setf x (second x)))
    (let ((r (intern (concatenate
                     'string "*;st;" (symbol-name x) "*") *cl-pkg*)))
      (if q (setf r (list 'quote r)))
      r)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun local-to-global (s)
  (intern (concatenate 'string "*" (symbol-name s) "*") *cl-pkg*))

(defun global-to-local (s)
  (let ((n (symbol-name s)))
    (if (eql (aref n 0) #\*)
        (intern (subseq n 1 (- (length n) 1)) (symbol-package s))
        s)))

(defun st-sym (sym &key (upcase nil))
  (let ((n (symbol-name sym)))
    (if (and (not upcase) (every (complement #'lower-case-p) n))
        (setf n (string-downcase n)))
    (setf n (with-output-to-string (o)
              (dotimes (i (length n))
                (let ((c (char n i)))
                  (when (or (eql c #\;) (eql c #\\))
                    (write-char #\\ o))
                  (write-char c o)))))
    (intern (concatenate 'string ";st;" n) *cl-pkg*)))


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
    ((lst? o) (st-list nil nil o))
    ((null o) nil)
    ((eql o 'n) nil)
    ((eql o 'y) t)
    ((eql o 'quote) `',(st-sym '|'|))
    ((symbolp o)
     (let ((n (symbol-name o)))
       (if (or (= (length n) 0) (not (char= (aref n 0) #\;)))
           `',(st-sym o)
           `',o)))
    (t o)))
) ; eval-when 

(defmacro st (x)
  (st-hlp x))

(defmacro $case (keyform &body cases)
  `(case ,keyform
     ,@(mapcar (fn (cs)
                 (let ((c (car cs)))
                   (cond ((eql c 'otherwise) cs)
                         ((consp c) (cons (mapcar #'st-sym c) (cdr cs)))
                         (t (cons (st-sym c) (cdr cs))))))
               cases)))

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
                          `((eql ,hd ',h)
                            ,@(unless (find '&rest vs)
                                `((assert (eql ,l (len ,tl)) nil
                                    "Invalid `~a` form" ',h)))
                            (bind ,vs (sq-to-list ,tl)
                              ,@b))))))
            cases)))))


(defmacro def-form-types (&body body)
  `(progn ,@(mapcar (fn (x) `(defun ,(first x) (o)
                               (and (lst? o)
                                    (eql (1st o) ',(st-sym (second x))))))
                    body)))

;; this crap could be avoided with $list-case macro
(def-form-types
  (_fn?     _fn )  (do?      do )  (bnd?     |:| )
  (set?     |=:|)  (unesc?  |!| )  (incut?   |@| )  (interp?  |$| )
  (quasi?   \#  )  (autogs? |&| )  (rng?     |..|)  (replica? |++|)
  (kw?      |=| )  (esc?    |'| )  (msg?     |.| )  (coma?    |,| ))


(defun blk? (x)
  (and (lst? x) (let ((h (lhd x)))
                  (or (eql h (st |blk|))
                      (eql h (st |namedBlk|))))))



(defun get-type (x)
  (typecase x
    (cons      nil)
    (null      nil)
    (typed     (typed-type x))
    (gs        nil)
    (string    (st |Str|))
    (array     nil)
    (symbol    (st |Sym|))
    (number    (st |Num|))
    (character (st |Chr|))
    (function  (st |Fn|))
    (otherwise (error "get-type: cant handle ~a" (type-of x)))))

(defun set-type (x v)
  (make-typed :type v :data (as-list x)))

(defun sym? (o) (and (symbolp o) (not (eql o t)) (not (eql o nil))))
(defun str? (o) (stringp o))
(defun num? (o) (numberp o))
(defun ch? (o) (characterp o))



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
  (if (functionp b) (return-from equ (funcall b #'identity a)))
  (relate-types a b nil nil
    (null       t)
    (string     (string= a b))
    (typed      (if (eql (typed-type a) (typed-type b))
                    (sq-equ (typed-data a) (typed-data b))))
    ((cons
      gs
      array)    (muffle-compiler-note 
                  (if (and (pt? a) (pt? b))
                      (pt-equ a b)
                      (sq-equ a b))))
    (number     (= a b))
    (symbol     (eql a b))
    (character  (char= a b))))

(defun lt (a b)
  (relate-types a b nil t
    (null       nil)
    (string     (string< a b))
    ((cons
      gs
      typed
      array)    (sq-lt a b))
    (number     (< a b))
    (symbol     (string< (symbol-name a) (symbol-name b)))
    (character  (char< a b))))

(defun lte (a b) (or (lt a b) (equ a b)))
(defun gt  (a b) (not (lte a b)))
(defun gte (a b) (not (lt a b)))


;; namespace implementation
(defun ns-find (x n found-fun else-fun)
  (let ((lb 0)
        (ub (len x)))
    (while (< lb ub)
      (let* ((p (truncate (+ lb ub) 2))
             (v (ind x p)))
        (cond
          ((lt n (1st v)) (setf ub p))
          ;; add 1 to exclude p from future search
          ((lt (1st v) n) (setf lb (+ p 1)))
          (t (return-from ns-find (funcall found-fun p))))))
    (funcall else-fun lb)))

(defun ns-get (x n)
  (ns-find x n
           (fn (i) (2nd (ind x i)))
           (fn (i) (identity nil))))

(defun ns-set (x n entry)
  (let ((e (lst n entry)))
    (ns-find x n
         (fn (i) (cng x i e))
         (fn (i) (cond ((= (len x) 0)          (lst e))
                       ((lt n (1st (ind x i))) (ins x i e))
                       (t                      (ins x (+ i 1) e)))))))
(defun ns-ins (x n entry)
  (let ((e (lst n entry)))
    (ns-find x n
         (fn (i) (ins x i e))
         (fn (i) (cond ((= (len x) 0)          (lst e))
                       ((lt n (1st (ind x i))) (ins x i e))
                       (t                      (ins x (+ i 1) e)))))))
(defun ns-cng (x n f)
  (unless (functionp f) (let ((r f)) (setf f (fn (x) r))))
  (ns-find x n
       (fn (i) (cng x i (lst n (funcall f #'identity (2nd (ind x i))))))
       (fn (i)
         (let ((e (lst n (funcall f #'identity nil))))
           (cond ((= (len x) 0)          (lst e))
                 ((lt n (1st (ind x i))) (ins x i e))
                 (t                      (ins x (+ i 1) e)))))))

(defun ns-rm (x n)
  (ns-find x n
           (fn (i) (rm x i))
           (fn (i) x)))


